#!/bin/bash
# inscribe-lib.sh — credential-gathering helpers for ac-inscribe and ac-os.
#
# Builds the JSON bundle that gets written to the AC OS USB's config.json
# (handle, sub, email, ac-token, claude-token, github-pat, optional Claude
# session). Sourceable from ac-inscribe (validating step-by-step CLI) and
# from ac-os (just-in-time fallback when no pre-built inscription exists).
#
# Conventions:
#   - All helpers prefixed aci_ to avoid clobbering ac-os functions.
#   - Helpers set ACI_* globals (uppercase) on success; return non-zero on
#     failure. Callers decide how to surface errors (UX is in ac-inscribe).
#   - No echo'd JSON unless the function is explicitly an emitter
#     (aci_build_bundle_json). Logging is the caller's job.

set -u

# ─── color helpers ─────────────────────────────────────────────────
# Honor NO_COLOR (per https://no-color.org) and detect tty automatically.
if [ -t 1 ] && [ -z "${NO_COLOR:-}" ]; then
    ACI_C_RESET=$'\033[0m'
    ACI_C_DIM=$'\033[2m'
    ACI_C_BOLD=$'\033[1m'
    ACI_C_RED=$'\033[0;31m'
    ACI_C_GREEN=$'\033[0;32m'
    ACI_C_YELLOW=$'\033[0;33m'
    ACI_C_CYAN=$'\033[0;36m'
    ACI_C_MAGENTA=$'\033[0;35m'
else
    ACI_C_RESET=""; ACI_C_DIM=""; ACI_C_BOLD=""
    ACI_C_RED=""; ACI_C_GREEN=""; ACI_C_YELLOW=""
    ACI_C_CYAN=""; ACI_C_MAGENTA=""
fi

aci_step() {
    # $1 = step number, $2 = total steps, $3 = title
    printf '\n%s[ac-inscribe]%s %sstep %s/%s%s — %s%s%s\n' \
        "${ACI_C_CYAN}" "${ACI_C_RESET}" \
        "${ACI_C_DIM}" "$1" "$2" "${ACI_C_RESET}" \
        "${ACI_C_BOLD}" "$3" "${ACI_C_RESET}"
}

aci_ok()    { printf '              %s✓%s %s\n'  "${ACI_C_GREEN}"  "${ACI_C_RESET}" "$*"; }
aci_warn()  { printf '              %s⚠%s %s\n'  "${ACI_C_YELLOW}" "${ACI_C_RESET}" "$*"; }
aci_fail()  { printf '              %s✗%s %s\n'  "${ACI_C_RED}"    "${ACI_C_RESET}" "$*" >&2; }
aci_info()  { printf '              %s·%s %s\n'  "${ACI_C_DIM}"    "${ACI_C_RESET}" "$*"; }

aci_die() {
    printf '\n%s[ac-inscribe]%s %saborting%s — %s\n' \
        "${ACI_C_CYAN}" "${ACI_C_RESET}" "${ACI_C_RED}" "${ACI_C_RESET}" "$*" >&2
    exit 1
}

# ─── auth + login ──────────────────────────────────────────────────

ACI_TOKEN_FILE="${HOME}/.ac-token"

aci_token_file() { printf '%s\n' "${ACI_TOKEN_FILE}"; }

# aci_require_login [--no-launch]
# Ensures ~/.ac-token exists and is parseable. Sets ACI_HANDLE, ACI_SUB,
# ACI_EMAIL, ACI_ACCESS_TOKEN. Returns 1 on failure.
aci_require_login() {
    local no_launch=0
    [ "${1:-}" = "--no-launch" ] && no_launch=1

    local script_dir
    script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
    local LOGIN_SCRIPT="${script_dir}/../../../tezos/ac-login.mjs"

    # Token is missing OR expired — both need ac-login. Use the same
    # 60s pre-expiry buffer flash-mac.sh uses so we never bake a token
    # that's about to flip on first boot.
    local needs_login=0
    if [ ! -f "${ACI_TOKEN_FILE}" ]; then
        needs_login=1
    elif command -v node >/dev/null 2>&1; then
        needs_login=$(node -e '
            try {
                const t = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
                const rawExp = t.expires_at || 0;
                const expMs  = rawExp > 10_000_000_000 ? rawExp : rawExp * 1000;
                process.stdout.write((!expMs || Date.now() >= expMs - 60_000) ? "1" : "0");
            } catch { process.stdout.write("1"); }
        ' "${ACI_TOKEN_FILE}" 2>/dev/null || echo 1)
    fi

    if [ "${needs_login}" = "1" ]; then
        if [ "${no_launch}" -eq 1 ]; then
            aci_fail "token at ${ACI_TOKEN_FILE} is missing or expired — run: ac-login"
            return 1
        fi
        if [ -f "${LOGIN_SCRIPT}" ]; then
            aci_warn "token missing/expired — launching ac-login..."
            node "${LOGIN_SCRIPT}" || { aci_fail "ac-login failed"; return 1; }
        else
            aci_fail "token missing/expired and ac-login script not found at ${LOGIN_SCRIPT}"
            return 1
        fi
    fi

    if [ ! -f "${ACI_TOKEN_FILE}" ]; then
        aci_fail "login cancelled (no token written)"
        return 1
    fi

    ACI_SUB=$(node -e "const t=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8')); console.log(t.user?.sub || '')" "${ACI_TOKEN_FILE}" 2>/dev/null)
    if [ -z "${ACI_SUB}" ]; then
        aci_fail "token at ${ACI_TOKEN_FILE} is invalid or expired — run: ac-login"
        return 1
    fi
    ACI_HANDLE=$(node -e "const t=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8')); let h=t.user?.handle || t.user?.name || ''; if(h.startsWith('@'))h=h.slice(1); console.log(h)" "${ACI_TOKEN_FILE}" 2>/dev/null)
    ACI_EMAIL=$(node -e "const t=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8')); console.log(t.user?.email || '')" "${ACI_TOKEN_FILE}" 2>/dev/null)
    ACI_ACCESS_TOKEN=$(node -e "const t=JSON.parse(require('fs').readFileSync(process.argv[1],'utf8')); process.stdout.write(t.access_token || '')" "${ACI_TOKEN_FILE}" 2>/dev/null)
    return 0
}

# ─── handle-token API fetch ────────────────────────────────────────

# aci_fetch_handle_tokens [target_handle]
# Calls /.netlify/functions/claude-token with the AC access token.
# When target_handle is provided, hits the admin path (?handle=X) — the
# server gates this on hasAdmin(user). Sets ACI_HANDLE_API,
# ACI_CLAUDE_TOKEN, ACI_GITHUB_PAT, ACI_TARGET_SUB. Returns 0 on HTTP
# success even if individual fields are empty; 1 on network/HTTP failure
# (including 403 when a non-admin asks for another handle).
aci_fetch_handle_tokens() {
    [ -n "${ACI_ACCESS_TOKEN:-}" ] || { aci_fail "no access token to fetch with"; return 1; }
    local target="${1:-}"
    local url="https://aesthetic.computer/.netlify/functions/claude-token"
    if [ -n "${target}" ]; then
        url="${url}?handle=${target}"
    fi
    local resp
    resp=$(curl -fsSL \
        -H "Authorization: Bearer ${ACI_ACCESS_TOKEN}" \
        "${url}" \
        --max-time 20 2>/dev/null) || return 1

    ACI_HANDLE_API=$(node -e "const d=JSON.parse(process.argv[1]||'{}'); process.stdout.write(d.handle||'')" "${resp}" 2>/dev/null)
    ACI_TARGET_SUB=$(node -e "const d=JSON.parse(process.argv[1]||'{}'); process.stdout.write(d.sub||'')" "${resp}" 2>/dev/null)
    ACI_CLAUDE_TOKEN=$(node -e "const d=JSON.parse(process.argv[1]||'{}'); process.stdout.write(d.token||'')" "${resp}" 2>/dev/null)
    ACI_GITHUB_PAT=$(node -e "const d=JSON.parse(process.argv[1]||'{}'); process.stdout.write(d.githubPat||'')" "${resp}" 2>/dev/null)
    return 0
}

# ─── local Claude session detection ────────────────────────────────

# ─── handle-colors + mood (boot personalization) ───────────────────

# aci_fetch_handle_colors <handle>
# Hits /api/handle-colors?handle=X and stores the raw "colors" JSON
# array (e.g. '[{"r":255,"g":128,"b":64},...]') on success in
# ACI_HANDLE_COLORS_JSON. Empty when no custom colors are stored — the
# C boot renderer falls back to its theme color in that case.
aci_fetch_handle_colors() {
    local handle="$1"
    [ -n "${handle}" ] || { ACI_HANDLE_COLORS_JSON=""; return 0; }
    local resp
    resp=$(curl -fsSL --max-time 10 \
        "https://aesthetic.computer/api/handle-colors?handle=${handle}" \
        2>/dev/null) || { ACI_HANDLE_COLORS_JSON=""; return 0; }
    ACI_HANDLE_COLORS_JSON=$(node -e "
        try {
            const d = JSON.parse(process.argv[1] || '{}');
            if (Array.isArray(d.colors) && d.colors.length > 0) {
                process.stdout.write(JSON.stringify(d.colors));
            }
        } catch (e) {}
    " "${resp}" 2>/dev/null)
    return 0
}

# aci_fetch_mood <handle>
# Hits /api/mood/@<handle> and stores the mood string (just the .mood
# text, not the full envelope) in ACI_MOOD. Empty if no mood is set.
aci_fetch_mood() {
    local handle="$1"
    [ -n "${handle}" ] || { ACI_MOOD=""; return 0; }
    local resp
    resp=$(curl -fsSL --max-time 10 \
        "https://aesthetic.computer/api/mood/@${handle}" \
        2>/dev/null) || { ACI_MOOD=""; return 0; }
    ACI_MOOD=$(node -e "
        try {
            const d = JSON.parse(process.argv[1] || '{}');
            if (typeof d.mood === 'string' && d.mood.length > 0) {
                process.stdout.write(d.mood);
            }
        } catch (e) {}
    " "${resp}" 2>/dev/null)
    return 0
}

# aci_collect_local_claude
# Sets ACI_CLAUDE_CREDS (raw JSON text), ACI_CLAUDE_STATE (derived JSON)
# when:
#   - ~/.claude.json exists AND
#   - ~/.claude.json oauthAccount.emailAddress matches ACI_EMAIL (case-
#     insensitive) AND
#   - AC_FLASH_INCLUDE_LOCAL_CLAUDE is not 0/false/no
# Returns 0 always; emptiness of globals is what callers check.
aci_collect_local_claude() {
    ACI_CLAUDE_CREDS=""
    ACI_CLAUDE_STATE=""
    ACI_CLAUDE_LOCAL_EMAIL=""

    local mode="${AC_FLASH_INCLUDE_LOCAL_CLAUDE:-auto}"
    case "${mode}" in
        0|false|FALSE|no|NO)  return 0 ;;
    esac

    [ -n "${ACI_EMAIL:-}" ] || return 0
    [ -f "${HOME}/.claude.json" ] || return 0

    ACI_CLAUDE_LOCAL_EMAIL=$(node -e "
        const d = JSON.parse(require('fs').readFileSync(process.argv[1], 'utf8'));
        const email = d.oauthAccount?.emailAddress || '';
        process.stdout.write(String(email).trim().toLowerCase());
    " "${HOME}/.claude.json" 2>/dev/null)

    [ -n "${ACI_CLAUDE_LOCAL_EMAIL}" ] || return 0
    local ac_lower
    ac_lower=$(printf '%s' "${ACI_EMAIL}" | tr '[:upper:]' '[:lower:]')
    case "${mode}" in
        1|true|TRUE|yes|YES)  ;;  # forced — skip mismatch check
        *)
            [ "${ACI_CLAUDE_LOCAL_EMAIL}" = "${ac_lower}" ] || return 0
            ;;
    esac

    if [ -f "${HOME}/.claude/.credentials.json" ]; then
        ACI_CLAUDE_CREDS=$(cat "${HOME}/.claude/.credentials.json" 2>/dev/null)
    fi
    ACI_CLAUDE_STATE=$(node -e "
        const d=JSON.parse(require('fs').readFileSync(process.env.HOME+'/.claude.json','utf8'));
        console.log(JSON.stringify({
            oauthAccount:d.oauthAccount,
            hasCompletedOnboarding:true,
            installMethod:'manual',
            numStartups:1,
            autoUpdates:false,
            autoUpdatesProtectedForNative:true
        }));
    " 2>/dev/null)
    return 0
}

# ─── bundle assembly ───────────────────────────────────────────────

# aci_build_usb_config_json
# Emits the inner JSON (matches the existing write_usb_config_file shape)
# to stdout. Inputs: ACI_HANDLE/ACI_SUB/ACI_EMAIL/ACI_ACCESS_TOKEN +
# optional ACI_CLAUDE_TOKEN/ACI_GITHUB_PAT/ACI_CLAUDE_CREDS/ACI_CLAUDE_STATE
# + boot personalization: ACI_HANDLE_COLORS_JSON (raw JSON array string)
# and ACI_MOOD (plain string).
aci_build_usb_config_json() {
    node -e "
        const [handle, sub, email, token, claudeToken, githubPat, claudeCreds, claudeState, colorsJson, mood, city] = process.argv.slice(1);
        const cfg = { handle, sub, email, token };
        if (claudeToken) cfg.claudeToken = claudeToken;
        if (githubPat)   cfg.githubPat   = githubPat;
        try { if (claudeCreds) cfg.claudeCreds = JSON.parse(claudeCreds); } catch (e) {}
        try { if (claudeState) cfg.claudeState = JSON.parse(claudeState); } catch (e) {}
        try {
            if (colorsJson) {
                const c = JSON.parse(colorsJson);
                if (Array.isArray(c) && c.length > 0) cfg.colors = c;
            }
        } catch (e) {}
        if (mood) cfg.mood = mood;
        if (city) cfg.city = city;
        process.stdout.write(JSON.stringify(cfg));
    " "${ACI_HANDLE:-}" "${ACI_SUB:-}" "${ACI_EMAIL:-}" "${ACI_ACCESS_TOKEN:-}" \
      "${ACI_CLAUDE_TOKEN:-}" "${ACI_GITHUB_PAT:-}" \
      "${ACI_CLAUDE_CREDS:-}" "${ACI_CLAUDE_STATE:-}" \
      "${ACI_HANDLE_COLORS_JSON:-}" "${ACI_MOOD:-}" "${ACI_CITY:-}"
}

# aci_build_inscription_json <out_path>
# Wraps the usbConfig JSON in an inscription envelope:
#   { "version": 1, "createdAt": "...", "createdBy": "@<handle>",
#     "usbConfig": <usb config json> }
aci_build_inscription_json() {
    local usb_json
    usb_json=$(aci_build_usb_config_json)
    node -e "
        const usb = JSON.parse(process.argv[1] || '{}');
        const env = {
            version: 1,
            createdAt: new Date().toISOString(),
            createdBy: process.argv[2] ? '@' + process.argv[2] : '',
            usbConfig: usb
        };
        process.stdout.write(JSON.stringify(env, null, 2));
    " "${usb_json}" "${ACI_HANDLE:-}"
}

# aci_build_anon_inscription_json
# Anonymous inscription — no tokens, no session.
aci_build_anon_inscription_json() {
    node -e "
        const env = {
            version: 1,
            createdAt: new Date().toISOString(),
            createdBy: 'anonymous',
            usbConfig: { handle: 'anonymous' }
        };
        process.stdout.write(JSON.stringify(env, null, 2));
    "
}

# aci_extract_usb_config_from_inscription <inscription_path>
# Reads an inscription file and prints its inner usbConfig JSON to stdout.
# Used by ac-os flash to consume a pre-built inscription.
aci_extract_usb_config_from_inscription() {
    local path="$1"
    [ -f "${path}" ] || return 1
    node -e "
        const d = JSON.parse(require('fs').readFileSync(process.argv[1], 'utf8'));
        if (!d.usbConfig) process.exit(2);
        process.stdout.write(JSON.stringify(d.usbConfig));
    " "${path}"
}
