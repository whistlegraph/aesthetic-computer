#!/bin/bash
# flash-mac.sh — macOS-native AC Native OS USB flasher (no Docker required).
#
# Mirrors the production scripts/flash-helper-runner.sh layout using only
# macOS CLI tools (diskutil, sgdisk, newfs_msdos, mount_msdos, cp, shasum).
#
# Two partitions, matching the Linux flow:
#   1. ACBOOT (FAT32, type "Microsoft Basic Data")
#      - kernel-direct boot: /EFI/BOOT/BOOTX64.EFI = full kernel
#      - /initramfs.cpio.gz at root
#      - /config.json
#      - Works on permissive PC firmware that scans non-ESP partitions for
#        the standard UEFI fallback path.
#
#   2. ACEFI (FAT32, type "EFI System Partition")
#      - splash.efi as /EFI/BOOT/BOOTX64.EFI (chains to LOADER.EFI)
#      - systemd-bootx64.efi as /EFI/BOOT/LOADER.EFI
#      - kernel as /EFI/BOOT/KERNEL.EFI
#      - /initramfs.cpio.gz at root
#      - /loader/entries/ac-native.conf with explicit cmdline
#      - Universal: works on any UEFI firmware that respects ESP type GUID.
#
# Skipped vs Linux helper:
#   - AC-MAC HFS+ partition for Intel Mac compatibility (TODO)
#
# Usage:
#   ./flash-mac.sh /dev/diskN [SRC_DIR]
#
# SRC_DIR defaults to /tmp/ac-os-pull (where ac-os pull stages downloads).
# Must contain `vmlinuz` and `initramfs.cpio.gz`.

set -euo pipefail

USB_DEV="${1:?usage: $0 /dev/diskN [SRC_DIR]}"
SRC_DIR="${2:-/tmp/ac-os-pull}"

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
REAL_REPO_ROOT="$(cd "${SCRIPT_DIR}/../../.." && pwd)"
NATIVE_DIR="${REPO_ROOT}/native"
[ -d "${NATIVE_DIR}/boot" ] || NATIVE_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

# --- step 1: ensure ~/.ac-token is fresh (run ac-login if stale) ---
# Done BEFORE the sudo exec so the OAuth browser dance happens as the
# actual user. If ac-login.mjs isn't findable we skip the refresh and
# leave the downstream bake to fail-soft with a warning.
if [ "$(id -u)" != "0" ]; then
    NEEDS_LOGIN=1
    if [ -f "${HOME}/.ac-token" ] && command -v node >/dev/null 2>&1; then
        NEEDS_LOGIN="$(node -e '
            try {
                const t = JSON.parse(require("fs").readFileSync(process.env.HOME+"/.ac-token", "utf8"));
                const rawExp = t.expires_at || 0;
                const expMs = rawExp > 10_000_000_000 ? rawExp : rawExp * 1000;
                // Consider stale if expired or within 60s of expiring
                process.stdout.write((!expMs || Date.now() >= expMs - 60000) ? "1" : "0");
            } catch { process.stdout.write("1"); }
        ')"
    fi
    if [ "${NEEDS_LOGIN}" = "1" ]; then
        AC_LOGIN=""
        for p in "${REAL_REPO_ROOT}/tezos/ac-login.mjs" \
                 "${HOME}/aesthetic-computer/tezos/ac-login.mjs"; do
            [ -f "${p}" ] && AC_LOGIN="${p}" && break
        done
        if [ -n "${AC_LOGIN}" ] && command -v node >/dev/null 2>&1; then
            echo "[flash-mac] ~/.ac-token is stale — running ac-login to refresh…"
            echo "[flash-mac]   script: ${AC_LOGIN}"
            node "${AC_LOGIN}" || {
                echo "[flash-mac] ac-login failed (non-fatal; proceeding without Claude bake)" >&2
            }
        else
            echo "[flash-mac] WARN: ac-login.mjs not found — Claude creds won't be baked" >&2
            echo "[flash-mac]       searched: ${REAL_REPO_ROOT}/tezos/ac-login.mjs" >&2
        fi
    fi
fi

# --- step 2: re-exec under sudo ---
# Needs root for diskutil/sgdisk/dd/newfs_msdos/mount_msdos. sudoers.d/
# ac-flash-mac whitelists this exact path NOPASSWD.
if [ "$(id -u)" != "0" ]; then
    exec sudo --preserve-env=PATH "$0" "$@"
fi

KERNEL="${SRC_DIR}/vmlinuz"
INITRAMFS="${SRC_DIR}/initramfs.cpio.gz"
SPLASH_EFI="${NATIVE_DIR}/bootloader/splash.efi"
SDBOOT_EFI="${NATIVE_DIR}/boot/systemd-bootx64.efi"

log() { echo "[flash-mac] $*"; }
err() { echo "[flash-mac] $*" >&2; }
die() { err "$*"; exit 1; }

# --- preflight ---
[ "$(uname)" = "Darwin" ]    || die "macOS only — use ac-os flash on Linux."
command -v sgdisk >/dev/null || die "sgdisk required: brew install gptfdisk"
[ -f "${KERNEL}" ]           || die "Missing kernel: ${KERNEL}"
[ -f "${INITRAMFS}" ]        || die "Missing initramfs: ${INITRAMFS}"
[ -f "${SPLASH_EFI}" ]       || die "Missing splash bootloader: ${SPLASH_EFI}"
[ -f "${SDBOOT_EFI}" ]       || die "Missing systemd-boot: ${SDBOOT_EFI}"

# --- credential gathering ----------------------------------------------
# Two paths:
#   1. AC_INSCRIPTION_FILE points to a valid inscription.json
#      (built by ac-inscribe ahead of flash). Read identity + tokens
#      from the artifact — no live API calls during the USB write.
#      This is the unified path matching the Linux flash flow.
#   2. No inscription artifact: fall back to the legacy live-fetch path
#      (read ~/.ac-token, hit /api/claude-token). Preserved for back-
#      compat and for AC_ANON=1.
INSCRIPTION_FILE="${AC_INSCRIPTION_FILE:-/tmp/ac-os-pull/inscription.json}"
USER_HANDLE=""; USER_SUB=""; USER_EMAIL=""
AC_ACCESS_TOKEN=""; AC_TOKEN_EXPIRED=0
CLAUDE_TOKEN=""; GITHUB_PAT=""
INSCRIBED_CLAUDE_STATE=""

if [ -f "${INSCRIPTION_FILE}" ] && command -v node >/dev/null 2>&1; then
    log "Reading inscription: ${INSCRIPTION_FILE}"
    eval "$(node -e '
        try {
            const d = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
            const c = d.usbConfig || {};
            const out = (k, v) => process.stdout.write(`${k}=${JSON.stringify(v || "")}\n`);
            out("USER_HANDLE",  c.handle);
            out("USER_SUB",     c.sub);
            out("USER_EMAIL",   c.email);
            out("CLAUDE_TOKEN", c.claudeToken);
            out("GITHUB_PAT",   c.githubPat);
            // Embed claudeState as a JSON string for direct file write below.
            out("INSCRIBED_CLAUDE_STATE", c.claudeState ? JSON.stringify(c.claudeState) : "");
        } catch (e) {
            process.stderr.write("inscription parse failed: " + e.message + "\n");
        }
    ' "${INSCRIPTION_FILE}" 2>/dev/null)"
    if [ -n "${USER_HANDLE}" ]; then
        log "  inscribed: @${USER_HANDLE}"
        [ -n "${CLAUDE_TOKEN}" ] && log "  claude token: ${#CLAUDE_TOKEN} bytes" || \
            log "  claude token: none in inscription"
        [ -n "${GITHUB_PAT}" ] && log "  github pat:   ${#GITHUB_PAT} bytes" || \
            log "  github pat:   none in inscription"
    elif [ "$(node -e '
        try {
            const d=JSON.parse(require("fs").readFileSync(process.argv[1],"utf8"));
            process.stdout.write((d.usbConfig||{}).handle === "anonymous" ? "1" : "0");
        } catch { process.stdout.write("0"); }
    ' "${INSCRIPTION_FILE}" 2>/dev/null)" = "1" ]; then
        log "  inscribed: anonymous (no tokens)"
    fi
else
    # --- legacy: read ac-login token (~/.ac-token) for handle/sub/email ---
    # When invoked as root via sudo, $HOME points at /var/root. Use SUDO_USER's
    # home so we read the actual operator's token, not the empty root one.
    TOKEN_HOME="${HOME}"
    [ -n "${SUDO_USER:-}" ] && TOKEN_HOME="$(eval echo ~${SUDO_USER})"
    TOKEN_FILE="${TOKEN_HOME}/.ac-token"

    if [ -f "${TOKEN_FILE}" ] && command -v node >/dev/null 2>&1; then
        eval "$(node -e '
            const t = JSON.parse(require("fs").readFileSync(process.argv[1], "utf8"));
            let h = t.user?.handle || t.user?.name || "";
            if (h.startsWith("@")) h = h.slice(1);
            const now = Date.now();
            const rawExp = t.expires_at || 0;
            const expMs  = rawExp > 10_000_000_000 ? rawExp : rawExp * 1000;
            const fresh  = expMs && now < expMs;
            const out = (k, v) => process.stdout.write(`${k}=${JSON.stringify(v || "")}\n`);
            out("USER_HANDLE",     h);
            out("USER_SUB",        t.user?.sub);
            out("USER_EMAIL",      t.user?.email);
            out("AC_ACCESS_TOKEN", fresh ? (t.access_token || "") : "");
            process.stdout.write(`AC_TOKEN_EXPIRED=${fresh ? 0 : 1}\n`);
        ' "${TOKEN_FILE}" 2>/dev/null)"
        [ -n "${USER_HANDLE}" ] && log "Authenticated as @${USER_HANDLE} (legacy fetch)"
    fi
    [ -z "${USER_HANDLE}${USER_SUB}${USER_EMAIL}" ] && \
        log "No ~/.ac-token (run \`ac-login\` or \`ac-inscribe\` first to bake credentials in)"

    # /api/claude-token returns { handle, token, githubPat } from @handles.
    # `token` is the year-long Claude Code OAuth bearer (sk-ant-...) — pty.c
    # reads /claude-token at spawn time and sets CLAUDE_CODE_OAUTH_TOKEN so
    # `claude` launches without interactive login. `githubPat` lets on-device
    # git push to the GitHub mirror without SSH.
    if [ -n "${AC_ACCESS_TOKEN}" ]; then
        log "Fetching Claude token + GitHub PAT from MongoDB…"
        CT_RESP="$(curl -fsS -H "Authorization: Bearer ${AC_ACCESS_TOKEN}" \
            "https://aesthetic.computer/api/claude-token" 2>/dev/null || echo "")"
        if [ -n "${CT_RESP}" ]; then
            eval "$(node -e '
                try {
                    const r = JSON.parse(process.argv[1]);
                    const out = (k, v) => process.stdout.write(`${k}=${JSON.stringify(v || "")}\n`);
                    out("CLAUDE_TOKEN", r.token);
                    out("GITHUB_PAT",   r.githubPat);
                } catch {}
            ' "${CT_RESP}" 2>/dev/null)"
        fi
        if [ -n "${CLAUDE_TOKEN}" ]; then
            log "  claude token: ${#CLAUDE_TOKEN} bytes"
        else
            log "  claude token: none stored in MongoDB (POST to /api/claude-token to save)"
        fi
        [ -n "${GITHUB_PAT}" ] && log "  github pat:   ${#GITHUB_PAT} bytes"
    elif [ "${AC_TOKEN_EXPIRED}" = "1" ]; then
        log "  creds fetch: skipped (~/.ac-token expired — run \`ac-login\` or \`ac-inscribe\` to refresh)"
    fi
fi

# --- locate Tangled SSH identity for knot.aesthetic.computer pushes ---
# Mirrors ac-os Linux flash: prefer the IdentityFile that `ssh -G knot...`
# resolves to (handles ~/.ssh/config overrides), fall back to ~/.ssh/tangled.
TANGLED_KEY=""
if command -v ssh >/dev/null 2>&1; then
    TANGLED_KEY="$(ssh -G knot.aesthetic.computer 2>/dev/null \
                   | awk '/^identityfile / {print $2; exit}')"
    case "${TANGLED_KEY}" in
        "~/"*) TANGLED_KEY="${HOME}/${TANGLED_KEY#~/}" ;;
    esac
fi
[ -z "${TANGLED_KEY}" ] && [ -f "${HOME}/.ssh/tangled" ] && TANGLED_KEY="${HOME}/.ssh/tangled"
[ -n "${TANGLED_KEY}" ] && [ ! -f "${TANGLED_KEY}" ] && TANGLED_KEY=""

# --- locate device-claude.md and SCORE.md for on-device context ---
DEVICE_CLAUDE_MD=""
[ -f "${NATIVE_DIR}/device-claude.md" ] && DEVICE_CLAUDE_MD="${NATIVE_DIR}/device-claude.md"
DEVICE_SCORE_MD=""
[ -f "${NATIVE_DIR}/SCORE.md" ] && DEVICE_SCORE_MD="${NATIVE_DIR}/SCORE.md"

# --- bake creds into initramfs via concatenated cpio archive ---
# The Linux kernel's unpack_to_rootfs() accepts multiple concatenated
# gzipped cpio archives in the initrd stream (same trick intel-ucode
# uses). We build a tiny supplementary archive containing the baked
# files and append it to the end of initramfs.cpio.gz.
# Files baked (matches ac-os Linux layout):
#   /claude-token            plain bearer — pty.c sets CLAUDE_CODE_OAUTH_TOKEN
#   /claude-state.json       init copies to /tmp/.claude.json (skips CC onboarding)
#   /github-pat              plain bearer — pty.c sets GITHUB_TOKEN
#   /tangled-key             SSH private key — init copies to /tmp/.ssh/tangled
#   /tangled-known-hosts     init copies to /tmp/.ssh/known_hosts
#   /tangled-ssh-config      init copies to /tmp/.ssh/config; pty.c sets GIT_SSH_COMMAND
#   /device-claude.md        on-device CLAUDE.md (init copies to /tmp/ac/CLAUDE.md)
#   /device-score.md         on-device SCORE.md (init symlinks to /tmp/ac/SCORE.md)
if [ -n "${CLAUDE_TOKEN}${GITHUB_PAT}${TANGLED_KEY}${DEVICE_CLAUDE_MD}${DEVICE_SCORE_MD}" ]; then
    BAKE_DIR="$(mktemp -d /tmp/ac-bake.XXXXXX)"
    BAKED_INITRAMFS="/tmp/ac-initramfs-baked.$$.cpio.gz"
    ORIG_INITRD_SIZE="$(stat -f%z "${INITRAMFS}")"
    BAKE_LIST=""
    if [ -n "${CLAUDE_TOKEN}" ]; then
        printf %s "${CLAUDE_TOKEN}" > "${BAKE_DIR}/claude-token"
        chmod 600 "${BAKE_DIR}/claude-token"
        # Prefer the inscribed claudeState (read from the operator's
        # actual ~/.claude.json by ac-inscribe — preserves oauthAccount
        # fields like organizationName/accountUuid). Fall back to the
        # minimal stub for legacy / no-inscription flashes.
        if [ -n "${INSCRIBED_CLAUDE_STATE}" ]; then
            printf '%s\n' "${INSCRIBED_CLAUDE_STATE}" > "${BAKE_DIR}/claude-state.json"
        else
            cat > "${BAKE_DIR}/claude-state.json" <<STATE
{"oauthAccount":{"emailAddress":"${USER_EMAIL}","organizationName":"","accountUuid":""},"hasCompletedOnboarding":true,"installMethod":"manual","numStartups":1,"autoUpdates":false,"autoUpdatesProtectedForNative":true}
STATE
        fi
        BAKE_LIST="${BAKE_LIST}claude-token
claude-state.json
"
    fi
    if [ -n "${GITHUB_PAT}" ]; then
        printf %s "${GITHUB_PAT}" > "${BAKE_DIR}/github-pat"
        chmod 600 "${BAKE_DIR}/github-pat"
        BAKE_LIST="${BAKE_LIST}github-pat
"
    fi
    if [ -n "${TANGLED_KEY}" ]; then
        cp "${TANGLED_KEY}" "${BAKE_DIR}/tangled-key"
        chmod 600 "${BAKE_DIR}/tangled-key"
        # Bring along known_hosts so first-push StrictHostKeyChecking has the
        # entry; otherwise let ssh fall back to accept-new from the config.
        if [ -f "${HOME}/.ssh/known_hosts" ]; then
            cp "${HOME}/.ssh/known_hosts" "${BAKE_DIR}/tangled-known-hosts"
            chmod 600 "${BAKE_DIR}/tangled-known-hosts"
        elif command -v ssh-keyscan >/dev/null 2>&1; then
            ssh-keyscan -H knot.aesthetic.computer 2>/dev/null \
                > "${BAKE_DIR}/tangled-known-hosts" || true
            [ -s "${BAKE_DIR}/tangled-known-hosts" ] \
                && chmod 600 "${BAKE_DIR}/tangled-known-hosts" \
                || rm -f "${BAKE_DIR}/tangled-known-hosts"
        fi
        cat > "${BAKE_DIR}/tangled-ssh-config" <<'SSHCFG'
Host knot.aesthetic.computer
    HostName knot.aesthetic.computer
    User git
    IdentityFile ~/.ssh/tangled
    IdentitiesOnly yes
    BatchMode yes
    StrictHostKeyChecking accept-new
    UserKnownHostsFile ~/.ssh/known_hosts
SSHCFG
        chmod 600 "${BAKE_DIR}/tangled-ssh-config"
        BAKE_LIST="${BAKE_LIST}tangled-key
tangled-ssh-config
"
        [ -f "${BAKE_DIR}/tangled-known-hosts" ] && BAKE_LIST="${BAKE_LIST}tangled-known-hosts
"
        log "  tangled ssh: baked (${TANGLED_KEY})"
    else
        log "  tangled ssh: not found (skipping knot push setup)"
    fi
    if [ -n "${DEVICE_CLAUDE_MD}" ]; then
        cp "${DEVICE_CLAUDE_MD}" "${BAKE_DIR}/device-claude.md"
        BAKE_LIST="${BAKE_LIST}device-claude.md
"
        log "  CLAUDE.md: baked"
    fi
    if [ -n "${DEVICE_SCORE_MD}" ]; then
        cp "${DEVICE_SCORE_MD}" "${BAKE_DIR}/device-score.md"
        BAKE_LIST="${BAKE_LIST}device-score.md
"
        log "  SCORE.md: baked"
    fi
    cp "${INITRAMFS}" "${BAKED_INITRAMFS}"
    ( cd "${BAKE_DIR}" && printf '%s' "${BAKE_LIST}" \
        | cpio -o -H newc 2>/dev/null ) \
        | gzip -9 >> "${BAKED_INITRAMFS}" \
        || die "Failed to append creds cpio to initramfs"
    rm -rf "${BAKE_DIR}"
    INITRAMFS="${BAKED_INITRAMFS}"
    NEW_INITRD_SIZE="$(stat -f%z "${INITRAMFS}")"
    log "  baked initramfs: ${NEW_INITRD_SIZE} bytes (+$(( NEW_INITRD_SIZE - ORIG_INITRD_SIZE )) bytes for creds cpio)"
fi

# --- preserve existing wifi_creds.json from target USB before we wipe it ---
# Linux `ac-os flash` does this via ac_media_merge_wifi_creds: read the
# previously-flashed USB for user-added networks, then merge with the
# built-in preset list. We mirror that behavior so a reflash keeps your
# kitchen/studio/friend's-apartment wifi without re-typing.
PRESERVE_WIFI=""
for mnt in /Volumes/ACBOOT /Volumes/ACEFI; do
    if [ -f "${mnt}/wifi_creds.json" ]; then
        # Copy while readable — the partition is about to be unmounted.
        PRESERVE_WIFI="/tmp/ac-wifi-preserve.$$.json"
        cp "${mnt}/wifi_creds.json" "${PRESERVE_WIFI}" 2>/dev/null && \
            log "Preserving wifi_creds.json from ${mnt}" && break
    fi
done
trap "rm -f '${PRESERVE_WIFI}' '${BAKED_INITRAMFS:-}' 2>/dev/null" EXIT

# --- hardcoded preset networks (kept in sync with media-layout.sh + src/wifi.c) ---
WIFI_PRESETS_JSON='[
  {"ssid":"aesthetic.computer","pass":"aesthetic.computer"},
  {"ssid":"ATT2AWTpcr","pass":"t84q%7%g2h8u"},
  {"ssid":"ATTcifXGXi","pass":"dvt%mnk8h6z"},
  {"ssid":"GettyLink","pass":""},
  {"ssid":"Tondo_Guest","pass":"California"},
  {"ssid":"Eightfold Coffee","pass":"wecloseat430"},
  {"ssid":"TP-Link_F12F","pass":"32139297"},
  {"ssid":"Plot","pass":"blanketfort"}
]'

INFO=$(diskutil info "${USB_DEV}" 2>/dev/null) || die "diskutil info failed for ${USB_DEV}"
echo "${INFO}" | grep -q "Removable Media:.*Removable\|Device Location:.*External" \
    || die "${USB_DEV} is not removable/external. Aborting."
DEV_NAME=$(echo "${INFO}" | awk -F': +' '/Device \/ Media Name/{print $2}' | head -1)
DEV_SIZE=$(echo "${INFO}" | awk -F': +' '/Disk Size/{print $2}' | head -1)
DEV_BYTES=$(echo "${INFO}" | awk -F': +' '/Disk Size:/ {print $2}' | grep -oE '\([0-9]+ Bytes\)' | head -1 | tr -dc 0-9)

# --- size budgeting (matches Linux helper math) ---
KERNEL_BYTES=$(stat -f%z "${KERNEL}")
INITRD_BYTES=$(stat -f%z "${INITRAMFS}")
KERNEL_SHA=$(shasum -a 256 "${KERNEL}"    | awk '{print $1}')
INITRD_SHA=$(shasum -a 256 "${INITRAMFS}" | awk '{print $1}')

mb_round_up() { echo $(( ($1 + 1048575) / 1048576 )); }
DISK_MB=$(( DEV_BYTES / 1048576 ))
STAGE_MB=$(( $(mb_round_up "${KERNEL_BYTES}") + $(mb_round_up "${INITRD_BYTES}") + 8 ))
# Size the ESP (ACEFI — the partition the device boots from and OTAs into) to
# hold TWO full copies of the boot tree plus margin: the on-device OTA writes
# the new kernel+initramfs ALONGSIDE the running ones before swapping, so a
# +96 MB headroom (one copy) made on-device updates fail with "not enough ESP
# space". 2× the staged size + 256 MB gives the OTA room to double-buffer.
# ACBOOT (MAIN) absorbs the difference out of its ~29 GB — negligible.
EFI_MB=$(( STAGE_MB * 2 + 256 ))
MAIN_MB=$(( DISK_MB - EFI_MB - 64 ))   # 64 MB GPT + alignment headroom

[ "${MAIN_MB}" -ge $(( STAGE_MB + 64 )) ] \
    || die "USB too small (${DISK_MB} MB) for hybrid layout."

echo
log "Target: ${USB_DEV} — ${DEV_NAME} — ${DEV_SIZE}"
log "Kernel:    ${KERNEL_BYTES} bytes  ${KERNEL_SHA:0:16}…"
log "Initramfs: ${INITRD_BYTES} bytes  ${INITRD_SHA:0:16}…"
FREE_MB=$(( DISK_MB - MAIN_MB - EFI_MB - 64 ))
log "Layout:    ACBOOT=${MAIN_MB}MB  ACEFI=${EFI_MB}MB  free=${FREE_MB}MB"
echo
if [ -n "${AC_FLASH_YES:-}" ] || [ ! -t 0 ]; then
    log "Auto-confirming wipe (AC_FLASH_YES set or stdin not a TTY)"
    CONFIRM=YES
else
    read -r -p "Type 'YES' to ERASE ${USB_DEV} and write AC Native OS: " CONFIRM
fi
[ "${CONFIRM}" = "YES" ] || die "Aborted."

# --- wipe + repartition ---
log "Unmounting…"
diskutil unmountDisk force "${USB_DEV}" >/dev/null

log "Zapping GPT + clearing first 16 MiB…"
sgdisk --zap-all "${USB_DEV}" >/dev/null
dd if=/dev/zero of="${USB_DEV}" bs=1m count=16 status=none

log "Creating GPT layout (ACBOOT + ACEFI)…"
sgdisk \
    --new=1:0:+${MAIN_MB}M --typecode=1:0700 --change-name=1:ACBOOT \
    --new=2:0:0           --typecode=2:ef00 --change-name=2:ACEFI \
    "${USB_DEV}" >/dev/null

# Force macOS to re-read the partition table after sgdisk wrote it. The
# kernel caches the old layout until we explicitly notify it; without this,
# the s1/s2 nodes either don't exist yet or still point at the pre-zap
# partitions and newfs_msdos errors with "No such file or directory".
diskutil unmountDisk force "${USB_DEV}" >/dev/null 2>&1 || true
diskutil list "${USB_DEV}" >/dev/null 2>&1 || true

P1="${USB_DEV}s1"
P2="${USB_DEV}s2"
RAW1="/dev/r$(basename "${P1}")"
RAW2="/dev/r$(basename "${P2}")"

# Wait up to 10s for both partition nodes to materialize.
for i in $(seq 1 20); do
    [ -e "${P1}" ] && [ -e "${P2}" ] && break
    sleep 0.5
    diskutil list "${USB_DEV}" >/dev/null 2>&1 || true
done
[ -e "${P1}" ] || die "Partition ${P1} did not appear after sgdisk + reread."
[ -e "${P2}" ] || die "Partition ${P2} did not appear after sgdisk + reread."

log "Formatting FAT32 partitions…"
newfs_msdos -F 32 -v ACBOOT "${RAW1}" >/dev/null
newfs_msdos -F 32 -v ACEFI  "${RAW2}" >/dev/null

# --- mount ---
M1=$(mktemp -d /tmp/ac-main.XXXXXX)
M2=$(mktemp -d /tmp/ac-efi.XXXXXX)
trap "umount '${M1}' 2>/dev/null; umount '${M2}' 2>/dev/null; rmdir '${M1}' '${M2}' 2>/dev/null; rm -f '${PRESERVE_WIFI}' '${BAKED_INITRAMFS:-}' 2>/dev/null; true" EXIT

log "Mounting partitions…"
mount_msdos "${P1}" "${M1}"
mount_msdos "${P2}" "${M2}"

# --- layout ACBOOT (kernel-direct + config) ---
log "Writing ACBOOT (kernel-direct boot tree)…"
mkdir -p "${M1}/EFI/BOOT"
cp "${KERNEL}"   "${M1}/EFI/BOOT/BOOTX64.EFI"
cp "${INITRAMFS}" "${M1}/initramfs.cpio.gz"
# Boot piece: notepat by default. Override per-flash with AC_BOOT_PIECE
# (e.g. AC_BOOT_PIECE=babypat flash-mac.sh ...). Kernel resolves the
# name to /pieces/<piece>.mjs at boot — see ac-native.c:3853.
BOOT_PIECE="${AC_BOOT_PIECE:-notepat}"

# Write a device config.json. Base identity fields come from the shell vars
# (set from the inscription OR the legacy API path); the boot-personalization
# fields (city / colors / mood) are pulled straight from the inscription's
# usbConfig so `ac-inscribe --city/--mood` + handle-colors actually reach the
# device — the old hardcoded printf silently dropped them.
write_device_config() {  # $1=dest  $2=udp(1=include udpMidiBroadcast)
    node -e '
        const fs = require("fs");
        const [dest, handle, piece, sub, email, udp, insc] = process.argv.slice(1);
        const cfg = { handle, piece, sub, email };
        if (udp === "1") cfg.udpMidiBroadcast = true;
        try {
            const c = (JSON.parse(fs.readFileSync(insc, "utf8")).usbConfig) || {};
            if (c.city) cfg.city = c.city;
            if (Array.isArray(c.colors) && c.colors.length) cfg.colors = c.colors;
            if (c.mood) cfg.mood = c.mood;
        } catch (e) { /* no inscription (anon/legacy) — base fields only */ }
        fs.writeFileSync(dest, JSON.stringify(cfg) + "\n");
    ' "$1" "${USER_HANDLE}" "${BOOT_PIECE}" "${USER_SUB}" "${USER_EMAIL}" "$2" "${INSCRIPTION_FILE}"
}
write_device_config "${M1}/config.json" 1
log "  config.json: $(cat "${M1}/config.json")"

# Build merged wifi_creds.json (presets + preserved + optional override)
# once, reuse for both partitions.
WIFI_MERGED="/tmp/ac-wifi-merged.$$.json"
if [ -f "${SRC_DIR}/wifi_creds.json" ]; then
    # Caller-provided override wins as source-of-truth.
    cp "${SRC_DIR}/wifi_creds.json" "${WIFI_MERGED}"
    log "Using wifi_creds.json from ${SRC_DIR}"
else
    # Presets + preserved merge. Python dedupes by SSID keeping the LAST
    # occurrence's password (preserved > presets, so user-saved networks
    # override the hardcoded entries if both reference the same SSID).
    if [ -n "${PRESERVE_WIFI}" ] && [ -f "${PRESERVE_WIFI}" ]; then
        python3 -c "
import json, sys
presets = json.loads(sys.argv[1])
preserved = json.load(open(sys.argv[2]))
order = []
merged = {}
for entry in (presets + preserved):
    if not isinstance(entry, dict): continue
    ssid = entry.get('ssid')
    if not ssid: continue
    if ssid not in merged: order.append(ssid)
    merged[ssid] = {'ssid': ssid, 'pass': str(entry.get('pass', ''))}
with open(sys.argv[3], 'w') as f:
    json.dump([merged[s] for s in order], f, indent=2)
" "${WIFI_PRESETS_JSON}" "${PRESERVE_WIFI}" "${WIFI_MERGED}" \
        && log "Merged $(python3 -c 'import json,sys; print(len(json.load(open(sys.argv[1]))))' "${WIFI_MERGED}") wifi networks (presets + preserved)"
    else
        printf '%s\n' "${WIFI_PRESETS_JSON}" > "${WIFI_MERGED}"
        log "Wrote 6 preset wifi networks (no previous USB to preserve from)"
    fi
fi
cp "${WIFI_MERGED}" "${M1}/wifi_creds.json"

# --- layout ACEFI (systemd-boot universal) ---
log "Writing ACEFI (splash → systemd-boot → kernel)…"
mkdir -p "${M2}/EFI/BOOT" "${M2}/loader/entries"
cp "${SPLASH_EFI}" "${M2}/EFI/BOOT/BOOTX64.EFI"
cp "${SDBOOT_EFI}" "${M2}/EFI/BOOT/LOADER.EFI"
cp "${KERNEL}"     "${M2}/EFI/BOOT/KERNEL.EFI"
cp "${INITRAMFS}"   "${M2}/initramfs.cpio.gz"
tee "${M2}/loader/loader.conf" >/dev/null <<'EOF'
default ac-native.conf
timeout 0
EOF
tee "${M2}/loader/entries/ac-native.conf" >/dev/null <<'EOF'
title AC Native OS
linux /EFI/BOOT/KERNEL.EFI
initrd /initramfs.cpio.gz
options console=tty0 quiet loglevel=3 vt.global_cursor_default=0 init=/init nomodeset efi=noruntime
EOF
write_device_config "${M2}/config.json" 0
[ -f "${WIFI_MERGED}" ] && cp "${WIFI_MERGED}" "${M2}/wifi_creds.json"

# --- verify (sha256 round-trip on every kernel + initramfs copy) ---
log "Verifying integrity…"
verify() {
    local label="$1" path="$2" expected="$3"
    local got
    got=$(shasum -a 256 "${path}" | awk '{print $1}')
    [ "${got}" = "${expected}" ] || die "${label} sha mismatch (${got} != ${expected})"
    log "  ✓ ${label}  ${got:0:16}…"
}
verify "ACBOOT/EFI/BOOT/BOOTX64.EFI" "${M1}/EFI/BOOT/BOOTX64.EFI"  "${KERNEL_SHA}"
verify "ACBOOT/initramfs.cpio.gz"   "${M1}/initramfs.cpio.gz"      "${INITRD_SHA}"
verify "ACEFI/EFI/BOOT/KERNEL.EFI"  "${M2}/EFI/BOOT/KERNEL.EFI"    "${KERNEL_SHA}"
verify "ACEFI/initramfs.cpio.gz"    "${M2}/initramfs.cpio.gz"      "${INITRD_SHA}"

# --- finalize ---
sync
log "Unmounting + ejecting…"
diskutil unmountDisk force "${USB_DEV}" >/dev/null
diskutil eject "${USB_DEV}" >/dev/null
trap - EXIT
rmdir "${M1}" "${M2}" 2>/dev/null || true

log "Done. USB has both kernel-direct (ACBOOT) + systemd-boot (ACEFI) layouts."
log "Plug into target hardware and boot — UEFI firmware should pick ACEFI (real ESP)."
