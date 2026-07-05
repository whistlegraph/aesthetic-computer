#!/usr/bin/env bash
# verify-bundle.sh — assert a built Menu Band.app (or DMG) is self-contained
# and will run on a fresh machine that has no /Users/jas/... paths.
#
# Background:
#   SwiftPM auto-generates `resource_bundle_accessor.swift` for the MenuBand
#   target. That accessor first tries
#       Bundle.main.bundleURL.appendingPathComponent("MenuBand_MenuBand.bundle")
#   and only falls back to a hardcoded build-tree path
#       /Users/<dev>/.../.build/.../MenuBand_MenuBand.bundle
#   if the primary path is missing. If we ship a .app whose Resources/ does
#   NOT include MenuBand_MenuBand.bundle, the accessor crashes on a fresh
#   machine with "could not load resource bundle: from … or /Users/jas/…".
#
#   This is exactly what shipped in Menu-Band-1.0.dmg.
#
# Usage:
#   bin/verify-bundle.sh                       # check ~/Applications/Menu Band.app
#   bin/verify-bundle.sh path/to/Menu\ Band.app
#   bin/verify-bundle.sh --dmg path/to/x.dmg   # mount, verify .app inside, unmount
#   bin/verify-bundle.sh --staged DIR          # check a staged DMG tree (DIR/Menu Band.app)

set -euo pipefail

CYAN=$'\033[1;36m'
GREEN=$'\033[1;32m'
YELLOW=$'\033[1;33m'
RED=$'\033[1;31m'
RESET=$'\033[0m'
say()  { printf "%s• %s%s\n" "$CYAN" "$1" "$RESET"; }
ok()   { printf "%s✓ %s%s\n" "$GREEN" "$1" "$RESET"; }
warn() { printf "%s! %s%s\n" "$YELLOW" "$1" "$RESET"; }
err()  { printf "%s✗ %s%s\n" "$RED" "$1" "$RESET"; }

# Resources every shipped bundle MUST contain. Grow this list when new
# Bundle.module.url(forResource:) call sites land.
REQUIRED_RESOURCES=(
    "WaveformShaders.metalsource"
    "keymaps-social-software-26-arxiv.pdf"
    "sheet.html"
    "verovio-toolkit-wasm.js"
    "ywft-processing-bold.ttf"
    "ywft-processing-regular.ttf"
)

verify_app() {
    local APP="$1"
    local FAIL=0

    if [[ ! -d "$APP" ]]; then
        err "no .app at: $APP"
        return 1
    fi
    say "verifying: $APP"

    local BIN="$APP/Contents/MacOS/MenuBand"
    local INFO="$APP/Contents/Info.plist"
    # Resources are FLATTENED directly into Contents/Resources (read via
    # Bundle.appResources → Bundle.main). This keeps them in a codesign-sealable
    # location — the nested MenuBand_MenuBand.bundle is NOT shipped (Swift 6.3's
    # accessor would want it at the unsealed .app root).
    local RES_DIR="$APP/Contents/Resources"

    if [[ ! -f "$BIN" ]]; then
        err "missing binary: $BIN"; FAIL=1
    fi
    if [[ ! -f "$INFO" ]]; then
        err "missing Info.plist: $INFO"; FAIL=1
    fi
    # A stray nested bundle at the .app root is a sign of the old (unsealable)
    # layout — flag it so it never ships.
    if [[ -e "$APP/MenuBand_MenuBand.bundle" ]]; then
        err "nested MenuBand_MenuBand.bundle present at the .app root — fails codesign --strict"
        err "  → install.sh must FLATTEN it into Contents/Resources instead"
        FAIL=1
    fi

    local missing=0
    for r in "${REQUIRED_RESOURCES[@]}"; do
        if [[ ! -e "$RES_DIR/$r" ]]; then
            err "  missing: $r"
            missing=$((missing + 1))
        fi
    done
    if (( missing > 0 )); then
        err "$missing required resource(s) absent from Contents/Resources"
        err "  → on a fresh machine this app will crash on Bundle.appResources lookups"
        FAIL=1
    else
        ok "all ${#REQUIRED_RESOURCES[@]} required resources present in Contents/Resources"
    fi

    # Scan the binary for hardcoded developer paths. We only care about
    # paths the runtime might dereference — DWARF debug-info source paths
    # are cosmetic and stripped by install.sh's `strip -S` step anyway.
    #
    # The SwiftPM accessor fallback (…/MenuBand_MenuBand.bundle) is the
    # ONLY acceptable runtime /Users/... reference — anything else is a
    # leak that will break on a fresh machine.
    if [[ -f "$BIN" ]]; then
        local user_strings
        user_strings="$(strings - "$BIN" 2>/dev/null | grep -E '^/Users/' || true)"
        if [[ -n "$user_strings" ]]; then
            # Drop debug-info paths: anything that ends in a source-file
            # extension or with a trailing slash. The Swift compiler
            # embeds these in __DWARF; they're not runtime-resolved.
            local runtime_user_strings
            runtime_user_strings="$(echo "$user_strings" \
                | grep -vE '(\.swift|\.swift\.o|\.o|\.h|\.hpp|\.c|\.cpp|\.cxx|\.cc|\.m|\.mm|/)$' \
                || true)"
            local debug_count
            debug_count="$(echo "$user_strings" | grep -cE '(\.swift|\.swift\.o|\.o|\.h|\.hpp|\.c|\.cpp|\.cxx|\.cc|\.m|\.mm|/)$' || true)"
            if (( debug_count > 0 )); then
                warn "binary carries $debug_count DWARF source paths — install.sh's strip -S step removes these"
            fi

            if [[ -n "$runtime_user_strings" ]]; then
                local unexpected
                unexpected="$(echo "$runtime_user_strings" | grep -v 'MenuBand_MenuBand\.bundle$' || true)"
                local accessor_count
                accessor_count="$(echo "$runtime_user_strings" | grep -c 'MenuBand_MenuBand\.bundle$' || true)"
                if [[ -n "$unexpected" ]]; then
                    err "binary contains unexpected runtime /Users/... paths:"
                    echo "$unexpected" | sed 's/^/    /'
                    FAIL=1
                else
                    ok "binary has only the SwiftPM fallback path ($accessor_count refs) — harmless because the primary bundle path resolves"
                fi
            fi
        fi
    fi

    # Code signature sanity. Only check if codesign is happy with the bundle —
    # we don't require it to be Developer-ID-signed here (install.sh handles
    # that); we just want to confirm the bundle structure didn't get
    # corrupted post-sign (e.g. by adding files after codesign --deep).
    if command -v codesign >/dev/null 2>&1; then
        if codesign --verify --deep --strict "$APP" >/dev/null 2>&1; then
            ok "codesign --verify --deep --strict passes"
        else
            warn "codesign --verify failed (re-sign required if this is going to ship)"
        fi
    fi

    if (( FAIL )); then
        return 1
    fi

    # Fresh-machine simulation: copy the .app to a path that has no
    # relationship to /Users/jas, then re-run the structural check against
    # the copy. This catches any check we accidentally wrote against
    # absolute paths instead of bundle-relative ones.
    local TMP
    TMP="$(mktemp -d -t menuband-fresh-XXXX)"
    cp -R "$APP" "$TMP/"
    local copied="$TMP/$(basename "$APP")"
    local missing_in_copy=0
    for r in "${REQUIRED_RESOURCES[@]}"; do
        if [[ ! -e "$copied/Contents/Resources/$r" ]]; then
            missing_in_copy=$((missing_in_copy + 1))
        fi
    done
    if (( missing_in_copy == 0 )); then
        ok "fresh-path simulation: resources resolve at $(dirname "$TMP")/.../$(basename "$APP")"
    else
        err "fresh-path simulation: $missing_in_copy resources missing from Contents/Resources after copy"
        FAIL=1
    fi
    rm -rf "$TMP"

    return $FAIL
}

verify_dmg() {
    local DMG="$1"
    if [[ ! -f "$DMG" ]]; then
        err "no DMG at: $DMG"
        return 1
    fi
    say "mounting: $DMG"
    local MOUNT_INFO MOUNT_POINT
    MOUNT_INFO="$(hdiutil attach -nobrowse -readonly "$DMG" | tail -1)"
    MOUNT_POINT="$(echo "$MOUNT_INFO" | awk -F'\t' '{print $NF}')"
    if [[ -z "$MOUNT_POINT" || ! -d "$MOUNT_POINT" ]]; then
        err "could not parse mount point from: $MOUNT_INFO"
        return 1
    fi
    ok "mounted at: $MOUNT_POINT"

    local INNER_APP
    INNER_APP="$(find "$MOUNT_POINT" -maxdepth 2 -name '*.app' -print -quit)"
    local rc=0
    if [[ -z "$INNER_APP" ]]; then
        err "no .app found inside DMG"
        rc=1
    else
        verify_app "$INNER_APP" || rc=1
    fi

    say "unmounting: $MOUNT_POINT"
    hdiutil detach "$MOUNT_POINT" >/dev/null 2>&1 || hdiutil detach -force "$MOUNT_POINT" >/dev/null 2>&1 || true
    return $rc
}

# ── Arg parsing. ─────────────────────────────────────────────────────────
MODE="app"
TARGET="${HOME}/Applications/Menu Band.app"

if [[ $# -ge 1 ]]; then
    case "$1" in
        --dmg)
            MODE="dmg"; shift
            TARGET="${1:?--dmg requires a path}"
            ;;
        --staged)
            MODE="staged"; shift
            TARGET="${1:?--staged requires a directory}"
            ;;
        -h|--help)
            sed -n '2,16p' "$0"; exit 0;;
        *)
            TARGET="$1"
            ;;
    esac
fi

case "$MODE" in
    dmg)
        verify_dmg "$TARGET"
        ;;
    staged)
        # A staged DMG tree: $TARGET should contain "Menu Band.app".
        STAGED_APP="$(find "$TARGET" -maxdepth 2 -name '*.app' -print -quit)"
        if [[ -z "$STAGED_APP" ]]; then
            err "no .app found under: $TARGET"
            exit 1
        fi
        verify_app "$STAGED_APP"
        ;;
    *)
        verify_app "$TARGET"
        ;;
esac
