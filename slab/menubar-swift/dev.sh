#!/usr/bin/env bash
# dev.sh — the Slab iteration loop: incremental debug build, then install.sh
# signs it with the stable identity (TCC grants hold) and relaunches the agent
# in place. A one-file change is back on screen in seconds.
#   ./dev.sh           build and relaunch once
#   ./dev.sh --watch   again on every save under Sources/ (needs fswatch)
# This runs the DEBUG binary as the live menubar. ./install.sh puts the
# release build back.
set -uo pipefail
DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG="${TMPDIR:-/tmp}/slab-dev-build.log"

APP="${HOME}/Applications/SlabMenubar.app"
APP_BIN="${APP}/Contents/MacOS/slab-menubar"
KC="${HOME}/Library/Keychains/slab-signing.keychain-db"
LABEL="gui/$(id -u)/computer.slab.menubar"

# The quick path, once install.sh has set the machine up: only the binary
# changed, so swap it, re-sign with the same stable identity (TCC grants
# hold) and kickstart. Skips install.sh's provisioning, which rewrites
# Terminal profiles, watchers, shims and the plist on every run. Returns
# non-zero whenever anything else might need installing; the caller falls
# back to install.sh.
swap() {
    local bin="$1" hash pid old i
    [[ -x "${APP_BIN}" && -f "${KC}" ]] || return 1
    cmp -s "${DIR}/Info.plist" "${APP}/Contents/Info.plist" || return 1
    launchctl print "${LABEL}" >/dev/null 2>&1 || return 1
    hash="$(security find-certificate -Z -c "Slab Menubar Self-Signed" "${KC}" 2>/dev/null | awk '/SHA-1 hash:/{print $3; exit}')"
    [[ -n "${hash}" ]] || return 1
    security unlock-keychain -p "${SLAB_SIGN_KC_PASS:-slab-signing}" "${KC}" 2>/dev/null || true
    # A fresh inode, not an overwrite: kickstart against the old inode's cached
    # signature is the "Code Signature Invalid" launch failure install.sh
    # documents. Then sign the bundle around it.
    cp "${bin}" "${APP_BIN}.next"
    mv -f "${APP_BIN}.next" "${APP_BIN}"
    codesign --force --sign "${hash}" --identifier computer.slab.menubar "${APP}" >/dev/null 2>&1 || return 1
    old="$(pgrep -f "SlabMenubar.app/Contents/MacOS/slab-menubar" | head -1)"
    launchctl kickstart -k "${LABEL}" || return 1
    for i in $(seq 1 16); do
        pid="$(pgrep -f "SlabMenubar.app/Contents/MacOS/slab-menubar" | head -1)"
        [[ -n "${pid}" && "${pid}" != "${old}" ]] && return 0
        sleep 0.25
    done
    return 1
}

# The debug binary, without asking SwiftPM (a second package load, ~1.3 s):
# the newest of the two layouts SwiftPM uses, the ask only as a fallback.
debug_bin() {
    local a="${DIR}/.build/out/Products/Debug/slab-menubar-swift" b="${DIR}/.build/debug/slab-menubar-swift"
    if [[ -x "$a" && ( ! -x "$b" || "$a" -nt "$b" ) ]]; then echo "$a"
    elif [[ -x "$b" ]]; then echo "$b"
    else echo "$(cd "${DIR}" && swift build -c debug --show-bin-path)/slab-menubar-swift"; fi
}

# 0 relaunched · 1 failed · 75 the host's performance guard deferred the build
once() {
    local t0=$SECONDS bin status sum
    # build-dev.sh and install.sh each take the host build lock, so they run
    # one after the other, never nested.
    "${DIR}/build-dev.sh" >"${LOG}" 2>&1; status=$?
    if (( status == 75 )); then
        printf '… deferred: %s\n' "$(grep -m1 -o 'deferred.*' "${LOG}" | cut -c1-110)"
        return 75
    elif (( status != 0 )); then
        grep -E 'error:' "${LOG}" | sort -u | head -20
        printf '✗ build failed (%ss) · full log %s\n' "$((SECONDS - t0))" "${LOG}"
        return 1
    fi
    bin="$(debug_bin)"
    # The same binary as last time (a save that compiled to nothing new): no
    # relaunch, so the menubar does not blink for a comment.
    sum="$(shasum -a 256 "${bin}" | cut -c1-64)"
    if [[ -f "${DIR}/.build/.dev-swapped" && "$(cat "${DIR}/.build/.dev-swapped")" == "${sum}" ]] \
       && pgrep -f "SlabMenubar.app/Contents/MacOS/slab-menubar" >/dev/null; then
        printf '= unchanged binary, still running (%ss)\n' "$((SECONDS - t0))"
        return 0
    fi
    if swap "${bin}"; then
        echo "${sum}" > "${DIR}/.build/.dev-swapped"
        printf '✓ slab relaunched on the debug build (%ss)\n' "$((SECONDS - t0))"
        return 0
    fi
    echo "• full install (first run, bundle changed, or the swap did not come up)"
    if ! SLAB_PREBUILT="${bin}" "${DIR}/install.sh" >"${LOG}.install" 2>&1; then
        tail -8 "${LOG}.install"
        printf '✗ install failed (%ss)\n' "$((SECONDS - t0))"
        return 1
    fi
    grep -E 'identical|signed with|running \(pid|recovered|! ' "${LOG}.install" | tail -3
    if pgrep -f "SlabMenubar.app/Contents/MacOS/slab-menubar" >/dev/null; then
        echo "${sum}" > "${DIR}/.build/.dev-swapped"
        printf '✓ slab relaunched on the debug build (%ss)\n' "$((SECONDS - t0))"
    else
        printf '✗ slab is not running after install (%ss) · /tmp/slab-menubar.err\n' "$((SECONDS - t0))"
        return 1
    fi
}

# Until it is not deferred: the guard admits the build once load falls.
until_built() {
    local status
    while :; do
        once; status=$?
        (( status == 75 )) || return "${status}"
        sleep 5
    done
}

if [[ "${1:-}" != "--watch" ]]; then once; exit $?; fi
# Watch mode waits out the performance guard instead of giving up.
command -v fswatch >/dev/null || { echo "--watch needs fswatch: brew install fswatch"; exit 1; }
until_built
echo "• watching ${DIR}/Sources (ctrl-c to stop)"
# -o: one line per batch of events; -l: let an editor's burst of writes settle
fswatch -o -l 0.4 -e '.*' -i '\.swift$' "${DIR}/Sources" | while read -r _; do
    echo "• change"; until_built
done
