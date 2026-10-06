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

once() {
    local t0=$SECONDS bin
    # build-dev.sh and install.sh each take the host build lock, so they run
    # one after the other, never nested.
    if ! "${DIR}/build-dev.sh" >"${LOG}" 2>&1; then
        grep -E 'error:' "${LOG}" | sort -u | head -20
        printf '✗ build failed (%ss) · full log %s\n' "$((SECONDS - t0))" "${LOG}"
        return 1
    fi
    bin="$(cd "${DIR}" && swift build -c debug --show-bin-path)/slab-menubar-swift"
    if ! SLAB_PREBUILT="${bin}" "${DIR}/install.sh" >"${LOG}.install" 2>&1; then
        tail -8 "${LOG}.install"
        printf '✗ install failed (%ss)\n' "$((SECONDS - t0))"
        return 1
    fi
    grep -E 'identical|signed with|running \(pid|recovered|! ' "${LOG}.install" | tail -3
    if pgrep -f "SlabMenubar.app/Contents/MacOS/slab-menubar" >/dev/null; then
        printf '✓ slab relaunched on the debug build (%ss)\n' "$((SECONDS - t0))"
    else
        printf '✗ slab is not running after install (%ss) · /tmp/slab-menubar.err\n' "$((SECONDS - t0))"
        return 1
    fi
}

if [[ "${1:-}" != "--watch" ]]; then once; exit $?; fi
command -v fswatch >/dev/null || { echo "--watch needs fswatch: brew install fswatch"; exit 1; }
once
echo "• watching ${DIR}/Sources (ctrl-c to stop)"
# -o: one line per batch of events; -l: let an editor's burst of writes settle
fswatch -o -l 0.4 -e '.*' -i '\.swift$' "${DIR}/Sources" | while read -r _; do
    echo "• change"; once
done
