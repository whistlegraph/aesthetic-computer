#!/usr/bin/env bash
# PATH shim for `swift`: serialize SwiftPM builds per AC package while leaving
# every other Swift command untouched. Xcode's internal absolute tool paths are
# unaffected; this covers humans and agents invoking `swift build` in shells.

set -euo pipefail
REAL_SWIFT="/usr/bin/swift"
SOURCE="${BASH_SOURCE[0]}"
while [[ -L "$SOURCE" ]]; do
    SOURCE_DIR="$(cd "$(dirname "$SOURCE")" && pwd)"
    SOURCE="$(readlink "$SOURCE")"
    [[ "$SOURCE" == /* ]] || SOURCE="$SOURCE_DIR/$SOURCE"
done
REPO="$(cd "$(dirname "$SOURCE")/../.." && pwd)"

[[ "${1:-}" == "build" ]] || exec "$REAL_SWIFT" "$@"

run_guarded_build() {
    local have_jobs=0 arg memory_bytes jobs=2
    for arg in "$@"; do
        case "$arg" in -j|--jobs|--jobs=*) have_jobs=1 ;; esac
    done
    memory_bytes="$(sysctl -n hw.memsize 2>/dev/null || echo 0)"
    if (( memory_bytes > 17179869184 )); then jobs=4; fi
    if (( have_jobs == 1 )); then
        /usr/bin/nice -n 8 "$REAL_SWIFT" "$@"
    else
        /usr/bin/nice -n 8 "$REAL_SWIFT" "$@" --jobs "$jobs"
    fi
}

package_path="$PWD"
args=("$@")
for ((i = 0; i < ${#args[@]}; i++)); do
    case "${args[$i]}" in
        --package-path)
            ((i + 1 < ${#args[@]})) && package_path="${args[$((i + 1))]}"
            ;;
        --package-path=*) package_path="${args[$i]#--package-path=}" ;;
    esac
done

if [[ -d "$package_path" ]]; then
    package_path="$(cd "$package_path" 2>/dev/null && pwd -P)"
fi
probe="$package_path"
while [[ "$probe" == "$REPO"/* && ! -f "$probe/Package.swift" ]]; do
    probe="$(dirname "$probe")"
done
[[ -f "$probe/Package.swift" ]] && package_path="$probe"

case "$package_path" in
    "$REPO/slab/menubar-swift"*) lock_name="slab-menubar" ;;
    "$REPO/slab/menuband"*) lock_name="menuband" ;;
    "$REPO"/*)
        lock_id="$(printf '%s' "$package_path" | cksum | awk '{print $1}')"
        lock_name="swift-${lock_id}"
        ;;
    *) exec "$REAL_SWIFT" "$@" ;;
esac

# An installer that already owns this exact lock must be allowed to invoke its
# child `swift build`; everyone else waits rather than compiling concurrently.
if [[ "${AC_BUILD_LOCK_HELD:-}" == "$lock_name" ]]; then
    run_guarded_build "$@"
    exit $?
fi

source "$REPO/slab/bin/build-lock.sh"
acquire_build_lock "$lock_name"
run_guarded_build "$@"
