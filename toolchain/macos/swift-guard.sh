#!/usr/bin/env bash
# PATH shim for `swift`: check headroom and serialize SwiftPM builds while leaving
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
GUARD_DIR="$(cd "$(dirname "$SOURCE")" && pwd)"
[[ ! -f "$GUARD_DIR/repo-path" ]] || IFS= read -r REPO < "$GUARD_DIR/repo-path"

[[ "${1:-}" == "build" ]] || exec "$REAL_SWIFT" "$@"

run_guarded_build() {
    local have_jobs=0 arg memory_bytes jobs=2
    /bin/bash "$GUARD_DIR/performance-guard.sh" --admit 'swift build' "$build_path" || return $?
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
build_path=""
args=("$@")
for ((i = 0; i < ${#args[@]}; i++)); do
    case "${args[$i]}" in
        --package-path)
            ((i + 1 < ${#args[@]})) && package_path="${args[$((i + 1))]}"
            ;;
        --package-path=*) package_path="${args[$i]#--package-path=}" ;;
        --scratch-path)
            ((i + 1 < ${#args[@]})) && build_path="${args[$((i + 1))]}"
            ;;
        --scratch-path=*) build_path="${args[$i]#--scratch-path=}" ;;
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
[[ -n "$build_path" ]] || build_path="$package_path/.build"

case "$package_path" in
    "$REPO/slab/menubar-swift"*) lock_name="slab-menubar" ;;
    "$REPO/slab/menuband"*) lock_name="menuband" ;;
    *)
        lock_id="$(printf '%s' "$package_path" | cksum | awk '{print $1}')"
        lock_name="swift-${lock_id}"
        ;;
esac

# An installer that already owns this exact lock must be allowed to invoke its
# child `swift build`; everyone else waits rather than compiling concurrently.
if [[ "${AC_BUILD_LOCK_HELD:-}" == "$lock_name" ]]; then
    run_guarded_build "$@"
    exit $?
fi

if [[ -f "$GUARD_DIR/build-lock.sh" ]]; then
    source "$GUARD_DIR/build-lock.sh"
else
    source "$REPO/slab/bin/build-lock.sh"
fi
# Check before waiting for a lock, then check again immediately before launch.
/bin/bash "$GUARD_DIR/performance-guard.sh" --admit 'swift build' "$build_path"
acquire_build_lock "$lock_name"
run_guarded_build "$@"
