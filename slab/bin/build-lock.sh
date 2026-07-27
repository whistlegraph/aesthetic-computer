#!/usr/bin/env bash
# Shared, dependency-free build lock for the host's Swift utilities.
# Source this file, then call `acquire_build_lock <name>` before compiling.

BUILD_LOCK_DIR=""

release_build_lock() {
    [[ -n "${BUILD_LOCK_DIR:-}" ]] || return 0
    local owner=""
    [[ -f "${BUILD_LOCK_DIR}/owner" ]] && owner="$(cat "${BUILD_LOCK_DIR}/owner" 2>/dev/null || true)"
    if [[ "${owner}" == "$$" ]]; then
        rm -f "${BUILD_LOCK_DIR}/owner"
        rmdir "${BUILD_LOCK_DIR}" 2>/dev/null || true
    fi
    BUILD_LOCK_DIR=""
    unset AC_BUILD_LOCK_HELD
}

acquire_build_lock() {
    local name="${1:?build-lock name is required}"
    local base="${TMPDIR:-/tmp}"
    local lock="${base%/}/computer.aesthetic.build-${name}-$(id -u).lock"
    local owner="" waited=0

    while ! mkdir "${lock}" 2>/dev/null; do
        [[ -f "${lock}/owner" ]] && owner="$(cat "${lock}/owner" 2>/dev/null || true)" || owner=""
        if [[ ! "${owner}" =~ ^[0-9]+$ ]] || ! kill -0 "${owner}" 2>/dev/null; then
            rm -f "${lock}/owner"
            rmdir "${lock}" 2>/dev/null || true
            continue
        fi
        if (( waited == 0 )); then
            printf '• waiting for %s build held by pid %s\n' "${name}" "${owner}"
        fi
        if (( waited >= 300 )); then
            printf 'build lock timed out after 300s: %s (owner pid %s)\n' "${lock}" "${owner}" >&2
            return 1
        fi
        sleep 1
        waited=$((waited + 1))
    done

    BUILD_LOCK_DIR="${lock}"
    export AC_BUILD_LOCK_HELD="${name}"
    printf '%s\n' "$$" > "${BUILD_LOCK_DIR}/owner"
    trap 'release_build_lock' EXIT
    trap 'release_build_lock; exit 129' HUP
    trap 'release_build_lock; exit 130' INT
    trap 'release_build_lock; exit 143' TERM
}
