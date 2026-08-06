#!/usr/bin/env bash
# Fast local compile for iteration. Production installation remains release.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "${SCRIPT_DIR}/../bin/build-lock.sh"
acquire_build_lock slab-menubar
cd "${SCRIPT_DIR}"
swift build -c debug
