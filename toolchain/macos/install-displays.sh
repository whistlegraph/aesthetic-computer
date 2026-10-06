#!/usr/bin/env bash
set -euo pipefail
repo=$(cd "$(dirname "$0")/../.." && pwd)
mkdir -p "$HOME/.local/bin"
build=$(mktemp -d "${TMPDIR:-/tmp}/slab-displays.XXXXXX")
trap 'rm -rf "$build"' EXIT
nice -n 10 xcrun swiftc -O "$repo/toolchain/macos/displays.swift" -o "$build/slab-displays-native"
install -m 755 "$build/slab-displays-native" "$HOME/.local/bin/slab-displays-native"
# A wrapper preserves import.meta.url's real entry point when called from PATH.
node_bin=$(node -p 'require("node:fs").realpathSync(process.execPath)')
printf '#!/usr/bin/env bash\nexec %q %q "$@"\n' "$node_bin" "$repo/toolchain/fleet/displays.mjs" > "$HOME/.local/bin/displays"
chmod 755 "$HOME/.local/bin/displays"
echo 'Installed displays and slab-displays-native'
