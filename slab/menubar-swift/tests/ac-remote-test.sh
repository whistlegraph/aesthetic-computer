#!/bin/bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
build="$(mktemp -d /tmp/ac-remote-test.XXXXXX)"
trap 'rm -rf "$build"' EXIT
cat "$root/Sources/SlabMenubar/ACKeyboardRemote.swift" "$root/tests/ac-remote-test.swift" > "$build/main.swift"
nice -n 8 swiftc "$root/Sources/SlabMenubar/GlobalHotkey.swift" "$build/main.swift" -o "$build/test"
"$build/test"
