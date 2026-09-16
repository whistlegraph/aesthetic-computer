#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
probeDir=$(mktemp -d /tmp/slab-easel-window-check.XXXXXX)
trap 'rm -rf "$probeDir"' EXIT
cat "$root/Sources/SlabMenubar/EaselWindowIdentity.swift" > "$probeDir/check.swift"
cat >> "$probeDir/check.swift" <<'SWIFT'
assert(EaselWindowIdentity.accepts(bundleID:"computer.aesthetic.easel",title:"Untitled"))
assert(EaselWindowIdentity.accepts(bundleID:"com.github.Electron",title:"fogozo.mjs · Easel"))
assert(EaselWindowIdentity.accepts(bundleID:"com.github.Electron",title:"Easel"))
assert(!EaselWindowIdentity.accepts(bundleID:"com.github.Electron",title:"Unrelated app"))
assert(!EaselWindowIdentity.accepts(bundleID:"com.github.Electron",title:"DevTools - Easel"))
assert(!EaselWindowIdentity.accepts(bundleID:"com.github.Electron",title:""))
assert(!EaselWindowIdentity.accepts(bundleID:"com.apple.Terminal",title:"Easel"))
print("Easel window identity checks passed")
SWIFT
swift "$probeDir/check.swift"
