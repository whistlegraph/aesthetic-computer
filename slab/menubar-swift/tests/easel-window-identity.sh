#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
probeDir=$(mktemp -d /tmp/slab-easel-window-check.XXXXXX)
trap 'rm -rf "$probeDir"' EXIT
cat "$root/Sources/SlabMenubar/AeselWindowIdentity.swift" > "$probeDir/check.swift"
cat >> "$probeDir/check.swift" <<'SWIFT'
assert(AeselWindowIdentity.accepts(bundleID:"computer.aesthetic.easel",title:"Untitled"))
assert(AeselWindowIdentity.accepts(bundleID:"com.github.Electron",title:"fogozo.mjs · aesel"))
assert(AeselWindowIdentity.accepts(bundleID:"com.github.Electron",title:"aesel"))
assert(!AeselWindowIdentity.accepts(bundleID:"com.github.Electron",title:"Unrelated app"))
assert(!AeselWindowIdentity.accepts(bundleID:"com.github.Electron",title:"DevTools - aesel"))
assert(!AeselWindowIdentity.accepts(bundleID:"com.github.Electron",title:""))
assert(!AeselWindowIdentity.accepts(bundleID:"com.apple.Terminal",title:"aesel"))
print("aesel window identity checks passed")
SWIFT
swift "$probeDir/check.swift"
