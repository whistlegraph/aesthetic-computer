#!/bin/sh
# Slab's ⌘⌥ pane chooser must stand down while Menu Band is the frontmost app.
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
probeDir=$(mktemp -d /tmp/slab-nav-hold-menuband.XXXXXX)
trap 'rm -rf "$probeDir"' EXIT
cat "$root/Sources/SlabMenubar/MenuBandFocus.swift" > "$probeDir/check.swift"
cat >> "$probeDir/check.swift" <<'SWIFT'
assert(MenuBandFocus.owns(bundleIdentifier: "computer.aestheticcomputer.menuband"))
assert(MenuBandFocus.owns(bundleIdentifier: "computer.aestheticcomputer.menubandlauncher"))
assert(MenuBandFocus.owns(bundleIdentifier: "computer.aesthetic.menuband"))
assert(!MenuBandFocus.owns(bundleIdentifier: "com.googlecode.iterm2"))
assert(!MenuBandFocus.owns(bundleIdentifier: "com.apple.Terminal"))
assert(!MenuBandFocus.owns(bundleIdentifier: nil))
print("nav-hold-menuband: ok")
SWIFT
swift "$probeDir/check.swift"
