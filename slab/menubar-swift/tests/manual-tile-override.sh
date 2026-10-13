#!/bin/bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
probe_dir="$(mktemp -d /tmp/slab-manual-tile.XXXXXX)"
trap 'rm -rf "$probe_dir"' EXIT
cp "$root/Sources/SlabMenubar/ManualTileOverride.swift" "$probe_dir/check.swift"
cat >> "$probe_dir/check.swift" <<'SWIFT'
let original = CGRect(x: 100, y: 100, width: 500, height: 400)
var override = ManualTileOverride()
// Ordinary clicks, small rounding changes and disappeared windows stay tiled.
override.begin(id: 7, frame: original)
assert(!override.observe(frames: [7: original]))
assert(!override.observe(frames: [7: original.offsetBy(dx: 1, dy: 1)]))
assert(!override.observe(frames: [:]))
assert(!override.observe(frames: [8: original.offsetBy(dx: 30, dy: 0)]))
override.end()
assert(!override.observe(frames: [7: original.offsetBy(dx: 30, dy: 0)]))
// Moving a tracked tile suspends even after the gesture ends or it snaps back.
override.begin(id: 7, frame: original)
assert(override.observe(frames: [7: original.offsetBy(dx: 30, dy: 0)]))
override.end()
assert(override.suspended)
assert(!override.observe(frames: [7: original]))
// Explicit tiling resumes; resizing either axis also leaves the grid.
for resized in [CGRect(x: 100, y: 100, width: 600, height: 400),
                CGRect(x: 100, y: 100, width: 500, height: 500)] {
    override.resume()
    assert(!override.suspended && !override.isTracking)
    override.begin(id: 7, frame: original)
    assert(override.observe(frames: [7: resized]))
    assert(override.suspended)
}
print("manual tile override: passed")
SWIFT
swift "$probe_dir/check.swift"
