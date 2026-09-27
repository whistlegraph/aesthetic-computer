#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
probeDir=$(mktemp -d /tmp/slab-native-aesel-check.XXXXXX)
trap 'rm -rf "$probeDir"' EXIT
cat "$root/Sources/SlabMenubar/AeselWindowIdentity.swift" "$root/Sources/SlabMenubar/NativeAeselSession.swift" > "$probeDir/check.swift"
cat >> "$probeDir/check.swift" <<'SWIFT'
enum Paths { static let activePromptsDir = "/unused" }
let now = Date()
let id = "aesel-native-" + UUID().uuidString
var marker: [String: Any] = ["session_id": id, "agent_type": "easel", "host_app": NativeAeselSession.bundleID,
    "agent_pid": 123, "host_pid": 123, "host_window_id": 456, "state": "working",
    "updated": ISO8601DateFormatter().string(from: now), "source": "never copy", "provider_session_id": "never copy"]
var layout: [String: Any] = ["sessionId": id, "visible": true, "x": 90.0, "y": 2.0, "size": 28.0,
    "windowWidth": 400.0, "windowHeight": 500.0]
func record() -> Data { try! JSONSerialization.data(withJSONObject: ["schema": 1, "marker": marker, "layout": layout]) }
func decode(_ live: Bool = true, at: Date = now) -> (marker: [String: Any], layout: [String: Any])? {
    NativeAeselSession.decode(record(), now: at, ownsPID: { $0 == 123 && live })
}
assert(AeselWindowIdentity.accepts(bundleID: NativeAeselSession.bundleID, title: ""))
assert(!AeselWindowIdentity.accepts(bundleID: "com.github.Electron", title: "Unrelated"))
assert(decode() != nil)
assert(decode()!.marker["source"] == nil && decode()!.marker["provider_session_id"] == nil)
assert(decode(false) == nil)
assert(decode(at: now.addingTimeInterval(31)) == nil)
assert(decode(at: now.addingTimeInterval(-10)) == nil)
layout["x"] = 390.0
assert(decode() == nil)
layout["x"] = 90.0
layout["sessionId"] = "wrong-window-session"
assert(decode() == nil)
layout["sessionId"] = id
marker["session_id"] = "aesel-native-../../escape"
assert(decode() == nil)
marker["session_id"] = id
for state in ["blank", "working", "complete", "awaiting", "interrupted"] {
    marker["state"] = state
    assert(decode()!.marker["state"] as? String == state)
}
marker["state"] = "unknown"
assert(decode() == nil)
print("Native aesel: identity, status, stale/dead process, geometry and export boundary checks passed")
SWIFT
swift "$probeDir/check.swift"
