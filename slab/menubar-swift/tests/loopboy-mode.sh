#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
probeDir=$(mktemp -d /tmp/slab-loopboy-mode.XXXXXX)
trap 'rm -rf "$probeDir"' EXIT
cat "$root/Sources/SlabMenubar/LoopboyRoutes.swift" > "$probeDir/check.swift"
cat >> "$probeDir/check.swift" <<'SWIFT'
enum Paths {
    static let slabHome = CommandLine.arguments[1]
    static var loopboyConfig: String { "\(slabHome)/loopboy.json" }
}
struct ClaudeSession { let sessionId: String; let loopboyContact: String }
let id = "cccccccc-1111-2222-3333-444444444444"
let session = ClaudeSession(sessionId: id, loopboyContact: "fia")
func write(_ obj: [String: Any], _ path: String) {
    try! JSONSerialization.data(withJSONObject: obj).write(to: URL(fileURLWithPath: path))
}
write(["loops": ["fia": ["sessionId": id, "wake": true, "autoRespond": true]]], Paths.loopboyConfig)
assert(LoopboyRoutes.verifiedContact(for: session) == "fia")
let modes = "\(Paths.slabHome)/state/loopboy-modes"
try! FileManager.default.createDirectory(atPath: modes, withIntermediateDirectories: true)
let modeFile = "\(modes)/\(id).json"
// OFF defeats a stale marker AND a stale route carrying both legacy flags.
write(["sessionId": id, "contact": "", "name": "koker"], modeFile)
assert(LoopboyRoutes.verifiedContact(for: session) == nil)
assert(LoopboyRoutes.mode(for: id)?["name"] as? String == "koker")
// In-place adoption needs neither a launched contact nor a new session.
write(["sessionId": id, "contact": "fia"], modeFile)
assert(LoopboyRoutes.verifiedContact(for: ClaudeSession(sessionId: id, loopboyContact: "")) == "fia")
write(["loops": ["fia": ["sessionId": "different-session"]]], Paths.loopboyConfig)
assert(LoopboyRoutes.verifiedContact(for: session) == nil)
write(["loops": ["fia": ["sessionId": id, "channel": "signal"]]], Paths.loopboyConfig)
assert(LoopboyRoutes.verifiedContact(for: session) == nil)
write(["loops": ["fia": ["sessionId": id]]], Paths.loopboyConfig)
try! Data("broken".utf8).write(to: URL(fileURLWithPath: modeFile))
assert(LoopboyRoutes.verifiedContact(for: session) == nil)
// Custom sources have their own namespace, label and generation.
let custom: [String: Any] = ["event": "fishery", "source": "studio", "contact": "", "bindingId": "one"]
var customMode = custom
customMode["sessionId"] = id
write(customMode, modeFile)
var customRoute = customMode
write(["loops": ["fishery:studio": customRoute]], Paths.loopboyConfig)
assert(LoopboyRoutes.verifiedContact(for: session) == "fishery:studio")
assert(LoopboyRoutes.all()["fishery:studio"]?.channel == "fishery")
customRoute["bindingId"] = "two"
write(["loops": ["fishery:studio": customRoute]], Paths.loopboyConfig)
assert(LoopboyRoutes.verifiedContact(for: session) == nil)
customRoute["bindingId"] = "one"
customRoute["expiresAt"] = "2000-01-01T00:00:00Z"
write(["loops": ["fishery:studio": customRoute]], Paths.loopboyConfig)
assert(LoopboyRoutes.verifiedContact(for: session) == nil)
assert(LoopboyRoutes.binding(["event": "fishery", "source": "../studio"]) == nil)
assert(LoopboyRoutes.binding(["event": "fishery", "source": "studio", "contact": "fia"]) == nil)
print("Loopboy mode: in-place adoption, durable exit, stale flags, route identity and channel checks passed")
SWIFT
swift "$probeDir/check.swift" "$probeDir"
# The event handler must not regain terminal control or retry wake machinery.
python3 - "$root/Sources/SlabMenubar/AppDelegate.swift" <<'PY'
import pathlib,sys
s=pathlib.Path(sys.argv[1]).read_text()
handler=s.split('private func bumpBoundProx(',1)[1].split('/// Stable Loopboy bindings',1)[0]
assert 'queueInbox(' in handler and 'pokeLocal(' in handler
for forbidden in ['wakeTerminal(', 'wakeEasel(', 'typePromptWithCGEvents(', 'focusTerminal(', 'flyPrompt(', 'asyncAfter', 'screen']:
 assert forbidden not in handler, forbidden
assert 'loopboyWakeInFlight' not in s
assert 'loopboyEvaluatedFingerprint' not in s
print('Loopboy arrivals cannot enter the terminal wake path; heartbeat retry loop removed')
PY
