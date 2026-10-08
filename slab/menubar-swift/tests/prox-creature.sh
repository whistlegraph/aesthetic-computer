#!/bin/bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
probe="$(mktemp -d /tmp/prox-creature-check.XXXXXX)"
trap 'rm -rf "$probe"' EXIT
cat "$root/Sources/SlabMenubar/ProxCreature.swift" \
    "$root/Sources/SlabMenubar/ProxCreatures.swift" > "$probe/main.swift"
cat >> "$probe/main.swift" <<'SWIFT'
enum Paths { static let home = CommandLine.arguments[1] }
struct ClaudeSession {
    enum State { case working, rendering, complete, blank }
    var sessionId: String
    var state: State
    var isRemote = false
}
enum SigilRenderer {
    static func seed(for id: String) -> UInt64 { id == "one" ? 123 : 456 }
    static func name(for session: ClaudeSession) -> String { "miva" }
}
let now = Date(timeIntervalSince1970: 1_800_000_000)
var creature = ProxCreature.egg(id: "one", name: "miva", seed: 123, now: now)
assert(creature.isValid && creature.stage == .egg)
assert(!creature.acquire(.ears, provider: .local, now: now))
creature.grow(by: .nan)
creature.grow(by: -100)
assert(creature.activeSeconds == 0)
creature.grow(by: 1800)
assert(creature.stage == .stirring && !creature.canAcquireFeature)
creature.grow(by: 5400)
assert(creature.stage == .hatchling && creature.canAcquireFeature)
assert(creature.acquire(.ears, provider: .local, now: now))
assert(!creature.acquire(.feet, provider: .local, now: now))
creature.grow(by: 7200)
assert(!creature.acquire(.ears, provider: .local, now: now))
assert(creature.acquire(.feet, provider: .haiku, now: now))
creature.grow(by: 7200)
assert(creature.acquire(.tail, provider: .local, now: now))
creature.grow(by: 7200)
assert(creature.stage == .familiar && !creature.canAcquireFeature && creature.isValid)
assert(creature.seed == "000000000000007b" && creature.name == "miva")
let data = try JSONEncoder().encode(creature)
let decoded = try JSONDecoder().decode(ProxCreature.self, from: data)
assert(decoded == creature)
let good = ProxCreatureInference.parse("```json\n{\"memoir\":\"Built a garden.\",\"feature\":\"sprout\"}\n```")
assert(good?.feature == .sprout && good?.memoir == "Built a garden.")
assert(ProxCreatureInference.parse("{\"memoir\":\"Built a garden.\",\"feature\":\"run shell\"}")?.feature == nil)
assert(ProxCreatureInference.parse("{\"memoir\":\"\",\"feature\":\"ears\"}") == nil)
assert(ProxCreatureInference.parse("{bad JSON") == nil)
assert(ProxCreatureInference.parse("A plain older-model summary.")?.feature == nil)

let store = ProxCreatures()
var session = ClaudeSession(sessionId: "one", state: .working)
store.observe([session], now: now)
store.observe([session], now: now.addingTimeInterval(10))
assert(store.appearance(for: "one")?.activeSeconds == 10)
// Sleep, idle, disappearing sessions, and remote sessions add no active time.
store.observe([session], now: now.addingTimeInterval(10000))
assert(store.appearance(for: "one")?.activeSeconds == 10)
session.state = .complete
store.observe([session], now: now.addingTimeInterval(10010))
session.state = .working
store.observe([session], now: now.addingTimeInterval(10020))
assert(store.appearance(for: "one")?.activeSeconds == 10)
store.observe([], now: now.addingTimeInterval(10021))
store.observe([session], now: now.addingTimeInterval(10022))
assert(store.appearance(for: "one")?.activeSeconds == 10)
var remote = ClaudeSession(sessionId: "remote", state: .working)
remote.isRemote = true
store.observe([session, remote], now: now.addingTimeInterval(10032))
assert(store.appearance(for: "remote") == nil)
store.observe([session], now: now.addingTimeInterval(10042))
let url = ProxCreatures.directory.appendingPathComponent("000000000000007b.json")
for _ in 0..<200 {
    if let saved = try? Data(contentsOf: url),
       let record = try? JSONDecoder().decode(ProxCreature.self, from: saved), record.activeSeconds >= 10 { break }
    Thread.sleep(forTimeInterval: 0.01)
}
let restored = ProxCreatures()
restored.observe([session], now: now.addingTimeInterval(20000))
assert(restored.appearance(for: "one")?.activeSeconds == 10)
assert(restored.appearance(for: "one")?.bornAt == now.timeIntervalSince1970 * 1000)
print("PASS: identity, active-time growth, sleep/idle/restart, inference parsing, additive feature limits, JSON round-trip")
SWIFT
swift "$probe/main.swift" "$probe"
