// Pure binding policy: no application, permissions, screenshots, or input.
import Foundation
import CoreGraphics

@main struct FrameNativeInputTests {
    static func check(_ ok: Bool, _ message: String) { if !ok { fatalError(message) } }
    static func main() {
        let store = FrameNativeBindings()
        let target = FrameNativeTarget(windowID: 4, pid: 123, bounds: CGRect(x: 10, y: 20, width: 400, height: 300))
        store.record(session: "a", id: "one", target: target)
        check(store.validate(session: "a", id: "one", current: target, available: true) == nil, "current target")
        check(store.validate(session: "b", id: "one", current: target, available: true) != nil, "cross-session rejected")
        check(store.validate(session: "a", id: "old", current: target, available: true) != nil, "old observation rejected")
        check(store.validate(session: "a", id: "one", current: target, available: false) != nil, "locked display rejected")
        check(store.validate(session: "a", id: "one", current: nil, available: true) != nil, "missing window rejected")
        for changed in [FrameNativeTarget(windowID: 5, pid: 123, bounds: target.bounds),
                        FrameNativeTarget(windowID: 4, pid: 124, bounds: target.bounds),
                        FrameNativeTarget(windowID: 4, pid: 123, bounds: target.bounds.offsetBy(dx: 1, dy: 0))] {
            check(store.validate(session: "a", id: "one", current: changed, available: true) != nil, "identity/geometry rejected")
        }
        check(store.validate(session: "a", id: "one", current: target, available: true, point: CGPoint(x: 20, y: 30)) == nil, "inside point")
        check(store.validate(session: "a", id: "one", current: target, available: true, point: CGPoint(x: 0, y: 0)) != nil, "outside point")
        store.clear("a")
        check(store.validate(session: "a", id: "one", current: target, available: true) != nil, "consumed observation cannot replay")
        store.record(session: "a", id: "new", target: target)
        check(store.validate(session: "a", id: "one", current: target, available: true) != nil, "new observation replaces old")
        for i in 0..<32 { store.record(session: "s\(i)", id: "id", target: target) }
        check(store.validate(session: "a", id: "new", current: target, available: true) != nil, "bounded storage")
        check(store.validate(session: "s31", id: "id", current: target, available: true) == nil, "newest retained")
        check(FrameNativeClick(observationId: "x", x: 10, y: 20, count: 1, settleMs: 0).valid, "valid click")
        check(!FrameNativeClick(observationId: "x", x: .nan, y: 20, count: 1, settleMs: 0).valid, "NaN rejected")
        check(!FrameNativeClick(observationId: "x", x: 10, y: 20, count: 4, settleMs: 0).valid, "invalid count")
        check(!FrameNativeClick(observationId: "x", x: 10, y: 20, count: 1, settleMs: -1).valid, "invalid settling")
        let json = #"{"observationId":"x","x":10,"y":20,"count":1,"settleMs":0,"verify":{"x":30,"y":40,"role":"AXStaticText","attribute":"AXValue","equals":"Count: 3","timeoutMs":250}}"#
        let decoded = try! JSONDecoder().decode(FrameNativeClick.self, from: Data(json.utf8))
        check(decoded.valid && decoded.verify?.equals == "Count: 3", "verification decoding")
        let bad = json.replacingOccurrences(of: "AXValue", with: "AXChildren")
        check(!(try! JSONDecoder().decode(FrameNativeClick.self, from: Data(bad.utf8))).valid, "attribute allowlist")
        var held = decoded
        for hold in [0.0, 0.1, 0.5, 1, 2.5, 5, 10, 20, 40] {
            held.holdMs = hold
            check(held.valid && held.effectiveHoldMs == hold, "fractional hold supported")
        }
        for hold in [-1.0, Double.nan, Double.infinity, 1001] {
            held.holdMs = hold
            check(!held.valid, "invalid hold rejected")
        }
        held.holdMs = 0
        held.drag = FrameNativeDragPath(x: 100, y: 120, durationMs: 0)
        check(held.valid, "zero-wait drag supported")
        held.drag = FrameNativeDragPath(x: .infinity, y: 120, durationMs: 0)
        check(!held.valid, "invalid drag point rejected")
        held.drag = FrameNativeDragPath(x: 100, y: 120, durationMs: -1)
        check(!held.valid, "invalid duration rejected")
        held.drag = FrameNativeDragPath(x: 100, y: 120, durationMs: 0, releaseMs: 8)
        check(held.valid, "bounded destination dwell")
        held.drag = FrameNativeDragPath(x: 100, y: 120, durationMs: 0, releaseMs: .nan)
        check(!held.valid, "invalid destination dwell")
        check(decoded.effectiveHoldMs == 40, "older clients retain original timing")
        print("PASS: observation ownership, replacement, single use, geometry, availability, point bounds, capacity, validation")
    }
}
