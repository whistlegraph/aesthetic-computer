#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
probeDir=$(mktemp -d /tmp/slab-zoom-escape.XXXXXX)
trap 'rm -rf "$probeDir"' EXIT
cat "$root/Sources/SlabMenubar/CtrlDoubleTap.swift" > "$probeDir/check.swift"
cat >> "$probeDir/check.swift" <<'SWIFT'
extension CtrlDoubleTap {
    static func checkEscape() {
        var toggles = 0
        var escapes = 0
        var moves = 0
        let detector = CtrlDoubleTap(onDoubleTap: { toggles += 1 },
            onPointerMove: { _ in moves += 1 }, onEscape: { escapes += 1 })
        let event = CGEvent(keyboardEventSource: nil, virtualKey: 59, keyDown: true)!
        func control(_ down: Bool) {
            event.setIntegerValueField(.keyboardEventKeycode, value: 59)
            event.flags = down ? .maskControl : []
            detector.handle(type: .flagsChanged, event: event)
        }
        func drain() { RunLoop.main.run(until: Date(timeIntervalSinceNow: 0.02)) }
        control(true); control(true); drain()
        assert(toggles == 0, "duplicate Deskflow press must not zoom")
        control(false); control(true); drain()
        assert(toggles == 1, "real double Control must still work")
        control(false)
        control(true); control(false); control(true)
        detector.queuePointerMove(.zero)
        // Escape arrives before the scheduled toggle/follow callbacks.
        event.setIntegerValueField(.keyboardEventKeycode, value: 53)
        event.flags = [.maskControl, .maskCommand, .maskAlternate]
        detector.handle(type: .keyDown, event: event)
        drain()
        assert(escapes == 1 && toggles == 1 && moves == 0)
        event.setIntegerValueField(.keyboardEventAutorepeat, value: 1)
        detector.handle(type: .keyDown, event: event)
        assert(escapes == 1, "held Escape must not flood peers")
        event.setIntegerValueField(.keyboardEventAutorepeat, value: 0)
        control(false); control(true); control(false); control(true)
        detector.handle(type: .tapDisabledByTimeout, event: event)
        drain()
        assert(escapes == 2 && toggles == 1, "disabled tap must cancel queued zoom")
        control(false); control(true); control(false); control(true)
        detector.stop(); drain()
        assert(toggles == 1, "stopped listener must not execute queued toggle")
        print("Control / Escape event-order checks passed")
    }
}
CtrlDoubleTap.checkEscape()
SWIFT
swift "$probeDir/check.swift"
cat "$root/Sources/SlabMenubar/ZoomEscape.swift" > "$probeDir/fleet.swift"
cat >> "$probeDir/fleet.swift" <<'SWIFT'
enum ZoomLens {
    static var resets = 0
    static func zoomOut() { resets += 1 }
}
enum LedgerStore {
    static let peersDir = "/nonexistent-slab-test-peers"
    static let port: UInt16 = 5252
}
extension ZoomEscape {
    static func checkCancellation() {
        var cancelledInput = 0
        cancelPendingInput = { cancelledInput += 1 }
        let original = revision
        let oldHandoff = Date().timeIntervalSince1970
        assert(allowsRemoteZoom(startedAt: oldHandoff, revision: original))
        cancelLocal()
        assert(ZoomLens.resets == 1 && cancelledInput == 1)
        assert(!allowsRemoteZoom(startedAt: oldHandoff, revision: original))
        assert(!allowsRemoteZoom(startedAt: nil, revision: revision))
        // Simulate cooldown expiry without a sleep. A delayed pre-Escape
        // packet still cannot reactivate zoom, even after the cooldown.
        cancelledAt = Date(timeIntervalSinceNow: -4)
        assert(!allowsRemoteZoom(startedAt: Date().timeIntervalSince1970 - 5, revision: revision))
        assert(allowsRemoteZoom(startedAt: Date().timeIntervalSince1970, revision: revision))
        cancelFleet()
        cancelFleet()
        assert(ZoomLens.resets == 3, "broadcast debounce must never skip local reset")
        print("Fleet cancellation / stale handoff checks passed")
    }
}
ZoomEscape.checkCancellation()
SWIFT
swift "$probeDir/fleet.swift"
