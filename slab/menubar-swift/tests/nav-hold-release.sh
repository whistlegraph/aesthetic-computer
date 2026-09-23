#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
probeDir=$(mktemp -d /tmp/slab-nav-hold-check.XXXXXX)
trap 'rm -rf "$probeDir"' EXIT
cat "$root/Sources/SlabMenubar/NavHoldTap.swift" > "$probeDir/check.swift"
cat >> "$probeDir/check.swift" <<'SWIFT'
extension NavHoldTap {
    static func checkReleaseRecovery() {
        var starts = 0
        var ends = 0
        let detector = NavHoldTap(onHoldStart: { starts += 1 },
                                  onHoldEnd: { ends += 1 },
                                  onChordKey: {}, onPointerDown: {})
        let event = CGEvent(keyboardEventSource: nil, virtualKey: 53, keyDown: true)!
        let chord: CGEventFlags = [.maskCommand, .maskAlternate]
        func flags(_ flags: CGEventFlags) {
            event.flags = flags
            detector.handle(type: .flagsChanged, event: event)
        }
        func drain() { RunLoop.main.run(until: Date(timeIntervalSinceNow: 0.01)) }

        flags(chord)
        drain()
        assert(starts == 1 && detector.releaseTimer != nil)
        detector.reconcile(flags: chord, now: detector.holdStartedAt + 1)
        assert(detector.holding)
        // No release event arrives: polling session flags still closes it.
        detector.reconcile(flags: [], now: detector.holdStartedAt + 1)
        drain()
        assert(ends == 1 && !detector.holding && detector.releaseTimer == nil)
        for interruption in [CGEventType.tapDisabledByTimeout, .tapDisabledByUserInput] {
            flags(chord)
            drain()
            detector.handle(type: interruption, event: event)
            drain()
            assert(!detector.holding)
            flags([])
        }
        flags(chord)
        drain()
        event.setIntegerValueField(.keyboardEventKeycode, value: 53)
        detector.handle(type: .keyDown, event: event)
        drain()
        flags(chord.union(.maskSecondaryFn))
        assert(!detector.holding) // Escape stays dismissed until release.
        flags([])
        flags(chord)
        detector.reconcile(flags: chord, now: detector.holdStartedAt + 31)
        assert(!detector.holding) // Even stuck session flags expire.
        flags(chord)
        assert(!detector.holding)
        flags([])
        // A press canceled before its queued callback must never reveal.
        drain()
        let previousStarts = starts
        flags(chord)
        detector.stop()
        drain()
        assert(starts == previousStarts && detector.releaseTimer == nil)
        // Shift disqualifies the hold; Fn alone must not.
        flags(chord.union(.maskSecondaryFn))
        assert(detector.holding)
        flags(chord.union(.maskShift))
        assert(!detector.holding)
        detector.stop()
        drain()
        print("nav hold release recovery checks passed")
    }
}
NavHoldTap.checkReleaseRecovery()
SWIFT
swift "$probeDir/check.swift"
