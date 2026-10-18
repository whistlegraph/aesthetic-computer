import Foundation

/// Detect a stalled/restarted hardware clock, not silence in the music.
/// Startup, deliberate graph changes, route/rate changes and sleep establish
/// a fresh baseline. Each automatic reset needs new evidence and a budget.
struct InterfaceFaultRecovery {
    private var device: UInt32?
    private var rate: Double = 0
    private var sample: Double?
    private var observedAt: TimeInterval?
    private var stalledSince: TimeInterval?
    private var ignoreUntil: TimeInterval = 0
    private var attempts: [TimeInterval] = []

    mutating func suspend(at now: TimeInterval) {
        sample = nil
        observedAt = nil
        stalledSince = nil
        ignoreUntil = max(ignoreUntil, now + 3)
    }

    mutating func didReset(at now: TimeInterval) {
        attempts.removeAll { now - $0 >= 60 }
        attempts.append(now)
        suspend(at: now)
        ignoreUntil = now + 15
    }

    mutating func observe(device id: UInt32, rate hz: Double, sampleTime: Double?,
                          active: Bool, at now: TimeInterval) -> String? {
        guard active, hz > 0 else { suspend(at: now); return nil }
        if device != id || rate != hz || observedAt.map({ now - $0 > 3 }) == true {
            device = id
            rate = hz
            suspend(at: now)
        }
        let previous = sample
        sample = sampleTime.flatMap { $0.isFinite ? $0 : nil }
        observedAt = now
        guard now >= ignoreUntil else { return nil }
        let reason: String?
        if let previous, let sample, sample < previous - hz * 0.1 {
            reason = "hardware clock restarted unexpectedly"
            stalledSince = nil
        } else if sample == nil || (previous != nil && sample == previous) {
            if stalledSince == nil { stalledSince = now }
            reason = now - (stalledSince ?? now) >= 2 ? "hardware clock stopped advancing" : nil
        } else {
            stalledSince = nil
            reason = nil
        }
        guard let reason else { return nil }
        attempts.removeAll { now - $0 >= 60 }
        guard attempts.count < 2 else { return nil }
        didReset(at: now)
        return reason
    }
}
