import Foundation

/// Watches the actual output stream, independently of the popover and meters.
/// A valid device ID and isRunning can both survive a dead render stream.
final class MenuBandPlaybackHealth {
    private let lock = NSLock()
    private var lastBuffer: TimeInterval = 0
    private var healthySince: TimeInterval?
    private var invalidSamples = 0
    private var notBefore: TimeInterval = 0
    private var attempts = 0
    private var interfaceResetting = false

    func setInterfaceResetting(_ active: Bool,
                              at now: TimeInterval = ProcessInfo.processInfo.systemUptime) {
        lock.lock(); defer { lock.unlock() }
        interfaceResetting = active
        // Rate changes can briefly stop callbacks and produce invalid frames.
        // Containment stays active, but don't restart the engine mid-reset.
        lastBuffer = now
        invalidSamples = 0
        healthySince = nil
        notBefore = now + 2
    }

    func begin(at now: TimeInterval = ProcessInfo.processInfo.systemUptime,
               renewBudget: Bool = false) {
        lock.lock(); defer { lock.unlock() }
        lastBuffer = now
        healthySince = nil
        invalidSamples = 0
        notBefore = now + 2
        if renewBudget { attempts = 0 }
    }

    /// The callback never waits for the control thread. Missing one observation
    /// is harmless: the timeout spans seconds, not individual audio buffers.
    func observe(invalid: Int, at now: TimeInterval) {
        guard lock.try() else { return }
        defer { lock.unlock() }
        lastBuffer = now
        if invalid > 0 {
            invalidSamples += invalid
            healthySince = nil
        } else if invalidSamples == 0 {
            if healthySince == nil { healthySince = now }
            if now - (healthySince ?? now) >= 10 { attempts = 0 }
        }
    }

    /// Claim one bounded recovery. Intentional idle pauses never count as a
    /// failure; a route change or wake renews the budget if IO stayed broken.
    func recoveryReason(at now: TimeInterval = ProcessInfo.processInfo.systemUptime,
                        shouldRun: Bool, isRunning: Bool) -> String? {
        lock.lock(); defer { lock.unlock() }
        guard shouldRun, !interfaceResetting, now >= notBefore, attempts < 3 else { return nil }
        let reason: String
        if invalidSamples > 0 {
            reason = "\(invalidSamples) non-finite output samples"
        } else if !isRunning {
            reason = "engine stopped unexpectedly"
        } else if now - lastBuffer > 2 {
            reason = "output callbacks stopped"
        } else {
            return nil
        }
        attempts += 1
        notBefore = now + 5
        lastBuffer = now
        invalidSamples = 0
        healthySince = nil
        return "\(reason) (recovery \(attempts)/3)"
    }
}
