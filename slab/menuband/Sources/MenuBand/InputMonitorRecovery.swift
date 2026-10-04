import Foundation

/// Bounds microphone reopening independently of the audio callback threads.
/// A muted monitor still captures for tape, so audibility is not a recovery gate.
final class InputMonitorRecovery {
    private let lock = NSLock()
    private var attached = false
    private var sleeping = false
    private var attempts = 0
    private var generation = 0
    private var pending = false
    private var eligibleAfter: TimeInterval = 0
    static let maximumAttempts = 3

    var isSleeping: Bool {
        lock.lock(); defer { lock.unlock() }
        return sleeping
    }

    func setAttached(_ value: Bool, now: TimeInterval) {
        lock.lock(); defer { lock.unlock() }
        attached = value
        reset(now: now)
    }

    func setSleeping(_ value: Bool, now: TimeInterval) {
        lock.lock(); defer { lock.unlock() }
        sleeping = value
        reset(now: now)
    }

    func deviceDidChange(now: TimeInterval) {
        lock.lock(); defer { lock.unlock() }
        reset(now: now)
    }

    private func reset(now: TimeInterval) {
        generation += 1
        attempts = 0
        pending = false
        // Allow normal capture to resume before deciding the device is dead.
        eligibleAfter = now + 10
    }

    func request(captures: Int, now: TimeInterval) -> Int? {
        lock.lock(); defer { lock.unlock() }
        guard attached, !sleeping, !pending, captures == 0,
              now >= eligibleAfter, attempts < Self.maximumAttempts else { return nil }
        pending = true
        return generation
    }

    /// Recheck on the execution queue: sleep, detach, or a device change may
    /// have happened since the health timer queued this attempt.
    func claim(_ ticket: Int) -> Int? {
        lock.lock(); defer { lock.unlock() }
        guard ticket == generation, pending, attached, !sleeping else { return nil }
        pending = false
        attempts += 1
        return attempts
    }
}
