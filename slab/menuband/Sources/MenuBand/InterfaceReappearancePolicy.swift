import Foundation

/// One stream rebuild after a USB interface comes back. A Scarlett that drops
/// off the bus and re-enumerates often returns with its output silently dead
/// while every HAL property reads healthy; the rate flip that `Reset Interface`
/// performs is what a replug does, so do it once for the player when the
/// interface reappears. Launch establishes the baseline without a reset, a
/// present interface staying present never resets, and the budget is two
/// rebuilds per minute so a flapping cable cannot turn into a reset loop.
struct InterfaceReappearancePolicy {
    private var interfacePresent: Bool?
    private var attempts: [TimeInterval] = []

    /// Call on every route change with whether the bound output is the
    /// interface. Returns true when a rebuild should be scheduled.
    mutating func observe(interfacePresent present: Bool, at now: TimeInterval) -> Bool {
        let was = interfacePresent
        interfacePresent = present
        guard let was, !was, present else { return false }
        attempts.removeAll { now - $0 >= 60 }
        guard attempts.count < 2 else { return false }
        attempts.append(now)
        return true
    }
}
