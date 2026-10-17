import Foundation

/// A stream rebuild follows an actual connection or engine recovery, never
/// pointer entry or periodic device enumeration. Hardware can report healthy
/// callbacks while a Scarlett output stream remains silent.
struct InterfaceRecoveryPolicy {
    private var observedDevice: UInt32?
    private var pendingDevice: UInt32?
    private var readyAt: TimeInterval = 0
    private var resetTimes: [TimeInterval] = []

    mutating func observeRoute(device: UInt32?, affected: Bool, at now: TimeInterval) {
        guard device != observedDevice else { return }
        observedDevice = device
        pendingDevice = nil
        if affected, let device { engineRecovered(device: device, at: now) }
    }

    mutating func engineRecovered(device: UInt32, at now: TimeInterval) {
        pendingDevice = device
        readyAt = now + 3
    }

    mutating func didReset(at now: TimeInterval) {
        pendingDevice = nil
        resetTimes.removeAll { now - $0 >= 60 }
        resetTimes.append(now)
    }

    mutating func takeRequest(device: UInt32?, resetting: Bool, at now: TimeInterval) -> UInt32? {
        guard let pendingDevice, pendingDevice == device, !resetting,
              now >= readyAt else { return nil }
        resetTimes.removeAll { now - $0 >= 60 }
        // A fault loop must not keep interrupting all clients of the device.
        guard resetTimes.count < 3 else {
            self.pendingDevice = nil
            return nil
        }
        didReset(at: now)
        return pendingDevice
    }
}
