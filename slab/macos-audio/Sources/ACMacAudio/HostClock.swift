import AudioToolbox
import Foundation

public enum ACHostClock {
    @inline(__always)
    public static func nowNanos() -> UInt64 {
        AudioConvertHostTimeToNanos(AudioGetCurrentHostTime())
    }

    @inline(__always)
    public static func hostTime(fromNanos nanos: UInt64) -> UInt64 {
        AudioConvertNanosToHostTime(nanos)
    }

    @inline(__always)
    public static func nanos(fromHostTime hostTime: UInt64) -> UInt64 {
        AudioConvertHostTimeToNanos(hostTime)
    }
}

final class ACClockSynchronizer: @unchecked Sendable {
    struct Sample {
        let roundTrip: UInt64
        let offset: Int64 // server clock - client clock
    }

    private let lock = NSLock()
    private var samples: [Sample] = []
    private var value: Int64?

    var offsetNanos: Int64? {
        lock.lock(); defer { lock.unlock() }
        return value
    }

    func add(_ response: ACRoomClockResponse, arrived clientReceive: UInt64) {
        let t0 = Int64(clamping: response.clientSendNanos)
        let t1 = Int64(clamping: response.serverReceiveNanos)
        let t2 = Int64(clamping: response.serverSendNanos)
        let t3 = Int64(clamping: clientReceive)
        let network = max(Int64(0), (t3 - t0) - (t2 - t1))
        let estimate = ((t1 - t0) + (t2 - t3)) / 2
        lock.lock()
        samples.append(.init(roundTrip: UInt64(network), offset: estimate))
        if samples.count > 24 { samples.removeFirst(samples.count - 24) }
        // Minimum-RTT samples carry the least queueing error. Smooth the best
        // four so one timestamp does not jerk the audio image sideways.
        let best = samples.sorted { $0.roundTrip < $1.roundTrip }.prefix(4)
        let target = best.reduce(Int64(0)) { $0 + $1.offset } / Int64(max(1, best.count))
        if let old = value { value = old + (target - old) / 8 } else { value = target }
        lock.unlock()
    }
}
