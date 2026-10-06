import Foundation

/// Predictive headroom for overlapping melodic voices, including linger.
/// Same-pitch voices can add coherently; different pitches share a power
/// budget. Reserve a little extra space rather than fully normalizing that
/// worst case: the downstream compressor already controls measured level.
/// Expression follows the actual linger fade, not just held keys.
/// Control threads write events; the main-thread mix timer reads the gain.
final class MelodicHeadroom {
    private struct Voice {
        let note: UInt8
        let channel: UInt8
        let level: Float
        var releasedAt: TimeInterval?
        var releaseLevel: Float = 0
    }

    private let lock = NSLock()
    private var voices: [UInt16: Voice] = [:]
    private var releases: [Voice] = []
    private var expression = [Float](repeating: 1, count: 16)
    private var gain: Float = 1
    private var lastUpdate: TimeInterval?
    private static let releaseSeconds = 0.3

    func noteOn(_ note: UInt8, velocity: UInt8, channel: UInt8, at now: TimeInterval) {
        guard channel < 16, channel != 9 else { return }
        lock.lock(); defer { lock.unlock() }
        let key = UInt16(channel) << 8 | UInt16(note)
        if let old = voices.removeValue(forKey: key) { release(old, at: now) }
        // Ordinary keyboard velocity is 100. Quiet notes should not reserve
        // a full voice's headroom; harder hits may use more of the budget.
        voices[key] = Voice(note: note, channel: channel,
                            level: Float(velocity) / 100)
    }

    func noteOff(_ note: UInt8, channel: UInt8, at now: TimeInterval) {
        lock.lock(); defer { lock.unlock() }
        let key = UInt16(channel) << 8 | UInt16(note)
        if let old = voices.removeValue(forKey: key) { release(old, at: now) }
    }

    private func release(_ voice: Voice, at now: TimeInterval) {
        releases.removeAll { now - ($0.releasedAt ?? now) >= Self.releaseSeconds }
        var tail = voice
        tail.releasedAt = now
        // Freeze this value: resetting a recycled channel's expression must
        // not tell the automixer that its old release became a fresh voice.
        tail.releaseLevel = voice.level * expression[Int(voice.channel)]
        releases.append(tail)
    }

    func setExpression(_ value: UInt8, channel: UInt8) {
        guard channel < 16 else { return }
        lock.lock(); defer { lock.unlock() }
        expression[Int(channel)] = Float(value) / 127
    }

    /// Fast attenuation on an incoming voice; 250 ms recovery avoids gain
    /// jumps when a channel is recycled or a group of notes is released.
    func nextGain(at now: TimeInterval) -> Float {
        lock.lock(); defer { lock.unlock() }
        let elapsed = max(0, now - (lastUpdate ?? now))
        lastUpdate = now
        releases.removeAll { now - ($0.releasedAt ?? now) >= Self.releaseSeconds }
        var pitches = [Float](repeating: 0, count: 128)
        for voice in voices.values {
            pitches[Int(voice.note & 0x7F)] += voice.level * expression[Int(voice.channel)]
        }
        for voice in releases {
            let remaining = max(0, 1 - (now - voice.releasedAt!) / Self.releaseSeconds)
            pitches[Int(voice.note & 0x7F)] += voice.releaseLevel * Float(remaining)
        }
        let power = pitches.reduce(Float(0)) { $0 + $1 * $1 }
        // Four full-level copies of one pitch reserve 3 dB. A four-note
        // chord reserves 1.5 dB. Full inverse-amplitude normalization would
        // over-duck phase-cancelling repeats and quiet instrument patches.
        // Leave the remaining dynamics to the measured compressor/limiter.
        let target = max(0.5, pow(max(1, power), -0.125))
        if target < gain { gain = target }
        else { gain += (target - gain) * Float(1 - exp(-elapsed / 0.25)) }
        if target == 1, gain > 0.999 { gain = 1 }
        return gain
    }

    var isIdle: Bool {
        lock.lock(); defer { lock.unlock() }
        return voices.isEmpty && releases.isEmpty && gain == 1
    }

    func reset() {
        lock.lock(); defer { lock.unlock() }
        voices.removeAll(keepingCapacity: true)
        releases.removeAll(keepingCapacity: true)
        expression = [Float](repeating: 1, count: 16)
        gain = 1
        lastUpdate = nil
    }
}
