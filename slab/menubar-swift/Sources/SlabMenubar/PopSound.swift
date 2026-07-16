import AVFoundation
import Foundation

/// The little blip the zoom lens pops in and out with.
///
/// Synthesized rather than shipped as an asset, because that's what lets the
/// *pitch* carry the direction — a rising octave going in, the same octave
/// falling coming out. That's the whole reason it reads as "pop in" and "pop
/// out" instead of just "a sound happened", and you can't get it by playing
/// Pop.aiff twice.
///
/// The shape matters more than the notes: a near-instant attack and a steep
/// exponential decay is a *pop*; ease either one and it turns into a beep.
enum PopSound {
    private static let sampleRate = 44_100.0
    private static let duration = 0.085
    /// An octave, which is the smallest interval that reads unambiguously as
    /// "up" or "down" in a sound this short.
    private static let low = 540.0
    private static let high = 1_080.0
    /// Quiet. This fires on a hotkey someone may hit all day.
    private static let gain = 0.16

    private static let queue = DispatchQueue(label: "computer.slab.zoompop", qos: .userInitiated)
    private static let engine = AVAudioEngine()
    private static let player = AVAudioPlayerNode()
    private static let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                              channels: 1)
    private static var running = false

    /// Deliberately NOT gated on the menubar's mute. That toggle silences the
    /// *ambient* sonification — the background hum of session status, noise you
    /// didn't ask for. This is the acknowledgement of a key you just pressed, and
    /// muting the ambient track shouldn't cost you the feedback on your own
    /// keystroke.
    static func play(rising: Bool) {
        queue.async {
            guard let buffer = render(rising: rising), start() else { return }
            // .interrupts: a fast ⌃⌃ ⌃⌃ should re-pop, not queue a backlog.
            player.scheduleBuffer(buffer, at: nil, options: .interrupts)
            player.play()
        }
    }

    /// The tiny mechanical catch when a pointer-follow pan settles onto its new
    /// window. Kept separate from the rising/falling lens pop: this says "locked
    /// on", not "zoom mode changed".
    static func playTransferClick() {
        queue.async {
            guard let buffer = renderTransferClick(), start() else { return }
            player.scheduleBuffer(buffer, at: nil, options: .interrupts)
            player.play()
        }
    }

    /// Lazy, so an app that never zooms never spins up an audio engine.
    private static func start() -> Bool {
        if running { return true }
        guard let format = format else { return false }
        engine.attach(player)
        engine.connect(player, to: engine.mainMixerNode, format: format)
        do {
            try engine.start()
        } catch {
            NSLog("slab zoom lens: audio engine failed to start: \(error)")
            return false
        }
        running = true
        return true
    }

    private static func render(rising: Bool) -> AVAudioPCMBuffer? {
        guard let format = format else { return nil }
        let frames = AVAudioFrameCount(sampleRate * duration)
        guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames),
              let samples = buffer.floatChannelData?[0] else { return nil }
        buffer.frameLength = frames

        var phase = 0.0
        for i in 0..<Int(frames) {
            let t = Double(i) / Double(frames)          // 0…1 through the blip
            // Glide the octave exponentially — equal ratios per unit time is what
            // the ear hears as a straight line.
            let f = rising ? low * pow(high / low, t)
                           : high * pow(low / high, t)
            phase += 2 * .pi * f / sampleRate
            let attack = min(1.0, t / 0.02)             // ~2ms — a click, not a swell
            let decay = exp(-5.5 * t)
            samples[i] = Float(sin(phase) * attack * decay * gain)
        }
        return buffer
    }

    private static func renderTransferClick() -> AVAudioPCMBuffer? {
        guard let format = format else { return nil }
        let clickDuration = 0.052
        let frames = AVAudioFrameCount(sampleRate * clickDuration)
        guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames),
              let samples = buffer.floatChannelData?[0] else { return nil }
        buffer.frameLength = frames

        // A seeded noise tick supplies the physical "catch"; the short falling
        // partial underneath gives it a definite pitch without becoming a beep.
        var noise: UInt32 = 0x51ab_cafe
        var phase = 0.0
        for i in 0..<Int(frames) {
            let t = Double(i) / Double(frames)
            noise = 1_664_525 &* noise &+ 1_013_904_223
            let white = Double(Int32(bitPattern: noise)) / Double(Int32.max)
            let frequency = 1_340.0 * pow(620.0 / 1_340.0, t)
            phase += 2 * .pi * frequency / sampleRate
            let body = sin(phase) * exp(-8.5 * t)
            let tick = white * exp(-28 * t)
            samples[i] = Float((body * 0.12 + tick * 0.07) * (1 - t))
        }
        return buffer
    }
}
