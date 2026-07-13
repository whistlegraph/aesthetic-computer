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

    static func play(rising: Bool) {
        // The menubar's mute is a promise about noise, and this is noise.
        guard !FileManager.default.fileExists(atPath: Paths.muteFlag) else { return }
        queue.async {
            guard let buffer = render(rising: rising), start() else { return }
            // .interrupts: a fast ⌃⌃ ⌃⌃ should re-pop, not queue up a backlog.
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
}
