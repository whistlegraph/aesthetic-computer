import AVFoundation
import Foundation

/// Fixed-identity notification bell for the right-⌘ quiet-focus
/// toggle. Deliberately on its own tiny AVAudioEngine (the same
/// trick `CrumpleSound` uses) so it is **never** colored by the
/// user's selected instrument: enabling or disabling the board
/// always sounds exactly the same high, clean two-note bell, even
/// when the playable voice is a plugin, sample, radio, or some
/// far-off GM patch.
///
/// `rising: true` chirps up (board armed); `false` chirps down
/// (board released). The pitch order is the only thing that flips,
/// so the two are unmistakable by ear with no visual.
final class FocusCueBeep {
    static let shared = FocusCueBeep()

    private let engine = AVAudioEngine()
    private let player = AVAudioPlayerNode()
    private let sampleRate: Double = 44_100
    private var started = false

    private init() {
        engine.attach(player)
        engine.connect(player, to: engine.mainMixerNode,
                       format: AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                             channels: 1))
        engine.prepare()
    }

    func play(rising: Bool) {
        guard ensureStarted() else { return }
        guard let buffer = makeBuffer(rising: rising) else { return }
        player.scheduleBuffer(buffer, completionHandler: nil)
        if !player.isPlaying { player.play() }
    }

    /// A single ultra-short tick — the tactile "the command press
    /// registered" feedback, fired on every right-⌘ down regardless
    /// of which way the toggle goes. Same fixed engine, so it never
    /// changes with the instrument.
    func click() {
        guard ensureStarted() else { return }
        guard let buffer = makeClickBuffer() else { return }
        player.scheduleBuffer(buffer, completionHandler: nil)
        if !player.isPlaying { player.play() }
    }

    private func ensureStarted() -> Bool {
        if started { return true }
        do {
            try engine.start()
            started = true
            return true
        } catch {
            NSLog("FocusCueBeep: engine start failed — \(error)")
            return false
        }
    }

    /// ~7ms filtered click: a hard noise transient with an instant
    /// exponential collapse. Reads as a dry "tick," not a tone, so
    /// it sits under the bell without muddying it.
    private func makeClickBuffer() -> AVAudioPCMBuffer? {
        let dur = 0.009
        let frameCount = AVAudioFrameCount(sampleRate * dur)
        guard let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                         channels: 1),
              let buffer = AVAudioPCMBuffer(pcmFormat: format,
                                            frameCapacity: frameCount) else { return nil }
        buffer.frameLength = frameCount
        guard let data = buffer.floatChannelData?[0] else { return nil }
        let total = Int(frameCount)
        var last: Float = 0
        for i in 0..<total {
            let t = Float(i) / Float(total)
            let env = expf(-t * 22)
            // Lightly low-passed noise → a click with body, not hiss.
            let n = Float.random(in: -1...1)
            last = last * 0.6 + n * 0.4
            data[i] = last * env * 0.5
        }
        return buffer
    }

    /// One short bell "dink" — a single tone, not two. High
    /// register so the cue cuts cleanly over any instrument without
    /// sounding like a note in the music. Sine core + a soft odd
    /// harmonic for a glassy, faintly-square ring; instant attack,
    /// exp decay = bell, not buzzer. Pitch alone tells the two
    /// apart: enable = high E6, disable = lower A5.
    private func makeBuffer(rising: Bool) -> AVAudioPCMBuffer? {
        let toneDur = 0.12
        let frameCount = AVAudioFrameCount(sampleRate * toneDur)
        guard let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                         channels: 1),
              let buffer = AVAudioPCMBuffer(pcmFormat: format,
                                            frameCapacity: frameCount) else { return nil }
        buffer.frameLength = frameCount
        guard let data = buffer.floatChannelData?[0] else { return nil }

        let freq = rising ? 1318.5 : 880.0   // E6 enable / A5 disable
        let toneSamples = Int(frameCount)
        for j in 0..<toneSamples {
            let t = Double(j) / sampleRate
            let env = Float(exp(-Double(j) / Double(toneSamples) * 5.0))
            let phase = 2.0 * Double.pi * freq * t
            let s = sin(phase) + 0.18 * sin(3.0 * phase)
            var v = Float(s) * env * 0.22
            if v > 0.95 { v = 0.95 }
            if v < -0.95 { v = -0.95 }
            data[j] = v
        }
        return buffer
    }
}
