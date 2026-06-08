import AVFoundation
import Foundation

/// A short, bright major arpeggio played when the About-window icon is
/// clicked to reveal its QR-code easter egg. Like `FocusCueBeep` and
/// `CrumpleSound`, it runs on its own tiny `AVAudioEngine` so it sounds
/// the same regardless of the user's selected instrument — a fixed
/// "ta-da" sparkle, not a note in whatever patch is loaded.
///
/// The chord is a C-major triad topped with the octave (C5·E5·G5·C6),
/// each note staggered ~45 ms so it reads as a quick upward roll with a
/// glassy sine-plus-odd-harmonic timbre and an exponential bell decay.
final class EasterEggChord {
    static let shared = EasterEggChord()

    private let engine = AVAudioEngine()
    private let player = AVAudioPlayerNode()
    private let sampleRate: Double = 44_100
    private var started = false

    private init() {
        engine.attach(player)
        engine.connect(
            player, to: engine.mainMixerNode,
            format: AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                  channels: 1))
        engine.prepare()
    }

    /// Minimum gap between triggers — rapid clicks beyond this rate are
    /// swallowed so the sparkle doesn't machine-gun.
    private var lastPlay: TimeInterval = 0

    func play() {
        let now = ProcessInfo.processInfo.systemUptime
        guard now - lastPlay > 0.08 else { return }
        lastPlay = now
        guard ensureStarted() else { return }
        guard let buffer = makeChordBuffer() else { return }
        // `.interrupts` cancels any still-ringing chord and starts this
        // one immediately, so overlapping clicks replace rather than
        // pile up into a muddy stack.
        player.scheduleBuffer(buffer, at: nil, options: .interrupts,
                              completionHandler: nil)
        if !player.isPlaying { player.play() }
    }

    private func ensureStarted() -> Bool {
        if started { return true }
        do {
            try engine.start()
            started = true
            return true
        } catch {
            NSLog("EasterEggChord: engine start failed — \(error)")
            return false
        }
    }

    /// Sum of four staggered bell partials into one mono buffer. Each
    /// note: instant attack, exponential decay, sine core + a soft odd
    /// harmonic for a faint glassy ring. Onsets march up by `stagger`
    /// so the chord blooms instead of striking flat.
    private func makeChordBuffer() -> AVAudioPCMBuffer? {
        let freqs: [Double] = [523.25, 659.25, 783.99, 1046.50] // C5 E5 G5 C6
        let stagger = 0.045          // seconds between note onsets
        let noteDur = 0.55           // per-note tail
        let total = stagger * Double(freqs.count - 1) + noteDur
        let frameCount = AVAudioFrameCount(sampleRate * total)
        guard let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                         channels: 1),
              let buffer = AVAudioPCMBuffer(pcmFormat: format,
                                            frameCapacity: frameCount)
        else { return nil }
        buffer.frameLength = frameCount
        guard let data = buffer.floatChannelData?[0] else { return nil }

        let totalSamples = Int(frameCount)
        for i in 0..<totalSamples { data[i] = 0 }

        for (n, freq) in freqs.enumerated() {
            let onset = Int(Double(n) * stagger * sampleRate)
            let noteSamples = Int(sampleRate * noteDur)
            for j in 0..<noteSamples {
                let idx = onset + j
                if idx >= totalSamples { break }
                let t = Double(j) / sampleRate
                let env = Float(exp(-Double(j) / Double(noteSamples) * 4.5))
                let phase = 2.0 * Double.pi * freq * t
                let s = sin(phase) + 0.16 * sin(3.0 * phase)
                data[idx] += Float(s) * env * 0.16
            }
        }

        // Soft-clip the summed mix so overlapping tails never wrap.
        for i in 0..<totalSamples {
            var v = data[i]
            if v > 0.95 { v = 0.95 }
            if v < -0.95 { v = -0.95 }
            data[i] = v
        }
        return buffer
    }
}
