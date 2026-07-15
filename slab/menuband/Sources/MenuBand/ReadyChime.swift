import AVFoundation

/// A short crystalline "release" chime — a rising bell arpeggio that rings when
/// a finished take DMG lands on the Desktop. Runs on its own tiny engine so it
/// never touches the instrument's audio graph (mirrors `CountInWhoosh`).
final class ReadyChime {
    static let shared = ReadyChime()

    private let engine = AVAudioEngine()
    private let player = AVAudioPlayerNode()
    private let sampleRate = 44_100.0
    private var started = false

    private func ensureStarted() {
        guard !started else { return }
        engine.attach(player)
        guard let fmt = AVAudioFormat(standardFormatWithSampleRate: sampleRate, channels: 2) else { return }
        engine.connect(player, to: engine.mainMixerNode, format: fmt)
        do { try engine.start(); started = true }
        catch { NSLog("ReadyChime: engine start failed — \(error)") }
    }

    /// Ring it: a staggered bell arpeggio spelling a bright D major add-shimmer,
    /// each note a decaying sine + a glassy 2nd partial, panned to fan out.
    func play() {
        ensureStarted()
        guard started,
              let fmt = AVAudioFormat(standardFormatWithSampleRate: sampleRate, channels: 2)
        else { return }

        let dur = 1.4
        let n = Int(sampleRate * dur)
        guard let buf = AVAudioPCMBuffer(pcmFormat: fmt, frameCapacity: AVAudioFrameCount(n)),
              let left = buf.floatChannelData?[0],
              let right = buf.floatChannelData?[1]
        else { return }
        buf.frameLength = AVAudioFrameCount(n)

        struct Bell { let freq: Double; let start: Double; let pan: Double }
        let bells: [Bell] = [
            Bell(freq:  587.33, start: 0.00, pan: -0.35),  // D5
            Bell(freq:  739.99, start: 0.08, pan: -0.10),  // F#5
            Bell(freq:  880.00, start: 0.16, pan:  0.15),  // A5
            Bell(freq: 1174.66, start: 0.26, pan:  0.35),  // D6 shimmer
        ]
        let twoPi = 2.0 * Double.pi
        for i in 0..<n {
            let t = Double(i) / sampleRate
            var l = 0.0, r = 0.0
            for b in bells {
                let dt = t - b.start
                if dt < 0 { continue }
                let env = exp(-dt * 3.0)                       // bell-like decay
                let s = sin(twoPi * b.freq * dt) * 0.6
                      + sin(twoPi * b.freq * 2 * dt) * 0.16    // glassy overtone
                let amp = env * 0.30
                l += s * amp * (0.5 - b.pan * 0.5)
                r += s * amp * (0.5 + b.pan * 0.5)
            }
            left[i]  = Float(l)
            right[i] = Float(r)
        }

        player.stop()
        player.scheduleBuffer(buf, at: nil, options: [], completionHandler: nil)
        player.play()
    }
}
