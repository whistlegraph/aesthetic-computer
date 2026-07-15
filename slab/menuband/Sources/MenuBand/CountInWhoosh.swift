import AVFoundation

/// A rising white-noise "suck" for the record count-in — a lowpass that opens
/// as an amplitude swell builds, so it reads as charging up into the downbeat.
/// Runs on its own tiny engine so it never touches the instrument's audio graph.
final class CountInWhoosh {
    static let shared = CountInWhoosh()

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
        catch { NSLog("CountInWhoosh: engine start failed — \(error)") }
    }

    /// Play a `duration`-second charging swell: white noise through a filter
    /// that opens over time, under an amplitude envelope that ramps up.
    func play(duration: Double) {
        ensureStarted()
        guard started,
              let fmt = AVAudioFormat(standardFormatWithSampleRate: sampleRate, channels: 2),
              let buf = AVAudioPCMBuffer(
                pcmFormat: fmt,
                frameCapacity: AVAudioFrameCount(sampleRate * duration)),
              let left = buf.floatChannelData?[0],
              let right = buf.floatChannelData?[1]
        else { return }

        let n = Int(sampleRate * duration)
        buf.frameLength = AVAudioFrameCount(n)
        var lpL: Float = 0, lpR: Float = 0
        for i in 0..<n {
            let t = Float(i) / Float(n)            // 0 … 1
            let env = t * t                         // swell in
            let cutoff = 0.015 + 0.5 * t            // filter opens = the "suck"
            lpL += cutoff * (Float.random(in: -1...1) - lpL)
            lpR += cutoff * (Float.random(in: -1...1) - lpR)
            left[i]  = lpL * env * 0.30      // soft rising hush, a touch more present
            right[i] = lpR * env * 0.30
        }

        player.stop()
        player.scheduleBuffer(buf, at: nil, options: [], completionHandler: nil)
        player.play()
    }

    func stop() {
        if started { player.stop() }
    }
}
