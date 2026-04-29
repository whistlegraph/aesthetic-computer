import AVFoundation

/// Tiny synthesized tick — a few-millisecond damped sine, used for rollover
/// feedback when the cursor crosses between menubar piano keys or enters
/// the settings chip. We synthesize instead of using NSSound("Tink") so we
/// can tune pitch + duration to "barely-there" without resorting to bundled
/// WAVs.
///
/// Single shared engine + reusable player node (cheap, ~0 latency once
/// `engine.start()` has run). Playing while the previous tick is still
/// ringing just enqueues the next buffer — overlapping rollovers don't
/// drop, they layer.
final class MenuBandClick {
    private let engine = AVAudioEngine()
    private let player = AVAudioPlayerNode()
    private let format: AVAudioFormat

    init() {
        let sampleRate: Double = 44100
        format = AVAudioFormat(standardFormatWithSampleRate: sampleRate, channels: 1)!
        engine.attach(player)
        engine.connect(player, to: engine.mainMixerNode, format: format)
        do {
            try engine.start()
        } catch {
            NSLog("MenuBandClick engine.start failed: \(error)")
        }
    }

    /// Schedule a click. Defaults are tuned for rollover-between-keys: very
    /// high pitch, very short.
    /// - Parameters:
    ///   - frequency: tone frequency in Hz (5–8 kHz reads as "tiny tick").
    ///   - durationMs: total duration in milliseconds (4–10 reads as
    ///     instantaneous; longer than ~15 starts to sound like a "blip").
    ///   - volume: peak amplitude (0…1). 0.10–0.25 sits under speech.
    func play(frequency: Float = 5800, durationMs: Float = 5, volume: Float = 0.18) {
        guard engine.isRunning else { return }
        let sampleRate = Float(format.sampleRate)
        let frames = AVAudioFrameCount(max(1, sampleRate * durationMs / 1000.0))
        guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames) else { return }
        buffer.frameLength = frames
        guard let chan = buffer.floatChannelData?[0] else { return }
        let omega = 2 * Float.pi * frequency / sampleRate
        // ~0.6ms linear attack so the front edge isn't a hard click; rest is
        // an exponential decay to silence so the tick has a percussive tail
        // without sounding like a sustained tone.
        let attackFrames = max(1, Int(sampleRate * 0.0006))
        let totalFrames = Int(frames)
        for i in 0..<totalFrames {
            let env: Float
            if i < attackFrames {
                env = Float(i) / Float(attackFrames)
            } else {
                let decayPos = Float(i - attackFrames) / Float(max(1, totalFrames - attackFrames))
                env = exp(-5 * decayPos)
            }
            chan[i] = sin(omega * Float(i)) * env * volume
        }
        player.scheduleBuffer(buffer, completionHandler: nil)
        if !player.isPlaying { player.play() }
    }
}
