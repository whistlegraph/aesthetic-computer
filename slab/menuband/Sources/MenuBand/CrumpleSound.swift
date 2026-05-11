import AVFoundation
import Foundation

/// Procedural paper-crumple SFX. Played when the user drags the
/// staff sheet out of the popover — the visual crumple animation
/// pairs with this rip-of-the-page noise.
///
/// Lives on its own tiny AVAudioEngine so it stays independent of
/// MenuBandSynth's graph (which carries the user's musical voices
/// and shouldn't be ducked or interrupted by a UI sound). Buffers
/// are synthesised fresh on every play so back-to-back drags don't
/// sound identical.
final class CrumpleSound {
    static let shared = CrumpleSound()

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

    func play() {
        if !started {
            do {
                try engine.start()
                started = true
            } catch {
                NSLog("CrumpleSound: engine start failed — \(error)")
                return
            }
        }
        guard let buffer = makeBuffer() else { return }
        player.scheduleBuffer(buffer, completionHandler: nil)
        if !player.isPlaying { player.play() }
    }

    /// Build ~0.35s of crinkly noise. Two layers stacked:
    ///   1. Continuous low-amplitude broadband noise — the paper
    ///      "rustle." Tapered by an exp decay so the sound dies
    ///      away rather than cutting off abruptly.
    ///   2. A handful of randomly-timed crackle bursts. Each one
    ///      is a short noise blip with its own decay envelope,
    ///      and a slight random gain — these are the audible
    ///      paper-fold crackles. Random count + placement keeps
    ///      every drag sounding different.
    private func makeBuffer() -> AVAudioPCMBuffer? {
        let duration = 0.35
        let frameCount = AVAudioFrameCount(sampleRate * duration)
        guard let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                         channels: 1),
              let buffer = AVAudioPCMBuffer(pcmFormat: format,
                                            frameCapacity: frameCount) else { return nil }
        buffer.frameLength = frameCount
        guard let data = buffer.floatChannelData?[0] else { return nil }

        let total = Int(frameCount)
        // Layer 1: background rustle.
        let rustleAmp: Float = 0.08
        for i in 0..<total {
            data[i] = Float.random(in: -1...1) * rustleAmp
        }
        // Layer 2: per-crackle bursts.
        let crackleCount = Int.random(in: 7...12)
        let burstSamples = Int(sampleRate * 0.05) // ~50ms per burst
        for _ in 0..<crackleCount {
            let start = Int.random(in: 0..<max(1, total - burstSamples))
            let amp = Float.random(in: 0.35...0.85)
            for j in 0..<burstSamples {
                // Sharp-attack, exp-decay envelope per burst.
                let t = Float(j) / Float(burstSamples)
                let env: Float = exp(-t * 9)
                let n = Float.random(in: -1...1)
                let idx = start + j
                if idx < total {
                    data[idx] += n * env * amp
                }
            }
        }
        // Overall fade so the rustle floor doesn't cut hard at
        // the buffer end — natural-sounding tail off.
        for i in 0..<total {
            let t = Float(i) / Float(total)
            let env: Float = exp(-t * 2.2)
            data[i] *= env
            // Clamp to avoid any digital clipping when bursts pile up.
            if data[i] > 0.95 { data[i] = 0.95 }
            if data[i] < -0.95 { data[i] = -0.95 }
        }
        return buffer
    }
}
