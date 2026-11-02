import AVFoundation
import Foundation

/// Which trackpad page Tab just handed the fingers to. Each page announces
/// itself in its own voice, so the ear never has to decode a pitch table:
/// TrackDrum strikes a real kick, the pitch/echo slider lets fall one
/// echoing water droplet (bent like the next note would be), and handing
/// the trackpad back to the mouse lands a dry, dead tock — nothing
/// musical, because nothing musical is under the fingers anymore.
enum TrackpadPageCue: Equatable {
    case drum, slider, off

    /// Where the drum page's announcing hit lands on the skin: dead
    /// center, the kick zone — the same strike `selectDrumKit` auditions.
    static let drumStrike = CGPoint(x: 0.5, y: 0.5)
    static let drumVelocity: UInt8 = 96

    /// How long the slider droplet rings, echoes included.
    static let dropletSeconds = 0.5

    /// The slider page's cue: one water droplet with its own echo. The
    /// drop is a ~50 ms sine that chirps up a tenth from `pitch` × 520 Hz
    /// (a drop into a pool pitches up as the cavity closes), then it comes
    /// back three times at 130 ms, each repeat quieter and duller. Shared
    /// by the fx-bus cue (which bends `pitch` with the wheel) and the dry
    /// fallback (pitch 1), so the two never drift apart in character.
    static func fillDroplet(_ data: UnsafeMutablePointer<Float>, frames: Int,
                            sampleRate: Double, pitch: Double) {
        for i in 0..<frames { data[i] = 0 }
        let taps: [(delay: Double, gain: Double, overtone: Double)] = [
            (0.00, 1.00, 0.30), (0.13, 0.48, 0.18), (0.26, 0.24, 0.08), (0.39, 0.12, 0.0),
        ]
        let f0 = 520.0 * pitch
        let attack = Int(sampleRate * 0.0015)
        for tap in taps {
            let start = Int(tap.delay * sampleRate)
            let length = min(frames - start, Int(sampleRate * 0.11))
            guard length > 0 else { continue }
            var phase = 0.0
            for j in 0..<length {
                let t = Double(j) / sampleRate
                // Chirp: up a tenth over the first 50 ms, then hold.
                let freq = f0 * pow(2.5, min(1, t / 0.05))
                phase += 2.0 * Double.pi * freq / sampleRate
                let ramp: Double = j < attack
                    ? 0.5 - 0.5 * cos(.pi * Double(j) / Double(attack))
                    : 1
                let env = ramp * exp(-t * 48.0)
                let s = sin(phase) + tap.overtone * sin(2.0 * phase)
                data[start + j] += Float(s * env * tap.gain * 0.22)
            }
        }
    }
}

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

    /// REC just started: two quick ticks and the rising bell — unlike the
    /// plain focus bell, so the ear knows the tape is now rolling.
    func recordStart() {
        guard ensureStarted() else { return }
        guard let tick = makeClickBuffer(), let bell = makeBuffer(rising: true) else { return }
        let fmt = tick.format
        guard let gap = AVAudioPCMBuffer(pcmFormat: fmt, frameCapacity: AVAudioFrameCount(fmt.sampleRate * 0.09)) else { return }
        gap.frameLength = gap.frameCapacity
        if let d = gap.floatChannelData { for ch in 0..<Int(fmt.channelCount) { memset(d[ch], 0, Int(gap.frameLength) * MemoryLayout<Float>.size) } }
        player.stop()
        player.scheduleBuffer(tick, completionHandler: nil)
        player.scheduleBuffer(gap, completionHandler: nil)
        player.scheduleBuffer(tick, completionHandler: nil)
        player.scheduleBuffer(gap, completionHandler: nil)
        player.scheduleBuffer(bell, completionHandler: nil)
        player.play()
    }

    /// The Tab handoff cue — which trackpad page just took over — on the
    /// isolated engine. The live path (`MenuBandSurfaceCue` + the real
    /// TrackDrum skin) is preferred; this is the stand-in while the synth
    /// engine is down, so each page keeps the same character: a synthetic
    /// kick for TrackDrum, the echoing droplet for the slider, a dry tock for off.
    func padSwitch(_ cue: TrackpadPageCue) {
        guard ensureStarted() else { return }
        let buffer: AVAudioPCMBuffer?
        switch cue {
        case .drum: buffer = makeKickBuffer()
        case .slider: buffer = makeSliderDropletBuffer()
        case .off: buffer = makeOffTockBuffer()
        }
        guard let buffer else { return }
        // Flush anything still queued: `scheduleBuffer` appends, so rapid
        // Tab presses would otherwise stack 0.3 s chimes back-to-back and
        // the ear falls behind the page that's actually under the fingers.
        player.stop()
        player.scheduleBuffer(buffer, completionHandler: nil)
        player.play()
    }

    /// The ToneTrials "CLEAR!" fanfare: a quick rising C-major arpeggio in
    /// soft sines — unmistakably a reward, still small enough to sit under
    /// whatever the instrument is ringing.
    func trialClear() {
        guard ensureStarted() else { return }
        guard let buffer = makeTrialClearBuffer() else { return }
        player.scheduleBuffer(buffer, completionHandler: nil)
        if !player.isPlaying { player.play() }
    }

    private func makeTrialClearBuffer() -> AVAudioPCMBuffer? {
        let dur = 0.55
        let frameCount = AVAudioFrameCount(sampleRate * dur)
        guard let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                         channels: 1),
              let buffer = AVAudioPCMBuffer(pcmFormat: format,
                                            frameCapacity: frameCount) else {
            return nil
        }
        buffer.frameLength = frameCount
        guard let data = buffer.floatChannelData?[0] else { return nil }
        let total = Int(frameCount)
        for i in 0..<total { data[i] = 0 }
        // C5 → E5 → G5, each note overlapping the next by half.
        let notes: [(freq: Double, start: Double)] = [
            (523.25, 0.0), (659.25, 0.11), (783.99, 0.22),
        ]
        for note in notes {
            let startFrame = Int(note.start * sampleRate)
            let noteFrames = min(total - startFrame,
                                 Int(sampleRate * 0.30))
            let attack = Int(sampleRate * 0.006)
            for j in 0..<noteFrames {
                let t = Double(j) / sampleRate
                let ramp: Double = j < attack
                    ? 0.5 - 0.5 * cos(.pi * Double(j) / Double(attack))
                    : 1
                let env = ramp * exp(-Double(j) / Double(noteFrames) * 4.0)
                let phase = 2.0 * Double.pi * note.freq * t
                let s = sin(phase) + 0.12 * sin(2.0 * phase)
                data[startFrame + j] += Float(s * env * 0.16)
            }
        }
        return buffer
    }

    private func ensureStarted() -> Bool {
        if started { return true }
        do {
            MenuBandAudioDevices.bindToRawOutput(engine, label: "FocusCueBeep")
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

    private func makeMonoBuffer(seconds: Double) -> AVAudioPCMBuffer? {
        let frameCount = AVAudioFrameCount(sampleRate * seconds)
        guard let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                         channels: 1),
              let buffer = AVAudioPCMBuffer(pcmFormat: format,
                                            frameCapacity: frameCount) else {
            return nil
        }
        buffer.frameLength = frameCount
        return buffer
    }

    /// TrackDrum stand-in: a ~0.18 s kick — a sine dropping from 150 Hz to
    /// 45 Hz under a fast exponential, with a pinch of noise on the beater
    /// transient. Unmistakably a drum, never a tone.
    private func makeKickBuffer() -> AVAudioPCMBuffer? {
        let dur = 0.18
        guard let buffer = makeMonoBuffer(seconds: dur),
              let data = buffer.floatChannelData?[0] else { return nil }
        let total = Int(buffer.frameLength)
        var phase = 0.0
        var last: Float = 0
        for i in 0..<total {
            let t = Double(i) / sampleRate
            let freq = 45.0 + 105.0 * exp(-t * 38.0)
            phase += 2.0 * Double.pi * freq / sampleRate
            let body = sin(phase) * exp(-t * 16.0)
            let n = Float.random(in: -1...1)
            last = last * 0.5 + n * 0.5
            let beater = Double(last) * exp(-t * 220.0) * 0.35
            data[i] = Float((body + beater) * 0.55)
        }
        return buffer
    }

    /// Pitch/echo slider: the echoing water droplet, at concert pitch
    /// (this engine never sees the bend).
    private func makeSliderDropletBuffer() -> AVAudioPCMBuffer? {
        guard let buffer = makeMonoBuffer(seconds: TrackpadPageCue.dropletSeconds),
              let data = buffer.floatChannelData?[0] else { return nil }
        TrackpadPageCue.fillDroplet(data, frames: Int(buffer.frameLength),
                                    sampleRate: sampleRate, pitch: 1)
        return buffer
    }

    /// Trackpad handed back to the mouse: a dry, dead tock — a damped
    /// 220→110 Hz drop over ~60 ms with a woody click on top. No ring, no
    /// pitch to read; it sounds like a switch closing.
    private func makeOffTockBuffer() -> AVAudioPCMBuffer? {
        let dur = 0.07
        guard let buffer = makeMonoBuffer(seconds: dur),
              let data = buffer.floatChannelData?[0] else { return nil }
        let total = Int(buffer.frameLength)
        var phase = 0.0
        var last: Float = 0
        for i in 0..<total {
            let t = Double(i) / sampleRate
            let freq = 110.0 + 110.0 * exp(-t * 90.0)
            phase += 2.0 * Double.pi * freq / sampleRate
            let body = sin(phase) * exp(-t * 55.0)
            let n = Float.random(in: -1...1)
            last = last * 0.7 + n * 0.3
            let click = Double(last) * exp(-t * 400.0) * 0.5
            data[i] = Float((body + click) * 0.4)
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
