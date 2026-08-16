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

    /// The Tab handoff chime — which trackpad page just took over. A low,
    /// soft-attacked sine in the airport-PA register, deliberately unlike
    /// the high focus bell so "mode changed" and "focus changed" never blur:
    /// TrackDrum lands on G4, the pitch slider a fifth below on C4. Pitch
    /// alone says which surface is now under the fingers.
    func padSwitch(toTrackDrum: Bool) {
        guard ensureStarted() else { return }
        guard let buffer = makePadSwitchBuffer(toTrackDrum: toTrackDrum) else {
            return
        }
        player.scheduleBuffer(buffer, completionHandler: nil)
        if !player.isPlaying { player.play() }
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

    /// ~0.3s rounded sine "boop": an 8 ms cosine ramp in (no click
    /// transient — this is the mellow PA chime, not the tick), pure
    /// fundamental with a whisper of second harmonic for body, then an
    /// exponential settle.
    private func makePadSwitchBuffer(toTrackDrum: Bool) -> AVAudioPCMBuffer? {
        let dur = 0.3
        let frameCount = AVAudioFrameCount(sampleRate * dur)
        guard let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                                         channels: 1),
              let buffer = AVAudioPCMBuffer(pcmFormat: format,
                                            frameCapacity: frameCount) else {
            return nil
        }
        buffer.frameLength = frameCount
        guard let data = buffer.floatChannelData?[0] else { return nil }
        let freq = toTrackDrum ? 392.0 : 261.6   // G4 drum / C4 slider
        let total = Int(frameCount)
        let attack = Int(sampleRate * 0.008)
        for i in 0..<total {
            let t = Double(i) / sampleRate
            let ramp: Double = i < attack
                ? 0.5 - 0.5 * cos(.pi * Double(i) / Double(attack))
                : 1
            let env = ramp * exp(-Double(i) / Double(total) * 4.5)
            let phase = 2.0 * Double.pi * freq * t
            let s = sin(phase) + 0.10 * sin(2.0 * phase)
            data[i] = Float(s * env * 0.20)
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
