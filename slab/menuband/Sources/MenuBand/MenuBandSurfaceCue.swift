import AVFoundation

/// The FX surface's own voice: the Tab handoff chime plus a quiet friction
/// "rub", both fed into the synth's pre-limiter fx bus (the same wiring as
/// `MenuBandSpeechVoice`) so they ride the REAL space/echo sends. Where
/// `FocusCueBeep` is deliberately isolated and identical every time, these
/// two cues are deliberately colored: the chime sounds at the currently-bent
/// pitch and blooms with whatever echo/space is engaged, and scratching the
/// idle pitch page rubs out a soft noise whose speed follows the bend — so
/// the surface previews its own state without committing a note.
///
/// Pitch is applied at the source (a varispeed for the rub, baked into the
/// synthesized chime buffer) because MIDI pitch-bend only reaches the synth
/// voices — bus players never see it. One unit of `bend` = one octave,
/// matching `MenuBandController.bendSemitonesPerUnit`.
final class MenuBandSurfaceCue {
    private let chimePlayer = AVAudioPlayerNode()
    private let rubPlayer = AVAudioPlayerNode()
    /// Varispeed, not TimePitch: resampling couples pitch and speed, which
    /// is exactly right for a friction texture — bending up makes the rub
    /// skitter faster and higher, like dragging a record. Its 0.25…4 rate
    /// window is precisely the gesture's ±2 octave cap.
    private let rubSpeed = AVAudioUnitVarispeed()
    private let rubMixer = AVAudioMixerNode()
    private weak var engine: AVAudioEngine?
    private var attached = false
    private let sampleRate: Double = 44_100
    private let renderFormat = AVAudioFormat(
        commonFormat: .pcmFormatFloat32, sampleRate: 44_100,
        channels: 1, interleaved: false)!
    private var rubLoop: AVAudioPCMBuffer?
    /// Current rub gain (0…rubLevelCeiling), bumped by movement and decayed
    /// by the timer. Lives on the main thread with every caller.
    private var rubLevel: Float = 0
    private var rubDecayTimer: Timer?
    /// Signed bend in controller units (1 = one octave), mirrored from the
    /// controller's single `setBend` funnel so the rub and the next chime
    /// always speak at the sounding pitch.
    private var bend: Float = 0

    /// Loudest the rub mixer ever opens. The loop peaks near 0.6 and the
    /// fx bus compressor adds makeup gain downstream, so this stays small —
    /// the rub is a cue under the instrument, never a note.
    private static let rubLevelCeiling: Float = 0.5
    /// Per-tick decay at 60 Hz ≈ a 100 ms time constant: long enough that a
    /// continuous scrub sounds continuous, short enough that a resting
    /// finger is silent within a beat.
    private static let rubDecayPerTick: Float = 0.85
    private static let rubDecayHz: Double = 60
    /// Below this the player stops entirely so an idle surface renders
    /// nothing at all.
    private static let rubFloor: Float = 0.005

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(chimePlayer)
        engine.attach(rubPlayer)
        engine.attach(rubSpeed)
        engine.attach(rubMixer)
        engine.connect(chimePlayer, to: output, format: renderFormat)
        engine.connect(rubPlayer, to: rubSpeed, format: renderFormat)
        engine.connect(rubSpeed, to: rubMixer, format: renderFormat)
        engine.connect(rubMixer, to: output, format: nil)
        rubMixer.outputVolume = 0
        rubLoop = Self.makeRubLoop(sampleRate: sampleRate,
                                   format: renderFormat)
        attached = true
    }

    /// Mirror of the controller bend. Applied live so a rub already
    /// sounding slides with the gesture instead of stepping per grain.
    func setBend(amount: Float) {
        let clamped = max(-2, min(2, amount))
        bend = clamped
        rubSpeed.rate = pow(2, clamped)
    }

    /// The Tab handoff chime, at the currently-bent pitch, through the fx
    /// bus. Returns false when the engine can't sound it (caller falls back
    /// to the isolated `FocusCueBeep`). Stopping first flushes any queued
    /// chime so rapid Tab presses always hear the freshest destination —
    /// `scheduleBuffer` on a live player appends, it does not replace.
    func playPadSwitch(toTrackDrum: Bool) -> Bool {
        guard attached, let engine, engine.isRunning else { return false }
        guard let buffer = makePadSwitchBuffer(toTrackDrum: toTrackDrum) else {
            return false
        }
        chimePlayer.stop()
        chimePlayer.scheduleBuffer(buffer, completionHandler: nil)
        chimePlayer.play()
        return true
    }

    /// One movement's worth of friction, `intensity` 0…1 from the caller's
    /// per-event travel. The level snaps up to the target (a rub needs no
    /// attack — the loop has no transient) and the timer eases it back down.
    func rub(intensity: Float) {
        guard attached, let engine, engine.isRunning,
              let loop = rubLoop else { return }
        let target = max(0, min(1, intensity)) * Self.rubLevelCeiling
        rubLevel = max(rubLevel, target)
        guard rubLevel > Self.rubFloor else { return }
        rubMixer.outputVolume = rubLevel
        if !rubPlayer.isPlaying {
            // `stop()` clears the schedule, so each activation re-arms the
            // loop from a random-feeling point (wherever the last stop left
            // the texture is irrelevant — it's noise).
            rubPlayer.scheduleBuffer(loop, at: nil, options: .loops,
                                     completionHandler: nil)
            rubPlayer.play()
        }
        if rubDecayTimer == nil {
            let timer = Timer(timeInterval: 1.0 / Self.rubDecayHz,
                              repeats: true) { [weak self] _ in
                self?.decayRub()
            }
            // Common modes so the decay keeps running while the popover or
            // a menu is tracking — the same reason the bend easer does.
            RunLoop.main.add(timer, forMode: .common)
            rubDecayTimer = timer
        }
    }

    /// Immediate silence — session teardown, page switch, focus loss.
    func stopRub() {
        rubLevel = 0
        rubMixer.outputVolume = 0
        rubDecayTimer?.invalidate()
        rubDecayTimer = nil
        if rubPlayer.isPlaying { rubPlayer.stop() }
    }

    private func decayRub() {
        rubLevel *= Self.rubDecayPerTick
        if rubLevel <= Self.rubFloor {
            stopRub()
            return
        }
        rubMixer.outputVolume = rubLevel
    }

    /// Same DNA as `FocusCueBeep.makePadSwitchBuffer` — G4 names TrackDrum,
    /// C4 the slider — but rendered at the bent pitch. The two stay a fifth
    /// apart under any bend, so the destination remains readable by ear
    /// while the absolute register previews where the wheel sits.
    private func makePadSwitchBuffer(toTrackDrum: Bool) -> AVAudioPCMBuffer? {
        let dur = 0.3
        let frameCount = AVAudioFrameCount(sampleRate * dur)
        guard let buffer = AVAudioPCMBuffer(pcmFormat: renderFormat,
                                            frameCapacity: frameCount) else {
            return nil
        }
        buffer.frameLength = frameCount
        guard let data = buffer.floatChannelData?[0] else { return nil }
        let base = toTrackDrum ? 392.0 : 261.6   // G4 drum / C4 slider
        let freq = base * pow(2.0, Double(bend))
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

    /// ~0.7 s of dark, uneven friction noise that loops seamlessly. Two
    /// cascaded one-pole lowpasses keep it papery rather than hissy (the
    /// varispeed then moves that spectral band audibly with the bend), and
    /// a slow smoothed-noise wander in the amplitude makes it read as a
    /// finger dragging, not steady static. The tail crossfades into the
    /// head so the loop seam never clicks.
    private static func makeRubLoop(
        sampleRate: Double, format: AVAudioFormat
    ) -> AVAudioPCMBuffer? {
        let dur = 0.7
        let frameCount = AVAudioFrameCount(sampleRate * dur)
        guard let buffer = AVAudioPCMBuffer(pcmFormat: format,
                                            frameCapacity: frameCount) else {
            return nil
        }
        buffer.frameLength = frameCount
        guard let data = buffer.floatChannelData?[0] else { return nil }
        let total = Int(frameCount)
        var lp1: Float = 0
        var lp2: Float = 0
        var rough: Float = 0
        var peak: Float = 0
        for i in 0..<total {
            let n = Float.random(in: -1...1)
            lp1 = lp1 * 0.90 + n * 0.10
            lp2 = lp2 * 0.90 + lp1 * 0.10
            // ~70 Hz zero-mean wander, stretched wide enough to hear —
            // the grain of a finger pad catching, not a level tremolo.
            rough = rough * 0.99 + Float.random(in: -1...1) * 0.01
            let v = lp2 * max(0.25, 1 + 3.5 * rough)
            data[i] = v
            peak = max(peak, abs(v))
        }
        if peak > 0 {
            let norm = 0.6 / peak
            for i in 0..<total { data[i] *= norm }
        }
        // Seamless loop: equal-power crossfade the tail into the head, then
        // trim the head off the front. The blended tail now lands exactly
        // where the trimmed buffer's first frame picks up, so the seam is
        // sample-continuous instead of jumping back to the fade's source.
        let fade = min(total / 4, Int(sampleRate * 0.054))
        for j in 0..<fade {
            let mix = Float(j) / Float(fade)
            let tail = total - fade + j
            data[tail] = data[tail] * cos(mix * .pi / 2)
                + data[j] * sin(mix * .pi / 2)
        }
        let trimmed = total - fade
        for i in 0..<trimmed { data[i] = data[i + fade] }
        buffer.frameLength = AVAudioFrameCount(trimmed)
        return buffer
    }
}
