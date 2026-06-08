import AVFoundation

/// Spacebar = "instant replay backwards." Menu Band ALWAYS keeps a rolling
/// ring buffer of the audio the user just heard; pressing the spacebar
/// snapshots the most-recent few seconds, reverses the samples, and plays
/// that reversed clip once through a dedicated player node. This is the
/// native-notepat "hold space to reverse-play the actual recent audio"
/// gesture (see fedac/native/pieces/notepat.mjs — REVERSE PLAYBACK /
/// INSTANT REPLAY LOOP), ported to AVAudioEngine.
///
/// WHY this routing (no feedback loop):
///   • Recording feeds off the SAME post-effects buffers the waveform tap
///     on `mainMixerNode` already produces (forked in by the synth — see
///     `feed(_:)`). We do NOT install a second tap: AVAudioEngine allows
///     only one tap per bus, and `mainMixerNode` bus 0 is already taken by
///     the visualizer/tape tap. Reusing that fork captures exactly what the
///     user heard (post compressor/limiter) which is what we want to rewind.
///   • Playback goes through our OWN mixer into `engine.mainMixerNode` —
///     downstream of all the trackpad FX (echo/reverb/proximity) so the
///     already-effected captured audio is played DRY and never double-
///     processed.
///   • But `mainMixerNode` is where the recording tap lives, so the reversed
///     audio we play there WOULD get recaptured into the ring and stack a
///     reverse-of-a-reverse echo. To prevent that we GATE RECORDING OFF
///     (`recording = false`) for the duration of reverse playback — exactly
///     what notepat does via `setCapturePaused(true)` / `output_history_paused`.
///     The ring contents are untouched while paused, so a fresh press still
///     replays from the snapshot that existed before playback started.
final class MenuBandRewindVoice {
    /// How far back the ring physically remembers — a rolling 60 s tape
    /// loop (continuously overwritten). Plenty of headroom beyond the
    /// reverse window so longer windows can reach deep into recent play.
    /// (~23 MB at 96 kHz mono float — fine.)
    private let bufferSeconds: Double = 60.0
    /// Fixed reverse window: each Space press reverses the most-recent
    /// `captureSeconds` of the ring. Because capture FREEZES while in reverse
    /// mode (see `recording`) and only resumes when a real note is next
    /// played, repeated presses re-reverse the SAME window from the same
    /// start point — notepat's stutter gesture. Kept short + chunky.
    let captureSeconds: Double = 1.5

    private let player = AVAudioPlayerNode()
    /// Pitch + echo applied to the reverse playback ITSELF, so the trackpad
    /// gesture warps the tape while the spacebar is held. These have to be
    /// dedicated inserts on the rewind sub-chain (not the master fx bus): the
    /// reverse player joins downstream of the master echo/proximity AND
    /// downstream of the capture tap, so it can neither be processed by the
    /// shared inserts nor allowed to feed back into the ring. Pitch is a
    /// source-side shift (rate stays 1.0) like the speech voice; echo is a
    /// private slap delay mirroring the master one's curve.
    private let pitch = AVAudioUnitTimePitch()
    private let echo: AVAudioUnitDelay = {
        let d = AVAudioUnitDelay()
        d.delayTime = 0.33
        d.feedback = 0
        d.lowPassCutoff = 4_000
        d.wetDryMix = 0
        return d
    }()
    private let mixer = AVAudioMixerNode()
    private weak var engine: AVAudioEngine?
    private var attached = false

    // MARK: Ring buffer (single-writer = audio thread, single-reader = main)

    /// Mono float ring of the most-recent captured frames. Capacity is set
    /// once we know the tap's sample rate (in `feed`). Guarded by `ringLock`
    /// because the writer runs on the render/IO thread and the reader (the
    /// snapshot taken in `playReverse`) runs on the main thread.
    private var ring: [Float] = []
    private var ringCapacity = 0
    private var writeIdx = 0
    /// Total frames ever written; lets the reader tell whether the ring has
    /// wrapped (filled) yet, so a fresh launch doesn't reverse a window of
    /// uninitialized zeros.
    private var framesWritten: Int = 0
    private var ringSampleRate: Double = 0
    private let ringLock = NSLock()

    /// Sample rate the player is currently connected at — re-wired to the
    /// ring's capture rate before playback so the reverse plays at the SAME
    /// speed it was recorded (connecting at the engine's default 44.1k while
    /// the ring is 96k made the reverse play slow / pitched down).
    private var connectedRate: Double = 0
    /// Wall-clock start + duration of the in-flight one-shot reverse, so the
    /// strip can position its playhead from the ACTUAL playback offset
    /// (exact, drift-free) instead of a free-running per-frame counter.
    private var playStart: Double = 0
    private var clipDuration: Double = 0

    // MARK: Attach

    /// Wire the rewind player into the engine. Called from
    /// `MenuBandSynth.start()` alongside the other `.attach` calls. Plays
    /// DRY into `mainMixerNode` (see class doc) — NOT through preLimiterMixer.
    func attach(to engine: AVAudioEngine) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(player)
        engine.attach(pitch)
        engine.attach(echo)
        engine.attach(mixer)
        // Connect with engine-derived (nil) formats so the rewind graph
        // tracks the device's sample rate — a hardcoded 44.1k connection
        // breaks (and `scheduleBuffer` asserts) when the user is on a 96 kHz
        // interface or hot-swaps devices. The reversed clip is built in the
        // player's *actual* output format at play time (see `playReverse`).
        // player → pitch → echo → mixer → mainMixerNode: the bend/echo ride
        // the tape but stay private to this DRY sub-chain (no recapture).
        engine.connect(player, to: pitch, format: nil)
        engine.connect(pitch, to: echo, format: nil)
        engine.connect(echo, to: mixer, format: nil)
        engine.connect(mixer, to: engine.mainMixerNode, format: nil)
        mixer.outputVolume = 1.0
        attached = true
    }

    // MARK: Trackpad fx on the tape (driven while the spacebar is held)

    /// Slide the reverse playback's pitch with the same signed bend the
    /// instruments use (one unit = one octave). Source-side, rate-preserving,
    /// clamped to AVAudioUnitTimePitch's ±2 octave range.
    func setBend(amount: Float) {
        pitch.pitch = max(-2400, min(2400, amount * 1200))
    }

    /// Open the private slap echo on the tape, mirroring the master echo's
    /// wet/feedback curve so a right-swipe sounds the same on a rewind as on a
    /// live note.
    func setEcho(amount: Float) {
        let a = max(0, min(1, amount))
        echo.wetDryMix = a * 45
        echo.feedback = a * 78
    }

    // MARK: Capture (audio thread)

    /// Fork point for the synth's existing post-FX tap. The synth calls this
    /// from `ingestWaveformBuffer` (the same buffer the visualizer + tape
    /// see), so we piggy-back on the one allowed tap instead of installing a
    /// conflicting second one. Runs on the render/IO thread — keep it lean
    /// and lock-light.
    func feed(_ buffer: AVAudioPCMBuffer) {
        // Always capture — the tap is on the limiter (pre-mainMixer), so the
        // reverse playback isn't in this signal and can't feed back. This
        // lets the ring keep recording notes played WHILE reversing, so you
        // can build material in both directions.
        guard let ch = buffer.floatChannelData else { return }
        let frames = Int(buffer.frameLength)
        if frames == 0 { return }
        let channels = Int(buffer.format.channelCount)
        let rate = buffer.format.sampleRate
        if rate <= 0 { return }

        ringLock.lock()
        // (Re)allocate the ring the first time, or if the device sample rate
        // changed under us (aux cable / AirPods switch flips the tap format).
        if ring.isEmpty || ringSampleRate != rate {
            ringCapacity = Swift.max(1, Int(rate * bufferSeconds))
            ring = [Float](repeating: 0, count: ringCapacity)
            writeIdx = 0
            framesWritten = 0
            ringSampleRate = rate
        }
        var idx = writeIdx
        let cap = ringCapacity
        if channels >= 2 {
            // Down-mix L+R to mono so the reversed clip matches what notepat
            // captures (a single mixed channel).
            let l = ch[0]
            let r = ch[1]
            for i in 0..<frames {
                ring[idx] = (l[i] + r[i]) * 0.5
                idx += 1
                if idx >= cap { idx = 0 }
            }
        } else {
            let m = ch[0]
            for i in 0..<frames {
                ring[idx] = m[i]
                idx += 1
                if idx >= cap { idx = 0 }
            }
        }
        writeIdx = idx
        framesWritten += frames
        ringLock.unlock()
    }

    // MARK: Playback (main thread)

    /// Snapshot the most-recent `minCaptureSeconds`…`bufferSeconds` of audio,
    /// reverse it, and play it once. Called on the main thread from the
    /// controller's spacebar handler. Returns false if there was nothing
    /// meaningful to rewind (so the caller can fall back to a cue tone).
    @discardableResult
    func playReverse() -> Bool {
        guard attached, let engine = engine else { return false }

        // --- Snapshot the ring (reader side of the single-writer ring) ---
        ringLock.lock()
        let cap = ringCapacity
        let rate = ringSampleRate
        guard cap > 0, rate > 0 else { ringLock.unlock(); return false }
        // How many valid frames exist: the whole ring once it has wrapped,
        // otherwise only what's been written so far.
        let valid = Swift.min(framesWritten, cap)
        // Fixed window: the most-recent `captureSeconds`. Because capture is
        // frozen across the whole reverse session (until a note is next
        // played), the ring head doesn't move, so every press reverses the
        // SAME window from the same start point.
        let want = Int(captureSeconds * rate)
        let count = Swift.min(valid, Swift.min(want, cap))
        guard count >= 256 else { ringLock.unlock(); return false }
        // Walk backward from the write head, emitting samples in REVERSE
        // order directly (newest-first → the clip plays time-backwards).
        var reversed = [Float](repeating: 0, count: count)
        var readIdx = writeIdx - 1
        if readIdx < 0 { readIdx += cap }
        for i in 0..<count {
            reversed[i] = ring[readIdx]
            readIdx -= 1
            if readIdx < 0 { readIdx += cap }
        }
        ringLock.unlock()

        // --- Build the clip at the RING's capture rate (mono) ---
        // The samples were captured at `rate`; the clip MUST declare that
        // same rate or it plays at the wrong speed. We then (re)connect the
        // player at this rate so `scheduleBuffer` matches and the engine
        // resamples to the device downstream of our mixer.
        guard let clipFormat = AVAudioFormat(commonFormat: .pcmFormatFloat32,
                                             sampleRate: rate, channels: 1,
                                             interleaved: false),
              let clip = AVAudioPCMBuffer(pcmFormat: clipFormat,
                                          frameCapacity: AVAudioFrameCount(count)),
              let dst = clip.floatChannelData?[0]
        else { return false }
        reversed.withUnsafeBufferPointer { src in
            guard let base = src.baseAddress else { return }
            dst.update(from: base, count: count)
        }
        clip.frameLength = AVAudioFrameCount(count)
        if connectedRate != rate {
            // Re-wire only the player→pitch segment to the clip's rate; the
            // rest of the sub-chain stays device-rate (the AUs resample).
            engine.connect(player, to: pitch, format: clipFormat)
            connectedRate = rate
        }

        // --- Play DRY into mainMixerNode ---
        // The ring keeps recording throughout (its tap is on the limiter,
        // upstream of where this reverse player joins), so notes played while
        // reversing land in the buffer and the next press reverses them too.
        if !engine.isRunning { try? engine.start() }
        player.stop()
        // One-shot reverse (notepat's loop:false): plays the captured window
        // backward once and falls into silence. Press again to re-reverse.
        player.scheduleBuffer(clip, at: nil, options: [], completionHandler: nil)
        if !player.isPlaying { player.play() }
        clipDuration = Double(count) / rate
        playStart = CACurrentMediaTime()
        return true
    }

    /// Progress (0…1) through the current one-shot reverse, from wall-clock
    /// elapsed vs the clip duration (the clip plays at real time). Drives the
    /// strip's playhead so the visual is exactly aligned with the audio.
    /// Returns nil when nothing is reverse-playing.
    func reverseProgress() -> Double? {
        guard clipDuration > 0 else { return nil }
        let p = (CACurrentMediaTime() - playStart) / clipDuration
        guard p >= 0 else { return nil }
        return Swift.min(1, p)
    }

    /// Space released — stop the reverse voice. Capture stays FROZEN (it only
    /// resumes on the next note) so repeated presses re-reverse the same
    /// window.
    func release() {
        if player.isPlaying { player.stop() }
    }

}
