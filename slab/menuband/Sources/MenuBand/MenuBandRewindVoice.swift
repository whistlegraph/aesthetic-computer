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
    /// How far back the ring physically remembers. Native notepat caps a
    /// single reverse window at REVERSE_MAX_AGE_MS = 12 s; we keep that as
    /// the ring ceiling so the buffer is generously sized.
    private let bufferSeconds: Double = 12.0
    /// Window actually reversed per spacebar press. Notepat grabs "audio
    /// since the previous press" (floored to a 500 ms minimum); Menu Band's
    /// spacebar is a simple one-shot (no press-pair state), so we reverse a
    /// fixed recent window. ~4 s is long enough to hear a phrase rewind but
    /// short enough to feel like an instant replay.
    private let captureSeconds: Double = 4.0
    /// Floor so a brief history still yields an audible clip.
    private let minCaptureSeconds: Double = 0.5

    private let player = AVAudioPlayerNode()
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

    /// When false, incoming buffers are dropped on the floor. Held false
    /// while reverse playback is sounding so the player's own output (which
    /// flows through the tapped mainMixerNode) is not recaptured into the
    /// ring. Toggled on the main thread; read on the audio thread — a plain
    /// Bool is fine (single-word, set-then-read, no torn state that matters).
    private var recording = true

    /// Fixed render format for the player. The captured ring is mono; we
    /// connect the player as mono and let the engine up-mix to the device.
    private let renderFormat = AVAudioFormat(
        commonFormat: .pcmFormatFloat32, sampleRate: 44_100,
        channels: 1, interleaved: false)!

    // MARK: Attach

    /// Wire the rewind player into the engine. Called from
    /// `MenuBandSynth.start()` alongside the other `.attach` calls. Plays
    /// DRY into `mainMixerNode` (see class doc) — NOT through preLimiterMixer.
    func attach(to engine: AVAudioEngine) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(player)
        engine.attach(mixer)
        engine.connect(player, to: mixer, format: renderFormat)
        engine.connect(mixer, to: engine.mainMixerNode, format: renderFormat)
        mixer.outputVolume = 1.0
        attached = true
    }

    // MARK: Capture (audio thread)

    /// Fork point for the synth's existing post-FX tap. The synth calls this
    /// from `ingestWaveformBuffer` (the same buffer the visualizer + tape
    /// see), so we piggy-back on the one allowed tap instead of installing a
    /// conflicting second one. Runs on the render/IO thread — keep it lean
    /// and lock-light.
    func feed(_ buffer: AVAudioPCMBuffer) {
        guard recording else { return }
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
        // Capture window: the most-recent `captureSeconds`, floored to the
        // minimum so a quiet/short history still yields an audible clip.
        let want = Swift.max(Int(minCaptureSeconds * rate),
                             Int(captureSeconds * rate))
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

        // --- Build a mono PCM buffer at the captured rate ---
        guard let clipFormat = AVAudioFormat(commonFormat: .pcmFormatFloat32,
                                             sampleRate: rate,
                                             channels: 1, interleaved: false),
              let clip = AVAudioPCMBuffer(pcmFormat: clipFormat,
                                          frameCapacity: AVAudioFrameCount(count)),
              let dst = clip.floatChannelData?[0]
        else { return false }
        reversed.withUnsafeBufferPointer { src in
            dst.update(from: src.baseAddress!, count: count)
        }
        clip.frameLength = AVAudioFrameCount(count)

        // --- Pause capture, then play DRY into mainMixerNode ---
        // Gate recording off for the playback duration so the reversed audio
        // (which flows through the tapped mainMixerNode) is not recaptured.
        recording = false
        if !engine.isRunning { try? engine.start() }
        player.stop()
        // The player was connected with `renderFormat` (44.1k mono). If the
        // device is running at a different rate the engine resamples between
        // our mixer and mainMixerNode; scheduling a clip at the captured rate
        // is fine because AVAudioPlayerNode honours the buffer's own format.
        let frameDuration = Double(count) / rate
        player.scheduleBuffer(clip, at: nil, options: []) { [weak self] in
            // Completion fires on a render thread — bounce to main to resume
            // recording and re-arm the ring.
            DispatchQueue.main.async { self?.finishReverse() }
        }
        if !player.isPlaying { player.play() }
        // Safety net: if the completion handler is ever swallowed (engine
        // reconfig mid-clip), resume recording shortly after the clip's
        // natural end so we don't get stuck never recording again.
        DispatchQueue.main.asyncAfter(deadline: .now() + frameDuration + 0.1) {
            [weak self] in self?.finishReverse()
        }
        return true
    }

    /// Stop any in-flight reverse clip and resume recording. Idempotent —
    /// both the completion handler and the safety-net timer call it.
    func stop() {
        finishReverse()
    }

    private func finishReverse() {
        if player.isPlaying { player.stop() }
        // Resume capture. The ring kept its pre-playback contents (we only
        // skipped writes), so the live audio simply continues from here.
        recording = true
    }
}
