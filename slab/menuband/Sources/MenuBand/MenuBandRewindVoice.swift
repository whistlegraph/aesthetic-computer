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
    /// Floor so a brief history still yields an audible clip.
    private let minCaptureSeconds: Double = 0.5
    /// `CACurrentMediaTime()` of the previous Space press. The reverse window
    /// is "audio SINCE the last press" (notepat semantics) — so rapid taps
    /// reverse successive short chunks (a stutter), and a press after a long
    /// phrase reverses the whole phrase. Floored to `minCaptureSeconds`,
    /// capped at `bufferSeconds`. Set on attach so the first press measures
    /// from launch.
    private var lastPressTime: Double = 0
    /// Bumped each press so a stale backstop timer from an earlier press
    /// can't resume capture during a newer press's playback.
    private var pressGeneration = 0

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

    // MARK: Attach

    /// Wire the rewind player into the engine. Called from
    /// `MenuBandSynth.start()` alongside the other `.attach` calls. Plays
    /// DRY into `mainMixerNode` (see class doc) — NOT through preLimiterMixer.
    func attach(to engine: AVAudioEngine) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(player)
        engine.attach(mixer)
        // Connect with engine-derived (nil) formats so the rewind graph
        // tracks the device's sample rate — a hardcoded 44.1k connection
        // breaks (and `scheduleBuffer` asserts) when the user is on a 96 kHz
        // interface or hot-swaps devices. The reversed clip is built in the
        // player's *actual* output format at play time (see `playReverse`).
        engine.connect(player, to: mixer, format: nil)
        engine.connect(mixer, to: engine.mainMixerNode, format: nil)
        mixer.outputVolume = 1.0
        lastPressTime = CACurrentMediaTime()
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
        // Capture window = audio SINCE the last press (notepat), floored to
        // `minCaptureSeconds` and capped at the ring length. Rapid taps grab
        // short successive chunks; a press after a phrase grabs the phrase.
        let now = CACurrentMediaTime()
        let sinceLast = lastPressTime > 0 ? (now - lastPressTime) : bufferSeconds
        lastPressTime = now
        let windowSec = Swift.min(bufferSeconds, Swift.max(minCaptureSeconds, sinceLast))
        let want = Int(windowSec * rate)
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

        // --- Build the clip in the player's ACTUAL output format ---
        // (nil-connected, so it tracks the device). Building to match the
        // player's format means `scheduleBuffer` never asserts on a sample-
        // rate/channel mismatch — the bug that broke audio on a 96 kHz
        // device. The ring is captured at the same device rate, so the
        // reversed mono samples are copied into every output channel at the
        // right speed.
        let outFormat = player.outputFormat(forBus: 0)
        guard outFormat.channelCount >= 1, outFormat.sampleRate > 0,
              let clip = AVAudioPCMBuffer(pcmFormat: outFormat,
                                          frameCapacity: AVAudioFrameCount(count)),
              let chData = clip.floatChannelData
        else { return false }
        let chCount = Int(outFormat.channelCount)
        reversed.withUnsafeBufferPointer { src in
            guard let base = src.baseAddress else { return }
            for c in 0..<chCount { chData[c].update(from: base, count: count) }
        }
        clip.frameLength = AVAudioFrameCount(count)

        // --- Pause capture, then play DRY into mainMixerNode ---
        // Gate recording off WHILE reverse-playing so the reversed audio
        // (which flows through the tapped mainMixerNode) isn't recaptured.
        // Capture resumes the instant Space is RELEASED (see `release()`),
        // NOT when the clip ends — so notes you play right after letting go
        // are captured immediately and the next press reverses THEM (this is
        // the fix for the laggy/"adding-again" second run).
        recording = false
        pressGeneration += 1
        let gen = pressGeneration
        if !engine.isRunning { try? engine.start() }
        player.stop()
        player.scheduleBuffer(clip, at: nil, options: [], completionHandler: nil)
        if !player.isPlaying { player.play() }
        // Backstop only — if a Space key-up is ever dropped, don't get stuck
        // muted: resume capture a bit after the clip would have ended, but
        // only if no newer press has happened since.
        let frameDuration = Double(count) / outFormat.sampleRate
        DispatchQueue.main.asyncAfter(deadline: .now() + frameDuration + 2.0) {
            [weak self] in
            guard let self = self, self.pressGeneration == gen else { return }
            self.finishReverse()
        }
        return true
    }

    /// Space released — stop the reverse voice and resume live capture.
    func release() {
        finishReverse()
    }

    private func finishReverse() {
        if player.isPlaying { player.stop() }
        // Resume capture. The ring kept its pre-playback contents (we only
        // skipped writes), so the live audio simply continues from here.
        recording = true
    }
}
