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
///   • The capture tap lives on the limiter (pre-mainMixer) while the
///     reverse player joins AT `mainMixerNode` — downstream of the tap — so
///     reverse playback can never re-enter the ring (no reverse-of-a-reverse
///     stacking) and the tape keeps rolling even while reversing.
///
/// Press/release semantics (the REVERSE CURSOR): Space is a momentary dive.
/// The first press anchors T0 ("now") and runs the needle backwards through the
/// tape from there. RELEASING returns the needle to T0 — not to the live head,
/// which by then has drifted on — so a re-press dives again from the SAME
/// origin. That's what makes reverse REPEATABLE: whacking Space stutters the
/// same passage. (It used to bank the playhead instead, so each press burrowed
/// further and further back and you could never hit the same lick twice.)
/// A real note drops the anchor, earning the next dive a new origin.
///
/// REVERSE DUB: notes played while the needle is diving are printed BACKWARDS
/// onto the tape at the needle (see `feed`). The ring is only ever read
/// backwards, so those notes come back out FORWARD on the next dive — normal
/// phrases sitting inside reversed music. That's why `resetReverseAnchor` is a
/// no-op while a pass is sounding: playing into the dive must not end it.
final class MenuBandRewindVoice {
    /// How far back the ring physically remembers — a rolling 60 s tape
    /// loop (continuously overwritten). Plenty of headroom beyond the
    /// reverse window so longer windows can reach deep into recent play.
    /// (~37 MB at 96 kHz mono float — fine.) Ring must exceed the reverse
    /// window (`captureSeconds`) with headroom.
    private let bufferSeconds: Double = 96.0
    /// Reverse window: a session (first press → next real note) can rewind
    /// at most this far behind its anchor. Presses within a session RESUME
    /// from where the last one stopped (see `sessionAnchor`/`consumedFrames`)
    /// — they never re-add time. A new note re-anchors at the live head.
    let captureSeconds: Double = 90.0
    /// Generation token for the in-flight reverse one-shot. Bumped on every
    /// `playReverse` and `release` so a deferred release-fade can tell whether
    /// it still owns the player (a fresh press between the fade and its stop
    /// must NOT cut the new playback). See `release`.
    private var playGeneration: UInt64 = 0

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
    /// Format-absorbing entry stage. The reversed clip is mono and may be at a
    /// different rate than the device; an AVAudioUnit (pitch/delay) REJECTS a
    /// reconnect to such a format (-10868, kAudioUnitErr_FormatNotSupported),
    /// but a mixer node accepts any input-bus format and resamples/upmixes to
    /// its own stable output. So the rate-matching reconnect lands HERE, and
    /// the pitch/echo AUs downstream only ever see the mixer's steady format.
    private let entryMixer = AVAudioMixerNode()
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

    // MARK: Reverse session (persistent cursor across presses)

    /// Absolute frame count (`framesWritten`) at the FIRST press of the
    /// current reverse session — the fixed "now" edge the whole session
    /// rewinds from. nil = no session; the next press anchors one at the
    /// live head. Guarded by `ringLock` (reset can arrive from any thread
    /// via `resetReverseAnchor`).
    private var sessionAnchor: Int? = nil
    /// Frames already reverse-played this session, banked on release (and
    /// folded in by `settleInFlightLocked`). Each press resumes
    /// `consumedFrames` behind the anchor — the same reverse point the last
    /// press stopped at — never the top of the window again.
    private var consumedFrames = 0
    /// The session's fixed window length in frames (`captureSeconds` at the
    /// anchor's sample rate). Presses never re-add time: once a session has
    /// consumed this much tape, further presses are silent until a note
    /// resets the anchor.
    private var sessionWantFrames = 0
    /// Frame length of the in-flight one-shot, for the settle accounting.
    private var lastClipFrames = 0
    /// True while a reverse pass is sounding — i.e. while the needle is
    /// travelling backwards through the tape. Gates the reverse dub in `feed`
    /// and keeps `resetReverseAnchor` from yanking the needle back to the live
    /// head when you play a note INTO the dive. Guarded by `ringLock`.
    private var reverseActive = false

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
        engine.attach(entryMixer)
        engine.attach(pitch)
        engine.attach(echo)
        engine.attach(mixer)
        // Connect with engine-derived (nil) formats so the rewind graph
        // tracks the device's sample rate — a hardcoded 44.1k connection
        // breaks (and `scheduleBuffer` asserts) when the user is on a 96 kHz
        // interface or hot-swaps devices. The reversed clip is built in the
        // player's *actual* output format at play time (see `playReverse`).
        // player → entryMixer → pitch → echo → mixer → mainMixerNode: the
        // bend/echo ride the tape but stay private to this DRY sub-chain (no
        // recapture). entryMixer absorbs the clip's mono/rate so the AUs only
        // ever see a stable format (the -10868 crash fix).
        engine.connect(player, to: entryMixer, format: nil)
        engine.connect(entryMixer, to: pitch, format: nil)
        engine.connect(pitch, to: echo, format: nil)
        engine.connect(echo, to: mixer, format: nil)
        engine.connect(mixer, to: engine.mainMixerNode, format: nil)
        entryMixer.outputVolume = 1.0
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
        // reverse playback isn't in this signal and can't feed back. What
        // arrives here while reversing is therefore exactly what you're playing
        // live, clean of the rewind itself — which is what makes the reverse dub
        // below possible.
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
        let cap = ringCapacity

        // ── Reverse dub ──────────────────────────────────────────────────────
        // While a rewind pass is sounding, what you play is NOT appended at the
        // live head. It gets PRINTED BACKWARDS onto the tape right where the
        // needle is. Because the ring is only ever READ backwards, a backwards
        // print comes back out FORWARD on the next pass — normal-sounding
        // phrases sitting inside reversed music, exactly where you played them.
        // (Reel-flip: turn the tape over, record, turn it back.)
        //
        // The forward head deliberately does not advance: the tape isn't
        // rolling forward while you're diving back through it. It stays parked
        // at "now", which is where the needle snaps back to on release.
        //
        // Mixed in, not overwritten, so the dub sits ON the existing music
        // instead of erasing it. Clamped because two full-scale signals sum.
        if reverseActive, let head = reverseReadHeadLocked() {
            // Absolute frame `a` lives at ring index writeIdx-1-(framesWritten-a).
            var idx = (writeIdx - 1 - (framesWritten - head)) % cap
            if idx < 0 { idx += cap }
            if channels >= 2 {
                let l = ch[0]
                let r = ch[1]
                for i in 0..<frames {
                    ring[idx] = Swift.max(-1, Swift.min(1, ring[idx] + (l[i] + r[i]) * 0.5))
                    idx -= 1                       // walk BACKWARD with the needle
                    if idx < 0 { idx += cap }
                }
            } else {
                let m = ch[0]
                for i in 0..<frames {
                    ring[idx] = Swift.max(-1, Swift.min(1, ring[idx] + m[i]))
                    idx -= 1
                    if idx < 0 { idx += cap }
                }
            }
            ringLock.unlock()
            return
        }

        // ── Normal forward capture ───────────────────────────────────────────
        var idx = writeIdx
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

    /// Absolute frame the reverse pass is reading right now — the needle. The
    /// in-flight clip was snapshotted at press time, so there is no live read
    /// pointer to ask; the position comes from the clip's wall-clock progress
    /// (the same arithmetic `reverseProgress` shows the strip). Returns nil
    /// once the one-shot has run out, so a still-held spacebar over an
    /// exhausted window falls back to ordinary forward capture rather than
    /// piling every note onto one parked frame.
    ///
    /// Call with `ringLock` held.
    private func reverseReadHeadLocked() -> Int? {
        guard let anchor = sessionAnchor,
              clipDuration > 0, playStart > 0, lastClipFrames > 0 else { return nil }
        let p = (CACurrentMediaTime() - playStart) / clipDuration
        guard p >= 0, p < 1 else { return nil }
        let head = anchor - consumedFrames - Int(p * Double(lastClipFrames))
        return head > 0 ? head : nil
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
        let total = framesWritten
        let oldest = total - valid
        // Fold any still-in-flight press into the cursor first, so a press
        // that arrives without an intervening release can't lose position.
        settleInFlightLocked()
        // Session cursor: the first press anchors "now"; every later press
        // resumes from the same reverse point the last one stopped at
        // (anchor − consumed). Re-anchor only when there is no session (a
        // note reset it — see `resetReverseAnchor`), the ring was
        // reallocated under us (anchor > total), or the tape behind the
        // cursor has been fully overwritten.
        let sessionValid = sessionAnchor.map {
            $0 <= total && $0 - consumedFrames > oldest + 256
        } ?? false
        if !sessionValid {
            sessionAnchor = total
            consumedFrames = 0
            sessionWantFrames = Int(captureSeconds * rate)
        }
        let cursorAbs = (sessionAnchor ?? total) - consumedFrames
        // What's left to reverse: bounded by the session window (no re-added
        // time — the window is fixed at the first press) and by how much
        // tape behind the cursor still survives in the ring.
        let count = Swift.min(sessionWantFrames - consumedFrames, cursorAbs - oldest)
        guard count >= 256 else { ringLock.unlock(); return false }
        // Walk backward from the CURSOR (not the live head — the head keeps
        // advancing while the session idles), emitting samples in REVERSE
        // order directly (newest-first → the clip plays time-backwards).
        var reversed = [Float](repeating: 0, count: count)
        let behindHead = total - cursorAbs      // 0…cap frames behind writeIdx
        var readIdx = (writeIdx - 1 - behindHead) % cap
        if readIdx < 0 { readIdx += cap }
        for i in 0..<count {
            reversed[i] = ring[readIdx]
            readIdx -= 1
            if readIdx < 0 { readIdx += cap }
        }
        ringLock.unlock()

        // Declick: the window begins and ends on an arbitrary mid-waveform
        // sample, so jumping straight from silence to that value (on press)
        // and back (at the tail) snaps a click. A short raised-cosine fade at
        // both ends ramps those discontinuities away — the "very slight fade
        // in when reverse starts" that kills the spacebar pop, plus a matching
        // tail fade so the one-shot lands in silence cleanly.
        let fade = Swift.min(count / 2, Swift.max(1, Int(rate * 0.006)))
        if fade > 1 {
            for i in 0..<fade {
                // 0→1 half-cosine: smooth (zero-slope) at the silent end.
                let g = Float(0.5 - 0.5 * cos(Double.pi * Double(i) / Double(fade)))
                reversed[i] *= g                 // fade in  (clip head)
                reversed[count - 1 - i] *= g      // fade out (clip tail)
            }
        }

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
            // Re-wire only the player→entryMixer segment to the clip's rate.
            // The mixer accepts the mono/rate and feeds the pitch/echo AUs a
            // stable format — reconnecting an AU input here would throw -10868.
            engine.connect(player, to: entryMixer, format: clipFormat)
            connectedRate = rate
        }

        // --- Play DRY into mainMixerNode ---
        // The ring keeps recording throughout (its tap is on the limiter,
        // upstream of where this reverse player joins), so notes played while
        // reversing land in the buffer and the next press reverses them too.
        if !engine.isRunning { try? engine.start() }
        player.stop()
        // Own the player for this press — invalidates any pending release-fade
        // from a prior press and restores full level (a release fade may have
        // left the mixer ramped down).
        playGeneration &+= 1
        mixer.outputVolume = 1.0
        // One-shot reverse (notepat's loop:false): plays what remains of the
        // session window backward once and falls into silence. Press again
        // to resume from wherever this playback stops.
        player.scheduleBuffer(clip, at: nil, options: [], completionHandler: nil)
        if !player.isPlaying { player.play() }
        // Under the lock: `feed` reads these on the audio thread every buffer to
        // place the reverse dub, and a torn read would print it at a bogus frame.
        ringLock.lock()
        lastClipFrames = count
        clipDuration = Double(count) / rate
        playStart = CACurrentMediaTime()
        reverseActive = true
        ringLock.unlock()
        return true
    }

    /// Fold the elapsed portion of the in-flight one-shot into the session
    /// cursor (`consumedFrames`). Rate-free — fraction of the clip's
    /// duration × its frame length — so a device-rate change between press
    /// and release can't mis-count. Call with `ringLock` held: it mutates
    /// session state `playReverse` reads under the same lock.
    private func settleInFlightLocked() {
        guard clipDuration > 0, playStart > 0, lastClipFrames > 0 else { return }
        let fraction = Swift.min(1.0, Swift.max(0.0, (CACurrentMediaTime() - playStart) / clipDuration))
        consumedFrames += Int(fraction * Double(lastClipFrames))
        clipDuration = 0
        playStart = 0
        lastClipFrames = 0
    }

    /// A fresh note sounded — the reverse clock rejoins the record head.
    /// Drops the session so the next Space press anchors at the live "now"
    /// (which includes the note that just played) instead of resuming the
    /// old cursor. Called from `MenuBandSynth.noteOn` (any thread).
    ///
    /// EXCEPT while a reverse pass is sounding: notes played into the dive are
    /// the reverse dub, and re-anchoring would rip the needle back to the live
    /// head the instant you touched a key — you could never print anything.
    func resetReverseAnchor() {
        ringLock.lock()
        if !reverseActive {
            sessionAnchor = nil
            consumedFrames = 0
        }
        ringLock.unlock()
    }

    /// Progress (0…1) through the SESSION's reverse window — what earlier
    /// presses already consumed plus the in-flight clip's wall-clock elapsed
    /// — so the strip's playhead resumes where the last press left it
    /// instead of snapping back to the top of the window. Returns nil when
    /// nothing is reverse-playing.
    func reverseProgress() -> Double? {
        guard clipDuration > 0, sessionWantFrames > 0 else { return nil }
        let p = (CACurrentMediaTime() - playStart) / clipDuration
        guard p >= 0 else { return nil }
        let frames = Double(consumedFrames) + Swift.min(1, p) * Double(lastClipFrames)
        return Swift.min(1, frames / Double(sessionWantFrames))
    }

    /// Space released — the needle snaps back to NOW, and the reverse voice
    /// stops. Space is a momentary excursion: while it's held you dive back
    /// through the tape, and letting go drops you at the live record head
    /// again, so the next press rewinds from "now" rather than resuming deeper
    /// into the old dive. (This used to BANK the playhead so consecutive
    /// presses burrowed further and further back; the snap-back is what makes
    /// the gesture feel like a tape head you can lift.)
    ///
    /// A bare `player.stop()` mid-clip cuts a non-zero sample to silence and
    /// clicks, so ramp the mixer down first (AVAudioMixerNode smooths volume
    /// changes), then stop a beat later and restore level.
    func release() {
        ringLock.lock()
        reverseActive = false          // stop dubbing; forward capture resumes
        // The needle jumps back to T0 — the moment Space was first pressed —
        // NOT to "now". Keeping the anchor and zeroing the dive is what makes
        // reverse REPEATABLE: whack Space again and it re-dives from the same
        // original point, so quick represses stutter the same passage instead
        // of each one reversing from a "now" that has drifted on, or (as it
        // originally did) burrowing ever deeper into the tape.
        //
        // Only a real note (`resetReverseAnchor`, no-op mid-dive) drops the
        // anchor — new material earns a new origin.
        settleInFlightLocked()         // clears the in-flight clip's state
        consumedFrames = 0             // …and rewinds the dive to the anchor
        ringLock.unlock()
        guard player.isPlaying else { return }
        playGeneration &+= 1
        let gen = playGeneration
        mixer.outputVolume = 0
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.02) { [weak self] in
            guard let self = self, self.playGeneration == gen else { return }
            if self.player.isPlaying { self.player.stop() }
            self.mixer.outputVolume = 1.0
        }
    }

}
