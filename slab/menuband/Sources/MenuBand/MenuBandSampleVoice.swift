import Foundation
import AVFoundation
import AppKit

/// Microphone-sampled voice — the user holds backtick (`) to capture a
/// short clip from the default input device, then plays it back as a
/// pitched piano voice via per-note AVAudioPlayerNode + Varispeed pairs.
///
/// Architecture:
///   AVAudioEngine.inputNode ──(tap)──► recordBuffer (mono float32, ≤10 s)
///                                              │
///                  per active note             ▼
///              AVAudioPlayerNode ──► Varispeed ──► voiceMixer ──► output
///
/// `voiceMixer.outputVolume` is the master gate — closed (0.0) when the
/// sample backend isn't active, opened (1.0) when it is. Voices attach
/// lazily on first noteOn so cold idle costs nothing.
///
/// Mirrors `KPBJRadioStream`'s engine-attach pattern: static graph nodes
/// land in the host engine on `attach(to:output:)`, voice nodes attach
/// per-noteOn from a small reusable pool.
final class MenuBandSampleVoice {
    /// Internal sample rate. The recording tap converts whatever the
    /// input node gives us (44.1k or 48k typically) into this format
    /// before storage so playback math is consistent.
    private let sampleRate: Double = 44_100

    /// Mono float32. Single-channel keeps the recording small and lets
    /// the same buffer feed any number of pitched playback voices
    /// without extra channel-routing.
    private let storageFormat: AVAudioFormat

    /// Initial scratch capacity in frames (~10 s at 44.1 kHz). The
    /// scratch buffer grows geometrically beyond this if the user
    /// holds the record key longer — there's no fixed cap on
    /// recording length now, just whatever the in-process float
    /// buffer can hold.
    private let initialScratchFrames: Int

    /// The currently-stored recording. nil until a successful capture.
    /// Reads from the audio thread (player schedules) are safe because
    /// we never mutate this buffer in-place once published — a new
    /// recording fully replaces it under `bufferLock`.
    private var recordedBuffer: AVAudioPCMBuffer?
    private let bufferLock = NSLock()

    /// Active recording state. We tap the engine's input node into a
    /// scratch buffer; on `stopRecording` we trim to actual length and
    /// promote to `recordedBuffer`.
    private var recording = false
    private var recordWriteFrame: Int = 0
    private var recordScratch: AVAudioPCMBuffer?

    /// Live callback fired once per input-tap block with that block's
    /// RMS [0, 1]. Wired up from `MenuBandSynth` so the menubar VU
    /// bars can pulse with the user's voice while recording.
    var onLevel: ((Float) -> Void)?

    /// Public read of the recording flag — `MenuBandSynth` proxies
    /// this out to the controller / AppDelegate so the menubar icon
    /// can tint itself red while a clip is being captured.
    var isRecording: Bool { recording }
    /// Format the input node delivers (set on the first tap callback).
    /// Used to build a converter into our storage format.
    private var inputFormat: AVAudioFormat?
    private var inputConverter: AVAudioConverter?

    /// Mix bus for all per-note voices. Connects to the host's
    /// pre-limiter mixer once on attach. Master gate lives here.
    private let voiceMixer = AVAudioMixerNode()

    private weak var engine: AVAudioEngine?
    private weak var output: AVAudioNode?
    private var attached = false
    private var inputTapInstalled = false

    /// Per-note voice slot. Pool keyed by (channel, midi) so a note
    /// retriggered on the same channel reuses the same player + varispeed
    /// pair. Players persist in the engine graph for the app's lifetime
    /// once allocated — AVAudioEngine prefers stable graphs over
    /// frequent attach/detach.
    private final class Voice {
        let node = AVAudioPlayerNode()
        let varispeed = AVAudioUnitVarispeed()
        var midi: UInt8 = 60
        var releaseWork: DispatchWorkItem?
    }

    private var voices: [UInt16: Voice] = [:]

    /// Per-channel cursor — round-robin across a small pool inside each
    /// channel so back-to-back noteOns on the same key still get fresh
    /// players (no scheduling-collision gap).
    private let voicesPerChannel: Int = 4

    init() {
        guard let fmt = AVAudioFormat(commonFormat: .pcmFormatFloat32,
                                      sampleRate: sampleRate,
                                      channels: 1,
                                      interleaved: false) else {
            // Fallback — should never trigger; the standard mono
            // float32 format is universally supported.
            self.storageFormat = AVAudioFormat(standardFormatWithSampleRate: 44_100,
                                               channels: 1)!
            self.initialScratchFrames = Int(44_100 * 10)
            return
        }
        self.storageFormat = fmt
        self.initialScratchFrames = Int(sampleRate * 10.0)
    }

    // MARK: - Engine attach

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        self.output = output
        engine.attach(voiceMixer)
        engine.connect(voiceMixer, to: output, format: nil)
        // Master gate — closed until the synth flips this backend on.
        // Mirrors `KPBJRadioStream.crossfadeMixer.outputVolume`.
        voiceMixer.outputVolume = 0.0
        attached = true
    }

    func setOutputEnabled(_ enabled: Bool) {
        voiceMixer.outputVolume = enabled ? 1.0 : 0.0
    }

    // MARK: - Recording

    /// Begin capturing audio from the default input. Idempotent —
    /// calling while already recording is a no-op. Recording state is
    /// kept in a scratch buffer until `stopRecording` finalizes it.
    func startRecording() {
        guard !recording else { return }
        guard let engine = engine else {
            NSLog("MenuBand SampleVoice: startRecording without engine attached")
            return
        }
        // TCC: on macOS the very first attempt to access the default
        // input device only succeeds after the user has granted
        // microphone permission. Trigger the system prompt explicitly
        // here — without this, `inputNode.outputFormat(forBus:)` can
        // return a 0-channel format and the recording silently fails.
        let authStatus = AVCaptureDevice.authorizationStatus(for: .audio)
        switch authStatus {
        case .denied, .restricted:
            NSLog("MenuBand SampleVoice: microphone access denied (status \(authStatus.rawValue))")
            DispatchQueue.main.async { Self.showMicPermissionAlert(denied: true) }
            return
        case .notDetermined:
            NSLog("MenuBand SampleVoice: requesting microphone permission")
            AVCaptureDevice.requestAccess(for: .audio) { [weak self] granted in
                NSLog("MenuBand SampleVoice: microphone permission granted=\(granted)")
                DispatchQueue.main.async {
                    if granted {
                        self?.startRecording()
                    } else {
                        Self.showMicPermissionAlert(denied: true)
                    }
                }
            }
            return
        case .authorized:
            break
        @unknown default:
            break
        }
        let input = engine.inputNode
        let format = input.outputFormat(forBus: 0)
        // Some virtual input devices return a 0-channel / 0-Hz format
        // when no real device is selected. Bail loudly so the synth
        // doesn't think it has a usable buffer when the user releases.
        guard format.channelCount > 0, format.sampleRate > 0 else {
            NSLog("MenuBand SampleVoice: input format unusable (ch=\(format.channelCount) sr=\(format.sampleRate)) — engine running=\(engine.isRunning)")
            return
        }
        inputFormat = format
        inputConverter = AVAudioConverter(from: format, to: storageFormat)
        guard inputConverter != nil else {
            NSLog("MenuBand SampleVoice: failed to build converter from \(format) to \(storageFormat)")
            return
        }
        // Pre-allocate the scratch buffer to ~10 s so a typical record
        // doesn't have to resize. If the user holds longer we grow
        // geometrically inside `ingestInput`.
        guard let scratch = AVAudioPCMBuffer(pcmFormat: storageFormat,
                                             frameCapacity: AVAudioFrameCount(initialScratchFrames)) else {
            NSLog("MenuBand SampleVoice: failed to allocate scratch buffer")
            return
        }
        scratch.frameLength = 0
        recordScratch = scratch
        recordWriteFrame = 0

        // 4096-frame tap is a good balance: ~93 ms at 44.1k, plenty of
        // jitter slack without making the recording feel laggy on key
        // release.
        if !inputTapInstalled {
            input.installTap(onBus: 0, bufferSize: 4096, format: format) { [weak self] buffer, _ in
                self?.ingestInput(buffer)
            }
            inputTapInstalled = true
        }
        recording = true
        NSLog("MenuBand SampleVoice: recording started (input format ch=\(format.channelCount) sr=\(format.sampleRate))")
    }

    /// Stop capture and promote scratch to the active recording.
    /// Returns true iff at least 100 ms of usable samples were captured.
    /// On false, the synth should not switch to the sample backend —
    /// the user likely tapped the key without recording anything real.
    @discardableResult
    func stopRecording() -> Bool {
        guard recording else { return false }
        recording = false
        if inputTapInstalled, let engine = engine {
            engine.inputNode.removeTap(onBus: 0)
            inputTapInstalled = false
        }
        guard let scratch = recordScratch else {
            recordScratch = nil
            return false
        }
        recordScratch = nil
        let frames = recordWriteFrame
        let minFrames = Int(sampleRate * 0.1) // 100 ms
        guard frames >= minFrames else {
            NSLog("MenuBand SampleVoice: recording too short (\(frames) frames < \(minFrames)) — discarding")
            return false
        }
        // Copy into a right-sized buffer so playback doesn't drag a
        // 10-second-capacity scratch around for short clips.
        guard let out = AVAudioPCMBuffer(pcmFormat: storageFormat,
                                         frameCapacity: AVAudioFrameCount(frames)) else {
            return false
        }
        out.frameLength = AVAudioFrameCount(frames)
        if let src = scratch.floatChannelData?[0],
           let dst = out.floatChannelData?[0] {
            memcpy(dst, src, frames * MemoryLayout<Float>.size)
        }
        bufferLock.lock()
        recordedBuffer = out
        bufferLock.unlock()
        NSLog("MenuBand SampleVoice: recording captured \(frames) frames (\(Double(frames) / sampleRate) s)")
        return true
    }

    /// Tap-side: convert the input node's buffer into our storage
    /// format and append into the scratch buffer. There's no fixed
    /// cap — the scratch grows geometrically when filled so the user
    /// can hold the record key indefinitely (memory cost is the only
    /// real ceiling). Also computes per-block RMS and forwards to
    /// `onLevel` for the menubar VU meter.
    private func ingestInput(_ buffer: AVAudioPCMBuffer) {
        guard recording, let converter = inputConverter else { return }

        // Estimate output frames the converter will produce. For
        // sample-rate conversion the ratio matters; use ceiling +
        // padding so we never under-allocate the staging buffer.
        let inputFrames = Double(buffer.frameLength)
        let ratio = storageFormat.sampleRate / buffer.format.sampleRate
        let estOut = Int((inputFrames * ratio).rounded(.up)) + 16
        guard let staging = AVAudioPCMBuffer(pcmFormat: storageFormat,
                                             frameCapacity: AVAudioFrameCount(estOut)) else {
            return
        }

        var supplied = false
        var error: NSError?
        let status = converter.convert(to: staging, error: &error) { _, outStatus in
            if supplied {
                outStatus.pointee = .endOfStream
                return nil
            }
            supplied = true
            outStatus.pointee = .haveData
            return buffer
        }

        if status == .error {
            NSLog("MenuBand SampleVoice: convert failed: \(String(describing: error))")
            return
        }
        let producedFrames = Int(staging.frameLength)
        guard producedFrames > 0 else { return }

        // Grow the scratch buffer if this block would overflow. AVAudio
        // PCM buffers don't resize in place, so we re-allocate at 2×
        // current capacity (or just enough to fit, whichever is bigger)
        // and copy the existing contents over before appending.
        ensureScratchCapacity(forFramesToAppend: producedFrames)
        guard let scratch = recordScratch else { return }

        // Mix down to mono if the input is multichannel — pick channel
        // 0 (most macOS mics deliver mono on bus 0 anyway). Storage is
        // single-channel so we only ever write into channelData[0].
        if let src = staging.floatChannelData?[0],
           let dst = scratch.floatChannelData?[0] {
            memcpy(dst.advanced(by: recordWriteFrame), src,
                   producedFrames * MemoryLayout<Float>.size)
            // Compute RMS of the just-converted block for the level
            // meter. Cheap — single pass over the same data we just
            // copied. Forwarded out via `onLevel` (UI side smooths
            // and clamps further).
            var sumSq: Float = 0
            for i in 0..<producedFrames {
                let s = src[i]
                sumSq += s * s
            }
            let rms = sqrt(sumSq / Float(producedFrames))
            if let cb = self.onLevel {
                // Hop to main — the renderer reads on its own tick;
                // we just publish.
                DispatchQueue.main.async { cb(rms) }
            }
            recordWriteFrame += producedFrames
            scratch.frameLength = AVAudioFrameCount(recordWriteFrame)
        }
    }

    /// Grow `recordScratch` so it can hold `forFramesToAppend` more
    /// frames past the current write position. No-op if there's
    /// already room. AVAudioPCMBuffer is not resizable, so growth is
    /// done by allocating a new buffer (2× current capacity, or
    /// exactly enough to fit) and copying the existing samples in.
    /// Called from the audio thread — memcpy is cheap, but we only
    /// do it on overflow so amortized cost is negligible.
    private func ensureScratchCapacity(forFramesToAppend frames: Int) {
        guard let cur = recordScratch else { return }
        let needed = recordWriteFrame + frames
        let cap = Int(cur.frameCapacity)
        if needed <= cap { return }
        var newCap = max(cap * 2, needed + initialScratchFrames)
        // Round up to a frame-aligned chunk to avoid thrashing on
        // odd buffer sizes.
        newCap = ((newCap + 1023) / 1024) * 1024
        guard let bigger = AVAudioPCMBuffer(pcmFormat: storageFormat,
                                            frameCapacity: AVAudioFrameCount(newCap)) else {
            NSLog("MenuBand SampleVoice: failed to grow scratch to \(newCap) frames")
            return
        }
        bigger.frameLength = AVAudioFrameCount(recordWriteFrame)
        if recordWriteFrame > 0,
           let src = cur.floatChannelData?[0],
           let dst = bigger.floatChannelData?[0] {
            memcpy(dst, src, recordWriteFrame * MemoryLayout<Float>.size)
        }
        recordScratch = bigger
    }

    // MARK: - Playback

    private func voiceKey(channel: UInt8, slot: Int) -> UInt16 {
        UInt16(channel) << 8 | UInt16(slot)
    }

    /// Per-(channel, key) round-robin slot — same key retriggered on
    /// the same channel cycles through `voicesPerChannel` players so
    /// the previous tail can finish while the new attack starts.
    private var slotCursor: [UInt16: Int] = [:]

    private func nextSlot(channel: UInt8, midi: UInt8) -> Int {
        let routingKey = (UInt16(channel) << 8) | UInt16(midi)
        let cur = slotCursor[routingKey] ?? 0
        let next = (cur + 1) % voicesPerChannel
        slotCursor[routingKey] = next
        return cur
    }

    private func ensureVoice(channel: UInt8, slot: Int) -> Voice? {
        let key = voiceKey(channel: channel, slot: slot)
        if let v = voices[key] { return v }
        guard let engine = engine else { return nil }
        let v = Voice()
        engine.attach(v.node)
        engine.attach(v.varispeed)
        engine.connect(v.node, to: v.varispeed, format: storageFormat)
        engine.connect(v.varispeed, to: voiceMixer, format: storageFormat)
        voices[key] = v
        return v
    }

    /// Pitch ratio for `midi` relative to middle C (60). 60 → 1.0.
    @inline(__always)
    private func ratio(forNote midi: UInt8) -> Float {
        Float(pow(2.0, Double(Int(midi) - 60) / 12.0))
    }

    func noteOn(_ midi: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        bufferLock.lock()
        let buf = recordedBuffer
        bufferLock.unlock()
        guard let buf = buf else {
            // No recording yet — silent noteOn. The synth shouldn't
            // route to this backend in that state, but defend anyway.
            return
        }
        guard attached, engine != nil else { return }
        let slot = nextSlot(channel: channel, midi: midi)
        guard let voice = ensureVoice(channel: channel, slot: slot) else { return }

        // Cancel any pending release-fade — we're retriggering the
        // slot before the previous tail finished.
        voice.releaseWork?.cancel()
        voice.releaseWork = nil

        voice.midi = midi
        voice.varispeed.rate = ratio(forNote: midi)
        voice.node.volume = Float(velocity) / 127.0

        if voice.node.isPlaying {
            voice.node.stop()
        }
        // Loop the buffer for as long as the key is held — matches
        // notepat's native sampler tap-and-hold behavior. AVAudio's
        // `.loops` option re-schedules the same buffer back-to-back
        // on the audio thread, so the loop boundary is sample-
        // accurate (no click) and pitch-stable across restarts.
        // `.interrupts` cancels any prior schedule on the same node.
        voice.node.scheduleBuffer(buf, at: nil,
                                  options: [.interrupts, .loops]) { /* no-op */ }
        voice.node.play()
    }

    func noteOff(_ midi: UInt8, channel: UInt8 = 0) {
        // Fade out + stop every slot on this channel that's currently
        // playing the matched midi note. Per-note round-robin means
        // the same key may live in several slots at once when the user
        // re-presses faster than the release tail.
        for slot in 0..<voicesPerChannel {
            let key = voiceKey(channel: channel, slot: slot)
            guard let voice = voices[key], voice.midi == midi,
                  voice.node.isPlaying else { continue }
            scheduleRelease(voice)
        }
    }

    /// ~80 ms exponential fade by ramping the player's volume on a
    /// short timer. AVAudioPlayerNode.volume changes are sample-
    /// accurate so the user hears a smooth release rather than a
    /// click. Stops the node after the ramp.
    private func scheduleRelease(_ voice: Voice) {
        let totalSteps = 8
        let stepInterval: TimeInterval = 0.010 // 10 ms × 8 = 80 ms
        let startVolume = voice.node.volume
        var step = 0
        let work = DispatchWorkItem { [weak voice] in
            guard let voice = voice else { return }
            voice.node.volume = 0
            voice.node.stop()
        }
        voice.releaseWork = work
        // Drive the ramp on the main queue — cheap, no audio-thread
        // interaction, and we already touch player volume on main
        // elsewhere.
        func tick() {
            guard let work = voice.releaseWork, !work.isCancelled else { return }
            step += 1
            if step >= totalSteps {
                work.perform()
                return
            }
            let frac = Float(totalSteps - step) / Float(totalSteps)
            voice.node.volume = startVolume * frac
            DispatchQueue.main.asyncAfter(deadline: .now() + stepInterval) { tick() }
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + stepInterval) { tick() }
    }

    func panic() {
        for (_, v) in voices {
            v.releaseWork?.cancel()
            v.releaseWork = nil
            if v.node.isPlaying { v.node.stop() }
            v.node.volume = 1.0
        }
    }

    /// True if a recording exists and is long enough to be playable.
    var hasRecording: Bool {
        bufferLock.lock(); defer { bufferLock.unlock() }
        return recordedBuffer != nil
    }

    /// Show an NSAlert explaining how to enable microphone access for
    /// Menu Band. Called when TCC has already denied the request, OR
    /// after the user clicks "Don't Allow" on the system prompt.
    /// Includes a button that opens System Settings → Privacy →
    /// Microphone directly so the fix is one click away.
    private static var alertVisible = false
    private static func showMicPermissionAlert(denied: Bool) {
        guard !alertVisible else { return }
        alertVisible = true
        defer { alertVisible = false }
        let alert = NSAlert()
        alert.messageText = "Menu Band can't reach the microphone"
        alert.informativeText = denied
            ? "Microphone access has been denied. Open System Settings → Privacy & Security → Microphone and toggle Menu Band on, then try the sample voice again."
            : "Menu Band needs microphone access to record sample notes. Grant permission when macOS asks, or open System Settings → Privacy & Security → Microphone to enable it manually."
        alert.alertStyle = .warning
        alert.addButton(withTitle: "Open System Settings")
        alert.addButton(withTitle: "Cancel")
        let response = alert.runModal()
        if response == .alertFirstButtonReturn {
            // x-apple.systempreferences:com.apple.preference.security?Privacy_Microphone
            if let url = URL(string: "x-apple.systempreferences:com.apple.preference.security?Privacy_Microphone") {
                NSWorkspace.shared.open(url)
            }
        }
    }
}
