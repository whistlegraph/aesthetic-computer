import Foundation
import AVFoundation
import AppKit

extension Notification.Name {
    static let menuBandMicPermissionAlertWillShow =
        Notification.Name("MenuBandMicPermissionAlertWillShow")
}

/// Microphone-sampled voice — the user holds backtick (`) to capture a
/// short clip from the default input device, then plays it back as a
/// pitched piano voice via per-note AVAudioPlayerNode + Varispeed pairs.
///
/// Architecture:
///   recordEngine.inputNode ──(tap)──► recordBuffer (mono float32, ≤10 s)
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
    private let trimThreshold: Float = 0.012
    private let trimPrerollFrames = Int(44_100 * 0.012)

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
    /// True while the record key is still logically held. This protects
    /// the first-run microphone permission path: if the user releases
    /// the key while the TCC prompt is open, the async grant callback
    /// must not start a surprise recording afterwards.
    private var recordingRequested = false
    private var recordWriteFrame: Int = 0
    private var recordScratch: AVAudioPCMBuffer?
    /// Frames to drop from the very front of every record. The first
    /// tens of ms of input typically capture the mechanical click of
    /// the physical record key (and its acoustic decay through the
    /// laptop body) — discarding ~35 ms / 1543 frames at 44.1 kHz
    /// cleans that out for nearly every mechanical/scissor switch
    /// without eating into actual performance. `trimmedStartFrame`
    /// runs AFTER this and additionally rejects any leading
    /// transient burst that survives the fixed window.
    private let recordKeyClickSkipFrames: Int = Int(44_100 * 0.035)
    /// Decremented on each ingestInput block until zero. Set fresh
    /// at every `startRecording` call.
    private var framesRemainingToSkip: Int = 0

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
    private var debugTapBlocksLogged = 0
    // Smaller tap buffer = faster handoff from CoreAudio HAL into
    // our ingest loop, so the recording captures from the *first*
    // ms the user holds the key. 128 frames @ 44.1 kHz ≈ 2.9 ms.
    private let inputTapBufferFrames: AVAudioFrameCount = 128

    /// Mix bus for all per-note voices. Connects to the host's
    /// pre-limiter mixer once on attach. Master gate lives here.
    private let voiceMixer = AVAudioMixerNode()

    private weak var engine: AVAudioEngine?
    private weak var output: AVAudioNode?
    private var attached = false
    /// Dedicated input-only engine for microphone capture. Keeping this
    /// separate from the synth/playback engine avoids AUHAL device-routing
    /// conflicts when the output graph is pinned to a Multi-Output device or
    /// other output-only hardware.
    private let recordEngine = AVAudioEngine()
    private var inputTapInstalled = false
    private var hotMicStopWork: DispatchWorkItem?
    // Mic stays hot for 30 s after the last record so the
    // *second* and subsequent record key presses get zero start
    // latency. Lifted from 3 s — a typical record/audition/record
    // cycle is longer than that and the user was hitting the
    // cold-start path every iteration.
    private let hotMicIdleSeconds: TimeInterval = 30.0

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

    /// Current pitch-bend, in semitones. ±2 by convention (matches
    /// `MenuBandController.setBend`), applied on top of every voice's
    /// note-pitch ratio. Driven by `setBend(amount:)` from the
    /// controller while the trackpad pitch-bend cursor is active.
    private var bendSemitones: Float = 0

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
        // Pre-build the melodic sample playback graph before the host
        // engine starts. Lazy attachment after AVAudioEngine is already
        // running can produce silent player nodes on some CoreAudio graphs.
        for channel: UInt8 in 0..<8 {
            for slot in 0..<voicesPerChannel {
                _ = ensureVoice(channel: channel, slot: slot)
            }
        }
        // Prewarm the record engine if the user has already granted
        // microphone permission. Cold AVAudioEngine.start() on an
        // input graph takes 50–200 ms on Apple Silicon — that lag
        // shows up as a noticeable gap between hitting the record
        // key and the first audible sample. Lifting it to launch
        // time makes every backtick press feel instant.
        if AVCaptureDevice.authorizationStatus(for: .audio) == .authorized {
            DispatchQueue.main.async { [weak self] in
                _ = self?.ensureHotMicRunning()
                // Don't leave the mic hot forever — schedule the
                // normal idle-off timer so we drop the engine if
                // the user never actually records.
                self?.scheduleHotMicStop()
            }
        }
    }

    func setOutputEnabled(_ enabled: Bool) {
        voiceMixer.outputVolume = enabled ? 1.0 : 0.0
    }

    // MARK: - Recording

    /// Begin capturing audio from the default input. Idempotent —
    /// calling while already recording is a no-op. Recording state is
    /// kept in a scratch buffer until `stopRecording` finalizes it.
    func startRecording() {
        recordingRequested = true
        guard !recording else { return }
        guard engine != nil else {
            NSLog("MenuBand SampleVoice: startRecording without engine attached")
            recordingRequested = false
            return
        }
        let captureDevice = AVCaptureDevice.default(for: .audio)
        NSLog("MenuBand SampleVoice: startRecording auth=\(AVCaptureDevice.authorizationStatus(for: .audio).rawValue) captureDevice=\(captureDevice?.localizedName ?? "nil") id=\(captureDevice?.uniqueID ?? "nil")")
        // TCC: on macOS the very first attempt to access the default
        // input device only succeeds after the user has granted
        // microphone permission. Trigger the system prompt explicitly
        // here — without this, `inputNode.outputFormat(forBus:)` can
        // return a 0-channel format and the recording silently fails.
        let authStatus = AVCaptureDevice.authorizationStatus(for: .audio)
        switch authStatus {
        case .denied, .restricted:
            NSLog("MenuBand SampleVoice: microphone access denied (status \(authStatus.rawValue))")
            recordingRequested = false
            DispatchQueue.main.async { Self.showMicPermissionAlert(denied: true) }
            return
        case .notDetermined:
            NSLog("MenuBand SampleVoice: requesting microphone permission")
            AVCaptureDevice.requestAccess(for: .audio) { [weak self] granted in
                NSLog("MenuBand SampleVoice: microphone permission granted=\(granted)")
                DispatchQueue.main.async {
                    guard let self = self else { return }
                    if granted, self.recordingRequested {
                        self.startRecording()
                    } else if !granted {
                        self.recordingRequested = false
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
        hotMicStopWork?.cancel()
        hotMicStopWork = nil
        guard ensureHotMicRunning() else {
            recordingRequested = false
            return
        }
        // Pre-allocate the scratch buffer to ~10 s so a typical record
        // doesn't have to resize. If the user holds longer we grow
        // geometrically inside `ingestInput`.
        guard let scratch = AVAudioPCMBuffer(pcmFormat: storageFormat,
                                             frameCapacity: AVAudioFrameCount(initialScratchFrames)) else {
            NSLog("MenuBand SampleVoice: failed to allocate scratch buffer")
            recordingRequested = false
            return
        }
        scratch.frameLength = 0
        recordScratch = scratch
        recordWriteFrame = 0
        // Arm the keyboard-click skip — first ~12 ms of input is
        // dropped so the recording doesn't open with the record
        // key's mechanical transient.
        framesRemainingToSkip = recordKeyClickSkipFrames
        debugTapBlocksLogged = 0

        recording = true
        let format = inputFormat ?? recordEngine.inputNode.inputFormat(forBus: 0)
        NSLog("MenuBand SampleVoice: recording started instantly (input format ch=\(format.channelCount) sr=\(format.sampleRate) recordEngineRunning=\(recordEngine.isRunning))")
    }

    private func ensureHotMicRunning() -> Bool {
        if inputTapInstalled, recordEngine.isRunning { return true }
        let input = recordEngine.inputNode
        let format = input.inputFormat(forBus: 0)
        // Some virtual input devices return a 0-channel / 0-Hz format
        // when no real device is selected. Bail loudly so the synth
        // doesn't think it has a usable buffer when the user releases.
        guard format.channelCount > 0, format.sampleRate > 0 else {
            NSLog("MenuBand SampleVoice: input format unusable (ch=\(format.channelCount) sr=\(format.sampleRate)) — recordEngine running=\(recordEngine.isRunning)")
            return false
        }
        inputFormat = format
        inputConverter = AVAudioConverter(from: format, to: storageFormat)
        guard inputConverter != nil else {
            NSLog("MenuBand SampleVoice: failed to build converter from \(format) to \(storageFormat)")
            return false
        }
        if !inputTapInstalled {
            input.installTap(onBus: 0, bufferSize: inputTapBufferFrames, format: nil) { [weak self] buffer, _ in
                self?.ingestInput(buffer)
            }
            inputTapInstalled = true
        }
        if !recordEngine.isRunning {
            // CRITICAL: silence the record engine's output path BEFORE
            // start(). AVAudioEngine lazily attaches mainMixerNode and
            // wires it to outputNode the moment the property is first
            // accessed (or prepare/start touches it under the hood).
            // On some CoreAudio routings — notably built-in mic +
            // speaker on Apple Silicon — that creates an implicit
            // input → mainMixer → output path while the input tap is
            // running, producing audible echo of the user's voice
            // through the speakers during recording. Pinning
            // outputVolume to 0 (and reasserting on every restart)
            // kills the loopback. The tap callback runs independent
            // of the output graph, so recording data is unaffected.
            recordEngine.mainMixerNode.outputVolume = 0
            do {
                recordEngine.prepare()
                try recordEngine.start()
            } catch {
                if inputTapInstalled {
                    input.removeTap(onBus: 0)
                    inputTapInstalled = false
                }
                NSLog("MenuBand SampleVoice: hot mic start failed: \(error)")
                return false
            }
            // Belt + suspenders: assert silence again after start in
            // case AVAudioEngine's internal connect happened during
            // start() and reset the volume.
            recordEngine.mainMixerNode.outputVolume = 0
        }
        NSLog("MenuBand SampleVoice: hot mic ready (input format ch=\(format.channelCount) sr=\(format.sampleRate) buffer=\(inputTapBufferFrames))")
        return true
    }

    private func scheduleHotMicStop() {
        hotMicStopWork?.cancel()
        let work = DispatchWorkItem { [weak self] in
            guard let self = self, !self.recording else { return }
            if self.inputTapInstalled {
                self.recordEngine.inputNode.removeTap(onBus: 0)
                self.inputTapInstalled = false
            }
            self.recordEngine.stop()
            self.inputConverter = nil
            self.inputFormat = nil
            NSLog("MenuBand SampleVoice: hot mic idled off")
        }
        hotMicStopWork = work
        DispatchQueue.main.asyncAfter(deadline: .now() + hotMicIdleSeconds,
                                      execute: work)
    }

    /// Stop capture and promote scratch to the active recording.
    /// Returns true iff at least 100 ms of usable samples were captured.
    /// On false, the synth should not switch to the sample backend —
    /// the user likely tapped the key without recording anything real.
    @discardableResult
    func stopRecording() -> Bool {
        recordingRequested = false
        guard recording else {
            NSLog("MenuBand SampleVoice: stopRecording called while not recording")
            return false
        }
        recording = false
        scheduleHotMicStop()
        guard let scratch = recordScratch else {
            recordScratch = nil
            return false
        }
        recordScratch = nil
        let rawFrames = recordWriteFrame
        let minFrames = Int(sampleRate * 0.1) // 100 ms
        guard rawFrames >= minFrames else {
            NSLog("MenuBand SampleVoice: recording too short (\(rawFrames) frames < \(minFrames)) — discarding")
            return false
        }
        let startFrame = trimmedStartFrame(scratch: scratch, frames: rawFrames)
        let frames = rawFrames - startFrame
        guard frames >= minFrames else {
            NSLog("MenuBand SampleVoice: recording too short after trim (\(frames) frames < \(minFrames)) — discarding")
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
            memcpy(dst, src.advanced(by: startFrame), frames * MemoryLayout<Float>.size)
            let stats = shapeCapturedSample(dst, frames: frames)
            NSLog("MenuBand SampleVoice: sample shaped peak \(stats.peakBefore) -> \(stats.peakAfter), rms \(stats.rmsBefore) -> \(stats.rmsAfter), gain=\(stats.gain)")
        }
        bufferLock.lock()
        recordedBuffer = out
        bufferLock.unlock()
        NSLog("MenuBand SampleVoice: recording captured \(frames) frames (\(Double(frames) / sampleRate) s), trimmed \(startFrame) leading frames")
        return true
    }

    private func shapeCapturedSample(_ data: UnsafeMutablePointer<Float>, frames: Int)
        -> (peakBefore: Float, peakAfter: Float, rmsBefore: Float, rmsAfter: Float, gain: Float)
    {
        guard frames > 0 else { return (0, 0, 0, 0, 1) }
        var dc: Float = 0
        var peakBefore: Float = 0
        var sumSqBefore: Float = 0
        for i in 0..<frames {
            let s = data[i]
            dc += s
            let a = abs(s)
            peakBefore = max(peakBefore, a)
            sumSqBefore += s * s
        }
        dc /= Float(frames)
        let rmsBefore = sqrt(sumSqBefore / Float(frames))
        let targetPeak: Float = 0.82
        let targetRMS: Float = 0.18
        let peakGain = peakBefore > 0.000_001 ? targetPeak / peakBefore : 1
        let rmsGain = rmsBefore > 0.000_001 ? targetRMS / rmsBefore : 1
        let gain = min(max(1, min(peakGain, rmsGain)), 8)
        let threshold: Float = 0.22
        let ratio: Float = 4.0
        let limiter: Float = 0.92
        var peakAfter: Float = 0
        var sumSqAfter: Float = 0
        for i in 0..<frames {
            var s = (data[i] - dc) * gain
            let sign: Float = s < 0 ? -1 : 1
            var a = abs(s)
            if a > threshold {
                a = threshold + (a - threshold) / ratio
                s = sign * a
            }
            if s > limiter {
                s = limiter
            } else if s < -limiter {
                s = -limiter
            }
            data[i] = s
            peakAfter = max(peakAfter, abs(s))
            sumSqAfter += s * s
        }
        let rmsAfter = sqrt(sumSqAfter / Float(frames))
        return (peakBefore, peakAfter, rmsBefore, rmsAfter, gain)
    }

    private func trimmedStartFrame(scratch: AVAudioPCMBuffer, frames: Int) -> Int {
        guard let data = scratch.floatChannelData?[0], frames > 0 else { return 0 }
        let window = 256
        let requiredHotWindows = 2
        // If a SHORT hot burst (≤6 windows ≈ 35 ms at 44.1 kHz) is
        // followed by quiet, treat it as the residual keyboard
        // transient that survived the fixed skip and resume the
        // sustained-hot scan AFTER it. This is the difference
        // between "click + the user starts playing 40 ms later" and
        // "click runs into the user's first note" — the former gets
        // a clean leading edge; the latter passes straight through.
        let maxTransientWindows = 6
        var hotWindows = 0
        var transientStart = -1
        var transientLen = 0
        var i = 0
        while i < frames {
            let end = min(frames, i + window)
            var sumSq: Float = 0
            for j in i..<end {
                let s = data[j]
                sumSq += s * s
            }
            let rms = sqrt(sumSq / Float(end - i))
            if rms >= trimThreshold {
                if hotWindows == 0 { transientStart = i }
                hotWindows += 1
                if hotWindows >= requiredHotWindows {
                    return max(0, i - window * (requiredHotWindows - 1) - trimPrerollFrames)
                }
            } else {
                // Drop a short hot run as transient and keep scanning;
                // anything longer counts as the actual onset and the
                // hotWindows tracker already locked in above.
                if hotWindows > 0 && hotWindows <= maxTransientWindows {
                    transientLen = i - transientStart
                    NSLog("MenuBand SampleVoice: dropping leading transient at \(transientStart) (\(transientLen) frames)")
                }
                hotWindows = 0
                transientStart = -1
            }
            i += window
        }
        return 0
    }

    /// Tap-side: convert the input node's buffer into our storage
    /// format and append into the scratch buffer. There's no fixed
    /// cap — the scratch grows geometrically when filled so the user
    /// can hold the record key indefinitely (memory cost is the only
    /// real ceiling). Also computes per-block RMS and forwards to
    /// `onLevel` for the menubar VU meter.
    private func ingestInput(_ buffer: AVAudioPCMBuffer) {
        guard recording else { return }

        // Fast path for the common macOS case: built-in/default mics
        // usually deliver non-interleaved float32 at 44.1k/48k. When the
        // sample rate already matches our storage format, bypass
        // AVAudioConverter entirely. The old path drove one persistent
        // converter with `.endOfStream` for every tap block, which can
        // leave subsequent blocks producing zero frames; that manifested
        // as "recording started" followed by "0 frames".
        if abs(buffer.format.sampleRate - storageFormat.sampleRate) < 0.5,
           let channelData = buffer.floatChannelData {
            var frames = Int(buffer.frameLength)
            guard frames > 0 else { return }
            logTapBlockIfNeeded(buffer: buffer, path: "direct")
            // Drop the keyboard-click prefix transparently. Whole
            // blocks land entirely inside the skip window for the
            // first 1–2 callbacks; we advance the source pointer
            // and shrink the effective frame count when the skip
            // straddles a block boundary.
            var srcOffset = 0
            if framesRemainingToSkip > 0 {
                let skip = min(framesRemainingToSkip, frames)
                framesRemainingToSkip -= skip
                srcOffset = skip
                frames -= skip
                if frames <= 0 { return }
            }
            ensureScratchCapacity(forFramesToAppend: frames)
            guard let scratch = recordScratch,
                  let dst = scratch.floatChannelData?[0] else { return }
            let channelCount = max(1, Int(buffer.format.channelCount))
            var sumSq: Float = 0
            if channelCount == 1 {
                let src = channelData[0].advanced(by: srcOffset)
                memcpy(dst.advanced(by: recordWriteFrame), src,
                       frames * MemoryLayout<Float>.size)
                for i in 0..<frames {
                    let s = src[i]
                    sumSq += s * s
                }
            } else {
                for i in 0..<frames {
                    var mixed: Float = 0
                    for ch in 0..<channelCount {
                        mixed += channelData[ch][i + srcOffset]
                    }
                    mixed /= Float(channelCount)
                    dst[recordWriteFrame + i] = mixed
                    sumSq += mixed * mixed
                }
            }
            publishLevel(sumSq: sumSq, frames: frames)
            recordWriteFrame += frames
            scratch.frameLength = AVAudioFrameCount(recordWriteFrame)
            return
        }

        guard let converter = inputConverter else { return }
        converter.reset()
        logTapBlockIfNeeded(buffer: buffer, path: "converter")

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
        var producedFrames = Int(staging.frameLength)
        guard producedFrames > 0 else { return }

        // Same key-click skip as the direct path — drop the leading
        // transient before it lands in scratch.
        var srcOffset = 0
        if framesRemainingToSkip > 0 {
            let skip = min(framesRemainingToSkip, producedFrames)
            framesRemainingToSkip -= skip
            srcOffset = skip
            producedFrames -= skip
            if producedFrames <= 0 { return }
        }

        // Grow the scratch buffer if this block would overflow. AVAudio
        // PCM buffers don't resize in place, so we re-allocate at 2×
        // current capacity (or just enough to fit, whichever is bigger)
        // and copy the existing contents over before appending.
        ensureScratchCapacity(forFramesToAppend: producedFrames)
        guard let scratch = recordScratch else { return }

        // Mix down to mono if the input is multichannel — pick channel
        // 0 (most macOS mics deliver mono on bus 0 anyway). Storage is
        // single-channel so we only ever write into channelData[0].
        if let srcBase = staging.floatChannelData?[0],
           let dst = scratch.floatChannelData?[0] {
            let src = srcBase.advanced(by: srcOffset)
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
            publishLevel(rms)
            recordWriteFrame += producedFrames
            scratch.frameLength = AVAudioFrameCount(recordWriteFrame)
        }
    }

    private func publishLevel(sumSq: Float, frames: Int) {
        guard frames > 0 else { return }
        publishLevel(sqrt(sumSq / Float(frames)))
    }

    private func publishLevel(_ rms: Float) {
        if let cb = self.onLevel {
            // Hop to main — the renderer reads on its own tick; we just publish.
            DispatchQueue.main.async { cb(rms) }
        }
    }

    private func logTapBlockIfNeeded(buffer: AVAudioPCMBuffer, path: String) {
        guard debugTapBlocksLogged < 4 else { return }
        debugTapBlocksLogged += 1
        NSLog("MenuBand SampleVoice: tap block \(debugTapBlocksLogged) path=\(path) frames=\(buffer.frameLength) ch=\(buffer.format.channelCount) sr=\(buffer.format.sampleRate) written=\(recordWriteFrame)")
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
        // Prepare with a small frame count — AVAudioPlayerNode
        // pre-fetches this many frames before play() and that
        // pre-fetch is on the noteOn critical path. 256 frames @
        // 44.1 kHz ≈ 5.8 ms keeps onset latency tight without
        // starving the audio render thread.
        v.node.prepare(withFrameCount: 256)
        voices[key] = v
        NSLog("MenuBand SampleVoice: attached playback voice channel=\(channel) slot=\(slot)")
        return v
    }

    /// Pitch ratio for `midi` relative to middle C (60). 60 → 1.0.
    @inline(__always)
    private func ratio(forNote midi: UInt8) -> Float {
        Float(pow(2.0, Double(Int(midi) - 60) / 12.0))
    }

    /// Multiplicative pitch factor for the current trackpad bend.
    /// `bendSemitones = 0` → 1.0 (no change).
    @inline(__always)
    private func bendRateFactor() -> Float {
        Float(pow(2.0, Double(bendSemitones) / 12.0))
    }

    /// Linger fade hook. Adjust every currently-playing voice that
    /// matches `midi` (on `channel`) to volume `value/127`. The
    /// MIDISynth path uses CC 11 for the same effect — sample
    /// voice ignores CC 11 so we ramp the player node directly.
    func setNoteVolume(midi: UInt8, channel: UInt8, value: UInt8) {
        let vol = Float(value) / 127.0
        for slot in 0..<voicesPerChannel {
            let key = voiceKey(channel: channel, slot: slot)
            if let voice = voices[key], voice.midi == midi {
                voice.node.volume = vol
            }
        }
    }

    /// Snap every voice on `channel` back to full volume. Used by the
    /// controller when a new noteOn arrives mid-linger-fade — the
    /// channel might have voices at 0.05 volume from the ramp, and
    /// the new note inherits that until the fade resets at the
    /// end (matching CC11 reset on the MIDISynth side).
    func resetChannelVolumes(channel: UInt8) {
        for slot in 0..<voicesPerChannel {
            let key = voiceKey(channel: channel, slot: slot)
            if let voice = voices[key] {
                voice.node.volume = 1.0
            }
        }
    }

    /// Trackpad pitch-bend hook. `amount` is the controller-side
    /// signed bend; one unit = one octave (12 semitones). No clamp
    /// — the trackpad accumulator can swing far past ±1, and
    /// AVAudioUnitVarispeed will pitch-shift the loop accordingly
    /// until the rate hits its own internal range (~0.25×–4.0× ≈
    /// ±2 octaves). Updates every currently playing voice's
    /// varispeed.rate live so the bend slides audible pitch in
    /// real time, and stashes the bend so notes triggered after
    /// this call inherit it.
    func setBend(amount: Float) {
        bendSemitones = amount * 12.0
        let factor = bendRateFactor()
        for (_, v) in voices {
            if v.node.isPlaying {
                v.varispeed.rate = ratio(forNote: v.midi) * factor
            }
        }
    }

    func noteOn(_ midi: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        bufferLock.lock()
        let buf = recordedBuffer
        bufferLock.unlock()
        guard let buf = buf else {
            // No recording yet — silent noteOn. The synth shouldn't
            // route to this backend in that state, but defend anyway.
            NSLog("MenuBand SampleVoice: noteOn ignored — no recorded buffer")
            return
        }
        guard attached, engine != nil else {
            NSLog("MenuBand SampleVoice: noteOn ignored — voice not attached")
            return
        }
        // The controller rotates `nextMelodicChannel()` 0..3 on every
        // press, so the same midi can land on a fresh channel while
        // the previous channel's slot is still mid-release (~80ms
        // fade). For a looping sample buffer that produces a
        // perceptible echo — the same loop ringing out at the same
        // pitch on two voices simultaneously. Hard-stop every voice
        // already playing this midi (any channel, any slot) BEFORE
        // we kick off the new one so only one loop ever sounds per
        // pitch. No fade — the new attack masks the cut.
        stopAllVoices(playing: midi)
        let slot = nextSlot(channel: channel, midi: midi)
        guard let voice = ensureVoice(channel: channel, slot: slot) else {
            NSLog("MenuBand SampleVoice: noteOn ignored — failed to allocate voice")
            return
        }

        // Cancel any pending release-fade — we're retriggering the
        // slot before the previous tail finished.
        voice.releaseWork?.cancel()
        voice.releaseWork = nil

        voice.midi = midi
        // Compose the pitch ratio from the note's base ratio AND the
        // current trackpad pitch bend so dragging the cursor while a
        // sample voice rings shifts pitch in real time — mirror of
        // the MIDISynth pitch-bend path.
        voice.varispeed.rate = ratio(forNote: midi) * bendRateFactor()
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

    /// Immediately silence every Voice currently playing `midi`
    /// regardless of channel/slot. Used at noteOn to prevent stacking
    /// echoes from the controller's per-press channel rotation; the
    /// fresh attack starts on a clean slate.
    private func stopAllVoices(playing midi: UInt8) {
        for (_, v) in voices where v.midi == midi && v.node.isPlaying {
            v.releaseWork?.cancel()
            v.releaseWork = nil
            v.node.stop()
        }
    }

    func noteOff(_ midi: UInt8, channel: UInt8 = 0) {
        // Hard-stop: short attack / short decay is the desired
        // default for this sampler — no 80ms scheduleRelease fade,
        // no main-thread volume ramp. AVAudioPlayerNode.stop()
        // halts the render synchronously so the loop dies on the
        // current buffer slice. Per-note round-robin can leave the
        // same midi in several slots at once when the user
        // re-presses faster than this release; stop them all.
        for slot in 0..<voicesPerChannel {
            let key = voiceKey(channel: channel, slot: slot)
            guard let voice = voices[key], voice.midi == midi,
                  voice.node.isPlaying else { continue }
            voice.releaseWork?.cancel()
            voice.releaseWork = nil
            voice.node.stop()
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
        NotificationCenter.default.post(name: .menuBandMicPermissionAlertWillShow,
                                        object: nil)
        let alert = NSAlert()
        alert.messageText = "Menu Band can't reach the microphone"
        alert.informativeText = denied
            ? "Microphone access has been denied. Open System Settings → Privacy & Security → Microphone and toggle Menu Band on, then try the sample voice again. If this debug build was launched from Terminal or Codex, macOS may list Terminal/Codex instead of Menu Band; the installed app bundle will appear as Menu Band after it asks once."
            : "Menu Band needs microphone access to record sample notes. Grant permission when macOS asks. If this debug build was launched from Terminal or Codex, macOS may list Terminal/Codex instead of Menu Band; the installed app bundle will appear as Menu Band after it asks once."
        alert.alertStyle = .warning
        alert.addButton(withTitle: "Open System Settings")
        alert.addButton(withTitle: "Cancel")
        // Menu Band keeps its popovers/floating piano at pop-up levels.
        // A vanilla `runModal()` alert can appear behind those surfaces,
        // which is especially confusing for a permission failure. Lift
        // the alert window one notch above our popovers before entering
        // the modal loop so it is the frontmost Menu Band surface.
        let window = alert.window
        window.level = .screenSaver
        window.collectionBehavior.insert(.canJoinAllSpaces)
        NSApp.activate(ignoringOtherApps: true)
        window.orderFrontRegardless()
        let response = alert.runModal()
        if response == .alertFirstButtonReturn {
            // x-apple.systempreferences:com.apple.preference.security?Privacy_Microphone
            if let url = URL(string: "x-apple.systempreferences:com.apple.preference.security?Privacy_Microphone") {
                NSWorkspace.shared.open(url)
            }
        }
    }
}
