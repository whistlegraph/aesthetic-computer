import Foundation
import AVFoundation
import AppKit
import Accelerate

extension Notification.Name {
    /// Fired (on main) whenever tape state, position, or buffer changes
    /// in a way the UI cares about. Listeners refresh the menubar icon
    /// and any popover transport widgets.
    static let menuBandTapeChanged = Notification.Name("MenuBandTapeChanged")
    /// Fired when the user flips the tape-deck feature flag in the
    /// About window. AppDelegate re-reads UserDefaults, resizes the
    /// status item, and redraws the menubar icon.
    static let menuBandTapeFeatureChanged = Notification.Name("MenuBandTapeFeatureChanged")
    /// Fired when the user flips the right-hand percussion split in the
    /// About window. AppDelegate re-applies the split's side effects.
    static let menuBandPercussionSplitChanged = Notification.Name("MenuBandPercussionSplitChanged")
    /// Fired when the user flips "Use AC OS MIDI" in the About window.
    /// AppDelegate forwards the new flag to the synth so melodic notes
    /// route through the native gm_synth voices (or back to MIDISynth).
    static let menuBandUseACMIDIChanged = Notification.Name("MenuBandUseACMIDIChanged")
}

/// 90-second analog-style tape with **stem separation** baked in.
///
/// Two float buffers run in parallel — synth (stereo) and mic (mono) —
/// each fed by its own ingest path with an independent write head.
/// Playback sums them on the fly into a temporary buffer scheduled
/// through `AVAudioPlayerNode`; eject writes three AAC `.m4a` files
/// (mix / instruments / vocals) into a folder so the recording can be
/// dropped into a DAW with the vocals already split out from the
/// instruments.
///
///     synth.mainMixerNode ── (waveform tap fork) ──► ingestSynth ──► synthBuffer
///     sampleVoice.inputNode ── (mic tap fork)   ──► ingestMic   ──► micBuffer
///
///     playerNode ──► preLimiterMixer   ← scheduled from sum(synth, mic)
final class MenuBandTape {
    enum State: Equatable {
        case idle
        case recording
        case paused
        case playing
    }

    static let sampleRate: Double = 44_100
    static let maxDurationSeconds: Double = 90.0
    static let maxFrames: Int = Int(sampleRate * maxDurationSeconds)

    // MARK: - Storage formats

    /// Stereo float32 — the synth's stems live here.
    private let synthFormat: AVAudioFormat
    /// Mono float32 — the mic's stem lives here.
    private let micFormat: AVAudioFormat
    /// Stereo float32 — playback schedules a buffer in this format
    /// summed from synthBuffer + micBuffer.
    private let mixFormat: AVAudioFormat

    private let synthBuffer: AVAudioPCMBuffer
    private let micBuffer: AVAudioPCMBuffer
    private let bufferLock = NSLock()

    private var synthWriteFrame: Int = 0
    private var micWriteFrame: Int = 0
    private var playFrame: Int = 0

    /// Leading-transient trim. The hot mic is already running when the
    /// user presses backtick to arm REC, so the first frames captured
    /// include the mechanical click of the record key itself (and a few
    /// ms of pre-press room tone from the in-flight tap block). The
    /// sample-voice capture path drops the same window via
    /// `recordKeyClickSkipFrames`; the tape needs its own copy because
    /// it ingests raw frames directly. We skip the SAME count from both
    /// stems so synth and mic stay sample-aligned for clean DAW stems.
    private static let leadTrimFrames = Int(sampleRate * 0.035) // ~35 ms
    private var synthLeadSkip: Int = 0
    private var micLeadSkip: Int = 0

    private(set) var state: State = .idle {
        didSet { if oldValue != state { postChange() } }
    }

    var hasRecording: Bool {
        bufferLock.lock(); defer { bufferLock.unlock() }
        return max(synthWriteFrame, micWriteFrame) > 0
    }
    var durationSeconds: Double {
        bufferLock.lock(); defer { bufferLock.unlock() }
        return Double(max(synthWriteFrame, micWriteFrame)) / Self.sampleRate
    }
    var positionSeconds: Double {
        if state == .playing, let render = playerRenderFrame() {
            return Double(playStartFrame + render) / Self.sampleRate
        }
        return Double(playFrame) / Self.sampleRate
    }

    private var micConverter: AVAudioConverter?
    private var micConverterSrcFormat: AVAudioFormat?
    private var synthConverter: AVAudioConverter?
    private var synthConverterSrcFormat: AVAudioFormat?

    // MARK: - Playback

    let playerNode = AVAudioPlayerNode()
    private weak var engine: AVAudioEngine?
    private weak var playerOutput: AVAudioNode?
    private var playerAttached = false
    private var playStartFrame: Int = 0
    private var playSampleTimeZero: AVAudioFramePosition = 0
    private var playerRenderArmed = false

    /// Track which date the current take began on. Used by `eject` to
    /// stamp the cover art + name the file. Captured at `record()`.
    private var recordStartDate: Date = Date()

    /// Cached eject result for the current take. Set on first eject,
    /// reused for subsequent ejects of the same recording, cleared
    /// when `record()` is called for a new take. Saves us from
    /// re-encoding 90 s of audio every time the user drags the
    /// cassette out OR clicks the popover EJECT button after dragging.
    private var cachedEject: EjectResult?

    init() {
        guard let synth = AVAudioFormat(commonFormat: .pcmFormatFloat32,
                                         sampleRate: Self.sampleRate,
                                         channels: 2,
                                         interleaved: false),
              let mic = AVAudioFormat(commonFormat: .pcmFormatFloat32,
                                       sampleRate: Self.sampleRate,
                                       channels: 1,
                                       interleaved: false)
        else {
            fatalError("MenuBandTape: failed to build storage formats")
        }
        self.synthFormat = synth
        self.micFormat = mic
        self.mixFormat = synth   // playback is always stereo
        guard let sb = AVAudioPCMBuffer(pcmFormat: synth,
                                         frameCapacity: AVAudioFrameCount(Self.maxFrames)),
              let mb = AVAudioPCMBuffer(pcmFormat: mic,
                                         frameCapacity: AVAudioFrameCount(Self.maxFrames))
        else {
            fatalError("MenuBandTape: failed to allocate 90s stem buffers")
        }
        sb.frameLength = AVAudioFrameCount(Self.maxFrames)
        mb.frameLength = AVAudioFrameCount(Self.maxFrames)
        self.synthBuffer = sb
        self.micBuffer = mb
    }

    // MARK: - Engine attach

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !playerAttached else { return }
        self.engine = engine
        self.playerOutput = output
        engine.attach(playerNode)
        engine.connect(playerNode, to: output, format: mixFormat)
        playerAttached = true
    }

    // MARK: - Transport

    func record() {
        switch state {
        case .recording: return
        case .playing, .paused: stop()
        case .idle: break
        }
        bufferLock.lock()
        if let l = synthBuffer.floatChannelData?[0],
           let r = synthBuffer.floatChannelData?[1] {
            memset(l, 0, Self.maxFrames * MemoryLayout<Float>.size)
            memset(r, 0, Self.maxFrames * MemoryLayout<Float>.size)
        }
        if let m = micBuffer.floatChannelData?[0] {
            memset(m, 0, Self.maxFrames * MemoryLayout<Float>.size)
        }
        synthWriteFrame = 0
        micWriteFrame = 0
        playFrame = 0
        synthLeadSkip = Self.leadTrimFrames
        micLeadSkip = Self.leadTrimFrames
        recordStartDate = Date()
        cachedEject = nil
        bufferLock.unlock()
        midiLock.lock()
        midiEvents.removeAll()
        midiStart = ProcessInfo.processInfo.systemUptime
        midiLock.unlock()
        state = .recording
    }

    func stop() {
        switch state {
        case .recording:
            state = .idle
            postChange()
        case .playing, .paused:
            playerNode.stop()
            playerRenderArmed = false
            state = .idle
        case .idle:
            return
        }
    }

    func play() {
        guard state != .playing else { return }
        guard hasRecording else { return }
        guard let _ = engine else { return }
        let total = bufferLock.withLock { max(synthWriteFrame, micWriteFrame) }
        if playFrame >= total { playFrame = 0 }
        let start = playFrame
        let length = total - start
        guard length > 0 else { return }
        guard let mixSlice = renderMixSlice(startFrame: start, frames: length) else {
            return
        }
        playerNode.stop()
        playerRenderArmed = false
        playStartFrame = start
        playerNode.scheduleBuffer(mixSlice, at: nil, options: []) { [weak self] in
            DispatchQueue.main.async {
                guard let self = self, self.state == .playing else { return }
                let total = self.bufferLock.withLock {
                    max(self.synthWriteFrame, self.micWriteFrame)
                }
                self.playFrame = total
                self.playerRenderArmed = false
                self.state = .idle
            }
        }
        playerNode.play()
        state = .playing
    }

    func pause() {
        guard state == .playing else { return }
        let snapshot = positionSeconds
        playerNode.stop()
        playerRenderArmed = false
        playFrame = Int(snapshot * Self.sampleRate)
        state = .paused
    }

    func rewind() {
        let wasPlaying = (state == .playing)
        if wasPlaying { playerNode.stop(); playerRenderArmed = false }
        playFrame = 0
        if wasPlaying { state = .idle; play() }
        else if state == .playing { state = .idle }
        else { postChange() }
    }

    func seekToEnd() {
        let wasPlaying = (state == .playing)
        if wasPlaying { playerNode.stop(); playerRenderArmed = false; state = .idle }
        playFrame = bufferLock.withLock { max(synthWriteFrame, micWriteFrame) }
        postChange()
    }

    func clear() {
        stop()
        bufferLock.lock()
        synthWriteFrame = 0
        micWriteFrame = 0
        playFrame = 0
        bufferLock.unlock()
        postChange()
    }

    // MARK: - Eject (drag-out to Finder)

    /// Result of an eject — a single multi-channel WAV plus the date
    /// + duration metadata the cover-art renderer needs.
    struct EjectResult {
        let file: URL
        let date: Date
        let duration: TimeInterval
        var midi: URL? = nil   // sidecar .mid of the notes played (if any)
        // The generative album art (already stamped on `file`). Carried as the
        // in-memory image so downstream artifact-building uses it DIRECTLY —
        // never re-reading it off disk via `icon(forFile:)`, which races the
        // async icon write and returns the stale generic type icon.
        var cover: NSImage? = nil
    }

    // MIDI performance capture — note events (relative to record start) so a
    // take carries editable notes for Ableton alongside the rendered audio.
    private var midiEvents: [MidiFile.Event] = []
    private var midiStart: TimeInterval = 0
    private var midiLock = NSLock()

    /// Called from the synth's note hook (via the controller) on every
    /// note-on/off. Records only while the tape is rolling.
    func ingestNote(_ note: UInt8, velocity: UInt8, on: Bool, channel: UInt8, pan: UInt8) {
        guard state == .recording else { return }
        let t = ProcessInfo.processInfo.systemUptime - midiStart
        midiLock.lock()
        midiEvents.append(MidiFile.Event(time: t, note: note, velocity: velocity,
                                          on: on, channel: channel, pan: pan))
        midiLock.unlock()
    }

    /// Render the recording into a single 4-channel WAV:
    ///
    ///     Channel 1 — synth L
    ///     Channel 2 — synth R
    ///     Channel 3 — mic L (vocals duplicated)
    ///     Channel 4 — mic R (vocals duplicated)
    ///
    /// DAWs that handle multi-channel imports (Logic, ProTools,
    /// Reaper, Ableton, Audition) offer to split the file into
    /// separate tracks at import time; players that don't (QuickTime,
    /// Finder preview) just play channels 1-2 — i.e. the synth — which
    /// is a reasonable default.
    ///
    /// The mix is reconstructable from the file's channels (sum 1+3
    /// for left, 2+4 for right). We don't pre-bake a mix track here;
    /// a single 4-channel WAV is the cleanest "tape" artifact.
    @discardableResult
    /// The audible span of the take: first and last frames above a silence
    /// threshold across both stems, so leading dead air (count-in → first note)
    /// AND a trailing tail before Escape are both trimmed. Small pads keep the
    /// attack and release intact. Returns (0,0) when nothing is audible.
    private func trimmedRange(total: Int) -> (start: Int, end: Int) {
        guard total > 0 else { return (0, 0) }
        let threshold: Float = 0.0008
        var first = total, last = 0
        bufferLock.lock()
        if let l = synthBuffer.floatChannelData?[0],
           let r = synthBuffer.floatChannelData?[1] {
            var i = 0
            while i < total { if abs(l[i]) > threshold || abs(r[i]) > threshold { first = i; break }; i += 1 }
            var j = total - 1
            while j >= 0 { if abs(l[j]) > threshold || abs(r[j]) > threshold { last = j + 1; break }; j -= 1 }
        }
        if let m = micBuffer.floatChannelData?[0] {
            var i = 0
            while i < first { if abs(m[i]) > threshold { first = i; break }; i += 1 }
            var j = total - 1
            while j >= last { if abs(m[j]) > threshold { last = j + 1; break }; j -= 1 }
        }
        bufferLock.unlock()
        guard last > first else { return (0, 0) }
        let start = max(0, first - Int(Self.sampleRate * 0.03))   // 30 ms pre-roll
        let end = min(total, last + Int(Self.sampleRate * 0.15))  // 150 ms tail
        return (start, end)
    }

    /// Peak-normalization gain to master the take up to ~-1 dBFS. The signal is
    /// already through the compressor + limiter (mastered tone); this just lifts
    /// the ceiling so quiet takes aren't quiet files. Same gain on both stems so
    /// their balance is preserved. Boost is capped so a near-silent take doesn't
    /// roar its noise floor.
    private func normalizationGain(from offset: Int, count: Int) -> Float {
        guard count > 0 else { return 1 }
        var peak: Float = 0
        bufferLock.lock()
        if let sl = synthBuffer.floatChannelData?[0],
           let sr = synthBuffer.floatChannelData?[1] {
            var p0: Float = 0, p1: Float = 0
            vDSP_maxmgv(sl.advanced(by: offset), 1, &p0, vDSP_Length(count))
            vDSP_maxmgv(sr.advanced(by: offset), 1, &p1, vDSP_Length(count))
            peak = max(peak, p0, p1)
        }
        if let mc = micBuffer.floatChannelData?[0] {
            var pm: Float = 0
            vDSP_maxmgv(mc.advanced(by: offset), 1, &pm, vDSP_Length(count))
            peak = max(peak, pm)
        }
        bufferLock.unlock()
        guard peak > 1e-5 else { return 1 }
        return min(0.89 / peak, 24)   // -1 dBFS target, capped boost
    }

    func eject() -> EjectResult? {
        guard hasRecording else { return nil }
        // Reuse the cached result if the on-disk file is still
        // there. Same take → same file → faster drag, no duplicate
        // tapes accumulating in /tmp.
        if let cached = cachedEject,
           FileManager.default.fileExists(atPath: cached.file.path) {
            return cached
        }
        let rawFrames = bufferLock.withLock { max(synthWriteFrame, micWriteFrame) }
        // Trim dead air off BOTH ends — the take starts at the first note and
        // ends at the last, regardless of the count-in lead or the pause before
        // Escape. `offset` is where the trimmed audio begins in the buffer.
        let (offset, endFrame) = trimmedRange(total: rawFrames)
        let frames = endFrame - offset
        NSLog("MenuBandTape: eject raw=\(rawFrames) offset=\(offset) end=\(endFrame) frames=\(frames)")
        guard frames > 0 else { return nil }
        let duration = Double(frames) / Self.sampleRate
        let date = recordStartDate
        let normGain = normalizationGain(from: offset, count: frames)   // master to ~-1 dBFS

        // Pick a friendly filename + dodge collisions in /tmp.
        let baseName = Self.makeCuteName(date: date)
        let tmpRoot = FileManager.default.temporaryDirectory
        var url = tmpRoot.appendingPathComponent("\(baseName).wav")
        var suffix = 1
        while FileManager.default.fileExists(atPath: url.path) {
            suffix += 1
            url = tmpRoot.appendingPathComponent("\(baseName)-\(suffix).wav")
        }

        // 4-channel signed-16 WAV (the lingua franca for DAW import).
        // 32-bit float would preserve internal headroom but some
        // legacy DAW versions choke on float WAV; 16-bit is the
        // safest format that every audio app on macOS can open.
        // 4 channels need an EXPLICIT layout — the channels/interleaved
        // convenience initializer only knows the standard mono/stereo layouts
        // and returns nil for 4. Discrete-in-order = four independent tracks,
        // exactly the multitrack "tape" artifact we want.
        guard let quadLayout = AVAudioChannelLayout(
            layoutTag: kAudioChannelLayoutTag_DiscreteInOrder | 4)
        else { NSLog("MenuBandTape: eject nil — quadLayout"); return nil }
        let outFormat = AVAudioFormat(commonFormat: .pcmFormatInt16,
                                       sampleRate: Self.sampleRate,
                                       interleaved: true,
                                       channelLayout: quadLayout)
        // Float storage format for the in-memory 4-channel buffer
        // we hand to the converter. Non-interleaved so we can fill
        // each channel independently.
        let quadFloat = AVAudioFormat(commonFormat: .pcmFormatFloat32,
                                       sampleRate: Self.sampleRate,
                                       interleaved: false,
                                       channelLayout: quadLayout)

        // Optional so we can release it (→ flush + close the WAV header) BEFORE
        // appending the RIFF metadata chunk; a still-open AVAudioFile would
        // rewrite the header on deinit and clobber the appended LIST.
        var file: AVAudioFile?
        do {
            try? FileManager.default.removeItem(at: url)
            file = try AVAudioFile(forWriting: url,
                                    settings: outFormat.settings,
                                    commonFormat: .pcmFormatInt16,
                                    interleaved: true)
        } catch {
            NSLog("MenuBandTape: eject WAV open failed: \(error)")
            return nil
        }

        // Stream the recording out in 8192-frame chunks so the work
        // buffer stays small. Each chunk: copy synth L/R + mic into
        // a 4-channel float buffer, convert to int16, write.
        guard let converter = AVAudioConverter(from: quadFloat, to: outFormat)
        else { NSLog("MenuBandTape: eject nil — converter"); return nil }
        let chunkFrames = 8192
        var cursor = 0
        while cursor < frames {
            let take = min(chunkFrames, frames - cursor)
            guard let src = AVAudioPCMBuffer(pcmFormat: quadFloat,
                                              frameCapacity: AVAudioFrameCount(take))
            else { NSLog("MenuBandTape: eject nil — src buffer"); return nil }
            src.frameLength = AVAudioFrameCount(take)
            bufferLock.lock()
            if let sl = synthBuffer.floatChannelData?[0],
               let sr = synthBuffer.floatChannelData?[1],
               let mc = micBuffer.floatChannelData?[0],
               let d0 = src.floatChannelData?[0],
               let d1 = src.floatChannelData?[1],
               let d2 = src.floatChannelData?[2],
               let d3 = src.floatChannelData?[3] {
                let read = offset + cursor
                memcpy(d0, sl.advanced(by: read), take * MemoryLayout<Float>.size)
                memcpy(d1, sr.advanced(by: read), take * MemoryLayout<Float>.size)
                // Mic is mono — duplicate into ch3 + ch4 so DAWs
                // that auto-pair-stereo see a vocal stereo track.
                memcpy(d2, mc.advanced(by: read), take * MemoryLayout<Float>.size)
                memcpy(d3, mc.advanced(by: read), take * MemoryLayout<Float>.size)
                // Master gain — normalize the whole take to ~-1 dBFS.
                if normGain != 1 {
                    var g = normGain
                    vDSP_vsmul(d0, 1, &g, d0, 1, vDSP_Length(take))
                    vDSP_vsmul(d1, 1, &g, d1, 1, vDSP_Length(take))
                    vDSP_vsmul(d2, 1, &g, d2, 1, vDSP_Length(take))
                    vDSP_vsmul(d3, 1, &g, d3, 1, vDSP_Length(take))
                }
            }
            bufferLock.unlock()

            guard let dst = AVAudioPCMBuffer(pcmFormat: outFormat,
                                              frameCapacity: AVAudioFrameCount(take))
            else { NSLog("MenuBandTape: eject nil — dst buffer"); return nil }
            var supplied = false
            var error: NSError?
            let status = converter.convert(to: dst, error: &error) { _, outStatus in
                if supplied {
                    outStatus.pointee = .endOfStream
                    return nil
                }
                supplied = true
                outStatus.pointee = .haveData
                return src
            }
            if status == .error {
                NSLog("MenuBandTape: eject convert failed: \(error?.localizedDescription ?? "?")")
                return nil
            }
            do {
                try file?.write(from: dst)
            } catch {
                NSLog("MenuBandTape: eject write failed: \(error)")
                return nil
            }
            cursor += take
        }
        file = nil   // flush + close the WAV before touching its bytes below

        // Track metadata: RIFF LIST-INFO tags (also surfaced by Spotlight) plus
        // a Finder comment. Written BEFORE the icon so the byte-rewrite can't
        // strip the custom-icon resource fork. artist = "Menu Band".
        let dateISO: String = {
            let df = DateFormatter()
            df.locale = Locale(identifier: "en_US_POSIX")
            df.dateFormat = "yyyy-MM-dd"
            return df.string(from: date)
        }()
        let mmss = String(format: "%d:%02d", Int(duration) / 60, Int(duration) % 60)
        let comment = "\(mmss) · 4-channel stems (synth L/R + mic) · panned by keyboard position"
        TakeMetadata.writeWavInfo(url: url, title: baseName, artist: "Menu Band",
                                  dateISO: dateISO, software: "Menu Band", comment: comment)

        // Album-art cover for the .wav — a bold generative icon with the length
        // set large. (No waveform; the icon renderer ignores it.)
        let icon = TapeCoverArt.makeIcon(date: date, duration: duration)
        NSWorkspace.shared.setIcon(icon, forFile: url.path, options: [])
        TakeMetadata.setFinderComment(url: url, "\(baseName) — Menu Band · \(comment)")

        // Sidecar .mid of the performance — same basename as the WAV. Times
        // are shifted by the trimmed lead so the notes line up with the audio.
        var midiURL: URL?
        midiLock.lock()
        let events = midiEvents
        midiLock.unlock()
        if !events.isEmpty {
            let leadSeconds = Double(offset) / Self.sampleRate
            let shifted = events.map {
                MidiFile.Event(time: max(0, $0.time - leadSeconds), note: $0.note,
                               velocity: $0.velocity, on: $0.on, channel: $0.channel,
                               pan: $0.pan)
            }
            let mURL = url.deletingPathExtension().appendingPathExtension("mid")
            if (try? MidiFile.data(events: shifted, trackName: baseName).write(to: mURL)) != nil {
                midiURL = mURL
            }
        }

        NSLog("MenuBandTape: ejected \(baseName).wav (\(duration) s, 4ch, \(events.count) midi ev) → \(url.path)")
        let result = EjectResult(file: url, date: date, duration: duration, midi: midiURL, cover: icon)
        cachedEject = result
        return result
    }

    // MARK: - Ingest

    func ingestSynth(_ buffer: AVAudioPCMBuffer) {
        guard state == .recording else { return }
        let frames = Int(buffer.frameLength)
        guard frames > 0 else { return }
        let inFmt = buffer.format
        if inFmt.commonFormat == .pcmFormatFloat32,
           inFmt.channelCount == 2,
           !inFmt.isInterleaved,
           abs(inFmt.sampleRate - Self.sampleRate) < 0.5,
           let inL = buffer.floatChannelData?[0],
           let inR = buffer.floatChannelData?[1] {
            writeSynthFrames(left: inL, right: inR, frames: frames)
            return
        }
        if synthConverterSrcFormat != inFmt {
            synthConverter = AVAudioConverter(from: inFmt, to: synthFormat)
            synthConverterSrcFormat = inFmt
        }
        guard let conv = synthConverter,
              let scratch = AVAudioPCMBuffer(
                pcmFormat: synthFormat,
                frameCapacity: AVAudioFrameCount(
                    Int(Double(frames) * (Self.sampleRate / inFmt.sampleRate)) + 16))
        else { return }
        var supplied = false
        var err: NSError?
        // .noDataNow (NOT .endOfStream) once this buffer is consumed: the
        // converter produces what it can and RETAINS its resampling filter
        // state for the next buffer. endOfStream would flush + terminate it
        // (dropping the rest of the take), and reset()-per-buffer would clear
        // the filter each time (audible pops at every buffer boundary). This
        // keeps one continuous, glitch-free stream.
        let status = conv.convert(to: scratch, error: &err) { _, outStatus in
            if supplied { outStatus.pointee = .noDataNow; return nil }
            supplied = true; outStatus.pointee = .haveData
            return buffer
        }
        guard status != .error,
              scratch.frameLength > 0,
              let l = scratch.floatChannelData?[0],
              let r = scratch.floatChannelData?[1] else { return }
        writeSynthFrames(left: l, right: r, frames: Int(scratch.frameLength))
    }

    func ingestMic(_ buffer: AVAudioPCMBuffer) {
        guard state == .recording else { return }
        let frames = Int(buffer.frameLength)
        guard frames > 0 else { return }
        let inFmt = buffer.format
        if inFmt.commonFormat == .pcmFormatFloat32,
           abs(inFmt.sampleRate - Self.sampleRate) < 0.5,
           !inFmt.isInterleaved,
           let channelData = buffer.floatChannelData {
            let ch = max(1, Int(inFmt.channelCount))
            if ch == 1 {
                writeMicFrames(mono: channelData[0], frames: frames)
            } else {
                var mono = [Float](repeating: 0, count: frames)
                let denom = Float(ch)
                for i in 0..<frames {
                    var s: Float = 0
                    for c in 0..<ch { s += channelData[c][i] }
                    mono[i] = s / denom
                }
                mono.withUnsafeBufferPointer { bp in
                    if let base = bp.baseAddress {
                        writeMicFrames(mono: base, frames: frames)
                    }
                }
            }
            return
        }
        if micConverterSrcFormat != inFmt {
            micConverter = AVAudioConverter(from: inFmt, to: micFormat)
            micConverterSrcFormat = inFmt
        }
        guard let conv = micConverter else { return }
        let estFrames = Int(Double(frames) * (Self.sampleRate / inFmt.sampleRate)) + 16
        guard let scratch = AVAudioPCMBuffer(pcmFormat: micFormat,
                                              frameCapacity: AVAudioFrameCount(estFrames))
        else { return }
        var supplied = false
        var err: NSError?
        let status = conv.convert(to: scratch, error: &err) { _, outStatus in
            if supplied { outStatus.pointee = .noDataNow; return nil }   // keep filter state (see ingestSynth)
            supplied = true; outStatus.pointee = .haveData
            return buffer
        }
        guard status != .error,
              scratch.frameLength > 0,
              let mono = scratch.floatChannelData?[0] else { return }
        writeMicFrames(mono: mono, frames: Int(scratch.frameLength))
    }

    // MARK: - Buffer writers

    private func writeSynthFrames(left: UnsafePointer<Float>,
                                   right: UnsafePointer<Float>,
                                   frames: Int) {
        bufferLock.lock()
        // Drop the leading record-key transient window before anything
        // lands in the buffer. Whole blocks inside the window vanish;
        // a block straddling the boundary advances the source pointer.
        var left = left, right = right, frames = frames
        if synthLeadSkip > 0 {
            let skip = min(synthLeadSkip, frames)
            synthLeadSkip -= skip
            left = left.advanced(by: skip)
            right = right.advanced(by: skip)
            frames -= skip
            if frames <= 0 { bufferLock.unlock(); return }
        }
        let remaining = Self.maxFrames - synthWriteFrame
        let take = min(frames, remaining)
        if take > 0,
           let dl = synthBuffer.floatChannelData?[0],
           let dr = synthBuffer.floatChannelData?[1] {
            let off = synthWriteFrame
            memcpy(dl.advanced(by: off), left,  take * MemoryLayout<Float>.size)
            memcpy(dr.advanced(by: off), right, take * MemoryLayout<Float>.size)
            synthWriteFrame += take
        }
        let bothFull = synthWriteFrame >= Self.maxFrames && micWriteFrame >= Self.maxFrames
        bufferLock.unlock()
        if bothFull && state == .recording {
            DispatchQueue.main.async { [weak self] in
                guard self?.state == .recording else { return }
                self?.stop()
            }
        }
    }

    private func writeMicFrames(mono: UnsafePointer<Float>, frames: Int) {
        bufferLock.lock()
        // Same leading record-key transient trim as the synth stem,
        // and the SAME frame count, so the two stems stay aligned.
        var mono = mono, frames = frames
        if micLeadSkip > 0 {
            let skip = min(micLeadSkip, frames)
            micLeadSkip -= skip
            mono = mono.advanced(by: skip)
            frames -= skip
            if frames <= 0 { bufferLock.unlock(); return }
        }
        let remaining = Self.maxFrames - micWriteFrame
        let take = min(frames, remaining)
        if take > 0, let dst = micBuffer.floatChannelData?[0] {
            memcpy(dst.advanced(by: micWriteFrame), mono,
                   take * MemoryLayout<Float>.size)
            micWriteFrame += take
        }
        let bothFull = synthWriteFrame >= Self.maxFrames && micWriteFrame >= Self.maxFrames
        bufferLock.unlock()
        if bothFull && state == .recording {
            DispatchQueue.main.async { [weak self] in
                guard self?.state == .recording else { return }
                self?.stop()
            }
        }
    }

    // MARK: - Playback helpers

    private func renderMixSlice(startFrame: Int, frames: Int) -> AVAudioPCMBuffer? {
        guard let slice = AVAudioPCMBuffer(pcmFormat: mixFormat,
                                            frameCapacity: AVAudioFrameCount(frames))
        else { return nil }
        slice.frameLength = AVAudioFrameCount(frames)
        bufferLock.lock()
        if let sl = synthBuffer.floatChannelData?[0],
           let sr = synthBuffer.floatChannelData?[1],
           let mc = micBuffer.floatChannelData?[0],
           let ol = slice.floatChannelData?[0],
           let or = slice.floatChannelData?[1] {
            // Synth contributes its real L/R; mic is summed into both
            // channels. Same mix the eject path bakes into mix.m4a so
            // the user hears exactly what gets exported.
            for i in 0..<frames {
                let s = mc[startFrame + i]
                ol[i] = sl[startFrame + i] + s
                or[i] = sr[startFrame + i] + s
            }
        }
        bufferLock.unlock()
        return slice
    }

    private func playerRenderFrame() -> Int? {
        guard playerNode.isPlaying,
              let lastRender = playerNode.lastRenderTime,
              lastRender.isSampleTimeValid,
              let playerTime = playerNode.playerTime(forNodeTime: lastRender)
        else { return nil }
        if !playerRenderArmed {
            playSampleTimeZero = playerTime.sampleTime
            playerRenderArmed = true
            return 0
        }
        let delta = Int(playerTime.sampleTime - playSampleTimeZero)
        return max(0, delta)
    }

    private func postChange() {
        DispatchQueue.main.async {
            NotificationCenter.default.post(
                name: .menuBandTapeChanged,
                object: self)
        }
    }

    // MARK: - Mix waveform for cover art

    /// Downsample the recording's mix (synth+mic) to `buckets` RMS
    /// values. Drives the waveform plotted across the cassette's
    /// label-card cover art.
    private func mixDownsampleRMS(frames: Int, buckets: Int) -> [Float] {
        guard buckets > 0, frames > 0 else { return [] }
        let bucketLen = max(1, frames / buckets)
        var out = [Float](repeating: 0, count: buckets)
        bufferLock.lock()
        defer { bufferLock.unlock() }
        guard let sl = synthBuffer.floatChannelData?[0],
              let sr = synthBuffer.floatChannelData?[1],
              let mc = micBuffer.floatChannelData?[0]
        else { return [] }
        for b in 0..<buckets {
            let start = b * bucketLen
            let end = min(frames, start + bucketLen)
            if start >= end { break }
            var sumSq: Float = 0
            for i in start..<end {
                let mix = (sl[i] + sr[i]) * 0.5 + mc[i]
                sumSq += mix * mix
            }
            out[b] = sqrt(sumSq / Float(end - start))
        }
        return out
    }

    // MARK: - Cute names

    /// Adjective + noun + month-day. Pairs are picked from a small
    /// curated wordlist that leans on cassette/analog/light/weather
    /// nouns and saturated-color adjectives so the filenames feel
    /// like mixtape titles, not test fixtures. The (adj, noun) pair
    /// is seeded by the date's second-of-day so two ejects in the
    /// same second still get a deterministic split, while a fresh
    /// minute produces a different pairing.
    static func makeCuteName(date: Date) -> String {
        let cal = Calendar.current
        let comps = cal.dateComponents([.hour, .minute, .second], from: date)
        let seed = (comps.hour ?? 0) * 3600
            + (comps.minute ?? 0) * 60
            + (comps.second ?? 0)
        let adj = Self.adjectives[seed % Self.adjectives.count]
        let noun = Self.nouns[(seed / 7) % Self.nouns.count]
        let monthDay = Self.monthDayFormatter.string(from: date).lowercased()
        return "\(adj)-\(noun)-\(monthDay)"
    }

    private static let monthDayFormatter: DateFormatter = {
        let f = DateFormatter()
        f.dateFormat = "MMM-d"
        return f
    }()

    private static let adjectives: [String] = [
        "ruby", "cobalt", "amber", "violet", "emerald", "saffron",
        "midnight", "neon", "honey", "velvet", "frost", "ember",
        "lilac", "ivory", "indigo", "scarlet", "moss", "pearl",
        "copper", "lemon", "rose", "azure", "ochre", "blush",
        "cinder", "linen", "smoke", "candy", "jade", "peach",
        "mint", "cherry", "plum", "ash", "cream", "rust",
    ]
    private static let nouns: [String] = [
        "clouds", "rivers", "static", "lanterns", "highways",
        "kittens", "tapes", "mornings", "drifts", "echoes",
        "porches", "trains", "tides", "courtyards", "sparrows",
        "bridges", "marquees", "weather", "balconies", "comets",
        "fireflies", "pavilions", "ferries", "windowsills", "memos",
        "shadows", "carousels", "blossoms", "skylights", "pages",
        "footprints", "telescopes", "bonfires", "hallways", "barges",
        "sundials", "swallows",
    ]
}

// MARK: - Lock convenience

private extension NSLock {
    func withLock<T>(_ body: () -> T) -> T {
        lock(); defer { unlock() }
        return body()
    }
}
