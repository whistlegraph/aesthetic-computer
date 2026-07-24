import Foundation
import AVFoundation
import AudioToolbox
import CoreMedia
import Darwin

/// Live Icecast stream used by Menu Band's standalone CDJ Radio deck.
/// The continuous deck is independent from the piano instrument; its recent
/// ring can be copied into the Piano Sampler only through the explicit
/// "Sample to Piano" action. The older per-note radio voices remain as an
/// internal compatibility path, but ordinary piano notes never route here.
/// Pitch is shifted INDEPENDENTLY
/// of speed via AVAudioUnitTimePitch — a high note is higher, not faster —
/// so every voice consumes the stream at real time and stays locked to the
/// live edge (no tape-style speed-up, no drift, no replay). (Earlier this
/// used AVAudioUnitVarispeed, which coupled pitch to speed: pitched-up
/// notes ran the buffer faster, caught the live write head, and re-anchored
/// backwards — audible as a repeating loop on held notes.)
///
/// Architecture:
///   URLSession byte stream ──► AudioFileStream (MP3 parse) ──► AudioConverter
///                                              │ (PCM)
///                                              ▼
///                                     ring AVAudioPCMBuffer (~8 s)
///                                              │
///                  per active note             ▼
///              AVAudioPlayerNode ──► TimePitch ──► voiceMixer ─┐
///                                                              ▼
///                                                       crossfadeMixer ──► output
///                                                              ▲
///       AVAudioSourceNode (white noise) ──► noiseEQ ──────────┘
///
/// NOTE: `stream.kpbj.fm` is an endless, container-less Icecast MP3
/// (no duration, no moov/track table). `AVPlayer` can *play* it but
/// `AVAsset` never exposes an `AVAssetTrack`, so an `MTAudioProcessingTap`
/// can never attach — the old design left the ring empty and only the
/// synthesized AM static was ever audible. We decode the MP3 byte stream
/// ourselves instead, which works for an infinite stream.
///
/// `crossfadeMixer` blends pitched-stream and shaped-static based on a
/// `signalStrength` value (1.0 = clean radio, 0.0 = pure static). Strength
/// decays toward 0 when fresh tap frames stop arriving, so a network stall
/// fades into static the way a real AM dial does. Static volume is also
/// modulated by total NIC bytes/sec — quiet network = soft hiss; bursty
/// traffic = crackle.
/// A live MP3 station selectable on the standalone CDJ Radio deck. All
/// stations are plain Icecast MP3 (KPBJ direct; NTS via a 302 the URLSession
/// follows), so they share the same decode path — only the URL changes.
struct RadioStation: Equatable {
    let id: String      // persisted + typed-command name ("kpbj", "nts1", …)
    let label: String   // short grid-cell label ("KPBJ", "NTS1")
    let name: String    // full readout / tooltip ("KPBJ.FM", "NTS 1")
    let url: URL

    static let kpbj = RadioStation(
        id: "kpbj", label: "KPBJ", name: "KPBJ.FM",
        url: URL(string: "https://stream.kpbj.fm/")!)
    // r8dio.dk — Danish stream via radio.co (/listen 302s to MP3, same decode
    // path as NTS). Matches AC's r8dio piece (disks/r8dio.mjs).
    static let r8dio = RadioStation(
        id: "r8dio", label: "R8DIO", name: "r8dio.dk",
        url: URL(string: "https://s3.radio.co/s7cd1ffe2f/listen")!)
    static let nts1 = RadioStation(
        id: "nts1", label: "NTS1", name: "NTS 1",
        url: URL(string: "https://stream-relay-geo.ntslive.net/stream")!)
    static let nts2 = RadioStation(
        id: "nts2", label: "NTS2", name: "NTS 2",
        url: URL(string: "https://stream-relay-geo.ntslive.net/stream2")!)

    /// Display order in the chooser, left → right.
    static let all: [RadioStation] = [.kpbj, .r8dio, .nts1, .nts2]
    static func by(id: String) -> RadioStation { all.first { $0.id == id } ?? .kpbj }
}

final class KPBJRadioStream: NSObject, URLSessionDataDelegate {
    /// The station currently tuned. Defaults to KPBJ; `setStation` retunes.
    private var station: RadioStation = .kpbj

    /// Internal sample rate. Stream gets decoded into this rate; ring,
    /// voices, and noise all run here.
    private let sampleRate: Double = 44_100

    /// Ring length. 8 s of headroom so a pitched-DOWN voice (rate < 1, read
    /// head falling behind the live write head) can hold for a while before
    /// it wraps into not-yet-written territory.
    private let ringSeconds: Double = 8.0
    /// How far behind the live write head a voice reads — its target
    /// latency, and where it re-anchors to when it catches up. Small on
    /// purpose: the radio backend should track the LIVE stream, not replay
    /// a multi-second-old recording. The earlier design started 3 s behind
    /// and re-anchored 3 s back on catch-up, so a held note replayed the
    /// same 3 s span on a loop — the "it plays then plays it again" repeat.
    /// Reading ~`liveLagSeconds` behind keeps every note essentially live;
    /// at unity rate the read head tracks the stream continuously and never
    /// re-anchors. It's scaled by playback rate per voice (see `noteOn`) so
    /// a pitched-up note — which drains the ring faster than it fills —
    /// gets a little extra runway before it has to re-anchor, keeping the
    /// (now sub-chunk-sized) repeat as rare as the pitch allows.
    private let liveLagSeconds: Double = 0.35

    /// Stereo float32 non-interleaved — standard AVAudioEngine format.
    private let format: AVAudioFormat
    private let ring: AVAudioPCMBuffer
    private let ringFrames: Int
    private var ringWriteFrame: Int64 = 0
    private var lastTapHostTime: CFAbsoluteTime = 0
    private let ringLock = NSLock()

    // MARK: - Direct MP3 decode (replaces AVPlayer + tap)
    /// Network session pulling the endless Icecast MP3 as raw bytes.
    private var session: URLSession?
    private var dataTask: URLSessionDataTask?
    /// AudioFileStream parses the MP3 byte stream into compressed packets.
    private var audioFileStream: AudioFileStreamID?
    /// Converts decoded source packets → our internal stereo float32.
    private var converter: AVAudioConverter?
    /// Source PCM format discovered from the stream (set once the parser
    /// reports the data format), used to build the converter.
    private var sourceFormat: AVAudioFormat?
    /// Compressed packets awaiting conversion, with their descriptions.
    private var pendingPackets: [(data: Data, desc: AudioStreamPacketDescription)] = []
    private let decodeLock = NSLock()

    /// Per-channel voice slots. Channel matches the synth's round-robin
    /// melodic channels (0–7) so chord/retrigger behavior mirrors the GM
    /// path. Allocated lazily; once attached they stay in the graph.
    private var voices: [UInt8: Voice] = [:]

    /// Continuous unity-rate CDJ deck player. This used to be a faint bed
    /// under key-gated radio voices; it is now the primary radio output.
    private let bedNode = AVAudioPlayerNode()
    private let bedTimePitch = AVAudioUnitTimePitch()
    private var bedReadFrame: Int64 = 0
    private var bedActive = false

    /// Noise generator + bandpass + gain. White noise shaped to ~300–4000
    /// Hz gives the AM-radio "small speaker" character without sounding
    /// like a dentist drill at full strength.
    private var noiseSource: AVAudioSourceNode?
    /// Pitches the static with the held note (same cents as the per-note
    /// voices) so the connecting hiss tunes ALONG with the stream instead of
    /// sitting at a fixed pitch under it.
    private let noiseTimePitch = AVAudioUnitTimePitch()
    private let noiseEQ = AVAudioUnitEQ(numberOfBands: 2)
    private let noiseGain = AVAudioMixerNode()
    private let voiceMixer = AVAudioMixerNode()
    private let crossfadeMixer = AVAudioMixerNode()

    /// Smoothed signal strength in [0, 1]. 1 = clean radio audible,
    /// 0 = full static. Updated on a 30 Hz timer from tap freshness.
    private var signalStrength: Float = 0.0
    /// Smoothed normalized network activity in [0, 1]. Drives the noise
    /// gain so static crackle correlates with real machine traffic.
    private var networkActivity: Float = 0.0
    private var lastNetBytes: UInt64 = 0
    private var healthTimer: DispatchSourceTimer?

    private weak var engine: AVAudioEngine?
    private weak var output: AVAudioNode?
    private var attached = false
    private(set) var streaming = false

    private final class Voice {
        let node = AVAudioPlayerNode()
        /// Independent pitch shift — moves pitch WITHOUT changing speed, so
        /// the node always consumes the ring at real time (rate 1.0) no
        /// matter the note. That's what keeps every voice locked to the
        /// live edge: nothing pitches the playback faster/slower, so the
        /// read head never races ahead of (or lags behind) the write head.
        let timePitch = AVAudioUnitTimePitch()
        var midiNote: UInt8 = 60
        var held: Bool = false
        /// Read frame inside the global ring timeline. Each completion
        /// callback pumps the next chunk forward by `chunkFrames`.
        var readFrame: Int64 = 0
        /// Target latency behind the live write head for this voice, in
        /// frames (`liveLagSeconds` scaled by the note's playback rate).
        /// Used as the re-anchor point so catch-up snaps back to live, not
        /// to a multi-second-old position.
        var lagFrames: Int64 = 0
    }

    override init() {
        format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                               channels: 2)!
        ringFrames = Int(sampleRate * ringSeconds)
        ring = AVAudioPCMBuffer(pcmFormat: format,
                                frameCapacity: AVAudioFrameCount(ringFrames))!
        ring.frameLength = AVAudioFrameCount(ringFrames)
        super.init()
        // Zero-init both channel buffers so silence reads cleanly before
        // the first decoded frames arrive.
        if let chans = ring.floatChannelData {
            for c in 0..<Int(format.channelCount) {
                memset(chans[c], 0, ringFrames * MemoryLayout<Float>.size)
            }
        }
    }

    // MARK: - Engine attach

    /// Attach static graph nodes (mixers, noise source, EQ) into the host
    /// engine and route the crossfade mixer to `output`. Voices attach
    /// lazily on first noteOn so we don't pre-allocate channels we never
    /// use. Idempotent.
    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        self.output = output

        // Pink-ish noise via AVAudioSourceNode's render block. White noise
        // through a band-limit EQ is close enough to the AM-radio voicing
        // we want; cheaper than a real pink filter and tuned with the EQ
        // bands instead.
        let src = AVAudioSourceNode(format: format) { [weak self] _, _, frameCount, audioBufferList -> OSStatus in
            let abl = UnsafeMutableAudioBufferListPointer(audioBufferList)
            // Single static seed advances per call — good enough for noise.
            for buffer in abl {
                let ptr = buffer.mData?.assumingMemoryBound(to: Float.self)
                let frames = Int(frameCount)
                for i in 0..<frames {
                    // Linear-congruential noise; cheap and decorrelated.
                    let r = Float.random(in: -1...1)
                    ptr?[i] = r
                }
            }
            // Self-reference kept so the source keeps a pointer to us.
            _ = self
            return noErr
        }
        noiseSource = src

        engine.attach(src)
        engine.attach(noiseTimePitch)
        engine.attach(noiseEQ)
        engine.attach(noiseGain)
        engine.attach(voiceMixer)
        engine.attach(crossfadeMixer)
        engine.attach(bedNode)
        engine.attach(bedTimePitch)
        engine.connect(bedNode, to: bedTimePitch, format: format)
        engine.connect(bedTimePitch, to: voiceMixer, format: format)
        bedNode.volume = 1.0

        // Bandpass via two EQ bands: high-pass at 250 Hz, low-pass at
        // 4 kHz. AM broadcast voicing is roughly 100–5 kHz; we narrow it
        // a bit more for a "tinny pocket radio" vibe.
        let hp = noiseEQ.bands[0]
        hp.filterType = .highPass
        hp.frequency = 250
        hp.bypass = false
        let lp = noiseEQ.bands[1]
        lp.filterType = .lowPass
        lp.frequency = 4_000
        lp.bypass = false

        // Wire: noiseSource → noiseTimePitch → noiseEQ → noiseGain →
        // crossfadeMixer. The time-pitch shifts the static with the note.
        engine.connect(src, to: noiseTimePitch, format: format)
        engine.connect(noiseTimePitch, to: noiseEQ, format: format)
        engine.connect(noiseEQ, to: noiseGain, format: format)
        engine.connect(noiseGain, to: crossfadeMixer, format: format)

        // Wire: voiceMixer → crossfadeMixer
        engine.connect(voiceMixer, to: crossfadeMixer, format: format)

        // Wire: crossfadeMixer → host output (the synth's preLimiterMixer)
        engine.connect(crossfadeMixer, to: output, format: format)

        // Initial gains: full static, no signal — until the stream starts
        // delivering audio the user hears only the AM hiss.
        noiseGain.outputVolume = 0.0
        voiceMixer.outputVolume = 0.0
        // Master gate — gets opened when the synth actually engages this
        // backend. Closed by default so the radio's nodes contribute
        // nothing to the pre-limiter mix while a GM voice is selected.
        crossfadeMixer.outputVolume = 0.0

        attached = true
    }

    // MARK: - Master output gate

    /// Whether this station is the selected CDJ Radio source.
    private var outputEnabled = false

    /// Open/close the radio's master output. When closed, the radio's
    /// nodes contribute zero to the pre-limiter sum bus regardless of
    /// streaming state — used both for the "not the active backend"
    /// case and for the synth's 15 s linger window.
    func setOutputEnabled(_ enabled: Bool) {
        outputEnabled = enabled
        if enabled, streaming { startBed() }
        if !enabled { stopBed() }
        updateMasterGate()
    }

    /// CDJ Radio is a continuous deck. Piano key state never controls this
    /// gate; the source selector and close button do.
    private func updateMasterGate() {
        crossfadeMixer.outputVolume = outputEnabled ? 1.0 : 0.0
    }

    // MARK: - Stream lifecycle

    func startStreaming() {
        guard !streaming else { return }
        streaming = true
        reconnectAttempts = 0
        NSLog("MenuBand radio: startStreaming (\(station.name), direct MP3 decode) → \(station.url.absoluteString)")
        beginStreamRequest()

        if outputEnabled { startBed() }
        // The health timer shapes the AM static and tracks stream freshness.
        startHealthTimer()
    }

    /// Open (or re-open) the MP3 parser and kick off the network read.
    /// Used both for the initial connect and for auto-reconnect after the
    /// connection drops, so the parser/converter always start clean.
    private func beginStreamRequest() {
        // A reconnect may begin mid-frame; reset the parser + converter so
        // stale half-packets can't corrupt the new MP3 byte boundary.
        if let afs = audioFileStream {
            AudioFileStreamClose(afs)
            audioFileStream = nil
        }
        converter = nil
        sourceFormat = nil
        decodeLock.lock(); pendingPackets.removeAll(); decodeLock.unlock()

        // Open an AudioFileStream parser for the incoming MP3 bytes. It
        // calls back with the data format (once known) and with parsed
        // compressed packets, which we convert to PCM and write into the
        // ring. This replaces AVPlayer, which can't expose a track for an
        // endless container-less Icecast MP3.
        let selfPtr = Unmanaged.passUnretained(self).toOpaque()
        let status = AudioFileStreamOpen(selfPtr,
                                         kpbjPropertyListener,
                                         kpbjPacketsProc,
                                         kAudioFileMP3Type,
                                         &audioFileStream)
        if status != noErr {
            NSLog("MenuBand KPBJ: AudioFileStreamOpen failed err=\(status)")
        }

        // Reuse the session across reconnects (a dropped task leaves the
        // session valid); only build one on first connect or after teardown.
        let session: URLSession
        if let existing = self.session {
            session = existing
        } else {
            let config = URLSessionConfiguration.default
            config.timeoutIntervalForRequest = 60
            config.requestCachePolicy = .reloadIgnoringLocalCacheData
            session = URLSession(configuration: config, delegate: self,
                                 delegateQueue: nil)
            self.session = session
        }
        // IMPORTANT: do NOT send `Icy-MetaData: 1` — that REQUESTS the
        // server to interleave ICY title metadata into the audio bytes
        // every ~16 KB, which corrupts the raw MP3 the parser decodes
        // (→ garbage/no PCM → only synthesized static is audible). Plain
        // GET gives a clean continuous MP3.
        let req = URLRequest(url: station.url)
        let task = session.dataTask(with: req)
        dataTask = task
        task.resume()
    }

    /// Auto-reconnect bookkeeping. The endless Icecast feed can drop on a
    /// wifi blip, an audio-device switch, or a server hiccup; without this
    /// the radio would go permanently silent until the backend is toggled.
    private var reconnectAttempts = 0
    private var reconnectWork: DispatchWorkItem?
    /// Set when we intentionally cancel the data task (e.g. retuning to a
    /// new station) so the cancel's completion callback doesn't fire the
    /// auto-reconnect. Consumed once.
    private var suppressReconnectOnce = false

    /// Retune to a different station. If currently streaming, drop the old
    /// connection and open the new URL right away — the brief gap is masked
    /// by the AM static crossfade. The intentional cancel must NOT trigger
    /// the auto-reconnect, hence `suppressReconnectOnce`.
    func setStation(_ newStation: RadioStation) {
        guard newStation != station else { return }
        station = newStation
        loggedFirstFrames = false
        NSLog("MenuBand radio: station → \(station.name) (\(station.url.absoluteString))")
        guard streaming else { return }
        reconnectWork?.cancel(); reconnectWork = nil
        reconnectAttempts = 0
        suppressReconnectOnce = true
        dataTask?.cancel()
        dataTask = nil
        beginStreamRequest()
    }

    /// Schedule a reconnect with capped exponential backoff. Cancelled by
    /// `stopStreaming`; reset to attempt 0 once fresh frames decode again.
    private func scheduleReconnect() {
        reconnectAttempts += 1
        let delay = min(8.0, pow(1.6, Double(min(reconnectAttempts, 6))))
        NSLog("MenuBand KPBJ: connection lost — reconnecting in " +
              "\(String(format: "%.1f", delay))s (attempt \(reconnectAttempts))")
        reconnectWork?.cancel()
        let work = DispatchWorkItem { [weak self] in
            guard let self = self, self.streaming else { return }
            self.beginStreamRequest()
        }
        reconnectWork = work
        DispatchQueue.main.asyncAfter(deadline: .now() + delay, execute: work)
    }

    func stopStreaming() {
        guard streaming else { return }
        NSLog("MenuBand KPBJ: stopStreaming")
        streaming = false
        reconnectWork?.cancel()
        reconnectWork = nil
        reconnectAttempts = 0
        healthTimer?.cancel()
        healthTimer = nil
        dataTask?.cancel()
        dataTask = nil
        session?.invalidateAndCancel()
        session = nil
        if let afs = audioFileStream {
            AudioFileStreamClose(afs)
            audioFileStream = nil
        }
        converter = nil
        sourceFormat = nil
        decodeLock.lock(); pendingPackets.removeAll(); decodeLock.unlock()
        stopBed()
        // Stop active voices and reset gain so a re-enable starts clean.
        for (_, v) in voices {
            v.held = false
            v.node.stop()
        }
        signalStrength = 0
        applyMixGains()
    }

    // MARK: - URLSessionDataDelegate

    func urlSession(_ session: URLSession, dataTask: URLSessionDataTask,
                    didReceive data: Data) {
        guard streaming, let afs = audioFileStream else { return }
        data.withUnsafeBytes { (raw: UnsafeRawBufferPointer) -> Void in
            guard let base = raw.baseAddress else { return }
            let err = AudioFileStreamParseBytes(afs, UInt32(data.count), base, [])
            if err != noErr {
                NSLog("MenuBand KPBJ: parse bytes err=\(err)")
            }
        }
    }

    func urlSession(_ session: URLSession, task: URLSessionTask,
                    didCompleteWithError error: Error?) {
        if let error = error, (error as NSError).code != NSURLErrorCancelled {
            NSLog("MenuBand radio: stream task ended — \(error)")
        }
        // A station retune cancels the old task on purpose — that cancel
        // must not bounce into a reconnect (a fresh task is already up).
        if suppressReconnectOnce {
            suppressReconnectOnce = false
            return
        }
        // The feed is endless, so ANY completion while we still want to be
        // streaming (network drop, device switch, server close, even a
        // clean EOF) means the connection died — reconnect with backoff.
        // Intentional teardown sets `streaming = false` first, so a
        // cancelled task no-ops here.
        guard streaming else { return }
        scheduleReconnect()
    }

    // MARK: - AudioFileStream → ring

    /// Called by the property listener once the parser knows the source
    /// audio format. Builds the converter into our internal PCM format.
    fileprivate func handleStreamPropertyReady() {
        guard let afs = audioFileStream, converter == nil else { return }
        var asbd = AudioStreamBasicDescription()
        var size = UInt32(MemoryLayout<AudioStreamBasicDescription>.size)
        guard AudioFileStreamGetProperty(afs, kAudioFileStreamProperty_DataFormat,
                                         &size, &asbd) == noErr else { return }
        guard let src = AVAudioFormat(streamDescription: &asbd) else {
            NSLog("MenuBand KPBJ: could not build source format")
            return
        }
        sourceFormat = src
        converter = AVAudioConverter(from: src, to: format)
        NSLog("MenuBand KPBJ: source format \(src) → converter ready")
    }

    /// Called by the packets proc with freshly parsed compressed packets.
    /// Convert them to PCM and write into the ring.
    fileprivate func handlePackets(bytes: UnsafeRawPointer, byteCount: UInt32,
                                   packetCount: UInt32,
                                   descs: UnsafePointer<AudioStreamPacketDescription>?) {
        guard let converter = converter, let src = sourceFormat,
              let descs = descs else { return }

        // Queue the parsed packets (copying their bytes) so the converter's
        // pull-callback can hand them over one batch at a time.
        decodeLock.lock()
        for i in 0..<Int(packetCount) {
            let d = descs[i]
            let start = Int(d.mStartOffset)
            let len = Int(d.mDataByteSize)
            let pkt = Data(bytes: bytes.advanced(by: start), count: len)
            var rebased = d
            rebased.mStartOffset = 0
            pendingPackets.append((pkt, rebased))
        }
        decodeLock.unlock()

        // Decode in reasonably sized chunks into PCM and push to the ring.
        let framesPerPacket = max(1, Int(src.streamDescription.pointee.mFramesPerPacket))
        let capacityFrames = AVAudioFrameCount(framesPerPacket * 32)
        while true {
            decodeLock.lock(); let have = pendingPackets.count; decodeLock.unlock()
            if have == 0 { break }
            guard let out = AVAudioPCMBuffer(pcmFormat: format,
                                             frameCapacity: capacityFrames) else { break }
            var convError: NSError?
            let st = converter.convert(to: out, error: &convError) { [weak self] _, outStatus in
                guard let self = self else { outStatus.pointee = .endOfStream; return nil }
                self.decodeLock.lock()
                defer { self.decodeLock.unlock() }
                guard !self.pendingPackets.isEmpty else {
                    outStatus.pointee = .noDataNow
                    return nil
                }
                let (pktData, desc) = self.pendingPackets.removeFirst()
                guard let inBuf = AVAudioCompressedBuffer(
                    format: src, packetCapacity: 1,
                    maximumPacketSize: pktData.count) as AVAudioCompressedBuffer? else {
                    outStatus.pointee = .noDataNow
                    return nil
                }
                pktData.withUnsafeBytes { raw in
                    memcpy(inBuf.data, raw.baseAddress!, pktData.count)
                }
                inBuf.byteLength = UInt32(pktData.count)
                inBuf.packetCount = 1
                inBuf.packetDescriptions?.pointee = desc
                outStatus.pointee = .haveData
                return inBuf
            }
            if st == .error { if let e = convError { NSLog("MenuBand KPBJ: convert err \(e)") }; break }
            if out.frameLength > 0 { writePCMToRing(out) }
            if st == .endOfStream || st == .inputRanDry { break }
        }
    }

    /// Append a freshly decoded PCM buffer (our internal stereo float32)
    /// to the ring, advancing the live write head.
    private func writePCMToRing(_ buf: AVAudioPCMBuffer) {
        guard let srcCh = buf.floatChannelData, let dest = ring.floatChannelData else { return }
        let n = Int(buf.frameLength)
        let channels = Int(format.channelCount)
        let srcChannels = Int(buf.format.channelCount)
        ringLock.lock()
        let writeStart = Int(ringWriteFrame % Int64(ringFrames))
        for c in 0..<channels {
            let s = srcCh[min(c, srcChannels - 1)]
            var w = writeStart
            for f in 0..<n {
                dest[c][w] = s[f]
                w += 1
                if w >= ringFrames { w = 0 }
            }
        }
        ringWriteFrame &+= Int64(n)
        ringLock.unlock()
        lastTapHostTime = CFAbsoluteTimeGetCurrent()
        // Fresh frames decoded → the connection is healthy again, so reset
        // the backoff so a future drop starts from the short delay.
        if reconnectAttempts != 0 { reconnectAttempts = 0 }
        // One-time confirmation that decoded stream audio is actually
        // reaching the ring (vs. only synthesized static playing).
        if !loggedFirstFrames {
            loggedFirstFrames = true
            NSLog("MenuBand KPBJ: first decoded frames in ring (n=\(n)) — stream audio live")
        }
    }
    private var loggedFirstFrames = false

    // MARK: - Faint always-on bed

    private func startBed() {
        guard !bedActive else { return }
        bedActive = true
        ringLock.lock()
        let live = ringWriteFrame
        ringLock.unlock()
        bedReadFrame = max(0, live - Int64(sampleRate * 0.25))
        pumpBed()
        pumpBed()
        if !bedNode.isPlaying {
            bedNode.play()
        }
    }

    private func stopBed() {
        bedActive = false
        bedNode.stop()
    }

    private func pumpBed() {
        guard bedActive else { return }
        let chunkFrames = AVAudioFrameCount(sampleRate * 0.150)
        // Bed runs at unity rate; if it ever catches up, re-anchor to a
        // small 0.25 s live lag so it stays tuned to the live edge.
        guard let chunk = readRing(from: &bedReadFrame, frames: chunkFrames,
                                   reanchorLagFrames: Int64(sampleRate * 0.25)) else {
            return
        }
        bedNode.scheduleBuffer(chunk,
                               completionCallbackType: .dataConsumed) { [weak self] _ in
            guard let self = self, self.bedActive else { return }
            DispatchQueue.main.async { self.pumpBed() }
        }
    }

    // MARK: - Per-voice playback

    /// Pitch shift in cents for a MIDI note relative to middle C (60).
    /// Middle C → 0 cents (unpitched/live). 100 cents per semitone. This
    /// drives AVAudioUnitTimePitch's `pitch`, which shifts pitch WITHOUT
    /// changing playback speed — so a high note isn't sped up, it's just
    /// higher.
    private func pitchCents(forNote note: UInt8) -> Float {
        Float(Int(note) - 60) * 100.0
    }

    /// Current trackpad pitch-bend in semitones. One controller unit =
    /// one octave (12 semitones), matching `MenuBandSampleVoice`. Added on
    /// top of each held voice's note pitch so the bend gesture slides the
    /// radio's pitch live. AVAudioUnitTimePitch ignores MIDI pitch-bend, so
    /// the controller routes the signed amount in-process via `setBend`.
    private var bendSemitones: Float = 0

    /// Apply a voice's note + current bend to its time-pitch unit, in cents,
    /// clamped to AVAudioUnitTimePitch's ±2400-cent (±2 octave) range. Rate
    /// is left at 1.0 always — only pitch moves.
    private func applyPitch(_ v: Voice) {
        let cents = pitchCents(forNote: v.midiNote) + bendSemitones * 100.0
        let clamped = max(-2400, min(2400, cents))
        v.timePitch.pitch = clamped
        // Tune the static to the (most recent) held note too, so the
        // connecting hiss pitches right along with the stream.
        noiseTimePitch.pitch = clamped
    }

    /// Trackpad pitch-bend hook. `amount` is the controller's signed
    /// bend (one unit = one octave); slides every playing voice's pitch
    /// live and stashes the amount so notes started mid-bend pick it up.
    func setBend(amount: Float) {
        bendSemitones = amount * 12.0
        bedTimePitch.pitch = max(-2400, min(2400, bendSemitones * 100))
        for (_, v) in voices where v.node.isPlaying {
            applyPitch(v)
        }
    }

    func noteOn(_ midi: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        guard attached, let engine = engine else { return }
        let voice = ensureVoice(for: channel)
        voice.midiNote = midi
        voice.held = true
        applyPitch(voice)  // time-pitch only — playback stays at real time
        voice.node.volume = Float(velocity) / 127.0

        // Read just behind the live write head so the voice plays the LIVE
        // stream, not a delayed recording. Because time-pitch keeps the
        // node at real-time rate regardless of note, the read head tracks
        // the write head exactly — it never catches up, so it never has to
        // re-anchor (no repeat). The small fixed lag is just latency
        // headroom against stream jitter / a busy main thread.
        voice.lagFrames = Int64(Double(sampleRate) * liveLagSeconds)
        ringLock.lock()
        let live = ringWriteFrame
        ringLock.unlock()
        voice.readFrame = max(0, live - voice.lagFrames)

        // Pre-queue several chunks so a late refill can't underrun the
        // node. Deeper queue = more latency tolerance against the busy
        // main thread; refills run on a dedicated serial queue below.
        for _ in 0..<4 { pumpVoice(voice) }
        if !voice.node.isPlaying {
            voice.node.play()
        }
        updateMasterGate()  // a pad is now held → open the master gate
        _ = engine
    }

    func noteOff(_ midi: UInt8, channel: UInt8 = 0) {
        guard let voice = voices[channel] else { return }
        // Only stop if the channel's current note matches — same channel
        // may have been retriggered by a different note in flight.
        if voice.midiNote == midi || !voice.held {
            voice.held = false
            voice.node.stop()
        }
        updateMasterGate()  // last pad lifted → close the gate (silence)
    }

    func panic() {
        for (_, v) in voices {
            v.held = false
            v.node.stop()
        }
        updateMasterGate()
    }

    private func ensureVoice(for channel: UInt8) -> Voice {
        if let v = voices[channel] { return v }
        let v = Voice()
        if let engine = engine {
            engine.attach(v.node)
            engine.attach(v.timePitch)
            engine.connect(v.node, to: v.timePitch, format: format)
            engine.connect(v.timePitch, to: voiceMixer, format: format)
        }
        voices[channel] = v
        return v
    }

    /// Dedicated serial queue for ring reads + buffer scheduling. The old
    /// code refilled on `DispatchQueue.main`, which is saturated by the
    /// menubar's icon-animation + hover timers — a late refill underran
    /// the player node and you heard a skip. A private high-priority queue
    /// keeps refills punctual regardless of UI load.
    private let pumpQueue = DispatchQueue(label: "kpbj.pump", qos: .userInitiated)

    private func pumpVoice(_ voice: Voice) {
        guard voice.held else { return }
        // Smaller chunks (80 ms) than the bed so the read head tracks the
        // live edge finely — and so the rare re-anchor on a pitched-up hold
        // repeats at most a chunk's worth, not a long swatch.
        let chunkFrames = AVAudioFrameCount(sampleRate * 0.080) // 80 ms
        guard let chunk = readRing(from: &voice.readFrame, frames: chunkFrames,
                                   reanchorLagFrames: voice.lagFrames) else {
            return
        }
        voice.node.scheduleBuffer(chunk,
                                  completionCallbackType: .dataConsumed) { [weak self, weak voice] _ in
            guard let self = self, let voice = voice, voice.held else { return }
            // Refill on our own serial queue, NOT main — main is busy with
            // menubar timers and would deliver the next chunk late (→ skip).
            self.pumpQueue.async { self.pumpVoice(voice) }
        }
    }

    /// Copy `frames` from the ring starting at `readFrame`, advancing
    /// `readFrame` by the frame count. Returns nil if the ring isn't
    /// initialized. Clamps `readFrame` so it never overruns the live
    /// write head — if the voice is consuming faster than the stream is
    /// writing (e.g. rate > 1 holding for too long), we re-anchor 200 ms
    /// behind the live edge instead of reading garbage.
    private func readRing(from readFrame: inout Int64,
                          frames: AVAudioFrameCount,
                          reanchorLagFrames: Int64) -> AVAudioPCMBuffer? {
        guard let out = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames),
              let dest = out.floatChannelData,
              let src = ring.floatChannelData else { return nil }
        out.frameLength = frames

        ringLock.lock()
        let live = ringWriteFrame
        let safetyFrames = Int64(sampleRate * 0.05) // 50 ms read-ahead floor
        if readFrame > live - safetyFrames {
            // Caught up to the live edge: snap back to this voice's small
            // live-lag target so it keeps playing the freshest audio. The
            // repeat this introduces is at most `reanchorLagFrames` long (a
            // fraction of a second) instead of the old multi-second replay —
            // and at unity rate it effectively never fires.
            readFrame = max(0, live - reanchorLagFrames)
        }
        let startInRing = Int((readFrame % Int64(ringFrames) + Int64(ringFrames)) % Int64(ringFrames))
        let n = Int(frames)
        let firstChunk = min(n, ringFrames - startInRing)
        let channels = Int(format.channelCount)
        for c in 0..<channels {
            memcpy(dest[c], src[c].advanced(by: startInRing),
                   firstChunk * MemoryLayout<Float>.size)
            if n > firstChunk {
                memcpy(dest[c].advanced(by: firstChunk), src[c],
                       (n - firstChunk) * MemoryLayout<Float>.size)
            }
        }
        ringLock.unlock()
        readFrame &+= Int64(n)
        return out
    }

    /// Copy the newest decoded station audio for CDJ Radio's explicit
    /// Sample-to-Piano handoff. The live deck keeps playing unchanged.
    func copyRecentAudio(seconds: Double) -> AVAudioPCMBuffer? {
        ringLock.lock(); defer { ringLock.unlock() }
        let available = min(Int64(ringFrames), ringWriteFrame)
        let wanted = min(available, Int64(max(0.1, seconds) * sampleRate))
        guard wanted > 0,
              let output = AVAudioPCMBuffer(
                pcmFormat: format, frameCapacity: AVAudioFrameCount(wanted)),
              let source = ring.floatChannelData,
              let destination = output.floatChannelData else { return nil }
        output.frameLength = AVAudioFrameCount(wanted)
        let start = ringWriteFrame - wanted
        for channel in 0..<Int(format.channelCount) {
            for frame in 0..<Int(wanted) {
                let sourceIndex = Int(
                    (start + Int64(frame)) % Int64(ringFrames))
                destination[channel][frame] = source[channel][sourceIndex]
            }
        }
        return output
    }

    // MARK: - AM crossfade health timer

    /// Tick at 30 Hz: refresh signal strength from tap freshness, refresh
    /// network-activity meter, push smoothed values into the mixer gains.
    private func startHealthTimer() {
        healthTimer?.cancel()
        let t = DispatchSource.makeTimerSource(queue: .main)
        t.schedule(deadline: .now() + .milliseconds(33),
                   repeating: .milliseconds(33), leeway: .milliseconds(5))
        t.setEventHandler { [weak self] in
            self?.tickHealth()
        }
        t.resume()
        healthTimer = t
        lastNetBytes = totalNetworkBytes()
    }

    private func tickHealth() {
        // Signal: 1 if we got a tap callback in the last 100 ms; decays
        // exponentially toward 0 if frames stop arriving (network stall
        // or stream error). 200 ms half-life feels about right — fast
        // enough to be musical when the connection blips, slow enough
        // not to flutter on normal jitter.
        // Static is a CONNECTING cue, not a per-burst meter. MP3 frames
        // arrive in chunky network bursts, so a tight freshness window made
        // the static breathe in between perfectly-healthy bursts. Use a
        // generous 2 s window (the ring holds seconds of audio, so a sub-2 s
        // gap is not a stall) and snap to clean FAST when bits arrive but
        // fall to static SLOW — so streaming reads as full clean (static ≈
        // 0) and only a genuine long stall lets the hiss creep back.
        let age = CFAbsoluteTimeGetCurrent() - lastTapHostTime
        let target: Float = age < 2.0 ? 1.0 : 0.0
        let alpha: Float = target > signalStrength ? 0.5 : 0.03
        signalStrength += (target - signalStrength) * alpha

        // Network activity → noise gain. Sample total bytes across all
        // interfaces; differentiate; normalize to a soft 0..1 with a
        // gentle ceiling at ~1 MB/s.
        let nowBytes = totalNetworkBytes()
        let delta = nowBytes &- lastNetBytes
        lastNetBytes = nowBytes
        let bytesPerSec = Float(delta) * 30.0 // 30 Hz tick
        let norm = min(1.0, bytesPerSec / 1_000_000.0)
        networkActivity += (norm - networkActivity) * 0.10

        applyMixGains()
    }

    private func applyMixGains() {
        // Clean voice path scales with signal strength.
        voiceMixer.outputVolume = signalStrength
        // Static path: only meaningful while connecting (signalStrength
        // low). Once stream bits flow, signalStrength → 1 and the (1 −
        // signal) factor zeroes the hiss entirely. Kept gentle even at the
        // connecting peak so it reads as "tuning in", not a blast.
        let staticBase: Float = 0.03
        let staticPeak: Float = 0.30
        let netGain = staticBase + (staticPeak - staticBase) * networkActivity
        noiseGain.outputVolume = (1.0 - signalStrength) * netGain
    }

    // MARK: - Network activity probe

    /// Sum of input + output bytes across all non-loopback interfaces.
    /// Uses BSD `getifaddrs` + the `if_data` struct that hangs off each
    /// AF_LINK address — the same source `nettop` reads. Cheap (one
    /// syscall) and works without any entitlements.
    private func totalNetworkBytes() -> UInt64 {
        var total: UInt64 = 0
        var ifap: UnsafeMutablePointer<ifaddrs>?
        guard getifaddrs(&ifap) == 0, let head = ifap else { return 0 }
        defer { freeifaddrs(ifap) }
        var cursor: UnsafeMutablePointer<ifaddrs>? = head
        while let it = cursor {
            let ifa = it.pointee
            if let addr = ifa.ifa_addr,
               addr.pointee.sa_family == UInt8(AF_LINK),
               let dataPtr = ifa.ifa_data {
                let data = dataPtr.assumingMemoryBound(to: if_data.self).pointee
                total &+= UInt64(data.ifi_obytes)
                total &+= UInt64(data.ifi_ibytes)
            }
            cursor = ifa.ifa_next
        }
        return total
    }
}

// MARK: - AudioFileStream C callbacks

/// Fires when the parser discovers a stream property — we only care
/// about the data format being ready, which lets us build the converter.
private let kpbjPropertyListener: AudioFileStream_PropertyListenerProc = {
    clientData, _, propertyID, _ in
    guard propertyID == kAudioFileStreamProperty_ReadyToProducePackets ||
          propertyID == kAudioFileStreamProperty_DataFormat else { return }
    let stream = Unmanaged<KPBJRadioStream>.fromOpaque(clientData).takeUnretainedValue()
    stream.handleStreamPropertyReady()
}

/// Fires with freshly parsed compressed audio packets.
private let kpbjPacketsProc: AudioFileStream_PacketsProc = {
    clientData, byteCount, packetCount, bytes, descs in
    guard let descs = descs else { return }
    let stream = Unmanaged<KPBJRadioStream>.fromOpaque(clientData).takeUnretainedValue()
    stream.handlePackets(bytes: bytes, byteCount: byteCount,
                         packetCount: packetCount, descs: descs)
}
