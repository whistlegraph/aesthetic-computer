import Foundation
import AVFoundation
import AudioToolbox
import CoreMedia
import Darwin

/// Live KPBJ.FM Icecast stream surfaced as a "voice -1" backend for the
/// menuband synth: pads play the live audio at varying pitch (middle C =
/// unity rate, up/down by 2^(semitones/12) — vinyl-varispeed semantics, so
/// highs run faster and lows run slower).
///
/// Architecture:
///   AVPlayer ──► MTAudioProcessingTap ──► ring AVAudioPCMBuffer (~4 s)
///                                              │
///                  per active note             ▼
///              AVAudioPlayerNode ──► Varispeed ──► voiceMixer ─┐
///                                                              ▼
///                                                       crossfadeMixer ──► output
///                                                              ▲
///       AVAudioSourceNode (white noise) ──► noiseEQ ──────────┘
///
/// `crossfadeMixer` blends pitched-stream and shaped-static based on a
/// `signalStrength` value (1.0 = clean radio, 0.0 = pure static). Strength
/// decays toward 0 when fresh tap frames stop arriving, so a network stall
/// fades into static the way a real AM dial does. Static volume is also
/// modulated by total NIC bytes/sec — quiet network = soft hiss; bursty
/// traffic = crackle.
final class KPBJRadioStream {
    private static let streamURL = URL(string: "https://stream.kpbj.fm/")!

    /// Internal sample rate. Stream gets decoded into this rate; ring,
    /// voices, and noise all run here.
    private let sampleRate: Double = 44_100

    /// Ring length. 4 s leaves headroom even at rate 2.0 (top octave) so a
    /// held pad's read head never catches the live write head.
    private let ringSeconds: Double = 4.0

    /// Stereo float32 non-interleaved — standard AVAudioEngine format.
    private let format: AVAudioFormat
    private let ring: AVAudioPCMBuffer
    private let ringFrames: Int
    private var ringWriteFrame: Int64 = 0
    private var lastTapHostTime: CFAbsoluteTime = 0
    private let ringLock = NSLock()

    private let player = AVPlayer()
    private var playerItem: AVPlayerItem?
    private var assetObserver: NSKeyValueObservation?

    /// Per-channel voice slots. Channel matches the synth's round-robin
    /// melodic channels (0–7) so chord/retrigger behavior mirrors the GM
    /// path. Allocated lazily; once attached they stay in the graph.
    private var voices: [UInt8: Voice] = [:]

    /// Faint always-on monitor of the live stream — plays the ring at
    /// rate 1.0 underneath the per-pad voices so the user always hears
    /// the radio "tuned in" while in voice −1, even before pressing a
    /// pad. Volume is intentionally low (~0.18) so a held chord still
    /// dominates the mix.
    private let bedNode = AVAudioPlayerNode()
    private var bedReadFrame: Int64 = 0
    private var bedActive = false

    /// Noise generator + bandpass + gain. White noise shaped to ~300–4000
    /// Hz gives the AM-radio "small speaker" character without sounding
    /// like a dentist drill at full strength.
    private var noiseSource: AVAudioSourceNode?
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
        let varispeed = AVAudioUnitVarispeed()
        var midiNote: UInt8 = 60
        var held: Bool = false
        /// Read frame inside the global ring timeline. Each completion
        /// callback pumps the next chunk forward by `chunkFrames`.
        var readFrame: Int64 = 0
    }

    init() {
        format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                               channels: 2)!
        ringFrames = Int(sampleRate * ringSeconds)
        ring = AVAudioPCMBuffer(pcmFormat: format,
                                frameCapacity: AVAudioFrameCount(ringFrames))!
        ring.frameLength = AVAudioFrameCount(ringFrames)
        // Zero-init both channel buffers so silence reads cleanly before
        // the first tap callback delivers audio.
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
        engine.attach(noiseEQ)
        engine.attach(noiseGain)
        engine.attach(voiceMixer)
        engine.attach(crossfadeMixer)
        engine.attach(bedNode)
        engine.connect(bedNode, to: voiceMixer, format: format)
        bedNode.volume = 0.18

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

        // Wire: noiseSource → noiseEQ → noiseGain → crossfadeMixer
        engine.connect(src, to: noiseEQ, format: format)
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

    /// Open/close the radio's master output. When closed, the radio's
    /// nodes contribute zero to the pre-limiter sum bus regardless of
    /// streaming state — used both for the "not the active backend"
    /// case and for the synth's 15 s linger window.
    func setOutputEnabled(_ enabled: Bool) {
        crossfadeMixer.outputVolume = enabled ? 1.0 : 0.0
    }

    // MARK: - Stream lifecycle

    func startStreaming() {
        guard !streaming else { return }
        streaming = true
        NSLog("MenuBand KPBJ: startStreaming → \(Self.streamURL.absoluteString)")

        let item = AVPlayerItem(url: Self.streamURL)
        playerItem = item

        // Watch player item status so we can surface AVPlayer load
        // failures (ATS block, DNS fail, 5xx) — without this the user
        // just hears silence with no log.
        assetObserver = item.observe(\.status, options: [.new, .initial]) { item, _ in
            switch item.status {
            case .readyToPlay:
                NSLog("MenuBand KPBJ: player item readyToPlay")
            case .failed:
                NSLog("MenuBand KPBJ: player item FAILED — \(String(describing: item.error))")
            case .unknown:
                break
            @unknown default:
                break
            }
        }

        // Track loading is async — install the tap when the audio track
        // is ready. AVAsset.tracks isn't safe to read until status is
        // .loaded.
        let asset = item.asset
        asset.loadValuesAsynchronously(forKeys: ["tracks"]) { [weak self, weak item] in
            guard let self = self, let item = item else { return }
            var error: NSError?
            let status = asset.statusOfValue(forKey: "tracks", error: &error)
            guard status == .loaded else {
                NSLog("MenuBand KPBJ: asset tracks load failed: \(String(describing: error))")
                return
            }
            DispatchQueue.main.async {
                self.installTap(on: item)
            }
        }

        player.replaceCurrentItem(with: item)
        player.volume = 1.0
        player.automaticallyWaitsToMinimizeStalling = true
        player.play()

        // Kick off the always-on faint bed so the user hears "tuned in"
        // audio even before a pad is pressed. Bed reads from the same
        // ring as the pads but always at rate 1.0.
        startBed()
        startHealthTimer()
    }

    func stopStreaming() {
        guard streaming else { return }
        NSLog("MenuBand KPBJ: stopStreaming")
        streaming = false
        healthTimer?.cancel()
        healthTimer = nil
        player.pause()
        player.replaceCurrentItem(with: nil)
        playerItem?.audioMix = nil
        playerItem = nil
        assetObserver?.invalidate()
        assetObserver = nil
        stopBed()
        // Stop active voices and reset gain so a re-enable starts clean.
        for (_, v) in voices {
            v.held = false
            v.node.stop()
        }
        signalStrength = 0
        applyMixGains()
    }

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
        guard let chunk = readRing(from: &bedReadFrame, frames: chunkFrames) else {
            return
        }
        bedNode.scheduleBuffer(chunk,
                               completionCallbackType: .dataConsumed) { [weak self] _ in
            guard let self = self, self.bedActive else { return }
            DispatchQueue.main.async { self.pumpBed() }
        }
    }

    private func installTap(on item: AVPlayerItem) {
        guard let track = item.asset.tracks(withMediaType: .audio).first else {
            NSLog("MenuBand KPBJ: no audio track on player item")
            return
        }

        var callbacks = MTAudioProcessingTapCallbacks(
            version: kMTAudioProcessingTapCallbacksVersion_0,
            clientInfo: UnsafeMutableRawPointer(Unmanaged.passUnretained(self).toOpaque()),
            init: kpbjTapInit,
            finalize: kpbjTapFinalize,
            prepare: kpbjTapPrepare,
            unprepare: kpbjTapUnprepare,
            process: kpbjTapProcess)

        // Current SDK bridges MTAudioProcessingTapCreate's out-pointer as
        // `MTAudioProcessingTap?` directly — Swift handles the CFType
        // retain/release automatically. (Older signatures returned an
        // `Unmanaged<MTAudioProcessingTap>?` requiring takeRetainedValue;
        // that form no longer type-checks here.)
        var tap: MTAudioProcessingTap?
        let err = MTAudioProcessingTapCreate(
            kCFAllocatorDefault,
            &callbacks,
            kMTAudioProcessingTapCreationFlag_PreEffects,
            &tap)
        guard err == noErr, let createdTap = tap else {
            NSLog("MenuBand KPBJ: tap create failed err=\(err)")
            return
        }

        let inputParams = AVMutableAudioMixInputParameters(track: track)
        inputParams.audioTapProcessor = createdTap
        let mix = AVMutableAudioMix()
        mix.inputParameters = [inputParams]
        item.audioMix = mix
    }

    // MARK: - Tap → ring buffer

    /// Called from the tap process callback. `data` is interleaved or
    /// non-interleaved depending on the source; we coerce to our internal
    /// stereo non-interleaved float32 by reading channel-major and
    /// writing into the ring's per-channel arrays.
    fileprivate func ingestTapBuffers(abl: UnsafeMutableAudioBufferListPointer,
                                      frames: Int) {
        guard let dest = ring.floatChannelData else { return }
        ringLock.lock()
        defer { ringLock.unlock() }

        let channels = Int(format.channelCount)
        let writeStart = Int(ringWriteFrame % Int64(ringFrames))

        if abl.count >= channels {
            // Non-interleaved: one AudioBuffer per channel.
            for c in 0..<channels {
                let src = abl[c].mData?.assumingMemoryBound(to: Float.self)
                guard let src = src else { continue }
                copyIntoRing(channel: c, dest: dest, src: src,
                             frames: frames, writeStart: writeStart)
            }
        } else if let src = abl[0].mData?.assumingMemoryBound(to: Float.self) {
            // Interleaved: stride by channelCount, deinterleave on copy.
            let srcChans = Int(abl[0].mNumberChannels)
            for c in 0..<channels {
                var w = writeStart
                for f in 0..<frames {
                    let s = src[f * srcChans + min(c, srcChans - 1)]
                    dest[c][w] = s
                    w += 1
                    if w >= ringFrames { w = 0 }
                }
            }
        }

        ringWriteFrame &+= Int64(frames)
        lastTapHostTime = CFAbsoluteTimeGetCurrent()
    }

    private func copyIntoRing(channel c: Int,
                              dest: UnsafePointer<UnsafeMutablePointer<Float>>,
                              src: UnsafePointer<Float>,
                              frames: Int, writeStart: Int) {
        let firstChunk = min(frames, ringFrames - writeStart)
        memcpy(dest[c].advanced(by: writeStart), src,
               firstChunk * MemoryLayout<Float>.size)
        let remaining = frames - firstChunk
        if remaining > 0 {
            memcpy(dest[c], src.advanced(by: firstChunk),
                   remaining * MemoryLayout<Float>.size)
        }
    }

    // MARK: - Per-voice playback

    /// Pitch ratio for a MIDI note relative to middle C (60). Middle C
    /// returns 1.0 (unpitched/live). Each octave doubles or halves rate
    /// — same as a vinyl pitch wheel.
    private func ratio(forNote note: UInt8) -> Float {
        Float(pow(2.0, Double(Int(note) - 60) / 12.0))
    }

    func noteOn(_ midi: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        guard attached, let engine = engine else { return }
        let voice = ensureVoice(for: channel)
        voice.midiNote = midi
        voice.held = true
        voice.varispeed.rate = ratio(forNote: midi)
        voice.node.volume = Float(velocity) / 127.0

        // Start ~250 ms behind the live write head so high-pitch reads
        // (rate > 1) have headroom before catching up.
        ringLock.lock()
        let live = ringWriteFrame
        ringLock.unlock()
        voice.readFrame = max(0, live - Int64(sampleRate * 0.25))

        // Pre-queue two chunks so playback is continuous; completion
        // chain feeds the rest while held.
        pumpVoice(voice)
        pumpVoice(voice)
        if !voice.node.isPlaying {
            voice.node.play()
        }
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
    }

    func panic() {
        for (_, v) in voices {
            v.held = false
            v.node.stop()
        }
    }

    private func ensureVoice(for channel: UInt8) -> Voice {
        if let v = voices[channel] { return v }
        let v = Voice()
        if let engine = engine {
            engine.attach(v.node)
            engine.attach(v.varispeed)
            engine.connect(v.node, to: v.varispeed, format: format)
            engine.connect(v.varispeed, to: voiceMixer, format: format)
        }
        voices[channel] = v
        return v
    }

    private func pumpVoice(_ voice: Voice) {
        guard voice.held else { return }
        let chunkFrames = AVAudioFrameCount(sampleRate * 0.150) // 150 ms
        guard let chunk = readRing(from: &voice.readFrame, frames: chunkFrames) else {
            return
        }
        voice.node.scheduleBuffer(chunk,
                                  completionCallbackType: .dataConsumed) { [weak self, weak voice] _ in
            guard let self = self, let voice = voice, voice.held else { return }
            // Hop back to main queue — scheduleBuffer fires on the audio
            // thread and we want to keep the lock-protected ring read on
            // a regular queue.
            DispatchQueue.main.async { self.pumpVoice(voice) }
        }
    }

    /// Copy `frames` from the ring starting at `readFrame`, advancing
    /// `readFrame` by the frame count. Returns nil if the ring isn't
    /// initialized. Clamps `readFrame` so it never overruns the live
    /// write head — if the voice is consuming faster than the stream is
    /// writing (e.g. rate > 1 holding for too long), we re-anchor 200 ms
    /// behind the live edge instead of reading garbage.
    private func readRing(from readFrame: inout Int64,
                          frames: AVAudioFrameCount) -> AVAudioPCMBuffer? {
        guard let out = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames),
              let dest = out.floatChannelData,
              let src = ring.floatChannelData else { return nil }
        out.frameLength = frames

        ringLock.lock()
        let live = ringWriteFrame
        let safetyFrames = Int64(sampleRate * 0.05) // 50 ms read-ahead floor
        if readFrame > live - safetyFrames {
            readFrame = max(0, live - Int64(sampleRate * 0.20))
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
        let age = CFAbsoluteTimeGetCurrent() - lastTapHostTime
        let target: Float = age < 0.10 ? 1.0 : 0.0
        let alpha: Float = 0.20 // ~5-tick rise/fall at 30 Hz
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
        // Static path: floor of 0.05 (always-present hiss when off-air),
        // up to 0.6 when network is busy AND signal is weak. Multiplying
        // by (1 - signal) means the static fades as the carrier locks in.
        let staticBase: Float = 0.05
        let staticPeak: Float = 0.6
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

// MARK: - MTAudioProcessingTap C callbacks

private let kpbjTapInit: MTAudioProcessingTapInitCallback = { tap, clientInfo, tapStorageOut in
    tapStorageOut.pointee = clientInfo
}

private let kpbjTapFinalize: MTAudioProcessingTapFinalizeCallback = { _ in }

private let kpbjTapPrepare: MTAudioProcessingTapPrepareCallback = { _, _, _ in }

private let kpbjTapUnprepare: MTAudioProcessingTapUnprepareCallback = { _ in }

private let kpbjTapProcess: MTAudioProcessingTapProcessCallback = {
    tap, numberFrames, _, bufferListInOut, numberFramesOut, _ in
    var flags: MTAudioProcessingTapFlags = 0
    let status = MTAudioProcessingTapGetSourceAudio(
        tap, numberFrames, bufferListInOut, &flags, nil, numberFramesOut)
    guard status == noErr else {
        numberFramesOut.pointee = 0
        return
    }
    let storage = MTAudioProcessingTapGetStorage(tap)
    let stream = Unmanaged<KPBJRadioStream>
        .fromOpaque(storage)
        .takeUnretainedValue()
    let abl = UnsafeMutableAudioBufferListPointer(bufferListInOut)
    stream.ingestTapBuffers(abl: abl, frames: Int(numberFramesOut.pointee))
}
