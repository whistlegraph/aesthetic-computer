import AVFoundation

/// The sung voice's place in the engine: overlapping phrase players on the fx bus.
/// Connected once at attach with a fixed mono format (the engine resamples
/// to the device), so scheduling never touches topology.
final class MenuBandSingerVoice {
    // Phrase renders include preroll and release. A single player's FIFO
    // queues those overlapping buffers end-to-end and drifts off the score.
    // Mix three reusable players so each phrase can start at its own epoch.
    private let player = AVAudioMixerNode()
    private let pitch = AVAudioUnitTimePitch()
    func setBend(amount: Float) {
        guard amount.isFinite else { return }
        pitch.pitch = max(-2400, min(2400, amount * 1200))
    }
    private let phrasePlayers = (0..<3).map { _ in AVAudioPlayerNode() }
    private var nextPhrasePlayer = 0
    private weak var engine: AVAudioEngine?
    private var attached = false
    /// The face this voice's meter drives (a sim tile's, or the shared one).
    var face: SingerFace = .shared
    /// Stereo place, -1…1 (a sim slot sits its member left or right).
    var pan: Float = 0 { didSet { phrasePlayers.forEach { $0.pan = pan } } }
    private var generation = 0            // bumped on stop; late timers become no-ops
    let format = AVAudioFormat(commonFormat: .pcmFormatFloat32, sampleRate: 44_100,
                               channels: 1, interleaved: false)!

    private static var lastTapLog = 0.0

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(player)
        engine.attach(pitch)
        pitch.rate = 1 // pitch slides must not stretch the score or mouth timing
        engine.connect(player, to: pitch, format: format)
        engine.connect(pitch, to: output, format: format)
        for (i, node) in phrasePlayers.enumerated() {
            engine.attach(node)
            engine.connect(node, to: player, fromBus: 0, toBus: AVAudioNodeBus(i), format: format)
        }
        attached = true
        // Proof-of-life tap: whenever the singer's node actually renders
        // audio, log its peak (rate-limited). Silence here = the buffer never
        // sounded; sound here = a downstream bus (duck/limiter/route) ate it.
        // Lip-sync meter + proof of life: every ~23 ms, the singer node's
        // RMS (jaw) and zero-crossing rate (sibilants) go to the face; a
        // rate-limited peak log says the voice is really sounding.
        phrasePlayers.forEach { $0.pan = pan }
        player.installTap(onBus: 0, bufferSize: 1024, format: nil) { [weak self] buf, _ in
            guard let ch = buf.floatChannelData, buf.frameLength > 0 else { return }
            let n = Int(buf.frameLength)
            var peak: Float = 0, sum: Float = 0, cross = 0
            var prev = ch[0][0]
            for i in 0..<n {
                let v = ch[0][i]
                peak = max(peak, abs(v)); sum += v * v
                if (v >= 0) != (prev >= 0) { cross += 1 }
                prev = v
            }
            let rms = sqrt(sum / Float(n)), zcr = Float(cross) / Float(n)
            let face = self?.face ?? .shared
            DispatchQueue.main.async { face.meter(rms: CGFloat(rms), zcr: CGFloat(zcr)) }
            guard peak > 0.01 else { return }
            let now = Date().timeIntervalSince1970
            if now - MenuBandSingerVoice.lastTapLog > 2.0 {
                MenuBandSingerVoice.lastTapLog = now
                NSLog("🎤 tap: singer rendering, peak %.3f", peak)
            }
        }
    }

    /// Sound `render` so its first sample lands at `epoch` (Unix seconds).
    /// Uses the player's own sample clock when it has one (the engine is
    /// rendering), else the host clock; a late line is trimmed to start now.
    /// Main thread — node control. Returns the lead in seconds (negative = late).
    @discardableResult
    func schedule(_ render: SungRender, atEpoch epoch: Double) -> Double {
        guard attached, let engine else { return 0 }
        // Feed the pitch processor early by its reported latency so the
        // audible phrase still lands on the conductor's original downbeat.
        let lead = epoch - Date().timeIntervalSince1970 - pitch.auAudioUnit.latency
        let gen = generation
        // Play the line by waiting for its instant on the main thread, then
        // scheduling the buffer for immediate playout — the same clock the
        // drums and key-lights use. Precise sample-time scheduling on a
        // player node proved unreliable across the engine's idle pause and
        // restarts (the buffer was accepted but silently skipped — the
        // silent-voice bug of Sept 20); this always sounds.
        let fire: () -> Void = { [weak self] in
            guard let self, self.generation == gen else { return }
            if !engine.isRunning { try? engine.start() }
            let node = self.phrasePlayers[self.nextPhrasePlayer]
            self.nextPhrasePlayer = (self.nextPhrasePlayer + 1) % self.phrasePlayers.count
            node.stop() // reset this phrase's sample clock when a slot is reused
            node.scheduleBuffer(render.buffer, at: nil, options: .interrupts, completionHandler: nil)
            if !node.isPlaying { node.play() }
            guard node.isPlaying else { NSLog("🎤 sing: player would not start"); return }
            if let articulation = render.articulation {
                // Hardware latency queries cross into CoreAudio. Read once
                // per phrase, never six times per display frame (mouth +
                // breath across three overlapping players).
                let presentationLatency = engine.outputNode.presentationLatency + self.pitch.auAudioUnit.latency
                // The face samples the same player clock as this buffer. A
                // delayed start, engine stall or stop cannot leave it singing
                // on the conductor's old wall-clock timeline.
                self.face.follow(articulation, slot: ObjectIdentifier(node)) { [weak node] in
                    guard let node, node.isPlaying, let rt = node.lastRenderTime,
                          let pt = node.playerTime(forNodeTime: rt) else { return nil }
                    let hostNow = ProcessInfo.processInfo.systemUptime
                    let sinceRender = max(0, hostNow - AVAudioTime.seconds(forHostTime: rt.hostTime))
                    return Double(pt.sampleTime) / pt.sampleRate + sinceRender
                        - presentationLatency
                }
            }
            NSLog("🎤 sing: playing %.2f s now",
                  Double(render.buffer.frameLength) / render.buffer.format.sampleRate)
        }
        if lead <= 0.01 {
            if Thread.isMainThread { fire() } else { DispatchQueue.main.async(execute: fire) }
        } else {
            DispatchQueue.main.asyncAfter(deadline: .now() + lead, execute: fire)
        }
        return lead
    }

    /// Drop every queued line (stop cancels a pending sung line).
    func stop() {
        guard attached else { return }
        generation += 1
        phrasePlayers.forEach { $0.stop() }
        nextPhrasePlayer = 0
        face.clearArticulation()
    }

    static func convert(_ pcm: AVAudioPCMBuffer, to format: AVAudioFormat) -> AVAudioPCMBuffer? { MenuBandSinger.convert(pcm, to: format) }
}
