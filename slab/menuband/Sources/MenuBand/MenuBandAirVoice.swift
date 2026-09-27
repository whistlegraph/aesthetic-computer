import AVFoundation

/// ⇧Space: air. Holding it wooshes a bed of noise up under the playing —
/// cabin rumble low down, and above it the same noise blown through
/// resonators tuned to the last note or chord, so the air sings the pitch
/// you just left. Letting go lets it drift out. It sits on the pre-limiter
/// fx bus like the surface cues, so echo and space color it, and a
/// varispeed carries the trackpad bend so the air slides with the notes.
final class MenuBandAirVoice {
    private let player = AVAudioPlayerNode()
    private let speed = AVAudioUnitVarispeed()
    private let mixer = AVAudioMixerNode()
    private weak var engine: AVAudioEngine?
    private var attached = false
    private let sampleRate: Double = 44_100
    private let renderFormat = AVAudioFormat(
        commonFormat: .pcmFormatFloat32, sampleRate: 44_100,
        channels: 1, interleaved: false)!

    /// Where the swell is heading (1 while held, 0 after release) and
    /// where it is. Main thread, like every caller.
    private var target: Float = 0
    private var level: Float = 0
    private var easeTimer: Timer?
    private var lastEaseTick: CFTimeInterval = 0

    /// Test seam: the node the bed leaves through.
    var outputNodeForTesting: AVAudioNode { mixer }

    /// Loudest the bed opens. The loop peaks near 0.6 and the fx bus adds
    /// makeup downstream; the air is weather under the instrument.
    static let ceiling: Float = 0.42
    /// The woosh in: fast enough to answer the key, slow enough to breathe.
    static let attackSeconds: Double = 0.7
    /// The drift out after release.
    static let releaseSeconds: Double = 2.4
    private static let floor: Float = 0.004
    private static let easeHz: Double = 60

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(player)
        engine.attach(speed)
        engine.attach(mixer)
        engine.connect(player, to: speed, format: renderFormat)
        engine.connect(speed, to: mixer, format: renderFormat)
        engine.connect(mixer, to: output, format: nil)
        mixer.outputVolume = 0
        attached = true
    }

    /// Mirror of the controller bend, one unit = one octave.
    func setBend(amount: Float) {
        speed.rate = pow(2, max(-2, min(2, amount)))
    }

    /// Start the woosh. The loop is rendered now, around the pitches given,
    /// so each push sings whatever was played last. A push while already
    /// sounding just retunes and keeps swelling.
    func push(pitches: [UInt8]) {
        guard attached, let engine else {
            debugLog("air: not attached"); return
        }
        // `isRunning`, not `isRenderingLive`: the output's last render
        // time can sit a device latency AHEAD of now, which that check
        // reads as not live. A running engine is enough for a bed that
        // swells in over most of a second.
        guard engine.isRunning else {
            debugLog("air: engine not running"); return
        }
        guard let loop = Self.makeLoop(pitches: pitches, sampleRate: sampleRate,
                                       format: renderFormat) else {
            debugLog("air: no loop"); return
        }
        debugLog("air: push \(pitches) level=\(level)")
        target = 1
        player.stop()
        player.scheduleBuffer(loop, at: nil, options: .loops, completionHandler: nil)
        player.play()
        mixer.outputVolume = level * Self.ceiling
        startEase()
    }

    /// Let go: the bed drifts out on its own time.
    func release() {
        target = 0
    }

    /// Immediate silence — teardown, focus loss.
    func stop() {
        target = 0
        level = 0
        mixer.outputVolume = 0
        easeTimer?.invalidate()
        easeTimer = nil
        if player.isPlaying { player.stop() }
    }

    private func startEase() {
        guard easeTimer == nil else { return }
        lastEaseTick = CACurrentMediaTime()
        let timer = Timer(timeInterval: 1.0 / Self.easeHz, repeats: true) {
            [weak self] _ in self?.ease()
        }
        RunLoop.main.add(timer, forMode: .common)
        easeTimer = timer
    }

    /// One-pole toward the target, with its own time on the way up and the
    /// way down. Stops the player once it has drifted below hearing.
    private func ease() {
        let now = CACurrentMediaTime()
        let elapsed = max(1.0 / 240.0, min(1.0 / 20.0, now - lastEaseTick))
        lastEaseTick = now
        let tau = target > level ? Self.attackSeconds : Self.releaseSeconds
        let alpha = Float(1 - exp(-elapsed / tau))
        level += (target - level) * alpha
        if target == 0, level <= Self.floor {
            stop()
            return
        }
        mixer.outputVolume = level * Self.ceiling
    }

    // MARK: - Synthesis

    /// A resonant bandpass (RBJ, unity peak) — noise through it whistles at
    /// the center, the way air through a pipe does.
    private struct Resonator {
        let b0: Float, b2: Float, a1: Float, a2: Float
        var x1: Float = 0, x2: Float = 0, y1: Float = 0, y2: Float = 0

        init(frequency: Double, q: Double, sampleRate: Double) {
            let w = 2 * Double.pi * frequency / sampleRate
            let alpha = sin(w) / (2 * q)
            let a0 = 1 + alpha
            b0 = Float(alpha / a0)
            b2 = Float(-alpha / a0)
            a1 = Float(-2 * cos(w) / a0)
            a2 = Float((1 - alpha) / a0)
        }

        mutating func process(_ x: Float) -> Float {
            let y = b0 * x + b2 * x2 - a1 * y1 - a2 * y2
            x2 = x1; x1 = x
            y2 = y1; y1 = y
            return y
        }
    }

    /// Two seconds of air that loops without a seam: brown rumble low
    /// down, a thread of pink hiss, and the tuned whistle of each pitch
    /// (plus a soft octave above it) on top. Peak-normalized so a triad is
    /// no louder than a single note.
    static func makeLoop(pitches: [UInt8], sampleRate: Double,
                         format: AVAudioFormat) -> AVAudioPCMBuffer? {
        let dur = 2.0
        let frameCount = AVAudioFrameCount(sampleRate * dur)
        guard let buffer = AVAudioPCMBuffer(pcmFormat: format,
                                            frameCapacity: frameCount) else {
            return nil
        }
        buffer.frameLength = frameCount
        guard let data = buffer.floatChannelData?[0] else { return nil }
        let total = Int(frameCount)
        // Render a little past the end, then fold that overrun into the
        // head: the last sample runs straight into what the first sample
        // becomes, so the loop wraps without a click.
        let fade = Int(sampleRate * 0.08)
        var raw = [Float](repeating: 0, count: total + fade)
        let tones = Set(pitches.isEmpty ? [60] : pitches)
        var resonators: [Resonator] = tones.flatMap { midi -> [Resonator] in
            let f = 440 * pow(2, (Double(midi) - 69) / 12)
            return [
                Resonator(frequency: f, q: 38, sampleRate: sampleRate),
                Resonator(frequency: f * 2, q: 30, sampleRate: sampleRate),
            ]
        }
        let octaveWeight: Float = 0.35
        var brown: Float = 0
        var brownLP: Float = 0
        var pink0: Float = 0, pink1: Float = 0, pink2: Float = 0
        var wander: Float = 0
        var peak: Float = 0
        for i in 0..<(total + fade) {
            let white = Float.random(in: -1...1)
            // Cabin floor: integrated noise, leaky, then darkened again.
            brown = brown * 0.998 + white * 0.02
            brownLP = brownLP * 0.97 + brown * 0.03
            // Pink-ish hiss (three-pole approximation).
            pink0 = 0.99765 * pink0 + white * 0.0990460
            pink1 = 0.96300 * pink1 + white * 0.2965164
            pink2 = 0.57000 * pink2 + white * 1.0526913
            let pink = (pink0 + pink1 + pink2 + white * 0.1848) * 0.05
            // Slow swell in the whistle so it breathes rather than holds.
            wander = wander * 0.9995 + Float.random(in: -1...1) * 0.0005
            var whistle: Float = 0
            for r in resonators.indices {
                let w = resonators[r].process(white)
                whistle += r % 2 == 0 ? w : w * octaveWeight
            }
            // The whistle carries; the floor is weather under it (tuned
            // so a tone sits hundreds of times above the band beside it).
            let s = brownLP * 3.2 + pink * 0.5
                + whistle * (30 + wander * 300)
            raw[i] = s
        }
        for i in 0..<total {
            if i < fade {
                let t = Float(i) / Float(fade)
                data[i] = raw[i] * t + raw[total + i] * (1 - t)
            } else {
                data[i] = raw[i]
            }
            peak = max(peak, abs(data[i]))
        }
        let gain = peak > 0 ? 0.6 / peak : 1
        for i in 0..<total { data[i] *= gain }
        return buffer
    }
}
