import AVFoundation
import Foundation

/// The AC whistle — custom instrument `79, our own version of GM 79.
///
/// A human whistle, not a tin whistle. Pursed-lip whistling is a Helmholtz
/// resonator (the mouth) driven by the lip jet: one mode, so the tone is
/// nearly a pure sine with only a faint overtone, and a turbulent jet, so
/// the timbre never sits still (Shadle 1983; the oral-whistling MRI work).
/// That is modeled here directly rather than as a waveguide:
///
///   • a sine core whose pitch carries an onset scoop, 1/f-ish jitter and a
///     vibrato that only grows in after the note has settled;
///   • one gentle second harmonic that rises with breath pressure;
///   • "air": white noise through a high-Q resonator sitting on the
///     fundamental (the turbulence the cavity itself shapes) plus a quiet
///     hiss band under it, both riding the breath;
///   • breath pressure that drifts and flutters, so loudness and overtone
///     wander the way a real whistler's do.
///
/// Same chassis as the piano and Fluoddity voices: an AVAudioSourceNode with
/// a preallocated voice pool, control thread staging commands under a lock,
/// the render thread draining them per block. Trackpad bend rides the pitch.
final class MenuBandWhistleVoice {
    private struct Voice {
        var midi: UInt8 = 0
        var channel: UInt8 = 0
        var f0: Double = 440
        var gain: Float = 0
        var panL: Float = 0.7071, panR: Float = 0.7071
        var phase: Double = 0
        var age: Double = 0          // seconds since onset
        var env: Double = 0          // amplitude envelope 0…1
        var releasing = false
        var active = false
        var seed: UInt32 = 0x9E3779B9
        // Resonator (RBJ bandpass on the fundamental) + hiss filters
        var r1: Double = 0, r2: Double = 0
        var hpz: Double = 0, lpz: Double = 0
    }

    private var sampleRate: Double = 48_000
    private var format: AVAudioFormat!
    private var sourceNode: AVAudioSourceNode!
    private weak var engine: AVAudioEngine?
    private var attached = false

    private let maxVoices = 12
    private var voices: [Voice]
    private let masterGain: Float = 0.9
    /// A whistle is loud for how pure it is — fundamental level per voice.
    private let toneLevel: Float = 0.30
    /// Second harmonic at full breath: about −26 dB under the fundamental.
    private let overtoneLevel: Double = 0.05
    /// Tuned air (resonator on f0) and plain hiss, under the tone.
    private let airLevel: Double = 0.09
    private let hissLevel: Double = 0.014
    private let attackSeconds = 0.035
    private let releaseSeconds = 0.09
    /// Onset scoop: starts this many cents flat and settles in ~45 ms.
    private let scoopCents = -45.0
    private let scoopTau = 0.045
    private let jitterCents = 6.0
    private let vibratoCents = 5.0

    /// One whistler, one breath. Every sounding voice shares the breath
    /// drift, flutter, pitch jitter and vibrato below. Held together (K and
    /// L, say) two voices then move as one, so their interference pattern
    /// holds still and reads as a steady beat instead of a wandering phase —
    /// independent jitter per voice re-randomized it every few hundred ms.
    private var breathDrift: Double = 0
    private var flutter: Double = 0
    private var jitter: Double = 0
    private var vibPhase: Double = 0
    private var vibRate: Double = 5.2
    private var breathSeed: UInt32 = 0x2545F491
    private var sounding = 0
    /// Per-block scratch for the shared breath, preallocated: the render
    /// thread never allocates. 8192 frames covers any CoreAudio block.
    private var breathBuf = [Double](repeating: 0, count: 8192)
    private var centsBuf = [Double](repeating: 0, count: 8192)

    private var pitchScale: Double = 1.0
    private var glidePitch: Double = 1.0
    private var pitchGlideCoeff: Double = 1.0 - exp(-1.0 / (48_000 * 0.004))
    private var channelPan: [UInt8] = [UInt8](repeating: 64, count: 16)

    private enum Command {
        case noteOn(midi: UInt8, channel: UInt8, velocity: UInt8, pan: UInt8)
        case noteOff(midi: UInt8, channel: UInt8)
        case panic
    }
    private var pending: [Command] = []
    private let lock = NSLock()
    private var seedCounter: UInt32 = 0x1234_5678

    init() {
        voices = [Voice](repeating: Voice(), count: maxVoices)
        pending.reserveCapacity(64)
    }

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        let outRate = engine.outputNode.outputFormat(forBus: 0).sampleRate
        sampleRate = outRate > 0 ? outRate : 48_000
        pitchGlideCoeff = 1.0 - exp(-1.0 / (sampleRate * 0.004))
        format = AVAudioFormat(standardFormatWithSampleRate: sampleRate, channels: 2)!
        sourceNode = AVAudioSourceNode(format: format) {
            [weak self] _, _, frameCount, ablPointer -> OSStatus in
            self?.render(frameCount: Int(frameCount), abl: ablPointer)
            return noErr
        }
        engine.attach(sourceNode)
        engine.connect(sourceNode, to: output, format: format)
        attached = true
    }

    // MARK: Control thread

    func setPan(_ pan: UInt8, channel: UInt8) {
        lock.lock(); channelPan[Int(channel & 0x0F)] = pan & 0x7F; lock.unlock()
    }
    func setPitchBend(amount: Float) { pitchScale = pow(2.0, Double(amount)) }
    func noteOn(_ midi: UInt8, velocity: UInt8, channel: UInt8) {
        lock.lock()
        let pan = channelPan[Int(channel & 0x0F)]
        pending.append(.noteOn(midi: midi, channel: channel, velocity: velocity, pan: pan))
        lock.unlock()
    }
    func noteOff(_ midi: UInt8, channel: UInt8) {
        lock.lock(); pending.append(.noteOff(midi: midi, channel: channel)); lock.unlock()
    }
    func panic() { lock.lock(); pending.append(.panic); lock.unlock() }

    // MARK: Helpers

    @inline(__always)
    private static func freq(forMIDI midi: UInt8) -> Double {
        440.0 * pow(2.0, (Double(midi) - 69.0) / 12.0)
    }
    @inline(__always)
    private static func noise(_ s: inout UInt32) -> Double {
        s ^= s << 13; s ^= s >> 17; s ^= s << 5
        return Double(s) / Double(UInt32.max) * 2 - 1
    }
    private func allocateVoice() -> Int {
        for i in 0..<maxVoices where !voices[i].active { return i }
        var best = 0
        var bestScore = Double.greatestFiniteMagnitude
        for i in 0..<maxVoices {
            let score = (voices[i].releasing ? 0 : 1_000) + voices[i].env
            if score < bestScore { bestScore = score; best = i }
        }
        return best
    }

    // MARK: Render thread

    private func render(frameCount: Int, abl: UnsafeMutablePointer<AudioBufferList>) {
        let buffers = UnsafeMutableAudioBufferListPointer(abl)
        let left = buffers[0].mData!.assumingMemoryBound(to: Float.self)
        let right = (buffers.count > 1 ? buffers[1].mData! : buffers[0].mData!)
            .assumingMemoryBound(to: Float.self)
        for i in 0..<frameCount { left[i] = 0; right[i] = 0 }

        lock.lock()
        let cmds = pending
        if !pending.isEmpty { pending.removeAll(keepingCapacity: true) }
        lock.unlock()

        for cmd in cmds {
            switch cmd {
            case let .noteOn(midi, channel, velocity, pan):
                let slot = allocateVoice()
                seedCounter = seedCounter &* 1_664_525 &+ 1_013_904_223
                var v = Voice()
                v.midi = midi; v.channel = channel
                v.f0 = Self.freq(forMIDI: midi)
                v.gain = toneLevel * (0.55 + 0.45 * max(0.05, min(1, Float(velocity) / 127)))
                let p = Float(pan) / 127.0 * (.pi / 2)
                v.panL = cos(p); v.panR = sin(p)
                v.seed = seedCounter | 1
                v.active = true
                voices[slot] = v
            case let .noteOff(midi, channel):
                for i in 0..<maxVoices where voices[i].active && !voices[i].releasing
                    && voices[i].midi == midi && voices[i].channel == channel {
                    voices[i].releasing = true
                }
            case .panic:
                for i in 0..<maxVoices { voices[i].active = false }
            }
        }

        let sr = sampleRate
        let dt = 1.0 / sr
        let attackInc = dt / attackSeconds
        let releaseMul = exp(-dt / releaseSeconds)
        let pitchStart = glidePitch
        let pitchTarget = pitchScale
        let pitchCoeff = pitchGlideCoeff
        // Filter time constants for the stochastic parts.
        let driftCoeff = 1 - exp(-dt * 2 * .pi * 0.7)     // breath wander ~0.7 Hz
        let flutterCoeff = 1 - exp(-dt * 2 * .pi * 18)    // breath tremor ~18 Hz
        let jitterCoeff = 1 - exp(-dt * 2 * .pi * 7)      // pitch jitter bandwidth
        let hpCoeff = exp(-dt * 2 * .pi * 1_800)           // hiss band: 1.8–5 kHz
        let lpCoeff = exp(-dt * 2 * .pi * 5_000)

        // Shared breath for the block: wander, tremor, jitter and vibrato
        // advance once per sample into scratch arrays every voice reads.
        let frames = min(frameCount, breathBuf.count)
        sounding = 0
        var vibGrow = 0.0
        for i in 0..<maxVoices where voices[i].active {
            sounding += 1
            vibGrow = max(vibGrow, voices[i].age)
        }
        for i in 0..<frames {
            let n1 = Self.noise(&breathSeed)
            breathDrift += (n1 * 0.14 - breathDrift) * driftCoeff
            let n2 = Self.noise(&breathSeed)
            flutter += (n2 * 0.06 - flutter) * flutterCoeff
            breathBuf[i] = max(0.35, min(1.15, 0.82 + breathDrift + flutter))
            let n3 = Self.noise(&breathSeed)
            jitter += (n3 * jitterCents - jitter) * jitterCoeff
            let vibDepth = vibratoCents * min(1, vibGrow / 0.45) * breathBuf[i]
            vibPhase += vibRate * dt
            if vibPhase >= 1 { vibPhase -= 1 }
            centsBuf[i] = jitter + sin(vibPhase * 2 * .pi) * vibDepth
        }
        // Air thins as voices stack so the noise beds don't pile up.
        let airScale = 1.0 / sqrt(Double(max(1, sounding)))

        for idx in 0..<maxVoices where voices[idx].active {
            var v = voices[idx]
            var pitch = pitchStart
            var active = true
            for i in 0..<frames {
                pitch += (pitchTarget - pitch) * pitchCoeff
                // Envelope: quick breath-in, exponential damper-free release.
                if v.releasing {
                    v.env *= releaseMul
                    if v.env < 0.0008 { active = false; break }
                } else if v.env < 1 {
                    v.env = min(1, v.env + attackInc)
                }
                v.age += dt
                let breath = breathBuf[i]
                // Pitch: this voice's onset scoop and release droop, plus
                // the shared jitter and vibrato.
                let scoop = scoopCents * exp(-v.age / scoopTau)
                let droop = v.releasing ? -14.0 * (1 - v.env) : 0
                let cents = scoop + centsBuf[i] + droop
                let f = v.f0 * pitch * pow(2.0, cents / 1200.0)
                v.phase += f * dt
                if v.phase >= 1 { v.phase -= 1 }
                let ph = v.phase * 2 * .pi
                let tone = sin(ph) + sin(2 * ph) * overtoneLevel * breath
                // Air: a high-Q resonator on f0 fed with white noise, and a
                // quiet hiss band. RBJ bandpass, coefficients from the live f.
                let w = 2 * .pi * min(f, sr * 0.45) / sr
                let alpha = sin(w) / (2 * 28.0)
                let a0 = 1 + alpha
                let x = Self.noise(&v.seed)
                let y = (alpha * x - (-2 * cos(w)) * v.r1 - (1 - alpha) * v.r2) / a0
                // r1/r2 hold past outputs of the all-pole part; input x has
                // b1 = 0, b2 = -alpha, applied by tracking the input history
                // in the same registers is overkill — a resonator fed by white
                // noise sounds the same with the feed-forward zero dropped.
                v.r2 = v.r1; v.r1 = y
                v.hpz = hpCoeff * (v.hpz + x) - x        // one-pole highpass
                let hp = x + v.hpz
                v.lpz = v.lpz * lpCoeff + hp * (1 - lpCoeff)
                let air = (y * airLevel * (0.6 + 0.4 * breath)
                    + v.lpz * hissLevel * breath) * airScale
                let amp = Float(v.env * (0.78 + 0.22 * breath))
                let s = (Float(tone + air) * amp) * v.gain
                left[i] += s * v.panL
                right[i] += s * v.panR
            }
            v.active = active
            voices[idx] = v
        }
        glidePitch = pitchStart + (pitchTarget - pitchStart)
            * (1 - pow(1 - pitchCoeff, Double(frameCount)))
        let g = masterGain
        for i in 0..<frameCount {
            left[i] = tanh(left[i] * g)
            right[i] = tanh(right[i] * g)
        }
    }
}
