import AVFoundation
import Foundation

/// Composite — ~2, notepat's old `composite` wave, ported layer for layer
/// from disks/notepat.mjs (via lib/sound/synth.mjs's envelope rules).
///
/// Five oscillators per note, detuned by fixed HERTZ (not cents), so the
/// shimmer beats at the same slow rates whatever the pitch, and re-rolled
/// per strike so no two notes shimmer alike:
///
///   A  sine      f                      full      2.5 ms attack
///   B  sine      f + 9 ± 1 Hz           1/3       2.5 ms
///   C  sawtooth  f ± 6 Hz               1/48      0.5 ms   release ×2
///   D  triangle  f + 8 ± 5 Hz           1/32      1 s swell, release ×1.4
///   E  square    f ± 10 Hz              1/64      50 ms    release ×½
///
/// Held notes never decay (synth.mjs only applies decay to self-ending
/// sounds); the release is notepat's kill fade — the note's own held length
/// clamped to 75–150 ms, scaled per layer as above. Same chassis as the
/// other custom voices: an AVAudioSourceNode with a fixed voice pool and a
/// locked command queue; trackpad bend rides the pitch.
final class MenuBandCompositeVoice {
    private struct Layer {
        var phase: Double = 0
        var offsetHz: Double = 0
        var level: Double = 0
        var attack: Double = 0.0025
        var releaseScale: Double = 1
        var env: Double = 0          // attack ramp 0…1
        var fade: Double = 1         // release gain 1…0
        var fadeStep: Double = 0
        var wave: Int = 0            // 0 sine, 1 saw, 2 tri, 3 square
    }
    private struct Voice {
        var midi: UInt8 = 0
        var channel: UInt8 = 0
        var f0: Double = 440
        var gain: Float = 0
        var panL: Float = 0.7071, panR: Float = 0.7071
        var held: Double = 0
        var releasing = false
        var active = false
        var layers: [Layer] = []
    }

    private var sampleRate: Double = 48_000
    private var format: AVAudioFormat!
    private var sourceNode: AVAudioSourceNode!
    private weak var engine: AVAudioEngine?
    private var attached = false

    private let maxVoices = 12
    private var voices: [Voice]
    /// notepat's toneVolume for one layer-A sine, scaled to sit beside the
    /// other custom voices.
    private let toneLevel: Float = 0.26
    private let masterGain: Float = 0.9

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
    private var seed: UInt32 = 0xC0FFEE11

    init() {
        voices = [Voice](repeating: Voice(), count: maxVoices)
        for i in 0..<maxVoices { voices[i].layers = [Layer](repeating: Layer(), count: 5) }
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
    /// notepat's num.randIntRange(lo, hi): an integer in lo…hi inclusive.
    @inline(__always)
    private static func randInt(_ s: inout UInt32, _ lo: Int, _ hi: Int) -> Double {
        s ^= s << 13; s ^= s >> 17; s ^= s << 5
        return Double(lo + Int(s % UInt32(hi - lo + 1)))
    }
    private func allocateVoice() -> Int {
        for i in 0..<maxVoices where !voices[i].active { return i }
        var best = 0
        var bestScore = Double.greatestFiniteMagnitude
        for i in 0..<maxVoices {
            let score = (voices[i].releasing ? 0 : 1_000) + voices[i].layers[0].fade
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

        let sr = sampleRate
        for cmd in cmds {
            switch cmd {
            case let .noteOn(midi, channel, velocity, pan):
                let slot = allocateVoice()
                var v = voices[slot]
                v.midi = midi; v.channel = channel
                v.f0 = Self.freq(forMIDI: midi)
                v.gain = toneLevel * (0.6 + 0.4 * max(0.05, min(1, Float(velocity) / 127)))
                let p = Float(pan) / 127.0 * (.pi / 2)
                v.panL = cos(p); v.panR = sin(p)
                v.held = 0; v.releasing = false; v.active = true
                // The five layers, offsets re-rolled for this strike.
                v.layers[0] = Layer(offsetHz: 0, level: 1, attack: 0.0025, releaseScale: 1, wave: 0)
                v.layers[1] = Layer(offsetHz: 9 + Self.randInt(&seed, -1, 1), level: 1.0 / 3,
                                    attack: 0.0025, releaseScale: 1, wave: 0)
                v.layers[2] = Layer(offsetHz: Self.randInt(&seed, -6, 6), level: 1.0 / 48,
                                    attack: 0.0005, releaseScale: 2, wave: 1)
                v.layers[3] = Layer(offsetHz: 8 + Self.randInt(&seed, -5, 5), level: 1.0 / 32,
                                    attack: 0.999, releaseScale: 1.4, wave: 2)
                v.layers[4] = Layer(offsetHz: Self.randInt(&seed, -10, 10), level: 1.0 / 64,
                                    attack: 0.05, releaseScale: 0.5, wave: 3)
                voices[slot] = v
            case let .noteOff(midi, channel):
                for i in 0..<maxVoices where voices[i].active && !voices[i].releasing
                    && voices[i].midi == midi && voices[i].channel == channel {
                    voices[i].releasing = true
                    // notepat's kill fade: the held length, 75–150 ms.
                    let fade = max(0.075, min(voices[i].held, 0.15))
                    for l in 0..<5 {
                        let seconds = max(fade * voices[i].layers[l].releaseScale, 0.004)
                        voices[i].layers[l].fadeStep = 1.0 / (seconds * sr)
                    }
                }
            case .panic:
                for i in 0..<maxVoices { voices[i].active = false }
            }
        }

        let dt = 1.0 / sr
        let pitchStart = glidePitch
        let pitchTarget = pitchScale
        let pitchCoeff = pitchGlideCoeff

        for idx in 0..<maxVoices where voices[idx].active {
            var v = voices[idx]
            var pitch = pitchStart
            var active = true
            for i in 0..<frameCount {
                pitch += (pitchTarget - pitch) * pitchCoeff
                v.held += dt
                var mix = 0.0
                var alive = false
                for l in 0..<5 {
                    var L = v.layers[l]
                    if v.releasing {
                        L.fade -= L.fadeStep
                        if L.fade <= 0 { L.fade = 0; v.layers[l] = L; continue }
                    }
                    alive = true
                    if L.env < 1 { L.env = min(1, L.env + dt / L.attack) }
                    // Offsets are in Hz, scaled with the bend so they stay
                    // the same beat rates relative to a bent fundamental.
                    let f = (v.f0 + L.offsetHz) * pitch
                    L.phase += f * dt
                    if L.phase >= 1 { L.phase -= 1 }
                    let ph = L.phase
                    let s: Double
                    switch L.wave {
                    case 1:  s = 2 * ph - 1                                   // sawtooth
                    case 2:  s = ph < 0.5 ? 4 * ph - 1 : 3 - 4 * ph           // triangle
                    case 3:  s = ph < 0.5 ? 1 : -1                            // square
                    default: s = sin(ph * 2 * .pi)
                    }
                    mix += s * L.level * L.env * L.fade
                    v.layers[l] = L
                }
                if !alive { active = false; break }
                let out = Float(mix) * v.gain
                left[i] += out * v.panL
                right[i] += out * v.panR
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
