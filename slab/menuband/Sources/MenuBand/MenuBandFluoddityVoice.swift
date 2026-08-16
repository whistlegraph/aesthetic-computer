import AVFoundation
import CFluoddity
import Foundation

/// Real-time polyphonic Fluoddity voice — the audio port of
/// aphid91/Fluoddity's generalized-physarum particle system (`CFluoddity`,
/// `Sources/CFluoddity/fluoddity_voice.c`). Every sounding note owns a
/// little toroidal ecosystem: a swarm of particles sensing and depositing
/// flow trails under an 80-parameter Fourier "Rule", with the trail field
/// scanned as a wavetable at the note's fundamental (Mathews/Verplank
/// scanned synthesis). The Rule is the instrument: `setSeed` picks a
/// genome, `mutate` evolves the current one in place — a *dynamic*
/// instrument whose timbre is discovered and bred rather than designed.
///
/// Modeled directly on `MenuBandGMSynth`: an `AVAudioSourceNode` whose
/// render callback owns a fixed, preallocated voice pool (no allocation on
/// the render thread); the control thread stages note intent through a tiny
/// critical section; the render thread drains it at the top of each block.
/// The heavy C state (`FluodVoice`, ~36 KB of field/table buffers) lives in
/// a separately-allocated `cores` buffer addressed by slot — the same
/// stack-temporary/exclusivity dodge the GM node uses.
final class MenuBandFluoddityVoice {
    // MARK: Voice state (render thread owns the active pool)

    private struct Voice {
        var midi: UInt8 = 0
        var channel: UInt8 = 0
        var freq: Double = 440
        var velocityGain: Double = 1
        // Outer AR contour; the ecosystem's own trail build/decay lives
        // inside the C voice.
        var attack: Double = 0.008
        var release: Double = 0.22
        var env: Double = 0
        var releasing: Bool = false
        var active: Bool = false
    }

    // MARK: Audio graph

    private var sampleRate: Double = 48_000
    private var format: AVAudioFormat!
    private var sourceNode: AVAudioSourceNode!
    private weak var engine: AVAudioEngine?
    private var attached = false

    /// The C core AGC-levels each ecosystem near FLUOD_OUT_TARGET, so this
    /// is plain polyphony headroom like the GM node's.
    private let masterGain: Double = 0.55

    /// Fixed polyphony. Each voice simulates a whole 40-particle field, so
    /// the cap is lower than the GM node's 24.
    private let maxVoices = 10
    private var voices: [Voice]
    private let cores: UnsafeMutablePointer<FluodVoice>

    /// The current genome. Written on the control thread under `lock` and
    /// carried INTO the render thread by value inside each noteOn command,
    /// so the render callback never reads a half-mutated rule.
    private var genome = FluodRule()

    /// Per-trigger scatter seed so each note's swarm starts differently
    /// (same genome — the instrument stays the instrument).
    private var seedCounter: UInt32 = 0x517C0FFE

    /// Trackpad bend target + render-thread glide, same scheme (and time
    /// constant) as MenuBandGMSynth.
    private var pitchScale: Double = 1.0
    private var glidePitch: Double = 1.0
    private var pitchGlideCoeff: Double = 1.0 - exp(-1.0 / (48_000 * 0.004))

    // MARK: Control → render handoff

    private enum Command {
        case noteOn(midi: UInt8, channel: UInt8, velocity: UInt8,
                    rule: FluodRule, seed: UInt32)
        case noteOff(midi: UInt8, channel: UInt8)
        case panic
    }
    private var pending: [Command] = []
    private let lock = NSLock()

    init() {
        voices = [Voice](repeating: Voice(), count: maxVoices)
        cores = UnsafeMutablePointer<FluodVoice>.allocate(capacity: maxVoices)
        cores.initialize(repeating: FluodVoice(), count: maxVoices)
        pending.reserveCapacity(64)
        fluod_rule_from_seed(&genome, 100)
    }

    deinit {
        cores.deinitialize(count: maxVoices)
        cores.deallocate()
    }

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        let outRate = engine.outputNode.outputFormat(forBus: 0).sampleRate
        sampleRate = outRate > 0 ? outRate : 48_000
        pitchGlideCoeff = 1.0 - exp(-1.0 / (sampleRate * 0.004))
        format = AVAudioFormat(standardFormatWithSampleRate: sampleRate,
                               channels: 2)!
        sourceNode = AVAudioSourceNode(format: format) {
            [weak self] _, _, frameCount, ablPointer -> OSStatus in
            self?.render(frameCount: Int(frameCount), abl: ablPointer)
            return noErr
        }
        engine.attach(sourceNode)
        engine.connect(sourceNode, to: output, format: format)
        attached = true
    }

    // MARK: Public API (control thread)

    /// Pick a fresh genome from a seed — a whole new instrument.
    func setSeed(_ seed: UInt32) {
        lock.lock()
        fluod_rule_from_seed(&genome, seed)
        lock.unlock()
    }

    /// Evolve the current genome in place (Fluoddity's mutate_rule).
    /// Sounding notes keep their birth genome; future notes are the child.
    func mutate(amount: Float) {
        lock.lock()
        seedCounter = seedCounter &* 1_664_525 &+ 1_013_904_223
        fluod_rule_mutate(&genome, amount, seedCounter)
        lock.unlock()
    }

    /// Load an 80-float rule (e.g. straight out of a Fluoddity config's
    /// `rule` array) as the instrument.
    func setRule(_ flat: [Float]) {
        guard flat.count >= 80 else { return }
        lock.lock()
        flat.withUnsafeBufferPointer { fluod_rule_from_floats(&genome, $0.baseAddress!) }
        lock.unlock()
    }

    func setPitchBend(amount: Float) {
        pitchScale = pow(2.0, Double(amount))
    }

    func noteOn(_ midi: UInt8, velocity: UInt8, channel: UInt8) {
        lock.lock()
        seedCounter = seedCounter &* 1_664_525 &+ 1_013_904_223
        pending.append(.noteOn(midi: midi, channel: channel, velocity: velocity,
                               rule: genome, seed: seedCounter))
        lock.unlock()
    }

    func noteOff(_ midi: UInt8, channel: UInt8) {
        lock.lock()
        pending.append(.noteOff(midi: midi, channel: channel))
        lock.unlock()
    }

    func panic() {
        lock.lock()
        pending.append(.panic)
        lock.unlock()
    }

    // MARK: Helpers

    @inline(__always)
    private static func freq(forMIDI midi: UInt8) -> Double {
        440.0 * pow(2.0, (Double(midi) - 69.0) / 12.0)
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
            case let .noteOn(midi, channel, velocity, rule, seed):
                let slot = allocateVoice()
                let f = MenuBandFluoddityVoice.freq(forMIDI: midi)
                var r = rule
                _ = fluod_voice_init_rule(cores + slot, &r, seed, f, sampleRate)
                voices[slot].midi = midi
                voices[slot].channel = channel
                voices[slot].freq = f
                voices[slot].velocityGain = max(0.05, min(1.0, Double(velocity) / 127.0))
                voices[slot].env = 0
                voices[slot].releasing = false
                voices[slot].active = true
            case let .noteOff(midi, channel):
                for i in 0..<maxVoices where
                    voices[i].active && !voices[i].releasing
                    && voices[i].midi == midi && voices[i].channel == channel {
                    voices[i].releasing = true
                }
            case .panic:
                for i in 0..<maxVoices { voices[i].active = false }
            }
        }

        let dt = 1.0 / sampleRate
        let g = masterGain
        let pitchStart = glidePitch
        let pitchTarget = pitchScale
        let pitchCoeff = pitchGlideCoeff

        for idx in 0..<maxVoices where voices[idx].active {
            let ptr = cores + idx
            let baseFreq = voices[idx].freq
            let vGain = voices[idx].velocityGain
            let attackInc = voices[idx].attack > 0 ? dt / voices[idx].attack : 1.0
            let releaseDec = voices[idx].release > 0 ? dt / voices[idx].release : 1.0
            let releasing = voices[idx].releasing
            var env = voices[idx].env
            var active = true
            var pitch = pitchStart
            for i in 0..<frameCount {
                pitch += (pitchTarget - pitch) * pitchCoeff
                let f = baseFreq * pitch
                if releasing {
                    env -= releaseDec
                    if env <= 0 { env = 0; active = false; break }
                } else if env < 1.0 {
                    env += attackInc
                    if env > 1.0 { env = 1.0 }
                }
                let s = Double(fluod_voice_render(ptr, sampleRate, env, f))
                // The C core traps NaN internally, but never trust a
                // render-thread value blindly (same belt-and-suspenders as
                // the GM node).
                if !s.isFinite { active = false; break }
                let amp = Float(s * vGain * g)
                left[i] += amp * 0.707
                right[i] += amp * 0.707
            }
            voices[idx].env = env
            voices[idx].active = active
        }

        var p = pitchStart
        for _ in 0..<frameCount { p += (pitchTarget - p) * pitchCoeff }
        glidePitch = p
    }
}
