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
    /// is plain polyphony headroom like the GM node's. Ecosystem RMS sits
    /// well under a GM voice's peaks, hence the hotter master.
    private let masterGain: Double = 0.7

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

    /// Slot of the most recently triggered note, written by the render
    /// thread, read by the UI's live field strip. -1 until the first note.
    /// A torn read is harmless (visual only), so no lock.
    private var latestSlot: Int32 = -1

    /// While the Fluoddity TV is on screen, the render thread keeps the
    /// latest ecosystem's simulation ticking silently between notes so the
    /// picture stays alive instead of freezing on release. Costs about one
    /// voice of sim work, only while someone is watching. Control-thread
    /// write, render-thread read; a torn read is harmless.
    var visualLiveliness = false

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

    /// The current genome as 80 floats — the same layout Fluoddity writes to
    /// its configs, so a bred instrument can be saved, named, and returned to.
    /// Reseeding used to be the only way out of a genome, which made every
    /// good accident unrepeatable.
    func rule() -> [Float] {
        var flat = [Float](repeating: 0, count: 80)
        lock.lock()
        flat.withUnsafeMutableBufferPointer { fluod_rule_to_floats(&genome, $0.baseAddress!) }
        lock.unlock()
        return flat
    }

    /// Move the instrument a fraction of the way toward another genome.
    /// `amount` 0 changes nothing; 1 arrives. Repeated small steps walk a
    /// path between two species instead of jumping between them.
    func blend(toward target: [Float], amount: Float) {
        guard target.count >= 80, amount > 0 else { return }
        var other = FluodRule()
        lock.lock()
        target.withUnsafeBufferPointer { fluod_rule_from_floats(&other, $0.baseAddress!) }
        // The C tolerates `out` aliasing an input; Swift's exclusivity rules
        // do not, so the current genome goes through a local.
        var from = genome
        fluod_rule_lerp(&genome, &from, &other, min(1, amount))
        lock.unlock()
    }

    /// A is to B as the current genome is to — this. The parallelogram model
    /// (Ehresman & Wessel 1978): take the change that turned `a` into `b` and
    /// apply the same change here. Timbral transposition.
    func applyAnalogy(from a: [Float], to b: [Float]) {
        guard a.count >= 80, b.count >= 80 else { return }
        var ra = FluodRule(), rb = FluodRule()
        lock.lock()
        a.withUnsafeBufferPointer { fluod_rule_from_floats(&ra, $0.baseAddress!) }
        b.withUnsafeBufferPointer { fluod_rule_from_floats(&rb, $0.baseAddress!) }
        var c = genome
        fluod_rule_analogy(&genome, &ra, &rb, &c)
        lock.unlock()
    }

    /// Distance from the current genome to another, over the 80 parameters.
    /// Genome distance, not timbral distance — it answers "how far did that
    /// move me", which mutation alone never reported.
    func distance(to other: [Float]) -> Float {
        guard other.count >= 80 else { return .infinity }
        var ro = FluodRule()
        lock.lock()
        other.withUnsafeBufferPointer { fluod_rule_from_floats(&ro, $0.baseAddress!) }
        let d = fluod_rule_distance(&genome, &ro)
        lock.unlock()
        return d
    }

    func setPitchBend(amount: Float) {
        pitchScale = pow(2.0, Double(amount))
    }

    /// Copy the most recent ecosystem's flow field (FLUOD_FIELD_H rows ×
    /// FLUOD_FIELD_W cols × 2 components, row-major) for visualization.
    /// nil before any note has sounded. The copy may tear against the
    /// render thread mid-tick — fine for a picture, never used for audio.
    func fieldSnapshot() -> [Float]? {
        let slot = Int(latestSlot)
        guard slot >= 0 && slot < maxVoices else { return nil }
        let count = Int(FLUOD_FIELD_W) * Int(FLUOD_FIELD_H) * 2
        var out = [Float](repeating: 0, count: count)
        let src = fluod_voice_field_ptr(cores + slot)!
        out.withUnsafeMutableBufferPointer { dst in
            dst.baseAddress!.update(from: src, count: count)
        }
        return out
    }

    /// Copy the most recent ecosystem's particle positions — x,y pairs in
    /// [0,1)², FLUOD_PARTICLES of them. Same tear-tolerant visual-only
    /// contract as `fieldSnapshot()`.
    func particleSnapshot() -> [Float]? {
        let slot = Int(latestSlot)
        guard slot >= 0 && slot < maxVoices else { return nil }
        let n = Int(FLUOD_PARTICLES)
        var out = [Float](repeating: 0, count: n * 2)
        let p = fluod_voice_particles_ptr(cores + slot)!
        for i in 0..<n {
            out[i * 2] = p[i].px
            out[i * 2 + 1] = p[i].py
        }
        return out
    }

    /// Copy the most recent ecosystem's current scan table — the actual
    /// wavetable being heard, FLUOD_FIELD_W samples. Same tear-tolerant
    /// visual-only contract as `fieldSnapshot()`.
    func tableSnapshot() -> [Float]? {
        let slot = Int(latestSlot)
        guard slot >= 0 && slot < maxVoices else { return nil }
        let n = Int(FLUOD_FIELD_W)
        var out = [Float](repeating: 0, count: n)
        let src = fluod_voice_table_ptr(cores + slot)!
        out.withUnsafeMutableBufferPointer { dst in
            dst.baseAddress!.update(from: src, count: n)
        }
        return out
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
                latestSlot = Int32(slot)
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
                var sl: Float = 0
                var sr: Float = 0
                fluod_voice_render_stereo(ptr, sampleRate, env, f, &sl, &sr)
                // The C core traps NaN internally, but never trust a
                // render-thread value blindly (same belt-and-suspenders as
                // the GM node).
                if !sl.isFinite || !sr.isFinite { active = false; break }
                let vg = Float(vGain * g)
                left[i] += sl * vg
                right[i] += sr * vg
            }
            voices[idx].env = env
            voices[idx].active = active
        }

        var p = pitchStart
        for _ in 0..<frameCount { p += (pitchTarget - p) * pitchCoeff }
        glidePitch = p

        // TV attract mode: advance the latest ecosystem silently while no
        // note owns it, output discarded (env 0). Single writer — this is
        // the same thread that runs it when the note is sounding.
        if visualLiveliness {
            let slot = Int(latestSlot)
            if slot >= 0 && slot < maxVoices && !voices[slot].active {
                let ptr = cores + slot
                let f = voices[slot].freq
                for _ in 0..<frameCount {
                    _ = fluod_voice_render(ptr, sampleRate, 0, f)
                }
            }
        }
    }
}
