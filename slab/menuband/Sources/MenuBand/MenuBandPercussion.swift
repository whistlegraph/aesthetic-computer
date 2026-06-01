import AVFoundation
import Foundation
import QuartzCore

/// Real-time percussion synthesizer — a Swift port of the AC-native
/// `lib/percussion.mjs` 12-drum kit (kick/snare/clap/snap/hats/ride +
/// crash/splash/cowbell/block/tambo accents). Each drum is a stack of
/// layered one-shot voices (sine / triangle / square / biquad-filtered
/// noise with attack+decay envelopes), synthesized live per hit with the
/// same `rj`/`rn` randomization as notepat so repeats don't sound
/// identical. Mirrors the oscillator + `compute_envelope` math in
/// `fedac/native/src/audio.c`.
///
/// Used by Menu Band's right-hand percussion split: the upper-octave keys
/// trigger drums through this node while the lower keys stay melodic.
final class MenuBandPercussion {
    // MARK: Drum kit

    /// The 12 drum slots, indexed by pitch class (0 = C … 11 = B), so a
    /// key's intrinsic semitone (mod 12) selects its drum — octave-
    /// invariant, exactly like notepat's drum wave.
    enum Drum: Int, CaseIterable {
        case kick = 0, crash = 1, snare = 2, splash = 3, clap = 4, snap = 5
        case cowbell = 6, hatClosed = 7, block = 8, hatOpen = 9, tambo = 10, ride = 11

        static func forPitchClass(_ pc: Int) -> Drum {
            Drum(rawValue: ((pc % 12) + 12) % 12) ?? .kick
        }

        /// 3-char pad label, matching PERCUSSION_LABELS in percussion.mjs.
        var label: String {
            switch self {
            case .kick: return "BAS"
            case .snare: return "SNR"
            case .clap: return "CLP"
            case .snap: return "SNP"
            case .hatClosed: return "HHC"
            case .hatOpen: return "HHO"
            case .ride: return "RDE"
            case .crash: return "CRS"
            case .splash: return "SPL"
            case .cowbell: return "CBL"
            case .block: return "BLK"
            case .tambo: return "TMB"
            }
        }
    }

    private enum Wave { case sine, triangle, square, noise }

    /// A drum hit's timestamp + 0…1 level, kept per pitch class so the
    /// menubar can shake/blink the matching key with the live hit data.
    struct DrumPulse {
        var at: Double = 0       // CACurrentMediaTime of the hit
        var level: Double = 0    // 0…1 (velocity-derived)
    }
    /// Most recent hit per pitch class (index == Drum.rawValue). Read as a
    /// snapshot by the icon renderer each frame.
    private var pulses = [DrumPulse](repeating: DrumPulse(), count: 12)

    /// Thread-safe copy of the current per-pitch-class hit pulses.
    func pulseSnapshot() -> [DrumPulse] {
        lock.lock(); defer { lock.unlock() }
        return pulses
    }

    // MARK: Voice state (render thread owns the active pool)

    private struct Voice {
        var wave: Wave
        var freq: Double          // Hz; for noise this is the LPF cutoff
        var duration: Double      // seconds (.infinity for held voices)
        var volume: Double        // linear amplitude
        var attack: Double        // seconds
        var decay: Double         // seconds (fade at the tail)
        var gainL: Float
        var gainR: Float
        // held / release behavior (hi-hat foot-pedal model)
        var group: UInt64 = 0     // owning key-press, for release routing
        var held: Bool = false    // rings until released (ignores duration)
        var releaseFade: Double = 0   // fade-out seconds once released
        var toneTarget: Double = 0    // freq slides here on release (0 = keep)
        var releasing: Bool = false
        var releaseElapsed: Double = 0
        var releaseFromFreq: Double = 0
        // running state
        var phase: Double = 0
        var elapsed: Double = 0
        // biquad low-pass state for noise voices
        var nb0: Double = 0, nb1: Double = 0, nb2: Double = 0
        var na1: Double = 0, na2: Double = 0
        var nx1: Double = 0, nx2: Double = 0, ny1: Double = 0, ny2: Double = 0
        var seed: UInt32 = 1
    }

    // MARK: Audio graph

    /// Render sample rate — set to the engine's output rate at attach so
    /// our synthesis runs at the hardware clock with no resample (the noise
    /// biquad + phase increments use this). Pitch itself is absolute Hz, so
    /// every drum's tones match AC-native regardless of the rate.
    private var sampleRate: Double = 48_000
    private var format: AVAudioFormat!
    private var sourceNode: AVAudioSourceNode!
    private weak var engine: AVAudioEngine?
    private var attached = false

    /// Master headroom so a fistful of simultaneous drums doesn't slam the
    /// limiter — the kit's raw layer volumes sum well past 1.0 by design.
    private let masterGain: Float = 0.34

    /// Global pitch multiplier applied to every voice's phase increment
    /// (and noise LPF cutoff) so the trackpad pitch-bend warps the whole
    /// kit alongside the melodic voices. 1.0 = no shift; 2.0 = +1 octave.
    /// Written from the main thread by `setPitchBend`, read on the render
    /// thread — a plain Double load/store is atomic enough here (worst
    /// case a single block reads a half-stale value, inaudible).
    private var pitchScale: Double = 1.0

    /// Set the kit-wide pitch shift from a bend amount in the same units
    /// the melodic voices use (±1 == ±12 semitones). Applied live to all
    /// sounding + future drum voices.
    func setPitchBend(amount: Float) {
        pitchScale = pow(2.0, Double(amount))   // 1 unit = one octave
    }

    /// Active voices, mutated only on the render thread. Fixed cap keeps
    /// the per-cycle work bounded under fast play / rolls.
    private var active: [Voice] = []
    private let maxVoices = 96

    /// Pending voices staged by the control thread, drained at the top of
    /// each render cycle. Guarded by a tiny critical section.
    private var pending: [Voice] = []
    private let lock = NSLock()

    /// Monotonic id per key-press, so a key-up can release exactly the held
    /// voices it started (the hi-hat foot-pedal model).
    private var groupCounter: UInt64 = 0
    /// Release-burst voices staged at key-down, fired at key-up (closed-hat
    /// "lift click"). Keyed by group.
    private var releaseStore: [UInt64: [Voice]] = [:]
    /// Groups awaiting release, drained by the render thread to flip their
    /// held voices into the fade-out (open-hat foot-pedal damping).
    private var pendingReleases: [UInt64] = []

    /// Latency probe: when voices were staged (noteOn) vs. when the render
    /// thread first picked them up — the trigger→render handoff, the
    /// percussion-specific slice of keypress→sound latency.
    private var pendingStageTime: Double = 0
    private var lastHandoffSec: Double = 0
    /// Most recent trigger→render handoff in milliseconds (≤ one buffer).
    func triggerHandoffMs() -> Double {
        lock.lock(); defer { lock.unlock() }
        return lastHandoffSec * 1000.0
    }

    init() {
        active.reserveCapacity(maxVoices)
        pending.reserveCapacity(maxVoices)
    }

    /// Attach into the synth graph — mirrors MenuBandTape / radio / sample
    /// backends, connecting straight to the pre-limiter sum so drums share
    /// the master gain + limiter with every other voice. The source node is
    /// built here at the engine's output rate so there's no resample.
    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        let outRate = engine.outputNode.outputFormat(forBus: 0).sampleRate
        sampleRate = outRate > 0 ? outRate : 48_000
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

    // MARK: Trigger

    /// Fire a drum. `velocity` 0–127, `pan` −1…1. Synthesizes the layered
    /// voices on the calling (control) thread and stages them for the
    /// render thread — no audio work happens here.
    /// One-shot convenience (UI cues): fire a drum's down voices and let it
    /// ring out. Any held voices are forced finite — used only for the
    /// kick / closed-hat toggle cues, which have none.
    func play(_ drum: Drum, velocity: UInt8, pan: Double) {
        let v = max(0.1, min(2.2, Double(velocity) / 100.0))
        let dv = build(for: drum, v: v, basePan: pan, group: 0)
        let voices = dv.down.map(finite)
        guard !voices.isEmpty else { return }
        let now = CACurrentMediaTime()
        let level = min(1.0, max(0.0, Double(velocity) / 127.0))
        lock.lock()
        for s in voices where pending.count < maxVoices { pending.append(s) }
        pulses[drum.rawValue] = DrumPulse(at: now, level: level)
        lock.unlock()
    }

    /// Key-down: fire the drum and return a group token. Held voices (open
    /// hat) ring until `noteOff(group)`; closed-hat stores its release burst
    /// for that moment. The caller tracks the group per key.
    @discardableResult
    func noteOn(_ drum: Drum, velocity: UInt8, pan: Double, accent: Bool = false) -> UInt64 {
        let v = max(0.1, min(2.2, Double(velocity) / 100.0))
        lock.lock(); groupCounter &+= 1; let g = groupCounter; lock.unlock()
        let dv = build(for: drum, v: v, basePan: pan, group: g)
        var down = dv.down
        if accent {
            // Accent (shift + drum): a HARDER hit, not a reversal. Boost the
            // whole kit and stack a per-drum "intensity" layer so each sound
            // has its own idea of slamming harder — punchier kick, crackier
            // snare, brighter hats… It's the percussion analogue of the
            // melodic linger: shift = more.
            for i in down.indices { down[i].volume *= 1.30 }
            down.append(contentsOf: accentLayers(for: drum, v: v, basePan: pan))
        }
        let now = CACurrentMediaTime()
        let level = min(1.0, max(0.0, Double(velocity) / 127.0))
        lock.lock()
        for s in down where pending.count < maxVoices { pending.append(s) }
        if !dv.release.isEmpty { releaseStore[g] = dv.release }
        pulses[drum.rawValue] = DrumPulse(at: now, level: min(1.0, level * (accent ? 1.2 : 1.0)))
        pendingStageTime = now
        lock.unlock()
        return g
    }

    /// Per-drum "harder hit" layers stacked on top of the normal voices when
    /// shift accents a drum. Each drum gets its own character boost rather
    /// than a generic louder — a deeper/snappier kick, a crackier snare,
    /// brighter cymbals, and so on.
    private func accentLayers(for drum: Drum, v: Double, basePan: Double) -> [Voice] {
        var out: [Voice] = []
        let dp = basePan
        switch drum {
        case .kick:
            out.append(makeVoice(.sine, 44, rj(0.32, 0.15), 1.5 * v, 0.002, 0.31, dp))   // deeper sub
            out.append(makeVoice(.noise, 4200, 0.003, 0.62 * v, 0.0001, 0.0028, dp))      // sharper beater click
            out.append(makeVoice(.sine, 120, 0.060, 1.05 * v, 0.001, 0.058, dp))          // extra punch
        case .snare:
            out.append(makeVoice(.noise, 4800, rj(0.13, 0.2), 0.7 * v, 0.0004, 0.122, dp)) // brighter crack
            out.append(makeVoice(.triangle, 330, 0.020, 0.30 * v, 0.001, 0.019, dp))       // rim ring
        case .clap:
            out.append(makeVoice(.noise, 1400, 0.050, 0.7 * v, 0.001, 0.045, dp))          // extra burst
        case .snap:
            out.append(makeVoice(.noise, 7200, 0.003, 0.55 * v, 0.0001, 0.0028, dp))       // sharper snap
        case .hatClosed:
            out.append(makeVoice(.noise, 9500, rj(0.05, 0.2), 0.40 * v, 0.0004, 0.045, dp)) // brighter sizzle
        case .hatOpen:
            out.append(makeVoice(.noise, 9500, rj(0.55, 0.2), 0.34 * v, 0.003, 0.5, dp))    // longer sizzle
        case .ride:
            out.append(makeVoice(.square, 880, 0.030, 0.13 * v, 0.0005, 0.029, dp))         // harder bell ping
        case .crash:
            out.append(makeVoice(.noise, 9500, rj(1.6, 0.15), 0.5 * v, 0.006, 1.5, dp))      // bigger wash
        case .splash:
            out.append(makeVoice(.noise, 10000, rj(0.30, 0.2), 0.40 * v, 0.003, 0.29, dp))   // brighter splash
        case .cowbell:
            out.append(makeVoice(.square, 1000, rj(0.22, 0.15), 0.30 * v, 0.001, 0.21, dp))  // clangier
        case .block:
            out.append(makeVoice(.noise, 6000, 0.0018, 0.42 * v, 0.0001, 0.0016, dp))        // harder knock
            out.append(makeVoice(.triangle, 3000, 0.040, 0.40 * v, 0.0003, 0.038, dp))
        case .tambo:
            out.append(makeVoice(.noise, 8200, rj(0.18, 0.2), 0.35 * v, 0.002, 0.17, dp))    // more jingle
        }
        return out
    }

    /// Key-up: damp the group's held voices (open-hat foot-pedal close) and
    /// fire its stored release burst (closed-hat lift click). No-op for the
    /// one-shot drums.
    func noteOff(_ group: UInt64) {
        guard group != 0 else { return }
        lock.lock()
        if let bursts = releaseStore.removeValue(forKey: group) {
            for s in bursts where pending.count < maxVoices { pending.append(s) }
        }
        pendingReleases.append(group)
        lock.unlock()
    }

    /// Drop all ringing voices (e.g. when the split toggles off).
    func silence() {
        lock.lock()
        pending.removeAll(keepingCapacity: true)
        releaseStore.removeAll(keepingCapacity: true)
        pendingReleases.removeAll(keepingCapacity: true)
        lock.unlock()
        // active is cleared lazily by the render thread; mark a flush.
        flushRequested = true
    }
    private var flushRequested = false

    /// Force a (possibly held) voice to a short finite one-shot — for cues
    /// that never get a matching release.
    private func finite(_ v: Voice) -> Voice {
        guard v.held else { return v }
        var c = v
        c.held = false
        c.duration = 0.25
        c.decay = 0.25
        return c
    }

    // MARK: Recipe → voices (port of percussion.mjs)

    private func rj(_ c: Double, _ f: Double) -> Double {
        c * (1 + (Double.random(in: 0..<1) - 0.5) * 2 * f)
    }
    private func rn(_ a: Double, _ b: Double) -> Double {
        a + Double.random(in: 0..<1) * (b - a)
    }

    private static let hatFreqs: [Double] = [800, 540, 522.7, 369.6]

    /// Build one layer. `decay == 0` (the sustain voices) fades across the
    /// whole duration so the tail never clicks off — the JS relies on the
    /// engine's default release for that, which we make explicit here.
    private func makeVoice(_ wave: Wave, _ tone: Double, _ duration: Double,
                           _ volume: Double, _ attack: Double, _ decay: Double,
                           _ pan: Double) -> Voice {
        let p = max(-1.0, min(1.0, pan))
        let theta = (p + 1) * 0.25 * .pi   // constant-power pan
        var voice = Voice(
            wave: wave, freq: max(1, tone), duration: max(0.0005, duration),
            volume: volume, attack: max(0, attack),
            decay: decay > 0 ? decay : max(0.0005, duration),
            gainL: Float(cos(theta)), gainR: Float(sin(theta)),
            seed: UInt32.random(in: 1...UInt32.max)
        )
        if wave == .noise { setupNoiseFilter(&voice) }
        return voice
    }

    private struct DrumVoices {
        var down: [Voice] = []      // fired on key-down
        var release: [Voice] = []   // fired on key-up (closed-hat lift click)
    }

    /// A held voice that rings until released. On release it fades over
    /// `releaseFade`; noise voices step their LPF cutoff to `toneTarget`
    /// (the open-hat pedal-close darkening) — mirrors percussion.mjs's
    /// addSustain releaseFade + releaseUpdate.tone.
    private func makeHeld(_ wave: Wave, _ tone: Double, _ vol: Double,
                          _ atk: Double, _ pan: Double, group: UInt64,
                          releaseFade: Double, toneTarget: Double) -> Voice {
        let p = max(-1.0, min(1.0, pan))
        let theta = (p + 1) * 0.25 * .pi
        var voice = Voice(
            wave: wave, freq: max(1, tone), duration: .infinity,
            volume: vol, attack: max(0, atk), decay: 0,
            gainL: Float(cos(theta)), gainR: Float(sin(theta)),
            seed: UInt32.random(in: 1...UInt32.max)
        )
        voice.group = group
        voice.held = true
        voice.releaseFade = max(0.01, releaseFade)
        voice.toneTarget = toneTarget
        if wave == .noise { setupNoiseFilter(&voice) }
        return voice
    }

    private func build(for drum: Drum, v: Double, basePan: Double,
                       group: UInt64) -> DrumVoices {
        var dv = DrumVoices()
        // addHit: finite one-shot. sustain(): finite one-shot for cymbals
        // that ring out regardless of key (ride/crash). heldSustain(): rings
        // until release, then damps (the hi-hat foot pedal). releaseHit():
        // a one-shot fired on key-up (closed-hat lift click).
        func hit(_ w: Wave, _ tone: Double, _ dur: Double, _ vol: Double,
                 _ atk: Double, _ dec: Double, _ pan: Double) {
            dv.down.append(makeVoice(w, tone, dur, vol, atk, dec, pan))
        }
        func sustain(_ w: Wave, _ tone: Double, _ both: Double, _ vol: Double,
                     _ atk: Double, _ pan: Double) {
            dv.down.append(makeVoice(w, tone, both, vol, atk, 0, pan))
        }
        func heldSustain(_ w: Wave, _ tone: Double, _ vol: Double, _ atk: Double,
                         _ pan: Double, releaseFade: Double, toneTarget: Double) {
            dv.down.append(makeHeld(w, tone, vol, atk, pan, group: group,
                                    releaseFade: releaseFade, toneTarget: toneTarget))
        }
        func releaseHit(_ w: Wave, _ tone: Double, _ dur: Double, _ vol: Double,
                        _ atk: Double, _ dec: Double, _ pan: Double) {
            dv.release.append(makeVoice(w, tone, dur, vol, atk, dec, pan))
        }

        switch drum {
        case .kick:
            let dp = basePan + rn(-0.02, 0.02)
            hit(.noise, 2500, 0.0025, rj(0.50, 0.12) * v, 0.0002, 0.0022, dp)
            hit(.sine, 200, 0.012, rj(1.1, 0.10) * v, 0.0005, 0.011, dp)
            hit(.sine, 150, 0.045, rj(1.3, 0.10) * v, 0.001, 0.044, dp)
            hit(.sine, 90, 0.080, rj(0.85, 0.12) * v, 0.002, 0.078, dp)
            hit(.sine, 55, rj(0.35, 0.20), rj(1.0, 0.12) * v, 0.003, 0.345, dp)
            // Key-up "beater off" — a soft low thump, quieter + shorter
            // than the hit so a held-then-released kick reads as a
            // damped close rather than a second strike.
            releaseHit(.sine, 70, 0.045, rj(0.40, 0.12) * v, 0.001, 0.044, dp)
            releaseHit(.noise, 1600, 0.003, rj(0.16, 0.18) * v, 0.0002, 0.0028, dp)
        case .snare:
            let dp = basePan + rn(-0.02, 0.02)
            hit(.noise, 3500, 0.004, rj(0.95, 0.10) * v, 0.0001, 0.004, dp)
            hit(.sine, 238, 0.030, rj(0.35, 0.12) * v, 0.0003, 0.029, dp)
            hit(.sine, 476, 0.030, rj(0.28, 0.12) * v, 0.0003, 0.029, dp)
            hit(.noise, 3500, rj(0.11, 0.20), rj(0.85, 0.10) * v, 0.0005, 0.108, dp + rn(-0.04, 0.04))
            hit(.noise, 1800, rj(0.07, 0.20), rj(0.38, 0.15) * v, 0.0008, 0.068, dp + rn(-0.04, 0.04))
            hit(.triangle, 180, 0.025, rj(0.22, 0.15) * v, 0.001, 0.024, dp)
            // Key-up — a short, dry snare-wire "tick": a brief noise
            // crack plus a faint body ring, lighter than the down hit.
            releaseHit(.noise, 4000, 0.006, rj(0.30, 0.15) * v, 0.0002, 0.0055, dp)
            releaseHit(.triangle, 200, 0.018, rj(0.14, 0.15) * v, 0.0006, 0.017, dp)
        case .clap:
            let dp = basePan + rn(-0.06, 0.02)
            hit(.noise, 1000, 0.025, rj(0.90, 0.15) * v, 0.005, 0.020, dp)
            hit(.noise, 1100, 0.035, rj(0.95, 0.15) * v, 0.015, 0.020, dp)
            hit(.noise, 900, 0.045, rj(0.85, 0.15) * v, 0.025, 0.020, dp)
            hit(.noise, 3000, 0.008, rj(0.55, 0.15) * v, 0.001, 0.007, dp)
            hit(.noise, 1000, rj(0.14, 0.25), rj(0.85, 0.15) * v, 0.045, 0.135, dp + rn(-0.02, 0.10))
            hit(.noise, 2200, rj(0.10, 0.25), rj(0.35, 0.18) * v, 0.050, 0.095, dp + rn(-0.02, 0.10))
        case .snap:
            let dp = basePan + rn(-0.04, 0.04)
            hit(.noise, 6000, 0.003, rj(0.70, 0.15) * v, 0.0001, 0.0028, dp)
            hit(.sine, 2100, rj(0.045, 0.20), rj(0.55, 0.12) * v, 0.0005, 0.044, dp)
            hit(.sine, 3500, rj(0.020, 0.25), rj(0.28, 0.18) * v, 0.0005, 0.019, dp)
        case .hatClosed:
            // Down: the 4-square cluster + bright noise chick. Up: the
            // foot-pedal "lift click" release burst (separate key-up sound).
            let dp = basePan + rn(-0.03, 0.03)
            for f in Self.hatFreqs {
                hit(.square, f, rj(0.008, 0.20), rj(0.18, 0.18) * v, 0.0005, 0.0075, dp)
            }
            hit(.noise, 8000, rj(0.040, 0.20), rj(0.38, 0.12) * v, 0.0005, 0.038, dp)
            releaseHit(.noise, 9000, 0.004, rj(0.22, 0.20) * v, 0.0002, 0.0038, dp)
            releaseHit(.square, 6000, 0.003, rj(0.08, 0.25) * v, 0.0003, 0.0027, dp)
        case .hatOpen:
            // Down: cluster + noise chick, then LONG held shimmer that rings
            // while the key is down. Up: the held voices damp fast (foot
            // pedal closes), the noise layers darkening as they fade.
            let dp = basePan + rn(-0.04, 0.04)
            for f in Self.hatFreqs {
                hit(.square, f, 0.012, rj(0.16, 0.18) * v, 0.0005, 0.011, dp)
            }
            hit(.noise, 8200, 0.012, rj(0.42, 0.12) * v, 0.0003, 0.011, dp)
            heldSustain(.noise, 7000, rj(0.32, 0.15) * v, 0.003, dp + rn(-0.02, 0.08),
                        releaseFade: 0.12, toneTarget: 3500)
            heldSustain(.noise, 5000, rj(0.20, 0.18) * v, 0.003, dp + rn(-0.02, 0.08),
                        releaseFade: 0.10, toneTarget: 2800)
            heldSustain(.square, 800, rj(0.08, 0.20) * v, 0.005, dp,
                        releaseFade: 0.08, toneTarget: 0)
        case .ride:
            let dp = basePan + rn(-0.03, 0.03)
            hit(.square, 800, 0.020, rj(0.10, 0.18) * v, 0.0005, 0.019, dp)
            hit(.square, 540, 0.020, rj(0.08, 0.18) * v, 0.0005, 0.019, dp)
            sustain(.sine, 440, rj(0.40, 0.20), rj(0.24, 0.12) * v, 0.0008, dp)
            sustain(.sine, 587, rj(0.40, 0.20), rj(0.20, 0.12) * v, 0.0008, dp)
            sustain(.noise, 4200, rj(0.9, 0.20), rj(0.26, 0.12) * v, 0.005, dp + rn(-0.03, 0.03))
        case .crash:
            let dp = basePan + rn(-0.05, 0.05)
            hit(.noise, 8000, 0.030, rj(0.75, 0.15) * v, 0.0005, 0.029, dp)
            for f in Self.hatFreqs {
                hit(.square, f, 0.030, rj(0.12, 0.20) * v, 0.0005, 0.029, dp)
            }
            sustain(.noise, 5000, rj(1.4, 0.18), rj(0.45, 0.12) * v, 0.008, dp + rn(-0.04, 0.04))
            sustain(.noise, 7500, rj(0.9, 0.18), rj(0.30, 0.15) * v, 0.008, dp + rn(-0.04, 0.04))
            sustain(.square, 800, rj(0.5, 0.20), rj(0.08, 0.20) * v, 0.008, dp)
        case .splash:
            let dp = basePan + rn(-0.04, 0.04)
            hit(.noise, 9000, 0.012, rj(0.55, 0.15) * v, 0.0003, 0.011, dp)
            hit(.square, 800, 0.015, rj(0.14, 0.20) * v, 0.0005, 0.014, dp)
            hit(.square, 540, 0.015, rj(0.10, 0.20) * v, 0.0005, 0.014, dp)
            hit(.noise, 6000, rj(0.35, 0.20), rj(0.42, 0.12) * v, 0.004, 0.345, dp + rn(-0.03, 0.03))
            hit(.noise, 8500, rj(0.22, 0.20), rj(0.25, 0.15) * v, 0.004, 0.215, dp + rn(-0.03, 0.03))
        case .cowbell:
            let dp = basePan + rn(-0.03, 0.03)
            hit(.square, 1800, 0.004, rj(0.35, 0.15) * v, 0.0002, 0.0038, dp)
            hit(.triangle, 800, rj(0.28, 0.20), rj(0.42, 0.12) * v, 0.0008, 0.275, dp)
            hit(.triangle, 540, rj(0.28, 0.20), rj(0.36, 0.12) * v, 0.0008, 0.275, dp)
            // Key-up — a short muted "clack" an octave-ish up, like a
            // hand damping the bell: brief, bright, and quiet.
            releaseHit(.square, 2400, 0.006, rj(0.16, 0.15) * v, 0.0003, 0.0055, dp)
            releaseHit(.triangle, 1080, 0.020, rj(0.12, 0.15) * v, 0.0006, 0.019, dp)
        case .block:
            let dp = basePan + rn(-0.03, 0.03)
            hit(.noise, 5000, 0.002, rj(0.35, 0.18) * v, 0.0001, 0.0018, dp)
            hit(.triangle, 2500, rj(0.050, 0.25), rj(0.52, 0.12) * v, 0.0003, 0.048, dp)
            hit(.triangle, 1250, rj(0.050, 0.25), rj(0.18, 0.18) * v, 0.0005, 0.048, dp)
        case .tambo:
            let dp = basePan + rn(-0.04, 0.04)
            hit(.noise, 7000, 0.08, rj(0.38, 0.18) * v, 0.002, 0.075, dp)
            hit(.noise, 7500, 0.09, rj(0.30, 0.18) * v, 0.015, 0.075, dp)
            hit(.noise, 6500, 0.10, rj(0.25, 0.18) * v, 0.030, 0.070, dp)
            hit(.square, 6000, 0.030, rj(0.14, 0.20) * v, 0.001, 0.028, dp)
            hit(.noise, 7000, rj(0.20, 0.22), rj(0.32, 0.18) * v, 0.050, 0.195, dp + rn(-0.04, 0.04))
            hit(.noise, 4500, rj(0.15, 0.22), rj(0.20, 0.18) * v, 0.055, 0.145, dp + rn(-0.04, 0.04))
        }
        return dv
    }

    // MARK: Noise biquad (matches audio.c setup_noise_filter, Q = 1)

    private func setupNoiseFilter(_ v: inout Voice) {
        let cutoff = min(v.freq, sampleRate * 0.45)
        let q = 1.0
        let w0 = 2.0 * .pi * cutoff / sampleRate
        let alpha = sin(w0) / (2.0 * q)
        let cosw = cos(w0)
        let b = (1.0 - cosw) / 2.0
        let norm = 1.0 + alpha
        v.nb0 = b / norm
        v.nb1 = (1.0 - cosw) / norm
        v.nb2 = b / norm
        v.na1 = (-2.0 * cosw) / norm
        v.na2 = (1.0 - alpha) / norm
    }

    @inline(__always)
    private func xorshift(_ s: inout UInt32) -> UInt32 {
        s ^= s << 13; s ^= s >> 17; s ^= s << 5
        return s
    }

    // MARK: Envelope (matches audio.c compute_envelope)

    @inline(__always)
    private func envelope(_ v: Voice, at t: Double) -> Double {
        var env = 1.0
        if v.attack > 0, t < v.attack {
            env = t / v.attack
        }
        if v.decay > 0 {
            let decayStart = max(0, v.duration - v.decay)
            if t > decayStart {
                var prog = (t - decayStart) / v.decay
                if prog > 1 { prog = 1 }
                env *= (1 - prog)
            }
        }
        return env
    }

    // MARK: Render thread

    private func render(frameCount: Int, abl: UnsafeMutablePointer<AudioBufferList>) {
        let buffers = UnsafeMutableAudioBufferListPointer(abl)
        let left = buffers[0].mData!.assumingMemoryBound(to: Float.self)
        let right = (buffers.count > 1 ? buffers[1].mData! : buffers[0].mData!)
            .assumingMemoryBound(to: Float.self)
        for i in 0..<frameCount { left[i] = 0; right[i] = 0 }

        let now = CACurrentMediaTime()
        // Snapshot the kit-wide pitch shift once per block so every voice
        // in this buffer warps together with the trackpad pitch-bend.
        let pitch = pitchScale
        // Drain staged voices, releases, and honor a flush request.
        lock.lock()
        if flushRequested { active.removeAll(keepingCapacity: true); flushRequested = false }
        if !pending.isEmpty {
            for p in pending where active.count < maxVoices { active.append(p) }
            pending.removeAll(keepingCapacity: true)
            // Trigger→render handoff: how long staged voices waited for this
            // render cycle (≤ one buffer) — the percussion latency probe.
            if pendingStageTime > 0 { lastHandoffSec = max(0, now - pendingStageTime) }
        }
        var releases: Set<UInt64> = []
        if !pendingReleases.isEmpty {
            releases = Set(pendingReleases)
            pendingReleases.removeAll(keepingCapacity: true)
        }
        lock.unlock()

        // Flip newly-released held voices into their fade-out. Noise voices
        // step their LPF cutoff to the darker release target (pedal close).
        if !releases.isEmpty {
            for idx in 0..<active.count where
                active[idx].held && !active[idx].releasing
                && releases.contains(active[idx].group) {
                active[idx].releasing = true
                active[idx].releaseElapsed = 0
                active[idx].releaseFromFreq = active[idx].freq
                if active[idx].toneTarget > 0, active[idx].wave == .noise {
                    active[idx].freq = active[idx].toneTarget
                    setupNoiseFilter(&active[idx])
                }
            }
        }
        if active.isEmpty { return }

        let dt = 1.0 / sampleRate
        let g = Double(masterGain)
        var w = 0
        for idx in 0..<active.count {
            var v = active[idx]
            var alive = true
            for i in 0..<frameCount {
                // End conditions: a releasing voice dies when its fade
                // completes; a finite voice at its duration; a held voice
                // (not yet released) never ends here — it rings on.
                if v.releasing {
                    if v.releaseElapsed >= v.releaseFade { alive = false; break }
                } else if !v.held {
                    if v.elapsed >= v.duration { alive = false; break }
                }
                let env = envelope(v, at: v.elapsed)
                let relGain = v.releasing
                    ? max(0.0, 1.0 - v.releaseElapsed / v.releaseFade) : 1.0
                var s: Double
                switch v.wave {
                case .sine:
                    s = sin(2.0 * .pi * v.phase)
                case .triangle:
                    // Wrap the +0.25 phase offset back into [0,1) — without
                    // the wrap the last quarter ramps to +2.0, adding bright
                    // harmonics that alias at 48k and read as a higher,
                    // harsher pitch than AC-native (block / o key).
                    let tp = (v.phase + 0.25).truncatingRemainder(dividingBy: 1.0)
                    s = 4.0 * abs(tp - 0.5) - 1.0
                case .square:
                    s = v.phase < 0.5 ? 1.0 : -1.0
                case .noise:
                    let r = Double(xorshift(&v.seed)) / Double(UInt32.max)
                    let white = r * 2.0 - 1.0
                    let y = v.nb0 * white + v.nb1 * v.nx1 + v.nb2 * v.nx2
                          - v.na1 * v.ny1 - v.na2 * v.ny2
                    v.nx2 = v.nx1; v.nx1 = white
                    v.ny2 = v.ny1; v.ny1 = y
                    s = y
                }
                let amp = s * env * relGain * v.volume * g
                left[i] += Float(amp * Double(v.gainL))
                right[i] += Float(amp * Double(v.gainR))
                // Kit-wide pitch bend scales the phase increment so
                // tonal drums (kick/toms/blocks/cowbell) bend with the
                // melodic voices. Noise voices use `freq` as an LPF
                // cutoff, not a pitch, so they're left unscaled.
                v.phase += v.freq * (v.wave == .noise ? 1.0 : pitch) * dt
                if v.phase >= 1 { v.phase -= floor(v.phase) }
                v.elapsed += dt
                if v.releasing { v.releaseElapsed += dt }
            }
            if alive {
                active[w] = v
                w += 1
            }
        }
        if w < active.count { active.removeLast(active.count - w) }
    }
}
