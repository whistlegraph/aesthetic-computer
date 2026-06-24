import AVFoundation
import CGMSynth
import Foundation
import QuartzCore

/// Real-time polyphonic GM synthesizer driven by the Aesthetic Computer
/// native synthesis core (`fedac/native/src/gm_synth.c`, imported as
/// `CGMSynth`). Lets Menu Band audition the *real* AC OS melodic voices
/// — algorithmic modal piano, FM electric piano, Karplus-Strong plucks,
/// modal banks, subtractive synth bass — locally, without booting AC OS,
/// exactly the way `MenuBandPercussion` auditions the native drum kit.
///
/// Modeled directly on `MenuBandPercussion`: an `AVAudioSourceNode`
/// whose render callback owns a fixed, preallocated voice pool (no
/// allocation on the render thread). The control thread stages note-on /
/// note-off intent through a tiny critical section; the render thread
/// drains it at the top of each block.
///
/// Each `Voice` wraps one C `GMVoice` (which holds ALL per-voice GM DSP
/// state) plus a Swift-side amplitude envelope. `gm_voice_render` takes
/// the envelope value as its `env` argument and owns everything else, so
/// this file only has to: pick a voice, init the C struct on note-on,
/// advance an attack/sustain/release envelope, and sum the rendered
/// samples. `gm_voice_init` is fed a per-trigger seed counter so the same
/// note never renders identically (the native stochastic behaviour).
final class MenuBandGMSynth {
    // MARK: Voice state (render thread owns the active pool)

    /// One sounding GM note. The heavy C synthesis state (`GMVoice`, ~80 KB of
    /// inlined delay/chorus/bore buffers) lives OUT of this struct in the
    /// `cores` buffer below — keyed by the same slot index — so a `Voice` stays
    /// tiny. Embedding `GMVoice` here made `render()` materialize an ~80 KB
    /// stack temporary in unoptimized (Debug) builds when forming
    /// `&voices[idx].core`, and its prologue stack-probe then overran the small
    /// CoreAudio render-thread stack (EXC_BAD_ACCESS on the first callback).
    /// Keeping the C state in a separately-allocated buffer makes the pointer
    /// pure address arithmetic — no copy in any build — and removes the Swift
    /// exclusivity hazard of pointing into the same array we also read.
    private struct Voice {
        var midi: UInt8 = 0
        var channel: UInt8 = 0
        var freq: Double = 440         // base note frequency (Hz)
        var velocityGain: Double = 1   // linear, from MIDI velocity
        var gainL: Float = 0.707
        var gainR: Float = 0.707
        // Amplitude envelope (attack → held sustain → release). gm_synth's
        // voices bake their own internal decay into the timbre, so this is
        // a gentle outer AR contour: a short click-free attack, a flat hold
        // while the key is down, and a release fade on key-up.
        var attack: Double = 0.004     // seconds to reach full level
        var release: Double = 0.18     // seconds to fade after key-up
        var env: Double = 0            // current envelope level (0…1)
        var releasing: Bool = false
        var active: Bool = false       // slot occupied
    }

    // MARK: Audio graph

    /// Render sample rate — set to the engine's output rate at attach so
    /// synthesis runs at the hardware clock with no resample (matches the
    /// percussion node). gm_synth's phase increments use this rate.
    private var sampleRate: Double = 48_000
    private var format: AVAudioFormat!
    private var sourceNode: AVAudioSourceNode!
    private weak var engine: AVAudioEngine?
    private var attached = false

    /// Master headroom so a fistful of simultaneous notes doesn't slam the
    /// downstream limiter — gm_synth voices are normalized near unity each.
    private let masterGain: Double = 0.5

    /// Fixed polyphony. Preallocated so the render thread never allocates.
    private let maxVoices = 24
    private var voices: [Voice]

    /// Per-voice C synthesis state, slot-aligned with `voices`. Heap-allocated
    /// once (never on the render thread) and addressed by `cores + slot`, so
    /// the render thread forms its `GMVoice*` with no stack copy — see the
    /// `Voice` note above. `gm_voice_init` memsets the whole struct on note-on,
    /// so a slot's contents before its first note are irrelevant.
    private let cores: UnsafeMutablePointer<GMVoice>

    /// The current melodic GM program (0-based). Set by the coordinator via
    /// `setProgram`; read on note-on to init the matching C voice. Plain
    /// Int load/store is atomic enough here (a half-stale read at worst
    /// picks the previous program for a single note).
    private var program: Int32 = 0

    /// Per-trigger seed counter so each note-on draws a fresh stochastic
    /// stream (`gm_voice_init` seeds its xorshift from this). Bumped on the
    /// control thread.
    private var seedCounter: UInt32 = 0x9E3779B9

    /// Global pitch multiplier TARGET from the trackpad bend (1.0 = none,
    /// 2.0 = +1 octave). Written main-thread, read render-thread; a plain
    /// Double is atomic enough (worst case one block reads a half-stale
    /// value).
    private var pitchScale: Double = 1.0

    /// Render-thread-owned smoothed pitch that glides toward `pitchScale`.
    /// The trackpad hands us a NEW target on every mouse-moved tick, so
    /// applying it as a hard per-block step makes the native voices'
    /// pitch-derived state jump — a Karplus-Strong delay line resizes,
    /// modal banks re-tune — and that discontinuity reads as a skip/pop
    /// while sliding. Easing per-sample toward the target keeps the bend
    /// continuous so the slide is smooth. ~4 ms time constant: fast enough
    /// to feel immediate, slow enough to declick.
    private var glidePitch: Double = 1.0
    /// Per-sample glide coefficient, set from the real sample rate in
    /// `attach` (before the render thread runs) so render never has to
    /// derive it. ~4 ms one-pole time constant.
    private var pitchGlideCoeff: Double = 1.0 - exp(-1.0 / (48_000 * 0.004))

    // MARK: Control → render handoff

    private enum Command {
        case noteOn(midi: UInt8, channel: UInt8, velocity: UInt8, program: Int32, seed: UInt32)
        case noteOff(midi: UInt8, channel: UInt8)
        case panic
    }
    private var pending: [Command] = []
    private let lock = NSLock()

    init() {
        voices = [Voice](repeating: Voice(), count: maxVoices)
        cores = UnsafeMutablePointer<GMVoice>.allocate(capacity: maxVoices)
        cores.initialize(repeating: GMVoice(), count: maxVoices)
        pending.reserveCapacity(64)
        // Build the shared wavetable once, up front, so the first note-on
        // doesn't pay for it on the audio path. Idempotent.
        gm_synth_init()
    }

    deinit {
        cores.deinitialize(count: maxVoices)
        cores.deallocate()
    }

    /// Attach into the synth graph exactly like the percussion node, at the
    /// engine's output rate so there's no resample. `output` is the DRY
    /// pre-limiter / post-fx bus the coordinator chooses.
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

    /// 1 if the GM core has a bespoke algorithmic voice for `program`
    /// (0-based). The coordinator uses this to decide whether to route a
    /// note here or fall back to MIDISynth.
    static func programImplemented(_ program: UInt8) -> Bool {
        gm_program_implemented(Int32(program)) == 1
    }

    /// Inform the node of the current melodic program. Future note-ons use
    /// it; sounding notes keep their original voice.
    func setProgram(_ program: UInt8) {
        self.program = Int32(program)
    }

    /// Kit-wide pitch shift from a bend amount in the melodic voices' units
    /// (±1 == ±12 semitones). Applied live to sounding + future notes.
    func setPitchBend(amount: Float) {
        pitchScale = pow(2.0, Double(amount))
    }

    /// Stage a note-on for `program`. Returns false if the program has no
    /// GM voice (so the caller can fall back to MIDISynth); true if it was
    /// accepted for the GM path.
    @discardableResult
    func noteOn(_ midi: UInt8, velocity: UInt8, channel: UInt8, program: UInt8) -> Bool {
        guard gm_program_implemented(Int32(program)) == 1 else { return false }
        lock.lock()
        seedCounter = seedCounter &* 1_664_525 &+ 1_013_904_223
        let seed = seedCounter
        pending.append(.noteOn(midi: midi, channel: channel, velocity: velocity,
                               program: Int32(program), seed: seed))
        lock.unlock()
        return true
    }

    /// Stage a note-off: the matching sounding voice enters its release.
    func noteOff(_ midi: UInt8, channel: UInt8) {
        lock.lock()
        pending.append(.noteOff(midi: midi, channel: channel))
        lock.unlock()
    }

    /// Drop everything (e.g. backend switch / panic).
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

    /// Pick a free slot, or steal the oldest releasing/quietest voice.
    /// Render-thread only — never allocates.
    private func allocateVoice() -> Int {
        for i in 0..<maxVoices where !voices[i].active { return i }
        // No free slot: prefer stealing a releasing voice, else the
        // quietest, so a held note isn't cut for a transient tail.
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

        // Drain staged commands into the voice pool.
        lock.lock()
        let cmds = pending
        if !pending.isEmpty { pending.removeAll(keepingCapacity: true) }
        lock.unlock()

        for cmd in cmds {
            switch cmd {
            case let .noteOn(midi, channel, velocity, prog, seed):
                let slot = allocateVoice()
                let f = MenuBandGMSynth.freq(forMIDI: midi)
                // Init the C voice IN PLACE in the slot's `cores` entry. The
                // ~80 KB GMVoice (inlined ks_buf/fx_delay/bore_buf/ss_chorus_buf)
                // lives in a separately-allocated buffer, so `cores + slot` is a
                // plain pointer — no stack temporary, even in unoptimized Debug
                // builds (which is what blew the small render-thread stack when
                // the state was embedded in the `voices` array). gm_voice_init
                // memsets the whole struct.
                let ok = gm_voice_init(cores + slot, prog, f, sampleRate, seed)
                // -1 means unimplemented; the control thread already gated
                // on gm_program_implemented, but stay safe and drop it.
                if ok == 0 {
                    voices[slot].midi = midi
                    voices[slot].channel = channel
                    voices[slot].freq = f
                    voices[slot].velocityGain = max(0.05, min(1.0, Double(velocity) / 127.0))
                    // Constant-power center pan (the GM voices are mono).
                    voices[slot].gainL = 0.707
                    voices[slot].gainR = 0.707
                    voices[slot].env = 0
                    voices[slot].releasing = false
                    voices[slot].active = true
                } else {
                    // Unimplemented program: make sure a stolen slot is freed.
                    voices[slot].active = false
                }
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
        // Smooth the trackpad bend into a per-sample glide so frequency
        // never steps between blocks. Each voice rides the SAME trajectory,
        // so we replay the identical recurrence (same start, target, coeff)
        // per voice and commit the final value once at the end.
        let pitchStart = glidePitch
        let pitchTarget = pitchScale
        let pitchCoeff = pitchGlideCoeff

        for idx in 0..<maxVoices where voices[idx].active {
            // The C synthesis state lives in `cores`, not in `voices`, so the
            // render pointer is plain address arithmetic with no stack copy and
            // no overlap with our `voices` reads. Per-voice fields are still
            // hoisted into locals so the hot inner loop avoids repeated array
            // bounds checks; the mutated envelope state is written back after.
            let ptr = cores + idx
            let baseFreq = voices[idx].freq
            let vGain = voices[idx].velocityGain
            let gainL = Double(voices[idx].gainL)
            let gainR = Double(voices[idx].gainR)
            let attackInc = voices[idx].attack > 0 ? dt / voices[idx].attack : 1.0
            let releaseDec = voices[idx].release > 0 ? dt / voices[idx].release : 1.0
            let releasing = voices[idx].releasing
            var env = voices[idx].env
            var active = true
            var pitch = pitchStart
            for i in 0..<frameCount {
                pitch += (pitchTarget - pitch) * pitchCoeff
                let f = baseFreq * pitch
                // Advance the outer AR envelope.
                if releasing {
                    env -= releaseDec
                    if env <= 0 { env = 0; active = false; break }
                } else if env < 1.0 {
                    env += attackInc
                    if env > 1.0 { env = 1.0 }
                }
                let s = gm_voice_render(ptr, sampleRate, env, f)
                // Belt-and-suspenders against a divergent C voice: a NaN/Inf
                // sample summed into the mix poisons the whole buffer and
                // takes down the downstream limiter/engine. The C core now
                // traps this at the source, but never trust a render-thread
                // value blindly — kill the voice and stop mixing it.
                if !s.isFinite { active = false; break }
                let amp = s * vGain * g
                left[i] += Float(amp * gainL)
                right[i] += Float(amp * gainR)
            }
            voices[idx].env = env
            voices[idx].active = active
        }

        // Advance the shared glide once for this block (covers the
        // no-voices-active case too, so the bend is current when the next
        // note starts). Mirrors the per-sample recurrence above exactly.
        var p = pitchStart
        for _ in 0..<frameCount { p += (pitchTarget - p) * pitchCoeff }
        glidePitch = p
    }
}
