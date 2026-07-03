import Foundation
import AVFoundation
import AudioToolbox
import CoreAudio

/// Built-in soft-synth using Apple's bundled GM DLS sound bank
/// (`gs_instruments.dls`).
///
/// Architecture: two backends, one chosen at startup.
///
/// 1. **Multi-timbral (fast)** — Apple's MIDISynth audio unit
///    (`kAudioUnitSubType_MIDISynth`) with the GS DLS bank loaded once
///    and every program preloaded via `kAUMIDISynthProperty_EnablePreload`.
///    Program switches are instant MIDI Program Change messages — no disk
///    I/O, no DSP rebuild — so dragging across the instrument grid sounds
///    each cell with zero swap latency.
///
/// 2. **Single-timbral (fallback)** — `AVAudioUnitSampler` with the same
///    bank loaded one program at a time. Program switches re-load the
///    bank (~100 ms blocking + AU silences scheduled notes during the
///    swap). Used only if the MIDISynth path fails to instantiate or
///    produce audio.
///
/// MIDISynth instantiation is asynchronous (Apple's AU framework needs a
/// callback round-trip before the AU is usable). Until it's ready,
/// `noteOn`/`setMelodicProgram` route to the sampler so the user always
/// hears something. Once MIDISynth is up and primed, subsequent calls
/// flip over silently — no audible glitch.
final class MenuBandSynth {
    private let engine = AVAudioEngine()

    /// Preferred backend, asynchronously created. Nil until ready.
    private var midiSynth: AVAudioUnit?
    /// Fallback backend, always available immediately.
    private let melodic = AVAudioUnitSampler()
    private let drums = AVAudioUnitSampler()
    /// Live KPBJ.FM radio backend — pads play the live stream pitched by
    /// 2^((note-60)/12), with stalls fading into AM-style static driven
    /// by real-time NIC byte counters. Attached to the engine on
    /// `start()`; AVPlayer only spins up while `usingRadioBackend` is on.
    private let radio = KPBJRadioStream()
    /// Microphone-sampled "voice": user holds backtick to record a clip,
    /// then plays it back as a duration-preserving pitch-shifted piano
    /// voice (TimePitch). Same
    /// melodic-only routing semantics as the radio backend (channel 9
    /// drums always pass through to GM).
    private let sampleVoice = MenuBandSampleVoice()
    /// Speaks a language's own name (About-window easter egg) through the
    /// same pre-limiter fx bus, so the spoken voice picks up bend/space/echo.
    private let speechVoice = MenuBandSpeechVoice()
    /// Right-hand percussion split — the AC-native 12-drum kit, synthesized
    /// live. Always attached; only sounds when the controller fires hits.
    let percussion = MenuBandPercussion()
    /// Aesthetic Computer GM voices, synthesized live by the native
    /// `gm_synth` core (CGMSynth). When `useACMIDI` is on AND the current
    /// melodic program has a bespoke gm_synth voice, melodic notes route
    /// here instead of MIDISynth — letting us audition the real AC OS
    /// instruments locally, just like the percussion kit. Programs gm_synth
    /// doesn't implement fall back to MIDISynth. Always attached; silent
    /// until a note is routed to it.
    let gmSynth = MenuBandGMSynth()
    /// Spacebar reverse-replay. Continuously records the post-FX master into
    /// a rolling ring (fed from the same `mainMixerNode` tap the visualizer
    /// + tape use — see `ingestWaveformBuffer`), and on `playReverse()` plays
    /// the most-recent few seconds backwards. Plays DRY straight into
    /// mainMixerNode; capture self-gates during playback so the reversed
    /// audio isn't recaptured (notepat's `setCapturePaused` behaviour).
    private let rewindVoice = MenuBandRewindVoice()
    /// Sums every backend before the limiter so simultaneous voices share one
    /// gain stage. Without this, each backend would feed `mainMixerNode`
    /// directly and a chord across melodic + drums + midiSynth could exceed
    /// 0 dBFS at the output (audible clipping/crackle).
    private let preLimiterMixer = AVAudioMixerNode()
    /// Sums the fx-processed melodic bus with the DRY percussion bus right
    /// before the compressor. Percussion routes here instead of through
    /// echo/reverb/proximity, so the trackpad fx (and pitch-bend) never
    /// touch the drums — they still get the master compressor + limiter and
    /// are captured by the tape (which taps `mainMixerNode`, downstream).
    private let postFxMixer = AVAudioMixerNode()
    /// Apple PeakLimiter on the master path. Catches transient peaks from
    /// chords or stacked sustains and holds output below 0 dBFS regardless
    /// of how many notes are pressed simultaneously. Parameters tuned for
    /// transparency on instrument samples (fast attack, gentle release).
    private let limiter: AVAudioUnitEffect = {
        var desc = AudioComponentDescription(
            componentType: kAudioUnitType_Effect,
            componentSubType: kAudioUnitSubType_PeakLimiter,
            componentManufacturer: kAudioUnitManufacturer_Apple,
            componentFlags: 0,
            componentFlagsMask: 0)
        return AVAudioUnitEffect(audioComponentDescription: desc)
    }()
    /// "Glued master" bus compressor (Apple AUDynamicsProcessor) on the
    /// master path, sitting AFTER the reverb and BEFORE the PeakLimiter.
    /// Its job is loudness normalization: it evens out level across quiet
    /// single notes and loud chords, then makeup-gains the whole bus up so
    /// playing always lands near 0 dBFS ("max" within the app's own
    /// headroom — it never touches the macOS system volume). The PeakLimiter
    /// downstream stays the inviolable brick wall that catches whatever the
    /// compressor's makeup gain pushes past 0 dBFS, so this can drive hard
    /// without ever clipping. Tuned for a glued-but-musical feel (soft knee,
    /// transient-friendly attack, slow release to avoid pumping).
    private let compressor: AVAudioUnitEffect = {
        var desc = AudioComponentDescription(
            componentType: kAudioUnitType_Effect,
            componentSubType: kAudioUnitSubType_DynamicsProcessor,
            componentManufacturer: kAudioUnitManufacturer_Apple,
            componentFlags: 0,
            componentFlagsMask: 0)
        return AVAudioUnitEffect(audioComponentDescription: desc)
    }()
    /// Global "space" reverb on the master path — sits between the
    /// pre-limiter sum and the limiter so EVERY backend (MIDISynth,
    /// sampler, plugin AU, sample voice, radio) gets the exact same
    /// ambience regardless of the selected instrument. `wetDryMix`
    /// at 0 is fully dry: with the trackpad X-axis parked left the
    /// sound is identical to before this node existed. Sliding the
    /// gesture right opens the room up; left brings it back up
    /// front to the listener.
    private let spaceReverb: AVAudioUnitReverb = {
        let r = AVAudioUnitReverb()
        r.loadFactoryPreset(.largeHall2)
        r.wetDryMix = 0
        return r
    }()
    /// "Proximity" filter on the master path — the LEFT half of the X-axis
    /// gesture. Reverb (right idea, wrong direction) pushes a sound big and
    /// far; this does the opposite: as the gesture pulls left it narrows the
    /// band (lifts the low cutoff, drops the high cutoff) so the sound
    /// shrinks and pulls in CLOSE — a tiny pocket-radio / cupped-hands
    /// timbre. Flat + bypassed at center so parked-left is identical to dry.
    private let proximityEQ: AVAudioUnitEQ = {
        let eq = AVAudioUnitEQ(numberOfBands: 2)
        let hp = eq.bands[0]
        hp.filterType = .highPass
        hp.frequency = 40
        hp.bypass = true
        let lp = eq.bands[1]
        lp.filterType = .lowPass
        lp.frequency = 18_000
        lp.bypass = true
        eq.globalGain = 0
        return eq
    }()
    /// Global tape-style echo on the master path, sitting just BEFORE
    /// the reverb so its repeats wash into the room (not the other way
    /// round) and pre-master so the volume slider scales the echo tail
    /// with everything else. `wetDryMix` at 0 is fully dry — identical
    /// to before this node existed. The ⌥Option + horizontal trackpad
    /// gesture opens it up; `setEcho` drives wet + feedback together so
    /// one axis = "amount of echo".
    private let echo: AVAudioUnitDelay = {
        let d = AVAudioUnitDelay()
        d.delayTime = 0.33       // slap ≈ eighth-note at ~110 BPM
        d.feedback = 0           // ramps up with amount in setEcho
        d.lowPassCutoff = 4_000  // darken repeats so they sit under the dry
        d.wetDryMix = 0          // fully dry until the gesture opens it
        return d
    }()
    /// Third-party AU instrument hosted via the Plugins picker. When non-nil
    /// and `usingPluginInstrument` is true, melodic notes route through this
    /// AU instead of MIDISynth/sampler. Drums (channel 9) still go to GM —
    /// the picker is intentionally a melodic-only override.
    private var pluginUnit: AVAudioUnit?
    private var pluginConnected = false
    private(set) var usingPluginInstrument: Bool = false
    private var started = false
    private var melodicConnected = false
    private var drumsConnected = false
    private var midiSynthConnected = false
    private var limiterConnected = false
    private var waveformCaptureEnabled = false
    private var activeNotes: Set<UInt16> = []
    /// Note keys currently sounding on the AC `gmSynth` node (vs. MIDISynth).
    /// Recorded on note-on so `noteOff` releases the GM voice even if the
    /// melodic program changed or the toggle flipped between press and
    /// release — without this a held GM note could hang or a MIDISynth
    /// note-off could be misrouted.
    private var gmRoutedNotes: Set<UInt16> = []
    private var idleSuspendWorkItem: DispatchWorkItem?
    /// 60 s instead of 2 s so a short pause between phrases never trips
    /// `engine.pause()`. The pause itself is cheap, but `engine.start()`
    /// to bring the graph back online costs ~50–100 ms and that cost
    /// lands on the FIRST note after the pause — directly observable
    /// as the "sometimes the piano is slow on the first hit" feeling.
    /// With 60 s, normal playing keeps the engine continuously warm so
    /// every keystroke pays only the realtime buffer latency. The power
    /// saving still kicks in if the user walks away from the keyboard.
    private let idleSuspendDelay: TimeInterval = 60.0
    /// Audio I/O frames the device renders per cycle — the render-thread
    /// deadline budget. macOS uses `max(all clients)` in shared mode, so on a
    /// real system this request is observed but typically ignored (something
    /// else holds the device at 512). When Menu Band IS the sole client the
    /// device drops to this size.
    ///
    /// 512 @ 96 kHz ≈ 5.3 ms — a deliberately conservative budget. The full
    /// synth stack (GM C core + DLS MIDISynth + per-note sample voices +
    /// percussion + fx + limiter) must finish WITHIN this window every cycle
    /// or CoreAudio underruns and you hear a click/pop, in any playback. On a
    /// memory-constrained / few-core machine an ultra-low buffer (we used to
    /// dive to the device minimum, 32 frames / 0.33 ms) can't survive normal
    /// scheduling jitter, so it popped constantly. 5.3 ms is still well below
    /// the perceptual threshold for a keyboard instrument; bump it back down
    /// only on a machine with headroom to spare.
    private static let targetIOBufferFrames: UInt32 = 512
    /// True once we've successfully taken hog mode on the active
    /// output device. Tracks the device ID alongside so we can
    /// release the right one on shutdown / device switch.
    private var hogModeDeviceID: AudioDeviceID = 0
    private var hogModeAcquired = false

    /// When true the idle-suspend never pauses the engine — kept warm so
    /// the next note has zero engine-resume latency. Armed while the
    /// percussion split is on (drumming is bursty and silent between hits,
    /// so otherwise the engine would pause and the first hit would lag).
    var keepEngineWarm = false {
        didSet {
            guard keepEngineWarm, started else { return }
            _ = resumeAudioEngineIfNeeded()  // warm it immediately on arm
        }
    }
    /// True once MIDISynth has loaded its bank, preloaded all programs,
    /// and successfully attached to the engine. `noteOn` and
    /// `setMelodicProgram` route through the MIDISynth when this flips.
    private(set) var midiSynthReady = false
    /// Set whenever an audio-engine configuration change (device switch /
    /// aux pull) fires — the MIDISynth can silently reset to its default
    /// sine preset across the reconfig, and the reset may land AFTER our
    /// immediate recovery runs. We therefore also re-assert the soundbank
    /// LAZILY before the next note, which is guaranteed to be after the
    /// reconfig has fully settled. This is the self-healing path.
    private var midiSynthBankDirty = false

    /// Monotonic counter bumped on every `AVAudioEngineConfigurationChange`
    /// (device switch / aux pull / rate change). The MIDISynth rebuild is
    /// debounced against this: the heavy tear-down + sample-preload only runs
    /// once the epoch has stopped advancing for a full settle window, so the
    /// preload sweep never lands mid-reconfiguration (see
    /// `scheduleMIDISynthRebuild`).
    private var audioConfigEpoch = 0
    /// The epoch the currently-armed rebuild timer was scheduled against, or
    /// nil when no rebuild is pending. Used to coalesce a burst of switches
    /// into one rebuild and to re-arm if a newer switch lands mid-wait.
    private var rebuildArmedForEpoch: Int?
    /// How long the audio device must stay quiet (no new config change) before
    /// the MIDISynth is rebuilt. Long enough for CoreAudio's IO thread to
    /// finish negotiating the new device/rate, short enough to feel instant.
    private let midiSynthRebuildSettleDelay: TimeInterval = 0.30

    /// Serializes engine run-state (`start`/`pause`/`stop`) and graph
    /// mutations (`attach`/`connect`/`disconnect`) plus the one-shot
    /// MIDISynth preload sweep. These run from TWO threads: the main
    /// thread (synth `start`, the async `configureMIDISynth`, popover
    /// actions) and the dedicated `MenuBand-KeyTap` background thread
    /// (key-driven `noteOn` → `resumeAudioEngineIfNeeded`). Without
    /// serialization, an eager keypress during the ~1.5 s startup
    /// preload window calls `engine.start()` mid-sweep, which (see the
    /// note in `configureMIDISynth`) makes the remaining program-change
    /// events queue for the render thread while `EnablePreload(false)`
    /// lands first — programs silently fail to fault in, and the next
    /// `selectMelodicProgram` switches to a program with no samples, so
    /// MIDISynth plays its empty-state sine ("beep") instead of the
    /// chosen instrument. Recursive because the configure critical
    /// section calls graph helpers that re-acquire it.
    private let engineLock = NSRecursiveLock()

    /// Observer for `AVAudioEngineConfigurationChange`. The engine posts
    /// this whenever the output/input hardware changes — e.g. plugging in
    /// an aux cable or headphones, switching to AirPods, or a sample-rate
    /// change. On that event AVAudioEngine STOPS and uninitializes itself
    /// (nodes stay attached/connected, but rendering halts and MIDISynth's
    /// per-channel active program reverts). Nothing used to restart it or
    /// re-assert the programs, so the next note played MIDISynth's
    /// empty-state sine ("beep") on the user's voice — the exact "it
    /// desyncs from the MIDI instrument when the audio device changes"
    /// symptom. We restart + `reapplyCurrentPrograms` on receipt.
    private var configChangeObserver: NSObjectProtocol?

    /// Master output gain on the pre-limiter sum bus, 0.0…1.0. Applied
    /// to `preLimiterMixer.outputVolume` so every backend (MIDISynth,
    /// sampler, radio, sample voice, plugin) scales together. Sitting
    /// BEFORE the limiter means lowering the slider also pulls the
    /// peak-limit threshold down proportionally — the slider is the
    /// user's "max volume" knob in a literal sense.
    private var masterVolume: Float = 1.0

    /// External callers (the controller's hover-preview path) read this
    /// to decide whether they need to wait for the bank-swap settle delay
    /// before issuing a noteOn after a setMelodicProgram. When MIDISynth
    /// is the active backend, swaps are sub-millisecond and the delay
    /// can be zero.
    var supportsInstantProgramChange: Bool { midiSynthReady }
    private var currentMelodicProgram: UInt8 = 0

    /// UserDefaults key for the "Use AC OS MIDI" toggle (About window). When
    /// true, melodic notes whose GM program has a native `gm_synth` voice
    /// route through `gmSynth` (the real AC OS instruments) instead of
    /// MIDISynth; unimplemented programs still fall back to MIDISynth.
    static let useACMIDIDefaultsKey = "menuband.useACMIDI"

    /// Live mirror of `useACMIDIDefaultsKey`. Read on the noteOn/noteOff hot
    /// path. Seeded from UserDefaults at init and refreshed by
    /// `setUseACMIDI` when the About checkbox flips.
    private var useACMIDI: Bool =
        UserDefaults.standard.bool(forKey: MenuBandSynth.useACMIDIDefaultsKey)

    /// Flip the AC-OS-MIDI routing. Called from the About checkbox handler
    /// (via notification). Silences any in-flight GM voices on disable so a
    /// held note doesn't hang on the now-bypassed node.
    func setUseACMIDI(_ on: Bool) {
        useACMIDI = on
        if gmSynthEnabled, !on { gmSynth.panic() }
    }

    /// Feature flag: the AC OS native GM synth (`gmSynth`). Re-enabled now
    /// that the render-thread `EXC_BAD_ACCESS (code=2)` is fixed at its root:
    /// it was an audio-thread STACK OVERFLOW from constructing a `Voice`
    /// (whose `core: GMVoice` inlines ~80 KB of ks_buf/fx_delay/bore_buf/
    /// ss_chorus_buf) as a `var v = Voice()` local on the IOThread stack, then
    /// copying it into the pool — ~160 KB the deep AudioUnit pull chain could
    /// not afford. `MenuBandGMSynth.noteOn` now inits the C voice IN PLACE in
    /// the heap pool (`withUnsafeMutablePointer(to: &voices[slot].core)`), and
    /// the render loop only ever touches `core` via pointer — no by-value
    /// `Voice` copy on the audio thread. Set false to fall everything back to
    /// MIDISynth if a new render-path issue ever appears.
    private let gmSynthEnabled = true

    /// True when a melodic note for `program` should route to `gmSynth`:
    /// the feature is enabled, the toggle is on, and the GM core actually
    /// implements that program.
    private func gmRoutable(_ program: UInt8) -> Bool {
        gmSynthEnabled && useACMIDI && MenuBandGMSynth.programImplemented(program)
    }

    /// Attach an external `MenuBandTape` to this synth's audio graph.
    /// Connects the tape's `playerNode` to `preLimiterMixer` so tape
    /// playback shares the same limiter + master volume + waveform
    /// tap as the live synth. Called once from the controller after
    /// `start()`.
    func attachTape(_ tape: MenuBandTape) {
        tape.attach(to: engine, output: preLimiterMixer)
    }

    /// Pin the hot mic running for a named reason (the tape uses
    /// "tape-record" while REC is engaged). Forwards to the sample
    /// voice's pin mechanism. Returns true if the mic stream is now
    /// flowing.
    @discardableResult
    func pinHotMic(reason: String) -> Bool {
        sampleVoice.addHotMicPin(reason)
    }
    func unpinHotMic(reason: String) {
        sampleVoice.removeHotMicPin(reason)
    }

    // Tap-driven ring buffer for the popover's live waveform display.
    private static let waveformRingSize = 4096
    private var waveformRing = [Float](repeating: 0, count: waveformRingSize)
    private var waveformWriteIdx: Int = 0
    private let waveformLock = NSLock()
    private var waveformTapInstalled = false
    /// Additional consumers of the mainMixer tap buffer. AVAudioEngine
    /// only allows one tap per bus, so anyone else who wants the
    /// realtime synth output (the tape recorder) subscribes here and
    /// gets buffers forwarded synchronously from the audio thread.
    var onWaveformBuffer: ((AVAudioPCMBuffer) -> Void)?
    /// Bridge for the tape recorder: forward to the sample-voice's
    /// mic-tap fork. Controller sets this on init so the tape can
    /// subscribe without the synth importing `MenuBandTape`.
    var onMicInputBuffer: ((AVAudioPCMBuffer) -> Void)? {
        get { sampleVoice.onInputBuffer }
        set { sampleVoice.onInputBuffer = newValue }
    }
    /// External callers (the tape recorder) can pin the waveform tap
    /// on independently of `setWaveformCaptureEnabled`. The tap stays
    /// installed as long as EITHER the visualizer or the tape needs
    /// it. Tracked separately so toggling one off doesn't kill the
    /// other.
    private var waveformTapPinReasons: Set<String> = []

    /// Apple's DLS bank — present on every macOS install since 10.x.
    private static let bankURL = URL(
        fileURLWithPath: "/System/Library/Components/CoreAudio.component/Contents/Resources/gs_instruments.dls"
    )

    func start() {
        guard !started else { return }
        engine.attach(preLimiterMixer)
        engine.attach(echo)
        engine.attach(spaceReverb)
        engine.attach(proximityEQ)
        engine.attach(postFxMixer)
        engine.attach(compressor)
        engine.attach(limiter)
        engine.attach(melodic)
        engine.attach(drums)
        connectLimiterIfNeeded()
        connectMelodicSamplerIfNeeded()
        connectDrumsSamplerIfNeeded()
        // Wire the radio's static graph into the same pre-limiter sum
        // bus. The AVPlayer stays paused until `setRadioBackend(true)`,
        // so this just adds idle nodes — no CPU cost while inactive.
        radio.attach(to: engine, output: preLimiterMixer)
        // Sample voice: same pre-limiter sum bus. Master gate stays
        // closed until the user records a clip and `setSampleBackend`
        // opens it. Voice nodes attach lazily on first noteOn.
        sampleVoice.attach(to: engine, output: preLimiterMixer)
        // Speech easter egg: same pre-limiter sum bus, so spoken language
        // names ride the bend/space/echo fx. Idle (no player) until `speak`.
        speechVoice.attach(to: engine, output: preLimiterMixer)
        // Percussion: same pre-limiter sum bus. Renders silence until the
        // right-hand split fires a drum, so it's free while inactive.
        // Percussion routes to the DRY post-fx mixer, NOT preLimiterMixer —
        // so trackpad echo/reverb/proximity (and pitch-bend) never hit the
        // drums. Still compressed + limited + tape-captured downstream.
        percussion.attach(to: engine, output: postFxMixer)
        // AC GM synth: melodic backend, so it joins the pre-limiter fx bus
        // alongside MIDISynth / sampler / radio / sample — picking up the
        // trackpad space/echo/proximity exactly like every other melodic
        // voice. (Its own pitch-bend is routed in-process; the fx are the
        // master ones.) Silent until a note is routed here.
        // Disabled for now (see `gmSynthEnabled`): leaving the node UNATTACHED
        // means its render callback never runs, so the crashing audio-thread
        // path can't be entered at all.
        if gmSynthEnabled {
            gmSynth.attach(to: engine, output: preLimiterMixer)
            gmSynth.setProgram(currentMelodicProgram)
        }
        // Spacebar reverse-replay voice. Plays DRY into mainMixerNode (NOT
        // the pre-limiter bus) so the already-effected captured audio isn't
        // re-processed. Its rolling capture ring is fed from a dedicated tap
        // on the `limiter` (installed below) — which is UPSTREAM of where the
        // reverse player joins (mainMixerNode), so the reverse playback is
        // NOT recaptured and the ring can keep recording continuously
        // (including notes played WHILE reversing) without a feedback loop.
        rewindVoice.attach(to: engine)
        engine.prepare()
        // Request a smaller hardware IO buffer BEFORE the engine starts,
        // so the first render cycle is already at the low-latency size.
        // Setting it after engine.start() works too but causes a brief
        // restart on the underlying AU.
        lowerOutputBufferSizeIfNeeded()
        do {
            try engine.start()
            started = true
        } catch {
            NSLog("MenuBand synth engine start failed: \(error)")
            return
        }
        // Dedicated rewind-capture tap on the limiter (pre-mainMixer, so the
        // reverse playback is excluded). Always-on: the ring keeps recording
        // everything the user plays, even while a reverse is sounding.
        let rewindFmt = limiter.outputFormat(forBus: 0)
        limiter.installTap(onBus: 0, bufferSize: 256, format: rewindFmt) { [weak self] buffer, _ in
            self?.rewindVoice.feed(buffer)
        }
        applyOutputDeviceOverride()
        // Hog mode + buffer-size lowering on AVAudioEngine doesn't pan
        // out on macOS: even with `kAudioDevicePropertyHogMode` held by
        // us (verified via external query) and our 64-frame set call
        // returning noErr, the device reports back 512 frames. The
        // engine re-asserts its preferred buffer size on each render
        // cycle and there's no public API to override that — would
        // need a from-scratch AUHAL setup. So we get all the cost of
        // hog (other apps can't share the device) with none of the
        // latency win. Stay on the shared-device floor instead. The
        // acquire/release helpers are kept available in case a future
        // backend bypasses AVAudioEngine and can actually benefit.
        lowerOutputBufferSizeIfNeeded()
        loadDefaultPatches()
        primeForLowLatency()
        // Report the output-latency budget once the device has settled.
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.5) { [weak self] in
            self?.measuredOutputLatency()
        }
        // Apply the persisted master volume now that preLimiterMixer is
        // attached + connected (setting outputVolume before attach is a
        // silent no-op on AVAudioMixerNode).
        preLimiterMixer.outputVolume = masterVolume

        // Recover from audio-device changes (aux cable, headphones,
        // AirPods, sample-rate switch). See `configChangeObserver`.
        observeEngineConfigurationChanges()

        // Try to bring up the multi-timbral MIDISynth in the background.
        // If it works, we'll route notes through it for instant program
        // switching. If it doesn't, the user keeps the sampler fallback.
        startMIDISynthBackend()
        scheduleIdleSuspendIfNeeded()
    }

    private func observeEngineConfigurationChanges() {
        guard configChangeObserver == nil else { return }
        configChangeObserver = NotificationCenter.default.addObserver(
            forName: .AVAudioEngineConfigurationChange,
            object: engine,
            queue: .main
        ) { [weak self] _ in
            self?.handleEngineConfigurationChange()
        }
    }

    /// React to a hardware output/input change. The engine has already
    /// stopped itself by the time this fires; restart it, re-point at the
    /// (possibly new) default device, and re-assert the MIDISynth bank+PC
    /// so the next note plays the user's chosen instrument instead of the
    /// empty-state sine. Serialized + idempotent — safe alongside a
    /// concurrent `resumeAudioEngineIfNeeded` from the KeyTap thread.
    private func handleEngineConfigurationChange() {
        engineLock.lock()
        defer { engineLock.unlock() }
        guard started else { return }
        audioConfigEpoch &+= 1
        NSLog("MenuBand: audio engine configuration changed (device switch \(audioConfigEpoch)) — recovering")
        // Mark the bank dirty no matter which recovery branch runs below —
        // the immediate reapply can be overwritten as the reconfig settles,
        // so the next note re-asserts the bank as a guaranteed backstop.
        midiSynthBankDirty = true
        // Make sure the engine is live so we can re-point + rebuild on the
        // new device.
        if !engine.isRunning {
            do { try engine.start() }
            catch { NSLog("MenuBand: engine start after device change failed: \(error)") }
        }
        applyOutputDeviceOverride()
        lowerOutputBufferSizeIfNeeded()
        // The MIDISynth can't be coaxed back from its reset sine state by a
        // bank re-set OR an engine stop→start (both observed to no-op after
        // a device switch). The only reliable recovery is to REBUILD it —
        // tear the AU down and instantiate a fresh one, exactly like launch
        // (fresh bank + program preload).
        //
        // But the rebuild MUST NOT run now: the `applyOutputDeviceOverride` +
        // `lowerOutputBufferSizeIfNeeded` writes above can each emit their OWN
        // configuration-change, and CoreAudio's IO thread keeps negotiating
        // the new device/rate for a beat after this notification fires.
        // Rebuilding into that churn runs the sample preload while the engine
        // is being re-paused/restarted underneath it — the program-change
        // events miss the `EnablePreload` window and every instrument collapses
        // to MIDISynth's empty-state sine (silent). That's the "instruments
        // died after switching headphones" bug. So debounce: rebuild only once
        // the device has been quiet for a full settle window. The lazy
        // `reassertMIDISynthBankIfDirty` keeps the EXISTING AU limping on the
        // right bank for any notes played during the wait.
        scheduleMIDISynthRebuild()
        reassertActiveBackend()
    }

    /// Arm (or, if a burst of switches is in flight, re-arm) the debounced
    /// MIDISynth rebuild. Called under `engineLock` from
    /// `handleEngineConfigurationChange`. Idempotent within a burst — the
    /// first switch arms the timer; later switches just bump `audioConfigEpoch`
    /// and the fire handler notices it advanced and re-arms, so the rebuild
    /// slides out to land only after the device finally goes quiet.
    private func scheduleMIDISynthRebuild() {
        guard rebuildArmedForEpoch == nil else { return }
        armMIDISynthRebuildTimer()
    }

    private func armMIDISynthRebuildTimer() {
        rebuildArmedForEpoch = audioConfigEpoch
        DispatchQueue.main.asyncAfter(deadline: .now() + midiSynthRebuildSettleDelay) { [weak self] in
            self?.fireMIDISynthRebuildTimer()
        }
    }

    private func fireMIDISynthRebuildTimer() {
        engineLock.lock()
        defer { engineLock.unlock() }
        guard let armedEpoch = rebuildArmedForEpoch else { return }
        // Another switch landed while we waited — the device is still moving.
        // Re-arm for the newer epoch so the preload only runs once a full
        // settle window passes with no further change.
        if armedEpoch != audioConfigEpoch {
            armMIDISynthRebuildTimer()
            return
        }
        rebuildArmedForEpoch = nil
        guard started else { return }
        if !engine.isRunning {
            do { try engine.start() }
            catch { NSLog("MenuBand: engine start before MIDISynth rebuild failed: \(error)") }
        }
        // Re-assert the output binding against the NOW-settled default
        // device. The immediate recovery in `handleEngineConfigurationChange`
        // can run while CoreAudio still reports the OLD device as default
        // (the notification races the default-device flip), pinning the
        // engine to a stale device and skipping the buffer lowering — the
        // epochs whose logs miss "lowered IO buffer" are exactly those.
        // After the settle window the default is trustworthy.
        applyOutputDeviceOverride()
        lowerOutputBufferSizeIfNeeded()
        NSLog("MenuBand: device settled (epoch \(armedEpoch)) — rebuilding MIDISynth")
        // Runs the heavy bring-up async, after this lock releases, so it can't
        // deadlock on `engineLock`.
        rebuildMIDISynth()
    }

    /// Tear down the (reset) MIDISynth and build a fresh one — the reliable
    /// recovery after a device switch leaves it stuck on its sine default.
    /// The synchronous part (detach the dead AU) runs under the caller's
    /// `engineLock`; the bring-up (`configureMIDISynth`, which takes the
    /// lock itself) is kicked off asynchronously by `startMIDISynthBackend`,
    /// so it runs only after this lock is released.
    private func rebuildMIDISynth() {
        NSLog("MenuBand: rebuilding MIDISynth after device switch")
        midiSynthReady = false   // notes fall back to the sampler meanwhile
        if let old = midiSynth {
            engine.disconnectNodeOutput(old)
            engine.detach(old)
            midiSynth = nil
        }
        // CRITICAL: the old AU is now detached, so the graph no longer has a
        // MIDISynth wired into `preLimiterMixer`. `connectMIDISynthIfNeeded`
        // is guarded by this flag and would otherwise skip connecting the
        // FRESH AU built below — leaving it attached but floating (output to
        // nowhere). That is the silent-after-device-switch bug: the rebuilt
        // synth reports "ready", program changes apply, but no note is audible
        // because the node never reaches the mixer (and `midiSynthReady=true`
        // then disconnects the sampler fallback too). Clear it so the new AU
        // actually gets connected.
        midiSynthConnected = false
        startMIDISynthBackend()
    }

    // MARK: - MIDISynth (multi-timbral, instant switching)

    private func startMIDISynthBackend() {
        // Snapshot the device epoch this build is for. Instantiation +
        // configure run async; if a NEW device switch lands during that gap
        // the samples we're about to preload are already for the wrong device,
        // so `configureMIDISynth` re-arms a rebuild instead of trusting them.
        let builtForEpoch = audioConfigEpoch
        let desc = AudioComponentDescription(
            componentType: kAudioUnitType_MusicDevice,
            componentSubType: kAudioUnitSubType_MIDISynth,
            componentManufacturer: kAudioUnitManufacturer_Apple,
            componentFlags: 0,
            componentFlagsMask: 0
        )
        AVAudioUnit.instantiate(with: desc, options: []) { [weak self] avUnit, error in
            DispatchQueue.main.async {
                guard let self = self, let avUnit = avUnit, error == nil else {
                    NSLog("MenuBand: MIDISynth instantiate failed: \(String(describing: error)) — staying on sampler fallback")
                    return
                }
                self.configureMIDISynth(avUnit, builtForEpoch: builtForEpoch)
            }
        }
    }

    private func configureMIDISynth(_ avUnit: AVAudioUnit, builtForEpoch: Int = 0) {
        let au = avUnit.audioUnit

        // Hold the engine lock across the ENTIRE bring-up (bank set →
        // preload → program select → restart → `midiSynthReady = true`).
        // A key-driven `noteOn` on the KeyTap thread calls
        // `resumeAudioEngineIfNeeded`, which also takes this lock; without
        // it, that resume could `engine.start()` mid-preload and corrupt
        // the sweep (see `engineLock`). Blocking the keypress for the
        // ~tens-of-ms sweep is the right trade: the user might miss one
        // note's onset at launch, but MIDISynth comes up correct and never
        // beeps afterward. By the time the blocked resume proceeds,
        // `midiSynthReady` is true so its `noteOn` routes to the freshly
        // loaded MIDISynth voice, not the unconfigured one.
        engineLock.lock()
        defer { engineLock.unlock() }

        // 1. Set the GS DLS bank URL. Must be CFURL — passing a Swift `URL`
        //    fails the NSURL selector dispatch inside CoreAudio.
        var bankURL: CFURL = MenuBandSynth.bankURL as CFURL
        let bankStatus = withUnsafePointer(to: &bankURL) { ptr -> OSStatus in
            AudioUnitSetProperty(
                au,
                AudioUnitPropertyID(kMusicDeviceProperty_SoundBankURL),
                kAudioUnitScope_Global,
                0,
                ptr,
                UInt32(MemoryLayout<CFURL>.size)
            )
        }
        guard bankStatus == noErr else {
            NSLog("MenuBand: MIDISynth bank URL set failed status=\(bankStatus) — staying on sampler fallback")
            return
        }

        // 2. Attach + connect to the engine. It may be running or paused
        //    for hidden idle; AVAudioEngine handles graph reconfig.
        engine.attach(avUnit)
        connectMIDISynthIfNeeded(avUnit)

        // 3. Preload every GM program (and the drum kit) while the engine
        //    is paused. MIDISynth's EnablePreload→PC→disable dance only
        //    actually faults samples in when the PC events are processed
        //    during the preload-enabled window. With the engine running,
        //    MusicDeviceMIDIEvent gets queued for the next render cycle
        //    and the preload-disable on the main thread can land before
        //    that render runs — programs would silently fail to load and
        //    noteOn would play MIDISynth's default sine fallback instead
        //    of the chosen instrument. Pausing the engine around the
        //    preload turns the PC handling synchronous on the main
        //    thread and matches the original (pre-engine-start) sweep
        //    that worked. Cost: a ~few-hundred-ms gap in sampler audio
        //    once per launch; afterwards every program switch is an
        //    instant PC with no further loading.
        let engineWasRunning = engine.isRunning
        if engineWasRunning { engine.pause() }
        let sweepStart = CACurrentMediaTime()
        preloadAllMelodicPrograms(au)
        preloadDrumKit(au)
        let sweepMs = (CACurrentMediaTime() - sweepStart) * 1000.0
        // Always restart the engine after preload — selectMelodicProgram
        // below (and every subsequent noteOn) needs the render thread
        // alive to actually process the queued bank+PC MIDI events.
        // The old `if engineWasRunning` guard stranded us in a paused
        // state when configureMIDISynth happened during a graph-reconfig
        // window (engine.attach/engine.connect can transiently pause the
        // engine), with the symptom that the user's picked instrument
        // never lands on the AU — first noteOn after launch plays
        // MIDISynth's empty-state fallback (a sine tone) regardless of
        // which program the UI shows as selected.
        if !engine.isRunning {
            do { try engine.start() } catch {
                NSLog("MenuBand: engine restart after MIDISynth preload failed: \(error)")
            }
        }

        // After preload-disable the AU's *active* program on each channel
        // is unset — send a fresh bank-select + PC so the very first
        // noteOn lands on the user's chosen voice (and the drum kit on
        // channel 9) without an extra audible swap.
        selectMelodicProgram(au, program: currentMelodicProgram)
        selectDrumKit(au)

        midiSynth = avUnit
        midiSynthReady = true
        midiSynthBankDirty = false   // fresh AU has the bank loaded
        updateSamplerRoutingForActiveBackend()
        scheduleIdleSuspendIfNeeded()
        NSLog(String(format: "MenuBand: MIDISynth ready — instant program switching enabled (preload sweep %.1f ms)", sweepMs))

        // If the device switched again while this AU was instantiating +
        // preloading, the samples we just faulted in are for the OLD device
        // and may be silent on the new one. Re-arm the debounced rebuild so a
        // fresh AU is built once the device finally settles. (builtForEpoch is
        // 0 at launch, which only mismatches if a switch raced startup — also
        // correct to rebuild then.)
        if builtForEpoch != audioConfigEpoch {
            NSLog("MenuBand: device switched during MIDISynth bring-up (built \(builtForEpoch), now \(audioConfigEpoch)) — re-arming rebuild")
            scheduleMIDISynthRebuild()
        }

        // The MIDISynth stays connected so keyboard input remains playable
        // while the popover is hidden. The inactive sampler outputs are
        // disconnected from the render graph until a fallback or GarageBand
        // patch needs them again.
    }

    /// Wire preLimiterMixer → limiter → mainMixerNode and tune the limiter
    /// for transparent peak control on instrument samples. Called once from
    /// `start()` before any backend is connected so backends can route
    /// straight to `preLimiterMixer`.
    private func connectLimiterIfNeeded() {
        guard !limiterConnected else { return }
        // preLimiterMixer → echo → spaceReverb → proximityEQ → compressor →
        // limiter → main. Echo is first so the reverb washes its repeats
        // (not vice-versa); proximityEQ (left-axis "closer/tinier") sits
        // last in the fx group so it shrinks the whole wet+dry blend. All
        // sit pre-compressor so their tails are leveled + peak-controlled
        // and pre-master so the volume slider scales them with everything.
        // The compressor does the loudness normalization (glue + makeup gain);
        // the PeakLimiter remains the final brick wall at 0 dBFS.
        engine.connect(preLimiterMixer, to: echo, format: nil)
        engine.connect(echo, to: spaceReverb, format: nil)
        engine.connect(spaceReverb, to: proximityEQ, format: nil)
        engine.connect(proximityEQ, to: postFxMixer, format: nil)
        engine.connect(postFxMixer, to: compressor, format: nil)
        engine.connect(compressor, to: limiter, format: nil)
        engine.connect(limiter, to: engine.mainMixerNode, format: nil)

        // "Glued master" compressor: pull quiet notes up and tame loud
        // chords toward a common level, then makeup-gain the whole bus so
        // ordinary playing sits near 0 dBFS. Soft knee (HeadRoom) keeps it
        // musical rather than slammed; +9 dB makeup is what actually makes
        // it "as loud as it can be" — the downstream limiter absorbs any
        // overshoot so the makeup can be generous without clipping.
        // Expansion left at defaults (ExpansionThreshold ≈ -100 dB) so the
        // low-end expander never acts as a noise gate on soft tails.
        let cAU = compressor.audioUnit
        // dB. Everything above this is compressed.
        AudioUnitSetParameter(cAU, kDynamicsProcessorParam_Threshold,
                              kAudioUnitScope_Global, 0, -18.0, 0)
        // dB soft-knee width above threshold — wider = gentler ratio.
        AudioUnitSetParameter(cAU, kDynamicsProcessorParam_HeadRoom,
                              kAudioUnitScope_Global, 0, 8.0, 0)
        // 8 ms attack lets note transients punch through before the
        // gain reduction clamps down (keeps attacks crisp).
        AudioUnitSetParameter(cAU, kDynamicsProcessorParam_AttackTime,
                              kAudioUnitScope_Global, 0, 0.008, 0)
        // 180 ms release recovers smoothly between phrases without
        // audible pumping on sustained pads/chords.
        AudioUnitSetParameter(cAU, kDynamicsProcessorParam_ReleaseTime,
                              kAudioUnitScope_Global, 0, 0.18, 0)
        // Makeup gain — the "normalize to max" stage. (kDynamicsProcessorParam
        // _MasterGain is spelled _OverallGain in the Swift-imported header.)
        AudioUnitSetParameter(cAU, kDynamicsProcessorParam_OverallGain,
                              kAudioUnitScope_Global, 0, 9.0, 0)

        let au = limiter.audioUnit
        // Fast attack catches chord/transient peaks; medium release avoids
        // pumping on sustained notes. Pre-gain stays at 0 — the compressor
        // already supplies makeup gain, so the limiter only has to catch
        // the residual peaks the compressor lets through.
        AudioUnitSetParameter(au, kLimiterParam_AttackTime,
                              kAudioUnitScope_Global, 0, 0.002, 0)
        AudioUnitSetParameter(au, kLimiterParam_DecayTime,
                              kAudioUnitScope_Global, 0, 0.050, 0)
        AudioUnitSetParameter(au, kLimiterParam_PreGain,
                              kAudioUnitScope_Global, 0, 0.0, 0)
        limiterConnected = true
    }

    /// Master "proximity" amount, 0…1 — the LEFT half of the X gesture.
    /// 0 = untouched / natural; 1 = pulled all the way in close and tiny.
    /// This is the deliberate *opposite* of reverb: instead of a big hall
    /// pushing the sound back and far, it narrows the band so the sound
    /// shrinks and comes right up to the listener (pocket-radio / cupped-
    /// hands timbre). Driven from `setSpace` so the same X-axis plumbing
    /// (and the spring-back ramp) feeds it.
    func setSpace(_ amount: Float) {
        let p = max(0, min(1, amount))
        let hp = proximityEQ.bands[0]
        let lp = proximityEQ.bands[1]
        if p <= 0.001 {
            // Fully open / flat — identical to before the node existed.
            hp.bypass = true
            lp.bypass = true
            proximityEQ.globalGain = 0
            return
        }
        hp.bypass = false
        lp.bypass = false
        // Lift the low cutoff (drop the body) and pull the high cutoff down
        // (drop the air) so the band closes toward a small midrange window
        // as the gesture pushes left — the "shrinking, coming closer" cue.
        hp.frequency = 40 + p * (1_000 - 40)
        lp.frequency = 18_000 - p * (18_000 - 2_800)
        // A little make-up gain so "closer" reads as present and intimate
        // rather than just thin and distant.
        proximityEQ.globalGain = p * 2.0
        // Keep the hall fully dry — proximity replaces reverb on this axis.
        spaceReverb.wetDryMix = 0
    }

    /// Master echo knob, 0…1. Drives wet mix + feedback together so a
    /// single axis sweeps "no echo" → "long trailing tape repeats".
    /// Feedback is capped under runaway so it can hang and bloom for
    /// character without self-oscillating into a scream. On the shared
    /// master path, so it's independent of the selected instrument.
    func setEcho(_ amount: Float) {
        let a = max(0, min(1, amount))
        echoAmount = a
        // ≤45% wet keeps the dry transient clearly on top of the tail.
        echo.wetDryMix = a * 45
        // Feedback to 78%: long, obvious repeats that still decay.
        echo.feedback = a * 78
    }
    private var echoAmount: Float = 0
    var currentEcho: Float { echoAmount }

    /// Speak a short phrase through the fx bus (About-window language
    /// easter egg). The voice rides whatever bend/space/echo is engaged.
    func speak(_ text: String, languageCode: String) {
        speechVoice.say(text, languageCode: languageCode)
    }

    /// Spacebar reverse-replay: snapshot the most-recent few seconds of
    /// master output and play it backwards once. Returns false if there
    /// wasn't enough captured audio to rewind (caller can cue a fallback).
    @discardableResult
    func playReverse() -> Bool {
        return rewindVoice.playReverse()
    }

    /// Spacebar released — stop the reverse voice. Capture stays frozen
    /// until the next note (see `resumeRewindCapture`) so repeated presses
    /// re-reverse the same window.
    func releaseReverse() {
        rewindVoice.release()
    }

    /// The fixed reverse-window length, so the waveform strip can scrub
    /// exactly that span in sync with the one-shot reverse playback.
    var rewindWindowSeconds: Double { rewindVoice.captureSeconds }

    /// Reverse playback progress (0…1), or nil if not reversing — for the
    /// strip's exact, drift-free playhead.
    func rewindProgress() -> Double? { rewindVoice.reverseProgress() }

    /// Route the trackpad pitch-bend / echo onto the spacebar tape playback
    /// (its own DRY inserts — see MenuBandRewindVoice), so moving the mouse
    /// while space is held warps the reverse audio just like a live note.
    func setRewindBend(amount: Float) { rewindVoice.setBend(amount: amount) }
    func setRewindEcho(amount: Float) { rewindVoice.setEcho(amount: amount) }

    /// Play one random drum hit — the About-window card-flip easter egg
    /// pairs this with the sparkle chord so repeated flips build a little
    /// beat. Uses the live percussion kit (already attached to the bus).
    func playEasterEggDrum() {
        let kit: [MenuBandPercussion.Drum] =
            [.kick, .snare, .hatClosed, .clap, .tambo, .block, .cowbell, .snap, .ride]
        let drum = kit.randomElement() ?? .kick
        percussion.play(drum, velocity: 102, pan: 0)
    }

    /// Short wood-block tick for each octave step of the two-finger swipe —
    /// gives the gesture a tactile detent. Panned by direction (up → right,
    /// down → left) so a fast swipe reads as a little rising/falling clave run.
    /// Dry (percussion bypasses the fx bus) so it stays crisp under any bend.
    func playOctaveTick(up: Bool) {
        percussion.play(.block, velocity: 88, pan: up ? 0.28 : -0.28)
    }

    /// Route the trackpad pitch-bend into the spoken-language easter-egg
    /// voice (AVAudioUnitTimePitch — ignores MIDI bend), so the TTS slides
    /// in pitch alongside every other voice.
    func setSpeechPitchBend(amount: Float) {
        speechVoice.setBend(amount: amount)
    }

    private func connectMelodicSamplerIfNeeded() {
        engineLock.lock(); defer { engineLock.unlock() }
        guard !melodicConnected else { return }
        engine.connect(melodic, to: preLimiterMixer, format: nil)
        melodicConnected = true
    }

    private func disconnectMelodicSamplerIfNeeded() {
        engineLock.lock(); defer { engineLock.unlock() }
        guard melodicConnected else { return }
        stopAllSamplerNotes(melodic)
        engine.disconnectNodeOutput(melodic)
        melodicConnected = false
    }

    private func connectDrumsSamplerIfNeeded() {
        engineLock.lock(); defer { engineLock.unlock() }
        guard !drumsConnected else { return }
        engine.connect(drums, to: preLimiterMixer, format: nil)
        drumsConnected = true
    }

    private func disconnectDrumsSamplerIfNeeded() {
        engineLock.lock(); defer { engineLock.unlock() }
        guard drumsConnected else { return }
        stopAllSamplerNotes(drums)
        engine.disconnectNodeOutput(drums)
        drumsConnected = false
    }

    private func connectMIDISynthIfNeeded(_ avUnit: AVAudioUnit) {
        guard !midiSynthConnected else { return }
        engine.connect(avUnit, to: preLimiterMixer, format: nil)
        midiSynthConnected = true
    }

    private func updateSamplerRoutingForActiveBackend() {
        guard started else { return }
        if midiSynthReady {
            if usingGarageBandPatch {
                connectMelodicSamplerIfNeeded()
            } else {
                disconnectMelodicSamplerIfNeeded()
            }
            disconnectDrumsSamplerIfNeeded()
        } else {
            connectMelodicSamplerIfNeeded()
            connectDrumsSamplerIfNeeded()
        }
    }

    private func stopAllSamplerNotes(_ sampler: AVAudioUnitSampler) {
        for note: UInt8 in 0...127 {
            sampler.stopNote(note, onChannel: 0)
        }
    }

    @discardableResult
    private func resumeAudioEngineIfNeeded() -> Bool {
        guard started else { return false }
        // Serialize against `configureMIDISynth`'s preload sweep and the
        // idle-suspend pause. This call runs on the KeyTap background
        // thread for key-driven notes; without the lock it could
        // `engine.start()` while the main thread is mid-preload (see
        // `engineLock`). If configure is in flight, we block here until
        // it finishes — by then the engine is running and `midiSynthReady`
        // is true, so `engine.isRunning` short-circuits below.
        engineLock.lock()
        defer { engineLock.unlock() }
        idleSuspendWorkItem?.cancel()
        idleSuspendWorkItem = nil
        if engine.isRunning {
            return true
        }
        do {
            try engine.start()
            applyOutputDeviceOverride()
            // MIDISynth's per-channel program is unreliable across an
            // engine pause→start cycle — coming back from idle-suspend
            // (or any other engine restart) can leave each channel
            // pointing at the AU's default program 0 (Acoustic Grand
            // Piano) instead of the user's picked voice. Re-asserting
            // bank+PC here means the next noteOn lands on the right
            // instrument, no matter how long the user was idle.
            // Cost: two MIDI bytes × 8 channels — sub-millisecond.
            reapplyCurrentPrograms()
            return true
        } catch {
            NSLog("MenuBand synth engine resume failed: \(error)")
            return false
        }
    }

    /// Re-send bank+PC for the active melodic program and the GM drum
    /// kit on channel 9. Called after every engine restart so the
    /// MIDISynth AU doesn't silently revert to default Piano on
    /// melodic channels (and the standard kit on drums).
    private func reapplyCurrentPrograms() {
        guard midiSynthReady, let au = midiSynth?.audioUnit else { return }
        // An audio-device / engine reconfig can DROP the MIDISynth's loaded
        // DLS soundbank — after which a bare program change selects into an
        // empty bank and every voice plays as a sine, no matter the number.
        // Re-set the bank URL first so the program lands on a real patch.
        reloadMIDISynthBank(au)
        selectMelodicProgram(au, program: currentMelodicProgram)
        selectDrumKit(au)
    }

    /// Lazy self-heal: if a config change marked the bank dirty, reload the
    /// soundbank + re-select the program right before the next note (after
    /// the reconfig has fully settled). Cheap no-op when clean — the flag is
    /// checked before taking the lock so the common note path pays nothing.
    private func reassertMIDISynthBankIfDirty() {
        guard midiSynthBankDirty else { return }
        engineLock.lock()
        defer { engineLock.unlock() }
        guard midiSynthBankDirty else { return }   // re-check under lock
        midiSynthBankDirty = false
        guard midiSynthReady, let au = midiSynth?.audioUnit else { return }
        NSLog("MenuBand: re-asserting MIDISynth bank after device switch")
        reloadMIDISynthBank(au)
        selectMelodicProgram(au, program: currentMelodicProgram)
        selectDrumKit(au)
    }

    /// Re-assert the GS DLS soundbank URL on the MIDISynth (idempotent).
    /// Used by recovery after a device switch so the kit doesn't fall back
    /// to sine. Bank-URL only — the one-time per-program preload from
    /// `configureMIDISynth` isn't repeated (it's a latency optimization,
    /// not a correctness one).
    private func reloadMIDISynthBank(_ au: AudioUnit) {
        var bankURL: CFURL = MenuBandSynth.bankURL as CFURL
        let status = withUnsafePointer(to: &bankURL) { ptr -> OSStatus in
            AudioUnitSetProperty(
                au,
                AudioUnitPropertyID(kMusicDeviceProperty_SoundBankURL),
                kAudioUnitScope_Global, 0, ptr,
                UInt32(MemoryLayout<CFURL>.size))
        }
        if status != noErr {
            NSLog("MenuBand: MIDISynth bank reload failed status=\(status)")
        }
    }

    /// After an engine restart (e.g. an audio-device / aux switch) the GM
    /// program is re-asserted by `reapplyCurrentPrograms`, but a non-GM
    /// backend would otherwise fall silent or read as reverting to a plain
    /// voice. Re-point whichever backend is actually active so the device
    /// switch doesn't change the instrument out from under the user.
    private func reassertActiveBackend() {
        guard started else { return }
        if usingRadioBackend {
            // Re-open the radio's master gate and make sure the stream is
            // still pulling (both idempotent); its graph nodes survive the
            // reconfig, they just need the gate + stream re-asserted.
            radio.setOutputEnabled(true)
            radio.startStreaming()
        }
        // Sample voice + GB/plugin keep their attached nodes across a config
        // change and re-arm on the next note, so no extra work is needed.
    }

    private func scheduleIdleSuspendIfNeeded() {
        guard started, !waveformCaptureEnabled, activeNotes.isEmpty,
              !sampleRecordingActive else { return }
        idleSuspendWorkItem?.cancel()
        let workItem = DispatchWorkItem { [weak self] in
            self?.suspendAudioEngineForHiddenIdleIfNeeded()
        }
        idleSuspendWorkItem = workItem
        DispatchQueue.main.asyncAfter(deadline: .now() + idleSuspendDelay, execute: workItem)
    }

    private func suspendAudioEngineForHiddenIdleIfNeeded() {
        engineLock.lock(); defer { engineLock.unlock() }
        idleSuspendWorkItem = nil
        guard started, engine.isRunning, !waveformCaptureEnabled, activeNotes.isEmpty,
              !sampleRecordingActive, !keepEngineWarm else { return }
        removeWaveformTapIfNeeded()
        engine.pause()
    }

    /// True between `startSampleRecording` and `stopSampleRecording` —
    /// keeps the engine awake during a held backtick even when no
    /// notes are sounding so the input tap keeps delivering frames.
    private var sampleRecordingActive: Bool = false

    private func noteKey(_ midi: UInt8, channel: UInt8) -> UInt16 {
        (UInt16(channel) << 8) | UInt16(midi)
    }

    /// Fault in every GM melodic program (0-127) on the GM Melodic bank
    /// (MSB 0x79). Must be called with the audio engine paused — the
    /// preload protocol relies on MusicDeviceMIDIEvent's PC messages
    /// being processed synchronously while EnablePreload is true, and
    /// a running engine queues them for the render thread instead.
    private func preloadAllMelodicPrograms(_ au: AudioUnit) {
        setMIDISynthPreload(au, enable: true)
        // Bank select once per channel (the MSB+LSB pair sticks until
        // overridden), then issue a PC for every program. We preload on
        // channels 0-7 to match the controller's voice allocator
        // (live 0-3, doppler 4-7); channel 9 is drums, 8 and 10-15 are
        // unused so we skip them.
        for ch: UInt8 in 0..<8 {
            sendMIDIEvent(au, status: 0xB0 | ch, data1: 0,  data2: 0x79)
            sendMIDIEvent(au, status: 0xB0 | ch, data1: 32, data2: 0x00)
        }
        for program: UInt8 in 0...127 {
            for ch: UInt8 in 0..<8 {
                sendMIDIEvent(au, status: 0xC0 | ch, data1: program)
            }
            loadedPrograms.insert((UInt16(0x79) << 8) | UInt16(program))
        }
        setMIDISynthPreload(au, enable: false)
    }

    /// Fault in the standard GM drum kit on channel 9 (bank MSB 0x78).
    /// Same preload-with-engine-paused requirement as the melodic sweep.
    private func preloadDrumKit(_ au: AudioUnit) {
        setMIDISynthPreload(au, enable: true)
        sendMIDIEvent(au, status: 0xB9, data1: 0,  data2: 0x78)
        sendMIDIEvent(au, status: 0xB9, data1: 32, data2: 0x00)
        sendMIDIEvent(au, status: 0xC9, data1: 0)
        setMIDISynthPreload(au, enable: false)
        loadedPrograms.insert((UInt16(0x78) << 8) | 0)
    }

    /// Set of (bankMSB << 8 | program) keys faulted in by the startup
    /// preload sweep. Retained so the `selectMelodicProgram` /
    /// `selectDrumKit` callers can assert their program landed in the
    /// preload set, and so an unexpected slot (e.g. a future
    /// non-GM bank) can re-enter a one-shot preload if needed.
    private var loadedPrograms: Set<UInt16> = []

    /// Switch the active melodic program on the broadcast channels.
    /// With the startup preload sweep every GM program is already
    /// resident in the AU, so this is just a bank-select + PC pair —
    /// sub-millisecond, no disk I/O, no DSP rebuild. The 8-channel
    /// broadcast matches the controller's round-robin voice allocator
    /// (live 0-3, doppler 4-7) so a noteOn lands on the user's
    /// instrument no matter which channel it picked.
    private func selectMelodicProgram(_ au: AudioUnit, program: UInt8) {
        for ch: UInt8 in 0..<8 {
            sendMIDIEvent(au, status: 0xB0 | ch, data1: 0,  data2: 0x79)
            sendMIDIEvent(au, status: 0xB0 | ch, data1: 32, data2: 0x00)
            sendMIDIEvent(au, status: 0xC0 | ch, data1: program)
        }
    }

    /// Switch channel 9 back to the standard GM drum kit. The kit is
    /// preloaded once at startup so this is a bank-select + PC pair.
    private func selectDrumKit(_ au: AudioUnit) {
        sendMIDIEvent(au, status: 0xB9, data1: 0,  data2: 0x78)
        sendMIDIEvent(au, status: 0xB9, data1: 32, data2: 0x00)
        sendMIDIEvent(au, status: 0xC9, data1: 0)
    }

    private func setMIDISynthPreload(_ au: AudioUnit, enable: Bool) {
        var flag: UInt32 = enable ? 1 : 0
        let status = AudioUnitSetProperty(
            au,
            AudioUnitPropertyID(kAUMIDISynthProperty_EnablePreload),
            kAudioUnitScope_Global,
            0,
            &flag,
            UInt32(MemoryLayout<UInt32>.size)
        )
        if status != noErr {
            NSLog("MenuBand: MIDISynth EnablePreload(\(enable)) status=\(status)")
        }
    }

    @inline(__always)
    private func sendMIDIEvent(_ au: AudioUnit, status: UInt8, data1: UInt8, data2: UInt8 = 0) {
        MusicDeviceMIDIEvent(au, UInt32(status), UInt32(data1), UInt32(data2), 0)
    }

    // MARK: - Output device

    /// AVAudioEngine's AUHAL layer bypasses Multi-Output / Aggregate devices
    /// and connects directly to the underlying clock-source hardware device,
    /// so audio never reaches secondary members (e.g. BlackHole). Explicitly
    /// setting CurrentDevice to the system default after every engine start
    /// forces the engine to honour the full virtual device and its routing.
    private func applyOutputDeviceOverride() {
        guard let au = engine.outputNode.audioUnit else { return }
        var addr = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDefaultOutputDevice,
            mScope:    kAudioObjectPropertyScopeGlobal,
            mElement:  kAudioObjectPropertyElementMain)
        var deviceID = AudioDeviceID(0)
        var size = UInt32(MemoryLayout<AudioDeviceID>.size)
        guard AudioObjectGetPropertyData(
            AudioObjectID(kAudioObjectSystemObject), &addr, 0, nil, &size, &deviceID
        ) == noErr, deviceID != 0 else { return }
        // Skip the write when the AU is already bound to the default device —
        // a redundant CurrentDevice set can still emit a configuration-change
        // notification, and this is called from the settle-window rebuild
        // path, where that echo would re-arm the rebuild forever.
        var current = AudioDeviceID(0)
        var curSize = UInt32(MemoryLayout<AudioDeviceID>.size)
        if AudioUnitGetProperty(
            au, kAudioOutputUnitProperty_CurrentDevice,
            kAudioUnitScope_Global, 0, &current, &curSize
        ) == noErr, current == deviceID { return }
        let status = AudioUnitSetProperty(
            au, kAudioOutputUnitProperty_CurrentDevice,
            kAudioUnitScope_Global, 0,
            &deviceID, UInt32(MemoryLayout<AudioDeviceID>.size))
        if status != noErr {
            NSLog("MenuBand: output device override failed: \(status)")
        } else {
            NSLog("MenuBand: output device override — engine re-bound \(current) → \(deviceID)")
        }
    }

    /// Drive the engine's output AU toward a 128-frame IO buffer so
    /// keystroke → sound latency drops from the macOS default (~11.6 ms
    /// at 512 frames / 44.1 kHz, ~5.3 ms at 96 kHz) to ~2.9 ms / ~1.3 ms
    /// respectively.
    ///
    /// Two coordinated property writes:
    ///
    /// 1. Set `kAudioDevicePropertyBufferFrameSize` on the OUTPUT AU —
    ///    this is the request the AU uses when negotiating IO size with
    ///    the device. If we only set it on the device directly,
    ///    AVAudioEngine re-asserts its own preferred size on the next
    ///    render cycle and our value evaporates.
    /// 2. Also set it directly on the device — handles the case where
    ///    no other client has the device open yet and the device hasn't
    ///    been told what cycle to run at.
    ///
    /// Both writes are clamped to the device's supported range and only
    /// LOWER the existing value; if a pro-audio app already set it
    /// tighter (e.g. Ableton at 64), we leave it alone.
    private func lowerOutputBufferSizeIfNeeded() {
        guard let outAU = engine.outputNode.audioUnit else { return }
        var deviceID = AudioDeviceID(0)
        var devSize = UInt32(MemoryLayout<AudioDeviceID>.size)
        let devStatus = AudioUnitGetProperty(
            outAU, kAudioOutputUnitProperty_CurrentDevice,
            kAudioUnitScope_Global, 0, &deviceID, &devSize)
        guard devStatus == noErr, deviceID != 0 else {
            NSLog("MenuBand: low-latency buffer — could not resolve output device: \(devStatus)")
            return
        }

        var rangeAddr = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyBufferFrameSizeRange,
            mScope:    kAudioObjectPropertyScopeGlobal,
            mElement:  kAudioObjectPropertyElementMain)
        var range = AudioValueRange(mMinimum: 0, mMaximum: 0)
        var rangeSize = UInt32(MemoryLayout<AudioValueRange>.size)
        let rangeStatus = AudioObjectGetPropertyData(
            deviceID, &rangeAddr, 0, nil, &rangeSize, &range)

        // Aim for `targetIOBufferFrames` — a deadline budget the synth stack
        // can actually meet every cycle — clamped into the device's supported
        // range. We deliberately do NOT dive to the device MINIMUM: that floor
        // (typically 32 frames / 0.33 ms at 96 kHz) leaves no slack for
        // scheduling jitter on a constrained machine and underruns into
        // constant pops. Clamp the target UP to the device min only if the
        // hardware can't go as small as our target.
        var target = Self.targetIOBufferFrames
        if rangeStatus == noErr {
            let lo = UInt32(range.mMinimum)
            let hi = UInt32(range.mMaximum)
            target = min(hi, max(lo, Self.targetIOBufferFrames))
        }

        var currAddr = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyBufferFrameSize,
            mScope:    kAudioObjectPropertyScopeGlobal,
            mElement:  kAudioObjectPropertyElementMain)
        var current: UInt32 = 0
        var currSize = UInt32(MemoryLayout<UInt32>.size)
        let currStatus = AudioObjectGetPropertyData(
            deviceID, &currAddr, 0, nil, &currSize, &current)
        if currStatus == noErr, current > 0, current <= target {
            return  // already at-or-below target, leave it alone
        }

        // Write on the AU first so the engine's own negotiation
        // honours our preferred size.
        let auStatus = AudioUnitSetProperty(
            outAU,
            AudioUnitPropertyID(kAudioDevicePropertyBufferFrameSize),
            kAudioUnitScope_Global,
            0,
            &target,
            UInt32(MemoryLayout<UInt32>.size))

        // Then write on the device directly to cover the case where
        // the engine isn't yet driving the device.
        let devSetStatus = AudioObjectSetPropertyData(
            deviceID, &currAddr, 0, nil,
            UInt32(MemoryLayout<UInt32>.size), &target)

        if auStatus != noErr && devSetStatus != noErr {
            NSLog("MenuBand: low-latency buffer set to \(target) frames failed (auStatus=\(auStatus), devStatus=\(devSetStatus), current=\(current))")
        } else {
            NSLog("MenuBand: lowered IO buffer to \(target) frames (was \(current))")
        }
    }

    /// The audio side of the keypress→sound budget: the live IO buffer
    /// size + the engine's reported output presentation latency. Logged at
    /// startup and callable from a test/CLI to verify we're near the
    /// hardware floor. (A full keypress→audible measurement needs a
    /// loopback capture; this reports the deterministic output latency.)
    @discardableResult
    func measuredOutputLatency() -> (frames: UInt32, sampleRate: Double,
                                     bufferMs: Double, presentationMs: Double, totalMs: Double) {
        let sr = engine.outputNode.outputFormat(forBus: 0).sampleRate
        let presentation = engine.outputNode.presentationLatency  // seconds
        var frames: UInt32 = 0
        if let outAU = engine.outputNode.audioUnit {
            var dev = AudioDeviceID(0)
            var ds = UInt32(MemoryLayout<AudioDeviceID>.size)
            if AudioUnitGetProperty(outAU, kAudioOutputUnitProperty_CurrentDevice,
                                    kAudioUnitScope_Global, 0, &dev, &ds) == noErr {
                var addr = AudioObjectPropertyAddress(
                    mSelector: kAudioDevicePropertyBufferFrameSize,
                    mScope: kAudioObjectPropertyScopeGlobal,
                    mElement: kAudioObjectPropertyElementMain)
                var f: UInt32 = 0
                var fs = UInt32(MemoryLayout<UInt32>.size)
                if AudioObjectGetPropertyData(dev, &addr, 0, nil, &fs, &f) == noErr { frames = f }
            }
        }
        let bufferMs = sr > 0 ? Double(frames) / sr * 1000.0 : 0
        let presentationMs = presentation * 1000.0
        let totalMs = bufferMs + presentationMs
        NSLog(String(format:
            "MenuBand latency: buffer=%u frames (%.2f ms) @ %.0f Hz · presentation %.2f ms · output ≈ %.2f ms",
            frames, bufferMs, sr, presentationMs, totalMs))
        return (frames, sr, bufferMs, presentationMs, totalMs)
    }

    /// Take exclusive ownership of the engine's current output device.
    /// Hog mode is what lets us push the IO buffer below the shared-
    /// device floor (typically 512 frames). When we own the device,
    /// macOS no longer aggregates other clients' buffer requests, so
    /// our 64-frame target actually takes effect.
    ///
    /// Trade-off: while hogged, Music.app / Safari / system sounds /
    /// other audio apps go silent on this device — they can't share
    /// it with us. We release the moment Menu Band shuts down so the
    /// user's system isn't stranded.
    ///
    /// If another app already hogs the device (rare — usually only
    /// pro audio apps in low-latency sessions do this), we skip
    /// silently and stay on the shared-device floor.
    private func acquireHogModeIfNeeded() {
        guard !hogModeAcquired,
              let outAU = engine.outputNode.audioUnit else { return }
        var deviceID = AudioDeviceID(0)
        var devSize = UInt32(MemoryLayout<AudioDeviceID>.size)
        let devStatus = AudioUnitGetProperty(
            outAU, kAudioOutputUnitProperty_CurrentDevice,
            kAudioUnitScope_Global, 0, &deviceID, &devSize)
        guard devStatus == noErr, deviceID != 0 else { return }

        // Read current hog owner. -1 means no hog.
        var hogAddr = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyHogMode,
            mScope:    kAudioObjectPropertyScopeGlobal,
            mElement:  kAudioObjectPropertyElementMain)
        var currentOwner: pid_t = -1
        var ownerSize = UInt32(MemoryLayout<pid_t>.size)
        let readStatus = AudioObjectGetPropertyData(
            deviceID, &hogAddr, 0, nil, &ownerSize, &currentOwner)
        if readStatus == noErr, currentOwner != -1, currentOwner != getpid() {
            NSLog("MenuBand: hog mode skipped — device already owned by pid \(currentOwner)")
            return
        }
        if readStatus == noErr, currentOwner == getpid() {
            // Already ours from a prior start() — record + done.
            hogModeAcquired = true
            hogModeDeviceID = deviceID
            return
        }

        var us: pid_t = getpid()
        let setStatus = AudioObjectSetPropertyData(
            deviceID, &hogAddr, 0, nil,
            UInt32(MemoryLayout<pid_t>.size), &us)
        if setStatus == noErr {
            hogModeAcquired = true
            hogModeDeviceID = deviceID
            NSLog("MenuBand: acquired hog mode on device \(deviceID) (pid=\(us))")
        } else {
            NSLog("MenuBand: hog mode acquire failed status=\(setStatus)")
        }
    }

    /// Release exclusive ownership so other audio apps can use the
    /// device again. Always called from `stop()` (engine teardown)
    /// and indirectly from `applicationWillTerminate` via
    /// `MenuBandController.shutdown()`. Idempotent.
    private func releaseHogModeIfNeeded() {
        guard hogModeAcquired, hogModeDeviceID != 0 else { return }
        var hogAddr = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyHogMode,
            mScope:    kAudioObjectPropertyScopeGlobal,
            mElement:  kAudioObjectPropertyElementMain)
        var release: pid_t = -1
        let status = AudioObjectSetPropertyData(
            hogModeDeviceID, &hogAddr, 0, nil,
            UInt32(MemoryLayout<pid_t>.size), &release)
        if status == noErr {
            NSLog("MenuBand: released hog mode on device \(hogModeDeviceID)")
        } else {
            NSLog("MenuBand: hog mode release failed status=\(status)")
        }
        hogModeAcquired = false
        hogModeDeviceID = 0
    }

    // MARK: - Audio tap for visualizer

    func setWaveformCaptureEnabled(_ enabled: Bool) {
        guard started else { return }
        waveformCaptureEnabled = enabled
        if enabled {
            guard resumeAudioEngineIfNeeded() else {
                waveformCaptureEnabled = false
                return
            }
            installWaveformTapIfNeeded()
        } else if waveformTapPinReasons.isEmpty {
            removeWaveformTapIfNeeded()
            scheduleIdleSuspendIfNeeded()
        }
    }

    /// Pin the mainMixer tap on for a reason (currently: the tape
    /// recorder needs the synth output stream while RECORDING or
    /// PLAYING). Idempotent. Pair with `removeWaveformTapPin` so the
    /// tap goes away once both the visualizer AND every pin reason
    /// have released it.
    func addWaveformTapPin(_ reason: String) {
        guard started else { return }
        let wasEmpty = waveformTapPinReasons.isEmpty
        waveformTapPinReasons.insert(reason)
        if wasEmpty {
            _ = resumeAudioEngineIfNeeded()
            installWaveformTapIfNeeded()
        }
    }

    func removeWaveformTapPin(_ reason: String) {
        waveformTapPinReasons.remove(reason)
        if waveformTapPinReasons.isEmpty && !waveformCaptureEnabled {
            removeWaveformTapIfNeeded()
            scheduleIdleSuspendIfNeeded()
        }
    }

    private func installWaveformTapIfNeeded() {
        guard !waveformTapInstalled else { return }
        let mixer = engine.mainMixerNode
        let format = mixer.outputFormat(forBus: 0)
        // 256 frames ≈ 5.8 ms at 44.1 kHz — small buffer = fresh samples
        // for the visualizer without burning the audio thread.
        mixer.installTap(onBus: 0, bufferSize: 256, format: format) { [weak self] buffer, _ in
            self?.ingestWaveformBuffer(buffer)
        }
        waveformTapInstalled = true
    }

    private func removeWaveformTapIfNeeded() {
        guard waveformTapInstalled else { return }
        engine.mainMixerNode.removeTap(onBus: 0)
        waveformTapInstalled = false
    }

    private func ingestWaveformBuffer(_ buffer: AVAudioPCMBuffer) {
        // Fork the audio thread buffer to any extra consumers (the
        // tape recorder) BEFORE we lock the visualizer ring. Avoids
        // delivering to consumers AFTER we've returned from this
        // call — that's the whole point of the shared tap.
        if let extra = onWaveformBuffer {
            extra(buffer)
        }
        // (The rewind ring is fed by its own dedicated `limiter` tap — see
        // `start()` — not this mainMixer tap, so the reverse playback isn't
        // recaptured.)
        guard let data = buffer.floatChannelData?[0] else { return }
        let frames = Int(buffer.frameLength)
        waveformLock.lock()
        let ringSize = Self.waveformRingSize
        var idx = waveformWriteIdx
        for i in 0..<frames {
            waveformRing[idx] = data[i]
            idx += 1
            if idx >= ringSize { idx = 0 }
        }
        waveformWriteIdx = idx
        waveformLock.unlock()
    }

    func snapshotWaveform(into dest: inout [Float]) {
        let count = Swift.min(dest.count, Self.waveformRingSize)
        waveformLock.lock()
        let ringSize = Self.waveformRingSize
        var readIdx = (waveformWriteIdx - count + ringSize) % ringSize
        for i in 0..<count {
            dest[i] = waveformRing[readIdx]
            readIdx += 1
            if readIdx >= ringSize { readIdx = 0 }
        }
        waveformLock.unlock()
    }

    // MARK: - Sampler fallback setup

    private func loadDefaultPatches() {
        let url = MenuBandSynth.bankURL
        guard FileManager.default.fileExists(atPath: url.path) else {
            NSLog("MenuBand: gs_instruments.dls not found")
            return
        }
        let melodicMSB: UInt8 = 0x79
        let percussionMSB: UInt8 = 0x78
        do {
            try melodic.loadSoundBankInstrument(at: url, program: 0, bankMSB: melodicMSB, bankLSB: 0)
        } catch {
            NSLog("MenuBand: melodic patch load failed: \(error)")
        }
        do {
            try drums.loadSoundBankInstrument(at: url, program: 0, bankMSB: percussionMSB, bankLSB: 0)
        } catch {
            NSLog("MenuBand: drum kit load failed: \(error)")
        }
    }

    private func primeForLowLatency() {
        let melodicWarmup: [UInt8] = [60, 64, 67, 72]
        let drumWarmup: [UInt8] = [36, 38, 42, 46]
        for n in melodicWarmup {
            melodic.startNote(n, withVelocity: 1, onChannel: 0)
        }
        for n in drumWarmup {
            drums.startNote(n, withVelocity: 1, onChannel: 0)
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) { [weak self] in
            guard let self = self else { return }
            for n in melodicWarmup { self.melodic.stopNote(n, onChannel: 0) }
            for n in drumWarmup { self.drums.stopNote(n, onChannel: 0) }
        }
    }

    // MARK: - Public API

    /// Master output gain, 0.0…1.0. Stored on the pre-limiter sum bus
    /// so every backend scales together. Idempotent — safe to call
    /// before or after `start()`. Clamped to [0, 1].
    func setMasterVolume(_ value: Float) {
        let clamped = max(0, min(1, value))
        masterVolume = clamped
        // outputVolume is realtime-safe — AVAudioEngine ramps it to
        // the new target rather than snapping, so dragging the slider
        // doesn't click.
        preLimiterMixer.outputVolume = clamped
    }

    /// Read the current master volume (last persisted or set value).
    var currentMasterVolume: Float { masterVolume }

    /// Switch the current melodic program. Instant when MIDISynth is ready
    /// (sub-millisecond MIDI Program Change); blocks ~100 ms otherwise
    /// (sampler bank reload).
    func setMelodicProgram(_ program: UInt8) {
        currentMelodicProgram = program
        // Keep the AC GM node's program in sync so a note-on routed there
        // synthesizes the selected instrument. (Sounding voices keep their
        // own; this only affects future note-ons.)
        if gmSynthEnabled { gmSynth.setProgram(program) }
        // Leaving GarageBand mode: route melodic through MIDISynth/sampler
        // again. Reload the GM bank into `melodic` since the GB patch
        // load replaced its instrument data.
        usingGarageBandPatch = false
        // Picking any GM voice exits radio mode. Goes silent immediately
        // (master gate closed) but keeps the stream warm for the linger
        // window so a quick flip back avoids a reconnect.
        if usingRadioBackend {
            leaveRadioWithLinger()
        }
        // Same exit story for the sample backend — picking a GM voice
        // means "the voice picker takes over again."
        if usingSampleBackend {
            leaveSampleBackend()
        }
        if midiSynthReady, let au = midiSynth?.audioUnit {
            selectMelodicProgram(au, program: program)
            updateSamplerRoutingForActiveBackend()
            return
        }
        guard started else { return }
        connectMelodicSamplerIfNeeded()
        let url = MenuBandSynth.bankURL
        guard FileManager.default.fileExists(atPath: url.path) else { return }
        try? melodic.loadSoundBankInstrument(at: url, program: program, bankMSB: 0x79, bankLSB: 0)
    }

    // MARK: - GarageBand patch backend

    /// True while a GarageBand `.exs` patch is loaded into `melodic`. In
    /// that mode all melodic noteOn/Off route through the sampler instead
    /// of MIDISynth, even when MIDISynth is otherwise ready. Drum kit
    /// (channel 9) still uses MIDISynth so percussion keys stay GM.
    private(set) var usingGarageBandPatch: Bool = false

    /// Load a GarageBand `.exs` patch as the active melodic instrument.
    /// `loadInstrument(at:)` is synchronous on the calling thread but
    /// fast (typical 2–20 ms in our benchmark across the loadable patch
    /// set) so we don't bother with a delayed-playback dance.
    @discardableResult
    func setGarageBandPatch(at url: URL) -> Bool {
        guard started else { return false }
        do {
            try melodic.loadInstrument(at: url)
            usingGarageBandPatch = true
            // GB and radio are mutually exclusive — the GB sampler eats
            // the melodic note path that radio would otherwise take.
            if usingRadioBackend {
                leaveRadioWithLinger()
            }
            if usingSampleBackend {
                leaveSampleBackend()
            }
            updateSamplerRoutingForActiveBackend()
            return true
        } catch {
            NSLog("MenuBand: failed to load GB patch \(url.lastPathComponent): \(error)")
            return false
        }
    }

    // MARK: - KPBJ radio backend

    /// True while the live KPBJ stream is the active melodic source.
    /// Drum keys (channel 9) still go to GM — drums never go through
    /// the radio path.
    private(set) var usingRadioBackend: Bool = false

    /// Shared "leaving radio mode" helper used by setRadioBackend(false)
    /// and any other path that picks a non-radio voice. Closes the master
    /// gate immediately so the user hears silence the instant they pick
    /// a GM/GB voice, but keeps the AVPlayer streaming for the linger
    /// window so a quick flip back is instant (no reconnect).
    private func leaveRadioWithLinger() {
        usingRadioBackend = false
        radio.setOutputEnabled(false)
        radio.panic()
        radioLingerWorkItem?.cancel()
        let work = DispatchWorkItem { [weak self] in
            self?.radio.stopStreaming()
        }
        radioLingerWorkItem = work
        DispatchQueue.main.asyncAfter(deadline: .now() + radioLingerSeconds,
                                      execute: work)
    }

    /// Pending radio teardown. We keep the AVPlayer streaming for ~15 s
    /// after leaving voice −1 so toggling back in is instant — no
    /// reconnect, no buffer rewarm. Coming back within the window cancels
    /// this and reopens the master gate.
    private var radioLingerWorkItem: DispatchWorkItem?
    private let radioLingerSeconds: TimeInterval = 15.0

    /// Switch the active melodic source between the local synth and the
    /// live KPBJ stream. Enabling silences any in-flight sampler /
    /// MIDISynth notes so we don't double-trigger; disabling closes the
    /// radio's master gate immediately (no stuck audio when picking a GM
    /// voice mid-play) and schedules a 15 s teardown — so a quick flip
    /// back finds the stream still warm.
    func setRadioBackend(_ enabled: Bool) {
        if enabled {
            // Cancel any pending teardown — we're back in voice −1.
            radioLingerWorkItem?.cancel()
            radioLingerWorkItem = nil
            usingRadioBackend = true
            usingGarageBandPatch = false
            // Radio + sample are mutually exclusive — same melodic
            // note path.
            if usingSampleBackend {
                leaveSampleBackend()
            }
            for unit in [melodic, drums] {
                stopAllSamplerNotes(unit)
            }
            if midiSynthReady, let au = midiSynth?.audioUnit {
                for ch: UInt8 in 0..<16 {
                    sendMIDIEvent(au, status: 0xB0 | ch, data1: 123, data2: 0)
                }
            }
            if started {
                _ = resumeAudioEngineIfNeeded()
                radio.setOutputEnabled(true)
                radio.startStreaming() // idempotent if already running
            }
        } else {
            // Close the master gate immediately and start the linger
            // teardown — see `leaveRadioWithLinger` for details.
            leaveRadioWithLinger()
        }
    }

    /// Retune the radio backend to a different station. Safe to call while
    /// the radio is active (it reconnects) or inactive (takes effect when
    /// next enabled).
    func setRadioStation(_ station: RadioStation) {
        radio.setStation(station)
    }

    // MARK: - Third-party AU instrument backend

    /// Install (or clear) a third-party AU as the active melodic instrument.
    /// Pass `nil` to unload. The plugin sits on the same `preLimiterMixer`
    /// bus as every other backend, so master limiter + waveform tap apply.
    /// Mutually exclusive with radio/sample/GB — picking a plugin exits
    /// those, identical to `setMelodicProgram`'s mutex semantics.
    func setPluginInstrument(_ avUnit: AVAudioUnit?) {
        // Tear down any previously-loaded plugin first.
        if let prev = pluginUnit {
            if pluginConnected {
                engine.disconnectNodeOutput(prev)
                pluginConnected = false
            }
            engine.detach(prev)
            pluginUnit = nil
        }

        guard let avUnit = avUnit else {
            usingPluginInstrument = false
            updateSamplerRoutingForActiveBackend()
            return
        }

        // Same exit dance as setMelodicProgram — these backends share the
        // melodic note path with the plugin.
        usingGarageBandPatch = false
        if usingRadioBackend { leaveRadioWithLinger() }
        if usingSampleBackend { leaveSampleBackend() }

        engine.attach(avUnit)
        engine.connect(avUnit, to: preLimiterMixer, format: nil)
        pluginConnected = true
        pluginUnit = avUnit
        usingPluginInstrument = true
        // Drop the sampler off the bus while the plugin is the melodic
        // voice — drums (ch 9) still need their sampler/MIDISynth path.
        disconnectMelodicSamplerIfNeeded()
        if started {
            _ = resumeAudioEngineIfNeeded()
        }
    }

    /// Live AU handle so callers (e.g. the picker) can request its UI
    /// view controller via `auAudioUnit.requestViewController`.
    var pluginInstrument: AVAudioUnit? { pluginUnit }

    // MARK: - Sample voice recording control

    /// Begin recording into the sample voice's buffer. Wakes the audio
    /// engine first if it was suspended for idle, since the input-node
    /// tap won't deliver frames against a paused graph.
    func startSampleRecording(chromatic: Bool = false) { startSampleRecording(forKey: nil, chromatic: chromatic) }

    /// Start recording. `forKey` non-nil commits into that key's per-key slot
    /// (the ~+key gesture); nil records the global sample (the ` gesture).
    /// `chromatic` picks the global-sample playback mode: false = normal
    /// (C4 = raw), true = chromatic (pitch-corrected via detected fundamental).
    func startSampleRecording(forKey midi: UInt8?, chromatic: Bool = false) {
        guard started else { return }
        NSLog("MenuBand SampleVoice: synth startSampleRecording forKey=\(midi.map(String.init) ?? "global") chromatic=\(chromatic) (playbackEngineRunning=\(engine.isRunning))")
        _ = resumeAudioEngineIfNeeded()
        sampleRecordingActive = true
        sampleVoice.chromaticSample = chromatic
        sampleVoice.setOutputEnabled(false)
        sampleVoice.panic()
        sampleVoice.startRecording(forKey: midi)
    }

    /// Clear all per-key custom samples (the ` "Home" gesture).
    func clearPerKeySamples() { sampleVoice.clearPerKeySamples() }

    /// Stop recording. Returns true iff a usable buffer (≥100 ms) was
    /// captured; the caller flips the active backend to `.sample` only
    /// in that case.
    @discardableResult
    func stopSampleRecording() -> Bool {
        NSLog("MenuBand SampleVoice: synth stopSampleRecording")
        let ok = sampleVoice.stopRecording()
        sampleRecordingActive = false
        onSampleLevel?(0)
        if usingSampleBackend {
            sampleVoice.setOutputEnabled(true)
        }
        NSLog("MenuBand SampleVoice: synth stopSampleRecording result usable=\(ok)")
        scheduleIdleSuspendIfNeeded()
        return ok
    }

    /// Public read of the underlying sample voice's recording flag —
    /// used by the AppDelegate to drive the menubar icon's red
    /// "recording" tint.
    var sampleRecording: Bool { sampleRecordingActive || sampleVoice.isRecording }

    /// Forwarded RMS callback. Set by the AppDelegate so the menubar
    /// VU meter can pulse with the user's voice during recording. The
    /// underlying tap fires this on the main queue (see
    /// `MenuBandSampleVoice.ingestInput`).
    var onSampleLevel: ((Float) -> Void)? {
        get { sampleVoice.onLevel }
        set { sampleVoice.onLevel = newValue }
    }

    // MARK: - Sample voice backend

    /// True while microphone-recorded samples are the active melodic
    /// source. Drum keys (channel 9) still go to GM, identical to the
    /// radio-backend path.
    private(set) var usingSampleBackend: Bool = false

    /// True after the microphone sampler has captured a playable buffer.
    var hasSampleRecording: Bool { sampleVoice.hasRecording }

    /// Leave the sample backend, restoring whichever GM/sampler voice
    /// was last selected. Mirrors `leaveRadioWithLinger` minus the
    /// linger window — there's no expensive resource (network stream)
    /// to keep warm here, so we just close the gate and let the GM
    /// path take over.
    private func leaveSampleBackend() {
        guard usingSampleBackend else { return }
        usingSampleBackend = false
        sampleVoice.setOutputEnabled(false)
        sampleVoice.panic()
    }

    /// Switch the active melodic source between the local synth and
    /// the user-recorded sample voice. Enabling silences any in-flight
    /// MIDISynth / sampler notes so we don't double-trigger; disabling
    /// closes the master gate immediately.
    func setSampleBackend(_ enabled: Bool) {
        if enabled {
            usingSampleBackend = true
            usingGarageBandPatch = false
            // Sample + radio are mutually exclusive — same melodic
            // note path.
            if usingRadioBackend {
                leaveRadioWithLinger()
            }
            for unit in [melodic, drums] {
                stopAllSamplerNotes(unit)
            }
            if midiSynthReady, let au = midiSynth?.audioUnit {
                for ch: UInt8 in 0..<16 {
                    sendMIDIEvent(au, status: 0xB0 | ch, data1: 123, data2: 0)
                }
            }
            if started {
                _ = resumeAudioEngineIfNeeded()
                sampleVoice.setOutputEnabled(true)
            }
        } else {
            leaveSampleBackend()
        }
    }

    func stop() {
        engineLock.lock(); defer { engineLock.unlock() }
        guard started else { return }
        if let obs = configChangeObserver {
            NotificationCenter.default.removeObserver(obs)
            configChangeObserver = nil
        }
        idleSuspendWorkItem?.cancel()
        idleSuspendWorkItem = nil
        removeWaveformTapIfNeeded()
        // Release the device BEFORE stopping the engine so other apps
        // see the unhog event before they try to recover. Stopping the
        // engine first would leave a brief window where the device is
        // ours-but-idle, and Music.app etc. tend to give up retrying.
        releaseHogModeIfNeeded()
        engine.stop()
        started = false
        midiSynthReady = false
        waveformCaptureEnabled = false
        activeNotes.removeAll()
    }

    /// Send a CC#10 (pan) message on the given channel. Only takes
    /// effect when the MIDISynth backend is the audible path —
    /// AVAudioUnitSampler's pan is per-unit, not per-channel, so we
    /// no-op the sampler fallback rather than yanking the entire
    /// melodic mix.
    func setPan(_ pan: UInt8, channel: UInt8 = 0) {
        guard started, midiSynthReady, let au = midiSynth?.audioUnit else { return }
        sendMIDIEvent(au, status: 0xB0 | (channel & 0x0F),
                      data1: 10, data2: pan & 0x7F)
    }

    /// Fire a right-hand-split drum. `pan` is the 0–127 CC10 convention the
    /// keyboard uses (64 = center); converted to the −1…1 the percussion
    /// node wants.
    func playPercussion(_ drum: MenuBandPercussion.Drum, velocity: UInt8, pan: UInt8) {
        guard started else { return }
        // Drums don't register in `activeNotes`, so the idle-suspend logic
        // would otherwise leave the engine paused and the hit silent. Wake
        // it (and cancel any pending suspend) before staging the voices.
        _ = resumeAudioEngineIfNeeded()
        let p = max(-1.0, min(1.0, (Double(pan) - 64.0) / 63.0))
        percussion.play(drum, velocity: velocity, pan: p)
        // Keep the engine awake long enough for the drum tail to ring out,
        // since `activeNotes` stays empty. Re-arm the idle suspend timer so
        // it can't fire mid-tail.
        scheduleIdleSuspendAfterPercussion()
    }

    /// Key-down for a split drum. Returns a group token the caller passes to
    /// `percussionNoteOff` on key-up (hi-hat foot-pedal down/up sounds).
    @discardableResult
    func percussionNoteOn(_ drum: MenuBandPercussion.Drum, velocity: UInt8, pan: UInt8,
                          accent: Bool = false) -> UInt64 {
        guard started else { return 0 }
        // Warm path (split armed): the engine is already running and won't
        // idle-suspend, so skip the resume lock AND the idle reschedule —
        // the only work on a hit is staging the voices. Cold path keeps the
        // safety net for cues fired while the split is off.
        if keepEngineWarm {
            if !engine.isRunning { _ = resumeAudioEngineIfNeeded() }
        } else {
            _ = resumeAudioEngineIfNeeded()
        }
        let p = max(-1.0, min(1.0, (Double(pan) - 64.0) / 63.0))
        let g = percussion.noteOn(drum, velocity: velocity, pan: p, accent: accent)
        if !keepEngineWarm { scheduleIdleSuspendAfterPercussion() }
        return g
    }

    /// Most recent percussion trigger→render handoff (ms) — the drum-
    /// specific latency, ≤ one IO buffer.
    func percussionTriggerHandoffMs() -> Double { percussion.triggerHandoffMs() }

    /// Key-up for a split drum — damps held voices (open hat) and fires the
    /// release burst (closed hat). Harmless for one-shot drums.
    func percussionNoteOff(_ group: UInt64) {
        guard started, group != 0 else { return }
        percussion.noteOff(group)
        if !keepEngineWarm { scheduleIdleSuspendAfterPercussion() }
    }

    /// Live per-pitch-class drum hit pulses, for the menubar key vibe.
    func percussionPulses() -> [MenuBandPercussion.DrumPulse] {
        percussion.pulseSnapshot()
    }

    /// Re-arm the hidden-idle suspend after a drum hit so the engine stays
    /// up through the tail (crashes ring ~1.4 s) instead of pausing the
    /// instant the staging call returns.
    private func scheduleIdleSuspendAfterPercussion() {
        idleSuspendWorkItem?.cancel()
        let workItem = DispatchWorkItem { [weak self] in
            self?.suspendAudioEngineForHiddenIdleIfNeeded()
        }
        idleSuspendWorkItem = workItem
        DispatchQueue.main.asyncAfter(deadline: .now() + max(idleSuspendDelay, 2.0),
                                      execute: workItem)
    }

    func noteOn(_ midi: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        guard started else { return }
        guard resumeAudioEngineIfNeeded() else { return }
        // Self-heal after a device switch: reload the soundbank before the
        // first note so it can't sound as the default sine.
        reassertMIDISynthBankIfDirty()
        activeNotes.insert(noteKey(midi, channel: channel))
        // Plugin instrument wins on melodic — picked deliberately via the
        // About → Plugins picker, so it overrides every other melodic
        // backend. Drums (ch 9) still flow to GM below.
        if usingPluginInstrument && channel != 9, let au = pluginUnit?.audioUnit {
            sendMIDIEvent(au, status: 0x90 | (channel & 0x0F), data1: midi, data2: velocity)
            return
        }
        // Radio backend takes melodic ahead of GM/sampler/MIDISynth.
        // Drums still pass through to GM below — the KPBJ pads are a
        // melodic-only voice.
        if usingRadioBackend && channel != 9 {
            radio.noteOn(midi, velocity: velocity, channel: channel)
            return
        }
        // Sample backend — same melodic-only routing semantics as
        // radio. Drums always continue down to the GM path.
        if usingSampleBackend && channel != 9 {
            // Per-key/global sample plays it; if this key has no sample, fall
            // through to the GM instrument (hybrid kit — instruments per key).
            if sampleVoice.noteOn(midi, velocity: velocity, channel: channel) { return }
        }
        // Drums (channel 9) always route through MIDISynth/drums sampler
        // — drum kits are GM regardless of melodic backend choice.
        if channel == 9 {
            if midiSynthReady, let au = midiSynth?.audioUnit {
                sendMIDIEvent(au, status: 0x99, data1: midi, data2: velocity)
                return
            }
            connectDrumsSamplerIfNeeded()
            drums.startNote(midi, withVelocity: velocity, onChannel: 0)
            return
        }
        // Melodic — sampler if a GB patch is loaded, MIDISynth if ready,
        // sampler-with-DLS otherwise.
        if usingGarageBandPatch {
            connectMelodicSamplerIfNeeded()
            melodic.startNote(midi, withVelocity: velocity, onChannel: 0)
            return
        }
        // AC OS MIDI: when the toggle is on and gm_synth implements the
        // current program, synthesize the note with the real AC voices and
        // do NOT also fire MIDISynth. `noteOn` returns false if the program
        // turns out unimplemented (defensive) so we fall through to the
        // soundfont path for it.
        if gmRoutable(currentMelodicProgram),
           gmSynth.noteOn(midi, velocity: velocity, channel: channel,
                          program: currentMelodicProgram) {
            gmRoutedNotes.insert(noteKey(midi, channel: channel))
            return
        }
        if midiSynthReady, let au = midiSynth?.audioUnit {
            // Honor the channel argument so same-note voices on
            // different channels stay independent (essential for the
            // doppler retrigger tail to layer with a fresh live press).
            sendMIDIEvent(au, status: 0x90 | (channel & 0x0F), data1: midi, data2: velocity)
            return
        }
        connectMelodicSamplerIfNeeded()
        melodic.startNote(midi, withVelocity: velocity, onChannel: 0)
    }

    /// Send a 14-bit pitch-bend value to the active synth on the
    /// given channel. `value` is signed: -8192 (full down) to +8191
    /// (full up); 0 = center. Only the MIDISynth path supports
    /// pitch-bend natively; the AVAudioUnitSampler fallback is a
    /// no-op (and the trackpad gesture caller checks midiMode).
    func sendPitchBend(value: Int16, channel: UInt8 = 0) {
        guard started else { return }
        let v = max(-8192, min(8191, Int(value))) + 8192   // 0…16383
        let lsb = UInt8(v & 0x7F)
        let msb = UInt8((v >> 7) & 0x7F)
        let status: UInt8 = 0xE0 | (channel & 0x0F)
        if midiSynthReady, let au = midiSynth?.audioUnit {
            sendMIDIEvent(au, status: status, data1: lsb, data2: msb)
        }
    }

    /// Trackpad pitch-bend passthrough for the sample voice backend.
    /// `amount` is the same -1...+1 signed value the controller
    /// hands to `sendPitchBend` — sample voice multiplies it by one
    /// octave (12 semitones) to drive its AVAudioUnitTimePitch nodes'
    /// `.pitch` (cents). AVAudio's time-pitch doesn't respond to MIDI
    /// pitch-bend so we have to route this signal in-process.
    func setSamplePitchBend(amount: Float) {
        sampleVoice.setBend(amount: amount)
    }

    /// Route the trackpad pitch-bend into the internally-synthesized
    /// drum kit so tonal drums warp with the melodic voices.
    func setPercussionPitchBend(amount: Float) {
        percussion.setPitchBend(amount: amount)
    }

    /// Route the trackpad pitch-bend into the AC GM synth. gm_synth
    /// re-derives its phase increments from the live frequency we pass each
    /// sample, so (like the varispeed backends) it doesn't see MIDI
    /// pitch-bend — we route the signed amount in-process instead.
    func setGMPitchBend(amount: Float) {
        if gmSynthEnabled { gmSynth.setPitchBend(amount: amount) }
    }

    /// Route the trackpad pitch-bend into the KPBJ radio backend. Like
    /// the sample voice, the radio plays through AVAudioUnitVarispeed and
    /// ignores MIDI pitch-bend, so the signed amount is routed in-process
    /// — the live stream slides in pitch alongside every other voice.
    func setRadioPitchBend(amount: Float) {
        radio.setBend(amount: amount)
    }

    /// Per-channel Expression (CC 11), 0–127. Used by the linger
    /// fade so sustained patches (organ, pad, brass) ramp down
    /// audibly over the tail instead of holding at full volume and
    /// then snapping silent at the cleanup noteOff.
    func sendExpression(value: UInt8, channel: UInt8 = 0) {
        guard started, midiSynthReady, let au = midiSynth?.audioUnit else { return }
        sendMIDIEvent(au, status: 0xB0 | (channel & 0x0F),
                      data1: 11, data2: value & 0x7F)
    }

    /// Per-voice volume hook for the sample backend. Used by the
    /// linger fade — CC 11 doesn't reach AVAudioPlayerNode, so we
    /// ramp the per-note player volume directly.
    func setSampleNoteVolume(midi: UInt8, channel: UInt8, value: UInt8) {
        sampleVoice.setNoteVolume(midi: midi, channel: channel, value: value)
    }

    /// Snap every sample voice on `channel` back to full volume so a
    /// new noteOn isn't dragged under by a leftover linger fade.
    func resetSampleChannelVolumes(channel: UInt8) {
        sampleVoice.resetChannelVolumes(channel: channel)
    }

    func noteOff(_ midi: UInt8, channel: UInt8 = 0) {
        guard started else { return }
        let key = noteKey(midi, channel: channel)
        activeNotes.remove(key)
        defer { scheduleIdleSuspendIfNeeded() }
        // Release an AC GM voice if this exact note was routed there on
        // press — independent of the current toggle / program so it can't
        // hang or steal MIDISynth's note-off.
        if gmRoutedNotes.remove(key) != nil {
            gmSynth.noteOff(midi, channel: channel)
            return
        }
        if usingPluginInstrument && channel != 9, let au = pluginUnit?.audioUnit {
            sendMIDIEvent(au, status: 0x80 | (channel & 0x0F), data1: midi)
            return
        }
        if usingRadioBackend && channel != 9 {
            radio.noteOff(midi, channel: channel)
            return
        }
        if usingSampleBackend && channel != 9 {
            // Release the sample voice, then DON'T return — fall through to
            // also send a GM note-off, so keys that fell back to GM (no sample)
            // don't get stuck. Both are no-ops for notes they don't own.
            sampleVoice.noteOff(midi, channel: channel)
        }
        if channel == 9 {
            if midiSynthReady, let au = midiSynth?.audioUnit {
                sendMIDIEvent(au, status: 0x89, data1: midi)
                return
            }
            drums.stopNote(midi, onChannel: 0)
            return
        }
        if usingGarageBandPatch {
            melodic.stopNote(midi, onChannel: 0)
            return
        }
        if midiSynthReady, let au = midiSynth?.audioUnit {
            sendMIDIEvent(au, status: 0x80 | (channel & 0x0F), data1: midi)
            return
        }
        melodic.stopNote(midi, onChannel: 0)
    }

    func panic() {
        guard started else { return }
        activeNotes.removeAll()
        gmRoutedNotes.removeAll()
        if gmSynthEnabled { gmSynth.panic() }
        defer { scheduleIdleSuspendIfNeeded() }
        if midiSynthReady, let au = midiSynth?.audioUnit {
            for ch: UInt8 in 0..<16 {
                // CC 123 = All Notes Off.
                sendMIDIEvent(au, status: 0xB0 | ch, data1: 123, data2: 0)
            }
        }
        for unit in [melodic, drums] {
            stopAllSamplerNotes(unit)
        }
        if usingPluginInstrument, let au = pluginUnit?.audioUnit {
            for ch: UInt8 in 0..<16 {
                sendMIDIEvent(au, status: 0xB0 | ch, data1: 123, data2: 0)
            }
        }
        radio.panic()
        sampleVoice.panic()
    }
}
