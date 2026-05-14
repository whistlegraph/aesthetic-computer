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
    /// then plays it back as a varispeed-pitched piano voice. Same
    /// melodic-only routing semantics as the radio backend (channel 9
    /// drums always pass through to GM).
    private let sampleVoice = MenuBandSampleVoice()
    /// Sums every backend before the limiter so simultaneous voices share one
    /// gain stage. Without this, each backend would feed `mainMixerNode`
    /// directly and a chord across melodic + drums + midiSynth could exceed
    /// 0 dBFS at the output (audible clipping/crackle).
    private let preLimiterMixer = AVAudioMixerNode()
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
    /// Audio I/O frames the device renders per cycle. macOS uses
    /// `max(all clients)` in shared mode, so on a real system this
    /// request is observed but typically ignored (something else
    /// holds the device at 512). Kept for the rare case where Menu
    /// Band IS the sole client — then the device drops to ~1.3 ms
    /// at 96 kHz × 128 frames.
    private static let targetIOBufferFrames: UInt32 = 128
    /// True once we've successfully taken hog mode on the active
    /// output device. Tracks the device ID alongside so we can
    /// release the right one on shutdown / device switch.
    private var hogModeDeviceID: AudioDeviceID = 0
    private var hogModeAcquired = false
    /// True once MIDISynth has loaded its bank, preloaded all programs,
    /// and successfully attached to the engine. `noteOn` and
    /// `setMelodicProgram` route through the MIDISynth when this flips.
    private(set) var midiSynthReady = false

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

    // Tap-driven ring buffer for the popover's live waveform display.
    private static let waveformRingSize = 4096
    private var waveformRing = [Float](repeating: 0, count: waveformRingSize)
    private var waveformWriteIdx: Int = 0
    private let waveformLock = NSLock()
    private var waveformTapInstalled = false

    /// Apple's DLS bank — present on every macOS install since 10.x.
    private static let bankURL = URL(
        fileURLWithPath: "/System/Library/Components/CoreAudio.component/Contents/Resources/gs_instruments.dls"
    )

    func start() {
        guard !started else { return }
        engine.attach(preLimiterMixer)
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
        // Apply the persisted master volume now that preLimiterMixer is
        // attached + connected (setting outputVolume before attach is a
        // silent no-op on AVAudioMixerNode).
        preLimiterMixer.outputVolume = masterVolume

        // Try to bring up the multi-timbral MIDISynth in the background.
        // If it works, we'll route notes through it for instant program
        // switching. If it doesn't, the user keeps the sampler fallback.
        startMIDISynthBackend()
        scheduleIdleSuspendIfNeeded()
    }

    // MARK: - MIDISynth (multi-timbral, instant switching)

    private func startMIDISynthBackend() {
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
                self.configureMIDISynth(avUnit)
            }
        }
    }

    private func configureMIDISynth(_ avUnit: AVAudioUnit) {
        let au = avUnit.audioUnit

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

        // 3. Load just the programs we need. MIDISynth requires
        //    EnablePreload(true) → PC → EnablePreload(false) to actually
        //    fault each program's samples in from the DLS bank; without
        //    that, the bank URL is set but no instruments load and noteOn
        //    is silent. The earlier full 128-program sweep used the same
        //    mechanism but at startup cost; we now load programs lazily on
        //    first use via setMelodicProgram.
        selectMelodicProgram(au, program: currentMelodicProgram)
        selectDrumKit(au)

        midiSynth = avUnit
        midiSynthReady = true
        updateSamplerRoutingForActiveBackend()
        scheduleIdleSuspendIfNeeded()
        NSLog("MenuBand: MIDISynth ready — instant program switching enabled")

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
        engine.connect(preLimiterMixer, to: limiter, format: nil)
        engine.connect(limiter, to: engine.mainMixerNode, format: nil)
        let au = limiter.audioUnit
        // Fast attack catches chord/transient peaks; medium release avoids
        // pumping on sustained notes. Pre-gain stays at 0 so quiet input
        // doesn't get squashed into the limiter unnecessarily.
        AudioUnitSetParameter(au, kLimiterParam_AttackTime,
                              kAudioUnitScope_Global, 0, 0.002, 0)
        AudioUnitSetParameter(au, kLimiterParam_DecayTime,
                              kAudioUnitScope_Global, 0, 0.050, 0)
        AudioUnitSetParameter(au, kLimiterParam_PreGain,
                              kAudioUnitScope_Global, 0, 0.0, 0)
        limiterConnected = true
    }

    private func connectMelodicSamplerIfNeeded() {
        guard !melodicConnected else { return }
        engine.connect(melodic, to: preLimiterMixer, format: nil)
        melodicConnected = true
    }

    private func disconnectMelodicSamplerIfNeeded() {
        guard melodicConnected else { return }
        stopAllSamplerNotes(melodic)
        engine.disconnectNodeOutput(melodic)
        melodicConnected = false
    }

    private func connectDrumsSamplerIfNeeded() {
        guard !drumsConnected else { return }
        engine.connect(drums, to: preLimiterMixer, format: nil)
        drumsConnected = true
    }

    private func disconnectDrumsSamplerIfNeeded() {
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
        idleSuspendWorkItem?.cancel()
        idleSuspendWorkItem = nil
        if engine.isRunning {
            return true
        }
        do {
            try engine.start()
            applyOutputDeviceOverride()
            return true
        } catch {
            NSLog("MenuBand synth engine resume failed: \(error)")
            return false
        }
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
        idleSuspendWorkItem = nil
        guard started, engine.isRunning, !waveformCaptureEnabled, activeNotes.isEmpty,
              !sampleRecordingActive else { return }
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

    /// Set of (bankMSB << 8 | program) keys we've already faulted in. The
    /// EnablePreload dance only needs to run once per (bank, program) — once
    /// loaded, subsequent program changes to the same slot are instant.
    private var loadedPrograms: Set<UInt16> = []

    /// Select a melodic program on channel 0. First time a (bank, program)
    /// is touched we wrap the bank-select + PC in EnablePreload so MIDISynth
    /// actually faults the instrument's samples in from the DLS bank.
    /// Subsequent calls for an already-loaded program skip the preload
    /// toggle and just send the bank-select + PC for instant switching.
    /// CC0 = bank MSB (0x79 GM Melodic), CC32 = bank LSB (0).
    private func selectMelodicProgram(_ au: AudioUnit, program: UInt8) {
        let key: UInt16 = (UInt16(0x79) << 8) | UInt16(program)
        let needsLoad = !loadedPrograms.contains(key)
        if needsLoad { setMIDISynthPreload(au, enable: true) }
        // Broadcast bank+PC to all 8 melodic channels (0-7). The
        // controller's round-robin voice allocator (live 0-3, doppler
        // 4-7) needs every channel preloaded with the chosen voice so
        // a noteOn lands on the user's instrument no matter which
        // channel it picked. Channel 9 is reserved for drums; 8 and
        // 10-15 are unused. Without this loop only ch0 played the
        // selected voice, which collapsed all "rotation" back to a
        // single channel and let same-note retriggers cut each other.
        for ch: UInt8 in 0..<8 {
            sendMIDIEvent(au, status: 0xB0 | ch, data1: 0,  data2: 0x79)
            sendMIDIEvent(au, status: 0xB0 | ch, data1: 32, data2: 0x00)
            sendMIDIEvent(au, status: 0xC0 | ch, data1: program)
        }
        if needsLoad {
            setMIDISynthPreload(au, enable: false)
            loadedPrograms.insert(key)
            // After preload-disable the AU's *active* program is still
            // unset on each channel — re-send PC so noteOn lands on the
            // freshly loaded instrument instead of silence.
            for ch: UInt8 in 0..<8 {
                sendMIDIEvent(au, status: 0xC0 | ch, data1: program)
            }
        }
    }

    /// Select the standard GM drum kit on channel 9 (bank MSB 0x78).
    private func selectDrumKit(_ au: AudioUnit) {
        let key: UInt16 = (UInt16(0x78) << 8) | 0
        let needsLoad = !loadedPrograms.contains(key)
        if needsLoad { setMIDISynthPreload(au, enable: true) }
        sendMIDIEvent(au, status: 0xB9, data1: 0,  data2: 0x78)  // CC0  bank MSB
        sendMIDIEvent(au, status: 0xB9, data1: 32, data2: 0x00)  // CC32 bank LSB
        sendMIDIEvent(au, status: 0xC9, data1: 0)                // PC   kit 0
        if needsLoad {
            setMIDISynthPreload(au, enable: false)
            loadedPrograms.insert(key)
            sendMIDIEvent(au, status: 0xC9, data1: 0)
        }
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
        let status = AudioUnitSetProperty(
            au, kAudioOutputUnitProperty_CurrentDevice,
            kAudioUnitScope_Global, 0,
            &deviceID, UInt32(MemoryLayout<AudioDeviceID>.size))
        if status != noErr {
            NSLog("MenuBand: output device override failed: \(status)")
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

        var target = Self.targetIOBufferFrames
        if rangeStatus == noErr {
            let lo = UInt32(range.mMinimum)
            let hi = UInt32(range.mMaximum)
            target = max(lo, min(hi, target))
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
        } else {
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
    func startSampleRecording() {
        guard started else { return }
        NSLog("MenuBand SampleVoice: synth startSampleRecording (playbackEngineRunning=\(engine.isRunning))")
        _ = resumeAudioEngineIfNeeded()
        sampleRecordingActive = true
        sampleVoice.setOutputEnabled(false)
        sampleVoice.panic()
        sampleVoice.startRecording()
    }

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
        guard started else { return }
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

    func noteOn(_ midi: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        guard started else { return }
        guard resumeAudioEngineIfNeeded() else { return }
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
            NSLog("MenuBand SampleVoice: routing noteOn to sample midi=\(midi) channel=\(channel)")
            sampleVoice.noteOn(midi, velocity: velocity, channel: channel)
            return
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
    /// hands to `sendPitchBend` — sample voice multiplies it by ±2
    /// semitones to drive its AVAudioUnitVarispeed nodes. AVAudio's
    /// varispeed doesn't respond to MIDI pitch-bend so we have to
    /// route this signal in-process.
    func setSamplePitchBend(amount: Float) {
        sampleVoice.setBend(amount: amount)
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
        activeNotes.remove(noteKey(midi, channel: channel))
        defer { scheduleIdleSuspendIfNeeded() }
        if usingPluginInstrument && channel != 9, let au = pluginUnit?.audioUnit {
            sendMIDIEvent(au, status: 0x80 | (channel & 0x0F), data1: midi)
            return
        }
        if usingRadioBackend && channel != 9 {
            radio.noteOff(midi, channel: channel)
            return
        }
        if usingSampleBackend && channel != 9 {
            sampleVoice.noteOff(midi, channel: channel)
            return
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
