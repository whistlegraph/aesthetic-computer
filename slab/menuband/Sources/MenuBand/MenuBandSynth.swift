import Foundation
import AVFoundation
import AudioToolbox

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
    private var started = false
    /// True once MIDISynth has loaded its bank, preloaded all programs,
    /// and successfully attached to the engine. `noteOn` and
    /// `setMelodicProgram` route through the MIDISynth when this flips.
    private(set) var midiSynthReady = false

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

    /// Apple's DLS bank — present on every macOS install since 10.x.
    private static let bankURL = URL(
        fileURLWithPath: "/System/Library/Components/CoreAudio.component/Contents/Resources/gs_instruments.dls"
    )

    func start() {
        guard !started else { return }
        engine.attach(melodic)
        engine.attach(drums)
        engine.connect(melodic, to: engine.mainMixerNode, format: nil)
        engine.connect(drums, to: engine.mainMixerNode, format: nil)
        engine.prepare()
        do {
            try engine.start()
            started = true
        } catch {
            NSLog("MenuBand synth engine start failed: \(error)")
            return
        }
        loadDefaultPatches()
        primeForLowLatency()
        installWaveformTap()

        // Try to bring up the multi-timbral MIDISynth in the background.
        // If it works, we'll route notes through it for instant program
        // switching. If it doesn't, the user keeps the sampler fallback.
        startMIDISynthBackend()
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

        // 2. Attach + connect to the engine. Engine is already running so
        //    this is hot-attach; AVAudioEngine handles graph reconfig.
        engine.attach(avUnit)
        engine.connect(avUnit, to: engine.mainMixerNode, format: nil)

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
        NSLog("MenuBand: MIDISynth ready — instant program switching enabled")

        // The sampler fallback is no longer needed for melodic playback —
        // disconnect it to avoid double-triggering. Keep `drums` connected
        // since the MIDISynth's drum-kit voice is on channel 9 of the same
        // unit, so we'll just stop sending drum notes through `drums`.
        // (Leaving the nodes attached but unrouted is safe.)
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
        sendMIDIEvent(au, status: 0xB0, data1: 0,  data2: 0x79)  // CC0  bank MSB
        sendMIDIEvent(au, status: 0xB0, data1: 32, data2: 0x00)  // CC32 bank LSB
        sendMIDIEvent(au, status: 0xC0, data1: program)          // PC   program
        if needsLoad {
            setMIDISynthPreload(au, enable: false)
            loadedPrograms.insert(key)
            // After preload-disable, the AU's *active* program is still
            // unset on this channel — re-send the PC so noteOn lands on the
            // freshly loaded instrument instead of silence.
            sendMIDIEvent(au, status: 0xC0, data1: program)
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

    // MARK: - Audio tap for visualizer

    private func installWaveformTap() {
        let mixer = engine.mainMixerNode
        let format = mixer.outputFormat(forBus: 0)
        // 256 frames ≈ 5.8 ms at 44.1 kHz — small buffer = fresh samples
        // for the visualizer without burning the audio thread.
        mixer.installTap(onBus: 0, bufferSize: 256, format: format) { [weak self] buffer, _ in
            self?.ingestWaveformBuffer(buffer)
        }
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

    /// Switch the current melodic program. Instant when MIDISynth is ready
    /// (sub-millisecond MIDI Program Change); blocks ~100 ms otherwise
    /// (sampler bank reload).
    func setMelodicProgram(_ program: UInt8) {
        currentMelodicProgram = program
        // Leaving GarageBand mode: route melodic through MIDISynth/sampler
        // again. Reload the GM bank into `melodic` since the GB patch
        // load replaced its instrument data.
        usingGarageBandPatch = false
        if midiSynthReady, let au = midiSynth?.audioUnit {
            selectMelodicProgram(au, program: program)
            return
        }
        guard started else { return }
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
            return true
        } catch {
            NSLog("MenuBand: failed to load GB patch \(url.lastPathComponent): \(error)")
            return false
        }
    }

    func stop() {
        guard started else { return }
        engine.stop()
        started = false
        midiSynthReady = false
    }

    func noteOn(_ midi: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        guard started else { return }
        // Drums (channel 9) always route through MIDISynth/drums sampler
        // — drum kits are GM regardless of melodic backend choice.
        if channel == 9 {
            if midiSynthReady, let au = midiSynth?.audioUnit {
                sendMIDIEvent(au, status: 0x99, data1: midi, data2: velocity)
                return
            }
            drums.startNote(midi, withVelocity: velocity, onChannel: 0)
            return
        }
        // Melodic — sampler if a GB patch is loaded, MIDISynth if ready,
        // sampler-with-DLS otherwise.
        if usingGarageBandPatch {
            melodic.startNote(midi, withVelocity: velocity, onChannel: 0)
            return
        }
        if midiSynthReady, let au = midiSynth?.audioUnit {
            sendMIDIEvent(au, status: 0x90, data1: midi, data2: velocity)
            return
        }
        melodic.startNote(midi, withVelocity: velocity, onChannel: 0)
    }

    func noteOff(_ midi: UInt8, channel: UInt8 = 0) {
        guard started else { return }
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
            sendMIDIEvent(au, status: 0x80, data1: midi)
            return
        }
        melodic.stopNote(midi, onChannel: 0)
    }

    func panic() {
        guard started else { return }
        if midiSynthReady, let au = midiSynth?.audioUnit {
            for ch: UInt8 in 0..<16 {
                // CC 123 = All Notes Off.
                sendMIDIEvent(au, status: 0xB0 | ch, data1: 123, data2: 0)
            }
            return
        }
        for unit in [melodic, drums] {
            for note: UInt8 in 0...127 { unit.stopNote(note, onChannel: 0) }
        }
    }
}
