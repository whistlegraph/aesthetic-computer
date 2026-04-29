import Foundation
import AVFoundation
import AudioToolbox

/// Built-in soft-synth backed by Apple's multi-timbral MIDI synth audio unit
/// (`kAudioUnitSubType_MIDISynth`) with the GS DLS bank loaded once.
///
/// Why not AVAudioUnitSampler? Sampler is monotimbral — switching programs
/// requires `loadSoundBankInstrument`, a ~100 ms blocking call that re-parses
/// the .dls file. That latency was killing the popover's "drag across the
/// instrument grid to browse" interaction: each cell-cross swapped the bank,
/// the next note had to wait for the swap, and the user heard stuttering.
///
/// MIDISynth is multi-timbral: 16 simultaneous programs, one per MIDI channel.
/// We assign channels 0–14 to recently-touched melodic programs (LRU) and
/// channel 9 to the GM drum kit. Switching the user's "current" program is a
/// MIDI Program Change message — sub-millisecond. We also pre-warm every
/// program at startup via `kMusicDeviceProperty_BankPreload` so the first
/// note on any channel never blocks on a sample-data load.
final class MenuBandSynth {
    private let engine = AVAudioEngine()
    private var synth: AVAudioUnitMIDIInstrument!
    private var started = false

    /// Per-channel current program, kept in sync with the actual MIDI state
    /// of the AU. Channel 9 is fixed to the drum kit.
    private var channelProgram: [UInt8: UInt8] = [:]
    /// LRU order of channels for melodic-channel rotation. Front = most
    /// recently assigned. We avoid stomping a channel that's still holding
    /// a recent note, so quickly tapping two different programs in the
    /// instrument grid plays both their notes audibly through release.
    private var melodicChannelLRU: [UInt8] = Array(0..<9) + Array(10..<16)
    private let stateLock = NSLock()

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
        let desc = AudioComponentDescription(
            componentType: kAudioUnitType_MusicDevice,
            componentSubType: kAudioUnitSubType_MIDISynth,
            componentManufacturer: kAudioUnitManufacturer_Apple,
            componentFlags: 0,
            componentFlagsMask: 0
        )
        let unit = AVAudioUnitMIDIInstrument(audioComponentDescription: desc)
        synth = unit
        engine.attach(unit)
        engine.connect(unit, to: engine.mainMixerNode, format: nil)

        // Sound-bank URL must be set BEFORE engine.start(). The property
        // expects a CFURLRef; passing a Swift `URL` directly hits the AU
        // as a `_SwiftURL` and fails the NSURL selector dispatch inside
        // CoreAudio (you'll see "does not implement -baseURL"). Cast to
        // CFURL so the AU sees a proper toll-free-bridged NSURL.
        var bankURL: CFURL = MenuBandSynth.bankURL as CFURL
        let bankStatus = withUnsafePointer(to: &bankURL) { ptr -> OSStatus in
            AudioUnitSetProperty(
                unit.audioUnit,
                AudioUnitPropertyID(kMusicDeviceProperty_SoundBankURL),
                kAudioUnitScope_Global,
                0,
                ptr,
                UInt32(MemoryLayout<CFURL>.size)
            )
        }
        if bankStatus != noErr {
            NSLog("MenuBand: SoundBankURL set failed status=\(bankStatus)")
        }

        engine.prepare()

        // Apple's MIDISynth preload protocol (per CoreAudio docs): set the
        // EnableLoadPreset property to 1, then send a Program Change for
        // every (bank, program) combination you intend to use. Each PC
        // triggers a foreground load of that program's samples *before*
        // we start the engine. Once preloaded, channel-program switches
        // at runtime are sub-millisecond — no disk I/O, no DSP rebuild.
        // After preloading, set EnableLoadPreset back to 0 and start the
        // engine; subsequent PCs are now treated as instant switches.
        setEnablePreload(true)
        for p: UInt8 in 0...127 {
            synth.sendProgramChange(p, bankMSB: 0x79, bankLSB: 0, onChannel: 0)
        }
        synth.sendProgramChange(0, bankMSB: 0x78, bankLSB: 0, onChannel: 9)
        setEnablePreload(false)

        do {
            try engine.start()
            started = true
        } catch {
            NSLog("MenuBand synth engine start failed: \(error)")
            return
        }

        configureChannels()
        primeForLowLatency()
        installWaveformTap()
    }

    /// Toggle MIDISynth's preload mode. While enabled, Program Change events
    /// synchronously load that preset; while disabled (the default after
    /// startup), PCs only switch the channel's current program from already-
    /// loaded presets.
    private func setEnablePreload(_ enable: Bool) {
        var flag: UInt32 = enable ? 1 : 0
        let status = AudioUnitSetProperty(
            synth.audioUnit,
            AudioUnitPropertyID(kAUMIDISynthProperty_EnablePreload),
            kAudioUnitScope_Global,
            0,
            &flag,
            UInt32(MemoryLayout<UInt32>.size)
        )
        if status != noErr {
            NSLog("MenuBand: EnablePreload(\(enable)) status=\(status)")
        }
    }

    /// Reset every melodic channel to program 0 (acoustic grand) and the
    /// drum channel to the standard kit. After this, channelProgram is in
    /// sync with the AU's actual MIDI state. Runs after engine.start() so
    /// PCs are treated as instant program switches (no preload reload).
    private func configureChannels() {
        for ch in 0..<16 where ch != 9 {
            synth.sendProgramChange(0, bankMSB: 0x79, bankLSB: 0, onChannel: UInt8(ch))
            channelProgram[UInt8(ch)] = 0
        }
        synth.sendProgramChange(0, bankMSB: 0x78, bankLSB: 0, onChannel: 9)
        channelProgram[9] = 0
    }

    /// Tap the engine's main mixer so the WaveformView gets a live picture
    /// of whatever the synth is producing. 256 frames ≈ 5.8 ms at 44.1 kHz —
    /// small buffer = fresher samples for the visualizer.
    private func installWaveformTap() {
        let mixer = engine.mainMixerNode
        let format = mixer.outputFormat(forBus: 0)
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

    /// Copy the most recent `dest.count` samples into `dest`, oldest first.
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

    /// Velocity-1 warmup notes across the range so the AU's render thread
    /// has its DSP buffers primed before the user's first real tap.
    private func primeForLowLatency() {
        let melodicWarmup: [UInt8] = [60, 64, 67, 72]
        let drumWarmup: [UInt8] = [36, 38, 42, 46]
        for n in melodicWarmup {
            synth.startNote(n, withVelocity: 1, onChannel: 0)
        }
        for n in drumWarmup {
            synth.startNote(n, withVelocity: 1, onChannel: 9)
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) { [weak self] in
            guard let self = self else { return }
            for n in melodicWarmup { self.synth.stopNote(n, onChannel: 0) }
            for n in drumWarmup { self.synth.stopNote(n, onChannel: 9) }
        }
    }

    /// Switch the *current* melodic program on channel 0. With the bank
    /// preloaded, this is a single MIDI Program Change message — no file
    /// I/O, no DSP rebuild. Returns immediately. The next noteOn on
    /// channel 0 will play the new program.
    func setMelodicProgram(_ program: UInt8) {
        guard started else { return }
        stateLock.lock()
        channelProgram[0] = program
        stateLock.unlock()
        synth.sendProgramChange(program, bankMSB: 0x79, bankLSB: 0, onChannel: 0)
    }

    func stop() {
        guard started else { return }
        engine.stop()
        started = false
    }

    func noteOn(_ midi: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        guard started else { return }
        synth.startNote(midi, withVelocity: velocity, onChannel: channel)
    }

    func noteOff(_ midi: UInt8, channel: UInt8 = 0) {
        guard started else { return }
        synth.stopNote(midi, onChannel: channel)
    }

    func panic() {
        guard started else { return }
        // All-notes-off CC 123 on every channel.
        for ch: UInt8 in 0..<16 {
            synth.sendController(123, withValue: 0, onChannel: ch)
            for note: UInt8 in 0...127 {
                synth.stopNote(note, onChannel: ch)
            }
        }
    }
}
