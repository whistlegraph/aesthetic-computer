import Foundation
import AVFoundation

// Built-in soft-synth using Apple's bundled GM DLS sound bank
// (`gs_instruments.dls`, shipped with every macOS install inside
// CoreAudio.component). Real piano and drum-kit samples — not the bare
// AVAudioUnitSampler default, which is a sine-ish placeholder.
//
// Two samplers are kept: one melodic (channel 0 — piano by default), one
// percussion (channel 9 — GM drum kit). MenuBandController routes drum notes
// to the drum sampler.
final class MenuBandSynth {
    private let engine = AVAudioEngine()
    private let melodic = AVAudioUnitSampler()
    private let drums = AVAudioUnitSampler()
    private var started = false

    // Tap-driven ring buffer for the popover's live waveform display. The
    // tap fires on the audio render thread and writes here; the main thread
    // reads via `snapshotWaveform(into:)` whenever the WaveformView wants
    // a fresh frame.
    private static let waveformRingSize = 4096
    private var waveformRing = [Float](repeating: 0, count: waveformRingSize)
    private var waveformWriteIdx: Int = 0
    private let waveformLock = NSLock()

    // Apple's DLS bank — present on every macOS install since 10.x.
    private static let bankURL = URL(
        fileURLWithPath: "/System/Library/Components/CoreAudio.component/Contents/Resources/gs_instruments.dls"
    )

    func start() {
        guard !started else { return }
        engine.attach(melodic)
        engine.attach(drums)
        engine.connect(melodic, to: engine.mainMixerNode, format: nil)
        engine.connect(drums, to: engine.mainMixerNode, format: nil)
        engine.prepare()  // pre-allocate buffers before .start()
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
    }

    /// Tap the engine's main mixer so the WaveformView gets a live picture
    /// of whatever the synth is producing — both melodic and drum hits go
    /// through the mainMixer. Buffer size 512 frames ≈ 11 ms at 44.1 kHz,
    /// small enough that the waveform feels live.
    private func installWaveformTap() {
        let mixer = engine.mainMixerNode
        let format = mixer.outputFormat(forBus: 0)
        // 256 frames ≈ 5.8 ms at 44.1 kHz — small buffer = fresher samples
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

    /// Copy the most recent `dest.count` samples from the tap ring (in
    /// chronological order) into `dest`. Older samples first, newest last.
    /// Cheap; safe to call from main thread on every screen frame.
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

    /// Plays inaudible velocity-1 notes through both samplers immediately
    /// after start(). This forces sample-bank loads and audio-thread warmup
    /// to happen NOW instead of on the user's first real tap, so fast taps
    /// trigger sound with no perceptible delay.
    private func primeForLowLatency() {
        // Warmup notes: a few across the range so the sampler caches more of
        // its sample map. Velocity 1 is essentially silent.
        let melodicWarmup: [UInt8] = [60, 64, 67, 72]
        let drumWarmup: [UInt8] = [36, 38, 42, 46]
        for n in melodicWarmup {
            melodic.startNote(n, withVelocity: 1, onChannel: 0)
        }
        for n in drumWarmup {
            drums.startNote(n, withVelocity: 1, onChannel: 0)
        }
        // Stop them on the next run loop tick so the engine actually
        // schedules the start side first.
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.05) { [weak self] in
            guard let self = self else { return }
            for n in melodicWarmup { self.melodic.stopNote(n, onChannel: 0) }
            for n in drumWarmup { self.drums.stopNote(n, onChannel: 0) }
        }
    }

    private func loadDefaultPatches() {
        let url = MenuBandSynth.bankURL
        guard FileManager.default.fileExists(atPath: url.path) else {
            NSLog("MenuBand: gs_instruments.dls not found — falling back to default sampler tone")
            return
        }
        // CoreAudio's DLS bank uses Roland's GS conventions:
        //   melodic instruments — bankMSB = 0x79 (kAUSampler_DefaultMelodicBankMSB)
        //   percussion          — bankMSB = 0x78 (kAUSampler_DefaultPercussionBankMSB)
        // Program 0 on each = the standard kit / acoustic grand piano.
        let melodicMSB: UInt8 = 0x79
        let percussionMSB: UInt8 = 0x78
        let lsb: UInt8 = 0
        do {
            try melodic.loadSoundBankInstrument(at: url, program: 0, bankMSB: melodicMSB, bankLSB: lsb)
        } catch {
            NSLog("MenuBand: melodic patch load failed: \(error)")
        }
        do {
            try drums.loadSoundBankInstrument(at: url, program: 0, bankMSB: percussionMSB, bankLSB: lsb)
        } catch {
            NSLog("MenuBand: drum kit load failed: \(error)")
        }
    }

    /// Switch the melodic sampler to a different GM program (0–127).
    /// Examples: 0=piano, 4=electric piano, 24=nylon guitar, 32=acoustic bass,
    /// 40=violin, 48=string ensemble, 56=trumpet, 73=flute, 80=square lead.
    func setMelodicProgram(_ program: UInt8) {
        guard started else { return }
        let url = MenuBandSynth.bankURL
        guard FileManager.default.fileExists(atPath: url.path) else { return }
        try? melodic.loadSoundBankInstrument(at: url, program: program, bankMSB: 0x79, bankLSB: 0)
    }

    func stop() {
        guard started else { return }
        engine.stop()
        started = false
    }

    func noteOn(_ midi: UInt8, velocity: UInt8 = 100, channel: UInt8 = 0) {
        guard started else { return }
        // Each AVAudioUnitSampler is itself single-channel; we pick which
        // sampler based on the *requested* channel.
        let unit = (channel == 9) ? drums : melodic
        unit.startNote(midi, withVelocity: velocity, onChannel: 0)
    }

    func noteOff(_ midi: UInt8, channel: UInt8 = 0) {
        guard started else { return }
        let unit = (channel == 9) ? drums : melodic
        unit.stopNote(midi, onChannel: 0)
    }

    func panic() {
        guard started else { return }
        for unit in [melodic, drums] {
            for note: UInt8 in 0...127 { unit.stopNote(note, onChannel: 0) }
        }
    }
}
