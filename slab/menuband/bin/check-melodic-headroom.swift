import AVFoundation
import AudioToolbox

/// Render Apple's real DLS voices without opening an audio device. The event
/// sequence matches sustained linger: four rotating channels, 100 ms CC11
/// fade ticks, note-off before resetting expression on channel reuse.
@main
enum MelodicHeadroomChecks {
    struct Measurement {
        var peak: Float = 0
        var power = 0.0
        var frames = 0
        var rmsDB: Double { 10 * log10(max(1e-20, power / Double(max(1, frames)))) }
    }

    static func check(_ status: OSStatus, line: Int = #line) {
        precondition(status == noErr, "AudioUnit status \(status) at line \(line)")
    }

    static func effect(_ type: OSType) -> AVAudioUnitEffect {
        AVAudioUnitEffect(audioComponentDescription: AudioComponentDescription(
            componentType: kAudioUnitType_Effect, componentSubType: type,
            componentManufacturer: kAudioUnitManufacturer_Apple,
            componentFlags: 0, componentFlagsMask: 0))
    }

    static func render(program: UInt32, automix: Bool, repeated: Bool,
                       save: URL? = nil) throws -> Measurement {
        let headroom = MelodicHeadroom()
        let engine = AVAudioEngine()
        let synth = AVAudioUnitMIDIInstrument(audioComponentDescription: AudioComponentDescription(
            componentType: kAudioUnitType_MusicDevice, componentSubType: kAudioUnitSubType_MIDISynth,
            componentManufacturer: kAudioUnitManufacturer_Apple,
            componentFlags: 0, componentFlagsMask: 0))
        let mixer = AVAudioMixerNode()
        let compressor = effect(kAudioUnitSubType_DynamicsProcessor)
        let limiter = effect(kAudioUnitSubType_PeakLimiter)
        let format = AVAudioFormat(standardFormatWithSampleRate: 48_000, channels: 2)!
        for node in [synth, mixer, compressor, limiter] as [AVAudioNode] { engine.attach(node) }
        engine.connect(synth, to: mixer, format: format)
        engine.connect(mixer, to: compressor, format: format)
        engine.connect(compressor, to: limiter, format: format)
        engine.connect(limiter, to: engine.mainMixerNode, format: format)
        // Match MenuBandSynth's existing downstream dynamics; the production
        // headroom model above is the only difference between A and B.
        for (id, value): (AudioUnitParameterID, Float) in [
            (kDynamicsProcessorParam_Threshold, -18), (kDynamicsProcessorParam_HeadRoom, 8),
            (kDynamicsProcessorParam_AttackTime, 0.008), (kDynamicsProcessorParam_ReleaseTime, 0.18),
            (kDynamicsProcessorParam_OverallGain, 1.5),
        ] { check(AudioUnitSetParameter(compressor.audioUnit, id, kAudioUnitScope_Global, 0, value, 0)) }
        for (id, value): (AudioUnitParameterID, Float) in [
            (kLimiterParam_AttackTime, 0.002), (kLimiterParam_DecayTime, 0.05), (kLimiterParam_PreGain, 0),
        ] { check(AudioUnitSetParameter(limiter.audioUnit, id, kAudioUnitScope_Global, 0, value, 0)) }
        var bank = URL(fileURLWithPath:
            "/System/Library/Components/CoreAudio.component/Contents/Resources/gs_instruments.dls") as CFURL
        check(AudioUnitSetProperty(synth.audioUnit, kMusicDeviceProperty_SoundBankURL,
                                  kAudioUnitScope_Global, 0, &bank, UInt32(MemoryLayout<CFURL>.size)))
        var now = 0.0
        func midi(_ status: UInt32, _ a: UInt32, _ b: UInt32 = 0) {
            let channel = UInt8(status & 15)
            switch status & 0xf0 {
            case 0x90: headroom.noteOn(UInt8(a), velocity: UInt8(b), channel: channel, at: now)
            case 0x80: headroom.noteOff(UInt8(a), channel: channel, at: now)
            case 0xb0: if a == 11 { headroom.setExpression(UInt8(b), channel: channel) }
            default: break
            }
            check(MusicDeviceMIDIEvent(synth.audioUnit, status, a, b, 0))
        }
        func selectProgram() {
            for channel: UInt32 in 0..<4 {
                midi(0xB0 | channel, 0, 0x79)
                midi(0xB0 | channel, 32)
                midi(0xC0 | channel, program)
            }
        }
        try engine.enableManualRenderingMode(.offline, format: format, maximumFrameCount: 480)
        engine.prepare()
        var preload: UInt32 = 1
        check(AudioUnitSetProperty(synth.audioUnit, kAUMIDISynthProperty_EnablePreload,
                                  kAudioUnitScope_Global, 0, &preload, 4))
        selectProgram()
        preload = 0
        check(AudioUnitSetProperty(synth.audioUnit, kAUMIDISynthProperty_EnablePreload,
                                  kAudioUnitScope_Global, 0, &preload, 4))
        try engine.start()
        defer { engine.stop() }
        selectProgram() // Starting resets channel programs; do not test the fallback sine.
        let file = try save.map { try AVAudioFile(forWriting: $0, settings: format.settings) }
        let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: 480)!
        var lastPress = [Int](repeating: -10_000, count: 4)
        var result = Measurement()
        for tick in 0..<1700 {
            now = Double(tick) / 100
            if repeated ? (tick >= 100 && tick < 600 && tick % 15 == 0) : tick == 105 {
                let channel = (tick / 15) % 4
                if lastPress[channel] >= 0 { midi(0x80 | UInt32(channel), 72) }
                midi(0xB0 | UInt32(channel), 11, 127)
                midi(0x90 | UInt32(channel), 72, 100)
                lastPress[channel] = tick
            }
            for channel in 0..<4 where lastPress[channel] >= 0 {
                let age = tick - lastPress[channel]
                if age >= 5 && (age - 5) % 10 == 0 {
                    let value = max(0, 127 - Int((Double(age - 5) / 1000 * 127).rounded()))
                    midi(0xB0 | UInt32(channel), 11, UInt32(value))
                }
                if age == 1025 { midi(0x80 | UInt32(channel), 72) }
            }
            if automix { mixer.outputVolume = headroom.nextGain(at: now) }
            let status = try engine.renderOffline(480, to: buffer)
            precondition(status == .success, "offline render failed")
            try file?.write(from: buffer)
            for channel in 0..<2 {
                for frame in 0..<Int(buffer.frameLength) {
                    let x = buffer.floatChannelData![channel][frame]
                    precondition(x.isFinite, "non-finite audio")
                    result.peak = max(result.peak, abs(x))
                    if tick >= 200 && tick < 600 {
                        result.power += Double(x * x)
                        result.frames += 1
                    }
                }
            }
        }
        precondition(result.peak < 1 && result.peak > 0.001, "clipped or silent render")
        return result
    }

    static func checkLifecycle() {
        let mix = MelodicHeadroom()
        mix.noteOn(72, velocity: 100, channel: 0, at: 0)
        mix.noteOn(36, velocity: 127, channel: 9, at: 0)
        precondition(mix.nextGain(at: 0) == 1, "solo/drums unnecessarily attenuated")
        for channel: UInt8 in 1..<4 { mix.noteOn(72, velocity: 100, channel: channel, at: 0) }
        let stacked = mix.nextGain(at: 0)
        precondition(stacked > 0.7 && stacked < 0.72, "four repeats need about 3 dB of headroom")
        for channel: UInt8 in 0..<4 { mix.setExpression(0, channel: channel) }
        let fading = mix.nextGain(at: 0.01)
        precondition(fading > stacked && fading < stacked + 0.02, "gain recovery jumped")
        precondition(mix.nextGain(at: 3) == 1, "silent linger reserved headroom")
        mix.reset()
        precondition(mix.isIdle, "reset retained voices or attenuation")
        mix.noteOn(72, velocity: 100, channel: 0, at: 4)
        mix.noteOff(72, channel: 0, at: 4.5)
        mix.noteOn(72, velocity: 100, channel: 0, at: 4.5)
        precondition(mix.nextGain(at: 4.5) < 0.9, "channel reuse lost its release tail")
        mix.noteOff(72, channel: 0, at: 4.6)
        precondition(!mix.isIdle, "released voice discarded too early")
        precondition(mix.nextGain(at: 8) == 1 && mix.isIdle, "release failed to recover to idle")
        mix.reset()
        for channel: UInt8 in 0..<4 { mix.noteOn(72, velocity: 20, channel: channel, at: 9) }
        precondition(mix.nextGain(at: 9) == 1, "quiet repeats lost their dynamics")
        mix.reset()
        for channel: UInt8 in 0..<4 { mix.noteOn(60 + channel * 4, velocity: 100, channel: channel, at: 10) }
        precondition(mix.nextGain(at: 10) > stacked, "chords treated as coherent copies")
        print("PASS voice lifecycle: drums, quiet notes, chords, expression, release, channel reuse, reset")
    }

    static func main() throws {
        checkLifecycle()
        let directory = CommandLine.arguments.dropFirst().first.map { URL(fileURLWithPath: $0) }
        if let directory { try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true) }
        for program: UInt32 in [16, 48, 56, 78, 79, 88] {
            let before = try render(program: program, automix: false, repeated: true,
                save: program == 78 ? directory?.appendingPathComponent("whistle-before.wav") : nil)
            let after = try render(program: program, automix: true, repeated: true,
                save: program == 78 ? directory?.appendingPathComponent("whistle-after.wav") : nil)
            let reduction = before.rmsDB - after.rmsDB
            precondition(reduction > 0.5 && reduction < 6, "repeats unbalanced: \(program), \(reduction) dB")
            print(String(format: "PASS instrument %d: repeated-note RMS %.2f → %.2f dBFS; peak %.2f → %.2f dBFS",
                         program + 1, before.rmsDB, after.rmsDB, 20 * log10(before.peak), 20 * log10(after.peak)))
        }
        let soloBefore = try render(program: 78, automix: false, repeated: false)
        let soloAfter = try render(program: 78, automix: true, repeated: false)
        precondition(abs(soloBefore.rmsDB - soloAfter.rmsDB) < 0.01, "solo dynamics changed")
        print("PASS solo whistle level preserved")
    }
}
