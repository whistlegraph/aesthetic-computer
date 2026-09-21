import AVFoundation
#if os(macOS)
import AudioToolbox
import CoreAudio
#endif
import Foundation

/// The Prox hover synthesizer, shared in character with PromptRockSound.swift.
///
/// Project title, revision, control and a quarter-second time bucket seed each strike.
enum AeselHoverSound {
    private static let sampleRate = 44_100.0
    private static let format = AVAudioFormat(
        standardFormatWithSampleRate: sampleRate, channels: 1)
    private static let engine = AVAudioEngine()
    private static let players = (0..<6).map { _ in AVAudioPlayerNode() }
    private static let queue = DispatchQueue(
        label: "computer.aesthetic.aesel.hover-sound", qos: .userInitiated)
    private static var configured = false
    private static var started = false
    private static var nextVoice = 0
    private static var configurationObserver: NSObjectProtocol?

    private static var lastStrike = -Double.infinity

    private static func seed(_ text: String) -> UInt64 {
        text.utf8.reduce(UInt64(14695981039346656037)) { ($0 ^ UInt64($1)) &* 1099511628211 }
    }

    /// Stable project/revision pitch with time-shaped timbre, using Prox's
    /// harmonic wavetable, struck resonance and short decaying noise.
    static func buffer(project: String, revision: Int, control: String, at time: TimeInterval) -> AVAudioPCMBuffer? {
        let identity = "\(project)|v\(revision)|\(control)"
        return render(graphicSeed: seed("\(identity)|\(Int64(floor(time * 4)))"), nameSeed: seed(identity))
    }

    static func play(project: String, revision: Int, control: String) {
        let time = Date.timeIntervalSinceReferenceDate
        queue.async {
            guard time - lastStrike >= 0.08,
                  let buffer = buffer(project: project, revision: revision, control: control, at: time),
                  start() else { return }
            lastStrike = time
            let voice = players[nextVoice]
            nextVoice = (nextVoice + 1) % players.count
            voice.scheduleBuffer(buffer, at: nil, options: .interrupts)
            voice.play()
        }
    }

    private static func start() -> Bool {
        guard configureIfNeeded() else { return false }
        if started && engine.isRunning { return true }
        #if os(macOS)
        _ = applyDefaultOutputDevice()
        #else
        try? AVAudioSession.sharedInstance().setCategory(.ambient, options: [.mixWithOthers])
        #endif
        engine.prepare()
        do {
            try engine.start()
        } catch {
            started = false
            NSLog("Aesel hover: audio engine failed to start: \(error)")
            return false
        }
        started = true
        return true
    }

    private static func configureIfNeeded() -> Bool {
        if configured { return true }
        guard let format else { return false }
        for player in players {
            engine.attach(player)
            engine.connect(player, to: engine.mainMixerNode, format: format)
        }
        engine.mainMixerNode.outputVolume = 1
        configurationObserver = NotificationCenter.default.addObserver(
            forName: .AVAudioEngineConfigurationChange,
            object: engine,
            queue: nil
        ) { _ in
            queue.async {
                started = false
                NSLog("🪨 [sound] audio route changed; will recover on next strike")
            }
        }
        configured = true
        return true
    }

    #if os(macOS)
    /// Explicitly follow the system's default output. AUHAL can retain the
    /// device it first opened across a headphone/sample-rate change even while
    /// CoreAudio reports a new default route.
    @discardableResult
    private static func applyDefaultOutputDevice() -> AudioDeviceID {
        let device = defaultOutputDeviceID()
        guard device != 0, let audioUnit = engine.outputNode.audioUnit else { return device }
        var current = AudioDeviceID(0)
        var currentSize = UInt32(MemoryLayout<AudioDeviceID>.size)
        if AudioUnitGetProperty(
            audioUnit, kAudioOutputUnitProperty_CurrentDevice,
            kAudioUnitScope_Global, 0, &current, &currentSize
        ) == noErr, current == device { return device }

        var requested = device
        let status = AudioUnitSetProperty(
            audioUnit, kAudioOutputUnitProperty_CurrentDevice,
            kAudioUnitScope_Global, 0,
            &requested, UInt32(MemoryLayout<AudioDeviceID>.size))
        if status != noErr {
            NSLog("Aesel hover: output-device route failed: \(status)")
        } else {
            NSLog("🪨 [sound] rebound output-device \(current) → \(device)")
        }
        return device
    }

    private static func defaultOutputDeviceID() -> AudioDeviceID {
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDefaultOutputDevice,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain)
        var device = AudioDeviceID(0)
        var size = UInt32(MemoryLayout<AudioDeviceID>.size)
        guard AudioObjectGetPropertyData(
            AudioObjectID(kAudioObjectSystemObject), &address,
            0, nil, &size, &device) == noErr else {
            return 0
        }
        return device
    }

    #endif

    private static func render(graphicSeed: UInt64,
                               nameSeed: UInt64) -> AVAudioPCMBuffer? {
        guard let format else { return nil }

        // The same bits that grow the rock select its spectral ridges. Build a
        // compact, normalized wavetable rather than picking a stock waveform.
        let tableSize = 64
        var table = [Double](repeating: 0, count: tableSize)
        for harmonic in 1...8 {
            let shift = UInt64((harmonic - 1) * 8)
            let byte = Double((graphicSeed >> shift) & 0xff) / 255.0
            let amplitude = (0.12 + byte * 0.88) / pow(Double(harmonic), 1.18)
            let phase = Double((nameSeed >> shift) & 0xff) / 255.0 * 2 * .pi
            for i in 0..<tableSize {
                table[i] += sin(2 * .pi * Double(harmonic * i) / Double(tableSize) + phase)
                    * amplitude
            }
        }
        let peak = max(0.001, table.map(abs).max() ?? 1)
        table = table.map { $0 / peak }

        let scale = [0, 3, 5, 7, 10]
        let degree = scale[Int(nameSeed % UInt64(scale.count))]
        let octave = Int((nameSeed >> 9) & 1)
        let root = 150.0 * pow(2, Double(degree) / 12.0) * Double(octave + 1)
        let duration = 0.075 + Double((graphicSeed >> 17) & 0xff) / 255.0 * 0.09
        let decayRate = 6.5 + Double((graphicSeed >> 41) & 0xff) / 255.0 * 7.0
        let resonanceRatio = 1.45 + Double((nameSeed >> 24) & 0xff) / 255.0 * 1.7
        let frames = AVAudioFrameCount(sampleRate * duration)
        guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames),
              let samples = buffer.floatChannelData?[0] else { return nil }
        buffer.frameLength = frames

        var phase = 0.0
        var resonancePhase = 0.0
        var noise = UInt32(truncatingIfNeeded: graphicSeed ^ nameSeed)
        for i in 0..<Int(frames) {
            let t = Double(i) / Double(frames)
            // A tiny downward bend makes the strike feel physical. The first
            // millisecond is intentionally sharp enough to play rhythmically.
            let frequency = root * (1.08 - 0.12 * t)
            phase += frequency / sampleRate
            resonancePhase += 2 * .pi * frequency * resonanceRatio / sampleRate
            let tablePosition = phase * Double(tableSize)
            let index = Int(tablePosition) % tableSize
            let next = (index + 1) % tableSize
            let fraction = tablePosition - floor(tablePosition)
            let wave = table[index] * (1 - fraction) + table[next] * fraction

            noise = 1_664_525 &* noise &+ 1_013_904_223
            let grit = Double(Int32(bitPattern: noise)) / Double(Int32.max)
            let attack = min(1.0, t / 0.012)
            let body = exp(-decayRate * t)
            let click = grit * exp(-55 * t)
            let ring = sin(resonancePhase) * exp(-(decayRate * 1.35) * t)
            // The original -20-ish dB strike vanished at normal headphone
            // volume. This still leaves generous overlap headroom, but lands
            // like an intentional playable hit rather than ambient texture.
            let hit = (wave * 0.34 + ring * 0.13 + click * 0.10) * attack * body
            samples[i] = Float(tanh(hit))
        }
        return buffer
    }
}
