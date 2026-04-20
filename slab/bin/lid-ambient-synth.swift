// lid-ambient-synth — live ambient drone generator and recorder.
//
// Owns the lid-closed ambient bed: instead of looping a pre-rendered
// ambient.wav, this binary synthesises pentatonic notes in real time and
// pipes them through built-in Audio Units (delay + reverb), then taps the
// final mix back to a timestamped wav at $SLAB_HOME/sessions/ambient-*.wav.
//
// Build:
//   swiftc -O -o lid-ambient-synth lid-ambient-synth.swift
//
// Run:
//   lid-ambient-synth          # plays + records until SIGTERM/SIGINT
//
// Lifecycle:
//   start      — engine.start, write header to ambient-<ts>.wav
//   SIGTERM    — fade master gain to 0 over FADE_DUR seconds, then exit 0
//                (giving the lid-open return chime a soft bed to land on)

import AVFoundation
import Foundation
import Darwin

// ---------- knobs ----------
// SAMPLE_RATE is overridden at startup to match the output device, so the
// synth's notion of time matches the audio engine's render rate.
var SAMPLE_RATE: Double = 44100.0
let FADE_DUR: Double = 2.0

// C major pentatonic, C3..E5 (matches the previous python generator).
let SCALE_MIDI: [Double] = [48, 50, 52, 55, 57, 60, 62, 64, 67, 69, 72, 74, 76]

let NOTE_GAP_RANGE: ClosedRange<Double> = 4.0...10.0
let NOTE_DUR_RANGE: ClosedRange<Double> = 18.0...45.0
let NOTE_AMP_RANGE: ClosedRange<Double> = 0.07...0.13
let NOTE_FIN_RANGE: ClosedRange<Double> = 3.5...8.0
let NOTE_FOUT_RANGE: ClosedRange<Double> = 8.0...18.0
let NOTE_DETUNE_CENTS: ClosedRange<Double> = -7.0...7.0

let DRONE_GAP_RANGE: ClosedRange<Double> = 30.0...60.0
let DRONE_DUR_RANGE: ClosedRange<Double> = 40.0...80.0
let DRONE_AMP_RANGE: ClosedRange<Double> = 0.05...0.09
let DRONE_FIN_RANGE: ClosedRange<Double> = 6.0...12.0
let DRONE_FOUT_RANGE: ClosedRange<Double> = 15.0...25.0
let DRONE_DETUNE_CENTS: ClosedRange<Double> = -5.0...5.0

let REVERB_PRESET: AVAudioUnitReverbPreset = .largeHall
let REVERB_WET: Float = 40.0
let DELAY_TIME: TimeInterval = 0.5
let DELAY_FEEDBACK: Float = 25.0
let DELAY_WET: Float = 20.0
let DELAY_LP: Float = 3000.0

// ---------- voice ----------
final class Voice {
    let freq: Double
    let freq2: Double
    let startSample: Int64
    let durSamples: Int64
    let amp: Double
    let fadeInS: Double
    let fadeOutS: Double
    let durS: Double

    init(freq: Double, detuneCents: Double, startSample: Int64,
         dur: Double, amp: Double, fadeIn: Double, fadeOut: Double) {
        self.freq = freq
        self.freq2 = freq * pow(2.0, detuneCents / 1200.0)
        self.startSample = startSample
        self.durSamples = Int64(dur * SAMPLE_RATE)
        self.amp = amp
        self.fadeInS = max(fadeIn, 1e-9)
        self.fadeOutS = max(fadeOut, 1e-9)
        self.durS = dur
    }

    func render(into buf: UnsafeMutablePointer<Float>, count: Int, startAt: Int64) {
        let endLocal = durSamples
        for i in 0..<count {
            let local = startAt + Int64(i) - startSample
            if local < 0 || local >= endLocal { continue }
            let t = Double(local) / SAMPLE_RATE
            let env = min(min(t / fadeInS, (durS - t) / fadeOutS), 1.0)
            if env <= 0 { continue }
            let s1 = sin(2 * .pi * freq * t)
            let s2 = sin(2 * .pi * freq2 * t)
            buf[i] += Float(amp * env * (0.6 * s1 + 0.4 * s2))
        }
    }

    func expired(at sample: Int64) -> Bool {
        return sample > startSample + durSamples
    }
}

// ---------- synth ----------
final class AmbientSynth {
    private var voices: [Voice] = []
    private var currentSample: Int64 = 0
    private var nextNoteSample: Int64
    private var nextDroneSample: Int64
    private let pitches: [Double]
    private let bassPitches: [Double]

    private var masterGain: Double = 1.0
    private var fadePerSample: Double = 0.0

    init() {
        pitches = SCALE_MIDI.map { 440.0 * pow(2.0, ($0 - 69) / 12.0) }
        bassPitches = pitches.prefix(4).map { $0 / 2.0 }
        nextNoteSample = Int64(Double.random(in: 0.5...3.0) * SAMPLE_RATE)
        nextDroneSample = Int64(Double.random(in: 5.0...15.0) * SAMPLE_RATE)
    }

    func render(into buf: UnsafeMutablePointer<Float>, count: Int) {
        for i in 0..<count { buf[i] = 0 }

        let endSample = currentSample + Int64(count)

        while nextNoteSample <= endSample {
            voices.append(Voice(
                freq: pitches.randomElement()!,
                detuneCents: Double.random(in: NOTE_DETUNE_CENTS),
                startSample: nextNoteSample,
                dur: Double.random(in: NOTE_DUR_RANGE),
                amp: Double.random(in: NOTE_AMP_RANGE),
                fadeIn: Double.random(in: NOTE_FIN_RANGE),
                fadeOut: Double.random(in: NOTE_FOUT_RANGE)
            ))
            nextNoteSample += Int64(Double.random(in: NOTE_GAP_RANGE) * SAMPLE_RATE)
        }

        while nextDroneSample <= endSample {
            voices.append(Voice(
                freq: bassPitches.randomElement()!,
                detuneCents: Double.random(in: DRONE_DETUNE_CENTS),
                startSample: nextDroneSample,
                dur: Double.random(in: DRONE_DUR_RANGE),
                amp: Double.random(in: DRONE_AMP_RANGE),
                fadeIn: Double.random(in: DRONE_FIN_RANGE),
                fadeOut: Double.random(in: DRONE_FOUT_RANGE)
            ))
            nextDroneSample += Int64(Double.random(in: DRONE_GAP_RANGE) * SAMPLE_RATE)
        }

        for v in voices {
            v.render(into: buf, count: count, startAt: currentSample)
        }

        if fadePerSample != 0 {
            for i in 0..<count {
                masterGain = max(0.0, min(1.0, masterGain + fadePerSample))
                buf[i] *= Float(masterGain)
            }
        } else if masterGain != 1.0 {
            let g = Float(masterGain)
            for i in 0..<count { buf[i] *= g }
        }

        voices.removeAll { $0.expired(at: endSample) }
        currentSample = endSample
    }

    func startFade(seconds: Double) {
        fadePerSample = -1.0 / (seconds * SAMPLE_RATE)
    }
}

// ---------- engine ----------
let engine = AVAudioEngine()

// Drive the synth at the device's native rate so AVAudioEngine doesn't have
// to resample our mono output (which is the real source of -10868 mismatches
// when piping a fixed-rate source through built-in AUs into mainMixerNode).
let deviceFormat = engine.outputNode.inputFormat(forBus: 0)
SAMPLE_RATE = deviceFormat.sampleRate

// Stereo throughout — AVAudioUnitDelay/Reverb fail to set a mono format on
// their input/output buses (-10868), so the synth renders mono into the L
// channel and we duplicate to R before the AU chain.
guard let synthFormat = AVAudioFormat(
    commonFormat: deviceFormat.commonFormat,
    sampleRate: deviceFormat.sampleRate,
    channels: 2,
    interleaved: deviceFormat.isInterleaved
) else {
    FileHandle.standardError.write("failed to build synthFormat\n".data(using: .utf8)!)
    exit(1)
}

let synth = AmbientSynth()

let sourceNode = AVAudioSourceNode(format: synthFormat) {
    _, _, frameCount, audioBufferList -> OSStatus in
    let abl = UnsafeMutableAudioBufferListPointer(audioBufferList)
    let n = Int(frameCount)
    if abl.count >= 2,
       let lRaw = abl[0].mData,
       let rRaw = abl[1].mData {
        let l = lRaw.assumingMemoryBound(to: Float.self)
        let r = rRaw.assumingMemoryBound(to: Float.self)
        synth.render(into: l, count: n)
        memcpy(r, l, n * MemoryLayout<Float>.size)
    } else if abl.count >= 1, let raw = abl[0].mData {
        // Interleaved fallback: write mono into stride-2 buffer.
        let buf = raw.assumingMemoryBound(to: Float.self)
        var mono = [Float](repeating: 0, count: n)
        mono.withUnsafeMutableBufferPointer { mb in
            synth.render(into: mb.baseAddress!, count: n)
        }
        for i in 0..<n {
            buf[2 * i] = mono[i]
            buf[2 * i + 1] = mono[i]
        }
    }
    return noErr
}

let delay = AVAudioUnitDelay()
delay.delayTime = DELAY_TIME
delay.feedback = DELAY_FEEDBACK
delay.wetDryMix = DELAY_WET
delay.lowPassCutoff = DELAY_LP

let reverb = AVAudioUnitReverb()
reverb.loadFactoryPreset(REVERB_PRESET)
reverb.wetDryMix = REVERB_WET

engine.attach(sourceNode)
engine.attach(delay)
engine.attach(reverb)

engine.connect(sourceNode, to: delay, format: synthFormat)
engine.connect(delay, to: reverb, format: synthFormat)
engine.connect(reverb, to: engine.mainMixerNode, format: synthFormat)

// ---------- recording tap ----------
let slabHome = ProcessInfo.processInfo.environment["SLAB_HOME"]
    ?? NSString(string: "~/.local/share/slab").expandingTildeInPath
let sessionDir = "\(slabHome)/sessions"
try? FileManager.default.createDirectory(
    atPath: sessionDir, withIntermediateDirectories: true)

let formatter = DateFormatter()
formatter.dateFormat = "yyyyMMdd-HHmmss"
let timestamp = formatter.string(from: Date())
let outPath = "\(sessionDir)/ambient-\(timestamp).wav"
let outURL = URL(fileURLWithPath: outPath)

let tapFormat = engine.mainMixerNode.outputFormat(forBus: 0)

let fileSettings: [String: Any] = [
    AVFormatIDKey: kAudioFormatLinearPCM,
    AVSampleRateKey: tapFormat.sampleRate,
    AVNumberOfChannelsKey: tapFormat.channelCount,
    AVLinearPCMBitDepthKey: 16,
    AVLinearPCMIsFloatKey: false,
    AVLinearPCMIsBigEndianKey: false,
]

// `var` (not `let`) so we can drop the reference before exit() — AVAudioFile
// only finalises its WAV header (data-chunk length, RIFF size) when the
// instance deallocates, so without this the file looks empty to afinfo.
var outFile: AVAudioFile?
do {
    outFile = try AVAudioFile(
        forWriting: outURL,
        settings: fileSettings,
        commonFormat: tapFormat.commonFormat,
        interleaved: tapFormat.isInterleaved)
} catch {
    FileHandle.standardError.write(
        "open output file failed: \(error)\n".data(using: .utf8)!)
    exit(1)
}

engine.mainMixerNode.installTap(
    onBus: 0, bufferSize: 4096, format: tapFormat
) { buffer, _ in
    do {
        try outFile?.write(from: buffer)
    } catch {
        FileHandle.standardError.write(
            "tap write failed: \(error)\n".data(using: .utf8)!)
    }
}

do {
    try engine.start()
} catch {
    FileHandle.standardError.write(
        "engine.start failed: \(error)\n".data(using: .utf8)!)
    exit(1)
}

print("ambient synth running → \(outPath)")
fflush(stdout)

// ---------- signal handling ----------
var exiting = false
func gracefulExit() {
    if exiting { return }
    exiting = true
    synth.startFade(seconds: FADE_DUR)
    DispatchQueue.main.asyncAfter(deadline: .now() + FADE_DUR + 0.3) {
        engine.mainMixerNode.removeTap(onBus: 0)
        engine.stop()
        outFile = nil  // drops the file's writer → finalises WAV header
        exit(0)
    }
}

let termSrc = DispatchSource.makeSignalSource(signal: SIGTERM, queue: .main)
termSrc.setEventHandler { gracefulExit() }
termSrc.resume()
signal(SIGTERM, SIG_IGN)

let intSrc = DispatchSource.makeSignalSource(signal: SIGINT, queue: .main)
intSrc.setEventHandler { gracefulExit() }
intSrc.resume()
signal(SIGINT, SIG_IGN)

RunLoop.main.run()
