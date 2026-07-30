#!/usr/bin/env swift

import AVFoundation
import AudioToolbox
import Foundation

struct Note: Codable {
    let t: Double
    let dur: Double
    let midi: Int
    let soundMidi: Int
    let vel: Double
    let lane: String
}

struct Score: Codable {
    let id: String
    let name: String
    let bpm: Double
    let meter: String
    let beatSec: Double
    let barSec: Double
    let durationSec: Double
    let revealAtSec: Double
    let revealDurationSec: Double
    let exitAtSec: Double
    let exitDurationSec: Double
    let instrumentProgram: Int
    let instrumentName: String
    let notes: [Note]
}

struct Arguments {
    var manifest = "pop/menuband/waltzes/menu-band-waltzes.json"
    var id: String?
    var outDir: String?
}

typealias JSONObject = [String: Any]

func usage() -> Never {
    print("usage: render-menu-band-waltz.swift --id ID [--manifest FILE] [--out-dir DIR]")
    exit(2)
}

func parseArguments() -> Arguments {
    var result = Arguments()
    var args = Array(CommandLine.arguments.dropFirst())
    while !args.isEmpty {
        let arg = args.removeFirst()
        switch arg {
        case "--manifest":
            guard let value = args.first else { usage() }
            result.manifest = value
            args.removeFirst()
        case "--id":
            guard let value = args.first else { usage() }
            result.id = value
            args.removeFirst()
        case "--out-dir":
            guard let value = args.first else { usage() }
            result.outDir = value
            args.removeFirst()
        case "--help", "-h": usage()
        default: usage()
        }
    }
    guard result.id != nil else { usage() }
    return result
}

func number(_ object: JSONObject, _ key: String, fallback: Double) -> Double {
    if let value = object[key] as? NSNumber { return value.doubleValue }
    if let value = object[key] as? String, let parsed = Double(value) { return parsed }
    return fallback
}

func string(_ object: JSONObject, _ key: String, fallback: String) -> String {
    (object[key] as? String).flatMap { $0.isEmpty ? nil : $0 } ?? fallback
}

func merged(_ base: JSONObject, _ override: JSONObject) -> JSONObject {
    var result = base
    for (key, value) in override {
        if let incoming = value as? JSONObject, let prior = result[key] as? JSONObject {
            result[key] = merged(prior, incoming)
        } else {
            result[key] = value
        }
    }
    return result
}

let visibleMIDIs = [60, 62, 64, 65, 67, 69, 71, 72, 74, 76, 77, 79, 81, 83]
let lowerDisplayByPitchClass = [0: 60, 2: 62, 4: 64, 5: 65, 7: 67, 9: 69, 11: 71]

func displayMidi(for soundMidi: Int) -> Int {
    if visibleMIDIs.contains(soundMidi) { return soundMidi }
    let pitchClass = (soundMidi % 12 + 12) % 12
    guard let lower = lowerDisplayByPitchClass[pitchClass] else {
        fatalError("waltz note is not a white Menu Band key: \(soundMidi)")
    }
    return soundMidi >= 72 && visibleMIDIs.contains(lower + 12) ? lower + 12 : lower
}

func scaleIntervals(_ mode: String) -> [Int] {
    switch mode.lowercased() {
    case "minor", "aeolian": return [0, 2, 3, 5, 7, 8, 10]
    case "dorian": return [0, 2, 3, 5, 7, 9, 10]
    default: return [0, 2, 4, 5, 7, 9, 11]
    }
}

func tonicMidi(_ tonic: String) -> Int {
    switch tonic.uppercased() {
    case "A": return 45
    case "D": return 50
    default: return 48
    }
}

func floorDiv(_ value: Int, _ divisor: Int) -> Int {
    let quotient = value / divisor
    let remainder = value % divisor
    return remainder < 0 ? quotient - 1 : quotient
}

let args = parseArguments()
let manifestURL = URL(fileURLWithPath: args.manifest)
let rootAny = try JSONSerialization.jsonObject(with: Data(contentsOf: manifestURL))
guard let root = rootAny as? JSONObject,
      let variations = root["variations"] as? [JSONObject],
      let wanted = args.id,
      let selected = variations.first(where: { ($0["id"] as? String) == wanted }) else {
    fatalError("waltz not found in manifest")
}
let defaults = root["defaults"] as? JSONObject ?? [:]
let spec = merged(defaults, selected)
let visual = spec["visual"] as? JSONObject ?? [:]

let id = string(spec, "id", fallback: wanted)
let name = string(spec, "name", fallback: id)
let bpm = number(spec, "bpm", fallback: 84)
let bars = Int(number(spec, "bars", fallback: 28))
let duration = number(spec, "durationSec", fallback: 60)
let beat = 60.0 / bpm
let bar = beat * 3
precondition(abs(Double(bars) * bar - duration) < 0.001,
             "bars × 3 beats must equal the requested duration")

let tonic = tonicMidi(string(spec, "tonic", fallback: "C"))
let mode = string(spec, "mode", fallback: "major")
let intervals = scaleIntervals(mode)
let program = Int(number(spec, "instrumentProgram", fallback: 0))
let instrumentName = string(spec, "instrumentName", fallback: "Acoustic Grand Piano")
let development = string(spec, "development", fallback: "lift")
guard let harmony = spec["harmonyDegrees"] as? [Int], !harmony.isEmpty,
      let melodyBars = spec["melodyBars"] as? [[Int]], !melodyBars.isEmpty else {
    fatalError("waltz requires harmonyDegrees and melodyBars")
}

func scaleMidi(_ degree: Int, octaveShift: Int = 0) -> Int {
    let octave = floorDiv(degree, 7)
    let index = (degree % 7 + 7) % 7
    return tonic + intervals[index] + 12 * (octave + octaveShift)
}

var notes: [Note] = []
func add(_ at: Double, _ dur: Double, _ soundMidi: Int, _ velocity: Double) {
    guard at >= 0, at < duration - 0.05 else { return }
    notes.append(Note(
        t: at,
        dur: min(dur, duration - at - 0.02),
        midi: displayMidi(for: soundMidi),
        soundMidi: soundMidi,
        vel: max(0.06, min(0.9, velocity)),
        lane: "tone"
    ))
}

func melodyOffsets(count: Int) -> [Double] {
    switch count {
    case 1: return [0]
    case 2: return [0, 1.5]
    case 3: return [0, 1, 2]
    case 4: return [0, 0.5, 1.5, 2]
    default: return (0..<count).map { Double($0) * 3.0 / Double(count) }
    }
}

func developed(_ source: [Int], barIndex: Int) -> [Int] {
    let section = barIndex / 8
    if barIndex >= bars - 4 {
        let cadences = [[11, 9, 7], [10, 8], [9, 8], [7]]
        return cadences[barIndex - (bars - 4)]
    }
    guard section > 0 else { return source }
    switch development {
    case "mirror":
        let center = source.reduce(0, +) / max(1, source.count)
        return source.map { max(0, min(13, center - ($0 - center) + (section == 2 ? 1 : 0))) }
    case "turn":
        if section % 2 == 1 { return Array(source.dropFirst()) + source.prefix(1) }
        return source.enumerated().map { $0.offset % 2 == 0 ? min(13, $0.element + 1) : $0.element }
    default:
        return source.enumerated().map { index, degree in
            index == source.count - 1 || (section >= 2 && index == 0) ? min(13, degree + 7) : degree
        }
    }
}

for barIndex in 0..<bars {
    let barStart = Double(barIndex) * bar
    let rootDegree = barIndex >= bars - 2 ? 0 : harmony[barIndex % harmony.count]

    // Beat one is the bass; beats two and three are the same quiet triad.
    add(barStart, beat * 0.78, scaleMidi(rootDegree), 0.30)
    for chordBeat in [1.0, 2.0] {
        for (voice, chordDegree) in [rootDegree, rootDegree + 2, rootDegree + 4].enumerated() {
            add(barStart + chordBeat * beat, beat * 0.62,
                scaleMidi(chordDegree, octaveShift: 1),
                0.14 + Double(voice) * 0.018)
        }
    }

    let source = melodyBars[barIndex % melodyBars.count]
    let melody = developed(source, barIndex: barIndex)
    let offsets = melodyOffsets(count: melody.count)
    for (index, degree) in melody.enumerated() {
        let at = barStart + offsets[index] * beat
        let next = index + 1 < offsets.count ? offsets[index + 1] : 3.0
        let durationBeats = max(0.32, next - offsets[index])
        add(at, durationBeats * beat * 0.82, scaleMidi(degree, octaveShift: 1),
            index == 0 ? 0.46 : 0.40)
    }
}

notes.sort { $0.t == $1.t ? $0.soundMidi < $1.soundMidi : $0.t < $1.t }
precondition(!notes.isEmpty, "empty waltz")
precondition(notes.allSatisfy { visibleMIDIs.contains($0.midi) }, "visual note escaped Menu Band")
precondition(notes.allSatisfy { $0.t >= 0 && $0.t + $0.dur <= duration + 0.001 }, "note escaped duration")

let sampleRate = 48_000.0
let engine = AVAudioEngine()
let dryMixer = AVAudioMixerNode()
let reverb = AVAudioUnitReverb()
reverb.loadFactoryPreset(.mediumHall)
reverb.wetDryMix = program == 21 ? 10 : 16
engine.attach(dryMixer)
engine.attach(reverb)
engine.connect(dryMixer, to: reverb, format: nil)
engine.connect(reverb, to: engine.mainMixerNode, format: nil)

let bank = URL(fileURLWithPath: "/System/Library/Components/CoreAudio.component/Contents/Resources/gs_instruments.dls")
guard FileManager.default.fileExists(atPath: bank.path) else { fatalError("macOS GM sound bank not found") }

let keyCodeByDisplayMidi: [Int: Int] = [
    60: 8, 62: 2, 64: 14, 65: 3, 67: 5, 69: 0, 71: 11,
    72: 4, 74: 34, 76: 38, 77: 40, 79: 37, 81: 46, 83: 45,
]
let qwertyRows = [[12, 13, 14, 15, 17, 16, 32, 34, 31, 35],
                  [0, 1, 2, 3, 5, 4, 38, 40, 37, 41, 39],
                  [6, 7, 8, 9, 11, 45, 46]]
let rowOffsets = [0.0, 0.5, 1.0]

func menuBandPan(_ displayMidi: Int) -> Float {
    guard let keyCode = keyCodeByDisplayMidi[displayMidi] else { return 0 }
    for row in qwertyRows.indices {
        if let column = qwertyRows[row].firstIndex(of: keyCode) {
            let x = Double(column) + rowOffsets[row]
            return Float((x / 10.5 * 2 - 1) * 0.78)
        }
    }
    return 0
}

var samplers: [Int: AVAudioUnitSampler] = [:]
for (bus, midi) in Set(notes.map(\.midi)).sorted().enumerated() {
    let sampler = AVAudioUnitSampler()
    engine.attach(sampler)
    try sampler.loadSoundBankInstrument(
        at: bank,
        program: UInt8(max(0, min(127, program))),
        bankMSB: UInt8(kAUSampler_DefaultMelodicBankMSB),
        bankLSB: UInt8(kAUSampler_DefaultBankLSB)
    )
    sampler.stereoPan = menuBandPan(midi)
    sampler.masterGain = -2.5 * abs(menuBandPan(midi)) / 0.78
    engine.connect(sampler, to: dryMixer, fromBus: 0, toBus: AVAudioNodeBus(bus), format: nil)
    samplers[midi] = sampler
}

guard let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate, channels: 2) else {
    fatalError("could not create output format")
}
try engine.enableManualRenderingMode(.offline, format: format, maximumFrameCount: 2048)
engine.mainMixerNode.outputVolume = 0.68
dryMixer.outputVolume = 0.84
engine.prepare()
try engine.start()

struct MIDIEvent {
    let frame: AVAudioFramePosition
    let on: Bool
    let displayMidi: Int
    let soundMidi: UInt8
    let velocity: UInt8
}

var events: [MIDIEvent] = []
for note in notes {
    events.append(MIDIEvent(
        frame: AVAudioFramePosition((note.t * sampleRate).rounded()),
        on: true,
        displayMidi: note.midi,
        soundMidi: UInt8(note.soundMidi),
        velocity: UInt8(max(1, min(127, Int((note.vel * 127).rounded()))))
    ))
    events.append(MIDIEvent(
        frame: AVAudioFramePosition(((note.t + note.dur) * sampleRate).rounded()),
        on: false,
        displayMidi: note.midi,
        soundMidi: UInt8(note.soundMidi),
        velocity: 0
    ))
}
events.sort { $0.frame == $1.frame ? (!$0.on && $1.on) : $0.frame < $1.frame }

let outputDirectory = args.outDir ?? "pop/menuband/out/menu-band-waltzes/\(id)"
let outDir = URL(fileURLWithPath: outputDirectory, isDirectory: true)
try FileManager.default.createDirectory(at: outDir, withIntermediateDirectories: true)
let wavURL = outDir.appendingPathComponent("\(id).raw.wav")
let audioFile = try AVAudioFile(forWriting: wavURL, settings: format.settings)
guard let buffer = AVAudioPCMBuffer(pcmFormat: engine.manualRenderingFormat,
                                    frameCapacity: engine.manualRenderingMaximumFrameCount) else {
    fatalError("could not allocate render buffer")
}

let totalFrames = AVAudioFramePosition((duration * sampleRate).rounded())
var cursor: AVAudioFramePosition = 0
var eventIndex = 0
while cursor < totalFrames {
    while eventIndex < events.count && events[eventIndex].frame <= cursor {
        let event = events[eventIndex]
        guard let sampler = samplers[event.displayMidi] else { fatalError("missing sampler") }
        if event.on { sampler.startNote(event.soundMidi, withVelocity: event.velocity, onChannel: 0) }
        else { sampler.stopNote(event.soundMidi, onChannel: 0) }
        eventIndex += 1
    }
    let nextEvent = eventIndex < events.count ? events[eventIndex].frame : totalFrames
    let request = AVAudioFrameCount(min(2048, min(totalFrames - cursor, max(1, nextEvent - cursor))))
    let status = try engine.renderOffline(request, to: buffer)
    switch status {
    case .success:
        try audioFile.write(from: buffer)
        cursor += AVAudioFramePosition(buffer.frameLength)
    case .cannotDoInCurrentContext: continue
    case .insufficientDataFromInputNode: cursor += AVAudioFramePosition(request)
    case .error: fatalError("offline audio render failed")
    @unknown default: fatalError("unknown offline render status")
    }
}
engine.stop()

let score = Score(
    id: id,
    name: name,
    bpm: bpm,
    meter: "3/4",
    beatSec: beat,
    barSec: bar,
    durationSec: duration,
    revealAtSec: number(spec, "revealAtSec", fallback: 0.5),
    revealDurationSec: number(spec, "revealDurationSec", fallback: 1.2),
    exitAtSec: number(spec, "exitAtSec", fallback: 58.2),
    exitDurationSec: number(spec, "exitDurationSec", fallback: 1.4),
    instrumentProgram: program,
    instrumentName: instrumentName,
    notes: notes
)
let encoder = JSONEncoder()
encoder.outputFormatting = [.prettyPrinted, .sortedKeys, .withoutEscapingSlashes]
let scoreURL = outDir.appendingPathComponent("\(id).notes.json")
try encoder.encode(score).write(to: scoreURL)

print("✓ \(id) · \(name) · \(bars) bars · \(String(format: "%.1f", duration))s · \(notes.count) notes")
print("✓ \(wavURL.path)")
print("✓ \(scoreURL.path)")
