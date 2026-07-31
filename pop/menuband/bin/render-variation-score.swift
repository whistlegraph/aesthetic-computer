#!/usr/bin/env swift
// Seeded, offline audio renderer for the 32-reel MenuBand variation set.
//
// Usage:
//   swift pop/menuband/bin/render-variation-score.swift --index 0
//   swift pop/menuband/bin/render-variation-score.swift --index 7 --manifest path/to/variations-32.json
//   swift pop/menuband/bin/render-variation-score.swift --spec one-variation.json --out-dir /tmp/menu
//
// The manifest may be either an array or { "variations": [...] }. Every field
// is optional. Supported fields are: id/slug/name/title, seed, bpm,
// durationSec, instrumentProgram (zero-based GM), gmProgram/instrumentNumber
// (one-based GM), instrumentName, or instrument: { program, name }, plus form,
// mode, drumStyle, density (0...1), swing (0...0.3),
// revealAtSec, revealDurationSec, exitDurationSec, and outputStem. Unknown fields
// are ignored. Music fields may instead live in a nested `music` object. With
// no manifest, index alone deterministically selects a unique
// GM instrument, riff grammar, rhythm, and arrangement from the built-in set.

import AVFoundation
import AudioToolbox
import Foundation

let visibleRange = 60...83
let boardMIDIs = [60, 62, 64, 65, 67, 69, 71, 72, 74, 76, 77, 79, 81, 83]

struct Note: Codable {
    let t: Double
    let dur: Double
    let midi: Int       // physical/display MenuBand note; always 60...83
    let soundMidi: Int  // note sent to the GM sampler
    let vel: Double
    let lane: String    // tone | drum
}

struct Score: Codable {
    let id: String
    let index: Int
    let seed: UInt64
    let bpm: Double
    let beatSec: Double
    let barSec: Double
    let durationSec: Double
    let openingPercussionOffAtSec: Double
    let splitAtSec: Double
    let fullPercussionAtSec: Double
    let percussionOffAtSec: Double
    let revealAtSec: Double
    let revealDurationSec: Double
    let exitAtSec: Double
    let exitDurationSec: Double
    let leadPreset: String
    let leadProgram: Int
    let form: String
    let mode: String
    let drumStyle: String
    let notes: [Note]
}

struct Arguments {
    var index = 0
    var id: String?
    var manifest = "pop/menuband/variations/variations-32.json"
    var spec: String?
    var outDir: String?
    var stem: String?
}

func usage() -> Never {
    print("usage: render-variation-score.swift [--index 0...31 | --id ID] [--manifest FILE | --spec FILE] [--out-dir DIR] [--stem NAME]")
    exit(2)
}

func parseArguments() -> Arguments {
    var result = Arguments()
    var args = Array(CommandLine.arguments.dropFirst())
    while !args.isEmpty {
        let arg = args.removeFirst()
        switch arg {
        case "--index", "-i":
            guard let value = args.first, let parsed = Int(value) else { usage() }
            result.index = parsed
            args.removeFirst()
        case "--id":
            guard let value = args.first else { usage() }
            result.id = value
            args.removeFirst()
        case "--manifest":
            guard let value = args.first else { usage() }
            result.manifest = value
            args.removeFirst()
        case "--spec":
            guard let value = args.first else { usage() }
            result.spec = value
            args.removeFirst()
        case "--out-dir":
            guard let value = args.first else { usage() }
            result.outDir = value
            args.removeFirst()
        case "--stem":
            guard let value = args.first else { usage() }
            result.stem = value
            args.removeFirst()
        case "--help", "-h": usage()
        default:
            if let parsed = Int(arg) { result.index = parsed } else { usage() }
        }
    }
    guard result.index >= 0 && result.index < 32 else {
        fputs("index must be in 0...31\n", stderr)
        exit(2)
    }
    return result
}

typealias JSONObject = [String: Any]

func object(at path: String) throws -> Any {
    try JSONSerialization.jsonObject(with: Data(contentsOf: URL(fileURLWithPath: path)))
}

func merged(_ base: JSONObject, _ override: JSONObject) -> JSONObject {
    var result = base
    if let music = override["music"] as? JSONObject {
        for (key, value) in music { result[key] = value }
    }
    for (key, value) in override { result[key] = value }
    return result
}

func selectedSpec(_ args: Arguments) throws -> (JSONObject, Int) {
    if let path = args.spec {
        let root = try object(at: path)
        if let object = root as? JSONObject { return (merged([:], object), args.index) }
        if let array = root as? [JSONObject], let first = array.first { return (merged([:], first), args.index) }
        return ([:], args.index)
    }
    guard FileManager.default.fileExists(atPath: args.manifest) else { return ([:], args.index) }
    let root = try object(at: args.manifest)
    let array: [JSONObject]
    var defaults: JSONObject = [:]
    if let direct = root as? [JSONObject] { array = direct }
    else if let wrapped = root as? JSONObject, let values = wrapped["variations"] as? [JSONObject] {
        array = values
        defaults = wrapped["defaults"] as? JSONObject ?? [:]
        if defaults["durationSec"] == nil, let duration = wrapped["durationSec"] {
            defaults["durationSec"] = duration
        }
    } else { return ([:], args.index) }
    let selectedIndex: Int
    if let wanted = args.id {
        guard let found = array.firstIndex(where: { string($0, "id", "slug", "name") == wanted }) else {
            fputs("variation id not found: \(wanted)\n", stderr)
            exit(2)
        }
        selectedIndex = found
    } else {
        selectedIndex = args.index
    }
    guard selectedIndex < array.count else { return ([:], selectedIndex) }
    return (merged(defaults, array[selectedIndex]), selectedIndex)
}

func string(_ object: JSONObject, _ keys: String...) -> String? {
    for key in keys {
        if let value = object[key] as? String, !value.isEmpty { return value }
    }
    return nil
}

func number(_ object: JSONObject, _ keys: String...) -> Double? {
    for key in keys {
        if let value = object[key] as? NSNumber { return value.doubleValue }
        if let value = object[key] as? String, let parsed = Double(value) { return parsed }
    }
    return nil
}

func boolean(_ object: JSONObject, _ keys: String...) -> Bool? {
    for key in keys {
        if let value = object[key] as? Bool { return value }
    }
    return nil
}

func sectionRanges(_ description: String) -> [(Double, Double)] {
    let pattern = #"([0-9]+(?:\.[0-9]+)?)-([0-9]+(?:\.[0-9]+)?)"#
    guard let expression = try? NSRegularExpression(pattern: pattern) else { return [] }
    let string = description as NSString
    return expression.matches(in: description, range: NSRange(location: 0, length: string.length)).compactMap { match in
        guard match.numberOfRanges == 3,
              let start = Double(string.substring(with: match.range(at: 1))),
              let end = Double(string.substring(with: match.range(at: 2))) else { return nil }
        return (start, end)
    }
}

func stableHash(_ text: String) -> UInt64 {
    var hash: UInt64 = 1469598103934665603
    for byte in text.utf8 { hash = (hash ^ UInt64(byte)) &* 1099511628211 }
    return hash
}

struct RNG {
    var state: UInt64
    mutating func next() -> UInt64 {
        state = state &* 6364136223846793005 &+ 1442695040888963407
        return state
    }
    mutating func unit() -> Double { Double(next() >> 11) / Double(1 << 53) }
    mutating func integer(_ upper: Int) -> Int { Int(next() % UInt64(max(1, upper))) }
}

let instrumentBank: [(Int, String)] = [
    (0, "Acoustic Grand Piano"), (4, "Electric Piano 1"), (6, "Harpsichord"),
    (8, "Celesta"), (10, "Music Box"), (11, "Vibraphone"), (12, "Marimba"),
    (13, "Xylophone"), (14, "Tubular Bells"), (18, "Rock Organ"),
    (19, "Church Organ"), (24, "Nylon Guitar"), (25, "Steel Guitar"),
    (32, "Acoustic Bass"), (39, "Synth Bass 2"), (40, "Violin"), (41, "Viola"),
    (42, "Cello"), (45, "Pizzicato Strings"), (46, "Orchestral Harp"),
    (48, "String Ensemble 1"), (51, "Synth Strings 2"), (52, "Choir Aahs"),
    (53, "Voice Oohs"), (56, "Trumpet"), (60, "French Horn"), (65, "Alto Sax"),
    (68, "Oboe"), (71, "Clarinet"), (73, "Flute"), (75, "Pan Flute"), (78, "Whistle"),
]
let forms = [
    "ascending-arch", "descending-answer", "mirror-canon", "pedal-bloom",
    "broken-thirds", "syncopated-cascade", "wide-leaps", "suspended-loop",
    "question-answer", "hemiola", "two-note-cell", "staircase",
    "orbit", "bass-led", "upper-voice", "palindrome",
]
let modes = ["ionian", "dorian", "phrygian", "lydian", "mixolydian", "aeolian", "locrian"]
let drumStyles = ["two-step", "breakbeat", "half-time", "garage", "motorik", "jungle", "broken-beat", "minimal"]

func cleanedStem(_ value: String) -> String {
    let legal = value.lowercased().map { character -> Character in
        (character.isLetter || character.isNumber) ? character : "-"
    }
    return String(legal).split(separator: "-").filter { !$0.isEmpty }.joined(separator: "-")
}

let args = parseArguments()
let (raw, variationIndex) = try selectedSpec(args)
let fallbackID = String(format: "%02d-%@", variationIndex + 1, forms[variationIndex % forms.count])
let id = string(raw, "id", "slug", "name") ?? fallbackID
let seed = number(raw, "seed").map { UInt64(max(0, $0)) } ?? stableHash("\(variationIndex):\(id)")
let bpm = max(64, min(176, number(raw, "bpm", "tempo") ?? Double(74 + (variationIndex * 7) % 54)))
let beat = 60.0 / bpm
let duration = max(30, min(34, number(raw, "durationSec", "duration") ?? 32.0))
let defaultInstrument = instrumentBank[variationIndex]
let instrumentObject = raw["instrument"] as? JSONObject ?? [:]
let oneBasedValue = number(raw, "gmProgram", "instrumentNumber") ?? number(instrumentObject, "program", "gmProgram", "number")
let oneBased = oneBasedValue.map { Int($0) - 1 }
let melodyOnly = boolean(raw, "melodyOnly") ?? false
let selectedLeadProgram = max(0, min(127, Int(number(raw, "instrumentProgram") ?? Double(oneBased ?? defaultInstrument.0))))
let leadProgram = melodyOnly ? 0 : selectedLeadProgram
let leadName = melodyOnly ? "Acoustic Grand Piano" : (string(raw, "instrumentName", "instrument") ?? string(instrumentObject, "name", "title") ?? (instrumentBank.first { $0.0 == leadProgram }?.1 ?? "GM Program \(leadProgram + 1)"))
let form = string(raw, "form", "arrangement") ?? forms[variationIndex % forms.count]
let mode = string(raw, "mode", "scale") ?? modes[(variationIndex * 3) % modes.count]
let drumStyle = string(raw, "drumStyle", "drums", "beat") ?? drumStyles[(variationIndex * 5) % drumStyles.count]
let density = max(0.28, min(1.0, number(raw, "density") ?? (0.52 + Double(variationIndex % 5) * 0.09)))
let swing = max(0, min(0.3, number(raw, "swing") ?? Double(variationIndex % 4) * 0.025))
let timeline = sectionRanges(form)
let openingPercussionOffAt = timeline.count >= 5 ? timeline[0].1 : 2.15
let splitAt = timeline.count >= 5 ? timeline[2].0 : 8.3 + Double(variationIndex % 4) * 0.25
let fullPercussionAt = timeline.count >= 5 ? timeline[3].0 : 20.0 + Double(variationIndex % 3) * 0.35
let percussionOffAt = timeline.count >= 5 ? timeline[4].0 : 24.6 + Double(variationIndex % 4) * 0.2
let revealAt = max(1.8, min(7, number(raw, "revealAtSec") ?? (openingPercussionOffAt + 0.8)))
let revealDuration = max(0.5, min(2, number(raw, "revealDurationSec") ?? 1.15))
let exitDuration = max(0.7, min(2, number(raw, "exitDurationSec") ?? 1.1))
let exitAt = duration - exitDuration - 0.4
precondition(0 < openingPercussionOffAt && openingPercussionOffAt < splitAt && splitAt < fullPercussionAt && fullPercussionAt < percussionOffAt && percussionOffAt < duration,
             "variation form must provide ordered sections inside duration")

var rng = RNG(state: seed == 0 ? 1 : seed)
var notes: [Note] = []

func addTone(_ at: Double, _ dur: Double, _ boardIndex: Int, _ velocity: Double) {
    guard at >= 0, at < duration - 0.35 else { return }
    let index = max(0, min(boardMIDIs.count - 1, boardIndex))
    let midi = boardMIDIs[index]
    precondition(visibleRange.contains(midi), "tone outside visible board")
    notes.append(Note(t: at, dur: min(dur, duration - at - 0.12), midi: midi,
                      soundMidi: midi, vel: max(0.08, min(0.92, velocity)), lane: "tone"))
}

func addDrum(_ at: Double, _ displayIndex: Int, _ gmMidi: Int, _ velocity: Double) {
    guard at >= 0, at < percussionOffAt else { return }
    let midi = boardMIDIs[max(0, min(boardMIDIs.count - 1, displayIndex))]
    precondition(visibleRange.contains(midi), "drum outside visible board")
    notes.append(Note(t: at, dur: 0.12, midi: midi, soundMidi: gmMidi,
                      vel: max(0.08, min(0.92, velocity)), lane: "drum"))
}

// Every form has a separate contour contract. Rotation by mode gives each
// contour a different tonal center while retaining exact MenuBand pitches.
func motif(for name: String) -> [Int] {
    switch name {
    case "ascending-arch": return [0, 2, 4, 6, 8, 6, 4, 2]
    case "descending-answer": return [9, 7, 5, 3, 1, 4, 2, 0]
    case "mirror-canon": return [1, 4, 6, 3, 8, 5, 3, 6]
    case "pedal-bloom": return [0, 4, 0, 6, 0, 8, 2, 0]
    case "broken-thirds": return [0, 4, 2, 6, 4, 8, 6, 10]
    case "syncopated-cascade": return [10, 7, 9, 5, 6, 3, 4, 1]
    case "wide-leaps": return [1, 8, 3, 10, 5, 12, 7, 2]
    case "suspended-loop": return [3, 6, 8, 6, 4, 7, 9, 7]
    case "question-answer": return [2, 5, 7, 9, 4, 7, 5, 2]
    case "hemiola": return [0, 3, 6, 2, 5, 8, 4, 7]
    case "two-note-cell": return [2, 3, 2, 5, 3, 6, 2, 7]
    case "staircase": return [0, 1, 3, 4, 6, 7, 9, 10]
    case "orbit": return [5, 8, 3, 9, 2, 7, 4, 6]
    case "bass-led": return [0, 7, 1, 6, 2, 8, 3, 5]
    case "upper-voice": return [8, 10, 12, 9, 11, 7, 10, 6]
    case "palindrome": return [0, 3, 7, 10, 7, 3, 0, 5]
    default:
        return (0..<8).map { _ in rng.integer(boardMIDIs.count) }
    }
}

let modeOffset = modes.firstIndex(of: mode.lowercased()) ?? (variationIndex % 7)
let baseMotif = motif(for: form)
func pitch(_ motifValue: Int, transpose: Int = 0) -> Int {
    let rawIndex = motifValue + modeOffset + transpose
    return (rawIndex % boardMIDIs.count + boardMIDIs.count) % boardMIDIs.count
}

// A small off-stage hat current precedes the instrument. Density and index
// alter the spacing, but the opening always clears before the lead arrives.
let openingStep = variationIndex % 3 == 0 ? 0.33 : (variationIndex % 3 == 1 ? 0.42 : 0.5)
var openingCursor = 0.22
var openingHit = 0
while openingCursor < openingPercussionOffAt {
    if melodyOnly {
        let motifIndex = baseMotif[openingHit % baseMotif.count]
        addTone(openingCursor, openingStep * 0.82, pitch(motifIndex),
                openingHit % 2 == 0 ? 0.42 : 0.34)
    } else {
        addDrum(openingCursor, 11, openingHit % 5 == 4 ? 46 : 42,
                openingHit % 2 == 0 ? 0.72 : 0.58)
    }
    openingCursor += openingStep
    openingHit += 1
}

// Intro: exposed motif, intentionally spacious while the keyboard approaches.
let introStart = openingPercussionOffAt + 0.12
let introStep = max(0.28, beat * (variationIndex % 4 == 0 ? 0.75 : 0.5))
for i in 0..<baseMotif.count {
    let stagger = (i % 2 == 1 ? swing * introStep : 0)
    let at = introStart + Double(i) * introStep + stagger
    guard at < splitAt - 0.25 else { break }
    addTone(at, introStep * (0.58 + rng.unit() * 0.55), pitch(baseMotif[i]), 0.36 + rng.unit() * 0.16)
    if i % (variationIndex % 3 + 3) == 0 {
        addTone(at, introStep * 1.7, pitch(baseMotif[i], transpose: -7), 0.16 + rng.unit() * 0.08)
    }
}

// Groove: the motif is transformed each pass instead of copied. Reversal,
// rotation, register exchange, rests, and step-size define distinct forms.
var phrase = 0
var grooveCursor = splitAt
let grooveStep = max(0.18, beat * (variationIndex % 5 == 0 ? 0.375 : 0.5))
while grooveCursor < fullPercussionAt - grooveStep {
    let transformed: [Int]
    switch phrase % 4 {
    case 1: transformed = Array(baseMotif.reversed())
    case 2: transformed = Array(baseMotif.dropFirst(2) + baseMotif.prefix(2))
    case 3: transformed = baseMotif.enumerated().map { $0.offset % 2 == 0 ? $0.element + 2 : $0.element - 1 }
    default: transformed = baseMotif
    }
    for i in transformed.indices {
        let at = grooveCursor + Double(i) * grooveStep + (i % 2 == 1 ? swing * grooveStep : 0)
        guard at < fullPercussionAt - 0.08 else { break }
        if rng.unit() < 0.24 * (1 - density) { continue }
        let octaveExchange = (phrase + i + variationIndex) % 7 == 0 ? 7 : 0
        addTone(at, grooveStep * (0.45 + rng.unit() * 0.7),
                pitch(transformed[i], transpose: octaveExchange), 0.27 + rng.unit() * 0.22)
        if i % 4 == 0 && variationIndex % 2 == 0 {
            addTone(at, grooveStep * 2.3, pitch(transformed[i], transpose: -7), 0.14 + rng.unit() * 0.08)
        }
    }
    grooveCursor += Double(transformed.count) * grooveStep
    phrase += 1
}

// Right-side groove percussion. Each named style changes kick/snare geometry;
// hats remain tied to their actual QWERTY/MenuBand stereo positions.
let half = beat / 2
var drumCursor = splitAt
var step = 0
while !melodyOnly && drumCursor < fullPercussionAt {
    let local = step % 8
    let kick: Bool
    let snare: Bool
    switch drumStyle {
    case "two-step": kick = [0, 5].contains(local); snare = [2, 6].contains(local)
    case "breakbeat": kick = [0, 3, 7].contains(local); snare = [2, 6].contains(local)
    case "half-time": kick = [0, 5].contains(local); snare = local == 4
    case "garage": kick = [0, 3, 5].contains(local); snare = [2, 6].contains(local)
    case "motorik": kick = local % 2 == 0; snare = [2, 6].contains(local)
    case "jungle": kick = [0, 3, 5, 7].contains(local); snare = [2, 4, 6].contains(local)
    case "broken-beat": kick = [0, 2, 5].contains(local); snare = [3, 6].contains(local)
    default: kick = local == 0; snare = local == 4
    }
    if kick { addDrum(drumCursor, 7, 36, 0.48 + rng.unit() * 0.10) }
    if snare { addDrum(drumCursor, 8, 38, 0.43 + rng.unit() * 0.10) }
    if density > 0.42 || local % 2 == 0 {
        addDrum(drumCursor + (local % 2 == 1 ? swing * half : 0), 11,
                local == 7 ? 46 : 42, 0.22 + rng.unit() * 0.13)
    }
    if local == 6 && variationIndex % 3 == 1 { addDrum(drumCursor, 9, 39, 0.31) }
    drumCursor += half
    step += 1
}

// Full-board break: left side runs double-time and is never rhythmically
// equivalent to the right side, even when both sides address the same GM drum.
let breakStep = max(0.105, beat / 4)
var breakCursor = fullPercussionAt
var breakIndex = 0
while breakCursor < percussionOffAt {
    if melodyOnly {
        let melodicIndex = baseMotif[breakIndex % baseMotif.count]
        addTone(breakCursor, breakStep * 0.78, pitch(melodicIndex),
                breakIndex % 4 == 0 ? 0.48 : 0.34)
        if breakIndex % 4 == 0 {
            addTone(breakCursor, breakStep * 2.6,
                    pitch(melodicIndex, transpose: -7), 0.24)
        }
    } else {
        addDrum(breakCursor, 4, breakIndex % 8 == 7 ? 46 : 42,
                breakIndex % 4 == 0 ? 0.46 : 0.29)
        if [0, 3, 7, 10].contains(breakIndex % 12) { addDrum(breakCursor, 0, 36, 0.52) }
        if [2, 6, 9].contains(breakIndex % 12) { addDrum(breakCursor, 1, 38, 0.47) }
        if breakIndex % 8 == 5 { addDrum(breakCursor, 2, 39, 0.34) }
        if breakIndex % 4 == 0 {
            addDrum(breakCursor, 11, 42, 0.33)
            if breakIndex % 8 == 0 { addDrum(breakCursor, 7, 36, 0.55) }
            else { addDrum(breakCursor, 8, 38, 0.50) }
        }
    }
    breakCursor += breakStep
    breakIndex += 1
}

// Graceful ending: percussion is completely gone; a slowed, thinned version
// of the opening contour resolves to the modal center before the board exits.
let codaMotif = variationIndex % 2 == 0 ? Array(baseMotif.reversed()) : baseMotif
let codaStart = percussionOffAt + 0.12
let codaWindow = exitAt - codaStart - 0.15
let codaStep = max(0.36, codaWindow / Double(codaMotif.count + 1))
for i in codaMotif.indices {
    let at = codaStart + Double(i) * codaStep
    guard at < exitAt - 0.2 else { break }
    if i > 0 && i % 3 == 1 && density < 0.65 { continue }
    addTone(at, codaStep * (i == codaMotif.count - 1 ? 1.8 : 0.8),
            pitch(codaMotif[i]), 0.34 + (i == 0 ? 0.08 : 0))
}
addTone(min(exitAt - 0.4, duration - 1.5), 1.15, modeOffset, 0.48)

notes.sort { $0.t == $1.t ? ($0.lane, $0.midi) < ($1.lane, $1.midi) : $0.t < $1.t }

// Fail before touching AVAudioEngine if the score violates reel invariants.
precondition(!notes.isEmpty, "empty score")
precondition(notes.allSatisfy { visibleRange.contains($0.midi) }, "visual MIDI escaped 60...83")
if melodyOnly {
    precondition(!notes.contains { $0.lane == "drum" }, "percussion entered melody-only score")
} else {
    precondition(!notes.contains { $0.lane == "tone" && $0.t >= fullPercussionAt && $0.t < percussionOffAt }, "tone entered full percussion break")
    precondition(!notes.contains { $0.lane == "drum" && $0.t >= percussionOffAt }, "drum entered graceful coda")
}
precondition(notes.allSatisfy { $0.t >= 0 && $0.t + $0.dur <= duration + 0.001 }, "event escaped duration")

// Exact MenuBandLayout.panByKeyCode model: physical QWERTY position, row
// offsets, and the app's +/-0.9 stereo span. A sampler per display key keeps
// simultaneous events spatially independent.
let keyCodeByDisplayMidi: [Int: Int] = [
    60: 8, 62: 2, 64: 14, 65: 3, 67: 5, 69: 0, 71: 11,
    72: 4, 74: 34, 76: 38, 77: 40, 79: 37, 81: 46, 83: 45,
]
let qwertyRows: [[Int]] = [
    [12, 13, 14, 15, 17, 16, 32, 34, 31, 35],
    [0, 1, 2, 3, 5, 4, 38, 40, 37, 41, 39],
    [6, 7, 8, 9, 11, 45, 46],
]
let qwertyRowOffsets = [0.0, 0.5, 1.0]
func menuBandPan(_ displayMidi: Int) -> Float {
    guard let keyCode = keyCodeByDisplayMidi[displayMidi] else { return 0 }
    for row in qwertyRows.indices {
        if let column = qwertyRows[row].firstIndex(of: keyCode) {
            let x = Double(column) + qwertyRowOffsets[row]
            return Float((x / 10.5 * 2 - 1) * 0.9)
        }
    }
    return 0
}
func menuBandDistanceGain(_ displayMidi: Int) -> Float {
    -4.5 * abs(menuBandPan(displayMidi)) / 0.9
}

let sampleRate = 48_000.0
let engine = AVAudioEngine()
let toneMixer = AVAudioMixerNode()
let drumMixer = AVAudioMixerNode()
let stageMixer = AVAudioMixerNode()
let stageEQ = AVAudioUnitEQ(numberOfBands: 1)
let reverb = AVAudioUnitReverb()
reverb.loadFactoryPreset(.mediumHall)
reverb.wetDryMix = Float(13 + variationIndex % 6 * 3)
stageEQ.bands[0].filterType = .lowPass
stageEQ.bands[0].bypass = false

engine.attach(toneMixer)
engine.attach(drumMixer)
engine.attach(stageMixer)
engine.attach(stageEQ)
engine.attach(reverb)
engine.connect(toneMixer, to: reverb, format: nil)
engine.connect(reverb, to: stageMixer, fromBus: 0, toBus: 0, format: nil)
engine.connect(stageMixer, to: stageEQ, format: nil)
engine.connect(stageEQ, to: engine.mainMixerNode, format: nil)
// The lead belongs to the moving board, so its gain and low-pass follow the
// board's offstage distance. Opening percussion falls in from above the frame:
// route it around that filter so the first hats stay crisp before reveal.
engine.connect(drumMixer, to: engine.mainMixerNode, format: nil)

let bank = URL(fileURLWithPath: "/System/Library/Components/CoreAudio.component/Contents/Resources/gs_instruments.dls")
guard FileManager.default.fileExists(atPath: bank.path) else { fatalError("macOS GM sound bank not found") }
var toneSamplers: [Int: AVAudioUnitSampler] = [:]
var drumSamplers: [Int: AVAudioUnitSampler] = [:]
for (bus, midi) in Set(notes.filter { $0.lane == "tone" }.map(\.midi)).sorted().enumerated() {
    let sampler = AVAudioUnitSampler()
    engine.attach(sampler)
    try sampler.loadSoundBankInstrument(at: bank, program: UInt8(leadProgram),
                                        bankMSB: UInt8(kAUSampler_DefaultMelodicBankMSB),
                                        bankLSB: UInt8(kAUSampler_DefaultBankLSB))
    sampler.stereoPan = menuBandPan(midi)
    sampler.masterGain = menuBandDistanceGain(midi)
    engine.connect(sampler, to: toneMixer, fromBus: 0, toBus: AVAudioNodeBus(bus), format: nil)
    toneSamplers[midi] = sampler
}
for (bus, midi) in Set(notes.filter { $0.lane == "drum" }.map(\.midi)).sorted().enumerated() {
    let sampler = AVAudioUnitSampler()
    engine.attach(sampler)
    try sampler.loadSoundBankInstrument(at: bank, program: 0, bankMSB: 0x78, bankLSB: 0)
    sampler.stereoPan = menuBandPan(midi)
    sampler.masterGain = menuBandDistanceGain(midi)
    engine.connect(sampler, to: drumMixer, fromBus: 0, toBus: AVAudioNodeBus(bus), format: nil)
    drumSamplers[midi] = sampler
}

guard let format = AVAudioFormat(standardFormatWithSampleRate: sampleRate, channels: 2) else {
    fatalError("could not create render format")
}
try engine.enableManualRenderingMode(.offline, format: format, maximumFrameCount: 2048)
engine.mainMixerNode.outputVolume = 0.86
drumMixer.outputVolume = 0.96
engine.prepare()
try engine.start()

struct MIDIEvent {
    let frame: AVAudioFramePosition
    let on: Bool
    let drum: Bool
    let displayMidi: Int
    let soundMidi: UInt8
    let velocity: UInt8
}
var events: [MIDIEvent] = []
for note in notes {
    events.append(MIDIEvent(frame: AVAudioFramePosition((note.t * sampleRate).rounded()), on: true,
                            drum: note.lane == "drum", displayMidi: note.midi,
                            soundMidi: UInt8(note.soundMidi),
                            velocity: UInt8(max(1, min(127, Int((note.vel * 127).rounded()))))))
    events.append(MIDIEvent(frame: AVAudioFramePosition(((note.t + note.dur) * sampleRate).rounded()), on: false,
                            drum: note.lane == "drum", displayMidi: note.midi,
                            soundMidi: UInt8(note.soundMidi), velocity: 0))
}
events.sort { $0.frame == $1.frame ? (!$0.on && $1.on) : $0.frame < $1.frame }

let outputDirectory = args.outDir ?? "pop/menuband/out/variations/\(cleanedStem(id))"
let outDir = URL(fileURLWithPath: outputDirectory, isDirectory: true)
try FileManager.default.createDirectory(at: outDir, withIntermediateDirectories: true)
let configuredStem = args.stem ?? string(raw, "outputStem")
let stem = cleanedStem(configuredStem ?? id)
let wavURL = outDir.appendingPathComponent("\(stem).wav")
let scoreURL = outDir.appendingPathComponent("\(stem).notes.json")
let audioFile = try AVAudioFile(forWriting: wavURL, settings: format.settings,
                                commonFormat: .pcmFormatFloat32, interleaved: false)
guard let buffer = AVAudioPCMBuffer(pcmFormat: engine.manualRenderingFormat, frameCapacity: 2048) else {
    fatalError("could not make render buffer")
}

let totalFrames = AVAudioFramePosition((duration * sampleRate).rounded())
var cursor: AVAudioFramePosition = 0
var eventIndex = 0
while cursor < totalFrames {
    while eventIndex < events.count && events[eventIndex].frame <= cursor {
        let event = events[eventIndex]
        guard let sampler = event.drum ? drumSamplers[event.displayMidi] : toneSamplers[event.displayMidi] else {
            fatalError("missing sampler for display MIDI \(event.displayMidi)")
        }
        if event.on { sampler.startNote(event.soundMidi, withVelocity: event.velocity, onChannel: 0) }
        else { sampler.stopNote(event.soundMidi, onChannel: 0) }
        eventIndex += 1
    }
    let now = Double(cursor) / sampleRate
    let presence: Double
    if melodyOnly {
        presence = sin(.pi * max(0, min(1, now / duration)))
    } else {
        let reveal = max(0, min(1, (now - revealAt) / revealDuration))
        let revealEase = 1 - pow(1 - reveal, 3)
        let exit = max(0, min(1, (now - exitAt) / exitDuration))
        let exitEase = exit * exit * exit
        presence = revealEase * (1 - exitEase)
    }
    stageMixer.outputVolume = Float(0.32 + presence * 0.68)
    stageEQ.bands[0].frequency = Float(2_600 + presence * 15_400)
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
    @unknown default: fatalError("unknown offline audio render status")
    }
}
engine.stop()

let score = Score(id: id, index: variationIndex, seed: seed, bpm: bpm, beatSec: beat,
                  barSec: beat * 4, durationSec: duration,
                  openingPercussionOffAtSec: openingPercussionOffAt,
                  splitAtSec: splitAt, fullPercussionAtSec: fullPercussionAt,
                  percussionOffAtSec: percussionOffAt, revealAtSec: revealAt,
                  revealDurationSec: revealDuration, exitAtSec: exitAt,
                  exitDurationSec: exitDuration,
                  leadPreset: "\(leadProgram + 1) \(leadName)", leadProgram: leadProgram,
                  form: form, mode: mode, drumStyle: drumStyle, notes: notes)
let encoder = JSONEncoder()
encoder.outputFormatting = [.prettyPrinted, .sortedKeys, .withoutEscapingSlashes]
try encoder.encode(score).write(to: scoreURL, options: .atomic)

print("✓ \(wavURL.path) (\(String(format: "%.2f", duration))s)")
print("✓ \(scoreURL.path) (\(notes.count) events, all visual MIDI 60...83)")
print("  #\(variationIndex + 1) · \(leadProgram + 1) \(leadName) · \(form) · \(mode) · \(drumStyle) · seed \(seed)")
