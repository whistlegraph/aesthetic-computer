#!/usr/bin/env swift
// Render the Menu Band reel performance with macOS's bundled GM bank:
// instrument 79 Whistle (zero-based program 78), then right-side percussion.

import AVFoundation
import AudioToolbox
import Foundation

let root = URL(fileURLWithPath: FileManager.default.currentDirectoryPath)
let outDir = root.appendingPathComponent("pop/menuband/out", isDirectory: true)
try FileManager.default.createDirectory(at: outDir, withIntermediateDirectories: true)

let sampleRate = 48_000.0
let bpm = 76.0
let beat = 60.0 / bpm
let opening = 0.28
let splitBeat = 8.0
let fullPercussionBeat = 18.0
let percussionOffBeat = 22.0
let tail = 1.20
let visibleRange = 60...83

struct Note: Codable {
    let t: Double
    let dur: Double
    let midi: Int       // board/display MIDI; always inside the visible range
    let soundMidi: Int  // GM note sent to the selected sampler
    let vel: Double
    let lane: String    // tone | drum
}

struct Score: Codable {
    let bpm: Double
    let beatSec: Double
    let barSec: Double
    let durationSec: Double
    let openingPercussionOffAtSec: Double
    let splitAtSec: Double
    let fullPercussionAtSec: Double
    let percussionOffAtSec: Double
    let leadPreset: String
    let notes: [Note]
}

var notes: [Note] = []
func tone(_ beatAt: Double, _ beatDur: Double, _ midi: Int, _ velocity: Double) {
    precondition(visibleRange.contains(midi), "tone outside visible board")
    notes.append(Note(t: opening + beatAt * beat, dur: beatDur * beat,
                      midi: midi, soundMidi: midi, vel: velocity, lane: "tone"))
}
func drum(_ beatAt: Double, _ displayMidi: Int, _ gmMidi: Int, _ velocity: Double) {
    precondition(visibleRange.contains(displayMidi), "drum outside visible board")
    notes.append(Note(t: opening + beatAt * beat, dur: 0.16,
                      midi: displayMidi, soundMidi: gmMidi, vel: velocity, lane: "drum"))
}

// Opening: hats alone first, then a spare Whistle phrase while the board is
// still visually far away / hidden above the frame.
for (at, dur, midi, vel) in [
    (2.0, 0.65, 72, 0.46), (2.75, 0.35, 76, 0.41),
    (3.25, 0.65, 79, 0.47), (4.0, 0.45, 76, 0.42),
    (4.5, 0.45, 74, 0.39), (5.0, 0.70, 71, 0.40),
    (5.75, 0.35, 72, 0.38), (6.25, 0.45, 76, 0.43),
    (6.75, 0.35, 79, 0.45), (7.25, 0.65, 76, 0.44),
] { tone(at, dur, midi, vel) }
for (at, dur, midi, vel) in [
    (2.0, 1.35, 60, 0.23), (3.5, 0.65, 67, 0.20),
    (4.25, 1.25, 69, 0.21), (5.75, 0.65, 65, 0.19),
    (6.5, 1.20, 67, 0.21),
] { tone(at, dur, midi, vel) }

// A few quiet closed-hat drops tease the percussion while the keyboard is
// still hidden. Display MIDI 79 is the physical `l` key, so its symbol and
// stereo position already match the board that will later slide into view.
for (index, at) in [0.0, 0.5, 1.0, 1.5].enumerated() {
    drum(at, 79, 42, index % 2 == 0 ? 0.44 : 0.36)
}

// After `]`: a developed, syncopated left-octave variation answers the
// right-side beat. Quarter-beat turns keep it composed rather than looped.
for (at, dur, midi, vel) in [
    (8.0, 0.65, 60, 0.37), (8.75, 0.22, 64, 0.29),
    (9.0, 0.42, 67, 0.32), (9.5, 0.30, 71, 0.28),
    (10.0, 0.55, 69, 0.35), (10.75, 0.22, 65, 0.27),
    (11.0, 0.42, 62, 0.31), (11.5, 0.35, 67, 0.30),
    (12.0, 0.65, 60, 0.37), (12.75, 0.22, 64, 0.29),
    (13.0, 0.30, 67, 0.31), (13.5, 0.25, 69, 0.28),
    (13.75, 0.22, 71, 0.27), (14.0, 0.55, 65, 0.33),
    (14.75, 0.22, 62, 0.27), (15.0, 0.42, 67, 0.31),
    (15.5, 0.30, 60, 0.34), (16.0, 0.42, 64, 0.31),
    (16.5, 0.25, 69, 0.29), (16.75, 0.22, 71, 0.27),
    (17.0, 0.38, 67, 0.31), (17.5, 0.38, 60, 0.36),
] { tone(at, dur, midi, vel) }

// After the second shake, every percussion layer drops away and Whistle
// closes alone with a slower final arch.
for (at, dur, midi, vel) in [
    (22.0, 1.00, 72, 0.44), (23.0, 0.35, 74, 0.38),
    (23.5, 0.65, 76, 0.43), (24.25, 0.35, 79, 0.42),
    (24.75, 0.55, 83, 0.45), (25.5, 0.45, 79, 0.41),
    (26.0, 0.35, 76, 0.39), (26.5, 0.35, 74, 0.37),
    (27.0, 1.50, 72, 0.48),
] { tone(at, dur, midi, vel) }
for (at, dur, midi, vel) in [
    (22.0, 1.8, 60, 0.22), (24.0, 1.8, 67, 0.20),
    (26.0, 0.9, 65, 0.19), (27.0, 1.5, 60, 0.23),
] { tone(at, dur, midi, vel) }

// Right-octave drum mapping mirrors Menu Band's pitch-class kit:
// C kick, D snare, E clap, G closed hat, A open hat, B ride.
for step in 0..<28 {
    let at = splitBeat + Double(step) * 0.5
    drum(at, 79, 42, step % 2 == 0 ? 0.34 : 0.27) // closed hat
}
for at in [8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0] { drum(at, 72, 36, 0.58) }
for at in [9.0, 11.0, 13.0, 15.0, 17.0, 19.0, 21.0] { drum(at, 74, 38, 0.52) }
for at in [11.0, 15.0, 19.0] { drum(at, 76, 39, 0.36) }
drum(15.5, 81, 46, 0.30)
drum(19.5, 81, 46, 0.30)

// Four-beat full-board drum-and-bass break. The right octave keeps the
// established half-time backbone; the left octave is deliberately different:
// double-time hats plus syncopated kick/snare/clap ghosts at a 152-BPM feel.
for step in 0..<16 {
    drum(fullPercussionBeat + Double(step) * 0.25, 67, 42,
         step % 4 == 0 ? 0.43 : (step % 2 == 0 ? 0.34 : 0.26))
}
for at in [18.5, 20.25, 21.5] { drum(at, 60, 36, 0.54) }
for at in [18.75, 19.5, 20.75, 21.5] { drum(at, 62, 38, 0.48) }
for at in [19.25, 20.5, 21.75] { drum(at, 64, 39, 0.38) }
drum(21.875, 71, 51, 0.40)

notes.sort { $0.t == $1.t ? $0.midi < $1.midi : $0.t < $1.t }
let musicalEnd = notes.map { $0.t + $0.dur }.max() ?? 0
let total = musicalEnd + tail
let stageRevealAt = 3.0
let stageRevealDuration = 1.15
let stageExitAt = total - 1.5
let stageExitDuration = 1.1

// Menu Band pans by the physical QWERTY letter position—not by pitch. This
// reproduces MenuBandLayout.panByKeyCode exactly (row offsets and ±0.9 span),
// then gives each visible key its own sampler so simultaneous notes retain
// independent spatial positions in the offline render.
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
    for row in 0..<qwertyRows.count {
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

let engine = AVAudioEngine()
let toneMixer = AVAudioMixerNode()
let drumMixer = AVAudioMixerNode()
let stageMixer = AVAudioMixerNode()
let stageEQ = AVAudioUnitEQ(numberOfBands: 1)
let reverb = AVAudioUnitReverb()
reverb.loadFactoryPreset(.mediumHall)
reverb.wetDryMix = 20
stageEQ.bands[0].filterType = .lowPass
stageEQ.bands[0].bypass = false

engine.attach(toneMixer)
engine.attach(drumMixer)
engine.attach(stageMixer)
engine.attach(stageEQ)
engine.attach(reverb)
engine.connect(toneMixer, to: reverb, format: nil)
engine.connect(reverb, to: stageMixer, fromBus: 0, toBus: 0, format: nil)
engine.connect(drumMixer, to: stageMixer, fromBus: 0, toBus: 1, format: nil)
engine.connect(stageMixer, to: stageEQ, format: nil)
engine.connect(stageEQ, to: engine.mainMixerNode, format: nil)

let bank = URL(fileURLWithPath: "/System/Library/Components/CoreAudio.component/Contents/Resources/gs_instruments.dls")
var toneSamplers: [Int: AVAudioUnitSampler] = [:]
var drumSamplers: [Int: AVAudioUnitSampler] = [:]
for (bus, midi) in Set(notes.filter { $0.lane == "tone" }.map(\.midi)).sorted().enumerated() {
    let sampler = AVAudioUnitSampler()
    engine.attach(sampler)
    try sampler.loadSoundBankInstrument(
        at: bank, program: 78,
        bankMSB: UInt8(kAUSampler_DefaultMelodicBankMSB),
        bankLSB: UInt8(kAUSampler_DefaultBankLSB)
    )
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
    fatalError("could not make render format")
}
try engine.enableManualRenderingMode(.offline, format: format, maximumFrameCount: 2048)
engine.mainMixerNode.outputVolume = 0.86
engine.prepare()
try engine.start()

struct MIDIEvent {
    let frame: AVAudioFramePosition
    let on: Bool
    let drum: Bool
    let displayMidi: Int
    let midi: UInt8
    let velocity: UInt8
}
var events: [MIDIEvent] = []
for note in notes {
    events.append(MIDIEvent(
        frame: AVAudioFramePosition((note.t * sampleRate).rounded()), on: true,
        drum: note.lane == "drum", displayMidi: note.midi, midi: UInt8(note.soundMidi),
        velocity: UInt8(max(1, min(127, Int((note.vel * 127).rounded()))))
    ))
    events.append(MIDIEvent(
        frame: AVAudioFramePosition(((note.t + note.dur) * sampleRate).rounded()), on: false,
        drum: note.lane == "drum", displayMidi: note.midi, midi: UInt8(note.soundMidi), velocity: 0
    ))
}
events.sort { $0.frame == $1.frame ? (!$0.on && $1.on) : $0.frame < $1.frame }

let wavURL = outDir.appendingPathComponent("menuband-graceful-piano.wav")
let audioFile = try AVAudioFile(forWriting: wavURL, settings: format.settings,
                                commonFormat: .pcmFormatFloat32, interleaved: false)
guard let buffer = AVAudioPCMBuffer(pcmFormat: engine.manualRenderingFormat, frameCapacity: 2048) else {
    fatalError("could not make render buffer")
}

let totalFrames = AVAudioFramePosition((total * sampleRate).rounded())
var cursor: AVAudioFramePosition = 0
var eventIndex = 0
while cursor < totalFrames {
    while eventIndex < events.count && events[eventIndex].frame <= cursor {
        let event = events[eventIndex]
        guard let sampler = event.drum
                ? drumSamplers[event.displayMidi]
                : toneSamplers[event.displayMidi] else {
            fatalError("missing spatial sampler for display MIDI \(event.displayMidi)")
        }
        if event.on { sampler.startNote(event.midi, withVelocity: event.velocity, onChannel: 0) }
        else { sampler.stopNote(event.midi, onChannel: 0) }
        eventIndex += 1
    }
    let now = Double(cursor) / sampleRate
    let reveal = max(0, min(1, (now - stageRevealAt) / stageRevealDuration))
    let revealEase = 1 - pow(1 - reveal, 3)
    let exit = max(0, min(1, (now - stageExitAt) / stageExitDuration))
    let exitEase = exit * exit * exit
    let presence = revealEase * (1 - exitEase)
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

let score = Score(bpm: bpm, beatSec: beat, barSec: beat * 4,
                  durationSec: total,
                  openingPercussionOffAtSec: opening + 1.75 * beat,
                  splitAtSec: opening + splitBeat * beat,
                  fullPercussionAtSec: opening + fullPercussionBeat * beat,
                  percussionOffAtSec: opening + percussionOffBeat * beat,
                  leadPreset: "79 Whistle", notes: notes)
let encoder = JSONEncoder()
encoder.outputFormatting = [.prettyPrinted, .sortedKeys, .withoutEscapingSlashes]
let scoreURL = outDir.appendingPathComponent("menuband-graceful-piano.notes.json")
try encoder.encode(score).write(to: scoreURL)

print("✓ \(wavURL.path) (\(String(format: "%.2f", total))s)")
print("✓ \(scoreURL.path) (\(notes.count) in-range events; ] \(String(format: "%.2f", score.splitAtSec))s; full break \(String(format: "%.2f", score.fullPercussionAtSec))–\(String(format: "%.2f", score.percussionOffAtSec))s)")
