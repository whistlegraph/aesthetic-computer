// ChartModel.swift — the chart, in memory, and the sidecar it writes back.
//
// Reads pop/<lane>/vox4/.wizard.json (bin/wizard.py) and writes
// pop/<lane>/chart-edits.json, which halo3.py merges over its CHART
// literal. The GUI never touches the Python: the CHART keeps the prose
// explaining every number, and this file keeps only numbers.
import Foundation

struct Unit: Codable {
    var t: String              // the word or syllable as it is sung
    var beat: Double           // where the block sits on the grid
    var dur: Double            // how many beats it holds
    var st: Double             // her measured semitone, vs the lane tonic
    var src0: Double           // which piece of the take the block plays,
    var src1: Double           // in slice seconds

    // which CHART knob owns this block's left edge (halo3 emits it)
    var pin: Int?              // pre-split word index
    var cut: CutKind?          // nil = the word's own start; k = its k-th
                               // syllable cut; .auto = halo3 found it
}

enum CutKind: Codable, Equatable {
    case syllable(Int)
    case auto

    init(from decoder: Decoder) throws {
        let c = try decoder.singleValueContainer()
        if let k = try? c.decode(Int.self) { self = .syllable(k) }
        else { self = .auto }
    }
    func encode(to encoder: Encoder) throws {
        var c = encoder.singleValueContainer()
        switch self {
        case .syllable(let k): try c.encode(k)
        case .auto: try c.encode("auto")
        }
    }
    var isDraggable: Bool { self != .auto }
}

struct Event: Codable {
    var a: Double
    var b: Double
    var kind: String           // NOTE · FRIC · PUFF
    var st: Double?
}

struct Frames: Codable {
    var st: [Double?]          // semitones vs tonic, nil where unvoiced
    var db: [Double]           // level, dB below the take's peak
    var hf: [Double]           // share of energy above 3 kHz
}

struct Phrase: Codable {
    var slice: String
    var wav: String
    var sr: Int
    var leadIn: Double
    var beats: Double
    var units: [Unit]
    var events: [Event]
    var frames: Frames
}

struct ChartDoc: Codable {
    var lane: String
    var bpm: Double
    var tonic: Double
    var frame_s: Double
    var phrases: [String: Phrase]
}

// ── what a drag writes ────────────────────────────────────────────────
// Only the four knobs the roll can move. Everything else in the CHART —
// the stretch caps, nohold, end — stays where its reasons are written.
struct PhraseEdits: Codable {
    var times: [String: Double]?
    var sylls: [String: [[SyllCut]]]?
    var durs: [String: Double]?
    var gaps: [String: Double]?
}

// a syllable cut is [seconds-or-null, label] in the Python literal, so it
// has to survive a round trip through a heterogeneous JSON array.
enum SyllCut: Codable {
    case time(Double)
    case none
    case label(String)

    init(from decoder: Decoder) throws {
        let c = try decoder.singleValueContainer()
        if c.decodeNil() { self = .none }
        else if let d = try? c.decode(Double.self) { self = .time(d) }
        else { self = .label(try c.decode(String.self)) }
    }
    func encode(to encoder: Encoder) throws {
        var c = encoder.singleValueContainer()
        switch self {
        case .time(let d): try c.encode(d)
        case .none: try c.encodeNil()
        case .label(let s): try c.encode(s)
        }
    }
}

final class ChartModel {
    let doc: ChartDoc
    let laneDir: URL
    private(set) var name: String
    private(set) var units: [Unit]
    private(set) var dirty = false

    var phrase: Phrase { doc.phrases[name]! }
    var bpm: Double { doc.bpm }
    var secondsPerBeat: Double { 60.0 / doc.bpm }

    init(wizardJSON: URL) throws {
        let data = try Data(contentsOf: wizardJSON)
        doc = try JSONDecoder().decode(ChartDoc.self, from: data)
        // …/pop/<lane>/vox4/.wizard.json → …/pop/<lane>
        laneDir = wizardJSON.deletingLastPathComponent().deletingLastPathComponent()
        guard let first = doc.phrases.keys.sorted().first else {
            throw NSError(domain: "ChartWizard", code: 1, userInfo: [
                NSLocalizedDescriptionKey: "no phrases in \(wizardJSON.lastPathComponent)"])
        }
        name = first
        units = doc.phrases[first]!.units
    }

    func select(phrase p: String) {
        guard let ph = doc.phrases[p] else { return }
        name = p
        units = ph.units
    }

    // ── the two drags ────────────────────────────────────────────────
    // A block's LEFT EDGE is where her word begins in the take. Moving it
    // moves the previous block's end with it — the warp is one sequential
    // frame map, so units cannot overlap and cannot leave a hole.
    func moveBoundary(_ i: Int, toSource t: Double) {
        guard i > 0, i < units.count, units[i].cut?.isDraggable ?? true else { return }
        let lo = units[i - 1].src0 + 0.040        // never starve a neighbour
        let hi = units[i].src1 - 0.040
        let t = min(max(t, lo), hi)
        units[i].src0 = t
        units[i - 1].src1 = t
        dirty = true
    }

    // A block's POSITION on the grid. Moving it right takes beats from the
    // rest before it and gives them back to the rest after, so everything
    // downstream keeps the bar it already has — which is how this chart has
    // been tuned by hand all along.
    func moveBlock(_ i: Int, toBeat b: Double) {
        guard i >= 0, i < units.count else { return }
        let lo = i > 0 ? units[i - 1].beat + units[i - 1].dur : 0
        let hi = i + 1 < units.count ? units[i + 1].beat - units[i].dur : doc.phrases[name]!.beats - units[i].dur
        units[i].beat = min(max(b, lo), max(lo, hi))
        dirty = true
    }

    // A block's RIGHT EDGE is how long it holds — the beats it is warped
    // into, not how much audio it owns.
    func resizeBlock(_ i: Int, toDur d: Double) {
        guard i >= 0, i < units.count else { return }
        let hi = i + 1 < units.count ? units[i + 1].beat - units[i].beat : doc.phrases[name]!.beats - units[i].beat
        units[i].dur = min(max(d, 0.25), max(0.25, hi))
        dirty = true
    }

    func revert() {
        units = doc.phrases[name]!.units
        dirty = false
    }

    // ── the sidecar ──────────────────────────────────────────────────
    var editsURL: URL { laneDir.appendingPathComponent("chart-edits.json") }

    /// Only what actually moved. A block left alone writes nothing, so the
    /// sidecar stays a short list of this session's decisions rather than a
    /// second copy of the chart.
    func edits() -> PhraseEdits {
        let original = doc.phrases[name]!.units
        var times: [String: Double] = [:]
        var sylls: [String: [[SyllCut]]] = [:]
        var durs: [String: Double] = [:]
        var gaps: [String: Double] = [:]

        for (i, u) in units.enumerated() where i < original.count {
            let o = original[i]
            if abs(u.src0 - o.src0) > 1e-4, let pin = u.pin {
                switch u.cut {
                case nil:
                    times["\(pin)"] = round(u.src0 * 1000) / 1000
                case .syllable:
                    // one word's cuts are one list, so rebuild the whole
                    // list from every unit sharing this pin.
                    let family = units.filter { $0.pin == pin }
                        .sorted { $0.src0 < $1.src0 }
                    sylls["\(pin)"] = [family.enumerated().map { (k, f) -> [SyllCut] in
                        k == 0 ? [.none, .label(f.t)]
                               : [.time(round(f.src0 * 1000) / 1000), .label(f.t)]
                    }].flatMap { $0 }
                case .auto:
                    break
                }
            }
            if abs(u.dur - o.dur) > 1e-4 { durs["\(i)"] = round(u.dur * 100) / 100 }
            // a block's grid position is the rest in front of it
            let prevEnd = i > 0 ? units[i - 1].beat + units[i - 1].dur : 0
            let origPrevEnd = i > 0 ? original[i - 1].beat + original[i - 1].dur : 0
            let gap = u.beat - prevEnd, origGap = o.beat - origPrevEnd
            if i > 0, abs(gap - origGap) > 1e-4 {
                gaps["\(i - 1)"] = round(gap * 100) / 100
            }
        }
        return PhraseEdits(times: times.isEmpty ? nil : times,
                           sylls: sylls.isEmpty ? nil : sylls,
                           durs: durs.isEmpty ? nil : durs,
                           gaps: gaps.isEmpty ? nil : gaps)
    }

    /// Merge into whatever is already on disk — another phrase's edits from
    /// an earlier session must survive saving this one.
    func save() throws {
        var all: [String: PhraseEdits] = [:]
        if let data = try? Data(contentsOf: editsURL) {
            all = (try? JSONDecoder().decode([String: PhraseEdits].self, from: data)) ?? [:]
        }
        all[name] = edits()
        let enc = JSONEncoder()
        enc.outputFormatting = [.prettyPrinted, .sortedKeys]
        try enc.encode(all).write(to: editsURL)
        dirty = false
    }
}
