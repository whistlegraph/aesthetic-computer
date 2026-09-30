// SyllaWizard — native IMAB vocal queue + syllable-boundary editor.
//
//   swift run --package-path sylla-wizard SyllaWizard [take-id|1...N]
//
// Assets: ~/.cache/ac/imab/takes/<take-id>/{audio.mp3,spec.png}, made lazily
//         from pop/imab/out/imab-sets-successive.mp3
// Edits:  pop/imab/processed-boundaries-<take-id>.json and set-fixes/<take-id>.json
//
//   swift run --package-path sylla-wizard SyllaWizard --spec pop/<lane>/src/sylla/spec.json [n]
//
// SPEC mode opens any lane's vocal: the spec lists takes, each with its own clip
// (audio), seed words (fix: {"words":[{text,fromMs,toMs}]}, ms into the clip), and
// where the drawn syllables go (out). Nothing IMAB is read. The lane collects the
// saved words back into its own timebase (sailor-song: bin/sylla-collect.py).
//
// A syllable is a box in two dimensions: when it happens and which part of
// the spectrum it occupies. The queue build had collapsed the second one —
// it kept fLo/fHi in the schema and wrote them to every export, but hardcoded
// them to 0...1 with no way to touch them, which also silently discarded the
// 0.15/0.9 seeds wizard-prep.py writes. The 2D grips, the undo history and
// the loop-selection below come from the single-take editor that grew in
// parallel (rescue/neo-syllawizard-diverged-20260910); the take queue,
// autosave and set-fixes export are the queue build's. Downstream
// (vocalset.mjs, lyrictrack.mjs, setscroll.py) still reads only fromMs/toMs,
// so the band is an authoring aid today — it is drawn, edited, and preserved
// rather than invented, which is the condition for it to become more later.

import AppKit
import AVFoundation

let PXS: CGFloat = 260
let PAPER = NSColor(calibratedRed: 0.97, green: 0.955, blue: 0.93, alpha: 1)
let INK = NSColor(calibratedRed: 0.17, green: 0.12, blue: 0.2, alpha: 1)
let LINK_MS = 15                 // edges this close are one separator: moving it moves both words
let SPEC_TOP: CGFloat = 44
var SYLS: [(String, Int)] = [
    ("i'm", 0), ("a", 1), ("but", 2), ("ter", 2), ("fly", 2),
    ("flap", 3), ("ping", 3), ("for", 4), ("you", 5), ("guys", 6),
    ("just", 7), ("a", 8), ("cos", 9), ("tume", 9), ("i", 10),
    ("put", 11), ("on", 12), ("in", 13), ("my", 14), ("room", 15),
]

func findRepo() -> URL {
    let fm = FileManager.default
    var url = URL(fileURLWithPath: fm.currentDirectoryPath)
    for _ in 0..<8 {
        if fm.fileExists(atPath: url.appendingPathComponent("sylla-wizard/Package.swift").path) {
            return url
        }
        url.deleteLastPathComponent()
    }
    return fm.homeDirectoryForCurrentUser.appendingPathComponent("aesthetic-computer")
}

let repo = findRepo()
let home = FileManager.default.homeDirectoryForCurrentUser
let work = home.appendingPathComponent(".cache/ac/imab")

struct Take {
    let id: String
    let date: String
    let title: String
    let role: String?
    var audio: URL? = nil          // SPEC mode: this take's own clip, and its sidecars
    var fix: URL? = nil
    var out: URL? = nil
    var drawn: URL? = nil
}

func argument(after flag: String) -> String? {
    let args = CommandLine.arguments
    guard let i = args.firstIndex(of: flag), i + 1 < args.count else { return nil }
    return args[i + 1]
}
let specURL: URL? = argument(after: "--spec").map { path in
    path.hasPrefix("/") ? URL(fileURLWithPath: path) : URL(fileURLWithPath: FileManager.default.currentDirectoryPath).appendingPathComponent(path)
}
let spec: [String: Any]? = specURL.flatMap { jsonObject($0) }
func specPath(_ value: Any?) -> URL? {
    guard let path = value as? String else { return nil }
    return path.hasPrefix("/") ? URL(fileURLWithPath: path) : repo.appendingPathComponent(path)
}
func fixURL(_ take: Take) -> URL { take.fix ?? repo.appendingPathComponent("pop/imab/set-fixes/\(take.id).json") }
func outURLFor(_ take: Take) -> URL { take.out ?? repo.appendingPathComponent("pop/imab/processed-boundaries-\(take.id).json") }
func drawnURLFor(_ take: Take) -> URL { take.drawn ?? repo.appendingPathComponent("pop/imab/boundaries-drawn-\(take.id).json") }
func assetsDir(_ take: Take) -> URL {
    let name = (spec?["name"] as? String) ?? "imab"
    return home.appendingPathComponent(".cache/ac/\(name == "imab" ? "imab" : "sylla/" + name)/takes/\(take.id)")
}

let leadTake = "7311159624588070175"

/// Track mode: the spec names the whole vocal (`"track": "<audio>"`) and every
/// take is one lyric line cut from it (its seed carries `offsetSec`). The
/// editor then shows ONE spectrogram of the entire track that scrolls end to
/// end, every line's boxes laid at their real place; the sidebar becomes a
/// jump list. Files stay per line (sylls-NN.json / words-NN.json in clip ms),
/// so sylla-collect.py and the line pipeline read them unchanged.
/// `--lines` opens the old one-line-per-page view of the same spec.
let trackAudio: URL? = specPath(spec?["track"])
let trackMode = trackAudio != nil && !CommandLine.arguments.contains("--lines")
let trackTake = Take(id: "track", date: "", title: (spec?["title"] as? String) ?? "track", role: nil, audio: trackAudio)
struct Line {
    let take: Take
    let offsetMs: Int          // clip start in the track
    let words: Range<Int>      // global word indices
    let syls: Range<Int>       // global syllable indices
}

func jsonObject(_ url: URL) -> [String: Any]? {
    guard let data = try? Data(contentsOf: url) else { return nil }
    return try? JSONSerialization.jsonObject(with: data) as? [String: Any]
}

func takeDate(_ id: String) -> String {
    guard let value = UInt64(id) else { return "" }
    let date = Date(timeIntervalSince1970: TimeInterval(value >> 32))
    let formatter = DateFormatter()
    formatter.locale = Locale(identifier: "en_US_POSIX")
    formatter.timeZone = TimeZone(secondsFromGMT: 0)
    formatter.dateFormat = "yyyy-MM-dd"
    return formatter.string(from: date)
}

func loadTakes() -> [Take] {
    if let spec, let list = spec["takes"] as? [[String: Any]] {
        return list.map { t in
            Take(id: t["id"] as? String ?? "?", date: t["date"] as? String ?? "", title: t["title"] as? String ?? "",
                 role: t["role"] as? String, audio: specPath(t["audio"]), fix: specPath(t["fix"]),
                 out: specPath(t["out"]), drawn: specPath(t["drawn"]))
        }
    }
    let setsURL = repo.appendingPathComponent("pop/imab/vocal-sets.json")
    guard let sets = jsonObject(setsURL)?["sets"] as? [[String: Any]] else {
        fatalError("Missing or invalid \(setsURL.path)")
    }
    let ids = [leadTake] + sets.compactMap { $0["take"] as? String }
    let indexURL = repo.appendingPathComponent("toolchain/whistlegraph/downloads/INDEX.json")
    let clips = jsonObject(indexURL)?["clips"] as? [[String: Any]] ?? []
    let titles = Dictionary(uniqueKeysWithValues: clips.compactMap { clip -> (String, String)? in
        guard let id = clip["id"] as? String, let title = clip["title"] as? String else {
            return nil
        }
        return (id, title)
    })
    return ids.map { id in
        Take(id: id, date: takeDate(id), title: titles[id] ?? "IMAB vocal",
             role: id == leadTake ? "lead" : nil)
    }
}

func detectedPhrase(_ take: Take) -> String {
    phraseWords(take).joined(separator: " ")
}

func phraseWords(_ take: Take) -> [String] {
    if let words = jsonObject(fixURL(take))?["words"] as? [[String: Any]] {
        return words.compactMap { $0["text"] as? String }
    }
    let url = repo.appendingPathComponent(
        "toolchain/whistlegraph/downloads/whistlegraph-\(take.id).syllnote.json")
    guard let words = jsonObject(url)?["words"] as? [[String: Any]] else { return [] }
    return words.compactMap { $0["text"] as? String }
}

func normalized(_ word: String) -> String {
    word.lowercased().filter { $0.isLetter || $0 == "'" }
}

func syllables(for words: [String]) -> [(String, Int)] {
    words.enumerated().flatMap { index, raw -> [(String, Int)] in
        switch normalized(raw) {
        case "butterfly": return [("but", index), ("ter", index), ("fly", index)]
        case "flapping": return [("flap", index), ("ping", index)]
        case "costume": return [("cos", index), ("tume", index)]
        default: return [(normalized(raw), index)]
        }
    }
}

func savedProgress(_ take: Take) -> (Int, Int) {
    let total = syllables(for: phraseWords(take)).count
    if let values = jsonObject(outURLFor(take))?["sylls"] as? [[String: Any]] {
        return (values.count, total)
    }
    return (0, total)
}

func isOlder(_ output: URL, than input: URL) -> Bool {
    let fm = FileManager.default
    guard fm.fileExists(atPath: output.path),
          let outDate = try? output.resourceValues(forKeys: [.contentModificationDateKey]).contentModificationDate,
          let inDate = try? input.resourceValues(forKeys: [.contentModificationDateKey]).contentModificationDate else {
        return true
    }
    return outDate < inDate
}

@discardableResult
func run(_ executable: URL, _ arguments: [String]) -> Bool {
    let process = Process()
    process.executableURL = executable
    process.arguments = arguments
    process.standardOutput = FileHandle.nullDevice
    process.standardError = FileHandle.nullDevice
    do {
        try process.run()
        process.waitUntilExit()
        return process.terminationStatus == 0
    } catch {
        return false
    }
}

func ffmpegURL() -> URL? {
    let candidates = [
        home.appendingPathComponent(".local/bin/ffmpeg"),
        URL(fileURLWithPath: "/opt/homebrew/bin/ffmpeg"),
        URL(fileURLWithPath: "/usr/local/bin/ffmpeg"),
    ]
    return candidates.first { FileManager.default.isExecutableFile(atPath: $0.path) }
}

func prepareAssets(for take: Take, index: Int) {
    if let clip = take.audio {         // SPEC mode: the take is its own clip; only the spectrogram is made here
        guard let ffmpeg = ffmpegURL() else { return }
        let directory = assetsDir(take)
        try? FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
        // keep the clip's own extension: AVAudioPlayer trusts the name (wav bytes as .mp3 fail with 'typ?')
        let audio = directory.appendingPathComponent("audio.\(clip.pathExtension)"), spec = directory.appendingPathComponent("spec-light.png")
        if isOlder(audio, than: clip) { try? FileManager.default.removeItem(at: audio); try? FileManager.default.copyItem(at: clip, to: audio) }
        if isOlder(spec, than: audio), let seconds = AVURLAsset(url: audio).duration.seconds as Double?, seconds > 0 {
            _ = run(ffmpeg, ["-y", "-i", audio.path, "-lavfi",
                             "showspectrumpic=s=\(Int((seconds * Double(PXS)).rounded()))x520:legend=disabled:color=fiery:stop=8000,hue=s=0,negate,eq=contrast=1.6:brightness=0.12:gamma=1.3,colorchannelmixer=rr=0.97:gg=0.93:bb=0.9",
                             "-frames:v", "1", spec.path])
        }
        return
    }
    let source = repo.appendingPathComponent("pop/imab/out/imab-sets-successive.mp3")
    guard FileManager.default.fileExists(atPath: source.path), let ffmpeg = ffmpegURL() else {
        return
    }
    let directory = work.appendingPathComponent("takes/\(take.id)")
    let audio = directory.appendingPathComponent("audio.mp3")
    let spec = directory.appendingPathComponent("spec-light.png")
    let segment = 9.0 * 4.0 * 60.0 / 124.0
    try? FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
    if isOlder(audio, than: source) {
        let made = run(ffmpeg, ["-y", "-ss", String(Double(index) * segment),
                                "-t", String(segment), "-i", source.path,
                                "-vn", "-c:a", "libmp3lame", "-q:a", "2", audio.path])
        if !made { return }
    }
    if isOlder(spec, than: audio) {
        let width = Int((segment * Double(PXS)).rounded())
        _ = run(ffmpeg, ["-y", "-i", audio.path, "-lavfi",
                         "showspectrumpic=s=\(width)x520:legend=disabled:color=fiery,hue=s=0,negate,eq=contrast=1.6:brightness=0.12:gamma=1.3,colorchannelmixer=rr=0.97:gg=0.93:bb=0.9",
                         "-frames:v", "1", spec.path])
    }
}

struct Rect: Codable, Equatable {
    var fromMs: Int
    var toMs: Int
    var fLo: Double
    var fHi: Double
}

/// Direct manipulation: a box is a thing you grab, not something you can only
/// redraw from scratch. Grab an edge to move that boundary, a corner for both,
/// the middle to slide the whole syllable.
enum Grip { case body, left, right, top, bottom, tl, tr, bl, br }

final class SpectroView: NSView {
    let spec: NSImage
    var rects: [Rect?] = Array(repeating: nil, count: SYLS.count)
    var cur = 0
    var player: AVAudioPlayer?
    var dragStart: NSPoint?
    var drag: (i: Int, grip: Grip, orig: Rect, from: NSPoint)?
    var link: (j: Int, orig: Rect, followsEnd: Bool)?   // the neighbour whose edge rides this drag
    var creating = false
    var dragged = false
    var history: [[Rect?]] = []
    var loopSel = false
    let GRAB: CGFloat = 7            // edge grab tolerance in px
    /// Responsive: the whole editor is drawn at natural (spectrogram pixel)
    /// size then scaled by `zoom`, so a small window — a slab grid cell —
    /// shows the entire take height instead of clipping it. Mouse points
    /// come back through `loc(_:)` into natural coordinates, so every
    /// hit-test and drag stays in one coordinate system.
    var zoom: CGFloat = 1
    var naturalSize: NSSize { NSSize(width: spec.size.width, height: spec.size.height + SPEC_TOP + 20) }
    var onChange: () -> Void = {}
    var onEdit: () -> Void = {}
    var onTransport: () -> Void = {}

    init(spec: NSImage) {
        self.spec = spec
        super.init(frame: NSRect(x: 0, y: 0, width: spec.size.width,
                                 height: spec.size.height + SPEC_TOP + 20))
        wantsLayer = true
        Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in
            guard let self else { return }
            // Looping the selected syllable is how a boundary gets judged: the
            // same 300ms over and over until the consonant is inside it.
            if self.loopSel, let value = self.rects[self.cur], let player = self.player,
               player.isPlaying,
               player.currentTime >= TimeInterval(Double(value.toMs) / 1000) {
                player.currentTime = TimeInterval(Double(value.fromMs) / 1000)
            }
            self.needsDisplay = true
        }
    }

    required init?(coder: NSCoder) { fatalError() }

    /// Fit the natural height into `height` (never upscaled past 1:1).
    func fit(height: CGFloat) {
        let z = min(1, max(0.2, height / naturalSize.height))
        guard abs(z - zoom) > 0.001 else { return }
        zoom = z
        setFrameSize(NSSize(width: naturalSize.width * z, height: naturalSize.height * z))
        needsDisplay = true
    }

    /// The event's location in natural coordinates.
    func loc(_ event: NSEvent) -> NSPoint {
        let p = convert(event.locationInWindow, from: nil)
        return NSPoint(x: p.x / zoom, y: p.y / zoom)
    }

    override var isFlipped: Bool { true }
    override var acceptsFirstResponder: Bool { true }

    // The pointer says what a drag will do here, so the eight grab points
    // don't have to be discovered by trial.
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        for area in trackingAreas { removeTrackingArea(area) }
        addTrackingArea(NSTrackingArea(rect: bounds,
            options: [.mouseMoved, .activeInKeyWindow, .inVisibleRect], owner: self))
    }

    override func mouseMoved(with event: NSEvent) {
        let point = loc(event)
        switch grip(at: point)?.1 {
        case .left, .right:      NSCursor.resizeLeftRight.set()
        case .top, .bottom:      NSCursor.resizeUpDown.set()
        case .tl, .tr, .bl, .br: NSCursor.crosshair.set()
        case .body:              NSCursor.openHand.set()
        case nil:                NSCursor.crosshair.set()
        }
    }

    func specY(_ frequency: Double) -> CGFloat {
        SPEC_TOP + (1 - CGFloat(frequency)) * spec.size.height
    }

    func frameOf(_ value: Rect) -> NSRect {
        let x = CGFloat(value.fromMs) / 1000 * PXS
        let y = specY(value.fHi)
        return NSRect(x: x, y: y,
                      width: CGFloat(value.toMs - value.fromMs) / 1000 * PXS,
                      height: specY(value.fLo) - y)
    }

    /// Which box and which part of it is under this point. The current
    /// syllable is tested first so its handles stay reachable when boxes
    /// overlap.
    func grip(at point: NSPoint) -> (Int, Grip)? {
        var order = Array(rects.indices)
        if let k = order.firstIndex(of: cur) { order.remove(at: k); order.insert(cur, at: 0) }
        let grab = GRAB / zoom                       // the same reach on screen at any zoom
        for index in order {
            guard let value = rects[index] else { continue }
            let box = frameOf(value)
            guard box.insetBy(dx: -grab, dy: -grab).contains(point) else { continue }
            let nearL = abs(point.x - box.minX) <= grab, nearR = abs(point.x - box.maxX) <= grab
            let nearT = abs(point.y - box.minY) <= grab, nearB = abs(point.y - box.maxY) <= grab
            switch (nearL, nearR, nearT, nearB) {
            case (true, _, true, _): return (index, .tl)
            case (_, true, true, _): return (index, .tr)
            case (true, _, _, true): return (index, .bl)
            case (_, true, _, true): return (index, .br)
            case (true, _, _, _):    return (index, .left)
            case (_, true, _, _):    return (index, .right)
            case (_, _, true, _):    return (index, .top)
            case (_, _, _, true):    return (index, .bottom)
            default:                 return (index, .body)
            }
        }
        return nil
    }

    func msAt(_ x: CGFloat) -> Int { max(0, Int(x / PXS * 1000)) }

    func freqAt(_ y: CGFloat) -> Double {
        Double(max(0, min(1, 1 - (y - SPEC_TOP) / spec.size.height)))
    }

    // Bounded so an overshot drag costs nothing; a tool that punishes trying
    // things stops being used for judgment.
    func pushHistory() {
        history.append(rects)
        if history.count > 100 { history.removeFirst() }
    }

    func playRange(_ value: Rect) {
        guard let player else { return }
        player.currentTime = TimeInterval(Double(value.fromMs) / 1000)
        player.play()
        onTransport()
    }

    override func draw(_ dirty: NSRect) {
        PAPER.setFill()
        dirty.fill()
        NSGraphicsContext.current?.cgContext.scaleBy(x: zoom, y: zoom)   // natural coords from here on
        // Only the visible slice of the image is rasterized: a whole-track
        // spectrogram is tens of thousands of pixels wide and redraws 60×/s.
        let seen = enclosingScrollView?.contentView.bounds ?? bounds
        let x0 = max(0, floor(seen.minX / zoom) - 2), x1 = min(spec.size.width, ceil(seen.maxX / zoom) + 2)
        if x1 > x0 {
            spec.draw(in: NSRect(x: x0, y: SPEC_TOP, width: x1 - x0, height: spec.size.height),
                      from: NSRect(x: x0, y: 0, width: x1 - x0, height: spec.size.height),
                      operation: .sourceOver, fraction: 1)
        }

        let duration = spec.size.width / PXS
        for tick in 0...Int(duration * 10) {
            let seconds = CGFloat(tick) / 10
            let x = seconds * PXS
            let major = tick % 10 == 0
            (major ? INK : INK.withAlphaComponent(0.3)).setFill()
            NSRect(x: x, y: major ? 4 : 22, width: major ? 2 : 1,
                   height: major ? 38 : 20).fill()
            if major {
                NSAttributedString(string: "\(tick / 10)", attributes: [
                    .font: NSFont.boldSystemFont(ofSize: 20),
                    .foregroundColor: INK,
                ]).draw(at: NSPoint(x: x + 6, y: 2))
            }
        }

        for (index, value) in rects.enumerated() {
            guard let value else { continue }
            let box = frameOf(value)
            let hot = index == cur
            let color = hot
                ? NSColor(calibratedRed: 1, green: 0.36, blue: 0.62, alpha: 1)
                : NSColor(calibratedRed: 0.16, green: 0.36, blue: 0.78, alpha: 0.9)
            color.withAlphaComponent(hot ? 0.22 : 0.12).setFill()
            box.fill()
            color.setStroke()
            let path = NSBezierPath(rect: box)
            path.lineWidth = 2
            path.stroke()
            NSAttributedString(string: SYLS[index].0, attributes: [
                .font: NSFont.boldSystemFont(ofSize: 18),
                .foregroundColor: color,
            ]).draw(at: NSPoint(x: box.minX + 4, y: max(SPEC_TOP - 24, box.minY - 26)))
            if hot {
                // the grab points, made visible
                NSColor.white.setFill()
                color.setStroke()
                for handlePoint in [
                    NSPoint(x: box.minX, y: box.minY), NSPoint(x: box.midX, y: box.minY),
                    NSPoint(x: box.maxX, y: box.minY), NSPoint(x: box.minX, y: box.midY),
                    NSPoint(x: box.maxX, y: box.midY), NSPoint(x: box.minX, y: box.maxY),
                    NSPoint(x: box.midX, y: box.maxY), NSPoint(x: box.maxX, y: box.maxY),
                ] {
                    let handle = NSRect(x: handlePoint.x - 4, y: handlePoint.y - 4,
                                        width: 8, height: 8)
                    let outline = NSBezierPath(rect: handle)
                    outline.fill()
                    outline.lineWidth = 1.5
                    outline.stroke()
                }
                // width / band readout, so the drag is a measurement
                let span = value.toMs - value.fromMs
                let band = Int(round((value.fHi - value.fLo) * 100))
                let caption = NSAttributedString(
                    string: "\(value.fromMs)→\(value.toMs)ms · \(span)ms · band \(band)%",
                    attributes: [
                        .font: NSFont.monospacedDigitSystemFont(ofSize: 12, weight: .medium),
                        .foregroundColor: NSColor.white,
                    ])
                let plate = NSRect(x: box.minX, y: box.maxY + 6,
                                   width: caption.size().width + 10, height: 18)
                NSColor.black.withAlphaComponent(0.65).setFill()
                NSBezierPath(rect: plate).fill()
                caption.draw(at: NSPoint(x: box.minX + 5, y: box.maxY + 8))
            }
        }

        // shared separators: where one word ends and the next begins, one bold bar with a grip on top
        let live = rects.compactMap { $0 }
        for a in live { for b in live where abs(a.toMs - b.fromMs) <= LINK_MS && a.fromMs < b.fromMs {
            let x = CGFloat(b.fromMs) / 1000 * PXS
            INK.withAlphaComponent(0.85).setFill()
            NSRect(x: x - 1.5, y: SPEC_TOP - 6, width: 3, height: spec.size.height + 6).fill()
            let tri = NSBezierPath(); tri.move(to: NSPoint(x: x - 7, y: SPEC_TOP - 14)); tri.line(to: NSPoint(x: x + 7, y: SPEC_TOP - 14))
            tri.line(to: NSPoint(x: x, y: SPEC_TOP - 4)); tri.close(); tri.fill()
        } }

        if let player {
            NSColor(calibratedRed: 1, green: 0.84, blue: 0.33, alpha: 0.95).setFill()
            NSRect(x: CGFloat(player.currentTime) * PXS - 1, y: 0,
                   width: 2, height: naturalSize.height).fill()
            if player.isPlaying, let clip = enclosingScrollView?.contentView {
                let x = CGFloat(player.currentTime) * PXS * zoom       // clip bounds are on-screen units
                if x < clip.bounds.minX || x > clip.bounds.maxX - 120 {
                    clip.scroll(to: NSPoint(x: max(0, x - 200), y: 0))
                }
            }
        }
    }

    override func mouseDown(with event: NSEvent) {
        let point = loc(event)
        dragStart = point
        dragged = false
        creating = false
        drag = nil
        if let (index, grip) = grip(at: point), let value = rects[index] {
            cur = index                                  // click selects
            drag = (index, grip, value, point)
            link = nil
            if !event.modifierFlags.contains(.option) {  // ⌥ pulls a joined edge apart
                if [.right, .tr, .br].contains(grip),
                   let j = rects.indices.first(where: { $0 != index && rects[$0].map { abs($0.fromMs - value.toMs) <= LINK_MS } == true }) {
                    link = (j, rects[j]!, false)
                } else if [.left, .tl, .bl].contains(grip),
                          let j = rects.indices.first(where: { $0 != index && rects[$0].map { abs($0.toMs - value.fromMs) <= LINK_MS } == true }) {
                    link = (j, rects[j]!, true)
                }
            }
            pushHistory()
        } else {
            creating = true
        }
        onChange()
    }

    override func mouseDragged(with event: NSEvent) {
        guard let start = dragStart else { return }
        let end = loc(event)
        if abs(end.x - start.x) > 3 || abs(end.y - start.y) > 3 { dragged = true }
        guard dragged else { return }
        if let drag {
            let dx = end.x - drag.from.x, dy = end.y - drag.from.y
            let deltaMs = Int(dx / PXS * 1000)
            let deltaF = Double(-dy / spec.size.height)
            var value = drag.orig
            switch drag.grip {
            case .body:   value.fromMs += deltaMs; value.toMs += deltaMs
                          value.fLo += deltaF;     value.fHi += deltaF
            case .left:   value.fromMs += deltaMs
            case .right:  value.toMs += deltaMs
            case .top:    value.fHi += deltaF
            case .bottom: value.fLo += deltaF
            case .tl:     value.fromMs += deltaMs; value.fHi += deltaF
            case .tr:     value.toMs += deltaMs;   value.fHi += deltaF
            case .bl:     value.fromMs += deltaMs; value.fLo += deltaF
            case .br:     value.toMs += deltaMs;   value.fLo += deltaF
            }
            if value.fromMs > value.toMs { swap(&value.fromMs, &value.toMs) }
            if value.fLo > value.fHi { swap(&value.fLo, &value.fHi) }
            value.fromMs = max(0, value.fromMs)
            value.toMs = max(value.fromMs + 20, value.toMs)
            value.fLo = max(0, min(1, value.fLo))
            value.fHi = max(0, min(1, value.fHi))
            // a loose edge snaps onto a neighbour's edge (8 px), which joins them
            if link == nil {
                let snap = Int(8 / PXS * 1000)
                let others = rects.indices.filter { $0 != drag.i }.compactMap { rects[$0] }.flatMap { [$0.fromMs, $0.toMs] }
                if [.right, .tr, .br].contains(drag.grip), let e = others.min(by: { abs($0 - value.toMs) < abs($1 - value.toMs) }), abs(e - value.toMs) <= snap { value.toMs = max(value.fromMs + 20, e) }
                if [.left, .tl, .bl].contains(drag.grip), let e = others.min(by: { abs($0 - value.fromMs) < abs($1 - value.fromMs) }), abs(e - value.fromMs) <= snap { value.fromMs = min(value.toMs - 20, e) }
            }
            rects[drag.i] = value
            if let link {
                var n = link.orig
                if link.followsEnd { n.toMs = max(n.fromMs + 20, value.fromMs) } else { n.fromMs = min(n.toMs - 20, value.toMs) }
                rects[link.j] = n
            }
        } else if creating {
            let x0 = min(start.x, end.x), x1 = max(start.x, end.x)
            rects[cur] = Rect(fromMs: msAt(x0), toMs: msAt(x1),
                              fLo: freqAt(max(start.y, end.y)),
                              fHi: freqAt(min(start.y, end.y)))
        }
        onChange()
        onEdit()
    }

    override func mouseUp(with event: NSEvent) {
        let wasCreating = creating
        defer { dragStart = nil; drag = nil; link = nil; creating = false }
        guard let start = dragStart else { return }
        if !dragged {
            if drag != nil {
                history.removeLast()             // a plain click is not an edit
            } else {
                player?.currentTime = TimeInterval(start.x / PXS)   // seek in empty space
                player?.play()
                onTransport()
            }
        } else if wasCreating {
            if let next = (cur + 1..<SYLS.count).first(where: { rects[$0] == nil }) {
                cur = next                        // drawing a fresh one advances
            } else if cur < SYLS.count - 1 {
                cur += 1
            }
        }
        onChange()
    }

    /// Move this syllable and every one after it by `ms`: a drift after a
    /// cut runs to the end of the take (in track mode, of the whole song).
    func shiftFromHere(_ ms: Int) {
        guard rects.indices.contains(cur) else { return }
        pushHistory()
        for i in cur..<rects.count {
            guard var value = rects[i] else { continue }
            value.fromMs = max(0, value.fromMs + ms)
            value.toMs = max(value.fromMs + 1, value.toMs + ms)
            rects[i] = value
        }
        onEdit()
        onChange()
    }

    override func keyDown(with event: NSEvent) {
        // ⌘Z undoes the last edit. The queue build autosaves every
        // adjustment, which without this leaves no way back from a slip.
        if event.modifierFlags.contains(.command),
           event.charactersIgnoringModifiers?.lowercased() == "z" {
            if let previous = history.popLast() {
                rects = previous
                onChange()
                onEdit()
            }
            return
        }
        let shift = event.modifierFlags.contains(.shift)
        let step = shift ? 50 : 10                                   // ms nudge
        switch event.keyCode {
        case 49:                                                     // space
            if let player {
                if player.isPlaying { player.pause() } else { player.play() }
            }
            onTransport()
        case 36:                                                     // return — hear this syllable
            if let value = rects[cur] { playRange(value) }
        case 37:                                                     // L — loop the selection
            loopSel.toggle()
            if loopSel, let value = rects[cur] { playRange(value) }
            onChange()
        case 51, 117:                                                // delete
            pushHistory()
            rects[cur] = nil
            onChange()
            onEdit()
        case 123:                                                    // ←   (⌥⇧: this and every later word)
            if event.modifierFlags.contains([.option, .shift]) {
                shiftFromHere(-step)
            } else if event.modifierFlags.contains(.option), var value = rects[cur] {
                pushHistory()
                value.fromMs = max(0, value.fromMs - step)
                value.toMs -= step
                rects[cur] = value
                onEdit()
            } else {
                cur = max(0, cur - 1)
            }
            onChange()
        case 124:                                                    // →   (⌥⇧: this and every later word)
            if event.modifierFlags.contains([.option, .shift]) {
                shiftFromHere(step)
            } else if event.modifierFlags.contains(.option), var value = rects[cur] {
                pushHistory()
                value.fromMs += step
                value.toMs += step
                rects[cur] = value
                onEdit()
            } else {
                cur = min(SYLS.count - 1, cur + 1)
            }
            onChange()
        case 48:                                                     // tab
            cur = shift ? max(0, cur - 1) : min(SYLS.count - 1, cur + 1)
            onChange()
        default:
            super.keyDown(with: event)
        }
    }

    override func performKeyEquivalent(with event: NSEvent) -> Bool {
        if event.modifierFlags.contains(.command),
           event.charactersIgnoringModifiers?.lowercased() == "z" {
            keyDown(with: event)
            return true
        }
        return false
    }
}

/// The editor's scroll view: every layout pass refits the spectrogram to the
/// height the window leaves it. (A frame-changed observer on the clip view
/// never fired for the initial layout, so the zoom stayed at 1:1.)
final class FitScrollView: NSScrollView {
    override func layout() {
        // Keep the same moment of the track under the middle of the view
        // across a resize (slab tiles the window right after launch).
        let clip = contentView
        let view = documentView as? SpectroView
        let centerSec = view.map { clip.bounds.midX / (PXS * $0.zoom) }
        super.layout()
        guard let view, let centerSec else { return }
        view.fit(height: contentSize.height)
        let x = centerSec * PXS * view.zoom - clip.bounds.width / 2
        let maximum = max(0, view.bounds.width - clip.bounds.width)
        clip.scroll(to: NSPoint(x: min(maximum, max(0, x)), y: 0))
        reflectScrolledClipView(clip)
    }
}

final class App: NSObject, NSApplicationDelegate, NSTableViewDataSource,
                 NSTableViewDelegate, AVAudioPlayerDelegate {
    let takes = loadTakes()
    var selectedIndex = 0
    var playlistMode = false
    var window: NSWindow!
    var table: NSTableView!
    var editor: NSStackView!
    var view: SpectroView?
    var chips: [NSButton] = []
    var status: NSTextField!
    var activeWords: [String] = []
    var draftDirty = false
    var spectroFit: NSObjectProtocol?
    var lines: [Line] = []            // track mode: one per take, in order
    var savedRects: [Rect?] = []      // track mode: rects as last seeded/saved, so only touched lines write
    var touchedLines = Set<String>()  // track mode: take ids edited this session (Save boundaries promotes only these + already-promoted ones)
    var chipLine = -1                 // which line the chip bar shows
    var chipBar: NSStackView!
    var phrase: NSTextField!
    var sourceLabel: NSTextField!
    var syncingTable = false          // programmatic row selection, not a click

    func line(of syl: Int) -> Int { lines.firstIndex { $0.syls.contains(syl) } ?? 0 }

    var selectedTake: Take { takes[selectedIndex] }
    var outURL: URL { outURLFor(selectedTake) }
    var setFixURL: URL { fixURL(selectedTake) }
    /// Hand-drawn bands from the single-take editor and syllawizard.mjs. The
    /// queue build never read this file, so two takes' worth of drawn bands
    /// (0.12...0.9 on 7311159624588070175) sat unread while the queue wrote
    /// 0...1 over the same syllables.
    var drawnURL: URL { drawnURLFor(selectedTake) }

    func applicationDidFinishLaunching(_ notification: Notification) {
        if let selection = CommandLine.arguments.dropFirst().filter({ !$0.hasPrefix("--") && $0 != specURL?.path && !(argument(after: "--spec") == $0) }).first {
            if let number = Int(selection), (1...takes.count).contains(number) {
                selectedIndex = number - 1
            } else if let index = takes.firstIndex(where: { $0.id == selection }) {
                selectedIndex = index
            }
        }

        let sidebar = makeSidebar()
        editor = NSStackView()
        editor.orientation = .vertical
        editor.spacing = 0
        editor.alignment = .leading

        let root = NSStackView()
        root.orientation = .horizontal
        root.spacing = 0
        root.distribution = .fill
        root.addArrangedSubview(sidebar)
        root.addArrangedSubview(editor)
        // The sidebar takes ~28% of the width, 160…410 px; the editor gets
        // the rest. Both run the full height so the spectrogram can fit
        // itself to whatever a small window (a slab cell) leaves it.
        sidebar.widthAnchor.constraint(lessThanOrEqualToConstant: 410).isActive = true
        sidebar.widthAnchor.constraint(greaterThanOrEqualToConstant: 160).isActive = true
        let share = sidebar.widthAnchor.constraint(equalTo: root.widthAnchor, multiplier: 0.28)
        share.priority = NSLayoutConstraint.Priority(700)
        share.isActive = true
        sidebar.heightAnchor.constraint(equalTo: root.heightAnchor).isActive = true
        editor.heightAnchor.constraint(equalTo: root.heightAnchor).isActive = true
        editor.setClippingResistancePriority(.defaultLow, for: .horizontal)

        window = NSWindow(contentRect: NSRect(x: 70, y: 70, width: 1760, height: 820),
                          styleMask: [.titled, .closable, .resizable, .miniaturizable],
                          backing: .buffered, defer: false)
        window.title = "SyllaWizard"
        window.contentMinSize = NSSize(width: 480, height: 300)
        window.contentView = root
        window.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)

        table.selectRowIndexes(IndexSet(integer: selectedIndex), byExtendingSelection: false)
        loadSelectedTake()
    }

    func makeSidebar() -> NSView {
        let heading = NSTextField(labelWithString: "\(((spec?["title"] as? String) ?? "IMAB takes").uppercased())  \(takes.count)")
        heading.font = .boldSystemFont(ofSize: 28)
        heading.textColor = NSColor(calibratedRed: 1, green: 0.36, blue: 0.62, alpha: 1)
        heading.lineBreakMode = .byTruncatingTail
        heading.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)

        table = NSTableView()
        table.headerView = nil
        table.rowHeight = 74
        table.intercellSpacing = NSSize(width: 0, height: 6)
        table.backgroundColor = NSColor(calibratedRed: 0.95, green: 0.935, blue: 0.91, alpha: 1)   // light mode, like the rest
        table.addTableColumn(NSTableColumn(identifier: NSUserInterfaceItemIdentifier("take")))
        table.dataSource = self
        table.delegate = self

        let scroll = NSScrollView()
        scroll.documentView = table
        scroll.hasVerticalScroller = true

        let playAll = NSButton(title: "▶ Play all", target: self, action: #selector(playAllTakes))
        playAll.bezelStyle = .rounded

        let stack = NSStackView()
        stack.orientation = .vertical
        stack.spacing = 12
        stack.edgeInsets = NSEdgeInsets(top: 18, left: 18, bottom: 18, right: 18)
        stack.alignment = .leading
        stack.addArrangedSubview(heading)
        stack.addArrangedSubview(scroll)
        stack.addArrangedSubview(playAll)
        scroll.widthAnchor.constraint(equalTo: stack.widthAnchor, constant: -36).isActive = true
        return stack
    }

    func numberOfRows(in tableView: NSTableView) -> Int { takes.count }

    func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
        let take = takes[row]
        let progress = savedProgress(take)
        let done = progress.0 == progress.1 && progress.1 > 0
        let hasWords = FileManager.default.fileExists(atPath: fixURL(take).path) && spec == nil
        let cell = NSTableCellView(frame: NSRect(x: 0, y: 0, width: 370, height: 74))
        let lead = take.role == "lead" ? "   LEAD" : ""
        let title = NSTextField(labelWithString: "\(row + 1)   \(take.id)\(lead)")
        title.frame = NSRect(x: 10, y: 46, width: 350, height: 22)
        title.font = .monospacedSystemFont(ofSize: 15, weight: .semibold)
        let name = NSTextField(labelWithString: take.title)
        name.frame = NSRect(x: 42, y: 25, width: 316, height: 19)
        name.lineBreakMode = .byTruncatingTail
        name.textColor = .secondaryLabelColor
        let state = done ? "✓ drawn" : (hasWords ? "✓ words" : "○ \(progress.0)/\(progress.1)")
        let detail = NSTextField(labelWithString: "\(take.date)   \(state)")
        detail.frame = NSRect(x: 42, y: 5, width: 316, height: 19)
        detail.textColor = done ? .systemGreen : .secondaryLabelColor
        cell.addSubview(title)
        cell.addSubview(name)
        cell.addSubview(detail)
        return cell
    }

    func tableViewSelectionDidChange(_ notification: Notification) {
        // Capture the clicked row before saving. Refreshing table data can clear
        // AppKit's live selection while this notification is still on the stack.
        let row = table.selectedRow
        guard !syncingTable, takes.indices.contains(row), row != selectedIndex else { return }
        if trackMode { select(row); return }
        save()
        selectedIndex = row
        playlistMode = false
        loadSelectedTake()
    }

    func clearEditor() {
        if let spectroFit { NotificationCenter.default.removeObserver(spectroFit) }
        spectroFit = nil
        view?.player?.stop()
        for item in editor.arrangedSubviews {
            editor.removeArrangedSubview(item)
            item.removeFromSuperview()
        }
        chips.removeAll()
    }

    func loadSelectedTake() {
        clearEditor()
        let take = trackMode ? trackTake : selectedTake
        if trackMode {
            // every line's words and syllables, back to back, with global indices
            activeWords = []; lines = []
            var all: [(String, Int)] = []
            for lineTake in takes {
                let words = phraseWords(lineTake)
                let offset = Int((((jsonObject(fixURL(lineTake))?["offsetSec"] as? Double) ?? 0) * 1000).rounded())
                let w0 = activeWords.count, s0 = all.count
                activeWords += words
                all += syllables(for: words).map { ($0.0, $0.1 + w0) }
                lines.append(Line(take: lineTake, offsetMs: offset, words: w0..<activeWords.count, syls: s0..<all.count))
            }
            SYLS = all
        } else {
            activeWords = phraseWords(take)
            SYLS = syllables(for: activeWords)
        }
        draftDirty = false
        prepareAssets(for: take, index: selectedIndex)
        let assets = take.audio != nil ? assetsDir(take) : work.appendingPathComponent("takes/\(take.id)")
        let audioURL = assets.appendingPathComponent(take.audio.map { "audio.\($0.pathExtension)" } ?? "audio.mp3")
        let specURL = assets.appendingPathComponent("spec-light.png")
        guard let image = NSImage(contentsOf: specURL),
              let player = try? AVAudioPlayer(contentsOf: audioURL) else {
            let missing = NSTextField(labelWithString: "Missing assets for take \(take.id)\n\(assets.path)")
            missing.font = .systemFont(ofSize: 22)
            editor.addArrangedSubview(missing)
            return
        }

        let controls = NSStackView()
        controls.orientation = .horizontal
        controls.spacing = 10
        controls.edgeInsets = NSEdgeInsets(top: 12, left: 14, bottom: 10, right: 14)
        let previous = NSButton(title: "←", target: self, action: #selector(previousTake))
        let play = NSButton(title: "▶ / Ⅱ", target: self, action: #selector(togglePlay))
        let next = NSButton(title: "→", target: self, action: #selector(nextTake))
        let export = NSButton(title: "Save boundaries", target: self, action: #selector(exportBoundaries))
        // shift the selected word and everything after it (10 ms; ⌥-click 100 ms; keys ⌥⇧← ⌥⇧→)
        let shiftBack = NSButton(title: "◀ from here", target: self, action: #selector(shiftFromHereBack))
        let shiftForward = NSButton(title: "from here ▶", target: self, action: #selector(shiftFromHereForward))
        shiftBack.toolTip = "Move this word and every later one 10 ms earlier (⌥-click: 100 ms; ⌥⇧←)"
        shiftForward.toolTip = "Move this word and every later one 10 ms later (⌥-click: 100 ms; ⌥⇧→)"
        for button in [previous, play, next, export, shiftBack, shiftForward] {
            button.bezelStyle = .rounded
            controls.addArrangedSubview(button)
        }
        status = NSTextField(labelWithString: "")
        controls.addArrangedSubview(status)
        controls.setClippingResistancePriority(.defaultLow, for: .horizontal)
        editor.addArrangedSubview(controls)

        let source = FileManager.default.fileExists(atPath: setFixURL.path)
            ? "CONFIRMED PHRASING" : "DETECTED PHRASING — CHECK EACH WORD"
        sourceLabel = NSTextField(labelWithString: source)
        sourceLabel.font = .boldSystemFont(ofSize: 12)
        sourceLabel.textColor = FileManager.default.fileExists(atPath: setFixURL.path)
            ? .systemGreen : .systemYellow
        phrase = NSTextField(wrappingLabelWithString: trackMode ? detectedPhrase(selectedTake) : detectedPhrase(take))
        phrase.font = .systemFont(ofSize: 24, weight: .medium)
        phrase.textColor = .labelColor
        phrase.maximumNumberOfLines = 2
        phrase.preferredMaxLayoutWidth = 1270
        phrase.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
        let phraseBox = NSStackView(views: [sourceLabel, phrase])
        phraseBox.orientation = .vertical
        phraseBox.spacing = 5
        phraseBox.edgeInsets = NSEdgeInsets(top: 10, left: 16, bottom: 12, right: 16)
        editor.addArrangedSubview(phraseBox)

        chipBar = NSStackView()
        chipBar.orientation = .horizontal
        chipBar.spacing = 5
        chipBar.edgeInsets = NSEdgeInsets(top: 7, left: 12, bottom: 7, right: 12)
        chipBar.setClippingResistancePriority(.defaultLow, for: .horizontal)
        chipLine = -1
        buildChips(for: trackMode ? lines[selectedIndex].syls : 0..<SYLS.count)
        editor.addArrangedSubview(chipBar)

        let spectro = SpectroView(spec: image)
        spectro.player = player
        player.delegate = self
        view = spectro
        seed()
        savedRects = spectro.rects
        let durationMs = Int(player.duration * 1000)
        if trackMode {
            spectro.cur = lines[selectedIndex].syls.lowerBound
        } else if let overflow = spectro.rects.firstIndex(where: { ($0?.toMs ?? 0) > durationMs }) {
            spectro.cur = max(0, overflow - 2)
        }
        spectro.onEdit = { [weak self] in
            self?.draftDirty = true
            self?.refresh(message: "autosaved")
        }
        spectro.onChange = { [weak self] in self?.refresh() }

        let scroll = FitScrollView()
        scroll.documentView = spectro
        scroll.hasHorizontalScroller = true
        scroll.hasVerticalScroller = false
        // Never taller than the take at 1:1; otherwise the scroll takes what
        // the window leaves and the spectrogram zooms to fit that height.
        scroll.heightAnchor.constraint(lessThanOrEqualToConstant: spectro.frame.height + 16).isActive = true
        scroll.setContentHuggingPriority(.defaultLow, for: .vertical)
        scroll.setContentCompressionResistancePriority(.defaultLow, for: .vertical)
        editor.addArrangedSubview(scroll)
        scroll.widthAnchor.constraint(equalTo: editor.widthAnchor).isActive = true

        window.title = trackMode ? "SyllaWizard · \(take.title) · whole track"
                                 : "SyllaWizard · \(selectedIndex + 1)/\(takes.count) · \(take.id)"
        window.makeFirstResponder(spectro)
        refresh()
        DispatchQueue.main.async { [weak self] in
            guard let self, let view = self.view, let scroll = view.enclosingScrollView else { return }
            scroll.layoutSubtreeIfNeeded()               // fit first, so the reveal uses the final zoom
            view.fit(height: scroll.contentSize.height)
            self.revealCurrent()
        }
    }

    // wizard-prep.py seeds fLo 0.15 / fHi 0.9 and syllawizard.mjs writes real
    // bands, so a load that hardcoded 0...1 silently discarded work every time
    // the take was reopened. Read what is there; fall back to the full band
    // only when the file has none.
    private func band(_ source: [String: Any]) -> (Double, Double) {
        let lo = source["fLo"] as? Double ?? 0
        let hi = source["fHi"] as? Double ?? 1
        return hi > lo ? (max(0, lo), min(1, hi)) : (0, 1)
    }

    /// label+wi -> band, from a `sylls` document. Keyed by label rather than
    /// index because the two tools disagree about ordering.
    private func drawnBands(_ url: URL) -> [String: (Double, Double)] {
        guard let syllables = jsonObject(url)?["sylls"] as? [[String: Any]] else { return [:] }
        var out: [String: (Double, Double)] = [:]
        for syllable in syllables {
            guard let label = syllable["label"] as? String,
                  let wordIndex = syllable["wi"] as? Int else { continue }
            let (lo, hi) = band(syllable)
            if hi - lo < 0.999 { out["\(label)#\(wordIndex)"] = (lo, hi) }
        }
        return out
    }

    /// Timings come from the newest edit; a flat band is filled in from the
    /// drawn file rather than left at full height. Only flat bands are
    /// touched, so a band edited here always wins.
    private func recoverBands(drawn url: URL, syls: Range<Int>, wordBase: Int) {
        guard let view else { return }
        let drawn = drawnBands(url)
        guard !drawn.isEmpty else { return }
        var recovered = 0
        for index in syls {
            guard var value = view.rects[index] else { continue }
            guard value.fHi - value.fLo > 0.999 else { continue }
            guard let (lo, hi) = drawn["\(SYLS[index].0)#\(SYLS[index].1 - wordBase)"] else { continue }
            value.fLo = lo
            value.fHi = hi
            view.rects[index] = value
            recovered += 1
        }
        if recovered > 0 { refresh(message: "recovered \(recovered) drawn bands") }
    }

    /// A sylls file whose timings equal its seed word for word was never
    /// touched by hand (an older whole-track autosave wrote every line).
    /// Drop it so the line keeps its "○" mark and re-seeds from the words.
    func pruneSeedCopies() {
        for l in lines {
            let out = outURLFor(l.take)
            // A promoted line ("Save boundaries") copies its timings into the
            // words file too, so it always matches: never prune those.
            guard let fix = jsonObject(fixURL(l.take)),
                  !((fix["source"] as? String) ?? "").hasPrefix("SyllaWizard"),
                  let sylls = jsonObject(out)?["sylls"] as? [[String: Any]],
                  let words = fix["words"] as? [[String: Any]],
                  sylls.count == words.count, !sylls.isEmpty else { continue }
            let same = zip(sylls, words).allSatisfy {
                ($0["fromMs"] as? Int) == ($1["fromMs"] as? Int) && ($0["toMs"] as? Int) == ($1["toMs"] as? Int)
            }
            if same { try? FileManager.default.removeItem(at: out) }
        }
    }

    func seed() {
        if trackMode {
            pruneSeedCopies()
            for l in lines {
                seedRects(out: outURLFor(l.take), fix: fixURL(l.take), drawn: drawnURLFor(l.take),
                          words: l.words, syls: l.syls, offsetMs: l.offsetMs)
            }
        } else {
            seedRects(out: outURL, fix: setFixURL, drawn: drawnURL,
                      words: 0..<activeWords.count, syls: 0..<SYLS.count, offsetMs: 0)
        }
    }

    /// Boxes for one clip's files into the global rects: saved syllables first,
    /// else the seed words split evenly into their syllables. File ms are clip
    /// ms; `offsetMs` places them on the track (0 in line mode).
    func seedRects(out: URL, fix: URL, drawn: URL, words: Range<Int>, syls: Range<Int>, offsetMs: Int) {
        guard let view else { return }
        defer { recoverBands(drawn: drawn, syls: syls, wordBase: words.lowerBound) }
        if let syllables = jsonObject(out)?["sylls"] as? [[String: Any]],
           !syllables.isEmpty {
            for syllable in syllables {
                guard let label = syllable["label"] as? String,
                      let wordIndex = syllable["wi"] as? Int,
                      let from = syllable["fromMs"] as? Int,
                      let to = syllable["toMs"] as? Int,
                      let index = syls.first(where: { SYLS[$0].0 == label && SYLS[$0].1 == words.lowerBound + wordIndex }) else { continue }
                let (lo, hi) = band(syllable)
                view.rects[index] = Rect(fromMs: from + offsetMs, toMs: to + offsetMs, fLo: lo, fHi: hi)
            }
            return
        }
        guard let seedWords = jsonObject(fix)?["words"] as? [[String: Any]] else { return }
        for wordIndex in words {
            let local = wordIndex - words.lowerBound
            guard seedWords.indices.contains(local),
                  let from0 = seedWords[local]["fromMs"] as? Int,
                  let to0 = seedWords[local]["toMs"] as? Int else { continue }
            let from = from0 + offsetMs, to = to0 + offsetMs
            let (lo, hi) = band(seedWords[local])
            let members = syls.filter { SYLS[$0].1 == wordIndex }
            for (part, index) in members.enumerated() {
                let a = from + (to - from) * part / members.count
                let b = from + (to - from) * (part + 1) / members.count
                view.rects[index] = Rect(fromMs: a, toMs: b, fLo: lo, fHi: hi)
            }
        }
    }

    /// One chip per syllable in `range` (track mode: the current line only).
    func buildChips(for range: Range<Int>) {
        for chip in chips { chipBar.removeArrangedSubview(chip); chip.removeFromSuperview() }
        chips.removeAll()
        for index in range {
            let button = NSButton(title: SYLS[index].0, target: self, action: #selector(pick(_:)))
            button.tag = index
            button.bezelStyle = .rounded
            button.setButtonType(.momentaryPushIn)
            chips.append(button)
            chipBar.addArrangedSubview(button)
        }
    }

    /// Track mode: keep the sidebar row, phrase and chips on the line that
    /// holds the current syllable, as ← → Tab walk across line boundaries.
    func syncLine() {
        guard trackMode, let view, !lines.isEmpty else { return }
        let li = line(of: view.cur)
        guard li != chipLine else { return }
        chipLine = li
        selectedIndex = li
        buildChips(for: lines[li].syls)
        phrase.stringValue = detectedPhrase(lines[li].take)
        let confirmed = FileManager.default.fileExists(atPath: fixURL(lines[li].take).path)
        sourceLabel.stringValue = confirmed ? "CONFIRMED PHRASING" : "DETECTED PHRASING — CHECK EACH WORD"
        sourceLabel.textColor = confirmed ? .systemGreen : .systemYellow
        if table != nil, table.selectedRow != li {
            syncingTable = true
            table.selectRowIndexes(IndexSet(integer: li), byExtendingSelection: false)
            table.scrollRowToVisible(li)
            syncingTable = false
        }
    }

    @objc func pick(_ sender: NSButton) {
        view?.cur = sender.tag
        if let view { window.makeFirstResponder(view) }
        revealCurrent()
        refresh()
    }

    func revealCurrent() {
        guard let view, view.rects.indices.contains(view.cur),
              let scroll = view.enclosingScrollView else { return }
        let ms = view.rects[view.cur]?.fromMs ?? (trackMode ? lines[line(of: view.cur)].offsetMs + 400 : 0)
        let clip = scroll.contentView
        let x = CGFloat(ms) / 1000 * PXS * view.zoom
        let maximum = max(0, view.bounds.width - clip.bounds.width)
        let centered = min(maximum, max(0, x - clip.bounds.width / 2))
        clip.scroll(to: NSPoint(x: centered, y: 0))
        scroll.reflectScrolledClipView(clip)
    }

    @objc func shiftFromHereBack() { view?.shiftFromHere(NSEvent.modifierFlags.contains(.option) ? -100 : -10) }
    @objc func shiftFromHereForward() { view?.shiftFromHere(NSEvent.modifierFlags.contains(.option) ? 100 : 10) }

    @objc func togglePlay() {
        guard let player = view?.player else { return }
        if player.isPlaying { player.pause() } else { player.play() }
    }

    @objc func previousTake() { select(max(0, selectedIndex - 1)) }
    @objc func nextTake() { select(min(takes.count - 1, selectedIndex + 1)) }
    @objc func exportBoundaries() {
        save(promote: true)
        refresh(message: "saved to set-fixes")
    }

    @objc func playAllTakes() {
        if trackMode, let player = view?.player { player.currentTime = 0; player.play(); return }
        playlistMode = true
        select(0, preservePlaylist: true)
        view?.player?.play()
    }

    func select(_ index: Int, preservePlaylist: Bool = false) {
        guard takes.indices.contains(index) else { return }
        if trackMode, let view {              // jump, don't reload: the track is already up
            view.cur = lines[index].syls.lowerBound
            revealCurrent()
            refresh()
            window.makeFirstResponder(view)
            return
        }
        save()
        selectedIndex = index
        if !preservePlaylist { playlistMode = false }
        table.selectRowIndexes(IndexSet(integer: index), byExtendingSelection: false)
        loadSelectedTake()
    }

    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        guard playlistMode else { return }
        if selectedIndex + 1 < takes.count {
            select(selectedIndex + 1, preservePlaylist: true)
            view?.player?.play()
        } else {
            playlistMode = false
            refresh(message: "playlist complete")
        }
    }

    func refresh(message: String? = nil) {
        guard let view, status != nil else { return }
        syncLine()
        for button in chips {
            let index = button.tag
            button.contentTintColor = index == view.cur
                ? NSColor(calibratedRed: 1, green: 0.36, blue: 0.62, alpha: 1)
                : (view.rects[index] != nil ? .systemGreen : .secondaryLabelColor)
        }
        let done = view.rects.compactMap { $0 }.count
        status.stringValue = message ?? "\(done)/\(SYLS.count) · → \(SYLS[view.cur].0) · drag band or edge"
        view.needsDisplay = true
        save()
    }

    func save(promote: Bool = false) {
        guard view != nil else { return }
        if trackMode, let view {
            // A line's files are written only when one of its boxes moved (or
            // on Save boundaries): an untouched line keeps the aligner's seed
            // and its "○" mark, exactly as in the one-line view.
            if savedRects.count != view.rects.count { savedRects = Array(repeating: nil, count: view.rects.count) }
            for l in lines {
                let touched = l.syls.contains { view.rects[$0] != savedRects[$0] }
                if touched { touchedLines.insert(l.take.id) }
                // an untouched line whose seed is still the aligner's is never promoted
                let promoted = ((jsonObject(fixURL(l.take))?["source"] as? String) ?? "").hasPrefix("SyllaWizard")
                let promoteThis = promote && (touchedLines.contains(l.take.id) || promoted)
                guard touched || promoteThis else { continue }
                saveRects(takeID: l.take.id, out: outURLFor(l.take), fix: fixURL(l.take),
                          words: l.words, syls: l.syls, offsetMs: l.offsetMs, promote: promoteThis)
                if draftDirty || promoteThis { for i in l.syls { savedRects[i] = view.rects[i] } }
            }
            if draftDirty || promote { draftDirty = false }
            if table != nil { table.reloadData() }
            return
        }
        saveRects(takeID: selectedTake.id, out: outURL, fix: setFixURL,
                  words: 0..<activeWords.count, syls: 0..<SYLS.count, offsetMs: 0, promote: promote)
        if draftDirty || promote { draftDirty = false }
        if table != nil, takes.indices.contains(selectedIndex) {
            table.reloadData(forRowIndexes: IndexSet(integer: selectedIndex),
                             columnIndexes: IndexSet(integer: 0))
        }
    }

    /// One clip's files from the global rects (track ms → clip ms, global
    /// word index → the clip's own).
    func saveRects(takeID: String, out: URL, fix: URL, words: Range<Int>, syls: Range<Int>,
                   offsetMs: Int, promote: Bool) {
        guard let view else { return }
        var syllables: [[String: Any]] = []
        for index in syls {
            guard let value = view.rects[index] else { continue }
            syllables.append([
                "label": SYLS[index].0, "wi": SYLS[index].1 - words.lowerBound,
                "fromMs": value.fromMs - offsetMs, "toMs": value.toMs - offsetMs,
                "fLo": value.fLo, "fHi": value.fHi,
            ])
        }
        if draftDirty || promote {
            let document: [String: Any] = [
                "take": takeID,
                "drawn": ISO8601DateFormatter().string(from: Date()),
                "tool": "SyllaWizard",
                "sylls": syllables,
            ]
            if let data = try? JSONSerialization.data(withJSONObject: document,
                                                       options: [.prettyPrinted, .sortedKeys]) {
                try? data.write(to: out, options: .atomic)
            }
        }
        if promote && syls.allSatisfy({ view.rects[$0] != nil }) {
            var wordDocs: [[String: Any]] = []
            for wordIndex in words {
                let members = syls.compactMap { index -> Rect? in
                    guard SYLS[index].1 == wordIndex else { return nil }
                    return view.rects[index]
                }
                guard members.count == syls.filter({ SYLS[$0].1 == wordIndex }).count,
                      let from = members.map(\.fromMs).min(),
                      let to = members.map(\.toMs).max() else { continue }
                wordDocs.append(["text": normalized(activeWords[wordIndex]),
                                 "fromMs": from - offsetMs, "toMs": to - offsetMs])
            }
            var fixDoc = jsonObject(fix) ?? [:]
            fixDoc["take"] = takeID
            fixDoc["source"] = "SyllaWizard processed-pass timebase"
            fixDoc["words"] = wordDocs
            try? FileManager.default.createDirectory(at: fix.deletingLastPathComponent(),
                                                     withIntermediateDirectories: true)
            if let data = try? JSONSerialization.data(withJSONObject: fixDoc,
                                                       options: [.prettyPrinted, .sortedKeys]) {
                try? data.write(to: fix, options: .atomic)
            }
        }
    }

    func applicationWillTerminate(_ notification: Notification) { save() }
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

let app = NSApplication.shared
let delegate = App()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.appearance = NSAppearance(named: .aqua)
app.run()
