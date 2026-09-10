// SyllaWizard — native IMAB vocal queue + syllable-boundary editor.
//
//   swift run --package-path sylla-wizard SyllaWizard [take-id|1...N]
//
// Assets: ~/.cache/ac/imab/takes/<take-id>/{audio.mp3,spec.png}, made lazily
//         from pop/imab/out/imab-sets-successive.mp3
// Edits:  pop/imab/processed-boundaries-<take-id>.json and set-fixes/<take-id>.json
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
}

let leadTake = "7311159624588070175"

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
    let fixURL = repo.appendingPathComponent("pop/imab/set-fixes/\(take.id).json")
    if let words = jsonObject(fixURL)?["words"] as? [[String: Any]] {
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
    let url = repo.appendingPathComponent("pop/imab/processed-boundaries-\(take.id).json")
    if let values = jsonObject(url)?["sylls"] as? [[String: Any]] {
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
    let source = repo.appendingPathComponent("pop/imab/out/imab-sets-successive.mp3")
    guard FileManager.default.fileExists(atPath: source.path), let ffmpeg = ffmpegURL() else {
        return
    }
    let directory = work.appendingPathComponent("takes/\(take.id)")
    let audio = directory.appendingPathComponent("audio.mp3")
    let spec = directory.appendingPathComponent("spec.png")
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
                         "showspectrumpic=s=\(width)x520:legend=disabled:color=fiery",
                         "-frames:v", "1", spec.path])
    }
}

struct Rect: Codable {
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
    var creating = false
    var dragged = false
    var history: [[Rect?]] = []
    var loopSel = false
    let GRAB: CGFloat = 7            // edge grab tolerance in px
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
        let point = convert(event.locationInWindow, from: nil)
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
        for index in order {
            guard let value = rects[index] else { continue }
            let box = frameOf(value)
            guard box.insetBy(dx: -GRAB, dy: -GRAB).contains(point) else { continue }
            let nearL = abs(point.x - box.minX) <= GRAB, nearR = abs(point.x - box.maxX) <= GRAB
            let nearT = abs(point.y - box.minY) <= GRAB, nearB = abs(point.y - box.maxY) <= GRAB
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
        NSColor(calibratedRed: 0.055, green: 0.05, blue: 0.08, alpha: 1).setFill()
        dirty.fill()
        spec.draw(in: NSRect(x: 0, y: SPEC_TOP, width: spec.size.width, height: spec.size.height))

        let duration = spec.size.width / PXS
        for tick in 0...Int(duration * 10) {
            let seconds = CGFloat(tick) / 10
            let x = seconds * PXS
            let major = tick % 10 == 0
            (major ? NSColor.white : NSColor.white.withAlphaComponent(0.3)).setFill()
            NSRect(x: x, y: major ? 4 : 22, width: major ? 2 : 1,
                   height: major ? 38 : 20).fill()
            if major {
                NSAttributedString(string: "\(tick / 10)", attributes: [
                    .font: NSFont.boldSystemFont(ofSize: 20),
                    .foregroundColor: NSColor.white,
                ]).draw(at: NSPoint(x: x + 6, y: 2))
            }
        }

        for (index, value) in rects.enumerated() {
            guard let value else { continue }
            let box = frameOf(value)
            let hot = index == cur
            let color = hot
                ? NSColor(calibratedRed: 1, green: 0.36, blue: 0.62, alpha: 1)
                : NSColor(calibratedRed: 0.48, green: 0.78, blue: 1, alpha: 0.9)
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

        if let player {
            NSColor(calibratedRed: 1, green: 0.84, blue: 0.33, alpha: 0.95).setFill()
            NSRect(x: CGFloat(player.currentTime) * PXS - 1, y: 0,
                   width: 2, height: bounds.height).fill()
            if player.isPlaying, let clip = enclosingScrollView?.contentView {
                let x = CGFloat(player.currentTime) * PXS
                if x < clip.bounds.minX || x > clip.bounds.maxX - 120 {
                    clip.scroll(to: NSPoint(x: max(0, x - 200), y: 0))
                }
            }
        }
    }

    override func mouseDown(with event: NSEvent) {
        let point = convert(event.locationInWindow, from: nil)
        dragStart = point
        dragged = false
        creating = false
        drag = nil
        if let (index, grip) = grip(at: point), let value = rects[index] {
            cur = index                                  // click selects
            drag = (index, grip, value, point)
            pushHistory()
        } else {
            creating = true
        }
        onChange()
    }

    override func mouseDragged(with event: NSEvent) {
        guard let start = dragStart else { return }
        let end = convert(event.locationInWindow, from: nil)
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
            rects[drag.i] = value
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
        defer { dragStart = nil; drag = nil; creating = false }
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
        case 123:                                                    // ←
            if event.modifierFlags.contains(.option), var value = rects[cur] {
                pushHistory()
                value.fromMs = max(0, value.fromMs - step)
                value.toMs -= step
                rects[cur] = value
                onEdit()
            } else {
                cur = max(0, cur - 1)
            }
            onChange()
        case 124:                                                    // →
            if event.modifierFlags.contains(.option), var value = rects[cur] {
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

    var selectedTake: Take { takes[selectedIndex] }
    var outURL: URL {
        repo.appendingPathComponent("pop/imab/processed-boundaries-\(selectedTake.id).json")
    }
    var setFixURL: URL {
        repo.appendingPathComponent("pop/imab/set-fixes/\(selectedTake.id).json")
    }
    /// Hand-drawn bands from the single-take editor and syllawizard.mjs. The
    /// queue build never read this file, so two takes' worth of drawn bands
    /// (0.12...0.9 on 7311159624588070175) sat unread while the queue wrote
    /// 0...1 over the same syllables.
    var drawnURL: URL {
        repo.appendingPathComponent("pop/imab/boundaries-drawn-\(selectedTake.id).json")
    }

    func applicationDidFinishLaunching(_ notification: Notification) {
        if CommandLine.arguments.count > 1 {
            let selection = CommandLine.arguments[1]
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
        sidebar.widthAnchor.constraint(equalToConstant: 410).isActive = true

        window = NSWindow(contentRect: NSRect(x: 70, y: 70, width: 1760, height: 820),
                          styleMask: [.titled, .closable, .resizable, .miniaturizable],
                          backing: .buffered, defer: false)
        window.title = "SyllaWizard"
        window.contentView = root
        window.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)

        table.selectRowIndexes(IndexSet(integer: selectedIndex), byExtendingSelection: false)
        loadSelectedTake()
    }

    func makeSidebar() -> NSView {
        let heading = NSTextField(labelWithString: "IMAB TAKES  \(takes.count)")
        heading.font = .boldSystemFont(ofSize: 28)
        heading.textColor = NSColor(calibratedRed: 1, green: 0.36, blue: 0.62, alpha: 1)

        table = NSTableView()
        table.headerView = nil
        table.rowHeight = 74
        table.intercellSpacing = NSSize(width: 0, height: 6)
        table.backgroundColor = NSColor(calibratedWhite: 0.07, alpha: 1)
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
        stack.addArrangedSubview(heading)
        stack.addArrangedSubview(scroll)
        stack.addArrangedSubview(playAll)
        scroll.widthAnchor.constraint(equalToConstant: 374).isActive = true
        return stack
    }

    func numberOfRows(in tableView: NSTableView) -> Int { takes.count }

    func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
        let take = takes[row]
        let progress = savedProgress(take)
        let done = progress.0 == progress.1 && progress.1 > 0
        let hasWords = FileManager.default.fileExists(atPath:
            repo.appendingPathComponent("pop/imab/set-fixes/\(take.id).json").path)
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
        guard takes.indices.contains(row), row != selectedIndex else { return }
        save()
        selectedIndex = row
        playlistMode = false
        loadSelectedTake()
    }

    func clearEditor() {
        view?.player?.stop()
        for item in editor.arrangedSubviews {
            editor.removeArrangedSubview(item)
            item.removeFromSuperview()
        }
        chips.removeAll()
    }

    func loadSelectedTake() {
        clearEditor()
        let take = selectedTake
        activeWords = phraseWords(take)
        SYLS = syllables(for: activeWords)
        draftDirty = false
        prepareAssets(for: take, index: selectedIndex)
        let assets = work.appendingPathComponent("takes/\(take.id)")
        let audioURL = assets.appendingPathComponent("audio.mp3")
        let specURL = assets.appendingPathComponent("spec.png")
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
        for button in [previous, play, next, export] {
            button.bezelStyle = .rounded
            controls.addArrangedSubview(button)
        }
        status = NSTextField(labelWithString: "")
        controls.addArrangedSubview(status)
        editor.addArrangedSubview(controls)

        let source = FileManager.default.fileExists(atPath: setFixURL.path)
            ? "CONFIRMED PHRASING" : "DETECTED PHRASING — CHECK EACH WORD"
        let sourceLabel = NSTextField(labelWithString: source)
        sourceLabel.font = .boldSystemFont(ofSize: 12)
        sourceLabel.textColor = FileManager.default.fileExists(atPath: setFixURL.path)
            ? .systemGreen : .systemYellow
        let phrase = NSTextField(wrappingLabelWithString: detectedPhrase(take))
        phrase.font = .systemFont(ofSize: 24, weight: .medium)
        phrase.textColor = .labelColor
        phrase.maximumNumberOfLines = 2
        phrase.preferredMaxLayoutWidth = 1270
        let phraseBox = NSStackView(views: [sourceLabel, phrase])
        phraseBox.orientation = .vertical
        phraseBox.spacing = 5
        phraseBox.edgeInsets = NSEdgeInsets(top: 10, left: 16, bottom: 12, right: 16)
        editor.addArrangedSubview(phraseBox)

        let chipBar = NSStackView()
        chipBar.orientation = .horizontal
        chipBar.spacing = 5
        chipBar.edgeInsets = NSEdgeInsets(top: 7, left: 12, bottom: 7, right: 12)
        for (index, syllable) in SYLS.enumerated() {
            let button = NSButton(title: syllable.0, target: self, action: #selector(pick(_:)))
            button.tag = index
            button.bezelStyle = .rounded
            button.setButtonType(.momentaryPushIn)
            chips.append(button)
            chipBar.addArrangedSubview(button)
        }
        editor.addArrangedSubview(chipBar)

        let spectro = SpectroView(spec: image)
        spectro.player = player
        player.delegate = self
        view = spectro
        seed()
        let durationMs = Int(player.duration * 1000)
        if let overflow = spectro.rects.firstIndex(where: { ($0?.toMs ?? 0) > durationMs }) {
            spectro.cur = max(0, overflow - 2)
        }
        spectro.onEdit = { [weak self] in
            self?.draftDirty = true
            self?.refresh(message: "autosaved")
        }
        spectro.onChange = { [weak self] in self?.refresh() }

        let scroll = NSScrollView()
        scroll.documentView = spectro
        scroll.hasHorizontalScroller = true
        scroll.hasVerticalScroller = false
        scroll.heightAnchor.constraint(equalToConstant: spectro.frame.height + 16).isActive = true
        editor.addArrangedSubview(scroll)
        scroll.widthAnchor.constraint(equalTo: editor.widthAnchor).isActive = true

        window.title = "SyllaWizard · \(selectedIndex + 1)/\(takes.count) · \(take.id)"
        window.makeFirstResponder(spectro)
        refresh()
        DispatchQueue.main.async { [weak self] in self?.revealCurrent() }
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
    private func drawnBands() -> [String: (Double, Double)] {
        guard let syllables = jsonObject(drawnURL)?["sylls"] as? [[String: Any]] else { return [:] }
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
    private func recoverBands() {
        guard let view else { return }
        let drawn = drawnBands()
        guard !drawn.isEmpty else { return }
        var recovered = 0
        for index in view.rects.indices {
            guard var value = view.rects[index] else { continue }
            guard value.fHi - value.fLo > 0.999 else { continue }
            guard let (lo, hi) = drawn["\(SYLS[index].0)#\(SYLS[index].1)"] else { continue }
            value.fLo = lo
            value.fHi = hi
            view.rects[index] = value
            recovered += 1
        }
        if recovered > 0 { refresh(message: "recovered \(recovered) drawn bands") }
    }

    func seed() {
        guard let view else { return }
        defer { recoverBands() }
        if let syllables = jsonObject(outURL)?["sylls"] as? [[String: Any]],
           !syllables.isEmpty {
            for syllable in syllables {
                guard let label = syllable["label"] as? String,
                      let wordIndex = syllable["wi"] as? Int,
                      let from = syllable["fromMs"] as? Int,
                      let to = syllable["toMs"] as? Int,
                      let index = SYLS.firstIndex(where: { $0.0 == label && $0.1 == wordIndex }) else { continue }
                let (lo, hi) = band(syllable)
                view.rects[index] = Rect(fromMs: from, toMs: to, fLo: lo, fHi: hi)
            }
            return
        }
        guard let words = jsonObject(setFixURL)?["words"] as? [[String: Any]] else { return }
        for wordIndex in activeWords.indices {
            guard words.indices.contains(wordIndex),
                  let from = words[wordIndex]["fromMs"] as? Int,
                  let to = words[wordIndex]["toMs"] as? Int else { continue }
            let (lo, hi) = band(words[wordIndex])
            let members = SYLS.indices.filter { SYLS[$0].1 == wordIndex }
            for (part, index) in members.enumerated() {
                let a = from + (to - from) * part / members.count
                let b = from + (to - from) * (part + 1) / members.count
                view.rects[index] = Rect(fromMs: a, toMs: b, fLo: lo, fHi: hi)
            }
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
              let value = view.rects[view.cur],
              let scroll = view.enclosingScrollView else { return }
        let clip = scroll.contentView
        let x = CGFloat(value.fromMs) / 1000 * PXS
        let maximum = max(0, view.bounds.width - clip.bounds.width)
        let centered = min(maximum, max(0, x - clip.bounds.width / 2))
        clip.scroll(to: NSPoint(x: centered, y: 0))
        scroll.reflectScrolledClipView(clip)
    }

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
        playlistMode = true
        select(0, preservePlaylist: true)
        view?.player?.play()
    }

    func select(_ index: Int, preservePlaylist: Bool = false) {
        guard takes.indices.contains(index) else { return }
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
        for (index, button) in chips.enumerated() {
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
        guard let view else { return }
        var syllables: [[String: Any]] = []
        for (index, value) in view.rects.enumerated() {
            guard let value else { continue }
            syllables.append([
                "label": SYLS[index].0, "wi": SYLS[index].1,
                "fromMs": value.fromMs, "toMs": value.toMs,
                "fLo": value.fLo, "fHi": value.fHi,
            ])
        }
        if draftDirty || promote {
            let document: [String: Any] = [
                "take": selectedTake.id,
                "drawn": ISO8601DateFormatter().string(from: Date()),
                "tool": "SyllaWizard",
                "sylls": syllables,
            ]
            if let data = try? JSONSerialization.data(withJSONObject: document,
                                                       options: [.prettyPrinted, .sortedKeys]) {
                try? data.write(to: outURL, options: .atomic)
                draftDirty = false
            }
        }
        if promote && view.rects.compactMap({ $0 }).count == SYLS.count {
            var words: [[String: Any]] = []
            for wordIndex in activeWords.indices {
                let members = view.rects.enumerated().compactMap { index, value -> Rect? in
                    guard SYLS[index].1 == wordIndex else { return nil }
                    return value
                }
                guard members.count == SYLS.filter({ $0.1 == wordIndex }).count,
                      let from = members.map(\.fromMs).min(),
                      let to = members.map(\.toMs).max() else { continue }
                words.append(["text": normalized(activeWords[wordIndex]),
                              "fromMs": from, "toMs": to])
            }
            var fix = jsonObject(setFixURL) ?? [:]
            fix["take"] = selectedTake.id
            fix["source"] = "SyllaWizard processed-pass timebase"
            fix["words"] = words
            try? FileManager.default.createDirectory(at: setFixURL.deletingLastPathComponent(),
                                                     withIntermediateDirectories: true)
            if let data = try? JSONSerialization.data(withJSONObject: fix,
                                                       options: [.prettyPrinted, .sortedKeys]) {
                try? data.write(to: setFixURL, options: .atomic)
            }
        }
        if table != nil, takes.indices.contains(selectedIndex) {
            table.reloadData(forRowIndexes: IndexSet(integer: selectedIndex),
                             columnIndexes: IndexSet(integer: 0))
        }
    }

    func applicationWillTerminate(_ notification: Notification) { save() }
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }
}

let app = NSApplication.shared
let delegate = App()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
