// SyllaWizard — draw rectangles around spectrum bands per syllable.
// A native wizard (Aesthetic Inc): the take's spectrogram scrolls in a
// window, the vocal stem plays under a click-to-seek needle, and
// @jeffrey drags one rectangle per syllable — his judgment exported
// straight into pop/imab/boundaries-drawn-<take>.json, the boundary
// ground truth the aesthetivox chain proceeds from. Repeat for N takes.
//
//   swift build && .build/debug/SyllaWizard [take-id]
//
// Assets per take (rendered by pop/imab tooling into ~/.cache/ac/imab):
//   wizard-spec.png (260 px/s mel spectrogram) · the demucs vocal stem
//   bounds-<take>.json — machine guesses, pre-seeded as editable rects

import AppKit
import AVFoundation

let PXS: CGFloat = 260
let SPEC_TOP: CGFloat = 44
let take = CommandLine.arguments.count > 1 ? CommandLine.arguments[1] : "7311159624588070175"
let home = FileManager.default.homeDirectoryForCurrentUser
let work = home.appendingPathComponent(".cache/ac/imab")
let repo = home.appendingPathComponent("aesthetic-computer")
let stemURL = work.appendingPathComponent("sep/htdemucs/whistlegraph-\(take)/vocals.wav")
let specURL = work.appendingPathComponent("wizard-spec.png")
let outURL = repo.appendingPathComponent("pop/imab/boundaries-drawn-\(take).json")

let SYLS: [(String, Int)] = [
    ("i'm", 0), ("a", 1), ("but", 2), ("ter", 2), ("fly", 2), ("flap", 3), ("ping", 3),
    ("for", 4), ("you", 5), ("guys", 6), ("just", 7), ("a", 8), ("cos", 9), ("tume", 9),
    ("i", 10), ("put", 11), ("on", 12), ("in", 13), ("my", 14), ("room", 15),
]

struct Rect: Codable {
    var fromMs: Int
    var toMs: Int
    var fLo: Double
    var fHi: Double
}

final class SpectroView: NSView {
    let spec: NSImage
    var rects: [Rect?] = Array(repeating: nil, count: SYLS.count)
    var cur = 0
    var player: AVAudioPlayer?
    var dragStart: NSPoint?
    var dragged = false
    var onChange: () -> Void = {}

    init(spec: NSImage) {
        self.spec = spec
        super.init(frame: NSRect(x: 0, y: 0, width: spec.size.width,
                                 height: spec.size.height + SPEC_TOP + 20))
        wantsLayer = true
        Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in
            self?.needsDisplay = true
        }
    }
    required init?(coder: NSCoder) { fatalError() }
    override var isFlipped: Bool { true }
    override var acceptsFirstResponder: Bool { true }

    func specY(_ f: Double) -> CGFloat { SPEC_TOP + (1 - f) * spec.size.height }

    override func draw(_ dirty: NSRect) {
        NSColor(calibratedRed: 0.055, green: 0.05, blue: 0.08, alpha: 1).setFill()
        dirty.fill()
        spec.draw(in: NSRect(x: 0, y: SPEC_TOP, width: spec.size.width, height: spec.size.height))
        // second ruler
        let dur = spec.size.width / PXS
        for k in 0...Int(dur * 10) {
            let ts = CGFloat(k) / 10
            let x = ts * PXS
            let big = k % 10 == 0
            (big ? NSColor.white : NSColor.white.withAlphaComponent(0.3)).setFill()
            NSRect(x: x, y: big ? 4 : 22, width: big ? 2 : 1, height: big ? 38 : 20).fill()
            if big {
                let s = NSAttributedString(string: "\(k / 10)", attributes: [
                    .font: NSFont.boldSystemFont(ofSize: 20), .foregroundColor: NSColor.white])
                s.draw(at: NSPoint(x: x + 6, y: 2))
            }
        }
        // syllable rects
        for (i, r) in rects.enumerated() {
            guard let r = r else { continue }
            let x = CGFloat(r.fromMs) / 1000 * PXS
            let w = CGFloat(r.toMs - r.fromMs) / 1000 * PXS
            let y = specY(r.fHi)
            let h = specY(r.fLo) - y
            let hot = i == cur
            let stroke = hot ? NSColor(calibratedRed: 1, green: 0.36, blue: 0.62, alpha: 1)
                             : NSColor(calibratedRed: 0.48, green: 0.78, blue: 1, alpha: 0.9)
            stroke.withAlphaComponent(hot ? 0.22 : 0.12).setFill()
            let rr = NSRect(x: x, y: y, width: w, height: h)
            rr.fill()
            stroke.setStroke()
            let p = NSBezierPath(rect: rr); p.lineWidth = 2; p.stroke()
            let s = NSAttributedString(string: SYLS[i].0, attributes: [
                .font: NSFont.boldSystemFont(ofSize: 18), .foregroundColor: stroke])
            s.draw(at: NSPoint(x: x + 4, y: max(SPEC_TOP - 24, y - 26)))
        }
        // playhead
        if let p = player {
            NSColor(calibratedRed: 1, green: 0.84, blue: 0.33, alpha: 0.95).setFill()
            NSRect(x: CGFloat(p.currentTime) * PXS - 1, y: 0, width: 2, height: bounds.height).fill()
            if p.isPlaying, let clip = enclosingScrollView?.contentView {
                let x = CGFloat(p.currentTime) * PXS
                if x < clip.bounds.minX || x > clip.bounds.maxX - 120 {
                    clip.scroll(to: NSPoint(x: max(0, x - 200), y: 0))
                }
            }
        }
    }

    override func mouseDown(with e: NSEvent) {
        dragStart = convert(e.locationInWindow, from: nil)
        dragged = false
    }
    override func mouseDragged(with e: NSEvent) {
        guard let a = dragStart else { return }
        let b = convert(e.locationInWindow, from: nil)
        if abs(b.x - a.x) > 4 { dragged = true }
        if dragged {
            let x0 = min(a.x, b.x), x1 = max(a.x, b.x)
            let yTop = min(a.y, b.y), yBot = max(a.y, b.y)
            let fHi = Double(max(0, min(1, 1 - (yTop - SPEC_TOP) / spec.size.height)))
            let fLo = Double(max(0, min(1, 1 - (yBot - SPEC_TOP) / spec.size.height)))
            rects[cur] = Rect(fromMs: Int(x0 / PXS * 1000), toMs: Int(x1 / PXS * 1000),
                              fLo: fLo, fHi: fHi)
            onChange()
        }
    }
    override func mouseUp(with e: NSEvent) {
        defer { dragStart = nil }
        guard let a = dragStart else { return }
        if !dragged {
            player?.currentTime = TimeInterval(a.x / PXS)
            player?.play()
        } else if let next = (cur + 1..<SYLS.count).first(where: { rects[$0] == nil }) {
            cur = next
        } else if cur < SYLS.count - 1 {
            cur += 1
        }
        onChange()
    }
    override func keyDown(with e: NSEvent) {
        switch e.keyCode {
        case 49: if let p = player { if p.isPlaying { p.pause() } else { p.play() } }   // space
        case 51: rects[cur] = nil; onChange()                               // delete
        case 123: cur = max(0, cur - 1); onChange()                         // ←
        case 124: cur = min(SYLS.count - 1, cur + 1); onChange()            // →
        default: super.keyDown(with: e)
        }
    }
}

final class App: NSObject, NSApplicationDelegate {
    var window: NSWindow!
    var view: SpectroView!
    var chips: [NSButton] = []
    var status: NSTextField!

    func applicationDidFinishLaunching(_ n: Notification) {
        guard let img = NSImage(contentsOf: specURL) else {
            fputs("✗ missing \(specURL.path) — render wizard assets first\n", stderr); exit(1)
        }
        view = SpectroView(spec: img)
        view.player = try? AVAudioPlayer(contentsOf: stemURL)
        view.player?.prepareToPlay()
        seed()

        let scroll = NSScrollView()
        scroll.documentView = view
        scroll.hasHorizontalScroller = true
        scroll.hasVerticalScroller = false

        let bar = NSStackView()
        bar.orientation = .horizontal
        bar.spacing = 6
        bar.edgeInsets = NSEdgeInsets(top: 8, left: 12, bottom: 8, right: 12)
        for (i, s) in SYLS.enumerated() {
            let b = NSButton(title: s.0, target: self, action: #selector(pick(_:)))
            b.tag = i; b.bezelStyle = .rounded; b.setButtonType(.momentaryPushIn)
            chips.append(b); bar.addArrangedSubview(b)
        }
        let export = NSButton(title: "⇩ export", target: self, action: #selector(doExport))
        bar.addArrangedSubview(export)
        status = NSTextField(labelWithString: "")
        bar.addArrangedSubview(status)

        let root = NSStackView()
        root.orientation = .vertical
        root.spacing = 0
        root.addArrangedSubview(bar)
        root.addArrangedSubview(scroll)
        scroll.heightAnchor.constraint(equalToConstant: view.frame.height + 16).isActive = true

        window = NSWindow(contentRect: NSRect(x: 80, y: 80, width: 1680, height: view.frame.height + 70),
                          styleMask: [.titled, .closable, .resizable, .miniaturizable],
                          backing: .buffered, defer: false)
        window.title = "SyllaWizard · take \(take)"
        window.contentView = root
        window.makeKeyAndOrderFront(nil)
        window.makeFirstResponder(view)
        NSApp.activate(ignoringOtherApps: true)
        view.onChange = { [weak self] in self?.refresh() }
        refresh()
    }

    func seed() {
        // work already drawn wins over machine guesses, and it is saved on
        // every change — closing the window must never lose an adjustment
        if let data = try? Data(contentsOf: outURL),
           let j = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
           let sylls = j["sylls"] as? [[String: Any]] {
            for s in sylls {
                guard let lab = s["label"] as? String, let wi = s["wi"] as? Int,
                      let a = s["fromMs"] as? Int, let b = s["toMs"] as? Int else { continue }
                if let i = SYLS.firstIndex(where: { $0.0 == lab && $0.1 == wi }),
                   view.rects[i] == nil {
                    view.rects[i] = Rect(fromMs: a, toMs: b,
                                         fLo: (s["fLo"] as? Double) ?? 0.12,
                                         fHi: (s["fHi"] as? Double) ?? 0.9)
                }
            }
            return
        }
        let bp = work.appendingPathComponent("bounds-\(take).json")
        guard let data = try? Data(contentsOf: bp),
              let j = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let words = j["words"] as? [[String: Any]] else { return }
        var flat: [[String: Any]] = []
        for w in words { flat += (w["sylls"] as? [[String: Any]]) ?? [] }
        for (i, s) in flat.prefix(SYLS.count).enumerated() {
            if let a = s["fromMs"] as? Int, let b = s["toMs"] as? Int {
                view.rects[i] = Rect(fromMs: a, toMs: b, fLo: 0.12, fHi: 0.9)
            }
        }
    }

    @objc func pick(_ sender: NSButton) {
        view.cur = sender.tag
        window.makeFirstResponder(view)
        refresh()
    }
    func refresh() {
        for (i, b) in chips.enumerated() {
            b.contentTintColor = i == view.cur ? NSColor(calibratedRed: 1, green: 0.36, blue: 0.62, alpha: 1)
                : (view.rects[i] != nil ? NSColor(calibratedRed: 0.42, green: 0.8, blue: 0.55, alpha: 1)
                                        : NSColor.secondaryLabelColor)
        }
        let done = view.rects.compactMap { $0 }.count
        status.stringValue = "\(done)/\(SYLS.count) · → \(SYLS[view.cur].0) · autosaved"
        view.needsDisplay = true
        save()                                  // every adjustment persists
    }

    @objc func doExport() { save(); status.stringValue = "✓ wrote \(outURL.lastPathComponent)" }

    func save() {
        var sylls: [[String: Any]] = []
        for (i, r) in view.rects.enumerated() {
            guard let r = r else { continue }
            sylls.append(["label": SYLS[i].0, "wi": SYLS[i].1,
                          "fromMs": r.fromMs, "toMs": r.toMs, "fLo": r.fLo, "fHi": r.fHi])
        }
        let doc: [String: Any] = ["take": take, "drawn": ISO8601DateFormatter().string(from: Date()),
                                  "tool": "SyllaWizard", "sylls": sylls]
        if let data = try? JSONSerialization.data(withJSONObject: doc, options: [.prettyPrinted, .sortedKeys]) {
            try? data.write(to: outURL)
        }
    }

    func applicationWillTerminate(_ n: Notification) { save() }
    func applicationShouldTerminateAfterLastWindowClosed(_ s: NSApplication) -> Bool { true }
}

let app = NSApplication.shared
let delegate = App()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
