// WhistlegraphWizard — a trackpad instrument for recording whistlegraph
// gestures word by word (TrackDrum's sibling). Pops open with a score,
// reads the trackpad's indirect touches as pen-down/pen-up drawing, and
// stores every take so videos and sequences can be built from the data.
//
//   WhistlegraphWizard --score wordclock.json --out recording.json \
//                      [--under drawing.png]
//
// The score is a wordclock: [{word, mark, v0, v1, ...}] — entries are
// read until the first repeated mark (one pass). Coordinates are stored
// in the underlay's pixel space (default 452x698).
//
// Keys:  → / return  accept word, advance      ←  back a word
//        r  retake (archives the current take)  u  toggle underlay
//        p  replay the aggregate                 s  save
//        q / esc  save & quit
//
// A take is every stroke drawn while a word is up (multi-stroke words —
// two eye dots — are one take with two segments). All takes are kept;
// the newest is chosen.

import AppKit

// ── data ───────────────────────────────────────────────────────────────

struct Pt: Codable { var t, x, y: Double }
struct Take: Codable { var t0: Double; var segments: [[Pt]] }
struct WordRec: Codable {
  var word: String
  var mark: String?
  var v0: Double?
  var v1: Double?
  var takes: [Take] = []
  var chosen: Int = -1
}
struct Recording: Codable {
  var version = 1
  var canvasW: Double
  var canvasH: Double
  var words: [WordRec]
}
struct ClockEntry: Codable {
  var word: String
  var mark: String?
  var v0: Double?
  var v1: Double?
}

func arg(_ key: String) -> String? {
  let a = CommandLine.arguments
  if let i = a.firstIndex(of: key), i + 1 < a.count { return a[i + 1] }
  return nil
}

// ── the canvas ─────────────────────────────────────────────────────────

final class WizardView: NSView {
  var rec: Recording
  var outPath: String
  var under: NSImage?
  var showUnder = true
  var idx = 0
  var liveSegments: [[Pt]] = []
  var livePoints: [Pt] = []
  var penDown = false
  var takeStart: Double = 0
  var replayT: Double? = nil
  var replayTimer: Timer?

  let ink = NSColor(calibratedRed: 0.13, green: 0.11, blue: 0.16, alpha: 0.92)
  let red = NSColor(calibratedRed: 0.75, green: 0.2, blue: 0.24, alpha: 0.95)

  init(rec: Recording, outPath: String, under: NSImage?) {
    self.rec = rec
    self.outPath = outPath
    self.under = under
    super.init(frame: .zero)
  }
  required init?(coder: NSCoder) { fatalError() }
  override var acceptsFirstResponder: Bool { true }
  override var isFlipped: Bool { true }

  // canvas rect: the score space letterboxed into the view
  var canvasRect: NSRect {
    let aspect = rec.canvasW / rec.canvasH
    let pad: CGFloat = 24
    let availW = bounds.width - pad * 2
    let availH = bounds.height - 120 - pad
    let scale = min(availW / rec.canvasW, availH / rec.canvasH)
    let w = rec.canvasW * scale, h = rec.canvasH * scale
    _ = aspect
    return NSRect(x: (bounds.width - w) / 2, y: pad, width: w, height: h)
  }
  func toCanvas(_ n: NSPoint) -> Pt {
    // trackpad normalized (y up) -> score space (y down), full-pad map
    Pt(t: 0, x: Double(n.x) * rec.canvasW,
       y: (1 - Double(n.y)) * rec.canvasH)
  }
  func toView(_ p: Pt) -> NSPoint {
    let r = canvasRect
    return NSPoint(x: r.minX + CGFloat(p.x / rec.canvasW) * r.width,
                   y: r.minY + CGFloat(p.y / rec.canvasH) * r.height)
  }

  func penUp() {
    guard penDown else { return }
    penDown = false
    if livePoints.count >= 1 { liveSegments.append(livePoints) }
    livePoints = []
    needsDisplay = true
  }

  // the pen: click = dot, click-hold-drag = line
  override func mouseDown(with event: NSEvent) {
    guard replayT == nil else { return }
    penDown = true
    if liveSegments.isEmpty { takeStart = event.timestamp }
    let r = canvasRect
    let l = convert(event.locationInWindow, from: nil)
    var p = Pt(t: event.timestamp - takeStart,
               x: Double((l.x - r.minX) / r.width) * rec.canvasW,
               y: Double((l.y - r.minY) / r.height) * rec.canvasH)
    p.x = min(max(p.x, 0), rec.canvasW)
    p.y = min(max(p.y, 0), rec.canvasH)
    livePoints = [p]
    needsDisplay = true
  }
  override func mouseDragged(with event: NSEvent) {
    guard penDown else { return }
    let r = canvasRect
    let l = convert(event.locationInWindow, from: nil)
    livePoints.append(Pt(
      t: event.timestamp - takeStart,
      x: min(max(Double((l.x - r.minX) / r.width) * rec.canvasW, 0),
             rec.canvasW),
      y: min(max(Double((l.y - r.minY) / r.height) * rec.canvasH, 0),
             rec.canvasH)))
    needsDisplay = true
  }
  override func mouseUp(with event: NSEvent) { penUp() }

  // ── words ────────────────────────────────────────────────────────────
  func acceptWord() {
    penUp()
    if !liveSegments.isEmpty {
      rec.words[idx].takes.append(
        Take(t0: takeStart, segments: liveSegments))
      rec.words[idx].chosen = rec.words[idx].takes.count - 1
      liveSegments = []
    }
    save()
    if idx < rec.words.count - 1 { idx += 1 }
    needsDisplay = true
  }
  func retake() {
    penUp()
    if !liveSegments.isEmpty {
      rec.words[idx].takes.append(          // archive the abandoned take
        Take(t0: takeStart, segments: liveSegments))
      rec.words[idx].chosen = -1
      liveSegments = []
    } else if rec.words[idx].chosen >= 0 {
      rec.words[idx].chosen = -1            // unchoose the stored take
    }
    needsDisplay = true
  }
  func save() {
    let enc = JSONEncoder()
    if let d = try? enc.encode(rec) {
      try? d.write(to: URL(fileURLWithPath: outPath))
    }
  }

  override func keyDown(with event: NSEvent) {
    switch event.keyCode {
    case 124, 36: acceptWord()                       // →, return
    case 123: penUp(); liveSegments = []; if idx > 0 { idx -= 1 }
              needsDisplay = true                     // ←
    case 53: save(); NSApp.terminate(nil)             // esc
    default:
      switch event.charactersIgnoringModifiers?.lowercased() {
      case "r": retake()
      case "u": showUnder.toggle(); needsDisplay = true
      case "s": save()
      case "p": startReplay()
      case "q": save(); NSApp.terminate(nil)
      default: super.keyDown(with: event)
      }
    }
  }

  func startReplay() {
    replayTimer?.invalidate()
    replayT = 0
    replayTimer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60,
                                       repeats: true) { [weak self] tm in
      guard let self else { tm.invalidate(); return }
      self.replayT! += 1.0 / 60
      if self.replayT! > 27 { self.replayT = nil; tm.invalidate() }
      self.needsDisplay = true
    }
  }

  // ── drawing ──────────────────────────────────────────────────────────
  func strokePath(_ seg: [Pt], _ color: NSColor, upTo: Double = .infinity) {
    guard let first = seg.first else { return }
    color.setStroke()
    color.setFill()
    if seg.count == 1 {
      let v = toView(first)
      NSBezierPath(ovalIn: NSRect(x: v.x - 3, y: v.y - 3,
                                  width: 6, height: 6)).fill()
      return
    }
    let path = NSBezierPath()
    path.lineWidth = 3.4
    path.lineCapStyle = .round
    path.lineJoinStyle = .round
    path.move(to: toView(first))
    for p in seg.dropFirst() where p.t <= upTo {
      path.line(to: toView(p))
    }
    path.stroke()
  }

  override func draw(_ dirtyRect: NSRect) {
    NSColor(calibratedRed: 0.965, green: 0.945, blue: 0.92,
            alpha: 1).setFill()
    bounds.fill()
    let r = canvasRect
    NSColor.white.setFill()
    r.fill()
    NSColor(white: 0.6, alpha: 0.6).setStroke()
    NSBezierPath(rect: r).stroke()
    if showUnder, let u = under {
      u.draw(in: r, from: .zero, operation: .sourceOver,
             fraction: 0.18, respectFlipped: true, hints: nil)
    }

    if let rt = replayT {
      // replay: each word's chosen take retimed to its score window
      for w in rec.words where w.chosen >= 0 {
        let take = w.takes[w.chosen]
        let v0 = w.v0 ?? 0, v1 = w.v1 ?? v0 + 1
        guard rt > v0 else { continue }
        let dur = take.segments.flatMap { $0 }.map(\.t).max() ?? 1
        let local = (min(rt, v1) - v0) / max(v1 - v0, 0.001) * dur
        for seg in take.segments {
          strokePath(seg, ink, upTo: local)
        }
      }
    } else {
      // accepted words in ink, current word's live stroke in red
      for (i, w) in rec.words.enumerated()
      where w.chosen >= 0 && (i != idx || liveSegments.isEmpty) {
        for seg in w.takes[w.chosen].segments { strokePath(seg, ink) }
      }
      for seg in liveSegments { strokePath(seg, red) }
      if !livePoints.isEmpty { strokePath(livePoints, red) }
    }

    // HUD: the word being drawn
    let w = rec.words[idx]
    let title = "\(w.word)" + (w.mark.map { "   ·   \($0)" } ?? "")
    let sub = "word \(idx + 1)/\(rec.words.count)   takes \(w.takes.count)"
      + (w.chosen >= 0 ? "  ✓" : "")
      + "     →accept  ←back  r retake  p replay  u underlay  q quit"
    let big: [NSAttributedString.Key: Any] = [
      .font: NSFont.boldSystemFont(ofSize: 34),
      .foregroundColor: NSColor(calibratedRed: 0.13, green: 0.11,
                                blue: 0.16, alpha: 1)]
    let small: [NSAttributedString.Key: Any] = [
      .font: NSFont.systemFont(ofSize: 13),
      .foregroundColor: NSColor(white: 0.35, alpha: 1)]
    (title as NSString).draw(
      at: NSPoint(x: 28, y: bounds.height - 62), withAttributes: big)
    (sub as NSString).draw(
      at: NSPoint(x: 28, y: bounds.height - 88), withAttributes: small)
  }
}

// ── boot ───────────────────────────────────────────────────────────────

let scorePath = arg("--score") ?? "wordclock.json"
let outPath = arg("--out") ?? "wg-recording.json"
let underPath = arg("--under")

let scoreData = FileManager.default.contents(atPath: scorePath)!
let entries = try! JSONDecoder().decode([ClockEntry].self, from: scoreData)
var seen = Set<String>()
var words: [WordRec] = []
for e in entries {
  let key = "\(e.word)|\(e.mark ?? "")"
  if seen.contains(key) { break }  // pass two of the score begins
  seen.insert(key)
  words.append(WordRec(word: e.word, mark: e.mark, v0: e.v0, v1: e.v1))
}

var underImage: NSImage? = nil
var cw = 452.0, ch = 698.0
if let up = underPath, let img = NSImage(contentsOfFile: up) {
  underImage = img
  if let rep = img.representations.first {
    cw = Double(rep.pixelsWide)
    ch = Double(rep.pixelsHigh)
  }
}

// resume an existing recording if the out file already holds one
var rec = Recording(canvasW: cw, canvasH: ch, words: words)
if let d = FileManager.default.contents(atPath: outPath),
   let old = try? JSONDecoder().decode(Recording.self, from: d),
   old.words.map(\.word) == words.map(\.word) {
  rec = old
}

let app = NSApplication.shared
app.setActivationPolicy(.regular)
// size to the display: as tall as fits, width from the score's aspect
let screen = NSScreen.main?.visibleFrame
  ?? NSRect(x: 0, y: 0, width: 1280, height: 800)
let winH = min(screen.height - 24, 1100)
let winW = min(max((winH - 150) * CGFloat(cw / ch) + 56, 560),
               screen.width - 24)
let win = NSWindow(
  contentRect: NSRect(x: screen.midX - winW / 2,
                      y: screen.minY + (screen.height - winH) / 2,
                      width: winW, height: winH),
  styleMask: [.titled, .closable, .resizable],
  backing: .buffered, defer: false)
win.minSize = NSSize(width: 480, height: 620)
win.title = "Whistlegraph Wizard"
let view = WizardView(rec: rec, outPath: outPath, under: underImage)
win.contentView = view
win.makeFirstResponder(view)
win.makeKeyAndOrderFront(nil)
app.activate(ignoringOtherApps: true)

final class Delegate: NSObject, NSApplicationDelegate {
  let view: WizardView
  init(view: WizardView) { self.view = view }
  func applicationWillTerminate(_ n: Notification) { view.save() }
  func applicationShouldTerminateAfterLastWindowClosed(
    _ s: NSApplication) -> Bool { true }
}
let delegate = Delegate(view: view)
app.delegate = delegate
app.run()
