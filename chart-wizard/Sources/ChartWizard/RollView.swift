// RollView.swift — the chart, as something you can grab.
//
// Two clocks are on screen at once, which is the whole reason this is
// hard to do in a text editor. Horizontally the roll is the BEAT GRID —
// where each word lands in the bar. Inside every block, drawn to that
// block's own width, is the piece of the take it plays, in SOURCE
// seconds. Dragging the block moves it in the first clock; dragging its
// edge moves it in the second. Melodyne's arrangement, with her measured
// pitch on the vertical axis so a word sits at the note she sang.
//
//   drag a block            move it on the grid (steals from the rest
//                           in front, gives it back to the one behind,
//                           so nothing downstream moves)
//   drag its LEFT edge      move the boundary in her voice — the cut
//                           that decides which mouth belongs to which
//                           word. Snaps to measured event edges.
//   drag its RIGHT edge     how many beats it holds
//   ⌥ while dragging        no snapping
//   click                   select · space plays from there
import AppKit

protocol RollViewDelegate: AnyObject {
    func rollDidEdit(_ view: RollView)
    func rollDidSelect(_ view: RollView, unit: Int?)
    func rollRequestsPlay(_ view: RollView, fromBeat: Double)
}

final class RollView: NSView {
    weak var delegate: RollViewDelegate?
    var model: ChartModel? { didSet { needsDisplay = true } }
    var playhead: Double? { didSet { needsDisplay = true } }   // seconds
    private(set) var selected: Int?

    private var pxPerBeat: CGFloat = 46
    private let topPad: CGFloat = 26
    private let lane: CGFloat = 15          // pixels per semitone

    // ── geometry ─────────────────────────────────────────────────────
    private var stCenter: Double {
        guard let m = model, !m.units.isEmpty else { return 4 }
        let all = m.units.map { $0.st }
        return (all.min()! + all.max()!) / 2
    }
    private func x(beat: Double) -> CGFloat { CGFloat(beat) * pxPerBeat + 8 }
    private func beat(x: CGFloat) -> Double { Double((x - 8) / pxPerBeat) }
    private func y(st: Double) -> CGFloat {
        bounds.midY + CGFloat(st - stCenter) * -lane
    }
    private func rect(_ u: Unit) -> NSRect {
        NSRect(x: x(beat: u.beat), y: y(st: u.st) - 13,
               width: max(6, CGFloat(u.dur) * pxPerBeat - 2), height: 26)
    }

    override var isFlipped: Bool { false }
    override var acceptsFirstResponder: Bool { true }

    override var intrinsicContentSize: NSSize {
        guard let m = model else { return NSSize(width: 900, height: 380) }
        return NSSize(width: CGFloat(m.phrase.beats + 2) * pxPerBeat + 16, height: 380)
    }

    func zoom(by f: CGFloat) {
        pxPerBeat = min(220, max(14, pxPerBeat * f))
        invalidateIntrinsicContentSize()
        needsDisplay = true
    }

    // ── drawing ──────────────────────────────────────────────────────
    override func draw(_ dirtyRect: NSRect) {
        NSColor(calibratedWhite: 0.08, alpha: 1).setFill()
        dirtyRect.fill()
        guard let m = model else { return }
        let ph = m.phrase

        drawGrid(m)
        for (i, u) in m.units.enumerated() { drawBlock(m, ph, u, index: i) }
        drawPlayhead(m)
    }

    private func drawGrid(_ m: ChartModel) {
        let bars = Int((m.phrase.beats / 4).rounded(.up))
        for bar in 0...max(0, bars) {
            let bx = x(beat: Double(bar * 4))
            NSColor(calibratedWhite: 0.30, alpha: 1).setFill()
            NSRect(x: bx, y: 0, width: 1, height: bounds.height).fill()
            let label = NSAttributedString(string: "\(bar)", attributes: [
                .font: NSFont.monospacedSystemFont(ofSize: 9, weight: .regular),
                .foregroundColor: NSColor(calibratedWhite: 0.45, alpha: 1)])
            label.draw(at: NSPoint(x: bx + 3, y: bounds.height - topPad + 8))
            for b in 1..<4 {
                let sx = x(beat: Double(bar * 4 + b))
                NSColor(calibratedWhite: 0.16, alpha: 1).setFill()
                NSRect(x: sx, y: 0, width: 1, height: bounds.height).fill()
            }
        }
    }

    // Her voice, drawn INSIDE the block at the block's width: peak per
    // column over the source span. This is the part that makes a bad
    // boundary visible — a word holding the next word's consonant shows
    // it as a bright tail with nothing behind it.
    private func drawBlock(_ m: ChartModel, _ ph: Phrase, _ u: Unit, index i: Int) {
        let r = rect(u)
        let isSel = (selected == i)
        let body = isSel ? NSColor(calibratedRed: 0.98, green: 0.36, blue: 0.62, alpha: 0.22)
                         : NSColor(calibratedRed: 0.44, green: 0.62, blue: 0.98, alpha: 0.18)
        body.setFill()
        NSBezierPath(roundedRect: r, xRadius: 3, yRadius: 3).fill()

        let f0 = Int(u.src0 / m.doc.frame_s), f1 = Int(u.src1 / m.doc.frame_s)
        let n = max(1, f1 - f0)
        let cols = max(1, Int(r.width))
        NSColor(calibratedWhite: 0.92, alpha: 0.75).setFill()
        for c in 0..<cols {
            let a = f0 + n * c / cols, b = max(a + 1, f0 + n * (c + 1) / cols)
            var peak = -90.0
            var bright = 0.0
            for k in a..<min(b, ph.frames.db.count) {
                peak = max(peak, ph.frames.db[k])
                bright = max(bright, ph.frames.hf[k])
            }
            guard peak > -60 else { continue }
            let h = CGFloat((peak + 60) / 60) * (r.height * 0.44)
            // a consonant is dim but BRIGHT — tint it so it can be seen
            if bright > 0.5 {
                NSColor(calibratedRed: 1.0, green: 0.85, blue: 0.35, alpha: 0.85).setFill()
            } else {
                NSColor(calibratedWhite: 0.92, alpha: 0.75).setFill()
            }
            NSRect(x: r.minX + CGFloat(c), y: r.midY - h, width: 1, height: h * 2).fill()
        }

        (isSel ? NSColor(calibratedRed: 1, green: 0.45, blue: 0.7, alpha: 1)
               : NSColor(calibratedWhite: 0.55, alpha: 1)).setStroke()
        let p = NSBezierPath(roundedRect: r, xRadius: 3, yRadius: 3)
        p.lineWidth = isSel ? 2 : 1
        p.stroke()

        // an edge no chart knob owns cannot be dragged — say so rather
        // than letting a drag silently do nothing
        if u.cut == .auto {
            NSColor(calibratedWhite: 0.35, alpha: 1).setFill()
            NSRect(x: r.minX, y: r.minY, width: 2, height: r.height).fill()
        }

        let text = NSAttributedString(string: u.t, attributes: [
            .font: NSFont.monospacedSystemFont(ofSize: 10, weight: .medium),
            .foregroundColor: NSColor(calibratedWhite: isSel ? 1.0 : 0.80, alpha: 1)])
        text.draw(at: NSPoint(x: r.minX + 3, y: r.maxY + 1))
    }

    private func drawPlayhead(_ m: ChartModel) {
        guard let t = playhead else { return }
        let px = x(beat: t / m.secondsPerBeat)
        NSColor(calibratedRed: 1, green: 0.9, blue: 0.3, alpha: 0.9).setFill()
        NSRect(x: px, y: 0, width: 1.5, height: bounds.height).fill()
    }

    // ── dragging ─────────────────────────────────────────────────────
    private enum Grab { case body(Int, Double), leftEdge(Int), rightEdge(Int) }
    private var grab: Grab?

    private func hit(_ p: NSPoint) -> Grab? {
        guard let m = model else { return nil }
        for (i, u) in m.units.enumerated().reversed() {
            let r = rect(u).insetBy(dx: 0, dy: -4)
            guard r.contains(p) else { continue }
            if p.x - r.minX < 6 { return .leftEdge(i) }
            if r.maxX - p.x < 6 { return .rightEdge(i) }
            return .body(i, beat(x: p.x) - u.beat)
        }
        return nil
    }

    override func resetCursorRects() {
        guard let m = model else { return }
        for u in m.units {
            let r = rect(u)
            addCursorRect(NSRect(x: r.minX - 3, y: r.minY, width: 8, height: r.height),
                          cursor: .resizeLeftRight)
            addCursorRect(NSRect(x: r.maxX - 5, y: r.minY, width: 8, height: r.height),
                          cursor: .resizeLeftRight)
        }
    }

    override func mouseDown(with e: NSEvent) {
        let p = convert(e.locationInWindow, from: nil)
        grab = hit(p)
        switch grab {
        case .body(let i, _), .leftEdge(let i), .rightEdge(let i):
            selected = i
        case nil:
            selected = nil
        }
        delegate?.rollDidSelect(self, unit: selected)
        needsDisplay = true
    }

    override func mouseDragged(with e: NSEvent) {
        guard let m = model, let g = grab else { return }
        let p = convert(e.locationInWindow, from: nil)
        let free = e.modifierFlags.contains(.option)
        switch g {
        case .body(let i, let offset):
            var b = beat(x: p.x) - offset
            if !free { b = (b * 2).rounded() / 2 }          // half-beat grid
            m.moveBlock(i, toBeat: b)
        case .rightEdge(let i):
            var d = beat(x: p.x) - m.units[i].beat
            if !free { d = (d * 2).rounded() / 2 }
            m.resizeBlock(i, toDur: d)
        case .leftEdge(let i):
            guard i > 0 else { return }
            // the left edge lives in HER clock, not the grid's: how far
            // into the block the pointer is, scaled back to source seconds
            let u = m.units[i]
            let r = rect(u)
            let frac = Double((p.x - r.minX) / max(1, r.width))
            var t = u.src0 + frac * (u.src1 - u.src0)
            if !free { t = snapToEvent(m, t) }
            m.moveBoundary(i, toSource: t)
        }
        delegate?.rollDidEdit(self)
        needsDisplay = true
    }

    override func mouseUp(with e: NSEvent) { grab = nil }

    /// The audio already told us where the edges are. Snap to the nearest
    /// measured event edge within 40 ms so a drag lands on a real onset
    /// rather than near one.
    private func snapToEvent(_ m: ChartModel, _ t: Double) -> Double {
        var best = t, bestD = 0.040
        for e in m.phrase.events {
            for edge in [e.a, e.b] where abs(edge - t) < bestD {
                bestD = abs(edge - t); best = edge
            }
        }
        return best
    }

    override func keyDown(with e: NSEvent) {
        guard let m = model else { return super.keyDown(with: e) }
        switch e.charactersIgnoringModifiers {
        case " ":
            let from = selected.map { m.units[$0].beat } ?? 0
            delegate?.rollRequestsPlay(self, fromBeat: from)
        case "=", "+": zoom(by: 1.25)
        case "-", "_": zoom(by: 0.8)
        default: super.keyDown(with: e)
        }
    }
}
