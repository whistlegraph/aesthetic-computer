// CanvasView — draws the word from the current skeletons + axes, overlays the
// editable skeleton, and lets you drag skeleton points to author letterforms.
import AppKit

final class Doc {
    var glyphs = Skeletons.base()
    var axes = Axes()
    var word = "regarde"
    var showSkeleton = true
}

final class CanvasView: NSView {
    var doc = Doc()
    private var layout: [(ch: Character, penX: CGFloat)] = []
    private var scale: CGFloat = 0.4
    private var originX: CGFloat = 90
    private var originY: CGFloat = 0
    private var drag: (ch: Character, si: Int, pi: Int)? = nil

    private let ink = NSColor(red: 0.047, green: 0.047, blue: 0.051, alpha: 1)
    private let paper = NSColor(red: 0.969, green: 0.965, blue: 0.957, alpha: 1)
    private let red = NSColor(red: 0.898, green: 0.141, blue: 0.165, alpha: 1)
    private let skel = NSColor(red: 0.0, green: 0.45, blue: 0.95, alpha: 0.9)

    override var isFlipped: Bool { false }

    private func tanSlant() -> CGFloat { tan(doc.axes.slant * .pi / 180) }

    private func toView(_ p: CGPoint, _ penX: CGFloat) -> CGPoint {
        let gx = p.x * doc.axes.width + penX
        let gy = p.y * doc.axes.xHeight
        return CGPoint(x: originX + (gx + tanSlant()*gy) * scale, y: originY + gy * scale)
    }
    private func toUnit(_ v: CGPoint, _ penX: CGFloat) -> CGPoint {
        let gy = (v.y - originY) / scale
        let gx = (v.x - originX) / scale - tanSlant()*gy
        return CGPoint(x: (gx - penX) / doc.axes.width, y: gy / doc.axes.xHeight)
    }

    private func relayout() {
        layout.removeAll()
        var penX: CGFloat = 0
        for ch in doc.word {
            guard let g = doc.glyphs[ch] else { continue }
            layout.append((ch, penX))
            penX += g.advance * doc.axes.width + doc.axes.tracking
        }
        scale = max((bounds.height * 0.46) / Metrics.em, 0.05)
        originY = bounds.height * 0.36
    }

    override func draw(_ dirty: NSRect) {
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        paper.setFill(); ctx.fill(bounds)
        relayout()

        // glyphs
        ink.setFill()
        var lastEnd: CGFloat = originX
        for (ch, penX) in layout {
            guard let g = doc.glyphs[ch] else { continue }
            let path = CGMutablePath()
            for loop in Expander.contours(g, doc.axes) {
                guard let f = loop.first else { continue }
                path.move(to: toView(f, penX))
                for q in loop.dropFirst() { path.addLine(to: toView(q, penX)) }
                path.closeSubpath()
            }
            ctx.addPath(path); ctx.fillPath(using: .winding)
            lastEnd = toView(CGPoint(x: g.advance, y: 0), penX).x
        }

        // the ■ mark
        let sq = Metrics.xHeight * 0.32 * scale * doc.axes.xHeight
        red.setFill()
        ctx.fill(CGRect(x: lastEnd + 28, y: originY, width: sq, height: sq))

        // skeleton overlay
        if doc.showSkeleton {
            for (ch, penX) in layout {
                guard let g = doc.glyphs[ch] else { continue }
                for s in g.strokes {
                    ctx.setStrokeColor(skel.withAlphaComponent(0.35).cgColor); ctx.setLineWidth(1)
                    let pts = s.pts.map { toView($0, penX) }
                    if let f = pts.first { ctx.move(to: f); for q in pts.dropFirst() { ctx.addLine(to: q) }; if s.closed, let ff = pts.first { ctx.addLine(to: ff) }; ctx.strokePath() }
                    skel.setFill()
                    for v in pts { ctx.fillEllipse(in: CGRect(x: v.x-3, y: v.y-3, width: 6, height: 6)) }
                }
            }
        }
    }

    // ── editing ──
    override func mouseDown(with e: NSEvent) {
        let v = convert(e.locationInWindow, from: nil)
        var best: (d: CGFloat, ch: Character, si: Int, pi: Int)? = nil
        for (ch, penX) in layout {
            guard let g = doc.glyphs[ch] else { continue }
            for (si, s) in g.strokes.enumerated() {
                for (pi, p) in s.pts.enumerated() {
                    let vp = toView(p, penX)
                    let d = hypot(vp.x - v.x, vp.y - v.y)
                    if d < 9, best == nil || d < best!.d { best = (d, ch, si, pi) }
                }
            }
        }
        if let b = best { drag = (b.ch, b.si, b.pi) }
    }
    override func mouseDragged(with e: NSEvent) {
        guard let d = drag else { return }
        let v = convert(e.locationInWindow, from: nil)
        let penX = layout.first(where: { $0.ch == d.ch })?.penX ?? 0
        let u = toUnit(v, penX)
        doc.glyphs[d.ch]?.strokes[d.si].pts[d.pi] = u
        needsDisplay = true
    }
    override func mouseUp(with e: NSEvent) { drag = nil }
}
