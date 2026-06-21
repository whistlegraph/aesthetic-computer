// ThumbStripView — the bottom filmstrip: every shot as a thumbnail tile in
// running order, the selected one ringed. Click a tile to select it; DRAG a
// tile to reorder the sequence (drag-and-drop replaces the old ◀/▶ reorder
// buttons). Tile aspect tracks the board dimensions. Lives as the documentView of
// the strip scroll view so it scrolls when there are more tiles than fit.
import AppKit

final class ThumbStripView: NSView {
    weak var controller: ShotWizardController?
    private let inset: CGFloat = 10
    private let gap: CGFloat = 10

    private var dragFrom: Int? = nil
    private var dragging = false
    private var downX: CGFloat = 0
    private var curX: CGFloat = 0

    override var isFlipped: Bool { true }
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }

    private var count: Int { controller?.board.shots.count ?? 0 }
    private var tileH: CGFloat { max(40, bounds.height - inset * 2) }
    /// Tile aspect tracks the board's real dimensions (landscape 16:9, portrait
    /// 9:16, square — whatever the project is), not a hardcoded reel shape.
    private var aspect: CGFloat {
        guard let b = controller?.board, b.width > 0, b.height > 0 else { return 9.0 / 16.0 }
        return CGFloat(b.width) / CGFloat(b.height)
    }
    private var tileW: CGFloat { tileH * aspect }
    private var stride: CGFloat { tileW + gap }

    /// Total content width so the enclosing scroll view can scroll.
    var contentWidth: CGFloat { inset * 2 + CGFloat(max(1, count)) * stride - gap }

    private func tileRect(_ i: Int) -> NSRect {
        NSRect(x: inset + CGFloat(i) * stride, y: inset, width: tileW, height: tileH)
    }
    private func indexAt(_ p: NSPoint) -> Int? {
        for i in 0..<count where tileRect(i).contains(p) { return i }
        return nil
    }
    /// Insertion slot for a drop centered at x.
    private func slotForX(_ x: CGFloat) -> Int {
        let s = Int(((x - inset) / stride).rounded())
        return max(0, min(count - 1, s))
    }

    func reload() {
        frame = NSRect(x: 0, y: 0, width: max(contentWidth, superview?.bounds.width ?? contentWidth),
                       height: superview?.bounds.height ?? bounds.height)
        needsDisplay = true
    }

    override func draw(_ dirty: NSRect) {
        guard let c = controller else { return }
        for (i, shot) in c.board.shots.enumerated() {
            if dragging && i == dragFrom { continue }      // hide the lifted tile in place
            drawTile(i, shot: shot, rect: tileRect(i), c: c)
        }
        // the lifted tile floats under the cursor, drawn last (on top)
        if dragging, let di = dragFrom, c.board.shots.indices.contains(di) {
            var r = tileRect(di); r.origin.x = curX - tileW / 2
            // insertion marker
            let slot = slotForX(curX)
            let markX = inset + CGFloat(slot) * stride - gap / 2
            NSColor.systemYellow.setFill()
            NSBezierPath(rect: NSRect(x: markX - 1.5, y: inset, width: 3, height: tileH)).fill()
            drawTile(di, shot: c.board.shots[di], rect: r, c: c, floating: true)
        }
    }

    private func drawTile(_ i: Int, shot: Shot, rect r: NSRect, c: ShotWizardController, floating: Bool = false) {
        let img = c.thumbnail(for: shot)
        NSGraphicsContext.saveGraphicsState()
        if floating {
            let sh = NSShadow(); sh.shadowColor = NSColor.black.withAlphaComponent(0.6)
            sh.shadowBlurRadius = 12; sh.shadowOffset = NSSize(width: 0, height: -3); sh.set()
        }
        let clip = NSBezierPath(roundedRect: r, xRadius: 6, yRadius: 6)
        clip.addClip()
        // aspect-FILL the portrait tile
        let iw = img.size.width, ih = img.size.height
        var dr = r
        if iw > 0 && ih > 0 {
            let scale = max(r.width / iw, r.height / ih)
            let w = iw * scale, h = ih * scale
            dr = NSRect(x: r.midX - w / 2, y: r.midY - h / 2, width: w, height: h)
        }
        // respectFlipped so still PNGs aren't drawn upside-down in this flipped view
        img.draw(in: dr, from: .zero, operation: .copy, fraction: 1.0,
                 respectFlipped: true, hints: nil)
        NSGraphicsContext.restoreGraphicsState()
        // ring
        let ring = NSBezierPath(roundedRect: r.insetBy(dx: 1.5, dy: 1.5), xRadius: 6, yRadius: 6)
        (i == c.sel ? NSColor.systemYellow : c.borderColor(for: shot, selected: false)).setStroke()
        ring.lineWidth = i == c.sel ? 3 : 1.5
        ring.stroke()
        // index badge
        let badge = "\(i + 1)"
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 11, weight: .bold),
            .foregroundColor: NSColor.white,
        ]
        let bg = NSRect(x: r.minX + 4, y: r.minY + 4, width: 18, height: 16)
        NSColor.black.withAlphaComponent(0.55).setFill()
        NSBezierPath(roundedRect: bg, xRadius: 3, yRadius: 3).fill()
        badge.draw(at: NSPoint(x: r.minX + 8, y: r.minY + 4), withAttributes: attrs)
    }

    override func mouseDown(with e: NSEvent) {
        let p = convert(e.locationInWindow, from: nil)
        downX = p.x; curX = p.x
        dragFrom = indexAt(p)
        dragging = false
    }
    override func mouseDragged(with e: NSEvent) {
        guard dragFrom != nil else { return }
        let p = convert(e.locationInWindow, from: nil)
        if !dragging && abs(p.x - downX) < 6 { return }   // small threshold → it's a click
        dragging = true; curX = p.x
        // auto-scroll the enclosing clip view when dragging near an edge
        if let clip = superview as? NSClipView {
            let vis = clip.documentVisibleRect
            if p.x > vis.maxX - 30 { clip.scroll(to: NSPoint(x: min(frame.width - vis.width, vis.minX + 24), y: 0)) }
            else if p.x < vis.minX + 30 { clip.scroll(to: NSPoint(x: max(0, vis.minX - 24), y: 0)) }
        }
        needsDisplay = true
    }
    override func mouseUp(with e: NSEvent) {
        let p = convert(e.locationInWindow, from: nil)
        defer { dragging = false; dragFrom = nil; needsDisplay = true }
        guard let di = dragFrom else { return }
        if !dragging { controller?.select(di); return }   // a click → select
        let to = slotForX(p.x)
        if to != di { controller?.reorder(from: di, to: to) }
    }
}
