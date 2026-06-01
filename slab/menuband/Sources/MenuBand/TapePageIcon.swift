import AppKit
import UniformTypeIdentifiers

/// Small "page" affordance pinned to the bottom-right of the tape
/// staff. The score itself is now the live Swift staff (no draggable
/// page), so this icon is the handle for the PDF:
///
///   • click → engrave a PDF on demand and open it in Preview
///   • drag  → export the PDF (drop on Finder / Mail / etc.)
///   • drag to Trash → clear the take (with the paper-crumple SFX)
///
/// PDF engraving is delegated back to the popover via `requestPDF`,
/// which routes to the lazy Verovio `SheetMusicView`.
final class TapePageIcon: NSView, NSDraggingSource {
    weak var score: TapeScore?
    /// Popover hook: engrave the current take and call back with the
    /// PDF URL (nil on failure).
    var requestPDF: ((@escaping (URL?) -> Void) -> Void)?
    /// Called after a successful drag (export or trash) so the popover
    /// can reset the staff/scroll alongside the score clear.
    var onCleared: (() -> Void)?

    var tint: NSColor = .secondaryLabelColor { didSet { needsDisplay = true } }

    private var mouseDownAt: NSPoint?
    private var dragging = false
    /// Freshest engraved PDF, primed on mouse-down so a drag has a
    /// file to carry. May be a few ms stale relative to the very last
    /// note — acceptable, matches the old drag-out behavior.
    private var cachedPDFURL: URL?

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        toolTip = "Score PDF — click to open, drag to export, drag to Trash to clear"
    }
    required init?(coder: NSCoder) { nil }

    override var intrinsicContentSize: NSSize { NSSize(width: 22, height: 28) }

    // MARK: - Draw — a little folded-corner page with staff ticks

    override func draw(_ dirtyRect: NSRect) {
        let r = bounds.insetBy(dx: 2, dy: 2)
        let fold: CGFloat = 6
        let page = NSBezierPath()
        page.move(to: NSPoint(x: r.minX, y: r.minY))
        page.line(to: NSPoint(x: r.minX, y: r.maxY))
        page.line(to: NSPoint(x: r.maxX - fold, y: r.maxY))
        page.line(to: NSPoint(x: r.maxX, y: r.maxY - fold))
        page.line(to: NSPoint(x: r.maxX, y: r.minY))
        page.close()

        (NSColor.textBackgroundColor).withAlphaComponent(0.9).setFill()
        page.fill()
        tint.withAlphaComponent(0.85).setStroke()
        page.lineWidth = 1
        page.stroke()

        // Folded corner triangle.
        let corner = NSBezierPath()
        corner.move(to: NSPoint(x: r.maxX - fold, y: r.maxY))
        corner.line(to: NSPoint(x: r.maxX - fold, y: r.maxY - fold))
        corner.line(to: NSPoint(x: r.maxX, y: r.maxY - fold))
        tint.withAlphaComponent(0.6).setStroke()
        corner.lineWidth = 1
        corner.stroke()

        // A few faux staff lines so it reads as "sheet music".
        tint.withAlphaComponent(0.5).setStroke()
        let lines = 3
        for i in 0..<lines {
            let y = r.minY + r.height * (0.32 + 0.16 * CGFloat(i))
            let l = NSBezierPath()
            l.lineWidth = 0.8
            l.move(to: NSPoint(x: r.minX + 3, y: y))
            l.line(to: NSPoint(x: r.maxX - 3, y: y))
            l.stroke()
        }
    }

    // MARK: - Mouse

    override func mouseDown(with event: NSEvent) {
        mouseDownAt = convert(event.locationInWindow, from: nil)
        dragging = false
        // Prime a PDF so a drag started a moment later has a file.
        guard score?.isEmpty == false else { return }
        requestPDF? { [weak self] url in self?.cachedPDFURL = url }
    }

    override func mouseDragged(with event: NSEvent) {
        guard let start = mouseDownAt, !dragging else { return }
        let p = convert(event.locationInWindow, from: nil)
        guard abs(p.x - start.x) > 6 || abs(p.y - start.y) > 6 else { return }
        guard let url = cachedPDFURL else {
            // Not engraved yet — kick one for the next attempt and
            // let this gesture fall through (the first drag right
            // after a fresh note can miss; the next one lands).
            requestPDF? { [weak self] u in self?.cachedPDFURL = u }
            return
        }
        dragging = true
        mouseDownAt = nil
        let item = NSDraggingItem(pasteboardWriter: url as NSURL)
        item.setDraggingFrame(bounds, contents: snapshot())
        beginDraggingSession(with: [item], event: event, source: self)
    }

    override func mouseUp(with event: NSEvent) {
        let wasClick = mouseDownAt != nil && !dragging
        mouseDownAt = nil
        guard wasClick, score?.isEmpty == false else { return }
        // Click: engrave fresh + open in Preview.
        requestPDF? { url in
            guard let url = url else { return }
            NSWorkspace.shared.open(url)
        }
    }

    private func snapshot() -> NSImage {
        let rep = bitmapImageRepForCachingDisplay(in: bounds) ?? NSBitmapImageRep()
        cacheDisplay(in: bounds, to: rep)
        let img = NSImage(size: bounds.size)
        img.addRepresentation(rep)
        return img
    }

    // MARK: - NSDraggingSource

    func draggingSession(_ session: NSDraggingSession,
                         sourceOperationMaskFor context: NSDraggingContext) -> NSDragOperation {
        // `.delete` makes the Dock Trash a valid drop → clears the
        // take; `.copy` is the regular export-anywhere path.
        return [.copy, .delete]
    }

    func draggingSession(_ session: NSDraggingSession,
                         endedAt screenPoint: NSPoint,
                         operation: NSDragOperation) {
        dragging = false
        guard operation != [] else { return }
        // Trash drop = the explicit "throw it away" gesture → SFX.
        if operation.contains(.delete) {
            CrumpleSound.shared.play()
        }
        // Either way a successful drop means "done with this take".
        score?.clear()
        cachedPDFURL = nil
        onCleared?()
    }
}
