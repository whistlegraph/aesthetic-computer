import AppKit

/// Tiny numeric flat-map of all 128 General MIDI programs. 8 columns × 16
/// rows, one cell per program — number-only, family-colored background.
/// No scrolling, no names: a glance shows the whole instrument set.
/// Click a cell → onCommit; the popover commits the program and plays a
/// preview note in it.
///
/// Replaces the scrollable named-list because the named list ate the
/// popover's vertical real estate. The numeric map is ~224 × 224 px and
/// fits comfortably alongside the rest of the controls.
final class InstrumentListView: NSView {
    static let cols = 8
    static let rows = 16
    static let cellW: CGFloat = 28
    static let cellH: CGFloat = 14

    static let preferredWidth:  CGFloat = cellW * CGFloat(cols)    // 224
    static let preferredHeight: CGFloat = cellH * CGFloat(rows)    // 224

    var selectedProgram: UInt8 = 0 { didSet { needsDisplay = true } }
    private(set) var hoveredProgram: UInt8?
    var onCommit: ((Int) -> Void)?
    /// Fires whenever the hovered cell changes (including transitions to
    /// "no hover" → nil). Drives the controller's hover-preview note for
    /// sonic browsing.
    var onHover: ((Int?) -> Void)?

    private var trackingArea: NSTrackingArea?

    override var isFlipped: Bool { true }   // top-down rows, reading order

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setFrameSize(NSSize(width: Self.preferredWidth, height: Self.preferredHeight))
    }
    required init?(coder: NSCoder) { fatalError() }

    override var intrinsicContentSize: NSSize {
        NSSize(width: Self.preferredWidth, height: Self.preferredHeight)
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let ta = trackingArea { removeTrackingArea(ta) }
        let ta = NSTrackingArea(
            rect: bounds,
            options: [.mouseEnteredAndExited, .mouseMoved,
                      .activeAlways, .inVisibleRect],
            owner: self, userInfo: nil
        )
        addTrackingArea(ta)
        trackingArea = ta
    }

    // MARK: - Geometry

    private func cellRect(program p: Int) -> NSRect {
        let col = p % Self.cols
        let row = p / Self.cols
        return NSRect(x: CGFloat(col) * Self.cellW,
                      y: CGFloat(row) * Self.cellH,
                      width: Self.cellW,
                      height: Self.cellH)
    }

    private func program(at point: NSPoint) -> Int? {
        guard bounds.contains(point) else { return nil }
        let col = Int(point.x / Self.cellW)
        let row = Int(point.y / Self.cellH)
        guard col >= 0, col < Self.cols, row >= 0, row < Self.rows else { return nil }
        let p = row * Self.cols + col
        return p < 128 ? p : nil
    }

    private func familyColor(forProgram p: Int) -> NSColor {
        // 16 families × 8 programs each. Each row IS a family (since cols=8).
        let famIdx = p / 8
        return NSColor(hue: CGFloat(famIdx) / 16.0,
                       saturation: 0.55, brightness: 0.88, alpha: 1.0)
    }

    // MARK: - Drawing

    private static let numAttrs: [NSAttributedString.Key: Any] = [
        .font: NSFont.monospacedDigitSystemFont(ofSize: 9, weight: .medium),
        .foregroundColor: NSColor.labelColor.withAlphaComponent(0.85),
    ]
    private static let numAttrsSelected: [NSAttributedString.Key: Any] = [
        .font: NSFont.monospacedDigitSystemFont(ofSize: 9, weight: .bold),
        .foregroundColor: NSColor.white,
    ]

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        for p in 0..<128 {
            let r = cellRect(program: p)
            guard r.intersects(dirtyRect) else { continue }
            let fam = familyColor(forProgram: p)

            // Background — family-tinted at low opacity so the grid reads
            // as 16 rainbow rows.
            fam.withAlphaComponent(0.20).setFill()
            NSBezierPath(rect: r).fill()

            if selectedProgram == UInt8(p) {
                fam.setFill()
                NSBezierPath(rect: r).fill()
            } else if hoveredProgram == UInt8(p) {
                NSColor.controlAccentColor.withAlphaComponent(0.35).setFill()
                NSBezierPath(rect: r).fill()
            }

            // 1px hairline grid.
            NSColor.black.withAlphaComponent(0.10).setStroke()
            let path = NSBezierPath(rect: r.insetBy(dx: 0.25, dy: 0.25))
            path.lineWidth = 0.5
            path.stroke()

            // Program number, centered.
            let attrs = (selectedProgram == UInt8(p)) ? Self.numAttrsSelected : Self.numAttrs
            let str = NSAttributedString(string: String(format: "%03d", p), attributes: attrs)
            let size = str.size()
            str.draw(at: NSPoint(x: r.midX - size.width / 2,
                                 y: r.midY - size.height / 2))
        }
    }

    // MARK: - Mouse

    // Hover preview is press-gated: passive mouse-over does NOT light cells
    // or trigger preview audio. The user has to mouseDown first; while held,
    // dragging across cells lights/sounds each one and unlights it on the
    // way out. mouseUp stops the sound and commits the cell under the cursor.
    private var dragging = false

    override func mouseEntered(with event: NSEvent) {
        guard dragging else { return }
        updateHover(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseMoved(with event: NSEvent) {
        guard dragging else { return }
        updateHover(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseExited(with event: NSEvent) {
        guard dragging else { return }
        if let prev = hoveredProgram {
            hoveredProgram = nil
            setNeedsDisplay(cellRect(program: Int(prev)))
            onHover?(nil)
        }
    }

    private func updateHover(at point: NSPoint) {
        let p = program(at: point).map { UInt8($0) }
        if p != hoveredProgram {
            let prev = hoveredProgram
            hoveredProgram = p
            if let prev = prev { setNeedsDisplay(cellRect(program: Int(prev))) }
            if let p = p { setNeedsDisplay(cellRect(program: Int(p))) }
            onHover?(p.map { Int($0) })
        }
    }

    override func mouseDown(with event: NSEvent) {
        dragging = true
        let pt = convert(event.locationInWindow, from: nil)
        if let p = program(at: pt) {
            // Treat the press as a hover-into-this-cell so the preview note
            // and lit highlight start immediately on click.
            if hoveredProgram != UInt8(p) {
                let prev = hoveredProgram
                hoveredProgram = UInt8(p)
                if let prev = prev { setNeedsDisplay(cellRect(program: Int(prev))) }
                setNeedsDisplay(cellRect(program: p))
            }
            onHover?(p)
        }
    }

    override func mouseDragged(with event: NSEvent) {
        guard dragging else { return }
        updateHover(at: convert(event.locationInWindow, from: nil))
    }

    override func mouseUp(with event: NSEvent) {
        guard dragging else { return }
        dragging = false
        let pt = convert(event.locationInWindow, from: nil)
        // Release stops the preview note + clears the lit highlight, then
        // commits the cell that was under the cursor at release time. The
        // commit path will re-light the cell as the *selected* program.
        let prevHover = hoveredProgram
        hoveredProgram = nil
        if let prev = prevHover { setNeedsDisplay(cellRect(program: Int(prev))) }
        onHover?(nil)
        if let p = program(at: pt) {
            onCommit?(p)
        }
    }

    /// No-op kept for API compatibility with the popover (was needed when
    /// the list was scrollable; the numeric map shows everything at once).
    func scrollProgramIntoView(_ program: UInt8, animated: Bool = false) {}
}
