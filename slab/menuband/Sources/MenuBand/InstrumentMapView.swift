import AppKit

/// Scrollable named-list of all 128 General MIDI programs. Each row shows
/// the program number, a family-colored stripe, and the instrument name.
/// Hover plays a held middle-C preview note in that program (silent in
/// MIDI mode); click commits.
///
/// Wrap in an NSScrollView before adding to the popover so the rows scroll
/// inside a fixed-height window — keeps the popover compact even though
/// the full list is ~2300 px tall.
final class InstrumentListView: NSView {
    static let rowHeight: CGFloat = 18
    static let stripeWidth: CGFloat = 4
    static let totalRows = 128

    static let intrinsicHeight: CGFloat = rowHeight * CGFloat(totalRows)
    static let preferredWidth: CGFloat = 248

    var selectedProgram: UInt8 = 0 { didSet { needsDisplay = true } }
    private(set) var hoveredProgram: UInt8?

    var onHover: ((Int?) -> Void)?
    var onCommit: ((Int) -> Void)?

    private var trackingArea: NSTrackingArea?

    /// Top-down rows: y=0 should be row 0 (Acoustic Grand Piano).
    override var isFlipped: Bool { true }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setFrameSize(NSSize(width: Self.preferredWidth, height: Self.intrinsicHeight))
    }
    required init?(coder: NSCoder) { fatalError() }

    override var intrinsicContentSize: NSSize {
        NSSize(width: Self.preferredWidth, height: Self.intrinsicHeight)
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

    private func rowRect(_ program: Int) -> NSRect {
        NSRect(x: 0, y: CGFloat(program) * Self.rowHeight,
               width: bounds.width, height: Self.rowHeight)
    }

    private func program(at point: NSPoint) -> Int? {
        let row = Int(point.y / Self.rowHeight)
        guard row >= 0, row < Self.totalRows else { return nil }
        guard point.x >= 0, point.x <= bounds.width else { return nil }
        return row
    }

    private func familyIndex(forProgram p: Int) -> Int { p / 8 }

    private func familyColor(_ idx: Int) -> NSColor {
        NSColor(hue: CGFloat(idx) / 16.0,
                saturation: 0.55, brightness: 0.85, alpha: 1.0)
    }

    private static let numberAttrs: [NSAttributedString.Key: Any] = [
        .font: NSFont.monospacedDigitSystemFont(ofSize: 10, weight: .regular),
        .foregroundColor: NSColor.secondaryLabelColor,
    ]
    private static let nameAttrs: [NSAttributedString.Key: Any] = [
        .font: NSFont.systemFont(ofSize: 11, weight: .regular),
        .foregroundColor: NSColor.labelColor,
    ]
    private static let nameAttrsSelected: [NSAttributedString.Key: Any] = [
        .font: NSFont.systemFont(ofSize: 11, weight: .semibold),
        .foregroundColor: NSColor.labelColor,
    ]

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)

        // Only draw rows in dirtyRect for scroll perf.
        let firstRow = max(0, Int(dirtyRect.minY / Self.rowHeight))
        let lastRow  = min(Self.totalRows - 1, Int(dirtyRect.maxY / Self.rowHeight))

        for p in firstRow...lastRow {
            let r = rowRect(p)

            // Hover/selected backgrounds.
            if hoveredProgram == UInt8(p) {
                NSColor.controlAccentColor.withAlphaComponent(0.18).setFill()
                NSBezierPath(rect: r).fill()
            } else if selectedProgram == UInt8(p) {
                NSColor.controlAccentColor.withAlphaComponent(0.10).setFill()
                NSBezierPath(rect: r).fill()
            } else if p % 2 == 0 {
                // Subtle alternating zebra so the list scans easily.
                NSColor.labelColor.withAlphaComponent(0.025).setFill()
                NSBezierPath(rect: r).fill()
            }

            // Family-colored stripe on the left.
            let famIdx = familyIndex(forProgram: p)
            let stripeRect = NSRect(x: 0, y: r.minY + 2,
                                    width: Self.stripeWidth, height: r.height - 4)
            familyColor(famIdx).setFill()
            NSBezierPath(rect: stripeRect).fill()

            // Program number (000–127), monospaced for clean alignment.
            let number = NSString(format: "%03d", p)
            number.draw(at: NSPoint(x: Self.stripeWidth + 6, y: r.minY + 3),
                        withAttributes: Self.numberAttrs)

            // Name.
            let name = GeneralMIDI.programNames[p] as NSString
            let nameAttrs = (selectedProgram == UInt8(p))
                ? Self.nameAttrsSelected
                : Self.nameAttrs
            name.draw(at: NSPoint(x: Self.stripeWidth + 36, y: r.minY + 2),
                      withAttributes: nameAttrs)
        }
    }

    override func mouseEntered(with event: NSEvent) {
        updateHover(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseMoved(with event: NSEvent) {
        updateHover(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseExited(with event: NSEvent) {
        if let prev = hoveredProgram {
            hoveredProgram = nil
            setNeedsDisplay(rowRect(Int(prev)))
            onHover?(nil)
        }
    }

    private func updateHover(at point: NSPoint) {
        let p = program(at: point).map { UInt8($0) }
        if p != hoveredProgram {
            let prev = hoveredProgram
            hoveredProgram = p
            if let prev = prev { setNeedsDisplay(rowRect(Int(prev))) }
            if let p = p { setNeedsDisplay(rowRect(Int(p))) }
            onHover?(p.map { Int($0) })
        }
    }

    override func mouseDown(with event: NSEvent) {
        if let p = program(at: convert(event.locationInWindow, from: nil)) {
            onCommit?(p)
        }
    }

    /// Scroll so the row for `program` is visible. Called after commit so
    /// the user's selection re-anchors at view center.
    func scrollProgramIntoView(_ program: UInt8, animated: Bool = false) {
        let r = rowRect(Int(program))
        guard let scroll = enclosingScrollView else { return }
        let visible = scroll.documentVisibleRect
        if !visible.contains(r) {
            // Center it if possible.
            let centerY = r.midY - visible.height / 2
            let clampedY = max(0, min(bounds.height - visible.height, centerY))
            let newOrigin = NSPoint(x: 0, y: clampedY)
            if animated {
                NSAnimationContext.runAnimationGroup { ctx in
                    ctx.duration = 0.25
                    ctx.allowsImplicitAnimation = true
                    scroll.contentView.animator().setBoundsOrigin(newOrigin)
                    scroll.reflectScrolledClipView(scroll.contentView)
                }
            } else {
                scroll.contentView.setBoundsOrigin(newOrigin)
                scroll.reflectScrolledClipView(scroll.contentView)
            }
        }
    }
}
