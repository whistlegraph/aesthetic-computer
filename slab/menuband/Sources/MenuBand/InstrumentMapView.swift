import AppKit

/// Two-column sectioned browser for the 128 General MIDI programs. Left
/// column carries the first 8 GM families (Piano … Brass), right column
/// carries the last 8 (Reed … Sound FX). Each family has a colored
/// header + 8 instrument rows beneath it. Click a row → onCommit; the
/// popover hands that to the controller, which sets the program and
/// auditions middle-C through the local synth so the user hears their
/// pick.
///
/// No hover audio. Hover only nudges the row's background slightly so
/// the cursor's resting target reads at a glance.
final class InstrumentListView: NSView {
    static let rowHeight: CGFloat = 18
    static let headerHeight: CGFloat = 22
    static let stripeWidth: CGFloat = 4
    static let sectionPadding: CGFloat = 8

    static let columnWidth: CGFloat = 220
    static let columnGap: CGFloat = 8
    static let totalColumns = 2
    static let familiesPerColumn = 8       // 16 families / 2 columns
    static let programsPerFamily = 8

    static let sectionHeight: CGFloat =
        headerHeight + CGFloat(programsPerFamily) * rowHeight + sectionPadding

    static let preferredWidth: CGFloat =
        CGFloat(totalColumns) * columnWidth + CGFloat(totalColumns - 1) * columnGap

    static let intrinsicHeight: CGFloat = CGFloat(familiesPerColumn) * sectionHeight

    var selectedProgram: UInt8 = 0 { didSet { needsDisplay = true } }
    private(set) var hoveredProgram: UInt8?

    /// Only `onCommit` is exposed — the design dropped hover audio, so we
    /// don't even surface a hover callback. The view tracks hover for the
    /// background highlight only.
    var onCommit: ((Int) -> Void)?

    private var trackingArea: NSTrackingArea?

    /// Rows top-down (y=0 → topmost row), matching reading order.
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

    // MARK: - Layout helpers

    /// Origin x of the given column (0 or 1).
    private func columnX(_ col: Int) -> CGFloat {
        CGFloat(col) * (Self.columnWidth + Self.columnGap)
    }

    /// Origin y (top-down) of family index `i` within its column.
    private func sectionY(_ familyInColumn: Int) -> CGFloat {
        CGFloat(familyInColumn) * Self.sectionHeight
    }

    private func rowRect(programInFamily: Int, familyInColumn: Int, column: Int) -> NSRect {
        let x = columnX(column) + Self.stripeWidth
        let y = sectionY(familyInColumn) + Self.headerHeight + CGFloat(programInFamily) * Self.rowHeight
        return NSRect(x: x, y: y,
                      width: Self.columnWidth - Self.stripeWidth,
                      height: Self.rowHeight)
    }

    private func headerRect(familyInColumn: Int, column: Int) -> NSRect {
        NSRect(x: columnX(column),
               y: sectionY(familyInColumn),
               width: Self.columnWidth,
               height: Self.headerHeight)
    }

    private func stripeRect(familyInColumn: Int, column: Int) -> NSRect {
        NSRect(x: columnX(column),
               y: sectionY(familyInColumn) + Self.headerHeight,
               width: Self.stripeWidth,
               height: CGFloat(Self.programsPerFamily) * Self.rowHeight)
    }

    /// Given a point in the view, return the program (0..127) under it,
    /// or nil if the point falls in a header / column gap / padding area.
    private func program(at point: NSPoint) -> Int? {
        // Column.
        let stride = Self.columnWidth + Self.columnGap
        let col = Int(point.x / stride)
        guard col >= 0, col < Self.totalColumns else { return nil }
        let xInColumn = point.x - CGFloat(col) * stride
        guard xInColumn >= 0, xInColumn <= Self.columnWidth else { return nil }

        // Family within column.
        let familyInCol = Int(point.y / Self.sectionHeight)
        guard familyInCol >= 0, familyInCol < Self.familiesPerColumn else { return nil }
        let yInSection = point.y - CGFloat(familyInCol) * Self.sectionHeight

        // Skip headers + bottom padding.
        if yInSection < Self.headerHeight { return nil }
        let yInBody = yInSection - Self.headerHeight
        let progInFam = Int(yInBody / Self.rowHeight)
        guard progInFam >= 0, progInFam < Self.programsPerFamily else { return nil }

        let familyAbs = col * Self.familiesPerColumn + familyInCol
        return familyAbs * Self.programsPerFamily + progInFam
    }

    // MARK: - Colors

    private func familyColor(_ familyAbs: Int) -> NSColor {
        NSColor(hue: CGFloat(familyAbs) / CGFloat(GeneralMIDI.families.count),
                saturation: 0.55, brightness: 0.85, alpha: 1.0)
    }

    // MARK: - Drawing

    private static let headerAttrs: [NSAttributedString.Key: Any] = [
        .font: NSFont.systemFont(ofSize: 10, weight: .bold),
        .foregroundColor: NSColor.labelColor,
        .kern: 0.6,
    ]
    private static let numberAttrs: [NSAttributedString.Key: Any] = [
        .font: NSFont.monospacedDigitSystemFont(ofSize: 10, weight: .regular),
        .foregroundColor: NSColor.tertiaryLabelColor,
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

        for col in 0..<Self.totalColumns {
            for familyInCol in 0..<Self.familiesPerColumn {
                let familyAbs = col * Self.familiesPerColumn + familyInCol
                guard familyAbs < GeneralMIDI.families.count else { continue }
                let (familyName, range) = GeneralMIDI.families[familyAbs]
                let color = familyColor(familyAbs)

                // Header.
                let hRect = headerRect(familyInColumn: familyInCol, column: col)
                if hRect.intersects(dirtyRect) {
                    color.withAlphaComponent(0.18).setFill()
                    NSBezierPath(rect: hRect).fill()
                    let titleStr = NSString(string: familyName.uppercased())
                    titleStr.draw(at: NSPoint(x: hRect.minX + 8, y: hRect.minY + 5),
                                  withAttributes: Self.headerAttrs)
                }

                // Family color stripe spanning the body.
                let sRect = stripeRect(familyInColumn: familyInCol, column: col)
                if sRect.intersects(dirtyRect) {
                    color.setFill()
                    NSBezierPath(rect: sRect).fill()
                }

                // Programs.
                for progInFam in 0..<Self.programsPerFamily {
                    let p = range.lowerBound + progInFam
                    guard p < GeneralMIDI.programNames.count else { continue }
                    let r = rowRect(programInFamily: progInFam,
                                    familyInColumn: familyInCol,
                                    column: col)
                    guard r.intersects(dirtyRect) else { continue }

                    if hoveredProgram == UInt8(p) {
                        NSColor.controlAccentColor.withAlphaComponent(0.20).setFill()
                        NSBezierPath(rect: r).fill()
                    } else if selectedProgram == UInt8(p) {
                        NSColor.controlAccentColor.withAlphaComponent(0.12).setFill()
                        NSBezierPath(rect: r).fill()
                    } else if progInFam % 2 == 0 {
                        NSColor.labelColor.withAlphaComponent(0.025).setFill()
                        NSBezierPath(rect: r).fill()
                    }

                    // Program number — three monospace digits, dim.
                    let num = NSString(format: "%03d", p)
                    num.draw(at: NSPoint(x: r.minX + 4, y: r.minY + 3),
                             withAttributes: Self.numberAttrs)

                    // Name — truncated to fit column width.
                    let nameAttrs = (selectedProgram == UInt8(p))
                        ? Self.nameAttrsSelected
                        : Self.nameAttrs
                    let name = GeneralMIDI.programNames[p]
                    let nameOriginX = r.minX + 32
                    let nameMaxWidth = r.maxX - nameOriginX - 4
                    drawTruncated(name, at: NSPoint(x: nameOriginX, y: r.minY + 2),
                                  maxWidth: nameMaxWidth, attrs: nameAttrs)
                }
            }
        }
    }

    private func drawTruncated(_ text: String, at point: NSPoint,
                                maxWidth: CGFloat,
                                attrs: [NSAttributedString.Key: Any]) {
        var s = text
        var attr = NSAttributedString(string: s, attributes: attrs)
        if attr.size().width <= maxWidth {
            attr.draw(at: point)
            return
        }
        // Tail-truncate. Strip until it fits, then append ellipsis.
        let ellipsis = "…"
        while !s.isEmpty {
            s.removeLast()
            attr = NSAttributedString(string: s + ellipsis, attributes: attrs)
            if attr.size().width <= maxWidth { break }
        }
        attr.draw(at: point)
    }

    // MARK: - Mouse

    override func mouseEntered(with event: NSEvent) {
        updateHover(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseMoved(with event: NSEvent) {
        updateHover(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseExited(with event: NSEvent) {
        if hoveredProgram != nil {
            hoveredProgram = nil
            needsDisplay = true
        }
    }

    private func updateHover(at point: NSPoint) {
        let p = program(at: point).map { UInt8($0) }
        if p != hoveredProgram {
            hoveredProgram = p
            needsDisplay = true
        }
    }

    override func mouseDown(with event: NSEvent) {
        let pt = convert(event.locationInWindow, from: nil)
        let p = program(at: pt)
        debugLog("InstrumentListView.mouseDown pt=(\(pt.x),\(pt.y)) program=\(String(describing: p))")
        if let p = p {
            onCommit?(p)
        }
    }

    /// Scroll so the row for `program` is visible.
    func scrollProgramIntoView(_ program: UInt8, animated: Bool = false) {
        let prog = Int(program)
        let familyAbs = prog / Self.programsPerFamily
        let progInFam = prog % Self.programsPerFamily
        let col = familyAbs / Self.familiesPerColumn
        let familyInCol = familyAbs % Self.familiesPerColumn
        let r = rowRect(programInFamily: progInFam,
                        familyInColumn: familyInCol,
                        column: col)
        guard let scroll = enclosingScrollView else { return }
        let visible = scroll.documentVisibleRect
        if !visible.contains(r) {
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
