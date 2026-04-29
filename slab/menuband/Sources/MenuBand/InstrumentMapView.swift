import AppKit

/// Flat 8 × 16 grid showing all 128 General MIDI programs at once. Rows
/// correspond to the 16 GM families; columns are the 8 programs in each
/// family. Hover plays a held preview note in that program (silent when
/// MIDI mode is on); click commits.
///
/// Used inside the popover instead of an NSPopUpButton so the user can
/// scan the whole instrument set at a glance and "skim" instruments by
/// dragging the cursor across the map.
final class InstrumentMapView: NSView {
    static let cellSize: CGFloat = 14
    static let cellGap: CGFloat = 1
    static let cols = 8
    static let rows = 16  // 16 GM families

    static let intrinsicSize = NSSize(
        width:  cellSize * CGFloat(cols) + cellGap * CGFloat(cols - 1),
        height: cellSize * CGFloat(rows) + cellGap * CGFloat(rows - 1)
    )

    var selectedProgram: UInt8 = 0 { didSet { needsDisplay = true } }
    private(set) var hoveredProgram: UInt8?

    /// Fires whenever the hovered cell changes (including transitions
    /// to/from "no hover" → nil). Drives the program preview note.
    var onHover: ((Int?) -> Void)?
    /// Click-commit. The popover should write this through to the
    /// controller's `setMelodicProgram(_:)`.
    var onCommit: ((Int) -> Void)?

    private var trackingArea: NSTrackingArea?

    override var intrinsicContentSize: NSSize { Self.intrinsicSize }

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

    private func cellRect(program: Int) -> NSRect {
        let row = program / Self.cols
        let col = program % Self.cols
        let stride = Self.cellSize + Self.cellGap
        let x = CGFloat(col) * stride
        // Top-down rows in a bottom-left NSView coord space.
        let y = bounds.height - (CGFloat(row) * stride + Self.cellSize)
        return NSRect(x: x, y: y, width: Self.cellSize, height: Self.cellSize)
    }

    private func program(at point: NSPoint) -> Int? {
        let stride = Self.cellSize + Self.cellGap
        let col = Int(point.x / stride)
        let yFromTop = bounds.height - point.y
        let row = Int(yFromTop / stride)
        guard col >= 0, col < Self.cols, row >= 0, row < Self.rows else { return nil }
        return row * Self.cols + col
    }

    /// Family index (0…15) for a program. Each family is 8 programs.
    private func familyIndex(forProgram p: Int) -> Int { p / 8 }

    /// Soft pastel hue per family — 16 hues evenly distributed around the
    /// color wheel at low saturation so they read as distinct categories
    /// without clashing.
    private func familyColor(_ idx: Int) -> NSColor {
        NSColor(hue: CGFloat(idx) / CGFloat(Self.rows),
                saturation: 0.42, brightness: 0.88, alpha: 1.0)
    }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        for p in 0..<128 {
            let r = cellRect(program: p)
            let famIdx = familyIndex(forProgram: p)
            var color = familyColor(famIdx)
            // Brightness ramp within family: lighter at left (program 0
            // of family) → darker at right (program 7) so each row reads
            // as a gradient and the user can spot the variant they want.
            let progInFam = p % 8
            let darken = CGFloat(progInFam) * 0.038
            color = color.blended(withFraction: darken, of: .black) ?? color

            color.setFill()
            NSBezierPath(rect: r).fill()

            if hoveredProgram == UInt8(p) {
                NSColor.controlAccentColor.withAlphaComponent(0.45).setFill()
                NSBezierPath(rect: r).fill()
            }

            if selectedProgram == UInt8(p) {
                NSColor.controlAccentColor.setStroke()
                let path = NSBezierPath(rect: r.insetBy(dx: 0.75, dy: 0.75))
                path.lineWidth = 1.5
                path.stroke()
            }
        }
    }

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
            onHover?(nil)
        }
    }

    private func updateHover(at point: NSPoint) {
        let p = program(at: point).map { UInt8($0) }
        if p != hoveredProgram {
            hoveredProgram = p
            needsDisplay = true
            onHover?(p.map { Int($0) })
        }
    }

    override func mouseDown(with event: NSEvent) {
        if let p = program(at: convert(event.locationInWindow, from: nil)) {
            onCommit?(p)
        }
    }
}
