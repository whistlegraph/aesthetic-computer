import AppKit

/// MacBook-style arrow-keys cluster, drawn as four little keycaps
/// in the classic inverted-T arrangement: ↑ alone on top row, then
/// ← ↓ → in a row below. Doubles as a clickable D-pad — mousing
/// over a cap lights it up (rollover), pressing fires the same
/// action a real arrow keystroke would.
///
/// Direction indices match `InstrumentMapView.onArrowKey`:
///   0 = ←   1 = →   2 = ↓   3 = ↑
final class ArrowKeysIndicator: NSView {
    private var pressed: Set<Int> = []
    private var hovered: Int?
    private var trackingArea: NSTrackingArea?

    /// Fires when one of the keycaps is clicked. Same semantics as a
    /// physical arrow keystroke — `isDown` is true on mouseDown,
    /// false on mouseUp. The popover wires this to the same path the
    /// keyboard arrows drive (preview note while held, commit on
    /// release).
    var onClick: ((Int, Bool) -> Void)?

    static let intrinsicSize = NSSize(width: 46, height: 30)
    override var intrinsicContentSize: NSSize { Self.intrinsicSize }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setFrameSize(Self.intrinsicSize)
    }
    required init?(coder: NSCoder) { fatalError() }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let ta = trackingArea { removeTrackingArea(ta) }
        let ta = NSTrackingArea(rect: bounds,
                                 options: [.mouseEnteredAndExited, .mouseMoved,
                                           .activeAlways, .inVisibleRect],
                                 owner: self, userInfo: nil)
        addTrackingArea(ta)
        trackingArea = ta
    }

    /// Pressed-state setter — used by the keyboard path so on-screen
    /// keycaps light up while a real arrow key is held.
    func setHighlight(direction: Int, on: Bool) {
        if on { pressed.insert(direction) }
        else  { pressed.remove(direction) }
        needsDisplay = true
    }

    override var isFlipped: Bool { false }

    // MARK: - Geometry

    /// Compute the four keycap rects in the same layout as `draw`.
    /// Returned in direction index order: 0=←, 1=→, 2=↓, 3=↑.
    private func keyRects() -> [NSRect] {
        let r = bounds
        let key: CGFloat = 13
        let gap: CGFloat = 1
        let centerX = r.midX
        let bottomY = r.minY + 1
        let topY    = bottomY + key + gap
        let downRect  = NSRect(x: centerX - key / 2, y: bottomY, width: key, height: key)
        let leftRect  = NSRect(x: downRect.minX - key - gap, y: bottomY, width: key, height: key)
        let rightRect = NSRect(x: downRect.maxX + gap,        y: bottomY, width: key, height: key)
        let upRect    = NSRect(x: downRect.minX, y: topY, width: key, height: key)
        return [leftRect, rightRect, downRect, upRect]
    }

    private func direction(at point: NSPoint) -> Int? {
        for (idx, kr) in keyRects().enumerated() where kr.contains(point) {
            return idx
        }
        return nil
    }

    // MARK: - Mouse

    override func mouseEntered(with event: NSEvent) {
        updateHover(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseMoved(with event: NSEvent) {
        updateHover(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseExited(with event: NSEvent) {
        if hovered != nil { hovered = nil; needsDisplay = true }
    }
    private func updateHover(at point: NSPoint) {
        let d = direction(at: point)
        if d != hovered { hovered = d; needsDisplay = true }
    }

    override func mouseDown(with event: NSEvent) {
        let pt = convert(event.locationInWindow, from: nil)
        guard let dir = direction(at: pt) else { return }
        pressed.insert(dir)
        needsDisplay = true
        onClick?(dir, true)
    }
    override func mouseUp(with event: NSEvent) {
        // Fire keyUp for every pressed cap so we can't get stuck if
        // the cursor leaves the cap during the drag.
        for dir in pressed {
            onClick?(dir, false)
        }
        pressed.removeAll()
        needsDisplay = true
    }

    // MARK: - Drawing

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        let radius: CGFloat = 2.5
        let glyphs = ["←", "→", "↓", "↑"]
        for (idx, kr) in keyRects().enumerated() {
            let lit = pressed.contains(idx)
            let isHover = (!lit && hovered == idx)
            let path = NSBezierPath(roundedRect: kr, xRadius: radius, yRadius: radius)
            if lit {
                NSColor.controlAccentColor.withAlphaComponent(0.85).setFill()
                path.fill()
            } else if isHover {
                // Rollover wash — soft accent tint behind the glyph,
                // no fill on the rest of the cap so it reads as a
                // "ready to click" state, not "selected".
                NSColor.controlAccentColor.withAlphaComponent(0.18).setFill()
                path.fill()
            }
            let stroke: NSColor = lit
                ? NSColor.controlAccentColor
                : isHover
                    ? NSColor.controlAccentColor.withAlphaComponent(0.75)
                    : NSColor.labelColor.withAlphaComponent(0.55)
            stroke.setStroke()
            path.lineWidth = 0.8
            path.stroke()
            let glyphColor: NSColor = lit
                ? .black
                : (isHover ? .controlAccentColor : NSColor.labelColor)
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 10, weight: .heavy),
                .foregroundColor: glyphColor,
            ]
            let s = NSAttributedString(string: glyphs[idx], attributes: attrs)
            let size = s.size()
            s.draw(at: NSPoint(x: kr.midX - size.width / 2,
                               y: kr.midY - size.height / 2 - 0.5))
        }
    }
}
