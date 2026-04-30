import AppKit

/// MacBook-style arrow-keys cluster, drawn as four little keycaps
/// in the classic inverted-T arrangement: ↑ alone on top row, then
/// ← ↓ → in a row below. Each keycap can light up independently —
/// pressed direction fills with the accent color, the rest stay
/// white-outlined.
///
/// Drawn entirely with NSBezierPath so the keys read as physical
/// chiclet shapes (rounded squares + centered arrow glyph), not
/// abstract chevrons.
///
/// Direction indices match `InstrumentMapView.onArrowKey`:
///   0 = ←   1 = →   2 = ↓   3 = ↑
final class ArrowKeysIndicator: NSView {
    private var pressed: Set<Int> = []

    static let intrinsicSize = NSSize(width: 46, height: 30)
    override var intrinsicContentSize: NSSize { Self.intrinsicSize }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setFrameSize(Self.intrinsicSize)
    }
    required init?(coder: NSCoder) { fatalError() }

    /// Light up (or dim) one of the four direction keycaps. Multiple
    /// directions can be lit at once.
    func setHighlight(direction: Int, on: Bool) {
        if on { pressed.insert(direction) }
        else  { pressed.remove(direction) }
        needsDisplay = true
    }

    override var isFlipped: Bool { false }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        let r = bounds
        // Modern MacBook arrow ratios: all four keys are the same
        // full-size square keycap, arranged in an inverted-T. Up
        // sits alone on top, ◀ ▼ ▶ in a row below. 1 px gap between
        // caps.
        let key: CGFloat = 13
        let gap: CGFloat = 1
        let radius: CGFloat = 2.5
        let centerX = r.midX
        let bottomY = r.minY + 1                         // bottom row baseline
        let topY    = bottomY + key + gap                // top row sits above
        // Bottom row: left, down, right (centered around centerX).
        let downRect  = NSRect(x: centerX - key / 2, y: bottomY, width: key, height: key)
        let leftRect  = NSRect(x: downRect.minX - key - gap, y: bottomY, width: key, height: key)
        let rightRect = NSRect(x: downRect.maxX + gap,        y: bottomY, width: key, height: key)
        // Top row: up alone, full-size, centered over down.
        let upRect    = NSRect(x: downRect.minX, y: topY, width: key, height: key)

        let glyphs = ["←", "→", "↓", "↑"]
        let rects  = [leftRect, rightRect, downRect, upRect]
        for (idx, kr) in rects.enumerated() {
            let lit = pressed.contains(idx)
            // Keycap.
            let path = NSBezierPath(roundedRect: kr, xRadius: radius, yRadius: radius)
            if lit {
                NSColor.controlAccentColor.withAlphaComponent(0.85).setFill()
                path.fill()
            }
            (lit ? NSColor.controlAccentColor : NSColor.labelColor.withAlphaComponent(0.55))
                .setStroke()
            path.lineWidth = 0.8
            path.stroke()
            // Arrow glyph centered in the keycap.
            let glyphColor: NSColor = lit ? .black : NSColor.labelColor
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 10, weight: .heavy),
                .foregroundColor: glyphColor,
            ]
            let s = NSAttributedString(string: glyphs[idx], attributes: attrs)
            let size = s.size()
            // Slight y-offset to optically center the unicode arrow
            // (their natural baseline puts them a bit high in the box).
            s.draw(at: NSPoint(x: kr.midX - size.width / 2,
                               y: kr.midY - size.height / 2 - 0.5))
        }
    }
}
