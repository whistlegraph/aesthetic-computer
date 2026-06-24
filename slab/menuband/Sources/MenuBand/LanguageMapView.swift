import AppKit

/// Flat-map language picker for the About window — every supported
/// language shown at once as chiclet cells (flag + native label on one
/// line), laid out two-per-row like a compact instrument picker instead
/// of a chip that pops a dropdown. The active language fills solid
/// (accent); the rest are outlined. Click a cell to switch — the host also
/// speaks the language's own name through the Menu Band fx (easter egg).
final class LanguageMapView: NSView {
    struct Item: Equatable {
        let code: String
        let label: String   // native-language name ("Español", "中文")
        let flag: String    // emoji flag
    }

    var items: [Item] = [] {
        didSet { invalidateIntrinsicContentSize(); needsDisplay = true }
    }
    var selectedCode: String = "" { didSet { needsDisplay = true } }
    /// Fires on press / slide-into a cell — speak that language's name only
    /// (instant, no UI rebuild) so the map plays like a sample board.
    var onPlay: ((Item) -> Void)?
    /// Fires on release — commit the actual language switch (the heavier
    /// rebuild) for whichever cell the cursor lifted over.
    var onPick: ((Item) -> Void)?

    /// Two columns; each row is this tall.
    static let cols = 2
    static let rowH: CGFloat = 40

    /// Order used to spread the accent-shade across languages.
    private static let order = ["en", "es", "zh", "ja", "ru", "da"]

    /// Per-language tint — all in the SYSTEM ACCENT hue, just walked a
    /// little in brightness/saturation per language so the grid reads as a
    /// calm, cohesive set rather than a rainbow (or one flat green).
    static func theme(for code: String) -> NSColor {
        let accent = NSColor.controlAccentColor
        guard let hsb = accent.usingColorSpace(.sRGB) else { return accent }
        var h: CGFloat = 0, s: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
        hsb.getHue(&h, saturation: &s, brightness: &b, alpha: &a)
        let idx = order.firstIndex(of: code) ?? 0
        let t = CGFloat(idx) / CGFloat(max(1, order.count - 1)) - 0.5  // −0.5…+0.5
        let b2 = max(0.20, min(1.0, b + t * 0.20))
        let s2 = max(0.20, min(1.0, s - t * 0.12))
        return NSColor(hue: h, saturation: s2, brightness: b2, alpha: a)
    }

    private var hovered: Int?
    /// Index currently held down (mouse button down over it) — drawn as a
    /// "played" cell so the buttons read as playable keys.
    private var pressed: Int?
    private var trackingArea: NSTrackingArea?

    override var isFlipped: Bool { true }   // top-down rows, reading order

    private var rowCount: Int { (items.count + Self.cols - 1) / Self.cols }

    override var intrinsicContentSize: NSSize {
        NSSize(width: NSView.noIntrinsicMetric,
               height: CGFloat(max(1, rowCount)) * Self.rowH)
    }

    /// An odd final item spans the full width (centered) instead of
    /// leaving an empty right column.
    private func isLoneLast(_ i: Int) -> Bool {
        items.count % Self.cols == 1 && i == items.count - 1
    }

    private func cellRect(_ i: Int) -> NSRect {
        let row = i / Self.cols
        let cw = bounds.width / CGFloat(Self.cols)
        if isLoneLast(i) {
            return NSRect(x: 0, y: CGFloat(row) * Self.rowH,
                          width: bounds.width, height: Self.rowH)
        }
        let col = i % Self.cols
        return NSRect(x: CGFloat(col) * cw, y: CGFloat(row) * Self.rowH,
                      width: cw, height: Self.rowH)
    }

    private func index(at point: NSPoint) -> Int? {
        guard !items.isEmpty, bounds.contains(point) else { return nil }
        let row = Int(point.y / Self.rowH)
        // The lone final row maps to the last item across its whole width.
        if items.count % Self.cols == 1, row == rowCount - 1 {
            return items.count - 1
        }
        let cw = bounds.width / CGFloat(Self.cols)
        let col = min(Self.cols - 1, Int(point.x / cw))
        let i = row * Self.cols + col
        return (i >= 0 && i < items.count) ? i : nil
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let ta = trackingArea { removeTrackingArea(ta) }
        let ta = NSTrackingArea(
            rect: bounds,
            options: [.mouseEnteredAndExited, .mouseMoved, .activeAlways, .inVisibleRect],
            owner: self, userInfo: nil)
        addTrackingArea(ta)
        trackingArea = ta
    }

    override func mouseMoved(with event: NSEvent) {
        let i = index(at: convert(event.locationInWindow, from: nil))
        if i != hovered { hovered = i; needsDisplay = true }
    }
    override func mouseExited(with event: NSEvent) {
        if hovered != nil { hovered = nil; needsDisplay = true }
    }
    // Sample-board feel: fire on mouse-DOWN (instant — the spoken name starts
    // the moment you press, so holding hears the whole word), and re-fire as
    // you DRAG across cells so the language names can be "played" like pads.
    // Release just clears the lit state. Re-triggering is cheap — the host
    // speaks immediately and skips the heavy rebuild when the code is unchanged.
    override func mouseDown(with event: NSEvent) {
        let i = index(at: convert(event.locationInWindow, from: nil))
        pressed = i
        needsDisplay = true
        if let i = i { onPlay?(items[i]) }   // instant sound, no rebuild
    }
    override func mouseDragged(with event: NSEvent) {
        let i = index(at: convert(event.locationInWindow, from: nil))
        if i != pressed {
            pressed = i
            needsDisplay = true
            if let i = i { onPlay?(items[i]) }   // slide to play the next name
        }
    }
    override func mouseUp(with event: NSEvent) {
        let i = index(at: convert(event.locationInWindow, from: nil))
        pressed = nil
        needsDisplay = true
        if let i = i { onPick?(items[i]) }   // commit the switch on release
    }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        for (i, item) in items.enumerated() {
            let r = cellRect(i)
            guard r.intersects(dirtyRect) else { continue }
            let accent = Self.theme(for: item.code)
            let isSelected = item.code == selectedCode
            let isPressed = pressed == i
            let isHovered = hovered == i

            // Pressed cells "depress" a hair (extra inset) so the down
            // state reads as a played key, not just a color change.
            let inset: CGFloat = isPressed ? 3.5 : 2
            let cap = NSBezierPath(roundedRect: r.insetBy(dx: inset, dy: inset),
                                   xRadius: 5, yRadius: 5)

            // Fill ramps with engagement: idle → hover → played, and the
            // active language sits bright by default. Pressing any cell
            // lights it like it's sounding.
            let fillAlpha: CGFloat
            let textWhite: Bool
            if isPressed {
                fillAlpha = 1.0; textWhite = true
            } else if isSelected {
                fillAlpha = isHovered ? 0.95 : 0.85; textWhite = true
            } else if isHovered {
                fillAlpha = 0.30; textWhite = false
            } else {
                fillAlpha = 0.10; textWhite = false
            }
            accent.withAlphaComponent(fillAlpha).setFill(); cap.fill()
            accent.withAlphaComponent(isSelected || isPressed ? 1.0
                                      : (isHovered ? 0.8 : 0.5)).setStroke()
            cap.lineWidth = (isSelected || isPressed) ? 1.6 : (isHovered ? 1.2 : 0.8)
            cap.stroke()

            // Flag then label on one line — larger type for legibility.
            let labelColor: NSColor = textWhite ? .white : .labelColor
            let line = NSMutableAttributedString(
                string: "\(item.flag) ",
                attributes: [.font: NSFont.systemFont(ofSize: 18)])
            line.append(NSAttributedString(
                string: item.label,
                attributes: [
                    .font: NSFont.systemFont(ofSize: 13, weight: .semibold),
                    .foregroundColor: labelColor.withAlphaComponent(textWhite ? 1.0 : 0.9),
                ]))
            let sz = line.size()
            line.draw(at: NSPoint(x: r.midX - sz.width / 2,
                                  y: r.midY - sz.height / 2))
        }
    }
}
