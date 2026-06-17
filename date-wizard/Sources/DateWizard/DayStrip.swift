// DayStrip.swift — shared drawing for the seven-day ROYGBIV dot strip,
// used by both the menu bar (MenuBarDays, rendered into an NSImage) and the
// in-window navigator (DayDotsView, drawn live). One routine so the two
// always match: same dots, same rings, same accent glow.
import AppKit

enum DayStrip {
    static let gap: CGFloat = 4
    static let padX: CGFloat = 3
    static let padY: CGFloat = 2

    static func diameter(forHeight h: CGFloat) -> CGFloat { max(12, h - padY * 2) }

    /// Total width the seven dots want at a given height (menu bar sizing).
    static func contentWidth(forHeight h: CGFloat) -> CGFloat {
        padX * 2 + 7 * diameter(forHeight: h) + 6 * gap
    }

    /// Frame of dot `i`, centered within `bounds` (non-flipped, y-up).
    static func dotRect(_ i: Int, in bounds: NSRect) -> NSRect {
        let d = diameter(forHeight: bounds.height)
        let contentW = 7 * d + 6 * gap
        let startX = bounds.minX + max(padX, (bounds.width - contentW) / 2)
        return NSRect(x: startX + CGFloat(i) * (d + gap),
                      y: bounds.minY + (bounds.height - d) / 2,
                      width: d, height: d)
    }

    /// Day index (0…6) nearest a click x within `bounds`.
    static func index(atX x: CGFloat, in bounds: NSRect) -> Int {
        var best = 0, bestDist = CGFloat.greatestFiniteMagnitude
        for i in 0..<7 {
            let dist = abs(dotRect(i, in: bounds).midX - x)
            if dist < bestDist { bestDist = dist; best = i }
        }
        return best
    }

    /// Draw the strip into the current graphics context.
    /// - selected: the focused day (bright + white ring) — drives the theme.
    /// - today:    marked with a thin ring when it isn't the focused day.
    /// - hovered:  lit under the cursor.
    static func draw(in bounds: NSRect, selected: Int?, today: Int?, hovered: Int?) {
        let active = selected ?? today
        for i in 0..<7 {
            let rect = dotRect(i, in: bounds)
            let isActive = (i == active)
            let isHovered = (i == hovered)
            let isToday = (i == today)
            let lit = isActive || isHovered
            // More subtle palette: ease the lit colors back a touch and dim
            // the rest further so the strip reads quietly in the menu bar.
            let base = DayPalette.colors[i].withAlphaComponent(lit ? 0.85 : 0.22)

            // Fill, with a soft dark drop shadow.
            NSGraphicsContext.saveGraphicsState()
            let shadow = NSShadow()
            shadow.shadowColor = NSColor.black.withAlphaComponent(lit ? 0.45 : 0.3)
            shadow.shadowBlurRadius = lit ? 2.0 : 1.5
            shadow.shadowOffset = NSSize(width: 0, height: -1)
            shadow.set()
            base.setFill()
            NSBezierPath(ovalIn: rect).fill()
            NSGraphicsContext.restoreGraphicsState()

            // Rings (no shadow), all thin: bright for the focused day, fainter
            // for hover, and a faint "today" marker when today isn't focused.
            if isActive {
                NSColor.white.setStroke()
                let ring = NSBezierPath(ovalIn: rect.insetBy(dx: 0.7, dy: 0.7))
                ring.lineWidth = 1.0
                ring.stroke()
            } else if isHovered {
                NSColor.white.withAlphaComponent(0.85).setStroke()
                let ring = NSBezierPath(ovalIn: rect.insetBy(dx: 0.7, dy: 0.7))
                ring.lineWidth = 0.75
                ring.stroke()
            }
            if isToday && !isActive {
                NSColor.white.withAlphaComponent(0.6).setStroke()
                let ring = NSBezierPath(ovalIn: rect.insetBy(dx: 0.7, dy: 0.7))
                ring.lineWidth = 0.75
                ring.stroke()
            }

            // Letter.
            let letter = DayPalette.letters[i] as NSString
            let fg = lit ? DayPalette.contrastColor(on: base) : NSColor.white.withAlphaComponent(0.6)
            let fontSize = max(9, rect.height * 0.55)
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: fontSize, weight: lit ? .bold : .semibold),
                .foregroundColor: fg,
            ]
            let sz = letter.size(withAttributes: attrs)
            letter.draw(at: NSPoint(x: rect.midX - sz.width / 2,
                                    y: rect.midY - sz.height / 2),
                        withAttributes: attrs)
        }
    }
}
