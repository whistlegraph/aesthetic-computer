import AppKit

/// Custom mouse cursor used while one or more notes are sounding.
/// Visually a tiny vertical wheel (think pitch-bend lever) so the
/// user gets a hint that single-finger trackpad Y movement is now
/// going to bend the pitch. The wheel **stretches** in the bend
/// direction (bend up → wheel taller above center, ridges shift
/// up; bend down → mirror) so the cursor itself reads the current
/// pitch offset.
enum PitchBendCursor {
    /// Centered, no-bend cursor — used as the push baseline.
    static let neutral: NSCursor = cursor(forBend: 0)

    /// Build a cursor whose internal grip ridges + body extension
    /// reflect a normalized bend amount in [-1, +1]. +1 = full
    /// pitch up (ridges shift up, top of wheel stretched), −1 =
    /// full pitch down (mirror).
    static func cursor(forBend amount: Float) -> NSCursor {
        let bend = max(-1, min(1, CGFloat(amount)))
        let size = NSSize(width: 32, height: 40)
        let image = NSImage(size: size, flipped: false) { rect in
            let center = NSPoint(x: rect.midX, y: rect.midY)
            let bodyW: CGFloat = 12
            // Base height plus an extension biased toward the bend
            // direction. Total height grows with |bend| but the
            // extra mass sits on the side the user is pulling
            // toward, so the wheel reads as "leaning into" the
            // pitch rather than just inflating uniformly.
            let baseH: CGFloat = 20
            let stretch: CGFloat = 8 * abs(bend)
            let bodyH = baseH + stretch
            // Bias the body's vertical center: when bending up
            // (positive), shift the body upward; when down, shift
            // the body downward. The hot spot (center of view)
            // stays constant so cursor placement is stable.
            let bias: CGFloat = bend * 4
            let bodyRect = NSRect(
                x: center.x - bodyW / 2,
                y: center.y - bodyH / 2 + bias,
                width: bodyW,
                height: bodyH
            )

            let shadow = NSShadow()
            shadow.shadowColor = NSColor.black.withAlphaComponent(0.55)
            shadow.shadowOffset = .zero
            shadow.shadowBlurRadius = 2
            NSGraphicsContext.saveGraphicsState()
            shadow.set()

            let bodyPath = NSBezierPath(roundedRect: bodyRect,
                                         xRadius: bodyW / 2,
                                         yRadius: bodyW / 2)
            // Hue tracks bend direction so the wheel reads at a
            // glance: neutral = white; bending up tints toward
            // accent (system color); bending down tints toward
            // a complementary warm tone.
            let accent = NSColor.controlAccentColor
            let warm = NSColor(srgbRed: 1.0, green: 0.45, blue: 0.35, alpha: 1)
            let topColor: NSColor = bend > 0
                ? (NSColor.white.blended(withFraction: bend * 0.75, of: accent) ?? .white)
                : (NSColor.white.blended(withFraction: -bend * 0.45, of: warm) ?? .white)
            let bottomColor: NSColor = bend < 0
                ? (NSColor(white: 0.78, alpha: 1).blended(withFraction: -bend * 0.75, of: warm)
                    ?? NSColor(white: 0.78, alpha: 1))
                : (NSColor(white: 0.78, alpha: 1).blended(withFraction: bend * 0.45, of: accent)
                    ?? NSColor(white: 0.78, alpha: 1))
            let bodyGradient = NSGradient(starting: topColor, ending: bottomColor)
            bodyGradient?.draw(in: bodyPath, angle: -90)
            NSColor.black.withAlphaComponent(0.6).setStroke()
            bodyPath.lineWidth = 0.8
            bodyPath.stroke()

            NSGraphicsContext.restoreGraphicsState()

            // Three horizontal grip ridges. Spacing tightens as
            // the bend grows so the ridges look "compressed" on
            // the side opposite the bend, which sells the lever
            // squishing.
            let ridgeOffsets: [CGFloat] = [-3, 0, 3]
            let ridgeShift = bend * 4
            for offset in ridgeOffsets {
                let path = NSBezierPath()
                let y = center.y + offset + ridgeShift
                path.move(to: NSPoint(x: bodyRect.minX + 2, y: y))
                path.line(to: NSPoint(x: bodyRect.maxX - 2, y: y))
                NSColor.black.withAlphaComponent(0.45).setStroke()
                path.lineWidth = 0.9
                path.stroke()
            }

            // ↕ glyph above the wheel — slides up/down with the
            // bend so the affordance keeps reading even when the
            // wheel itself is heavily distorted.
            let arrows = NSAttributedString(
                string: "↕",
                attributes: [
                    .font: NSFont.systemFont(ofSize: 9, weight: .heavy),
                    .foregroundColor: NSColor.black.withAlphaComponent(0.85),
                ]
            )
            let arrowSize = arrows.size()
            arrows.draw(at: NSPoint(
                x: center.x - arrowSize.width / 2,
                y: bodyRect.maxY + 1
            ))
            return true
        }
        // Hot spot pinned to the static frame center so cursor
        // position is stable as the wheel stretches.
        return NSCursor(image: image, hotSpot: NSPoint(x: 16, y: 20))
    }
}

extension NSCursor {
    /// Convenience to push the neutral pitch-bend cursor onto the
    /// stack. Mirrors the original `PitchBendCursor.shared.push()`
    /// callsite.
    static func pushPitchBend() {
        PitchBendCursor.neutral.push()
    }
}
