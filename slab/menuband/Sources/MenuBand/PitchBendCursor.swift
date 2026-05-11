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

    /// Hot-spot used by both `cursor(forBend:)` and the floating
    /// overlay window — keeps the bend wheel anchored over the
    /// frozen cursor position when CGAssociateMouseAndMouseCursorPosition
    /// detaches the system cursor.
    static let hotSpot = NSPoint(x: 16, y: 20)
    static let cursorSize = NSSize(width: 32, height: 40)

    /// Same bitmap the `cursor(forBend:)` factory uses, exposed
    /// so a floating overlay window can draw the wheel itself
    /// while the system cursor is hidden via CGDisplayHideCursor.
    /// That hide-and-draw approach is what kills the cross-app
    /// cursor flicker — other apps' cursorUpdate handlers
    /// can't fight us if there's no system cursor for them to
    /// reset.
    static func image(forBend amount: Float) -> NSImage {
        return buildImage(forBend: amount)
    }

    /// Build a cursor whose internal grip ridges + body extension
    /// reflect a normalized bend amount in [-1, +1]. +1 = full
    /// pitch up (ridges shift up, top of wheel stretched), −1 =
    /// full pitch down (mirror).
    static func cursor(forBend amount: Float) -> NSCursor {
        let image = buildImage(forBend: amount)
        return NSCursor(image: image, hotSpot: hotSpot)
    }

    private static func buildImage(forBend amount: Float) -> NSImage {
        let bend = max(-1, min(1, CGFloat(amount)))
        let size = cursorSize
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
        return image
    }
}

/// Borderless transparent panel that draws the pitch-bend wheel
/// at the cursor's locked screen position. Floats above every
/// app so the wheel stays visible regardless of which window the
/// mouse is over — pair with `CGDisplayHideCursor` to hide the
/// real system cursor and you get a flicker-free custom cursor
/// that doesn't fight other apps' cursorUpdate handlers.
final class PitchBendCursorOverlayWindow: NSPanel {
    private let imageView = NSImageView()

    init() {
        let frame = NSRect(origin: .zero, size: PitchBendCursor.cursorSize)
        super.init(contentRect: frame,
                   styleMask: [.borderless, .nonactivatingPanel],
                   backing: .buffered,
                   defer: false)
        isOpaque = false
        backgroundColor = .clear
        hasShadow = false
        // Float above every other window in every space, never
        // steal focus, never accept mouse clicks (so the trackpad
        // gesture still routes to whatever app is below).
        level = .screenSaver
        ignoresMouseEvents = true
        hidesOnDeactivate = false
        collectionBehavior = [.canJoinAllSpaces,
                              .stationary,
                              .ignoresCycle,
                              .fullScreenAuxiliary]
        imageView.frame = frame
        imageView.imageScaling = .scaleNone
        contentView?.addSubview(imageView)
    }

    /// `screenPoint` is the absolute screen position the cursor
    /// is currently locked at. The panel is repositioned so the
    /// wheel image lands centered on the hot spot at that point.
    func show(image: NSImage, atScreenPoint screenPoint: NSPoint) {
        imageView.image = image
        let hot = PitchBendCursor.hotSpot
        let origin = NSPoint(x: screenPoint.x - hot.x,
                             y: screenPoint.y - hot.y)
        setFrameOrigin(origin)
        if !isVisible { orderFrontRegardless() }
    }

    /// Update only the wheel image; position stays put. Used by
    /// the bend-amount changes and the rubber-band decay tick.
    func update(image: NSImage) {
        imageView.image = image
    }

    func dismiss() {
        orderOut(nil)
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
