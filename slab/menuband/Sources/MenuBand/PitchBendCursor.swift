import AppKit

/// Custom cursor replacement used while the trackpad bend gesture
/// is engaged. It renders as a small XY modulation pad — frozen at
/// the lock point — with a puck that slides up/down to show the
/// current pitch-bend and right to show echo amount. The puck IS
/// the live state visualisation; the chart itself never moves, so
/// the user has a stable reference frame to read both axes
/// against as the audio rubber-bands back to neutral on release.
enum PitchBendCursor {
    /// Centered, no-bend, no-echo cursor — used as the push baseline
    /// fallback for in-app cursorUpdate handlers (the actual
    /// visual live one is the floating overlay window).
    static let neutral: NSCursor = cursor(forBend: 0, echo: 0)

    /// Hot-spot at the chart's center so the overlay window anchors
    /// the chart directly over the user's frozen cursor position.
    static let hotSpot = NSPoint(x: 40, y: 40)
    static let cursorSize = NSSize(width: 80, height: 80)

    static func image(forBend amount: Float) -> NSImage {
        buildImage(bend: CGFloat(amount), echo: 0)
    }

    static func image(forBend bend: Float, echo: Float) -> NSImage {
        buildImage(bend: CGFloat(bend), echo: CGFloat(echo))
    }

    static func cursor(forBend amount: Float) -> NSCursor {
        cursor(forBend: amount, echo: 0)
    }

    static func cursor(forBend bend: Float, echo: Float) -> NSCursor {
        NSCursor(image: image(forBend: bend, echo: echo), hotSpot: hotSpot)
    }

    private static func buildImage(bend: CGFloat, echo: CGFloat) -> NSImage {
        let bendC = max(-1, min(1, bend))
        // `echo` is the bipolar fx-X driver in [-1, +1]: positive
        // (right) is echo, negative (left) is space/reverb. We keep
        // the parameter name `echo` for source-call stability; the
        // chart treats it as a signed X value.
        let xC = max(-1, min(1, echo))
        let size = cursorSize
        return NSImage(size: size, flipped: false) { rect in
            drawChart(in: rect, bend: bendC, echo: xC)
            return true
        }
    }

    private static func drawChart(in rect: NSRect, bend: CGFloat, echo: CGFloat) {
        // Inset so the rounded background doesn't clip on the
        // cursor canvas edge.
        let chart = rect.insetBy(dx: 4, dy: 4)
        let bg = NSBezierPath(roundedRect: chart, xRadius: 6, yRadius: 6)
        // Semi-transparent dark plate so the puck and labels read
        // against any app background. Adapts implicitly with the
        // system appearance via the system accent reference.
        NSColor.black.withAlphaComponent(0.5).setFill()
        bg.fill()
        NSColor.white.withAlphaComponent(0.45).setStroke()
        bg.lineWidth = 0.8
        bg.stroke()

        let cx = chart.midX
        let cy = chart.midY

        // Faint center crosshair — gives the puck a "zero" reference
        // so the user can see when they've returned to neutral on
        // either axis independently.
        let cross = NSBezierPath()
        cross.move(to: NSPoint(x: chart.minX + 6, y: cy))
        cross.line(to: NSPoint(x: chart.maxX - 6, y: cy))
        cross.move(to: NSPoint(x: cx, y: chart.minY + 6))
        cross.line(to: NSPoint(x: cx, y: chart.maxY - 6))
        NSColor.white.withAlphaComponent(0.22).setStroke()
        cross.lineWidth = 0.5
        cross.stroke()

        // Axis labels: + / − for bend, « space (left) / » echo
        // (right). Small, faint.
        let labelAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 8, weight: .heavy),
            .foregroundColor: NSColor.white.withAlphaComponent(0.6),
        ]
        let plus = NSAttributedString(string: "+", attributes: labelAttrs)
        let minus = NSAttributedString(string: "−", attributes: labelAttrs)
        let echoMark = NSAttributedString(string: "»", attributes: labelAttrs)
        let spaceMark = NSAttributedString(string: "«", attributes: labelAttrs)
        plus.draw(at: NSPoint(x: cx - plus.size().width / 2,
                              y: chart.maxY - plus.size().height - 1))
        minus.draw(at: NSPoint(x: cx - minus.size().width / 2,
                               y: chart.minY + 1))
        echoMark.draw(at: NSPoint(x: chart.maxX - echoMark.size().width - 2,
                                  y: cy - echoMark.size().height / 2))
        spaceMark.draw(at: NSPoint(x: chart.minX + 2,
                                   y: cy - spaceMark.size().height / 2))

        // Puck position. Y axis uses the full [-1, +1] range of bend;
        // X axis uses [0, +1] of echo (puck starts at center and
        // rides right). Inset by puck radius so the puck stays
        // inside the chart at extremes.
        let puckR: CGFloat = 5.5
        let halfW = chart.width / 2 - puckR - 3
        let halfH = chart.height / 2 - puckR - 3
        let puckX = cx + echo * halfW
        let puckY = cy + bend * halfH
        let puckRect = NSRect(x: puckX - puckR, y: puckY - puckR,
                              width: puckR * 2, height: puckR * 2)

        // Hue tracks bend direction so the puck reads at a glance.
        let accent = NSColor.controlAccentColor
        let warm = NSColor(srgbRed: 1.0, green: 0.45, blue: 0.35, alpha: 1)
        let puckColor: NSColor
        if bend > 0 {
            puckColor = NSColor.white.blended(withFraction: bend * 0.7, of: accent) ?? .white
        } else if bend < 0 {
            puckColor = NSColor.white.blended(withFraction: -bend * 0.7, of: warm) ?? .white
        } else {
            puckColor = .white
        }

        // Soft drop shadow so the puck pops off the chart plate.
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.black.withAlphaComponent(0.65)
        shadow.shadowOffset = .zero
        shadow.shadowBlurRadius = 3
        NSGraphicsContext.saveGraphicsState()
        shadow.set()
        let puckPath = NSBezierPath(ovalIn: puckRect)
        puckColor.setFill()
        puckPath.fill()
        NSGraphicsContext.restoreGraphicsState()
        NSColor.black.withAlphaComponent(0.55).setStroke()
        puckPath.lineWidth = 0.6
        puckPath.stroke()
    }
}

/// Borderless transparent panel that draws the XY pad at the
/// cursor's locked screen position. Floats above every app so the
/// chart stays visible regardless of which window the mouse is
/// over — pair with `CGDisplayHideCursor` to hide the real system
/// cursor so the chart visibly replaces it.
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
    /// chart lands centered on the hot spot at that point.
    func show(image: NSImage, atScreenPoint screenPoint: NSPoint) {
        imageView.image = image
        let hot = PitchBendCursor.hotSpot
        let origin = NSPoint(x: screenPoint.x - hot.x,
                             y: screenPoint.y - hot.y)
        setFrameOrigin(origin)
        alphaValue = 1
        if !isVisible { orderFrontRegardless() }
    }

    /// Update only the chart image (puck position changes); window
    /// position stays put. The chart is intentionally frozen at the
    /// lock point — only the internal puck moves.
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
