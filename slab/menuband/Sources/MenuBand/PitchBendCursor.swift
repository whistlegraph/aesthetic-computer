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
        // Theme off the live system appearance. The overlay image is
        // rebuilt on every move, so this re-reads each frame and even
        // tracks a light/dark flip mid-gesture. Drawing inside the
        // appearance also resolves `controlAccentColor` correctly.
        let appearance = NSApp.effectiveAppearance
        let isDark = appearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        return NSImage(size: size, flipped: false) { rect in
            if #available(macOS 11.0, *) {
                appearance.performAsCurrentDrawingAppearance {
                    drawChart(in: rect, bend: bendC, echo: xC, isDark: isDark)
                }
            } else {
                drawChart(in: rect, bend: bendC, echo: xC, isDark: isDark)
            }
            return true
        }
    }

    private static func drawChart(in rect: NSRect, bend: CGFloat, echo: CGFloat,
                                  isDark: Bool) {
        // Modeled on a Menu Band keycap: a skeuomorphic key plate with a
        // top-lit gradient, engraved axis "grooves" like the gaps between
        // keys, a glossy black-key knob, and a defined keycap outline — so
        // the pad reads as grounded and physical, not a flat light chip.
        let chart = rect.insetBy(dx: 4, dy: 4)
        let radius: CGFloat = 7
        let body = NSBezierPath(roundedRect: chart, xRadius: radius, yRadius: radius)

        // Plate gradient (lighter at top) — cream "white key" in light
        // mode, charcoal key in dark mode.
        let plateTop = isDark
            ? NSColor(white: 0.26, alpha: 1)
            : NSColor(srgbRed: 0.99, green: 0.97, blue: 0.91, alpha: 1)
        let plateBot = isDark
            ? NSColor(white: 0.13, alpha: 1)
            : NSColor(srgbRed: 0.90, green: 0.86, blue: 0.76, alpha: 1)
        NSGraphicsContext.saveGraphicsState()
        body.addClip()
        NSGradient(starting: plateTop, ending: plateBot)?.draw(in: chart, angle: -90)
        NSGraphicsContext.restoreGraphicsState()

        let cx = chart.midX, cy = chart.midY

        // Engraved axis grooves: a dark recess with a light bevel highlight
        // offset just below/right, exactly like the seam between two keys.
        let groove = (isDark ? NSColor.black
                             : NSColor(srgbRed: 0.42, green: 0.36, blue: 0.26, alpha: 1))
            .withAlphaComponent(isDark ? 0.7 : 0.55)
        let bevel = (isDark ? NSColor(white: 0.4, alpha: 1) : NSColor.white)
            .withAlphaComponent(isDark ? 0.5 : 0.8)
        func grooveLine(_ a: NSPoint, _ b: NSPoint) {
            let hi = NSBezierPath()
            hi.move(to: NSPoint(x: a.x + 0.6, y: a.y - 0.6))
            hi.line(to: NSPoint(x: b.x + 0.6, y: b.y - 0.6))
            bevel.setStroke(); hi.lineWidth = 0.8; hi.stroke()
            let p = NSBezierPath(); p.move(to: a); p.line(to: b)
            groove.setStroke(); p.lineWidth = 1.0; p.stroke()
        }
        grooveLine(NSPoint(x: chart.minX + 6, y: cy), NSPoint(x: chart.maxX - 6, y: cy))
        grooveLine(NSPoint(x: cx, y: chart.minY + 6), NSPoint(x: cx, y: chart.maxY - 6))

        // Axis labels: + / − (bend) and « space / » echo, inked to read on
        // the plate.
        let ink = isDark ? NSColor.white
                         : NSColor(srgbRed: 0.30, green: 0.25, blue: 0.16, alpha: 1)
        let labelAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 8, weight: .heavy),
            .foregroundColor: ink.withAlphaComponent(0.55),
        ]
        let plus = NSAttributedString(string: "+", attributes: labelAttrs)
        let minus = NSAttributedString(string: "−", attributes: labelAttrs)
        let echoMark = NSAttributedString(string: "»", attributes: labelAttrs)
        let spaceMark = NSAttributedString(string: "«", attributes: labelAttrs)
        plus.draw(at: NSPoint(x: cx - plus.size().width / 2,
                              y: chart.maxY - plus.size().height - 1))
        minus.draw(at: NSPoint(x: cx - minus.size().width / 2, y: chart.minY + 1))
        echoMark.draw(at: NSPoint(x: chart.maxX - echoMark.size().width - 2,
                                  y: cy - echoMark.size().height / 2))
        spaceMark.draw(at: NSPoint(x: chart.minX + 2,
                                   y: cy - spaceMark.size().height / 2))

        // Knob = a black key: a dark glossy puck (top-lit gradient) with a
        // crisp dark outline. Position carries the live bend/echo; the
        // color stays "black key" regardless so it reads as part of the kit.
        let puckR: CGFloat = 6
        let halfW = chart.width / 2 - puckR - 3
        let halfH = chart.height / 2 - puckR - 3
        let puckRect = NSRect(x: cx + echo * halfW - puckR,
                              y: cy + bend * halfH - puckR,
                              width: puckR * 2, height: puckR * 2)
        let knob = NSBezierPath(roundedRect: puckRect, xRadius: 3, yRadius: 3)
        NSGraphicsContext.saveGraphicsState()
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.black.withAlphaComponent(0.5)
        shadow.shadowOffset = NSSize(width: 0, height: -1)
        shadow.shadowBlurRadius = 2
        shadow.set()
        knob.addClip()
        NSGradient(starting: NSColor(white: 0.28, alpha: 1),
                   ending: NSColor(white: 0.07, alpha: 1))?.draw(in: puckRect, angle: -90)
        NSGraphicsContext.restoreGraphicsState()
        NSColor.black.withAlphaComponent(0.9).setStroke()
        knob.lineWidth = 0.8
        knob.stroke()

        // Keycap outline last so the whole pad is framed like a key.
        let edge = isDark
            ? NSColor.black.withAlphaComponent(0.85)
            : NSColor(srgbRed: 0.34, green: 0.28, blue: 0.18, alpha: 0.85)
        edge.setStroke(); body.lineWidth = 1.3; body.stroke()
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
