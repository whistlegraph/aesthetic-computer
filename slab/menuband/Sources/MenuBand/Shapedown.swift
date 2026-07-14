import AppKit
import CoreGraphics

// Shapedown — double-tap LEFT ⌘ and the whole screen turns into a glass
// wall you draw on with the trackpad. The trackpad maps 1:1 onto the display,
// so every finger is a vertex somewhere on screen, and the *set* of fingers
// is one filled, colored shape:
//
//     1 finger  → a dot
//     2 fingers → a line
//     3 fingers → a triangle
//     4 fingers → a quad
//     N fingers → an N-gon (as many contacts as the trackpad reports)
//
// Hold a shape still for a beat and it "sets" (it pulses to tell you). Lift
// your fingers and the set shape stamps onto the wall, sustains, then fades.
// Each stamp takes the next hue in a rotating palette, so a run of shapes
// fans out into a spectrum. Double-tap ⌘ again (or press Escape) to leave.
//
// Input is the same focus-independent MultitouchSupport tap the pitch-bend fx
// pad uses (`MultitouchTrackpad`) — NSTouch never reaches a non-activating
// menubar panel, so the private framework is the only thing that sees every
// finger regardless of what's frontmost. Because that path is App-Store
// forbidden, the whole feature is gated out of the MAS build alongside it.
#if !MAC_APP_STORE

final class Shapedown {
    private var overlay: ShapedownOverlayPanel?
    private var canvas: ShapedownCanvas?

    /// Global modifier/key monitor. flagsChanged carries the ⌘ taps; keyDown
    /// carries Escape while the wall is up. Global monitors can't consume, but
    /// we never want to — ⌘ still means ⌘ to everyone else.
    private var monitor: Any?
    /// Time of the last CLEAN left-⌘ tap (⌘ pressed and released with no other
    /// key or modifier touched in between). Two of these inside the window are
    /// the gesture.
    private var lastCleanTap: TimeInterval = 0
    /// True from a lone left-⌘ press until either it's released cleanly or
    /// something else is pressed during the hold, which dirties it — so
    /// ⌘C, ⌘V, ⌘⇧…, ⌘Tab, etc. never count toward the double-tap.
    private var commandClean = false
    /// Balances CGDisplayHideCursor/ShowCursor (they nest a hide count, so an
    /// unmatched show would leak the pointer back mid-draw).
    private var cursorHidden = false
    /// Swallows scroll + trackpad gestures WHILE the wall is up so fingers only
    /// ever draw shapes — no scroll, pinch-zoom, rotate, or swipe underneath.
    private let gestureTap = ShapedownGestureTap()

    /// Two left-⌘ presses closer than this count as the double-tap gesture.
    private static let doubleTapWindow: TimeInterval = 0.4

    /// True while the wall is showing — AppDelegate checks this to route
    /// trackpad frames here instead of into the pitch-bend fx pad.
    var isActive: Bool { overlay?.isVisible == true }

    /// Arm the global ⌘ listener. Idempotent. Needs Accessibility (same grant
    /// TYPE mode already asks for); silently inert until it's given.
    func start() {
        guard monitor == nil else { return }
        monitor = NSEvent.addGlobalMonitorForEvents(
            matching: [.flagsChanged, .keyDown]
        ) { [weak self] event in
            self?.handle(event)
        }
    }

    private func handle(_ event: NSEvent) {
        let mods = event.modifierFlags.intersection(.deviceIndependentFlagsMask)
        switch event.type {
        case .flagsChanged:
            if event.keyCode == 55 {                   // left ⌘ itself
                if mods.contains(.command) {
                    // Pressed. A clean tap can only start from a LONE ⌘ —
                    // if ⇧/⌥/⌃ are already down, it's a chord, not a tap.
                    commandClean = (mods == .command)
                } else {
                    // Released. Two clean taps inside the window = the gesture.
                    if commandClean { registerCleanTap() }
                    commandClean = false
                }
            } else if mods.contains(.command) {
                // Another modifier toggled while ⌘ is held → dirty the hold.
                commandClean = false
            }
        case .keyDown:
            // Any REAL key struck while ⌘ is held (⌘C, ⌘Tab, …) dirties the
            // hold. Exclude the ⌘ keycodes themselves — a modifier's own keycode
            // can arrive as a synthesized keyDown and must not cancel the run.
            let isCommandKey = event.keyCode == 55 || event.keyCode == 54
            if mods.contains(.command), !isCommandKey { commandClean = false }
            if event.keyCode == 53, isActive {         // Escape closes the wall
                DispatchQueue.main.async { [weak self] in self?.hide() }
            }
        default:
            break
        }
    }

    private func registerCleanTap() {
        let now = ProcessInfo.processInfo.systemUptime
        if now - lastCleanTap < Self.doubleTapWindow {
            lastCleanTap = 0
            DispatchQueue.main.async { [weak self] in self?.toggle() }
        } else {
            lastCleanTap = now
        }
    }

    private func toggle() { isActive ? hide() : show() }

    private func show() {
        guard overlay == nil else { return }
        guard let screen = NSScreen.main ?? NSScreen.screens.first else { return }

        let canvas = ShapedownCanvas(frame: NSRect(origin: .zero, size: screen.frame.size))
        canvas.autoresizingMask = [.width, .height]

        let panel = ShapedownOverlayPanel(
            contentRect: screen.frame,
            styleMask: [.borderless],
            backing: .buffered,
            defer: false
        )
        panel.level = .screenSaver                 // above menubar + Dock
        panel.backgroundColor = .clear
        panel.isOpaque = false
        panel.hasShadow = false
        panel.animationBehavior = .none
        panel.isMovable = false
        panel.isReleasedWhenClosed = false
        panel.hidesOnDeactivate = false
        panel.ignoresMouseEvents = true            // input is the trackpad, not the pointer
        panel.collectionBehavior = [
            .canJoinAllSpaces, .fullScreenAuxiliary, .stationary, .ignoresCycle,
        ]
        panel.contentView = canvas
        panel.alphaValue = 0

        self.overlay = panel
        self.canvas = canvas
        // Hide the pointer for the whole session. CGDisplayHideCursor is
        // focus-independent (NSCursor.hide only works while frontmost + over
        // our window), which a non-activating click-through panel never is.
        if !cursorHidden {
            CGDisplayHideCursor(CGMainDisplayID())
            cursorHidden = true
        }
        _ = gestureTap.start()          // block system gestures for the session
        panel.orderFrontRegardless()
        NSAnimationContext.runAnimationGroup { ctx in
            ctx.duration = 0.16
            panel.animator().alphaValue = 1
        }
    }

    private func hide() {
        guard let panel = overlay else { return }
        overlay = nil
        canvas?.stop()
        canvas = nil
        if cursorHidden {
            CGDisplayShowCursor(CGMainDisplayID())
            cursorHidden = false
        }
        gestureTap.stop()               // gestures return to the system
        NSAnimationContext.runAnimationGroup({ ctx in
            ctx.duration = 0.2
            panel.animator().alphaValue = 0
        }, completionHandler: {
            panel.orderOut(nil)
        })
    }

    /// Feed the current fingers (normalized 0…1, origin bottom-left) to the
    /// wall. Called from AppDelegate's trackpad frame handler while active.
    func ingest(_ touches: [CGPoint]) {
        canvas?.ingest(touches)
    }
}

/// A CGEventTap, live ONLY while the wall is up, that consumes scroll and the
/// whole NSEvent gesture family (magnify / rotate / swipe / smart-magnify /
/// pressure) so a finger on the trackpad can't scroll, zoom, rotate, or swipe
/// the desktop out from under the drawing. Runs on its own run-loop thread,
/// same shape as `KeyEventTap`.
///
/// One honest limit: Mission Control and Spaces swipes (3–4 finger) are acted
/// on by the WindowServer ahead of a session-level tap, so those particular
/// swipes can still fire. Everything delivered as an ordinary gesture/scroll
/// event is blocked here. Needs Accessibility; without it `start()` returns
/// false and the wall simply runs without gesture suppression.
final class ShapedownGestureTap {
    private var tap: CFMachPort?
    private var source: CFRunLoopSource?
    private var thread: Thread?
    private var runLoop: CFRunLoop?

    @discardableResult
    func start() -> Bool {
        guard tap == nil else { return true }
        // Raw event-type numbers the tap sees: 22 = scrollWheel, and the
        // NSEvent gesture family 18/19/20 (rotate/begin/end), 29 gesture,
        // 30 magnify, 31 swipe, 32 smartMagnify, 33 quickLook, 34 pressure.
        let types: [UInt32] = [18, 19, 20, 22, 29, 30, 31, 32, 33, 34]
        var mask: CGEventMask = 0
        for t in types { mask |= (CGEventMask(1) << CGEventMask(t)) }

        let opaque = Unmanaged.passRetained(self).toOpaque()
        let callback: CGEventTapCallBack = { _, type, event, refcon in
            guard let refcon else { return Unmanaged.passUnretained(event) }
            let me = Unmanaged<ShapedownGestureTap>.fromOpaque(refcon).takeUnretainedValue()
            if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
                if let tap = me.tap { CGEvent.tapEnable(tap: tap, enable: true) }
                return Unmanaged.passUnretained(event)
            }
            return nil                  // consume — the wall owns the trackpad
        }

        guard let tap = CGEvent.tapCreate(
            tap: .cgSessionEventTap,
            place: .headInsertEventTap,
            options: .defaultTap,
            eventsOfInterest: mask,
            callback: callback,
            userInfo: opaque
        ) else {
            Unmanaged<ShapedownGestureTap>.fromOpaque(opaque).release()
            return false
        }
        self.tap = tap
        source = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, tap, 0)

        let thread = Thread { [weak self] in
            guard let self, let source = self.source, let tap = self.tap else { return }
            self.runLoop = CFRunLoopGetCurrent()
            CFRunLoopAddSource(CFRunLoopGetCurrent(), source, .commonModes)
            CGEvent.tapEnable(tap: tap, enable: true)
            CFRunLoopRun()
        }
        thread.qualityOfService = .userInteractive
        thread.name = "Shapedown-GestureTap"
        thread.start()
        self.thread = thread
        return true
    }

    func stop() {
        if let tap { CGEvent.tapEnable(tap: tap, enable: false) }
        if let runLoop { CFRunLoopStop(runLoop) }
        if tap != nil { Unmanaged.passUnretained(self).release() }
        tap = nil
        source = nil
        thread = nil
        runLoop = nil
    }

    deinit { stop() }
}

/// Borderless glass wall. Non-key so it never steals focus from whatever the
/// user is looking at — the wall is a heads-up display, not an app to click.
final class ShapedownOverlayPanel: NSPanel {
    override var canBecomeKey: Bool { false }
    override var canBecomeMain: Bool { false }
}

/// The drawing surface: turns finger sets into filled shapes, holds a steady
/// shape until it "sets", then stamps + fades it. Bottom-left origin so the
/// MultitouchSupport normalized space (also bottom-left) maps straight across
/// without a Y flip.
final class ShapedownCanvas: NSView {
    /// A committed shape, sustaining then fading on the wall.
    private struct Stamp {
        var points: [CGPoint]     // view space
        var hue: CGFloat
        var born: TimeInterval
    }

    private var live: [CGPoint] = []          // fingers right now, view space
    private var stamps: [Stamp] = []
    private var stampCount = 0                 // ever — drives hue cycling

    // Hold-to-set: a shape "sets" once its centroid has sat within `stillSlop`
    // for `holdThreshold`. armedPoints is the frozen shape we'll stamp on lift.
    private var heldSince: TimeInterval = 0
    private var heldCentroid: CGPoint = .zero
    private var heldCount = 0
    private var armed = false
    private var armedPoints: [CGPoint] = []

    private var timer: Timer?

    // Tuning.
    private let holdThreshold: TimeInterval = 0.4     // stillness before a shape sets
    private let stillSlop: CGFloat = 22               // px of centroid drift still counts as "held"
    private let sustain: TimeInterval = 1.6           // full-strength hold after stamping
    private let fadeDur: TimeInterval = 2.6           // fade to gone

    override var isFlipped: Bool { false }
    override var wantsDefaultClipping: Bool { false }

    private var now: TimeInterval { ProcessInfo.processInfo.systemUptime }

    /// Golden-ratio hue walk so consecutive stamps are always far apart in color.
    private func hue(for index: Int) -> CGFloat {
        CGFloat((0.08 + Double(index) * 0.6180339887).truncatingRemainder(dividingBy: 1))
    }

    private func centroid(_ pts: [CGPoint]) -> CGPoint {
        var sx: CGFloat = 0, sy: CGFloat = 0
        for p in pts { sx += p.x; sy += p.y }
        let n = CGFloat(max(1, pts.count))
        return CGPoint(x: sx / n, y: sy / n)
    }

    func ingest(_ normalized: [CGPoint]) {
        let w = bounds.width, h = bounds.height
        let pts = normalized.map { CGPoint(x: $0.x * w, y: $0.y * h) }
        let t = now

        if pts.isEmpty {
            // All fingers lifted — stamp the shape iff it had set.
            if armed, !armedPoints.isEmpty {
                stamps.append(Stamp(points: armedPoints, hue: hue(for: stampCount), born: t))
                stampCount += 1
            }
            live = []
            armed = false
            armedPoints = []
            heldSince = 0
            heldCount = 0
        } else {
            live = pts
            let c = centroid(pts)
            let stillHolding = pts.count == heldCount && hypot(c.x - heldCentroid.x, c.y - heldCentroid.y) < stillSlop
            if stillHolding {
                heldCentroid = c                        // track slowly with the hand
                if t - heldSince >= holdThreshold {
                    armed = true
                    armedPoints = pts                   // freeze the latest steady pose
                }
            } else {
                heldCentroid = c
                heldCount = pts.count
                heldSince = t
                armed = false
            }
        }
        ensureTimer()
        needsDisplay = true
    }

    /// ~60fps while anything is on screen (live fingers or fading stamps);
    /// stops itself once the wall is empty so it costs nothing at rest.
    private func ensureTimer() {
        guard timer == nil else { return }
        timer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in
            guard let self else { return }
            self.needsDisplay = true
            if self.live.isEmpty && self.stamps.isEmpty {
                self.timer?.invalidate()
                self.timer = nil
            }
        }
    }

    func stop() {
        timer?.invalidate()
        timer = nil
        live = []
        stamps = []
    }

    override func draw(_ dirtyRect: NSRect) {
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        let t = now

        // Faint dim so bright shapes read against a busy desktop.
        NSColor(white: 0, alpha: 0.16).setFill()
        bounds.fill()

        // Stamps: oldest first, sustaining then fading.
        stamps.removeAll { t - $0.born > sustain + fadeDur }
        for s in stamps {
            let age = t - s.born
            let a: CGFloat = age <= sustain ? 1 : CGFloat(1 - (age - sustain) / fadeDur)
            drawShape(s.points, hue: s.hue, alpha: max(0, a) * 0.9, glow: true, ctx: ctx)
        }

        // The shape under your fingers, on top. Pulses once it has set.
        if !live.isEmpty {
            let h = hue(for: stampCount)
            let alpha: CGFloat = armed ? (0.7 + 0.3 * CGFloat(abs(sin(t * 6)))) : 0.55
            drawShape(live, hue: h, alpha: alpha, glow: armed, ctx: ctx)
            for p in live { drawVertex(p, hue: h, ctx: ctx) }
        }
    }

    /// Sort a point set by angle around its centroid so any N points close
    /// into a clean, non-self-intersecting polygon.
    private func hull(_ pts: [CGPoint]) -> [CGPoint] {
        let c = centroid(pts)
        return pts.sorted { atan2($0.y - c.y, $0.x - c.x) < atan2($1.y - c.y, $1.x - c.x) }
    }

    private func drawShape(_ points: [CGPoint], hue: CGFloat, alpha: CGFloat, glow: Bool, ctx: CGContext) {
        guard !points.isEmpty, alpha > 0.001 else { return }
        let fill = NSColor(hue: hue, saturation: 0.85, brightness: 1, alpha: alpha)

        ctx.saveGState()
        if glow {
            ctx.setShadow(offset: .zero, blur: 24,
                          color: NSColor(hue: hue, saturation: 0.9, brightness: 1, alpha: alpha).cgColor)
        }

        switch points.count {
        case 1:
            let p = points[0], r: CGFloat = 44
            fill.setFill()
            ctx.fillEllipse(in: CGRect(x: p.x - r, y: p.y - r, width: r * 2, height: r * 2))
        case 2:
            let path = NSBezierPath()
            path.lineWidth = 26
            path.lineCapStyle = .round
            path.move(to: points[0])
            path.line(to: points[1])
            fill.setStroke()
            path.stroke()
        default:
            let poly = hull(points)
            let path = NSBezierPath()
            path.move(to: poly[0])
            for p in poly.dropFirst() { path.line(to: p) }
            path.close()
            fill.setFill()
            path.fill()
            NSColor(hue: hue, saturation: 0.4, brightness: 1, alpha: alpha).setStroke()
            path.lineWidth = 3
            path.stroke()
        }
        ctx.restoreGState()
    }

    /// A little white core at every fingertip so the vertices are always
    /// legible even inside a big filled shape.
    private func drawVertex(_ p: CGPoint, hue: CGFloat, ctx: CGContext) {
        let r: CGFloat = 7
        NSColor(white: 1, alpha: 0.9).setFill()
        ctx.fillEllipse(in: CGRect(x: p.x - r, y: p.y - r, width: r * 2, height: r * 2))
    }
}

#endif
