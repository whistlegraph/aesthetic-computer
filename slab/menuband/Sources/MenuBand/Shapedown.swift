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

    // MARK: Feedback cues — the same full-screen flash and bell/click the
    // right-⌘ focus gesture uses, each independently switchable in Settings.
    private static let flashKey = "ShapedownFlash"
    private static let soundKey = "ShapedownSound"
    /// Both default ON (absent key == on), like Haptics.
    static var flashesEnabled: Bool {
        get { UserDefaults.standard.object(forKey: flashKey) == nil
                ? true : UserDefaults.standard.bool(forKey: flashKey) }
        set { UserDefaults.standard.set(newValue, forKey: flashKey) }
    }
    static var soundsEnabled: Bool {
        get { UserDefaults.standard.object(forKey: soundKey) == nil
                ? true : UserDefaults.standard.bool(forKey: soundKey) }
        set { UserDefaults.standard.set(newValue, forKey: soundKey) }
    }
    private func flashCue(rising: Bool) {
        if Self.flashesEnabled { FocusFlashOverlay.shared.flash(rising: rising) }
    }
    private func bellCue(rising: Bool) {
        if Self.soundsEnabled { FocusCueBeep.shared.play(rising: rising) }
    }
    private func clickCue() {
        if Self.soundsEnabled { FocusCueBeep.shared.click() }
    }

    /// Physical trackpad click (pushing the pad in) → pin the current shape so
    /// it stays put until the wall closes, with a click + flash to confirm.
    private func stampPermanently() {
        guard isActive else { return }
        canvas?.pinCurrent()
        clickCue()
        flashCue(rising: true)
    }

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

    /// Close the wall without cues — used when the ⌘⌘↩ record gesture starts
    /// and a left-⌘⌘ had just opened it as a side effect.
    func dismissIfActive() { if isActive { hide(cues: false) } }

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
        // Hide the pointer for the whole session, from a BACKGROUND app.
        // CGDisplayHideCursor alone is ignored unless the caller is frontmost,
        // which this menubar panel never is — so first flip the private
        // `SetsCursorInBackground` connection property, which lifts that
        // restriction. Then decouple the mouse so a single finger stops
        // dragging the (now hidden) pointer. All three are undone in hide().
        if !cursorHidden {
            Self.setBackgroundCursorHiding(true)
            CGAssociateMouseAndMouseCursorPosition(0)
            CGDisplayHideCursor(CGMainDisplayID())
            cursorHidden = true
        }
        gestureTap.onClick = { [weak self] in self?.stampPermanently() }
        _ = gestureTap.start()          // block system gestures + catch clicks
        panel.orderFrontRegardless()
        NSAnimationContext.runAnimationGroup { ctx in
            ctx.duration = 0.16
            panel.animator().alphaValue = 1
        }
        flashCue(rising: true)          // "wall on" cue, matching the ⌘ gesture
        bellCue(rising: true)
    }

    private func hide(cues: Bool = true) {
        guard let panel = overlay else { return }
        if cues {
            flashCue(rising: false)     // "wall off" cue
            bellCue(rising: false)
        }
        overlay = nil
        // Cursor + gestures return to the system immediately.
        if cursorHidden {
            CGAssociateMouseAndMouseCursorPosition(1)   // mouse drives the cursor again
            CGDisplayShowCursor(CGMainDisplayID())
            Self.setBackgroundCursorHiding(false)
            cursorHidden = false
        }
        gestureTap.stop()               // gestures return to the system
        // Bloom every committed shape out together, and fade the wall over the
        // same beat, so the whole thing leaves as one gesture.
        let dyingCanvas = canvas
        canvas = nil
        let dur = dyingCanvas?.beginClose() ?? 0.2
        NSAnimationContext.runAnimationGroup({ ctx in
            ctx.duration = max(0.2, dur)
            panel.animator().alphaValue = 0
        }, completionHandler: {
            dyingCanvas?.stop()
            panel.orderOut(nil)
        })
    }

    /// Feed the current fingers (normalized 0…1, origin bottom-left) to the
    /// wall. Called from AppDelegate's trackpad frame handler while active.
    func ingest(_ touches: [CGPoint]) {
        canvas?.ingest(touches)
    }

    /// Toggle the private CGS `SetsCursorInBackground` connection property so
    /// CGDisplayHideCursor works from a non-frontmost app. The two symbols live
    /// in the already-loaded CoreGraphics image; we dlsym them (RTLD_DEFAULT)
    /// rather than link, so a future SDK that drops them just no-ops instead of
    /// failing to build. Private API — fine here (this whole file is out of the
    /// MAS build alongside the MultitouchSupport tap).
    private static func setBackgroundCursorHiding(_ on: Bool) {
        typealias MainConnFn = @convention(c) () -> Int32
        typealias SetPropFn = @convention(c) (Int32, Int32, CFString, CFTypeRef) -> Int32
        let dflt = UnsafeMutableRawPointer(bitPattern: -2)   // RTLD_DEFAULT
        guard let connSym = dlsym(dflt, "CGSMainConnectionID"),
              let setSym = dlsym(dflt, "CGSSetConnectionProperty") else { return }
        let mainConn = unsafeBitCast(connSym, to: MainConnFn.self)
        let setProp = unsafeBitCast(setSym, to: SetPropFn.self)
        let cid = mainConn()
        _ = setProp(cid, cid, "SetsCursorInBackground" as CFString,
                    (on ? kCFBooleanTrue : kCFBooleanFalse))
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

    /// Fired (on main) when the trackpad is physically clicked in — the wall
    /// uses it to pin the current shape permanently.
    var onClick: (() -> Void)?

    @discardableResult
    func start() -> Bool {
        guard tap == nil else { return true }
        // Raw event-type numbers the tap sees: 1/2 left mouse down/up (the
        // physical trackpad click), 3/4 right, 25/26 other; 22 = scrollWheel;
        // and the NSEvent gesture family 18/19/20 (rotate/begin/end), 29
        // gesture, 30 magnify, 31 swipe, 32 smartMagnify, 33 quickLook, 34
        // pressure. All consumed so the wall owns the trackpad completely.
        let types: [UInt32] = [1, 2, 3, 4, 18, 19, 20, 22, 25, 26, 29, 30, 31, 32, 33, 34]
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
            if type == .leftMouseDown {
                DispatchQueue.main.async { me.onClick?() }
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
    /// A committed shape, held crisp on the wall until close.
    private struct Stamp {
        var points: [CGPoint]     // view space
        var hue: CGFloat
        var born: TimeInterval    // for the pop-in entrance
    }
    /// A transient fade — a lifted sketch evaporating, or a committed shape
    /// blooming away as the wall closes. One mechanism for every fade.
    private struct Ghost {
        var points: [CGPoint]
        var hue: CGFloat
        var born: TimeInterval
        var dur: TimeInterval
        var bloom: CGFloat        // outward swell across the fade (0 = none)
    }

    private var live: [CGPoint] = []          // fingers right now, view space
    private var pinned: [Stamp] = []          // committed shapes — held until close
    private var ghosts: [Ghost] = []          // transient fades (evaporate / close bloom)
    private var recent: [(t: TimeInterval, pts: [CGPoint])] = []  // short frame history
    private var stampCount = 0                 // the pen-color index; advances per commit
    private var committedThisTouch = false     // don't evaporate a touch that got committed
    private var closing = false

    private var timer: Timer?

    // Tuning.
    private let evaporateDur: TimeInterval = 0.5      // lift-without-commit: quick dissolve
    private let evaporateBloom: CGFloat = 0.06        // barely swells — it just goes
    private let popDur: TimeInterval = 0.22           // commit entrance settle
    private let closeBloomDur: TimeInterval = 0.75    // all commits bloom out together on close
    private let closeBloom: CGFloat = 0.28
    private let nodeRadius: CGFloat = 13               // ONE thickness: dot / line / corner-wrap
    private let commitLookback: TimeInterval = 0.25   // peak-finger window a click commits from

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
        guard !closing else { return }
        let w = bounds.width, h = bounds.height
        let pts = normalized.map { CGPoint(x: $0.x * w, y: $0.y * h) }

        if pts.isEmpty {
            // Fingers lifted. If this touch was never committed with a click,
            // the sketch just evaporates — using the peak shape so a press that
            // collapsed the last frame to one finger doesn't shrink it.
            if !live.isEmpty, !committedThisTouch {
                ghosts.append(Ghost(points: committedShape(), hue: hue(for: stampCount),
                                    born: now, dur: evaporateDur, bloom: evaporateBloom))
            }
            live = []
            recent.removeAll()
            committedThisTouch = false
        } else {
            if live.isEmpty { committedThisTouch = false }   // a fresh touch begins
            live = pts
            let t = now
            recent.append((t, pts))
            recent.removeAll { t - $0.t > commitLookback }
        }
        ensureTimer()
        needsDisplay = true
    }

    /// The shape a click should commit: the frame with the MOST fingers seen in
    /// the last `commitLookback` (ties → most recent). Pressing the pad to click
    /// briefly collapses the contacts to the one finger doing the pressing, so
    /// sampling `live` at the click instant loses the line/tri/quad; the peak
    /// over the recent window recovers what the hand was actually holding.
    private func committedShape() -> [CGPoint] {
        let cutoff = now - commitLookback
        let best = recent.filter { $0.t >= cutoff }
            .max { $0.pts.count < $1.pts.count }?.pts
        return (best?.isEmpty == false) ? best! : live
    }

    /// ~60fps while anything is animating — fingers down, a fade in flight, a
    /// commit still popping, or the closing bloom. Idles to nothing at rest
    /// (committed shapes are static, so they just stay drawn).
    private func ensureTimer() {
        guard timer == nil else { return }
        timer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in
            guard let self else { return }
            self.needsDisplay = true
            let popping = self.pinned.contains { self.now - $0.born < self.popDur }
            if self.live.isEmpty, self.ghosts.isEmpty, !self.closing, !popping {
                self.timer?.invalidate()
                self.timer = nil
            }
        }
    }

    /// Commit the shape currently under the fingers (physical trackpad click):
    /// it pops in crisp and stays until the wall closes. Advances the pen color.
    func pinCurrent() {
        let shape = committedShape()
        guard !shape.isEmpty else { return }
        pinned.append(Stamp(points: shape, hue: hue(for: stampCount), born: now))
        stampCount += 1
        committedThisTouch = true
        needsDisplay = true
    }

    /// Begin the closing exit: turn every committed shape into a bloom-fade so
    /// they all leave together. Returns the duration so the overlay can match
    /// its own fade to it. Further input is ignored once closing.
    @discardableResult
    func beginClose() -> TimeInterval {
        closing = true
        let t = now
        for p in pinned {
            ghosts.append(Ghost(points: p.points, hue: p.hue, born: t,
                                dur: closeBloomDur, bloom: closeBloom))
        }
        pinned = []
        live = []
        recent.removeAll()
        ensureTimer()
        needsDisplay = true
        return closeBloomDur
    }

    func stop() {
        timer?.invalidate()
        timer = nil
        live = []
        pinned = []
        ghosts = []
        recent.removeAll()
        closing = false
    }

    override func draw(_ dirtyRect: NSRect) {
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        let t = now

        // Faint dim so bright shapes read against a busy desktop.
        NSColor(white: 0, alpha: 0.16).setFill()
        bounds.fill()

        // Transient fades: evaporating sketches and the closing bloom, one path.
        ghosts.removeAll { t - $0.born > $0.dur }
        for g in ghosts {
            let f = CGFloat(min(max((t - g.born) / g.dur, 0), 1))
            let eased = f * f * (3 - 2 * f)                   // smoothstep
            let alpha = (1 - eased) * 0.9
            let scale = 1 + g.bloom * eased
            let pts = scale == 1 ? g.points : scaled(g.points, by: scale)
            drawShape(pts, hue: g.hue, alpha: alpha, glow: true, ctx: ctx)
        }

        // Committed shapes: crisp and permanent, with a little pop as they land.
        for p in pinned {
            let age = t - p.born
            let alpha: CGFloat
            let scale: CGFloat
            if age < popDur {
                let f = CGFloat(age / popDur)
                alpha = f
                scale = 1 + 0.10 * (1 - f)
            } else {
                alpha = 1
                scale = 1
            }
            let pts = scale == 1 ? p.points : scaled(p.points, by: scale)
            drawShape(pts, hue: p.hue, alpha: alpha * 0.95, glow: true, ctx: ctx)
        }

        // The wet sketch under your fingers, on top — dim, and in the NEXT pen
        // color, so it reads as provisional until you click to commit it.
        if !live.isEmpty {
            drawShape(live, hue: hue(for: stampCount), alpha: 0.5, glow: false, ctx: ctx)
        }
    }

    /// Sort a point set by angle around its centroid so any N points close
    /// into a clean, non-self-intersecting polygon.
    private func hull(_ pts: [CGPoint]) -> [CGPoint] {
        let c = centroid(pts)
        return pts.sorted { atan2($0.y - c.y, $0.x - c.x) < atan2($1.y - c.y, $1.x - c.x) }
    }

    /// Scale a point set out from its own centroid — used for the fade bloom.
    private func scaled(_ pts: [CGPoint], by k: CGFloat) -> [CGPoint] {
        let c = centroid(pts)
        return pts.map { CGPoint(x: c.x + ($0.x - c.x) * k, y: c.y + ($0.y - c.y) * k) }
    }

    private func drawShape(_ points: [CGPoint], hue: CGFloat, alpha: CGFloat, glow: Bool, ctx: CGContext) {
        guard !points.isEmpty, alpha > 0.001 else { return }
        // Composite the whole shape as ONE group: draw fill + stroke fully
        // OPAQUE inside a transparency layer, then let the layer composite at
        // `alpha`. Opaque-over-opaque unions to flat coverage, so the stroke
        // and fill can't double-blend into a darker seam — the bug that showed
        // as a doubled outline at low alpha. Glow + alpha apply to the layer
        // as a whole, so the glow is drawn once around the union, not per-op.
        let color = NSColor(hue: hue, saturation: 0.85, brightness: 1, alpha: 1)
        let r = nodeRadius

        ctx.saveGState()
        if glow {
            ctx.setShadow(offset: .zero, blur: 24,
                          color: NSColor(hue: hue, saturation: 0.9, brightness: 1, alpha: 1).cgColor)
        }
        ctx.setAlpha(alpha)
        ctx.beginTransparencyLayer(auxiliaryInfo: nil)
        color.setFill()
        color.setStroke()

        switch points.count {
        case 1:
            // A dot exactly nodeRadius in radius.
            let p = points[0]
            ctx.fillEllipse(in: CGRect(x: p.x - r, y: p.y - r, width: r * 2, height: r * 2))
        case 2:
            // A capsule the same thickness as the dot — each rounded end IS a dot.
            let path = NSBezierPath()
            path.move(to: points[0])
            path.line(to: points[1])
            path.lineWidth = r * 2
            path.lineCapStyle = .round
            path.lineJoinStyle = .round
            path.stroke()
        default:
            // Fill the polygon, then stroke its outline with a round-joined pen
            // of the same nodeRadius. The stroke bulges OUTWARD around every
            // vertex, so each corner wraps around its dot instead of cutting
            // across it — and inside the transparency layer the two union into
            // one solid weight, no seam.
            let poly = hull(points)
            let path = NSBezierPath()
            path.move(to: poly[0])
            for p in poly.dropFirst() { path.line(to: p) }
            path.close()
            path.lineWidth = r * 2
            path.lineCapStyle = .round
            path.lineJoinStyle = .round
            path.stroke()
            path.fill()
        }
        ctx.endTransparencyLayer()
        ctx.restoreGState()
    }
}

#endif
