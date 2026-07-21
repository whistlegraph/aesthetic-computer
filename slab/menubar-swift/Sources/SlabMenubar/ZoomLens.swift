import AppKit
import ApplicationServices
import CoreGraphics

/// ⌃⌃ zooms in on whatever the pointer is on — the whole thing, scaled to fit,
/// centred, with a margin of surrounding context. Moving onto another window
/// follows, refits, and recentres it. ⌃⌃ again zooms back out.
///
/// "Whatever" means an ordinary window, or a menu bar item. Status items turn out
/// to be real windows (Control Center hosts one apiece, up at the menu bar
/// level), so they hit-test like anything else and Menu Band can be zoomed the
/// same as a terminal. The app-menu half of the bar is not — those titles are
/// painted straight into a single full-width Window Server surface, with no
/// per-title window to aim at — so there we zoom a slice of the bar around the
/// pointer instead.
///
/// This drives the window server's *own* zoom — the same compositor path
/// Accessibility Zoom uses — rather than rendering a magnifier of our own. That
/// distinction is the whole feature. A userspace lens has to re-composite the
/// screen every frame (capture → GPU → transform) and hand-draw the cursor from
/// mouse events, and both sit on latency the real zoom doesn't have. We built
/// that first. It was janky, and no polish gets it under that floor.
///
/// `CGSSetZoomParameters` is the knob, and it takes exactly the two things the
/// public surface won't give up: an origin *and* a factor. So the framing is
/// entirely ours — fit the window, leave `contextMargin` of slack, centre it —
/// while the rendering stays the compositor's.
///
/// Notes from prising it open, since none of this is documented:
///
///   • `factor` is a **double**, not a float. Read it through a `Float *` and it
///     comes back as exactly 0.0 every time (the low 32 bits of 1.0, 2.0, 4.0 as
///     doubles are all zero), which reads like a dead API rather than a type
///     error. It cost an hour. It is not dead.
///
///   • `origin` is the viewport's *centre*, in CoreGraphics globals (top-left,
///     y down) and in points, not pixels — at rest it reads as the centre of the
///     screen.
///
///   • Setting a factor > 1 engages the zoom on its own. No ⌥⌘8, no
///     `closeViewHotkeysEnabled`, no preference writes: we never touch the
///     user's Accessibility settings. Setting factor 1.0 exits cleanly.
///
///   • `UAZoomChangeFocus` is useless here — it's a no-op against a zoom that
///     wasn't engaged by CloseView itself. We set the origin directly instead.
///
/// The zoom is compositor state, so it OUTLIVES this process: quit while zoomed
/// and the screen stays zoomed. Hence `zoomOut()` on terminate. (A user is never
/// truly stuck — ⌃+scroll still gets them out — but they shouldn't have to.)
enum ZoomLens {
    /// How much of the screen to leave around the window. 1.1 → the window fills
    /// ~90% of the screen's tighter axis, and the 10% left over is the context
    /// that tells you *where* the window is.
    private static let contextMargin: CGFloat = 1.1
    /// Above this, a tiny palette window becomes an unreadable wall of pixels.
    /// There is deliberately no minimum factor: tall and nearly full-screen
    /// windows may only have a little room to grow, and forcing a minimum zoom
    /// would crop them instead of preserving the whole window in the viewport.
    private static let maxFactor: CGFloat = 8.0
    /// Floating-point slop — the compositor reports 1.0029 for "not zoomed".
    private static let zoomedThreshold: CGFloat = 1.01
    /// Window-list hit-testing is much heavier than reading a pointer sample.
    /// 30 Hz keeps cross-window dragging responsive without doing expensive
    /// window-list hit tests for every raw mouse sample.
    private static let followInterval: CFTimeInterval = 1.0 / 30.0
    /// A retarget should read as a deliberate camera move, not a teleport.
    private static let panDuration: CFTimeInterval = 0.26

    private struct Target: Equatable {
        let id: CGWindowID?
        let frame: CGRect
    }

    private static var activeTarget: Target?
    private static var lastFollowAt: CFTimeInterval = 0
    private static var panTimer: Timer?

    static var isZoomed: Bool { current().factor > zoomedThreshold }

    static func toggle() {
        if isZoomed {
            zoomOut()
            PopSound.play(rising: false)
            return
        }
        guard let target = targetUnderCursor(excluding: getpid()),
              let screen = screen(bestContaining: target.frame) else {
            NSSound.beep()   // pointer is over bare desktop — nothing to aim at
            return
        }

        zoom(to: target, on: screen, animated: false)
        PopSound.play(rising: true)
    }

    /// While the lens is up, crossing onto a different window makes that window
    /// the new subject. Recompute both magnification and origin so differently
    /// sized windows still fit and land centred, rather than merely sliding the
    /// old zoom factor across the desktop.
    static func followCursor(to point: CGPoint) {
        guard activeTarget != nil else { return }

        let now = CACurrentMediaTime()
        guard now - lastFollowAt >= followInterval else { return }
        lastFollowAt = now

        // Compositor zoom can also be changed outside Slab (for example with
        // Accessibility shortcuts). Notice that promptly and stop following.
        guard isZoomed else {
            activeTarget = nil
            return
        }

        guard let target = targetUnderCursor(excluding: getpid(), at: point),
              target != activeTarget,
              let screen = screen(bestContaining: target.frame) else { return }
        zoom(to: target, on: screen, animated: true)
    }

    private static func zoom(to target: Target, on screen: NSScreen, animated: Bool) {
        // Fit the whole window on the tighter axis, then back off by the margin.
        // The looser axis keeps whatever slack the aspect ratio gives it — which
        // is why a window never fills the screen edge-to-edge, and why you can
        // still see what's around it.
        let fit = min(screen.frame.width / target.frame.width,
                      screen.frame.height / target.frame.height)
        // Never exceed `fit`: that is the largest factor at which the target's
        // complete width and height remain visible. Usually `contextMargin`
        // makes the factor smaller still, but for a tall/full-height window the
        // requested margin can take it below 1×. The compositor cannot zoom out,
        // so settle at 1× while retaining the window's unmodified aspect ratio.
        let factor = min(max(fit / contextMargin, 1.0), fit, maxFactor)
        let centre = CGPoint(x: target.frame.midX, y: target.frame.midY)

        activeTarget = target
        if animated {
            pan(to: centre, factor: factor)
        } else {
            panTimer?.invalidate()
            panTimer = nil
            apply(origin: centre, factor: factor)
        }
        // The particle bloom is punctuation for acquisition, not navigation.
        // Rebuilding it at every cross-window drag handoff competes with the
        // compositor pan and makes following feel sticky.
        if !animated { ZoomSpecialMove.fire(around: target.frame, on: screen) }
    }

    static func zoomOut() {
        panTimer?.invalidate()
        panTimer = nil
        activeTarget = nil
        lastFollowAt = 0
        guard let screen = NSScreen.main else { return }
        // Factor 1.0 is the exit. The origin is irrelevant at 1×, but hand back
        // the screen centre so a subsequent zoom-by-hand starts somewhere sane.
        apply(origin: CGPoint(x: screen.frame.midX, y: screen.frame.midY), factor: 1.0)
    }

    /// Ease both the viewport centre and magnification. Factor is interpolated
    /// logarithmically because zoom is perceived as a ratio: halfway from 2× to
    /// 8× should feel like 4×, not 5×.
    private static func pan(to destination: CGPoint, factor destinationFactor: CGFloat) {
        panTimer?.invalidate()
        let start = current()
        let began = CACurrentMediaTime()
        let startLogFactor = log(max(start.factor, 0.001))
        let endLogFactor = log(max(destinationFactor, 0.001))

        let timer = Timer(timeInterval: 1.0 / 60.0, repeats: true) { timer in
            let elapsed = CACurrentMediaTime() - began
            let unit = min(max(CGFloat(elapsed / panDuration), 0), 1)
            // Smoothstep: zero velocity at both ends, with no sluggish wind-up.
            let eased = unit * unit * (3 - 2 * unit)
            let origin = CGPoint(
                x: start.origin.x + (destination.x - start.origin.x) * eased,
                y: start.origin.y + (destination.y - start.origin.y) * eased)
            let factor = exp(startLogFactor + (endLogFactor - startLogFactor) * eased)
            apply(origin: origin, factor: factor, smoothing: start.smoothing)

            if unit >= 1 {
                timer.invalidate()
                panTimer = nil
                PopSound.playTransferClick()
            }
        }
        panTimer = timer
        RunLoop.main.add(timer, forMode: .common)
    }

    // MARK: - the window server's zoom

    // Private CGS/SkyLight. Resolved by dlsym rather than linked, so a future
    // macOS dropping the symbols degrades to a dead ⌃⌃ rather than a binary that
    // won't launch.
    private typealias MainConnectionID = @convention(c) () -> UInt32
    private typealias GetZoomParameters =
        @convention(c) (UInt32, UnsafeMutablePointer<CGPoint>,
                        UnsafeMutablePointer<Double>, UnsafeMutablePointer<Bool>) -> Int32
    private typealias SetZoomParameters =
        @convention(c) (UInt32, UnsafeMutablePointer<CGPoint>, Double, Bool) -> Int32

    private struct CGS {
        let connection: UInt32
        let get: GetZoomParameters
        let set: SetZoomParameters
    }

    private static let cgs: CGS? = {
        guard let handle = dlopen(nil, RTLD_NOW),
              let conn = dlsym(handle, "CGSMainConnectionID"),
              let get = dlsym(handle, "CGSGetZoomParameters"),
              let set = dlsym(handle, "CGSSetZoomParameters") else {
            NSLog("slab zoom lens: CGS zoom symbols missing — ⌃⌃ disabled")
            return nil
        }
        return CGS(connection: unsafeBitCast(conn, to: MainConnectionID.self)(),
                   get: unsafeBitCast(get, to: GetZoomParameters.self),
                   set: unsafeBitCast(set, to: SetZoomParameters.self))
    }()

    private static func current() -> (origin: CGPoint, factor: CGFloat, smoothing: Bool) {
        guard let cgs = cgs else { return (.zero, 1, false) }
        var origin = CGPoint.zero
        var factor: Double = 1
        var smoothing = false
        _ = cgs.get(cgs.connection, &origin, &factor, &smoothing)
        return (origin, CGFloat(factor), smoothing)
    }

    private static func apply(origin: CGPoint, factor: CGFloat, smoothing suppliedSmoothing: Bool? = nil) {
        guard let cgs = cgs else { return }
        // Preserve the user's smoothing choice; it's their Accessibility setting,
        // not ours to flip.
        let smoothing = suppliedSmoothing ?? current().smoothing
        var o = origin
        let err = cgs.set(cgs.connection, &o, Double(factor), smoothing)
        if err != 0 { NSLog("slab zoom lens: CGSSetZoomParameters failed (\(err))") }
    }

    // MARK: - what to aim at

    /// What the pointer is on, in CoreGraphics globals — the same space the zoom
    /// origin uses. An ordinary window, or a menu bar item, whichever is under it.
    ///
    /// SELECTION IS PURELY POSITIONAL: the topmost thing whose bounds contain the
    /// pointer, full stop. Focus is never consulted, so ⌃⌃ over a background
    /// window zooms *that* window and not the one you happen to be typing in.
    /// What you're pointing at is what you meant — that's the whole contract, and
    /// it's why nothing here asks who's frontmost.
    private static func targetUnderCursor(excluding pid: pid_t,
                                          at suppliedPoint: CGPoint? = nil) -> Target? {
        guard let point = suppliedPoint ?? CGEvent(source: nil)?.location,
              let info = CGWindowListCopyWindowInfo(
                [.optionOnScreenOnly, .excludeDesktopElements],
                kCGNullWindowID) as? [[String: Any]] else { return nil }

        // Front-to-back, which is the order `optionOnScreenOnly` returns, so the
        // first hit is the thing you're actually looking at. A status item (layer
        // 25) therefore wins over the menu bar behind it (24).
        for w in info {
            guard let layer = w[kCGWindowLayer as String] as? Int,
                  let owner = w[kCGWindowOwnerPID as String] as? pid_t,
                  let number = w[kCGWindowNumber as String] as? CGWindowID,
                  let b = w[kCGWindowBounds as String] as? [String: CGFloat],
                  let x = b["X"], let y = b["Y"],
                  let width = b["Width"], let height = b["Height"] else { continue }
            let rect = CGRect(x: x, y: y, width: width, height: height)
            guard rect.contains(point) else { continue }

            if layer == 0 {
                guard owner != pid else { continue }
                // Smaller than this is a shadow, tooltip or other chrome that
                // happens to sit at layer 0 — never what "this window" means.
                guard width >= 64, height >= 64 else { continue }
                return Target(id: number, frame: rect)
            }

            if isStatusItem(layer: layer, rect: rect) {
                return Target(id: number, frame: rect)
            }
        }

        // Nothing addressable, but we might still be ON the menu bar — the left
        // half of it has no per-item windows at all (the app menus are painted
        // straight into one full-width Window Server surface), so there is
        // nothing to hit-test. Zoom a slice of the bar around the pointer instead
        // of beeping.
        return menuBarSlice(around: point).map { Target(id: nil, frame: $0) }
    }

    /// A menu bar status item — Control Center hosts one window per item, up at
    /// the menu bar level. Distinguishing them from the two impostors that also
    /// sit at layer ≥ 24: the full-width "Menubar" surface itself, and the
    /// screen-sized Screenshot window.
    private static func isStatusItem(layer: Int, rect: CGRect) -> Bool {
        guard layer >= Int(CGWindowLevelForKey(.mainMenuWindow)) else { return false }
        guard let screen = NSScreen.main else { return false }
        guard rect.height <= menuBarDepth(of: screen) else { return false }
        guard rect.width >= 16 else { return false }
        // The bar itself spans the display; an item never comes close.
        return rect.width < screen.frame.width * 0.9
    }

    /// The app-menu half of the bar, where there's nothing to hit-test. A slice
    /// this wide lands around 3×, which is enough to read the titles without
    /// losing your place in the bar.
    private static func menuBarSlice(around point: CGPoint) -> CGRect? {
        guard let screen = NSScreen.main else { return nil }
        let depth = menuBarDepth(of: screen)
        guard point.y <= depth else { return nil }
        let width = screen.frame.width / 3
        return CGRect(x: point.x - width / 2, y: 0, width: width, height: depth)
    }

    /// The menu bar's own height: the gap AppKit leaves at the TOP of the screen.
    /// Not `frame.height - visibleFrame.height`, which also swallows the Dock.
    /// Notched and unnotched Macs disagree on the number, so ask rather than
    /// hardcode 24.
    private static func menuBarDepth(of screen: NSScreen) -> CGFloat {
        max(screen.frame.maxY - screen.visibleFrame.maxY, 24)
    }

    private static func screen(bestContaining rect: CGRect) -> NSScreen? {
        // rect is CoreGraphics (y down); NSScreen.frame is AppKit (y up). Flip the
        // screens, not the rect — only their overlap matters here.
        let top = NSScreen.screens.first?.frame.maxY ?? 0
        func asCG(_ f: CGRect) -> CGRect {
            CGRect(x: f.minX, y: top - f.maxY, width: f.width, height: f.height)
        }
        return NSScreen.screens.max {
            asCG($0.frame).intersection(rect).area < asCG($1.frame).intersection(rect).area
        } ?? NSScreen.main
    }
}

private extension CGRect {
    var area: CGFloat { isNull ? 0 : width * height }
}
