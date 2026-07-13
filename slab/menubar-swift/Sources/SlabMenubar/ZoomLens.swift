import AppKit
import ApplicationServices
import CoreGraphics

/// ⌃⌃ zooms in on the window under the pointer — the whole window, scaled to fit,
/// centred, with a margin of surrounding context. ⌃⌃ again zooms back out.
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
    /// Below this the zoom isn't worth the interruption; above it, a tiny palette
    /// window becomes an unreadable wall of pixels.
    private static let minFactor: CGFloat = 1.2
    private static let maxFactor: CGFloat = 8.0
    /// Floating-point slop — the compositor reports 1.0029 for "not zoomed".
    private static let zoomedThreshold: CGFloat = 1.01

    static var isZoomed: Bool { current().factor > zoomedThreshold }

    static func toggle() {
        if isZoomed {
            zoomOut()
            return
        }
        guard let window = windowUnderCursor(excluding: getpid()),
              let screen = screen(bestContaining: window) else {
            NSSound.beep()   // pointer is over the desktop — nothing to aim at
            return
        }

        // Fit the whole window on the tighter axis, then back off by the margin.
        // The looser axis keeps whatever slack the aspect ratio gives it — which
        // is why a window never fills the screen edge-to-edge, and why you can
        // still see what's around it.
        let fit = min(screen.frame.width / window.width,
                      screen.frame.height / window.height)
        let factor = min(max(fit / contextMargin, minFactor), maxFactor)
        let centre = CGPoint(x: window.midX, y: window.midY)

        apply(origin: centre, factor: factor)
    }

    static func zoomOut() {
        guard let screen = NSScreen.main else { return }
        // Factor 1.0 is the exit. The origin is irrelevant at 1×, but hand back
        // the screen centre so a subsequent zoom-by-hand starts somewhere sane.
        apply(origin: CGPoint(x: screen.frame.midX, y: screen.frame.midY), factor: 1.0)
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

    private static func apply(origin: CGPoint, factor: CGFloat) {
        guard let cgs = cgs else { return }
        // Preserve the user's smoothing choice; it's their Accessibility setting,
        // not ours to flip.
        let smoothing = current().smoothing
        var o = origin
        let err = cgs.set(cgs.connection, &o, Double(factor), smoothing)
        if err != 0 { NSLog("slab zoom lens: CGSSetZoomParameters failed (\(err))") }
    }

    // MARK: - what to aim at

    /// The topmost ordinary window under the pointer, in CoreGraphics globals —
    /// the same space the zoom origin uses. `optionOnScreenOnly` hands the list
    /// back already sorted front-to-back, so the first hit wins.
    private static func windowUnderCursor(excluding pid: pid_t) -> CGRect? {
        guard let point = CGEvent(source: nil)?.location,
              let info = CGWindowListCopyWindowInfo(
                [.optionOnScreenOnly, .excludeDesktopElements],
                kCGNullWindowID) as? [[String: Any]] else { return nil }
        for w in info {
            guard let layer = w[kCGWindowLayer as String] as? Int, layer == 0,
                  let owner = w[kCGWindowOwnerPID as String] as? pid_t, owner != pid,
                  let b = w[kCGWindowBounds as String] as? [String: CGFloat],
                  let x = b["X"], let y = b["Y"],
                  let width = b["Width"], let height = b["Height"] else { continue }
            // Anything smaller than this is a shadow, tooltip or other chrome that
            // happens to sit at layer 0 — never what someone means by "this window".
            guard width >= 64, height >= 64 else { continue }
            let rect = CGRect(x: x, y: y, width: width, height: height)
            if rect.contains(point) { return rect }
        }
        return nil
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
