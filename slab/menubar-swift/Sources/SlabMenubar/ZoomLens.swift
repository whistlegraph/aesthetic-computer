import AppKit
import CoreGraphics
import IOSurface
import ScreenCaptureKit

/// ⌃⌃ magnifies the screen around the window under the pointer. ⌃⌃ again (or
/// Esc) drops back out.
///
/// This is macOS's Accessibility Zoom in spirit — the screen swells *in place*,
/// neighbours still visible around the edges — but aimed, which the real one
/// can't be: there is no public API to point system zoom at a rect or set a
/// magnification factor (`UAZoomChangeFocus` only pans a zoom the user already
/// turned on). So we render it: a live ScreenCaptureKit stream of the whole
/// display, drawn over the display, springing from 1:1 up to a factor that fits
/// the window you're pointing at.
///
/// Starting at 1:1 is what makes it seamless. The overlay's first frame is a
/// pixel-exact copy of the screen it covers, so there's no moment where the
/// lens visibly *appears* — it just starts growing.
///
/// It stays INTERACTIVE, and the trick is that we never consume the mouse. A
/// head-inserted session tap rewrites each mouse event's `location` from where
/// the pointer *looks* like it is — out on the magnified image — to the point on
/// the real screen that pixel came from, then passes the event through otherwise
/// untouched. The cursor physically follows the rewritten location, so clicks,
/// drags, scroll, right-click menus, hover and tooltips all land natively: no
/// synthetic re-posting, no drag state machine. Pointing also gets `scale`×
/// finer, since a physical inch of mouse now sweeps an inch of *magnified*
/// screen. The overlay is click-through and never takes key focus, so keystrokes
/// keep going wherever they were already going.
///
/// The one thing we must hand-roll is the pointer's picture. The real cursor
/// draws at the rewritten location — the wrong place on screen now — so we hide
/// it and draw our own where the user actually feels it. (And no, ScreenCaptureKit
/// can't do it for us: a hidden cursor isn't composited into captures either, so
/// `showsCursor` yields nothing once we've hidden it.)
@available(macOS 12.3, *)
final class ZoomLens: NSObject, SCStreamOutput, SCStreamDelegate {
    static let shared = ZoomLens()

    /// Below this the lens isn't worth the interruption; above it, a small
    /// palette window blows up into an unreadable wall of pixels.
    private static let minScale: CGFloat = 1.5
    private static let maxScale: CGFloat = 6.0
    /// Windows smaller than this are shadows, tooltips and other chrome that
    /// happen to sit at layer 0 — never what someone means by "this window".
    private static let minWindowSide: CGFloat = 64

    private(set) var isZoomed = false
    /// Set for the length of the collapse animation so a fast second ⌃⌃ can't
    /// re-enter on top of a teardown that's still running.
    private var isCollapsing = false

    // Geometry lives in the overlay view's own coordinate space (AppKit: origin
    // bottom-left of the target screen, y up), because that's where the layers
    // are. CoreGraphics globals (top-left, y down) — what CGWindowList hands back
    // and what CGEvent.location wants — are converted at the boundary.
    private var scale: CGFloat = 1
    /// Pan of the magnified image, clamped so no screen edge ever shows through.
    private var offset: CGPoint = .zero
    private var viewSize: CGSize = .zero
    private var screenFrame: CGRect = .zero
    /// Where the user *feels* the pointer is, out on the magnified image.
    /// Integrated from raw mouse deltas — never read back from the system, so
    /// relocating the real cursor can't feed back into it.
    private var virtualPoint: CGPoint = .zero

    private var overlay: NSWindow?
    private var contentLayer: CALayer?
    private var cursorLayer: CALayer?

    private var stream: SCStream?
    private var pointerTap: CFMachPort?
    private var pointerTapSource: CFRunLoopSource?
    private var pointerTapRunLoop: CFRunLoop?
    private var cursorHidden = false

    private override init() { super.init() }

    func toggle() {
        guard !isCollapsing else { return }
        if isZoomed { collapse() } else { popIn() }
    }

    // MARK: - pop in

    private func popIn() {
        let cursorCG = Self.cursorLocation()
        guard let window = Self.windowUnderCursor(excluding: getpid()) else {
            NSSound.beep()   // pointer is over the desktop — nothing to aim at
            return
        }
        guard let screen = Self.screen(bestContaining: window) else { return }

        screenFrame = screen.frame
        viewSize = screen.frame.size
        let center = CGPoint(x: viewSize.width / 2, y: viewSize.height / 2)

        // Fit the window we're pointing at. The two axes rarely agree, so the
        // slack on the looser one is what keeps the neighbours in frame — the
        // "in context" part.
        scale = min(max(min(viewSize.width / window.width,
                            viewSize.height / window.height),
                        Self.minScale), Self.maxScale)

        // Pan so the window's centre lands at the screen's centre…
        let focus = toView(CGPoint(x: window.midX, y: window.midY))
        offset = CGPoint(x: (center.x - focus.x) * scale,
                         y: (center.y - focus.y) * scale)
        // …but never so far that the magnified image stops covering the screen.
        let slackX = (scale - 1) * viewSize.width / 2
        let slackY = (scale - 1) * viewSize.height / 2
        offset.x = min(max(offset.x, -slackX), slackX)
        offset.y = min(max(offset.y, -slackY), slackY)

        // Start the pointer at the magnified home of the pixel it's already
        // resting on: `realPoint(from:)` then maps straight back to where the
        // cursor physically is, so nothing jumps on the way in.
        let cursorView = toView(cursorCG)
        virtualPoint = magnify(cursorView)

        buildOverlay(on: screen)
        hideSystemCursor()
        installPointerTap()
        startStream()
        animatePop(inward: true, cursorFrom: cursorView)

        isZoomed = true
        NSLog("slab zoom lens: in — scale %.2f, window %@", scale, NSStringFromRect(window))
    }

    // MARK: - collapse

    private func collapse() {
        guard isZoomed, !isCollapsing else { return }
        isCollapsing = true

        // Give the pointer back to the system first, so it's under normal control
        // for the whole collapse. The real cursor is already sitting on the point
        // you were aiming at, so letting go here lands you exactly where the lens
        // said you were.
        removePointerTap()
        showSystemCursor()
        stopStream()

        animatePop(inward: false, cursorFrom: .zero) { [weak self] in
            guard let self = self else { return }
            self.overlay?.orderOut(nil)
            self.overlay = nil
            self.contentLayer = nil
            self.cursorLayer = nil
            self.isZoomed = false
            self.isCollapsing = false
        }
        NSLog("slab zoom lens: out")
    }

    // MARK: - overlay

    private func buildOverlay(on screen: NSScreen) {
        let win = NSWindow(contentRect: screen.frame, styleMask: .borderless,
                           backing: .buffered, defer: false)
        // Never opaque, and never key. Not opaque because a fully-opaque window
        // on top would let the window server mark everything beneath it as
        // occluded — and occluded windows are exactly what apps stop drawing,
        // which would freeze the very content we're magnifying. The sub-unity
        // alpha is the belt to isOpaque's braces: a window with alpha < 1 cannot
        // occlude, and 0.995 is not a difference anyone can see. Not key because
        // keystrokes belong to whatever window you were already typing into.
        win.isOpaque = false
        win.backgroundColor = .clear
        win.alphaValue = 0.995
        win.hasShadow = false
        win.ignoresMouseEvents = true
        win.level = .screenSaver
        win.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]

        let view = NSView(frame: NSRect(origin: .zero, size: screen.frame.size))
        view.wantsLayer = true
        let root = view.layer ?? CALayer()
        root.masksToBounds = true

        // The display, drawn 1:1 over the display. Magnification is pure
        // transform, so the identity state is a perfect pixel-for-pixel overlap
        // with the screen underneath it.
        let content = CALayer()
        content.frame = view.bounds
        content.contentsGravity = .resize
        content.contentsScale = screen.backingScaleFactor
        content.magnificationFilter = .linear
        root.addSublayer(content)

        let cursor = CALayer()
        cursor.contentsScale = screen.backingScaleFactor
        cursor.magnificationFilter = .linear
        cursor.zPosition = 1
        root.addSublayer(cursor)

        view.layer = root
        win.contentView = view
        // orderFrontRegardless, never makeKey: an accessory app ordering a window
        // front must not steal focus from whatever the user is working in.
        win.orderFrontRegardless()

        overlay = win
        contentLayer = content
        cursorLayer = cursor
        drawCursor(at: toView(Self.cursorLocation()), animated: false)
    }

    /// Identity → magnified, or back. The model transform is set to the
    /// destination up front so any frame arriving mid-flight composites against
    /// the right end state.
    private func animatePop(inward: Bool, cursorFrom: CGPoint,
                            completion: (() -> Void)? = nil) {
        guard let content = contentLayer else {
            completion?()
            return
        }
        let magnified = CATransform3DConcat(
            CATransform3DMakeScale(scale, scale, 1),
            CATransform3DMakeTranslation(offset.x, offset.y, 0))
        let to = inward ? magnified : CATransform3DIdentity

        CATransaction.begin()
        CATransaction.setCompletionBlock(completion)

        let spring = CASpringAnimation(keyPath: "transform")
        spring.mass = 1
        spring.stiffness = inward ? 260 : 420
        spring.damping = inward ? 26 : 34
        spring.fromValue = NSValue(caTransform3D: inward ? CATransform3DIdentity : magnified)
        spring.toValue = NSValue(caTransform3D: to)
        spring.duration = spring.settlingDuration
        content.transform = to
        content.add(spring, forKey: "pop")

        // Ride the same spring with the pointer on the way in, so it drifts out
        // to the magnified home of the pixel it was resting on rather than
        // teleporting there. On the way out the tap is already gone and the real
        // cursor is back, so there's nothing to animate.
        if inward, let cursor = cursorLayer {
            let target = cursorCenter(for: virtualPoint)
            let move = CASpringAnimation(keyPath: "position")
            move.mass = 1
            move.stiffness = 260
            move.damping = 26
            move.fromValue = NSValue(point: cursorCenter(for: cursorFrom))
            move.toValue = NSValue(point: target)
            move.duration = move.settlingDuration
            cursor.position = target
            cursor.add(move, forKey: "pop")
        } else if let cursor = cursorLayer {
            cursor.isHidden = true
        }

        CATransaction.commit()
    }

    // MARK: - live capture

    private func startStream() {
        // Our own overlay must be excluded, or we'd be magnifying a magnified
        // image of ourselves — a feedback tunnel. Everything *else* Slab draws
        // (prompt rocks, badges) is real screen content and should magnify.
        let selfID = CGWindowID(overlay?.windowNumber ?? 0)

        Task { [weak self] in
            guard let self = self else { return }
            guard let content = try? await SCShareableContent.excludingDesktopWindows(
                    false, onScreenWindowsOnly: true),
                  let display = content.displays.first(where: {
                      $0.displayID == self.displayID()
                  }) ?? content.displays.first
            else {
                NSLog("slab zoom lens: no display to capture — Screen Recording not granted?")
                await MainActor.run { self.collapse() }
                return
            }

            let cfg = SCStreamConfiguration()
            let backing = await MainActor.run { self.screenFrame.size }
            let density = await MainActor.run { NSScreen.main?.backingScaleFactor ?? 2 }
            cfg.width = Int(backing.width * density)
            cfg.height = Int(backing.height * density)
            // Pointless to ask for it: we've hidden the real cursor, and a hidden
            // cursor isn't composited into captures. We draw our own instead.
            cfg.showsCursor = false
            cfg.queueDepth = 5
            cfg.minimumFrameInterval = CMTime(value: 1, timescale: 60)

            let exclude = content.windows.filter { $0.windowID == selfID }
            let filter = SCContentFilter(display: display, excludingWindows: exclude)
            let s = SCStream(filter: filter, configuration: cfg, delegate: self)
            do {
                try s.addStreamOutput(self, type: .screen,
                                      sampleHandlerQueue: DispatchQueue(
                                        label: "computer.slab.zoomlens", qos: .userInteractive))
                try await s.startCapture()
                self.stream = s
            } catch {
                NSLog("slab zoom lens: stream failed: \(error)")
                await MainActor.run { self.collapse() }
            }
        }
    }

    private func stopStream() {
        guard let s = stream else { return }
        stream = nil
        Task { try? await s.stopCapture() }
    }

    private func displayID() -> CGDirectDisplayID {
        guard let screen = NSScreen.screens.first(where: { $0.frame == screenFrame }),
              let number = screen.deviceDescription[
                NSDeviceDescriptionKey("NSScreenNumber")] as? NSNumber
        else { return CGMainDisplayID() }
        return CGDirectDisplayID(number.uint32Value)
    }

    func stream(_ stream: SCStream, didOutputSampleBuffer sampleBuffer: CMSampleBuffer,
                of type: SCStreamOutputType) {
        guard type == .screen,
              let attachments = CMSampleBufferGetSampleAttachmentsArray(
                sampleBuffer, createIfNecessary: false) as? [[SCStreamFrameInfo: Any]],
              let raw = attachments.first?[.status] as? Int,
              SCFrameStatus(rawValue: raw) == .complete,
              let pixels = CMSampleBufferGetImageBuffer(sampleBuffer),
              let surface = CVPixelBufferGetIOSurface(pixels)?.takeUnretainedValue()
        else { return }

        // Handing CALayer the IOSurface keeps the display's pixels on the GPU the
        // whole way — no readback, no copy, no CGImage per frame.
        DispatchQueue.main.async { [weak self] in
            CATransaction.begin()
            CATransaction.setDisableActions(true)
            self?.contentLayer?.contents = surface
            CATransaction.commit()
        }
    }

    func stream(_ stream: SCStream, didStopWithError error: Error) {
        NSLog("slab zoom lens: stream stopped: \(error)")
        DispatchQueue.main.async { [weak self] in self?.collapse() }
    }

    // MARK: - the mapping

    /// Screen point → where it's drawn once magnified.
    private func magnify(_ p: CGPoint) -> CGPoint {
        let c = CGPoint(x: viewSize.width / 2, y: viewSize.height / 2)
        return CGPoint(x: c.x + (p.x - c.x) * scale + offset.x,
                       y: c.y + (p.y - c.y) * scale + offset.y)
    }

    /// The inverse: the magnified image at `v` is showing this point of the real
    /// screen. This is the whole illusion in one line.
    private func realPoint(from v: CGPoint) -> CGPoint {
        let c = CGPoint(x: viewSize.width / 2, y: viewSize.height / 2)
        return CGPoint(x: c.x + (v.x - c.x - offset.x) / scale,
                       y: c.y + (v.y - c.y - offset.y) / scale)
    }

    // MARK: - pointer

    private func installPointerTap() {
        // A consuming tap blocks the event stream while its callback runs, so it
        // gets a thread of its own: if the main runloop stalls on a menu build or
        // an osascript, the pointer must not stall with it.
        let thread = Thread { [weak self] in
            guard let self = self else { return }
            let mask: CGEventMask =
                (1 << CGEventType.mouseMoved.rawValue) |
                (1 << CGEventType.leftMouseDown.rawValue) |
                (1 << CGEventType.leftMouseUp.rawValue) |
                (1 << CGEventType.leftMouseDragged.rawValue) |
                (1 << CGEventType.rightMouseDown.rawValue) |
                (1 << CGEventType.rightMouseUp.rawValue) |
                (1 << CGEventType.rightMouseDragged.rawValue) |
                (1 << CGEventType.otherMouseDown.rawValue) |
                (1 << CGEventType.otherMouseUp.rawValue) |
                (1 << CGEventType.otherMouseDragged.rawValue) |
                (1 << CGEventType.scrollWheel.rawValue) |
                (1 << CGEventType.keyDown.rawValue)

            let callback: CGEventTapCallBack = { _, type, event, refcon in
                guard let refcon = refcon else { return Unmanaged.passUnretained(event) }
                let me = Unmanaged<ZoomLens>.fromOpaque(refcon).takeUnretainedValue()
                return me.handlePointer(type: type, event: event)
            }

            guard let port = CGEvent.tapCreate(
                tap: .cgSessionEventTap,
                place: .headInsertEventTap,
                options: .defaultTap,
                eventsOfInterest: mask,
                callback: callback,
                userInfo: Unmanaged.passUnretained(self).toOpaque()
            ) else {
                NSLog("slab zoom lens: pointer tap creation failed")
                DispatchQueue.main.async { self.collapse() }
                return
            }

            let src = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, port, 0)
            self.pointerTap = port
            self.pointerTapSource = src
            self.pointerTapRunLoop = CFRunLoopGetCurrent()
            CFRunLoopAddSource(CFRunLoopGetCurrent(), src, .commonModes)
            CGEvent.tapEnable(tap: port, enable: true)
            CFRunLoopRun()
        }
        thread.qualityOfService = .userInteractive
        thread.name = "computer.slab.zoomlens.pointer"
        thread.start()
    }

    private func removePointerTap() {
        if let port = pointerTap { CGEvent.tapEnable(tap: port, enable: false) }
        if let loop = pointerTapRunLoop {
            if let src = pointerTapSource { CFRunLoopRemoveSource(loop, src, .commonModes) }
            CFRunLoopStop(loop)
        }
        pointerTap = nil
        pointerTapSource = nil
        pointerTapRunLoop = nil
    }

    /// Runs on the pointer thread. Keep it short — this is on the critical path
    /// of the user's mouse.
    private func handlePointer(type: CGEventType, event: CGEvent) -> Unmanaged<CGEvent>? {
        if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
            if let port = pointerTap { CGEvent.tapEnable(tap: port, enable: true) }
            return Unmanaged.passUnretained(event)
        }

        if type == .keyDown {
            // Esc is the escape hatch, and the only key we ever eat.
            if event.getIntegerValueField(.keyboardEventKeycode) == 53 {
                DispatchQueue.main.async { [weak self] in self?.collapse() }
                return nil
            }
            return Unmanaged.passUnretained(event)
        }

        switch type {
        case .mouseMoved, .leftMouseDragged, .rightMouseDragged, .otherMouseDragged:
            // Integrate the *physical* delta, unscaled: the pointer sweeps the
            // magnified image at its usual speed, which is what buys `scale`×
            // finer aim on the real screen. CG deltas are y-down; the view is
            // y-up.
            virtualPoint.x += event.getDoubleValueField(.mouseEventDeltaX)
            virtualPoint.y -= event.getDoubleValueField(.mouseEventDeltaY)
            virtualPoint.x = min(max(virtualPoint.x, 0), viewSize.width)
            virtualPoint.y = min(max(virtualPoint.y, 0), viewSize.height)
        default:
            break   // clicks and scrolls happen wherever the pointer already is
        }

        let v = virtualPoint
        // Rewriting `location` both moves the real cursor and tells the app under
        // it where the click landed. The delta fields are left alone: apps
        // hit-test on location, and rounding a scaled delta into the event's
        // integer fields would only throw away the sub-point precision we just
        // bought.
        event.location = toCG(realPoint(from: v))

        DispatchQueue.main.async { [weak self] in self?.drawCursor(at: v, animated: false) }
        return Unmanaged.passUnretained(event)
    }

    // MARK: - the drawn cursor

    /// A layer is positioned by its centre, but a cursor is positioned by its
    /// hotspot — which is measured from the image's top-left, in a y-down space,
    /// while the layer lives in a y-up one. Getting this wrong is what stranded
    /// the pointer in the corner the first time.
    private func cursorCenter(for v: CGPoint, cursor: NSCursor = .arrow) -> CGPoint {
        let size = cursorSize(cursor)
        let hot = cursor.hotSpot
        let k = size.width / max(cursor.image.size.width, 1)   // hotspot scales with the image
        return CGPoint(x: v.x + size.width / 2 - hot.x * k,
                       y: v.y - size.height / 2 + hot.y * k)
    }

    /// Magnify the pointer along with everything else — a native-size arrow adrift
    /// in 4×-scale UI reads as broken. Capped, because cursor art is a bitmap and
    /// past ~3× it turns to mush.
    private func cursorSize(_ cursor: NSCursor) -> CGSize {
        let k = min(scale, 3.0)
        return CGSize(width: cursor.image.size.width * k,
                      height: cursor.image.size.height * k)
    }

    private func drawCursor(at v: CGPoint, animated: Bool) {
        guard let layer = cursorLayer else { return }
        // Track the system cursor's *shape*, so an I-beam over text still reads as
        // an I-beam even though we're the one drawing it.
        let cursor = NSCursor.currentSystem ?? NSCursor.arrow
        CATransaction.begin()
        CATransaction.setDisableActions(!animated)
        layer.contents = cursor.image
        layer.bounds = CGRect(origin: .zero, size: cursorSize(cursor))
        layer.position = cursorCenter(for: v, cursor: cursor)
        layer.isHidden = false
        CATransaction.commit()
    }

    /// Hide the real cursor everywhere, not just while we're frontmost.
    ///
    /// `CGDisplayHideCursor` only bites for the *active* application — and we
    /// deliberately aren't it, because the window being magnified needs to keep
    /// the keyboard. `SetsCursorInBackground` is the long-lived private CGS
    /// property that lifts that restriction; it's what screen recorders and
    /// magnifiers on this platform have leaned on for years. Verified working
    /// from a background accessory app. Looked up via `dlsym` so a future macOS
    /// dropping the symbol degrades to a visible second cursor rather than a
    /// binary that won't launch.
    private func hideSystemCursor() {
        typealias MainConnectionID = @convention(c) () -> UInt32
        typealias SetConnectionProperty = @convention(c) (UInt32, UInt32, CFString, CFTypeRef) -> Int32
        if let handle = dlopen(nil, RTLD_NOW),
           let connSym = dlsym(handle, "CGSMainConnectionID"),
           let propSym = dlsym(handle, "CGSSetConnectionProperty") {
            let mainConnection = unsafeBitCast(connSym, to: MainConnectionID.self)
            let setProperty = unsafeBitCast(propSym, to: SetConnectionProperty.self)
            let cid = mainConnection()
            _ = setProperty(cid, cid, "SetsCursorInBackground" as CFString, kCFBooleanTrue)
        }
        CGDisplayHideCursor(CGMainDisplayID())
        cursorHidden = true
    }

    private func showSystemCursor() {
        guard cursorHidden else { return }
        CGDisplayShowCursor(CGMainDisplayID())
        cursorHidden = false
    }

    /// Belt and braces for the one state that would really ruin someone's day:
    /// the app going away with the cursor still hidden. (Hiding is scoped to our
    /// window-server connection, so a crash restores it anyway — but a clean quit
    /// shouldn't have to rely on that.)
    func restoreCursorOnExit() {
        showSystemCursor()
    }

    // MARK: - geometry

    private static func cursorLocation() -> CGPoint {
        CGEvent(source: nil)?.location ?? .zero
    }

    /// CoreGraphics global (top-left origin, y down) → the overlay view's space
    /// (bottom-left origin, y up). The flip pivots on the primary screen's top
    /// edge, the one line both spaces agree on.
    private func toView(_ p: CGPoint) -> CGPoint {
        let top = NSScreen.screens.first?.frame.maxY ?? 0
        return CGPoint(x: p.x - screenFrame.minX, y: (top - p.y) - screenFrame.minY)
    }

    private func toCG(_ p: CGPoint) -> CGPoint {
        let top = NSScreen.screens.first?.frame.maxY ?? 0
        return CGPoint(x: p.x + screenFrame.minX, y: top - (p.y + screenFrame.minY))
    }

    /// The topmost ordinary window under the pointer, in CoreGraphics globals.
    /// `optionOnScreenOnly` hands the list back already sorted front-to-back, so
    /// the first hit wins.
    private static func windowUnderCursor(excluding pid: pid_t) -> CGRect? {
        let point = cursorLocation()
        guard let info = CGWindowListCopyWindowInfo(
                [.optionOnScreenOnly, .excludeDesktopElements],
                kCGNullWindowID) as? [[String: Any]] else { return nil }
        for w in info {
            guard let layer = w[kCGWindowLayer as String] as? Int, layer == 0,
                  let owner = w[kCGWindowOwnerPID as String] as? pid_t, owner != pid,
                  let b = w[kCGWindowBounds as String] as? [String: CGFloat],
                  let x = b["X"], let y = b["Y"],
                  let width = b["Width"], let height = b["Height"] else { continue }
            guard width >= minWindowSide, height >= minWindowSide else { continue }
            let rect = CGRect(x: x, y: y, width: width, height: height)
            if rect.contains(point) { return rect }
        }
        return nil
    }

    private static func screen(bestContaining rect: CGRect) -> NSScreen? {
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

/// AppDelegate targets macOS 11; the lens needs ScreenCaptureKit. This is the
/// seam, so callers don't carry the availability check around.
enum ZoomLensBridge {
    static var isSupported: Bool {
        if #available(macOS 12.3, *) { return true }
        return false
    }

    static var isZoomed: Bool {
        if #available(macOS 12.3, *) { return ZoomLens.shared.isZoomed }
        return false
    }

    static func toggle() {
        if #available(macOS 12.3, *) { ZoomLens.shared.toggle() }
    }

    static func restoreCursorOnExit() {
        if #available(macOS 12.3, *) { ZoomLens.shared.restoreCursorOnExit() }
    }
}
