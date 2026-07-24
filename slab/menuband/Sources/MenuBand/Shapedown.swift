import AppKit
import AVFoundation
import CoreGraphics
import QuartzCore
import ScreenCaptureKit

// Shapedown — double-tap LEFT ⌘ and the whole screen turns into a glass
// wall you draw on with the trackpad. The trackpad maps 1:1 onto the display,
// so every finger is a vertex somewhere on screen, and the *set* of fingers
// is one filled, colored shape:
//
//     1 finger  → nudge the whole display buffer (no pointer / no mark)
//     2 fingers → a line
//     3 fingers → a triangle
//     4 fingers → a quad
//     N fingers → an N-gon (as many contacts as the trackpad reports)
//
// One finger nudges one captured texture of the whole display by up to 12px
// (menu bar, Dock, desktop, and windows together) and restores it on lift.
// Two or more fingers make a shape;
// lifting lets the fullest version settle into the display like a puddle,
// sustain, then fade. A physical click pins that shape until the wall closes.
// Notepat note keys choose the ink color (C is red, D orange, and so on).
// Double-tap ⌘ again (or press Escape) to leave.
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
    private var displayCapture: AnyObject?

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
        guard canvas?.pinCurrent() == true else { return }
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
            DispatchQueue.main.async { [weak self] in
                self?.clickCue()         // same registration tick as Menu Band focus
                self?.toggle()
            }
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

        let stage = NSView(frame: NSRect(origin: .zero, size: screen.frame.size))
        stage.autoresizingMask = [.width, .height]
        stage.wantsLayer = true
        stage.layer?.backgroundColor = NSColor.clear.cgColor

        // The captured display surface sits behind the ink canvas. It remains
        // empty except during a one-finger nudge, when the live composited
        // display below this panel is translated as a single GPU layer.
        let displaySurface = ShapedownDisplayBufferView(frame: stage.bounds)
        displaySurface.autoresizingMask = [.width, .height]
        let canvas = ShapedownCanvas(frame: stage.bounds, displaySurface: displaySurface)
        canvas.autoresizingMask = [.width, .height]
        stage.addSubview(displaySurface)
        stage.addSubview(canvas)

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
        panel.sharingType = .none                  // never feed the overlay back into its capture
        panel.collectionBehavior = [
            .canJoinAllSpaces, .fullScreenAuxiliary, .stationary, .ignoresCycle,
        ]
        panel.contentView = stage
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
        gestureTap.onKeyDown = { [weak self] keyCode in
            self?.canvas?.selectColor(forKeyCode: keyCode)
        }
        _ = gestureTap.start()          // block system gestures + catch clicks
        panel.orderFrontRegardless()
        if #available(macOS 12.3, *) {
            // Capture starts with the wall, not with the first finger. By the
            // time a nudge begins there is already a current IOSurface ready;
            // finger motion itself performs no capture or window enumeration.
            let permission = CGPreflightScreenCaptureAccess() || CGRequestScreenCaptureAccess()
            if permission {
                let capture = ShapedownDisplayStream(surface: displaySurface)
                displayCapture = capture
                capture.start(screen: screen, excludingWindowID: CGWindowID(panel.windowNumber))
            } else {
                NSLog("MenuBand Shapedown: Screen Recording permission is required for display nudge")
            }
        }
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
        if #available(macOS 12.3, *),
           let capture = displayCapture as? ShapedownDisplayStream {
            capture.stop()
        }
        displayCapture = nil
        // Cursor + gestures return to the system immediately.
        if cursorHidden {
            CGAssociateMouseAndMouseCursorPosition(1)   // mouse drives the cursor again
            CGDisplayShowCursor(CGMainDisplayID())
            Self.setBackgroundCursorHiding(false)
            cursorHidden = false
        }
        gestureTap.stop()               // gestures return to the system
        gestureTap.onClick = nil
        gestureTap.onKeyDown = nil
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
    /// Note keys pass through untouched, but Shapedown sees key-down first so
    /// TYPE mode (which consumes musical keys later in the event chain) cannot
    /// prevent the wall from selecting the matching Notepat color.
    var onKeyDown: ((UInt16) -> Void)?

    @discardableResult
    func start() -> Bool {
        guard tap == nil else { return true }
        // Raw event-type numbers the tap sees: 1/2 left mouse down/up (the
        // physical trackpad click), 3/4 right, 25/26 other; 22 = scrollWheel;
        // and the NSEvent gesture family 18/19/20 (rotate/begin/end), 29
        // gesture, 30 magnify, 31 swipe, 32 smartMagnify, 33 quickLook, 34
        // pressure. All consumed so the wall owns the trackpad completely.
        let types: [UInt32] = [1, 2, 3, 4, 10, 18, 19, 20, 22, 25, 26, 29, 30, 31, 32, 33, 34]
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
            } else if type == .keyDown {
                let keyCode = UInt16(event.getIntegerValueField(.keyboardEventKeycode))
                DispatchQueue.main.async { me.onKeyDown?(keyCode) }
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

/// Pure gesture memory shared by live drawing, lift, and physical-click pinning.
/// Keeping this separate from AppKit makes the most important Shapedown rule
/// directly regression-testable: once an N-point shape exists, staggered lift
/// frames may not collapse it to N-1 points.
struct ShapedownGestureMemory {
    private(set) var peak: [CGPoint] = []

    mutating func update(_ points: [CGPoint]) -> [CGPoint] {
        guard !points.isEmpty else { return peak }
        if points.count >= peak.count { peak = points }
        return peak.count >= 2 && points.count < peak.count ? peak : points
    }

    func shape(fallback: [CGPoint] = []) -> [CGPoint] {
        peak.isEmpty ? fallback : peak
    }

    mutating func release(fallback: [CGPoint] = []) -> [CGPoint] {
        let result = shape(fallback: fallback)
        peak = []
        return result
    }

    mutating func reset() { peak = [] }
}

/// Reject isolated multitouch teleports while preserving the low latency of
/// ordinary one-finger motion. A real fast move is accepted on the next
/// coherent frame; a lone wild sample never throws the display across screen.
struct ShapedownSinglePointFilter {
    private(set) var point: CGPoint?
    private var pendingOutlier: CGPoint?

    mutating func update(raw: CGPoint, canvasSize: CGSize) -> CGPoint {
        guard let previous = point else {
            point = raw
            return raw
        }
        let threshold = max(48, hypot(canvasSize.width, canvasSize.height) * 0.08)
        let jump = hypot(raw.x - previous.x, raw.y - previous.y)
        let blend: CGFloat
        if jump > threshold {
            guard let candidate = pendingOutlier,
                  hypot(raw.x - candidate.x, raw.y - candidate.y) < threshold * 0.5 else {
                pendingOutlier = raw
                return previous
            }
            pendingOutlier = nil
            blend = 0.25
        } else {
            pendingOutlier = nil
            blend = 0.48
        }
        let filtered = CGPoint(x: previous.x + (raw.x - previous.x) * blend,
                               y: previous.y + (raw.y - previous.y) * blend)
        point = filtered
        return filtered
    }

    mutating func reset() {
        point = nil
        pendingOutlier = nil
    }
}

enum ShapedownPalette {
    static func color(forKeyCode keyCode: UInt16) -> NSColor? {
        guard let semitone = MenuBandLayout.semitone(forKeyCode: keyCode, keymap: .notepat) else { return nil }
        let pitchClass = ((semitone % 12) + 12) % 12
        let octaveOffset = semitone >= 0 ? semitone / 12 : (semitone - 11) / 12
        return NoteColors.color(pitchClass: pitchClass, octave: 4 + octaveOffset)
    }
}

/// The one live texture translated by a one-finger nudge. This view is normally
/// transparent. ScreenCaptureKit keeps the display layer current while hidden;
/// a nudge only reveals and transforms it. Real windows never move, and the
/// menu bar, Dock, desktop, and windows have already been composited together.
final class ShapedownDisplayBufferView: NSView {
    private let displayLayer = AVSampleBufferDisplayLayer()
    private var nudging = false
    private var hasFrame = false

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.masksToBounds = true
        displayLayer.videoGravity = .resize
        displayLayer.isHidden = true
        displayLayer.actions = [
            "position": NSNull(), "bounds": NSNull(), "transform": NSNull(),
            "contents": NSNull(), "opacity": NSNull(),
        ]
        layer?.addSublayer(displayLayer)
    }

    required init?(coder: NSCoder) { nil }

    override func layout() {
        super.layout()
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        displayLayer.frame = bounds
        CATransaction.commit()
    }

    /// Frames arrive on the main callback queue and go straight from the
    /// IOSurface-backed CMSampleBuffer to the compositor without a CPU copy.
    func enqueue(_ sampleBuffer: CMSampleBuffer) {
        guard sampleBuffer.isValid else { return }
        if displayLayer.status == .failed {
            displayLayer.flush()
        }
        displayLayer.enqueue(sampleBuffer)
        guard !hasFrame else { return }
        hasFrame = true
        if nudging { showIfReady() }
    }

    func beginNudge() {
        nudging = true
        showIfReady()
    }

    private func showIfReady() {
        guard nudging, hasFrame else { return }
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        layer?.backgroundColor = NSColor.black.cgColor
        displayLayer.frame = bounds
        displayLayer.setAffineTransform(.identity)
        displayLayer.isHidden = false
        CATransaction.commit()
    }

    func translate(x: CGFloat, y: CGFloat) {
        guard nudging, hasFrame else { return }
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        displayLayer.setAffineTransform(CGAffineTransform(translationX: x, y: y))
        CATransaction.commit()
    }

    func endNudge() {
        nudging = false
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        displayLayer.setAffineTransform(.identity)
        displayLayer.isHidden = true
        layer?.backgroundColor = NSColor.clear.cgColor
        CATransaction.commit()
    }

    func stop() {
        endNudge()
        hasFrame = false
        displayLayer.flushAndRemoveImage()
    }
}

/// A display-wide ScreenCaptureKit stream feeding the hidden buffer view.
/// Screen samples are IOSurface-backed and arrive on the main queue, where
/// AVSampleBufferDisplayLayer can enqueue them without a CPU readback. The
/// display-excluding filter includes the desktop, Dock, and system menu bar.
@available(macOS 12.3, *)
final class ShapedownDisplayStream: NSObject, SCStreamOutput, SCStreamDelegate {
    private weak var surface: ShapedownDisplayBufferView?
    private var stream: SCStream?
    private var startTask: Task<Void, Never>?

    init(surface: ShapedownDisplayBufferView) {
        self.surface = surface
        super.init()
    }

    func start(screen: NSScreen, excludingWindowID: CGWindowID) {
        guard startTask == nil, stream == nil else { return }
        let displayID = (screen.deviceDescription[NSDeviceDescriptionKey("NSScreenNumber")]
                         as? NSNumber)?.uint32Value ?? CGMainDisplayID()
        startTask = Task { [weak self] in
            guard let self else { return }
            do {
                let content = try await SCShareableContent.excludingDesktopWindows(
                    false, onScreenWindowsOnly: true)
                try Task.checkCancellation()
                guard let display = content.displays.first(where: { $0.displayID == displayID })
                        ?? content.displays.first else { return }
                let excluded = content.windows.filter { $0.windowID == excludingWindowID }
                let filter = SCContentFilter(display: display, excludingWindows: excluded)
                if #available(macOS 14.2, *) {
                    filter.includeMenuBar = true
                }

                let config = SCStreamConfiguration()
                config.width = display.width
                config.height = display.height
                config.pixelFormat = kCVPixelFormatType_32BGRA
                config.minimumFrameInterval = CMTime(value: 1, timescale: 60)
                config.queueDepth = 3
                config.showsCursor = false

                let stream = SCStream(filter: filter, configuration: config, delegate: self)
                try stream.addStreamOutput(self, type: .screen, sampleHandlerQueue: .main)
                try Task.checkCancellation()
                self.stream = stream
                try await stream.startCapture()
            } catch is CancellationError {
                // Normal Shapedown shutdown while shareable content was loading.
            } catch {
                NSLog("MenuBand Shapedown: display stream failed: \(error)")
            }
            self.startTask = nil
        }
    }

    func stop() {
        startTask?.cancel()
        startTask = nil
        let oldStream = stream
        stream = nil
        surface?.stop()
        if let oldStream {
            Task { try? await oldStream.stopCapture() }
        }
    }

    func stream(_ stream: SCStream, didOutputSampleBuffer sampleBuffer: CMSampleBuffer,
                of type: SCStreamOutputType) {
        guard type == .screen else { return }
        surface?.enqueue(sampleBuffer)
    }

    func stream(_ stream: SCStream, didStopWithError error: Error) {
        NSLog("MenuBand Shapedown: display stream stopped: \(error)")
        DispatchQueue.main.async { [weak self] in
            self?.surface?.stop()
            self?.stream = nil
        }
    }

    deinit { stop() }
}

/// A tiny, reversible translation of the active display's live composited
/// buffer. Every motion frame is only a transform on one layer, independent of
/// the number, ownership, or type of windows visible on the display.
final class ShapedownDisplayNudge {
    private weak var surface: ShapedownDisplayBufferView?
    private var active = false
    private var lastApplied = CGPoint.zero

    init(surface: ShapedownDisplayBufferView) {
        self.surface = surface
    }

    func begin() {
        guard !active, let surface else { return }
        active = true
        lastApplied = .zero
        surface.beginNudge()
    }

    func apply(x: CGFloat, y: CGFloat) {
        guard active, let surface else { return }
        guard abs(x - lastApplied.x) >= 0.5 || abs(y - lastApplied.y) >= 0.5 else { return }
        lastApplied = CGPoint(x: x, y: y)
        surface.translate(x: x, y: y)
    }

    func restore() {
        surface?.endNudge()
        active = false
        lastApplied = .zero
    }

    deinit { restore() }
}

/// The drawing surface: one finger nudges the full display buffer while finger
/// sets become translucent highlighter puddles. Bottom-left origin so the
/// MultitouchSupport normalized space (also bottom-left) maps straight across
/// without a Y flip.
final class ShapedownCanvas: NSView {
    /// A committed shape, held crisp on the wall until close.
    private struct Stamp {
        var points: [CGPoint]     // view space
        var color: NSColor
        var born: TimeInterval    // for the pop-in entrance
    }
    /// A transient color skin — a released puddle settling/fading, or a pinned
    /// shape blooming away as the wall closes. One mechanism for every fade.
    private struct Ghost {
        var points: [CGPoint]
        var color: NSColor
        var born: TimeInterval
        var settle: TimeInterval
        var hold: TimeInterval
        var dur: TimeInterval
        var bloom: CGFloat        // outward swell across the fade (0 = none)
    }
    private var live: [CGPoint] = []          // fingers right now, view space
    private var gestureMemory = ShapedownGestureMemory()
    private var pinned: [Stamp] = []          // committed shapes — held until close
    private var ghosts: [Ghost] = []          // transient color skins / close bloom
    private var singleFilter = ShapedownSinglePointFilter()
    private let displayNudge: ShapedownDisplayNudge
    private var nudgeAnchor: CGPoint?
    private var currentColor = NoteColors.color(pitchClass: 0, octave: 4)
    private var committedThisTouch = false     // don't release-fade a touch already pinned
    private var closing = false

    private var timer: Timer?

    // Tuning.
    private let releaseSettle: TimeInterval = 0.2     // soft landing into the display
    private let releaseHold: TimeInterval = 1.1       // readable puddle before it drains
    private let releaseFade: TimeInterval = 1.6       // slow pixel-soak dissolve
    private let releaseBloom: CGFloat = 0.035         // surface-tension spread
    private let popDur: TimeInterval = 0.22           // commit entrance settle
    private let closeBloomDur: TimeInterval = 0.75    // all commits bloom out together on close
    private let closeBloom: CGFloat = 0.28
    private let nodeRadius: CGFloat = 13               // ONE thickness: dot / line / corner-wrap
    private let maxNudge: CGFloat = 12

    init(frame frameRect: NSRect, displaySurface: ShapedownDisplayBufferView) {
        displayNudge = ShapedownDisplayNudge(surface: displaySurface)
        super.init(frame: frameRect)
    }

    required init?(coder: NSCoder) { nil }

    override var isFlipped: Bool { false }
    override var wantsDefaultClipping: Bool { false }

    private var now: TimeInterval { ProcessInfo.processInfo.systemUptime }

    /// Latch the ink to the exact note color used by Notepat/Menu Band.
    /// The key table carries octave as well as pitch class, so `C` is base red
    /// while upper-row naturals get the brighter dayglo palette.
    func selectColor(forKeyCode keyCode: UInt16) {
        guard let color = ShapedownPalette.color(forKeyCode: keyCode) else { return }
        currentColor = color
        ensureTimer()
        needsDisplay = true
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
        var pts = normalized.map {
            CGPoint(x: min(max($0.x, 0), 1) * w,
                    y: min(max($0.y, 0), 1) * h)
        }

        if pts.isEmpty {
            // Two-plus fingers land as a puddle. Use the peak across the WHOLE
            // gesture, not a short lookback: four contacts often report 3→2→1
            // while lifting, which used to turn a finished quad into a triangle.
            let released = gestureMemory.release(fallback: live)
            if released.count >= 2, !committedThisTouch {
                let total = releaseSettle + releaseHold + releaseFade
                ghosts.append(Ghost(points: released, color: currentColor,
                                    born: now, settle: releaseSettle,
                                    hold: releaseHold, dur: total,
                                    bloom: releaseBloom))
            }
            live = []
            endDisplayNudge()
            singleFilter.reset()
            committedThisTouch = false
        } else {
            if live.isEmpty {
                committedThisTouch = false
                gestureMemory.reset()
                singleFilter.reset()
            }

            if pts.count == 1, gestureMemory.peak.count < 2 {
                let raw = pts[0]
                let smooth = singleFilter.update(raw: raw, canvasSize: bounds.size)
                pts = [smooth]
                updateDisplayNudge(at: smooth)
            } else {
                endDisplayNudge()
                singleFilter.reset()
            }

            // Once an N-finger shape has formed, keep it intact through the
            // staggered lift frames instead of visibly collapsing N→N-1→….
            live = gestureMemory.update(pts)
        }
        ensureTimer()
        needsDisplay = true
    }

    /// The shape a click or lift should commit: the fullest frame seen anywhere
    /// in this gesture. Trackpad clicks and finger lifts both shed contacts one
    /// at a time, so sampling only the last frame loses the intended polygon.
    private func committedShape() -> [CGPoint] {
        gestureMemory.shape(fallback: live)
    }

    private func updateDisplayNudge(at point: CGPoint) {
        guard let anchor = nudgeAnchor else {
            nudgeAnchor = point
            displayNudge.begin()
            return
        }
        let x = min(max((point.x - anchor.x) * 0.045, -maxNudge), maxNudge)
        let y = min(max((point.y - anchor.y) * 0.045, -maxNudge), maxNudge)
        displayNudge.apply(x: x, y: y)
    }

    private func endDisplayNudge() {
        displayNudge.restore()
        nudgeAnchor = nil
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
    /// it pops in crisp and stays until the wall closes. A one-finger nudge
    /// intentionally cannot be pinned. Returns whether a shape was committed.
    @discardableResult
    func pinCurrent() -> Bool {
        let shape = committedShape()
        guard shape.count >= 2 else { return false }
        pinned.append(Stamp(points: shape, color: currentColor, born: now))
        committedThisTouch = true
        needsDisplay = true
        return true
    }

    /// Begin the closing exit: turn every committed shape into a bloom-fade so
    /// they all leave together. Returns the duration so the overlay can match
    /// its own fade to it. Further input is ignored once closing.
    @discardableResult
    func beginClose() -> TimeInterval {
        closing = true
        let t = now
        for p in pinned {
            ghosts.append(Ghost(points: p.points, color: p.color, born: t,
                                settle: 0, hold: 0, dur: closeBloomDur,
                                bloom: closeBloom))
        }
        pinned = []
        live = []
        endDisplayNudge()
        gestureMemory.reset()
        ensureTimer()
        needsDisplay = true
        return closeBloomDur
    }

    func stop() {
        timer?.invalidate()
        timer = nil
        live = []
        gestureMemory.reset()
        pinned = []
        ghosts = []
        endDisplayNudge()
        singleFilter.reset()
        closing = false
    }

    override func draw(_ dirtyRect: NSRect) {
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        let t = now

        // The canvas itself stays perfectly clear: only placed ink affects the
        // display, so Shapedown highlights content instead of covering it.
        // Transient highlighter skins and the closing bloom share one path.
        ghosts.removeAll { t - $0.born > $0.dur }
        for g in ghosts {
            let age = max(0, t - g.born)
            let fadeStart = g.settle + g.hold
            let fadeDuration = max(0.001, g.dur - fadeStart)
            let fade = CGFloat(min(max((age - fadeStart) / fadeDuration, 0), 1))
            let eased = fade * fade * (3 - 2 * fade)          // smoothstep
            let settle = g.settle > 0
                ? CGFloat(min(max(age / g.settle, 0), 1))
                : 1
            let alpha = (0.20 + 0.12 * settle) * (1 - eased)
            let landingScale = 1 + 0.055 * (1 - settle)
            let scale = landingScale + g.bloom * eased
            let pts = scale == 1 ? g.points : scaled(g.points, by: scale)
            drawShape(pts, color: g.color, alpha: alpha, glow: true, ctx: ctx)
        }

        // Committed shapes: crisp and permanent, with a little pop as they land.
        for p in pinned {
            let age = t - p.born
            let alpha: CGFloat
            let scale: CGFloat
            if age < popDur {
                let f = CGFloat(age / popDur)
                alpha = 0.20 + 0.16 * f
                scale = 1 + 0.10 * (1 - f)
            } else {
                alpha = 0.36
                scale = 1
            }
            let pts = scale == 1 ? p.points : scaled(p.points, by: scale)
            drawShape(pts, color: p.color, alpha: alpha, glow: true, ctx: ctx)
        }

        // While fingers are down the multi-finger sketch is the exact, solid
        // selected color with no halo. Highlighter translucency starts only
        // after lift/click. One finger draws nothing; it nudges the display.
        if live.count >= 2 {
            drawShape(live, color: currentColor, alpha: 1, glow: false, ctx: ctx)
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

    private func drawShape(_ points: [CGPoint], color: NSColor, alpha: CGFloat,
                           glow: Bool, ctx: CGContext) {
        guard !points.isEmpty, alpha > 0.001 else { return }
        // Composite the whole shape as ONE group: draw fill + stroke fully
        // OPAQUE inside a transparency layer, then let the layer composite at
        // `alpha`. Opaque-over-opaque unions to flat coverage, so the stroke
        // and fill can't double-blend into a darker seam — the bug that showed
        // as a doubled outline at low alpha. Glow + alpha apply to the layer
        // as a whole, so the glow is drawn once around the union, not per-op.
        let r = nodeRadius

        ctx.saveGState()
        if glow {
            ctx.setShadow(offset: .zero, blur: 24,
                          color: color.withAlphaComponent(1).cgColor)
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
