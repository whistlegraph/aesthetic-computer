import AppKit

/// Sandbox-friendly key capture: an invisible 1×1 NSPanel that becomes the
/// key window after the user clicks the menubar piano. A local NSEvent
/// monitor catches keyDown / keyUp while the panel is key — no
/// `CGEventTap`, no Accessibility prompt, no global hook.
///
/// Tradeoff: this only captures keys while Menu Band is the active app.
/// The moment the user clicks any other app the panel resigns key and
/// capture ends. That's the whole point — it's what makes this MAS-safe.
/// The current global-tap mode (Notepat / Ableton in the popover) survives
/// any focus change, but requires Accessibility and isn't App-Store-eligible.
final class LocalKeyCapture {
    enum EndReason {
        case cancelled
        case resignedKey
    }

    /// Called on every keyDown the panel observes. Return `true` to consume
    /// (so cmd-q etc. can still fall through if false). Receives the same
    /// (keyCode, isDown, isRepeat, flags) shape as the global tap so the
    /// AppDelegate can route both paths through one handler.
    var onKey: ((UInt16, Bool, Bool, NSEvent.ModifierFlags) -> Bool)?
    /// Called when the panel resigns key — capture has ended naturally
    /// because the user clicked another app.
    var onCaptureEnd: ((EndReason) -> Void)?
    /// Fires `true` when one or more fingers rest on the trackpad and
    /// `false` when the last finger lifts. Used by the pitch-bend
    /// gesture so the bend holds (and survives note changes) for as
    /// long as a finger is down, instead of springing back on a
    /// silence timeout or a note release.
    var onTrackpadTouchActiveChanged: ((Bool) -> Void)?
    var cancelShortcut: MenuBandShortcut?

    private var panel: NSPanel?
    private var monitor: Any?
    private var resignKeyObserver: NSObjectProtocol?
    private(set) var isArmed = false

    /// Bring up the invisible panel and start listening. Idempotent — if
    /// already armed, just refreshes the panel as key.
    func arm() {
        if panel == nil { buildPanel() }
        guard let panel = panel else { return }
        // Activate the app + make panel key so keyDown events route here
        // instead of the previously-foreground app. Without `activate`, our
        // panel can't become key and keys go elsewhere.
        NSApp.activate(ignoringOtherApps: true)
        // If another window in our app is already key (e.g. the
        // popover), DON'T steal key — `addLocalMonitorForEvents`
        // catches keys at the app level regardless of which specific
        // window is key, so the panel doesn't need to be key for
        // capture to work. Keeping the popover key prevents its
        // controls from rendering in the inactive/grey state.
        let otherKey = NSApp.windows.contains { $0.isKeyWindow && $0 !== panel }
        if otherKey {
            panel.orderFront(nil)
        } else {
            panel.makeKeyAndOrderFront(nil)
        }
        // Indirect (trackpad) touches are delivered down the key
        // window's responder chain, not by hit-testing — so the
        // sensor view has to be first responder to see resting
        // fingers. Harmless when another window is key (the gesture
        // just falls back to note-release reset in that mode).
        if let sensor = panel.contentView as? TouchSensorView {
            panel.makeFirstResponder(sensor)
        }
        if monitor == nil {
            monitor = NSEvent.addLocalMonitorForEvents(matching: [.keyDown, .keyUp]) { [weak self] event in
                guard let self = self else { return event }
                let isDown = (event.type == .keyDown)
                if isDown, self.cancelShortcut?.matches(event: event) == true {
                    self.disarm(reason: .cancelled)
                    return nil
                }
                let consumed = self.onKey?(event.keyCode, isDown, event.isARepeat, event.modifierFlags) ?? false
                return consumed ? nil : event
            }
        }
        if resignKeyObserver == nil {
            // Disarm only when our APP loses focus (user clicked another
            // app). Don't disarm when key passes to another window inside
            // our own app — opening the popover used to disarm capture
            // here, killing both the typing-on-piano sound AND the ghost-
            // letter wave while the popover was visible.
            resignKeyObserver = NotificationCenter.default.addObserver(
                forName: NSWindow.didResignKeyNotification,
                object: panel, queue: .main
            ) { [weak self] _ in
                guard let self = self else { return }
                let stillKeyInApp = NSApp.windows.contains { $0.isKeyWindow }
                if !stillKeyInApp { self.disarm(reason: .resignedKey) }
            }
        }
        isArmed = true
    }

    /// Tear down the panel + monitor. Called when the user clicks another
    /// app (panel resigns key) or explicitly when we want to drop capture.
    func disarm(reason: EndReason = .cancelled) {
        guard isArmed else { return }
        isArmed = false
        if let m = monitor {
            NSEvent.removeMonitor(m)
            monitor = nil
        }
        if let obs = resignKeyObserver {
            NotificationCenter.default.removeObserver(obs)
            resignKeyObserver = nil
        }
        panel?.orderOut(nil)
        // Capture ended → the sensor stops seeing touches; clear any
        // stale "finger down" so a held bend doesn't strand.
        onTrackpadTouchActiveChanged?(false)
        onCaptureEnd?(reason)
    }

    private func buildPanel() {
        // 1×1 px, off-screen, fully transparent. Borderless NSWindow /
        // NSPanel return `false` from `canBecomeKey` by default —
        // makeKeyAndOrderFront silently fails and keyDown events never
        // route to our local monitor. The KeyCapturePanel subclass below
        // overrides that so the panel actually becomes key.
        let p = KeyCapturePanel(
            contentRect: NSRect(x: -1000, y: -1000, width: 1, height: 1),
            styleMask: [.borderless, .nonactivatingPanel],
            backing: .buffered, defer: false
        )
        p.isOpaque = false
        p.backgroundColor = .clear
        p.alphaValue = 0
        p.hasShadow = false
        p.level = .statusBar
        p.hidesOnDeactivate = false
        p.canHide = false
        p.acceptsMouseMovedEvents = false
        // Indirect-touch sensor lives as the panel's content view so it
        // sits in the key window's responder chain while the user plays.
        let sensor = TouchSensorView(frame: NSRect(x: 0, y: 0, width: 1, height: 1))
        sensor.onActiveChanged = { [weak self] active in
            self?.onTrackpadTouchActiveChanged?(active)
        }
        p.contentView = sensor
        // Rob ⌘/⌥/⌃+letter combos from the system. The local keyDown monitor
        // (above) doesn't reliably see modifier key-equivalents, and the
        // morph path only fires once a bare note is already held. This makes
        // a SIMULTANEOUS ⌘+c (major) / ⌥+c (minor) chord consistent — and,
        // for mapped note keys, deliberately shadows Cut/Copy/Paste/Undo/Save
        // while quiet-focus is armed (returns true → the system shortcut
        // never fires; ⌘+note plays the chord, ⌘ never reaches the menu).
        // Non-note combos (⌘-Tab, ⌘-Q) fall through to `super` and behave
        // normally.
        p.keyEquivalentHandler = { [weak self] event in
            guard let self = self else { return false }
            let flags = event.modifierFlags
            guard flags.contains(.command) || flags.contains(.control)
                || flags.contains(.option) else { return false }
            return self.onKey?(event.keyCode, true, event.isARepeat, flags) ?? false
        }
        panel = p
    }

    deinit { disarm(reason: .cancelled) }
}

/// Invisible view that reports whether any finger is currently resting
/// on the trackpad. Indirect touches (the Magic Trackpad / built-in
/// trackpad) only reach a view that opts in via `allowedTouchTypes`
/// and is in the key window's responder chain; `wantsRestingTouches`
/// keeps a motionless finger counted instead of dropping it the moment
/// it stops moving (the exact case that was snapping the bend back).
final class TouchSensorView: NSView {
    var onActiveChanged: ((Bool) -> Void)?
    /// Primary finger's absolute trackpad position (0…1 each axis, origin
    /// bottom-left), nil when no finger touches — the clean signal for a
    /// flat-mapped XY pad (no pointer acceleration).
    var onTouchXY: ((CGPoint?) -> Void)?
    private var lastActive = false
    private var loggedCount = 0

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        allowedTouchTypes = [.indirect]
        wantsRestingTouches = true
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        allowedTouchTypes = [.indirect]
        wantsRestingTouches = true
    }

    override var acceptsFirstResponder: Bool { true }

    // Pure touch listener — never intercept the mouse. Returning nil
    // from hitTest keeps clicks/drag flowing through to whatever
    // control sits underneath (the popover's keys, buttons, grid),
    // while indirect (trackpad) touches still arrive via the key
    // window's responder chain because the view is first responder.
    override func hitTest(_ point: NSPoint) -> NSView? { nil }

    override func touchesBegan(with event: NSEvent) { recompute(event) }
    override func touchesMoved(with event: NSEvent) { recompute(event) }
    override func touchesEnded(with event: NSEvent) { recompute(event) }
    override func touchesCancelled(with event: NSEvent) { recompute(event) }

    private func recompute(_ event: NSEvent) {
        let touches = event.touches(matching: .touching, in: self)
        let active = !touches.isEmpty
        // [prototype] Report + log the primary finger's absolute position so we
        // can confirm whether indirect-touch delivery actually reaches this view
        // during the bend (the bend path warns it was historically unreliable).
        if let t = touches.first(where: { $0.type == .indirect }) ?? touches.first {
            let p = t.normalizedPosition
            onTouchXY?(p)
            if loggedCount < 60 {
                loggedCount += 1
                NSLog("MenuBand TouchXY: x=%.3f y=%.3f  fingers=%d  firstResponder=%@",
                      p.x, p.y, touches.count,
                      (window?.firstResponder === self) ? "self" : "OTHER")
            }
        } else {
            onTouchXY?(nil)
        }
        guard active != lastActive else { return }
        lastActive = active
        onActiveChanged?(active)
    }
}

/// `NSPanel` subclass that overrides `canBecomeKey` to return true. Without
/// this, borderless panels silently refuse to become key window — events
/// route nowhere and the system beep fires when we expected note playback.
private final class KeyCapturePanel: NSPanel {
    override var canBecomeKey: Bool { true }
    // canBecomeMain stays default (false). We don't want to look like the
    // primary window — just receive keyboard events.

    /// Routes ⌘/⌥+letter key-equivalents to the note path. Returns true to
    /// consume (note key → chord, shadowing the system command); false lets
    /// the combo pass through unchanged. See `buildPanel`.
    var keyEquivalentHandler: ((NSEvent) -> Bool)?
    override func performKeyEquivalent(with event: NSEvent) -> Bool {
        if let handler = keyEquivalentHandler, handler(event) { return true }
        return super.performKeyEquivalent(with: event)
    }
}
