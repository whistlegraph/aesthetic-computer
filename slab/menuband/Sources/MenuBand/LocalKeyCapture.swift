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
        panel = p
    }

    deinit { disarm(reason: .cancelled) }
}

/// `NSPanel` subclass that overrides `canBecomeKey` to return true. Without
/// this, borderless panels silently refuse to become key window — events
/// route nowhere and the system beep fires when we expected note playback.
private final class KeyCapturePanel: NSPanel {
    override var canBecomeKey: Bool { true }
    // canBecomeMain stays default (false). We don't want to look like the
    // primary window — just receive keyboard events.
}
