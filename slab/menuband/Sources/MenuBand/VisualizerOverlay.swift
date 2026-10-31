import AppKit

/// Full-screen jam wall — the desktop taken over by one lava lamp (see
/// `JamWallView`: melody blobs on the left, drum bursts on the right, the
/// level breathing up the seam). It covers everything but the menu bar, so
/// the piano stays visible up top: the wall is the piano's display, not a
/// window in front of it.
///
/// Two ways in, two postures:
///
/// **`^` (Shift+6, "hat") while playing.** The wall comes up with hands on
/// keys and goes down the same way. Nothing about the pointer matters here —
/// a hand resting on the trackpad, or drumming on it, must not tear the
/// picture down. Escape also exits.
///
/// **Clicking a scope** (the popover's LED strip, the keymap window's big
/// one). The old posture: the mouse means "I'm done." Any real pointer
/// travel dismisses, screensaver-style, after a grace period that lets the
/// hand come off the mouse. *Real* is the load-bearing word: the trigger
/// re-anchors to wherever the pointer settled and wants a few points of
/// travel before it fires. Scroll never dismisses — it's pitch-bend and
/// octave, and moves no pointer.
///
/// Either way the keyboard still reaches the instrument: notes route through
/// `handleLocalKey` exactly as they do with the popover open.
final class VisualizerOverlay: NSObject {
    private weak var menuBand: MenuBandController?
    private var panel: VisualizerOverlayPanel?
    private var wall: JamWallView?

    private var keyMonitor: Any?
    private var mouseMonitors: [Any] = []
    private var tintTimer: Timer?
    private var armWorkItem: DispatchWorkItem?

    /// Whether this showing lets the pointer dismiss (scope-click entry) or
    /// only the keyboard (`^` / Escape / the sibling notification).
    private var pointerDismisses = false
    /// Pointer position the dismissal measures against. Travel is compared to
    /// this fixed anchor rather than frame-to-frame, so a slow deliberate
    /// drift still accumulates into a dismiss instead of sneaking under a
    /// per-event threshold forever.
    private var anchor: NSPoint = .zero
    private var isArmed = false
    private var cursorHidden = false
    /// Whoever was frontmost before we activated, so exiting hands focus back
    /// rather than dumping the user in a menubar-only app.
    private var appBeforeOpen: NSRunningApplication?

    /// Beat of stillness before the pointer can dismiss. Long enough to cover
    /// the popover's fade-out and the hand coming off the mouse.
    private static let armDelay: TimeInterval = 0.45
    /// Pointer travel from `anchor` that counts as intent to leave.
    private static let dismissSlop: CGFloat = 6
    var isShown: Bool { panel?.isVisible == true }

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        super.init()
    }

    /// The wall itself, minus the window around it. `--render-visualizer`
    /// builds its capture through here too, so a headless shot can't drift
    /// from what actually ships.
    static func makeWall(menuBand: MenuBandController?, size: NSSize) -> JamWallView {
        let wall = JamWallView(frame: NSRect(origin: .zero, size: size))
        wall.menuBand = menuBand
        wall.autoresizingMask = [.width, .height]
        return wall
    }

    deinit {
        // Never strand a hidden cursor if we're torn down mid-show.
        if cursorHidden { NSCursor.unhide() }
    }

    /// `fromKeyboard` picks the posture (see the class note). A toggle that
    /// finds the wall up takes it down regardless of how it came up.
    func toggle(fromKeyboard: Bool = false) {
        if isShown { dismiss() } else { show(fromKeyboard: fromKeyboard) }
    }

    func show(fromKeyboard: Bool = false) {
        guard !isShown, let menuBand else { return }
        let mouse = NSEvent.mouseLocation
        let screen = NSScreen.screens.first { NSMouseInRect(mouse, $0.frame, false) }
            ?? NSScreen.main
            ?? NSScreen.screens.first
        guard let screen else { return }

        // Everything below the menu bar — the Dock included — and not one
        // point more: the piano up top stays in view, playing along.
        let menuBarGap = max(0, screen.frame.maxY - screen.visibleFrame.maxY)
        let frame = NSRect(x: screen.frame.minX, y: screen.frame.minY,
                           width: screen.frame.width,
                           height: screen.frame.height - menuBarGap)

        let wall = Self.makeWall(menuBand: menuBand, size: frame.size)
        wall.isLive = true
        pointerDismisses = !fromKeyboard
        // Clicking the wall exits in the mouse posture, same as moving it. In
        // the keyboard posture a trackpad tap is a drum, not a door.
        wall.onClick = { [weak self] in
            guard let self, self.pointerDismisses else { return }
            self.dismiss()
        }
        // Notes already ringing when the wall comes up draw from this frame.
        for n in menuBand.litNotes { wall.noteOn(n, velocity: 96) }

        let panel = VisualizerOverlayPanel(
            contentRect: frame,
            styleMask: [.borderless],
            backing: .buffered,
            defer: false
        )
        // Above the Dock and any floating palette — the machine taking over
        // the desktop, not a big window on it.
        panel.level = .screenSaver
        panel.backgroundColor = .black
        panel.isOpaque = true
        panel.hasShadow = false
        panel.animationBehavior = .none
        panel.isMovable = false
        panel.acceptsMouseMovedEvents = true
        panel.isReleasedWhenClosed = false
        panel.hidesOnDeactivate = false
        panel.collectionBehavior = [
            .canJoinAllSpaces,
            .fullScreenAuxiliary,
            .stationary,
            .ignoresCycle,
        ]
        panel.contentView = wall
        panel.alphaValue = 0

        self.panel = panel
        self.wall = wall
        applyTint()

        appBeforeOpen = frontmostOtherApp()
        NSApp.activate(ignoringOtherApps: true)
        panel.makeKeyAndOrderFront(nil)
        NSAnimationContext.runAnimationGroup { ctx in
            ctx.duration = 0.18
            ctx.timingFunction = CAMediaTimingFunction(name: .easeOut)
            panel.animator().alphaValue = 1
        }

        if !cursorHidden {
            NSCursor.hide()
            cursorHidden = true
        }

        installMonitors()
        // Keep the wall's color on the voice: the tint follows the active
        // instrument family (red while sampling, accent in MIDI mode), and the
        // user can change voice from the keyboard while the wall is up.
        let timer = Timer(timeInterval: 0.25, repeats: true) { [weak self] _ in
            self?.applyTint()
        }
        timer.tolerance = 0.1
        RunLoop.main.add(timer, forMode: .common)
        tintTimer = timer

        if pointerDismisses { arm() }
    }

    func dismiss() {
        guard let panel else { return }
        removeMonitors()
        armWorkItem?.cancel()
        armWorkItem = nil
        isArmed = false
        tintTimer?.invalidate()
        tintTimer = nil
        // Stop the capture client before the fade so the synth's tap can wind
        // down with the display rather than 140 ms after it.
        wall?.isLive = false
        // Mouse posture: the wall's own key monitor was the only thing
        // hearing key-ups, so let go of anything it was holding. Keyboard
        // posture: the route that opened the wall is still live, and a chord
        // the other hand is holding through `^` must keep ringing.
        if pointerDismisses { menuBand?.releaseAllHeldNotes() }
        if cursorHidden {
            NSCursor.unhide()
            cursorHidden = false
        }
        self.panel = nil
        self.wall = nil
        NSAnimationContext.runAnimationGroup({ ctx in
            ctx.duration = 0.14
            ctx.timingFunction = CAMediaTimingFunction(name: .easeOut)
            panel.animator().alphaValue = 0
        }, completionHandler: {
            panel.orderOut(nil)
            panel.contentView = nil
        })
        restorePreviousAppFocus()
    }

    // MARK: - Feed (main thread)

    /// Melodic note-on/off from the synth's note hook, routed by the
    /// AppDelegate. Cheap no-ops while the wall is down.
    func noteEvent(midi: UInt8, velocity: UInt8, on: Bool) {
        guard let wall else { return }
        if on { wall.noteOn(midi, velocity: velocity) } else { wall.noteOff(midi) }
    }

    func drumHit(_ drum: MenuBandPercussion.Drum, velocity: UInt8) {
        wall?.drumHit(drum, velocity: velocity)
    }

    // MARK: - Dismissal arming

    /// Wait out the grace period, then re-anchor to wherever the pointer
    /// actually is. Re-anchoring matters: between the click and now the
    /// popover faded and the user may have lifted or nudged the mouse, and
    /// measuring from the stale click point would fire immediately.
    private func arm() {
        armWorkItem?.cancel()
        let work = DispatchWorkItem { [weak self] in
            guard let self, self.isShown else { return }
            self.anchor = NSEvent.mouseLocation
            self.isArmed = true
        }
        armWorkItem = work
        DispatchQueue.main.asyncAfter(deadline: .now() + Self.armDelay, execute: work)
    }

    private func pointerMoved() {
        guard pointerDismisses, isArmed, isShown else { return }
        let p = NSEvent.mouseLocation
        guard hypot(p.x - anchor.x, p.y - anchor.y) > Self.dismissSlop else { return }
        dismiss()
    }

    // MARK: - Monitors

    private func installMonitors() {
        if pointerDismisses {
            let moves: NSEvent.EventTypeMask = [
                .mouseMoved, .leftMouseDragged, .rightMouseDragged, .otherMouseDragged,
            ]
            // Local covers the pointer over our own key panel; global covers
            // the pointer crossing anything else (another Space, a second
            // display).
            if let m = NSEvent.addLocalMonitorForEvents(matching: moves, handler: { [weak self] event in
                self?.pointerMoved()
                return event
            }) {
                mouseMonitors.append(m)
            }
            if let m = NSEvent.addGlobalMonitorForEvents(matching: moves, handler: { [weak self] _ in
                self?.pointerMoved()
            }) {
                mouseMonitors.append(m)
            }
        }

        // Deliberately NOT gated on `panel.isKeyWindow`. LocalKeyCapture arms
        // an invisible panel that can hold key while the user plays, and
        // requiring key here would leave Escape with nowhere to land. While
        // the wall is up it owns the interaction, whichever of our windows is
        // key.
        //
        // No double-trigger with LocalKeyCapture's own monitor, either: it is
        // registered first, and a local monitor returning nil ends the chain.
        // Note keys it consumes never reach us; Escape it declines does.
        keyMonitor = NSEvent.addLocalMonitorForEvents(matching: [.keyDown, .keyUp]) { [weak self] event in
            guard let self, self.isShown else { return event }
            let isDown = event.type == .keyDown
            if isDown, event.keyCode == 53 {   // Escape
                self.dismiss()
                return nil
            }
            // Everything else is music — `^` included, which the controller
            // turns back into a toggle. Same routing the expanded keymap
            // panel uses, so the instrument behaves identically with the
            // wall up.
            let consumed = self.menuBand?.handleLocalKey(
                keyCode: event.keyCode,
                isDown: isDown,
                isRepeat: event.isARepeat,
                flags: event.modifierFlags
            ) ?? false
            return consumed ? nil : event
        }
    }

    private func removeMonitors() {
        for m in mouseMonitors { NSEvent.removeMonitor(m) }
        mouseMonitors.removeAll()
        if let keyMonitor {
            NSEvent.removeMonitor(keyMonitor)
            self.keyMonitor = nil
        }
    }

    // MARK: - Tint

    /// Mirrors `ExpandedPianoWaveformView.applyWaveformTint` — the wall wears
    /// the same color the little scope would.
    private func applyTint() {
        guard let wall, let menuBand else { return }
        if menuBand.sampleRecordingActive {
            wall.tintColor = .systemRed
        } else if menuBand.midiMode {
            wall.tintColor = .controlAccentColor
        } else {
            let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
            wall.tintColor = InstrumentListView.colorForProgram(safe)
        }
    }

    // MARK: - Focus

    private func frontmostOtherApp() -> NSRunningApplication? {
        let frontmost = NSWorkspace.shared.frontmostApplication
        guard frontmost?.bundleIdentifier != Bundle.main.bundleIdentifier else { return nil }
        return frontmost
    }

    private func restorePreviousAppFocus() {
        guard let app = appBeforeOpen,
              !app.isTerminated,
              app.bundleIdentifier != Bundle.main.bundleIdentifier else { return }
        appBeforeOpen = nil
        app.activate(options: [])
    }
}

/// Borderless panels refuse key focus by default. The wall needs it — it's
/// a playable surface, and Escape has to land somewhere.
final class VisualizerOverlayPanel: NSPanel {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }
}
