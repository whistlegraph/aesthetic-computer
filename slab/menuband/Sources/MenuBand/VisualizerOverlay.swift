import AppKit

/// Full-screen LED visualizer — the popover's little scope with everything
/// else taken away. One dot-matrix raster, edge to edge, dark glass all the
/// way out to the corners, the live waveform sweeping across it. No chrome,
/// no buttons, nothing to read: the display IS the mode.
///
/// Two ideas hold it together.
///
/// **You play into it, you don't drive it.** The keyboard still reaches the
/// instrument while the wall is up — that's the whole point of standing back
/// from the machine. Notes route through `handleLocalKey` exactly as they do
/// with the popover open, and Escape is the only key that means "exit."
///
/// **The mouse means "I'm done."** Any real pointer movement tears it down,
/// screensaver-style, so there's no affordance to hunt for and nothing to
/// break the picture. *Real* is the load-bearing word: the pointer is parked
/// on the LED strip the user just clicked, and a hand resting on a trackpad
/// twitches. So the trigger arms after a grace period, re-anchors to wherever
/// the pointer actually settled, and then wants a few points of travel before
/// it fires. Scroll is deliberately NOT a dismissal — it's pitch-bend and
/// octave, and it moves no pointer.
final class VisualizerOverlay: NSObject {
    private weak var menuBand: MenuBandController?
    private var panel: VisualizerOverlayPanel?
    private var strip: WaveformStripView?

    private var keyMonitor: Any?
    private var mouseMonitors: [Any] = []
    private var tintTimer: Timer?
    private var armWorkItem: DispatchWorkItem?

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
    /// LED pitch for the wall — 2× the popover's 5/3.5 pt cells. A laptop
    /// display lands around 140 × 90 lamps, which is both a longer buffer (the
    /// sweep is one column per frame, so more columns = more seconds of music
    /// on screen at once — about 5 s here) and still coarse enough to read as
    /// hardware rather than a plotted line.
    private static let cellStep: CGFloat = 10
    private static let cellSize: CGFloat = 7

    var isShown: Bool { panel?.isVisible == true }

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        super.init()
    }

    /// The wall itself, minus the window around it. `--render-visualizer`
    /// builds its capture through here too, so a headless shot can't drift
    /// from what actually ships.
    static func makeStrip(menuBand: MenuBandController?, size: NSSize) -> WaveformStripView {
        let strip = WaveformStripView(frame: NSRect(origin: .zero, size: size))
        strip.menuBand = menuBand
        strip.autoresizingMask = [.width, .height]
        // The host bezel is the screen itself — no recessed plate, no rounded
        // corners, glass straight out to the edges.
        strip.drawsPlate = false
        strip.setCellGeometry(step: cellStep, size: cellSize)
        return strip
    }

    deinit {
        // Never strand a hidden cursor if we're torn down mid-show.
        if cursorHidden { NSCursor.unhide() }
    }

    func toggle() {
        if isShown { dismiss() } else { show() }
    }

    func show() {
        guard !isShown, let menuBand else { return }
        let mouse = NSEvent.mouseLocation
        let screen = NSScreen.screens.first { NSMouseInRect(mouse, $0.frame, false) }
            ?? NSScreen.main
            ?? NSScreen.screens.first
        guard let screen else { return }

        let strip = Self.makeStrip(menuBand: menuBand, size: screen.frame.size)
        strip.isLive = true
        // Clicking anywhere on the wall exits, same as moving the mouse.
        strip.onClick = { [weak self] in self?.dismiss() }

        let panel = VisualizerOverlayPanel(
            contentRect: screen.frame,
            styleMask: [.borderless],
            backing: .buffered,
            defer: false
        )
        // Above the menubar and the Dock — a visualizer that stopped at the
        // menubar would read as a big window, not as the machine taking over
        // the screen.
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
        panel.contentView = strip
        panel.alphaValue = 0

        self.panel = panel
        self.strip = strip
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
        // user can change voice from the keyboard while the visualizer is up.
        let timer = Timer(timeInterval: 0.25, repeats: true) { [weak self] _ in
            self?.applyTint()
        }
        timer.tolerance = 0.1
        RunLoop.main.add(timer, forMode: .common)
        tintTimer = timer

        arm()
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
        strip?.isLive = false
        menuBand?.releaseAllHeldNotes()
        if cursorHidden {
            NSCursor.unhide()
            cursorHidden = false
        }
        self.panel = nil
        self.strip = nil
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
        guard isArmed, isShown else { return }
        let p = NSEvent.mouseLocation
        guard hypot(p.x - anchor.x, p.y - anchor.y) > Self.dismissSlop else { return }
        dismiss()
    }

    // MARK: - Monitors

    private func installMonitors() {
        let moves: NSEvent.EventTypeMask = [
            .mouseMoved, .leftMouseDragged, .rightMouseDragged, .otherMouseDragged,
        ]
        // Local covers the pointer over our own key panel; global covers the
        // pointer crossing anything else (another Space, a second display).
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
            // Everything else is music. Same routing the expanded keymap panel
            // uses, so the instrument behaves identically with the wall up.
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
        guard let strip, let menuBand else { return }
        if menuBand.sampleRecordingActive {
            strip.tintColor = .systemRed
        } else if menuBand.midiMode {
            strip.tintColor = .controlAccentColor
        } else {
            let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
            strip.tintColor = InstrumentListView.colorForProgram(safe)
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

/// Borderless panels refuse key focus by default. The visualizer needs it —
/// it's a playable surface, and Escape has to land somewhere.
final class VisualizerOverlayPanel: NSPanel {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }
}
