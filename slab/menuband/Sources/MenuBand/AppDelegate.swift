import AppKit
import Carbon

final class AppDelegate: NSObject, NSApplicationDelegate {
    private var statusItem: NSStatusItem!
    private let menuBand = MenuBandController()
    private let hoverResponder = HoverResponder()
    private var hoveredElement: KeyboardIconRenderer.HitResult? = nil
    private var trackingArea: NSTrackingArea?
    private var typeModeHotkey: GlobalHotkey?
    private let popover = NSPopover()
    private var popoverVC: MenuBandPopoverViewController?

    /// Periodic check that the status item is actually visible in the
    /// menu bar. macOS silently hides items when there's no room (notch +
    /// many menubar apps). When that happens we shrink the layout —
    /// full piano → 1 octave → compact chip — until something fits.
    private var visibilityTimer: Timer?
    /// Set to true once we've shown the "no room even for compact" alert
    /// so we don't spam the user every check.
    private var hasAlertedNoSpace = false

    /// Click-away monitor active while the popover is shown. Catches clicks
    /// on OTHER apps and dismisses the popover. Clicks on our status-item
    /// button stay in-app and route through `statusClicked`, which only
    /// closes the popover when the user clicks the settings chip — piano
    /// taps keep the popover open.
    private var clickAwayMonitor: Any?
    private var popoverEscMonitor: Any?

    /// Sandbox-friendly local key capture. Armed when the user clicks the
    /// menubar piano (without opening the popover). The companion ghost
    /// timer paints letter labels on the menubar keys briefly when armed
    /// or when an actual key is pressed — visual signal that "you're
    /// capturing, type now."
    private let localCapture = LocalKeyCapture()
    private var ghostUntil: CFTimeInterval = 0
    private var ghostRefreshTimer: Timer?

    /// Letter-wave state. Pivot is the most recently lit display note;
    /// `phaseStartedAt` is when the current direction began; `fadingIn`
    /// indicates direction. The renderer queries per-cell alpha derived
    /// from these — fade-in ripples outward from the pivot, fade-out
    /// retreats inward (far cells fade first, the pivot last).
    private var letterPivot: UInt8 = 60
    private var letterPhaseStartedAt: CFTimeInterval = 0
    private var letterFadingIn: Bool = false
    private var letterFadeTimer: Timer?
    private static let letterWaveStep: CFTimeInterval = 0.025  // 25 ms per cell of distance
    private static let letterFadeInDur: CFTimeInterval = 0.08
    private static let letterFadeOutDur: CFTimeInterval = 0.18

    func applicationDidFinishLaunching(_ notification: Notification) {
        debugLog("applicationDidFinishLaunching pid=\(ProcessInfo.processInfo.processIdentifier)")
        Timer.scheduledTimer(withTimeInterval: 5.0, repeats: true) { _ in
            debugLog("heartbeat")
        }
        menuBand.onChange = { [weak self] in
            DispatchQueue.main.async { self?.updateIcon() }
        }
        menuBand.onLitChanged = { [weak self] in
            self?.updateIcon()
        }
        menuBand.bootstrap()

        statusItem = NSStatusBar.system.statusItem(withLength: KeyboardIconRenderer.imageSize.width)
        debugLog("statusItem created, button=\(statusItem.button != nil) length=\(statusItem.length)")
        if let button = statusItem.button {
            let cell = NoHighlightStatusBarCell()
            cell.imagePosition = .imageOnly
            cell.isBordered = false
            cell.highlightsBy = []
            button.cell = cell
            button.imagePosition = .imageOnly
            button.target = self
            button.action = #selector(statusClicked(_:))
            button.sendAction(on: [.leftMouseDown, .rightMouseDown])
            button.isBordered = false

            hoverResponder.onMove = { [weak self] ev in self?.handleHover(event: ev) }
            hoverResponder.onExit = { [weak self] in self?.handleHoverExit() }
            let area = NSTrackingArea(
                rect: button.bounds,
                options: [.mouseMoved, .mouseEnteredAndExited,
                          .activeAlways, .inVisibleRect],
                owner: hoverResponder, userInfo: nil
            )
            button.addTrackingArea(area)
            trackingArea = area
        }
        updateIcon()

        // Global hotkey: Ctrl+Opt+Cmd+P toggles TYPE.
        let hotkey = GlobalHotkey { [weak self] in
            self?.menuBand.toggleTypeMode()
        }
        let modMask: UInt32 = UInt32(cmdKey | controlKey | optionKey)
        hotkey.register(keyCode: UInt32(kVK_ANSI_P), modifiers: modMask)
        typeModeHotkey = hotkey

        // Local key capture wiring. Routes keys to the same note logic the
        // global tap uses, with the ghost-label flash on every press so the
        // user sees the layout dynamically appear while typing.
        localCapture.onKey = { [weak self] keyCode, isDown, isRepeat, flags in
            guard let self = self else { return false }
            // Escape disarms capture explicitly. Useful when the user
            // wants to release focus without clicking another app.
            if isDown && keyCode == 53 /* kVK_Escape */ {
                NSSound(named: NSSound.Name("Tink"))?.play()
                self.localCapture.disarm()
                return true
            }
            let consumed = self.menuBand.handleLocalKey(
                keyCode: keyCode, isDown: isDown, isRepeat: isRepeat, flags: flags
            )
            if consumed && isDown {
                // Use the most-recent lit display note as the wave pivot
                // so the ripple emanates from whichever key the user just
                // played. `litNotes` is updated synchronously on this
                // thread, so it already contains the new note.
                let pivot = self.menuBand.litNotes.max() ?? 60
                DispatchQueue.main.async { self.extendGhost(0.4, pivot: pivot) }
            }
            return consumed
        }
        localCapture.onCaptureEnd = { [weak self] in
            // Focus lost (user clicked another app). Drop the ghost and
            // any held notes so we don't leave anything hanging.
            self?.ghostUntil = 0
            self?.ghostRefreshTimer?.invalidate()
            self?.ghostRefreshTimer = nil
            self?.updateIcon()
        }

        // Pre-instance the popover + force its view to load now so the
        // first click pops it instantly. With `animates = false` the
        // open/close has no transition — it's a snap, much more "playable"
        // for quickly toggling between the menubar piano and the picker.
        let vc = MenuBandPopoverViewController()
        vc.menuBand = menuBand
        vc.popover = popover
        popoverVC = vc
        popover.contentViewController = vc
        // .applicationDefined: never auto-close. We manage closing manually
        // so clicking a menubar piano key (which would normally count as
        // "outside" the popover under .transient) doesn't dismiss the
        // popover while the user is playing.
        popover.behavior = .applicationDefined
        popover.animates = false
        _ = vc.view

        startAdaptiveLayoutChecks()

        // Retint the bundle's Finder icon to the user's accent color.
        // Stored as an xattr on the bundle folder, so the signed payload
        // isn't modified. Refreshed whenever the accent changes.
        IconTinter.applyTintedIcon()
        NotificationCenter.default.addObserver(
            forName: NSColor.systemColorsDidChangeNotification,
            object: nil, queue: .main
        ) { _ in
            IconTinter.applyTintedIcon()
        }
    }

    // MARK: - Adaptive menubar layout

    /// `true` when the status item button is laid out on a real screen.
    /// macOS leaves the button's window assigned but with `screen == nil`
    /// when there's no room for it in the menu bar.
    private func isStatusItemVisible() -> Bool {
        guard let button = statusItem.button else { return false }
        guard let window = button.window else { return false }
        if window.screen == nil { return false }
        return NSScreen.screens.contains { $0.frame.intersects(window.frame) }
    }

    private func startAdaptiveLayoutChecks() {
        // Initial fit pass — give the system a beat to lay out before
        // probing visibility.
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.4) { [weak self] in
            self?.adaptLayoutForAvailableSpace()
        }
        // Periodic re-check. Menubar real-estate changes throughout the
        // day as apps come and go; 6s polling is cheap and lets us both
        // shrink (when squeezed) and re-expand (when room opens up).
        visibilityTimer?.invalidate()
        visibilityTimer = Timer.scheduledTimer(withTimeInterval: 6.0, repeats: true) { [weak self] _ in
            self?.adaptLayoutForAvailableSpace()
        }
    }

    private func adaptLayoutForAvailableSpace() {
        let current = KeyboardIconRenderer.displayLayout
        let visible = isStatusItemVisible()

        if !visible {
            // Shrink to the next-smaller layout. If we're already at
            // .compact and STILL not visible, alert the user once.
            if let smaller = current.smaller {
                debugLog("statusItem hidden — shrinking \(current) → \(smaller)")
                KeyboardIconRenderer.displayLayout = smaller
                updateIcon()
            } else if !hasAlertedNoSpace {
                hasAlertedNoSpace = true
                DispatchQueue.main.async { [weak self] in self?.alertNoMenuBarSpace() }
            }
            return
        }

        // Visible. If we previously shrunk, try to expand back. Set the
        // larger layout, force a layout, then re-check; revert if we
        // lost the slot.
        guard let bigger = current.larger else { return }
        KeyboardIconRenderer.displayLayout = bigger
        updateIcon()
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.25) { [weak self] in
            guard let self = self else { return }
            if !self.isStatusItemVisible() {
                debugLog("expand \(current) → \(bigger) didn't fit; reverting")
                KeyboardIconRenderer.displayLayout = current
                self.updateIcon()
            } else {
                debugLog("statusItem expanded \(current) → \(bigger)")
            }
        }
    }

    private func alertNoMenuBarSpace() {
        let alert = NSAlert()
        alert.messageText = "Menu Band can't fit in your menu bar"
        alert.informativeText = """
            There's no room in your menu bar — even for the compact icon. \
            Try quitting an app that puts items in the menu bar (slack, \
            dropbox, etc.), or use Bartender / Hidden Bar to manage them.

            Menu Band will keep trying every few seconds.
            """
        alert.alertStyle = .informational
        alert.addButton(withTitle: "OK")
        NSApp.activate(ignoringOtherApps: true)
        alert.runModal()
    }

    func applicationWillTerminate(_ notification: Notification) {
        menuBand.shutdown()
    }

    private func updateIcon() {
        guard let button = statusItem.button else { return }
        // Use *effective* keymap/typeMode so popover hover-preview can
        // override the live state without writing to UserDefaults. The
        // renderer keeps `imageSize` constant across keymaps (always the
        // 2-octave Notepat layout area) — Ableton is drawn with negative
        // space on the right — so the status item slot never resizes and
        // the popover anchor stays put.
        //
        // Ghost-label flash: when the user clicks the menubar piano (or
        // types while armed), letters render on the keys for ~0.5–0.7 s.
        // It's a temporal "you're capturing right now" hint, not a
        // permanent overlay.
        KeyboardIconRenderer.activeKeymap = menuBand.keymap
        statusItem.length = KeyboardIconRenderer.imageSize.width
        button.image = KeyboardIconRenderer.image(
            litNotes: menuBand.litNotes,
            enabled: menuBand.midiMode,
            typeMode: menuBand.typeMode,
            melodicProgram: menuBand.melodicProgram,
            hovered: hoveredElement,
            letterAlpha: { [weak self] midi in
                self?.letterAlpha(for: midi) ?? 0
            }
        )
        // Force a synchronous redraw — the click drag-loop runs the runloop
        // in `eventTracking` mode and has been swallowing the next CA flush
        // until mouseUp. Without this, key blinks and hover highlights only
        // appeared after the user released the mouse.
        button.needsDisplay = true
        button.displayIfNeeded()
    }

    /// Extend the letter ghost duration and pivot the wave at the given
    /// display note. Each keypress restarts (or extends) the fade-in
    /// from that pivot; when the ghost expires the wave reverses outward
    /// (far cells fade out first). The animation tick drives 60 Hz
    /// redraws while the fade is in progress, then auto-stops.
    private func extendGhost(_ duration: CFTimeInterval, pivot: UInt8? = nil) {
        let now = CACurrentMediaTime()
        let target = now + duration
        if target > ghostUntil { ghostUntil = target }
        if let pivot = pivot { letterPivot = pivot }
        // Restart the fade-in phase from "now" so cells progressively
        // re-attack from the new pivot. Already-bright cells stay bright
        // because the alpha formula is monotonic for time within a phase.
        if !letterFadingIn {
            letterFadingIn = true
            letterPhaseStartedAt = now
        }
        ghostRefreshTimer?.invalidate()
        ghostRefreshTimer = Timer.scheduledTimer(withTimeInterval: duration + 0.02, repeats: false) { [weak self] _ in
            self?.startLetterFadeOut()
        }
        startLetterFadeTickIfNeeded()
        updateIcon()
    }

    private func startLetterFadeOut() {
        letterFadingIn = false
        letterPhaseStartedAt = CACurrentMediaTime()
        startLetterFadeTickIfNeeded()
        updateIcon()
    }

    private func startLetterFadeTickIfNeeded() {
        guard letterFadeTimer == nil else { return }
        letterFadeTimer = Timer.scheduledTimer(withTimeInterval: 1.0/60.0, repeats: true) { [weak self] _ in
            self?.tickLetterFade()
        }
    }

    private func tickLetterFade() {
        // Stable when fade-out has had enough time for the slowest
        // (closest-to-pivot) cell to reach 0. Stop the timer there to
        // avoid burning idle CPU.
        let now = CACurrentMediaTime()
        let phase = now - letterPhaseStartedAt
        let maxDist: CFTimeInterval = 24
        let stableAt = letterFadingIn
            ? maxDist * Self.letterWaveStep + Self.letterFadeInDur
            : maxDist * Self.letterWaveStep + Self.letterFadeOutDur
        if !letterFadingIn && phase >= stableAt {
            letterFadeTimer?.invalidate()
            letterFadeTimer = nil
        }
        updateIcon()
    }

    /// Per-cell alpha based on radial distance from the pivot. Fade-in
    /// starts at the pivot and ripples outward; fade-out starts at the
    /// outermost cell and retreats toward the pivot.
    private func letterAlpha(for midi: UInt8) -> CGFloat {
        let dist = CFTimeInterval(abs(Int(midi) - Int(letterPivot)))
        let phase = CACurrentMediaTime() - letterPhaseStartedAt
        if letterFadingIn {
            let cellStart = dist * Self.letterWaveStep
            let t = (phase - cellStart) / Self.letterFadeInDur
            return CGFloat(max(0, min(1, t)))
        } else {
            // Fade-out delay: far cells (large dist) start first.
            let maxDist: CFTimeInterval = 24
            let cellStart = (maxDist - dist) * Self.letterWaveStep
            let t = (phase - cellStart) / Self.letterFadeOutDur
            return CGFloat(max(0, min(1, 1 - t)))
        }
    }

    // MARK: - Hover

    private var lastHoverLogTime: TimeInterval = 0
    private func handleHover(event: NSEvent) {
        let now = CACurrentMediaTime()
        if now - lastHoverLogTime > 1.0 {
            lastHoverLogTime = now
            debugLog("handleHover (1Hz throttle)")
        }
        handleHoverInner(event: event)
    }

    private func handleHoverInner(event: NSEvent) {
        guard let button = statusItem.button else { return }
        let imgSize = KeyboardIconRenderer.imageSize
        let bb = button.bounds
        let xOff = (bb.width - imgSize.width) / 2.0
        let yOff = (bb.height - imgSize.height) / 2.0
        let local = button.convert(event.locationInWindow, from: nil)
        let yLocal = button.isFlipped ? (bb.height - local.y) : local.y
        let pt = NSPoint(x: local.x - xOff, y: yLocal - yOff)
        let result = KeyboardIconRenderer.hit(at: pt)
        if hoveredElement != result {
            hoveredElement = result
            updateIcon()
        }
    }

    private func handleHoverExit() {
        if hoveredElement != nil {
            hoveredElement = nil
            updateIcon()
        }
    }

    // MARK: - Click + drag

    @objc private func statusClicked(_ sender: Any?) {
        guard let button = statusItem.button else { return }
        let event = NSApp.currentEvent
        let isRight = event?.type == .rightMouseDown
        let isCtrl = event?.modifierFlags.contains(.control) ?? false
        debugLog("statusClicked type=\(event?.type.rawValue.description ?? "nil") isRight=\(isRight) isCtrl=\(isCtrl) midiMode=\(menuBand.midiMode)")

        if hoveredElement != nil {
            hoveredElement = nil
            updateIcon()
        }

        if isRight || isCtrl {
            showPopover()
            return
        }
        guard let downEvent = event, downEvent.type == .leftMouseDown else { return }

        let imgSize = KeyboardIconRenderer.imageSize
        let bb = button.bounds
        let xOff = (bb.width - imgSize.width) / 2.0
        let yOff = (bb.height - imgSize.height) / 2.0
        func imagePoint(from windowPoint: NSPoint) -> NSPoint {
            let local = button.convert(windowPoint, from: nil)
            let yLocal = button.isFlipped ? (bb.height - local.y) : local.y
            return NSPoint(x: local.x - xOff, y: yLocal - yOff)
        }

        let initialHitPt = imagePoint(from: downEvent.locationInWindow)
        let initial = KeyboardIconRenderer.hit(at: initialHitPt)
        debugLog("hit pt=(\(initialHitPt.x),\(initialHitPt.y)) -> \(String(describing: initial))")
        let startNote: UInt8
        switch initial {
        case .openSettings:
            showPopover()
            return
        case .note(let n):
            startNote = n
        case .none:
            return
        }

        let initialPt = imagePoint(from: downEvent.locationInWindow)
        let (vel0, pan0) = expression(for: startNote, at: initialPt)
        menuBand.startTapNote(startNote, velocity: vel0, pan: pan0)
        // Arm sandbox-friendly local capture on a real piano click. We
        // skip arming when global TYPE mode is already on — the global
        // tap is already handling keys, doubling up would re-trigger
        // every note. No letter flash on click: the label overlay is
        // reserved for actual key presses, so the menubar stays clean
        // when you're just tapping the piano with the mouse.
        if !menuBand.typeMode {
            localCapture.arm()
        }
        var current: UInt8? = startNote
        while let next = NSApp.nextEvent(
            matching: [.leftMouseDragged, .leftMouseUp],
            until: .distantFuture,
            inMode: .eventTracking,
            dequeue: true
        ) {
            if next.type == .leftMouseUp {
                if let c = current { menuBand.stopTapNote(c) }
                break
            }
            let pt = imagePoint(from: next.locationInWindow)
            let hovered = KeyboardIconRenderer.noteAt(pt)
            if hovered != current {
                if let prev = current { menuBand.stopTapNote(prev) }
                if let nxt = hovered {
                    let (v, p) = expression(for: nxt, at: pt)
                    menuBand.startTapNote(nxt, velocity: v, pan: p)
                }
                current = hovered
            } else if let c = current {
                let (_, p) = expression(for: c, at: pt)
                menuBand.updateTapPan(c, pan: p)
            }
        }
    }

    private func expression(for midiNote: UInt8, at pt: NSPoint) -> (UInt8, UInt8) {
        guard let rect = KeyboardIconRenderer.keyRect(for: midiNote) else {
            return (100, 64)
        }
        let xRel = max(0, min(1, (pt.x - rect.minX) / rect.width))
        let yRel = max(0, min(1, (pt.y - rect.minY) / rect.height))
        let pan = UInt8(max(0, min(127, Int(round(xRel * 127)))))
        let yDist = abs(yRel - 0.5) * 2.0
        let vMin: Double = 60, vMax: Double = 120
        let vel = vMax - (vMax - vMin) * yDist
        let velocity = UInt8(max(1, min(127, Int(round(vel)))))
        return (velocity, pan)
    }

    // MARK: - Popover

    /// Close the popover and tear down the click-away monitor. Called from
    /// `statusClicked` (settings-chip toggle) and from the click-away
    /// monitor itself when the user clicks anywhere outside our app.
    private func closePopover() {
        if popover.isShown {
            popover.performClose(nil)
        }
        if let m = clickAwayMonitor { NSEvent.removeMonitor(m); clickAwayMonitor = nil }
        if let m = popoverEscMonitor { NSEvent.removeMonitor(m); popoverEscMonitor = nil }
    }

    private func showPopover() {
        guard let button = statusItem.button else { return }
        // popoverVC is pre-built in applicationDidFinishLaunching so the first
        // open is instant — no lazy view inflation here.
        popoverVC?.syncFromController()
        if popover.isShown {
            closePopover()
        } else {
            let imgSize = KeyboardIconRenderer.imageSize
            let bb = button.bounds
            let xOff = (bb.width - imgSize.width) / 2.0
            let yOff = (bb.height - imgSize.height) / 2.0
            let latch = KeyboardIconRenderer.settingsRectPublic
            let anchor = NSRect(
                x: xOff + latch.minX,
                y: yOff + latch.minY,
                width: latch.width,
                height: latch.height
            )
            // Activate the app + make the popover key so hover and clicks
            // register immediately. NSStatusItem popovers don't pull focus
            // by default; without this you have to click into the popover
            // once before its controls react.
            NSApp.activate(ignoringOtherApps: true)
            popover.show(relativeTo: anchor, of: button, preferredEdge: .minY)
            DispatchQueue.main.async {
                self.popover.contentViewController?.view.window?.makeKey()
            }
            // Click-away monitor: clicks on OTHER apps close the popover.
            // In-app clicks (status item button, the popover itself) don't
            // fire global monitors and therefore don't dismiss — that's how
            // we keep the popover open while the user taps menubar piano
            // keys.
            if clickAwayMonitor == nil {
                clickAwayMonitor = NSEvent.addGlobalMonitorForEvents(
                    matching: [.leftMouseDown, .rightMouseDown]
                ) { [weak self] _ in
                    self?.closePopover()
                }
            }
            // Esc closes the popover when it has key focus.
            if popoverEscMonitor == nil {
                popoverEscMonitor = NSEvent.addLocalMonitorForEvents(
                    matching: [.keyDown]
                ) { [weak self] event in
                    if event.keyCode == 53 /* kVK_Escape */ {
                        self?.closePopover()
                        return nil
                    }
                    return event
                }
            }
        }
    }
}
