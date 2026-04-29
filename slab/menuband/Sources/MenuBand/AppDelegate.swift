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
        KeyboardIconRenderer.activeKeymap = menuBand.effectiveKeymap
        statusItem.length = KeyboardIconRenderer.imageSize.width
        button.image = KeyboardIconRenderer.image(
            litNotes: menuBand.litNotes,
            enabled: menuBand.midiMode,
            typeMode: menuBand.effectiveTypeMode,
            melodicProgram: menuBand.melodicProgram,
            hovered: hoveredElement
        )
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
            let prev = hoveredElement
            hoveredElement = result
            updateIcon()
            playRolloverTick(prev: prev, next: result)
        }
    }

    /// Tiny ASMR-y tick on hover boundaries — between two piano keys (edge
    /// rollover) or onto the settings chip. Respects the user's "Play UI
    /// sound effects" preference automatically because NSSound routes
    /// through the macOS UI-effects channel.
    private func playRolloverTick(prev: KeyboardIconRenderer.HitResult?,
                                   next: KeyboardIconRenderer.HitResult?) {
        switch (prev, next) {
        case (.note, .note):
            // Edge crossing between two keys — the soft pianistic tick.
            let s = NSSound(named: NSSound.Name("Tink"))
            s?.volume = 0.22
            s?.play()
        case (_, .openSettings) where prev != .openSettings:
            // Cursor entered the settings chip from outside or from a key.
            let s = NSSound(named: NSSound.Name("Pop"))
            s?.volume = 0.18
            s?.play()
        default:
            break
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
            NSSound(named: NSSound.Name("Tink"))?.play()
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
        debugLog("hit pt=(\(initialHitPt.x),\(initialHitPt.y)) -> \(initial)")
        let startNote: UInt8
        switch initial {
        case .openSettings:
            NSSound(named: NSSound.Name("Tink"))?.play()
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

    private func showPopover() {
        guard let button = statusItem.button else { return }
        if popoverVC == nil {
            let vc = MenuBandPopoverViewController()
            vc.menuBand = menuBand
            popoverVC = vc
            popover.contentViewController = vc
            popover.behavior = .transient
            popover.animates = true
            _ = vc.view
        }
        popoverVC?.syncFromController()
        if popover.isShown {
            popover.performClose(nil)
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
        }
    }
}
