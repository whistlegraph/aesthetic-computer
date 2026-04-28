import AppKit
import Carbon

final class AppDelegate: NSObject, NSApplicationDelegate {
    private var statusItem: NSStatusItem!
    private let notepat = NotepatController()
    private let hoverResponder = HoverResponder()
    private var hoveredElement: KeyboardIconRenderer.HitResult? = nil
    private var trackingArea: NSTrackingArea?
    private var typeModeHotkey: GlobalHotkey?
    private let popover = NSPopover()
    private var popoverVC: NotepatPopoverViewController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        notepat.onChange = { [weak self] in
            DispatchQueue.main.async { self?.updateIcon() }
        }
        notepat.onLitChanged = { [weak self] in
            self?.updateIcon()
        }
        notepat.bootstrap()

        statusItem = NSStatusBar.system.statusItem(withLength: KeyboardIconRenderer.imageSize.width)
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
            self?.notepat.toggleTypeMode()
        }
        let modMask: UInt32 = UInt32(cmdKey | controlKey | optionKey)
        hotkey.register(keyCode: UInt32(kVK_ANSI_P), modifiers: modMask)
        typeModeHotkey = hotkey
    }

    func applicationWillTerminate(_ notification: Notification) {
        notepat.shutdown()
    }

    private func updateIcon() {
        guard let button = statusItem.button else { return }
        statusItem.length = KeyboardIconRenderer.imageSize.width
        button.image = KeyboardIconRenderer.image(
            litNotes: notepat.litNotes,
            enabled: notepat.midiMode,
            typeMode: notepat.typeMode,
            melodicProgram: notepat.melodicProgram,
            hovered: hoveredElement
        )
    }

    // MARK: - Hover

    private func handleHover(event: NSEvent) {
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

        let initial = KeyboardIconRenderer.hit(at: imagePoint(from: downEvent.locationInWindow))
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
        notepat.startTapNote(startNote, velocity: vel0, pan: pan0)
        var current: UInt8? = startNote
        while let next = NSApp.nextEvent(
            matching: [.leftMouseDragged, .leftMouseUp],
            until: .distantFuture,
            inMode: .eventTracking,
            dequeue: true
        ) {
            if next.type == .leftMouseUp {
                if let c = current { notepat.stopTapNote(c) }
                break
            }
            let pt = imagePoint(from: next.locationInWindow)
            let hovered = KeyboardIconRenderer.noteAt(pt)
            if hovered != current {
                if let prev = current { notepat.stopTapNote(prev) }
                if let nxt = hovered {
                    let (v, p) = expression(for: nxt, at: pt)
                    notepat.startTapNote(nxt, velocity: v, pan: p)
                }
                current = hovered
            } else if let c = current {
                let (_, p) = expression(for: c, at: pt)
                notepat.updateTapPan(c, pan: p)
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
            let vc = NotepatPopoverViewController()
            vc.notepat = notepat
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
