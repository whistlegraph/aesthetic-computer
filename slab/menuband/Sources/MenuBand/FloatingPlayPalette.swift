import AppKit

final class FloatingPlayPaletteController: NSObject, NSWindowDelegate {
    enum DismissReason {
        case closeButton
        case shortcut
        case programmatic

        var shouldRestoreFocus: Bool {
            switch self {
            case .closeButton, .shortcut:
                return true
            case .programmatic:
                return false
            }
        }
    }

    private let menuBand: MenuBandController
    private let viewController: FloatingPlayPaletteViewController
    private var panel: FloatingPlayPalettePanel?
    private var keyMonitor: Any?
    private var appBeforeOpen: NSRunningApplication?
    private var isDismissing = false

    var onDismiss: (() -> Void)?
    var onFocusRelease: (() -> Void)?
    var onToggleKeymap: (() -> Void)?
    var isPianoFocusActive: (() -> Bool)? {
        get { viewController.isPianoFocusActive }
        set { viewController.isPianoFocusActive = newValue }
    }

    var isShown: Bool {
        panel?.isVisible == true
    }

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        self.viewController = FloatingPlayPaletteViewController(menuBand: menuBand)
        super.init()
        self.viewController.onClose = { [weak self] in
            self?.dismiss(reason: .closeButton)
        }
    }

    func toggleFromShortcut() {
        if isShown {
            dismiss(reason: .shortcut)
        } else {
            show()
        }
    }

    func showFromCommand(restoringTo previousApp: NSRunningApplication? = nil) {
        show(restoringTo: previousApp)
    }

    func show(restoringTo previousApp: NSRunningApplication? = nil) {
        if panel == nil { buildPanel() }
        guard let panel = panel else { return }

        if panel.isVisible {
            panel.makeKeyAndOrderFront(nil)
            return
        }

        appBeforeOpen = previousApp ?? currentFrontmostOtherApp()
        viewController.refresh()
        panel.setFrame(frameForCurrentMouseScreen(size: viewController.preferredContentSize), display: false)

        NSApp.activate(ignoringOtherApps: true)
        panel.makeKeyAndOrderFront(nil)
        viewController.setPresented(true)
        installMonitors()
    }

    func dismiss(reason: DismissReason = .programmatic) {
        guard !isDismissing else { return }
        guard isShown || keyMonitor != nil else { return }

        isDismissing = true
        removeMonitors()
        viewController.setPresented(false)
        viewController.clearInteraction()
        menuBand.releaseAllHeldNotes()
        panel?.orderOut(nil)
        onDismiss?()
        if reason.shouldRestoreFocus {
            restorePreviousAppFocus()
        }
        appBeforeOpen = nil
        isDismissing = false
    }

    func refresh() {
        guard isShown else { return }
        viewController.refresh()
        resizePanelToCurrentContent()
    }

    func clearInteraction() {
        viewController.clearInteraction()
    }

    var isKeyboardFocused: Bool {
        panel?.isKeyWindow == true
    }

    func releaseKeyboardFocus() {
        guard isShown else { return }
        restorePreviousAppFocus()
    }

    private func buildPanel() {
        let p = FloatingPlayPalettePanel(
            contentRect: NSRect(origin: .zero, size: viewController.preferredContentSize),
            styleMask: [.borderless],
            backing: .buffered,
            defer: false
        )
        p.contentViewController = viewController
        p.delegate = self
        p.isOpaque = false
        p.backgroundColor = .clear
        p.hasShadow = true
        p.level = .floating
        p.animationBehavior = .none
        p.collectionBehavior = [.transient]
        p.hidesOnDeactivate = false
        p.canHide = false
        p.isMovableByWindowBackground = false
        p.acceptsMouseMovedEvents = true
        panel = p
    }

    private func installMonitors() {
        if keyMonitor == nil {
            keyMonitor = NSEvent.addLocalMonitorForEvents(matching: [.keyDown, .keyUp]) { [weak self] event in
                guard let self = self, self.panel?.isKeyWindow == true else { return event }
                let isDown = event.type == .keyDown
                if isDown && event.keyCode == 53 /* kVK_Escape */ {
                    self.onFocusRelease?()
                    return nil
                }
                if isDown && MenuBandShortcutPreferences.focusShortcut.matches(event: event) {
                    self.onFocusRelease?()
                    return nil
                }
                if isDown && MenuBandShortcut.layoutToggle.matches(event: event) {
                    self.onToggleKeymap?()
                    return nil
                }
                let consumed = self.menuBand.handleLocalKey(
                    keyCode: event.keyCode,
                    isDown: isDown,
                    isRepeat: event.isARepeat,
                    flags: event.modifierFlags
                )
                if consumed {
                    self.refresh()
                    return nil
                }
                return event
            }
        }
    }

    private func removeMonitors() {
        if let m = keyMonitor {
            NSEvent.removeMonitor(m)
            keyMonitor = nil
        }
    }

    private func resizePanelToCurrentContent() {
        guard let panel = panel, panel.isVisible else { return }
        let size = viewController.preferredContentSize
        let old = panel.frame
        guard abs(old.width - size.width) > 0.5 || abs(old.height - size.height) > 0.5 else { return }
        let center = NSPoint(x: old.midX, y: old.midY)
        var frame = NSRect(
            x: center.x - size.width / 2,
            y: center.y - size.height / 2,
            width: size.width,
            height: size.height
        )
        let visible = (panel.screen ?? NSScreen.main)?.visibleFrame
            ?? NSRect(x: 0, y: 0, width: 1024, height: 768)
        frame = clamped(frame, to: visible)
        panel.setFrame(frame, display: true)
    }

    private func frameForCurrentMouseScreen(size: NSSize) -> NSRect {
        let mouse = NSEvent.mouseLocation
        let screen = NSScreen.screens.first { NSMouseInRect(mouse, $0.frame, false) }
            ?? NSScreen.main
            ?? NSScreen.screens.first
        let visible = screen?.visibleFrame ?? NSRect(x: 0, y: 0, width: 1024, height: 768)
        let margin: CGFloat = 16
        let preferred = NSPoint(
            x: visible.midX - size.width / 2,
            y: visible.midY + visible.height * 0.12 - size.height / 2
        )
        let x = min(max(preferred.x, visible.minX + margin), visible.maxX - size.width - margin)
        let y = min(max(preferred.y, visible.minY + margin), visible.maxY - size.height - margin)
        return NSRect(origin: NSPoint(x: x, y: y), size: size)
    }

    private func clamped(_ frame: NSRect, to visible: NSRect) -> NSRect {
        let margin: CGFloat = 16
        let x = min(max(frame.origin.x, visible.minX + margin), visible.maxX - frame.width - margin)
        let y = min(max(frame.origin.y, visible.minY + margin), visible.maxY - frame.height - margin)
        return NSRect(origin: NSPoint(x: x, y: y), size: frame.size)
    }

    private func currentFrontmostOtherApp() -> NSRunningApplication? {
        let frontmost = NSWorkspace.shared.frontmostApplication
        guard frontmost?.bundleIdentifier != Bundle.main.bundleIdentifier else { return nil }
        return frontmost
    }

    private func restorePreviousAppFocus() {
        guard let app = appBeforeOpen,
              !app.isTerminated,
              app.bundleIdentifier != Bundle.main.bundleIdentifier else { return }
        app.activate(options: [.activateIgnoringOtherApps])
    }

    deinit {
        dismiss(reason: .programmatic)
    }
}

private final class FloatingPlayPaletteViewController: NSViewController {
    private let paletteView: FloatingPlayPaletteView
    var onClose: (() -> Void)? {
        get { paletteView.onClose }
        set { paletteView.onClose = newValue }
    }
    var isPianoFocusActive: (() -> Bool)? {
        get { paletteView.isPianoFocusActive }
        set { paletteView.isPianoFocusActive = newValue }
    }

    init(menuBand: MenuBandController) {
        self.paletteView = FloatingPlayPaletteView(menuBand: menuBand)
        super.init(nibName: nil, bundle: nil)
        preferredContentSize = paletteView.fittingSize
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        nil
    }

    override func loadView() {
        view = paletteView
    }

    func refresh() {
        paletteView.refresh()
        preferredContentSize = paletteView.fittingSize
    }

    func clearInteraction() {
        paletteView.clearInteraction()
    }

    func setPresented(_ isPresented: Bool) {
        paletteView.setPresented(isPresented)
    }
}

private final class FloatingPlayPaletteView: NSView {
    private weak var menuBand: MenuBandController?
    private let waveformView = WaveformView()
    private let waveformBezel = NSView()
    private let pianoView: FloatingPianoView
    private let dragHandle = FloatingPaletteDragHandleView()
    private let closeButton = NSButton()
    private let shortcutHintLabel = NSTextField(labelWithString: "")

    var onClose: (() -> Void)?
    var isPianoFocusActive: (() -> Bool)?

    private let pianoScale: CGFloat = 2.0
    private let inset: CGFloat = 14
    private let gap: CGFloat = 8
    private let closeSize: CGFloat = 18
    private let hintHeight: CGFloat = 42
    private var waveformHeightConstraint: NSLayoutConstraint?

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        self.pianoView = FloatingPianoView(menuBand: menuBand, pianoScale: pianoScale)
        super.init(frame: NSRect(origin: .zero, size: .zero))
        wantsLayer = true

        waveformView.menuBand = menuBand
        waveformView.translatesAutoresizingMaskIntoConstraints = false
        waveformBezel.wantsLayer = true
        waveformBezel.layer?.cornerRadius = 6
        waveformBezel.layer?.backgroundColor = NSColor(white: 0.06, alpha: 1.0).cgColor
        waveformBezel.layer?.borderWidth = 1
        waveformBezel.translatesAutoresizingMaskIntoConstraints = false
        pianoView.translatesAutoresizingMaskIntoConstraints = false
        dragHandle.translatesAutoresizingMaskIntoConstraints = false
        closeButton.translatesAutoresizingMaskIntoConstraints = false
        shortcutHintLabel.translatesAutoresizingMaskIntoConstraints = false
        waveformBezel.addSubview(waveformView)
        addSubview(waveformBezel)
        addSubview(pianoView)
        shortcutHintLabel.font = NSFont.systemFont(ofSize: 10)
        shortcutHintLabel.textColor = .secondaryLabelColor
        shortcutHintLabel.alignment = .center
        shortcutHintLabel.maximumNumberOfLines = 2
        shortcutHintLabel.lineBreakMode = .byWordWrapping
        addSubview(shortcutHintLabel)
        addSubview(dragHandle)
        updateShortcutHint()

        let closeConfig = NSImage.SymbolConfiguration(pointSize: 12, weight: .semibold)
        closeButton.image = NSImage(systemSymbolName: "xmark", accessibilityDescription: "Close")?
            .withSymbolConfiguration(closeConfig)
        closeButton.isBordered = false
        closeButton.imagePosition = .imageOnly
        closeButton.contentTintColor = .secondaryLabelColor
        closeButton.toolTip = "Close"
        closeButton.target = self
        closeButton.action = #selector(closeClicked(_:))
        addSubview(closeButton)

        let keyboardSize = self.keyboardSize()
        let waveformHeightConstraint = waveformView.heightAnchor.constraint(
            equalToConstant: waveformHeight(for: keyboardSize)
        )
        self.waveformHeightConstraint = waveformHeightConstraint
        let bezelInset: CGFloat = 5

        NSLayoutConstraint.activate([
            widthAnchor.constraint(equalToConstant: keyboardSize.width + inset * 2),

            closeButton.topAnchor.constraint(equalTo: topAnchor, constant: inset),
            closeButton.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -inset),
            closeButton.widthAnchor.constraint(equalToConstant: closeSize),
            closeButton.heightAnchor.constraint(equalToConstant: closeSize),

            dragHandle.leadingAnchor.constraint(equalTo: leadingAnchor, constant: inset),
            dragHandle.trailingAnchor.constraint(equalTo: closeButton.leadingAnchor, constant: -gap),
            dragHandle.centerYAnchor.constraint(equalTo: closeButton.centerYAnchor),
            dragHandle.heightAnchor.constraint(equalToConstant: closeSize),

            waveformBezel.topAnchor.constraint(equalTo: closeButton.bottomAnchor, constant: gap),
            waveformBezel.leadingAnchor.constraint(equalTo: leadingAnchor, constant: inset),
            waveformBezel.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -inset),
            waveformView.leadingAnchor.constraint(equalTo: waveformBezel.leadingAnchor, constant: bezelInset),
            waveformView.trailingAnchor.constraint(equalTo: waveformBezel.trailingAnchor, constant: -bezelInset),
            waveformView.topAnchor.constraint(equalTo: waveformBezel.topAnchor, constant: bezelInset),
            waveformView.bottomAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: -bezelInset),
            waveformHeightConstraint,

            pianoView.topAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: gap),
            pianoView.leadingAnchor.constraint(equalTo: leadingAnchor, constant: inset),
            pianoView.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -inset),

            shortcutHintLabel.topAnchor.constraint(equalTo: pianoView.bottomAnchor, constant: gap),
            shortcutHintLabel.leadingAnchor.constraint(equalTo: leadingAnchor, constant: inset),
            shortcutHintLabel.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -inset),
            shortcutHintLabel.heightAnchor.constraint(equalToConstant: hintHeight),
            shortcutHintLabel.bottomAnchor.constraint(equalTo: bottomAnchor, constant: -inset)
        ])
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        nil
    }

    override var acceptsFirstResponder: Bool { true }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        applyAppearanceToVisualizer()
    }

    override var fittingSize: NSSize {
        pianoView.refreshLayout()
        layoutSubtreeIfNeeded()
        return super.fittingSize
    }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        let background = bounds.insetBy(dx: 0.5, dy: 0.5)
        let path = NSBezierPath(roundedRect: background, xRadius: 13, yRadius: 13)
        NSColor.windowBackgroundColor.withAlphaComponent(0.96).setFill()
        path.fill()
        NSColor.separatorColor.withAlphaComponent(0.55).setStroke()
        path.lineWidth = 1
        path.stroke()
    }

    func refresh() {
        updateShortcutHint()
        let keyboardSize = keyboardSize()
        waveformHeightConstraint?.constant = waveformHeight(for: keyboardSize)
        pianoView.refreshLayout()
        layoutSubtreeIfNeeded()
        applyAppearanceToVisualizer()
        applyWaveformTint()
        updateWaveformLiveState(isPresented: window?.isVisible == true)
        needsDisplay = true
        pianoView.needsDisplay = true
    }

    func clearInteraction() {
        pianoView.clearInteraction()
    }

    func setPresented(_ isPresented: Bool) {
        applyAppearanceToVisualizer()
        applyWaveformTint()
        updateWaveformLiveState(isPresented: isPresented)
    }

    private func updateWaveformLiveState(isPresented: Bool) {
        waveformView.isLive = isPresented && !(menuBand?.midiMode ?? false)
        waveformView.alphaValue = (menuBand?.midiMode ?? false) ? 0.35 : 1.0
    }

    private func applyAppearanceToVisualizer() {
        let isDark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        waveformView.setLightMode(!isDark)
        if isDark {
            waveformBezel.layer?.backgroundColor = NSColor(white: 0.06, alpha: 1.0).cgColor
        } else {
            waveformBezel.layer?.backgroundColor = NSColor(white: 0.82, alpha: 1.0).cgColor
        }
    }

    private func applyWaveformTint() {
        guard let menuBand else { return }
        if menuBand.midiMode {
            waveformView.setDotMatrix(MenuBandPopoverViewController.midiDotPattern)
            waveformView.setBaseColor(.controlAccentColor)
            waveformBezel.layer?.borderColor = NSColor.controlAccentColor
                .withAlphaComponent(0.55).cgColor
        } else {
            waveformView.setDotMatrix(nil)
            let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
            let familyColor = InstrumentListView.colorForProgram(safe)
            waveformView.setBaseColor(familyColor)
            waveformBezel.layer?.borderColor = familyColor
                .withAlphaComponent(0.55).cgColor
        }
    }

    private func keyboardSize() -> NSSize {
        withFloatingPaletteKeyboard(menuBand: menuBand) {
            let piano = KeyboardIconRenderer.pianoImageSize(layout: .tightActiveRange)
            return NSSize(width: piano.width * pianoScale, height: piano.height * pianoScale)
        }
    }

    private func waveformHeight(for keyboard: NSSize) -> CGFloat {
        keyboard.height * 2.0
    }

    private func updateShortcutHint() {
        let floatingShortcut = MenuBandShortcutPreferences.playPaletteShortcut.displayString
        let focusShortcut = MenuBandShortcutPreferences.focusShortcut.displayString
        let layoutShortcut = MenuBandShortcut.layoutToggle.displayString
        let focusText = (isPianoFocusActive?() ?? false)
            ? "Exit focus: \(focusShortcut)"
            : "Focus piano: \(focusShortcut)"
        shortcutHintLabel.stringValue =
            "Show/hide floating piano: \(floatingShortcut)\n\(focusText)  Toggle layout: \(layoutShortcut)"
    }

    @objc private func closeClicked(_ sender: NSButton) {
        onClose?()
    }
}

private final class FloatingPianoView: NSView {
    private static let rendererLayout: KeyboardIconRenderer.Layout = .tightActiveRange

    private weak var menuBand: MenuBandController?
    private var trackingArea: NSTrackingArea?
    private var hoveredNote: UInt8?
    private var currentNote: UInt8?

    private let pianoScale: CGFloat
    private var widthConstraint: NSLayoutConstraint!
    private var heightConstraint: NSLayoutConstraint!

    init(menuBand: MenuBandController, pianoScale: CGFloat) {
        self.menuBand = menuBand
        self.pianoScale = pianoScale
        super.init(frame: NSRect(origin: .zero, size: .zero))
        wantsLayer = true

        let preferredSize = preferredSize()
        widthConstraint = widthAnchor.constraint(equalToConstant: preferredSize.width)
        heightConstraint = heightAnchor.constraint(equalToConstant: preferredSize.height)
        NSLayoutConstraint.activate([widthConstraint, heightConstraint])
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        nil
    }

    override var acceptsFirstResponder: Bool { true }

    func refreshLayout() {
        let preferredSize = preferredSize()
        widthConstraint.constant = preferredSize.width
        heightConstraint.constant = preferredSize.height
    }

    private func preferredSize() -> NSSize {
        withFloatingPaletteKeyboard(menuBand: menuBand) {
            let piano = KeyboardIconRenderer.pianoImageSize(layout: Self.rendererLayout)
            return NSSize(
                width: piano.width * pianoScale,
                height: piano.height * pianoScale
            )
        }
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let trackingArea = trackingArea {
            removeTrackingArea(trackingArea)
        }
        let area = NSTrackingArea(
            rect: bounds,
            options: [.mouseEnteredAndExited, .mouseMoved, .activeAlways, .inVisibleRect],
            owner: self,
            userInfo: nil
        )
        addTrackingArea(area)
        trackingArea = area
    }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        guard let menuBand = menuBand else { return }

        withFloatingPaletteKeyboard(menuBand: menuBand) {
            KeyboardIconRenderer.activeKeymap = menuBand.keymap
            let image = KeyboardIconRenderer.image(
                litNotes: menuBand.litNotes,
                enabled: menuBand.midiMode,
                typeMode: true,
                hovered: hoveredNote.map { .note($0) },
                includeSettings: false,
                layout: Self.rendererLayout
            )
            image.draw(in: pianoTargetRect())
        }
    }

    override func mouseMoved(with event: NSEvent) {
        updateHover(with: event)
    }

    override func mouseExited(with event: NSEvent) {
        if hoveredNote != nil {
            hoveredNote = nil
            needsDisplay = true
        }
    }

    override func mouseDown(with event: NSEvent) {
        window?.makeKey()
        guard let menuBand = menuBand,
              let point = rendererPoint(from: event),
              let note = withFloatingPaletteKeyboard(
                  menuBand: menuBand,
                  { KeyboardIconRenderer.noteAt(point, layout: Self.rendererLayout) }
              )
        else { return }
        let expression = withFloatingPaletteKeyboard(menuBand: menuBand) {
            NoteExpression.values(for: note, at: point, layout: Self.rendererLayout)
        }
        currentNote = note
        hoveredNote = note
        menuBand.startTapNote(note, velocity: expression.velocity, pan: expression.pan)
        needsDisplay = true
    }

    override func mouseDragged(with event: NSEvent) {
        guard let menuBand = menuBand,
              let point = rendererPoint(from: event) else { return }
        let hovered = withFloatingPaletteKeyboard(
            menuBand: menuBand,
            { KeyboardIconRenderer.noteAt(point, layout: Self.rendererLayout) }
        )
        hoveredNote = hovered

        if hovered != currentNote {
            if let previous = currentNote {
                menuBand.stopTapNote(previous)
            }
            if let next = hovered {
                let expression = withFloatingPaletteKeyboard(menuBand: menuBand) {
                    NoteExpression.values(for: next, at: point, layout: Self.rendererLayout)
                }
                menuBand.startTapNote(next, velocity: expression.velocity, pan: expression.pan)
            }
            currentNote = hovered
        } else if let current = currentNote {
            let expression = withFloatingPaletteKeyboard(menuBand: menuBand) {
                NoteExpression.values(for: current, at: point, layout: Self.rendererLayout)
            }
            menuBand.updateTapPan(current, pan: expression.pan)
        }
        needsDisplay = true
    }

    override func mouseUp(with event: NSEvent) {
        if let note = currentNote {
            menuBand?.stopTapNote(note)
        }
        currentNote = nil
        updateHover(with: event)
    }

    func clearInteraction() {
        currentNote = nil
        hoveredNote = nil
        needsDisplay = true
    }

    private func updateHover(with event: NSEvent) {
        guard let point = rendererPoint(from: event) else {
            if hoveredNote != nil {
                hoveredNote = nil
                needsDisplay = true
            }
            return
        }
        let next = withFloatingPaletteKeyboard(
            menuBand: menuBand,
            { KeyboardIconRenderer.noteAt(point, layout: Self.rendererLayout) }
        )
        if next != hoveredNote {
            hoveredNote = next
            needsDisplay = true
        }
    }

    private func rendererPoint(from event: NSEvent) -> NSPoint? {
        let local = convert(event.locationInWindow, from: nil)
        let target = pianoTargetRect()
        let point = NSPoint(
            x: (local.x - target.minX) / pianoScale,
            y: (local.y - target.minY) / pianoScale
        )
        let piano = withFloatingPaletteKeyboard(menuBand: menuBand) {
            KeyboardIconRenderer.pianoImageSize(layout: Self.rendererLayout)
        }
        guard point.x >= -KeyboardIconRenderer.whiteW,
              point.x <= piano.width + KeyboardIconRenderer.whiteW,
              point.y >= -piano.height,
              point.y <= piano.height * 2 else { return nil }
        return point
    }

    private func pianoTargetRect() -> NSRect {
        let piano = withFloatingPaletteKeyboard(menuBand: menuBand) {
            KeyboardIconRenderer.pianoImageSize(layout: Self.rendererLayout)
        }
        let size = NSSize(width: piano.width * pianoScale, height: piano.height * pianoScale)
        return NSRect(
            x: bounds.midX - size.width / 2,
            y: bounds.midY - size.height / 2,
            width: size.width,
            height: size.height
        )
    }
}

private final class FloatingPaletteDragHandleView: NSView {
    private var dragStartMouse: NSPoint?
    private var dragStartWindowOrigin: NSPoint?

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        let pan = NSPanGestureRecognizer(target: self, action: #selector(handlePan(_:)))
        addGestureRecognizer(pan)
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        nil
    }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        let gripWidth: CGFloat = 34
        let gripHeight: CGFloat = 3
        let grip = NSRect(
            x: bounds.midX - gripWidth / 2,
            y: bounds.midY - gripHeight / 2,
            width: gripWidth,
            height: gripHeight
        )
        NSColor.tertiaryLabelColor.withAlphaComponent(0.55).setFill()
        NSBezierPath(roundedRect: grip, xRadius: gripHeight / 2, yRadius: gripHeight / 2).fill()
    }

    override func resetCursorRects() {
        super.resetCursorRects()
        addCursorRect(bounds, cursor: .openHand)
    }

    @objc private func handlePan(_ recognizer: NSPanGestureRecognizer) {
        switch recognizer.state {
        case .began:
            dragStartMouse = NSEvent.mouseLocation
            dragStartWindowOrigin = window?.frame.origin
            NSCursor.closedHand.set()
        case .changed:
            guard let window = window,
                  let startMouse = dragStartMouse,
                  let startOrigin = dragStartWindowOrigin else { return }
            let mouse = NSEvent.mouseLocation
            let nextOrigin = NSPoint(
                x: startOrigin.x + mouse.x - startMouse.x,
                y: startOrigin.y + mouse.y - startMouse.y
            )
            NSAnimationContext.runAnimationGroup { context in
                context.duration = 0
                context.allowsImplicitAnimation = false
                window.setFrameOrigin(nextOrigin)
            }
        case .ended, .cancelled, .failed:
            dragStartMouse = nil
            dragStartWindowOrigin = nil
            NSCursor.openHand.set()
        default:
            break
        }
    }
}

private func withFloatingPaletteKeyboard<T>(menuBand: MenuBandController?, _ body: () -> T) -> T {
    let oldLayout = KeyboardIconRenderer.displayLayout
    let oldKeymap = KeyboardIconRenderer.activeKeymap
    KeyboardIconRenderer.displayLayout = .full
    if let menuBand = menuBand {
        KeyboardIconRenderer.activeKeymap = menuBand.keymap
    }
    defer {
        KeyboardIconRenderer.displayLayout = oldLayout
        KeyboardIconRenderer.activeKeymap = oldKeymap
    }
    return body()
}

private final class FloatingPlayPalettePanel: NSPanel {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { false }
}
