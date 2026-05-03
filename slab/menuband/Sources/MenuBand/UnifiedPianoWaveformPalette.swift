import AppKit

final class UnifiedPianoWaveformPalette: NSObject, NSWindowDelegate {
    enum State {
        case collapsed
        case expanded
    }

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
    private let paletteController: FloatingPlayPaletteViewController
    private var panel: UnifiedPianoWaveformPalettePanel?
    private weak var statusItemButton: NSStatusBarButton?
    private var state: State = .collapsed
    private var preferredState: State
    private var keyMonitor: Any?
    private var appBeforeOpen: NSRunningApplication?
    private var dismissHandler: (() -> Void)?
    private var isDismissing = false
    private var isPositioningPanel = false
    private var savedExpandedOrigin: NSPoint?
    private var collapsedCustomOrigin: NSPoint?
    private var hideWorkItem: DispatchWorkItem?
    private var isEnabled: Bool

    private static let expandedOriginXKey = "notepat.unifiedPalette.expandedOriginX"
    private static let expandedOriginYKey = "notepat.unifiedPalette.expandedOriginY"
    private static let collapsedOriginXKey = "notepat.unifiedPalette.collapsedOriginX"
    private static let collapsedOriginYKey = "notepat.unifiedPalette.collapsedOriginY"
    private static let enabledKey = "notepat.unifiedPalette.enabled"
    private static let preferredStateKey = "notepat.unifiedPalette.preferredState"
    private static let collapsedHideDelay: TimeInterval = 2.0

    var onDismiss: (() -> Void)? {
        get { dismissHandler }
        set { dismissHandler = newValue }
    }

    var onFocusRelease: (() -> Void)?

    var onToggleKeymap: (() -> Void)?

    var isPianoFocusActive: (() -> Bool)? {
        get { paletteController.isPianoFocusActive }
        set { paletteController.isPianoFocusActive = newValue }
    }

    var isShown: Bool {
        state == .expanded && panel?.isVisible == true
    }

    var isKeyboardFocused: Bool {
        state == .expanded && panel?.isKeyWindow == true
    }

    var isCollapsedState: Bool { state == .collapsed }

    var isFeatureEnabled: Bool { isEnabled }

    var onStepBackward: (() -> Void)? {
        get { paletteController.onStepBackward }
        set { paletteController.onStepBackward = newValue }
    }

    var onStepForward: (() -> Void)? {
        get { paletteController.onStepForward }
        set { paletteController.onStepForward = newValue }
    }

    var onStepUp: (() -> Void)? {
        get { paletteController.onStepUp }
        set { paletteController.onStepUp = newValue }
    }

    var onStepDown: (() -> Void)? {
        get { paletteController.onStepDown }
        set { paletteController.onStepDown = newValue }
    }

    var suppressed: Bool = false {
        didSet {
            guard suppressed != oldValue else { return }
            if suppressed && state == .collapsed {
                dismissCollapsedPanel()
            }
        }
    }

    var isDocked: Bool { collapsedCustomOrigin == nil }

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        self.paletteController = FloatingPlayPaletteViewController(menuBand: menuBand)
        self.preferredState = Self.loadPreferredState()
        self.isEnabled = Self.loadEnabledState()
        self.savedExpandedOrigin = Self.loadOrigin(
            xKey: Self.expandedOriginXKey,
            yKey: Self.expandedOriginYKey
        )
        self.collapsedCustomOrigin = Self.loadOrigin(
            xKey: Self.collapsedOriginXKey,
            yKey: Self.collapsedOriginYKey
        )
        super.init()

        paletteController.onCloseRequested = { [weak self] in
            self?.disable(reason: .closeButton)
        }
        paletteController.onDockRequested = { [weak self] in
            self?.dockOnMenu()
        }
        paletteController.onTogglePresentationMode = { [weak self] in
            self?.togglePresentationMode()
        }
    }

    func toggleFromShortcut() {
        if isEnabled {
            disable(reason: .shortcut)
            return
        }
        enableAndShowPreferred(restoringTo: nil)
    }

    func showFromCommand(restoringTo previousApp: NSRunningApplication? = nil) {
        setEnabled(true)
        preferredState = .expanded
        persistPreferredState()
        transitionToExpanded()
        showExpanded(restoringTo: previousApp)
    }

    func show(restoringTo previousApp: NSRunningApplication? = nil) {
        setEnabled(true)
        preferredState = .expanded
        persistPreferredState()
        transitionToExpanded()
        showExpanded(restoringTo: previousApp)
    }

    func dismiss(reason: DismissReason = .programmatic) {
        guard state == .expanded else { return }
        dismissExpanded(reason: reason)
    }

    func refresh() {
        switch state {
        case .expanded:
            paletteController.setDisplayMode(.expanded)
            paletteController.refresh()
            if let panel, panel.isVisible {
                setPanelFrame(expandedFrame(
                    size: paletteController.preferredContentSize,
                    fallbackOrigin: panel.frame.origin
                ))
            }
        case .collapsed:
            paletteController.setDisplayMode(.collapsed)
            paletteController.refresh()
            if let panel, panel.isVisible {
                setPanelFrame(collapsedFrame(size: paletteController.preferredContentSize))
            }
        }
    }

    func clearInteraction() {
        paletteController.clearInteraction()
    }

    func releaseKeyboardFocus() {
        guard state == .expanded else { return }
        restorePreviousAppFocus()
    }

    func warmUp() {
        if panel == nil {
            buildPanel()
        }
        paletteController.setDisplayMode(.collapsed)
        paletteController.refresh()
    }

    func showIfNeeded() {
        guard state == .collapsed, !suppressed, isEnabled, preferredState == .collapsed else { return }
        cancelPendingHide()
        if panel == nil {
            buildPanel()
        }
        paletteController.setDisplayMode(.collapsed)
        paletteController.refresh()
        showCollapsedIfNeeded()
        if !menuBand.litNotes.isEmpty {
            focusCollapsedPaletteIfNeeded()
        }
    }

    func scheduleHide() {
        guard state == .collapsed else { return }
        cancelPendingHide()
        let work = DispatchWorkItem { [weak self] in
            self?.dismissCollapsedPanel()
        }
        hideWorkItem = work
        DispatchQueue.main.asyncAfter(deadline: .now() + Self.collapsedHideDelay, execute: work)
    }

    func reposition(statusItemButton: NSStatusBarButton?) {
        self.statusItemButton = statusItemButton
        guard state == .collapsed, let panel, panel.isVisible else { return }
        setPanelFrame(collapsedFrame(size: paletteController.preferredContentSize))
    }

    func refreshAppearance() {
        guard state == .collapsed else { return }
        paletteController.setDisplayMode(.collapsed)
        paletteController.refresh()
        if let panel, panel.isVisible {
            setPanelFrame(collapsedFrame(size: paletteController.preferredContentSize))
        }
    }

    func registerArrowInput() {
        guard state == .collapsed, !suppressed, isEnabled else { return }
        showIfNeeded()
        paletteController.refresh()
        focusCollapsedPaletteIfNeeded()
        if menuBand.litNotes.isEmpty {
            scheduleHide()
        }
    }

    func windowDidMove(_ notification: Notification) {
        guard let panel, !isPositioningPanel else { return }
        switch state {
        case .expanded:
            savedExpandedOrigin = panel.frame.origin
            persistOrigin(savedExpandedOrigin, xKey: Self.expandedOriginXKey, yKey: Self.expandedOriginYKey)
        case .collapsed:
            collapsedCustomOrigin = panel.frame.origin
            persistOrigin(collapsedCustomOrigin, xKey: Self.collapsedOriginXKey, yKey: Self.collapsedOriginYKey)
        }
    }

    private func buildPanel() {
        let panel = UnifiedPianoWaveformPalettePanel(
            contentRect: NSRect(origin: .zero, size: paletteController.preferredContentSize),
            styleMask: [.titled, .closable, .fullSizeContentView],
            backing: .buffered,
            defer: false
        )
        panel.delegate = self
        panel.isOpaque = false
        panel.backgroundColor = .clear
        panel.hasShadow = true
        panel.level = .floating
        panel.animationBehavior = .none
        panel.collectionBehavior = [.transient]
        panel.hidesOnDeactivate = false
        panel.canHide = false
        panel.isMovableByWindowBackground = true
        panel.acceptsMouseMovedEvents = true
        panel.titleVisibility = .hidden
        panel.titlebarAppearsTransparent = true
        panel.isReleasedWhenClosed = false
        panel.contentViewController = paletteController
        if let button = panel.standardWindowButton(.closeButton) {
            button.isHidden = true
        }
        if let mini = panel.standardWindowButton(.miniaturizeButton) {
            mini.isHidden = true
        }
        if let zoom = panel.standardWindowButton(.zoomButton) {
            zoom.isHidden = true
        }
        self.panel = panel
    }

    private func showExpanded(restoringTo previousApp: NSRunningApplication?) {
        if panel == nil {
            buildPanel()
        }
        guard let panel else { return }
        cancelPendingHide()
        setEnabled(true)
        appBeforeOpen = previousApp ?? currentFrontmostOtherApp()
        paletteController.setDisplayMode(.expanded)
        paletteController.refresh()
        panel.ignoresMouseEvents = false
        setPanelFrame(expandedFrame(
            size: paletteController.preferredContentSize,
            fallbackOrigin: panel.frame.origin
        ))
        NSApp.activate(ignoringOtherApps: true)
        panel.makeKeyAndOrderFront(nil)
        paletteController.setPresented(true)
        installMonitors()
    }

    private func showCollapsedIfNeeded() {
        guard let panel else { return }
        paletteController.setDisplayMode(.collapsed)
        panel.ignoresMouseEvents = false
        setPanelFrame(collapsedFrame(size: paletteController.preferredContentSize))
        if !panel.isVisible {
            panel.orderFrontRegardless()
        }
        paletteController.setPresented(true)
    }

    private func focusCollapsedPaletteIfNeeded() {
        guard state == .collapsed, let panel else { return }
        NSApp.activate(ignoringOtherApps: true)
        panel.makeKeyAndOrderFront(nil)
        paletteController.setPresented(true)
    }

    private func dismissExpanded(reason: DismissReason) {
        guard !isDismissing else { return }
        guard panel?.isVisible == true || keyMonitor != nil else { return }
        isDismissing = true
        removeMonitors()
        paletteController.setPresented(false)
        paletteController.clearInteraction()
        menuBand.releaseAllHeldNotes()
        panel?.ignoresMouseEvents = false
        panel?.orderOut(nil)
        state = .collapsed
        dismissHandler?()
        if reason.shouldRestoreFocus {
            restorePreviousAppFocus()
        }
        appBeforeOpen = nil
        isDismissing = false
    }

    private func dismissCollapsedPanel() {
        cancelPendingHide()
        paletteController.setPresented(false)
        panel?.ignoresMouseEvents = false
        panel?.orderOut(nil)
    }

    private func togglePresentationMode() {
        switch state {
        case .collapsed:
            expandFromStrip()
        case .expanded:
            collapseToStrip()
        }
    }

    private func dockOnMenu() {
        collapsedCustomOrigin = nil
        persistOrigin(nil, xKey: Self.collapsedOriginXKey, yKey: Self.collapsedOriginYKey)
        collapseToStrip()
        if let panel, panel.isVisible {
            setPanelFrame(collapsedFrame(size: paletteController.preferredContentSize))
        }
    }

    private func transitionToExpanded() {
        if panel == nil {
            buildPanel()
        }
        state = .expanded
        preferredState = .expanded
        persistPreferredState()
        panel?.ignoresMouseEvents = false
        paletteController.setDisplayMode(.expanded)
    }

    private func collapseToStrip() {
        guard state != .collapsed else {
            preferredState = .collapsed
            persistPreferredState()
            paletteController.setDisplayMode(.collapsed)
            paletteController.refresh()
            showIfNeeded()
            if menuBand.litNotes.isEmpty {
                scheduleHide()
            }
            return
        }
        removeMonitors()
        paletteController.setPresented(false)
        paletteController.clearInteraction()
        state = .collapsed
        preferredState = .collapsed
        persistPreferredState()
        paletteController.setDisplayMode(.collapsed)
        paletteController.refresh()
        showCollapsedIfNeeded()
        if menuBand.litNotes.isEmpty {
            scheduleHide()
        }
    }

    private func expandFromStrip() {
        transitionToExpanded()
        showExpanded(restoringTo: nil)
    }

    private func installMonitors() {
        if keyMonitor == nil {
            keyMonitor = NSEvent.addLocalMonitorForEvents(matching: [.keyDown, .keyUp]) { [weak self] event in
                guard let self, self.state == .expanded, self.panel?.isKeyWindow == true else { return event }
                let isDown = event.type == .keyDown
                if isDown && event.keyCode == 53 {
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
        if let keyMonitor {
            NSEvent.removeMonitor(keyMonitor)
            self.keyMonitor = nil
        }
    }

    private func expandedFrame(size: NSSize, fallbackOrigin: NSPoint?) -> NSRect {
        let origin = fallbackOrigin ?? savedExpandedOrigin
        return clampedFrame(
            origin: origin ?? centeredOrigin(for: size),
            size: size,
            preferredScreen: panel?.screen ?? NSScreen.main
        )
    }

    private func collapsedFrame(size: NSSize) -> NSRect {
        guard let anchoredFrame = anchoredCollapsedFrame(size: size) else {
            return clampedFrame(
                origin: collapsedCustomOrigin ?? centeredOrigin(for: size),
                size: size,
                preferredScreen: panel?.screen ?? NSScreen.main
            )
        }
        guard let collapsedCustomOrigin else { return anchoredFrame }
        return clampedFrame(
            origin: collapsedCustomOrigin,
            size: anchoredFrame.size,
            preferredScreen: panel?.screen ?? statusItemButton?.window?.screen
        )
    }

    private func anchoredCollapsedFrame(size: NSSize) -> NSRect? {
        guard let button = statusItemButton,
              let buttonWindow = button.window else { return nil }

        let imgSize = KeyboardIconRenderer.imageSize
        let buttonBounds = button.bounds
        let xOffset = (buttonBounds.width - imgSize.width) / 2.0
        let pianoOriginX = xOffset + KeyboardIconRenderer.pad
        let pianoWidth = imgSize.width - KeyboardIconRenderer.settingsW
            - KeyboardIconRenderer.settingsGap - KeyboardIconRenderer.pad * 2

        let localRect = NSRect(x: pianoOriginX, y: 0, width: pianoWidth, height: buttonBounds.height)
        let windowRect = button.convert(localRect, to: nil)
        let screenRect = buttonWindow.convertToScreen(windowRect)
        return NSRect(
            x: screenRect.origin.x,
            y: screenRect.origin.y - size.height,
            width: screenRect.width,
            height: size.height
        )
    }

    private func centeredOrigin(for size: NSSize) -> NSPoint {
        let mouse = NSEvent.mouseLocation
        let screen = NSScreen.screens.first { NSMouseInRect(mouse, $0.frame, false) }
            ?? NSScreen.main
            ?? NSScreen.screens.first
        let visible = screen?.visibleFrame ?? NSRect(x: 0, y: 0, width: 1024, height: 768)
        return NSPoint(
            x: visible.midX - size.width / 2,
            y: visible.midY - size.height / 2
        )
    }

    private func clampedFrame(origin: NSPoint, size: NSSize, preferredScreen: NSScreen?) -> NSRect {
        let visible = visibleFrame(for: origin, preferredScreen: preferredScreen)
        let margin: CGFloat = 16
        let x = min(max(origin.x, visible.minX + margin), visible.maxX - size.width - margin)
        let y = min(max(origin.y, visible.minY + margin), visible.maxY - size.height - margin)
        return NSRect(origin: NSPoint(x: x, y: y), size: size)
    }

    private func visibleFrame(for origin: NSPoint, preferredScreen: NSScreen?) -> NSRect {
        if let screen = NSScreen.screens.first(where: { $0.visibleFrame.contains(origin) }) {
            return screen.visibleFrame
        }
        if let preferredScreen {
            return preferredScreen.visibleFrame
        }
        return NSScreen.main?.visibleFrame ?? NSRect(x: 0, y: 0, width: 1440, height: 900)
    }

    private func setPanelFrame(_ frame: NSRect) {
        guard let panel else { return }
        isPositioningPanel = true
        panel.setFrame(frame, display: true)
        isPositioningPanel = false
    }

    private func cancelPendingHide() {
        hideWorkItem?.cancel()
        hideWorkItem = nil
    }

    private func enableAndShowPreferred(restoringTo previousApp: NSRunningApplication?) {
        setEnabled(true)
        switch preferredState {
        case .expanded:
            transitionToExpanded()
            showExpanded(restoringTo: previousApp)
        case .collapsed:
            state = .collapsed
            showIfNeeded()
            focusCollapsedPaletteIfNeeded()
        }
    }

    private func disable(reason: DismissReason) {
        setEnabled(false)
        switch state {
        case .expanded:
            dismissExpanded(reason: reason)
        case .collapsed:
            dismissCollapsedPanel()
        }
    }

    private func setEnabled(_ isEnabled: Bool) {
        guard self.isEnabled != isEnabled else { return }
        self.isEnabled = isEnabled
        UserDefaults.standard.set(isEnabled, forKey: Self.enabledKey)
    }

    private func persistPreferredState() {
        let value = preferredState == .expanded ? "expanded" : "collapsed"
        UserDefaults.standard.set(value, forKey: Self.preferredStateKey)
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

    private func persistOrigin(_ origin: NSPoint?, xKey: String, yKey: String) {
        let defaults = UserDefaults.standard
        guard let origin else {
            defaults.removeObject(forKey: xKey)
            defaults.removeObject(forKey: yKey)
            return
        }
        defaults.set(origin.x, forKey: xKey)
        defaults.set(origin.y, forKey: yKey)
    }

    private static func loadOrigin(xKey: String, yKey: String) -> NSPoint? {
        let defaults = UserDefaults.standard
        guard defaults.object(forKey: xKey) != nil,
              defaults.object(forKey: yKey) != nil else { return nil }
        return NSPoint(
            x: defaults.double(forKey: xKey),
            y: defaults.double(forKey: yKey)
        )
    }

    private static func loadEnabledState() -> Bool {
        UserDefaults.standard.bool(forKey: enabledKey)
    }

    private static func loadPreferredState() -> State {
        UserDefaults.standard.string(forKey: preferredStateKey) == "expanded"
            ? .expanded
            : .collapsed
    }
}

private final class UnifiedPianoWaveformPalettePanel: NSPanel {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { false }
}

@available(macOS 26.0, *)
private final class UnifiedWaveformStripGlassEffectView: NSGlassEffectView {
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
}

final class UnifiedWaveformStripView: NSView {
    private static var shouldUseLiquidGlass: Bool {
        if #available(macOS 26.0, *) { return true }
        return false
    }

    private weak var menuBand: MenuBandController?
    private let contentContainer = NSView()
    private let waveformContainer = NSView()
    private let waveformClipView = NSView()
    private let waveformView = WaveformView()
    private let heldNotesContainer = NSView()
    private let heldNotesStack = NSStackView()
    private let instrumentRow = NSView()
    private let instrumentArrows = ArrowKeysIndicator()
    private let instrumentLabel = NSTextField(labelWithString: "")
    private var trackingArea: NSTrackingArea?
    private weak var paletteGlassView: NSView?
    private weak var waveformGlassView: NSView?
    private weak var instrumentRowGlassView: NSView?

    var onHoverChanged: ((Bool) -> Void)?
    var onStepBackward: (() -> Void)?
    var onStepForward: (() -> Void)?
    var onStepUp: (() -> Void)?
    var onStepDown: (() -> Void)?

    private static let waveformHeight: CGFloat = 64
    private static let heldNotesRowHeight: CGFloat = 16
    private static let instrumentRowHeight: CGFloat = 22

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        super.init(frame: .zero)
        wantsLayer = true
        layer?.cornerRadius = 10
        layer?.masksToBounds = true

        contentContainer.translatesAutoresizingMaskIntoConstraints = false
        waveformView.menuBand = menuBand
        waveformView.translatesAutoresizingMaskIntoConstraints = false
        waveformView.setSurfaceStyle(.standard)

        waveformContainer.wantsLayer = true
        waveformContainer.translatesAutoresizingMaskIntoConstraints = false
        waveformContainer.layer?.cornerRadius = 8
        waveformContainer.layer?.masksToBounds = false

        waveformClipView.wantsLayer = true
        waveformClipView.translatesAutoresizingMaskIntoConstraints = false
        waveformClipView.layer?.cornerRadius = 8
        waveformClipView.layer?.masksToBounds = true

        heldNotesContainer.translatesAutoresizingMaskIntoConstraints = false
        heldNotesStack.orientation = .horizontal
        heldNotesStack.alignment = .centerY
        heldNotesStack.spacing = 4
        heldNotesStack.translatesAutoresizingMaskIntoConstraints = false

        instrumentRow.translatesAutoresizingMaskIntoConstraints = false
        instrumentRow.wantsLayer = true
        instrumentRow.layer?.cornerRadius = 7
        if #available(macOS 10.15, *) {
            instrumentRow.layer?.cornerCurve = .continuous
        }
        instrumentArrows.translatesAutoresizingMaskIntoConstraints = false
        instrumentArrows.setContentHuggingPriority(.required, for: .horizontal)
        instrumentArrows.setContentCompressionResistancePriority(.required, for: .horizontal)
        instrumentArrows.displayMode = .horizontalPair
        instrumentArrows.style = .prominent
        instrumentArrows.toolTip = "Change instrument"
        instrumentArrows.onClick = { [weak self] direction, isDown in
            guard let self, isDown else { return }
            switch direction {
            case 0:
                self.onStepBackward?()
            case 1:
                self.onStepForward?()
            case 2:
                self.onStepDown?()
            case 3:
                self.onStepUp?()
            default:
                break
            }
        }

        instrumentLabel.translatesAutoresizingMaskIntoConstraints = false
        instrumentLabel.lineBreakMode = .byTruncatingTail
        instrumentLabel.drawsBackground = false
        instrumentLabel.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)

        addSubview(contentContainer)
        contentContainer.addSubview(waveformContainer)
        contentContainer.addSubview(heldNotesContainer)
        contentContainer.addSubview(instrumentRow)
        waveformContainer.addSubview(waveformClipView)
        waveformClipView.addSubview(waveformView)
        heldNotesContainer.addSubview(heldNotesStack)
        instrumentRow.addSubview(instrumentArrows)
        instrumentRow.addSubview(instrumentLabel)
        installLiquidGlassBackgrounds()

        NSLayoutConstraint.activate([
            widthAnchor.constraint(equalToConstant: KeyboardIconRenderer.imageSize.width),
            contentContainer.leadingAnchor.constraint(equalTo: leadingAnchor),
            contentContainer.trailingAnchor.constraint(equalTo: trailingAnchor),
            contentContainer.topAnchor.constraint(equalTo: topAnchor),
            contentContainer.bottomAnchor.constraint(equalTo: bottomAnchor),

            waveformContainer.leadingAnchor.constraint(equalTo: contentContainer.leadingAnchor),
            waveformContainer.trailingAnchor.constraint(equalTo: contentContainer.trailingAnchor),
            waveformContainer.topAnchor.constraint(equalTo: contentContainer.topAnchor),
            waveformContainer.heightAnchor.constraint(equalToConstant: Self.waveformHeight),

            waveformClipView.leadingAnchor.constraint(equalTo: waveformContainer.leadingAnchor, constant: 5),
            waveformClipView.trailingAnchor.constraint(equalTo: waveformContainer.trailingAnchor, constant: -5),
            waveformClipView.topAnchor.constraint(equalTo: waveformContainer.topAnchor, constant: 5),
            waveformClipView.bottomAnchor.constraint(equalTo: waveformContainer.bottomAnchor, constant: -5),

            waveformView.leadingAnchor.constraint(equalTo: waveformClipView.leadingAnchor),
            waveformView.trailingAnchor.constraint(equalTo: waveformClipView.trailingAnchor),
            waveformView.topAnchor.constraint(equalTo: waveformClipView.topAnchor),
            waveformView.bottomAnchor.constraint(equalTo: waveformClipView.bottomAnchor),

            heldNotesContainer.leadingAnchor.constraint(equalTo: contentContainer.leadingAnchor),
            heldNotesContainer.trailingAnchor.constraint(equalTo: contentContainer.trailingAnchor),
            heldNotesContainer.topAnchor.constraint(equalTo: waveformContainer.bottomAnchor, constant: 2),
            heldNotesContainer.heightAnchor.constraint(equalToConstant: Self.heldNotesRowHeight),

            heldNotesStack.centerXAnchor.constraint(equalTo: heldNotesContainer.centerXAnchor),
            heldNotesStack.centerYAnchor.constraint(equalTo: heldNotesContainer.centerYAnchor),

            instrumentRow.leadingAnchor.constraint(equalTo: contentContainer.leadingAnchor),
            instrumentRow.trailingAnchor.constraint(equalTo: contentContainer.trailingAnchor),
            instrumentRow.topAnchor.constraint(equalTo: heldNotesContainer.bottomAnchor, constant: 2),
            instrumentRow.heightAnchor.constraint(equalToConstant: Self.instrumentRowHeight),
            instrumentRow.bottomAnchor.constraint(equalTo: contentContainer.bottomAnchor, constant: -2),

            instrumentArrows.leadingAnchor.constraint(equalTo: instrumentRow.leadingAnchor, constant: 6),
            instrumentArrows.centerYAnchor.constraint(equalTo: instrumentRow.centerYAnchor),

            instrumentLabel.leadingAnchor.constraint(equalTo: instrumentArrows.trailingAnchor, constant: 6),
            instrumentLabel.centerYAnchor.constraint(equalTo: instrumentRow.centerYAnchor),
            instrumentLabel.trailingAnchor.constraint(equalTo: instrumentRow.trailingAnchor, constant: -8),
        ])

        refresh()
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        nil
    }

    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let trackingArea {
            removeTrackingArea(trackingArea)
        }
        let trackingArea = NSTrackingArea(
            rect: bounds,
            options: [.activeAlways, .inVisibleRect, .mouseEnteredAndExited],
            owner: self,
            userInfo: nil
        )
        addTrackingArea(trackingArea)
        self.trackingArea = trackingArea
    }

    override func mouseEntered(with event: NSEvent) {
        onHoverChanged?(true)
        super.mouseEntered(with: event)
    }

    override func mouseExited(with event: NSEvent) {
        onHoverChanged?(false)
        super.mouseExited(with: event)
    }

    func refresh() {
        guard let menuBand else { return }
        let isDark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        waveformView.setLightMode(!isDark)

        let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
        let familyColor = menuBand.midiMode
            ? NSColor.controlAccentColor
            : InstrumentListView.colorForProgram(safe)

        let waveformBackground = isDark
            ? NSColor(white: 0.06, alpha: 1.0)
            : NSColor(white: 0.82, alpha: 1.0)
        let glassWaveformBackground = isDark
            ? NSColor.white.withAlphaComponent(0.04)
            : NSColor.white.withAlphaComponent(0.18)
        let instrumentRowBackground = isDark
            ? NSColor.white.withAlphaComponent(0.08)
            : NSColor.white.withAlphaComponent(0.3)
        waveformContainer.layer?.backgroundColor = NSColor.clear.cgColor
        waveformClipView.layer?.backgroundColor = Self.shouldUseLiquidGlass
            ? glassWaveformBackground.cgColor
            : waveformBackground.cgColor
        waveformContainer.layer?.borderWidth = 1
        waveformContainer.layer?.borderColor = familyColor.withAlphaComponent(
            Self.shouldUseLiquidGlass ? 0.22 : 0.55
        ).cgColor
        instrumentRow.layer?.backgroundColor = Self.shouldUseLiquidGlass
            ? NSColor.clear.cgColor
            : instrumentRowBackground.cgColor
        instrumentRow.layer?.borderWidth = 1
        instrumentRow.layer?.borderColor = familyColor.withAlphaComponent(
            Self.shouldUseLiquidGlass ? 0.22 : 0.35
        ).cgColor
        instrumentLabel.textColor = isDark ? .white : .black
        instrumentArrows.accentColor = familyColor
        instrumentArrows.isDarkAppearance = isDark

        if menuBand.midiMode {
            waveformView.setDotMatrix(MenuBandPopoverViewController.midiDotPattern)
            waveformView.setBaseColor(.controlAccentColor)
        } else {
            waveformView.setDotMatrix(nil)
            waveformView.setBaseColor(familyColor)
        }

        let shadow = NSShadow()
        shadow.shadowColor = familyColor.withAlphaComponent(isDark ? 0.9 : 0.55)
        shadow.shadowOffset = NSSize(width: 0, height: -1)
        shadow.shadowBlurRadius = 3
        let titleFont: NSFont = {
            if let descriptor = AppDelegate.ywftBoldDescriptor,
               let font = NSFont(descriptor: descriptor, size: 14),
               font.familyName == "YWFT Processing" {
                return font
            }
            return NSFont.systemFont(ofSize: 14, weight: .black)
        }()
        instrumentLabel.attributedStringValue = NSAttributedString(
            string: GeneralMIDI.programNames[safe],
            attributes: [
                .font: titleFont,
                .foregroundColor: isDark ? NSColor.white : NSColor.black,
                .shadow: shadow,
            ]
        )

        for view in heldNotesStack.arrangedSubviews {
            heldNotesStack.removeArrangedSubview(view)
            view.removeFromSuperview()
        }
        for name in menuBand.heldNoteNames() {
            heldNotesStack.addArrangedSubview(makeHeldNoteBox(name: name, color: familyColor))
        }

        if Self.shouldUseLiquidGlass, #available(macOS 26.0, *) {
            let paletteTint = familyColor.withAlphaComponent(menuBand.midiMode ? 0.20 : 0.16)
            (paletteGlassView as? NSGlassEffectView)?.style = .clear
            (paletteGlassView as? NSGlassEffectView)?.tintColor = paletteTint
            (waveformGlassView as? NSGlassEffectView)?.style = .clear
            (waveformGlassView as? NSGlassEffectView)?.tintColor = paletteTint.withAlphaComponent(0.55)
            (instrumentRowGlassView as? NSGlassEffectView)?.style = .clear
            (instrumentRowGlassView as? NSGlassEffectView)?.tintColor = paletteTint.withAlphaComponent(0.42)
            layer?.backgroundColor = NSColor.clear.cgColor
        } else {
            layer?.backgroundColor = (isDark
                ? NSColor(white: 0.06, alpha: 0.96)
                : NSColor(white: 0.88, alpha: 0.96)).cgColor
        }
    }

    func setLive(_ isLive: Bool) {
        waveformView.isLive = isLive
    }

    private func makeHeldNoteBox(name: String, color: NSColor) -> NSView {
        let box = NSView()
        box.translatesAutoresizingMaskIntoConstraints = false
        box.wantsLayer = true
        box.layer?.cornerRadius = 4
        box.layer?.backgroundColor = color.withAlphaComponent(0.85).cgColor

        let label = NSTextField(labelWithString: name)
        label.translatesAutoresizingMaskIntoConstraints = false
        label.font = NSFont.monospacedSystemFont(ofSize: 9, weight: .heavy)
        label.textColor = .black
        box.addSubview(label)

        NSLayoutConstraint.activate([
            label.leadingAnchor.constraint(equalTo: box.leadingAnchor, constant: 5),
            label.trailingAnchor.constraint(equalTo: box.trailingAnchor, constant: -5),
            label.topAnchor.constraint(equalTo: box.topAnchor, constant: 1),
            label.bottomAnchor.constraint(equalTo: box.bottomAnchor, constant: -1),
        ])
        return box
    }

    private func installLiquidGlassBackgrounds() {
        guard Self.shouldUseLiquidGlass, #available(macOS 26.0, *) else { return }

        let paletteGlassView = UnifiedWaveformStripGlassEffectView()
        paletteGlassView.translatesAutoresizingMaskIntoConstraints = false
        paletteGlassView.cornerRadius = 10
        addSubview(paletteGlassView, positioned: .below, relativeTo: contentContainer)
        NSLayoutConstraint.activate([
            paletteGlassView.leadingAnchor.constraint(equalTo: leadingAnchor),
            paletteGlassView.trailingAnchor.constraint(equalTo: trailingAnchor),
            paletteGlassView.topAnchor.constraint(equalTo: topAnchor),
            paletteGlassView.bottomAnchor.constraint(equalTo: bottomAnchor),
        ])
        self.paletteGlassView = paletteGlassView

        self.waveformGlassView = installGlassBackground(
            matchedTo: waveformContainer,
            below: waveformContainer,
            cornerRadius: 8
        )
        self.instrumentRowGlassView = installGlassBackground(
            matchedTo: instrumentRow,
            below: instrumentRow,
            cornerRadius: 7
        )
    }

    @available(macOS 26.0, *)
    private func installGlassBackground(matchedTo target: NSView,
                                        below anchor: NSView,
                                        cornerRadius: CGFloat) -> NSView {
        let glassView = UnifiedWaveformStripGlassEffectView()
        glassView.translatesAutoresizingMaskIntoConstraints = false
        glassView.cornerRadius = cornerRadius
        addSubview(glassView, positioned: .below, relativeTo: anchor)
        NSLayoutConstraint.activate([
            glassView.leadingAnchor.constraint(equalTo: target.leadingAnchor),
            glassView.trailingAnchor.constraint(equalTo: target.trailingAnchor),
            glassView.topAnchor.constraint(equalTo: target.topAnchor),
            glassView.bottomAnchor.constraint(equalTo: target.bottomAnchor),
        ])
        return glassView
    }

}
