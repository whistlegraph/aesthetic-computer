import AppKit

final class PianoWaveformWindowDelegate: NSObject, NSWindowDelegate {
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
    private let pianoWaveformViewController: PianoWaveformViewController
    private var panel: PianoWaveformPanel?
    private weak var statusItemButton: NSStatusBarButton?
    private var presentationState: State = .collapsed
    private var preferredPresentationState: State
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
        get { pianoWaveformViewController.isPianoFocusActive }
        set { pianoWaveformViewController.isPianoFocusActive = newValue }
    }

    var isShown: Bool {
        presentationState == .expanded && panel?.isVisible == true
    }

    var isKeyboardFocused: Bool {
        presentationState == .expanded && panel?.isKeyWindow == true
    }

    var isCollapsedState: Bool { presentationState == .collapsed }

    var isFeatureEnabled: Bool { isEnabled }

    var onStepBackward: (() -> Void)? {
        get { pianoWaveformViewController.onStepBackward }
        set { pianoWaveformViewController.onStepBackward = newValue }
    }

    var onStepForward: (() -> Void)? {
        get { pianoWaveformViewController.onStepForward }
        set { pianoWaveformViewController.onStepForward = newValue }
    }

    var onStepUp: (() -> Void)? {
        get { pianoWaveformViewController.onStepUp }
        set { pianoWaveformViewController.onStepUp = newValue }
    }

    var onStepDown: (() -> Void)? {
        get { pianoWaveformViewController.onStepDown }
        set { pianoWaveformViewController.onStepDown = newValue }
    }

    var isCollapsedPresentationSuppressed: Bool = false {
        didSet {
            guard isCollapsedPresentationSuppressed != oldValue else { return }
            if isCollapsedPresentationSuppressed && presentationState == .collapsed {
                dismissCollapsedPanel()
            }
        }
    }

    var isDocked: Bool { collapsedCustomOrigin == nil }

    /// When the popover is on screen, its window frame is fed in here
    /// so the floating panel can right-align snug against the popover's
    /// left edge instead of anchoring under the menubar status item.
    /// Returning nil falls back to the status-item anchor.
    var popoverFrameProvider: (() -> NSRect?)?

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        self.pianoWaveformViewController = PianoWaveformViewController(menuBand: menuBand)
        self.preferredPresentationState = Self.loadPreferredState()
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

        pianoWaveformViewController.onTogglePresentationMode = { [weak self] in
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
        enableAndShowPreferred(restoringTo: previousApp)
    }

    func show(restoringTo previousApp: NSRunningApplication? = nil) {
        enableAndShowPreferred(restoringTo: previousApp)
    }

    /// Force the expanded presentation regardless of saved preferred
    /// state — used when the popover opens so the GM chooser inside
    /// the expanded panel is what the user sees, not the collapsed
    /// strip (which has no chooser).
    func showExpandedForPopover(restoringTo previousApp: NSRunningApplication? = nil) {
        setEnabled(true)
        cancelPendingHide()
        transitionToExpanded()
        showExpanded(restoringTo: previousApp)
    }

    /// Force the collapsed presentation regardless of saved preferred
    /// state. Used when pairing with the popover — the popover always
    /// brings up the small panel; the expanded view is only reachable
    /// by user action (the expand button on the small panel).
    /// The user's saved preferred state is left untouched so a
    /// standalone open later still honors it.
    func showCollapsedForPopover() {
        setEnabled(true)
        cancelPendingHide()
        if panel == nil { buildPanel() }
        if presentationState == .expanded {
            dismissExpanded(reason: .programmatic)
        }
        presentationState = .collapsed
        showCollapsedIfNeeded()
    }

    func dismiss(reason: DismissReason = .programmatic) {
        // Closes whichever panel is currently presented. The popover
        // pairs the panel one-to-one, so when the popover dismisses
        // we want both expanded and collapsed forms to go away.
        switch presentationState {
        case .expanded:
            dismissExpanded(reason: reason)
        case .collapsed:
            dismissCollapsedPanel()
        }
    }

    func refresh() {
        switch presentationState {
        case .expanded:
            pianoWaveformViewController.setPresentationMode(.expanded)
            pianoWaveformViewController.refresh()
            if let panel, panel.isVisible {
                setPanelFrame(expandedFrame(
                    size: pianoWaveformViewController.preferredContentSize,
                    fallbackOrigin: panel.frame.origin
                ))
            }
        case .collapsed:
            pianoWaveformViewController.setPresentationMode(.collapsed)
            pianoWaveformViewController.refresh()
            if let panel, panel.isVisible {
                setPanelFrame(collapsedFrame(size: pianoWaveformViewController.preferredContentSize))
            }
        }
    }

    func clearInteraction() {
        pianoWaveformViewController.clearInteraction()
    }

    func releaseKeyboardFocus() {
        guard presentationState == .expanded else { return }
        restorePreviousAppFocus()
    }

    func warmUp() {
        if panel == nil {
            buildPanel()
        }
        pianoWaveformViewController.setPresentationMode(.collapsed)
        pianoWaveformViewController.refresh()
    }

    func showIfNeeded() {
        guard presentationState == .collapsed, !isCollapsedPresentationSuppressed, isEnabled, preferredPresentationState == .collapsed else { return }
        cancelPendingHide()
        if panel == nil {
            buildPanel()
        }
        pianoWaveformViewController.setPresentationMode(.collapsed)
        pianoWaveformViewController.refresh()
        showCollapsedIfNeeded()
        if !menuBand.litNotes.isEmpty {
            focusCollapsedPaletteIfNeeded()
        }
    }

    func scheduleHide() {
        guard presentationState == .collapsed else { return }
        cancelPendingHide()
        let work = DispatchWorkItem { [weak self] in
            self?.dismissCollapsedPanel()
        }
        hideWorkItem = work
        DispatchQueue.main.asyncAfter(deadline: .now() + Self.collapsedHideDelay, execute: work)
    }

    func reposition(statusItemButton: NSStatusBarButton?) {
        self.statusItemButton = statusItemButton
        guard presentationState == .collapsed, let panel, panel.isVisible else { return }
        setPanelFrame(collapsedFrame(size: pianoWaveformViewController.preferredContentSize))
    }

    func refreshAppearance() {
        guard presentationState == .collapsed else { return }
        pianoWaveformViewController.setPresentationMode(.collapsed)
        pianoWaveformViewController.refresh()
        if let panel, panel.isVisible {
            setPanelFrame(collapsedFrame(size: pianoWaveformViewController.preferredContentSize))
        }
    }

    func registerArrowInput() {
        guard presentationState == .collapsed, !isCollapsedPresentationSuppressed, isEnabled else { return }
        showIfNeeded()
        pianoWaveformViewController.refresh()
        focusCollapsedPaletteIfNeeded()
        // Only auto-hide if we're standalone; when paired with the
        // popover (popoverFrameProvider returns a frame), the panel
        // must stay until the popover closes.
        let pairedWithPopover = (popoverFrameProvider?() != nil)
        if menuBand.litNotes.isEmpty && !pairedWithPopover {
            scheduleHide()
        } else {
            cancelPendingHide()
        }
    }

    func windowDidMove(_ notification: Notification) {
        guard let panel, !isPositioningPanel else { return }
        switch presentationState {
        case .expanded:
            savedExpandedOrigin = panel.frame.origin
            persistOrigin(savedExpandedOrigin, xKey: Self.expandedOriginXKey, yKey: Self.expandedOriginYKey)
        case .collapsed:
            collapsedCustomOrigin = panel.frame.origin
            persistOrigin(collapsedCustomOrigin, xKey: Self.collapsedOriginXKey, yKey: Self.collapsedOriginYKey)
        }
    }

    private func buildPanel() {
        // Singleton guard — every show/refresh path already passes
        // `if panel == nil { buildPanel() }`, but be explicit here
        // so a future caller can't accidentally rebuild a fresh
        // panel and leave the prior one orphaned on screen.
        guard panel == nil else { return }
        let panel = PianoWaveformPanel(
            contentRect: NSRect(origin: .zero, size: pianoWaveformViewController.preferredContentSize),
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
        // Drag-by-background is off — the panel always pairs with the
        // popover (snug-left), so a draggable body just lets clicks
        // on the chooser / held-notes / button area accidentally
        // move the window.
        panel.isMovableByWindowBackground = false
        panel.acceptsMouseMovedEvents = true
        panel.titleVisibility = .hidden
        panel.titlebarAppearsTransparent = true
        panel.isReleasedWhenClosed = false
        panel.contentViewController = pianoWaveformViewController
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
        pianoWaveformViewController.setPresentationMode(.expanded)
        pianoWaveformViewController.refresh()
        panel.ignoresMouseEvents = false
        setPanelFrame(expandedFrame(
            size: pianoWaveformViewController.preferredContentSize,
            fallbackOrigin: panel.frame.origin
        ))
        NSApp.activate(ignoringOtherApps: true)
        panel.makeKeyAndOrderFront(nil)
        pianoWaveformViewController.setPresented(true)
        installMonitors()
    }

    private func showCollapsedIfNeeded() {
        guard let panel else { return }
        cancelPendingHide()
        pianoWaveformViewController.setPresentationMode(.collapsed)
        panel.ignoresMouseEvents = false
        setPanelFrame(collapsedFrame(size: pianoWaveformViewController.preferredContentSize))
        if !panel.isVisible {
            panel.orderFrontRegardless()
        }
        pianoWaveformViewController.setPresented(true)
    }

    private func focusCollapsedPaletteIfNeeded() {
        guard presentationState == .collapsed, let panel else { return }
        cancelPendingHide()
        NSApp.activate(ignoringOtherApps: true)
        panel.makeKeyAndOrderFront(nil)
        pianoWaveformViewController.setPresented(true)
    }

    private func dismissExpanded(reason: DismissReason) {
        guard !isDismissing else { return }
        guard panel?.isVisible == true || keyMonitor != nil else { return }
        isDismissing = true
        removeMonitors()
        pianoWaveformViewController.setPresented(false)
        pianoWaveformViewController.clearInteraction()
        menuBand.releaseAllHeldNotes()
        panel?.ignoresMouseEvents = false
        panel?.orderOut(nil)
        presentationState = .collapsed
        dismissHandler?()
        if reason.shouldRestoreFocus {
            restorePreviousAppFocus()
        }
        appBeforeOpen = nil
        isDismissing = false
    }

    private func dismissCollapsedPanel() {
        cancelPendingHide()
        // Hard close — popover dismiss is the canonical "go away"
        // signal for the paired panel. Earlier this guarded on held
        // notes (kept the panel open while you were still pressing
        // a chooser cell), but that left a stale panel on screen
        // when the popover came back, doubling instances.
        menuBand.releaseAllHeldNotes()
        pianoWaveformViewController.setPresented(false)
        panel?.ignoresMouseEvents = false
        panel?.orderOut(nil)
    }

    private func togglePresentationMode() {
        switch presentationState {
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
            setPanelFrame(collapsedFrame(size: pianoWaveformViewController.preferredContentSize))
        }
    }

    private func transitionToExpanded() {
        if panel == nil {
            buildPanel()
        }
        presentationState = .expanded
        preferredPresentationState = .expanded
        persistPreferredState()
        panel?.ignoresMouseEvents = false
        pianoWaveformViewController.setPresentationMode(.expanded)
    }

    private func collapseToStrip() {
        guard presentationState != .collapsed else {
            preferredPresentationState = .collapsed
            persistPreferredState()
            pianoWaveformViewController.setPresentationMode(.collapsed)
            pianoWaveformViewController.refresh()
            showIfNeeded()
            if menuBand.litNotes.isEmpty {
                scheduleHide()
            }
            return
        }
        cancelPendingHide()
        removeMonitors()
        pianoWaveformViewController.setPresented(false)
        pianoWaveformViewController.clearInteraction()
        presentationState = .collapsed
        preferredPresentationState = .collapsed
        persistPreferredState()
        pianoWaveformViewController.setPresentationMode(.collapsed)
        pianoWaveformViewController.refresh()
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
                guard let self, self.presentationState == .expanded, self.panel?.isKeyWindow == true else { return event }
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
        // Popover-snug positioning wins over the saved drag origin so
        // the expanded panel pairs cleanly with the popover when both
        // are on screen.
        if let popoverRect = popoverFrameProvider?(), !popoverRect.isEmpty {
            return NSRect(
                x: popoverRect.minX - size.width,
                y: popoverRect.maxY - size.height,
                width: size.width,
                height: size.height
            )
        }
        let origin = fallbackOrigin ?? savedExpandedOrigin
        return clampedFrame(
            origin: origin ?? centeredOrigin(for: size),
            size: size,
            preferredScreen: panel?.screen ?? NSScreen.main
        )
    }

    private func collapsedFrame(size: NSSize) -> NSRect {
        // Popover-snug positioning always wins over the user's
        // dragged-to position. The floating panel pairs with the
        // popover; honoring a stale custom origin while the popover
        // is up would scatter the two surfaces.
        if let popoverRect = popoverFrameProvider?(), !popoverRect.isEmpty {
            return NSRect(
                x: popoverRect.minX - size.width,
                y: popoverRect.maxY - size.height,
                width: size.width,
                height: size.height
            )
        }
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
        // Snug-left-of-popover takes precedence whenever the popover is
        // on screen — the floating panel's right edge sits flush
        // against the popover's left edge, tops aligned, so the two
        // surfaces read as one continuous strip with the popover on
        // the right and the floating piano on the left.
        if let popoverRect = popoverFrameProvider?(), !popoverRect.isEmpty {
            return NSRect(
                x: popoverRect.minX - size.width,
                y: popoverRect.maxY - size.height,
                width: size.width,
                height: size.height
            )
        }

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

    func cancelPendingHide() {
        hideWorkItem?.cancel()
        hideWorkItem = nil
    }

    private func enableAndShowPreferred(restoringTo previousApp: NSRunningApplication?) {
        setEnabled(true)
        cancelPendingHide()
        switch preferredPresentationState {
        case .expanded:
            transitionToExpanded()
            showExpanded(restoringTo: previousApp)
        case .collapsed:
            presentationState = .collapsed
            showIfNeeded()
            if !menuBand.litNotes.isEmpty {
                focusCollapsedPaletteIfNeeded()
            }
        }
    }

    private func disable(reason: DismissReason) {
        setEnabled(false)
        switch presentationState {
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
        let value = preferredPresentationState == .expanded ? "expanded" : "collapsed"
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

