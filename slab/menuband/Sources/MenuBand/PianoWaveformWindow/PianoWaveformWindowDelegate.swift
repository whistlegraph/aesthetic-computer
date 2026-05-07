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

    /// Screen-coordinate frame of the floating panel when visible.
    /// Used by AppDelegate's custom popover panel to align its left
    /// edge against the floating panel's right edge.
    var visiblePanelFrame: NSRect? {
        guard let panel, panel.isVisible else { return nil }
        return panel.frame
    }

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
        // Auto-hide is disabled — once the floating panel is up it
        // stays up until the user explicitly dismisses it (popover
        // dismiss, shortcut toggle, or close click). The earlier
        // 2-second timer was firing whenever held notes drained,
        // which felt like the panel was "timing out" mid-play.
        cancelPendingHide()
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

    /// Mirror a keyboard arrow press onto the on-screen arrow
    /// cluster so the easter-egg keyboard nav feels physical.
    /// Direction matches `ArrowKeysIndicator`: 0 ←, 1 →, 2 ↓, 3 ↑.
    func setArrowHighlight(direction: Int, on: Bool) {
        pianoWaveformViewController.setArrowHighlight(direction: direction, on: on)
    }

    func registerArrowInput() {
        guard presentationState == .collapsed, !isCollapsedPresentationSuppressed, isEnabled else { return }
        // Only nudge visibility / focus when already paired with
        // the popover — arrow-key stepping shouldn't be the trigger
        // that pops the panel up; only the music-note icon does.
        let pairedWithPopover = (popoverFrameProvider?() != nil)
        if pairedWithPopover {
            showIfNeeded()
        }
        pianoWaveformViewController.refresh()
        if pairedWithPopover {
            focusCollapsedPaletteIfNeeded()
        }
        // Only auto-hide if we're standalone; when paired with the
        // popover (popoverFrameProvider returns a frame), the panel
        // must stay until the popover closes.
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
        // popUpMenu level + cross-space + full-screen-aux collection
        // behavior keeps the floating piano panel rendered above
        // everything (system menus included) and clickable across
        // Spaces / Mission Control / fullscreen apps. Same trick
        // clock / calculator widgets use to stay reachable from
        // anywhere; the expanded surface in particular is meant to
        // be a "displays always, on top of everything" instrument.
        panel.level = .popUpMenu
        panel.animationBehavior = .none
        panel.collectionBehavior = [
            .transient,
            .canJoinAllSpaces,
            .fullScreenAuxiliary,
            .stationary,
            .ignoresCycle,
        ]
        panel.hidesOnDeactivate = false
        panel.canHide = false
        // Locked in place for now — positioning logic is in flux
        // and a draggable panel just lets clicks on the chooser /
        // held-notes / button area drift it off snug-pair with
        // the popover. Both background-drag and title-bar drag
        // are disabled.
        panel.isMovable = false
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
        // Popover dismiss is the canonical "go away" signal for
        // the paired panel. Fade out (~140 ms) instead of
        // cutting so the panel dissolves alongside the popover.
        menuBand.releaseAllHeldNotes()
        pianoWaveformViewController.setPresented(false)
        panel?.ignoresMouseEvents = false
        guard let panel = panel else { return }
        NSAnimationContext.runAnimationGroup({ ctx in
            ctx.duration = 0.14
            ctx.timingFunction = CAMediaTimingFunction(name: .easeOut)
            panel.animator().alphaValue = 0
        }, completionHandler: { [weak panel] in
            panel?.orderOut(nil)
            panel?.alphaValue = 1
        })
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
        // The expanded "large" panel is a full-display surface — it
        // always centers on the active screen, no matter where the
        // popover sits or which entry path opened it. (Earlier this
        // shared the small-panel anchoring logic, which dragged the
        // expanded panel sideways when the popover moved.) The
        // popover-paired small panel keeps the relative anchor.
        _ = fallbackOrigin
        return clampedFrame(
            origin: centeredOrigin(for: size),
            size: size,
            preferredScreen: panel?.screen ?? NSScreen.main
        )
    }

    private func collapsedFrame(size: NSSize) -> NSRect {
        // Always anchor under the piano keys section when we have a
        // status item button — the user's dragged-to origin only
        // applies when the panel is fully detached (no popover).
        guard let anchoredFrame = anchoredCollapsedFrame(size: size) else {
            return clampedFrame(
                origin: collapsedCustomOrigin ?? centeredOrigin(for: size),
                size: size,
                preferredScreen: panel?.screen ?? NSScreen.main
            )
        }
        // If the popover is up, ignore the saved custom origin so the
        // panel pairs cleanly under the piano keys.
        if popoverFrameProvider?() != nil {
            return anchoredFrame
        }
        guard let collapsedCustomOrigin else { return anchoredFrame }
        return clampedFrame(
            origin: collapsedCustomOrigin,
            size: anchoredFrame.size,
            preferredScreen: panel?.screen ?? statusItemButton?.window?.screen
        )
    }

    private func anchoredCollapsedFrame(size: NSSize) -> NSRect? {
        // Right edge of the panel aligns with the LEFT edge of the
        // popover: panel.maxX = popover.minX. Top-aligned to
        // popover.maxY. Panel hangs off the popover's left side
        // forming a single horizontal strip with the popover on
        // the right.
        guard let button = statusItemButton,
              let buttonWindow = button.window else { return nil }

        let imgSize = KeyboardIconRenderer.imageSize
        let buttonBounds = button.bounds
        let xOffset = (buttonBounds.width - imgSize.width) / 2.0
        let buttonScreenFrame = buttonWindow.convertToScreen(button.frame)
        let menubarBottom = buttonScreenFrame.minY
        let latch = KeyboardIconRenderer.settingsRectPublic
        let gearLocal = NSPoint(x: xOffset + latch.midX, y: 0)
        let gearWindow = button.convert(gearLocal, to: nil)
        let gearScreen = buttonWindow.convertPoint(toScreen: gearWindow)
        let popoverWidth = popoverFrameProvider?()?.width
            ?? popoverPreferredWidth
            ?? 360

        // popover.minX = gear.x - popoverWidth/2 (live or predicted).
        let popoverLeft: CGFloat = popoverFrameProvider?()?.minX
            ?? (gearScreen.x - popoverWidth / 2)
        // Match the panel's top to the popover BODY's top, not its
        // arrow tip. The popover frame includes the arrow strip
        // (arrowHeight points above the body), so subtract that
        // to land the panel's top edge flush with the body.
        let popoverTop: CGFloat = popoverFrameProvider?()?.maxY ?? menubarBottom
        let bodyTop = popoverTop - MenuBandPopoverPanel.arrowHeight

        // Small horizontal gap so the panel doesn't pixel-touch the
        // popover — reads as a paired-but-discrete duo.
        let gap: CGFloat = 6
        let x = popoverLeft - size.width - gap
        let y = bodyTop - size.height
        return NSRect(x: x, y: y, width: size.width, height: size.height)
    }

    /// Optional popover-width hint set by AppDelegate when it
    /// builds the popover VC. Falls back to a reasonable default
    /// (360pt) when the hint isn't available — keeps the predicted
    /// right edge close enough to the live one that the panel
    /// doesn't visibly hop the first time the popover opens.
    var popoverPreferredWidth: CGFloat?

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
        // Always show the collapsed panel for click-driven entry.
        // Earlier this honored the saved `preferredPresentationState`,
        // which meant once the user opened the expanded surface
        // even once, every later click on the LED chip dumped them
        // back into it — surprising when they were just trying to
        // peek at the small panel. Expanded is now only reachable
        // via the explicit expand button + dedicated focus
        // shortcut, so a casual chip click can't blow it up.
        _ = previousApp
        if presentationState == .expanded {
            dismissExpanded(reason: .programmatic)
        }
        presentationState = .collapsed
        showIfNeeded()
        if !menuBand.litNotes.isEmpty {
            focusCollapsedPaletteIfNeeded()
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

