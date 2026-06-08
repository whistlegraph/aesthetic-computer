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
    /// Read-only accessor for the active floating panel so other
    /// parts of the app (e.g. AppDelegate's octave-scroll monitor)
    /// can match incoming events against this window without
    /// taking ownership of the panel lifecycle.
    var activeWindow: NSWindow? { panel }
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

    /// [v1] The floating piano window is retired — its content folded
    /// into the popover (single column). Every present-the-panel entry
    /// point bails on this flag so the window never appears, while the
    /// delegate object + its callbacks stay intact so AppDelegate's
    /// many call sites keep compiling. Flip to false to revive it.
    static let retiredForV1 = true

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

    var onOpenKeymap: (() -> Void)? {
        get { pianoWaveformViewController.onOpenKeymap }
        set { pianoWaveformViewController.onOpenKeymap = newValue }
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

    /// True while the collapsed pitch strip is on screen because the
    /// pointer is hovering the menubar item (not because the popover or
    /// a user action opened it). Gates the hover auto-hide so we never
    /// dismiss a popover-paired or user-pinned panel.
    private var hoverPresented = false

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

        pianoWaveformViewController.onClose = { [weak self] in
            self?.dismiss(reason: .closeButton)
        }
    }

    func toggleFromShortcut() {
        if Self.retiredForV1 { return }
        if isEnabled {
            disable(reason: .shortcut)
            return
        }
        enableAndShowPreferred(restoringTo: nil)
    }

    func showFromCommand(restoringTo previousApp: NSRunningApplication? = nil) {
        if Self.retiredForV1 { return }
        enableAndShowPreferred(restoringTo: previousApp)
    }

    func show(restoringTo previousApp: NSRunningApplication? = nil) {
        if Self.retiredForV1 { return }
        enableAndShowPreferred(restoringTo: previousApp)
    }

    /// Force the expanded presentation regardless of saved preferred
    /// state — used when the popover opens so the GM chooser inside
    /// the expanded panel is what the user sees, not the collapsed
    /// strip (which has no chooser).
    func showExpandedForPopover(restoringTo previousApp: NSRunningApplication? = nil) {
        // NOTE: intentionally NOT gated by `retiredForV1`. The rest of the
        // floating-window system stays retired, but this is the one live
        // entry point — the popover's "Keymap" button opens the full-screen
        // view as the official keymap screen (large piano + QWERTY + mode).
        setEnabled(true)
        cancelPendingHide()
        hoverPresented = false  // popover is driving now, not hover
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
        if Self.retiredForV1 { return }
        setEnabled(true)
        cancelPendingHide()
        hoverPresented = false  // popover is driving now, not hover
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
                panel.allowsSurfaceDrag = true
                setPanelFrame(expandedFrame(
                    size: pianoWaveformViewController.preferredContentSize,
                    fallbackOrigin: panel.frame.origin
                ))
            }
        case .collapsed:
            pianoWaveformViewController.setPresentationMode(.collapsed)
            pianoWaveformViewController.refresh()
            if let panel, panel.isVisible {
                panel.allowsSurfaceDrag = false
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
        if Self.retiredForV1 { return }
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

    // MARK: - Menubar hover reveal

    /// Reveal the collapsed pitch strip while the pointer is over the
    /// menubar item. Anchors under the status item (no popover is up).
    /// Idempotent and non-persistent: it never touches the saved
    /// preferred state, and it bows out while the popover or the
    /// expanded view is driving the panel so it can't fight them.
    func showFromHover() {
        if Self.retiredForV1 { return }
        guard !isCollapsedPresentationSuppressed, popoverFrameProvider?() == nil else { return }
        if presentationState == .expanded, panel?.isVisible == true { return }
        // Already up because of hover → just keep it up.
        if hoverPresented, presentationState == .collapsed, panel?.isVisible == true {
            cancelPendingHide()
            return
        }
        setEnabled(true)
        cancelPendingHide()
        if panel == nil { buildPanel() }
        if presentationState == .expanded { dismissExpanded(reason: .programmatic) }
        presentationState = .collapsed
        hoverPresented = true
        showCollapsedIfNeeded()
    }

    /// Begin a hover-driven hide. Defers (re-arming itself) while the
    /// pointer is still over the panel or the menubar item, so moving
    /// from the icon down into the strip doesn't dismiss it; once the
    /// pointer is truly away the strip fades out.
    func scheduleHideFromHover() {
        guard hoverPresented else { return }
        cancelPendingHide()
        let work = DispatchWorkItem { [weak self] in self?.hoverHideTick() }
        hideWorkItem = work
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.30, execute: work)
    }

    private func hoverHideTick() {
        guard hoverPresented, presentationState == .collapsed else { return }
        // The popover took over → leave that flow alone.
        if popoverFrameProvider?() != nil { hoverPresented = false; return }
        let mouse = NSEvent.mouseLocation
        let overPanel = (panel?.isVisible == true) && (panel?.frame.contains(mouse) ?? false)
        let overButton: Bool = {
            guard let b = statusItemButton, let w = b.window else { return false }
            return w.convertToScreen(b.frame).contains(mouse)
        }()
        if overPanel || overButton {
            // Still hovering the icon→panel duo — re-check shortly.
            let work = DispatchWorkItem { [weak self] in self?.hoverHideTick() }
            hideWorkItem = work
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.30, execute: work)
            return
        }
        hoverPresented = false
        dismissCollapsedPanel()
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
        // Surface dragging is gated by `PianoWaveformPanel`: passive
        // expanded-view areas move the window, while keys, keymaps,
        // switches, and buttons keep their own clicks.
        panel.isMovable = true
        panel.isMovableByWindowBackground = false
        panel.allowsSurfaceDrag = presentationState == .expanded
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
        panel.allowsSurfaceDrag = true
        setPanelFrame(expandedFrame(
            size: pianoWaveformViewController.preferredContentSize,
            fallbackOrigin: savedExpandedOrigin ?? panel.frame.origin
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
        panel.allowsSurfaceDrag = false
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
        panel?.allowsSurfaceDrag = false
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
        hoverPresented = false
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
        panel?.allowsSurfaceDrag = true
        pianoWaveformViewController.setPresentationMode(.expanded)
    }

    private func collapseToStrip() {
        guard presentationState != .collapsed else {
            preferredPresentationState = .collapsed
            persistPreferredState()
            panel?.allowsSurfaceDrag = false
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
        panel?.allowsSurfaceDrag = false
        pianoWaveformViewController.setPresentationMode(.collapsed)
        pianoWaveformViewController.refresh()
        showCollapsedIfNeeded()
        if menuBand.litNotes.isEmpty {
            scheduleHide()
        }
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
                if isDown && MenuBandShortcutPreferences.exitFocusShortcut.matches(event: event) {
                    self.onFocusRelease?()
                    return nil
                }
                if isDown && MenuBandShortcutPreferences.layoutShortcut.matches(event: event) {
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
        // The expanded panel opens from the user's last dragged
        // position when one exists, otherwise it centers on the
        // active screen. The popover-paired small panel keeps the
        // relative anchor separately in `collapsedFrame`.
        let origin = savedExpandedOrigin ?? fallbackOrigin ?? centeredOrigin(for: size)
        return clampedFrame(
            origin: origin,
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
        // Cap the panel to the usable screen so a tall pitch grid "maxes
        // out under the menu bar" instead of overflowing off the bottom:
        // pin the top just beneath the menu bar (visible.maxY) and clip
        // the height to whatever fits down to the dock/edge.
        let h = min(size.height, visible.height - 2 * margin)
        let w = min(size.width, visible.width - 2 * margin)
        let x = min(max(origin.x, visible.minX + margin), visible.maxX - w - margin)
        let y = min(max(origin.y, visible.minY + margin), visible.maxY - h - margin)
        return NSRect(origin: NSPoint(x: x, y: y), size: NSSize(width: w, height: h))
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
