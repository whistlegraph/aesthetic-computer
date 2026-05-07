import AppKit

final class PianoWaveformViewController: NSViewController {
    enum PresentationMode {
        case expanded
        case collapsed
    }

    private static let panelCornerRadius: CGFloat = 18

    private let containerView = NSView()
    private let expandedView: ExpandedPianoWaveformView
    private let collapsedView: CollapsedPianoWaveformView
    private let expandCollapseButton = NSButton()
    private var activeContentView: NSView?
    private var presentationMode: PresentationMode = .expanded
    private var isPresented = false
    private var trackingArea: NSTrackingArea?
    private var isMouseInsideView = false
    private weak var expandCollapseButtonGlassView: NSView?
    private let closeButtonSize: CGFloat = 22
    private let closeButtonCornerInset: CGFloat = 3

    var onTogglePresentationMode: (() -> Void)?

    var onStepBackward: (() -> Void)? {
        get { collapsedView.onStepBackward }
        set { collapsedView.onStepBackward = newValue }
    }

    var onStepForward: (() -> Void)? {
        get { collapsedView.onStepForward }
        set { collapsedView.onStepForward = newValue }
    }

    var onStepUp: (() -> Void)? {
        get { collapsedView.onStepUp }
        set { collapsedView.onStepUp = newValue }
    }

    var onStepDown: (() -> Void)? {
        get { collapsedView.onStepDown }
        set { collapsedView.onStepDown = newValue }
    }

    func setArrowHighlight(direction: Int, on: Bool) {
        collapsedView.setArrowHighlight(direction: direction, on: on)
    }

    var isPianoFocusActive: (() -> Bool)? {
        get { expandedView.isPianoFocusActive }
        set { expandedView.isPianoFocusActive = newValue }
    }

    init(menuBand: MenuBandController) {
        self.expandedView = ExpandedPianoWaveformView(menuBand: menuBand)
        self.collapsedView = CollapsedPianoWaveformView(menuBand: menuBand)
        super.init(nibName: nil, bundle: nil)
        preferredContentSize = preferredSize(for: presentationMode)
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        nil
    }

    override func loadView() {
        view = containerView
        containerView.wantsLayer = true
        // Overlay expand/collapse button retired — the panel is now
        // chrome-free. Mode toggle still happens via the popover and
        // the Cmd-Ctrl-Opt-K shortcut. Button object kept around so
        // the rest of the controller (visibility cycling, KVO) keeps
        // compiling without changes.
        expandCollapseButton.isHidden = true
        installTrackingArea()
        installDisplayedView()
        isMouseInsideView = isMouseInsideContainer()
        setOverlayControlsVisible(isMouseInsideView, animated: false)
    }

    func refresh() {
        expandedView.refresh()
        collapsedView.refresh()
        preferredContentSize = preferredSize(for: presentationMode)
    }

    func clearInteraction() {
        expandedView.clearInteraction()
    }

    func setPresented(_ isPresented: Bool) {
        self.isPresented = isPresented
        updatePresentationState()
    }

    func setPresentationMode(_ presentationMode: PresentationMode) {
        guard self.presentationMode != presentationMode else {
            preferredContentSize = preferredSize(for: presentationMode)
            updatePresentationState()
            return
        }
        self.presentationMode = presentationMode
        if isViewLoaded {
            installDisplayedView()
        }
        preferredContentSize = preferredSize(for: presentationMode)
        updatePresentationState()
    }

    private func installDisplayedView() {
        let nextView: NSView
        switch presentationMode {
        case .expanded:
            nextView = expandedView
        case .collapsed:
            nextView = collapsedView
        }

        guard activeContentView !== nextView else { return }
        activeContentView?.removeFromSuperview()
        nextView.translatesAutoresizingMaskIntoConstraints = false
        containerView.addSubview(nextView, positioned: .below, relativeTo: nil)
        NSLayoutConstraint.activate([
            nextView.leadingAnchor.constraint(equalTo: containerView.leadingAnchor),
            nextView.trailingAnchor.constraint(equalTo: containerView.trailingAnchor),
            nextView.topAnchor.constraint(equalTo: containerView.topAnchor),
            nextView.bottomAnchor.constraint(equalTo: containerView.bottomAnchor),
        ])
        activeContentView = nextView
    }

    private func preferredSize(for presentationMode: PresentationMode) -> NSSize {
        switch presentationMode {
        case .expanded:
            return expandedView.fittingSize
        case .collapsed:
            return collapsedView.fittingSize
        }
    }

    private func updatePresentationState() {
        expandedView.setPresented(isPresented && presentationMode == .expanded)
        // collapsed view no longer hosts a live visualizer; nothing to gate.
        [expandCollapseButton, expandCollapseButtonGlassView]
            .compactMap { $0 }
            .forEach { $0.isHidden = false }
        updateExpandCollapseButtonAppearance()
        isMouseInsideView = isMouseInsideContainer()
        setOverlayControlsVisible(isMouseInsideView, animated: false)
        applyOverlayButtonAppearance()
    }

    private func configureOverlayButton(
        _ button: NSButton,
        symbolName: String,
        toolTip: String,
        action: Selector
    ) {
        let config = NSImage.SymbolConfiguration(pointSize: 9, weight: .semibold)
        button.translatesAutoresizingMaskIntoConstraints = false
        button.image = NSImage(systemSymbolName: symbolName, accessibilityDescription: toolTip)?
            .withSymbolConfiguration(config)
        button.isBordered = false
        button.imagePosition = .imageOnly
        button.contentTintColor = .white.withAlphaComponent(0.92)
        button.toolTip = toolTip
        button.target = self
        button.action = action
        // Always visible (no hover fade) — `setOverlayControlsVisible`
        // is now a no-op. Start at 1 so the button paints on first
        // appearance instead of waiting for the tracking-area enter.
        button.alphaValue = 1
        button.wantsLayer = true
        button.layer?.cornerRadius = closeButtonSize / 2
        button.layer?.borderWidth = 1
        if #available(macOS 10.15, *) {
            button.layer?.cornerCurve = .continuous
        }
    }

    private func installOverlayGlassBackgrounds() {
        guard ExpandedPianoWaveformView.shouldUseLiquidGlass, #available(macOS 26.0, *) else { return }
        self.expandCollapseButtonGlassView = installGlassBackground(for: expandCollapseButton)
    }

    @available(macOS 26.0, *)
    private func installGlassBackground(for target: NSView) -> NSView {
        let glassView = ExpandedPianoWaveformGlassEffectView()
        glassView.translatesAutoresizingMaskIntoConstraints = false
        glassView.cornerRadius = closeButtonSize / 2
        containerView.addSubview(glassView, positioned: .below, relativeTo: target)
        NSLayoutConstraint.activate([
            glassView.leadingAnchor.constraint(equalTo: target.leadingAnchor),
            glassView.trailingAnchor.constraint(equalTo: target.trailingAnchor),
            glassView.topAnchor.constraint(equalTo: target.topAnchor),
            glassView.bottomAnchor.constraint(equalTo: target.bottomAnchor),
        ])
        return glassView
    }

    private func setOverlayControlsVisible(_ isVisible: Bool, animated: Bool = true) {
        // Expand/collapse button is always visible now — keep the
        // method around so existing call sites still compile but
        // pin alpha to 1 regardless of hover state. `_ = isVisible`
        // / `_ = animated` swallow the parameters cleanly.
        _ = isVisible
        _ = animated
        expandCollapseButton.alphaValue = 1
        expandCollapseButtonGlassView?.alphaValue = 1
    }

    private func installTrackingArea() {
        if let trackingArea {
            containerView.removeTrackingArea(trackingArea)
        }
        let trackingArea = NSTrackingArea(
            rect: .zero,
            options: [.mouseEnteredAndExited, .activeAlways, .inVisibleRect],
            owner: self,
            userInfo: nil
        )
        containerView.addTrackingArea(trackingArea)
        self.trackingArea = trackingArea
    }

    override func mouseEntered(with event: NSEvent) {
        isMouseInsideView = true
        setOverlayControlsVisible(true)
    }

    override func mouseExited(with event: NSEvent) {
        isMouseInsideView = false
        setOverlayControlsVisible(false)
    }

    private func isMouseInsideContainer() -> Bool {
        guard let window = containerView.window else { return false }
        let location = containerView.convert(window.mouseLocationOutsideOfEventStream, from: nil)
        return containerView.bounds.contains(location)
    }

    private func applyOverlayButtonAppearance() {
        let effectiveView: NSView
        switch presentationMode {
        case .expanded:
            effectiveView = expandedView
        case .collapsed:
            effectiveView = collapsedView
        }
        let isDark = effectiveView.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        let tintColor = expandedView.paletteTintColor
        if ExpandedPianoWaveformView.shouldUseLiquidGlass, #available(macOS 26.0, *) {
            (expandCollapseButtonGlassView as? NSGlassEffectView)?.style = .clear
            (expandCollapseButtonGlassView as? NSGlassEffectView)?.tintColor = tintColor.withAlphaComponent(0.34)
            expandCollapseButton.layer?.backgroundColor = NSColor.clear.cgColor
            expandCollapseButton.layer?.borderColor = NSColor.clear.cgColor
        } else {
            let border = NSColor.white.withAlphaComponent(0.28).cgColor
            let background = NSColor.windowBackgroundColor.withAlphaComponent(isDark ? 0.18 : 0.22).cgColor
            expandCollapseButton.layer?.backgroundColor = background
            expandCollapseButton.layer?.borderColor = border
        }
    }

    private func updateExpandCollapseButtonAppearance() {
        // Quick Look-style fullscreen toggle: diagonal corner arrows
        // pointing outward when collapsed (= "expand"), pointing
        // inward when expanded (= "exit fullscreen").
        let symbolName = presentationMode == .expanded
            ? "arrow.down.right.and.arrow.up.left"
            : "arrow.up.left.and.arrow.down.right"
        let toolTip = presentationMode == .expanded ? "Collapse" : "Expand floating piano"
        let config = NSImage.SymbolConfiguration(pointSize: 9, weight: .semibold)
        expandCollapseButton.image = NSImage(systemSymbolName: symbolName, accessibilityDescription: toolTip)?
            .withSymbolConfiguration(config)
        expandCollapseButton.toolTip = toolTip
    }

    @objc private func expandCollapseClicked(_ sender: NSButton) {
        onTogglePresentationMode?()
    }
}
