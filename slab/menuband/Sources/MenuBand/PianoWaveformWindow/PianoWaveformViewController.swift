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
    private let closeButton = OverlayCircleButton()
    private var activeContentView: NSView?
    private var presentationMode: PresentationMode = .expanded
    private var isPresented = false
    private var trackingArea: NSTrackingArea?
    private var isMouseInsideView = false
    private let closeButtonCircle = CircleBackgroundView()
    private weak var closeButtonGlassView: NSView?
    private let closeButtonSize: CGFloat = 30
    private let closeButtonCornerInset: CGFloat = 3

    var onClose: (() -> Void)?

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

    var onOpenKeymap: (() -> Void)? {
        get { collapsedView.onOpenKeymap }
        set { collapsedView.onOpenKeymap = newValue }
    }

    /// Click either LED scope — the keymap window's big one or the collapsed
    /// cluster's little one — and it blows up to the full-screen visualizer.
    ///
    /// This used to forward to `expandedView` only, on the theory that "the
    /// collapsed cluster's little scope is wired separately, by the popover."
    /// That's true of the POPOVER's cluster, which is a different instance —
    /// the one in this window never got the closure, so its
    /// `onOpenVisualizer` stayed nil and `waveformStrip.onClick` fell through
    /// to its `onOpenKeymap` fallback. Clicking the scope here opened the
    /// keymap instead of the visualizer, and quietly: nil-coalescing to
    /// another real action makes a missing wire look like a design choice.
    var onOpenVisualizer: (() -> Void)? {
        get { expandedView.onOpenVisualizer }
        set {
            expandedView.onOpenVisualizer = newValue
            collapsedView.onOpenVisualizer = newValue
        }
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
        installTrackingArea()
        installDisplayedView()
        installOverlayControls()
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
        let isExpanded = presentationMode == .expanded
        closeButton.isHidden = !isExpanded
        closeButtonGlassView?.isHidden = !isExpanded
        isMouseInsideView = isMouseInsideContainer()
        setOverlayControlsVisible(isMouseInsideView, animated: false)
        applyOverlayButtonAppearance()
    }

    private func installOverlayControls() {
        configureOverlayButton(
            closeButton,
            symbolName: "xmark",
            toolTip: "Close floating piano",
            action: #selector(closeClicked(_:))
        )

        closeButtonCircle.translatesAutoresizingMaskIntoConstraints = false
        closeButtonCircle.alphaValue = 0
        containerView.addSubview(closeButtonCircle)
        containerView.addSubview(closeButton)
        installOverlayGlassBackgrounds()

        let buttonInset = Self.panelCornerRadius - closeButtonSize / 2 + closeButtonCornerInset
        NSLayoutConstraint.activate([
            closeButton.widthAnchor.constraint(equalToConstant: closeButtonSize),
            closeButton.heightAnchor.constraint(equalToConstant: closeButtonSize),
            closeButton.leadingAnchor.constraint(equalTo: containerView.leadingAnchor, constant: buttonInset),
            closeButton.topAnchor.constraint(equalTo: containerView.topAnchor, constant: buttonInset),

            closeButtonCircle.centerXAnchor.constraint(equalTo: closeButton.centerXAnchor),
            closeButtonCircle.centerYAnchor.constraint(equalTo: closeButton.centerYAnchor),
            closeButtonCircle.widthAnchor.constraint(equalToConstant: closeButtonSize),
            closeButtonCircle.heightAnchor.constraint(equalToConstant: closeButtonSize),
        ])
    }

    private func configureOverlayButton(
        _ button: NSButton,
        symbolName: String,
        toolTip: String,
        action: Selector
    ) {
        let config = NSImage.SymbolConfiguration(pointSize: 12, weight: .semibold)
        button.translatesAutoresizingMaskIntoConstraints = false
        button.image = NSImage(systemSymbolName: symbolName, accessibilityDescription: toolTip)?
            .withSymbolConfiguration(config)
        button.isBordered = false
        button.imagePosition = .imageOnly
        button.contentTintColor = .white.withAlphaComponent(0.92)
        button.toolTip = toolTip
        button.target = self
        button.action = action
        button.alphaValue = 0
    }

    private func installOverlayGlassBackgrounds() {
        guard ExpandedPianoWaveformView.shouldUseLiquidGlass, #available(macOS 26.0, *) else { return }
        self.closeButtonGlassView = installGlassBackground(for: closeButton)
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
        let alpha: CGFloat = isVisible ? 1 : 0
        let allViews: [NSView] = [
            closeButton,
            closeButtonCircle,
            closeButtonGlassView,
        ].compactMap { $0 }
        if animated {
            NSAnimationContext.runAnimationGroup { context in
                context.duration = 0.12
                context.timingFunction = CAMediaTimingFunction(name: .easeOut)
                allViews.forEach { $0.animator().alphaValue = alpha }
            }
        } else {
            allViews.forEach { $0.alphaValue = alpha }
        }
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
            (closeButtonGlassView as? NSGlassEffectView)?.style = .clear
            (closeButtonGlassView as? NSGlassEffectView)?.tintColor = tintColor.withAlphaComponent(0.34)
            closeButtonCircle.fillColor = .clear
            closeButtonCircle.strokeColor = .clear
        } else {
            closeButtonCircle.fillColor = NSColor.windowBackgroundColor.withAlphaComponent(isDark ? 0.18 : 0.22)
            closeButtonCircle.strokeColor = NSColor.white.withAlphaComponent(0.28)
        }
    }

    @objc private func closeClicked(_ sender: NSButton) {
        onClose?()
    }
}

private final class OverlayCircleButton: NSButton {
    override var intrinsicContentSize: NSSize {
        NSSize(width: NSView.noIntrinsicMetric, height: NSView.noIntrinsicMetric)
    }
}

private final class CircleBackgroundView: NSView {
    var fillColor: NSColor = .clear { didSet { needsDisplay = true } }
    var strokeColor: NSColor = .clear { didSet { needsDisplay = true } }

    override var intrinsicContentSize: NSSize {
        NSSize(width: NSView.noIntrinsicMetric, height: NSView.noIntrinsicMetric)
    }

    override func draw(_ dirtyRect: NSRect) {
        let circle = NSBezierPath(ovalIn: bounds.insetBy(dx: 0.5, dy: 0.5))
        fillColor.setFill()
        circle.fill()
        strokeColor.setStroke()
        circle.lineWidth = 1
        circle.stroke()
    }
}
