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
    private let closeButton = NSButton()
    private let dockButton = NSButton()
    private let expandCollapseButton = NSButton()
    private var activeContentView: NSView?
    private var presentationMode: PresentationMode = .expanded
    private var isPresented = false
    private var trackingArea: NSTrackingArea?
    private var isMouseInsideView = false
    private weak var closeButtonGlassView: NSView?
    private weak var dockButtonGlassView: NSView?
    private weak var expandCollapseButtonGlassView: NSView?
    private let closeButtonSize: CGFloat = 30
    private let closeButtonCornerInset: CGFloat = 3
    private let gap: CGFloat = 8

    var onCloseRequested: (() -> Void)?

    var onDockRequested: (() -> Void)?

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
        configureOverlayButton(
            closeButton,
            symbolName: "xmark",
            toolTip: "Close",
            action: #selector(closeClicked(_:))
        )
        configureOverlayButton(
            dockButton,
            symbolName: "menubar.dock.rectangle",
            toolTip: "Dock Below Menubar Piano",
            action: #selector(dockClicked(_:))
        )
        configureOverlayButton(
            expandCollapseButton,
            symbolName: "square.resize.down",
            toolTip: "Collapse",
            action: #selector(expandCollapseClicked(_:))
        )
        containerView.addSubview(closeButton)
        containerView.addSubview(dockButton)
        containerView.addSubview(expandCollapseButton)
        installOverlayGlassBackgrounds()
        NSLayoutConstraint.activate([
            closeButton.topAnchor.constraint(
                equalTo: containerView.topAnchor,
                constant: PianoWaveformViewController.panelCornerRadius - closeButtonSize / 2 + closeButtonCornerInset
            ),
            closeButton.leadingAnchor.constraint(
                equalTo: containerView.leadingAnchor,
                constant: PianoWaveformViewController.panelCornerRadius - closeButtonSize / 2 + closeButtonCornerInset
            ),
            closeButton.widthAnchor.constraint(equalToConstant: closeButtonSize),
            closeButton.heightAnchor.constraint(equalToConstant: closeButtonSize),

            expandCollapseButton.topAnchor.constraint(equalTo: closeButton.topAnchor),
            expandCollapseButton.trailingAnchor.constraint(
                equalTo: containerView.trailingAnchor,
                constant: -(PianoWaveformViewController.panelCornerRadius - closeButtonSize / 2 + closeButtonCornerInset)
            ),
            expandCollapseButton.widthAnchor.constraint(equalToConstant: closeButtonSize),
            expandCollapseButton.heightAnchor.constraint(equalToConstant: closeButtonSize),

            dockButton.topAnchor.constraint(equalTo: closeButton.topAnchor),
            dockButton.trailingAnchor.constraint(equalTo: expandCollapseButton.leadingAnchor, constant: -gap),
            dockButton.widthAnchor.constraint(equalToConstant: closeButtonSize),
            dockButton.heightAnchor.constraint(equalToConstant: closeButtonSize),
        ])
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
        collapsedView.setLive(isPresented && presentationMode == .collapsed)
        let controlsHidden = false
        [closeButton, dockButton, expandCollapseButton, closeButtonGlassView, dockButtonGlassView, expandCollapseButtonGlassView]
            .compactMap { $0 }
            .forEach { $0.isHidden = controlsHidden }
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
        button.wantsLayer = true
        button.layer?.cornerRadius = closeButtonSize / 2
        button.layer?.borderWidth = 1
        if #available(macOS 10.15, *) {
            button.layer?.cornerCurve = .continuous
        }
    }

    private func installOverlayGlassBackgrounds() {
        guard ExpandedPianoWaveformView.shouldUseLiquidGlass, #available(macOS 26.0, *) else { return }
        self.closeButtonGlassView = installGlassBackground(for: closeButton)
        self.dockButtonGlassView = installGlassBackground(for: dockButton)
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
        let alpha: CGFloat = isVisible ? 1.0 : 0.0
        let views = [closeButton, dockButton, expandCollapseButton, closeButtonGlassView, dockButtonGlassView, expandCollapseButtonGlassView]
            .compactMap { $0 }
        if animated {
            NSAnimationContext.runAnimationGroup { context in
                context.duration = 0.12
                closeButton.animator().alphaValue = alpha
                dockButton.animator().alphaValue = alpha
                expandCollapseButton.animator().alphaValue = alpha
                closeButtonGlassView?.animator().alphaValue = alpha
                dockButtonGlassView?.animator().alphaValue = alpha
                expandCollapseButtonGlassView?.animator().alphaValue = alpha
            }
        } else {
            views.forEach { $0.alphaValue = alpha }
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
            for view in [closeButtonGlassView, dockButtonGlassView, expandCollapseButtonGlassView] {
                (view as? NSGlassEffectView)?.style = .clear
                (view as? NSGlassEffectView)?.tintColor = tintColor.withAlphaComponent(0.34)
            }
            for button in [closeButton, dockButton, expandCollapseButton] {
                button.layer?.backgroundColor = NSColor.clear.cgColor
                button.layer?.borderColor = NSColor.clear.cgColor
            }
        } else {
            let border = NSColor.white.withAlphaComponent(0.28).cgColor
            let background = NSColor.windowBackgroundColor.withAlphaComponent(isDark ? 0.18 : 0.22).cgColor
            for button in [closeButton, dockButton, expandCollapseButton] {
                button.layer?.backgroundColor = background
                button.layer?.borderColor = border
            }
        }
    }

    private func updateExpandCollapseButtonAppearance() {
        let symbolName = presentationMode == .expanded ? "square.resize.down" : "square.resize.up"
        let toolTip = presentationMode == .expanded ? "Collapse" : "Expand floating piano"
        let config = NSImage.SymbolConfiguration(pointSize: 12, weight: .semibold)
        expandCollapseButton.image = NSImage(systemSymbolName: symbolName, accessibilityDescription: toolTip)?
            .withSymbolConfiguration(config)
        expandCollapseButton.toolTip = toolTip
    }

    @objc private func closeClicked(_ sender: NSButton) {
        onCloseRequested?()
    }

    @objc private func dockClicked(_ sender: NSButton) {
        onDockRequested?()
    }

    @objc private func expandCollapseClicked(_ sender: NSButton) {
        onTogglePresentationMode?()
    }
}
