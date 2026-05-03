import AppKit

private enum FloatingPaletteVisualStyleOverride: String {
    case automatic
    case liquid
    case legacy

    init(rawValue: String?) {
        switch rawValue?.trimmingCharacters(in: .whitespacesAndNewlines).lowercased() {
        case Self.liquid.rawValue:
            self = .liquid
        case Self.legacy.rawValue:
            self = .legacy
        default:
            self = .automatic
        }
    }
}

@available(macOS 26.0, *)
private final class FloatingPaletteGlassEffectView: NSGlassEffectView {
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
}

final class FloatingPlayPaletteViewController: NSViewController {
    enum DisplayMode {
        case expanded
        case collapsed
    }

    private static let panelCornerRadius: CGFloat = 18

    private let containerView = NSView()
    private let paletteView: FloatingPlayPaletteView
    private let stripView: UnifiedWaveformStripView
    private let closeButton = NSButton()
    private let dockButton = NSButton()
    private let expandCollapseButton = NSButton()
    private var displayedView: NSView?
    private var displayMode: DisplayMode = .expanded
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
        get { stripView.onStepBackward }
        set { stripView.onStepBackward = newValue }
    }

    var onStepForward: (() -> Void)? {
        get { stripView.onStepForward }
        set { stripView.onStepForward = newValue }
    }

    var onStepUp: (() -> Void)? {
        get { stripView.onStepUp }
        set { stripView.onStepUp = newValue }
    }

    var onStepDown: (() -> Void)? {
        get { stripView.onStepDown }
        set { stripView.onStepDown = newValue }
    }

    var isPianoFocusActive: (() -> Bool)? {
        get { paletteView.isPianoFocusActive }
        set { paletteView.isPianoFocusActive = newValue }
    }

    init(menuBand: MenuBandController) {
        self.paletteView = FloatingPlayPaletteView(menuBand: menuBand)
        self.stripView = UnifiedWaveformStripView(menuBand: menuBand)
        super.init(nibName: nil, bundle: nil)
        preferredContentSize = preferredSize(for: displayMode)
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
                constant: Self.panelCornerRadius - closeButtonSize / 2 + closeButtonCornerInset
            ),
            closeButton.leadingAnchor.constraint(
                equalTo: containerView.leadingAnchor,
                constant: Self.panelCornerRadius - closeButtonSize / 2 + closeButtonCornerInset
            ),
            closeButton.widthAnchor.constraint(equalToConstant: closeButtonSize),
            closeButton.heightAnchor.constraint(equalToConstant: closeButtonSize),

            expandCollapseButton.topAnchor.constraint(equalTo: closeButton.topAnchor),
            expandCollapseButton.trailingAnchor.constraint(
                equalTo: containerView.trailingAnchor,
                constant: -(Self.panelCornerRadius - closeButtonSize / 2 + closeButtonCornerInset)
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
        paletteView.refresh()
        stripView.refresh()
        preferredContentSize = preferredSize(for: displayMode)
    }

    func clearInteraction() {
        paletteView.clearInteraction()
    }

    func setPresented(_ isPresented: Bool) {
        self.isPresented = isPresented
        updatePresentationState()
    }

    func setDisplayMode(_ displayMode: DisplayMode) {
        guard self.displayMode != displayMode else {
            preferredContentSize = preferredSize(for: displayMode)
            updatePresentationState()
            return
        }
        self.displayMode = displayMode
        if isViewLoaded {
            installDisplayedView()
        }
        preferredContentSize = preferredSize(for: displayMode)
        updatePresentationState()
    }

    private func installDisplayedView() {
        let nextView: NSView
        switch displayMode {
        case .expanded:
            nextView = paletteView
        case .collapsed:
            nextView = stripView
        }

        guard displayedView !== nextView else { return }
        displayedView?.removeFromSuperview()
        nextView.translatesAutoresizingMaskIntoConstraints = false
        containerView.addSubview(nextView, positioned: .below, relativeTo: nil)
        NSLayoutConstraint.activate([
            nextView.leadingAnchor.constraint(equalTo: containerView.leadingAnchor),
            nextView.trailingAnchor.constraint(equalTo: containerView.trailingAnchor),
            nextView.topAnchor.constraint(equalTo: containerView.topAnchor),
            nextView.bottomAnchor.constraint(equalTo: containerView.bottomAnchor),
        ])
        displayedView = nextView
    }

    private func preferredSize(for displayMode: DisplayMode) -> NSSize {
        switch displayMode {
        case .expanded:
            return paletteView.fittingSize
        case .collapsed:
            return stripView.fittingSize
        }
    }

    private func updatePresentationState() {
        paletteView.setPresented(isPresented && displayMode == .expanded)
        stripView.setLive(isPresented && displayMode == .collapsed)
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
        guard FloatingPlayPaletteView.shouldUseLiquidGlass, #available(macOS 26.0, *) else { return }
        self.closeButtonGlassView = installGlassBackground(for: closeButton)
        self.dockButtonGlassView = installGlassBackground(for: dockButton)
        self.expandCollapseButtonGlassView = installGlassBackground(for: expandCollapseButton)
    }

    @available(macOS 26.0, *)
    private func installGlassBackground(for target: NSView) -> NSView {
        let glassView = FloatingPaletteGlassEffectView()
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
        switch displayMode {
        case .expanded:
            effectiveView = paletteView
        case .collapsed:
            effectiveView = stripView
        }
        let isDark = effectiveView.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        let tintColor = paletteView.paletteTintColor
        if FloatingPlayPaletteView.shouldUseLiquidGlass, #available(macOS 26.0, *) {
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
        let symbolName = displayMode == .expanded ? "square.resize.down" : "square.resize.up"
        let toolTip = displayMode == .expanded ? "Collapse" : "Expand floating piano"
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

private final class FloatingPlayPaletteView: NSView {
    private static let defaultsDomain = "computer.aestheticcomputer.menuband"
    private static let styleOverrideDefaultsKey = "MenuBandFloatingPlayPaletteStyle"
    private static let styleOverrideEnvironmentKey = "MENUBAND_FLOATING_PLAY_PALETTE_STYLE"

    private static var visualStyleOverride: FloatingPaletteVisualStyleOverride {
        let environmentValue = ProcessInfo.processInfo.environment[styleOverrideEnvironmentKey]
        if environmentValue != nil {
            return FloatingPaletteVisualStyleOverride(rawValue: environmentValue)
        }
        let defaultsValue = UserDefaults(suiteName: defaultsDomain)?.string(forKey: styleOverrideDefaultsKey)
            ?? UserDefaults.standard.string(forKey: styleOverrideDefaultsKey)
        return FloatingPaletteVisualStyleOverride(rawValue: defaultsValue)
    }

    static var shouldUseLiquidGlass: Bool {
        switch visualStyleOverride {
        case .liquid:
            if #available(macOS 26.0, *) { return true }
            return false
        case .legacy:
            return false
        case .automatic:
            if #available(macOS 26.0, *) { return true }
            return false
        }
    }

    private weak var menuBand: MenuBandController?
    private let contentStack = NSStackView()
    private let waveformSection = NSView()
    private let waveformView = WaveformView()
    private let waveformBezel = NSView()
    private let waveformClipView = NSView()
    private let heldNotesStack = NSStackView()
    private let heldNotesRow = NSView()
    private let chordCandidatesStack = NSStackView()
    private let chordCandidatesRow = NSView()
    private var lastCompleteChordNames: Set<String> = []
    private let instrumentReadout = NSTextField(labelWithString: "")
    private let instrumentTitleRow: NSStackView
    private let pianoView: FloatingPianoView
    private let shortcutHintRow = NSStackView()
    private let focusHintLabel = NSTextField(labelWithString: "")
    private let layoutHintLabel = NSTextField(labelWithString: "")
    private weak var paletteGlassView: NSView?
    private weak var waveformGlassView: NSView?
    private weak var waveformSectionGlassView: NSView?
    private weak var pianoGlassView: NSView?
    private weak var keymapGlassView: NSView?
    private weak var hintGlassView: NSView?
    /// Large QWERTY keymap shown beneath the piano so the user can
    /// see at a glance which physical keys play which notes. Driven
    /// at 2× scale so it's legible at the floating palette's size.
    private let qwertyView = QwertyLayoutView()

    var isPianoFocusActive: (() -> Bool)?
    var onHoverChanged: ((Bool) -> Void)?

    private let pianoScale: CGFloat = 1.6
    private let inset: CGFloat = 14
    private let gap: CGFloat = 8
    private let hintHeight: CGFloat = 20
    private let heldNotesRowHeight: CGFloat = 26
    private let chordCandidatesRowHeight: CGFloat = 30
    private var waveformHeightConstraint: NSLayoutConstraint?
    private var trackingArea: NSTrackingArea?
    private static let panelCornerRadius: CGFloat = 18
    private static let sectionCornerRadius: CGFloat = 14
    private static let waveformClipCornerRadius: CGFloat = 12
    private static let expandedPanelWidth: CGFloat = 440

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        let titleLeftSpacer = NSView()
        let titleRightSpacer = NSView()
        titleLeftSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        titleRightSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        self.instrumentTitleRow = NSStackView(views: [titleLeftSpacer, instrumentReadout, titleRightSpacer])
        self.pianoView = FloatingPianoView(menuBand: menuBand, pianoScale: pianoScale)
        super.init(frame: NSRect(origin: .zero, size: .zero))
        wantsLayer = true

        waveformView.menuBand = menuBand
        waveformView.translatesAutoresizingMaskIntoConstraints = false
        waveformView.setSurfaceStyle(.glassEmbedded)
        contentStack.orientation = .vertical
        contentStack.alignment = .centerX
        contentStack.distribution = .fill
        contentStack.spacing = gap
        contentStack.translatesAutoresizingMaskIntoConstraints = false
        waveformSection.wantsLayer = true
        waveformSection.layer?.cornerRadius = Self.sectionCornerRadius
        waveformSection.layer?.borderWidth = 0.8
        if #available(macOS 10.15, *) {
            waveformSection.layer?.cornerCurve = .continuous
        }
        waveformSection.translatesAutoresizingMaskIntoConstraints = false
        waveformBezel.wantsLayer = true
        waveformBezel.layer?.cornerRadius = Self.waveformClipCornerRadius + 2
        waveformBezel.layer?.borderWidth = 0
        if #available(macOS 10.15, *) {
            waveformBezel.layer?.cornerCurve = .continuous
        }
        waveformBezel.translatesAutoresizingMaskIntoConstraints = false
        waveformClipView.wantsLayer = true
        waveformClipView.translatesAutoresizingMaskIntoConstraints = false
        waveformClipView.layer?.cornerRadius = Self.waveformClipCornerRadius
        waveformClipView.layer?.masksToBounds = true
        if #available(macOS 10.15, *) {
            waveformClipView.layer?.cornerCurve = .continuous
        }
        heldNotesStack.orientation = .horizontal
        heldNotesStack.alignment = .centerY
        heldNotesStack.spacing = 6
        heldNotesStack.translatesAutoresizingMaskIntoConstraints = false
        heldNotesRow.translatesAutoresizingMaskIntoConstraints = false
        heldNotesRow.addSubview(heldNotesStack)
        chordCandidatesStack.orientation = .horizontal
        chordCandidatesStack.alignment = .centerY
        chordCandidatesStack.spacing = 6
        chordCandidatesStack.translatesAutoresizingMaskIntoConstraints = false
        chordCandidatesRow.translatesAutoresizingMaskIntoConstraints = false
        chordCandidatesRow.addSubview(chordCandidatesStack)
        instrumentReadout.lineBreakMode = .byTruncatingTail
        instrumentReadout.alignment = .center
        instrumentReadout.setContentHuggingPriority(.defaultHigh, for: .horizontal)
        instrumentReadout.setContentCompressionResistancePriority(.required, for: .horizontal)
        instrumentTitleRow.orientation = .horizontal
        instrumentTitleRow.alignment = .centerY
        instrumentTitleRow.distribution = .fill
        instrumentTitleRow.spacing = 0
        instrumentTitleRow.translatesAutoresizingMaskIntoConstraints = false
        pianoView.translatesAutoresizingMaskIntoConstraints = false
        shortcutHintRow.orientation = .horizontal
        shortcutHintRow.alignment = .centerY
        shortcutHintRow.distribution = .fill
        shortcutHintRow.spacing = gap
        shortcutHintRow.translatesAutoresizingMaskIntoConstraints = false
        focusHintLabel.translatesAutoresizingMaskIntoConstraints = false
        layoutHintLabel.translatesAutoresizingMaskIntoConstraints = false
        qwertyView.scale = 1.4
        qwertyView.keymap = menuBand.keymap
        qwertyView.translatesAutoresizingMaskIntoConstraints = false
        // Mouse-tap on a keycap → route through the controller's
        // local-key handler so the floating palette's QWERTY map
        // plays the same notes the physical keyboard would.
        qwertyView.onKey = { [weak self] keyCode, isDown in
            guard let self = self, let menuBand = self.menuBand else { return }
            menuBand.handleLocalKey(keyCode: keyCode, isDown: isDown,
                                    isRepeat: false, flags: [])
            self.refresh()
        }
        // Held-note pills and chord-suggestion cards overlay the metal
        // visualizer directly so the waveform doubles as the canvas
        // for the chord readout. Stack vertically: pills on top, cards
        // beneath, both centered. Waveform draws underneath, peeking
        // through the translucent card backgrounds.
        waveformBezel.addSubview(waveformClipView)
        waveformClipView.addSubview(waveformView)
        waveformBezel.layer?.masksToBounds = false
        heldNotesRow.wantsLayer = true
        heldNotesRow.layer?.masksToBounds = false
        chordCandidatesRow.wantsLayer = true
        chordCandidatesRow.layer?.masksToBounds = false
        waveformBezel.addSubview(heldNotesRow)
        waveformBezel.addSubview(chordCandidatesRow)
        waveformSection.addSubview(waveformBezel)
        waveformSection.addSubview(instrumentTitleRow)
        addSubview(contentStack)
        contentStack.addArrangedSubview(waveformSection)
        contentStack.addArrangedSubview(pianoView)
        shortcutHintRow.addArrangedSubview(layoutHintLabel)
        shortcutHintRow.addArrangedSubview(NSView())
        shortcutHintRow.addArrangedSubview(focusHintLabel)
        contentStack.addArrangedSubview(shortcutHintRow)
        contentStack.addArrangedSubview(qwertyView)
        for label in [focusHintLabel, layoutHintLabel] {
            label.font = NSFont.systemFont(ofSize: 10, weight: .bold)
            label.textColor = .secondaryLabelColor
            label.maximumNumberOfLines = 1
            label.lineBreakMode = .byTruncatingTail
        }
        layoutHintLabel.alignment = .left
        focusHintLabel.alignment = .right
        updateShortcutHint()
        installLiquidGlassBackgrounds()

        let keyboardSize = self.keyboardSize()
        let waveformHeightConstraint = waveformView.heightAnchor.constraint(
            equalToConstant: waveformHeight(for: keyboardSize)
        )
        self.waveformHeightConstraint = waveformHeightConstraint
        let bezelInset: CGFloat = 5
        let titleSpacers = instrumentTitleRow.arrangedSubviews

        NSLayoutConstraint.activate([
            widthAnchor.constraint(
                equalToConstant: max(keyboardSize.width + inset * 2, Self.expandedPanelWidth)
            ),

            contentStack.topAnchor.constraint(equalTo: topAnchor, constant: inset),
            contentStack.leadingAnchor.constraint(equalTo: leadingAnchor, constant: inset),
            contentStack.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -inset),
            contentStack.bottomAnchor.constraint(equalTo: bottomAnchor, constant: -inset),

            waveformSection.leadingAnchor.constraint(equalTo: contentStack.leadingAnchor),
            waveformSection.trailingAnchor.constraint(equalTo: contentStack.trailingAnchor),

            heldNotesStack.centerXAnchor.constraint(equalTo: heldNotesRow.centerXAnchor),
            heldNotesStack.centerYAnchor.constraint(equalTo: heldNotesRow.centerYAnchor),
            heldNotesStack.leadingAnchor.constraint(greaterThanOrEqualTo: heldNotesRow.leadingAnchor, constant: 6),
            heldNotesStack.trailingAnchor.constraint(lessThanOrEqualTo: heldNotesRow.trailingAnchor, constant: -6),
            heldNotesRow.heightAnchor.constraint(equalToConstant: heldNotesRowHeight),

            chordCandidatesStack.centerXAnchor.constraint(equalTo: chordCandidatesRow.centerXAnchor),
            chordCandidatesStack.centerYAnchor.constraint(equalTo: chordCandidatesRow.centerYAnchor),
            chordCandidatesStack.leadingAnchor.constraint(greaterThanOrEqualTo: chordCandidatesRow.leadingAnchor, constant: 6),
            chordCandidatesStack.trailingAnchor.constraint(lessThanOrEqualTo: chordCandidatesRow.trailingAnchor, constant: -6),
            chordCandidatesRow.heightAnchor.constraint(equalToConstant: chordCandidatesRowHeight),

            waveformBezel.topAnchor.constraint(equalTo: waveformSection.topAnchor, constant: bezelInset),
            waveformBezel.leadingAnchor.constraint(equalTo: waveformSection.leadingAnchor, constant: bezelInset),
            waveformBezel.trailingAnchor.constraint(equalTo: waveformSection.trailingAnchor, constant: -bezelInset),
            waveformClipView.leadingAnchor.constraint(equalTo: waveformBezel.leadingAnchor, constant: bezelInset),
            waveformClipView.trailingAnchor.constraint(equalTo: waveformBezel.trailingAnchor, constant: -bezelInset),
            waveformClipView.topAnchor.constraint(equalTo: waveformBezel.topAnchor, constant: bezelInset),
            waveformClipView.bottomAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: -bezelInset),
            waveformView.leadingAnchor.constraint(equalTo: waveformClipView.leadingAnchor),
            waveformView.trailingAnchor.constraint(equalTo: waveformClipView.trailingAnchor),
            waveformView.topAnchor.constraint(equalTo: waveformClipView.topAnchor),
            waveformView.bottomAnchor.constraint(equalTo: waveformClipView.bottomAnchor),
            waveformHeightConstraint,

            heldNotesRow.leadingAnchor.constraint(equalTo: waveformBezel.leadingAnchor),
            heldNotesRow.trailingAnchor.constraint(equalTo: waveformBezel.trailingAnchor),
            heldNotesRow.topAnchor.constraint(equalTo: waveformBezel.topAnchor, constant: 8),

            chordCandidatesRow.leadingAnchor.constraint(equalTo: waveformBezel.leadingAnchor),
            chordCandidatesRow.trailingAnchor.constraint(equalTo: waveformBezel.trailingAnchor),
            chordCandidatesRow.bottomAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: -8),

            instrumentTitleRow.topAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: 6),
            instrumentTitleRow.leadingAnchor.constraint(equalTo: waveformSection.leadingAnchor, constant: 6),
            instrumentTitleRow.trailingAnchor.constraint(equalTo: waveformSection.trailingAnchor, constant: -6),
            instrumentTitleRow.bottomAnchor.constraint(equalTo: waveformSection.bottomAnchor, constant: -6),

            shortcutHintRow.leadingAnchor.constraint(equalTo: contentStack.leadingAnchor),
            shortcutHintRow.trailingAnchor.constraint(equalTo: contentStack.trailingAnchor),
            shortcutHintRow.heightAnchor.constraint(equalToConstant: hintHeight),

            qwertyView.centerXAnchor.constraint(equalTo: centerXAnchor),
            qwertyView.widthAnchor.constraint(
                equalToConstant: QwertyLayoutView.intrinsicSize.width * 1.4
            ),
            qwertyView.heightAnchor.constraint(
                equalToConstant: QwertyLayoutView.intrinsicSize.height * 1.4
            ),
        ])
        if titleSpacers.count == 3 {
            titleSpacers[0].widthAnchor.constraint(equalTo: titleSpacers[2].widthAnchor).isActive = true
        }
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        nil
    }

    private func installLiquidGlassBackgrounds() {
        guard Self.shouldUseLiquidGlass, #available(macOS 26.0, *) else { return }

        let paletteGlassView = FloatingPaletteGlassEffectView()
        paletteGlassView.translatesAutoresizingMaskIntoConstraints = false
        paletteGlassView.cornerRadius = Self.panelCornerRadius
        addSubview(paletteGlassView, positioned: .below, relativeTo: waveformSection)
        NSLayoutConstraint.activate([
            paletteGlassView.leadingAnchor.constraint(equalTo: leadingAnchor),
            paletteGlassView.trailingAnchor.constraint(equalTo: trailingAnchor),
            paletteGlassView.topAnchor.constraint(equalTo: topAnchor),
            paletteGlassView.bottomAnchor.constraint(equalTo: bottomAnchor),
        ])
        self.paletteGlassView = paletteGlassView

        self.waveformSectionGlassView = installGlassBackground(
            matchedTo: waveformSection,
            below: waveformSection,
            cornerRadius: Self.sectionCornerRadius
        )
        self.pianoGlassView = installGlassBackground(
            matchedTo: pianoView,
            below: pianoView,
            cornerRadius: Self.sectionCornerRadius
        )
        self.keymapGlassView = installGlassBackground(
            matchedTo: qwertyView,
            below: qwertyView,
            cornerRadius: Self.sectionCornerRadius
        )
        self.hintGlassView = installGlassBackground(
            matchedTo: shortcutHintRow,
            below: shortcutHintRow,
            cornerRadius: Self.sectionCornerRadius
        )
    }

    @available(macOS 26.0, *)
    private func installGlassBackground(matchedTo target: NSView,
                                        below anchor: NSView,
                                        cornerRadius: CGFloat) -> NSView {
        let glassView = FloatingPaletteGlassEffectView()
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

    override var acceptsFirstResponder: Bool { true }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let trackingArea {
            removeTrackingArea(trackingArea)
        }
        let trackingArea = NSTrackingArea(
            rect: bounds,
            options: [.mouseEnteredAndExited, .activeAlways, .inVisibleRect],
            owner: self,
            userInfo: nil
        )
        addTrackingArea(trackingArea)
        self.trackingArea = trackingArea
    }

    override func mouseEntered(with event: NSEvent) {
        super.mouseEntered(with: event)
        onHoverChanged?(true)
    }

    override func mouseExited(with event: NSEvent) {
        super.mouseExited(with: event)
        onHoverChanged?(false)
    }

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
        if Self.shouldUseLiquidGlass, #available(macOS 26.0, *) {
            return
        }
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
        refreshHeldNotes()
        updateInstrumentReadout()
        applyWaveformTint()
        updateWaveformLiveState(isPresented: window?.isVisible == true)
        needsDisplay = true
        pianoView.needsDisplay = true
    }

    func clearInteraction() {
        pianoView.clearInteraction()
        lastCompleteChordNames = []
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
            waveformSection.layer?.backgroundColor = NSColor.white.withAlphaComponent(0.06).cgColor
            waveformBezel.layer?.backgroundColor = NSColor.white.withAlphaComponent(0.04).cgColor
        } else {
            waveformSection.layer?.backgroundColor = NSColor.white.withAlphaComponent(0.22).cgColor
            waveformBezel.layer?.backgroundColor = NSColor.white.withAlphaComponent(0.18).cgColor
        }
    }

    private func applyWaveformTint() {
        guard let menuBand else { return }
        if menuBand.midiMode {
            waveformView.setDotMatrix(MenuBandPopoverViewController.midiDotPattern)
            waveformView.setBaseColor(.controlAccentColor)
            waveformSection.layer?.borderColor = NSColor.controlAccentColor
                .withAlphaComponent(0.24).cgColor
        } else {
            waveformView.setDotMatrix(nil)
            let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
            let familyColor = InstrumentListView.colorForProgram(safe)
            waveformView.setBaseColor(familyColor)
            waveformSection.layer?.borderColor = familyColor
                .withAlphaComponent(0.22).cgColor
        }
        if #available(macOS 26.0, *) {
            let paletteTint = paletteTintColor
            for view in [paletteGlassView, waveformSectionGlassView, pianoGlassView, keymapGlassView, hintGlassView] {
                (view as? NSGlassEffectView)?.tintColor = paletteTint
            }
        }
    }

    var paletteTintColor: NSColor {
        guard let menuBand else { return NSColor.controlAccentColor.withAlphaComponent(0.20) }
        if menuBand.midiMode {
            return NSColor.controlAccentColor.withAlphaComponent(0.20)
        }
        let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
        return InstrumentListView.colorForProgram(safe).withAlphaComponent(0.16)
    }

    private func keyboardSize() -> NSSize {
        withFloatingPaletteKeyboard(menuBand: menuBand) {
            let piano = KeyboardIconRenderer.pianoImageSize(layout: .tightActiveRange)
            return NSSize(width: piano.width * pianoScale, height: piano.height * pianoScale)
        }
    }

    private func waveformHeight(for keyboard: NSSize) -> CGFloat {
        keyboard.height * 1.25
    }

    private func refreshHeldNotes() {
        guard let menuBand else { return }
        for view in heldNotesStack.arrangedSubviews {
            heldNotesStack.removeArrangedSubview(view)
            view.removeFromSuperview()
        }
        for view in chordCandidatesStack.arrangedSubviews {
            chordCandidatesStack.removeArrangedSubview(view)
            view.removeFromSuperview()
        }
        // Live state for the QWERTY keymap below the piano: light up
        // the physical keys the user is currently holding, and route
        // the qwertyView's onKey (mouse taps on caps) through the
        // same handleLocalKey path the keyboard uses so the overlay
        // is fully interactive.
        qwertyView.litKeyCodes = menuBand.heldKeyCodes()
        qwertyView.keymap = menuBand.keymap
        let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
        let familyColor = menuBand.midiMode
            ? NSColor.controlAccentColor
            : InstrumentListView.colorForProgram(safe)
        qwertyView.voiceColor = familyColor

        // Held-notes row: each currently sounding note as its own large
        // pill so the chord shape reads at-a-glance. Always per-note —
        // chord recognition lives in the candidates row below.
        let names = menuBand.heldNoteNames()
        for name in names {
            heldNotesStack.addArrangedSubview(makeHeldNoteBox(name: name, color: familyColor))
        }

        // Chord candidates: every chord shape that contains the held
        // pitch classes and whose missing notes are reachable on the
        // active keymap. Cards re-render every refresh; transitions
        // from incomplete → complete trigger a brief shake on the new
        // complete card so the user feels the chord "lock in".
        let candidates = menuBand.chordCandidates(maxResults: 8)
        let newComplete = Set(candidates.filter(\.isComplete).map(\.name))
        let justCompleted = newComplete.subtracting(lastCompleteChordNames)
        for candidate in candidates {
            let card = makeChordCandidateCard(candidate: candidate, color: familyColor)
            chordCandidatesStack.addArrangedSubview(card)
            if candidate.isComplete && justCompleted.contains(candidate.name) {
                applyShake(to: card)
            }
        }
        lastCompleteChordNames = newComplete
    }

    private func makeHeldNoteBox(name: String, color: NSColor) -> NSView {
        let box = NSView()
        box.wantsLayer = true
        box.layer?.cornerRadius = 4
        box.layer?.backgroundColor = color.withAlphaComponent(0.92).cgColor
        box.layer?.borderWidth = 1
        box.layer?.borderColor = color.shadow(withLevel: 0.35)?.cgColor ?? color.cgColor
        box.translatesAutoresizingMaskIntoConstraints = false
        let label = NSTextField(labelWithString: name)
        label.font = NSFont.monospacedSystemFont(ofSize: 11, weight: .heavy)
        label.textColor = .black
        label.drawsBackground = false
        label.translatesAutoresizingMaskIntoConstraints = false
        box.addSubview(label)
        NSLayoutConstraint.activate([
            label.leadingAnchor.constraint(equalTo: box.leadingAnchor, constant: 5),
            label.trailingAnchor.constraint(equalTo: box.trailingAnchor, constant: -5),
            label.topAnchor.constraint(equalTo: box.topAnchor, constant: 2),
            label.bottomAnchor.constraint(equalTo: box.bottomAnchor, constant: -2),
            box.heightAnchor.constraint(equalToConstant: 20),
        ])
        return box
    }

    private func makeChordCandidateCard(candidate: MenuBandController.ChordCandidate,
                                        color: NSColor) -> NSView {
        FloatingChordCandidateCard.build(candidate: candidate,
                                          isDark: effectiveAppearance
                                              .bestMatch(from: [.aqua, .darkAqua]) == .darkAqua)
    }

    private func applyShake(to view: NSView) {
        guard let layer = view.layer else { return }
        let shake = CAKeyframeAnimation(keyPath: "transform.translation.x")
        shake.values = [0, -7, 7, -5, 5, -3, 3, 0]
        shake.keyTimes = [0, 0.12, 0.27, 0.42, 0.57, 0.72, 0.87, 1.0]
        shake.duration = 0.46
        shake.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
        layer.add(shake, forKey: "shake")
    }

    private func updateInstrumentReadout() {
        guard let menuBand else { return }
        let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
        let title = GeneralMIDI.programNames[safe]
        let familyColor = InstrumentListView.colorForProgram(safe)
        let isDark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        let textColor: NSColor = isDark ? .white : .black
        let shadow = NSShadow()
        shadow.shadowColor = (familyColor.highlight(withLevel: isDark ? 0.3 : 0.7) ?? familyColor)
        shadow.shadowOffset = NSSize(width: 1, height: -1)
        shadow.shadowBlurRadius = 0
        let titleFont: NSFont = {
            if let desc = AppDelegate.ywftBoldDescriptor,
               let font = NSFont(descriptor: desc, size: 18),
               font.familyName == "YWFT Processing" {
                return font
            }
            NSLog("MenuBand: YWFT bold descriptor unavailable; floating title falling back to system font")
            return NSFont.systemFont(ofSize: 18, weight: .black)
        }()
        instrumentReadout.attributedStringValue = NSAttributedString(
            string: title,
            attributes: [
                .font: titleFont,
                .foregroundColor: textColor,
                .shadow: shadow,
            ]
        )
    }

    private func updateShortcutHint() {
        let focusShortcut = MenuBandShortcutPreferences.focusShortcut.displayString
        let layoutShortcut = MenuBandShortcut.layoutToggle.displayString
        layoutHintLabel.stringValue = "Toggle Layout: \(layoutShortcut)"
        focusHintLabel.stringValue = (isPianoFocusActive?() ?? false)
            ? "Exit Focus: \(focusShortcut)"
            : "Focus Piano: \(focusShortcut)"
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
    override var mouseDownCanMoveWindow: Bool { false }

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

/// Tall traditional-piano aspect for the floating overlay. Multiplies
/// white-key height (and black-key height proportionally) so the keys
/// read like a real keyboard instead of menubar squares.
private let floatingPaletteKeyHeightScale: CGFloat = 2.4

private func withFloatingPaletteKeyboard<T>(menuBand: MenuBandController?, _ body: () -> T) -> T {
    let oldLayout = KeyboardIconRenderer.displayLayout
    let oldKeymap = KeyboardIconRenderer.activeKeymap
    let oldScale = KeyboardIconRenderer.keyHeightScale
    KeyboardIconRenderer.displayLayout = .full
    KeyboardIconRenderer.keyHeightScale = floatingPaletteKeyHeightScale
    if let menuBand = menuBand {
        KeyboardIconRenderer.activeKeymap = menuBand.keymap
    }
    defer {
        KeyboardIconRenderer.displayLayout = oldLayout
        KeyboardIconRenderer.activeKeymap = oldKeymap
        KeyboardIconRenderer.keyHeightScale = oldScale
    }
    return body()
}

private final class FloatingPlayPalettePanel: NSPanel {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { false }
}
