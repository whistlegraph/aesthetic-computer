import AppKit

private enum StripVisualStyleOverride: String {
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

private final class LegacyMenuBarWaveformStripPanel: NSPanel {
    var onDragBegan: (() -> Void)?
    var onDragMoved: ((NSPoint) -> Void)?
    var onDragEnded: (() -> Void)?

    private var dragStartMouseLocation: NSPoint?
    private var dragStartOrigin: NSPoint = .zero
    private var isDraggingStrip = false

    override func sendEvent(_ event: NSEvent) {
        switch event.type {
        case .leftMouseDown:
            if event.clickCount == 1 {
                dragStartMouseLocation = convertToScreen(
                    NSRect(origin: event.locationInWindow, size: .zero)
                ).origin
                dragStartOrigin = frame.origin
                isDraggingStrip = false
            }
        case .leftMouseDragged:
            guard let startMouseLocation = dragStartMouseLocation else { break }
            let current = convertToScreen(
                NSRect(origin: event.locationInWindow, size: .zero)
            ).origin
            let deltaX = current.x - startMouseLocation.x
            let deltaY = current.y - startMouseLocation.y
            if !isDraggingStrip {
                isDraggingStrip = true
                onDragBegan?()
            }
            onDragMoved?(NSPoint(x: dragStartOrigin.x + deltaX, y: dragStartOrigin.y + deltaY))
        case .leftMouseUp:
            dragStartMouseLocation = nil
            if isDraggingStrip {
                isDraggingStrip = false
                onDragEnded?()
            }
        default:
            break
        }
        super.sendEvent(event)
    }
}

private final class LegacyMenuBarWaveformStrip: NSObject, MenuBarWaveformStripImplementation {
    private let menuBand: MenuBandController
    private let waveformView = WaveformView()
    private let waveformBezel = NSView()
    private var waveformBezelHeightConstraint: NSLayoutConstraint?
    private let instrumentArrows = ArrowKeysIndicator()
    private let instrumentRowContainer = NSView()
    private let instrumentLabel = NSTextField(labelWithString: "")
    private let closeButton = NSButton()
    private let dockButton = NSButton()
    private let expandButton = NSButton()
    private let heldNotesContainer = NSView()
    private let heldNotesStack = NSStackView()
    private var panel: NSPanel?
    private weak var statusButton: NSStatusBarButton?
    private var isPointerInsideStrip = false
    var onStepBackward: (() -> Void)?
    var onStepForward: (() -> Void)?
    var onStepUp: (() -> Void)?
    var onStepDown: (() -> Void)?
    var onExpandRequested: (() -> Void)?

    private let hideDelay: TimeInterval = 2.0
    private var hideWorkItem: DispatchWorkItem?
    private let customOriginXKey = "notepat.waveformStrip.customOriginX"
    private let customOriginYKey = "notepat.waveformStrip.customOriginY"
    private var customOrigin: NSPoint?

    private static let waveformAspectRatio: CGFloat = InstrumentListView.preferredWidth / 64
    private static let controlButtonSize: CGFloat = 20
    private static let footerHeight: CGFloat = 46
    private static let heldNotesRowHeight: CGFloat = 14
    private static let instrumentRowHeight: CGFloat = 22
    private static let fadeInDuration: TimeInterval = 0.18
    private static let fadeOutDuration: TimeInterval = 0.18
    private static let stripLevel = NSWindow.Level(rawValue: NSWindow.Level.mainMenu.rawValue - 1)

    private var slideLink: CVDisplayLink?
    private var slideStartTime: CFTimeInterval = 0
    private var slideFromY: CGFloat = 0
    private var slideToY: CGFloat = 0
    private var slideCompletion: (() -> Void)?
    private var isSliding = false

    var isShown: Bool { panel?.isVisible == true }
    var isDocked: Bool { customOrigin == nil }

    var suppressed: Bool = false {
        didSet {
            guard suppressed != oldValue else { return }
            if suppressed { hide(animated: false) }
        }
    }

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        super.init()
        customOrigin = loadCustomOrigin()
        waveformView.menuBand = menuBand
        waveformView.setSurfaceStyle(.standard)
        waveformBezel.wantsLayer = true
        waveformBezel.layer?.cornerRadius = 6
        waveformBezel.layer?.backgroundColor = NSColor(white: 0.06, alpha: 1.0).cgColor
        waveformBezel.layer?.borderWidth = 1
        heldNotesStack.orientation = .horizontal
        heldNotesStack.alignment = .centerY
        heldNotesStack.spacing = 4
        instrumentLabel.drawsBackground = false
        instrumentLabel.lineBreakMode = .byTruncatingTail
        instrumentArrows.displayMode = .horizontalPair
        instrumentArrows.toolTip = "Change instrument"
        instrumentArrows.onClick = { [weak self] dir, isDown in
            guard isDown else { return }
            self?.registerArrowInput()
            switch dir {
            case 0: self?.onStepBackward?()
            case 1: self?.onStepForward?()
            default: break
            }
        }
        configureControlButton(
            closeButton,
            symbolName: "xmark",
            toolTip: "Close",
            action: #selector(closeButtonClicked(_:))
        )
        configureControlButton(
            dockButton,
            symbolName: "menubar.dock.rectangle",
            toolTip: "Dock Below Menubar Piano",
            action: #selector(dockButtonClicked(_:))
        )
        configureControlButton(
            expandButton,
            symbolName: "square.resize.up",
            toolTip: "Expand floating piano",
            action: #selector(expandButtonClicked(_:))
        )
    }

    deinit {
        stopSlide()
    }

    private func handleStripDragBegan() {
        cancelPendingHide()
        stopSlide()
        isSliding = false
    }

    private func handleStripDragEnded() {
        if menuBand.litNotes.isEmpty {
            scheduleHide()
        }
    }

    private func handleStripMouseEntered() {
        guard !isPointerInsideStrip else { return }
        isPointerInsideStrip = true
        setControlsVisible(true)
        cancelPendingHide()
    }

    private func handleStripMouseExited() {
        guard isPointerInsideStrip else { return }
        isPointerInsideStrip = false
        setControlsVisible(false)
        if menuBand.litNotes.isEmpty {
            scheduleHide()
        }
    }

    func warmUp() {
        if panel == nil { buildPanel() }
    }

    func showIfNeeded() {
        guard !suppressed else { return }
        cancelPendingHide()
        if !isShown && !isSliding { show() }
    }

    func scheduleHide() {
        guard !isPointerInsideStrip else { return }
        cancelPendingHide()
        let work = DispatchWorkItem { [weak self] in
            self?.hide(animated: true)
        }
        hideWorkItem = work
        DispatchQueue.main.asyncAfter(deadline: .now() + hideDelay, execute: work)
    }

    func dismiss() {
        cancelPendingHide()
        stopSlide()
        isPointerInsideStrip = false
        setControlsVisible(false, animated: false)
        waveformView.isLive = false
        panel?.orderOut(nil)
    }

    func reposition(statusItemButton: NSStatusBarButton?) {
        statusButton = statusItemButton
        guard let panel = panel, panel.isVisible, !isSliding else { return }
        positionPanel(panel)
    }

    func refreshAppearance() {
        applyAppearanceToVisualizer()
        applyWaveformTint()
        refreshReadout()
    }

    func registerArrowInput() {
        guard !suppressed else { return }
        showIfNeeded()
        refreshReadout()
        if menuBand.litNotes.isEmpty {
            scheduleHide()
        }
    }

    private func targetFrame() -> NSRect? {
        guard let button = statusButton,
              let buttonWindow = button.window else { return nil }

        let imgSize = KeyboardIconRenderer.imageSize
        let bb = button.bounds
        let xOff = (bb.width - imgSize.width) / 2.0

        let pianoOriginX = xOff + KeyboardIconRenderer.pad
        let pianoWidth = imgSize.width - KeyboardIconRenderer.settingsW
            - KeyboardIconRenderer.settingsGap - KeyboardIconRenderer.pad * 2

        let localRect = NSRect(x: pianoOriginX, y: 0, width: pianoWidth, height: bb.height)
        let windowRect = button.convert(localRect, to: nil)
        let screenRect = buttonWindow.convertToScreen(windowRect)

        let stripHeight = currentStripHeight(for: screenRect.width)
        let anchoredFrame = NSRect(
            x: screenRect.origin.x,
            y: screenRect.origin.y - stripHeight,
            width: screenRect.width,
            height: stripHeight
        )
        guard let customOrigin else { return anchoredFrame }
        return clampedFrame(
            origin: customOrigin,
            size: anchoredFrame.size,
            preferredScreen: panel?.screen ?? buttonWindow.screen
        )
    }

    private func currentWaveformBezelHeight(for width: CGFloat) -> CGFloat {
        round(width / Self.waveformAspectRatio)
    }

    private func currentStripHeight(for width: CGFloat) -> CGFloat {
        currentWaveformBezelHeight(for: width) + Self.footerHeight
    }

    private func loadCustomOrigin() -> NSPoint? {
        let defaults = UserDefaults.standard
        guard defaults.object(forKey: customOriginXKey) != nil,
              defaults.object(forKey: customOriginYKey) != nil else { return nil }
        return NSPoint(
            x: defaults.double(forKey: customOriginXKey),
            y: defaults.double(forKey: customOriginYKey)
        )
    }

    private func saveCustomOrigin(_ origin: NSPoint) {
        let defaults = UserDefaults.standard
        defaults.set(origin.x, forKey: customOriginXKey)
        defaults.set(origin.y, forKey: customOriginYKey)
    }

    private func clampedFrame(origin: NSPoint, size: NSSize, preferredScreen: NSScreen?) -> NSRect {
        let visible = visibleFrame(for: origin, preferredScreen: preferredScreen)
        let x = min(max(origin.x, visible.minX), visible.maxX - size.width)
        let y = min(max(origin.y, visible.minY), visible.maxY - size.height)
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

    private func moveStrip(to origin: NSPoint) {
        guard let panel = panel else { return }
        let frame = clampedFrame(origin: origin, size: panel.frame.size, preferredScreen: panel.screen)
        customOrigin = frame.origin
        saveCustomOrigin(frame.origin)
        panel.setFrameOrigin(frame.origin)
    }

    private func resetCustomPosition() {
        customOrigin = nil
        let defaults = UserDefaults.standard
        defaults.removeObject(forKey: customOriginXKey)
        defaults.removeObject(forKey: customOriginYKey)
        guard let panel = panel else { return }
        positionPanel(panel)
    }

    private func show() {
        if panel == nil { buildPanel() }
        guard let panel = panel, let target = targetFrame() else { return }

        isPointerInsideStrip = false
        setControlsVisible(false, animated: false)
        waveformBezelHeightConstraint?.constant = currentWaveformBezelHeight(for: target.width)
        panel.setFrame(target, display: true)
        isSliding = true
        panel.alphaValue = 1.0
        panel.alphaValue = 0.0
        applyAppearanceToVisualizer()
        applyWaveformTint()
        refreshReadout()
        waveformView.isLive = true
        panel.orderFrontRegardless()

        NSAnimationContext.runAnimationGroup { context in
            context.duration = Self.fadeInDuration
            context.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
            panel.animator().alphaValue = 1.0
        } completionHandler: { [weak self, weak panel] in
            guard let self, let panel else { return }
            panel.alphaValue = 1.0
            self.isSliding = false
        }
    }

    private func hide(animated: Bool) {
        cancelPendingHide()
        stopSlide()
        guard let panel = panel, panel.isVisible else {
            waveformView.isLive = false
            return
        }
        if !animated {
            panel.animator().alphaValue = 1.0
            waveformView.isLive = false
            panel.orderOut(nil)
            return
        }
        NSAnimationContext.runAnimationGroup { context in
            context.duration = Self.fadeOutDuration
            context.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
            panel.animator().alphaValue = 0.0
        } completionHandler: { [weak self, weak panel] in
            guard let self, let panel else { return }
            self.waveformView.isLive = false
            panel.orderOut(nil)
            panel.alphaValue = 1.0
            self.isSliding = false
        }
    }

    private func startSlide(fromY: CGFloat, toY: CGFloat, completion: @escaping () -> Void) {
        stopSlide()
        isSliding = true
        slideFromY = fromY
        slideToY = toY
        slideStartTime = CACurrentMediaTime()
        slideCompletion = completion

        var link: CVDisplayLink?
        CVDisplayLinkCreateWithActiveCGDisplays(&link)
        guard let link = link else {
            panel?.setFrameOrigin(NSPoint(x: panel?.frame.origin.x ?? 0, y: toY))
            completion()
            return
        }
        let opaque = Unmanaged.passUnretained(self).toOpaque()
        CVDisplayLinkSetOutputCallback(link, { _, _, _, _, _, ctx -> CVReturn in
            guard let ctx = ctx else { return kCVReturnSuccess }
            let strip = Unmanaged<LegacyMenuBarWaveformStrip>.fromOpaque(ctx).takeUnretainedValue()
            DispatchQueue.main.async { strip.tickSlide() }
            return kCVReturnSuccess
        }, opaque)
        CVDisplayLinkStart(link)
        slideLink = link
    }

    private func tickSlide() {
        guard isSliding, let panel = panel else {
            stopSlide()
            return
        }
        let elapsed = CACurrentMediaTime() - slideStartTime
        let t = min(1.0, elapsed / Self.fadeInDuration)
        let eased: CGFloat
        if slideToY < slideFromY {
            let f = 1.0 - t
            eased = CGFloat(1.0 - f * f * f)
        } else {
            eased = CGFloat(t * t * t)
        }
        let currentY = slideFromY + (slideToY - slideFromY) * eased
        panel.setFrameOrigin(NSPoint(x: panel.frame.origin.x, y: currentY))

        if t >= 1.0 {
            let completion = slideCompletion
            stopSlide()
            completion?()
        }
    }

    private func stopSlide() {
        if let link = slideLink {
            CVDisplayLinkStop(link)
            slideLink = nil
        }
        slideCompletion = nil
    }

    private func cancelPendingHide() {
        hideWorkItem?.cancel()
        hideWorkItem = nil
    }

    private func buildPanel() {
        let initialWidth: CGFloat = 200
        let p = LegacyMenuBarWaveformStripPanel(
            contentRect: NSRect(
                origin: .zero,
                size: NSSize(width: initialWidth, height: currentStripHeight(for: initialWidth))
            ),
            styleMask: [.borderless, .nonactivatingPanel],
            backing: .buffered,
            defer: false
        )
        p.isOpaque = true
        p.backgroundColor = .black
        p.hasShadow = true
        p.level = Self.stripLevel
        p.collectionBehavior = [.transient, .ignoresCycle]
        p.hidesOnDeactivate = false
        p.canHide = false
        p.isMovable = false
        p.animationBehavior = .none
        p.acceptsMouseMovedEvents = false
        p.onDragBegan = { [weak self] in
            self?.handleStripDragBegan()
        }
        p.onDragMoved = { [weak self] origin in
            self?.moveStrip(to: origin)
        }
        p.onDragEnded = { [weak self] in
            self?.handleStripDragEnded()
        }

        waveformView.translatesAutoresizingMaskIntoConstraints = false
        waveformBezel.translatesAutoresizingMaskIntoConstraints = false
        instrumentArrows.translatesAutoresizingMaskIntoConstraints = false
        instrumentRowContainer.translatesAutoresizingMaskIntoConstraints = false
        instrumentLabel.translatesAutoresizingMaskIntoConstraints = false
        closeButton.translatesAutoresizingMaskIntoConstraints = false
        dockButton.translatesAutoresizingMaskIntoConstraints = false
        expandButton.translatesAutoresizingMaskIntoConstraints = false
        heldNotesContainer.translatesAutoresizingMaskIntoConstraints = false
        heldNotesStack.translatesAutoresizingMaskIntoConstraints = false
        let content = StripInteractiveSurfaceView()
        content.onMouseEntered = { [weak self] in self?.handleStripMouseEntered() }
        content.onMouseExited = { [weak self] in self?.handleStripMouseExited() }
        content.wantsLayer = true
        content.layer?.backgroundColor = NSColor.clear.cgColor
        p.contentView = content
        p.contentView?.addSubview(waveformBezel)
        p.contentView?.addSubview(heldNotesContainer)
        p.contentView?.addSubview(instrumentRowContainer)
        p.contentView?.addSubview(closeButton)
        p.contentView?.addSubview(dockButton)
        p.contentView?.addSubview(expandButton)
        instrumentRowContainer.addSubview(instrumentArrows)
        instrumentRowContainer.addSubview(instrumentLabel)
        heldNotesContainer.addSubview(heldNotesStack)
        waveformBezel.addSubview(waveformView)
        let bezelInset: CGFloat = 5
        let waveformBezelHeightConstraint = waveformBezel.heightAnchor.constraint(
            equalToConstant: currentWaveformBezelHeight(for: initialWidth)
        )
        self.waveformBezelHeightConstraint = waveformBezelHeightConstraint
        NSLayoutConstraint.activate([
            waveformBezel.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            waveformBezel.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            waveformBezel.topAnchor.constraint(equalTo: content.topAnchor),
            waveformBezelHeightConstraint,
            waveformView.leadingAnchor.constraint(equalTo: waveformBezel.leadingAnchor, constant: bezelInset),
            waveformView.trailingAnchor.constraint(equalTo: waveformBezel.trailingAnchor, constant: -bezelInset),
            waveformView.topAnchor.constraint(equalTo: waveformBezel.topAnchor, constant: bezelInset),
            waveformView.bottomAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: -bezelInset),
            heldNotesContainer.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            heldNotesContainer.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            heldNotesContainer.topAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: 2),
            heldNotesContainer.heightAnchor.constraint(equalToConstant: Self.heldNotesRowHeight),
            heldNotesStack.centerXAnchor.constraint(equalTo: heldNotesContainer.centerXAnchor),
            heldNotesStack.centerYAnchor.constraint(equalTo: heldNotesContainer.centerYAnchor),
            closeButton.topAnchor.constraint(equalTo: content.topAnchor, constant: 6),
            closeButton.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 6),
            closeButton.widthAnchor.constraint(equalToConstant: Self.controlButtonSize),
            closeButton.heightAnchor.constraint(equalToConstant: Self.controlButtonSize),
            expandButton.topAnchor.constraint(equalTo: content.topAnchor, constant: 6),
            expandButton.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -6),
            expandButton.widthAnchor.constraint(equalToConstant: Self.controlButtonSize),
            expandButton.heightAnchor.constraint(equalToConstant: Self.controlButtonSize),
            dockButton.topAnchor.constraint(equalTo: content.topAnchor, constant: 6),
            dockButton.trailingAnchor.constraint(equalTo: expandButton.leadingAnchor, constant: -4),
            dockButton.widthAnchor.constraint(equalToConstant: Self.controlButtonSize),
            dockButton.heightAnchor.constraint(equalToConstant: Self.controlButtonSize),
            instrumentRowContainer.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            instrumentRowContainer.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            instrumentRowContainer.topAnchor.constraint(equalTo: heldNotesContainer.bottomAnchor, constant: 2),
            instrumentRowContainer.heightAnchor.constraint(equalToConstant: Self.instrumentRowHeight),
            instrumentRowContainer.bottomAnchor.constraint(equalTo: content.bottomAnchor, constant: -2),
            instrumentArrows.leadingAnchor.constraint(equalTo: instrumentRowContainer.leadingAnchor, constant: 4),
            instrumentArrows.centerYAnchor.constraint(equalTo: instrumentRowContainer.centerYAnchor),
            instrumentLabel.leadingAnchor.constraint(equalTo: instrumentArrows.trailingAnchor, constant: 4),
            instrumentLabel.centerYAnchor.constraint(equalTo: instrumentRowContainer.centerYAnchor),
            instrumentLabel.trailingAnchor.constraint(equalTo: instrumentRowContainer.trailingAnchor, constant: -6),
        ])
        applyAppearanceToVisualizer()
        applyWaveformTint()
        refreshReadout()
        setControlsVisible(false, animated: false)

        panel = p
    }

    private func positionPanel(_ panel: NSPanel) {
        guard let target = targetFrame() else { return }
        waveformBezelHeightConstraint?.constant = currentWaveformBezelHeight(for: target.width)
        panel.setFrame(target, display: true)
    }

    private func applyAppearanceToVisualizer() {
        let isDark = NSApp.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        waveformView.setLightMode(!isDark)
        if isDark {
            waveformBezel.layer?.backgroundColor = NSColor(white: 0.06, alpha: 1.0).cgColor
            instrumentLabel.textColor = .white
        } else {
            waveformBezel.layer?.backgroundColor = NSColor(white: 0.82, alpha: 1.0).cgColor
            instrumentLabel.textColor = .black
        }
    }

    private func applyWaveformTint() {
        if menuBand.midiMode {
            waveformView.setDotMatrix(MenuBandPopoverViewController.midiDotPattern)
            waveformView.setBaseColor(.controlAccentColor)
            waveformBezel.layer?.borderColor = NSColor.controlAccentColor.withAlphaComponent(0.55).cgColor
        } else {
            waveformView.setDotMatrix(nil)
            let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
            let familyColor = InstrumentListView.colorForProgram(safe)
            waveformView.setBaseColor(familyColor)
            waveformBezel.layer?.borderColor = familyColor.withAlphaComponent(0.55).cgColor
        }
        for button in [closeButton, dockButton, expandButton] {
            button.layer?.backgroundColor = NSColor.windowBackgroundColor.withAlphaComponent(0.22).cgColor
            button.layer?.borderColor = NSColor.white.withAlphaComponent(0.28).cgColor
        }
    }

    private func refreshReadout() {
        let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
        let familyColor = menuBand.midiMode
            ? NSColor.controlAccentColor
            : InstrumentListView.colorForProgram(safe)
        let isDark = NSApp.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        let textColor: NSColor = isDark ? .white : .black
        let shadow = NSShadow()
        shadow.shadowColor = (familyColor.highlight(withLevel: isDark ? 0.3 : 0.7) ?? familyColor)
        shadow.shadowOffset = NSSize(width: 1, height: -1)
        shadow.shadowBlurRadius = 0
        let titleFont: NSFont = {
            if let desc = AppDelegate.ywftBoldDescriptor,
               let f = NSFont(descriptor: desc, size: 14),
               f.familyName == "YWFT Processing" {
                return f
            }
            return NSFont.systemFont(ofSize: 14, weight: .black)
        }()
        instrumentLabel.attributedStringValue = NSAttributedString(
            string: GeneralMIDI.programNames[safe],
            attributes: [
                .font: titleFont,
                .foregroundColor: textColor,
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
    }

    private func makeHeldNoteBox(name: String, color: NSColor) -> NSView {
        let box = NSView()
        box.wantsLayer = true
        box.layer?.cornerRadius = 4
        box.layer?.backgroundColor = color.withAlphaComponent(0.85).cgColor
        box.translatesAutoresizingMaskIntoConstraints = false
        let label = NSTextField(labelWithString: name)
        label.font = NSFont.monospacedSystemFont(ofSize: 9, weight: .heavy)
        label.textColor = .black
        label.drawsBackground = false
        label.translatesAutoresizingMaskIntoConstraints = false
        box.addSubview(label)
        NSLayoutConstraint.activate([
            label.leadingAnchor.constraint(equalTo: box.leadingAnchor, constant: 5),
            label.trailingAnchor.constraint(equalTo: box.trailingAnchor, constant: -5),
            label.topAnchor.constraint(equalTo: box.topAnchor, constant: 1),
            label.bottomAnchor.constraint(equalTo: box.bottomAnchor, constant: -1),
        ])
        return box
    }

    private func configureControlButton(_ button: NSButton,
                                        symbolName: String,
                                        toolTip: String,
                                        action: Selector) {
        let config = NSImage.SymbolConfiguration(pointSize: 10, weight: .semibold)
        button.image = NSImage(
            systemSymbolName: symbolName,
            accessibilityDescription: toolTip
        )?.withSymbolConfiguration(config)
        button.isBordered = false
        button.setButtonType(.momentaryChange)
        button.imagePosition = .imageOnly
        button.contentTintColor = .white.withAlphaComponent(0.92)
        button.toolTip = toolTip
        button.target = self
        button.action = action
        button.alphaValue = 0
        button.wantsLayer = true
        button.layer?.cornerRadius = CGFloat(Self.controlButtonSize) / 2
        button.layer?.borderWidth = 1
    }

    @objc private func closeButtonClicked(_ sender: NSButton) {
        dismiss()
    }

    @objc private func dockButtonClicked(_ sender: NSButton) {
        resetCustomPosition()
    }

    @objc private func expandButtonClicked(_ sender: NSButton) {
        onExpandRequested?()
    }

    private func setControlsVisible(_ isVisible: Bool, animated: Bool = true) {
        let alpha: CGFloat = isVisible ? 1.0 : 0.0
        if animated {
            NSAnimationContext.runAnimationGroup { context in
                context.duration = 0.12
                closeButton.animator().alphaValue = alpha
                dockButton.animator().alphaValue = alpha
                expandButton.animator().alphaValue = alpha
            }
        } else {
            closeButton.alphaValue = alpha
            dockButton.alphaValue = alpha
            expandButton.alphaValue = alpha
        }
    }
}

final class MenuBarWaveformStrip {
    private let implementation: any MenuBarWaveformStripImplementation

    var onStepBackward: (() -> Void)? {
        get { implementation.onStepBackward }
        set { implementation.onStepBackward = newValue }
    }

    var onStepForward: (() -> Void)? {
        get { implementation.onStepForward }
        set { implementation.onStepForward = newValue }
    }

    var onStepUp: (() -> Void)? {
        get { implementation.onStepUp }
        set { implementation.onStepUp = newValue }
    }

    var onStepDown: (() -> Void)? {
        get { implementation.onStepDown }
        set { implementation.onStepDown = newValue }
    }

    var onExpandRequested: (() -> Void)? {
        get { implementation.onExpandRequested }
        set { implementation.onExpandRequested = newValue }
    }

    var isDocked: Bool { implementation.isDocked }

    var suppressed: Bool {
        get { implementation.suppressed }
        set { implementation.suppressed = newValue }
    }

    init(menuBand: MenuBandController) {
        switch resolvedStripVisualStyle() {
        case .liquid:
            implementation = LiquidMenuBarWaveformStrip(menuBand: menuBand)
        case .legacy:
            implementation = LegacyMenuBarWaveformStrip(menuBand: menuBand)
        }
    }

    func warmUp() {
        implementation.warmUp()
    }

    func showIfNeeded() {
        implementation.showIfNeeded()
    }

    func scheduleHide() {
        implementation.scheduleHide()
    }

    func dismiss() {
        implementation.dismiss()
    }

    func reposition(statusItemButton: NSStatusBarButton?) {
        implementation.reposition(statusItemButton: statusItemButton)
    }

    func refreshAppearance() {
        implementation.refreshAppearance()
    }

    func registerArrowInput() {
        implementation.registerArrowInput()
    }
}

private enum ResolvedStripVisualStyle {
    case liquid
    case legacy
}

private let stripStyleDefaultsDomain = "computer.aestheticcomputer.menuband"
private let stripStyleDefaultsKey = "MenuBandWaveformStripStyle"
private let stripStyleEnvironmentKey = "MENUBAND_WAVEFORM_STRIP_STYLE"

private func resolvedStripVisualStyle() -> ResolvedStripVisualStyle {
    let environmentValue = ProcessInfo.processInfo.environment[stripStyleEnvironmentKey]
    let defaultsValue = UserDefaults(suiteName: stripStyleDefaultsDomain)?.string(forKey: stripStyleDefaultsKey)
        ?? UserDefaults.standard.string(forKey: stripStyleDefaultsKey)
    let override = StripVisualStyleOverride(rawValue: environmentValue ?? defaultsValue)
    switch override {
    case .liquid:
        if #available(macOS 26.0, *) {
            return .liquid
        }
        return .legacy
    case .legacy:
        return .legacy
    case .automatic:
        if #available(macOS 26.0, *) {
            return .liquid
        }
        return .legacy
    }
}

private protocol MenuBarWaveformStripImplementation: AnyObject {
    var onStepBackward: (() -> Void)? { get set }
    var onStepForward: (() -> Void)? { get set }
    var onStepUp: (() -> Void)? { get set }
    var onStepDown: (() -> Void)? { get set }
    var onExpandRequested: (() -> Void)? { get set }
    var isDocked: Bool { get }
    var suppressed: Bool { get set }
    func warmUp()
    func showIfNeeded()
    func scheduleHide()
    func dismiss()
    func reposition(statusItemButton: NSStatusBarButton?)
    func refreshAppearance()
    func registerArrowInput()
}

private final class StripDragHandleView: NSView {
    var onResetRequested: (() -> Void)?

    private let imageView = NSImageView()
    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        translatesAutoresizingMaskIntoConstraints = false
        toolTip = "Double-click to re-dock."
        imageView.translatesAutoresizingMaskIntoConstraints = false
        imageView.imageScaling = .scaleProportionallyUpOrDown
        imageView.image = NSImage(
            systemSymbolName: "move.3d",
            accessibilityDescription: "Drag strip"
        )?.withSymbolConfiguration(.init(pointSize: 10, weight: .semibold))
        addSubview(imageView)
        NSLayoutConstraint.activate([
            widthAnchor.constraint(equalToConstant: 14),
            heightAnchor.constraint(equalToConstant: 14),
            imageView.centerXAnchor.constraint(equalTo: centerXAnchor),
            imageView.centerYAnchor.constraint(equalTo: centerYAnchor),
        ])
    }

    required init?(coder: NSCoder) { fatalError() }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        imageView.contentTintColor = .secondaryLabelColor
    }

    override func mouseDown(with event: NSEvent) {
        if event.clickCount == 2 {
            onResetRequested?()
        }
    }
}

private class StripInteractiveSurfaceView: NSView {
    var onMouseEntered: (() -> Void)?
    var onMouseExited: (() -> Void)?
    private var trackingArea: NSTrackingArea?

    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }

    override func updateTrackingAreas() {
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
        super.updateTrackingAreas()
    }

    override func mouseEntered(with event: NSEvent) {
        onMouseEntered?()
        super.mouseEntered(with: event)
    }

    override func mouseExited(with event: NSEvent) {
        onMouseExited?()
        super.mouseExited(with: event)
    }
}

private final class MenuBarWaveformStripPanel: NSPanel {
    var onDragBegan: (() -> Void)?
    var onDragEnded: (() -> Void)?
    var onFrameChanged: ((NSRect) -> Void)?
    var passthroughViews: [NSView] = []

    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { false }

    override func setFrame(_ frameRect: NSRect, display flag: Bool) {
        super.setFrame(frameRect, display: flag)
        onFrameChanged?(frame)
    }

    override func setFrameOrigin(_ point: NSPoint) {
        super.setFrameOrigin(point)
        onFrameChanged?(frame)
    }

    override func sendEvent(_ event: NSEvent) {
        if event.type == .leftMouseDown,
           event.clickCount == 1,
           shouldBeginDrag(for: event) {
            onDragBegan?()
            performDrag(with: event)
            onDragEnded?()
            return
        }
        super.sendEvent(event)
    }

    private func shouldBeginDrag(for event: NSEvent) -> Bool {
        guard let contentView else { return true }
        let point = contentView.convert(event.locationInWindow, from: nil)
        guard let hitView = contentView.hitTest(point) else { return true }
        return !passthroughViews.contains(where: { hitView === $0 || hitView.isDescendant(of: $0) })
    }
}

@available(macOS 26.0, *)
private final class StripMovableGlassEffectView: NSGlassEffectView {
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
}

/// Thin floating waveform strip that slides down from beneath the menubar
/// piano when notes are played, and slides back up after a configurable
/// idle delay. Hidden whenever the settings popover or floating piano
/// palette is visible to avoid visual clutter.
///
/// The panel sits one level below `NSWindow.Level.mainMenu` so the menubar
/// itself occludes it. The slide animation moves the panel from behind the
/// menubar downward into view, and reverses to hide.
private final class LiquidMenuBarWaveformStrip: NSObject, MenuBarWaveformStripImplementation {
    private let menuBand: MenuBandController
    private let waveformView = WaveformView()
    private let waveformBezel = StripInteractiveSurfaceView()
    private let waveformClipView = StripInteractiveSurfaceView()
    private var waveformBezelHeightConstraint: NSLayoutConstraint?
    private let instrumentArrows = ArrowKeysIndicator()
    private let instrumentRowContainer = StripInteractiveSurfaceView()
    private let instrumentLabel = NSTextField(labelWithString: "")
    private let closeButton = NSButton()
    private let dockButton = NSButton()
    private let expandButton = NSButton()
    private let heldNotesContainer = StripInteractiveSurfaceView()
    private let heldNotesStack = NSStackView()
    private weak var closeButtonGlassView: NSView?
    private weak var dockButtonGlassView: NSView?
    private weak var expandButtonGlassView: NSView?
    private weak var waveformGlassView: NSView?
    private weak var stripBackgroundView: NSView?
    private var panel: NSPanel?
    private weak var statusButton: NSStatusBarButton?
    var onStepBackward: (() -> Void)?
    var onStepForward: (() -> Void)?
    var onStepUp: (() -> Void)?
    var onStepDown: (() -> Void)?
    var onExpandRequested: (() -> Void)?

    /// How long to keep the strip visible after the last note ends.
    private let hideDelay: TimeInterval = 2.0
    private var hideWorkItem: DispatchWorkItem?
    private let customOriginXKey = "notepat.waveformStrip.customOriginX"
    private let customOriginYKey = "notepat.waveformStrip.customOriginY"
    private var customOrigin: NSPoint?

    /// Match the popover waveform bezel's width:height ratio (224:64) so the
    /// meter reads consistently in both places.
    private static let waveformAspectRatio: CGFloat = InstrumentListView.preferredWidth / 64
    private static let waveformGlassCornerRadius: CGFloat = 14
    private static let waveformClipCornerRadius: CGFloat = 12
    private static let controlButtonSize: CGFloat = 24
    private static let controlInset: CGFloat = 8
    private static let waveformOuterInset: CGFloat = 4
    private static let waveformInnerInset: CGFloat = 2
    private static let footerHeight: CGFloat = 54
    private static let heldNotesRowHeight: CGFloat = 14
    private static let instrumentRowHeight: CGFloat = 30
    private static let hitTestBackdropAlpha: CGFloat = 0.001
    private static let defaultsDomain = "computer.aestheticcomputer.menuband"
    private static let styleOverrideDefaultsKey = "MenuBandWaveformStripStyle"
    private static let styleOverrideEnvironmentKey = "MENUBAND_WAVEFORM_STRIP_STYLE"

    /// Animation duration in seconds.
    private static let fadeInDuration: TimeInterval = 0.18
    private static let fadeOutDuration: TimeInterval = 0.18

    /// Window level just below the menubar so the menubar occludes the
    /// strip while it's tucked behind.
    private static let stripLevel = NSWindow.Level.floating

    private static var visualStyleOverride: StripVisualStyleOverride {
        let environmentValue = ProcessInfo.processInfo.environment[styleOverrideEnvironmentKey]
        if environmentValue != nil {
            return StripVisualStyleOverride(rawValue: environmentValue)
        }
        let defaultsValue = UserDefaults(suiteName: defaultsDomain)?.string(forKey: styleOverrideDefaultsKey)
            ?? UserDefaults.standard.string(forKey: styleOverrideDefaultsKey)
        return StripVisualStyleOverride(rawValue: defaultsValue)
    }

    private static var shouldUseLiquidGlass: Bool {
        switch visualStyleOverride {
        case .liquid:
            if #available(macOS 26.0, *) {
                return true
            }
            return false
        case .legacy:
            return false
        case .automatic:
            if #available(macOS 26.0, *) {
                return true
            }
            return false
        }
    }

    /// CVDisplayLink-driven slide animation. NSAnimationContext + animator()
    /// proxy doesn't reliably move borderless non-activating panels, so we
    /// drive the frame ourselves at display refresh rate.
    private var slideLink: CVDisplayLink?
    private var slideStartTime: CFTimeInterval = 0
    private var slideFromY: CGFloat = 0
    private var slideToY: CGFloat = 0
    private var slideCompletion: (() -> Void)?
    private var isSliding = false
    private var isPointerInsideStrip = false
    private var isDraggingStrip = false

    /// True while the strip panel is on screen (visible or animating in).
    var isShown: Bool { panel?.isVisible == true }
    var isDocked: Bool { customOrigin == nil }

    /// External suppression — callers set this when the popover or floating
    /// palette opens. While suppressed, `showIfNeeded` is a no-op and any
    /// visible strip is immediately hidden.
    var suppressed: Bool = false {
        didSet {
            guard suppressed != oldValue else { return }
            if suppressed { hide(animated: false) }
        }
    }

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        super.init()
        customOrigin = loadCustomOrigin()
        waveformView.menuBand = menuBand
        waveformView.setSurfaceStyle(.glassEmbedded)
        waveformBezel.wantsLayer = true
        waveformBezel.layer?.cornerRadius = Self.waveformGlassCornerRadius
        waveformBezel.layer?.borderWidth = 0.8
        if #available(macOS 10.15, *) {
            waveformBezel.layer?.cornerCurve = .continuous
        }
        waveformClipView.wantsLayer = true
        waveformClipView.layer?.cornerRadius = Self.waveformClipCornerRadius
        waveformClipView.layer?.masksToBounds = true
        if #available(macOS 10.15, *) {
            waveformClipView.layer?.cornerCurve = .continuous
        }
        heldNotesStack.orientation = .horizontal
        heldNotesStack.alignment = .centerY
        heldNotesStack.spacing = 4
        instrumentLabel.drawsBackground = false
        instrumentLabel.lineBreakMode = .byTruncatingTail
        instrumentArrows.displayMode = .cluster
        instrumentArrows.toolTip = "Change instrument or octave"
        instrumentArrows.onClick = { [weak self] dir, isDown in
            guard isDown else { return }
            self?.registerArrowInput()
            switch dir {
            case 0: self?.onStepBackward?()
            case 1: self?.onStepForward?()
            case 2: self?.onStepDown?()
            case 3: self?.onStepUp?()
            default: break
            }
        }
        configureControlButton(
            closeButton,
            symbolName: "xmark",
            toolTip: "Close",
            action: #selector(closeButtonClicked(_:))
        )
        configureControlButton(
            dockButton,
            symbolName: "menubar.dock.rectangle",
            toolTip: "Dock Below Menubar Piano",
            action: #selector(dockButtonClicked(_:))
        )
        configureControlButton(
            expandButton,
            symbolName: "square.resize.up",
            toolTip: "Expand floating piano",
            action: #selector(expandButtonClicked(_:))
        )
    }

    private func handleStripDragBegan() {
        isDraggingStrip = true
        cancelPendingHide()
        stopSlide()
        isSliding = false
    }

    private func handleStripDragEnded() {
        isDraggingStrip = false
        persistPanelOriginIfNeeded(force: true)
        refreshAutoHideState()
    }

    private func persistPanelOriginIfNeeded(force: Bool = false) {
        guard let panel, force || isDraggingStrip else { return }
        let frame = clampedFrame(origin: panel.frame.origin, size: panel.frame.size, preferredScreen: panel.screen)
        customOrigin = frame.origin
        saveCustomOrigin(frame.origin)
        if panel.frame.origin != frame.origin {
            panel.setFrameOrigin(frame.origin)
        }
    }

    private func handleStripMouseEntered() {
        guard !isPointerInsideStrip else { return }
        isPointerInsideStrip = true
        setControlsVisible(true)
        cancelPendingHide()
    }

    private func handleStripMouseExited() {
        guard isPointerInsideStrip else { return }
        isPointerInsideStrip = false
        setControlsVisible(false)
        refreshAutoHideState()
    }

    private func refreshAutoHideState() {
        guard menuBand.litNotes.isEmpty else {
            cancelPendingHide()
            return
        }
        scheduleHide()
    }

    /// Pre-build the panel so the first note doesn't pay construction cost.
    func warmUp() {
        if panel == nil { buildPanel() }
    }

    deinit {
        stopSlide()
    }

    // MARK: - Show / hide

    /// Call whenever `litNotes` becomes non-empty (a note starts).
    func showIfNeeded() {
        guard !suppressed else { return }
        cancelPendingHide()
        if !isShown && !isSliding { show() }
    }

    /// Call whenever `litNotes` becomes empty (all notes released).
    func scheduleHide() {
        guard !isPointerInsideStrip, !isDraggingStrip else {
            cancelPendingHide()
            return
        }
        cancelPendingHide()
        let work = DispatchWorkItem { [weak self] in
            self?.hide(animated: true)
        }
        hideWorkItem = work
        DispatchQueue.main.asyncAfter(deadline: .now() + hideDelay, execute: work)
    }

    /// Immediately tear down (app termination, etc.).
    func dismiss() {
        cancelPendingHide()
        stopSlide()
        isPointerInsideStrip = false
        setControlsVisible(false, animated: false)
        waveformView.isLive = false
        panel?.orderOut(nil)
    }

    /// Store the button reference so positioning works from inside show().
    func reposition(statusItemButton: NSStatusBarButton?) {
        statusButton = statusItemButton
        guard let panel = panel, panel.isVisible, !isSliding else { return }
        guard isDocked else { return }
        positionPanel(panel)
    }

    func refreshAppearance() {
        applyAppearanceToVisualizer()
        applyWaveformTint()
        refreshReadout()
    }

    func registerArrowInput() {
        guard !suppressed else { return }
        showIfNeeded()
        refreshReadout()
        if menuBand.litNotes.isEmpty {
            scheduleHide()
        }
    }

    // MARK: - Geometry

    /// Compute the target frame for the strip: directly below the menubar,
    /// aligned to the piano key region of the status item.
    private func targetFrame() -> NSRect? {
        guard let button = statusButton,
              let buttonWindow = button.window else { return nil }

        let imgSize = KeyboardIconRenderer.imageSize
        let bb = button.bounds
        let xOff = (bb.width - imgSize.width) / 2.0

        let pianoOriginX = xOff + KeyboardIconRenderer.pad
        let pianoWidth = imgSize.width - KeyboardIconRenderer.settingsW
            - KeyboardIconRenderer.settingsGap - KeyboardIconRenderer.pad * 2

        let localRect = NSRect(x: pianoOriginX, y: 0, width: pianoWidth, height: bb.height)
        let windowRect = button.convert(localRect, to: nil)
        let screenRect = buttonWindow.convertToScreen(windowRect)

        let stripHeight = currentStripHeight(for: screenRect.width)
        let anchoredFrame = NSRect(
            x: screenRect.origin.x,
            y: screenRect.origin.y - stripHeight,
            width: screenRect.width,
            height: stripHeight
        )
        guard let customOrigin else { return anchoredFrame }
        return clampedFrame(
            origin: customOrigin,
            size: anchoredFrame.size,
            preferredScreen: panel?.screen ?? buttonWindow.screen
        )
    }

    private func currentWaveformBezelHeight(for width: CGFloat) -> CGFloat {
        round(width / Self.waveformAspectRatio)
    }

    private func currentStripHeight(for width: CGFloat) -> CGFloat {
        currentWaveformBezelHeight(for: width) + Self.footerHeight
    }

    private func loadCustomOrigin() -> NSPoint? {
        let defaults = UserDefaults.standard
        guard defaults.object(forKey: customOriginXKey) != nil,
              defaults.object(forKey: customOriginYKey) != nil else { return nil }
        return NSPoint(
            x: defaults.double(forKey: customOriginXKey),
            y: defaults.double(forKey: customOriginYKey)
        )
    }

    private func saveCustomOrigin(_ origin: NSPoint) {
        let defaults = UserDefaults.standard
        defaults.set(origin.x, forKey: customOriginXKey)
        defaults.set(origin.y, forKey: customOriginYKey)
    }

    private func clampedFrame(origin: NSPoint, size: NSSize, preferredScreen: NSScreen?) -> NSRect {
        let visible = visibleFrame(for: origin, preferredScreen: preferredScreen)
        let x = min(max(origin.x, visible.minX), visible.maxX - size.width)
        let y = min(max(origin.y, visible.minY), visible.maxY - size.height)
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

    private func moveStrip(to origin: NSPoint) {
        guard let panel = panel else { return }
        let frame = clampedFrame(origin: origin, size: panel.frame.size, preferredScreen: panel.screen)
        customOrigin = frame.origin
        saveCustomOrigin(frame.origin)
        panel.setFrameOrigin(frame.origin)
    }

    private func resetCustomPosition() {
        customOrigin = nil
        let defaults = UserDefaults.standard
        defaults.removeObject(forKey: customOriginXKey)
        defaults.removeObject(forKey: customOriginYKey)
        guard let panel = panel else { return }
        positionPanel(panel)
    }

    // MARK: - Visibility animation

    private func show() {
        if panel == nil { buildPanel() }
        guard let panel = panel, let target = targetFrame() else { return }

        isPointerInsideStrip = false
        setControlsVisible(false, animated: false)
        waveformBezelHeightConstraint?.constant = currentWaveformBezelHeight(for: target.width)
        panel.setFrame(target, display: true)
        isSliding = true
        panel.alphaValue = 1.0
        panel.alphaValue = 0.0
        applyAppearanceToVisualizer()
        applyWaveformTint()
        refreshReadout()
        waveformView.isLive = true
        panel.orderFrontRegardless()

        NSAnimationContext.runAnimationGroup { context in
            context.duration = Self.fadeInDuration
            context.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
            panel.animator().alphaValue = 1.0
        } completionHandler: { [weak self, weak panel] in
            guard let self, let panel else { return }
            panel.alphaValue = 1.0
            self.isSliding = false
        }
    }

    private func hide(animated: Bool) {
        cancelPendingHide()
        stopSlide()
        guard !isPointerInsideStrip, !isDraggingStrip else { return }
        guard let panel = panel, panel.isVisible else {
            waveformView.isLive = false
            return
        }
        if !animated {
            panel.animator().alphaValue = 1.0
            waveformView.isLive = false
            panel.orderOut(nil)
            return
        }
        NSAnimationContext.runAnimationGroup { context in
            context.duration = Self.fadeOutDuration
            context.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
            panel.animator().alphaValue = 0.0
        } completionHandler: { [weak self, weak panel] in
            guard let self, let panel else { return }
            self.waveformView.isLive = false
            panel.orderOut(nil)
            panel.alphaValue = 1.0
            self.isSliding = false
        }
    }

    private func startSlide(fromY: CGFloat, toY: CGFloat, completion: @escaping () -> Void) {
        stopSlide()
        isSliding = true
        slideFromY = fromY
        slideToY = toY
        slideStartTime = CACurrentMediaTime()
        slideCompletion = completion

        var link: CVDisplayLink?
        CVDisplayLinkCreateWithActiveCGDisplays(&link)
        guard let link = link else {
            // Fallback: jump immediately if CVDisplayLink fails.
            panel?.setFrameOrigin(NSPoint(x: panel?.frame.origin.x ?? 0, y: toY))
            completion()
            return
        }
        let opaque = Unmanaged.passUnretained(self).toOpaque()
        CVDisplayLinkSetOutputCallback(link, { _, _, _, _, _, ctx -> CVReturn in
            guard let ctx = ctx else { return kCVReturnSuccess }
            let strip = Unmanaged<LiquidMenuBarWaveformStrip>.fromOpaque(ctx).takeUnretainedValue()
            DispatchQueue.main.async { strip.tickSlide() }
            return kCVReturnSuccess
        }, opaque)
        CVDisplayLinkStart(link)
        slideLink = link
    }

    private func tickSlide() {
        guard isSliding, let panel = panel else {
            stopSlide()
            return
        }
        let elapsed = CACurrentMediaTime() - slideStartTime
        let t = min(1.0, elapsed / Self.fadeInDuration)
        // Ease-out cubic for show, ease-in cubic for hide.
        let eased: CGFloat
        if slideToY < slideFromY {
            // Sliding down (show): ease-out
            let f = 1.0 - t
            eased = CGFloat(1.0 - f * f * f)
        } else {
            // Sliding up (hide): ease-in
            eased = CGFloat(t * t * t)
        }
        let currentY = slideFromY + (slideToY - slideFromY) * eased
        panel.setFrameOrigin(NSPoint(x: panel.frame.origin.x, y: currentY))

        if t >= 1.0 {
            let completion = slideCompletion
            stopSlide()
            completion?()
        }
    }

    private func stopSlide() {
        if let link = slideLink {
            CVDisplayLinkStop(link)
            slideLink = nil
        }
        slideCompletion = nil
    }

    private func cancelPendingHide() {
        hideWorkItem?.cancel()
        hideWorkItem = nil
    }

    // MARK: - Panel construction

    private func buildPanel() {
        let initialWidth: CGFloat = 200
        let p = MenuBarWaveformStripPanel(
            contentRect: NSRect(
                origin: .zero,
                size: NSSize(width: initialWidth, height: currentStripHeight(for: initialWidth))
            ),
            styleMask: [.borderless],
            backing: .buffered,
            defer: false
        )
        p.isOpaque = false
        p.backgroundColor = .clear
        p.hasShadow = true
        p.level = Self.stripLevel
        p.collectionBehavior = [.transient, .ignoresCycle]
        p.hidesOnDeactivate = false
        p.canHide = false
        p.isMovable = true
        p.isMovableByWindowBackground = false
        p.animationBehavior = .none
        p.acceptsMouseMovedEvents = false
        p.onDragBegan = { [weak self] in self?.handleStripDragBegan() }
        p.onDragEnded = { [weak self] in self?.handleStripDragEnded() }
        p.onFrameChanged = { [weak self] _ in
            self?.persistPanelOriginIfNeeded()
        }
        p.passthroughViews = [instrumentArrows, closeButton, dockButton, expandButton]
        waveformView.translatesAutoresizingMaskIntoConstraints = false
        waveformBezel.translatesAutoresizingMaskIntoConstraints = false
        waveformClipView.translatesAutoresizingMaskIntoConstraints = false
        instrumentArrows.translatesAutoresizingMaskIntoConstraints = false
        instrumentRowContainer.translatesAutoresizingMaskIntoConstraints = false
        instrumentLabel.translatesAutoresizingMaskIntoConstraints = false
        closeButton.translatesAutoresizingMaskIntoConstraints = false
        dockButton.translatesAutoresizingMaskIntoConstraints = false
        expandButton.translatesAutoresizingMaskIntoConstraints = false
        heldNotesContainer.translatesAutoresizingMaskIntoConstraints = false
        heldNotesStack.translatesAutoresizingMaskIntoConstraints = false
        let rootView = StripInteractiveSurfaceView()
        rootView.translatesAutoresizingMaskIntoConstraints = false
        rootView.onMouseEntered = { [weak self] in self?.handleStripMouseEntered() }
        rootView.onMouseExited = { [weak self] in self?.handleStripMouseExited() }
        rootView.wantsLayer = true
        // Keep the panel visually transparent while ensuring WindowServer
        // still treats it as a hittable surface instead of passing clicks
        // through to the app underneath.
        rootView.layer?.backgroundColor = NSColor.black.withAlphaComponent(Self.hitTestBackdropAlpha).cgColor
        let layoutRoot: NSView
        if #available(macOS 26.0, *) {
            let glassContainer = NSGlassEffectContainerView()
            glassContainer.translatesAutoresizingMaskIntoConstraints = false
            glassContainer.spacing = 18
            glassContainer.contentView = rootView
            p.contentView = glassContainer

            let stripGlassView = StripMovableGlassEffectView()
            stripGlassView.translatesAutoresizingMaskIntoConstraints = false
            stripGlassView.cornerRadius = 14
            let stripContentView = StripInteractiveSurfaceView()
            stripContentView.translatesAutoresizingMaskIntoConstraints = false
            stripGlassView.contentView = stripContentView
            rootView.addSubview(stripGlassView)
            NSLayoutConstraint.activate([
                stripGlassView.leadingAnchor.constraint(equalTo: rootView.leadingAnchor),
                stripGlassView.trailingAnchor.constraint(equalTo: rootView.trailingAnchor),
                stripGlassView.topAnchor.constraint(equalTo: rootView.topAnchor),
                stripGlassView.bottomAnchor.constraint(equalTo: rootView.bottomAnchor),
            ])

            stripBackgroundView = stripGlassView
            layoutRoot = stripContentView

            let waveformGlassView = StripMovableGlassEffectView()
            waveformGlassView.translatesAutoresizingMaskIntoConstraints = false
            waveformGlassView.cornerRadius = Self.waveformGlassCornerRadius
            let waveformContentView = StripInteractiveSurfaceView()
            waveformContentView.translatesAutoresizingMaskIntoConstraints = false
            waveformGlassView.contentView = waveformContentView
            layoutRoot.addSubview(waveformBezel)
            waveformBezel.addSubview(waveformGlassView)
            self.waveformGlassView = waveformGlassView
            waveformContentView.addSubview(waveformClipView)
            waveformClipView.addSubview(waveformView)
            NSLayoutConstraint.activate([
                waveformBezel.leadingAnchor.constraint(equalTo: layoutRoot.leadingAnchor, constant: Self.waveformOuterInset),
                waveformBezel.trailingAnchor.constraint(equalTo: layoutRoot.trailingAnchor, constant: -Self.waveformOuterInset),
                waveformBezel.topAnchor.constraint(equalTo: layoutRoot.topAnchor, constant: Self.waveformOuterInset),
                waveformGlassView.leadingAnchor.constraint(equalTo: waveformBezel.leadingAnchor),
                waveformGlassView.trailingAnchor.constraint(equalTo: waveformBezel.trailingAnchor),
                waveformGlassView.topAnchor.constraint(equalTo: waveformBezel.topAnchor),
                waveformGlassView.bottomAnchor.constraint(equalTo: waveformBezel.bottomAnchor),
                waveformClipView.leadingAnchor.constraint(equalTo: waveformContentView.leadingAnchor, constant: Self.waveformInnerInset),
                waveformClipView.trailingAnchor.constraint(equalTo: waveformContentView.trailingAnchor, constant: -Self.waveformInnerInset),
                waveformClipView.topAnchor.constraint(equalTo: waveformContentView.topAnchor, constant: Self.waveformInnerInset),
                waveformClipView.bottomAnchor.constraint(equalTo: waveformContentView.bottomAnchor, constant: -Self.waveformInnerInset),
                waveformView.leadingAnchor.constraint(equalTo: waveformClipView.leadingAnchor),
                waveformView.trailingAnchor.constraint(equalTo: waveformClipView.trailingAnchor),
                waveformView.topAnchor.constraint(equalTo: waveformClipView.topAnchor),
                waveformView.bottomAnchor.constraint(equalTo: waveformClipView.bottomAnchor),
            ])
        } else {
            p.contentView = rootView
            stripBackgroundView = rootView
            rootView.addSubview(waveformBezel)
            waveformBezel.addSubview(waveformClipView)
            waveformClipView.addSubview(waveformView)
            NSLayoutConstraint.activate([
                waveformBezel.leadingAnchor.constraint(equalTo: rootView.leadingAnchor, constant: Self.waveformOuterInset),
                waveformBezel.trailingAnchor.constraint(equalTo: rootView.trailingAnchor, constant: -Self.waveformOuterInset),
                waveformBezel.topAnchor.constraint(equalTo: rootView.topAnchor, constant: Self.waveformOuterInset),
                waveformClipView.leadingAnchor.constraint(equalTo: waveformBezel.leadingAnchor, constant: Self.waveformInnerInset),
                waveformClipView.trailingAnchor.constraint(equalTo: waveformBezel.trailingAnchor, constant: -Self.waveformInnerInset),
                waveformClipView.topAnchor.constraint(equalTo: waveformBezel.topAnchor, constant: Self.waveformInnerInset),
                waveformClipView.bottomAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: -Self.waveformInnerInset),
                waveformView.leadingAnchor.constraint(equalTo: waveformClipView.leadingAnchor),
                waveformView.trailingAnchor.constraint(equalTo: waveformClipView.trailingAnchor),
                waveformView.topAnchor.constraint(equalTo: waveformClipView.topAnchor),
                waveformView.bottomAnchor.constraint(equalTo: waveformClipView.bottomAnchor),
            ])
            layoutRoot = rootView
            waveformGlassView = waveformBezel
        }
        layoutRoot.addSubview(heldNotesContainer)
        layoutRoot.addSubview(instrumentRowContainer)
        rootView.addSubview(closeButton)
        rootView.addSubview(dockButton)
        rootView.addSubview(expandButton)
        if #available(macOS 26.0, *) {
            closeButtonGlassView = installControlGlassBackground(for: closeButton, in: rootView)
            dockButtonGlassView = installControlGlassBackground(for: dockButton, in: rootView)
            expandButtonGlassView = installControlGlassBackground(for: expandButton, in: rootView)
        }
        instrumentRowContainer.addSubview(instrumentArrows)
        instrumentRowContainer.addSubview(instrumentLabel)
        heldNotesContainer.addSubview(heldNotesStack)
        let waveformBezelHeightConstraint = waveformBezel.heightAnchor.constraint(
            equalToConstant: currentWaveformBezelHeight(for: initialWidth)
        )
        self.waveformBezelHeightConstraint = waveformBezelHeightConstraint
        NSLayoutConstraint.activate([
            waveformBezelHeightConstraint,
            heldNotesContainer.leadingAnchor.constraint(equalTo: layoutRoot.leadingAnchor, constant: 4),
            heldNotesContainer.trailingAnchor.constraint(equalTo: layoutRoot.trailingAnchor, constant: -4),
            heldNotesContainer.topAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: 3),
            heldNotesContainer.heightAnchor.constraint(equalToConstant: Self.heldNotesRowHeight),
            heldNotesStack.centerXAnchor.constraint(equalTo: heldNotesContainer.centerXAnchor),
            heldNotesStack.centerYAnchor.constraint(equalTo: heldNotesContainer.centerYAnchor),
            closeButton.topAnchor.constraint(equalTo: rootView.topAnchor, constant: Self.controlInset),
            closeButton.leadingAnchor.constraint(equalTo: rootView.leadingAnchor, constant: Self.controlInset),
            closeButton.widthAnchor.constraint(equalToConstant: Self.controlButtonSize),
            closeButton.heightAnchor.constraint(equalToConstant: Self.controlButtonSize),
            expandButton.topAnchor.constraint(equalTo: rootView.topAnchor, constant: Self.controlInset),
            expandButton.trailingAnchor.constraint(equalTo: rootView.trailingAnchor, constant: -Self.controlInset),
            expandButton.widthAnchor.constraint(equalToConstant: Self.controlButtonSize),
            expandButton.heightAnchor.constraint(equalToConstant: Self.controlButtonSize),
            dockButton.topAnchor.constraint(equalTo: rootView.topAnchor, constant: Self.controlInset),
            dockButton.trailingAnchor.constraint(equalTo: expandButton.leadingAnchor, constant: -6),
            dockButton.widthAnchor.constraint(equalToConstant: Self.controlButtonSize),
            dockButton.heightAnchor.constraint(equalToConstant: Self.controlButtonSize),
            instrumentRowContainer.leadingAnchor.constraint(equalTo: layoutRoot.leadingAnchor, constant: 4),
            instrumentRowContainer.trailingAnchor.constraint(equalTo: layoutRoot.trailingAnchor, constant: -4),
            instrumentRowContainer.topAnchor.constraint(equalTo: heldNotesContainer.bottomAnchor, constant: 3),
            instrumentRowContainer.heightAnchor.constraint(equalToConstant: Self.instrumentRowHeight),
            instrumentRowContainer.bottomAnchor.constraint(equalTo: layoutRoot.bottomAnchor, constant: -4),
            instrumentArrows.leadingAnchor.constraint(equalTo: instrumentRowContainer.leadingAnchor, constant: 6),
            instrumentArrows.centerYAnchor.constraint(equalTo: instrumentRowContainer.centerYAnchor),
            instrumentLabel.leadingAnchor.constraint(equalTo: instrumentArrows.trailingAnchor, constant: 4),
            instrumentLabel.centerYAnchor.constraint(equalTo: instrumentRowContainer.centerYAnchor),
            instrumentLabel.trailingAnchor.constraint(equalTo: instrumentRowContainer.trailingAnchor, constant: -6),
        ])
        applyAppearanceToVisualizer()
        applyWaveformTint()
        refreshReadout()
        setControlsVisible(false, animated: false)

        panel = p
    }

    private func positionPanel(_ panel: NSPanel) {
        guard let target = targetFrame() else { return }
        waveformBezelHeightConstraint?.constant = currentWaveformBezelHeight(for: target.width)
        panel.setFrame(target, display: true)
    }

    private func applyAppearanceToVisualizer() {
        let isDark = NSApp.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        waveformView.setLightMode(!isDark)
        if isDark {
            waveformBezel.layer?.backgroundColor = NSColor.white.withAlphaComponent(0.04).cgColor
            instrumentLabel.textColor = .labelColor
        } else {
            waveformBezel.layer?.backgroundColor = NSColor.white.withAlphaComponent(0.18).cgColor
            instrumentLabel.textColor = .labelColor
        }
    }

    private func applyWaveformTint() {
        let stripTint: NSColor
        if menuBand.midiMode {
            waveformView.setDotMatrix(MenuBandPopoverViewController.midiDotPattern)
            waveformView.setBaseColor(.controlAccentColor)
            waveformBezel.layer?.borderColor = NSColor.controlAccentColor
                .withAlphaComponent(0.24).cgColor
            stripTint = NSColor.controlAccentColor.withAlphaComponent(0.20)
        } else {
            waveformView.setDotMatrix(nil)
            let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
            let familyColor = InstrumentListView.colorForProgram(safe)
            waveformView.setBaseColor(familyColor)
            waveformBezel.layer?.borderColor = familyColor
                .withAlphaComponent(0.22).cgColor
            stripTint = familyColor.withAlphaComponent(0.16)
        }
        if #available(macOS 26.0, *),
           let glassView = stripBackgroundView as? NSGlassEffectView {
            glassView.tintColor = stripTint
        }
        if #available(macOS 26.0, *),
           let glassView = waveformGlassView as? NSGlassEffectView {
            glassView.tintColor = stripTint.withAlphaComponent(0.55)
        }
        if #available(macOS 26.0, *) {
            for view in [closeButtonGlassView, dockButtonGlassView, expandButtonGlassView] {
                (view as? NSGlassEffectView)?.style = .clear
                (view as? NSGlassEffectView)?.tintColor = stripTint.withAlphaComponent(0.34)
            }
            for button in [closeButton, dockButton, expandButton] {
                button.layer?.backgroundColor = NSColor.clear.cgColor
                button.layer?.borderColor = NSColor.clear.cgColor
            }
        } else {
            for button in [closeButton, dockButton, expandButton] {
                button.layer?.backgroundColor = NSColor.windowBackgroundColor.withAlphaComponent(0.22).cgColor
                button.layer?.borderColor = NSColor.white.withAlphaComponent(0.28).cgColor
            }
        }
    }

    private func refreshReadout() {
        let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
        let familyColor = menuBand.midiMode
            ? NSColor.controlAccentColor
            : InstrumentListView.colorForProgram(safe)
        let isDark = NSApp.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        let textColor: NSColor = isDark ? .white : .black
        let shadow = NSShadow()
        shadow.shadowColor = (familyColor.highlight(withLevel: isDark ? 0.3 : 0.7) ?? familyColor)
        shadow.shadowOffset = NSSize(width: 1, height: -1)
        shadow.shadowBlurRadius = 0
        let titleFont: NSFont = {
            if let desc = AppDelegate.ywftBoldDescriptor,
               let f = NSFont(descriptor: desc, size: 14),
               f.familyName == "YWFT Processing" {
                return f
            }
            return NSFont.systemFont(ofSize: 14, weight: .black)
        }()
        instrumentLabel.attributedStringValue = NSAttributedString(
            string: GeneralMIDI.programNames[safe],
            attributes: [
                .font: titleFont,
                .foregroundColor: textColor,
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
    }

    private func makeHeldNoteBox(name: String, color: NSColor) -> NSView {
        let box = NSView()
        box.wantsLayer = true
        box.layer?.cornerRadius = 4
        box.layer?.backgroundColor = color.withAlphaComponent(0.85).cgColor
        box.translatesAutoresizingMaskIntoConstraints = false
        let label = NSTextField(labelWithString: name)
        label.font = NSFont.monospacedSystemFont(ofSize: 9, weight: .heavy)
        label.textColor = .black
        label.drawsBackground = false
        label.translatesAutoresizingMaskIntoConstraints = false
        box.addSubview(label)
        NSLayoutConstraint.activate([
            label.leadingAnchor.constraint(equalTo: box.leadingAnchor, constant: 5),
            label.trailingAnchor.constraint(equalTo: box.trailingAnchor, constant: -5),
            label.topAnchor.constraint(equalTo: box.topAnchor, constant: 1),
            label.bottomAnchor.constraint(equalTo: box.bottomAnchor, constant: -1),
        ])
        return box
    }

    private func configureControlButton(_ button: NSButton,
                                        symbolName: String,
                                        toolTip: String,
                                        action: Selector) {
        let config = NSImage.SymbolConfiguration(pointSize: 11, weight: .semibold)
        button.image = NSImage(
            systemSymbolName: symbolName,
            accessibilityDescription: toolTip
        )?.withSymbolConfiguration(config)
        button.isBordered = false
        button.setButtonType(.momentaryChange)
        button.imagePosition = .imageOnly
        button.contentTintColor = .white.withAlphaComponent(0.92)
        button.toolTip = toolTip
        button.target = self
        button.action = action
        button.alphaValue = 0
        button.wantsLayer = true
        button.layer?.cornerRadius = Self.controlButtonSize / 2
        button.layer?.borderWidth = 1
    }

    @objc private func closeButtonClicked(_ sender: NSButton) {
        dismiss()
    }

    @objc private func dockButtonClicked(_ sender: NSButton) {
        resetCustomPosition()
    }

    @objc private func expandButtonClicked(_ sender: NSButton) {
        onExpandRequested?()
    }

    @available(macOS 26.0, *)
    private func installControlGlassBackground(for button: NSButton, in container: NSView) -> NSView {
        let glassView = StripMovableGlassEffectView()
        glassView.translatesAutoresizingMaskIntoConstraints = false
        glassView.cornerRadius = Self.controlButtonSize / 2
        glassView.style = .clear
        glassView.alphaValue = 0
        container.addSubview(glassView, positioned: .below, relativeTo: button)
        NSLayoutConstraint.activate([
            glassView.leadingAnchor.constraint(equalTo: button.leadingAnchor),
            glassView.trailingAnchor.constraint(equalTo: button.trailingAnchor),
            glassView.topAnchor.constraint(equalTo: button.topAnchor),
            glassView.bottomAnchor.constraint(equalTo: button.bottomAnchor),
        ])
        return glassView
    }

    private func setControlsVisible(_ isVisible: Bool, animated: Bool = true) {
        let alpha: CGFloat = isVisible ? 1.0 : 0.0
        if animated {
            NSAnimationContext.runAnimationGroup { context in
                context.duration = 0.12
                closeButton.animator().alphaValue = alpha
                dockButton.animator().alphaValue = alpha
                expandButton.animator().alphaValue = alpha
                closeButtonGlassView?.animator().alphaValue = alpha
                dockButtonGlassView?.animator().alphaValue = alpha
                expandButtonGlassView?.animator().alphaValue = alpha
            }
        } else {
            closeButton.alphaValue = alpha
            dockButton.alphaValue = alpha
            expandButton.alphaValue = alpha
            closeButtonGlassView?.alphaValue = alpha
            dockButtonGlassView?.alphaValue = alpha
            expandButtonGlassView?.alphaValue = alpha
        }
    }

}
