//
//  ExpandedPianoWaveformView.swift
//  MenuBand
//
//  Created by Esteban Uribe on 5/3/26.
//


import AppKit

final class ExpandedPianoWaveformView: NSView {
    static var shouldUseLiquidGlass: Bool {
        PianoWaveformWindowStyle.shouldUseLiquidGlass
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
    private let instrumentArrows = ArrowKeysIndicator()
    private let instrumentTitleRow = NSView()
    private let hapticsControls = NSStackView()
    private let hapticsLabel = NSTextField(labelWithString: "Haptics")
    private let hapticsSwitch = NSSwitch()
    private let hapticsInfoButton = NSButton()
    private let pianoView: PianoKeyboardView
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

    private var outlineBorderColor: NSColor = .separatorColor.withAlphaComponent(0.55)

    var isPianoFocusActive: (() -> Bool)?
    var onHoverChanged: ((Bool) -> Void)?
    var onStepBackward: (() -> Void)?
    var onStepForward: (() -> Void)?
    var onStepUp: (() -> Void)?
    var onStepDown: (() -> Void)?

    private let pianoScale: CGFloat = 1.6
    private let inset: CGFloat = 14
    private let gap: CGFloat = 8
    private let hintHeight: CGFloat = 20
    private let heldNotesRowHeight: CGFloat = 26
    private let chordCandidatesRowHeight: CGFloat = 30
    private let chordCandidatesRowHorizontalInset: CGFloat = 6
    private var widthConstraint: NSLayoutConstraint?
    private var waveformHeightConstraint: NSLayoutConstraint?
    private var isPresented = false
    private var trackingArea: NSTrackingArea?
    private static let panelCornerRadius: CGFloat = 18
    private static let sectionCornerRadius: CGFloat = 14
    private static let waveformClipCornerRadius: CGFloat = 12
    private static let expandedPanelWidth: CGFloat = 440

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        self.pianoView = PianoKeyboardView(menuBand: menuBand, pianoScale: pianoScale)
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
        instrumentReadout.translatesAutoresizingMaskIntoConstraints = false
        instrumentReadout.setContentHuggingPriority(.defaultHigh, for: .horizontal)
        instrumentReadout.setContentCompressionResistancePriority(.required, for: .horizontal)
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
        instrumentTitleRow.translatesAutoresizingMaskIntoConstraints = false
        hapticsControls.orientation = .horizontal
        hapticsControls.alignment = .centerY
        hapticsControls.spacing = 4
        hapticsControls.translatesAutoresizingMaskIntoConstraints = false
        hapticsControls.setContentHuggingPriority(.required, for: .horizontal)
        hapticsControls.setContentCompressionResistancePriority(.required, for: .horizontal)
        hapticsLabel.font = NSFont.systemFont(ofSize: 10, weight: .semibold)
        hapticsLabel.textColor = .secondaryLabelColor
        hapticsLabel.lineBreakMode = .byClipping
        hapticsSwitch.controlSize = .mini
        hapticsSwitch.target = self
        hapticsSwitch.action = #selector(hapticsSwitchChanged(_:))
        let infoConfig = NSImage.SymbolConfiguration(pointSize: 11, weight: .semibold)
        hapticsInfoButton.image = NSImage(
            systemSymbolName: "info.circle",
            accessibilityDescription: "About trackpad haptics"
        )?.withSymbolConfiguration(infoConfig)
        hapticsInfoButton.isBordered = false
        hapticsInfoButton.imagePosition = .imageOnly
        hapticsInfoButton.contentTintColor = .secondaryLabelColor
        hapticsInfoButton.toolTip = "Force Touch must be enabled in System Settings > Trackpad > Point & Click > Force Click and haptic feedback."
        hapticsInfoButton.target = self
        hapticsInfoButton.action = #selector(showHapticsInfo(_:))
        hapticsInfoButton.setButtonType(.momentaryPushIn)
        hapticsControls.addArrangedSubview(hapticsLabel)
        hapticsControls.addArrangedSubview(hapticsSwitch)
        hapticsControls.addArrangedSubview(hapticsInfoButton)
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
        instrumentTitleRow.addSubview(instrumentArrows)
        instrumentTitleRow.addSubview(instrumentReadout)
        instrumentTitleRow.addSubview(hapticsControls)
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
        updateHapticsControl()
        installLiquidGlassBackgrounds()

        let keyboardSize = self.keyboardSize()
        let widthConstraint = widthAnchor.constraint(
            equalToConstant: max(keyboardSize.width + inset * 2, Self.expandedPanelWidth)
        )
        self.widthConstraint = widthConstraint
        let waveformHeightConstraint = waveformView.heightAnchor.constraint(
            equalToConstant: waveformHeight(for: keyboardSize)
        )
        self.waveformHeightConstraint = waveformHeightConstraint
        let bezelInset: CGFloat = 5
        NSLayoutConstraint.activate([
            widthConstraint,

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
            chordCandidatesStack.leadingAnchor.constraint(greaterThanOrEqualTo: chordCandidatesRow.leadingAnchor, constant: chordCandidatesRowHorizontalInset),
            chordCandidatesStack.trailingAnchor.constraint(lessThanOrEqualTo: chordCandidatesRow.trailingAnchor, constant: -chordCandidatesRowHorizontalInset),
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

            instrumentArrows.leadingAnchor.constraint(equalTo: instrumentTitleRow.leadingAnchor, constant: 2),
            instrumentArrows.centerYAnchor.constraint(equalTo: instrumentTitleRow.centerYAnchor),
            hapticsControls.trailingAnchor.constraint(equalTo: instrumentTitleRow.trailingAnchor, constant: -2),
            hapticsControls.centerYAnchor.constraint(equalTo: instrumentTitleRow.centerYAnchor),
            instrumentReadout.centerXAnchor.constraint(equalTo: instrumentTitleRow.centerXAnchor),
            instrumentReadout.centerYAnchor.constraint(equalTo: instrumentTitleRow.centerYAnchor),
            instrumentReadout.topAnchor.constraint(greaterThanOrEqualTo: instrumentTitleRow.topAnchor),
            instrumentReadout.bottomAnchor.constraint(lessThanOrEqualTo: instrumentTitleRow.bottomAnchor),
            instrumentReadout.leadingAnchor.constraint(
                greaterThanOrEqualTo: instrumentArrows.trailingAnchor,
                constant: 6
            ),
            instrumentReadout.trailingAnchor.constraint(
                lessThanOrEqualTo: hapticsControls.leadingAnchor,
                constant: -6
            ),

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
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) {
        nil
    }

    private func installLiquidGlassBackgrounds() {
        guard Self.shouldUseLiquidGlass, #available(macOS 26.0, *) else { return }

        let paletteGlassView = ExpandedPianoWaveformGlassEffectView()
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
        let glassView = ExpandedPianoWaveformGlassEffectView()
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
        outlineBorderColor.setStroke()
        path.lineWidth = 1
        path.stroke()
    }

    func refresh() {
        updateShortcutHint()
        updateHapticsControl()
        let keyboardSize = keyboardSize()
        widthConstraint?.constant = max(keyboardSize.width + inset * 2, Self.expandedPanelWidth)
        waveformHeightConstraint?.constant = waveformHeight(for: keyboardSize)
        pianoView.refreshLayout()
        layoutSubtreeIfNeeded()
        applyAppearanceToVisualizer()
        refreshHeldNotes()
        updateInstrumentReadout()
        applyWaveformTint()
        updateWaveformLiveState(isPresented: isPresented)
        needsDisplay = true
        pianoView.needsDisplay = true
    }

    func clearInteraction() {
        pianoView.clearInteraction()
        lastCompleteChordNames = []
    }

    func setPresented(_ isPresented: Bool) {
        self.isPresented = isPresented
        if !isPresented {
            clearHeldNotes()
        }
        applyAppearanceToVisualizer()
        applyWaveformTint()
        updateWaveformLiveState(isPresented: isPresented)
    }

    private func updateWaveformLiveState(isPresented: Bool) {
        waveformView.isLive = isPresented && !(menuBand?.midiMode ?? false)
        waveformView.alphaValue = (menuBand?.midiMode ?? false) ? 0.35 : 1.0
    }

    private func updateHapticsControl() {
        hapticsSwitch.state = (menuBand?.hapticsEnabled ?? true) ? .on : .off
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
            outlineBorderColor = NSColor.controlAccentColor.withAlphaComponent(0.45)
        } else {
            waveformView.setDotMatrix(nil)
            let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
            let familyColor = InstrumentListView.colorForProgram(safe)
            waveformView.setBaseColor(familyColor)
            waveformSection.layer?.borderColor = familyColor
                .withAlphaComponent(0.22).cgColor
            outlineBorderColor = familyColor.withAlphaComponent(0.45)
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
        KeyboardIconRenderer.withPianoWaveformKeyboard(keymap: menuBand?.keymap) {
            let piano = KeyboardIconRenderer.pianoImageSize(layout: .tightActiveRange)
            return NSSize(width: piano.width * pianoScale, height: piano.height * pianoScale)
        }
    }

    private func waveformHeight(for keyboard: NSSize) -> CGFloat {
        keyboard.height * 1.25
    }

    private func refreshHeldNotes() {
        guard isPresented else {
            clearHeldNotes()
            return
        }
        guard let menuBand else { return }
        clearHeldNotes()
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
        let availableChordWidth = max(
            chordCandidatesRow.bounds.width - chordCandidatesRowHorizontalInset * 2,
            Self.expandedPanelWidth - inset * 2 - 20 - chordCandidatesRowHorizontalInset * 2
        )
        var consumedChordWidth: CGFloat = 0
        for candidate in candidates {
            let card = makeChordCandidateCard(candidate: candidate, color: familyColor)
            card.layoutSubtreeIfNeeded()
            let cardWidth = card.fittingSize.width
            let nextWidth = consumedChordWidth == 0
                ? cardWidth
                : consumedChordWidth + chordCandidatesStack.spacing + cardWidth
            guard consumedChordWidth == 0 || nextWidth <= availableChordWidth else { break }
            chordCandidatesStack.addArrangedSubview(card)
            consumedChordWidth = nextWidth
            if candidate.isComplete && justCompleted.contains(candidate.name) {
                applyShake(to: card)
            }
        }
        lastCompleteChordNames = newComplete
    }

    private func clearHeldNotes() {
        for view in heldNotesStack.arrangedSubviews {
            heldNotesStack.removeArrangedSubview(view)
            view.removeFromSuperview()
        }
        for view in chordCandidatesStack.arrangedSubviews {
            chordCandidatesStack.removeArrangedSubview(view)
            view.removeFromSuperview()
        }
        qwertyView.litKeyCodes = []
        lastCompleteChordNames = []
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
        instrumentArrows.accentColor = familyColor
        instrumentArrows.isDarkAppearance = isDark
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

    @objc private func hapticsSwitchChanged(_ sender: NSSwitch) {
        menuBand?.hapticsEnabled = (sender.state == .on)
    }

    @objc private func showHapticsInfo(_ sender: NSButton) {
        guard let window else { return }
        let alert = NSAlert()
        alert.alertStyle = .informational
        alert.messageText = "Trackpad Haptics"
        alert.informativeText = """
        Haptics use the Force Touch trackpad. If you don't feel feedback, open System Settings > Trackpad > Point & Click and turn on “Force Click and haptic feedback.”
        """
        alert.addButton(withTitle: "OK")
        alert.beginSheetModal(for: window)
    }

}

@available(macOS 26.0, *)
final class ExpandedPianoWaveformGlassEffectView: NSGlassEffectView {
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
}
