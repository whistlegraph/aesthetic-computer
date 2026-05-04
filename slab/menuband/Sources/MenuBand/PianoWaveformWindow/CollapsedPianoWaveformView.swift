//
//  CollapsedPianoWaveformView.swift
//  MenuBand
//
//  Created by Esteban Uribe on 5/3/26.
//


import AppKit

final class CollapsedPianoWaveformView: NSView {
    private static var shouldUseLiquidGlass: Bool {
        PianoWaveformWindowStyle.shouldUseLiquidGlass
    }

    private weak var menuBand: MenuBandController?
    private let contentContainer = NSView()
    /// GM instrument chooser — embedded as the audio-reactive
    /// bee-vision picker in the collapsed floating panel. Held-note
    /// pills + chord-candidate cards live in the popover and
    /// expanded view only; the collapsed strip stays compact.
    private let instrumentList = InstrumentListView()
    /// QWERTY keymap visualization — moved out of the popover so
    /// the user can see which physical keys play which notes while
    /// the chooser is open. Lit cells reflect held keys.
    private let qwertyMap = QwertyLayoutView()
    /// Four-arrow cluster below the chooser — preview while held,
    /// commit on release. Mirrors the popover's old arrows hint
    /// position (under the keyboard) one level down.
    private let arrowsCluster = ArrowKeysIndicator()
    /// Notepat / Ableton mode picker — moved out of the popover so
    /// the liquid panel reads as the operational/physical "extended
    /// instrument," and the popover stays a music-theory surface.
    private let modeStack = NSStackView()
    private var modeButtons: [NSButton] = []
    /// Compact "About" row at the panel's bottom — Menu Band
    /// description + aesthetic.computer link. Moved out of the
    /// popover so the popover stays a music-theory surface.
    private let aboutBody = NSTextField(wrappingLabelWithString: "")
    private let aboutLinkButton = NSButton()
    private var trackingArea: NSTrackingArea?
    private weak var paletteGlassView: NSView?

    var onHoverChanged: ((Bool) -> Void)?
    var onStepBackward: (() -> Void)?
    var onStepForward: (() -> Void)?
    var onStepUp: (() -> Void)?
    var onStepDown: (() -> Void)?

    private static let arrowsRowHeight: CGFloat = 34
    private static let modeRowHeight: CGFloat = 22
    private static let aboutRowHeight: CGFloat = 36
    private static let edgePadding: CGFloat = 6
    private static let rowGap: CGFloat = 4
    /// Reserved at the top — hosts the chord-candidate row above
    /// the chooser. 30pt fits the FloatingChordCandidateCard's
    /// intrinsic height.
    private static let topInset: CGFloat = 6
    /// Reserved at the bottom so the arrows cluster doesn't sit
    /// under the bottom-leading fullscreen toggle button.
    private static let bottomInset: CGFloat = 28

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        super.init(frame: .zero)
        wantsLayer = true
        layer?.cornerRadius = 10
        layer?.masksToBounds = true

        contentContainer.translatesAutoresizingMaskIntoConstraints = false

        instrumentList.menuBand = menuBand
        instrumentList.translatesAutoresizingMaskIntoConstraints = false
        instrumentList.onCommit = { [weak self] prog in
            guard let self = self, let m = self.menuBand else { return }
            if m.midiMode { m.toggleMIDIMode() }
            m.setMelodicProgram(UInt8(prog))
            self.refresh()
        }
        instrumentList.onHover = { [weak self] prog in
            self?.menuBand?.setInstrumentPreview(prog.map { UInt8($0) })
            self?.refresh()
        }
        instrumentList.onMusicKey = { [weak self] kc, isDown, isRepeat, flags in
            return self?.menuBand?.handleLocalKey(
                keyCode: kc, isDown: isDown, isRepeat: isRepeat, flags: flags
            ) ?? false
        }

        qwertyMap.translatesAutoresizingMaskIntoConstraints = false
        qwertyMap.scale = 1.0
        qwertyMap.keymap = menuBand.keymap
        qwertyMap.onKey = { [weak self] kc, isDown in
            _ = self?.menuBand?.handleLocalKey(
                keyCode: kc, isDown: isDown, isRepeat: false, flags: []
            )
        }

        modeStack.orientation = .horizontal
        modeStack.alignment = .centerY
        modeStack.spacing = 6
        modeStack.translatesAutoresizingMaskIntoConstraints = false
        let modeSymbolConfig = NSImage.SymbolConfiguration(pointSize: 11, weight: .regular)
        let modeSpecs: [(label: String, image: NSImage?, tag: Int)] = [
            ("Notepat",
             NotepatFavicon.image
                ?? NSImage(systemSymbolName: "keyboard", accessibilityDescription: "Notepat")?
                    .withSymbolConfiguration(modeSymbolConfig),
             0),
            ("Ableton", AbletonLogo.image(height: 11), 1),
        ]
        for (idx, spec) in modeSpecs.enumerated() {
            let b = NSButton(title: spec.label, target: self,
                             action: #selector(modeButtonClicked(_:)))
            b.tag = spec.tag
            b.bezelStyle = .recessed
            b.setButtonType(.pushOnPushOff)
            b.controlSize = .small
            b.imagePosition = .imageLeading
            b.imageHugsTitle = true
            b.image = spec.image
            b.translatesAutoresizingMaskIntoConstraints = false
            modeButtons.append(b)
            modeStack.addArrangedSubview(b)
            // "?" help button immediately after the Notepat (idx 0)
            // button — opens the keymaps paper so the user can read
            // why notepat looks the way it does.
            if idx == 0 {
                let helpConfig = NSImage.SymbolConfiguration(pointSize: 10, weight: .semibold)
                let help = NSButton()
                help.image = NSImage(systemSymbolName: "questionmark.circle",
                                     accessibilityDescription: "Why this layout?")?
                    .withSymbolConfiguration(helpConfig)
                help.imagePosition = .imageOnly
                help.bezelStyle = .recessed
                help.controlSize = .small
                help.toolTip = "Why this layout?"
                help.target = self
                help.action = #selector(whyKeymapClicked(_:))
                help.translatesAutoresizingMaskIntoConstraints = false
                modeStack.addArrangedSubview(help)
            }
        }

        arrowsCluster.translatesAutoresizingMaskIntoConstraints = false
        arrowsCluster.displayMode = .cluster
        arrowsCluster.style = .prominent
        arrowsCluster.toolTip = "Step instruments — ←/→ one at a time, ↑/↓ by row"
        arrowsCluster.onClick = { [weak self] direction, isDown in
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

        // About row — bold "Menu Band" lead + secondary copy +
        // aesthetic.computer chip link. Replicates the popover's
        // about block in compact form.
        aboutBody.font = NSFont.systemFont(ofSize: 10)
        aboutBody.textColor = .secondaryLabelColor
        aboutBody.maximumNumberOfLines = 2
        aboutBody.lineBreakMode = .byTruncatingTail
        aboutBody.translatesAutoresizingMaskIntoConstraints = false
        let aboutText = NSMutableAttributedString()
        let bodyFont = NSFont.systemFont(ofSize: 10)
        let boldFont = NSFont.systemFont(ofSize: 10, weight: .bold)
        aboutText.append(NSAttributedString(
            string: "Menu Band",
            attributes: [.font: boldFont, .foregroundColor: NSColor.labelColor]))
        aboutText.append(NSAttributedString(
            string: " — your menubar piano, an instrument woven into ",
            attributes: [.font: bodyFont, .foregroundColor: NSColor.secondaryLabelColor]))
        aboutBody.attributedStringValue = aboutText

        let acPurple = NSColor(red: 167/255, green: 139/255, blue: 250/255, alpha: 1)
        let acTitle = NSAttributedString(
            string: "aesthetic.computer",
            attributes: [
                .font: NSFont.systemFont(ofSize: 10, weight: .semibold),
                .foregroundColor: acPurple,
            ])
        aboutLinkButton.attributedTitle = acTitle
        aboutLinkButton.bezelStyle = .recessed
        aboutLinkButton.controlSize = .small
        aboutLinkButton.translatesAutoresizingMaskIntoConstraints = false
        aboutLinkButton.target = self
        aboutLinkButton.action = #selector(openAestheticClicked(_:))
        aboutLinkButton.toolTip = "https://aesthetic.computer"

        addSubview(contentContainer)
        contentContainer.addSubview(instrumentList)
        contentContainer.addSubview(qwertyMap)
        contentContainer.addSubview(arrowsCluster)
        contentContainer.addSubview(modeStack)
        contentContainer.addSubview(aboutBody)
        contentContainer.addSubview(aboutLinkButton)
        installLiquidGlassBackgrounds()

        // Panel widens to fit either the chooser or the keyboard
        // row (qwerty + gap + arrows cluster), whichever is larger.
        let chooserRowWidth = Self.edgePadding + InstrumentListView.preferredWidth + Self.edgePadding
        let arrowsClusterWidth: CGFloat = 46
        let keyboardRowWidth = Self.edgePadding + QwertyLayoutView.intrinsicSize.width
            + Self.rowGap + arrowsClusterWidth + Self.edgePadding
        let totalWidth = max(chooserRowWidth, keyboardRowWidth)

        NSLayoutConstraint.activate([
            widthAnchor.constraint(equalToConstant: totalWidth),
            contentContainer.leadingAnchor.constraint(equalTo: leadingAnchor),
            contentContainer.trailingAnchor.constraint(equalTo: trailingAnchor),
            contentContainer.topAnchor.constraint(equalTo: topAnchor),
            contentContainer.bottomAnchor.constraint(equalTo: bottomAnchor),

            instrumentList.leadingAnchor.constraint(equalTo: contentContainer.leadingAnchor, constant: Self.edgePadding),
            instrumentList.topAnchor.constraint(equalTo: contentContainer.topAnchor, constant: Self.topInset),
            instrumentList.widthAnchor.constraint(equalToConstant: InstrumentListView.preferredWidth),
            instrumentList.heightAnchor.constraint(equalToConstant: InstrumentListView.preferredHeight),

            // Qwerty + arrows row centered horizontally in the panel
            // — looked too left-shoved when flush against the leading
            // edge. The cluster (qwerty + gap + arrows) is treated
            // as one unit so it sits balanced under the chooser.
            qwertyMap.topAnchor.constraint(equalTo: instrumentList.bottomAnchor, constant: Self.rowGap),
            qwertyMap.widthAnchor.constraint(equalToConstant: QwertyLayoutView.intrinsicSize.width),
            qwertyMap.heightAnchor.constraint(equalToConstant: QwertyLayoutView.intrinsicSize.height),
            qwertyMap.leadingAnchor.constraint(
                equalTo: contentContainer.leadingAnchor,
                constant: (totalWidth - QwertyLayoutView.intrinsicSize.width - Self.rowGap - arrowsClusterWidth) / 2
            ),

            arrowsCluster.leadingAnchor.constraint(equalTo: qwertyMap.trailingAnchor, constant: Self.rowGap),
            arrowsCluster.bottomAnchor.constraint(equalTo: qwertyMap.bottomAnchor),
            arrowsCluster.heightAnchor.constraint(equalToConstant: Self.arrowsRowHeight),

            // Mode picker (Notepat / Ableton) sits below the qwerty
            // row. Centered horizontally; the about row beneath it
            // pads the panel's bottom-leading fullscreen toggle.
            modeStack.topAnchor.constraint(equalTo: qwertyMap.bottomAnchor, constant: Self.rowGap),
            modeStack.centerXAnchor.constraint(equalTo: contentContainer.centerXAnchor),
            modeStack.heightAnchor.constraint(equalToConstant: Self.modeRowHeight),

            // About row — wrapped Menu Band description on one line,
            // aesthetic.computer link on the next. Pinned at the
            // bottom inset so the fullscreen button stays visible
            // bottom-leading.
            aboutBody.leadingAnchor.constraint(equalTo: contentContainer.leadingAnchor, constant: Self.edgePadding + 32),
            aboutBody.trailingAnchor.constraint(equalTo: contentContainer.trailingAnchor, constant: -Self.edgePadding),
            aboutBody.topAnchor.constraint(equalTo: modeStack.bottomAnchor, constant: Self.rowGap),

            aboutLinkButton.leadingAnchor.constraint(equalTo: contentContainer.leadingAnchor, constant: Self.edgePadding + 32),
            aboutLinkButton.topAnchor.constraint(equalTo: aboutBody.bottomAnchor, constant: 2),
            aboutLinkButton.bottomAnchor.constraint(equalTo: contentContainer.bottomAnchor, constant: -Self.edgePadding),
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

        let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
        let familyColor = menuBand.midiMode
            ? NSColor.controlAccentColor
            : InstrumentListView.colorForProgram(safe)

        // Follow the *effective* program (preview ?? committed) so the
        // bee-vision typographic center shifts live during hover-drag
        // through the grid and during arrow-key stepping. Without this
        // the giant selected number stays anchored to the committed
        // voice while the preview note plays a different program.
        instrumentList.selectedProgram = menuBand.effectiveMelodicProgram

        arrowsCluster.accentColor = familyColor
        arrowsCluster.isDarkAppearance = isDark

        qwertyMap.keymap = menuBand.keymap
        qwertyMap.voiceColor = familyColor
        qwertyMap.litKeyCodes = menuBand.heldKeyCodes()

        // Mirror the keymap selection on the mode-picker buttons.
        let activeTag = (menuBand.keymap == .ableton) ? 1 : 0
        for button in modeButtons {
            button.state = (button.tag == activeTag) ? .on : .off
        }

        if Self.shouldUseLiquidGlass, #available(macOS 26.0, *) {
            // Tinting is disabled for the collapsed view — even a
            // normalized hue tint perceptibly shifted the glass blur
            // between voices. Leaving NSGlassEffectView at its
            // default style keeps the panel visually identical from
            // open to close, no matter which instrument is active.
            (paletteGlassView as? NSGlassEffectView)?.style = .clear
            layer?.backgroundColor = NSColor.clear.cgColor
        } else {
            layer?.backgroundColor = (isDark
                ? NSColor(white: 0.06, alpha: 0.96)
                : NSColor(white: 0.88, alpha: 0.96)).cgColor
        }
    }

    @objc private func openAestheticClicked(_ sender: NSButton) {
        if let url = URL(string: "https://aesthetic.computer") {
            NSWorkspace.shared.open(url)
        }
    }

    @objc private func whyKeymapClicked(_ sender: NSButton) {
        // Same fallback chain as the popover's whyKeymapButton —
        // bundled PDF first (offline-friendly), then the public URL.
        if let url = Bundle.module.url(
            forResource: "keymaps-social-software-26-arxiv",
            withExtension: "pdf")
        {
            NSWorkspace.shared.open(url)
            return
        }
        if let url = URL(string:
            "https://papers.aesthetic.computer/keymaps-social-software-26-arxiv.pdf") {
            NSWorkspace.shared.open(url)
        }
    }

    @objc private func modeButtonClicked(_ sender: NSButton) {
        guard let menuBand else { return }
        let next: Keymap = (sender.tag == 1) ? .ableton : .notepat
        if menuBand.keymap != next {
            menuBand.keymap = next
        }
        // Manual radio behaviour: only the clicked button stays .on.
        for button in modeButtons {
            button.state = (button == sender) ? .on : .off
        }
    }

    private func installLiquidGlassBackgrounds() {
        guard Self.shouldUseLiquidGlass, #available(macOS 26.0, *) else { return }

        let paletteGlassView = CollapsedPianoWaveformGlassEffectView()
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
    }
}

@available(macOS 26.0, *)
private final class CollapsedPianoWaveformGlassEffectView: NSGlassEffectView {
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
}
