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
    private let instrumentGridContainer = NSView()
    /// Active program number + name, sitting right above the GM
    /// chooser grid. Uses YWFT Processing so the readout matches
    /// the popover's title typography. Color-keyed to the family.
    private let instrumentReadoutLabel = NSTextField(labelWithString: "")
    /// Dark rounded "pill" behind the readout for contrast. A SIBLING behind
    /// the label (not its parent) so the label never gets layer-backed — that
    /// would soften the 1px Riso-misregister shadow (see note in init).
    private let readoutBackground = NSView()
    /// QWERTY keymap visualization — moved out of the popover so
    /// the user can see which physical keys play which notes while
    /// the chooser is open. Lit cells reflect held keys.
    private let qwertyMap = QwertyLayoutView()
    /// Solid recessed "ground" behind the QWERTY map so the keymap
    /// reads as a seated plate rather than floating glyphs on the
    /// glass. Mirrors the instrument grid's housing fill + hairline.
    private let qwertyBackground = NSView()
    /// Four-arrow cluster below the chooser — preview while held,
    /// commit on release. Mirrors the popover's old arrows hint
    /// position (under the keyboard) one level down.
    private let arrowsCluster = ArrowKeysIndicator()
    /// Notepat / Ableton mode picker — moved out of the popover so
    /// the liquid panel reads as the operational/physical "extended
    /// instrument," and the popover stays a music-theory surface.
    private let modeStack = NSStackView()
    private var modeButtons: [NSButton] = []
    /// Live audio strip above the instrument name — shows the output
    /// waveform + reverse/forward direction so spacebar-rewind is debuggable.
    private let waveformStrip = WaveformStripView()
    /// Single accent-colored "Keymap" button under the instrument picker.
    /// Opens the full-screen liquid-glass keymap view (large piano + large
    /// QWERTY + the Notepat/Conventional mode toggle). The collapsed panel
    /// stays focused purely on the instrument picker.
    private let keymapButton = NSButton()
    private var trackingArea: NSTrackingArea?
    private weak var paletteGlassView: NSView?

    var onHoverChanged: ((Bool) -> Void)?
    var onStepBackward: (() -> Void)?
    var onStepForward: (() -> Void)?
    var onStepUp: (() -> Void)?
    var onStepDown: (() -> Void)?
    /// Fired when the Keymap button is clicked — opens the full-screen view.
    var onOpenKeymap: (() -> Void)?

    /// Light up an arrow cap as if the keyboard was pressing it.
    /// Direction indices match `ArrowKeysIndicator`:
    /// 0 = ←, 1 = →, 2 = ↓, 3 = ↑.
    func setArrowHighlight(direction: Int, on: Bool) {
        arrowsCluster.setHighlight(direction: direction, on: on)
    }

    private static let arrowsRowHeight: CGFloat = 34
    private static let modeRowHeight: CGFloat = 22
    private static let edgePadding: CGFloat = 6
    private static let gridPadding: CGFloat = 7
    private static let rowGap: CGFloat = 4
    /// Padding between the QWERTY map and its recessed ground plate.
    private static let qwertyGroundInset: CGFloat = 5
    private static let modeRowTopGap: CGFloat = 14
    /// Reserved at the top — hosts the chord-candidate row above
    /// the chooser. 30pt fits the FloatingChordCandidateCard's
    /// intrinsic height.
    private static let topInset: CGFloat = 6
    /// Reserved at the bottom so the arrows cluster doesn't sit
    /// under the bottom-leading fullscreen toggle button.
    private static let bottomInset: CGFloat = 12

    /// When true the cluster paints NO panel surface of its own (no
    /// liquid-glass backdrop, no opaque fallback fill) — it's being
    /// hosted inside another glass surface (the v1 popover) and a second
    /// backdrop would read as two stacked sheets. Only the grid's own
    /// recessed housing remains.
    private let embedded: Bool

    init(menuBand: MenuBandController, embedded: Bool = false) {
        self.menuBand = menuBand
        self.embedded = embedded
        super.init(frame: .zero)
        wantsLayer = true
        layer?.cornerRadius = 10
        layer?.masksToBounds = true

        contentContainer.translatesAutoresizingMaskIntoConstraints = false
        instrumentGridContainer.translatesAutoresizingMaskIntoConstraints = false
        instrumentGridContainer.wantsLayer = true
        instrumentGridContainer.layer?.cornerRadius = 10
        instrumentGridContainer.layer?.masksToBounds = true
        if #available(macOS 10.15, *) {
            instrumentGridContainer.layer?.cornerCurve = .continuous
        }

        qwertyBackground.translatesAutoresizingMaskIntoConstraints = false
        qwertyBackground.wantsLayer = true
        qwertyBackground.layer?.cornerRadius = 8
        qwertyBackground.layer?.masksToBounds = true
        if #available(macOS 10.15, *) {
            qwertyBackground.layer?.cornerCurve = .continuous
        }

        instrumentList.menuBand = menuBand
        instrumentList.translatesAutoresizingMaskIntoConstraints = false
        instrumentList.onCommit = { [weak self] prog in
            guard let self = self, let m = self.menuBand else { return }
            if m.midiMode { m.toggleMIDIMode() }
            m.setMelodicProgram(UInt8(prog))
            self.refresh()
        }
        // Slot 0 — "MIDI OUT" addressable cell at the top of the
        // grid. Toggles MIDI passthrough mode on the controller; the
        // refresh() call repaints the cell in its new state.
        instrumentList.onMidiOutCommit = { [weak self] in
            self?.menuBand?.toggleMIDIMode()
            self?.refresh()
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
        // Radio-station cells sit in the top row, left of MIDI OUT. Clicking
        // one tunes the radio ("voice −1") to that station and engages it.
        instrumentList.radioStations = RadioStation.all
        instrumentList.onRadioCommit = { [weak self] station in
            self?.menuBand?.selectRadioStation(station)
            self?.refresh()
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

        instrumentReadoutLabel.translatesAutoresizingMaskIntoConstraints = false
        instrumentReadoutLabel.alignment = .center
        instrumentReadoutLabel.maximumNumberOfLines = 1
        instrumentReadoutLabel.lineBreakMode = .byTruncatingTail
        instrumentReadoutLabel.drawsBackground = false
        instrumentReadoutLabel.isBordered = false
        instrumentReadoutLabel.isEditable = false
        instrumentReadoutLabel.isSelectable = false
        // Don't layer-back — a CA rasterization step softens the
        // 1px Riso-misregister shadow we apply in refresh().
        instrumentReadoutLabel.setContentHuggingPriority(.required, for: .vertical)
        instrumentReadoutLabel.setContentCompressionResistancePriority(.required, for: .horizontal)

        // Dark contrast pill behind the readout. Fill + border are themed in
        // refresh(); here just the shape.
        readoutBackground.translatesAutoresizingMaskIntoConstraints = false
        readoutBackground.wantsLayer = true
        readoutBackground.layer?.cornerRadius = 7
        readoutBackground.layer?.masksToBounds = true

        // Single accent "Keymap" button — opens the full-screen keymap
        // view. The QWERTY graphic + Notepat/Conventional mode toggle now
        // live there, so the collapsed panel is purely the instrument
        // picker + this one button.
        keymapButton.title = "Keymap"
        keymapButton.bezelStyle = .rounded
        keymapButton.isBordered = true
        keymapButton.bezelColor = .controlAccentColor
        keymapButton.controlSize = .small
        keymapButton.translatesAutoresizingMaskIntoConstraints = false
        keymapButton.target = self
        keymapButton.action = #selector(keymapButtonClicked(_:))
        keymapButton.attributedTitle = NSAttributedString(
            string: "Keymap",
            attributes: [
                .foregroundColor: NSColor.white,
                .font: NSFont.systemFont(ofSize: 11, weight: .semibold),
            ])
        keymapButton.toolTip = "Open the full-screen keymap (piano + QWERTY)"

        waveformStrip.menuBand = menuBand
        waveformStrip.translatesAutoresizingMaskIntoConstraints = false

        addSubview(contentContainer)
        contentContainer.addSubview(instrumentGridContainer)
        instrumentGridContainer.addSubview(instrumentList)
        contentContainer.addSubview(waveformStrip)
        contentContainer.addSubview(readoutBackground)   // behind the label
        contentContainer.addSubview(instrumentReadoutLabel)
        // [v1] Skip our own glass backdrop when embedded in the popover's
        // glass surface — otherwise the two stack into a doubled sheet.
        if !embedded { installLiquidGlassBackgrounds() }

        // Panel width is just the chooser grid's row now (no keyboard row).
        let totalWidth = Self.edgePadding + InstrumentListView.preferredWidth
            + Self.gridPadding * 2 + Self.edgePadding

        NSLayoutConstraint.activate([
            widthAnchor.constraint(equalToConstant: totalWidth),
            contentContainer.leadingAnchor.constraint(equalTo: leadingAnchor),
            contentContainer.trailingAnchor.constraint(equalTo: trailingAnchor),
            contentContainer.topAnchor.constraint(equalTo: topAnchor),
            contentContainer.bottomAnchor.constraint(equalTo: bottomAnchor),

            // Live waveform strip pinned at the very top of the picker.
            waveformStrip.topAnchor.constraint(
                equalTo: contentContainer.topAnchor, constant: Self.topInset),
            waveformStrip.centerXAnchor.constraint(equalTo: contentContainer.centerXAnchor),
            waveformStrip.widthAnchor.constraint(
                equalToConstant: InstrumentListView.preferredWidth),
            waveformStrip.heightAnchor.constraint(equalToConstant: 30),

            // Active-instrument readout below the strip.
            instrumentReadoutLabel.leadingAnchor.constraint(
                greaterThanOrEqualTo: contentContainer.leadingAnchor,
                constant: Self.edgePadding),
            instrumentReadoutLabel.trailingAnchor.constraint(
                lessThanOrEqualTo: contentContainer.trailingAnchor,
                constant: -Self.edgePadding),
            instrumentReadoutLabel.centerXAnchor.constraint(
                equalTo: contentContainer.centerXAnchor),
            instrumentReadoutLabel.topAnchor.constraint(
                equalTo: waveformStrip.bottomAnchor, constant: Self.rowGap),

            // Pill hugs the label with a little padding (h:10, v:4).
            readoutBackground.leadingAnchor.constraint(
                equalTo: instrumentReadoutLabel.leadingAnchor, constant: -10),
            readoutBackground.trailingAnchor.constraint(
                equalTo: instrumentReadoutLabel.trailingAnchor, constant: 10),
            readoutBackground.topAnchor.constraint(
                equalTo: instrumentReadoutLabel.topAnchor, constant: -4),
            readoutBackground.bottomAnchor.constraint(
                equalTo: instrumentReadoutLabel.bottomAnchor, constant: 4),

            // Instrument chooser grid below the readout.
            instrumentGridContainer.centerXAnchor.constraint(equalTo: contentContainer.centerXAnchor),
            instrumentGridContainer.topAnchor.constraint(equalTo: readoutBackground.bottomAnchor, constant: Self.rowGap),
            instrumentGridContainer.widthAnchor.constraint(
                equalToConstant: InstrumentListView.preferredWidth + Self.gridPadding * 2
            ),
            instrumentGridContainer.heightAnchor.constraint(
                equalToConstant: InstrumentListView.preferredHeight + Self.gridPadding * 2
            ),

            instrumentList.leadingAnchor.constraint(equalTo: instrumentGridContainer.leadingAnchor, constant: Self.gridPadding),
            instrumentList.trailingAnchor.constraint(equalTo: instrumentGridContainer.trailingAnchor, constant: -Self.gridPadding),
            instrumentList.topAnchor.constraint(equalTo: instrumentGridContainer.topAnchor, constant: Self.gridPadding),
            instrumentList.bottomAnchor.constraint(equalTo: instrumentGridContainer.bottomAnchor, constant: -Self.gridPadding),
            instrumentList.widthAnchor.constraint(equalToConstant: InstrumentListView.preferredWidth),
            instrumentList.heightAnchor.constraint(equalToConstant: InstrumentListView.preferredHeight),

            // Grid is the bottom element — the Keymap button now lives in
            // the popover footer alongside About / Quit.
            instrumentGridContainer.bottomAnchor.constraint(
                equalTo: contentContainer.bottomAnchor, constant: -Self.bottomInset),
        ])

        refresh()
    }

    @objc private func keymapButtonClicked(_ sender: NSButton) {
        onOpenKeymap?()
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
        instrumentList.midiModeActive = menuBand.midiMode
        instrumentList.radioBackendActive = (menuBand.instrumentBackend == .kpbj)
        instrumentList.selectedRadioStationID = menuBand.radioStation.id

        applyInstrumentReadout(safe: safe, familyColor: familyColor, isDark: isDark)

        arrowsCluster.accentColor = familyColor
        arrowsCluster.isDarkAppearance = isDark

        qwertyMap.keymap = menuBand.keymap
        qwertyMap.voiceColor = familyColor
        qwertyMap.litKeyCodes = menuBand.heldKeyCodes()
        // Forward the active octave shift so out-of-MIDI-range caps
        // render dimmed — visual cue that those keys won't sound.
        qwertyMap.octaveShift = menuBand.octaveShift

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
            instrumentGridContainer.layer?.backgroundColor =
                NSColor.black.withAlphaComponent(isDark ? 0.26 : 0.10).cgColor
            instrumentGridContainer.layer?.borderColor =
                NSColor.white.withAlphaComponent(isDark ? 0.16 : 0.22).cgColor
            instrumentGridContainer.layer?.borderWidth = 1.0
            qwertyBackground.layer?.backgroundColor =
                NSColor.black.withAlphaComponent(isDark ? 0.26 : 0.10).cgColor
            qwertyBackground.layer?.borderColor =
                NSColor.white.withAlphaComponent(isDark ? 0.16 : 0.22).cgColor
            qwertyBackground.layer?.borderWidth = 1.0
            layer?.backgroundColor = NSColor.clear.cgColor
        } else {
            instrumentGridContainer.layer?.backgroundColor =
                NSColor.windowBackgroundColor.withAlphaComponent(isDark ? 0.26 : 0.50).cgColor
            instrumentGridContainer.layer?.borderColor =
                NSColor.separatorColor.withAlphaComponent(0.45).cgColor
            instrumentGridContainer.layer?.borderWidth = 1.0
            qwertyBackground.layer?.backgroundColor =
                NSColor.windowBackgroundColor.withAlphaComponent(isDark ? 0.26 : 0.50).cgColor
            qwertyBackground.layer?.borderColor =
                NSColor.separatorColor.withAlphaComponent(0.45).cgColor
            qwertyBackground.layer?.borderWidth = 1.0
            layer?.backgroundColor = (isDark
                ? NSColor(white: 0.06, alpha: 0.96)
                : NSColor(white: 0.88, alpha: 0.96)).cgColor
        }
        // [v1] Embedded in the popover's glass — never paint our own panel
        // fill; let the single host surface show through. (The grid's
        // recessed housing above stays for legibility.)
        if embedded { layer?.backgroundColor = NSColor.clear.cgColor }
    }

    /// Mirror of `ExpandedPianoWaveformView.updateInstrumentReadout`
    /// — keep the two floating panels titled identically. Format:
    /// "078  Acoustic Grand Piano". Number + name in YWFT Processing
    /// with a 1-px family-colored Riso shadow under the title text.
    private func applyInstrumentReadout(safe: Int,
                                        familyColor: NSColor,
                                        isDark: Bool) {
        guard let menuBand else { return }
        let title: String
        let badgeColor: NSColor
        if menuBand.midiMode {
            title = "MIDI · MIDI OUT"
            badgeColor = .controlAccentColor
        } else {
            switch menuBand.instrumentBackend {
            case .sample:
                title = "`  Sample Voice"
                badgeColor = .systemRed
            case .kpbj:
                title = "RADIO  KPBJ.FM"
                badgeColor = .systemOrange
            case .garageBand:
                title = "GB  GarageBand"
                badgeColor = .systemPurple
            case .gm:
                title = String(format: "%03d  %@",
                               safe + 1,
                               GeneralMIDI.programName(safe))
                badgeColor = familyColor
            }
        }
        let titleFont: NSFont = {
            if let desc = AppDelegate.ywftBoldDescriptor,
               let font = NSFont(descriptor: desc, size: 16),
               font.familyName == "YWFT Processing" {
                return font
            }
            NSLog("MenuBand: YWFT bold descriptor unavailable; collapsed-panel readout falling back to system font")
            return NSFont.systemFont(ofSize: 16, weight: .black)
        }()
        // Always light text — the readout now sits on a dark contrast pill.
        let textColor: NSColor = .white
        let shadow = NSShadow()
        shadow.shadowColor = (badgeColor.highlight(withLevel: 0.4) ?? badgeColor)
        shadow.shadowOffset = NSSize(width: 1, height: -1)
        shadow.shadowBlurRadius = 0
        instrumentReadoutLabel.attributedStringValue = NSAttributedString(
            string: title,
            attributes: [
                .font: titleFont,
                .foregroundColor: textColor,
                .shadow: shadow,
            ]
        )
        instrumentReadoutLabel.toolTip = title
        // Dark pill + a 1px family-colored hairline so the readout keeps its
        // instrument identity while gaining contrast.
        readoutBackground.layer?.backgroundColor =
            NSColor.black.withAlphaComponent(isDark ? 0.62 : 0.78).cgColor
        readoutBackground.layer?.borderColor =
            badgeColor.withAlphaComponent(0.85).cgColor
        readoutBackground.layer?.borderWidth = 1.0
    }

    @objc private func whyKeymapClicked(_ sender: NSButton) {
        // Same fallback chain as the popover's whyKeymapButton —
        // bundled PDF first (offline-friendly), then the public URL.
        if let url = Bundle.appResources.url(
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
