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
    private let outerStack = NSStackView()
    private let contentStack = NSStackView()
    /// GM instrument chooser — same view the popover uses, embedded
    /// here on the left of the floating panel so the panel pairs
    /// with the popover (popover on the right, chooser on the left
    /// of the floating window). Doubles as an audio-reactive
    /// LED-grid visualizer.
    private let instrumentList = InstrumentListView()
    private let waveformSection = NSView()
    private let waveformBezel = NSView()
    private let heldNotesStack = NSStackView()
    private let heldNotesRow = NSView()
    private let chordCandidatesStack = NSStackView()
    private let chordCandidatesRow = NSView()
    private var lastCompleteChordNames: Set<String> = []
    private let instrumentReadout = NSTextField(labelWithString: "")
    private let instrumentTitleRow: NSStackView
    private let pianoView: PianoKeyboardView
    private let shortcutHintRow = NSStackView()
    private let focusHintLabel = NSTextField(labelWithString: "")
    private let layoutHintLabel = NSTextField(labelWithString: "")
    private weak var paletteGlassView: NSView?
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

    private let pianoScale: CGFloat
    private let inset: CGFloat = 14
    private let gap: CGFloat = 8
    private let hintHeight: CGFloat = 20
    private let heldNotesRowHeight: CGFloat = 58
    private let chordCandidatesRowHeight: CGFloat = 30
    private let chordCandidatesRowHorizontalInset: CGFloat = 6
    private var isPresented = false
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
        // Scale so the piano spans the right column of the panel
        // exactly. Previously a fixed 1.6 scale made the keyboard
        // wider than the glass panel and forced the panel to grow.
        let basePianoWidth = KeyboardIconRenderer.withPianoWaveformKeyboard(keymap: menuBand.keymap) {
            KeyboardIconRenderer.pianoImageSize(layout: .tightActiveRange).width
        }
        let computedPianoScale = Self.expandedPanelWidth / max(1, basePianoWidth)
        self.pianoScale = computedPianoScale
        self.pianoView = PianoKeyboardView(menuBand: menuBand, pianoScale: computedPianoScale)
        super.init(frame: NSRect(origin: .zero, size: .zero))
        wantsLayer = true

        contentStack.orientation = .vertical
        contentStack.alignment = .centerX
        contentStack.distribution = .fill
        contentStack.spacing = gap
        contentStack.translatesAutoresizingMaskIntoConstraints = false

        // Outer horizontal split: instrument chooser on the left,
        // existing piano + held-notes / chord-cards / qwerty stack
        // on the right. Aligned to top so a tall chooser doesn't
        // float in the vertical middle while the right column
        // sits at the top.
        outerStack.orientation = .horizontal
        outerStack.alignment = .top
        outerStack.distribution = .fill
        outerStack.spacing = gap
        outerStack.translatesAutoresizingMaskIntoConstraints = false

        // Wire the chooser the same way the popover does — commit
        // sets the program (auto-flips out of MIDI mode), hover
        // previews the program, music keys passthrough so qwerty
        // still plays while the chooser holds focus.
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
        // Held-note pills and chord-suggestion cards live in the
        // bezel housing. The Metal visualizer that used to sit
        // underneath them was retired — the bezel is just the
        // pills+cards housing now.
        waveformBezel.layer?.masksToBounds = false
        heldNotesRow.wantsLayer = true
        heldNotesRow.layer?.masksToBounds = false
        chordCandidatesRow.wantsLayer = true
        chordCandidatesRow.layer?.masksToBounds = false
        waveformBezel.addSubview(heldNotesRow)
        waveformBezel.addSubview(chordCandidatesRow)
        waveformSection.addSubview(waveformBezel)
        waveformSection.addSubview(instrumentTitleRow)
        addSubview(outerStack)
        outerStack.addArrangedSubview(instrumentList)
        outerStack.addArrangedSubview(contentStack)
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
        // Bezel is now sized purely by its content (held-notes pills
        // + chord cards) — the Metal canvas it used to host is gone.
        let bezelInset: CGFloat = 5
        let titleSpacers = instrumentTitleRow.arrangedSubviews

        // Right column is fixed at the panel's intended width; the
        // keyboard scales (above) to fit it, never the other way
        // around — that keeps the keys visually inside the glass.
        _ = keyboardSize  // keep helper warm; sizing is column-driven now
        let rightColumnWidth = Self.expandedPanelWidth
        let totalWidth = InstrumentListView.preferredWidth + gap + rightColumnWidth

        NSLayoutConstraint.activate([
            widthAnchor.constraint(equalToConstant: totalWidth),

            outerStack.topAnchor.constraint(equalTo: topAnchor, constant: inset),
            outerStack.leadingAnchor.constraint(equalTo: leadingAnchor, constant: inset),
            outerStack.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -inset),
            outerStack.bottomAnchor.constraint(equalTo: bottomAnchor, constant: -inset),

            instrumentList.widthAnchor.constraint(equalToConstant: InstrumentListView.preferredWidth),
            instrumentList.heightAnchor.constraint(equalToConstant: InstrumentListView.preferredHeight),

            contentStack.widthAnchor.constraint(equalToConstant: rightColumnWidth),

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

            qwertyView.centerXAnchor.constraint(equalTo: contentStack.centerXAnchor),
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
        NSColor.separatorColor.withAlphaComponent(0.55).setStroke()
        path.lineWidth = 1
        path.stroke()
    }

    func refresh() {
        updateShortcutHint()
        pianoView.refreshLayout()
        layoutSubtreeIfNeeded()
        applyAppearanceToVisualizer()
        refreshHeldNotes()
        updateInstrumentReadout()
        applyWaveformTint()
        if let m = menuBand {
            // Follow the *effective* program so the bee-vision center
            // tracks hover-drag previews + arrow-key stepping live.
            instrumentList.selectedProgram = m.effectiveMelodicProgram
        }
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
    }

    private func applyAppearanceToVisualizer() {
        // Held-notes / chord housing is now transparent — let the
        // panel's glass material show through behind the pills +
        // chord cards instead of pasting a slate slab over it.
        // Border + tint still update via applyWaveformTint() so
        // the family color stays as a hairline frame.
        waveformSection.layer?.backgroundColor = NSColor.clear.cgColor
        waveformBezel.layer?.backgroundColor = NSColor.clear.cgColor
    }

    private func applyWaveformTint() {
        guard let menuBand else { return }
        // The visualizer is gone; this only retints the section
        // border so MIDI mode still reads as accent-colored chrome.
        if menuBand.midiMode {
            waveformSection.layer?.borderColor = NSColor.controlAccentColor
                .withAlphaComponent(0.24).cgColor
        } else {
            let safe = max(0, min(127, Int(menuBand.effectiveMelodicProgram)))
            let familyColor = InstrumentListView.colorForProgram(safe)
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
        KeyboardIconRenderer.withPianoWaveformKeyboard(keymap: menuBand?.keymap) {
            let piano = KeyboardIconRenderer.pianoImageSize(layout: .tightActiveRange)
            return NSSize(width: piano.width * pianoScale, height: piano.height * pianoScale)
        }
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
        let entries = menuBand.heldNoteEntries()
        for entry in entries {
            heldNotesStack.addArrangedSubview(
                makeHeldNoteBox(name: entry.pitchClass,
                                 key: entry.keyLabel,
                                 color: familyColor))
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

    private func makeHeldNoteBox(name: String, key: String?, color: NSColor) -> NSView {
        let box = NSView()
        box.wantsLayer = true
        box.layer?.cornerRadius = 6
        box.layer?.backgroundColor = color.withAlphaComponent(0.92).cgColor
        box.layer?.borderWidth = 1
        box.layer?.borderColor = color.shadow(withLevel: 0.35)?.cgColor ?? color.cgColor
        box.translatesAutoresizingMaskIntoConstraints = false

        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .centerX
        stack.spacing = 0
        stack.translatesAutoresizingMaskIntoConstraints = false

        // Top: keyboard key the user pressed. Smaller, tinted so it
        // reads as a label above the louder note name.
        if let key = key, !key.isEmpty {
            let keyLabel = NSTextField(labelWithString: key)
            keyLabel.font = NSFont.monospacedSystemFont(ofSize: 12, weight: .bold)
            keyLabel.textColor = NSColor.white.withAlphaComponent(0.85)
            keyLabel.drawsBackground = false
            keyLabel.alignment = .center
            stack.addArrangedSubview(keyLabel)
        }

        // Bottom: the bare pitch-class. Big + heavy so the played
        // note is the dominant element of each pill.
        let noteLabel = NSTextField(labelWithString: name)
        noteLabel.font = NSFont.monospacedSystemFont(ofSize: 22, weight: .heavy)
        noteLabel.textColor = .black
        noteLabel.drawsBackground = false
        noteLabel.alignment = .center
        stack.addArrangedSubview(noteLabel)

        box.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: box.leadingAnchor, constant: 8),
            stack.trailingAnchor.constraint(equalTo: box.trailingAnchor, constant: -8),
            stack.topAnchor.constraint(equalTo: box.topAnchor, constant: 3),
            stack.bottomAnchor.constraint(equalTo: box.bottomAnchor, constant: -3),
            box.heightAnchor.constraint(equalToConstant: 52),
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
        // MIDI mode replaces the GM voice name with a MIDI label so
        // the panel title matches the popover's "0 MIDI OUT" cue
        // instead of leaving a stale instrument name on screen.
        let title = menuBand.midiMode ? "MIDI" : GeneralMIDI.programNames[safe]
        let familyColor = menuBand.midiMode
            ? NSColor.controlAccentColor
            : InstrumentListView.colorForProgram(safe)
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

@available(macOS 26.0, *)
final class ExpandedPianoWaveformGlassEffectView: NSGlassEffectView {
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
}
