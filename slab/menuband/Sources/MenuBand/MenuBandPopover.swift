import AppKit

/// NSSegmentedControl subclass that reports the currently-hovered segment
/// (or nil when the cursor leaves). Used by the input-mode picker so
/// hovering a segment can preview that mode in the menubar piano without
/// committing the click. Equal-width segments are assumed.
final class HoverSegmentedControl: NSSegmentedControl {
    var onHoverChange: ((Int?) -> Void)?
    private var trackingArea: NSTrackingArea?
    private var lastHovered: Int?

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let ta = trackingArea { removeTrackingArea(ta) }
        let ta = NSTrackingArea(
            rect: bounds,
            options: [.mouseEnteredAndExited, .mouseMoved,
                      .activeAlways, .inVisibleRect],
            owner: self, userInfo: nil
        )
        addTrackingArea(ta)
        trackingArea = ta
    }

    private func segment(at point: NSPoint) -> Int? {
        let count = segmentCount
        guard count > 0, bounds.contains(point) else { return nil }
        let w = bounds.width / CGFloat(count)
        let idx = Int(point.x / w)
        return max(0, min(count - 1, idx))
    }

    private func report(at point: NSPoint) {
        let seg = segment(at: point)
        if seg != lastHovered {
            lastHovered = seg
            onHoverChange?(seg)
        }
    }

    override func mouseEntered(with event: NSEvent) {
        report(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseMoved(with event: NSEvent) {
        report(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseExited(with event: NSEvent) {
        if lastHovered != nil {
            lastHovered = nil
            onHoverChange?(nil)
        }
    }
}

/// Settings popover for the menubar piano. Custom NSViewController with
/// native AppKit controls (NSSwitch / NSPopUpButton / NSStepper) for a
/// richer feel than a plain NSMenu.
final class MenuBandPopoverViewController: NSViewController {
    weak var menuBand: MenuBandController?
    /// Owning popover, set by AppDelegate after construction. Held weak so
    /// we don't extend its lifetime; used to animate `contentSize` when the
    /// instrument palette collapses / expands.
    weak var popover: NSPopover?

    private var inputSegmented: HoverSegmentedControl!  // legacy reference; no longer added to stack
    private var modeButtons: [NSButton] = []           // vertical stack: Mouse Only / Notepat.com / Ableton MIDI Keys
    private var midiSwitch: NSSwitch!
    private var midiInlineLabel: NSTextField!
    private var midiSelfTestLabel: NSTextField!  // legacy — created but never added to stack
    private var instrumentList: InstrumentListView!
    private var instrumentReadout: NSTextField!
    private var instrumentLabel: NSTextField!
    private var instrumentTitleRow: NSStackView!
    private var arrowsHint: ArrowKeysIndicator!
    private var qwertyMap: QwertyLayoutView!
    private var keyboardDeck: NSView!
    /// Horizontal stack of small floating boxes, one per currently-
    /// held note. Sits above the visualizer; empty (zero-height) at
    /// rest so the layout doesn't wobble when notes come and go.
    private var heldNotesStack: NSStackView!
    private var heldNotesContainer: NSView!
    private var instrumentSeparator: NSView!
    private var octaveStepper: NSStepper!
    private var octaveLabel: NSTextField!
    private var crashStatusLabel: NSTextField!
    private var crashHintLabel: NSTextField!
    private var crashSendButton: NSButton!
    private var updateBanner: NSView!
    private var updateLabel: NSTextField!
    private var waveformView: WaveformView!
    private var waveformBezel: NSView!

    override func loadView() {
        // Plain solid-color background — no NSVisualEffectView. The visual
        // effect view sampled the surrounding context and shifted appearance
        // when focus moved between the menu bar and the popover. A flat
        // background keeps the popover homogeneous in all states.
        let root = NSView()
        root.wantsLayer = true
        root.layer?.backgroundColor = NSColor.windowBackgroundColor.cgColor
        root.translatesAutoresizingMaskIntoConstraints = false

        // Vertical stack of rows. Tight spacing + small edge insets so the
        // popover hugs the 224 px instrument grid with no slack.
        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 6
        stack.edgeInsets = NSEdgeInsets(top: 8, left: 8, bottom: 8, right: 8)
        stack.translatesAutoresizingMaskIntoConstraints = false
        root.addSubview(stack)

        // Pin the stack to exactly the instrument-grid width plus the
        // 8 px insets on each side. Without this, NSSegmentedControl's
        // intrinsic-content-size for "Notepat.com" pushes the stack wider
        // than 224, which then drags the popover out with it.
        stack.widthAnchor.constraint(
            equalToConstant: InstrumentListView.preferredWidth + 16
        ).isActive = true

        // Top control row: octave hugs the left, MIDI hugs the right. Brand
        // title moved into the About section below — fewer wasted rows up top.
        let titleRow = NSStackView()
        titleRow.orientation = .horizontal
        titleRow.alignment = .centerY
        titleRow.distribution = .fill
        titleRow.spacing = 8

        let titleSpacer = NSView()
        titleSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        // Spacer is added BETWEEN the octave trio and the MIDI pair below
        // so the octave widget pins to the row's leading edge and MIDI
        // floats to the trailing edge — no longer center-clustered.

        // Compact octave: large monospaced number, then a tight pair of
        // chevron arrows to its right. NSStepper kept invisibly as the
        // value model so the rest of the controller's API doesn't change;
        // the visible buttons just nudge its `integerValue`.
        octaveLabel = NSTextField(labelWithString: "4")
        // Fully monospaced so "+", "-", and digits all carry the same
        // advance — value flips don't slide left or right between the
        // arrows. No fixed width — intrinsic content width hugs the text
        // tightly so the 4 px spacing on each side stays exactly 4 px,
        // not "4 + half-of-padding".
        octaveLabel.font = NSFont.monospacedSystemFont(ofSize: 17, weight: .semibold)
        octaveLabel.textColor = .controlAccentColor
        octaveLabel.alignment = .center

        octaveStepper = NSStepper()
        octaveStepper.minValue = -4
        octaveStepper.maxValue = 4
        octaveStepper.increment = 1
        octaveStepper.valueWraps = false
        octaveStepper.target = self
        octaveStepper.action = #selector(octaveChanged(_:))
        octaveStepper.isHidden = true   // value model only — UI is the arrows below

        // Chevrons sized to balance the 17 pt label between them.
        let chevConfig = NSImage.SymbolConfiguration(pointSize: 12, weight: .semibold)

        let leftArrow = NSButton()
        leftArrow.image = NSImage(systemSymbolName: "chevron.left",
                                  accessibilityDescription: "Octave down")?
            .withSymbolConfiguration(chevConfig)
        leftArrow.isBordered = false
        leftArrow.controlSize = .small
        leftArrow.imagePosition = .imageOnly
        leftArrow.contentTintColor = .secondaryLabelColor
        leftArrow.target = self
        leftArrow.action = #selector(octaveDown)

        let rightArrow = NSButton()
        rightArrow.image = NSImage(systemSymbolName: "chevron.right",
                                   accessibilityDescription: "Octave up")?
            .withSymbolConfiguration(chevConfig)
        rightArrow.isBordered = false
        rightArrow.controlSize = .small
        rightArrow.imagePosition = .imageOnly
        rightArrow.contentTintColor = .secondaryLabelColor
        rightArrow.target = self
        rightArrow.action = #selector(octaveUp)

        // Octave trio first — left arrow, big number, right arrow — pinned
        // to the row's leading edge with a tight 4 px gap on each side so
        // the trio reads as a single widget instead of three buttons. A tiny
        // "octave" hint label sits to the right of the rightArrow so the
        // number reads as scientific pitch notation (C4, C5, …) without
        // taking much room.
        let octaveHint = NSTextField(labelWithString: "Octave")
        octaveHint.font = NSFont.systemFont(ofSize: 9, weight: .regular)
        octaveHint.textColor = .tertiaryLabelColor

        titleRow.addArrangedSubview(leftArrow)
        titleRow.setCustomSpacing(4, after: leftArrow)
        titleRow.addArrangedSubview(octaveLabel)
        titleRow.setCustomSpacing(4, after: octaveLabel)
        titleRow.addArrangedSubview(rightArrow)
        titleRow.addArrangedSubview(octaveStepper)  // hidden, here for layout-time only
        titleRow.setCustomSpacing(8, after: rightArrow)
        titleRow.addArrangedSubview(octaveHint)

        // Spacer lives in the middle so the octave widget pins LEFT and
        // the MIDI pair pins RIGHT.
        titleRow.addArrangedSubview(titleSpacer)

        // MIDI toggle — tucked into the title row instead of its own panel.
        // Enabling MIDI also silences the local keyboard (notes route to the
        // DAW instead), so a separate mute button would be redundant.
        midiSwitch = NSSwitch()
        midiSwitch.target = self
        midiSwitch.action = #selector(midiSwitchToggled(_:))
        midiInlineLabel = NSTextField(labelWithString: "MIDI")
        midiInlineLabel.font = NSFont.systemFont(ofSize: 10, weight: .semibold)
        midiInlineLabel.textColor = .secondaryLabelColor
        titleRow.addArrangedSubview(midiInlineLabel)
        titleRow.setCustomSpacing(4, after: midiInlineLabel)
        titleRow.addArrangedSubview(midiSwitch)

        stack.addArrangedSubview(titleRow)
        titleRow.widthAnchor.constraint(equalTo: stack.widthAnchor,
                                         constant: -16).isActive = true

        // Update banner — hidden until UpdateChecker reports a newer
        // release. Tinted accent so the user notices it without it feeling
        // like an alert.
        updateBanner = NSView()
        updateBanner.wantsLayer = true
        updateBanner.layer?.backgroundColor = NSColor.controlAccentColor
            .withAlphaComponent(0.14).cgColor
        updateBanner.layer?.cornerRadius = 6
        updateBanner.translatesAutoresizingMaskIntoConstraints = false
        updateLabel = NSTextField(labelWithString: "")
        updateLabel.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        updateLabel.textColor = .labelColor
        updateLabel.lineBreakMode = .byWordWrapping
        updateLabel.maximumNumberOfLines = 0
        updateLabel.translatesAutoresizingMaskIntoConstraints = false
        let updateLink = NSButton(title: "Open menuband.com",
                                  target: self,
                                  action: #selector(openMenuBandSite))
        updateLink.bezelStyle = .recessed
        updateLink.controlSize = .small
        updateLink.translatesAutoresizingMaskIntoConstraints = false
        updateBanner.addSubview(updateLabel)
        updateBanner.addSubview(updateLink)
        NSLayoutConstraint.activate([
            updateLabel.leadingAnchor.constraint(equalTo: updateBanner.leadingAnchor, constant: 10),
            updateLabel.topAnchor.constraint(equalTo: updateBanner.topAnchor, constant: 7),
            updateLabel.trailingAnchor.constraint(equalTo: updateBanner.trailingAnchor, constant: -10),
            updateLink.leadingAnchor.constraint(equalTo: updateBanner.leadingAnchor, constant: 10),
            updateLink.topAnchor.constraint(equalTo: updateLabel.bottomAnchor, constant: 4),
            updateLink.bottomAnchor.constraint(equalTo: updateBanner.bottomAnchor, constant: -7),
        ])
        stack.addArrangedSubview(updateBanner)
        updateBanner.widthAnchor.constraint(equalToConstant: InstrumentListView.preferredWidth).isActive = true
        updateBanner.isHidden = true

        stack.addArrangedSubview(makeSeparator())

        // Input mode picker. Three states:
        //   Pointer      — mouse only, two octaves (Notepat range)
        //   Notepat.com  — global keystroke capture, two octaves
        //   Ableton      — global keystroke capture, one octave (Live's M-mode)
        // Hovering a segment previews that mode in the menubar piano (range
        // shrinks/grows, letter labels appear) and lets you tap keys for a
        // quick demo without committing.
        // Layout block — built here, but appended to the stack
        // *below* the palettePanel so the Layout choice reads as a
        // configuration knob you reach for after picking a voice.
        let inputLabel = NSTextField(labelWithString: "Layout")
        inputLabel.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        inputLabel.textColor = .labelColor

        // Vertical mode buttons — full labels fit without truncation, each
        // button is the full content width with an SF Symbol leading the
        // text so the mode is recognizable at a glance.
        let modeSymbolConfig = NSImage.SymbolConfiguration(pointSize: 13,
                                                           weight: .semibold)
        // "Mouse Only" was the way to disable global keyboard capture; with
        // local capture (click menubar piano → type to play, no Accessibility
        // needed) that mode is handled implicitly by simply not triggering
        // global capture. The popover now just picks the keymap layout.
        // Custom branding: Notepat.com uses the live favicon
        // (`NotepatFavicon.image`, lazily fetched + cached); Ableton
        // gets the canonical logo we render programmatically.
        let modeSpecs: [(label: String, image: NSImage?)] = [
            ("Notepat.com",
             NotepatFavicon.image
                ?? NSImage(systemSymbolName: "keyboard",
                            accessibilityDescription: "Notepat.com")?
                    .withSymbolConfiguration(modeSymbolConfig)),
            ("Ableton Computer Keyboard",
             AbletonLogo.image(height: 11)),
        ]
        modeButtons = []
        let modeStack = NSStackView()
        modeStack.orientation = .vertical
        modeStack.alignment = .leading
        modeStack.spacing = 2
        modeStack.translatesAutoresizingMaskIntoConstraints = false
        for (idx, spec) in modeSpecs.enumerated() {
            let b = NSButton(title: spec.label, target: self,
                             action: #selector(modeButtonClicked(_:)))
            b.tag = idx
            b.bezelStyle = .recessed
            b.setButtonType(.pushOnPushOff)
            b.controlSize = .regular
            b.alignment = .left
            b.imagePosition = .imageLeading
            b.imageHugsTitle = true
            b.image = spec.image
            b.translatesAutoresizingMaskIntoConstraints = false
            b.widthAnchor.constraint(
                equalToConstant: InstrumentListView.preferredWidth
            ).isActive = true
            modeButtons.append(b)
            modeStack.addArrangedSubview(b)
        }
        modeStack.widthAnchor.constraint(
            equalToConstant: InstrumentListView.preferredWidth
        ).isActive = true

        let inputHint = NSTextField(labelWithString:
            "⌃⌥⌘P toggles last keystrokes mode")
        inputHint.font = NSFont.systemFont(ofSize: 10)
        inputHint.textColor = .secondaryLabelColor
        // Layout label / mode buttons / hint are inserted into the
        // popover stack farther down — see the `palettePanel`
        // insertion point.
        let layoutBlock = (label: inputLabel, picker: modeStack, hint: inputHint)

        // Live segmented LED meter of the local synth output. Hidden in
        // MIDI mode (DAW handles audio there; our local mixer is silent
        // so the bars would just sit dark).
        //
        // Wrapped in a layer-backed bezel so the meter reads as a
        // proper VU display housing — dark recessed background, soft
        // border, uniform inner margin around the bars. Without the
        // bezel the bars sit flush against the popover walls and feel
        // unfinished.
        waveformView = WaveformView()
        waveformView.menuBand = menuBand
        waveformView.translatesAutoresizingMaskIntoConstraints = false

        waveformBezel = NSView()
        waveformBezel.wantsLayer = true
        waveformBezel.layer?.cornerRadius = 6
        // Bezel substrate color is set per-appearance in
        // `applyAppearanceToVisualizer` (called from syncFromController);
        // start dark so first-paint before sync isn't a flash.
        waveformBezel.layer?.backgroundColor = NSColor(white: 0.06, alpha: 1.0).cgColor
        waveformBezel.layer?.borderWidth = 1
        // Border color is set in `updateInstrumentReadout` so the
        // housing tracks the chosen voice's family hue.
        waveformBezel.translatesAutoresizingMaskIntoConstraints = false
        waveformBezel.addSubview(waveformView)
        let bezelInset: CGFloat = 5
        NSLayoutConstraint.activate([
            waveformView.leadingAnchor.constraint(equalTo: waveformBezel.leadingAnchor, constant: bezelInset),
            waveformView.trailingAnchor.constraint(equalTo: waveformBezel.trailingAnchor, constant: -bezelInset),
            waveformView.topAnchor.constraint(equalTo: waveformBezel.topAnchor, constant: bezelInset),
            waveformView.bottomAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: -bezelInset),
        ])
        // Held-notes goes ABOVE the visualizer so the actively-
        // sounding pitches read like a label on the meter housing.
        // Build the floating-boxes container HERE so the ivar is
        // populated before we add it to the stack.
        heldNotesStack = NSStackView()
        heldNotesStack.orientation = .horizontal
        heldNotesStack.alignment = .centerY
        heldNotesStack.spacing = 4
        heldNotesStack.translatesAutoresizingMaskIntoConstraints = false
        heldNotesContainer = NSView()
        heldNotesContainer.translatesAutoresizingMaskIntoConstraints = false
        heldNotesContainer.addSubview(heldNotesStack)
        NSLayoutConstraint.activate([
            heldNotesStack.centerXAnchor.constraint(equalTo: heldNotesContainer.centerXAnchor),
            heldNotesStack.centerYAnchor.constraint(equalTo: heldNotesContainer.centerYAnchor),
        ])
        stack.addArrangedSubview(heldNotesContainer)
        heldNotesContainer.widthAnchor.constraint(equalToConstant: InstrumentListView.preferredWidth).isActive = true
        heldNotesContainer.heightAnchor.constraint(equalToConstant: 22).isActive = true

        stack.addArrangedSubview(waveformBezel)
        waveformBezel.widthAnchor.constraint(equalToConstant: InstrumentListView.preferredWidth).isActive = true
        waveformBezel.heightAnchor.constraint(equalToConstant: 64).isActive = true

        // (MIDI switch lives in the title row above — see octave + MIDI block.)

        // MIDI self-test status — populated by the controller after each
        // toggle-on. Empty when MIDI is off.
        // Legacy textual self-test label is allocated but never added to
        // the popover stack — the test outcome is surfaced as the MIDI
        // label color in the title row instead (green = ok, red = failed).
        midiSelfTestLabel = NSTextField(labelWithString: "")

        // No divider above the Voice row — the visualizer's lit
        // bezel below is enough visual separation. Allocate the ivar
        // anyway so the dim/animation hooks below don't crash; just
        // never add it to the stack.
        instrumentSeparator = makeSeparator()
        instrumentSeparator.isHidden = true

        // Instrument named-list. All 128 GM programs in a scrollable list,
        // family-colored stripe on the left, name on the right. Hover plays
        // a preview note; click commits. Compact (~180 px window) so the
        // popover stays small even though the full list is much taller.
        // Hidden in MIDI mode — the DAW picks instruments there, so this
        // local picker would just be misleading dead UI.
        //
        // Title row: "Instrument:" left, "078  Whistle" right (greyed). The
        // readout used to live under the grid; promoting it to the title
        // row keeps the eye on a single line while browsing cells.
        // No leading "Voice:" label — the family-tinted chip itself
        // *is* the voice title now. Bigger, bolder, sits like a
        // chapter heading under the LED meter.
        instrumentLabel = NSTextField(labelWithString: "")
        instrumentLabel.isHidden = true
        instrumentReadout = NSTextField(labelWithString: "")
        // Big, bold, family-colored title with a soft drop shadow —
        // no chip backdrop. Text color carries the GM family hue
        // (set/refreshed in updateInstrumentReadout); the shadow gives
        // it presence without the rectangular bg framing it had before.
        instrumentReadout.font = NSFont.systemFont(ofSize: 18, weight: .black)
        instrumentReadout.textColor = .labelColor
        instrumentReadout.drawsBackground = false
        // No `wantsLayer = true` here — layer-backing this label adds
        // a CA rasterization step that can soften pixel-aligned glyph
        // edges and the hard 1-px shadow we set in the attributed
        // string. Plain non-layered drawing keeps the shadow crisp.
        instrumentReadout.lineBreakMode = .byTruncatingTail
        instrumentReadout.alignment = .center
        // No view-level shadow — the per-glyph shadow lives in the
        // attributed string (set in updateInstrumentReadout) so it
        // stays crisp. A view shadow on top of that double-shadows
        // the text and looks like a soft halo.
        // Center the chip in its row by flanking it with greedy
        // spacers — without these, .fill distribution lets the
        // (now-shrunken) text hug the leading edge.
        let titleLeftSpacer = NSView()
        let titleRightSpacer = NSView()
        titleLeftSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        titleRightSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        instrumentTitleRow = NSStackView(views: [titleLeftSpacer, instrumentReadout, titleRightSpacer])
        instrumentTitleRow.orientation = .horizontal
        instrumentTitleRow.alignment = .centerY
        instrumentTitleRow.distribution = .fill
        instrumentTitleRow.spacing = 0
        instrumentLabel.setContentHuggingPriority(.defaultHigh, for: .horizontal)
        instrumentReadout.setContentHuggingPriority(.defaultHigh, for: .horizontal)
        instrumentReadout.setContentCompressionResistancePriority(.required, for: .horizontal)
        titleLeftSpacer.widthAnchor.constraint(equalTo: titleRightSpacer.widthAnchor).isActive = true
        stack.addArrangedSubview(instrumentTitleRow)

        // (held-notes floating-boxes container is built ABOVE the
        // visualizer — see the block before `waveformBezel`.)

        // GarageBand backend toggle was prototyped here (see
        // GarageBandLibrary + GarageBandPatchView), then deprecated
        // pending UX polish. Source files retained for future revival;
        // the popover currently exposes only the General MIDI grid.

        instrumentList = InstrumentListView()
        instrumentList.translatesAutoresizingMaskIntoConstraints = false
        instrumentList.onCommit = { [weak self] prog in
            self?.handleInstrumentCommit(prog)
        }
        instrumentList.onArrowKey = { [weak self] dir, isDown in
            self?.arrowsHint.setHighlight(direction: dir, on: isDown)
        }
        // Forward non-arrow keys through the controller's local-key
        // handler so notepat / Ableton letter keys still play notes
        // while the popover holds first-responder focus on the grid.
        instrumentList.onMusicKey = { [weak self] kc, isDown, isRepeat, flags in
            return self?.menuBand?.handleLocalKey(
                keyCode: kc, isDown: isDown, isRepeat: isRepeat, flags: flags
            ) ?? false
        }
        instrumentList.onHover = { [weak self] prog in
            guard let self = self else { return }
            // Hover plays a continuous preview note in the hovered program;
            // moving to another cell stops + restarts in the new program.
            self.menuBand?.setInstrumentPreview(prog.map { UInt8($0) })
            // Live-preview the chrome too — chip backdrop, chip text,
            // and visualizer base color all retint to whatever cell is
            // under the cursor while dragging. When hover ends (prog ==
            // nil) we snap back to the committed instrument.
            let safe: Int
            let nameForChip: String
            if let p = prog {
                safe = max(0, min(127, p))
                nameForChip = GeneralMIDI.programNames[safe]
            } else if let m = self.menuBand {
                safe = max(0, min(127, Int(m.melodicProgram)))
                nameForChip = GeneralMIDI.programNames[safe]
            } else {
                return
            }
            let famColor = InstrumentListView.colorForProgram(safe)
            self.instrumentReadout.stringValue = nameForChip
            self.instrumentReadout.textColor = famColor
            // Don't retint the LED bezel during hover-drag while
            // MIDI mode is on — it stays accent-colored as a status
            // badge.
            if let m = self.menuBand, !m.midiMode {
                self.waveformView.setBaseColor(famColor)
                self.waveformBezel?.layer?.borderColor =
                    famColor.withAlphaComponent(0.55).cgColor
            }
        }
        // Wrap the grid in a panel that adds an extra strip BELOW the
        // 8×16 cells where the arrow-keys hint glyph lives. The hint
        // is its own bottom-right ornament — never overlaying the
        // voice cells — so the whole assembly reads like a stepper-
        // button corner on a stereo's faceplate.
        let palettePanel = NSView()
        palettePanel.translatesAutoresizingMaskIntoConstraints = false
        palettePanel.addSubview(instrumentList)

        // MacBook-style keyboard chassis behind the QWERTY map +
        // arrow keys. Layer-painted rounded slab tinted to read as
        // brushed silver in light mode and space-gray in dark mode.
        // Added BEFORE the keycap views so it sits beneath them in
        // z-order — keys render on top of the chassis. Substrate
        // colors are applied per-appearance in
        // `applyAppearanceToKeyboardDeck`.
        //
        // Volume + right-side perspective: a soft drop shadow gives
        // the slab perceived thickness, and a tiny Y-axis rotation
        // (with m34 perspective) tilts the right edge slightly back,
        // so the chassis reads as a real laptop angled away from
        // the viewer. The keys are siblings of the deck (not
        // children), so they stay flat — only the substrate tilts.
        keyboardDeck = NSView()
        keyboardDeck.wantsLayer = true
        keyboardDeck.layer?.cornerRadius = 7
        keyboardDeck.layer?.borderWidth = 0.5
        keyboardDeck.layer?.shadowOpacity = 0.35
        keyboardDeck.layer?.shadowRadius = 6
        keyboardDeck.layer?.shadowOffset = CGSize(width: 0, height: -2)
        keyboardDeck.layer?.shadowColor = NSColor.black.cgColor
        var deckTransform = CATransform3DIdentity
        deckTransform.m34 = -1.0 / 900   // perspective foreshortening
        deckTransform = CATransform3DRotate(deckTransform, -.pi / 36, 0, 1, 0)
        keyboardDeck.layer?.transform = deckTransform
        keyboardDeck.translatesAutoresizingMaskIntoConstraints = false
        palettePanel.addSubview(keyboardDeck)

        arrowsHint = ArrowKeysIndicator()
        arrowsHint.toolTip = "Arrow keys move the selection."
        arrowsHint.translatesAutoresizingMaskIntoConstraints = false
        arrowsHint.onClick = { [weak self] dir, isDown in
            // Drive the same selection path the physical arrow keys
            // do — preview while pressed, commit on release.
            self?.simulateArrow(direction: dir, isDown: isDown)
        }
        palettePanel.addSubview(arrowsHint)
        let cornerInset: CGFloat = 4
        // Strip below the grid: keyboard chassis wraps the QWERTY
        // map on top and the arrow-keys cluster tucked into its
        // bottom-right corner. Reads like a tiny laptop keyboard
        // glued to the base of the voice grid. Sized to hug the
        // qwerty (46h) + arrows (30h) + insets — no trackpad slab
        // anymore, so the strip can be tighter.
        let strip: CGFloat = 88
        qwertyMap = QwertyLayoutView()
        qwertyMap.translatesAutoresizingMaskIntoConstraints = false
        // Pointer-driven play: clicks/drags on caps route through the
        // same handleLocalKey path the physical keyboard uses, so notes
        // light up, octave keys shift, and lit-state updates round-trip
        // back into the layout view automatically.
        qwertyMap.onKey = { [weak self] kc, isDown in
            _ = self?.menuBand?.handleLocalKey(
                keyCode: kc, isDown: isDown, isRepeat: false, flags: []
            )
        }
        palettePanel.addSubview(qwertyMap)
        NSLayoutConstraint.activate([
            instrumentList.topAnchor.constraint(equalTo: palettePanel.topAnchor),
            instrumentList.leadingAnchor.constraint(equalTo: palettePanel.leadingAnchor),
            instrumentList.trailingAnchor.constraint(equalTo: palettePanel.trailingAnchor),
            instrumentList.heightAnchor.constraint(equalToConstant: InstrumentListView.preferredHeight),

            // Deck wraps the keys + trackpad. Spans the full panel
            // width so the chassis reads as a real laptop deck under
            // the voice grid.
            keyboardDeck.leadingAnchor.constraint(equalTo: palettePanel.leadingAnchor),
            keyboardDeck.trailingAnchor.constraint(equalTo: palettePanel.trailingAnchor),
            keyboardDeck.topAnchor.constraint(equalTo: instrumentList.bottomAnchor, constant: 6),
            keyboardDeck.bottomAnchor.constraint(equalTo: palettePanel.bottomAnchor),

            // QWERTY map sits at the top of the chassis with a
            // small inset from the deck's rounded edge.
            qwertyMap.centerXAnchor.constraint(equalTo: keyboardDeck.centerXAnchor),
            qwertyMap.topAnchor.constraint(equalTo: keyboardDeck.topAnchor, constant: 6),
            qwertyMap.widthAnchor.constraint(equalToConstant: QwertyLayoutView.intrinsicSize.width),
            qwertyMap.heightAnchor.constraint(equalToConstant: QwertyLayoutView.intrinsicSize.height),

            // Arrow cluster nestles below the QWERTY rows on the
            // right edge — same inverted-T position a real laptop
            // would put it.
            arrowsHint.trailingAnchor.constraint(equalTo: keyboardDeck.trailingAnchor, constant: -cornerInset),
            arrowsHint.topAnchor.constraint(equalTo: qwertyMap.bottomAnchor, constant: 2),
        ])
        stack.addArrangedSubview(palettePanel)
        palettePanel.widthAnchor.constraint(equalToConstant: InstrumentListView.preferredWidth).isActive = true
        palettePanel.heightAnchor.constraint(equalToConstant: InstrumentListView.preferredHeight + strip).isActive = true

        // Layout block (built earlier, appended here so it sits below
        // the voice grid + arrow keys).
        stack.setCustomSpacing(8, after: palettePanel)
        stack.addArrangedSubview(layoutBlock.label)
        stack.addArrangedSubview(layoutBlock.picker)
        stack.addArrangedSubview(layoutBlock.hint)

        // No divider above the about/brand block — the palette + Layout
        // section above gives plenty of separation. Custom airspace
        // before the about block.
        stack.setCustomSpacing(14, after: layoutBlock.hint)

        // About + Crash logs in a side-by-side row. About has low hugging
        // so it expands when the crash column is hidden (no reports) —
        // takes the whole row instead of leaving negative space on the
        // right. With reports present, the crash column claims its
        // intrinsic content width and About fills what's left.
        let aboutCrashRow = NSStackView()
        aboutCrashRow.orientation = .horizontal
        aboutCrashRow.alignment = .top
        aboutCrashRow.distribution = .fill
        aboutCrashRow.spacing = 12

        let aboutCol = NSStackView()
        aboutCol.orientation = .vertical
        aboutCol.alignment = .leading
        aboutCol.spacing = 6
        // No heading — the prose itself is the about content. The bold
        // "Menu Band" header on top read as a duplicate of the menubar
        // identity above and ate vertical space.
        let aboutBody = NSTextField(wrappingLabelWithString: "")
        aboutBody.font = NSFont.systemFont(ofSize: 10.5)
        aboutBody.textColor = .secondaryLabelColor
        aboutBody.maximumNumberOfLines = 0
        aboutBody.lineBreakMode = .byWordWrapping
        // "Menu Band" stays bold + label-colored; the rest of the
        // sentence is regular weight in secondary color so the eye
        // catches the brand first.
        let aboutText = NSMutableAttributedString()
        let bodyFont = NSFont.systemFont(ofSize: 10.5)
        let boldFont = NSFont.systemFont(ofSize: 10.5, weight: .bold)
        aboutText.append(NSAttributedString(string: "Menu Band",
            attributes: [.font: boldFont, .foregroundColor: NSColor.labelColor]))
        aboutText.append(NSAttributedString(
            string: " brings the built-in macOS instruments into the menu bar.",
            attributes: [.font: bodyFont, .foregroundColor: NSColor.secondaryLabelColor]))
        aboutBody.attributedStringValue = aboutText
        aboutBody.preferredMaxLayoutWidth = InstrumentListView.preferredWidth
        aboutCol.setContentHuggingPriority(.defaultLow, for: .horizontal)
        aboutCol.addArrangedSubview(aboutBody)
        // Two badge-style links stacked vertically. Aesthetic.Computer
        // wears its purple-on-pale-purple identity; notepat.com gets a
        // dark gray slab so the concert-poster white/black shadow play
        // reads.
        let linksCol = NSStackView()
        linksCol.orientation = .vertical
        linksCol.alignment = .leading
        linksCol.spacing = 4
        let acPurple = NSColor(red: 167/255, green: 139/255, blue: 250/255, alpha: 1)
        let acLink = Self.makeLinkButton(
            attr: Self.aestheticComputerTitle(),
            target: self, action: #selector(openAesthetic),
            background: acPurple.withAlphaComponent(0.14),
            border: acPurple.withAlphaComponent(0.55))
        let npLink = Self.makeLinkButton(
            attr: Self.notepatComTitle(),
            target: self, action: #selector(openNotepat),
            background: NSColor(white: 0.30, alpha: 1),
            border: nil)
        linksCol.addArrangedSubview(acLink)
        linksCol.addArrangedSubview(npLink)
        aboutCol.addArrangedSubview(linksCol)

        // Crash-send moved out of this row — it now lives next to Quit
        // below as a small standalone button. Keeping it here as a side-by-
        // side column was pushing the about copy and clipping the popover
        // bottom on multi-line crash hints.
        crashStatusLabel = NSTextField(labelWithString: "")  // legacy ivar — unused
        crashHintLabel = NSTextField(labelWithString: "")    // legacy ivar — unused
        crashSendButton = NSButton(title: "Send crash reports",
                                   target: self,
                                   action: #selector(sendCrashLogs(_:)))
        crashSendButton.bezelStyle = .recessed
        crashSendButton.controlSize = .small
        crashSendButton.isHidden = true  // shown by refreshCrashStatus when n>0

        aboutCrashRow.addArrangedSubview(aboutCol)
        stack.addArrangedSubview(aboutCrashRow)
        // Air between the About/Crash block and the Quit button below so
        // Quit reads as its own action, not a list item under About.
        stack.setCustomSpacing(10, after: aboutCrashRow)

        // Quit — red bezel, white bold title. Bottom-right of the footer
        // row; crash-send button (when present) sits at the left of the
        // same row.
        let quit = NSButton()
        quit.bezelStyle = .rounded
        quit.isBordered = true
        quit.bezelColor = .systemRed
        quit.controlSize = .small
        quit.target = self
        quit.action = #selector(quitApp)
        quit.attributedTitle = NSAttributedString(
            string: "Quit Menu Band",
            attributes: [
                .foregroundColor: NSColor.white,
                .font: NSFont.systemFont(ofSize: 11, weight: .semibold),
            ]
        )
        let quitRow = NSStackView()
        quitRow.orientation = .horizontal
        quitRow.alignment = .centerY
        quitRow.spacing = 8
        let quitSpacer = NSView()
        quitSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        quitRow.addArrangedSubview(crashSendButton)
        quitRow.addArrangedSubview(quitSpacer)
        quitRow.addArrangedSubview(quit)
        stack.addArrangedSubview(quitRow)
        quitRow.widthAnchor.constraint(equalTo: stack.widthAnchor,
                                       constant: -16).isActive = true

        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: root.leadingAnchor),
            stack.trailingAnchor.constraint(equalTo: root.trailingAnchor),
            stack.topAnchor.constraint(equalTo: root.topAnchor),
            stack.bottomAnchor.constraint(equalTo: root.bottomAnchor),
        ])

        view = root
        // Force layout once so the autolayout fitting size accounts for
        // the flat-map (the tallest panel). The popover frame tracks
        // `preferredContentSize`, so we set it from the actual fitting
        // height rather than guessing — keeps the popover snug whether
        // the instrument map is added/removed/resized.
        root.layoutSubtreeIfNeeded()
        let fitting = stack.fittingSize
        // Width tracks the actual fitting size — the instrument map's
        // 224 px sets the floor, so the popover is as skinny as the
        // contents allow.
        preferredContentSize = NSSize(width: fitting.width,
                                       height: fitting.height)
    }

    // Pause the visualizer's display link whenever the popover isn't on
    // screen. The CVDisplayLink would otherwise keep firing 60 (or 120)
    // times a second while the popover is hidden, redrawing into a layer
    // no one can see. Patch contributed by Esteban Uribe.
    override func viewDidAppear() {
        super.viewDidAppear()
        guard isViewLoaded, let menuBand = waveformView.menuBand else { return }
        syncFromController()
        applyVisualizerForMidiMode(menuBand.midiMode)
        // Make the voice grid first responder on every popover open so
        // arrow keys always step the selection — even before the user
        // has clicked into the grid this session.
        view.window?.makeFirstResponder(instrumentList)
        // Refresh the Notepat mode button if a freshly-cached
        // favicon has landed since loadView() ran. One-shot observer
        // re-installs each time the popover appears so we don't leak
        // listeners.
        NotificationCenter.default.removeObserver(self,
            name: .notepatFaviconLoaded, object: nil)
        NotificationCenter.default.addObserver(forName: .notepatFaviconLoaded,
                                                object: nil, queue: .main) { [weak self] _ in
            guard let self = self,
                  self.modeButtons.indices.contains(0),
                  let img = NotepatFavicon.image else { return }
            self.modeButtons[0].image = img
        }
    }

    override func viewDidDisappear() {
        super.viewDidDisappear()
        waveformView.isLive = false
    }

    /// Drive the voice grid's selection from the on-screen arrow
    /// keycaps as if the physical arrow key had been pressed —
    /// `isDown` triggers the move + preview note, the matching `up`
    /// commits the cell. Mirrors `InstrumentMapView.keyDown` /
    /// `keyUp`'s logic so click-on-the-D-pad and arrow-key-on-the-
    /// keyboard share one code path.
    private func simulateArrow(direction dir: Int, isDown: Bool) {
        guard let list = instrumentList else { return }
        if isDown {
            let cur = Int(list.selectedProgram)
            var next = cur
            switch dir {
            case 0: next = cur - 1                                // ←
            case 1: next = cur + 1                                // →
            case 2: next = cur + InstrumentListView.cols          // ↓
            case 3: next = cur - InstrumentListView.cols          // ↑
            default: return
            }
            next = max(0, min(127, next))
            arrowsHint.setHighlight(direction: dir, on: true)
            if next != cur {
                list.selectedProgram = UInt8(next)
                list.onHover?(next)
            }
        } else {
            arrowsHint.setHighlight(direction: dir, on: false)
            list.onHover?(nil)
            list.onCommit?(Int(list.selectedProgram))
        }
    }

    /// Refresh the active-notes LCD from the controller. Hidden when
    /// nothing is held; otherwise lit with the held note names in
    /// the GM family color of the current voice (so the LCD readout
    /// reads as part of the same instrument).
    func refreshHeldNotes() {
        guard isViewLoaded, let m = menuBand else { return }
        // Mirror the controller's held key codes onto the QWERTY
        // map so the physical keys light up as the user plays.
        qwertyMap?.litKeyCodes = m.heldKeyCodes()
        let names = m.heldNoteNames()
        // Rebuild the floating-box stack from scratch — at most a
        // few notes held at any moment, so reusing views isn't worth
        // the bookkeeping. When nothing is held, the stack has no
        // arranged subviews and the reserved 22 px row is empty
        // (no visible chrome).
        for v in heldNotesStack.arrangedSubviews {
            heldNotesStack.removeArrangedSubview(v)
            v.removeFromSuperview()
        }
        let safe = max(0, min(127, Int(m.melodicProgram)))
        let famColor = m.midiMode
            ? NSColor.controlAccentColor
            : InstrumentListView.colorForProgram(safe)
        for name in names {
            heldNotesStack.addArrangedSubview(makeHeldNoteBox(name: name,
                                                               color: famColor))
        }
    }

    /// Small floating note badge — rounded layer-painted box with
    /// the note name in heavy mono. No surrounding bezel; the boxes
    /// just appear above the visualizer when you press keys.
    private func makeHeldNoteBox(name: String, color: NSColor) -> NSView {
        let box = NSView()
        box.wantsLayer = true
        box.layer?.cornerRadius = 4
        box.layer?.backgroundColor = color.withAlphaComponent(0.85).cgColor
        box.translatesAutoresizingMaskIntoConstraints = false
        let label = NSTextField(labelWithString: name)
        label.font = NSFont.monospacedSystemFont(ofSize: 10, weight: .heavy)
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

    /// Refresh control state from the controller — call right before showing.
    func syncFromController() {
        guard isViewLoaded, let n = menuBand else { return }
        midiSwitch.state = n.midiMode ? .on : .off
        refreshHeldNotes()
        octaveStepper.integerValue = n.octaveShift
        updateOctaveLabel(n.octaveShift)
        let segIdx = inputModeSegment(keymap: n.keymap)
        for (i, btn) in modeButtons.enumerated() {
            btn.state = (i == segIdx) ? .on : .off
        }
        instrumentList.selectedProgram = n.melodicProgram
        applyAppearanceToVisualizer()
        updateInstrumentReadout()
        // Keep the QWERTY layout's keymap + tint synced with the
        // controller. Voice color picks up the family hue for the
        // current voice; keymap variant follows the controller.
        let safe = max(0, min(127, Int(n.melodicProgram)))
        qwertyMap?.keymap = n.keymap
        qwertyMap?.voiceColor = n.midiMode
            ? .controlAccentColor
            : InstrumentListView.colorForProgram(safe)
        updateSelfTestLabel(state: n.midiMode ? n.midiSelfTest : .unknown)
        refreshCrashStatus()
        refreshUpdateBanner()
        // Waveform: live only when local synth is the audible path.
        // Stays in the layout when MIDI mode is on; the palette
        // visibility helper greys it out instead of collapsing.
        waveformView.isHidden = false
        // isLive is driven from viewDidAppear/viewDidDisappear so the
        // display link only runs while the popover is actually on screen.
        // Instrument palette: stays in the layout but greys out when
        // MIDI mode owns the audio path. Same physical width either way.
        applyInstrumentPaletteVisibility(midiMode: n.midiMode)
        // Wire up live updates so the label reflects loopback results as
        // they land (test runs ~50ms after toggle-on; result settles a moment
        // later).
        n.onSelfTestChanged = { [weak self] in
            DispatchQueue.main.async {
                guard let self = self, let nn = self.menuBand else { return }
                self.updateSelfTestLabel(state: nn.midiMode ? nn.midiSelfTest : .unknown)
            }
        }
        // Re-fit the popover after sync. preferredContentSize was locked
        // in loadView() while the crash column was empty/hidden and the
        // update banner was not yet shown; both can grow the layout
        // (multi-line crash hint, banner row) and would otherwise be
        // clipped at the bottom of the popover.
        refitContentSize()
    }

    /// Re-measure the stack's intrinsic fitting size and update
    /// `preferredContentSize` to match. Run after any change that can
    /// add/remove rows or change wrapping height (crash status,
    /// update banner, instrument palette toggle).
    private func refitContentSize() {
        guard isViewLoaded else { return }
        view.needsLayout = true
        view.layoutSubtreeIfNeeded()
        let fitting = view.fittingSize
        if fitting.height > 0 && fitting.width > 0 {
            preferredContentSize = NSSize(width: fitting.width,
                                           height: fitting.height)
        }
    }

    /// Format the readout in the title row as just the instrument name
    /// (no leading "078" program number — the cell's own backdrop color
    /// is the visual identifier; the number was redundant noise). A
    /// hair space on each side keeps the chip from touching its text.
    private func updateInstrumentReadout() {
        guard let m = menuBand else { return }
        let safe = max(0, min(127, Int(m.melodicProgram)))
        let title = GeneralMIDI.programNames[safe]
        let famColor = InstrumentListView.colorForProgram(safe)
        // Dark hues (Bass / Strings / Percussive) wash out against
        // the dark popover background; light hues (Piano ivory) wash
        // out in light mode. Adjust per-appearance so the title
        // always has presence.
        let isDark = view.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        // Flip the foreground/shadow relationship: max-contrast text
        // (white in dark, black in light) with the family color as a
        // hard 1-px shadow. Reads like a Risograph misregister — the
        // hue still keys the voice but the title stays legible.
        let textColor: NSColor = isDark ? .white : .black
        let shadow = NSShadow()
        // Light-tinted family color in both modes — a pastel offset
        // that reads as a Riso misregister rather than a dark drop
        // shadow weighing the title down. Light mode pushes harder
        // toward white so the offset stays clearly *lighter* than
        // the black title sitting on top.
        shadow.shadowColor = (famColor.highlight(withLevel: isDark ? 0.3 : 0.7)
            ?? famColor)
        shadow.shadowOffset = NSSize(width: 1, height: -1)
        shadow.shadowBlurRadius = 0
        // YWFT Processing — see AppDelegate.registerBundledFonts.
        // 0.7/0.8 tried to resolve the bold cut by PostScript name and
        // by family+symbolic-traits. Both paths returned a non-nil
        // wrong font (NSFont(descriptor:) silently substitutes the
        // system font on a miss instead of nil-ing), so the title
        // shipped in system black for two releases without any
        // visible signal of the failure. The reliable path is the
        // descriptor parsed directly from the .ttf URL at launch.
        // Verify familyName before accepting; log and fall back if
        // anything is off so the next regression can't hide.
        let titleFont: NSFont = {
            if let desc = AppDelegate.ywftBoldDescriptor,
               let f = NSFont(descriptor: desc, size: 18),
               f.familyName == "YWFT Processing" {
                return f
            }
            NSLog("MenuBand: YWFT bold descriptor unavailable; title falling back to system font")
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
        // The visualizer is the only piece of chrome that does NOT
        // track the voice color in MIDI mode — there it reads as a
        // status badge ("MIDI" dot-matrix in system accent), so we
        // skip the retint when MIDI is on.
        if m.midiMode {
            waveformView.setBaseColor(.controlAccentColor)
            waveformBezel?.layer?.borderColor = NSColor.controlAccentColor
                .withAlphaComponent(0.55).cgColor
        } else {
            waveformView.setBaseColor(famColor)
            waveformBezel?.layer?.borderColor = famColor
                .withAlphaComponent(0.55).cgColor
        }
    }

    // Appearance changes (light/dark toggle) refresh on next popover
    // open via syncFromController — viewDidChangeEffectiveAppearance
    // isn't on NSViewController in macOS so we don't try to hook it
    // mid-session.

    /// Flip the LED bezel + visualizer between dark-mode (LED-on-black
    /// glow) and light-mode (ink-on-paper) substrates so the meter
    /// doesn't look like a black slab pasted onto a white popover.
    private func applyAppearanceToVisualizer() {
        let isDark = view.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        waveformView.setLightMode(!isDark)
        if isDark {
            waveformBezel?.layer?.backgroundColor =
                NSColor(white: 0.06, alpha: 1.0).cgColor
        } else {
            // Slightly darker than the visualizer's own clear color so
            // the inset bars read as recessed into the bezel — same
            // recessed-housing effect as the dark mode 0.06 → 0.0 step.
            waveformBezel?.layer?.backgroundColor =
                NSColor(white: 0.82, alpha: 1.0).cgColor
        }
        applyAppearanceToKeyboardDeck(isDark: isDark)
    }

    /// Repaint the keyboard chassis against the current appearance.
    /// Light mode reads as brushed silver aluminum; dark mode reads
    /// as space-gray. Shadow tints darker in light mode (more
    /// contrast against the bright substrate) and a touch lighter
    /// against the dark popover background.
    private func applyAppearanceToKeyboardDeck(isDark: Bool) {
        guard let deck = keyboardDeck?.layer else { return }
        if isDark {
            deck.backgroundColor = NSColor(white: 0.18, alpha: 1.0).cgColor
            deck.borderColor     = NSColor(white: 0.30, alpha: 1.0).cgColor
            deck.shadowOpacity   = 0.55
        } else {
            deck.backgroundColor = NSColor(white: 0.86, alpha: 1.0).cgColor
            deck.borderColor     = NSColor(white: 0.68, alpha: 1.0).cgColor
            deck.shadowOpacity   = 0.30
        }
    }

    /// Reflect the MIDI loopback self-test status as the inline "MIDI"
    /// label's color. No textual chrome — the color is the indicator.
    private func updateSelfTestLabel(state: MenuBandController.MIDISelfTest) {
        guard let label = midiInlineLabel else { return }
        switch state {
        case .ok:
            label.textColor = .systemGreen
        case .failed:
            label.textColor = .systemRed
        case .running, .unknown:
            label.textColor = .secondaryLabelColor
        }
    }

    // MARK: - Builders

    private func makeSeparator() -> NSView {
        let box = NSBox()
        box.boxType = .separator
        return box
    }

    /// Badge-style link button — flat NSButton with a layer-painted
    /// fill + optional border, so the per-link attributed title sits
    /// inside a small chip. `bezelStyle = .inline` strips the system
    /// chrome; the layer below provides the badge look.
    static func makeLinkButton(attr: NSAttributedString,
                               target: AnyObject,
                               action: Selector,
                               background: NSColor? = nil,
                               border: NSColor? = nil) -> NSButton {
        let btn = NSButton()
        btn.bezelStyle = .inline
        btn.isBordered = false
        btn.controlSize = .small
        btn.target = target
        btn.action = action
        btn.attributedTitle = attr
        btn.wantsLayer = true
        btn.layer?.cornerRadius = 5
        if let bg = background {
            btn.layer?.backgroundColor = bg.cgColor
        }
        if let bd = border {
            btn.layer?.borderColor = bd.cgColor
            btn.layer?.borderWidth = 1
        }
        return btn
    }

    /// "Aesthetic.Computer" — purple words flanking a pink connecting
    /// dot. Padded with hair spaces on each side so the badge has
    /// breathing room from the layer-painted border.
    static func aestheticComputerTitle() -> NSAttributedString {
        let purple = NSColor(red: 167/255, green: 139/255, blue: 250/255, alpha: 1)
        let pink   = NSColor(red: 255/255, green: 107/255, blue: 157/255, alpha: 1)
        let font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        let s = NSMutableAttributedString()
        s.append(NSAttributedString(string: "Aesthetic",
            attributes: [.foregroundColor: purple, .font: font]))
        s.append(NSAttributedString(string: ".",
            attributes: [.foregroundColor: pink, .font: font]))
        s.append(NSAttributedString(string: "Computer",
            attributes: [.foregroundColor: purple, .font: font]))
        s.append(NSAttributedString(string: "  ",  // trailing badge padding
            attributes: [.font: font]))
        return s
    }

    /// "notepat.com" — all lowercase, heavy white set on the dark
    /// slab backdrop with a single down-right black drop shadow for
    /// concert-poster weight. Uniform coloring — no special dot.
    static func notepatComTitle() -> NSAttributedString {
        let font = NSFont.systemFont(ofSize: 11, weight: .heavy)
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.black
        shadow.shadowOffset = NSSize(width: 1, height: -1)
        shadow.shadowBlurRadius = 0
        return NSAttributedString(string: "notepat.com",
            attributes: [
                .foregroundColor: NSColor.white,
                .font: font,
                .shadow: shadow,
            ])
    }

    private func makeSwitchRow(label: String,
                               sublabel: String,
                               switchControl: NSSwitch) -> NSView {
        let row = NSStackView()
        row.orientation = .horizontal
        row.alignment = .centerY
        row.spacing = 10
        row.distribution = .fill

        let labelStack = NSStackView()
        labelStack.orientation = .vertical
        labelStack.alignment = .leading
        labelStack.spacing = 1

        let title = NSTextField(labelWithString: label)
        title.font = NSFont.systemFont(ofSize: 12, weight: .semibold)
        title.textColor = .labelColor

        let sub = NSTextField(labelWithString: sublabel)
        sub.font = NSFont.systemFont(ofSize: 10)
        sub.textColor = .secondaryLabelColor

        labelStack.addArrangedSubview(title)
        labelStack.addArrangedSubview(sub)

        row.addArrangedSubview(labelStack)
        let spacer = NSView()
        spacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        row.addArrangedSubview(spacer)
        row.addArrangedSubview(switchControl)
        return row
    }

    /// Update the crash-send button from disk. Called on every popover
    /// open so the count is current. The button lives in the quit row
    /// next to Quit Menu Band and stays hidden when there are no reports.
    private func refreshCrashStatus() {
        let n = CrashLogReader.recentLogs().count
        crashSendButton.isHidden = (n == 0)
        if n > 0 {
            crashSendButton.title = n == 1 ? "Send 1 crash" : "Send \(n) crashes"
            crashSendButton.isEnabled = true
        }
    }

    /// Hit the manifest at assets.aesthetic.computer/menuband/latest.json
    /// and show the banner if there's a newer version available than the
    /// one running. Cached for an hour inside UpdateChecker.
    private func refreshUpdateBanner() {
        let current = UpdateChecker.currentVersion()
        UpdateChecker.fetchLatest { [weak self] info in
            guard let self = self, let info = info else { return }
            if UpdateChecker.isNewer(info.version, than: current) {
                let notes = info.notes?.isEmpty == false ? " — \(info.notes!)" : ""
                self.updateLabel.stringValue =
                    "Update available: \(info.version)\(notes)"
                self.updateBanner.isHidden = false
            } else {
                self.updateBanner.isHidden = true
            }
        }
    }

    @objc private func openMenuBandSite() {
        if let url = URL(string: "https://aesthetic.computer/menuband") {
            NSWorkspace.shared.open(url)
        }
    }

    @objc private func sendCrashLogs(_ sender: NSButton) {
        let logs = CrashLogReader.recentLogs()
        guard !logs.isEmpty else { return }
        sender.isEnabled = false
        sender.title = "Sending…"

        let version = (Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String) ?? "?"
        var remaining = logs.count
        var ok = 0
        for log in logs {
            CrashLogReader.upload(log, version: version) { [weak self] success, _ in
                guard let self = self else { return }
                if success { ok += 1 }
                remaining -= 1
                if remaining == 0 {
                    self.crashSendButton.title = ok == logs.count
                        ? "Sent ✓"
                        : "Sent \(ok)/\(logs.count) — retry"
                    self.crashSendButton.isEnabled = ok != logs.count
                }
            }
        }
    }

    private func updateOctaveLabel(_ shift: Int) {
        // Piano starts at C4 (MIDI 60), so an octave shift of 0 shows "4".
        // Show the absolute octave number — easier to read than ±N for users
        // who think in scientific pitch notation.
        let octave = 4 + shift
        octaveLabel.stringValue = "\(octave)"
    }

    // MARK: - Actions

    @objc private func midiSwitchToggled(_ sender: NSSwitch) {
        // Just toggle — don't run the heavy syncFromController. The switch
        // already shows the user's intent; the loopback test (skipped on
        // toggles after the first per session) and other panels don't need
        // to refresh.
        menuBand?.toggleMIDIMode()
        // Tactile feedback — short system tick so the flip feels mechanical
        // even though it's a software switch.
        NSSound(named: NSSound.Name("Tink"))?.play()
        // Waveform + instrument palette grey out (don't hide) when MIDI
        // mode is on — the DAW chooses the instrument and the audio path,
        // so the local controls aren't useful, but keeping them in place
        // means the popover's geometry stays stable and the user can see
        // exactly what's available without MIDI engaged.
        if let m = menuBand {
            applyVisualizerForMidiMode(m.midiMode)
            applyInstrumentPaletteVisibility(midiMode: m.midiMode)
        }
    }

    /// 16-bar × 10-segment dot-matrix pattern that spells "MIDI"
    /// across the full height of the LED bezel. Each letter is 3
    /// bars wide, with a 1-bar gap between them: 3+1+3+1+3+1+3 = 15
    /// bars, with a 1-bar right margin. Letters span every segment
    /// (bits 0–9) so the readout fills the whole display.
    static let midiDotPattern: [UInt32] = {
        var p = [UInt32](repeating: 0, count: 16)
        let FULL   : UInt32 = 0x3FF       // segs 0..9 (all rows)
        let TOP_BOT: UInt32 = 0x201       // segs 0, 9 only
        let TOP_ROW: UInt32 = 0x100       // seg 8 (M's middle peak)
        let MID_BAR: UInt32 = 0x1FE       // segs 1..8 (D's right edge)
        // M (3 cols): full left + middle peak + full right.
        p[0]  = FULL
        p[1]  = TOP_ROW
        p[2]  = FULL
        // gap p[3]
        // I (3 cols)
        p[4]  = TOP_BOT
        p[5]  = FULL
        p[6]  = TOP_BOT
        // gap p[7]
        // D (3 cols): full left, top+bottom middle, mid-only right.
        p[8]  = FULL
        p[9]  = TOP_BOT
        p[10] = MID_BAR
        // gap p[11]
        // I (3 cols)
        p[12] = TOP_BOT
        p[13] = FULL
        p[14] = TOP_BOT
        return p
    }()

    private func applyInstrumentPaletteVisibility(midiMode: Bool, animated: Bool = false) {
        // Greyed-out, not hidden: keep the rows in place so the popover
        // doesn't reflow when MIDI flips. We dim alpha rather than touching
        // `isHidden`, which would collapse the row and resize the popover.
        let dimmed: CGFloat = midiMode ? 0.35 : 1.0
        instrumentSeparator.alphaValue = dimmed
        instrumentTitleRow.alphaValue = dimmed
        instrumentList.alphaValue = dimmed
        // Waveform stays visible (no longer collapses), just stops
        // ingesting samples — `isLive = false` (set by the caller) means
        // the bars freeze at their last value rather than going dark. To
        // convey "this is inactive," we fade it with the same alpha as
        // the palette. `_ = animated` keeps the parameter signature
        // compatible with existing call sites.
        waveformView.alphaValue = dimmed
        _ = animated
    }

    /// 0 = Notepat, 1 = Ableton. Matches the vertical button stack in
    /// `loadView()` after the "Mouse Only" option was retired.
    private func inputModeSegment(keymap: Keymap) -> Int {
        return keymap == .ableton ? 1 : 0
    }

    @objc private func modeButtonClicked(_ sender: NSButton) {
        guard let m = menuBand else { return }
        // Manual radio behaviour: only the clicked button stays .on.
        for btn in modeButtons { btn.state = (btn == sender) ? .on : .off }
        switch sender.tag {
        case 0:  // Notepat.com
            m.keymap = .notepat
            if !m.typeMode { m.toggleTypeMode() }
        case 1:  // Ableton MIDI Keys
            m.keymap = .ableton
            if !m.typeMode { m.toggleTypeMode() }
        default: break
        }
    }

    private func handleInstrumentCommit(_ program: Int) {
        guard let m = menuBand else { return }
        // If MIDI mode is on, picking an instrument from the GM palette
        // is a strong signal the user wants to *hear* their pick — but
        // MIDI mode silences the local synth (DAW is the audio path).
        // Auto-flip MIDI off + audition the new program so the click
        // makes sound immediately. Sync the switch UI to match.
        let wasMidiOn = m.midiMode
        if wasMidiOn {
            m.toggleMIDIMode()
            midiSwitch.state = .off
            updateSelfTestLabel(state: .unknown)
        }
        m.setMelodicProgram(UInt8(program))
        instrumentList.selectedProgram = UInt8(program)
        updateInstrumentReadout()
        debugLog("instrument commit prog=\(program) midiAutoOff=\(wasMidiOn)")
        if wasMidiOn {
            // Auto-off implicitly re-enables the local-synth audio
            // path; bring the visualizer back to live VU + voice
            // color in the same step. Single source of truth: the
            // controller's `midiMode` boolean drives both the synth
            // routing AND the meter visual state.
            applyVisualizerForMidiMode(false)
            m.auditionCurrentProgram()
        }
        // Otherwise no post-release audition: the press-gated rollover
        // already played a preview note while the mouse was held, so
        // retriggering on release just doubles the sound. mouseUp paths
        // through onHover(nil) first which stops the preview cleanly.
    }

    /// Single source-of-truth wiring between `midiMode` and the
    /// visualizer's three modes (live VU vs MIDI dot-matrix) plus
    /// its base color (voice family vs system accent). Called from
    /// every place that flips midiMode — keeps the meter from
    /// getting stuck in a stale state when MIDI is auto-disabled by
    /// picking a new voice.
    func applyVisualizerForMidiMode(_ midiOn: Bool) {
        guard let m = menuBand else { return }
        waveformView.isLive = !midiOn
        if midiOn {
            waveformView.setDotMatrix(Self.midiDotPattern)
            waveformView.setBaseColor(.controlAccentColor)
            waveformBezel?.layer?.borderColor = NSColor.controlAccentColor
                .withAlphaComponent(0.55).cgColor
        } else {
            waveformView.setDotMatrix(nil)
            let safe = max(0, min(127, Int(m.melodicProgram)))
            let famColor = InstrumentListView.colorForProgram(safe)
            waveformView.setBaseColor(famColor)
            waveformBezel?.layer?.borderColor = famColor
                .withAlphaComponent(0.55).cgColor
        }
    }

    @objc private func octaveChanged(_ sender: NSStepper) {
        menuBand?.octaveShift = sender.integerValue
        updateOctaveLabel(sender.integerValue)
    }

    @objc private func resetOctave() {
        menuBand?.octaveShift = 0
        octaveStepper.integerValue = 0
        updateOctaveLabel(0)
    }

    @objc private func octaveDown() {
        let new = max(Int(octaveStepper.minValue), octaveStepper.integerValue - 1)
        octaveStepper.integerValue = new
        menuBand?.octaveShift = new
        updateOctaveLabel(new)
    }

    @objc private func octaveUp() {
        let new = min(Int(octaveStepper.maxValue), octaveStepper.integerValue + 1)
        octaveStepper.integerValue = new
        menuBand?.octaveShift = new
        updateOctaveLabel(new)
    }

    @objc private func openAesthetic() {
        if let url = URL(string: "https://aesthetic.computer") {
            NSWorkspace.shared.open(url)
        }
    }

    @objc private func openNotepat() {
        if let url = URL(string: "https://notepat.com") {
            NSWorkspace.shared.open(url)
        }
    }

    @objc private func quitApp() {
        // Unload our LaunchAgent first so launchd doesn't immediately
        // relaunch us, then terminate.
        let plist = "\(NSHomeDirectory())/Library/LaunchAgents/computer.aestheticcomputer.menuband.plist"
        let task = Process()
        task.launchPath = "/bin/launchctl"
        task.arguments = ["unload", plist]
        try? task.run()
        task.waitUntilExit()
        NSApp.terminate(nil)
    }
}
