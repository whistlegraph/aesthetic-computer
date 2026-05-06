import AppKit
import Carbon

/// NSButton subclass for chip-shaped link buttons — paints a layer-backed
/// fill/border, swaps to a brighter "hover" pair when the cursor enters,
/// and switches the cursor to a pointing hand. Used by the Why-this-Keymap
/// chip and the Aesthetic.Computer / notepat.com brand badges so they all
/// behave like real links instead of mute-looking buttons.
final class HoverLinkButton: NSButton {
    var idleBackground: NSColor?
    var idleBorder: NSColor?
    var hoverBackground: NSColor?
    var hoverBorder: NSColor?
    private var trackingArea: NSTrackingArea?

    override func resetCursorRects() {
        super.resetCursorRects()
        addCursorRect(bounds, cursor: .pointingHand)
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let ta = trackingArea { removeTrackingArea(ta) }
        let ta = NSTrackingArea(
            rect: bounds,
            options: [.mouseEnteredAndExited, .activeAlways, .inVisibleRect],
            owner: self, userInfo: nil
        )
        addTrackingArea(ta)
        trackingArea = ta
    }

    override func mouseEntered(with event: NSEvent) {
        applyState(hovered: true)
    }

    override func mouseExited(with event: NSEvent) {
        applyState(hovered: false)
    }

    private func applyState(hovered: Bool) {
        let bg = hovered ? hoverBackground : idleBackground
        let bd = hovered ? hoverBorder : idleBorder
        layer?.backgroundColor = bg?.cgColor
        layer?.borderColor = bd?.cgColor
    }
}

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
    var onFocusShortcutChange: ((MenuBandShortcut) -> Bool)?
    var onFocusShortcutRecordingChanged: ((Bool) -> Void)?
    var onPlayPaletteToggle: (() -> Void)?
    var onPlayPaletteShortcutChange: ((MenuBandShortcut) -> Bool)?
    var onPlayPaletteShortcutRecordingChanged: ((Bool) -> Void)?
    var isPlayPaletteShown: (() -> Bool)?

    private var inputSegmented: HoverSegmentedControl!  // legacy reference; no longer added to stack
    private var modeButtons: [NSButton] = []           // vertical stack: Mouse Only / Notepat.com / Ableton MIDI Keys
    private var focusShortcutButton: NSButton!
    private var focusShortcutStatusLabel: NSTextField!
    private var focusShortcutRecorderMonitor: Any?
    private var isRecordingFocusShortcut = false
    private var playPaletteToggleButton: NSButton!
    private var playPaletteShortcutButton: NSButton!
    private var playPaletteShortcutStatusLabel: NSTextField!
    private var playPaletteShortcutRecorderMonitor: Any?
    private var isRecordingPlayPaletteShortcut = false
    private var midiSwitch: NSSwitch!
    private var midiInlineLabel: NSTextField!
    private var midiSelfTestLabel: NSTextField!  // legacy — created but never added to stack
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
    /// Held so `viewDidChangeEffectiveAppearance` can repaint the
    /// layer-painted background when the user toggles light/dark
    /// mode mid-session — `NSColor.windowBackgroundColor.cgColor`
    /// is resolved once at loadView, so we have to re-resolve it on
    /// each appearance change.
    private weak var rootBackgroundView: NSView?
    /// Chord-candidate cards live in their own row directly below the
    /// visualizer bezel, mirroring the floating play palette so the
    /// popover gets the same searchable chord readout.
    private var chordCandidatesStack: NSStackView!
    private var chordCandidatesRow: NSView!
    /// Internal but exposed so AppDelegate can push the pitch-shift
    /// value (octave + bend) directly when those change — the staff
    /// translates vertically by that amount so the user feels the
    /// shift visually too.
    private(set) var staffView: StaffView!
    private var lastCompleteChordNames: Set<String> = []
    private let chordCandidatesRowHorizontalInset: CGFloat = 6
    private var instrumentSeparator: NSView!
    private var octaveStepper: NSStepper!
    private var octaveLabel: NSTextField!
    private var crashStatusLabel: NSTextField!
    private var crashHintLabel: NSTextField!
    private var crashSendButton: NSButton!
    /// Cached result of the most recent UpdateChecker fetch. Populated
    /// asynchronously after view load; surfaced inside the custom About
    /// window when the user opens it.
    private var latestRemoteVersion: UpdateChecker.VersionInfo?

    /// Retained so the floating About window stays alive after
    /// `showAboutPanel` returns. Recreated on each open so the update
    /// state reflects the latest manifest fetch.
    private var aboutWindowController: AboutWindowController?
    /// Layered substrate for the held-notes pills + chord cards. The
    /// MTL waveform that used to live inside this bezel has been
    /// retired; the housing stays for visual continuity (rounded
    /// dark recess) but holds only the held-notes pills (top) and
    /// chord candidate cards (bottom).
    private var waveformBezel: NSView!
    private var metronome: MetronomeWidget!

    deinit {
        if let monitor = focusShortcutRecorderMonitor {
            NSEvent.removeMonitor(monitor)
        }
        if let monitor = playPaletteShortcutRecorderMonitor {
            NSEvent.removeMonitor(monitor)
        }
    }

    override func loadView() {
        // Plain solid-color background — no NSVisualEffectView. The visual
        // effect view sampled the surrounding context and shifted appearance
        // when focus moved between the menu bar and the popover. A flat
        // background keeps the popover homogeneous in all states.
        let root = MenuBandPopoverRootView()
        root.wantsLayer = true
        root.layer?.backgroundColor = NSColor.windowBackgroundColor.cgColor
        root.translatesAutoresizingMaskIntoConstraints = false
        root.onAppearanceChange = { [weak self] in
            self?.handleEffectiveAppearanceChange()
        }
        rootBackgroundView = root

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
                                  accessibilityDescription: L("popover.octave.down"))?
            .withSymbolConfiguration(chevConfig)
        leftArrow.isBordered = false
        leftArrow.controlSize = .small
        leftArrow.imagePosition = .imageOnly
        leftArrow.contentTintColor = .secondaryLabelColor
        leftArrow.target = self
        leftArrow.action = #selector(octaveDown)

        let rightArrow = NSButton()
        rightArrow.image = NSImage(systemSymbolName: "chevron.right",
                                   accessibilityDescription: L("popover.octave.up"))?
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
        let octaveHint = NSTextField(labelWithString: L("popover.octave"))
        octaveHint.font = NSFont.systemFont(ofSize: 9, weight: .regular)
        octaveHint.textColor = .tertiaryLabelColor

        // Octave widget temporarily retired from the popover — the
        // chevrons + active-octave readout are gone but the stepper
        // stays in the row (hidden) as the value model so the rest
        // of the controller bindings keep working unchanged.
        _ = leftArrow
        _ = rightArrow
        _ = octaveHint
        titleRow.addArrangedSubview(octaveStepper)  // hidden, value model only

        // Spacer lives in the middle so the octave widget pins LEFT and
        // the MIDI pair pins RIGHT.
        titleRow.addArrangedSubview(titleSpacer)

        // Metronome — sits between the octave widget and the MIDI
        // switch. Custom-drawn analog body with a real swinging
        // needle (animates while playing) and a tiny `<` BPM `>`
        // stepper underneath, mirroring the octave widget's chevron
        // look. Click the body to toggle play; chevrons step BPM.
        metronome = MetronomeWidget()
        metronome.translatesAutoresizingMaskIntoConstraints = false
        // Use the dynamic label color so the body / needle / arrows
        // invert with the system theme — was hardcoded to white,
        // which left the icon invisible against the popover's
        // light-mode background.
        metronome.tint = .labelColor
        metronome.toolTip = "Metronome — space to start / stop"
        metronome.onTick = { [weak self] in
            // Yellow-flash the menubar music-note icon on every
            // metronome beat. AppDelegate's animation tick decays
            // it back to zero between beats.
            KeyboardIconRenderer.metronomeFlash = 1
            // Drop a beat marker on the staff DIRECTLY UNDER the
            // metronome icon — projects the needle anchor point
            // through metronome → staff coordinates so each bar
            // literally tumbles out of the metronome.
            guard let self = self,
                  let staffView = self.staffView,
                  let metronome = self.metronome else { return }
            let anchor = metronome.needleAnchorPoint
            let inStaff = staffView.convert(anchor, from: metronome)
            staffView.dropBeatMarker(atX: inStaff.x)
        }
        metronome.onRunningChanged = { [weak self] in
            // Repaint the menubar icon as soon as the running state
            // flips so the wave indicator appears (or vanishes)
            // even while the popover stays open.
            (NSApp.delegate as? AppDelegate)?.updateIcon()
            _ = self
        }
        titleRow.addArrangedSubview(metronome)
        titleRow.setCustomSpacing(8, after: metronome)
        // Right-side spacer to balance `titleSpacer` on the left,
        // so the metronome floats horizontally centered in the
        // title row (sandwiched between two flex spacers).
        let trailingSpacer = NSView()
        trailingSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        titleRow.addArrangedSubview(trailingSpacer)
        if let titleSpacer = titleRow.arrangedSubviews.first(where: {
            $0.contentHuggingPriority(for: .horizontal) == .defaultLow && $0 !== trailingSpacer
        }) {
            trailingSpacer.widthAnchor.constraint(equalTo: titleSpacer.widthAnchor).isActive = true
        }

        // MIDI toggle is now slot 0 in the chooser ("0 MIDI OUT"). The
        // ivars below stay so existing references (status sync, the
        // legacy controller-on-change handler) keep compiling without
        // touching every callsite — they're driven invisibly.
        midiSwitch = NSSwitch()
        midiSwitch.target = self
        midiSwitch.action = #selector(midiSwitchToggled(_:))
        midiSwitch.isHidden = true
        midiInlineLabel = NSTextField(labelWithString: "")
        midiInlineLabel.isHidden = true

        stack.addArrangedSubview(titleRow)
        titleRow.widthAnchor.constraint(equalTo: stack.widthAnchor,
                                         constant: -16).isActive = true

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
        let inputLabel = NSTextField(labelWithString: L("popover.layout.label"))
        inputLabel.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        inputLabel.textColor = .labelColor

        let shortcuts = NSStackView()
        shortcuts.orientation = .horizontal
        shortcuts.alignment = .centerY
        shortcuts.distribution = .fill
        shortcuts.spacing = 8
        shortcuts.translatesAutoresizingMaskIntoConstraints = false

        let focusLabel = NSTextField(labelWithString: L("popover.shortcuts.focus"))
        focusLabel.font = NSFont.systemFont(ofSize: 11)
        focusLabel.textColor = .labelColor
        shortcuts.addArrangedSubview(focusLabel)

        let focusSpacer = NSView()
        focusSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        shortcuts.addArrangedSubview(focusSpacer)

        focusShortcutButton = NSButton(
            title: MenuBandShortcutPreferences.focusShortcut.displayString,
            target: self,
            action: #selector(focusShortcutButtonClicked(_:))
        )
        focusShortcutButton.bezelStyle = .recessed
        focusShortcutButton.controlSize = .small
        focusShortcutButton.translatesAutoresizingMaskIntoConstraints = false
        focusShortcutButton.widthAnchor.constraint(equalToConstant: 96).isActive = true
        shortcuts.addArrangedSubview(focusShortcutButton)

        shortcuts.widthAnchor.constraint(
            equalToConstant: InstrumentListView.preferredWidth
        ).isActive = true

        focusShortcutStatusLabel = NSTextField(labelWithString: "")
        focusShortcutStatusLabel.font = NSFont.systemFont(ofSize: 10)
        focusShortcutStatusLabel.textColor = .secondaryLabelColor
        focusShortcutStatusLabel.lineBreakMode = .byTruncatingTail
        focusShortcutStatusLabel.widthAnchor.constraint(
            equalToConstant: InstrumentListView.preferredWidth
        ).isActive = true

        let playPaletteRow = NSStackView()
        playPaletteRow.orientation = .horizontal
        playPaletteRow.alignment = .centerY
        playPaletteRow.distribution = .fill
        playPaletteRow.spacing = 6
        playPaletteRow.translatesAutoresizingMaskIntoConstraints = false

        let playPaletteLabel = NSTextField(labelWithString: L("popover.shortcuts.floating"))
        playPaletteLabel.font = NSFont.systemFont(ofSize: 11)
        playPaletteLabel.textColor = .labelColor
        playPaletteLabel.lineBreakMode = .byTruncatingTail
        playPaletteLabel.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
        playPaletteRow.addArrangedSubview(playPaletteLabel)

        let playPaletteSpacer = NSView()
        playPaletteSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        playPaletteRow.addArrangedSubview(playPaletteSpacer)

        playPaletteToggleButton = NSButton(
            title: L("popover.shortcuts.show"),
            target: self,
            action: #selector(playPaletteToggleButtonClicked(_:))
        )
        playPaletteToggleButton.bezelStyle = .recessed
        playPaletteToggleButton.controlSize = .small
        playPaletteToggleButton.translatesAutoresizingMaskIntoConstraints = false
        playPaletteToggleButton.widthAnchor.constraint(equalToConstant: 48).isActive = true
        playPaletteRow.addArrangedSubview(playPaletteToggleButton)

        playPaletteShortcutButton = NSButton(
            title: MenuBandShortcutPreferences.playPaletteShortcut.displayString,
            target: self,
            action: #selector(playPaletteShortcutButtonClicked(_:))
        )
        playPaletteShortcutButton.bezelStyle = .recessed
        playPaletteShortcutButton.controlSize = .small
        playPaletteShortcutButton.translatesAutoresizingMaskIntoConstraints = false
        playPaletteShortcutButton.widthAnchor.constraint(equalToConstant: 88).isActive = true
        playPaletteRow.addArrangedSubview(playPaletteShortcutButton)

        playPaletteRow.widthAnchor.constraint(
            equalToConstant: InstrumentListView.preferredWidth
        ).isActive = true

        playPaletteShortcutStatusLabel = NSTextField(labelWithString: "")
        playPaletteShortcutStatusLabel.font = NSFont.systemFont(ofSize: 10)
        playPaletteShortcutStatusLabel.textColor = .secondaryLabelColor
        playPaletteShortcutStatusLabel.lineBreakMode = .byTruncatingTail
        playPaletteShortcutStatusLabel.widthAnchor.constraint(
            equalToConstant: InstrumentListView.preferredWidth
        ).isActive = true

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
            (L("popover.layout.notepat"),
             NotepatFavicon.image
                ?? NSImage(systemSymbolName: "keyboard",
                            accessibilityDescription: L("popover.layout.notepat"))?
                    .withSymbolConfiguration(modeSymbolConfig)),
            (L("popover.layout.ableton"),
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
            L("popover.layout.hint"))
        inputHint.font = NSFont.systemFont(ofSize: 10)
        inputHint.textColor = .secondaryLabelColor

        // "Why this Keymap?" — small chip-link to the keymaps paper.
        // Sits right under the layout-mode picker so the user has a
        // one-tap path from "what is this layout" to the writeup
        // explaining the chromatic notepat keymap, the Ableton M-mode
        // overlap, and why the popover defaults to notepat.com.
        let whyKeymapAttr = NSMutableAttributedString(
            string: L("popover.layout.why"),
            attributes: [
                .font: NSFont.systemFont(ofSize: 11, weight: .semibold),
                .foregroundColor: NSColor.linkColor,
            ]
        )
        let whyKeymapButton = MenuBandPopoverViewController.makeLinkButton(
            attr: whyKeymapAttr,
            target: self,
            action: #selector(openKeymapsPaper(_:))
        )
        whyKeymapButton.toolTip = L("popover.layout.why.tooltip")

        // Layout label / mode buttons / hint are inserted into the
        // popover stack farther down — see the `palettePanel`
        // insertion point.
        let layoutBlock = (label: inputLabel, picker: modeStack,
                            hint: inputHint, link: whyKeymapButton)

        let shortcutLabel = NSTextField(labelWithString: L("popover.shortcuts.label"))
        shortcutLabel.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        shortcutLabel.textColor = .labelColor

        // Live segmented LED meter of the local synth output. Hidden in
        // MIDI mode (DAW handles audio there; our local mixer is silent
        // so the bars would just sit dark).
        //
        // Wrapped in a layer-backed bezel so the meter reads as a
        // proper VU display housing — dark recessed background, soft
        // border, uniform inner margin around the bars. Without the
        // bezel the bars sit flush against the popover walls and feel
        // unfinished.
        // The popover's visualizer was retired — the InstrumentListView
        // (above) is the only visualizer left. The container below is
        // a transparent layout wrapper for the held-notes pills + chord
        // cards (no chrome, no background, no border) so the rows just
        // sit on the popover surface.
        waveformBezel = NSView()
        waveformBezel.translatesAutoresizingMaskIntoConstraints = false
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

        // Visualizer bezel doubles as the chord-finder canvas: held-
        // note pills overlay the top of the meter, chord-candidate
        // cards overlay the bottom, both translucent enough that the
        // live waveform peeks through underneath. Bezel grows tall
        // enough to host both — same integrated layout the floating
        // play palette uses, so the two surfaces feel identical.
        stack.addArrangedSubview(waveformBezel)
        waveformBezel.widthAnchor.constraint(equalToConstant: InstrumentListView.preferredWidth).isActive = true
        // Bezel hosts the staff only — the big held-notes pill row
        // (key-over-letter pills) was retired in favor of the
        // staff being the single notation surface. Tall enough
        // to fit the extended A3..B5 range with breathing room.
        waveformBezel.heightAnchor.constraint(equalToConstant: 184).isActive = true
        // Pill container is allocated but unused (kept so any
        // legacy refresh path doesn't crash); not added to the
        // bezel.
        _ = heldNotesContainer

        // Chord candidates — every recognized chord shape that
        // contains the user's held pitch classes, plus partial
        // candidates whose missing notes are reachable on the
        // active keymap. Displayed as a wrapping flow row so
        // *all* available chords stay visible (not just three).
        chordCandidatesStack = NSStackView()
        chordCandidatesStack.orientation = .vertical
        chordCandidatesStack.alignment = .centerX
        chordCandidatesStack.spacing = 4
        chordCandidatesStack.translatesAutoresizingMaskIntoConstraints = false
        chordCandidatesRow = NSView()
        chordCandidatesRow.translatesAutoresizingMaskIntoConstraints = false
        chordCandidatesRow.wantsLayer = true
        chordCandidatesRow.layer?.masksToBounds = false
        chordCandidatesRow.addSubview(chordCandidatesStack)
        NSLayoutConstraint.activate([
            chordCandidatesStack.centerXAnchor.constraint(equalTo: chordCandidatesRow.centerXAnchor),
            chordCandidatesStack.topAnchor.constraint(equalTo: chordCandidatesRow.topAnchor, constant: 2),
            chordCandidatesStack.leadingAnchor.constraint(greaterThanOrEqualTo: chordCandidatesRow.leadingAnchor, constant: chordCandidatesRowHorizontalInset),
            chordCandidatesStack.trailingAnchor.constraint(lessThanOrEqualTo: chordCandidatesRow.trailingAnchor, constant: -chordCandidatesRowHorizontalInset),
            chordCandidatesStack.bottomAnchor.constraint(lessThanOrEqualTo: chordCandidatesRow.bottomAnchor, constant: -2),
        ])
        waveformBezel.addSubview(chordCandidatesRow)
        NSLayoutConstraint.activate([
            chordCandidatesRow.leadingAnchor.constraint(equalTo: waveformBezel.leadingAnchor),
            chordCandidatesRow.trailingAnchor.constraint(equalTo: waveformBezel.trailingAnchor),
            // Anchor to the bezel itself — heldNotesContainer used
            // to be a sibling but was retired with the big-pill
            // row, so referencing its bottomAnchor here crashed
            // the popover load (no common ancestor).
            chordCandidatesRow.topAnchor.constraint(equalTo: waveformBezel.topAnchor, constant: 4),
            chordCandidatesRow.bottomAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: -4),
        ])
        // Single traditional staff view in the lower half of the
        // bezel. Held notes draw as solid heads; *all* chord
        // candidates' missing notes are ghosted on top of the
        // SAME staff, each chord's notes connected with a colored
        // path keyed to the chord family — major routes are green,
        // minor blue, dom7 orange, etc. Routes overlap on the
        // staff; the user reads the suggestions visually rather
        // than as a list.
        staffView = StaffView()
        staffView.translatesAutoresizingMaskIntoConstraints = false
        chordCandidatesStack.addArrangedSubview(staffView)
        NSLayoutConstraint.activate([
            staffView.widthAnchor.constraint(equalToConstant: InstrumentListView.preferredWidth + 16),
            staffView.heightAnchor.constraint(equalToConstant: 168),
        ])

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
        // Instrument title moved out of the popover with the chooser
        // — the chooser cells in the floating panel are the only
        // place the active GM voice surfaces now.

        // (held-notes floating-boxes container is built ABOVE the
        // visualizer — see the block before `waveformBezel`.)

        // GarageBand backend toggle was prototyped here (see
        // GarageBandLibrary + GarageBandPatchView), then deprecated
        // pending UX polish. Source files retained for future revival;
        // the popover currently exposes only the General MIDI grid.

        // Instrument chooser was lifted out — it now lives in the
        // collapsed floating panel that pairs with the popover. The
        // popover holds settings + the QWERTY chassis below.
        let palettePanel = NSView()
        palettePanel.translatesAutoresizingMaskIntoConstraints = false

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
        arrowsHint.toolTip = L("popover.arrows.tooltip")
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
        // glued to the base of the voice grid. Tuned to hug the
        // qwerty (46h) + arrows (30h) with the qwerty/arrows gap
        // squeezed to 0 — no dead vertical band between rows.
        let strip: CGFloat = 82
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
            // Deck wraps the keys + trackpad. Sits at the top of the
            // panel since the chooser that used to live above it is
            // now in the floating window.
            keyboardDeck.leadingAnchor.constraint(equalTo: palettePanel.leadingAnchor),
            keyboardDeck.trailingAnchor.constraint(equalTo: palettePanel.trailingAnchor),
            keyboardDeck.topAnchor.constraint(equalTo: palettePanel.topAnchor),
            keyboardDeck.bottomAnchor.constraint(equalTo: palettePanel.bottomAnchor),

            // QWERTY map sits at the top of the chassis with a
            // small inset from the deck's rounded edge.
            qwertyMap.centerXAnchor.constraint(equalTo: keyboardDeck.centerXAnchor),
            qwertyMap.topAnchor.constraint(equalTo: keyboardDeck.topAnchor, constant: 4),
            qwertyMap.widthAnchor.constraint(equalToConstant: QwertyLayoutView.intrinsicSize.width),
            qwertyMap.heightAnchor.constraint(equalToConstant: QwertyLayoutView.intrinsicSize.height),

            // Arrow cluster nestles directly below the QWERTY rows on
            // the right edge — inverted-T position, no vertical gap so
            // the cluster reads as a continuation of the qwerty deck
            // instead of a stranded floating widget.
            arrowsHint.trailingAnchor.constraint(equalTo: keyboardDeck.trailingAnchor, constant: -cornerInset),
            arrowsHint.topAnchor.constraint(equalTo: qwertyMap.bottomAnchor, constant: 0),
        ])
        // QWERTY chassis + arrow cluster moved out of the popover —
        // they live under the chooser in the floating panel now.
        // The palettePanel + its subviews are still constructed
        // above so the .onChange handlers that poke `arrowsHint`,
        // `qwertyMap`, etc. don't crash; the panel just never makes
        // it into the popover view tree.
        _ = palettePanel
        // Keymap picker, focus / play-palette shortcut rows retired
        // from the popover — the liquid floating panel hosts the
        // qwerty map + chooser as an "expanded part of the
        // instrument," and the popover stays a music-theory surface
        // (held notes + chord cards above) plus app-meta (about /
        // language / quit below). Layout block + shortcut bindings
        // are still constructed above so syncFromController + recorder
        // wiring keep compiling, just never added to the stack.
        _ = layoutBlock
        _ = shortcutLabel
        _ = shortcuts
        _ = playPaletteRow

        stack.setCustomSpacing(14, after: waveformBezel)

        // Description + brand chip moved out of the popover proper —
        // they now live in the standard macOS About panel reachable via
        // the small "About" link at bottom-left. Frees the popover to
        // be operational chrome.
        crashStatusLabel = NSTextField(labelWithString: "")  // legacy ivar — unused
        crashHintLabel = NSTextField(labelWithString: "")    // legacy ivar — unused
        crashSendButton = NSButton(title: L("popover.about.crash.send"),
                                   target: self,
                                   action: #selector(sendCrashLogs(_:)))
        crashSendButton.bezelStyle = .recessed
        crashSendButton.controlSize = .small
        crashSendButton.isHidden = true  // shown by refreshCrashStatus when n>0

        // Language picker moved into the About window — the popover
        // stays a tight music-theory surface. The "About" link in
        // the footer row gets a flag emoji prepended so users know
        // there are options behind it (language + plugins + version).

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
            string: L("popover.about.quit"),
            attributes: [
                .foregroundColor: NSColor.white,
                .font: NSFont.systemFont(ofSize: 11, weight: .semibold),
            ]
        )
        // Small "About" link, bottom-left. Opens the custom About
        // window which now hosts the language picker + plugins
        // chip + version. The current language flag is prepended
        // to the link so the chip reads as "settings hide here"
        // rather than just a version button.
        let aboutLink = NSButton()
        aboutLink.bezelStyle = .recessed
        aboutLink.isBordered = false
        aboutLink.controlSize = .small
        let flag = Localization.language(for: Localization.current).flag
        let aboutTitle = NSMutableAttributedString(
            string: "\(flag)  ",
            attributes: [
                .font: NSFont.systemFont(ofSize: 11),
            ]
        )
        aboutTitle.append(NSAttributedString(
            string: L("popover.about.link"),
            attributes: [
                .foregroundColor: NSColor.secondaryLabelColor,
                .font: NSFont.systemFont(ofSize: 10, weight: .medium),
            ]
        ))
        aboutLink.attributedTitle = aboutTitle
        aboutLink.target = self
        aboutLink.action = #selector(showAboutPanel(_:))
        aboutLink.toolTip = "About / language / plugins"

        let quitRow = NSStackView()
        quitRow.orientation = .horizontal
        quitRow.alignment = .centerY
        quitRow.spacing = 8
        let quitSpacer = NSView()
        quitSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        quitRow.addArrangedSubview(aboutLink)
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
        guard isViewLoaded, let menuBand = self.menuBand else { return }
        syncFromController()
        applyVisualizerForMidiMode(menuBand.midiMode)
        // Voice-grid focus moved out of the popover with the chooser
        // (now in the floating panel). Nothing to make first responder
        // here.
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
        stopFocusShortcutRecording(status: nil)
        stopPlayPaletteShortcutRecording(status: nil)
    }

    /// Drive the program selection from the on-screen arrow keycaps.
    /// The visible chooser moved to the floating panel, so the popover
    /// now drives the program directly via the controller — preview
    /// while pressed, commit on release.
    private func simulateArrow(direction dir: Int, isDown: Bool) {
        guard let m = menuBand else { return }
        let cur = Int(m.effectiveMelodicProgram)
        var next = cur
        switch dir {
        case 0: next = cur - 1                                // ←
        case 1: next = cur + 1                                // →
        case 2: next = cur + InstrumentListView.cols          // ↓
        case 3: next = cur - InstrumentListView.cols          // ↑
        default: return
        }
        next = max(0, min(127, next))
        if isDown {
            arrowsHint.setHighlight(direction: dir, on: true)
            if next != cur {
                m.setInstrumentPreview(UInt8(next))
            }
        } else {
            arrowsHint.setHighlight(direction: dir, on: false)
            m.setInstrumentPreview(nil)
            m.setMelodicProgram(UInt8(cur))
        }
    }

    /// Refresh the active-notes LCD from the controller. Hidden when
    /// nothing is held; otherwise lit with the held note names in
    /// the GM family color of the current voice (so the LCD readout
    /// reads as part of the same instrument).
    func refreshHeldNotes() {
        guard isViewLoaded, let m = menuBand else { return }
        qwertyMap?.litKeyCodes = m.heldKeyCodes()
        for v in heldNotesStack.arrangedSubviews {
            heldNotesStack.removeArrangedSubview(v)
            v.removeFromSuperview()
        }
        let safe = max(0, min(127, Int(m.melodicProgram)))
        let famColor: NSColor
        if m.midiMode {
            famColor = NSColor.controlAccentColor
        } else if m.instrumentBackend == .kpbj {
            famColor = NSColor.systemOrange
        } else {
            famColor = InstrumentListView.colorForProgram(safe)
        }
        // When 3+ notes form a recognized triad/seventh, the controller's
        // chord-detector returns a name like "Cmaj7" — show that as a
        // single banner box on top of the meter instead of three
        // separate note labels. Falls back to per-note boxes for
        // anything that doesn't resolve to a known shape (and for
        // 1-2 note plays where there's no chord to compute).
        if let chord = m.currentChordName() {
            heldNotesStack.addArrangedSubview(makeHeldNoteBox(name: chord,
                                                               key: nil,
                                                               color: famColor))
        } else {
            // Use the keyed entries so each held note shows the
            // bare pitch class (no octave) with the keyboard key
            // that played it stacked above.
            let entries = m.heldNoteEntries()
            for entry in entries {
                heldNotesStack.addArrangedSubview(
                    makeHeldNoteBox(name: entry.pitchClass,
                                    key: entry.keyLabel,
                                    color: famColor))
            }
        }

        // Staff sheet: held notes as solid heads + two ghost
        // suggestions for the circle-of-fifths neighbors of the
        // currently-played lowest note. Forward = +7 semitones (one
        // step CW round the circle, e.g. C → G), backward = -7
        // (one step CCW, e.g. C → F). Two faded letters at the
        // preview column, no chord-shape ghosting.
        guard let staff = staffView else { lastCompleteChordNames = []; return }
        var entries: [StaffView.Note] = []
        for entry in m.heldNoteEntries() {
            entries.append(.init(
                midi: entry.midi,
                pitchClass: entry.pitchClass,
                keyLabel: entry.keyLabel,
                ghost: false,
                color: nil
            ))
        }
        if let root = entries.min(by: { $0.midi < $1.midi }) {
            let labels = KeyboardIconRenderer.labelByMidi
            let pitchNames = MenuBandController.pitchClassNames
            // Wrap the suggestion into the playable two-octave
            // keymap range so each circle-of-fifths neighbor lands
            // on a real key the user can press. Outside the labeled
            // set we fold by octaves until we find one — if nothing
            // fits (range too narrow), we skip the ghost rather
            // than show an unreachable suggestion.
            let labeledMidis = labels.keys.sorted()
            guard let lo = labeledMidis.first,
                  let hi = labeledMidis.last else {
                staff.notes = entries
                lastCompleteChordNames = Set(m.chordCandidates(maxResults: 6)
                    .filter(\.isComplete).map(\.name))
                return
            }
            for delta in [7, -7] {
                var target = Int(root.midi) + delta
                while target < lo { target += 12 }
                while target > hi { target -= 12 }
                guard target >= lo, target <= hi else { continue }
                let mid = UInt8(target)
                let pc = target % 12
                entries.append(.init(
                    midi: mid,
                    pitchClass: pitchNames[pc],
                    keyLabel: labels[target],
                    ghost: true,
                    color: nil
                ))
            }
        }
        staff.notes = entries
        lastCompleteChordNames = Set(m.chordCandidates(maxResults: 6).filter(\.isComplete).map(\.name))
    }

    /// Per-chord-family color, for ghost notes on the staff.
    /// Major = green, minor = blue, dim = purple, dom7 = orange,
    /// maj7 = teal, sus = pink, fallback = gray.
    private func chordFamilyColor(for name: String) -> NSColor {
        let lower = name.lowercased()
        if lower.contains("maj7") {
            return NSColor(srgbRed: 0.18, green: 0.72, blue: 0.62, alpha: 1)
        }
        if lower.contains("min7") || lower.contains("m7") {
            return NSColor(srgbRed: 0.18, green: 0.40, blue: 0.85, alpha: 1)
        }
        if lower.contains("dim") {
            return NSColor(srgbRed: 0.62, green: 0.30, blue: 0.85, alpha: 1)
        }
        if lower.contains("aug") {
            return NSColor(srgbRed: 0.92, green: 0.32, blue: 0.78, alpha: 1)
        }
        if lower.contains("sus") {
            return NSColor(srgbRed: 0.95, green: 0.40, blue: 0.60, alpha: 1)
        }
        if lower.contains("7") {
            return NSColor(srgbRed: 0.96, green: 0.55, blue: 0.18, alpha: 1)
        }
        if lower.contains("min") || lower.hasSuffix("m") {
            return NSColor(srgbRed: 0.22, green: 0.52, blue: 0.95, alpha: 1)
        }
        return NSColor(srgbRed: 0.30, green: 0.78, blue: 0.36, alpha: 1)
    }

    /// Floating note badge — rounded layer-painted box with the
    /// pressed-key letter on top (smaller) and the bare pitch
    /// class on the bottom (large + heavy). `key` is optional
    /// because chord-name banners (e.g. "Cmaj7") don't have a
    /// single key letter.
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

        if let key = key, !key.isEmpty {
            let keyLabel = NSTextField(labelWithString: key)
            keyLabel.font = NSFont.monospacedSystemFont(ofSize: 12, weight: .bold)
            keyLabel.textColor = NSColor.white.withAlphaComponent(0.85)
            keyLabel.drawsBackground = false
            keyLabel.alignment = .center
            stack.addArrangedSubview(keyLabel)
        }

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
        updateFocusShortcutControls()
        updatePlayPaletteShortcutControls()
        applyAppearanceToVisualizer()
        updateInstrumentReadout()
        // Keep the QWERTY layout's keymap + tint synced with the
        // controller. Voice color picks up the family hue for the
        // current voice; keymap variant follows the controller.
        let safe = max(0, min(127, Int(n.melodicProgram)))
        qwertyMap?.keymap = n.keymap
        if n.midiMode {
            qwertyMap?.voiceColor = .controlAccentColor
        } else if n.instrumentBackend == .kpbj {
            qwertyMap?.voiceColor = NSColor.systemOrange
        } else {
            qwertyMap?.voiceColor = InstrumentListView.colorForProgram(safe)
        }
        updateSelfTestLabel(state: n.midiMode ? n.midiSelfTest : .unknown)
        refreshCrashStatus()
        refreshUpdateInfo()
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
        // in loadView() while the crash column was empty/hidden; a
        // multi-line crash hint can grow the layout and would otherwise
        // be clipped at the bottom of the popover.
        refitContentSize()
    }

    /// Re-measure the stack's intrinsic fitting size and update
    /// `preferredContentSize` to match. Run after any change that can
    /// add/remove rows or change wrapping height (crash status,
    /// instrument palette toggle).
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
    private func updateFocusShortcutControls(status: String? = nil) {
        guard focusShortcutButton != nil else { return }
        if isRecordingFocusShortcut {
            focusShortcutButton.title = L("popover.shortcuts.pressKeys")
        } else {
            focusShortcutButton.title = MenuBandShortcutPreferences.focusShortcut.displayString
        }
        focusShortcutStatusLabel.stringValue = status ?? ""
    }

    private func startFocusShortcutRecording() {
        guard focusShortcutRecorderMonitor == nil else { return }
        stopPlayPaletteShortcutRecording(status: nil)
        isRecordingFocusShortcut = true
        onFocusShortcutRecordingChanged?(true)
        updateFocusShortcutControls(status: L("popover.shortcuts.use"))
        focusShortcutRecorderMonitor = NSEvent.addLocalMonitorForEvents(
            matching: [.keyDown]
        ) { [weak self] event in
            self?.handleFocusShortcutRecording(event)
            return nil
        }
    }

    private func stopFocusShortcutRecording(status: String?) {
        guard isRecordingFocusShortcut || focusShortcutRecorderMonitor != nil else { return }
        if let monitor = focusShortcutRecorderMonitor {
            NSEvent.removeMonitor(monitor)
            focusShortcutRecorderMonitor = nil
        }
        isRecordingFocusShortcut = false
        onFocusShortcutRecordingChanged?(false)
        updateFocusShortcutControls(status: status)
    }

    private func handleFocusShortcutRecording(_ event: NSEvent) {
        if event.keyCode == UInt16(kVK_Escape) {
            stopFocusShortcutRecording(status: nil)
            return
        }
        let shortcut = MenuBandShortcut(
            keyCode: UInt32(event.keyCode),
            modifiers: MenuBandShortcut.carbonModifiers(from: event.modifierFlags)
        )
        guard shortcut.isValidForRecording else {
            updateFocusShortcutControls(status: L("popover.shortcuts.use"))
            return
        }
        guard !shortcut.isReservedForTypeMode else {
            updateFocusShortcutControls(status: L("popover.shortcuts.reserved"))
            return
        }
        let saved = onFocusShortcutChange?(shortcut) ?? false
        stopFocusShortcutRecording(
            status: saved
                ? L("popover.shortcuts.saved", shortcut.displayString)
                : L("popover.shortcuts.unavailable")
        )
    }
    private func updatePlayPaletteShortcutControls(status: String? = nil) {
        guard playPaletteShortcutButton != nil else { return }
        playPaletteToggleButton.title = (isPlayPaletteShown?() ?? false)
            ? L("popover.shortcuts.focusButton")
            : L("popover.shortcuts.show")
        if isRecordingPlayPaletteShortcut {
            playPaletteShortcutButton.title = L("popover.shortcuts.press")
        } else {
            playPaletteShortcutButton.title = MenuBandShortcutPreferences.playPaletteShortcut.displayString
        }
        playPaletteShortcutStatusLabel.stringValue = status ?? ""
    }

    private func startPlayPaletteShortcutRecording() {
        guard playPaletteShortcutRecorderMonitor == nil else { return }
        stopFocusShortcutRecording(status: nil)
        isRecordingPlayPaletteShortcut = true
        onPlayPaletteShortcutRecordingChanged?(true)
        updatePlayPaletteShortcutControls(status: "Use ⌘, ⌃, or ⌥")
        playPaletteShortcutRecorderMonitor = NSEvent.addLocalMonitorForEvents(
            matching: [.keyDown]
        ) { [weak self] event in
            self?.handlePlayPaletteShortcutRecording(event)
            return nil
        }
    }

    private func stopPlayPaletteShortcutRecording(status: String?) {
        guard isRecordingPlayPaletteShortcut || playPaletteShortcutRecorderMonitor != nil else { return }
        if let monitor = playPaletteShortcutRecorderMonitor {
            NSEvent.removeMonitor(monitor)
            playPaletteShortcutRecorderMonitor = nil
        }
        isRecordingPlayPaletteShortcut = false
        onPlayPaletteShortcutRecordingChanged?(false)
        updatePlayPaletteShortcutControls(status: status)
    }

    private func handlePlayPaletteShortcutRecording(_ event: NSEvent) {
        if event.keyCode == UInt16(kVK_Escape) {
            stopPlayPaletteShortcutRecording(status: nil)
            return
        }
        let shortcut = MenuBandShortcut(
            keyCode: UInt32(event.keyCode),
            modifiers: MenuBandShortcut.carbonModifiers(from: event.modifierFlags)
        )
        guard shortcut.isValidForRecording else {
            updatePlayPaletteShortcutControls(status: L("popover.shortcuts.use"))
            return
        }
        guard !shortcut.isReservedForTypeMode else {
            updatePlayPaletteShortcutControls(status: L("popover.shortcuts.reserved"))
            return
        }
        let saved = onPlayPaletteShortcutChange?(shortcut) ?? false
        stopPlayPaletteShortcutRecording(
            status: saved
                ? L("popover.shortcuts.saved", shortcut.displayString)
                : L("popover.shortcuts.unavailable")
        )
    }
    private func updateInstrumentReadout() {
        guard let m = menuBand else { return }
        let safe = max(0, min(127, Int(m.melodicProgram)))
        let title: String
        let famColor: NSColor
        if m.midiMode {
            // MIDI mode = "instrument 0" in the addressable system.
            // Title reads simply "MIDI" — short enough to fit and
            // makes the routing instantly legible.
            title = "MIDI"
            famColor = NSColor.controlAccentColor
        } else if m.instrumentBackend == .kpbj {
            // Voice −1: live KPBJ stream replaces the GM grid. Distinct
            // amber lets the user spot it immediately and fits the
            // KPBJ web piece's sunrise palette.
            title = "−1 KPBJ"
            famColor = NSColor.systemOrange
        } else {
            title = GeneralMIDI.programNames[safe]
            famColor = InstrumentListView.colorForProgram(safe)
        }
        applyInstrumentReadoutStyle(title: title, famColor: famColor)
        // The visualizer is the only piece of chrome that does NOT
        // track the voice color in MIDI mode — there it reads as a
        // status badge ("MIDI" dot-matrix in system accent), so we
        // skip the retint when MIDI is on.
        if m.midiMode {
            waveformBezel?.layer?.borderColor = NSColor.controlAccentColor
                .withAlphaComponent(0.55).cgColor
        } else {
            waveformBezel?.layer?.borderColor = famColor
                .withAlphaComponent(0.55).cgColor
        }
    }

    /// Paint the title chip with YWFT Processing + Riso-misregister
    /// shadow keyed to the family color. Both the committed-update path
    /// (`updateInstrumentReadout`) and the hover-drag preview path
    /// (`instrumentList.onHover`) funnel through here so the typeface +
    /// shadow can never get wiped by a stray `.stringValue` assignment.
    private func applyInstrumentReadoutStyle(title: String, famColor: NSColor) {
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
    }

    // Appearance changes (light/dark toggle) repaint live: the
    // popover root + the visualizer bezel + the keyboard deck all
    // hold layer-painted CGColors that were resolved once at
    // loadView, so a system-wide light/dark flip wouldn't otherwise
    // propagate to them. `viewDidChangeEffectiveAppearance` is
    // declared on NSResponder but not on NSViewController in our
    // SDK target — the popover's custom root view (which IS an
    // NSView and does inherit the override) drives this callback
    // for us. See `MenuBandPopoverRootView` below.
    /// Public entry point so AppDelegate's system-appearance
    /// observer can trigger a full retint directly. The
    /// `viewDidChangeEffectiveAppearance` propagation through
    /// `MenuBandPopoverRootView` is best-effort and sometimes
    /// drops on a system flip while the app is in the background.
    func forceAppearanceRetint() {
        handleEffectiveAppearanceChange()
    }

    fileprivate func handleEffectiveAppearanceChange() {
        rootBackgroundView?.layer?.backgroundColor =
            NSColor.windowBackgroundColor.cgColor
        applyAppearanceToVisualizer()
        refreshHeldNotes()
        updateInstrumentReadout()
        // Most popover children (QwertyLayoutView, InstrumentListView,
        // chord cards' draw passes) paint via `draw(_:)` using
        // dynamically-resolving NSColors like .labelColor and
        // .controlAccentColor. Those resolve against effectiveAppearance
        // *at draw time*, but AppKit doesn't auto-invalidate display
        // when appearance changes — so the views keep showing stale
        // light-mode glyphs over a dark-mode background until something
        // else nudges them. Walk the subtree once and force a redraw so
        // the whole popover lands on the new appearance in one beat.
        forceRedrawSubtree(rootBackgroundView)
    }

    private func forceRedrawSubtree(_ root: NSView?) {
        guard let root = root else { return }
        root.needsDisplay = true
        for sub in root.subviews { forceRedrawSubtree(sub) }
    }

    /// Held-notes / chord bezel — bg is now transparent so the
    /// pills + chord cards float over the popover material rather
    /// than sitting on a slate slab. The border still tracks
    /// appearance via `updateInstrumentReadout` so the family
    /// color stays visible as a hairline frame.
    private func applyAppearanceToVisualizer() {
        let isDark = view.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        waveformBezel?.layer?.backgroundColor = NSColor.clear.cgColor
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

    /// Badge-style link button — flat HoverLinkButton with a layer-painted
    /// fill + optional border, so the per-link attributed title sits
    /// inside a small chip. `bezelStyle = .inline` strips the system
    /// chrome; the layer below provides the badge look. On hover the
    /// background/border brighten and the cursor flips to pointing hand.
    static func makeLinkButton(attr: NSAttributedString,
                               target: AnyObject,
                               action: Selector,
                               background: NSColor? = nil,
                               border: NSColor? = nil) -> NSButton {
        let btn = HoverLinkButton()
        btn.bezelStyle = .inline
        btn.isBordered = false
        btn.controlSize = .small
        btn.target = target
        btn.action = action
        btn.attributedTitle = attr
        btn.wantsLayer = true
        btn.layer?.cornerRadius = 5
        let idleBg = background
            ?? NSColor.controlAccentColor.withAlphaComponent(0.06)
        let idleBd = border
        let hoverBg = idleBg.withAlphaComponent(
            min(1.0, idleBg.alphaComponent + 0.18))
        let hoverBd = idleBd?.withAlphaComponent(
            min(1.0, (idleBd?.alphaComponent ?? 0.5) + 0.25))
            ?? NSColor.controlAccentColor.withAlphaComponent(0.55)
        btn.idleBackground = idleBg
        btn.idleBorder = idleBd
        btn.hoverBackground = hoverBg
        btn.hoverBorder = hoverBd
        btn.layer?.backgroundColor = idleBg.cgColor
        if let bd = idleBd {
            btn.layer?.borderColor = bd.cgColor
            btn.layer?.borderWidth = 1
        } else {
            btn.layer?.borderWidth = 0
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
            crashSendButton.title = n == 1
                ? L("popover.about.crash.sendOne")
                : L("popover.about.crash.sendMany", String(n))
            crashSendButton.isEnabled = true
        }
    }

    /// Hit the manifest at assets.aesthetic.computer/menuband/latest.json
    /// and stash the result for the About panel to surface. Cached for
    /// an hour inside UpdateChecker.
    private func refreshUpdateInfo() {
        UpdateChecker.fetchLatest { [weak self] info in
            self?.latestRemoteVersion = info
        }
    }

    @objc private func openMenuBandSite() {
        if let url = URL(string: "https://prompt.ac/menuband") {
            NSWorkspace.shared.open(url)
        }
    }

    @objc private func languageChipClicked(_ sender: NSButton) {
        guard let id = sender.identifier?.rawValue else { return }
        let prefix = "menuband.lang."
        guard id.hasPrefix(prefix) else { return }
        let code = String(id.dropFirst(prefix.count))
        guard code != Localization.current else { return }
        Localization.current = code
    }

    @objc private func openKeymapsPaper(_ sender: NSButton) {
        // Prefer the copy bundled inside the app — opens in Preview offline,
        // no network round-trip — then fall back to the public hosted PDF if
        // the bundled resource somehow goes missing.
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

    @objc private func sendCrashLogs(_ sender: NSButton) {
        let logs = CrashLogReader.recentLogs()
        guard !logs.isEmpty else { return }
        sender.isEnabled = false
        sender.title = L("popover.about.crash.sending")

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
                        ? L("popover.about.crash.sentAll")
                        : L("popover.about.crash.sentSome",
                            String(ok), String(logs.count))
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

    /// Programmatic toggle of the metronome's play/pause — wired from
    /// the spacebar shortcut in AppDelegate's local key handler so
    /// users can start/stop the swing without aiming the mouse.
    func toggleMetronome() {
        metronome?.isPlaying.toggle()
    }

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
        // The bezel housing dims with the rest of the palette; chord
        // cards / held-notes pills inside it inherit the dim via their
        // parent. `_ = animated` keeps the parameter signature
        // compatible with existing call sites.
        waveformBezel?.alphaValue = dimmed
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

    @objc private func focusShortcutButtonClicked(_ sender: NSButton) {
        if isRecordingFocusShortcut {
            stopFocusShortcutRecording(status: nil)
        } else {
            startFocusShortcutRecording()
        }
    }

    @objc private func playPaletteToggleButtonClicked(_ sender: NSButton) {
        onPlayPaletteToggle?()
        updatePlayPaletteShortcutControls()
    }


    @objc private func playPaletteShortcutButtonClicked(_ sender: NSButton) {
        if isRecordingPlayPaletteShortcut {
            stopPlayPaletteShortcutRecording(status: nil)
        } else {
            startPlayPaletteShortcutRecording()
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

    /// Single source-of-truth wiring for the popover's MIDI-mode
    /// visual state. The MTL meter that this used to drive is gone;
    /// only the bezel border tint remains so MIDI mode still reads
    /// as "accent-colored" while voice mode reads as family-colored.
    func applyVisualizerForMidiMode(_ midiOn: Bool) {
        guard let m = menuBand else { return }
        if midiOn {
            waveformBezel?.layer?.borderColor = NSColor.controlAccentColor
                .withAlphaComponent(0.55).cgColor
        } else {
            let safe = max(0, min(127, Int(m.melodicProgram)))
            let famColor = InstrumentListView.colorForProgram(safe)
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

    /// Classic macOS About panel — bundle icon, name, version, plus a
    /// credits block carrying the "Menu Band brings the built-in macOS
    /// instruments…" line and a clickable aesthetic.computer link.
    /// Replaces the inline AC chip that used to live in the popover.
    @objc private func showAboutPanel(_ sender: Any?) {
        // Kick off a fresh update check; if it lands before the user
        // dismisses the window the next open will reflect it. The first
        // open after launch shows whatever sync-time call cached.
        refreshUpdateInfo()

        // Rebuild every open so the flashing button (and version row)
        // pick up the most recent update info instead of going stale.
        aboutWindowController?.close()
        let ctrl = AboutWindowController(
            updateInfo: latestRemoteVersion,
            onOpenPlugins: { [weak self] in
                self?.menuBand?.presentPluginPicker()
            }
        )
        aboutWindowController = ctrl
        ctrl.present()
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

/// Custom NSView used as the popover root so we can repaint layer-
/// painted CGColors when the system flips light/dark mid-session.
/// NSView (via NSResponder) receives viewDidChangeEffectiveAppearance
/// callbacks; NSViewController in our SDK target does not, so the
/// root view forwards them through `onAppearanceChange` to the
/// popover view controller.
final class MenuBandPopoverRootView: NSView {
    var onAppearanceChange: (() -> Void)?
    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        onAppearanceChange?()
    }
}
