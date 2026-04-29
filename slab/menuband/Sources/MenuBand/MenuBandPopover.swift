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

    private var inputSegmented: HoverSegmentedControl!
    private var midiSwitch: NSSwitch!
    private var midiSelfTestLabel: NSTextField!
    private var instrumentList: InstrumentListView!
    private var instrumentReadout: NSTextField!
    private var octaveStepper: NSStepper!
    private var octaveLabel: NSTextField!
    private var keyMonitor: Any?

    override func loadView() {
        // Plain solid-color background — no NSVisualEffectView. The visual
        // effect view sampled the surrounding context and shifted appearance
        // when focus moved between the menu bar and the popover. A flat
        // background keeps the popover homogeneous in all states.
        let root = NSView()
        root.wantsLayer = true
        root.layer?.backgroundColor = NSColor.windowBackgroundColor.cgColor
        root.translatesAutoresizingMaskIntoConstraints = false

        // Vertical stack of rows.
        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 10
        stack.edgeInsets = NSEdgeInsets(top: 14, left: 16, bottom: 14, right: 16)
        stack.translatesAutoresizingMaskIntoConstraints = false
        root.addSubview(stack)

        // Title row.
        let title = NSTextField(labelWithString: "Menu Band")
        title.font = NSFont.systemFont(ofSize: 13, weight: .bold)
        title.textColor = .labelColor
        stack.addArrangedSubview(title)

        let subtitle = NSTextField(labelWithString: "Built-in macOS instruments, in the menu bar.")
        subtitle.font = NSFont.systemFont(ofSize: 10.5)
        subtitle.textColor = .secondaryLabelColor
        stack.addArrangedSubview(subtitle)

        stack.addArrangedSubview(makeSeparator())

        // Input mode picker. Three states:
        //   Pointer  — mouse only, two octaves (Notepat range)
        //   Notepat  — global keystroke capture, two octaves
        //   Ableton  — global keystroke capture, one octave (Live's M-mode)
        // Hovering a segment previews that mode in the menubar piano (range
        // shrinks/grows, letter labels appear) and lets you tap keys for a
        // quick demo without committing.
        let inputLabel = NSTextField(labelWithString: "Input")
        inputLabel.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        inputLabel.textColor = .labelColor
        stack.addArrangedSubview(inputLabel)

        inputSegmented = HoverSegmentedControl(
            labels: ["Pointer", "Notepat", "Ableton"],
            trackingMode: .selectOne,
            target: self,
            action: #selector(inputModeChanged(_:))
        )
        inputSegmented.translatesAutoresizingMaskIntoConstraints = false
        inputSegmented.onHoverChange = { [weak self] seg in
            self?.handleInputHover(segment: seg)
        }
        stack.addArrangedSubview(inputSegmented)
        inputSegmented.widthAnchor.constraint(equalToConstant: 248).isActive = true

        let inputHint = NSTextField(labelWithString:
            "Hover to preview · ⌃⌥⌘P toggles last keystrokes mode")
        inputHint.font = NSFont.systemFont(ofSize: 10)
        inputHint.textColor = .secondaryLabelColor
        stack.addArrangedSubview(inputHint)

        stack.addArrangedSubview(makeSeparator())

        // MIDI switch row.
        midiSwitch = NSSwitch()
        midiSwitch.target = self
        midiSwitch.action = #selector(midiSwitchToggled(_:))
        stack.addArrangedSubview(makeSwitchRow(
            label: "Send MIDI to DAW",
            sublabel: "Routes via virtual port",
            switchControl: midiSwitch
        ))

        // MIDI self-test status — populated by the controller after each
        // toggle-on. Empty when MIDI is off.
        midiSelfTestLabel = NSTextField(labelWithString: "")
        midiSelfTestLabel.font = NSFont.systemFont(ofSize: 10.5)
        midiSelfTestLabel.textColor = .secondaryLabelColor
        midiSelfTestLabel.lineBreakMode = .byWordWrapping
        midiSelfTestLabel.maximumNumberOfLines = 2
        stack.addArrangedSubview(midiSelfTestLabel)

        stack.addArrangedSubview(makeSeparator())

        // Instrument named-list. All 128 GM programs in a scrollable list,
        // family-colored stripe on the left, name on the right. Hover plays
        // a preview note; click commits. Compact (~180 px window) so the
        // popover stays small even though the full list is much taller.
        let instrumentLabel = NSTextField(labelWithString: "Instrument")
        instrumentLabel.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        instrumentLabel.textColor = .labelColor
        stack.addArrangedSubview(instrumentLabel)

        instrumentList = InstrumentListView()
        instrumentList.onHover = { [weak self] prog in
            self?.handleInstrumentHover(prog)
        }
        instrumentList.onCommit = { [weak self] prog in
            self?.handleInstrumentCommit(prog)
        }
        let scroll = NSScrollView()
        scroll.translatesAutoresizingMaskIntoConstraints = false
        scroll.hasVerticalScroller = true
        scroll.hasHorizontalScroller = false
        scroll.autohidesScrollers = true
        scroll.borderType = .lineBorder
        scroll.scrollerStyle = .overlay
        scroll.documentView = instrumentList
        stack.addArrangedSubview(scroll)
        scroll.widthAnchor.constraint(equalToConstant: InstrumentListView.preferredWidth).isActive = true
        scroll.heightAnchor.constraint(equalToConstant: 180).isActive = true

        stack.addArrangedSubview(makeSeparator())

        // Octave row.
        let octaveRow = NSStackView()
        octaveRow.orientation = .horizontal
        octaveRow.alignment = .centerY
        octaveRow.spacing = 8
        let octaveTitle = NSTextField(labelWithString: "Octave")
        octaveTitle.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        octaveTitle.textColor = .labelColor
        octaveLabel = NSTextField(labelWithString: "+0")
        octaveLabel.font = NSFont.monospacedSystemFont(ofSize: 11, weight: .bold)
        octaveLabel.textColor = .labelColor
        octaveLabel.alignment = .center
        octaveLabel.widthAnchor.constraint(equalToConstant: 28).isActive = true
        octaveStepper = NSStepper()
        octaveStepper.minValue = -4
        octaveStepper.maxValue = 4
        octaveStepper.increment = 1
        octaveStepper.valueWraps = false
        octaveStepper.target = self
        octaveStepper.action = #selector(octaveChanged(_:))
        let octaveReset = NSButton(title: "Reset", target: self, action: #selector(resetOctave))
        octaveReset.bezelStyle = .recessed
        octaveReset.controlSize = .small
        octaveRow.addArrangedSubview(octaveTitle)
        octaveRow.addArrangedSubview(octaveLabel)
        octaveRow.addArrangedSubview(octaveStepper)
        octaveRow.addArrangedSubview(octaveReset)
        stack.addArrangedSubview(octaveRow)

        stack.addArrangedSubview(makeSeparator())

        // About — inline rather than a separate dialog.
        let aboutTitle = NSTextField(labelWithString: "About")
        aboutTitle.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        aboutTitle.textColor = .labelColor
        stack.addArrangedSubview(aboutTitle)

        let aboutBody = NSTextField(wrappingLabelWithString:
            "A political project to bring the built-in macOS instruments — " +
            "the ones GarageBand uses — into the menu bar. Accessible " +
            "music-making is as essential as time, network connectivity, " +
            "and battery life.")
        aboutBody.font = NSFont.systemFont(ofSize: 10.5)
        aboutBody.textColor = .secondaryLabelColor
        aboutBody.maximumNumberOfLines = 0
        aboutBody.preferredMaxLayoutWidth = 248
        stack.addArrangedSubview(aboutBody)

        let linksRow = NSStackView()
        linksRow.orientation = .horizontal
        linksRow.alignment = .centerY
        linksRow.spacing = 8
        let acLink = NSButton(title: "aesthetic.computer",
                              target: self, action: #selector(openAesthetic))
        acLink.bezelStyle = .recessed
        acLink.controlSize = .small
        let npLink = NSButton(title: "notepat.com",
                              target: self, action: #selector(openNotepat))
        npLink.bezelStyle = .recessed
        npLink.controlSize = .small
        linksRow.addArrangedSubview(acLink)
        linksRow.addArrangedSubview(npLink)
        stack.addArrangedSubview(linksRow)

        stack.addArrangedSubview(makeSeparator())

        // Quit — red stop button so it's visually distinct from the
        // toggles/links above.
        let quit = NSButton()
        quit.title = "Quit Menu Band"
        quit.image = NSImage(systemSymbolName: "stop.circle.fill",
                             accessibilityDescription: "Quit")
        quit.imagePosition = .imageLeading
        quit.bezelStyle = .recessed
        quit.controlSize = .small
        quit.contentTintColor = NSColor.systemRed
        quit.target = self
        quit.action = #selector(quitApp)
        let quitTitle = NSAttributedString(
            string: "Quit Menu Band",
            attributes: [
                .foregroundColor: NSColor.systemRed,
                .font: NSFont.systemFont(ofSize: 11, weight: .semibold),
            ]
        )
        quit.attributedTitle = quitTitle
        stack.addArrangedSubview(quit)

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
        preferredContentSize = NSSize(width: max(280, fitting.width),
                                       height: fitting.height)
    }

    /// Refresh control state from the controller — call right before showing.
    func syncFromController() {
        guard isViewLoaded, let n = menuBand else { return }
        midiSwitch.state = n.midiMode ? .on : .off
        octaveStepper.integerValue = n.octaveShift
        updateOctaveLabel(n.octaveShift)
        inputSegmented.selectedSegment = inputModeSegment(typeMode: n.typeMode,
                                                           keymap: n.keymap)
        instrumentList.selectedProgram = n.melodicProgram
        updateInstrumentReadout(program: nil)
        instrumentList.scrollProgramIntoView(n.melodicProgram)
        updateSelfTestLabel(state: n.midiMode ? n.midiSelfTest : .unknown)
        // Wire up live updates so the label reflects loopback results as
        // they land (test runs ~50ms after toggle-on; result settles a moment
        // later).
        n.onSelfTestChanged = { [weak self] in
            DispatchQueue.main.async {
                guard let self = self, let nn = self.menuBand else { return }
                self.updateSelfTestLabel(state: nn.midiMode ? nn.midiSelfTest : .unknown)
            }
        }
    }

    private func updateSelfTestLabel(state: MenuBandController.MIDISelfTest) {
        guard let label = midiSelfTestLabel else { return }
        switch state {
        case .unknown:
            label.stringValue = ""
        case .running:
            label.stringValue = "Testing MIDI port…"
            label.textColor = .secondaryLabelColor
        case .ok(let ms):
            label.stringValue = "✓ MIDI port working (loopback \(ms)ms)"
            label.textColor = .systemGreen
        case .failed:
            label.stringValue = "✗ MIDI loopback failed — port may be blocked"
            label.textColor = .systemRed
        }
    }

    // MARK: - Builders

    private func makeSeparator() -> NSView {
        let box = NSBox()
        box.boxType = .separator
        return box
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

    private func updateOctaveLabel(_ shift: Int) {
        let s: String
        if shift == 0 { s = "0" }
        else if shift > 0 { s = "+\(shift)" }
        else { s = "\(shift)" }
        octaveLabel.stringValue = s
    }

    // MARK: - Actions

    @objc private func midiSwitchToggled(_ sender: NSSwitch) {
        menuBand?.toggleMIDIMode()
        syncFromController()
    }

    /// 0 = Pointer, 1 = Notepat, 2 = Ableton. Matches the segmented control
    /// in `loadView()`.
    private func inputModeSegment(typeMode: Bool, keymap: Keymap) -> Int {
        if !typeMode { return 0 }
        return keymap == .ableton ? 2 : 1
    }

    @objc private func inputModeChanged(_ sender: NSSegmentedControl) {
        guard let m = menuBand else { return }
        // Clicking commits — clear any in-flight hover preview first so the
        // controller's effective state isn't shadowed by a stale overlay.
        m.clearHoverPreview()
        switch sender.selectedSegment {
        case 0:  // Pointer
            if m.typeMode { m.toggleTypeMode() }
            // Pointer mode keeps the existing keymap so the piano range stays
            // wherever the user last set it (default .notepat = 2 octaves).
        case 1:  // Notepat
            m.keymap = .notepat
            if !m.typeMode { m.toggleTypeMode() }
        case 2:  // Ableton
            m.keymap = .ableton
            if !m.typeMode { m.toggleTypeMode() }
        default: break
        }
        syncFromController()
    }

    private func handleInputHover(segment: Int?) {
        guard let m = menuBand else { return }
        guard let seg = segment else {
            m.clearHoverPreview()
            return
        }
        switch seg {
        case 0:  // Pointer — preview as no-typeMode, keep current keymap range
            m.setHoverPreview(typeMode: false, keymap: m.keymap)
        case 1:
            m.setHoverPreview(typeMode: true, keymap: .notepat)
        case 2:
            m.setHoverPreview(typeMode: true, keymap: .ableton)
        default: break
        }
    }

    private func handleInstrumentHover(_ program: Int?) {
        // Forward to controller — handles preview state machine + audio.
        // Pass UInt8 or nil to clear preview.
        if let p = program {
            menuBand?.setInstrumentPreview(UInt8(p))
        } else {
            menuBand?.setInstrumentPreview(nil)
        }
        updateInstrumentReadout(program: program.map { UInt8($0) })
    }

    private func handleInstrumentCommit(_ program: Int) {
        guard let m = menuBand else { return }
        m.setMelodicProgram(UInt8(program))
        instrumentList.selectedProgram = UInt8(program)
        updateInstrumentReadout(program: nil)
        // Sound the freshly-picked instrument out — a half-second middle-C
        // confirmation note in the new program. Lets the user actually hear
        // their commit instead of just seeing a row highlight.
        let note: UInt8 = 60
        m.startTapNote(note, velocity: 90, pan: 64)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.6) { [weak m] in
            m?.stopTapNote(note)
        }
    }

    /// Show the hovered program name (or the committed one if not hovering).
    private func updateInstrumentReadout(program: UInt8?) {
        let p: Int
        if let h = program {
            p = Int(h)
        } else if let n = menuBand {
            p = Int(n.melodicProgram)
        } else {
            p = 0
        }
        let safe = max(0, min(127, p))
        let name = GeneralMIDI.programNames[safe]
        instrumentReadout.stringValue = String(format: "%03d  %@", safe, name)
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

    // MARK: - Local key demo (only fires while hovering an input segment)

    override func viewDidAppear() {
        super.viewDidAppear()
        // Local monitor: catches keystrokes while the popover window is key.
        // We only consume them when a hover preview is active, so popover
        // navigation (Esc, arrows on focused controls) keeps working when
        // no segment is hovered.
        keyMonitor = NSEvent.addLocalMonitorForEvents(matching: [.keyDown, .keyUp]) { [weak self] event in
            guard let self = self, let m = self.menuBand else { return event }
            // Modifier combos pass through (Cmd-Q etc still work).
            let mods: NSEvent.ModifierFlags = [.command, .control, .option]
            if !event.modifierFlags.intersection(mods).isEmpty { return event }
            // Only consume while hover-previewing Notepat or Ableton.
            guard m.isHoveringTypingMode else { return event }
            if event.isARepeat { return nil }
            m.previewPlayKey(keyCode: event.keyCode, isDown: event.type == .keyDown)
            return nil
        }
    }

    override func viewWillDisappear() {
        super.viewWillDisappear()
        if let m = keyMonitor {
            NSEvent.removeMonitor(m)
            keyMonitor = nil
        }
        // Ensure hover previews don't survive popover dismissal — clear
        // both the input-mode preview and the instrument preview, otherwise
        // a held preview note can keep ringing after the popover closes.
        menuBand?.clearHoverPreview()
        menuBand?.setInstrumentPreview(nil)
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
