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
    private var midiInlineLabel: NSTextField!
    private var midiSelfTestLabel: NSTextField!  // legacy — created but never added to stack
    private var instrumentList: InstrumentListView!
    private var octaveStepper: NSStepper!
    private var octaveLabel: NSTextField!
    private var crashStatusLabel: NSTextField!
    private var crashHintLabel: NSTextField!
    private var crashSendButton: NSButton!
    private var updateBanner: NSView!
    private var updateLabel: NSTextField!
    private var waveformView: WaveformView!

    override func loadView() {
        // Plain solid-color background — no NSVisualEffectView. The visual
        // effect view sampled the surrounding context and shifted appearance
        // when focus moved between the menu bar and the popover. A flat
        // background keeps the popover homogeneous in all states.
        let root = NSView()
        root.wantsLayer = true
        root.layer?.backgroundColor = NSColor.windowBackgroundColor.cgColor
        root.translatesAutoresizingMaskIntoConstraints = false

        // Vertical stack of rows. Tight spacing + smaller edge insets so
        // the popover doesn't carry a lot of negative space.
        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 6
        stack.edgeInsets = NSEdgeInsets(top: 10, left: 12, bottom: 10, right: 12)
        stack.translatesAutoresizingMaskIntoConstraints = false
        root.addSubview(stack)

        // Title row: app name on the left, octave control hugging the right.
        // Octave gets its own row in the popover via this top-anchored
        // arrangement so we don't need a dedicated "Octave" panel below.
        let titleRow = NSStackView()
        titleRow.orientation = .horizontal
        titleRow.alignment = .centerY
        titleRow.distribution = .fill
        titleRow.spacing = 8

        let titleStack = NSStackView()
        titleStack.orientation = .vertical
        titleStack.alignment = .leading
        titleStack.spacing = 0
        let title = NSTextField(labelWithString: "Menu Band")
        title.font = NSFont.systemFont(ofSize: 13, weight: .bold)
        title.textColor = .labelColor
        let subtitle = NSTextField(labelWithString: "Built-in macOS instruments, in the menu bar.")
        subtitle.font = NSFont.systemFont(ofSize: 10.5)
        subtitle.textColor = .secondaryLabelColor
        titleStack.addArrangedSubview(title)
        titleStack.addArrangedSubview(subtitle)
        titleRow.addArrangedSubview(titleStack)

        let titleSpacer = NSView()
        titleSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        titleRow.addArrangedSubview(titleSpacer)

        // Compact octave: large monospaced number, then a tight pair of
        // chevron arrows to its right. NSStepper kept invisibly as the
        // value model so the rest of the controller's API doesn't change;
        // the visible buttons just nudge its `integerValue`.
        octaveLabel = NSTextField(labelWithString: "+0")
        // Fully monospaced so "+", "-", and digits all carry the same
        // advance — value flips don't slide left or right between the
        // arrows. No fixed width — intrinsic content width hugs the text
        // tightly so the 4 px spacing on each side stays exactly 4 px,
        // not "4 + half-of-padding".
        octaveLabel.font = NSFont.monospacedSystemFont(ofSize: 17, weight: .semibold)
        octaveLabel.textColor = .labelColor
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

        // Arrows flank the number — left arrow, then big number, then
        // right arrow. Tight 4 px gap on each side so the trio reads as
        // a single widget instead of three separate buttons.
        titleRow.addArrangedSubview(leftArrow)
        titleRow.setCustomSpacing(4, after: leftArrow)
        titleRow.addArrangedSubview(octaveLabel)
        titleRow.setCustomSpacing(4, after: octaveLabel)
        titleRow.addArrangedSubview(rightArrow)
        titleRow.addArrangedSubview(octaveStepper)  // hidden, here for layout-time only

        // MIDI toggle — tucked into the title row instead of its own panel.
        // Tiny label + switch, no spacer between (sits flush right of the
        // octave arrows so the row stays compact).
        midiSwitch = NSSwitch()
        midiSwitch.target = self
        midiSwitch.action = #selector(midiSwitchToggled(_:))
        midiInlineLabel = NSTextField(labelWithString: "MIDI")
        midiInlineLabel.font = NSFont.systemFont(ofSize: 10, weight: .semibold)
        midiInlineLabel.textColor = .secondaryLabelColor
        titleRow.setCustomSpacing(12, after: rightArrow)
        titleRow.addArrangedSubview(midiInlineLabel)
        titleRow.addArrangedSubview(midiSwitch)

        stack.addArrangedSubview(titleRow)
        titleRow.widthAnchor.constraint(equalTo: stack.widthAnchor,
                                         constant: -24).isActive = true

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
        updateBanner.widthAnchor.constraint(equalToConstant: 248).isActive = true
        updateBanner.isHidden = true

        stack.addArrangedSubview(makeSeparator())

        // Input mode picker. Three states:
        //   Pointer      — mouse only, two octaves (Notepat range)
        //   Notepat.com  — global keystroke capture, two octaves
        //   Ableton      — global keystroke capture, one octave (Live's M-mode)
        // Hovering a segment previews that mode in the menubar piano (range
        // shrinks/grows, letter labels appear) and lets you tap keys for a
        // quick demo without committing.
        let inputLabel = NSTextField(labelWithString: "Keyboard & Mouse")
        inputLabel.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        inputLabel.textColor = .labelColor
        stack.addArrangedSubview(inputLabel)

        inputSegmented = HoverSegmentedControl(
            labels: ["Mouse Only", "Notepat.com", "Ableton"],
            trackingMode: .selectOne,
            target: self,
            action: #selector(inputModeChanged(_:))
        )
        inputSegmented.translatesAutoresizingMaskIntoConstraints = false
        // No hover preview — clicks commit the mode directly.
        stack.addArrangedSubview(inputSegmented)
        inputSegmented.widthAnchor.constraint(equalToConstant: 248).isActive = true

        let inputHint = NSTextField(labelWithString:
            "⌃⌥⌘P toggles last keystrokes mode")
        inputHint.font = NSFont.systemFont(ofSize: 10)
        inputHint.textColor = .secondaryLabelColor
        stack.addArrangedSubview(inputHint)

        // Live waveform of the local synth output. Hidden in MIDI mode
        // (DAW handles audio there; our local mixer is silent so the line
        // would just sit flat). Single antialiased path, ~60 Hz redraw.
        waveformView = WaveformView()
        waveformView.menuBand = menuBand
        waveformView.translatesAutoresizingMaskIntoConstraints = false
        stack.addArrangedSubview(waveformView)
        waveformView.widthAnchor.constraint(equalToConstant: InstrumentListView.preferredWidth).isActive = true
        waveformView.heightAnchor.constraint(equalToConstant: 64).isActive = true

        stack.addArrangedSubview(makeSeparator())

        // (MIDI switch lives in the title row above — see octave + MIDI block.)

        // MIDI self-test status — populated by the controller after each
        // toggle-on. Empty when MIDI is off.
        // Legacy textual self-test label is allocated but never added to
        // the popover stack — the test outcome is surfaced as the MIDI
        // label color in the title row instead (green = ok, red = failed).
        midiSelfTestLabel = NSTextField(labelWithString: "")

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

        // About + Crash logs in a side-by-side row to save vertical space.
        // Each takes half the popover width. Crash column is hidden when
        // there are no recent reports, leaving About full-width.
        let aboutCrashRow = NSStackView()
        aboutCrashRow.orientation = .horizontal
        aboutCrashRow.alignment = .top
        aboutCrashRow.distribution = .fillEqually
        aboutCrashRow.spacing = 12

        let aboutCol = NSStackView()
        aboutCol.orientation = .vertical
        aboutCol.alignment = .leading
        aboutCol.spacing = 4
        let aboutTitle = NSTextField(labelWithString: "About")
        aboutTitle.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        aboutTitle.textColor = .labelColor
        let aboutBody = NSTextField(wrappingLabelWithString:
            "A political project to bring the built-in macOS instruments — " +
            "the ones GarageBand uses — into the menu bar. Free + open source.")
        aboutBody.font = NSFont.systemFont(ofSize: 10)
        aboutBody.textColor = .secondaryLabelColor
        aboutBody.maximumNumberOfLines = 0
        aboutBody.preferredMaxLayoutWidth = 200
        aboutCol.addArrangedSubview(aboutTitle)
        aboutCol.addArrangedSubview(aboutBody)
        let linksRow = NSStackView()
        linksRow.orientation = .horizontal
        linksRow.alignment = .centerY
        linksRow.spacing = 6
        let acLink = NSButton(title: "ac",
                              target: self, action: #selector(openAesthetic))
        acLink.bezelStyle = .recessed
        acLink.controlSize = .small
        let npLink = NSButton(title: "notepat.com",
                              target: self, action: #selector(openNotepat))
        npLink.bezelStyle = .recessed
        npLink.controlSize = .small
        linksRow.addArrangedSubview(acLink)
        linksRow.addArrangedSubview(npLink)
        aboutCol.addArrangedSubview(linksRow)

        let crashCol = NSStackView()
        crashCol.orientation = .vertical
        crashCol.alignment = .leading
        crashCol.spacing = 4
        crashStatusLabel = NSTextField(labelWithString: "")
        crashStatusLabel.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        crashStatusLabel.textColor = .labelColor
        crashHintLabel = NSTextField(wrappingLabelWithString: "")
        crashHintLabel.font = NSFont.systemFont(ofSize: 10)
        crashHintLabel.textColor = .secondaryLabelColor
        crashHintLabel.maximumNumberOfLines = 0
        crashHintLabel.preferredMaxLayoutWidth = 200
        crashSendButton = NSButton(title: "Send crash reports",
                                   target: self,
                                   action: #selector(sendCrashLogs(_:)))
        crashSendButton.bezelStyle = .recessed
        crashSendButton.controlSize = .small
        crashCol.addArrangedSubview(crashStatusLabel)
        crashCol.addArrangedSubview(crashHintLabel)
        crashCol.addArrangedSubview(crashSendButton)

        aboutCrashRow.addArrangedSubview(aboutCol)
        aboutCrashRow.addArrangedSubview(crashCol)
        stack.addArrangedSubview(aboutCrashRow)

        // Quit — small, borderless, bottom-right. Red text only.
        let quit = NSButton()
        quit.bezelStyle = .inline
        quit.isBordered = false
        quit.controlSize = .small
        quit.target = self
        quit.action = #selector(quitApp)
        quit.attributedTitle = NSAttributedString(
            string: "Quit",
            attributes: [
                .foregroundColor: NSColor.systemRed,
                .font: NSFont.systemFont(ofSize: 11, weight: .semibold),
            ]
        )
        let quitRow = NSStackView()
        quitRow.orientation = .horizontal
        let quitSpacer = NSView()
        quitSpacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
        quitRow.addArrangedSubview(quitSpacer)
        quitRow.addArrangedSubview(quit)
        stack.addArrangedSubview(quitRow)
        quitRow.widthAnchor.constraint(equalTo: stack.widthAnchor,
                                       constant: -24).isActive = true

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
        instrumentList.scrollProgramIntoView(n.melodicProgram)
        updateSelfTestLabel(state: n.midiMode ? n.midiSelfTest : .unknown)
        refreshCrashStatus()
        refreshUpdateBanner()
        // Waveform: live only when local synth is the audible path. MIDI
        // mode silences the local mixer, so the line would be flat — hide
        // it instead of showing a misleading dead waveform.
        waveformView.isHidden = n.midiMode
        waveformView.isLive = !n.midiMode
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

    /// Update the crash-log status row from disk. Called on every popover
    /// open so the count is current. When there are zero crashes, the
    /// whole crash column hides — About sits side-by-side normally; when
    /// crashes are present, About narrows to share the row.
    private func refreshCrashStatus() {
        let logs = CrashLogReader.recentLogs()
        let n = logs.count
        // Walk up to the parent crash column to hide/show the whole panel.
        let crashCol = crashStatusLabel?.superview
        if n == 0 {
            crashCol?.isHidden = true
            crashSendButton.isHidden = true
        } else {
            crashCol?.isHidden = false
            crashStatusLabel.stringValue = n == 1 ? "1 crash" : "\(n) crashes"
            crashHintLabel.stringValue = "Send to aesthetic.computer to help debug."
            crashSendButton.isHidden = false
            crashSendButton.title = n == 1 ? "Send 1" : "Send all (\(n))"
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
        let s: String
        if shift == 0 { s = "0" }
        else if shift > 0 { s = "+\(shift)" }
        else { s = "\(shift)" }
        octaveLabel.stringValue = s
    }

    // MARK: - Actions

    @objc private func midiSwitchToggled(_ sender: NSSwitch) {
        // Just toggle — don't run the heavy syncFromController. The switch
        // already shows the user's intent; the loopback test (skipped on
        // toggles after the first per session) and other panels don't need
        // to refresh.
        menuBand?.toggleMIDIMode()
        // Waveform follows the new MIDI state directly so it appears /
        // disappears the moment the user flips the switch.
        if let m = menuBand {
            waveformView.isHidden = m.midiMode
            waveformView.isLive = !m.midiMode
        }
    }

    /// 0 = Pointer, 1 = Notepat, 2 = Ableton. Matches the segmented control
    /// in `loadView()`.
    private func inputModeSegment(typeMode: Bool, keymap: Keymap) -> Int {
        if !typeMode { return 0 }
        return keymap == .ableton ? 2 : 1
    }

    @objc private func inputModeChanged(_ sender: NSSegmentedControl) {
        guard let m = menuBand else { return }
        switch sender.selectedSegment {
        case 0:  // Pointer
            if m.typeMode { m.toggleTypeMode() }
        case 1:  // Notepat.com
            m.keymap = .notepat
            if !m.typeMode { m.toggleTypeMode() }
        case 2:  // Ableton
            m.keymap = .ableton
            if !m.typeMode { m.toggleTypeMode() }
        default: break
        }
        // No syncFromController — segmented control already reflects the
        // user's click and the rest of the popover doesn't need to refresh.
    }

    private func handleInstrumentCommit(_ program: Int) {
        guard let m = menuBand else { return }
        m.setMelodicProgram(UInt8(program))
        instrumentList.selectedProgram = UInt8(program)
        debugLog("instrument commit prog=\(program)")
        // setMelodicProgram → loadSoundBankInstrument is synchronous on the
        // calling thread, but AVAudioUnitSampler briefly drops scheduled
        // notes on the audio render thread while it swaps banks. Without
        // this small delay the audition note often falls into that gap and
        // the user hears nothing. 70 ms is enough for the swap to settle
        // on every Mac I've tested without feeling laggy.
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.07) { [weak m] in
            m?.auditionCurrentProgram()
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
