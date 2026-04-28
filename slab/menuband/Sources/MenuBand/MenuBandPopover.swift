import AppKit

/// Settings popover for the menubar piano. Custom NSViewController with
/// native AppKit controls (NSSwitch / NSPopUpButton / NSStepper) for a
/// richer feel than a plain NSMenu.
final class MenuBandPopoverViewController: NSViewController {
    weak var menuBand: MenuBandController?

    private var typeSwitch: NSSwitch!
    private var midiSwitch: NSSwitch!
    private var midiSelfTestLabel: NSTextField!
    private var instrumentPopUp: NSPopUpButton!
    private var octaveStepper: NSStepper!
    private var octaveLabel: NSTextField!
    private var keymapSegmented: NSSegmentedControl!

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
        let title = NSTextField(labelWithString: "MenuBand")
        title.font = NSFont.systemFont(ofSize: 13, weight: .bold)
        title.textColor = .labelColor
        stack.addArrangedSubview(title)

        let subtitle = NSTextField(labelWithString: "Built-in macOS instruments, in the menu bar.")
        subtitle.font = NSFont.systemFont(ofSize: 10.5)
        subtitle.textColor = .secondaryLabelColor
        stack.addArrangedSubview(subtitle)

        stack.addArrangedSubview(makeSeparator())

        // TYPE switch row — shortcut hint in the sublabel so it's right
        // next to its target instead of repeated below.
        typeSwitch = NSSwitch()
        typeSwitch.target = self
        typeSwitch.action = #selector(typeSwitchToggled(_:))
        stack.addArrangedSubview(makeSwitchRow(
            label: "Capture keystrokes",
            sublabel: "⌃⌥⌘P to toggle",
            switchControl: typeSwitch
        ))

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

        // Instrument popup row.
        let instrumentLabel = NSTextField(labelWithString: "Instrument")
        instrumentLabel.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        instrumentLabel.textColor = .labelColor
        stack.addArrangedSubview(instrumentLabel)

        instrumentPopUp = NSPopUpButton(frame: .zero, pullsDown: false)
        instrumentPopUp.target = self
        instrumentPopUp.action = #selector(instrumentChanged(_:))
        instrumentPopUp.translatesAutoresizingMaskIntoConstraints = false
        rebuildInstrumentMenu()
        stack.addArrangedSubview(instrumentPopUp)
        instrumentPopUp.widthAnchor.constraint(equalToConstant: 240).isActive = true

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

        // Keymap toggle (MenuBand / Ableton).
        let keymapLabel = NSTextField(labelWithString: "Keymap")
        keymapLabel.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        keymapLabel.textColor = .labelColor
        stack.addArrangedSubview(keymapLabel)

        keymapSegmented = NSSegmentedControl(labels: ["MenuBand", "Ableton"],
                                              trackingMode: .selectOne,
                                              target: self,
                                              action: #selector(keymapChanged(_:)))
        keymapSegmented.translatesAutoresizingMaskIntoConstraints = false
        stack.addArrangedSubview(keymapSegmented)
        keymapSegmented.widthAnchor.constraint(equalToConstant: 240).isActive = true

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
        quit.title = "Quit MenuBand"
        quit.image = NSImage(systemSymbolName: "stop.circle.fill",
                             accessibilityDescription: "Quit")
        quit.imagePosition = .imageLeading
        quit.bezelStyle = .recessed
        quit.controlSize = .small
        quit.contentTintColor = NSColor.systemRed
        quit.target = self
        quit.action = #selector(quitApp)
        let quitTitle = NSAttributedString(
            string: "Quit MenuBand",
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
        preferredContentSize = NSSize(width: 280, height: 320)
    }

    /// Refresh control state from the controller — call right before showing.
    func syncFromController() {
        guard isViewLoaded, let n = menuBand else { return }
        typeSwitch.state = n.typeMode ? .on : .off
        midiSwitch.state = n.midiMode ? .on : .off
        octaveStepper.integerValue = n.octaveShift
        updateOctaveLabel(n.octaveShift)
        keymapSegmented.selectedSegment = (n.keymap == .ableton) ? 1 : 0
        let target = Int(n.melodicProgram)
        for item in instrumentPopUp.itemArray {
            if item.tag == target {
                instrumentPopUp.select(item)
                break
            }
        }
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

    private func rebuildInstrumentMenu() {
        let menu = NSMenu()
        for (familyName, range) in GeneralMIDI.families {
            let parent = NSMenuItem(title: familyName, action: nil, keyEquivalent: "")
            let sub = NSMenu()
            for prog in range {
                let title = String(format: "%03d  %@", prog, GeneralMIDI.programNames[prog])
                let item = NSMenuItem(title: title, action: nil, keyEquivalent: "")
                item.tag = prog
                sub.addItem(item)
            }
            parent.submenu = sub
            menu.addItem(parent)
        }
        // Flatten — NSPopUpButton displays leaf selections better when the
        // popUp menu has flat items. Build a flat menu instead with section
        // headers.
        let flat = NSMenu()
        for (familyName, range) in GeneralMIDI.families {
            let header = NSMenuItem(title: familyName.uppercased(), action: nil, keyEquivalent: "")
            header.isEnabled = false
            flat.addItem(header)
            for prog in range {
                let title = String(format: "  %03d  %@", prog, GeneralMIDI.programNames[prog])
                let item = NSMenuItem(title: title, action: nil, keyEquivalent: "")
                item.tag = prog
                flat.addItem(item)
            }
        }
        instrumentPopUp.menu = flat
    }

    private func updateOctaveLabel(_ shift: Int) {
        let s: String
        if shift == 0 { s = "0" }
        else if shift > 0 { s = "+\(shift)" }
        else { s = "\(shift)" }
        octaveLabel.stringValue = s
    }

    // MARK: - Actions

    @objc private func typeSwitchToggled(_ sender: NSSwitch) {
        menuBand?.toggleTypeMode()
        // Re-read in case Accessibility prompt cancelled.
        syncFromController()
    }

    @objc private func midiSwitchToggled(_ sender: NSSwitch) {
        menuBand?.toggleMIDIMode()
        syncFromController()
    }

    @objc private func instrumentChanged(_ sender: NSPopUpButton) {
        guard let item = sender.selectedItem, item.tag >= 0 else { return }
        menuBand?.setMelodicProgram(UInt8(item.tag))
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

    @objc private func keymapChanged(_ sender: NSSegmentedControl) {
        menuBand?.keymap = (sender.selectedSegment == 1) ? .ableton : .notepat
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
