import AppKit

/// Menu Band's Settings window — the home the app's persistent preferences
/// never had.
///
/// Before this, settings with nowhere to live got wedged into whatever panel
/// was nearest: "Open at Login" landed between the version and copyright lines
/// of the About footer (a live control marooned in a band of inert gray text),
/// and the Haptics switch was pushed into the keymap's instrument *title row*,
/// so foreign there that 1.5.4 deleted it outright rather than rehome it.
/// Both were placement bugs, not feature bugs. This is the place.
///
/// Deliberately NOT a home for the popover's live performance controls (octave,
/// instrument, MIDI). Those are played, not configured — they belong under the
/// hand, in the popover. Settings holds only what persists between sessions.
///
/// Named "Settings", not "Preferences": Apple renamed it in macOS 13, which is
/// also the floor for `SMAppService` (see `MenuBandLoginItem`).
final class SettingsWindowController: NSWindowController, NSWindowDelegate {
    /// Keeps the controller alive while shown, and lets a second request focus
    /// the existing window instead of stacking a duplicate.
    static var active: SettingsWindowController?

    private var crashViewer: CrashViewerWindowController?
    private weak var menuBand: MenuBandController?

    init(menuBand: MenuBandController?) {
        self.menuBand = menuBand
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 320, height: 200),
            styleMask: [.titled, .closable],
            backing: .buffered,
            defer: false
        )
        window.title = "Settings"
        window.isReleasedWhenClosed = false
        // An ordinary window level. Menu Band's secondary windows used to sit
        // at popUpMenu + 1 so the status-bar popover couldn't bury them — but
        // that also floated them above every other app on the Mac, forever.
        // `present()` activates the app and orders front instead, which lifts
        // us over the popover at the only moment it matters: when we open.
        super.init(window: window)
        window.delegate = self
        buildContent()
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) { nil }

    static func showOrFocus(menuBand: MenuBandController?) {
        if let live = active {
            live.present()
            return
        }
        let c = SettingsWindowController(menuBand: menuBand)
        c.present()
    }

    func present() {
        guard let window = window else { return }
        SettingsWindowController.active = self
        window.center()
        NSApp.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
    }

    func windowWillClose(_ notification: Notification) {
        if SettingsWindowController.active === self { SettingsWindowController.active = nil }
    }

    // MARK: - Layout

    private func buildContent() {
        guard let content = window?.contentView else { return }

        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 12
        stack.edgeInsets = NSEdgeInsets(top: 20, left: 24, bottom: 20, right: 24)
        stack.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            stack.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            stack.topAnchor.constraint(equalTo: content.topAnchor),
        ])

        #if MAC_APP_STORE
        // App Store build only: the direct-download build auto-starts from the
        // LaunchAgent install.sh writes, so it has nothing to toggle. Hidden
        // below macOS 13, where SMAppService doesn't exist.
        if MenuBandLoginItem.isSupported {
            let login = NSButton(checkboxWithTitle: "Open at Login",
                                 target: self,
                                 action: #selector(toggleOpenAtLogin(_:)))
            login.state = MenuBandLoginItem.isEnabled ? .on : .off
            login.toolTip = "Start Menu Band automatically when you log in."
            stack.addArrangedSubview(login)
        }
        #endif

        // Trackpad Force Touch feedback on key taps. Disabled (not hidden) when
        // the hardware can't do it — a dimmed row explains why the feature is
        // absent, where a missing row would just look like it doesn't exist.
        let haptics = NSButton(checkboxWithTitle: "Haptics",
                               target: self,
                               action: #selector(toggleHaptics(_:)))
        haptics.state = (menuBand?.hapticsEnabled ?? true) ? .on : .off
        haptics.isEnabled = MenuBandHaptics.isAvailable
        haptics.toolTip = MenuBandHaptics.isAvailable
            ? "Force Touch feedback from the trackpad when you play a key."
            : "This Mac has no Force Touch trackpad."
        stack.addArrangedSubview(haptics)

        if !MenuBandHaptics.isAvailable {
            let note = NSTextField(labelWithString: "No Force Touch trackpad on this Mac.")
            note.font = NSFont.systemFont(ofSize: 11)
            note.textColor = .tertiaryLabelColor
            stack.addArrangedSubview(note)
        }

        // Crashes — conditional. A diagnostics link only earns a row if there
        // is something to diagnose; on a healthy install Settings shouldn't
        // advertise a crash viewer at all.
        let logs = CrashLogReader.recentLogs()
        if !logs.isEmpty {
            let sep = NSBox()
            sep.boxType = .separator
            sep.translatesAutoresizingMaskIntoConstraints = false
            stack.addArrangedSubview(sep)
            sep.widthAnchor.constraint(equalTo: stack.widthAnchor, constant: -48).isActive = true

            let title = logs.count == 1
                ? L("popover.about.crash.summaryOne")
                : L("popover.about.crash.summaryMany", String(logs.count))
            let crashes = NSButton(title: title,
                                   target: self,
                                   action: #selector(viewCrashLogs(_:)))
            crashes.bezelStyle = .rounded
            crashes.toolTip = "Review the crash reports and send them to Aesthetic Computer."
            stack.addArrangedSubview(crashes)
        }
    }

    // MARK: - Actions

    #if MAC_APP_STORE
    @objc private func toggleOpenAtLogin(_ sender: NSButton) {
        MenuBandLoginItem.isEnabled = (sender.state == .on)
    }
    #endif

    @objc private func toggleHaptics(_ sender: NSButton) {
        menuBand?.hapticsEnabled = (sender.state == .on)
    }

    @objc private func viewCrashLogs(_ sender: Any?) {
        let logs = CrashLogReader.recentLogs()
        guard !logs.isEmpty else { return }
        crashViewer?.close()
        let viewer = CrashViewerWindowController(logs: logs)
        crashViewer = viewer
        viewer.present()
    }
}
