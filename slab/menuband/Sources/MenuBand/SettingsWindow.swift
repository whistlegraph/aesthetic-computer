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
    #if !MAC_APP_STORE
    private let cloudToggle = NSButton(checkboxWithTitle: "Back up takes", target: nil, action: nil)
    private let cloudNote = NSTextField(labelWithString: "")
    private let cloudSignIn = NSButton(title: "Sign in with ac-login", target: nil, action: nil)
    private var sessionWatch: UUID?
    private var cloudObserver: NSObjectProtocol?
    #endif
    private weak var menuBand: MenuBandController?

    init(menuBand: MenuBandController?) {
        self.menuBand = menuBand
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 320, height: 320),
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
        #if !MAC_APP_STORE
        if let sessionWatch { ACSession.shared.stopWatching(sessionWatch) }
        if let cloudObserver { NotificationCenter.default.removeObserver(cloudObserver) }
        #endif
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

        let inputMonitor = NSButton(
            checkboxWithTitle: "Monitor audio input",
            target: self,
            action: #selector(toggleInputMonitoring(_:)))
        inputMonitor.state = (menuBand?.inputMonitoringEnabled ?? false) ? .on : .off
        inputMonitor.toolTip = "Hear the Mac's selected input (including Focusrite interfaces) through Menu Band. Use headphones to avoid feedback."
        stack.addArrangedSubview(inputMonitor)

        let inputNote = NSTextField(
            labelWithString: "Records the selected macOS input as the tape's microphone stem.")
        inputNote.font = NSFont.systemFont(ofSize: 11)
        inputNote.textColor = .tertiaryLabelColor
        inputNote.maximumNumberOfLines = 2
        stack.addArrangedSubview(inputNote)

        if !MenuBandHaptics.isAvailable {
            let note = NSTextField(labelWithString: "No Force Touch trackpad on this Mac.")
            note.font = NSFont.systemFont(ofSize: 11)
            note.textColor = .tertiaryLabelColor
            stack.addArrangedSubview(note)
        }

        #if !MAC_APP_STORE
        // Shapedown (double-tap left ⌘) feedback cues — the same flash + bell
        // the right-⌘ focus gesture uses, switchable independently.
        let sdFlash = NSButton(checkboxWithTitle: "Shapedown flashes",
                               target: self,
                               action: #selector(toggleShapedownFlash(_:)))
        sdFlash.state = Shapedown.flashesEnabled ? .on : .off
        sdFlash.toolTip = "Full-screen flash when the Shapedown wall opens, closes, or stamps."
        stack.addArrangedSubview(sdFlash)

        let sdSound = NSButton(checkboxWithTitle: "Shapedown sounds",
                               target: self,
                               action: #selector(toggleShapedownSound(_:)))
        sdSound.state = Shapedown.soundsEnabled ? .on : .off
        sdSound.toolTip = "Bell and click sounds for the Shapedown wall."
        stack.addArrangedSubview(sdSound)

        // Cloud backup of tape takes to the signed-in AC handle (~/.ac-token,
        // which the sandboxed App Store build can't read).
        cloudToggle.target = self
        cloudToggle.action = #selector(toggleCloudBackup(_:))
        cloudToggle.toolTip = "Upload every take — mix, stems, notes — privately to your Aesthetic Computer account."
        stack.addArrangedSubview(cloudToggle)
        cloudNote.font = NSFont.systemFont(ofSize: 11)
        cloudNote.textColor = .tertiaryLabelColor
        cloudNote.maximumNumberOfLines = 3
        cloudNote.lineBreakMode = .byWordWrapping
        cloudNote.preferredMaxLayoutWidth = 272
        stack.addArrangedSubview(cloudNote)
        cloudSignIn.bezelStyle = .rounded
        cloudSignIn.target = self
        cloudSignIn.action = #selector(signInToCloud(_:))
        stack.addArrangedSubview(cloudSignIn)
        refreshCloud()
        sessionWatch = ACSession.shared.startWatching { [weak self] in self?.refreshCloud() }
        cloudObserver = NotificationCenter.default.addObserver(
            forName: MenuBandCloud.statusChanged, object: nil, queue: .main
        ) { [weak self] _ in self?.refreshCloud() }
        #endif

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

    @objc private func toggleInputMonitoring(_ sender: NSButton) {
        menuBand?.inputMonitoringEnabled = (sender.state == .on)
    }

    #if !MAC_APP_STORE
    @objc private func toggleShapedownFlash(_ sender: NSButton) {
        Shapedown.flashesEnabled = (sender.state == .on)
    }

    @objc private func toggleShapedownSound(_ sender: NSButton) {
        Shapedown.soundsEnabled = (sender.state == .on)
    }

    @objc private func toggleCloudBackup(_ sender: NSButton) {
        MenuBandCloud.isEnabled = (sender.state == .on)
        refreshCloud()
    }

    @objc private func signInToCloud(_ sender: Any?) {
        ACSession.shared.runAcLogin()
    }

    /// Handle, session state and queue depth. Pending count reads the queue
    /// folder, so it runs off main.
    private func refreshCloud() {
        let session = ACSession.shared
        let handle = session.displayName
        let state = session.state
        cloudToggle.title = handle.map { "Back up takes to \($0)" } ?? "Back up takes to Aesthetic Computer"
        cloudToggle.state = MenuBandCloud.isEnabled ? .on : .off
        cloudToggle.isEnabled = state == .signedIn
        cloudSignIn.isHidden = state == .signedIn
        cloudSignIn.title = state == .expired ? "Session expired — sign in with ac-login" : "Sign in with ac-login"
        DispatchQueue.global(qos: .utility).async { [weak self] in
            let pending = MenuBandCloud.shared.pendingCount
            DispatchQueue.main.async {
                guard let self else { return }
                var note = state == .signedIn
                    ? "Private to your account. Browse them at aesthetic.computer/menuband."
                    : "Sign in to back takes up to your @handle."
                if pending > 0 { note += " \(pending) waiting to upload." }
                self.cloudNote.stringValue = note
            }
        }
    }
    #endif

    @objc private func viewCrashLogs(_ sender: Any?) {
        let logs = CrashLogReader.recentLogs()
        guard !logs.isEmpty else { return }
        crashViewer?.close()
        let viewer = CrashViewerWindowController(logs: logs)
        crashViewer = viewer
        viewer.present()
    }
}
