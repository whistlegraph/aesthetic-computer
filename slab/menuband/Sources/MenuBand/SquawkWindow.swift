import AppKit

/// The Squawk control window — a small panel that turns voice dictation on,
/// explains how it works, and lets you talk. Opened from the "Squawk" button
/// on the fullscreen keymap view (a peer of the "LLMs" guide window), so
/// Squawk has a home of its own instead of a checkbox buried in About.
///
/// Everything Squawk-related lives here: the enable switch (writes
/// `MenuBandSquawk.enabledDefaultsKey` and posts
/// `.menuBandSquawkEnabledChanged`, which arms the ⌘⌃⌥` hotkey and shows the
/// popover's 🦜 cell), the how-to, and a talk button that toggles listening.
final class SquawkWindowController: NSWindowController, NSWindowDelegate {

    private static var active: SquawkWindowController?

    private let enableToggle = NSButton()
    private let talkButton = NSButton()
    private var listening = false

    /// Open (or re-focus) the single Squawk window.
    @discardableResult
    static func show() -> SquawkWindowController {
        if let live = active {
            live.syncState()
            live.window?.makeKeyAndOrderFront(nil)
            NSApp.activate(ignoringOtherApps: true)
            return live
        }
        let ctrl = SquawkWindowController()
        ctrl.present()
        return ctrl
    }

    init() {
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 420, height: 300),
            styleMask: [.titled, .closable, .fullSizeContentView],
            backing: .buffered, defer: false)
        window.title = "Squawk"
        window.titlebarAppearsTransparent = true
        window.isMovableByWindowBackground = true
        super.init(window: window)
        window.delegate = self
        buildContent()
        NotificationCenter.default.addObserver(
            self, selector: #selector(externalStateChanged(_:)),
            name: .menuBandSquawkStateChanged, object: nil)
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) { nil }

    func present() {
        guard let window = window else { return }
        SquawkWindowController.active = self
        window.center()
        // Ordinary window level. These secondary windows used to sit at
        // popUpMenu + 1 so the status-bar popover couldn't bury them — but that
        // floated them above every other app on the Mac for as long as they
        // stayed open. Each one activates the app and orders front when shown,
        // which lifts it over the popover at the only moment that matters.
        window.level = .normal
        NSApp.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
        syncState()
    }

    func windowWillClose(_ notification: Notification) {
        if SquawkWindowController.active === self { SquawkWindowController.active = nil }
    }

    // MARK: - UI

    private func buildContent() {
        guard let content = window?.contentView else { return }

        let heading = NSTextField(labelWithString: "🦜 Squawk")
        heading.font = .systemFont(ofSize: 17, weight: .bold)

        let blurb = NSTextField(wrappingLabelWithString:
            "Talk and Menu Band types what you say into whatever app is in "
            + "front — a terminal, a chat, anywhere text goes. Transcription "
            + "runs on your Mac: offline, no account.")
        blurb.font = .systemFont(ofSize: 12)
        blurb.textColor = .secondaryLabelColor

        enableToggle.setButtonType(.switch)
        enableToggle.title = "Enable Squawk"
        enableToggle.font = .systemFont(ofSize: 12, weight: .semibold)
        enableToggle.target = self
        enableToggle.action = #selector(toggleEnabled(_:))

        let how = NSTextField(wrappingLabelWithString:
            "Hold ⌘⌃⌥` and talk, then release to insert. The 🦜 cell in the "
            + "popover toggles it with a click too. The first time, macOS asks "
            + "for Microphone and Speech Recognition — allow both.")
        how.font = .systemFont(ofSize: 11)
        how.textColor = .secondaryLabelColor

        talkButton.bezelStyle = .rounded
        talkButton.controlSize = .large
        talkButton.font = .systemFont(ofSize: 13, weight: .semibold)
        talkButton.target = self
        talkButton.action = #selector(talk(_:))

        #if MAC_APP_STORE
        let sandbox = NSTextField(wrappingLabelWithString:
            "This version can transcribe but, per App Store sandbox rules, "
            + "can't type into other apps.")
        sandbox.font = .systemFont(ofSize: 10)
        sandbox.textColor = .tertiaryLabelColor
        #endif

        let stack = NSStackView(views: [heading, blurb, enableToggle, how, talkButton])
        stack.orientation = .vertical
        stack.alignment = .leading
        stack.spacing = 12
        stack.translatesAutoresizingMaskIntoConstraints = false
        stack.setCustomSpacing(16, after: how)
        #if MAC_APP_STORE
        stack.addArrangedSubview(sandbox)
        #endif
        content.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.topAnchor.constraint(equalTo: content.topAnchor, constant: 28),
            stack.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 22),
            stack.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -22),
        ])
    }

    // MARK: - State

    /// Reflect the persisted enable flag + live listening state on the controls.
    private func syncState() {
        enableToggle.state = MenuBandSquawk.isEnabled ? .on : .off
        talkButton.isEnabled = MenuBandSquawk.isEnabled
        talkButton.title = listening ? "Stop" : "Talk"
    }

    @objc private func externalStateChanged(_ note: Notification) {
        listening = (note.object as? Bool) ?? false
        syncState()
    }

    @objc private func toggleEnabled(_ sender: NSButton) {
        let on = sender.state == .on
        UserDefaults.standard.set(on, forKey: MenuBandSquawk.enabledDefaultsKey)
        NotificationCenter.default.post(
            name: .menuBandSquawkEnabledChanged, object: nil)
        syncState()
    }

    @objc private func talk(_ sender: NSButton) {
        NotificationCenter.default.post(
            name: .menuBandSquawkToggleRequested, object: nil)
    }
}
