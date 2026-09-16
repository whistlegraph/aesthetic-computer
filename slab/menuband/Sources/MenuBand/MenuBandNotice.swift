import AppKit

/// A modeless, self-dismissing notice — Menu Band's replacement for
/// `NSAlert.runModal()` on the permission paths.
///
/// `runModal()` is a trap for a menu-bar instrument. Quiet focus arms an
/// invisible key-capture panel, hides the pointer and installs a global
/// click shield; a nested modal loop started underneath all of that
/// leaves the alert unclickable (the shield eats the button press) and
/// undismissable (Escape belongs to the capture monitor, which the modal
/// session has starved), so Menu Band reads as hung and permanently
/// focused. Worse, the old alert sat at `.screenSaver` — a tie with the
/// visualizer and the pitch-bend cursor, so it could end up *behind* one
/// and block the app invisibly.
///
/// This panel never starts a modal session. It floats one notch above
/// every Menu Band surface, takes key away from the capture panel so
/// Escape and Return land here, dismisses on a click anywhere outside
/// it, and fades itself out after `dismissAfter` no matter what the user
/// does. Nothing it does can strand the app.
final class MenuBandNotice: NSPanel {
    /// Notices already raised this launch, keyed by `key`. A permission
    /// failure fires on every record press; without this the user gets a
    /// fresh panel per keystroke. Suppressed repeats still log.
    private static var shownKeys: Set<String> = []
    /// Only ever one notice on screen — a second would stack under the
    /// first at the same level and confuse the dismissal.
    private static var live: MenuBandNotice?

    private var clickAwayMonitors: [Any] = []
    private var dismissWork: DispatchWorkItem?
    private var onChoice: ((Bool) -> Void)?
    private var answered = false

    private static let bodyWidth: CGFloat = 320
    private static let margin: CGFloat = 20

    /// Raise a notice, at most once per `key` per launch. `onChoice`
    /// receives true for the primary button, false for every other exit
    /// (secondary button, Escape, click-away, timeout) so a caller can
    /// treat "didn't say yes" uniformly.
    ///
    /// - Parameter dismissAfter: seconds before the panel fades itself
    ///   out. The guarantee that a notice can never outlive the moment
    ///   that caused it.
    @discardableResult
    static func show(key: String,
                     title: String,
                     body: String,
                     primary: String,
                     secondary: String,
                     dismissAfter: TimeInterval = 30,
                     onChoice: @escaping (Bool) -> Void) -> Bool {
        guard !shownKeys.contains(key) else {
            NSLog("MenuBand notice: '\(key)' already shown this launch — suppressed (\(title))")
            return false
        }
        shownKeys.insert(key)
        live?.finish(primaryChosen: false)
        let notice = MenuBandNotice(title: title, body: body,
                                    primary: primary, secondary: secondary)
        notice.onChoice = onChoice
        live = notice
        notice.present(dismissAfter: dismissAfter)
        NSLog("MenuBand notice: showing '\(key)' — \(title)")
        return true
    }

    private init(title: String, body: String, primary: String, secondary: String) {
        // Deliberately NOT `.nonactivatingPanel`. Menu Band is an
        // accessory app; a non-activating panel can never take key, so
        // Escape and Return would die and the only exits left would be
        // the mouse and the timeout. This notice is raised by something
        // the user just did in Menu Band, so taking key is honest — and
        // whatever it takes, `finish` gives back.
        super.init(contentRect: NSRect(x: 0, y: 0, width: 360, height: 200),
                   styleMask: [.borderless],
                   backing: .buffered,
                   defer: false)
        isOpaque = false
        backgroundColor = .clear
        hasShadow = true
        isFloatingPanel = true
        hidesOnDeactivate = false
        // One notch ABOVE `.screenSaver` so the notice can never lose an
        // ordering tie with the full-screen visualizer, the focus flash
        // or the pitch-bend cursor — all of which sit at `.screenSaver`
        // and would otherwise bury it.
        level = NSWindow.Level(rawValue: NSWindow.Level.screenSaver.rawValue + 1)
        collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary, .ignoresCycle]

        let backdrop = NSVisualEffectView()
        backdrop.material = .popover
        backdrop.blendingMode = .behindWindow
        backdrop.state = .active
        backdrop.wantsLayer = true
        backdrop.layer?.cornerRadius = 14
        backdrop.layer?.masksToBounds = true

        let icon = NSImageView(image: NSApp.applicationIconImage ?? NSImage())
        icon.imageScaling = .scaleProportionallyUpOrDown
        icon.translatesAutoresizingMaskIntoConstraints = false
        icon.widthAnchor.constraint(equalToConstant: 52).isActive = true
        icon.heightAnchor.constraint(equalToConstant: 52).isActive = true

        let titleLabel = NSTextField(wrappingLabelWithString: title)
        titleLabel.font = .systemFont(ofSize: 15, weight: .bold)
        titleLabel.isSelectable = false

        let bodyLabel = NSTextField(wrappingLabelWithString: body)
        bodyLabel.font = .systemFont(ofSize: 12)
        bodyLabel.textColor = .secondaryLabelColor
        bodyLabel.isSelectable = false

        let primaryButton = NSButton(title: primary, target: self,
                                     action: #selector(choosePrimary))
        primaryButton.bezelStyle = .rounded
        primaryButton.keyEquivalent = "\r"
        let secondaryButton = NSButton(title: secondary, target: self,
                                       action: #selector(chooseSecondary))
        secondaryButton.bezelStyle = .rounded
        // Escape is handled in `cancelOperation` rather than as this
        // button's key equivalent — a borderless panel routes escape
        // through the responder chain, and routing both exits through one method
        // keeps the "any non-primary exit" contract honest.

        let buttons = NSStackView(views: [secondaryButton, primaryButton])
        buttons.orientation = .horizontal
        buttons.spacing = 10

        let text = NSStackView(views: [titleLabel, bodyLabel])
        text.orientation = .vertical
        text.alignment = .leading
        text.spacing = 8

        let column = NSStackView(views: [icon, text, buttons])
        column.orientation = .vertical
        column.alignment = .leading
        column.spacing = 14
        column.translatesAutoresizingMaskIntoConstraints = false
        backdrop.addSubview(column)

        let m = Self.margin
        NSLayoutConstraint.activate([
            column.leadingAnchor.constraint(equalTo: backdrop.leadingAnchor, constant: m),
            column.trailingAnchor.constraint(equalTo: backdrop.trailingAnchor, constant: -m),
            column.topAnchor.constraint(equalTo: backdrop.topAnchor, constant: m),
            column.bottomAnchor.constraint(equalTo: backdrop.bottomAnchor, constant: -m),
            titleLabel.widthAnchor.constraint(equalToConstant: Self.bodyWidth),
            bodyLabel.widthAnchor.constraint(equalToConstant: Self.bodyWidth),
            buttons.trailingAnchor.constraint(equalTo: column.trailingAnchor),
        ])
        contentView = backdrop
        setContentSize(backdrop.fittingSize)
    }

    /// Borderless panels refuse key by default, which would leave Escape
    /// and Return dead and hand the keyboard back to the quiet-focus
    /// capture panel — the exact stranding this class exists to stop.
    override var canBecomeKey: Bool { true }

    private func present(dismissAfter: TimeInterval) {
        if let screen = NSScreen.main {
            let f = screen.visibleFrame
            setFrameOrigin(NSPoint(
                x: f.midX - frame.width / 2,
                // Sits in the upper third — near the menu bar the notice
                // is about, and clear of whatever is mid-screen.
                y: f.maxY - frame.height - f.height * 0.18
            ))
        }
        // `activate()`, not `activate(ignoringOtherApps:)` — the notice
        // should come forward the way any app's window does, not yank
        // the screen away from whatever the user moved on to.
        if #available(macOS 14.0, *) {
            NSApp.activate()
        } else {
            NSApp.activate(ignoringOtherApps: false)
        }
        makeKeyAndOrderFront(nil)
        orderFrontRegardless()
        installClickAwayMonitors()

        let work = DispatchWorkItem { [weak self] in
            NSLog("MenuBand notice: timed out after \(dismissAfter)s — dismissing")
            self?.finish(primaryChosen: false)
        }
        dismissWork = work
        DispatchQueue.main.asyncAfter(deadline: .now() + dismissAfter, execute: work)
    }

    /// A click anywhere but the notice closes it. The local monitor
    /// covers Menu Band's own surfaces (the popover, the piano), the
    /// global one covers every other app.
    private func installClickAwayMonitors() {
        let mask: NSEvent.EventTypeMask = [.leftMouseDown, .rightMouseDown, .otherMouseDown]
        if let global = NSEvent.addGlobalMonitorForEvents(matching: mask, handler: { [weak self] _ in
            self?.finish(primaryChosen: false)
        }) {
            clickAwayMonitors.append(global)
        }
        if let local = NSEvent.addLocalMonitorForEvents(matching: mask, handler: { [weak self] event in
            if event.window !== self { self?.finish(primaryChosen: false) }
            return event
        }) {
            clickAwayMonitors.append(local)
        }
    }

    override func cancelOperation(_ sender: Any?) { finish(primaryChosen: false) }

    @objc private func choosePrimary() { finish(primaryChosen: true) }
    @objc private func chooseSecondary() { finish(primaryChosen: false) }

    /// Single exit door — idempotent, so a timeout racing a button press
    /// can't fire the callback twice or double-remove the monitors.
    private func finish(primaryChosen: Bool) {
        guard !answered else { return }
        answered = true
        dismissWork?.cancel()
        dismissWork = nil
        for monitor in clickAwayMonitors { NSEvent.removeMonitor(monitor) }
        clickAwayMonitors.removeAll()
        if MenuBandNotice.live === self { MenuBandNotice.live = nil }
        let choice = onChoice
        onChoice = nil
        orderOut(nil)
        choice?(primaryChosen)
    }

    deinit {
        for monitor in clickAwayMonitors { NSEvent.removeMonitor(monitor) }
    }
}
