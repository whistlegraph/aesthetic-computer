import AppKit

/// Custom About window — replaces `NSApp.orderFrontStandardAboutPanel` so
/// we can host a real clickable AC chip and a flashing "New Menu Band
/// Available!" button when UpdateChecker reports a newer release.
///
/// The standard panel only takes attributed-string credits, so anything
/// that needs to be its own NSButton (custom hover, animated tint) has
/// to live in a window we own.
final class AboutWindowController: NSWindowController, NSWindowDelegate {
    private var flashTimer: Timer?
    private weak var flashButton: NSButton?
    private var flashOn = false
    private let updateInfo: UpdateChecker.VersionInfo?

    init(updateInfo: UpdateChecker.VersionInfo?) {
        self.updateInfo = updateInfo
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 320, height: 340),
            styleMask: [.titled, .closable, .fullSizeContentView],
            backing: .buffered,
            defer: false
        )
        window.titlebarAppearsTransparent = true
        window.titleVisibility = .hidden
        window.isMovableByWindowBackground = true
        window.isReleasedWhenClosed = false
        // Float above other apps so the user notices the update prompt
        // even if they triggered About from the popover and immediately
        // tabbed away. .floating sits above normal windows but below
        // status items, which is exactly what we want.
        // The Menu Band popover panel runs at .popUpMenu (101); sit one
        // step above it so the About window paints over the popover
        // instead of being hidden behind it when both are visible.
        window.level = NSWindow.Level(rawValue: NSWindow.Level.popUpMenu.rawValue + 1)
        super.init(window: window)
        window.delegate = self
        buildContent()
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) is not used")
    }

    deinit {
        flashTimer?.invalidate()
    }

    func present() {
        guard let window = window else { return }
        window.center()
        // The Menu Band popover panel runs at .popUpMenu (101); sit one
        // step above it so the About window paints over the popover
        // instead of being hidden behind it when both are visible.
        window.level = NSWindow.Level(rawValue: NSWindow.Level.popUpMenu.rawValue + 1)
        NSApp.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
        startFlashingIfNeeded()
    }

    func windowWillClose(_ notification: Notification) {
        flashTimer?.invalidate()
        flashTimer = nil
    }

    // MARK: - Layout

    private func buildContent() {
        guard let window = window else { return }
        let content = NSView()
        content.translatesAutoresizingMaskIntoConstraints = false
        window.contentView = content

        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .centerX
        stack.spacing = 12
        stack.edgeInsets = NSEdgeInsets(top: 18, left: 28, bottom: 24, right: 28)
        stack.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            stack.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            stack.topAnchor.constraint(equalTo: content.topAnchor),
            stack.bottomAnchor.constraint(equalTo: content.bottomAnchor),
        ])

        // App icon — try the bundled artwork first, then walk up to
        // a known repo location for `swift run` dev builds where
        // there's no .app bundle to host AppIcon.icns.
        if let icon = Self.loadAppIcon() {
            let view = NSImageView(image: icon)
            view.imageScaling = .scaleProportionallyUpOrDown
            view.translatesAutoresizingMaskIntoConstraints = false
            view.widthAnchor.constraint(equalToConstant: 96).isActive = true
            view.heightAnchor.constraint(equalToConstant: 96).isActive = true
            stack.addArrangedSubview(view)
        }

        let nameLabel = NSTextField(labelWithString: "Menu Band")
        nameLabel.font = NSFont.systemFont(ofSize: 18, weight: .bold)
        nameLabel.alignment = .center
        stack.addArrangedSubview(nameLabel)

        let version = (Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String) ?? "?"
        let build = (Bundle.main.infoDictionary?["CFBundleVersion"] as? String) ?? "?"
        // Mac-standard version line: "Version 0.9 (9)" — full
        // marketing version + build number in parentheses.
        let versionString = build == version ? "Version \(version)" : "Version \(version) (\(build))"
        let versionLabel = NSTextField(labelWithString: versionString)
        versionLabel.font = NSFont.systemFont(ofSize: 11)
        versionLabel.textColor = .secondaryLabelColor
        versionLabel.alignment = .center
        stack.addArrangedSubview(versionLabel)

        stack.setCustomSpacing(14, after: versionLabel)

        // Tagline — body alone (the bold "Menu Band" lead was a
        // duplicate of the name label above; trimmed to one mention).
        // Localized body starts with a leading space + lowercase
        // word ("makes the built-in…"); capitalize the first letter
        // so it reads as a standalone sentence.
        let raw = L("popover.about.body").trimmingCharacters(in: .whitespaces)
        let bodyText = raw.prefix(1).uppercased() + raw.dropFirst()
        let body = NSTextField(wrappingLabelWithString: String(bodyText))
        body.font = NSFont.systemFont(ofSize: 11)
        body.alignment = .center
        body.preferredMaxLayoutWidth = 264
        stack.addArrangedSubview(body)

        // Aesthetic.Computer chip — purple words flanking a pink dot, the
        // same brand badge that used to live inline in the popover.
        let acPurple = NSColor(red: 167/255, green: 139/255, blue: 250/255, alpha: 1)
        let acLink = MenuBandPopoverViewController.makeLinkButton(
            attr: MenuBandPopoverViewController.aestheticComputerTitle(),
            target: self,
            action: #selector(openAesthetic),
            background: acPurple.withAlphaComponent(0.14),
            border: acPurple.withAlphaComponent(0.55)
        )
        stack.addArrangedSubview(acLink)

        if let info = updateInfo,
           UpdateChecker.isNewer(info.version, than: UpdateChecker.currentVersion()) {
            stack.setCustomSpacing(16, after: acLink)
            let btn = NSButton(title: "New Menu Band Available!",
                               target: self,
                               action: #selector(openUpdateLink))
            btn.bezelStyle = .rounded
            btn.controlSize = .large
            btn.font = NSFont.systemFont(ofSize: 13, weight: .bold)
            btn.translatesAutoresizingMaskIntoConstraints = false
            stack.addArrangedSubview(btn)
            flashButton = btn
        }
    }

    // MARK: - Icon loader

    /// Look for the app icon, falling back to the repo's
    /// `AppIcon.icns` when running via `swift run` (no .app bundle).
    private static func loadAppIcon() -> NSImage? {
        if let icon = NSImage(named: NSImage.applicationIconName),
           icon.size != .zero {
            return icon
        }
        // SwiftPM debug binary lives under .build/<arch>/debug/MenuBand.
        // Walk up to the project root and grab the source icon.
        let exec = Bundle.main.executablePath ?? ""
        let url = URL(fileURLWithPath: exec)
        let candidates = [
            url.deletingLastPathComponent()
                .deletingLastPathComponent()
                .deletingLastPathComponent()
                .deletingLastPathComponent()
                .appendingPathComponent("AppIcon.icns"),
            url.deletingLastPathComponent()
                .appendingPathComponent("AppIcon.icns"),
        ]
        for candidate in candidates {
            if FileManager.default.fileExists(atPath: candidate.path),
               let img = NSImage(contentsOf: candidate) {
                return img
            }
        }
        return nil
    }

    // MARK: - Actions

    @objc private func openAesthetic() {
        // Open inside Menu Band's own glass-chromed webview instead
        // of bouncing to Safari — same Mac-native desktop feel as
        // the ac-electron app, but right out of the menubar piano.
        // Anchor the AC window to the About window's right edge so
        // they pair side-by-side instead of stacking on top of each
        // other.
        AestheticWebWindowController.showOrFocus(rightOf: window?.frame)
    }

    @objc private func openUpdateLink() {
        if let url = URL(string: "https://prompt.ac/menuband") {
            NSWorkspace.shared.open(url)
        }
    }

    // MARK: - Flashing

    private func startFlashingIfNeeded() {
        guard flashButton != nil, flashTimer == nil else { return }
        // 0.45s on / 0.45s off — fast enough to grab attention without
        // crossing into seizure territory. Two-color alternation between
        // pink and the system accent so it reads as alive against either
        // light or dark window chrome.
        flashTimer = Timer.scheduledTimer(withTimeInterval: 0.45, repeats: true) { [weak self] _ in
            self?.tickFlash()
        }
        tickFlash()
    }

    private func tickFlash() {
        guard let btn = flashButton else { return }
        flashOn.toggle()
        let pink = NSColor(red: 255/255, green: 107/255, blue: 157/255, alpha: 1)
        let purple = NSColor(red: 167/255, green: 139/255, blue: 250/255, alpha: 1)
        let attr = NSMutableAttributedString(
            string: "New Menu Band Available!",
            attributes: [
                .foregroundColor: flashOn ? pink : purple,
                .font: NSFont.systemFont(ofSize: 13, weight: .bold),
            ]
        )
        let para = NSMutableParagraphStyle()
        para.alignment = .center
        attr.addAttribute(.paragraphStyle, value: para,
                          range: NSRange(location: 0, length: attr.length))
        btn.attributedTitle = attr
    }
}
