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
    private let onOpenPlugins: (() -> Void)?

    /// Live-updating chrome — rebuilt against the current system accent
    /// whenever `NSColor.systemColorsDidChangeNotification` fires. The icon
    /// and Plugins chip are the two surfaces that take their tint from
    /// `controlAccentColor`; everything else in the About window is brand
    /// art (AC purple, NELA secondary label) and stays put.
    private weak var iconView: NSImageView?
    private weak var pluginsButton: HoverLinkButton?
    private var accentObserver: NSObjectProtocol?

    init(updateInfo: UpdateChecker.VersionInfo?,
         onOpenPlugins: (() -> Void)? = nil) {
        self.updateInfo = updateInfo
        self.onOpenPlugins = onOpenPlugins
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
        // Live accent-color updates: refresh the tinted icon and the
        // Plugins chip whenever the user changes the system accent in
        // System Settings → Appearance. NSImageView with a non-template
        // image won't auto-redraw, and HoverLinkButton's layer caches
        // the cgColor it baked at creation, so both need an explicit
        // poke on every accent change.
        accentObserver = NotificationCenter.default.addObserver(
            forName: NSColor.systemColorsDidChangeNotification,
            object: nil, queue: .main
        ) { [weak self] _ in
            self?.refreshAccentChrome()
        }
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) is not used")
    }

    deinit {
        flashTimer?.invalidate()
        if let observer = accentObserver {
            NotificationCenter.default.removeObserver(observer)
        }
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

        // App icon — prefer the hue-rotated tint that IconTinter
        // generates against the current accent (matches the Finder
        // icon the user sees everywhere else); fall back to the
        // un-tinted bundle icon if the tinter can't reach the source
        // (e.g. swift-run dev builds without an .app wrapper, where
        // Bundle.main.url(forResource: "AppIcon") resolves but the
        // CIHueAdjust chain might not).
        if let icon = IconTinter.tintedIcon() ?? Self.loadAppIcon() {
            let view = NSImageView(image: icon)
            view.imageScaling = .scaleProportionallyUpOrDown
            view.translatesAutoresizingMaskIntoConstraints = false
            view.widthAnchor.constraint(equalToConstant: 96).isActive = true
            view.heightAnchor.constraint(equalToConstant: 96).isActive = true
            stack.addArrangedSubview(view)
            iconView = view
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

        // NELA Computer Club invite — wrapping clickable line
        // beneath the AC chip. Click → opens
        // https://nelacomputer.club in the user's default browser.
        // Wraps to multi-line so the full sentence fits the
        // narrow About panel width.
        stack.setCustomSpacing(8, after: acLink)
        let para = NSMutableParagraphStyle()
        para.alignment = .center
        para.lineBreakMode = .byWordWrapping
        let nelaText = NSAttributedString(
            string: "Say hello to the Menu Band developers in person at the NELA Computer Club, Tuesday evenings @ plot.place in Los Angeles, CA",
            attributes: [
                .font: NSFont.monospacedSystemFont(ofSize: 10, weight: .regular),
                .foregroundColor: NSColor.secondaryLabelColor,
                .underlineStyle: NSUnderlineStyle.single.rawValue,
                .paragraphStyle: para,
            ]
        )
        let nelaButton = NSButton(title: "",
                                   target: self,
                                   action: #selector(openNELA))
        nelaButton.attributedTitle = nelaText
        nelaButton.bezelStyle = .regularSquare
        nelaButton.isBordered = false
        nelaButton.cell?.wraps = true
        nelaButton.cell?.lineBreakMode = .byWordWrapping
        if let cell = nelaButton.cell as? NSButtonCell {
            cell.imageScaling = .scaleNone
        }
        nelaButton.translatesAutoresizingMaskIntoConstraints = false
        nelaButton.toolTip = "https://nelacomputer.club"
        nelaButton.widthAnchor.constraint(equalToConstant: 264).isActive = true
        stack.addArrangedSubview(nelaButton)

        // Plugins chip — opens the AU instrument picker. Beta surface
        // for testing third-party AUs (angelsaw etc.) routed through
        // Menu Band's audio engine. Hidden when no host is wired up.
        // Tinted with `controlAccentColor` so the chip tracks whatever
        // accent the user has set in System Settings (refreshed live
        // by `refreshAccentChrome`).
        if onOpenPlugins != nil {
            stack.setCustomSpacing(10, after: nelaButton)
            let pluginsBtn = MenuBandPopoverViewController.makeLinkButton(
                attr: pluginsLabel(for: NSColor.controlAccentColor),
                target: self,
                action: #selector(openPlugins),
                background: NSColor.controlAccentColor.withAlphaComponent(0.14),
                border: NSColor.controlAccentColor.withAlphaComponent(0.55)
            )
            pluginsBtn.toolTip = "Beta — engage a Liam Hall pedal"
            stack.addArrangedSubview(pluginsBtn)
            // makeLinkButton always returns a HoverLinkButton — cast so
            // we can refresh idle/hover NSColor refs on accent changes.
            pluginsButton = pluginsBtn as? HoverLinkButton
        }

        // Language picker — same flag-chip pattern the popover used
        // to host. Lives in About now so the popover stays a tight
        // music-theory surface; the About link in the popover gets
        // a flag-emoji indicator next to it so the user knows
        // language settings live in here.
        stack.setCustomSpacing(16, after: stack.arrangedSubviews.last ?? stack)
        let langRow = buildLanguagePicker()
        stack.addArrangedSubview(langRow)

        if let info = updateInfo,
           UpdateChecker.isNewer(info.version, than: UpdateChecker.currentVersion()) {
            stack.setCustomSpacing(16, after: langRow)
            let btn = NSButton(title: "Menu Band \(info.version) Now Available!",
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

    private func buildLanguagePicker() -> NSView {
        let row = NSStackView()
        row.orientation = .horizontal
        row.alignment = .centerY
        row.spacing = 6
        row.translatesAutoresizingMaskIntoConstraints = false
        let label = NSTextField(labelWithString: L("popover.language.label"))
        label.font = NSFont.systemFont(ofSize: 10, weight: .semibold)
        label.textColor = .secondaryLabelColor
        row.addArrangedSubview(label)
        for lang in Localization.supported {
            let isActive = (lang.code == Localization.current)
            let attr = NSAttributedString(
                string: "\(lang.flag)  \(lang.label)",
                attributes: [
                    .font: NSFont.systemFont(
                        ofSize: 11,
                        weight: isActive ? .semibold : .regular),
                    .foregroundColor: isActive
                        ? NSColor.labelColor
                        : NSColor.secondaryLabelColor,
                ]
            )
            let accent = NSColor.controlAccentColor
            let chip = MenuBandPopoverViewController.makeLinkButton(
                attr: attr,
                target: self,
                action: #selector(languageChipClicked(_:)),
                background: isActive
                    ? accent.withAlphaComponent(0.18)
                    : NSColor.clear,
                border: isActive
                    ? accent.withAlphaComponent(0.55)
                    : NSColor.separatorColor.withAlphaComponent(0.5))
            chip.identifier = NSUserInterfaceItemIdentifier(
                rawValue: "menuband.about.lang.\(lang.code)")
            chip.toolTip = lang.label
            row.addArrangedSubview(chip)
        }
        return row
    }

    @objc private func languageChipClicked(_ sender: NSButton) {
        guard let id = sender.identifier?.rawValue else { return }
        let prefix = "menuband.about.lang."
        guard id.hasPrefix(prefix) else { return }
        let code = String(id.dropFirst(prefix.count))
        guard code != Localization.current else { return }
        Localization.current = code
        // Rebuild the About content to apply the new strings + chip
        // selection. Cheaper than tearing down the window.
        if let content = window?.contentView {
            for sub in content.subviews { sub.removeFromSuperview() }
        }
        buildContent()
    }

    // MARK: - Icon loader

    /// Look for the app icon. Tries explicit AppIcon.icns paths
    /// FIRST (the .app bundle's Resources/, and the SwiftPM dev
    /// repo location) so we don't get stuck with the generic
    /// SwiftPM folder icon that `NSImage.applicationIconName`
    /// returns for `swift run` debug binaries.
    private static func loadAppIcon() -> NSImage? {
        let exec = Bundle.main.executablePath ?? ""
        let url = URL(fileURLWithPath: exec)
        let candidates = [
            // Bundled .app: .app/Contents/MacOS/MenuBand →
            // ../Resources/AppIcon.icns
            url.deletingLastPathComponent()
                .deletingLastPathComponent()
                .appendingPathComponent("Resources/AppIcon.icns"),
            // Dev (`swift run`): .build-debug/<arch>/debug/MenuBand
            // → ../../../../AppIcon.icns under slab/menuband/.
            url.deletingLastPathComponent()
                .deletingLastPathComponent()
                .deletingLastPathComponent()
                .deletingLastPathComponent()
                .appendingPathComponent("AppIcon.icns"),
            // Same-dir fallback for any unusual layout.
            url.deletingLastPathComponent()
                .appendingPathComponent("AppIcon.icns"),
        ]
        for candidate in candidates {
            if FileManager.default.fileExists(atPath: candidate.path),
               let img = NSImage(contentsOf: candidate) {
                return img
            }
        }
        // Last resort: the system's idea of our app icon (works for
        // properly bundled installs, returns a folder for raw
        // SwiftPM debug binaries — hence trying the explicit path
        // candidates first).
        return NSImage(named: NSImage.applicationIconName)
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

    @objc private func openNELA() {
        if let url = URL(string: "https://nelacomputer.club") {
            NSWorkspace.shared.open(url)
        }
    }

    @objc private func openPlugins() {
        onOpenPlugins?()
    }

    // MARK: - Accent-color refresh

    /// Build the Pedals chip's title against the given accent. Pulled
    /// out so `init` and `refreshAccentChrome` use the same recipe.
    private func pluginsLabel(for accent: NSColor) -> NSAttributedString {
        NSAttributedString(
            string: "Pedals",
            attributes: [
                .font: NSFont.systemFont(ofSize: 11, weight: .semibold),
                .foregroundColor: accent,
            ]
        )
    }

    /// Re-tint everything that was painted against the previous accent.
    /// HoverLinkButton keeps its idle/hover backgrounds as NSColor refs
    /// but bakes a CGColor into the layer on construction; we re-bake
    /// after refreshing the NSColor sources.
    private func refreshAccentChrome() {
        if let iconView = iconView,
           let tinted = IconTinter.tintedIcon() ?? Self.loadAppIcon() {
            iconView.image = tinted
        }
        if let btn = pluginsButton {
            let accent = NSColor.controlAccentColor
            let bg = accent.withAlphaComponent(0.14)
            let bd = accent.withAlphaComponent(0.55)
            let hoverBg = accent.withAlphaComponent(0.32)
            let hoverBd = accent.withAlphaComponent(0.80)
            btn.idleBackground = bg
            btn.idleBorder = bd
            btn.hoverBackground = hoverBg
            btn.hoverBorder = hoverBd
            btn.attributedTitle = pluginsLabel(for: accent)
            btn.layer?.backgroundColor = bg.cgColor
            btn.layer?.borderColor = bd.cgColor
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
        let version = updateInfo?.version ?? ""
        let title = version.isEmpty
            ? "Menu Band Update Available!"
            : "Menu Band \(version) Now Available!"
        let attr = NSMutableAttributedString(
            string: title,
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
