import AppKit
import CoreImage

/// Custom About window — replaces `NSApp.orderFrontStandardAboutPanel` so
/// we can host a real clickable AC chip and a flashing "New Menu Band
/// Available!" button when UpdateChecker reports a newer release.
///
/// The standard panel only takes attributed-string credits, so anything
/// that needs to be its own NSButton (custom hover, animated tint) has
/// to live in a window we own.
final class AboutWindowController: NSWindowController, NSWindowDelegate {
    /// Self-retain so a language-switch-triggered popoverVC rebuild
    /// (which drops every strong ref the popover held) doesn't tear
    /// the window out mid-interaction. Cleared in `windowWillClose`.
    private static var active: AboutWindowController?

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

    /// Strong ref so the viewer window stays alive while it's open —
    /// AboutWindowController is the only thing that holds it.
    private var crashViewer: CrashViewerWindowController?

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

    /// Open or refocus the singleton About window. Dedupes via the
    /// static `active` registry so callers don't have to track the
    /// instance — and so a popoverVC rebuild can't accidentally
    /// orphan a live window.
    @discardableResult
    static func show(updateInfo: UpdateChecker.VersionInfo?,
                     onOpenPlugins: (() -> Void)? = nil) -> AboutWindowController {
        active?.close()
        let ctrl = AboutWindowController(updateInfo: updateInfo,
                                         onOpenPlugins: onOpenPlugins)
        ctrl.present()
        return ctrl
    }

    func present() {
        guard let window = window else { return }
        // Park ourselves in the static registry — keeps this controller
        // alive even if the popoverVC that opened us gets thrown away.
        AboutWindowController.active = self
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
        if AboutWindowController.active === self {
            AboutWindowController.active = nil
        }
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

        stack.setCustomSpacing(10, after: versionLabel)

        // Scannable QR code → prompt.ac/menuband. Useful when the
        // About panel is shown on a projector or shared screen so
        // people in the room can grab the download URL on their
        // phones without typing it. Click also opens the URL in
        // the user's default browser as a fallback path.
        if let qr = Self.qrCodeImage(
            for: "https://prompt.ac/menuband", side: 124) {
            let qrButton = NSButton(title: "",
                                     target: self,
                                     action: #selector(openMenubandLanding))
            qrButton.image = qr
            qrButton.imagePosition = .imageOnly
            qrButton.isBordered = false
            qrButton.bezelStyle = .regularSquare
            qrButton.toolTip = "https://prompt.ac/menuband"
            qrButton.translatesAutoresizingMaskIntoConstraints = false
            qrButton.widthAnchor.constraint(equalToConstant: 124).isActive = true
            qrButton.heightAnchor.constraint(equalToConstant: 124).isActive = true
            stack.addArrangedSubview(qrButton)
            // Tiny caption under the code so it reads as a real
            // QR badge ("scan to install / share") and not just a
            // decorative graphic.
            let qrCaption = NSTextField(labelWithString: "prompt.ac/menuband")
            qrCaption.font = NSFont.monospacedSystemFont(
                ofSize: 9, weight: .regular)
            qrCaption.textColor = .tertiaryLabelColor
            qrCaption.alignment = .center
            stack.setCustomSpacing(4, after: qrButton)
            stack.addArrangedSubview(qrCaption)
            stack.setCustomSpacing(14, after: qrCaption)
        } else {
            stack.setCustomSpacing(14, after: versionLabel)
        }

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
            string: L("popover.about.nela"),
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

        // Mikey's prompt to start your own — same wrapping style as
        // the NELA invite, click → startacomputer.club. Sits directly
        // below NELA so the two reads as a pair: "come hang out / or
        // start your own."
        stack.setCustomSpacing(4, after: nelaButton)
        let startClubText = NSAttributedString(
            string: L("popover.about.startaclub"),
            attributes: [
                .font: NSFont.monospacedSystemFont(ofSize: 10, weight: .regular),
                .foregroundColor: NSColor.secondaryLabelColor,
                .underlineStyle: NSUnderlineStyle.single.rawValue,
                .paragraphStyle: para,
            ]
        )
        let startClubButton = NSButton(title: "",
                                        target: self,
                                        action: #selector(openStartAClub))
        startClubButton.attributedTitle = startClubText
        startClubButton.bezelStyle = .regularSquare
        startClubButton.isBordered = false
        startClubButton.cell?.wraps = true
        startClubButton.cell?.lineBreakMode = .byWordWrapping
        if let cell = startClubButton.cell as? NSButtonCell {
            cell.imageScaling = .scaleNone
        }
        startClubButton.translatesAutoresizingMaskIntoConstraints = false
        startClubButton.toolTip = "https://startacomputer.club"
        startClubButton.widthAnchor.constraint(equalToConstant: 264).isActive = true
        stack.addArrangedSubview(startClubButton)

        // [v1 cutoff] Pedals (AU plugin picker) chip removed — Liam Hall
        // pedals / third-party AU hosting are post-v1. The `onOpenPlugins`
        // hook + AU picker code stay dormant for the post-release revival.

        // Language picker — same flag-chip pattern the popover used
        // to host. Lives in About now so the popover stays a tight
        // music-theory surface; the About link in the popover gets
        // a flag-emoji indicator next to it so the user knows
        // language settings live in here.
        stack.setCustomSpacing(16, after: stack.arrangedSubviews.last ?? stack)
        let langRow = buildLanguagePicker()
        stack.addArrangedSubview(langRow)

        // [v1 cutoff] "Cassette deck (beta)" + "Percussion split" beta
        // checkboxes removed — both features are post-v1. Their flags
        // default off and their live shortcuts stay in place, so the
        // toggles can return to About after release.

        // Crash-report summary — single orange ⚠️ button reading
        // "Menu Band crashed N times". Opens the scroll viewer where
        // the user can review the .ips contents and click Send to
        // Aesthetic.Computer. Lives bottom-left of the About window
        // (secondary-action position) so it never crowds the
        // primary brand chrome.
        let logs = CrashLogReader.recentLogs()
        if !logs.isEmpty {
            stack.setCustomSpacing(14, after: langRow)
            let summary = logs.count == 1
                ? L("popover.about.crash.summaryOne")
                : L("popover.about.crash.summaryMany", String(logs.count))
            let btn = NSButton(title: "",
                               target: self,
                               action: #selector(viewCrashLogs(_:)))
            btn.bezelStyle = .rounded
            btn.isBordered = true
            btn.bezelColor = .systemOrange
            btn.controlSize = .mini
            btn.attributedTitle = NSAttributedString(
                string: "⚠️  \(summary)",
                attributes: [
                    .foregroundColor: NSColor.white,
                    .font: NSFont.systemFont(ofSize: 9, weight: .medium),
                ]
            )

            let crashRow = NSStackView()
            crashRow.orientation = .horizontal
            crashRow.alignment = .centerY
            crashRow.spacing = 0
            crashRow.translatesAutoresizingMaskIntoConstraints = false
            let spacer = NSView()
            spacer.setContentHuggingPriority(.defaultLow, for: .horizontal)
            crashRow.addArrangedSubview(btn)
            crashRow.addArrangedSubview(spacer)
            stack.addArrangedSubview(crashRow)
            // Match the inner content width — stack's edgeInsets are
            // 28 left + 28 right, so the row spans the same inset
            // area the body label uses.
            crashRow.widthAnchor.constraint(
                equalTo: stack.widthAnchor, constant: -56).isActive = true
        }

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
        // Outer column: "Language" label on top, then chips wrapped
        // into rows of three. Wrapping is necessary because the About
        // window is narrow (320pt) and 5+ language chips with native
        // labels overflow a single row.
        let outer = NSStackView()
        outer.orientation = .vertical
        outer.alignment = .centerX
        outer.spacing = 6
        outer.translatesAutoresizingMaskIntoConstraints = false

        let label = NSTextField(labelWithString: L("popover.language.label"))
        label.font = NSFont.systemFont(ofSize: 10, weight: .semibold)
        label.textColor = .secondaryLabelColor
        outer.addArrangedSubview(label)

        // Build all chips first so we can pack them three-per-row.
        let chips: [NSButton] = Localization.supported.map { lang in
            let isActive = (lang.code == Localization.current)
            // Spaces inside the title give the chip visible breathing
            // room around its layer-painted bezel — the chip's
            // intrinsic width tracks the rendered text, so adding
            // pad-spaces is the simplest way to enlarge the touch
            // target without touching makeLinkButton's geometry.
            let padded = "  \(lang.flag)  \(lang.label)  "
            let attr = NSAttributedString(
                string: padded,
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
            return chip
        }

        // Three chips per row keeps each chip readable at the
        // narrower About-window width even with longer native
        // labels like "Русский" / "日本語".
        let perRow = 3
        var index = 0
        while index < chips.count {
            let slice = Array(chips[index..<min(index + perRow, chips.count)])
            let row = NSStackView(views: slice)
            row.orientation = .horizontal
            row.alignment = .centerY
            row.spacing = 6
            row.translatesAutoresizingMaskIntoConstraints = false
            outer.addArrangedSubview(row)
            index += perRow
        }
        return outer
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

    @objc private func openMenubandLanding() {
        if let url = URL(string: "https://prompt.ac/menuband") {
            NSWorkspace.shared.open(url)
        }
    }

    /// Generate a high-contrast QR code at the requested side
    /// length (in points). Uses CIQRCodeGenerator with the high
    /// error-correction level (`H`) so the code stays scannable
    /// even when projected at small sizes or photographed at an
    /// angle. The output is upscaled with nearest-neighbour so the
    /// modules render as crisp squares at any DPI — without that,
    /// CIImage's default interpolation softens the edges and trips
    /// some phone scanners.
    fileprivate static func qrCodeImage(for string: String,
                                        side: CGFloat) -> NSImage? {
        guard let data = string.data(using: .utf8) else { return nil }
        guard let filter = CIFilter(name: "CIQRCodeGenerator") else {
            return nil
        }
        filter.setValue(data, forKey: "inputMessage")
        filter.setValue("H", forKey: "inputCorrectionLevel")
        guard let output = filter.outputImage else { return nil }
        let scale = side / output.extent.width
        let scaled = output.transformed(
            by: CGAffineTransform(scaleX: scale, y: scale))
        let context = CIContext()
        guard let cg = context.createCGImage(
            scaled, from: scaled.extent) else { return nil }
        let image = NSImage(cgImage: cg,
                            size: NSSize(width: side, height: side))
        image.isTemplate = false
        return image
    }

    @objc private func openNELA() {
        if let url = URL(string: "https://nelacomputer.club") {
            NSWorkspace.shared.open(url)
        }
    }

    @objc private func openStartAClub() {
        if let url = URL(string: "https://startacomputer.club") {
            NSWorkspace.shared.open(url)
        }
    }

    @objc private func openPlugins() {
        onOpenPlugins?()
    }

    /// Persist the cassette-deck beta flag and tell AppDelegate to
    /// re-render the menubar icon. Status-item width changes on the
    /// flip, so the notification handler also resizes the slot.
    @objc private func toggleTapeFeature(_ sender: NSButton) {
        let on = sender.state == .on
        UserDefaults.standard.set(
            on, forKey: KeyboardIconRenderer.tapeFeatureDefaultsKey)
        NotificationCenter.default.post(
            name: .menuBandTapeFeatureChanged, object: nil)
    }

    /// Flip the right-hand percussion split. Writes the shared flag and
    /// posts so the controller applies side effects (silence drums on
    /// disable, redraw the icon) on the main runtime.
    @objc private func togglePercussionSplit(_ sender: NSButton) {
        // Whole-board master: set BOTH sided keys together, then let the
        // controller apply side effects on the main runtime.
        let on = sender.state == .on
        UserDefaults.standard.set(
            on, forKey: KeyboardIconRenderer.percussionLeftDefaultsKey)
        UserDefaults.standard.set(
            on, forKey: KeyboardIconRenderer.percussionRightDefaultsKey)
        NotificationCenter.default.post(
            name: .menuBandPercussionSplitChanged, object: nil)
    }

    /// Open a scroll panel containing the .ips text for every pending
    /// diagnostic report. Built fresh each time so it always reflects
    /// the current contents of `~/Library/Logs/DiagnosticReports/`.
    @objc private func viewCrashLogs(_ sender: NSButton) {
        let logs = CrashLogReader.recentLogs()
        guard !logs.isEmpty else { return }
        crashViewer?.close()
        let ctrl = CrashViewerWindowController(logs: logs)
        crashViewer = ctrl
        ctrl.present()
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
