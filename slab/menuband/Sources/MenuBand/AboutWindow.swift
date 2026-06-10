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
    /// Called when the user picks a language in the flat-map picker, with
    /// the language's own name + short code — the host speaks it through
    /// the Menu Band fx (easter egg). Nil = no audio, just the switch.
    private let onSpeakLanguage: ((String, String) -> Void)?
    /// Called on each icon-card flip to play a drum hit (paired with the
    /// sparkle chord) so flipping the card builds a little beat.
    private let onPlayDrum: (() -> Void)?
    /// Returns the set of currently-sounding MIDI notes, polled on each
    /// `menuBandLitNotesChanged` to drive the icon's live key glow.
    private let litNotesProvider: (() -> Set<UInt8>)?
    private var litNotesObserver: NSObjectProtocol?

    /// Live-updating chrome — rebuilt against the current system accent
    /// whenever `NSColor.systemColorsDidChangeNotification` fires. The icon
    /// and Plugins chip are the two surfaces that take their tint from
    /// `controlAccentColor`; everything else in the About window is brand
    /// art (AC purple, NELA secondary label) and stays put.
    /// The icon ↔ QR two-card stack at the top of the panel: hover fans
    /// the cards apart to peek, clicking shuffles the front card behind.
    private weak var cardStack: CardStackView?
    private weak var pluginsButton: HoverLinkButton?
    private var accentObserver: NSObjectProtocol?

    /// Strong ref so the viewer window stays alive while it's open —
    /// AboutWindowController is the only thing that holds it.
    private var crashViewer: CrashViewerWindowController?

    init(updateInfo: UpdateChecker.VersionInfo?,
         onOpenPlugins: (() -> Void)? = nil,
         onSpeakLanguage: ((String, String) -> Void)? = nil,
         onPlayDrum: (() -> Void)? = nil,
         litNotesProvider: (() -> Set<UInt8>)? = nil) {
        self.updateInfo = updateInfo
        self.onOpenPlugins = onOpenPlugins
        self.onSpeakLanguage = onSpeakLanguage
        self.onPlayDrum = onPlayDrum
        self.litNotesProvider = litNotesProvider
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

        // Dev affordances (mirroring the showAbout/showPopover remotes):
        // drive the icon-card hover/flip from the shell so the animation
        // can be triggered and screenshotted without a real pointer.
        let dnc = DistributedNotificationCenter.default()
        debugObservers.append(dnc.addObserver(
            forName: .init("computer.aestheticcomputer.menuband.cardHover"),
            object: nil, queue: .main
        ) { [weak self] _ in
            self?.cardHoverPreview.toggle()
            self?.cardStack?.setHoverPreview(self?.cardHoverPreview ?? false)
        })
        debugObservers.append(dnc.addObserver(
            forName: .init("computer.aestheticcomputer.menuband.cardFlip"),
            object: nil, queue: .main
        ) { [weak self] _ in
            self?.cardStack?.flip()
        })

        // Live icon keyboard: refresh the lit keys whenever the held-note
        // set changes while the About window is open.
        litNotesObserver = NotificationCenter.default.addObserver(
            forName: .menuBandLitNotesChanged, object: nil, queue: .main
        ) { [weak self] _ in
            self?.refreshLitKeys()
        }
    }

    /// Map the currently-held MIDI notes onto the icon's 5 chromatic keys
    /// (C C♯ D D♯ E) and light them. Pitch class folds onto the five keys
    /// so any octave lands consistently and higher notes still animate.
    private func refreshLitKeys() {
        guard let provider = litNotesProvider else { return }
        let indices = Set(provider().map { Int($0) % 12 % 5 })
        cardStack?.setLitKeys(indices)
    }

    /// Tracks the toggled state of the `cardHover` debug remote.
    private var cardHoverPreview = false
    private var debugObservers: [NSObjectProtocol] = []

    required init?(coder: NSCoder) {
        fatalError("init(coder:) is not used")
    }

    deinit {
        flashTimer?.invalidate()
        if let observer = accentObserver {
            NotificationCenter.default.removeObserver(observer)
        }
        let dnc = DistributedNotificationCenter.default()
        for observer in debugObservers { dnc.removeObserver(observer) }
        if let o = litNotesObserver { NotificationCenter.default.removeObserver(o) }
    }

    /// Open or refocus the singleton About window. Dedupes via the
    /// static `active` registry so callers don't have to track the
    /// instance — and so a popoverVC rebuild can't accidentally
    /// orphan a live window.
    @discardableResult
    static func show(updateInfo: UpdateChecker.VersionInfo?,
                     onOpenPlugins: (() -> Void)? = nil,
                     onSpeakLanguage: ((String, String) -> Void)? = nil,
                     onPlayDrum: (() -> Void)? = nil,
                     litNotesProvider: (() -> Set<UInt8>)? = nil) -> AboutWindowController {
        active?.close()
        let ctrl = AboutWindowController(updateInfo: updateInfo,
                                         onOpenPlugins: onOpenPlugins,
                                         onSpeakLanguage: onSpeakLanguage,
                                         onPlayDrum: onPlayDrum,
                                         litNotesProvider: litNotesProvider)
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

        // App icon — centered directly above the title, the same
        // masthead rhythm as the standard macOS "About This Mac" panel.
        // Prefer the hue-rotated tint that IconTinter generates against
        // the current accent (matches the Finder icon the user sees
        // everywhere else); fall back to the un-tinted bundle icon if
        // the tinter can't reach the source (e.g. swift-run dev builds
        // without an .app wrapper).
        if let icon = IconTinter.tintedIcon() ?? Self.loadAppIcon() {
            // Two physical cards — the app icon in front, a QR code to
            // prompt.ac/menuband behind. Hover fans them apart so the QR
            // peeks out; clicking shuffles the front card behind the
            // other (and plays a sparkle + particle burst). Modeled on
            // the stacked card-deck in aesthetic.computer's wg.mjs.
            let qr = Self.qrCodeImage(for: "https://prompt.ac/menuband", side: 96)
            let cards = CardStackView(front: icon, back: qr)
            cards.translatesAutoresizingMaskIntoConstraints = false
            cards.toolTip = "prompt.ac/menuband"
            cards.onFlip = { [weak self] in
                EasterEggChord.shared.play()
                self?.onPlayDrum?()
            }
            stack.addArrangedSubview(cards)
            cardStack = cards
        }

        // "Menu Band" is a coined brand name (à la GarageBand / Logic
        // Pro), so it stays constant across languages — Apple localizes
        // descriptive app names but never brand ones. The surrounding
        // copy (tagline, links) is what gets localized.
        let nameLabel = NSTextField(labelWithString: "Menu Band")
        nameLabel.font = NSFont.systemFont(ofSize: 18, weight: .bold)
        nameLabel.alignment = .center
        stack.addArrangedSubview(nameLabel)
        stack.setCustomSpacing(10, after: nameLabel)

        // (Version moved to a small gray line above the copyright at the
        // bottom — see the footer — matching how Terminal / Messages
        // close their About panels.)

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
        body.translatesAutoresizingMaskIntoConstraints = false
        body.preferredMaxLayoutWidth = 264
        // Host the tagline in a fixed-height box and center it vertically.
        // Translations wrap to 2–3 lines at different lengths; a constant
        // box keeps the panel from reflowing / resizing as the user flips
        // through the language map.
        let bodyBox = NSView()
        bodyBox.translatesAutoresizingMaskIntoConstraints = false
        bodyBox.addSubview(body)
        NSLayoutConstraint.activate([
            bodyBox.widthAnchor.constraint(equalToConstant: 264),
            bodyBox.heightAnchor.constraint(equalToConstant: 48),
            body.leadingAnchor.constraint(equalTo: bodyBox.leadingAnchor),
            body.trailingAnchor.constraint(equalTo: bodyBox.trailingAnchor),
            body.centerYAnchor.constraint(equalTo: bodyBox.centerYAnchor),
        ])
        stack.addArrangedSubview(bodyBox)

        // (Aesthetic.Computer badge + the NELA / start-a-club invites
        // moved to the popover's "Jam" button → JamWindow. About stays a
        // focused identity/settings panel.)

        // [v1 cutoff] Pedals (AU plugin picker) chip removed — Liam Hall
        // pedals / third-party AU hosting are post-v1. The `onOpenPlugins`
        // hook + AU picker code stay dormant for the post-release revival.

        // Language — a flat map of every supported language, shown all at
        // once as chiclet cells (flag + native label) like the instrument
        // picker. Click switches the language AND speaks its own name
        // through the Menu Band fx (easter egg). Replaces the chip+dropdown.
        stack.setCustomSpacing(16, after: stack.arrangedSubviews.last ?? stack)
        let langMap = LanguageMapView()
        langMap.items = Localization.supported.map {
            LanguageMapView.Item(code: $0.code, label: $0.label, flag: $0.flag)
        }
        langMap.selectedCode = Localization.current
        langMap.translatesAutoresizingMaskIntoConstraints = false
        langMap.onPick = { [weak self] item in self?.applyLanguage(item) }
        langMap.widthAnchor.constraint(equalToConstant: 264).isActive = true
        // Height comes from the view's intrinsic size (rows × rowH), so the
        // grid grows with the number of languages.
        stack.addArrangedSubview(langMap)
        let langRow: NSView = langMap

        // "Looking For Players?" — opens the Jam window (Aesthetic.Computer
        // badge + computer-club invites). Lives right under the language
        // chip so the come-hang-out surface is one tap from identity, and
        // replaces the old purple "Jam" button that used to sit in the
        // popover footer.
        stack.setCustomSpacing(12, after: langRow)
        let playersLink = NSButton(title: "",
                                   target: self,
                                   action: #selector(openLookingForPlayers(_:)))
        let acPurple = NSColor(red: 167/255, green: 139/255, blue: 250/255, alpha: 1)
        playersLink.attributedTitle = NSAttributedString(
            string: L("popover.about.lookingForPlayers"),
            attributes: [
                .font: NSFont.systemFont(ofSize: 11, weight: .semibold),
                .foregroundColor: acPurple,
                .underlineStyle: NSUnderlineStyle.single.rawValue,
            ])
        playersLink.bezelStyle = .regularSquare
        playersLink.isBordered = false
        playersLink.translatesAutoresizingMaskIntoConstraints = false
        playersLink.toolTip = "Aesthetic.Computer · computer clubs"
        stack.addArrangedSubview(playersLink)

        // [v1 cutoff] "Cassette deck (beta)" + "Percussion split" beta
        // checkboxes removed — both features are post-v1. Their flags
        // default off and their live shortcuts stay in place, so the
        // toggles can return to About after release.

        // "Use AC OS MIDI" — route melodic GM notes through the native
        // Aesthetic Computer gm_synth voices (modal piano, FM Rhodes,
        // Karplus pluck, modal bank, synth bass) instead of the system
        // soundfont. Programs gm_synth doesn't implement fall back to
        // MIDISynth, so the grid always sounds. Off by default.
        stack.setCustomSpacing(14, after: playersLink)
        let acMIDIRow = NSStackView()
        acMIDIRow.orientation = .vertical
        acMIDIRow.alignment = .leading
        acMIDIRow.spacing = 1
        acMIDIRow.translatesAutoresizingMaskIntoConstraints = false
        let acMIDIToggle = NSButton(
            checkboxWithTitle: "Use AC OS MIDI",
            target: self,
            action: #selector(toggleUseACMIDI(_:)))
        acMIDIToggle.state = UserDefaults.standard
            .bool(forKey: MenuBandSynth.useACMIDIDefaultsKey) ? .on : .off
        acMIDIToggle.font = NSFont.systemFont(ofSize: 11, weight: .semibold)
        acMIDIToggle.translatesAutoresizingMaskIntoConstraints = false
        let acMIDISub = NSTextField(labelWithString:
            "synthesize GM instruments with the Aesthetic Computer engine "
            + "instead of the system soundfont")
        acMIDISub.font = NSFont.systemFont(ofSize: 9, weight: .regular)
        acMIDISub.textColor = .secondaryLabelColor
        acMIDISub.lineBreakMode = .byWordWrapping
        acMIDISub.maximumNumberOfLines = 2
        acMIDISub.translatesAutoresizingMaskIntoConstraints = false
        acMIDIRow.addArrangedSubview(acMIDIToggle)
        acMIDIRow.addArrangedSubview(acMIDISub)
        stack.addArrangedSubview(acMIDIRow)
        acMIDIRow.widthAnchor.constraint(
            equalTo: stack.widthAnchor, constant: -56).isActive = true

        // Crash-report summary — single orange ⚠️ button reading
        // "Menu Band crashed N times". Opens the scroll viewer where
        // the user can review the .ips contents and click Send to
        // Aesthetic.Computer. Lives bottom-left of the About window
        // (secondary-action position) so it never crowds the
        // primary brand chrome.
        let logs = CrashLogReader.recentLogs()
        if !logs.isEmpty {
            stack.setCustomSpacing(14, after: playersLink)
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
            stack.setCustomSpacing(16, after: playersLink)
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

        // (The QR code lives as a click-to-reveal easter egg on the app
        // icon above — see toggleIconQR — rather than a standing footer
        // badge, keeping the panel Apple-clean.)

        // Version line — small gray, sitting just above the copyright,
        // the way Terminal / Messages close their About panels.
        let version = (Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String) ?? "?"
        let build = (Bundle.main.infoDictionary?["CFBundleVersion"] as? String) ?? "?"
        let versionWord = L("popover.about.version")
        let versionString = build == version
            ? "\(versionWord) \(version)"
            : "\(versionWord) \(version) (\(build))"
        let versionLabel = NSTextField(labelWithString: versionString)
        versionLabel.font = NSFont.systemFont(ofSize: 11)
        versionLabel.textColor = .secondaryLabelColor
        versionLabel.alignment = .center
        stack.setCustomSpacing(16, after: stack.arrangedSubviews.last ?? stack)
        stack.addArrangedSubview(versionLabel)
        stack.setCustomSpacing(4, after: versionLabel)

        // Faint copyright line — the quiet footer every macOS About
        // panel closes with (Terminal: "© 1991–2025 Apple Inc.",
        // Finder: "™ & © 1983–2025 …"). Tertiary gray so it recedes
        // beneath the brand chrome.
        let copyright = NSTextField(labelWithString: "© 2026 Aesthetic, Inc.")
        copyright.font = NSFont.systemFont(ofSize: 11)
        copyright.textColor = .tertiaryLabelColor
        copyright.alignment = .center
        stack.addArrangedSubview(copyright)
    }

    /// Open the Jam window ("Looking For Players?") — the social
    /// come-hang-out surface (Aesthetic.Computer + computer-club invites).
    @objc private func openLookingForPlayers(_ sender: Any?) {
        JamWindowController.show()
    }

    /// Pick a language from the flat map: speak its own name through the
    /// Menu Band fx (easter egg — works even when re-picking the active
    /// language), then switch + rebuild the panel so every string re-reads.
    private func applyLanguage(_ item: LanguageMapView.Item) {
        // Speak first so the audio fires regardless of whether this is a
        // real switch — re-clicking your current language still talks.
        onSpeakLanguage?(item.label, item.code)
        guard item.code != Localization.current else { return }
        Localization.current = item.code
        // Rebuild the About content to apply the new strings. Cheaper than
        // tearing down the window.
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

    /// Persist the "Use AC OS MIDI" flag and notify the runtime so the
    /// synth flips melodic routing between the native gm_synth voices and
    /// the system soundfont. AppDelegate forwards the new value to the synth.
    @objc private func toggleUseACMIDI(_ sender: NSButton) {
        let on = sender.state == .on
        UserDefaults.standard.set(on, forKey: MenuBandSynth.useACMIDIDefaultsKey)
        NotificationCenter.default.post(
            name: .menuBandUseACMIDIChanged, object: nil)
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
        if let tinted = IconTinter.tintedIcon() ?? Self.loadAppIcon() {
            cardStack?.setIconImage(tinted)
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

/// Two stacked "cards" — the app icon in front, a QR code behind —
/// modeled on the card-deck in aesthetic.computer's wg.mjs (absolutely
/// stacked layers, offset drop shadows for depth). Hovering fans the
/// back card out to peek; clicking shuffles the front card behind the
/// other. The controller wires `onFlip` to play a sparkle + particles.
final class CardStackView: NSView {
    /// Fired on each click (the shuffle), for sound + particle juice.
    var onFlip: (() -> Void)?

    private let iconCard = CALayer()
    private let qrCard = CALayer()
    /// Which chromatic keys (0…4 = C C♯ D D♯ E) are currently lit. The
    /// icon face is drawn dynamically (see `iconImage`), so lighting a key
    /// just regenerates the face — the keyboard is part of the icon, not an
    /// overlay on a static graphic.
    private var litKeyIndices: Set<Int> = []
    /// Distinct vivid tint per key so the lit keyboard reads as a small
    /// rainbow, matching the colorful menubar piano aesthetic.
    private static let keyColors: [NSColor] = [
        NSColor(srgbRed: 1.00, green: 0.30, blue: 0.42, alpha: 1), // C  red-pink
        NSColor(srgbRed: 1.00, green: 0.60, blue: 0.18, alpha: 1), // C♯ orange
        NSColor(srgbRed: 1.00, green: 0.84, blue: 0.22, alpha: 1), // D  yellow
        NSColor(srgbRed: 0.20, green: 0.82, blue: 0.70, alpha: 1), // D♯ teal
        NSColor(srgbRed: 0.38, green: 0.62, blue: 1.00, alpha: 1), // E  blue
    ]
    /// Which card is currently in front. Starts on the icon.
    private var frontIsIcon = true
    private var hovering = false
    private var hoverTracking: NSTrackingArea?
    /// Container holding both cards. Spins as a unit on each click;
    /// keeping the spin on the deck (not the cards) means it composes
    /// cleanly with the cards' own slot-swap springs, and additive
    /// impulses stack so rapid clicks build a continuous spin.
    private let deck = CALayer()

    /// The card's full bounding box (both cards share it). The app-icon
    /// art fills this with its own transparent margin (the macOS icon
    /// grid); the QR tile is inset by that SAME margin so the two cards'
    /// VISIBLE sizes line up instead of the QR overflowing the icon.
    private let cardSide: CGFloat = 108
    /// Transparent margin baked into a macOS app icon — the icon grid
    /// insets the rounded square by (1024−824)/2/1024 ≈ 0.098 of the
    /// canvas on each side. The QR tile matches it.
    private let artMarginRatio: CGFloat = 0.098

    init(front: NSImage?, back: NSImage?) {
        super.init(frame: .zero)
        wantsLayer = true
        layer?.masksToBounds = false
        configureIcon(image: front)
        configureQR(image: back)
        // Add QR first so it sits behind; z-positions are managed in
        // `relayout` and take over once laid out. Both live in the
        // spinning `deck` container.
        deck.addSublayer(qrCard)
        deck.addSublayer(iconCard)
        layer?.addSublayer(deck)
    }

    /// Continuous-corner radius as a fraction of the side — the macOS
    /// app-icon "squircle" ratio, so the QR card's corners match the
    /// app icon's rounding.
    private static let cornerRatio: CGFloat = 0.2235

    required init?(coder: NSCoder) { fatalError() }

    /// Reserve room for the fanned-out back card so the deck stays
    /// centered in the panel and the peek isn't clipped by siblings.
    override var intrinsicContentSize: NSSize { NSSize(width: 172, height: 144) }

    override var isFlipped: Bool { false }

    // Let a click work even when the About window isn't key.
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }

    override func resetCursorRects() {
        addCursorRect(bounds, cursor: .pointingHand)
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let existing = hoverTracking { removeTrackingArea(existing) }
        let area = NSTrackingArea(
            rect: bounds,
            options: [.mouseEnteredAndExited, .activeInActiveApp],
            owner: self, userInfo: nil)
        addTrackingArea(area)
        hoverTracking = area
    }

    override func layout() {
        super.layout()
        let scale = window?.backingScaleFactor ?? 2
        iconCard.contentsScale = scale
        qrCard.contentsScale = scale
        // The deck fills the view and pivots about its center so a spin
        // rotates the whole pair around the middle.
        CATransaction.begin(); CATransaction.setDisableActions(true)
        deck.anchorPoint = CGPoint(x: 0.5, y: 0.5)
        deck.bounds = CGRect(origin: .zero, size: bounds.size)
        deck.position = CGPoint(x: bounds.midX, y: bounds.midY)
        CATransaction.commit()
        relayout(animated: false)
    }

    override func mouseEntered(with event: NSEvent) { setHoverPreview(true) }
    override func mouseExited(with event: NSEvent) { setHoverPreview(false) }
    override func mouseDown(with event: NSEvent) { flip() }

    // MARK: - Public (also driven by debug notifications for validation)

    /// Fan the cards out (or back) as if hovered.
    func setHoverPreview(_ on: Bool) {
        guard hovering != on else { return }
        hovering = on
        // Don't fight an in-flight shuffle; it settles into the right
        // hover/rest layout when the queue drains.
        if !shuffling { relayout(animated: true, springDamping: 15) }
    }

    // MARK: - Shuffle

    /// Queued count of pending tucks and whether one is animating.
    /// Tucks run strictly one-at-a-time so the two cards never overlap
    /// mid-swap — that's what keeps the z-order clean (no flicker) and
    /// lets rapid clicks read as a continuous shuffle (the queue drains
    /// back-to-back).
    private var shuffleQueue = 0
    private var shuffling = false
    private var shuffleTick = 0

    /// Enqueue one "tuck the top card behind the other" shuffle. Fires
    /// the juice callback (throttled chord + particles) per click.
    func flip() {
        shuffleQueue += 1
        emitNotesBurst()
        onFlip?()
        if !shuffling { startNextTuck() }
    }

    /// A burst of little music notes puffing out from BETWEEN the two
    /// cards (the emitter sits at a z between the back and front card),
    /// so the front card partly occludes them — they read as coming
    /// from the seam, not floating on top.
    private func emitNotesBurst() {
        guard bounds.width > 0 else { return }
        let emitter = CAEmitterLayer()
        emitter.emitterPosition = CGPoint(x: deck.bounds.midX,
                                          y: deck.bounds.midY)
        // Emit from a single point at the seam as an upward fountain.
        emitter.emitterShape = .point
        emitter.emitterMode = .points
        // Normal alpha blending (not additive) so the notes keep their
        // own color + hard dark shadow and stay legible instead of
        // blowing out to white where they overlap.
        emitter.renderMode = .unordered
        // Between the back card (z 5) and the front card (z 10).
        emitter.zPosition = 7

        let colors: [NSColor] = [
            NSColor(red: 167/255, green: 139/255, blue: 250/255, alpha: 1), // purple
            NSColor(red: 255/255, green: 107/255, blue: 157/255, alpha: 1), // pink
            NSColor(red: 158/255, green: 212/255, blue: 80/255, alpha: 1),  // chartreuse
        ]
        emitter.emitterCells = colors.map { color in
            let cell = CAEmitterCell()
            cell.contents = Self.noteImage(color: color)?
                .cgImage(forProposedRect: nil, context: nil, hints: nil)
            // Just a few notes: low birth rate over a very short burst
            // (≈3 cells × rate × window → a handful total).
            cell.birthRate = 32
            // Fall longer: live long and arc down under gravity.
            cell.lifetime = 2.4
            cell.lifetimeRange = 0.5
            // Pop UP: strong initial upward velocity in a tight cone.
            cell.velocity = 130
            cell.velocityRange = 45
            cell.emissionLongitude = .pi / 2     // straight up (+y)
            cell.emissionRange = .pi / 4         // ~45° upward cone
            cell.yAcceleration = -200            // gravity → arc back down
            cell.scale = 0.7
            cell.scaleRange = 0.2
            cell.scaleSpeed = -0.05              // barely shrink → readable
            // Fade less: start opaque, decay slowly over the long fall.
            cell.alphaSpeed = -0.3
            // Less rotation so the glyphs stay legible.
            cell.spin = 0.4
            cell.spinRange = 0.8
            return cell
        }
        deck.addSublayer(emitter)
        // Very short emission window → only a handful of notes total.
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.06) {
            emitter.birthRate = 0
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 3.2) {
            emitter.removeFromSuperlayer()
        }
    }

    /// A small music-note glyph rendered to an image in `color`, with a
    /// hard dark drop shadow (blur 0) for legibility against the panel.
    private static func noteImage(color: NSColor) -> NSImage? {
        let side: CGFloat = 24
        let image = NSImage(size: NSSize(width: side, height: side))
        image.lockFocus()
        let glyph = ["♪", "♫", "♩", "♬"].randomElement() ?? "♪"
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.black.withAlphaComponent(0.85)
        shadow.shadowBlurRadius = 0                       // sharp, not soft
        shadow.shadowOffset = NSSize(width: 1.5, height: -1.5)
        shadow.set()
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 16, weight: .bold),
            .foregroundColor: color,
        ]
        let s = NSAttributedString(string: glyph, attributes: attrs)
        let sz = s.size()
        s.draw(at: NSPoint(x: (side - sz.width) / 2 - 0.75,
                           y: (side - sz.height) / 2 + 0.75))
        image.unlockFocus()
        return image
    }

    private func startNextTuck() {
        guard shuffleQueue > 0, bounds.width > 0 else {
            shuffling = false
            // Settle into the resting (or hovered) stack layout.
            relayout(animated: true, springDamping: 16)
            return
        }
        shuffling = true
        shuffleQueue -= 1
        shuffleTick += 1

        let base = CGPoint(x: deck.bounds.midX, y: deck.bounds.midY)
        let moving = frontIsIcon ? iconCard : qrCard    // current front → behind
        let staying = frontIsIcon ? qrCard : iconCard   // current back → front

        // Exit vector: mostly downward (the window has vertical room
        // there, so the card clears completely without being clipped by
        // the window edge) with a slightly different angle each time.
        // Distance > cardSide ⇒ the moving card ends up ENTIRELY outside
        // the other card's box before the z-swap, so the swap never
        // happens mid-overlap — no z-fighting, the illusion holds.
        let jitter = CGFloat.random(in: -0.7...0.7)
        let angle = -CGFloat.pi / 2 + jitter            // down ± ~40°
        let dist = cardSide * 1.25
        let ux = cos(angle), uy = sin(angle)
        let out = CGPoint(x: base.x + ux * dist, y: base.y + uy * dist)
        let nudge = CGPoint(x: base.x - ux * cardSide * 0.14,
                            y: base.y - uy * cardSide * 0.14)
        let half: CFTimeInterval = 0.15

        // Phase 1 — moving slides OUT over the top (z above everything);
        // staying rises to the front slot and nudges out of the way.
        moving.zPosition = 100
        staying.zPosition = 50
        move(moving, to: out, scale: 0.96, dur: half, timing: .easeIn)
        move(staying, to: nudge, scale: 1.0, dur: half, timing: .easeOut)

        CATransaction.begin()
        CATransaction.setCompletionBlock { [weak self] in
            guard let self = self else { return }
            // Fully clear now — drop it behind with zero overlap.
            moving.zPosition = 10
            // Phase 2 — moving returns to the (behind) slot; staying
            // centers as the new front.
            self.move(moving, to: base, scale: 0.9, dur: half, timing: .easeOut)
            self.move(staying, to: base, scale: 1.0, dur: half,
                      timing: .easeInEaseOut)
            CATransaction.begin()
            CATransaction.setCompletionBlock { [weak self] in
                guard let self = self else { return }
                self.frontIsIcon.toggle()   // swap committed
                self.startNextTuck()        // keep shuffling if queued
            }
            CATransaction.commit()
        }
        CATransaction.commit()
    }

    /// Animate a card's position + uniform scale together, reading the
    /// from-value off the live presentation so re-entrant moves chain.
    private func move(_ card: CALayer, to center: CGPoint, scale: CGFloat,
                      dur: CFTimeInterval, timing: CAMediaTimingFunctionName) {
        let transform = CATransform3DMakeScale(scale, scale, 1)
        let pos = CABasicAnimation(keyPath: "position")
        pos.fromValue = card.presentation()?.position ?? card.position
        pos.toValue = center
        let tform = CABasicAnimation(keyPath: "transform")
        tform.fromValue = card.presentation()?.transform ?? card.transform
        tform.toValue = transform
        let group = CAAnimationGroup()
        group.animations = [pos, tform]
        group.duration = dur
        group.timingFunction = CAMediaTimingFunction(name: timing)
        card.add(group, forKey: "move")
        card.position = center
        card.transform = transform
    }

    /// Kept for API compatibility with the accent-refresh path. The icon
    /// face is now drawn dynamically (fixed purple identity), so an
    /// accent re-tint just redraws the current state rather than swapping
    /// in a hue-rotated PNG.
    func setIconImage(_ image: NSImage) {
        redrawIcon()
    }

    // MARK: - Geometry

    /// The app-icon card. The face is drawn dynamically by `iconImage` —
    /// the purple squircle + the piano keyboard are one live drawing, so
    /// notes light real keys instead of glowing over a static PNG.
    private func configureIcon(image: NSImage?) {
        iconCard.bounds = CGRect(x: 0, y: 0, width: cardSide, height: cardSide)
        iconCard.anchorPoint = CGPoint(x: 0.5, y: 0.5)
        iconCard.contentsGravity = .resizeAspect
        iconCard.contentsScale = 2
        applyCardShadow(iconCard, path: nil)
        redrawIcon()
    }

    /// Light the icon's piano keys for the given chromatic key indices
    /// (0…4 = C C♯ D D♯ E). Driven live from held notes by the About
    /// window; empty clears them. Cheap — redraws one small face image.
    func setLitKeys(_ indices: Set<Int>) {
        guard indices != litKeyIndices else { return }
        litKeyIndices = indices
        redrawIcon()
    }

    private func redrawIcon() {
        CATransaction.begin(); CATransaction.setDisableActions(true)
        iconCard.contents = Self.iconImage(litKeys: litKeyIndices)
            .cgImage(forProposedRect: nil, context: nil, hints: nil)
        CATransaction.commit()
    }

    /// Draw the Menu Band app icon from scratch: a purple gradient
    /// squircle with a cream piano keyboard (3 white + 2 black keys).
    /// `litKeys` paints those chromatic keys in their vivid tints so the
    /// icon becomes a live keyboard. Top-left origin (`flipped: true`).
    static func iconImage(litKeys: Set<Int>, px: CGFloat = 256) -> NSImage {
        // Palette sampled from the original AppIcon art.
        let tl = NSColor(srgbRed: 211/255, green: 80/255, blue: 196/255, alpha: 1)
        let br = NSColor(srgbRed: 67/255, green: 29/255, blue: 113/255, alpha: 1)
        let cream = NSColor(srgbRed: 246/255, green: 242/255, blue: 232/255, alpha: 1)
        let black = NSColor(srgbRed: 27/255, green: 26/255, blue: 25/255, alpha: 1)
        let edge = NSColor(srgbRed: 40/255, green: 28/255, blue: 22/255, alpha: 1)
        func R(_ x0: CGFloat, _ y0: CGFloat, _ x1: CGFloat, _ y1: CGFloat) -> CGRect {
            CGRect(x: x0 * px, y: y0 * px, width: (x1 - x0) * px, height: (y1 - y0) * px)
        }
        return NSImage(size: NSSize(width: px, height: px), flipped: true) { _ in
            // Squircle body (macOS icon margin baked in so it matches the
            // QR card's visible size).
            let margin = px * 0.098
            let sq = CGRect(x: margin, y: margin, width: px - 2 * margin,
                            height: px - 2 * margin)
            let radius = sq.width * 0.2235
            let body = NSBezierPath(roundedRect: sq, xRadius: radius, yRadius: radius)
            if let ctx = NSGraphicsContext.current?.cgContext {
                ctx.saveGState()
                body.addClip()
                let cs = CGColorSpaceCreateDeviceRGB()
                if let g = CGGradient(colorsSpace: cs,
                                      colors: [tl.cgColor, br.cgColor] as CFArray,
                                      locations: [0, 1]) {
                    // Flipped context: (minX,minY) is top-left, (maxX,maxY)
                    // bottom-right → magenta TL to deep-purple BR.
                    ctx.drawLinearGradient(
                        g, start: CGPoint(x: sq.minX, y: sq.minY),
                        end: CGPoint(x: sq.maxX, y: sq.maxY), options: [])
                }
                ctx.restoreGState()
            } else {
                tl.setFill(); body.fill()
            }

            // Cream keyboard plate.
            let plate = R(0.254, 0.428, 0.744, 0.570)
            let platePath = NSBezierPath(roundedRect: plate,
                                         xRadius: px * 0.012, yRadius: px * 0.012)
            cream.setFill(); platePath.fill()

            // Lit white keys (drawn before dividers / black keys on top).
            let whites: [(Int, CGFloat, CGFloat)] =
                [(0, 0.254, 0.417), (2, 0.417, 0.580), (4, 0.580, 0.744)]
            for (idx, x0, x1) in whites where litKeys.contains(idx) {
                keyColors[idx].setFill()
                NSBezierPath(roundedRect: CGRect(x: x0 * px + 2, y: 0.428 * px + 2,
                                                 width: (x1 - x0) * px - 4,
                                                 height: 0.142 * px - 4),
                             xRadius: px * 0.01, yRadius: px * 0.01).fill()
            }

            // White-key dividers.
            edge.setStroke()
            for xf: CGFloat in [0.417, 0.580] {
                let p = NSBezierPath()
                p.move(to: CGPoint(x: xf * px, y: 0.432 * px))
                p.line(to: CGPoint(x: xf * px, y: 0.566 * px))
                p.lineWidth = px * 0.006
                p.stroke()
            }
            // Plate outline.
            edge.setStroke(); platePath.lineWidth = px * 0.008; platePath.stroke()

            // Black keys (lit → their tint, else black).
            let blacks: [(Int, CGFloat, CGFloat)] =
                [(1, 0.369, 0.463), (3, 0.535, 0.629)]
            for (idx, x0, x1) in blacks {
                (litKeys.contains(idx) ? keyColors[idx] : black).setFill()
                NSBezierPath(roundedRect: CGRect(x: x0 * px, y: 0.428 * px,
                                                 width: (x1 - x0) * px,
                                                 height: 0.098 * px),
                             xRadius: px * 0.01, yRadius: px * 0.01).fill()
            }
            return true
        }
    }

    /// The QR card. A white rounded "tile" (matching the icon's
    /// squircle corner curve) with the code inset by a quiet-zone
    /// margin so the rounding never clips the finder patterns. The
    /// shadow rides on the container via an explicit rounded
    /// `shadowPath` (the white face clips its own contents, which would
    /// otherwise clip the shadow too).
    private func configureQR(image: NSImage?) {
        qrCard.bounds = CGRect(x: 0, y: 0, width: cardSide, height: cardSide)
        qrCard.anchorPoint = CGPoint(x: 0.5, y: 0.5)
        qrCard.masksToBounds = false

        // Inset the white tile by the same transparent margin the app
        // icon carries, so the QR's VISIBLE size matches the icon
        // squircle (the margin is now "counted" in the shared box).
        let margin = cardSide * artMarginRatio
        let faceRect = qrCard.bounds.insetBy(dx: margin, dy: margin)
        let r = faceRect.width * Self.cornerRatio
        applyCardShadow(qrCard, path: CGPath(
            roundedRect: faceRect, cornerWidth: r, cornerHeight: r,
            transform: nil))

        let face = CALayer()
        face.frame = faceRect
        face.backgroundColor = NSColor.white.cgColor
        face.cornerRadius = r
        face.cornerCurve = .continuous
        face.masksToBounds = true

        // Quiet-zone inset for the code within the white tile so the
        // rounded corners never clip the finder patterns.
        let code = CALayer()
        let pad = faceRect.width * 0.11
        code.frame = CGRect(x: pad, y: pad,
                            width: faceRect.width - pad * 2,
                            height: faceRect.height - pad * 2)
        code.contents = image?.cgImage(
            forProposedRect: nil, context: nil, hints: nil)
        code.contentsGravity = .resizeAspect
        code.contentsScale = 2
        face.addSublayer(code)
        qrCard.addSublayer(face)
    }

    /// Offset drop shadow shared by both cards — the wg deck's
    /// `box-shadow: 4px 4px 12px rgba(0,0,0,.75)` feel.
    private func applyCardShadow(_ card: CALayer, path: CGPath?) {
        card.shadowColor = NSColor.black.cgColor
        card.shadowOpacity = 0.45
        card.shadowRadius = 6
        card.shadowOffset = CGSize(width: 3, height: -3)   // down-right
        card.shadowPath = path
        card.masksToBounds = false
    }

    /// Target center for whichever card is in front / behind. Hover is
    /// a gentle peek — just enough to show there are two cards.
    private func frontCenter(_ base: CGPoint) -> CGPoint {
        hovering ? CGPoint(x: base.x - 3, y: base.y - 2) : base
    }
    private func backCenter(_ base: CGPoint) -> CGPoint {
        let dx: CGFloat = hovering ? 13 : 6
        let dy: CGFloat = hovering ? 9 : 5
        return CGPoint(x: base.x + dx, y: base.y + dy)
    }
    private func backTilt() -> CGFloat { (hovering ? 6 : 3) * .pi / 180 }

    // MARK: - Layout / animation

    /// Settle both cards into their front/back slots with a spring, for
    /// rest ↔ hover transitions (and the initial non-animated layout).
    private func relayout(animated: Bool, springDamping: CGFloat = 18) {
        guard bounds.width > 0 else { return }
        let base = CGPoint(x: bounds.midX, y: bounds.midY)
        let front = frontIsIcon ? iconCard : qrCard
        let back = frontIsIcon ? qrCard : iconCard

        settle(front, center: frontCenter(base), rotation: 0,
               scale: hovering ? 1.02 : 1.0, z: 10,
               shadowRadius: hovering ? 8 : 6,
               animated: animated, damping: springDamping)
        settle(back, center: backCenter(base), rotation: backTilt(),
               scale: hovering ? 0.92 : 0.9, z: 5,
               shadowRadius: hovering ? 8 : 6,
               animated: animated, damping: springDamping)
    }

    /// Spring a card to a slot (position + transform), with the model
    /// values set so it rests there once the spring settles.
    private func settle(_ card: CALayer, center: CGPoint, rotation: CGFloat,
                        scale: CGFloat, z: CGFloat, shadowRadius: CGFloat,
                        animated: Bool, damping: CGFloat) {
        let transform = CATransform3DScale(
            CATransform3DMakeRotation(rotation, 0, 0, 1), scale, scale, 1)
        card.zPosition = z
        if animated {
            let pos = CASpringAnimation(keyPath: "position")
            pos.fromValue = card.presentation()?.position ?? card.position
            pos.toValue = center
            pos.damping = damping; pos.stiffness = 240; pos.mass = 1
            pos.duration = pos.settlingDuration
            let tform = CASpringAnimation(keyPath: "transform")
            tform.fromValue = card.presentation()?.transform ?? card.transform
            tform.toValue = transform
            tform.damping = damping; tform.stiffness = 240; tform.mass = 1
            tform.duration = tform.settlingDuration
            card.add(pos, forKey: "pos")
            card.add(tform, forKey: "tform")
        }
        card.position = center
        card.transform = transform
        card.shadowRadius = shadowRadius
    }
}
