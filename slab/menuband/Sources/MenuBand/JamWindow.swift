import AppKit

/// Small "Jam" window — the social / come-hang-out surface that used to
/// live at the bottom of the About panel: the Aesthetic.Computer badge
/// and the computer-club invites. Opened from the popover's Jam button
/// (the peer to About), so About stays a tight identity/settings panel.
final class JamWindowController: NSWindowController, NSWindowDelegate {
    /// Self-retain so a popoverVC rebuild (language switch) can't drop
    /// the window mid-interaction. Cleared in `windowWillClose`.
    private static var active: JamWindowController?

    init() {
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 256, height: 210),
            styleMask: [.titled, .closable, .fullSizeContentView],
            backing: .buffered,
            defer: false
        )
        window.titlebarAppearsTransparent = true
        window.titleVisibility = .hidden
        window.isMovableByWindowBackground = true
        window.isReleasedWhenClosed = false
        // Ordinary window level. These secondary windows used to sit at
        // popUpMenu + 1 so the status-bar popover couldn't bury them — but that
        // floated them above every other app on the Mac for as long as they
        // stayed open. Each one activates the app and orders front when shown,
        // which lifts it over the popover at the only moment that matters.
        window.level = .normal
        super.init(window: window)
        window.delegate = self
        buildContent()
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) is not used") }

    @discardableResult
    static func show() -> JamWindowController {
        active?.close()
        let ctrl = JamWindowController()
        ctrl.present()
        return ctrl
    }

    func present() {
        guard let window = window else { return }
        JamWindowController.active = self
        window.center()
        window.level = .normal
        NSApp.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
    }

    func windowWillClose(_ notification: Notification) {
        if JamWindowController.active === self {
            JamWindowController.active = nil
        }
    }

    private func buildContent() {
        guard let window = window else { return }
        let content = NSView()
        content.translatesAutoresizingMaskIntoConstraints = false
        window.contentView = content

        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .centerX
        stack.spacing = 12
        stack.edgeInsets = NSEdgeInsets(top: 40, left: 28, bottom: 26, right: 28)
        stack.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            stack.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            stack.topAnchor.constraint(equalTo: content.topAnchor),
            stack.bottomAnchor.constraint(equalTo: content.bottomAnchor),
        ])

        // Colored-pencil banner — a communal notepat jam (generated in the
        // marketing stack, house style). Sets the come-hang-out tone above
        // the heading + links.
        if let url = Bundle.appResources.url(forResource: "looking-for-players",
                                       withExtension: "png"),
           let banner = NSImage(contentsOf: url) {
            let bannerView = NSImageView()
            bannerView.image = banner
            bannerView.imageScaling = .scaleProportionallyUpOrDown
            bannerView.translatesAutoresizingMaskIntoConstraints = false
            bannerView.wantsLayer = true
            bannerView.layer?.cornerRadius = 8
            bannerView.layer?.masksToBounds = true
            if #available(macOS 10.15, *) {
                bannerView.layer?.cornerCurve = .continuous
            }
            stack.addArrangedSubview(bannerView)
            NSLayoutConstraint.activate([
                bannerView.widthAnchor.constraint(equalToConstant: 200),
                // 3:2 source → keep the aspect for the banner crop.
                bannerView.heightAnchor.constraint(equalToConstant: 133),
            ])
            stack.setCustomSpacing(14, after: bannerView)
        }

        let heading = NSTextField(labelWithString: L("popover.about.lookingForPlayers"))
        heading.font = NSFont.systemFont(ofSize: 18, weight: .bold)
        heading.alignment = .center
        stack.addArrangedSubview(heading)
        stack.setCustomSpacing(14, after: heading)

        // Aesthetic.Computer badge — purple words flanking a pink dot.
        let acPurple = NSColor(red: 167/255, green: 139/255, blue: 250/255, alpha: 1)
        let acLink = MenuBandPopoverViewController.makeLinkButton(
            attr: MenuBandPopoverViewController.aestheticComputerTitle(),
            target: self,
            action: #selector(openAesthetic),
            background: acPurple.withAlphaComponent(0.14),
            border: acPurple.withAlphaComponent(0.55)
        )
        stack.addArrangedSubview(acLink)
        stack.setCustomSpacing(12, after: acLink)

        // NELA invite + "start your own" — the come-hang-out pair.
        let para = NSMutableParagraphStyle()
        para.alignment = .center
        para.lineBreakMode = .byWordWrapping
        let nela = clubLink(L("popover.about.nela"),
                            action: #selector(openNELA),
                            tooltip: "https://nelacomputer.club", para: para,
                            color: NSColor(red: 0.36, green: 0.80, blue: 0.92, alpha: 1))  // cyan
        stack.addArrangedSubview(nela)
        stack.setCustomSpacing(12, after: nela)
        stack.addArrangedSubview(clubLink(L("popover.about.startaclub"),
                                          action: #selector(openStartAClub),
                                          tooltip: "https://startacomputer.club",
                                          para: para,
                                          color: NSColor(red: 1.0, green: 0.56, blue: 0.42, alpha: 1)))  // coral
    }

    private func clubLink(_ text: String, action: Selector,
                          tooltip: String, para: NSParagraphStyle,
                          color: NSColor) -> NSButton {
        // HoverFeedbackButton, not NSButton: these read as links (colored,
        // underlined) but had no pointing-hand cursor and no hover state, so
        // they looked like decorated text.
        let button = HoverFeedbackButton(title: "", target: self, action: action)
        button.attributedTitle = NSAttributedString(
            string: text,
            attributes: [
                .font: NSFont.monospacedSystemFont(ofSize: 10, weight: .regular),
                .foregroundColor: color,
                .underlineStyle: NSUnderlineStyle.single.rawValue,
                .underlineColor: color.withAlphaComponent(0.6),
                .paragraphStyle: para,
            ])
        button.bezelStyle = .regularSquare
        button.isBordered = false
        // Single line, sized to the text — a fixed 200pt width was wrapping
        // longer links onto two lines (looked broken).
        button.cell?.wraps = false
        button.cell?.lineBreakMode = .byClipping
        if let cell = button.cell as? NSButtonCell { cell.imageScaling = .scaleNone }
        button.translatesAutoresizingMaskIntoConstraints = false
        button.toolTip = tooltip
        button.titleBrightensOnHover(hoverColor: .white)
        return button
    }

    @objc private func openAesthetic() {
        // The real browser, like its two row-mates below (NELA, start-a-club).
        // This badge used to open the in-app AestheticWebWindow — the only one
        // of the three that didn't leave the app, and it floats at .statusBar
        // on top of everything besides.
        if let url = URL(string: "https://aesthetic.computer") {
            NSWorkspace.shared.open(url)
        }
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
}
