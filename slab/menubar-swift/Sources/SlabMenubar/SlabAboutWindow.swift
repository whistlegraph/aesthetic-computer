import AppKit

/// A small native About window shared by every Slab host. It deliberately
/// mirrors MacPal's calm masthead layout while using the installed Slab app
/// icon, so the status item finally has a visible identity outside the menu.
final class SlabAboutWindow: NSObject, NSWindowDelegate {
    private static var shared: SlabAboutWindow?

    private let window: NSWindow

    static func show() {
        let controller = shared ?? SlabAboutWindow()
        shared = controller
        NSApp.activate(ignoringOtherApps: true)
        controller.window.center()
        controller.window.makeKeyAndOrderFront(nil)
    }

    private override init() {
        window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 320, height: 330),
            styleMask: [.titled, .closable, .fullSizeContentView],
            backing: .buffered,
            defer: false
        )
        super.init()
        window.titlebarAppearsTransparent = true
        window.titleVisibility = .hidden
        window.isMovableByWindowBackground = true
        window.isReleasedWhenClosed = false
        window.level = .normal
        window.delegate = self
        buildContent()
    }

    func windowWillClose(_ notification: Notification) {
        SlabAboutWindow.shared = nil
    }

    private func buildContent() {
        let content = NSView()
        window.contentView = content

        let stack = NSStackView()
        stack.orientation = .vertical
        stack.alignment = .centerX
        stack.spacing = 8
        stack.edgeInsets = NSEdgeInsets(top: 24, left: 32, bottom: 24, right: 32)
        stack.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(stack)
        NSLayoutConstraint.activate([
            stack.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            stack.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            stack.topAnchor.constraint(equalTo: content.topAnchor),
            stack.bottomAnchor.constraint(equalTo: content.bottomAnchor),
        ])

        let iconView = NSImageView()
        iconView.image = slabIcon()
        iconView.imageScaling = .scaleProportionallyUpOrDown
        iconView.translatesAutoresizingMaskIntoConstraints = false
        iconView.widthAnchor.constraint(equalToConstant: 104).isActive = true
        iconView.heightAnchor.constraint(equalToConstant: 104).isActive = true
        stack.addArrangedSubview(iconView)
        stack.setCustomSpacing(12, after: iconView)

        let name = NSTextField(labelWithString: "Slab")
        name.font = NSFont.systemFont(ofSize: 22, weight: .bold)
        name.alignment = .center
        stack.addArrangedSubview(name)

        let tagline = NSTextField(labelWithString: "the living workstation behind Aesthetic Computer")
        tagline.font = NSFont.systemFont(ofSize: 12)
        tagline.textColor = .secondaryLabelColor
        tagline.alignment = .center
        stack.addArrangedSubview(tagline)
        stack.setCustomSpacing(16, after: tagline)

        let detail = NSTextField(wrappingLabelWithString:
            "Prompt sessions, fleet awareness, desktop choreography, and local tools — held together in one quiet place.")
        detail.font = NSFont.systemFont(ofSize: 13)
        detail.alignment = .center
        detail.maximumNumberOfLines = 3
        detail.preferredMaxLayoutWidth = 256
        detail.translatesAutoresizingMaskIntoConstraints = false
        detail.widthAnchor.constraint(equalToConstant: 256).isActive = true
        stack.addArrangedSubview(detail)
        stack.setCustomSpacing(16, after: detail)

        let link = NSButton(title: "aesthetic.computer", target: self, action: #selector(openAestheticComputer))
        link.isBordered = false
        link.font = NSFont.systemFont(ofSize: 12, weight: .semibold)
        link.contentTintColor = .controlAccentColor
        stack.addArrangedSubview(link)
        stack.setCustomSpacing(12, after: link)

        let version = Bundle.main.object(forInfoDictionaryKey: "CFBundleShortVersionString") as? String
        let build = Bundle.main.object(forInfoDictionaryKey: "CFBundleVersion") as? String
        let versionText: String
        if let version, let build {
            versionText = "Version \(version) (\(build))"
        } else {
            versionText = "Development build"
        }
        let versionLabel = NSTextField(labelWithString: versionText)
        versionLabel.font = NSFont.systemFont(ofSize: 11)
        versionLabel.textColor = .secondaryLabelColor
        versionLabel.alignment = .center
        stack.addArrangedSubview(versionLabel)

        let copyright = NSTextField(labelWithString: "© 2026 Aesthetic, Inc.")
        copyright.font = NSFont.systemFont(ofSize: 11)
        copyright.textColor = .tertiaryLabelColor
        copyright.alignment = .center
        stack.addArrangedSubview(copyright)

        content.layoutSubtreeIfNeeded()
        window.setContentSize(NSSize(width: 320, height: ceil(content.fittingSize.height)))
    }

    private func slabIcon() -> NSImage {
        if let path = Bundle.main.path(forResource: "AppIcon", ofType: "icns"),
           let icon = NSImage(contentsOfFile: path) {
            return icon
        }
        if let icon = NSApp.applicationIconImage, icon.isValid {
            return icon
        }
        return NSImage(systemSymbolName: "square.stack.3d.up.fill",
                       accessibilityDescription: "Slab") ?? NSImage()
    }

    @objc private func openAestheticComputer() {
        guard let url = URL(string: "https://aesthetic.computer") else { return }
        NSWorkspace.shared.open(url)
    }
}
