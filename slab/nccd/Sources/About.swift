// About.swift — the About NCCd window: the wordmark, large, on the donut's
// colour; what the app is, in one line; its version; what it stands on; and
// where it comes from.

import AppKit

final class AboutWindow: NSObject, NSWindowDelegate {
    static let shared = AboutWindow()
    private var window: NSWindow?

    func show() {
        if let window { window.makeKeyAndOrderFront(nil); return }
        let w = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 380, height: 372), styleMask: [.titled, .closable], backing: .buffered, defer: false)
        w.title = "About NCCd"
        w.isReleasedWhenClosed = false
        w.delegate = self
        w.contentView = build()
        w.center()
        window = w
        w.makeKeyAndOrderFront(nil)
    }
    func windowWillClose(_ n: Notification) { window = nil }

    private func build() -> NSView {
        let root = Fill(Theme.mint)
        root.grain = true
        // the wordmark on a plane of the icing's colour, a seam under it
        let head = Fill(Theme.wash(Donut.icing))
        head.grain = true
        let mark = NCCLogo(scale: 8)
        head.addSubview(mark)
        let seam = Hairline()
        seam.translatesAutoresizingMaskIntoConstraints = false

        let info = Bundle.main.infoDictionary ?? [:]
        let version = (info["CFBundleShortVersionString"] as? String) ?? "?"
        let build = (info["CFBundleVersion"] as? String) ?? "?"
        let what = label("The NELA Computer Club's chats, circles, wiki and code in one window. The d is for donut.", Theme.mono(12))
        what.maximumNumberOfLines = 3
        what.lineBreakMode = .byWordWrapping
        what.alignment = .center
        what.preferredMaxLayoutWidth = 320
        what.widthAnchor.constraint(equalToConstant: 320).isActive = true
        let ver = label("version \(version) (\(build))", Theme.mono(12, bold: true))
        let on = label("Signal by signal-cli · wiki by Outline · code by Forgejo · login by authentik", Theme.mono(10), Theme.faint)
        on.maximumNumberOfLines = 2
        on.lineBreakMode = .byWordWrapping
        on.alignment = .center
        on.preferredMaxLayoutWidth = 320
        on.widthAnchor.constraint(equalToConstant: 320).isActive = true
        let fine = label("Not affiliated with Signal. Signal is a trademark of Signal Technology Foundation.", Theme.mono(9), Theme.faint)
        fine.maximumNumberOfLines = 2
        fine.lineBreakMode = .byWordWrapping
        fine.alignment = .center
        fine.preferredMaxLayoutWidth = 320
        fine.widthAnchor.constraint(equalToConstant: 320).isActive = true

        func link(_ words: String, _ address: String) -> NSView {
            let b = LinkLabel(words, URL(string: address)!)
            return b
        }
        let links = NSStackView(views: [link("nela.computer", "https://nela.computer"), link("download page", "https://aesthetic.computer/nccd/"),
                                        link("source", "https://github.com/whistlegraph/aesthetic-computer/tree/main/slab/nccd")])
        links.spacing = 14

        let col = NSStackView(views: [what, ver, links, on, fine])
        col.orientation = .vertical
        col.alignment = .centerX
        col.spacing = 11
        for v in [head, seam, col] as [NSView] { v.translatesAutoresizingMaskIntoConstraints = false; root.addSubview(v) }
        NSLayoutConstraint.activate([
            head.leadingAnchor.constraint(equalTo: root.leadingAnchor), head.trailingAnchor.constraint(equalTo: root.trailingAnchor),
            head.topAnchor.constraint(equalTo: root.topAnchor), head.heightAnchor.constraint(equalToConstant: 132),
            mark.centerXAnchor.constraint(equalTo: head.centerXAnchor), mark.centerYAnchor.constraint(equalTo: head.centerYAnchor),
            seam.leadingAnchor.constraint(equalTo: root.leadingAnchor), seam.trailingAnchor.constraint(equalTo: root.trailingAnchor),
            seam.centerYAnchor.constraint(equalTo: head.bottomAnchor), seam.heightAnchor.constraint(equalToConstant: Hairline.tall),
            col.centerXAnchor.constraint(equalTo: root.centerXAnchor), col.topAnchor.constraint(equalTo: head.bottomAnchor, constant: 20),
        ])
        return root
    }
    var view: NSView? { window?.contentView }
}

// Words that open an address, underlined, lighter under the pointer.
final class LinkLabel: NSTextField {
    private let address: URL
    init(_ words: String, _ address: URL) {
        self.address = address
        super.init(frame: .zero)
        isEditable = false
        isBordered = false
        drawsBackground = false
        attributedStringValue = NSAttributedString(string: words, attributes: [
            .font: Theme.mono(11, bold: true), .foregroundColor: Theme.link, .underlineStyle: NSUnderlineStyle.single.rawValue])
    }
    required init?(coder: NSCoder) { fatalError() }
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        trackingAreas.forEach(removeTrackingArea)
        addTrackingArea(NSTrackingArea(rect: bounds, options: [.mouseEnteredAndExited, .activeInActiveApp, .inVisibleRect], owner: self))
    }
    override func mouseEntered(with event: NSEvent) { alphaValue = 0.6 }
    override func mouseExited(with event: NSEvent) { alphaValue = 1 }
    override func mouseDown(with event: NSEvent) { NSWorkspace.shared.open(address) }
    override func resetCursorRects() { addCursorRect(bounds, cursor: .pointingHand) }
}
