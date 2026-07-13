import AppKit
import WebKit

/// The Menu Band "Tips" manual, rendered in-app.
///
/// This is the FALLBACK for `AppDelegate.openTips()`: it prefers the real macOS
/// Help Viewer, and drops to this window only when Help Viewer doesn't come up
/// (it can silently no-op for a menu-bar/LSUIElement app). Either way the
/// content is the bundled Apple Help book (`Help/MenuBand.help`): this window
/// loads that book's HTML directly, so it always works offline and the `.help`
/// bundle stays the single source of content (and the place to add keymap
/// illustrations). It never loads the hosted site.
final class TipsWindowController: NSWindowController, NSWindowDelegate {

    private static var active: TipsWindowController?
    private let webView = WKWebView()

    /// Open (or re-focus) the single Tips window.
    @discardableResult
    static func show() -> TipsWindowController {
        if let live = active {
            live.window?.makeKeyAndOrderFront(nil)
            NSApp.activate(ignoringOtherApps: true)
            return live
        }
        let ctrl = TipsWindowController()
        ctrl.present()
        return ctrl
    }

    init() {
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 680, height: 640),
            styleMask: [.titled, .closable, .resizable, .fullSizeContentView],
            backing: .buffered, defer: false)
        window.title = "Menu Band Tips"
        window.titlebarAppearsTransparent = true
        window.isMovableByWindowBackground = true
        super.init(window: window)
        window.delegate = self

        webView.setValue(false, forKey: "drawsBackground")   // let the page bg show
        webView.translatesAutoresizingMaskIntoConstraints = false
        if let content = window.contentView {
            content.addSubview(webView)
            NSLayoutConstraint.activate([
                webView.topAnchor.constraint(equalTo: content.topAnchor),
                webView.leadingAnchor.constraint(equalTo: content.leadingAnchor),
                webView.trailingAnchor.constraint(equalTo: content.trailingAnchor),
                webView.bottomAnchor.constraint(equalTo: content.bottomAnchor),
            ])
        }
        load()
    }

    @available(*, unavailable)
    required init?(coder: NSCoder) { nil }

    func present() {
        guard let window = window else { return }
        TipsWindowController.active = self
        window.center()
        NSApp.activate(ignoringOtherApps: true)
        window.makeKeyAndOrderFront(nil)
    }

    func windowWillClose(_ notification: Notification) {
        if TipsWindowController.active === self { TipsWindowController.active = nil }
    }

    // MARK: - Content

    /// Load the bundled help HTML. If the book somehow isn't in the bundle we
    /// show a short inline note (with an email link) rather than the hosted
    /// site — the whole point of Tips is that it's the app's own manual, not a
    /// bounce out to menuband.app.
    private func load() {
        if let index = Self.bundledIndexURL() {
            // Grant read access to the whole en.lproj dir so style.css + any
            // images resolve.
            webView.loadFileURL(index, allowingReadAccessTo: index.deletingLastPathComponent())
        } else {
            let fallback = """
            <html><body style="font-family:-apple-system,sans-serif;padding:2.5em; \
            color:#333;line-height:1.5">
            <h2>Menu Band Tips</h2>
            <p>The built-in manual couldn't be found in this build. \
            Email <a href="mailto:mail@aesthetic.computer">mail@aesthetic.computer</a> \
            and we'll help.</p></body></html>
            """
            webView.loadHTMLString(fallback, baseURL: nil)
        }
    }

    /// `…/Menu Band.app/Contents/Resources/MenuBand.help/Contents/Resources/en.lproj/index.html`
    static func bundledIndexURL() -> URL? {
        guard let res = Bundle.main.resourceURL else { return nil }
        let url = res
            .appendingPathComponent("MenuBand.help/Contents/Resources/en.lproj/index.html")
        return FileManager.default.fileExists(atPath: url.path) ? url : nil
    }
}
