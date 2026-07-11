import AppKit
import WebKit

/// The Menu Band "Tips" manual, rendered in-app.
///
/// We ship the same content as an Apple Help book (see `Help/MenuBand.help`),
/// but Help Viewer registration is unreliable for a menu-bar (LSUIElement)
/// app on recent macOS — `showHelp` silently no-ops. So Tips opens its own
/// `WKWebView` window instead: it loads the bundled help HTML directly, always
/// works offline, and matches the Squawk / LLMs windows stylistically. The
/// `.help` bundle stays the single source of the content (and the place to add
/// keymap illustrations).
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

    /// Load the bundled help HTML. Falls back to the hosted support page if the
    /// bundled book can't be found (e.g. an unusual build layout).
    private func load() {
        if let index = Self.bundledIndexURL() {
            // Grant read access to the whole en.lproj dir so style.css + any
            // images resolve.
            webView.loadFileURL(index, allowingReadAccessTo: index.deletingLastPathComponent())
        } else if let hosted = URL(string: "https://menuband.app/support.html") {
            webView.load(URLRequest(url: hosted))
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
