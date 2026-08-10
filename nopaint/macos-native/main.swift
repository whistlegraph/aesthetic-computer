import AppKit
import WebKit

private struct Options {
    var url = ProcessInfo.processInfo.environment["NOPAINT_URL"] ?? "https://nopaint.art/"

    init(arguments: [String]) {
        var index = 1
        while index < arguments.count {
            if arguments[index] == "--url", index + 1 < arguments.count {
                index += 1
                url = arguments[index]
            }
            index += 1
        }
    }
}

private final class NoPaintApp: NSObject, NSApplicationDelegate, WKNavigationDelegate, WKUIDelegate {
    private let options = Options(arguments: CommandLine.arguments)
    private var window: NSWindow!
    private var webView: WKWebView!

    func applicationDidFinishLaunching(_ notification: Notification) {
        let configuration = WKWebViewConfiguration()
        configuration.websiteDataStore = .default()
        configuration.preferences.javaScriptCanOpenWindowsAutomatically = false

        webView = WKWebView(frame: .zero, configuration: configuration)
        webView.navigationDelegate = self
        webView.uiDelegate = self
        webView.allowsBackForwardNavigationGestures = false
        webView.allowsMagnification = false
        webView.customUserAgent = "NoPaintMac/1 (Macintosh; nopaint.art)"

        let frame = initialWindowFrame()
        window = NSWindow(
            contentRect: frame,
            styleMask: [.titled, .closable, .resizable, .miniaturizable],
            backing: .buffered,
            defer: false
        )
        window.title = "No Paint"
        window.titlebarAppearsTransparent = false
        window.contentView = webView
        window.setFrameAutosaveName("NoPaintWindow")
        window.collectionBehavior = [.fullScreenPrimary]
        window.makeKeyAndOrderFront(nil)

        installMenu()
        loadFresh(true)
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { true }

    func applicationShouldHandleReopen(_ sender: NSApplication, hasVisibleWindows flag: Bool) -> Bool {
        if !flag { window.makeKeyAndOrderFront(nil) }
        return true
    }

    private func initialWindowFrame() -> NSRect {
        let visible = NSScreen.main?.visibleFrame ?? NSRect(x: 0, y: 0, width: 1440, height: 900)
        let width = min(CGFloat(760), visible.width * 0.82)
        let height = min(CGFloat(900), visible.height * 0.88)
        return NSRect(
            x: visible.midX - width / 2,
            y: visible.midY - height / 2,
            width: width,
            height: height
        )
    }

    private func installMenu() {
        let menu = NSMenu()

        let appItem = NSMenuItem()
        let appMenu = NSMenu()
        appItem.submenu = appMenu
        appMenu.addItem(withTitle: "About No Paint", action: #selector(NSApplication.orderFrontStandardAboutPanel(_:)), keyEquivalent: "")
        appMenu.addItem(.separator())
        appMenu.addItem(withTitle: "Quit No Paint", action: #selector(NSApplication.terminate(_:)), keyEquivalent: "q")
        menu.addItem(appItem)

        let fileItem = NSMenuItem()
        let fileMenu = NSMenu(title: "File")
        fileItem.submenu = fileMenu
        let fresh = fileMenu.addItem(withTitle: "New Painting", action: #selector(newPainting), keyEquivalent: "n")
        fresh.target = self
        menu.addItem(fileItem)

        let viewItem = NSMenuItem()
        let viewMenu = NSMenu(title: "View")
        viewItem.submenu = viewMenu
        let reload = viewMenu.addItem(withTitle: "Reload", action: #selector(reloadPage), keyEquivalent: "r")
        reload.target = self
        viewMenu.addItem(withTitle: "Toggle Full Screen", action: #selector(NSWindow.toggleFullScreen(_:)), keyEquivalent: "f").target = window
        menu.addItem(viewItem)

        NSApp.mainMenu = menu
    }

    @objc private func newPainting() { loadFresh(true) }
    @objc private func reloadPage() { webView.reloadFromOrigin() }

    private func loadFresh(_ fresh: Bool) {
        guard var components = URLComponents(string: options.url) else {
            presentLoadError("Invalid No Paint URL: \(options.url)")
            return
        }
        if fresh {
            var items = components.queryItems ?? []
            items.removeAll { $0.name == "fresh" }
            items.append(URLQueryItem(name: "fresh", value: "1"))
            components.queryItems = items
        }
        guard let url = components.url else { return }
        webView.load(URLRequest(url: url, cachePolicy: .reloadRevalidatingCacheData))
    }

    private func isAppURL(_ url: URL) -> Bool {
        guard let host = url.host?.lowercased() else { return url.isFileURL }
        if host == "nopaint.art" || host == "www.nopaint.art" { return true }
        return host == "localhost" || host == "127.0.0.1" || host == "::1"
    }

    func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction,
                 decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        guard let url = navigationAction.request.url else {
            decisionHandler(.cancel)
            return
        }
        if navigationAction.targetFrame?.isMainFrame == false || !isAppURL(url) {
            NSWorkspace.shared.open(url)
            decisionHandler(.cancel)
            return
        }
        decisionHandler(.allow)
    }

    func webView(_ webView: WKWebView,
                 createWebViewWith configuration: WKWebViewConfiguration,
                 for navigationAction: WKNavigationAction,
                 windowFeatures: WKWindowFeatures) -> WKWebView? {
        if let url = navigationAction.request.url { NSWorkspace.shared.open(url) }
        return nil
    }

    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
        if (error as NSError).code != NSURLErrorCancelled { presentLoadError(error.localizedDescription) }
    }

    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
        if (error as NSError).code != NSURLErrorCancelled { presentLoadError(error.localizedDescription) }
    }

    private func presentLoadError(_ message: String) {
        let alert = NSAlert()
        alert.messageText = "No Paint could not open."
        alert.informativeText = message
        alert.alertStyle = .warning
        alert.addButton(withTitle: "Try Again")
        alert.addButton(withTitle: "Quit")
        alert.beginSheetModal(for: window) { [weak self] response in
            if response == .alertFirstButtonReturn { self?.loadFresh(false) }
            else { NSApp.terminate(nil) }
        }
    }
}

private let application = NSApplication.shared
private let delegate = NoPaintApp()
application.delegate = delegate
application.setActivationPolicy(.regular)
application.activate(ignoringOtherApps: true)
application.run()
