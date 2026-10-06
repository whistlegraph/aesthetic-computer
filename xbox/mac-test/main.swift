import Cocoa
import WebKit

final class FightAppDelegate: NSObject, NSApplicationDelegate {
    private var window: NSWindow!

    func applicationDidFinishLaunching(_ notification: Notification) {
        let configuration = WKWebViewConfiguration()
        let webView = WKWebView(frame: .zero, configuration: configuration)
        webView.setValue(false, forKey: "drawsBackground")

        window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 1600, height: 900),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered,
            defer: false
        )
        window.title = "oskiewar"
        window.minSize = NSSize(width: 960, height: 540)
        window.contentView = webView
        window.center()
        window.makeKeyAndOrderFront(nil)
        window.makeFirstResponder(webView)
        webView.load(URLRequest(url: URL(string: "https://oskiewar.com/")!,
                                cachePolicy: .reloadIgnoringLocalCacheData))
        NSApp.activate(ignoringOtherApps: true)
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool {
        true
    }
}

let application = NSApplication.shared
let delegate = FightAppDelegate()
application.setActivationPolicy(.regular)
application.delegate = delegate
application.run()
