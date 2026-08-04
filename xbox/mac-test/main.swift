import Cocoa
import WebKit

final class FightAppDelegate: NSObject, NSApplicationDelegate {
    private var window: NSWindow!

    func applicationDidFinishLaunching(_ notification: Notification) {
        guard let resources = Bundle.main.resourceURL else {
            fatalError("Missing app resources")
        }
        let live = resources.appendingPathComponent("live", isDirectory: true)
        let html = live.appendingPathComponent("mac-test.html")
        let piece = live.appendingPathComponent("hello.js")
        guard let pieceSource = try? String(contentsOf: piece, encoding: .utf8),
              let encoded = try? JSONSerialization.data(withJSONObject: [pieceSource]),
              let sourceArray = String(data: encoded, encoding: .utf8) else {
            fatalError("Missing fight source")
        }

        let configuration = WKWebViewConfiguration()
        configuration.userContentController.addUserScript(WKUserScript(
            source: "globalThis.__fightPieceSource = \(sourceArray)[0];",
            injectionTime: .atDocumentStart,
            forMainFrameOnly: true
        ))
        let webView = WKWebView(frame: .zero, configuration: configuration)
        webView.setValue(false, forKey: "drawsBackground")

        window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 1600, height: 900),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered,
            defer: false
        )
        window.title = "OSKIEWAR"
        window.minSize = NSSize(width: 960, height: 540)
        window.contentView = webView
        window.center()
        window.makeKeyAndOrderFront(nil)
        window.makeFirstResponder(webView)
        webView.loadFileURL(html, allowingReadAccessTo: live)
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
