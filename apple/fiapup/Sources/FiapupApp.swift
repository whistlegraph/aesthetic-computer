// fiapup for the Mac: one window around the game's web shell.
//
// The shell (xbox/fiapup/index.html), the game (fiapup.js) and the two scene
// modules it borrows from oskiewar ship in the bundle, and a fiapup:// scheme
// serves them, because WebKit won't load ES modules from file:// URLs.

import AppKit
import WebKit

final class BundleScheme: NSObject, WKURLSchemeHandler {
  static let types = ["html": "text/html", "js": "text/javascript", "mjs": "text/javascript"]

  func webView(_ webView: WKWebView, start task: WKURLSchemeTask) {
    guard let url = task.request.url else { return }
    // Resources land flat in the bundle, so live/scene3d.mjs is found by name.
    var name = url.lastPathComponent
    if name.isEmpty || name == "/" { name = "index.html" }
    let ext = (name as NSString).pathExtension
    guard let file = Bundle.main.url(forResource: (name as NSString).deletingPathExtension,
                                     withExtension: ext),
          let data = try? Data(contentsOf: file) else {
      task.didFailWithError(NSError(domain: "fiapup", code: 404,
                                    userInfo: [NSLocalizedDescriptionKey: "no \(name) in the bundle"]))
      return
    }
    let type = (Self.types[ext] ?? "application/octet-stream") + "; charset=utf-8"
    let response = HTTPURLResponse(url: url, statusCode: 200, httpVersion: "HTTP/1.1",
                                   headerFields: ["Content-Type": type, "Cache-Control": "no-store"])!
    task.didReceive(response)
    task.didReceive(data)
    task.didFinish()
  }

  func webView(_ webView: WKWebView, stop task: WKURLSchemeTask) {}
}

final class AppDelegate: NSObject, NSApplicationDelegate {
  var window: NSWindow!
  let scheme = BundleScheme()

  func applicationDidFinishLaunching(_ note: Notification) {
    let config = WKWebViewConfiguration()
    config.setURLSchemeHandler(scheme, forURLScheme: "fiapup")
    config.mediaTypesRequiringUserActionForPlayback = []  // the pup's sounds play at once
    let web = WKWebView(frame: NSRect(x: 0, y: 0, width: 1280, height: 720), configuration: config)

    window = NSWindow(contentRect: web.frame,
                      styleMask: [.titled, .closable, .miniaturizable, .resizable],
                      backing: .buffered, defer: false)
    window.title = "fiapup"
    window.contentView = web
    window.center()
    window.makeKeyAndOrderFront(nil)
    window.makeFirstResponder(web)  // keys go straight to the game
    web.load(URLRequest(url: URL(string: "fiapup://app/index.html")!))
    NSApp.activate(ignoringOtherApps: true)
  }

  func applicationShouldTerminateAfterLastWindowClosed(_ app: NSApplication) -> Bool { true }
}

@main
enum Fiapup {
  static let delegate = AppDelegate()

  static func main() {
    let app = NSApplication.shared
    app.setActivationPolicy(.regular)
    app.delegate = delegate
    let menu = NSMenu(), item = NSMenuItem()
    menu.addItem(item)
    let appMenu = NSMenu()
    appMenu.addItem(withTitle: "Quit fiapup", action: #selector(NSApplication.terminate(_:)),
                    keyEquivalent: "q")
    item.submenu = appMenu
    app.mainMenu = menu
    app.run()
  }
}
