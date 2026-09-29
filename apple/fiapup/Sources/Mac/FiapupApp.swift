// fiapup for the Mac: one window around the game's web shell.
//
// The shell (xbox/fiapup/index.html), the game (fiapup.js) and the two scene
// modules it borrows from oskiewar ship in the bundle, and a fiapup:// scheme
// (Shared/BundleScheme.swift) serves them, because WebKit won't load ES
// modules from file:// URLs.

import AppKit
import WebKit

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
