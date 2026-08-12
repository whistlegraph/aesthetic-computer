// ShapedownOverlay.swift — borderless, edge-to-edge stage for shapedown pages.
//
//   shapedown-overlay <page.html> [seconds]
//
// A single borderless window covering the WHOLE screen frame at normal level:
// the menu bar (the band) stays drawn above it, everything else disappears
// behind the visuals — no window chrome, no separate-window look. Escape
// quits; with [seconds] it exits by itself after the performance.

import Cocoa
import WebKit

final class Delegate: NSObject, NSApplicationDelegate {
    let page: URL
    let life: Double?
    var window: NSWindow!

    init(page: URL, life: Double?) { self.page = page; self.life = life }

    func applicationDidFinishLaunching(_ n: Notification) {
        let screen = NSScreen.main!.frame
        window = NSWindow(contentRect: screen, styleMask: [.borderless],
                          backing: .buffered, defer: false)
        // Floating: above every app window (terminals included), below the
        // menu bar — the whole screen becomes the stage and the band stays
        // visible on top. Click-through so it never traps the mouse.
        window.level = .floating
        window.ignoresMouseEvents = true
        window.isOpaque = false
        window.backgroundColor = .clear        // no backdrop — desktop shows through
        window.hasShadow = false
        window.collectionBehavior = [.canJoinAllSpaces, .stationary]

        let web = WKWebView(frame: screen)
        web.setValue(false, forKey: "drawsBackground") // transparent web content
        web.loadFileURL(page, allowingReadAccessTo: page.deletingLastPathComponent())
        window.contentView = web
        window.orderFrontRegardless() // desktop-level + click-through: never key

        NSEvent.addLocalMonitorForEvents(matching: .keyDown) { e in
            if e.keyCode == 53 { NSApp.terminate(nil) } // esc
            return e
        }
        if let life {
            DispatchQueue.main.asyncAfter(deadline: .now() + life) { NSApp.terminate(nil) }
        }
    }
}

let args = CommandLine.arguments
guard args.count >= 2 else { print("usage: shapedown-overlay <page.html> [seconds]"); exit(1) }
let app = NSApplication.shared
app.setActivationPolicy(.accessory) // no Dock icon — it's a stage, not an app
let delegate = Delegate(page: URL(fileURLWithPath: args[1]),
                        life: args.count > 2 ? Double(args[2]) : nil)
app.delegate = delegate
app.run()
