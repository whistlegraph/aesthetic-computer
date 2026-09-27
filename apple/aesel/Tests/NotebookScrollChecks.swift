// xcrun swiftc -parse-as-library Sources/NotebookWebView.swift Tests/NotebookScrollChecks.swift -o /tmp/aesel-scroll-check
import AppKit
import WebKit

private final class Wheel: NSEvent {
    var target: NSWindow?
    var point = NSPoint(x: 80, y: 80)
    var dx: CGFloat = 0
    var dy: CGFloat = -24
    override var window: NSWindow? { target }
    override var locationInWindow: NSPoint { point }
    override var scrollingDeltaX: CGFloat { dx }
    override var scrollingDeltaY: CGFloat { dy }
}
private final class Sheet: NSScrollView {
    var received: [NSEvent] = []
    override func scrollWheel(with event: NSEvent) { received.append(event) }
}
@main struct NotebookScrollChecks {
    @MainActor static func main() {
        _ = NSApplication.shared
        NSApp.setActivationPolicy(.prohibited)
        let rect = NSRect(x: 0, y: 0, width: 500, height: 400)
        let window = NSWindow(contentRect: rect, styleMask: .borderless, backing: .buffered, defer: false)
        let sheet = Sheet(frame: rect)
        let page = NSView(frame: NSRect(x: 0, y: 0, width: 500, height: 1200))
        let web = NotebookWebView(frame: page.bounds, configuration: WKWebViewConfiguration())
        page.addSubview(web)
        sheet.documentView = page
        window.contentView = sheet
        let event = Wheel()
        event.target = window
        precondition(web.routeScrollWheel(event), "Notebook swallowed a vertical wheel")
        precondition(sheet.received.count == 1 && sheet.received[0] === event, "Original event must reach the sheet exactly once")
        precondition(sheet.verticalScrollElasticity == .allowed, "Sheet must rubber-band")
        event.dy = 0 // Preserve zero-delta phase-end events, too.
        precondition(web.routeScrollWheel(event))
        event.dx = 30
        precondition(!web.routeScrollWheel(event), "Horizontal code scrolling belongs to WebKit")
        event.dx = 0; event.dy = -24
        let overlay = NSView(frame: rect)
        sheet.addSubview(overlay)
        precondition(!web.routeScrollWheel(event), "Overlays must keep their wheel input")
        overlay.removeFromSuperview()
        event.target = nil
        precondition(!web.routeScrollWheel(event), "Other windows must keep their events")
        event.target = window; web.isHidden = true
        precondition(!web.routeScrollWheel(event), "Hidden notebook must not intercept")
        print("PASS: vertical and phase-end events route once, original event retained, bounce enabled; horizontal, overlay, other-window and hidden events untouched")
    }
}
