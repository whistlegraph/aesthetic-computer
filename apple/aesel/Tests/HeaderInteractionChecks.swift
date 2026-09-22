// Hidden window: no visible demo, browser navigation, or saved-session changes.
import AppKit
import SwiftUI

@Observable final class Clicks {
    var title = 0
    var version = 0
}
private struct Header: View {
    let clicks: Clicks
    var body: some View {
        VStack(spacing: 0) {
            HStack(spacing: 12) {
                Color.clear.frame(width: 52)
                Button { clicks.title += 1 } label: {
                    AeselTitle(text: "notebook", size: 16, horizontalInset: 0)
                }.accessibilityLabel("Open piece").buttonStyle(AeselButtonStyle())
                Spacer(minLength: 8).frame(height: 32).background { AeselWindowDragArea() }
                Button { clicks.version += 1 } label: {
                    AeselTitle(text: "v0", size: 13, horizontalInset: 0, hoverAnchor: .trailing)
                }.accessibilityLabel("Open settings").buttonStyle(AeselButtonStyle())
            }.padding(.horizontal, 14).frame(height: 32)
            Color.clear
        }.ignoresSafeArea(.container, edges: .top).aeselWindowTitle("Test", paper: .white)
    }
}
@main struct Checks {
    @MainActor static func main() async {
        NSApplication.shared.setActivationPolicy(.prohibited)
        let clicks = Clicks()
        let host = NSHostingView(rootView: Header(clicks: clicks))
        let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 600, height: 400),
                              styleMask: [.titled, .closable, .fullSizeContentView], backing: .buffered, defer: false)
        window.contentView = host
        try! await Task.sleep(for: .milliseconds(200))
        func views(_ view: NSView) -> [NSView] { [view] + view.subviews.flatMap(views) }
        let cursors = views(host).filter { String(describing: type(of: $0)) == "CursorView" }
        precondition(cursors.count == 2, "Both header buttons need native cursor regions")
        let frames = cursors.map { $0.convert($0.bounds, to: host) }
        for view in cursors {
            precondition(!view.visibleRect.isEmpty)
            let root = host.superview!
            let point = view.convert(NSPoint(x: view.bounds.midX, y: view.bounds.midY), to: root.superview)
            let target = root.hitTest(point)
            precondition(target != nil && !(target is AeselWindowDragArea.DragView), "Window drag stole a button hit")
        }
        let root = host.superview!
        let middle = host.convert(NSPoint(x: 300, y: 16), to: root.superview)
        precondition(root.hitTest(middle) is AeselWindowDragArea.DragView, "Header gap is not draggable")
        try! await Task.sleep(for: .milliseconds(1900))
        precondition(cursors.map { $0.convert($0.bounds, to: host) } == frames, "Letter motion changed button bounds")
        print("PASS: native cursor regions, button hit targets, isolated window dragging, stable animated bounds")
    }
}
