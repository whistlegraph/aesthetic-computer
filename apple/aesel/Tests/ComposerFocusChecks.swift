// Runs without showing a window or touching saved notebooks.
// swiftc -parse-as-library Sources/AeselComposer.swift Tests/ComposerFocusChecks.swift -o /tmp/aesel-composer-check
import AppKit
import SwiftUI
typealias AeselFont = NSFont

@Observable final class Draft {
    var text = ""
    var height: CGFloat = 48
    var focused = false
    var ink = Color.purple
}
struct Paper: View {
    @Bindable var draft: Draft
    var body: some View {
        ScrollView {
            AeselComposer(text: $draft.text, height: $draft.height, focused: $draft.focused,
                          color: draft.ink, submit: {})
                .frame(height: max(480, draft.height))
                .padding(14)
        }
    }
}
@main struct Checks {
    @MainActor static func main() async {
        NSApplication.shared.setActivationPolicy(.prohibited)
        let draft = Draft()
        let host = NSHostingView(rootView: Paper(draft: draft))
        let window = NSWindow(contentRect: NSRect(x: 0, y: 0, width: 600, height: 540),
                              styleMask: [.titled], backing: .buffered, defer: false)
        window.contentView = host
        window.layoutIfNeeded()
        try! await Task.sleep(for: .milliseconds(150))
        func editor(_ view: NSView) -> NSTextView? {
            if let text = view as? NSTextView { return text }
            return view.subviews.compactMap { editor($0) }.first
        }
        let text = editor(host)!
        precondition(text.isEditable && text.isSelectable)
        // The first line and blank paper both route to the native editor.
        for y in [8.0, 350.0] {
            let point = text.convert(NSPoint(x: 20, y: y), to: host.superview)
            precondition(host.hitTest(point) === text, "Paper did not hit editor at \(y)")
        }
        precondition(window.makeFirstResponder(text))
        // Reproduce a redraw between AppKit focus and the async binding report.
        draft.ink = .blue
        host.layoutSubtreeIfNeeded()
        try! await Task.sleep(for: .milliseconds(100))
        precondition(window.firstResponder === text && draft.focused, "Redraw stole focus")
        text.insertText("First line", replacementRange: NSRange(location: NSNotFound, length: 0))
        try! await Task.sleep(for: .milliseconds(100))
        precondition(draft.text == "First line", "Typing did not update draft")
        draft.focused = false
        try! await Task.sleep(for: .milliseconds(100))
        precondition(window.firstResponder !== text, "Explicit blur was ignored")
        draft.focused = true
        try! await Task.sleep(for: .milliseconds(100))
        precondition(window.firstResponder === text, "Refocus failed")
        print("PASS: paper hit targets, focus through redraw, typing, blur and refocus")
    }
}
