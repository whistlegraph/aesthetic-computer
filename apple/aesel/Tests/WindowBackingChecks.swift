// swiftc -parse-as-library apple/aesel/Sources/ApplePlatform.swift apple/aesel/Tests/WindowBackingChecks.swift -o /tmp/aesel-window-backing-check
import SwiftUI
import AppKit

@main struct WindowBackingChecks {
    @MainActor static func main() async throws {
        NSApplication.shared.setActivationPolicy(.prohibited)
        let window = NSWindow(contentRect: CGRect(x: 0, y: 0, width: 300, height: 240),
                              styleMask: [.titled, .resizable], backing: .buffered, defer: false)
        for (name, paper) in [("light", NSColor(red: 252/255, green: 247/255, blue: 197/255, alpha: 1)),
                              ("dark", NSColor(red: 70/255, green: 50/255, blue: 100/255, alpha: 1))] {
            // Deliberately leave the SwiftUI layer clear. Newly exposed pixels
            // must already have paper behind them, before the next layout pass.
            window.contentView = NSHostingView(rootView: Color.clear.aeselWindowTitle("Resize check", paper: Color(paper)))
            try await Task.sleep(nanoseconds: 100_000_000)
            for size in [CGSize(width: 220, height: 160), CGSize(width: 971, height: 733),
                         CGSize(width: 321, height: 241), CGSize(width: 840, height: 680)] {
                window.setContentSize(size)
                guard let layer = window.contentView?.layer, let color = layer.backgroundColor,
                      let actual = NSColor(cgColor: color)?.usingColorSpace(.sRGB),
                      let outer = window.backgroundColor.usingColorSpace(.sRGB),
                      let expected = paper.usingColorSpace(.sRGB) else { fatalError("Missing native paper backing") }
                for candidate in [actual, outer] {
                    precondition(abs(candidate.redComponent - expected.redComponent) < 0.001)
                    precondition(abs(candidate.greenComponent - expected.greenComponent) < 0.001)
                    precondition(abs(candidate.blueComponent - expected.blueComponent) < 0.001)
                    precondition(candidate.alphaComponent == 1)
                }
                precondition(layer.bounds.size == window.contentView!.bounds.size)
            }
            print("\(name): window and hosting layer retain opaque paper through immediate resize")
        }
    }
}
