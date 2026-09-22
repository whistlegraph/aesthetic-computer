import SwiftUI

/// Plain artwork controls still communicate that they are buttons.
struct AeselButtonStyle: ButtonStyle {
    @Environment(\.isEnabled) private var enabled
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .contentShape(Rectangle())
            .opacity(enabled ? (configuration.isPressed ? 0.72 : 1) : 0.4)
            .modifier(AeselButtonPointer(enabled: enabled))
    }
}

struct AeselButtonPointer: ViewModifier {
    let enabled: Bool
    @ViewBuilder func body(content: Content) -> some View {
        #if os(macOS)
        content.background(AeselCursorRegion(enabled: enabled))
        #else
        content.hoverEffect(.highlight, isEnabled: enabled)
        #endif
    }
}

#if os(macOS)
private struct AeselCursorRegion: NSViewRepresentable {
    let enabled: Bool
    final class CursorView: NSView {
        var enabled = true
        override func hitTest(_ point: NSPoint) -> NSView? { nil }
        override func resetCursorRects() {
            if enabled { addCursorRect(visibleRect, cursor: .pointingHand) }
        }
        override func viewDidMoveToWindow() { window?.invalidateCursorRects(for: self) }
        override func setFrameSize(_ size: NSSize) {
            super.setFrameSize(size)
            window?.invalidateCursorRects(for: self)
        }
    }
    func makeNSView(context: Context) -> CursorView { CursorView() }
    func updateNSView(_ view: CursorView, context: Context) {
        view.enabled = enabled
        view.window?.invalidateCursorRects(for: view)
    }
}
#endif
