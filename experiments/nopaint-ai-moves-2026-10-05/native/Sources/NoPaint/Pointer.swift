import AppKit
import SwiftUI

struct PointerRegion: NSViewRepresentable {
    let enabled: Bool
    func makeNSView(context: Context) -> CursorView { CursorView() }
    func updateNSView(_ view: CursorView, context: Context) {
        if view.enabled != enabled {
            view.enabled = enabled
            view.window?.invalidateCursorRects(for: view)
        }
    }
    final class CursorView: NSView {
        var enabled = false
        override func hitTest(_ point: NSPoint) -> NSView? { nil }
        override func resetCursorRects() {
            super.resetCursorRects()
            addCursorRect(bounds, cursor: enabled ? .pointingHand : .arrow)
        }
    }
}

struct ActionPointer: ViewModifier {
    @Environment(\.isEnabled) private var enabled
    func body(content: Content) -> some View {
        content.background(PointerRegion(enabled: enabled))
            .onContinuousHover { phase in
                switch phase {
                case .active: (enabled ? NSCursor.pointingHand : NSCursor.arrow).set()
                case .ended: NSCursor.arrow.set()
                }
            }
    }
}
extension View {
    func actionPointer() -> some View { modifier(ActionPointer()) }
}
