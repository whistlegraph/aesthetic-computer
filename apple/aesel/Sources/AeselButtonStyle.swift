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

/// KidLisp's stop interaction: a small lift on hover, press inward.
struct AeselStopButtonStyle: ButtonStyle {
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @State private var hovered = false
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .contentShape(Circle())
            .scaleEffect(reduceMotion ? 1 : configuration.isPressed ? 0.95 : hovered ? 1.05 : 1)
            .brightness(configuration.isPressed ? -0.1 : 0)
            .animation(.easeOut(duration: 0.15), value: hovered)
            .animation(.easeOut(duration: 0.15), value: configuration.isPressed)
            .onHover { hovered = $0 }
            .modifier(AeselButtonPointer(enabled: true))
    }
}

struct AeselButtonPointer: ViewModifier {
    let enabled: Bool
    @ViewBuilder func body(content: Content) -> some View {
        #if os(macOS)
        content
            .background(AeselCursorRegion(enabled: enabled))
            .onContinuousHover { phase in
                switch phase {
                case .active: if enabled { NSCursor.pointingHand.set() }
                case .ended: NSCursor.arrow.set()
                }
            }
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
        private var pointerTracking: NSTrackingArea?
        override func updateTrackingAreas() {
            super.updateTrackingAreas()
            if let pointerTracking { removeTrackingArea(pointerTracking) }
            let area = NSTrackingArea(rect: .zero,
                options: [.cursorUpdate, .mouseEnteredAndExited, .activeAlways, .inVisibleRect],
                owner: self, userInfo: nil)
            addTrackingArea(area)
            pointerTracking = area
        }
        override func cursorUpdate(with event: NSEvent) {
            if enabled { NSCursor.pointingHand.set() }
        }
        override func mouseEntered(with event: NSEvent) {
            if enabled { NSCursor.pointingHand.set() }
        }
        override func mouseExited(with event: NSEvent) { NSCursor.arrow.set() }
        override func hitTest(_ point: NSPoint) -> NSView? { nil }
        override func resetCursorRects() {
            if enabled { addCursorRect(visibleRect, cursor: .pointingHand) }
        }
        override func viewDidMoveToWindow() { window?.invalidateCursorRects(for: self) }
        override func setFrameSize(_ size: NSSize) {
            guard size != frame.size else { return }
            super.setFrameSize(size)
            window?.invalidateCursorRects(for: self)
        }
    }
    func makeNSView(context: Context) -> CursorView { CursorView() }
    func updateNSView(_ view: CursorView, context: Context) {
        if view.enabled != enabled {
            view.enabled = enabled
            view.window?.invalidateCursorRects(for: view)
        }
    }
}
#endif

/// Color and lightweight interaction feedback, without changing layout.
struct AeselTintedButtonStyle: ButtonStyle {
    let tint: Color
    @Environment(\.isEnabled) private var enabled
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @State private var hovered = false
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .foregroundStyle(.white)
            .background(tint, in: RoundedRectangle(cornerRadius: 8))
            .brightness(configuration.isPressed ? -0.12 : hovered ? 0.06 : 0)
            .scaleEffect(reduceMotion ? 1 : configuration.isPressed ? 0.97 : 1)
            .opacity(enabled ? 1 : 0.45)
            .animation(.easeOut(duration: 0.14), value: configuration.isPressed)
            .animation(.easeOut(duration: 0.14), value: hovered)
            .onHover { hovered = $0 }
            .modifier(AeselButtonPointer(enabled: enabled))
    }
}
