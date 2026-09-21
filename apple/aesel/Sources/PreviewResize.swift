import SwiftUI

struct PreviewBounds: Equatable {
    static let edgeInset: CGFloat = 14
    var width: CGFloat = 178
    var height: CGFloat = 119.33
    var right: CGFloat = Self.edgeInset
    var top: CGFloat = Self.edgeInset

    func fitted(in size: CGSize) -> Self {
        var next = self
        next.right = max(8, min(max(8, size.width - 112), right))
        next.top = max(8, min(max(8, size.height - 120), top))
        next.width = max(96, min(max(96, size.width - next.right - 16), width))
        next.height = max(72, min(max(72, size.height - next.top - 48), height))
        return next
    }
    func resized(edge: String, translation: CGSize, in size: CGSize) -> Self {
        let start = fitted(in: size)
        var next = start
        func clamp(_ value: CGFloat, _ low: CGFloat, _ high: CGFloat) -> CGFloat {
            max(low, min(max(low, high), value))
        }
        if edge.contains("w") {
            next.width = clamp(start.width - translation.width, 96, size.width - start.right - 16)
        } else if edge.contains("e") {
            // Clamp the moving edge, then derive the opposite coordinate.
            // Clamping width and right separately made the left edge drift.
            next.width = clamp(start.width + translation.width, 96, start.width + start.right - 8)
            next.right = start.right + start.width - next.width
        }
        if edge.contains("n") {
            next.top = clamp(start.top + translation.height, 8, start.top + start.height - 72)
            next.height = start.top + start.height - next.top
        } else if edge.contains("s") {
            next.height = clamp(start.height + translation.height, 72, size.height - start.top - 48)
        }
        return next
    }

}

struct PreviewResizeOverlay: View {
    @Binding var bounds: PreviewBounds
    let container: CGSize
    @State private var origin: PreviewBounds?
    private let edges: [(String, Alignment)] = [
        ("n", .top), ("s", .bottom), ("w", .leading), ("e", .trailing),
        ("nw", .topLeading), ("ne", .topTrailing), ("sw", .bottomLeading), ("se", .bottomTrailing)
    ]
    var body: some View {
        GeometryReader { geometry in
            ForEach(edges, id: \.0) { edge, alignment in
                Color.clear
                    .frame(width: edge.count == 2 ? 18 : (edge == "n" || edge == "s" ? max(0, geometry.size.width - 36) : 10),
                           height: edge.count == 2 ? 18 : (edge == "w" || edge == "e" ? max(0, geometry.size.height - 36) : 10))
                    .contentShape(Rectangle())
                    .gesture(DragGesture(minimumDistance: 0, coordinateSpace: .named("aesel-ui"))
                        .onChanged { value in
                            if origin == nil { origin = bounds }
                            bounds = (origin ?? bounds).resized(edge: edge, translation: value.translation, in: container)
                        }.onEnded { _ in origin = nil })
                    .focusable().focusEffectDisabled()
                    .onKeyPress(.leftArrow) { adjust(edge, x: -16); return .handled }
                    .onKeyPress(.rightArrow) { adjust(edge, x: 16); return .handled }
                    .onKeyPress(.upArrow) { adjust(edge, y: -16); return .handled }
                    .onKeyPress(.downArrow) { adjust(edge, y: 16); return .handled }
                    .accessibilityLabel("Resize preview \(edge)")
                    .accessibilityValue("\(Int(bounds.width)) by \(Int(bounds.height))")
                    .accessibilityAdjustableAction { direction in
                        let step: CGFloat = direction == .increment ? 16 : -16
                        adjust(edge, x: edge.contains("w") ? -step : edge.contains("e") ? step : 0,
                               y: edge.contains("n") ? -step : edge.contains("s") ? step : 0)
                    }
                    .modifier(PreviewResizePointer(edge: edge))
                    .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: alignment)
            }
        }
    }
    #if os(macOS)
    fileprivate static func cursor(for edge: String) -> NSCursor {
        if #available(macOS 15, *) {
            let position: NSCursor.FrameResizePosition
            switch edge {
            case "n": position = .top
            case "s": position = .bottom
            case "e": position = .right
            case "w": position = .left
            case "nw": position = .topLeft
            case "ne": position = .topRight
            case "sw": position = .bottomLeft
            default: position = .bottomRight
            }
            return .frameResize(position: position, directions: .all)
        }
        if edge == "n" || edge == "s" { return .resizeUpDown }
        if edge == "e" || edge == "w" { return .resizeLeftRight }
        return edge == "nw" || edge == "se" ? diagonalDown : diagonalUp
    }
    private static let diagonalDown = diagonalCursor("arrow.up.left.and.arrow.down.right")
    private static let diagonalUp = diagonalCursor("arrow.up.right.and.arrow.down.left")
    private static func diagonalCursor(_ name: String) -> NSCursor {
        let image = NSImage(systemSymbolName: name, accessibilityDescription: "Resize diagonally")!
        image.size = NSSize(width: 18, height: 18)
        return NSCursor(image: image, hotSpot: NSPoint(x: 9, y: 9))
    }
    #endif
    private func adjust(_ edge: String, x: CGFloat = 0, y: CGFloat = 0) {
        bounds = bounds.resized(edge: edge, translation: CGSize(width: x, height: y), in: container)
    }
}

private struct PreviewResizePointer: ViewModifier {
    let edge: String
    @ViewBuilder func body(content: Content) -> some View {
        #if os(macOS)
        if #available(macOS 15, *) {
            content.pointerStyle(.frameResize(position: position))
        } else {
            content.onContinuousHover { phase in
                switch phase {
                case .active: PreviewResizeOverlay.cursor(for: edge).set()
                case .ended: NSCursor.arrow.set()
                }
            }
        }
        #else
        content
        #endif
    }
    #if os(macOS)
    @available(macOS 15, *)
    private var position: FrameResizePosition {
        switch edge {
        case "n": return .top
        case "s": return .bottom
        case "e": return .trailing
        case "w": return .leading
        case "nw": return .topLeading
        case "ne": return .topTrailing
        case "sw": return .bottomLeading
        default: return .bottomTrailing
        }
    }
    #endif
}
