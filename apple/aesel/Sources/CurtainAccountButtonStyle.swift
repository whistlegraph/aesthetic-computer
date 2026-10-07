import SwiftUI

/// Matches prompt.mjs's curtain login palette and square TextButton outline.
struct CurtainAccountButtonStyle: ButtonStyle {
    var signUp = false
    @Environment(\.colorScheme) private var colorScheme
    @Environment(\.isEnabled) private var isEnabled
    @State private var hovered = false

    func makeBody(configuration: Configuration) -> some View {
        let dark = colorScheme == .dark
        let pressed = configuration.isPressed
        let active = hovered && isEnabled
        let loginFill = pressed ? (dark ? 0x002878 : 0x285AC8)
            : active ? (dark ? 0x0064A0 : 0x3C78DC)
            : (dark ? 0x000040 : 0x000080)
        let loginEdge = pressed ? (dark ? 0x50A0C8 : 0x96C8FF)
            : active ? (dark ? 0x78DCFF : 0xB4E6FF) : 0xFFFFFF
        let fill = signUp
            ? (pressed ? (dark ? 0x144614 : 0x287828)
                : active ? (dark ? 0x286428 : 0x32A032) : (dark ? 0x004000 : 0x008000))
            : loginFill
        let edge = signUp ? (active || pressed ? (dark ? 0x64FF64 : 0x96FF96) : 0xFFFFFF) : loginEdge
        configuration.label
            .padding(.horizontal, 16).padding(.vertical, 10)
            .frame(minHeight: 44)
            .foregroundStyle(pressed ? Color.white : color(edge))
            .background(color(fill))
            .overlay(Rectangle().strokeBorder(color(edge), lineWidth: 2))
            .contentShape(Rectangle())
            .opacity(isEnabled ? 1 : 0.5)
            .onHover { hovered = $0 }
            .modifier(AeselButtonPointer(enabled: isEnabled))
    }

    private func color(_ hex: Int) -> Color {
        Color(red: Double((hex >> 16) & 255) / 255,
              green: Double((hex >> 8) & 255) / 255,
              blue: Double(hex & 255) / 255)
    }
}

