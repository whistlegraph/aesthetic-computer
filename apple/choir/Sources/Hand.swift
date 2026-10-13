import SwiftUI

/// Fía's hand-drawn pieces (Resources/Assets.xcassets/Hand*, Flower*), drawn
/// in whatever ink the surrounding view uses.
struct Hand: View {
    let name: String
    var height: CGFloat = 18
    init(_ name: String, height: CGFloat = 18) { self.name = name; self.height = height }
    var body: some View {
        Image(name).renderingMode(.template).resizable().scaledToFit().frame(height: height)
            .accessibilityHidden(true)
    }
}

/// A flower that turns slowly while something loads.
struct FlowerSpinner: View {
    var size: CGFloat = 28
    @State private var turn = false
    var body: some View {
        Hand("FlowerSmall", height: size)
            .rotationEffect(.degrees(turn ? 360 : 0))
            .animation(.linear(duration: 3).repeatForever(autoreverses: false), value: turn)
            .onAppear { turn = true }
            .foregroundStyle(Theme.ink.opacity(0.5))
    }
}
