import SwiftUI

/// Legal pad under a classic desktop: yellow ruled paper, square black frames,
/// title strips, monospaced chrome. Light only, like paper.
enum Theme {
    static let paper = Color(red: 1.0, green: 0.957, blue: 0.588)
    static let rule = Color(red: 0.47, green: 0.65, blue: 0.84).opacity(0.55)
    static let margin = Color(red: 0.84, green: 0.27, blue: 0.27).opacity(0.7)
    static let ink = Color(red: 0.11, green: 0.094, blue: 0.086)
    static let panel = Color.white
    static let line: CGFloat = 2
    static func chrome(_ size: CGFloat = 13, _ weight: Font.Weight = .bold) -> Font { .system(size: size, weight: weight, design: .monospaced) }
}

/// The pad itself: blue rules every line and a red double margin.
struct RuledPaper: View {
    var body: some View {
        Canvas { ctx, size in
            ctx.fill(Path(CGRect(origin: .zero, size: size)), with: .color(Theme.paper))
            var y: CGFloat = 30
            while y < size.height { ctx.fill(Path(CGRect(x: 0, y: y, width: size.width, height: 1)), with: .color(Theme.rule)); y += 28 }
            for x in [CGFloat(22), 26] { ctx.fill(Path(CGRect(x: x, y: 0, width: 1.2, height: size.height)), with: .color(Theme.margin)) }
        }
        .ignoresSafeArea()
    }
}

/// A desktop window: a black title strip and a white body in a square frame.
struct DeskWindow<Content: View>: View {
    let title: String
    var detail: String = ""
    @ViewBuilder var content: Content
    var body: some View {
        VStack(alignment: .leading, spacing: 0) {
            HStack {
                Text(title).font(Theme.chrome(13))
                Spacer()
                Text(detail).font(Theme.chrome(11, .regular))
            }
            .foregroundStyle(Theme.paper)
            .padding(.horizontal, 8).padding(.vertical, 4)
            .background(Theme.ink)
            content.padding(10).frame(maxWidth: .infinity, alignment: .leading).background(Theme.panel)
        }
        .overlay(Rectangle().strokeBorder(Theme.ink, lineWidth: Theme.line))
        .background(Rectangle().fill(Theme.ink).offset(x: 3, y: 3)) // the hard drop shadow
    }
}

/// A square desktop button: white face, black frame, inverted when on.
struct DeskButtonStyle: ButtonStyle {
    var on = false
    func makeBody(configuration: Configuration) -> some View {
        let inverted = on != configuration.isPressed
        configuration.label
            .font(Theme.chrome(12))
            .padding(.horizontal, 9).padding(.vertical, 5)
            .foregroundStyle(inverted ? Theme.panel : Theme.ink)
            .background(inverted ? Theme.ink : Theme.panel)
            .overlay(Rectangle().strokeBorder(Theme.ink, lineWidth: 1.5))
    }
}

extension View {
    /// Forms and sheets sit on the same pad.
    func onPad() -> some View {
        scrollContentBackground(.hidden).background(RuledPaper()).tint(Theme.ink)
            .toolbarBackground(Theme.panel, for: .navigationBar).toolbarBackground(.visible, for: .navigationBar)
    }
}
