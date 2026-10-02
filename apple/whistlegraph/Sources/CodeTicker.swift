import SwiftUI

struct CodeTicker: View {
    let output: String
    /// Reasoning scrolls dimmed and plain; code is bold and full strength.
    var thinking = false
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    private var line: String {
        String(output.suffix(1800)).replacingOccurrences(of: "\n", with: "  ").replacingOccurrences(of: "\r", with: " ")
    }
    var body: some View {
        ScrollViewReader { proxy in
            ScrollView(.horizontal, showsIndicators: false) {
                HStack(spacing: 0) {
                    Text(line).font(.custom(thinking ? "Courier" : "Courier-Bold", size: 18, relativeTo: .body))
                        .opacity(thinking ? 0.55 : 1)
                        .lineLimit(1).fixedSize(horizontal: true, vertical: false)
                    Color.clear.frame(width: 1, height: 1).id("latest-code")
                }
            }
            .onAppear { proxy.scrollTo("latest-code", anchor: .trailing) }
            .onChange(of: output) { _, _ in
                withAnimation(reduceMotion ? nil : .linear(duration: 0.12)) {
                    proxy.scrollTo("latest-code", anchor: .trailing)
                }
            }
        }.frame(maxWidth: .infinity).clipped()
            .accessibilityLabel(thinking ? "Model thinking" : "Live code").accessibilityIdentifier("code-ticker")
    }
}
