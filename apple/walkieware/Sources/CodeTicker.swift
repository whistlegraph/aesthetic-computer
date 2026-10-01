import SwiftUI

struct CodeTicker: View {
    let output: String
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    private var line: String {
        String(output.suffix(1800)).replacingOccurrences(of: "\n", with: "  ").replacingOccurrences(of: "\r", with: " ")
    }
    var body: some View {
        ScrollViewReader { proxy in
            ScrollView(.horizontal, showsIndicators: false) {
                HStack(spacing: 0) {
                    Text(line).font(.custom("Courier-Bold", size: 18, relativeTo: .body))
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
            .accessibilityLabel("Live code").accessibilityIdentifier("code-ticker")
    }
}
