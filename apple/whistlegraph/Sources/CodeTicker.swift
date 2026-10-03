import SwiftUI

struct CodeTicker: View {
    let output: String
    /// Reasoning scrolls dimmed and plain; code is bold and full strength.
    var thinking = false
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    private var line: AttributedString {
        // Lex before flattening newlines so // comments end on their real line.
        let source = String(output.suffix(6000))
        let styled = NSMutableAttributedString(string: source, attributes: [.foregroundColor: UIColor(red: 0.96, green: 0.94, blue: 0.87, alpha: 1)])
        if !thinking {
            for token in CodeSyntax.tokens(source) {
                let color: UIColor
                switch token.kind {
                case "keyword": color = UIColor(red: 1, green: 0.56, blue: 0.83, alpha: 1)
                case "string": color = UIColor(red: 0.67, green: 0.9, blue: 0.57, alpha: 1)
                case "number": color = UIColor(red: 1, green: 0.8, blue: 0.47, alpha: 1)
                case "call": color = UIColor(red: 0.48, green: 0.88, blue: 0.94, alpha: 1)
                default: color = UIColor(red: 0.7, green: 0.65, blue: 0.76, alpha: 1)
                }
                styled.addAttribute(.foregroundColor, value: color, range: token.range)
            }
        }
        for index in stride(from: styled.length - 1, through: 0, by: -1) {
            if [10, 13].contains((styled.string as NSString).character(at: index)) {
                styled.replaceCharacters(in: NSRange(location: index, length: 1), with: " ")
            }
        }
        return (try? AttributedString(styled, including: \.uiKit)) ?? AttributedString(source)
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
