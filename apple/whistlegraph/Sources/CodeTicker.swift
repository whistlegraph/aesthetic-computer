import SwiftUI

// What the working row shows while a turn runs: the request, alive letter by
// letter, and the code arriving as real lines rather than one scrolling strip.

/// The request while it is being worked on. Letters pop in one after another,
/// then bob in a slow wave with a glint passing through. Wraps by word, so no
/// glyph is ever cut off.
struct GlyphWave: View {
    let text: String
    let size: CGFloat
    var color: Color = .primary
    var alignment: HorizontalAlignment = .trailing
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @State private var born = Date()
    private struct Word: Identifiable { let id: Int; let start: Int; let glyphs: [Character] }
    private var words: [Word] {
        var result: [Word] = [], index = 0
        for (n, piece) in text.split(separator: " ", omittingEmptySubsequences: true).enumerated() {
            result.append(Word(id: n, start: index, glyphs: Array(piece)))
            index += piece.count + 1
        }
        return result
    }
    var body: some View {
        TimelineView(.animation(paused: reduceMotion)) { context in
            let t = reduceMotion ? 10_000 : context.date.timeIntervalSince(born)
            WordFlow(spacing: size * 0.3, lineSpacing: 0, alignment: alignment) {
                ForEach(words) { word in
                    HStack(spacing: 0) {
                        ForEach(Array(word.glyphs.enumerated()), id: \.offset) { i, glyph in
                            let n = Double(word.start + i)
                            let appear = min(1, max(0, (t - n * 0.035) / 0.3))
                            let glint = pow(max(0, sin(t * 1.6 - n * 0.22)), 12)
                            Text(String(glyph))
                                .font(.custom("ComicRelief-Regular", size: size, relativeTo: .title3))
                                .foregroundStyle(color)
                                .brightness(glint * 0.45)
                                .opacity(appear)
                                .scaleEffect(0.3 + 0.7 * appear, anchor: .bottom)
                                .offset(y: -sin(t * 3.2 - n * 0.5) * size * 0.08 - (1 - appear) * size * 0.4)
                        }
                    }
                }
            }
        }
        .accessibilityElement().accessibilityLabel(text)
    }
}

/// Lays words out left to right and wraps whole words onto new lines.
struct WordFlow: Layout {
    var spacing: CGFloat
    var lineSpacing: CGFloat
    var alignment: HorizontalAlignment = .leading
    private func lines(_ subviews: Subviews, width: CGFloat) -> [[(index: Int, size: CGSize)]] {
        var lines: [[(index: Int, size: CGSize)]] = [[]], used: CGFloat = 0
        for (index, view) in subviews.enumerated() {
            let size = view.sizeThatFits(.unspecified)
            if !lines[lines.count - 1].isEmpty, used + spacing + size.width > width { lines.append([]); used = 0 }
            used += (lines[lines.count - 1].isEmpty ? 0 : spacing) + size.width
            lines[lines.count - 1].append((index, size))
        }
        return lines
    }
    func sizeThatFits(proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) -> CGSize {
        let width = proposal.width ?? .infinity
        let rows = lines(subviews, width: width)
        let height = rows.reduce(0) { $0 + ($1.map(\.size.height).max() ?? 0) } + lineSpacing * CGFloat(max(0, rows.count - 1))
        let widest = rows.map { row in row.reduce(0) { $0 + $1.size.width } + spacing * CGFloat(max(0, row.count - 1)) }.max() ?? 0
        return CGSize(width: width.isFinite ? width : widest, height: height)
    }
    func placeSubviews(in bounds: CGRect, proposal: ProposedViewSize, subviews: Subviews, cache: inout ()) {
        var y = bounds.minY
        for row in lines(subviews, width: bounds.width) {
            let rowWidth = row.reduce(0) { $0 + $1.size.width } + spacing * CGFloat(max(0, row.count - 1))
            let rowHeight = row.map(\.size.height).max() ?? 0
            var x = alignment == .trailing ? bounds.maxX - rowWidth : alignment == .center ? bounds.midX - rowWidth / 2 : bounds.minX
            for item in row {
                subviews[item.index].place(at: CGPoint(x: x, y: y), proposal: ProposedViewSize(item.size))
                x += item.size.width + spacing
            }
            y += rowHeight + lineSpacing
        }
    }
}

/// The code as it arrives: the latest lines, numbered and wrapped, typed out
/// smoothly even when the worker sends it in bursts, with the newest characters
/// fading up behind a cursor.
struct CodeTicker: View {
    let output: String
    /// Reasoning scrolls dimmed and plain; code is colored and full strength.
    var thinking = false
    var visibleLines = 7
    var fontSize: CGFloat = 14
    @Environment(\.accessibilityReduceMotion) private var reduceMotion
    @State private var from = 0
    @State private var to = 0
    @State private var revealStart = Date()
    @State private var revealSeconds = 0.3
    private static let paper = UIColor(red: 0.96, green: 0.94, blue: 0.87, alpha: 1)
    private static let trail = 18
    private func revealed(at date: Date) -> Int {
        if reduceMotion { return output.count }
        let progress = min(1, max(0, date.timeIntervalSince(revealStart) / revealSeconds))
        return min(output.count, from + Int(Double(to - from) * progress))
    }
    private func styled(_ source: String) -> AttributedString {
        let styled = NSMutableAttributedString(string: source, attributes: [.foregroundColor: Self.paper])
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
        // The newest characters fade up, oldest of them strongest.
        let length = styled.length, fade = min(Self.trail, length)
        for k in 0..<fade {
            let location = length - fade + k
            let color = (styled.attribute(.foregroundColor, at: location, effectiveRange: nil) as? UIColor) ?? Self.paper
            styled.addAttribute(.foregroundColor, value: color.withAlphaComponent(1 - CGFloat(k + 1) / CGFloat(fade + 2)), range: NSRange(location: location, length: 1))
        }
        return (try? AttributedString(styled, including: \.uiKit)) ?? AttributedString(source)
    }
    var body: some View {
        TimelineView(.animation(paused: reduceMotion)) { context in
            let count = revealed(at: context.date)
            let shown = String(output.prefix(count))
            let all = shown.split(separator: "\n", omittingEmptySubsequences: false)
            let first = max(0, all.count - visibleLines)
            let blink = Int(context.date.timeIntervalSinceReferenceDate * 2.5) % 2 == 0
            VStack(alignment: .leading, spacing: 4) {
                if !thinking {
                    Text(output.count >= 6000 ? "\(all.count)+ lines" : all.count == 1 ? "1 line" : "\(all.count) lines")
                        .font(.custom("Courier", size: fontSize - 2, relativeTo: .caption)).monospacedDigit()
                        .foregroundStyle(Color(Self.paper).opacity(0.5))
                }
                VStack(alignment: .leading, spacing: 1) {
                    ForEach(first..<all.count, id: \.self) { n in
                        HStack(alignment: .firstTextBaseline, spacing: 8) {
                            if !thinking {
                                Text(output.count >= 6000 ? "" : "\(n + 1)")
                                    .font(.custom("Courier", size: fontSize - 2, relativeTo: .caption)).monospacedDigit()
                                    .foregroundStyle(Color(Self.paper).opacity(0.3)).frame(width: 24, alignment: .trailing)
                            }
                            (Text(styled(String(all[n]))) + Text(n == all.count - 1 && blink ? "▍" : " ").foregroundColor(Color(Self.paper).opacity(0.8)))
                                .font(.custom(thinking ? "Courier" : "Courier-Bold", size: fontSize, relativeTo: .body))
                                .fixedSize(horizontal: false, vertical: true)
                                .frame(maxWidth: .infinity, alignment: .leading)
                        }
                    }
                }
                .frame(height: CGFloat(visibleLines) * (fontSize + 3), alignment: .bottom).clipped()
                .mask(LinearGradient(stops: [.init(color: .clear, location: 0), .init(color: .black, location: 0.3)], startPoint: .top, endPoint: .bottom))
            }
            .opacity(thinking ? 0.6 : 1)
        }
        .padding(10)
        .background(RoundedRectangle(cornerRadius: 10).fill(Color.black.opacity(0.28)))
        .onAppear { from = 0; to = output.count; revealStart = .now; revealSeconds = 0.4 }
        .onChange(of: output) { old, new in
            // Bursts (a remote turn sends its tail every few seconds) type out
            // over about the time until the next one; anything else just shows.
            let now = revealed(at: .now)
            if new.hasPrefix(String(old.prefix(now))), new.count > now {
                from = now; to = new.count; revealStart = .now
                revealSeconds = min(2.4, max(0.2, Double(new.count - now) / 260))
            } else { from = new.count; to = new.count }
        }
        .accessibilityElement().accessibilityLabel(thinking ? "Model thinking" : "Live code").accessibilityValue(output.suffix(400).description)
        .accessibilityIdentifier("code-ticker")
    }
}
