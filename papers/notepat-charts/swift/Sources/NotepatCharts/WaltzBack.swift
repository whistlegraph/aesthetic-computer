import AppKit

/// Backs for the waltz cards. Instead of the shared dot pattern, each back
/// carries the card's own context: the QWERTY map with the keys the card
/// uses (right hand pink, left hand blue), the whole waltz as line score —
/// chord over each bar, `kd jfa kfa` under it — with this card's bars in
/// full ink and the rest faded, and a legend of the chords' left-hand keys.
extension ChartRenderer {

    /// Classic card margin for the waltz deck: content sits well inside the frame.
    static let handsMargin: CGFloat = 26

    /// Hand colors, shared by the intro text, hand glyphs, group lines and backs.
    static var rhColor: NSColor { theme.handRight }
    static var lhColor: NSColor { theme.handLeft }

    /// Group text for one beat: right letter bold pink, left letters blue.
    static func beatGroup(_ bar: ScoreBar, _ b: Int, size: CGFloat, alpha: CGFloat = 1) -> NSAttributedString {
        let rh = bar.right[b] == "-" ? "" : bar.right[b]
        let lh = bar.left[b] == "-" ? "" : bar.left[b].replacingOccurrences(of: "+", with: "")
        let g = NSMutableAttributedString(string: rh, attributes: [
            .font: berkeleyMono(size: size, weight: .bold), .foregroundColor: rhColor.withAlphaComponent(alpha)])
        g.append(NSAttributedString(string: lh, attributes: [
            .font: berkeleyMono(size: size, weight: .regular), .foregroundColor: lhColor.withAlphaComponent(alpha)]))
        return g
    }

    /// The QWERTY map with a hand bar under each lit cap: pink right, blue left.
    static func drawQwertyWithHands(in ctx: CGContext, origin: CGPoint, layout q: QwertyMap.Layout,
                                    highlights h: [Int: Piano.Highlight]) {
        QwertyMap.draw(in: ctx, origin: origin, layout: q, highlights: h, showLetters: true)
        let rows: [([String], CGFloat, CGFloat)] = [
            (QwertyMap.rowQ, 0, 2), (QwertyMap.rowA, q.row1Indent, 1), (QwertyMap.rowZ, q.row2Indent, 0)]
        for (letters, indent, rowIdx) in rows {
            let rowY = origin.y + rowIdx * (q.capH + q.rowGap)
            for (i, letter) in letters.enumerated() {
                guard let midi = QwertyMap.midiByLetter[letter], case .active(let c) = h[midi] ?? .normal else { continue }
                let x = origin.x + indent + CGFloat(i) * (q.capW + q.capGap)
                ctx.setFillColor(c.cgColor)
                let bar = CGRect(x: x + q.capW * 0.2, y: rowY - q.capW * 0.17, width: q.capW * 0.6, height: max(1.8, q.capW * 0.09))
                ctx.addPath(CGPath(roundedRect: bar, cornerWidth: bar.height / 2, cornerHeight: bar.height / 2, transform: nil)); ctx.fillPath()
            }
        }
    }

    static func renderWaltzBack(_ chart: Chart, in ctx: CGContext) {
        let m = handsMargin
        let innerW = pageWidth - 2 * m
        let bars = chart.backBars.isEmpty ? chart.score : chart.backBars
        guard !bars.isEmpty else { renderStandardBack(in: ctx); return }
        let focus: Range<Int>? = chart.score.isEmpty || chart.backBars.isEmpty
            ? nil : (chart.firstBarNumber - 1)..<(chart.firstBarNumber - 1 + chart.score.count)
        let own = chart.score.isEmpty ? bars : chart.score

        // Keys this card uses, by hand.
        var h: [Int: Piano.Highlight] = [:]
        for midi in QwertyMap.midiByLetter.values { h[midi] = .dimmed }
        for bar in own {
            for c in bar.left where c != "-" {
                for k in c.split(separator: "+") { if let midi = QwertyMap.midiByLetter[String(k)] { h[midi] = .active(lhColor) } }
            }
            for c in bar.right where c != "-" {
                if let midi = QwertyMap.midiByLetter[c] { h[midi] = .active(rhColor) }
            }
        }

        // Measure the block, then center it vertically.
        let titleText = chart.title.hasSuffix(".") ? String(chart.title.dropLast()) : chart.title
        let title = NSAttributedString(string: titleText, attributes: [
            .font: bodyFont(size: 11, bold: true), .foregroundColor: dark, .kern: 0.5])
        let titleH = ceil(title.size().height)
        let qLayout = QwertyMap.defaultLayout(capWidth: innerW / 10.9)
        let qH = QwertyMap.totalHeight(layout: qLayout)
        let perLine = 4
        let lines = Int(ceil(Double(bars.count) / Double(perLine)))
        let chordH: CGFloat = 10, textH: CGFloat = 12, lineGap: CGFloat = 9
        let lineH = chordH + textH
        let scoreH = CGFloat(lines) * lineH + CGFloat(lines - 1) * lineGap
        // Chord legend: each chord once, with its left-hand keys.
        var legend: [(String, String)] = []
        for bar in bars where !bar.chord.isEmpty && !legend.contains(where: { $0.0 == bar.chord }) {
            let keys = bar.left.filter { $0 != "-" }
            var seen: [String] = []
            for k in keys where !seen.contains(k) { seen.append(k) }
            legend.append((bar.chord, seen.joined(separator: " ")))
        }
        let legendCols = legend.count <= 3 ? max(1, legend.count) : 2
        let legendRows = Int(ceil(Double(legend.count) / Double(legendCols)))
        let legendRowH: CGFloat = 12
        let legendH = CGFloat(legendRows) * legendRowH
        let gapA: CGFloat = 12, gapB: CGFloat = 16, gapC: CGFloat = 14
        let blockH = titleH + gapA + qH + gapB + scoreH + gapC + legendH
        let availTop = pageHeight - m, availBot = m
        var y = availTop - max(0, (availTop - availBot - blockH) / 2)

        // Title.
        y -= titleH
        drawText(in: ctx, text: title, origin: CGPoint(x: (pageWidth - title.size().width) / 2, y: y))
        y -= gapA

        // Keyboard.
        y -= qH
        let qW = QwertyMap.totalWidth(layout: qLayout)
        drawQwertyWithHands(in: ctx, origin: CGPoint(x: (pageWidth - qW) / 2, y: y), layout: qLayout, highlights: h)
        y -= gapB

        // Line score: chord over each bar, groups under it, bar lines between.
        let cellW = innerW / CGFloat(perLine)
        let numFont = berkeleyMono(size: 5.5, weight: .bold)
        for line in 0..<lines {
            let lineTop = y
            let lineBot = y - lineH
            let count = min(perLine, bars.count - line * perLine)
            for col in 0...count {
                let x = m + CGFloat(col) * cellW
                ctx.setStrokeColor((col == 0 || col == count ? theme.separatorStrong : theme.separator).cgColor)
                ctx.setLineWidth(col == 0 || col == count ? 0.9 : 0.5)
                ctx.move(to: CGPoint(x: x, y: lineBot)); ctx.addLine(to: CGPoint(x: x, y: lineTop)); ctx.strokePath()
            }
            for col in 0..<count {
                let i = line * perLine + col
                let bar = bars[i]
                let alpha: CGFloat = (focus.map { $0.contains(i) } ?? true) ? 1 : 0.38
                let x = m + CGFloat(col) * cellW
                let chord = NSAttributedString(string: bar.chord, attributes: [
                    .font: bodyFont(size: 7.5, bold: true), .foregroundColor: rhColor.withAlphaComponent(alpha)])
                let cs = chord.size()
                drawText(in: ctx, text: chord, origin: CGPoint(x: x + (cellW - cs.width) / 2, y: lineTop - cs.height + 0.5))
                let n = NSAttributedString(string: "\(i + 1)", attributes: [.font: numFont, .foregroundColor: gray.withAlphaComponent(alpha)])
                drawText(in: ctx, text: n, origin: CGPoint(x: x + 2.5, y: lineTop - n.size().height + 0.5))
                let g = NSMutableAttributedString()
                for b in 0..<3 {
                    if b > 0 { g.append(NSAttributedString(string: " ", attributes: [.font: berkeleyMono(size: 7.5)])) }
                    g.append(beatGroup(bar, b, size: 7.5, alpha: alpha))
                }
                let gs = g.size()
                drawText(in: ctx, text: g, origin: CGPoint(x: x + (cellW - gs.width) / 2, y: lineBot + (textH - gs.height) / 2))
            }
            y -= lineH + (line < lines - 1 ? lineGap : 0)
        }
        y -= gapC

        // Legend.
        let colW = innerW / CGFloat(legendCols)
        for (k, (chord, keys)) in legend.enumerated() {
            let row = k / legendCols, col = k % legendCols
            let s = NSMutableAttributedString(string: chord + "  ", attributes: [
                .font: bodyFont(size: 7.5, bold: true), .foregroundColor: rhColor])
            s.append(NSAttributedString(string: keys, attributes: [
                .font: berkeleyMono(size: 7.5, weight: .regular), .foregroundColor: lhColor]))
            let ss = s.size()
            drawText(in: ctx, text: s, origin: CGPoint(x: m + CGFloat(col) * colW + (colW - ss.width) / 2,
                                                       y: y - CGFloat(row + 1) * legendRowH + (legendRowH - ss.height) / 2))
        }
    }
}
