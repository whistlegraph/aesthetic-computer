import AppKit

/// Time-boxed score cards. Every beat is a column of equal width, a bar is
/// a box of three columns with two lanes (left hand above, right hand
/// below, matching the `(left right)` tuple order), and a key that is held
/// across beats is one pill stretched over them. Phrases are bracketed in
/// the margin. Under each bar sits its s-expression, so the picture and
/// the list say the same thing.
extension ChartRenderer {

    private static let lhTint = NSColor(srgbRed: 60/255, green: 120/255, blue: 230/255, alpha: 1)

    /// Collapse "-" holds into (text, startBeat, beats) pills.
    private static func pills(_ cells: [String]) -> [(String, Int, Int)] {
        var out: [(String, Int, Int)] = []
        for (i, c) in cells.enumerated() {
            if c == "-", !out.isEmpty {
                out[out.count - 1].2 += 1
            } else {
                out.append((c, i, 1))
            }
        }
        return out
    }

    /// `(k j k)`, or `k` when one key fills the bar; holds stay as `-`.
    static func sexpr(_ cells: [String]) -> String {
        let p = pills(cells)
        if p.count == 1 && p[0].2 == cells.count { return p[0].0 }
        return "(" + cells.joined(separator: " ") + ")"
    }

    static func renderScore(_ chart: Chart, in ctx: CGContext) {
        // Foot: title + subtitle, same stamp as song cards.
        let innerW = pageWidth - 2 * marginX
        let titleAttr = NSAttributedString(string: chart.title, attributes: [
            .font: bodyFont(size: 11, bold: true), .foregroundColor: dark, .kern: 0.5,
        ])
        let subAttr = NSAttributedString(string: chart.subtitle, attributes: [
            .font: bodyFont(size: 7.5), .foregroundColor: gray,
        ])
        let subBounds = subAttr.boundingRect(
            with: CGSize(width: innerW, height: .greatestFiniteMagnitude),
            options: [.usesLineFragmentOrigin, .usesFontLeading])
        let subH = ceil(subBounds.height)
        let subY = cardInset + 12
        let titleY = subY + subH + 4
        drawWrappedText(in: ctx, attr: subAttr,
                        rect: CGRect(x: marginX, y: subY, width: innerW, height: subH + 2))
        let titleSize = titleAttr.size()
        drawText(in: ctx, text: titleAttr,
                 origin: CGPoint(x: (pageWidth - titleSize.width) / 2, y: titleY))
        let footTop = titleY + ceil(titleSize.height) + 10

        // Grid geometry: two bars per row.
        let barsPerRow = 2
        let gutter: CGFloat = 22          // margin for bar numbers + phrase bracket
        let barGap: CGFloat = 10
        let left = marginX + gutter
        let gridW = pageWidth - marginX - left
        let barW = (gridW - barGap * CGFloat(barsPerRow - 1)) / CGFloat(barsPerRow)
        let beatW = barW / 3
        let laneH: CGFloat = 19
        let laneGap: CGFloat = 3
        let boxH = laneH * 2 + laneGap + 8
        let textH: CGFloat = 11
        let rowGap: CGFloat = 9
        let rows = Int(ceil(Double(chart.score.count) / Double(barsPerRow)))
        let rowH = boxH + 3 + textH
        let totalH = CGFloat(rows) * rowH + CGFloat(max(0, rows - 1)) * rowGap
        let availTop = pageHeight - marginTop - 10
        let availH = availTop - footTop
        // Center the block vertically in what is left.
        var y = availTop - max(0, (availH - totalH) / 2)

        let pillFont = berkeleyMono(size: 10.5, weight: .bold)
        let sexprFont = berkeleyMono(size: 7.2, weight: .regular)
        let labelFont = berkeleyMono(size: 6.5, weight: .bold)

        for row in 0..<rows {
            let rowTop = y
            let rowBot = y - boxH
            for col in 0..<barsPerRow {
                let i = row * barsPerRow + col
                guard i < chart.score.count else { break }
                let bar = chart.score[i]
                let bx = left + CGFloat(col) * (barW + barGap)
                // Bar box.
                let box = CGRect(x: bx, y: rowBot, width: barW, height: boxH)
                let boxPath = CGPath(roundedRect: box, cornerWidth: 6, cornerHeight: 6, transform: nil)
                ctx.setFillColor(NSColor.white.withAlphaComponent(0.55).cgColor)
                ctx.addPath(boxPath); ctx.fillPath()
                ctx.setStrokeColor(dark.withAlphaComponent(0.55).cgColor)
                ctx.setLineWidth(0.9)
                ctx.addPath(boxPath); ctx.strokePath()
                // Beat columns (light ticks) + beat numbers along the top edge.
                for b in 1..<3 {
                    let x = bx + beatW * CGFloat(b)
                    ctx.setStrokeColor(dark.withAlphaComponent(0.14).cgColor)
                    ctx.setLineWidth(0.6)
                    ctx.move(to: CGPoint(x: x, y: rowBot + 2)); ctx.addLine(to: CGPoint(x: x, y: rowTop - 2))
                    ctx.strokePath()
                }
                for b in 0..<3 {
                    let n = NSAttributedString(string: "\(b + 1)", attributes: [
                        .font: labelFont, .foregroundColor: b == 0 ? dark : gray])
                    let sz = n.size()
                    drawText(in: ctx, text: n,
                             origin: CGPoint(x: bx + beatW * CGFloat(b) + (beatW - sz.width) / 2,
                                             y: rowTop - sz.height - 0.5))
                }
                // Lanes: left hand on top, right hand below.
                let lanes: [(cells: [String], isLeft: Bool)] = [(bar.left, true), (bar.right, false)]
                for (li, lane) in lanes.enumerated() {
                    let laneTop = rowTop - 8 - CGFloat(li) * (laneH + laneGap)
                    let laneBot = laneTop - laneH
                    for (text, start, len) in pills(lane.cells) {
                        let px = bx + beatW * CGFloat(start) + 2
                        let pw = beatW * CGFloat(len) - 4
                        let pill = CGRect(x: px, y: laneBot + 1.5, width: pw, height: laneH - 3)
                        let firstKey = String(text.prefix(1))
                        let raw = noteColorForLetter(firstKey)
                        let color = raw.usingColorSpace(.sRGB) ?? raw
                        let fill = lane.isLeft ? color.blended(withFraction: 0.35, of: lhTint) ?? color : color
                        let pillPath = CGPath(roundedRect: pill, cornerWidth: 4, cornerHeight: 4, transform: nil)
                        ctx.setFillColor((lane.isLeft ? fill.withAlphaComponent(0.42) : fill.withAlphaComponent(0.92)).cgColor)
                        ctx.addPath(pillPath); ctx.fillPath()
                        ctx.setStrokeColor(fill.blended(withFraction: 0.35, of: dark)?.cgColor ?? dark.cgColor)
                        ctx.setLineWidth(0.8)
                        ctx.addPath(pillPath); ctx.strokePath()
                        let lum = (0.299 * fill.redComponent + 0.587 * fill.greenComponent + 0.114 * fill.blueComponent)
                        let ink: NSColor = lane.isLeft ? dark : (lum > 0.62 ? NSColor(srgbRed: 0.10, green: 0.08, blue: 0.10, alpha: 1) : .white)
                        let t = NSAttributedString(string: text, attributes: [.font: pillFont, .foregroundColor: ink])
                        let ts = t.size()
                        // Key sits at the left of a long pill so the eye reads the onset.
                        let tx = len > 1 ? pill.minX + 5 : pill.midX - ts.width / 2
                        drawText(in: ctx, text: t, origin: CGPoint(x: tx, y: pill.midY - ts.height / 2 + 0.5))
                        if len > 1 {
                            // Hold tail: a thin line through the rest of the pill.
                            ctx.setStrokeColor(ink.withAlphaComponent(0.45).cgColor)
                            ctx.setLineWidth(1.2)
                            ctx.move(to: CGPoint(x: tx + ts.width + 4, y: pill.midY))
                            ctx.addLine(to: CGPoint(x: pill.maxX - 5, y: pill.midY))
                            ctx.strokePath()
                        }
                    }
                }
                // s-expression under the bar: (left right).
                let expr = "(" + sexpr(bar.left) + " " + sexpr(bar.right) + ")"
                let e = NSAttributedString(string: expr, attributes: [.font: sexprFont, .foregroundColor: gray])
                let es = e.size()
                drawText(in: ctx, text: e, origin: CGPoint(x: bx + (barW - es.width) / 2, y: rowBot - 2 - es.height))
                // Bar number in the gutter for the first bar of the row.
                if col == 0 {
                    let n = NSAttributedString(string: "\(chart.firstBarNumber + i)", attributes: [.font: labelFont, .foregroundColor: gray])
                    drawText(in: ctx, text: n, origin: CGPoint(x: marginX + 8, y: rowTop - 8 - n.size().height))
                }
            }
            // Phrase bracket: one bracket per `phraseLength` bars, in the gutter.
            let barsPerPhrase = max(1, chart.phraseLength)
            let rowsPerPhrase = max(1, barsPerPhrase / barsPerRow)
            if row % rowsPerPhrase == 0 {
                let lastRow = min(rows - 1, row + rowsPerPhrase - 1)
                let top = rowTop + 1
                let bot = rowTop - CGFloat(lastRow - row) * (rowH + rowGap) - boxH - 1
                let x = marginX + 2
                ctx.setStrokeColor(pink.cgColor)
                ctx.setLineWidth(1.1)
                ctx.move(to: CGPoint(x: x + 3, y: top)); ctx.addLine(to: CGPoint(x: x, y: top))
                ctx.addLine(to: CGPoint(x: x, y: bot)); ctx.addLine(to: CGPoint(x: x + 3, y: bot))
                ctx.strokePath()
            }
            y -= rowH + rowGap
        }
    }
}
