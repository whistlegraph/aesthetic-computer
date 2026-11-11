import AppKit

/// Hand-row score cards. Each bar is three beat cells. The top row is
/// the right hand, one keycap per beat; a key held across beats is one
/// long cap with a tail. The bottom row is the left hand: a tall bass
/// cap on beat one, then two small caps stacked for the two-key chord.
/// Thin lines split the beats, a heavy line ends the bar, and under
/// each beat its group reads as text — right letter, then left letters —
/// so a bar reads as a line, `kd jfa kfa`.
extension ChartRenderer {

    private struct CapStyle { let fill: NSColor; let stroke: NSColor; let ink: NSColor }

    private static func isSharpLetter(_ letter: String) -> Bool {
        for (midi, l) in Piano.notepatLetter where l == letter { return !Piano.isWhite(midi) }
        return false
    }

    /// Natural caps are the face tinted toward the note color; sharps take the
    /// theme's sharp-cap body, the same as on the QWERTY map.
    private static func capStyle(_ letter: String, bg: NSColor) -> CapStyle {
        let raw = noteColorForLetter(letter).usingColorSpace(.sRGB) ?? noteColorForLetter(letter)
        if isSharpLetter(letter) {
            return CapStyle(fill: theme.capSharp, stroke: raw, ink: theme.capSharpLetter)
        }
        let bgS = bg.usingColorSpace(.sRGB) ?? bg
        return CapStyle(fill: bgS.blended(withFraction: theme.keyTint, of: raw) ?? raw,
                        stroke: raw.blended(withFraction: theme.keyStrokeAmount, of: theme.keyStrokeToward) ?? raw,
                        ink: theme.ink)
    }

    /// Collapse "-" holds into (text, startBeat, beats).
    private static func handPills(_ cells: [String]) -> [(String, Int, Int)] {
        var out: [(String, Int, Int)] = []
        for (i, c) in cells.enumerated() {
            if c == "-", !out.isEmpty { out[out.count - 1].2 += 1 } else { out.append((c, i, 1)) }
        }
        return out
    }

    /// One keycap. `letterCenterX` pins the letter to the first beat of a held cap.
    private static func drawCap(in ctx: CGContext, rect: CGRect, letter: String, fontSize: CGFloat,
                                style: CapStyle, letterCenterX: CGFloat? = nil) {
        let r = min(3.5, rect.height / 3)
        let path = CGPath(roundedRect: rect, cornerWidth: r, cornerHeight: r, transform: nil)
        ctx.setFillColor(style.fill.cgColor); ctx.addPath(path); ctx.fillPath()
        ctx.setStrokeColor(style.stroke.cgColor); ctx.setLineWidth(0.9); ctx.addPath(path); ctx.strokePath()
        let t = NSAttributedString(string: letter, attributes: [
            .font: berkeleyMono(size: fontSize, weight: .bold), .foregroundColor: style.ink])
        let ts = t.size()
        let cx = letterCenterX ?? rect.midX
        drawText(in: ctx, text: t, origin: CGPoint(x: cx - ts.width / 2, y: rect.midY - ts.height / 2 + 0.5))
    }

    /// Vector hand seen from above as the player sees it: four fingers up, the
    /// thumb toward the body's midline (left side of the right hand). Unit
    /// space is ~4.4 wide by ~6.2 tall; `scale` is points per unit.
    private static func handPath(thumbOnRight: Bool) -> CGPath {
        let p = CGMutablePath()
        func rr(_ r: CGRect, _ rad: CGFloat, _ t: CGAffineTransform = .identity) {
            var t = t
            p.addPath(CGPath(roundedRect: r, cornerWidth: rad, cornerHeight: rad, transform: &t))
        }
        rr(CGRect(x: -2.2, y: -3.0, width: 4.4, height: 3.6), 1.0)                     // palm
        for (x, h) in [(-1.65, 2.6), (-0.55, 3.2), (0.55, 3.0), (1.65, 2.3)] as [(CGFloat, CGFloat)] {
            rr(CGRect(x: x - 0.5, y: 0.2, width: 1.0, height: h - 0.2), 0.5)           // fingers
        }
        let pivot = CGPoint(x: 2.2, y: -1.6)
        let rot = CGAffineTransform(translationX: pivot.x, y: pivot.y)
            .rotated(by: -35 * .pi / 180).translatedBy(x: -pivot.x, y: -pivot.y)
        rr(CGRect(x: 1.8, y: -2.2, width: 1.2, height: 2.5), 0.6, rot)                 // thumb
        if thumbOnRight { return p }
        var flip = CGAffineTransform(scaleX: -1, y: 1)
        return p.copy(using: &flip) ?? p
    }

    private static func drawHand(in ctx: CGContext, center: CGPoint, left: Bool, scale: CGFloat = 1.7) {
        ctx.saveGState()
        ctx.translateBy(x: center.x, y: center.y - 0.2 * scale)
        ctx.scaleBy(x: scale, y: scale)
        ctx.addPath(handPath(thumbOnRight: left))
        ctx.setFillColor((left ? lhColor : rhColor).cgColor)
        ctx.fillPath(using: .winding)
        ctx.restoreGState()
    }

    static func renderHands(_ chart: Chart, in ctx: CGContext) {
        // Foot: title + subtitle, same stamp as the other score cards.
        let m = handsMargin
        let innerW = pageWidth - 2 * m
        let titleAttr = NSAttributedString(string: chart.title, attributes: [
            .font: bodyFont(size: 11, bold: true), .foregroundColor: dark, .kern: 0.5])
        let subAttr = NSAttributedString(string: chart.subtitle, attributes: [
            .font: bodyFont(size: 7.5), .foregroundColor: gray])
        let subBounds = subAttr.boundingRect(with: CGSize(width: innerW, height: .greatestFiniteMagnitude),
                                             options: [.usesLineFragmentOrigin, .usesFontLeading])
        let subH = ceil(subBounds.height)
        let subY = m - 4
        let titleY = subY + subH + 4
        drawWrappedText(in: ctx, attr: subAttr, rect: CGRect(x: m, y: subY, width: innerW, height: subH + 2))
        let titleSize = titleAttr.size()
        drawText(in: ctx, text: titleAttr, origin: CGPoint(x: (pageWidth - titleSize.width) / 2, y: titleY))
        let footTop = titleY + ceil(titleSize.height) + 10

        // Grid: two bars per row, bars butt against a shared bar line.
        let barsPerRow = 2
        let gutter: CGFloat = 24
        let left = m + gutter
        let gridW = pageWidth - m - left
        let barW = gridW / CGFloat(barsPerRow)
        let beatW = barW / 3
        let capInset: CGFloat = 2.5
        let numH: CGFloat = 7.5
        let rhH: CGFloat = 20
        let gapA: CGFloat = 3
        let lhH: CGFloat = 26
        let smallGap: CGFloat = 2
        let smallH = (lhH - smallGap) / 2
        let gapB: CGFloat = 2
        let lineH: CGFloat = 9.5
        let rowH = numH + rhH + gapA + lhH + gapB + lineH
        let rowGap: CGFloat = 7
        let rows = Int(ceil(Double(chart.score.count) / Double(barsPerRow)))
        let totalH = CGFloat(rows) * rowH + CGFloat(max(0, rows - 1)) * rowGap
        let availTop = pageHeight - m - 6
        let availH = availTop - footTop
        var y = availTop - max(0, (availH - totalH) / 2)

        let numFont = berkeleyMono(size: 6.5, weight: .bold)

        for row in 0..<rows {
            let rowTop = y
            let rowBot = y - rowH
            let rhTop = rowTop - numH
            let lhTop = rhTop - rhH - gapA
            let lineTop = lhTop - lhH - gapB
            let barsInRow = min(barsPerRow, chart.score.count - row * barsPerRow)

            // Lines first so a long cap covers the beat line it crosses.
            for col in 0..<barsInRow {
                let bx = left + CGFloat(col) * barW
                for b in 1..<3 {
                    let x = bx + beatW * CGFloat(b)
                    ctx.setStrokeColor(theme.separator.cgColor); ctx.setLineWidth(0.5)
                    ctx.move(to: CGPoint(x: x, y: rowBot)); ctx.addLine(to: CGPoint(x: x, y: rowTop)); ctx.strokePath()
                }
            }
            for col in 0...barsInRow {
                let x = left + CGFloat(col) * barW
                ctx.setStrokeColor(theme.separatorStrong.cgColor); ctx.setLineWidth(1.0)
                ctx.move(to: CGPoint(x: x, y: rowBot)); ctx.addLine(to: CGPoint(x: x, y: rowTop)); ctx.strokePath()
            }

            for col in 0..<barsInRow {
                let i = row * barsPerRow + col
                let bar = chart.score[i]
                let bx = left + CGFloat(col) * barW
                // Beat numbers.
                for b in 0..<3 {
                    let n = NSAttributedString(string: "\(b + 1)", attributes: [
                        .font: numFont, .foregroundColor: b == 0 ? dark : gray])
                    let sz = n.size()
                    drawText(in: ctx, text: n, origin: CGPoint(x: bx + beatW * CGFloat(b) + (beatW - sz.width) / 2,
                                                               y: rowTop - sz.height + 0.5))
                }
                // Right hand row.
                for (text, start, len) in handPills(bar.right) {
                    let rect = CGRect(x: bx + beatW * CGFloat(start) + capInset, y: rhTop - rhH,
                                      width: beatW * CGFloat(len) - 2 * capInset, height: rhH)
                    let style = capStyle(String(text.prefix(1)), bg: chart.background)
                    drawCap(in: ctx, rect: rect, letter: text, fontSize: 12, style: style,
                            letterCenterX: len > 1 ? bx + beatW * CGFloat(start) + beatW / 2 : nil)
                }
                // Left hand row: a chord "f+a" is two small caps stacked, a single key is one tall cap.
                for (text, start, len) in handPills(bar.left) {
                    let x = bx + beatW * CGFloat(start) + capInset
                    let w = beatW * CGFloat(len) - 2 * capInset
                    let parts = text.split(separator: "+").map(String.init)
                    if parts.count >= 2 {
                        for (k, p) in parts.prefix(2).enumerated() {
                            let top = lhTop - CGFloat(k) * (smallH + smallGap)
                            drawCap(in: ctx, rect: CGRect(x: x, y: top - smallH, width: w, height: smallH),
                                    letter: p, fontSize: 8.5, style: capStyle(p, bg: chart.background))
                        }
                    } else {
                        drawCap(in: ctx, rect: CGRect(x: x, y: lhTop - lhH, width: w, height: lhH),
                                letter: text, fontSize: 12, style: capStyle(String(text.prefix(1)), bg: chart.background),
                                letterCenterX: len > 1 ? bx + beatW * CGFloat(start) + beatW / 2 : nil)
                    }
                }
                // Group line: right letter, then left letters, per beat.
                for b in 0..<3 {
                    let g = beatGroup(bar, b, size: 8)
                    let gs = g.size()
                    drawText(in: ctx, text: g, origin: CGPoint(x: bx + beatW * CGFloat(b) + (beatW - gs.width) / 2,
                                                               y: lineTop - lineH + (lineH - gs.height) / 2))
                }
                // Bar number for the first bar of the row.
                if col == 0 {
                    let n = NSAttributedString(string: "\(chart.firstBarNumber + i)", attributes: [.font: numFont, .foregroundColor: gray])
                    drawText(in: ctx, text: n, origin: CGPoint(x: m + 7, y: rowTop - n.size().height + 0.5))
                }
            }
            // Hands in the gutter.
            drawHand(in: ctx, center: CGPoint(x: m + 13, y: rhTop - rhH / 2), left: false)
            drawHand(in: ctx, center: CGPoint(x: m + 13, y: lhTop - lhH / 2), left: true)

            // Phrase bracket, as on the pill score cards.
            let barsPerPhrase = max(1, chart.phraseLength)
            let rowsPerPhrase = max(1, barsPerPhrase / barsPerRow)
            if row % rowsPerPhrase == 0 {
                let lastRow = min(rows - 1, row + rowsPerPhrase - 1)
                let top = rowTop + 1
                let bot = rowTop - CGFloat(lastRow - row) * (rowH + rowGap) - rowH - 1
                let x = m + 1
                ctx.setStrokeColor(pink.cgColor); ctx.setLineWidth(1.1); ctx.setLineCap(.butt)
                ctx.move(to: CGPoint(x: x + 3, y: top)); ctx.addLine(to: CGPoint(x: x, y: top))
                ctx.addLine(to: CGPoint(x: x, y: bot)); ctx.addLine(to: CGPoint(x: x + 3, y: bot))
                ctx.strokePath()
            }
            y -= rowH + rowGap
        }
    }
}
