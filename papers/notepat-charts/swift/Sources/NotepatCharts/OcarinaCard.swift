import AppKit

/// Ocarina song cards for Menu Band's ocarina instrument. The five keys are
/// the Switch 2 ocarina buttons spelled on QWERTY — L R A Y X play D F A B D —
/// so a card's letters are the same letters you press on either. Each card is
/// one song: an ocarina strip with the song's holes lit, then the song as rows
/// of big keycaps, the note name under each cap, and the whole line as text.
/// `NotepatCharts out.pdf --ocarina` renders the deck.

/// The five holes, low to high: key letter, note name, display MIDI.
let ocarinaHoles: [(key: String, note: String, midi: Int)] = [
    ("L", "D", 74), ("R", "F", 77), ("A", "A", 81), ("Y", "B", 83), ("X", "D", 86),
]

/// The twelve songs, as the key letters played. Melodies are the short
/// phrases the game listens for; the game checks the order of the notes,
/// not their rhythm, so the cards print them as even beats.
let ocarinaSongs: [(title: String, keys: String, rows: [Int])] = [
    ("Zelda's Lullaby",    "Y X A Y X A",     [3, 3]),
    ("Epona's Song",       "X Y A X Y A",     [3, 3]),
    ("Saria's Song",       "R A Y R A Y",     [3, 3]),
    ("Sun's Song",         "A R X A R X",     [3, 3]),
    ("Song of Time",       "A L R A L R",     [3, 3]),
    ("Song of Storms",     "L R X L R X",     [3, 3]),
    ("Minuet of Forest",   "L X Y A Y A",     [2, 4]),
    ("Bolero of Fire",     "R L R L A R A R", [4, 4]),
    ("Serenade of Water",  "L R A A Y",       [3, 2]),
    ("Nocturne of Shadow", "Y A A L Y A R",   [3, 4]),
    ("Requiem of Spirit",  "L R L A R L",     [3, 3]),
    ("Prelude of Light",   "X A X A Y X",     [4, 2]),
]

func ocarinaHole(_ key: String) -> (key: String, note: String, midi: Int)? {
    ocarinaHoles.first { $0.key == key }
}

func ocarinaCharts() -> [Chart] {
    var out: [Chart] = []
    out.append(Chart(
        title: "Ocarina Songs",
        subtitle: "for Menu Band's ocarina · five keys, L R A Y X · by @jeffrey with Claude · aesthetic.computer · november 2026",
        highlights: [:], body: [], footer: "november 2026",
        backStyle: .pattern, ocarina: ["L", "R", "A", "Y", "X"], ocarinaCover: true))
    for song in ocarinaSongs {
        let keys = song.keys.split(separator: " ").map(String.init)
        let notes = keys.compactMap { ocarinaHole($0)?.note }.joined(separator: " ")
        out.append(Chart(
            title: song.title + ".",
            subtitle: "\(keys.count) notes · \(notes)",
            highlights: [:], body: [], footer: "",
            backStyle: .pattern, ocarina: keys, ocarinaRows: song.rows))
    }
    return out
}

extension ChartRenderer {

    /// Cap colors from the note palette, blended into the card face the way
    /// the hand-row caps are. High D darkens so the two Ds part.
    private static func ocarinaCapColors(_ midi: Int, bg: NSColor) -> (fill: NSColor, stroke: NSColor) {
        let raw = Piano.noteColor(midi: midi).usingColorSpace(.sRGB) ?? Piano.noteColor(midi: midi)
        let note = midi >= 86 ? (raw.blended(withFraction: 0.35, of: .black) ?? raw) : raw
        let bgS = bg.usingColorSpace(.sRGB) ?? bg
        return (bgS.blended(withFraction: theme.keyTint, of: note) ?? note,
                note.blended(withFraction: theme.keyStrokeAmount, of: theme.keyStrokeToward) ?? note)
    }

    private static func drawOcarinaCap(in ctx: CGContext, rect: CGRect, key: String, bg: NSColor,
                                       fontSize: CGFloat) {
        guard let hole = ocarinaHole(key) else { return }
        let c = ocarinaCapColors(hole.midi, bg: bg)
        let r = min(8, rect.height / 4)
        let path = CGPath(roundedRect: rect, cornerWidth: r, cornerHeight: r, transform: nil)
        ctx.setFillColor(c.fill.cgColor); ctx.addPath(path); ctx.fillPath()
        ctx.setStrokeColor(c.stroke.cgColor); ctx.setLineWidth(1.4); ctx.addPath(path); ctx.strokePath()
        let t = NSAttributedString(string: key, attributes: [
            .font: berkeleyMono(size: fontSize, weight: .bold), .foregroundColor: theme.ink])
        let ts = t.size()
        drawText(in: ctx, text: t, origin: CGPoint(x: rect.midX - ts.width / 2,
                                                    y: rect.midY - ts.height / 2 + 1))
    }

    /// The body outline from Menu Band's menu bar: four curves with matched
    /// tangents — round end, belly, a tail that narrows and tips up.
    private static func ocarinaOutline(_ r: CGRect) -> CGPath {
        let w = r.width, h = r.height
        let tailY = r.midY + h * 0.07, tailR = h * 0.4
        let topX = r.minX + w * 0.3, botX = r.minX + w * 0.36
        let p = CGMutablePath()
        p.move(to: CGPoint(x: r.minX, y: r.midY))
        p.addCurve(to: CGPoint(x: topX, y: r.maxY),
                   control1: CGPoint(x: r.minX, y: r.maxY - h * 0.2),
                   control2: CGPoint(x: r.minX + w * 0.08, y: r.maxY))
        p.addCurve(to: CGPoint(x: r.maxX, y: tailY),
                   control1: CGPoint(x: topX + w * 0.4, y: r.maxY),
                   control2: CGPoint(x: r.maxX + w * 0.015, y: tailY + tailR * 1.2))
        p.addCurve(to: CGPoint(x: botX, y: r.minY),
                   control1: CGPoint(x: r.maxX + w * 0.015, y: tailY - tailR * 1.35),
                   control2: CGPoint(x: botX + w * 0.38, y: r.minY))
        p.addCurve(to: CGPoint(x: r.minX, y: r.midY),
                   control1: CGPoint(x: r.minX + w * 0.1, y: r.minY),
                   control2: CGPoint(x: r.minX, y: r.minY + h * 0.2))
        p.closeSubpath()
        return p
    }

    /// The instrument as drawn in Menu Band's menu bar: a widened blue body,
    /// mouthpiece left, five holes. Holes in `lit` fill with their note color.
    private static func drawOcarinaStrip(in ctx: CGContext, rect: CGRect, lit: Set<String>) {
        let w = rect.width, h = rect.height
        let mouthW = h * 0.75, mouthH = h * 0.42
        let mouth = CGRect(x: rect.minX, y: rect.midY - mouthH / 2 + h * 0.08,
                           width: mouthW, height: mouthH)
        let body = CGRect(x: rect.minX + mouthW * 0.7, y: rect.minY,
                          width: w - mouthW * 0.7, height: h)
        let shape = ocarinaOutline(body)
        let top = NSColor(srgbRed: 78/255, green: 120/255, blue: 214/255, alpha: 1)
        let bottom = NSColor(srgbRed: 36/255, green: 62/255, blue: 148/255, alpha: 1)
        let rim = NSColor(srgbRed: 14/255, green: 24/255, blue: 62/255, alpha: 0.85)
        let grad = CGGradient(colorsSpace: CGColorSpace(name: CGColorSpace.sRGB),
                              colors: [top.cgColor, bottom.cgColor] as CFArray, locations: [0, 1])
        for part in [CGPath(roundedRect: mouth, cornerWidth: mouthH * 0.3,
                            cornerHeight: mouthH * 0.3, transform: nil), shape] {
            ctx.saveGState()
            ctx.addPath(part); ctx.clip()
            if let grad {
                ctx.drawLinearGradient(grad, start: CGPoint(x: 0, y: rect.maxY),
                                       end: CGPoint(x: 0, y: rect.minY), options: [])
            }
            ctx.restoreGState()
            ctx.setStrokeColor(rim.cgColor); ctx.setLineWidth(1); ctx.addPath(part); ctx.strokePath()
        }
        // Shine: the body's own outline shrunk toward its upper edge, so it
        // follows the taper, fading out toward the tail.
        ctx.saveGState()
        ctx.addPath(shape); ctx.clip()
        let shine = ocarinaOutline(CGRect(x: body.minX + body.width * 0.05, y: body.maxY - h * 0.34,
                                          width: body.width * 0.8, height: h * 0.24))
        ctx.addPath(shine); ctx.clip()
        if let fade = CGGradient(colorsSpace: CGColorSpace(name: CGColorSpace.sRGB),
                                 colors: [NSColor.white.withAlphaComponent(0.42).cgColor,
                                          NSColor.white.withAlphaComponent(0.18).cgColor,
                                          NSColor.white.withAlphaComponent(0).cgColor] as CFArray,
                                 locations: [0, 0.6, 1]) {
            ctx.drawLinearGradient(fade, start: CGPoint(x: body.minX, y: 0),
                                   end: CGPoint(x: body.minX + body.width * 0.85, y: 0), options: [])
        }
        ctx.restoreGState()

        // Holes along the belly, growing from low D to high D: bigger holes
        // sound higher on a real ocarina.
        let spanX = body.minX + body.width * 0.1, spanW = body.width * 0.78
        let col = spanW / CGFloat(ocarinaHoles.count)
        for (i, hole) in ocarinaHoles.enumerated() {
            let grow = CGFloat(i) / CGFloat(ocarinaHoles.count - 1)
            let d = min(col * 0.8, h * (0.4 + 0.14 * grow))
            let cx = spanX + col * (CGFloat(i) + 0.5)
            let circle = CGRect(x: cx - d / 2, y: body.midY - d / 2 + h * 0.02, width: d, height: d)
            let on = lit.contains(hole.key)
            let fill = on ? Piano.noteColor(midi: hole.midi)
                          : NSColor(srgbRed: 8/255, green: 12/255, blue: 30/255, alpha: 0.9)
            ctx.setFillColor(fill.cgColor); ctx.fillEllipse(in: circle)
            if on {
                ctx.setStrokeColor(NSColor.white.withAlphaComponent(0.9).cgColor)
                ctx.setLineWidth(1.2); ctx.strokeEllipse(in: circle)
            }
            let t = NSAttributedString(string: hole.key, attributes: [
                .font: berkeleyMono(size: d * 0.55, weight: .bold),
                .foregroundColor: on ? NSColor(srgbRed: 20/255, green: 20/255, blue: 30/255, alpha: 1)
                                     : NSColor.white.withAlphaComponent(0.88)])
            let ts = t.size()
            drawText(in: ctx, text: t, origin: CGPoint(x: circle.midX - ts.width / 2,
                                                        y: circle.midY - ts.height / 2 + 0.5))
        }
    }

    private static func centered(_ ctx: CGContext, _ s: String, font: NSFont, color: NSColor, y: CGFloat) {
        let t = NSAttributedString(string: s, attributes: [.font: font, .foregroundColor: color])
        drawText(in: ctx, text: t, origin: CGPoint(x: (pageWidth - t.size().width) / 2, y: y))
    }

    static func renderOcarina(_ chart: Chart, in ctx: CGContext) {
        let m: CGFloat = 26
        let innerW = pageWidth - 2 * m
        let bg = chart.background
        let titleText = chart.title.hasSuffix(".") ? String(chart.title.dropLast()) : chart.title

        if chart.ocarinaCover {
            centered(ctx, titleText, font: displayFont(size: 34), color: dark, y: pageHeight - 96)
            let sub = NSAttributedString(string: chart.subtitle, attributes: [
                .font: bodyFont(size: 9), .foregroundColor: gray])
            let subH = ceil(sub.boundingRect(with: CGSize(width: innerW, height: 200),
                                             options: [.usesLineFragmentOrigin]).height)
            drawWrappedText(in: ctx, attr: sub, rect: CGRect(x: m, y: pageHeight - 118 - subH,
                                                              width: innerW, height: subH + 2))
            drawOcarinaStrip(in: ctx, rect: CGRect(x: m, y: 236, width: innerW, height: 40),
                             lit: Set(chart.ocarina))
            // Key → note legend, one cap per hole.
            let capW: CGFloat = 40, gap = (innerW - capW * 5) / 4
            for (i, hole) in ocarinaHoles.enumerated() {
                let x = m + CGFloat(i) * (capW + gap)
                drawOcarinaCap(in: ctx, rect: CGRect(x: x, y: 170, width: capW, height: capW),
                               key: hole.key, bg: bg, fontSize: 22)
                let n = NSAttributedString(string: hole.note, attributes: [
                    .font: bodyFont(size: 12, bold: true), .foregroundColor: gray])
                drawText(in: ctx, text: n, origin: CGPoint(x: x + (capW - n.size().width) / 2, y: 150))
            }
            let how = NSAttributedString(string:
                "Set Menu Band's instrument to Ocarina. The letters are the Switch 2 ocarina buttons: press L R A Y X on the keyboard, or the same buttons on a controller. Each card is one song, left to right, row by row. The order of the notes is what counts; take them at an even pace.",
                attributes: [.font: bodyFont(size: 9.5), .foregroundColor: dark])
            drawWrappedText(in: ctx, attr: how, rect: CGRect(x: m, y: 40, width: innerW, height: 96))
            return
        }

        // Song card: title, note line, lit strip, cap rows, text line.
        centered(ctx, titleText, font: displayFont(size: 24), color: dark, y: pageHeight - 70)
        centered(ctx, chart.subtitle, font: bodyFont(size: 9), color: gray, y: pageHeight - 88)
        drawOcarinaStrip(in: ctx, rect: CGRect(x: m + 20, y: pageHeight - 140, width: innerW - 40, height: 32),
                         lit: Set(chart.ocarina))

        let rows = chart.ocarinaRows.isEmpty ? [chart.ocarina.count] : chart.ocarinaRows
        let maxPerRow = CGFloat(rows.max() ?? 4)
        let capGap: CGFloat = 8
        let capW = min(56, (innerW - capGap * (maxPerRow - 1)) / maxPerRow)
        let rowH = capW + 26
        let blockH = rowH * CGFloat(rows.count)
        var y = (pageHeight - 150 + 70) / 2 + blockH / 2 - capW
        var i = 0
        for (r, n) in rows.enumerated() {
            let rowW = capW * CGFloat(n) + capGap * CGFloat(n - 1)
            var x = (pageWidth - rowW) / 2
            for _ in 0..<n where i < chart.ocarina.count {
                let key = chart.ocarina[i]
                drawOcarinaCap(in: ctx, rect: CGRect(x: x, y: y, width: capW, height: capW),
                               key: key, bg: bg, fontSize: capW * 0.52)
                if let hole = ocarinaHole(key) {
                    let num = NSAttributedString(string: "\(i + 1)", attributes: [
                        .font: berkeleyMono(size: 7.5, weight: .bold), .foregroundColor: muted])
                    drawText(in: ctx, text: num, origin: CGPoint(x: x + 3, y: y + capW - 11))
                    let note = NSAttributedString(string: hole.note + (hole.midi >= 86 ? "ʹ" : ""), attributes: [
                        .font: bodyFont(size: 11, bold: true), .foregroundColor: gray])
                    drawText(in: ctx, text: note, origin: CGPoint(x: x + (capW - note.size().width) / 2,
                                                                  y: y - 16))
                }
                x += capW + capGap
                i += 1
            }
            if r < rows.count - 1 { y -= rowH }
        }
        // The whole song as one typed line.
        let line = chart.ocarina.joined(separator: " ").lowercased()
        centered(ctx, line, font: berkeleyMono(size: 13, weight: .bold), color: pink, y: 44)
    }
}
