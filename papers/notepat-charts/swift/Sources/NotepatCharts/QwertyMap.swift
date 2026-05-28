import AppKit
import CoreGraphics

/// Drawn underneath the piano on each chart — a compact 3-row map of
/// the physical QWERTY layout (q-row, a-row, z-row) with notepat keys
/// colored to match the piano's note tabs. The cap for a key gets:
///
///   • A bright fill in that note's color, if the chart marks it `active`.
///   • A muted treatment, if the chart marks it `dimmed`.
///   • A neutral fill, if the chart leaves it as `normal` (layout legend).
///
/// Caller passes a [MIDI: Piano.Highlight] map and we map each cap's
/// QWERTY letter back through `Piano.notepatLetter` to find which note
/// it plays.
enum QwertyMap {

    struct Layout {
        let capW: CGFloat
        let capH: CGFloat
        let capGap: CGFloat
        let rowGap: CGFloat
        let cornerR: CGFloat
        let letterFontSize: CGFloat
        let row1Indent: CGFloat   // a-row vs q-row stagger
        let row2Indent: CGFloat   // z-row vs q-row stagger
    }

    static func defaultLayout(capWidth: CGFloat) -> Layout {
        let w = capWidth
        return Layout(
            capW: w,
            capH: w * 0.95,
            capGap: w * 0.10,
            rowGap: w * 0.12,
            cornerR: w * 0.15,
            letterFontSize: w * 0.50,
            row1Indent: w * 0.30,
            row2Indent: w * 0.90
        )
    }

    /// The three physical rows in notepat order (left → right). The
    /// q-row carries most of the sharps; the a-row carries the lower
    /// octave's whites; the z-row reaches below middle C.
    static let rowQ: [String] = ["q","w","e","r","t","y","u","i","o","p"]
    static let rowA: [String] = ["a","s","d","f","g","h","j","k","l",";"]
    static let rowZ: [String] = ["z","x","c","v","b","n","m"]

    /// Reverse-map from QWERTY letter to the MIDI it plays. Built once.
    static let midiByLetter: [String: Int] = {
        var out: [String: Int] = [:]
        for (m, l) in Piano.notepatLetter {
            out[l] = m
        }
        return out
    }()

    /// Total drawn width of the QWERTY block (q-row, no indent).
    static func totalWidth(layout: Layout) -> CGFloat {
        let count = max(rowQ.count, max(rowA.count, rowZ.count))
        return CGFloat(count) * layout.capW
             + CGFloat(count - 1) * layout.capGap
    }

    /// Total drawn height of the QWERTY block (3 rows + 2 gaps).
    static func totalHeight(layout: Layout) -> CGFloat {
        return layout.capH * 3 + layout.rowGap * 2
    }

    /// Draw the map. `origin` is the BOTTOM-LEFT of the q-row (which
    /// is the longest, leftmost row in CGContext y-up coords).
    static func draw(in ctx: CGContext,
                     origin: CGPoint,
                     layout: Layout,
                     highlights: [Int: Piano.Highlight] = [:],
                     showLetters: Bool = true) {
        let topRowY    = origin.y + layout.capH * 2 + layout.rowGap * 2   // q-row top
        let midRowY    = origin.y + layout.capH * 1 + layout.rowGap * 1   // a-row top
        let botRowY    = origin.y                                          // z-row top
        drawRow(in: ctx, letters: rowQ,
                originX: origin.x, originY: topRowY,
                layout: layout, highlights: highlights, showLetters: showLetters)
        drawRow(in: ctx, letters: rowA,
                originX: origin.x + layout.row1Indent, originY: midRowY,
                layout: layout, highlights: highlights, showLetters: showLetters)
        drawRow(in: ctx, letters: rowZ,
                originX: origin.x + layout.row2Indent, originY: botRowY,
                layout: layout, highlights: highlights, showLetters: showLetters)
    }

    private static func drawRow(in ctx: CGContext,
                                letters: [String],
                                originX: CGFloat,
                                originY: CGFloat,
                                layout: Layout,
                                highlights: [Int: Piano.Highlight],
                                showLetters: Bool) {
        for (i, letter) in letters.enumerated() {
            let x = originX + CGFloat(i) * (layout.capW + layout.capGap)
            let rect = CGRect(x: x, y: originY,
                              width: layout.capW, height: layout.capH)
            let midi = midiByLetter[letter]
            let highlight: Piano.Highlight = midi.flatMap { highlights[$0] } ?? .normal
            drawCap(in: ctx, rect: rect, layout: layout,
                    letter: letter, midi: midi, highlight: highlight,
                    showLetter: showLetters)
        }
    }

    private static func drawCap(in ctx: CGContext,
                                rect: CGRect,
                                layout: Layout,
                                letter: String,
                                midi: Int?,
                                highlight: Piano.Highlight,
                                showLetter: Bool = true) {
        // Color treatment is an ACCENT only:
        //   • naturals  → white cap, small colored swatch tab at the
        //                  bottom (same as menuband piano whites)
        //   • sharps    → dark cap, thin colored stripe at the bottom
        //                  taken from the natural it sharps
        //   • active    → bright color, full opacity + dark outer ring
        //   • dimmed    → desaturated accent, body stays white/dark
        let isSharp: Bool = midi.map { !Piano.isWhite($0) } ?? false
        let dimmed: Bool = { if case .dimmed = highlight { return true } else { return false } }()
        let isActive: Bool = { if case .active = highlight { return true } else { return false } }()

        // Accent color: the natural's color (for naturals) or the
        // adjacent natural's color (for sharps).
        let accent: NSColor = {
            guard let m = midi else { return .clear }
            if Piano.isWhite(m) { return Piano.noteColor(midi: m) }
            var left = m - 1
            while !Piano.isWhite(left) { left -= 1 }
            return Piano.noteColor(midi: left)
        }()

        // ── Cap body ────────────────────────────────────────────────
        let bodyColor: NSColor
        let letterColor: NSColor
        let border: NSColor
        if isSharp {
            bodyColor = NSColor(srgbRed: 18/255, green: 16/255, blue: 18/255, alpha: 1.0)
            letterColor = .white
            border = NSColor.black
        } else {
            bodyColor = .white
            letterColor = NSColor(srgbRed: 18/255, green: 16/255, blue: 18/255, alpha: 1.0)
            border = NSColor(srgbRed: 110/255, green: 104/255, blue: 120/255, alpha: 1.0)
        }

        ctx.saveGState()
        let path = CGPath(roundedRect: rect,
                          cornerWidth: layout.cornerR,
                          cornerHeight: layout.cornerR,
                          transform: nil)
        ctx.setFillColor(bodyColor.cgColor)
        ctx.addPath(path)
        ctx.fillPath()
        // Drop-shadow bar under each cap.
        ctx.setStrokeColor(NSColor.black.withAlphaComponent(0.18).cgColor)
        ctx.setLineWidth(0.4)
        ctx.move(to: CGPoint(x: rect.minX + layout.cornerR * 0.4,
                              y: rect.minY - 0.4))
        ctx.addLine(to: CGPoint(x: rect.maxX - layout.cornerR * 0.4,
                                 y: rect.minY - 0.4))
        ctx.strokePath()
        // Outline.
        ctx.setStrokeColor(border.cgColor)
        ctx.setLineWidth(0.5)
        ctx.addPath(path)
        ctx.strokePath()
        ctx.restoreGState()

        // ── Note color IS the highlight ─────────────────────────────
        // Active  → the cap floods with its own note color (lit).
        // Normal  → a ring in the note's color (the layout legend).
        // Dimmed  → ring drains to a colorless gray so only the lesson
        //           notes carry color on a chord / triad card.
        var finalLetterColor = letterColor
        if midi != nil {
            let ringW: CGFloat = max(0.9, layout.capW * 0.09)
            let inset = ringW / 2 + 0.5

            if isActive {
                // Flood fill in the note color.
                ctx.saveGState()
                let clip = CGPath(roundedRect: rect.insetBy(dx: 0.5, dy: 0.5),
                                  cornerWidth: max(0, layout.cornerR - 0.5),
                                  cornerHeight: max(0, layout.cornerR - 0.5),
                                  transform: nil)
                ctx.addPath(clip)
                ctx.clip()
                ctx.setFillColor(accent.cgColor)
                ctx.fill(rect)
                ctx.restoreGState()
                // Letter contrast picked off the flood color.
                finalLetterColor = luminance(accent) > 0.62 ? NSColor.black : NSColor.white
            }

            let ringColor: NSColor = dimmed
                ? Piano.dimGray.withAlphaComponent(0.55)
                : accent
            let ringRect = rect.insetBy(dx: inset, dy: inset)
            ctx.saveGState()
            ctx.setStrokeColor(ringColor.cgColor)
            ctx.setLineWidth(isActive ? ringW * 1.1 : ringW)
            ctx.addPath(CGPath(roundedRect: ringRect,
                               cornerWidth: max(0, layout.cornerR - inset),
                               cornerHeight: max(0, layout.cornerR - inset),
                               transform: nil))
            ctx.strokePath()
            ctx.restoreGState()
        }

        // Letter — uppercase, centered. Skipped on song cards (mini-mode).
        if showLetter {
            drawCenteredText(in: ctx,
                              text: letter.uppercased(),
                              center: CGPoint(x: rect.midX, y: rect.midY + layout.capH * 0.06),
                              font: berkeleyMono(size: layout.letterFontSize, weight: .bold),
                              color: finalLetterColor)
        }
    }

    private static func blend(_ a: NSColor, with b: NSColor, fraction: CGFloat) -> NSColor {
        return a.blended(withFraction: fraction, of: b) ?? a
    }

    private static func luminance(_ c: NSColor) -> CGFloat {
        guard let rgb = c.usingColorSpace(.sRGB) else { return 0.5 }
        return 0.2126 * rgb.redComponent
            + 0.7152 * rgb.greenComponent
            + 0.0722 * rgb.blueComponent
    }
}
