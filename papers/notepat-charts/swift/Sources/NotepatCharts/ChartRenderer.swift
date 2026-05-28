import AppKit
import CoreGraphics

/// A single 4×6" page. Contains the title chrome at the top, the
/// piano hero in the middle, and a body-text block underneath.
struct Chart {
    let title: String           // e.g. "the notepat layout."
    let subtitle: String        // one-line strap
    let highlights: [Int: Piano.Highlight]   // per-key overrides
    let body: [BodyParagraph]   // stacked text below the piano
    let footer: String          // e.g. "NOTEPAT · 1"
    /// Pastel page background. Naturals on the piano stay white so the
    /// colored note tabs read against this softer canvas.
    var background: NSColor = NSColor(srgbRed: 252/255, green: 246/255, blue: 240/255, alpha: 1)
    /// When true, render as a deck cover: oversize title, no piano,
    /// no QWERTY map, just title + subtitle + body + footer.
    var isCover: Bool = false
    /// When true, render via renderSong: drop piano + QWERTY; instead
    /// draw each phrase as a row of colored note-circles above
    /// rounded syllable-boxes (each colored to its notepat note).
    var isSong: Bool = false
    /// Phrase data for song cards. Each phrase is a list of
    /// (notepat-letter, syllable) pairs.
    var songPhrases: [[(String, String)]] = []
    /// How the back face of this card is drawn.
    var backStyle: BackStyle = .info
}

/// Card back layouts. `.info` shows the card's title + subtitle centered;
/// `.pattern` shows a standard brand mark shared across the deck.
enum BackStyle {
    case info
    case pattern
}

struct BodyParagraph {
    /// Inline runs — each chunk is either plain text or bold accent.
    let runs: [Run]
    let topSpacing: CGFloat   // extra space above this paragraph
    enum Run {
        case text(String)
        case bold(String)
        case key(String)      // Berkeley Mono, pink — for note letters
        case mono(String)     // Berkeley Mono, dark — for aligned lyrics
    }
}

extension BodyParagraph {
    static func plain(_ s: String, topSpacing: CGFloat = 6) -> BodyParagraph {
        BodyParagraph(runs: [.text(s)], topSpacing: topSpacing)
    }
}

enum ChartRenderer {

    // Page geometry in PDF points (1pt = 1/72in). 4×6 inches.
    static let pageWidth:  CGFloat = 4 * 72   // 288
    static let pageHeight: CGFloat = 6 * 72   // 432
    static let marginX:    CGFloat = 18
    static let marginTop:  CGFloat = 22
    static let marginBot:  CGFloat = 18

    // Die-cut card chrome — each PDF page is laid out as a physical
    // flashcard sitting on a transparent page surface. The card itself
    // is a rounded-rect fill in the chart's pastel color with a soft
    // drop shadow underneath.
    static let cardInset:  CGFloat = 6        // page-edge → card-edge
    static let cardCorner: CGFloat = 14       // card corner radius
    static let cardShadowOffsetY: CGFloat = -1.4
    static let cardShadowBlur:    CGFloat = 2.4
    static let cardShadowAlpha:   CGFloat = 0.62

    // AC palette — pulled from `ac-paper-cards.sty`.
    static let pink   = NSColor(srgbRed: 205/255, green:  92/255, blue: 155/255, alpha: 1)
    static let dark   = NSColor(srgbRed:  18/255, green:  16/255, blue:  18/255, alpha: 1)
    static let gray   = NSColor(srgbRed: 119/255, green: 119/255, blue: 119/255, alpha: 1)
    static let muted  = NSColor(srgbRed: 168/255, green: 168/255, blue: 172/255, alpha: 1)

    /// Optional embedded keycap illustration for the cover. When set
    /// (and the file exists), the cover renders this PNG in place of
    /// the procedural keycap row. Set from main.swift before render.
    static var coverKeysImagePath: String?

    static func renderPDF(charts: [Chart], to url: URL) {
        // Each PDF page shows BOTH sides of one card — front on the
        // left, standard graphic back on the right. Each face carries
        // the same global card identifier ("01 / 19") in the corner.
        let cardW = pageWidth
        let cardH = pageHeight
        var mediaBox = CGRect(x: 0, y: 0, width: cardW * 2, height: cardH)
        guard let ctx = CGContext(url as CFURL, mediaBox: &mediaBox, nil) else {
            FileHandle.standardError.write(
                Data("Failed to open PDF context\n".utf8))
            return
        }
        let total = charts.count
        for (idx, chart) in charts.enumerated() {
            let id = String(format: "%02d / %02d", idx + 1, total)
            ctx.beginPage(mediaBox: &mediaBox)
            // Front (left).
            ctx.saveGState()
            renderChart(chart, in: ctx)
            drawCardId(id, in: ctx)
            ctx.restoreGState()
            // Back (right) — standard graphic pattern, no card ID.
            ctx.saveGState()
            ctx.translateBy(x: cardW, y: 0)
            drawCardChrome(bg: chart.background, in: ctx)
            renderStandardBack(in: ctx)
            ctx.restoreGState()
            ctx.endPage()
        }
        ctx.closePDF()
    }

    /// Small Berkeley-Mono card identifier in the top-right corner of
    /// the card face. Sits just inside the white polaroid frame.
    private static func drawCardId(_ id: String, in ctx: CGContext) {
        let attr = NSAttributedString(string: id, attributes: [
            .font: berkeleyMono(size: 7.5, weight: .bold),
            .foregroundColor: gray,
            .kern: 0.8,
        ])
        let size = attr.size()
        let pad: CGFloat = cardInset + 12
        drawText(in: ctx, text: attr,
                 origin: CGPoint(x: pageWidth - pad - size.width,
                                  y: pageHeight - pad - size.height))
    }

    /// Info back — big centered title + small wrapped subtitle.
    /// Kept around in case a card wants a per-card back instead of
    /// the standard graphic pattern. Not wired into renderPDF today.
    private static func renderInfoBack(_ chart: Chart, in ctx: CGContext) {
        drawCardChrome(bg: chart.background, in: ctx)

        let innerW = pageWidth - 2 * marginX
        let dotIndex = chart.title.lastIndex(of: ".")
        let titleText: String = {
            if let dot = dotIndex { return String(chart.title[..<dot]) }
            return chart.title
        }()

        // Auto-shrink title to one line (mirrors renderSong's logic).
        func buildTitle(at size: CGFloat) -> NSAttributedString {
            NSAttributedString(string: titleText, attributes: [
                .font: displayFont(size: size),
                .foregroundColor: dark,
            ])
        }
        let dTitleSize: CGFloat = 32
        let probe = buildTitle(at: dTitleSize)
        let probeW = probe.size().width
        let titleSize: CGFloat = probeW <= innerW
            ? dTitleSize
            : max(14, floor(dTitleSize * innerW / probeW))
        let titleAttr = titleSize == dTitleSize ? probe : buildTitle(at: titleSize)
        let titleH = ceil(titleAttr.size().height)
        let titleW = ceil(titleAttr.size().width)

        let subAttr = NSAttributedString(string: chart.subtitle, attributes: [
            .font: bodyFont(size: 11),
            .foregroundColor: gray,
        ])
        let subBounds = subAttr.boundingRect(
            with: CGSize(width: innerW, height: .greatestFiniteMagnitude),
            options: [.usesLineFragmentOrigin, .usesFontLeading])
        let subH = ceil(subBounds.height)

        // Vertically center the title + subtitle block.
        let blockH = titleH + 14 + subH
        let blockTopY = (pageHeight + blockH) / 2
        let titleY = blockTopY - titleH
        let subY   = titleY - 14 - subH

        drawText(in: ctx, text: titleAttr,
                 origin: CGPoint(x: (pageWidth - titleW) / 2, y: titleY))
        drawWrappedText(in: ctx, attr: subAttr,
                        rect: CGRect(x: marginX, y: subY,
                                      width: innerW, height: subH + 4))
    }

    /// Standard graphic-only back — a tiled grid of colored note
    /// dots (the C D E F G A B notepat palette) covering the whole
    /// inner card. No text, no wordmark. The same back appears on
    /// every card so the deck reads as a unified pack when flipped.
    private static func renderStandardBack(in ctx: CGContext) {
        let innerLeft  = cardInset + 14
        let innerRight = pageWidth - cardInset - 14
        let innerTop   = pageHeight - cardInset - 14
        let innerBot   = cardInset + 14
        let innerW = innerRight - innerLeft
        let innerH = innerTop - innerBot

        let colors: [NSColor] = [
            NSColor(srgbRed: 255/255, green:  50/255, blue:  50/255, alpha: 1),  // C
            NSColor(srgbRed: 255/255, green: 160/255, blue:   0/255, alpha: 1),  // D
            NSColor(srgbRed: 255/255, green: 230/255, blue:   0/255, alpha: 1),  // E
            NSColor(srgbRed:  50/255, green: 200/255, blue:  50/255, alpha: 1),  // F
            NSColor(srgbRed:  50/255, green: 120/255, blue: 255/255, alpha: 1),  // G
            NSColor(srgbRed: 130/255, green:  50/255, blue: 200/255, alpha: 1),  // A
            NSColor(srgbRed: 180/255, green:  80/255, blue: 255/255, alpha: 1),  // B
        ]
        let dot: CGFloat = 14
        let dotGap: CGFloat = 8
        let pitch = dot + dotGap
        let cols = Int(floor(innerW / pitch))
        let rows = Int(floor(innerH / pitch))
        let gridW = CGFloat(cols) * pitch - dotGap
        let gridH = CGFloat(rows) * pitch - dotGap
        let gridX = innerLeft + (innerW - gridW) / 2
        let gridY = innerBot  + (innerH - gridH) / 2
        for r in 0..<rows {
            for c in 0..<cols {
                let color = colors[(r * cols + c) % colors.count]
                let x = gridX + CGFloat(c) * pitch
                let y = gridY + CGFloat(r) * pitch
                ctx.setFillColor(color.withAlphaComponent(0.55).cgColor)
                ctx.fillEllipse(in: CGRect(x: x, y: y, width: dot, height: dot))
            }
        }
    }

    static func renderChart(_ chart: Chart, in ctx: CGContext) {
        // Die-cut card chrome — the rounded-corner card silhouette in
        // the chart's pastel color, with a soft shadow underneath. The
        // PDF page itself is left unfilled (transparent), so the deck
        // reads as a stack of physical cards.
        drawCardChrome(bg: chart.background, in: ctx)

        if chart.isCover {
            renderCover(chart, in: ctx)
            return
        }
        if chart.isSong {
            renderSong(chart, in: ctx)
            return
        }

        // Header: big title, then a one-line subtitle. Sit slightly
        // lower so the title clears the inset border with breathing room.
        var y = pageHeight - marginTop - 8

        // ── Title (big YWFT Processing). Wraps within the inner width.
        let titleFont = displayFont(size: 26)
        let titleAttr = NSMutableAttributedString()
        let dotIndex = chart.title.lastIndex(of: ".")
        if let dot = dotIndex {
            let main = String(chart.title[..<dot])
            titleAttr.append(NSAttributedString(string: main, attributes: [
                .font: titleFont,
                .foregroundColor: dark,
            ]))
            titleAttr.append(NSAttributedString(string: ".", attributes: [
                .font: titleFont,
                .foregroundColor: pink,
            ]))
        } else {
            titleAttr.append(NSAttributedString(string: chart.title, attributes: [
                .font: titleFont,
                .foregroundColor: dark,
            ]))
        }
        let titleWidth = pageWidth - 2 * marginX
        let titleBounds = titleAttr.boundingRect(
            with: CGSize(width: titleWidth, height: .greatestFiniteMagnitude),
            options: [.usesLineFragmentOrigin, .usesFontLeading])
        let titleH = ceil(titleBounds.height)
        y -= titleH
        drawWrappedText(in: ctx,
                        attr: titleAttr,
                        rect: CGRect(x: marginX, y: y, width: titleWidth, height: titleH + 2))

        // ── Subtitle.
        y -= 6
        let subAttr = NSAttributedString(string: chart.subtitle, attributes: [
            .font: bodyFont(size: 9.5),
            .foregroundColor: gray,
        ])
        let subBounds = subAttr.boundingRect(
            with: CGSize(width: titleWidth, height: .greatestFiniteMagnitude),
            options: [.usesLineFragmentOrigin, .usesFontLeading])
        let subH = ceil(subBounds.height)
        y -= subH
        drawWrappedText(in: ctx,
                        attr: subAttr,
                        rect: CGRect(x: marginX, y: y, width: titleWidth, height: subH + 2))

        // ── Piano + QWERTY diagrams. On theory cards they stack
        // vertically (piano above, QWERTY below). On SONG cards they
        // render side-by-side as tiny outline-only reference badges
        // (no QWERTY letters, no note-name captions) so the body
        // block dominates the card.
        if chart.isSong {
            // Side-by-side micro layout.
            let whiteW: CGFloat = 6.0
            let layout = Piano.defaultLayout(whiteWidth: whiteW)
            let whites = Piano.whiteList()
            let pianoW = CGFloat(whites.count) * whiteW
            let capW: CGFloat = 6.0
            let qLayout = QwertyMap.defaultLayout(capWidth: capW)
            let qW = QwertyMap.totalWidth(layout: qLayout)
            let qH = QwertyMap.totalHeight(layout: qLayout)
            let blockGap: CGFloat = 16
            let blockW = pianoW + blockGap + qW
            let blockX = (pageWidth - blockW) / 2

            y -= 16
            // Piano on the left — align by piano bottom and QWERTY bottom.
            let pianoBottomY = y - layout.whiteH
            Piano.draw(in: ctx,
                       origin: CGPoint(x: blockX, y: pianoBottomY),
                       layout: layout,
                       highlights: chart.highlights,
                       showLetters: false,
                       showNoteNames: false)
            // QWERTY on the right — bottom-align with the piano so the
            // two reference badges sit on the same baseline.
            let qX = blockX + pianoW + blockGap
            let qY = pianoBottomY + (layout.whiteH - qH) / 2
            QwertyMap.draw(in: ctx,
                           origin: CGPoint(x: qX, y: qY),
                           layout: qLayout,
                           highlights: chart.highlights,
                           showLetters: false)
            y = pianoBottomY - 14
        } else {
            // Default stacked layout (theory cards).
            let whiteW: CGFloat = 14.0
            let layout = Piano.defaultLayout(whiteWidth: whiteW)
            let whites = Piano.whiteList()
            let pianoW = CGFloat(whites.count) * whiteW
            let pianoX = (pageWidth - pianoW) / 2

            y -= 22
            let pianoBottomY = y - layout.whiteH
            Piano.draw(in: ctx,
                       origin: CGPoint(x: pianoX, y: pianoBottomY),
                       layout: layout,
                       highlights: chart.highlights)
            y = pianoBottomY - 16

            let capW: CGFloat = 14.0
            let qLayout = QwertyMap.defaultLayout(capWidth: capW)
            let qW = QwertyMap.totalWidth(layout: qLayout)
            let qH = QwertyMap.totalHeight(layout: qLayout)
            let qX = (pageWidth - qW) / 2
            let qY = y - qH
            QwertyMap.draw(in: ctx,
                           origin: CGPoint(x: qX, y: qY),
                           layout: qLayout,
                           highlights: chart.highlights)
            y = qY - 20
        }

        // ── Body paragraphs. On song cards the lyric/key block is the
        // hero — bump font sizes and inset the margin so the text
        // reads at arm's length.
        let bodyFontReg:  NSFont
        let bodyFontBold: NSFont
        let bodyFontKey:  NSFont
        let bodyMarginX:  CGFloat
        if chart.isSong {
            // 11pt mono is sized so Twinkle's longest phrase (7 pairs
            // ≈ 37 chars wide) fits in the inner card width without
            // mid-phrase wrapping (which would break the key↔syllable
            // alignment). All other songs have shorter phrases.
            bodyFontReg  = bodyFont(size: 11.0)
            bodyFontBold = bodyFont(size: 11.0, bold: true)
            bodyFontKey  = berkeleyMono(size: 11.0, weight: .bold)
            bodyMarginX  = 22
        } else {
            bodyFontReg  = bodyFont(size: 9.5)
            bodyFontBold = bodyFont(size: 9.5, bold: true)
            bodyFontKey  = berkeleyMono(size: 9.0, weight: .bold)
            bodyMarginX  = marginX
        }
        let bodyWidth = pageWidth - 2 * bodyMarginX

        for para in chart.body {
            y -= para.topSpacing
            let m = NSMutableAttributedString()
            for run in para.runs {
                switch run {
                case .text(let s):
                    m.append(NSAttributedString(string: s, attributes: [
                        .font: bodyFontReg,
                        .foregroundColor: dark,
                    ]))
                case .bold(let s):
                    m.append(NSAttributedString(string: s, attributes: [
                        .font: bodyFontBold,
                        .foregroundColor: dark,
                    ]))
                case .key(let s):
                    m.append(NSAttributedString(string: s, attributes: [
                        .font: bodyFontKey,
                        .foregroundColor: pink,
                    ]))
                case .mono(let s):
                    m.append(NSAttributedString(string: s, attributes: [
                        .font: bodyFontKey,        // same mono pitch as .key
                        .foregroundColor: dark,
                    ]))
                }
            }
            // Measure how tall the wrapped text will be in `bodyWidth`.
            let bounds = m.boundingRect(
                with: CGSize(width: bodyWidth, height: .greatestFiniteMagnitude),
                options: [.usesLineFragmentOrigin, .usesFontLeading])
            let h = ceil(bounds.height)
            y -= h
            let rect = CGRect(x: bodyMarginX, y: y, width: bodyWidth, height: h + 2)
            drawWrappedText(in: ctx, attr: m, rect: rect)
        }

        // Footer intentionally omitted — the deck reads cleaner without
        // page-numbering chrome.
    }

    private static func drawText(in ctx: CGContext,
                                  text: NSAttributedString,
                                  origin: CGPoint) {
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(cgContext: ctx, flipped: false)
        text.draw(at: origin)
        NSGraphicsContext.restoreGraphicsState()
    }

    /// Draw the die-cut card chrome: tight drop shadow under a white
    /// outer frame (polaroid style), with the chart's pastel color
    /// filling the inner area. Page background is left transparent.
    private static func drawCardChrome(bg: NSColor, in ctx: CGContext) {
        let cardRect = CGRect(
            x: cardInset,
            y: cardInset,
            width: pageWidth - 2 * cardInset,
            height: pageHeight - 2 * cardInset)
        let outerPath = CGPath(roundedRect: cardRect,
                                cornerWidth: cardCorner,
                                cornerHeight: cardCorner,
                                transform: nil)
        // 1. White outer frame with the tight drop shadow.
        ctx.saveGState()
        ctx.setShadow(
            offset: CGSize(width: 0, height: cardShadowOffsetY),
            blur: cardShadowBlur,
            color: NSColor.black.withAlphaComponent(cardShadowAlpha).cgColor)
        ctx.setFillColor(NSColor.white.cgColor)
        ctx.addPath(outerPath)
        ctx.fillPath()
        ctx.restoreGState()
        // 2. Inner pastel area — inset by frameW, corner radius reduced
        //    to match so the white frame reads as a uniform-width strip
        //    all the way around.
        let frameW: CGFloat = 6
        let innerRect = cardRect.insetBy(dx: frameW, dy: frameW)
        let innerPath = CGPath(
            roundedRect: innerRect,
            cornerWidth: max(0, cardCorner - frameW),
            cornerHeight: max(0, cardCorner - frameW),
            transform: nil)
        ctx.setFillColor(bg.cgColor)
        ctx.addPath(innerPath)
        ctx.fillPath()
        // 3. Thin purple stroke on the very outer edge of the card —
        //    the material edge of the printed stock. acpurple at full
        //    opacity, ~0.7pt so it reads as a deliberate finishing line.
        let purple = NSColor(srgbRed: 120/255, green: 80/255, blue: 180/255, alpha: 1)
        ctx.saveGState()
        ctx.setStrokeColor(purple.cgColor)
        ctx.setLineWidth(0.7)
        ctx.addPath(outerPath)
        ctx.strokePath()
        ctx.restoreGState()
    }

    /// Deck cover page. Smart anchored layout:
    ///   - title pinned to the top of the inner card area
    ///   - subtitle directly below the title
    ///   - byline pinned to the bottom of the inner card area
    ///   - keys illustration fills whatever vertical space remains
    ///     between the subtitle and the byline (height-OR-width fit,
    ///     centered both axes, aspect preserved)
    /// Skips chart.body entirely; the cover is meant to be sparse.
    private static func renderCover(_ chart: Chart, in ctx: CGContext) {
        // ── PHASE 1 · FULL-BLEED ILLY ────────────────────────────────
        // Cover-fit the keys illustration to the entire card silhouette
        // FIRST so it sits behind the title/byline text, then re-stroke
        // the purple rim on top so the material edge stays crisp.
        let cardRect = CGRect(
            x: cardInset, y: cardInset,
            width: pageWidth - 2 * cardInset,
            height: pageHeight - 2 * cardInset)
        let outerPath = CGPath(roundedRect: cardRect,
                                cornerWidth: cardCorner,
                                cornerHeight: cardCorner,
                                transform: nil)
        if let imgPath = ChartRenderer.coverKeysImagePath,
           let nsimg = NSImage(contentsOfFile: imgPath),
           let cgimg = nsimg.cgImage(forProposedRect: nil, context: nil, hints: nil) {
            ctx.saveGState()
            ctx.addPath(outerPath)
            ctx.clip()
            let imgW = CGFloat(cgimg.width)
            let imgH = CGFloat(cgimg.height)
            let scale = max(cardRect.width / imgW, cardRect.height / imgH)
            let drawW = imgW * scale
            let drawH = imgH * scale
            let drawX = cardRect.midX - drawW / 2
            let drawY = cardRect.midY - drawH / 2
            ctx.draw(cgimg, in: CGRect(x: drawX, y: drawY,
                                        width: drawW, height: drawH))
            ctx.restoreGState()
            let purple = NSColor(srgbRed: 120/255, green: 80/255, blue: 180/255, alpha: 1)
            ctx.saveGState()
            ctx.setStrokeColor(purple.cgColor)
            ctx.setLineWidth(0.7)
            ctx.addPath(outerPath)
            ctx.strokePath()
            ctx.restoreGState()
        }

        // ── PHASE 2 · TEXT OVERLAYS ──────────────────────────────────
        // Inner card bounds for text placement.
        let pad: CGFloat = 16
        let innerLeft  = cardInset + pad
        let innerRight = pageWidth  - cardInset - pad
        let innerTop   = pageHeight - cardInset - pad
        let innerBot   = cardInset + pad
        let innerW     = innerRight - innerLeft

        // ── Title (top-anchored). Auto-shrink until it fits in ≤2 lines.
        let titleStr = chart.title
        let dotIndex = titleStr.lastIndex(of: ".")
        func buildTitle(size: CGFloat) -> NSAttributedString {
            let f = displayFont(size: size)
            let m = NSMutableAttributedString()
            if let dot = dotIndex {
                m.append(NSAttributedString(string: String(titleStr[..<dot]), attributes: [
                    .font: f, .foregroundColor: dark,
                ]))
                m.append(NSAttributedString(string: ".", attributes: [
                    .font: f, .foregroundColor: pink,
                ]))
            } else {
                m.append(NSAttributedString(string: titleStr, attributes: [
                    .font: f, .foregroundColor: dark,
                ]))
            }
            return m
        }
        var titleSize: CGFloat = 44
        var titleAttr = buildTitle(size: titleSize)
        var titleBounds = titleAttr.boundingRect(
            with: CGSize(width: innerW, height: .greatestFiniteMagnitude),
            options: [.usesLineFragmentOrigin, .usesFontLeading])
        // Allow up to 2 lines of title; shrink in 2pt steps otherwise.
        while ceil(titleBounds.height) > titleSize * 2.2 && titleSize > 28 {
            titleSize -= 2
            titleAttr = buildTitle(size: titleSize)
            titleBounds = titleAttr.boundingRect(
                with: CGSize(width: innerW, height: .greatestFiniteMagnitude),
                options: [.usesLineFragmentOrigin, .usesFontLeading])
        }
        let titleH = ceil(titleBounds.height)
        let titleY = innerTop - titleH
        drawWrappedText(in: ctx, attr: titleAttr,
                        rect: CGRect(x: innerLeft, y: titleY, width: innerW, height: titleH + 2))

        // Subtitle intentionally omitted from the cover — the illy and
        // the title carry the message; copy lives on the inner cards.

        // ── Byline (bottom-anchored).
        let bylineAttr = NSMutableAttributedString()
        bylineAttr.append(NSAttributedString(string: "by ", attributes: [
            .font: bodyFont(size: 9.5), .foregroundColor: gray,
        ]))
        bylineAttr.append(NSAttributedString(string: "@jeffrey", attributes: [
            .font: bodyFont(size: 9.5, bold: true), .foregroundColor: dark,
        ]))
        bylineAttr.append(NSAttributedString(string: "  ·  aesthetic.computer  ·  may 2026", attributes: [
            .font: bodyFont(size: 9.5), .foregroundColor: gray,
        ]))
        let bylineBounds = bylineAttr.boundingRect(
            with: CGSize(width: innerW, height: .greatestFiniteMagnitude),
            options: [.usesLineFragmentOrigin, .usesFontLeading])
        let bylineH = ceil(bylineBounds.height)
        let bylineSize = bylineAttr.size()
        let bylineY = innerBot
        drawText(in: ctx, text: bylineAttr,
                 origin: CGPoint(x: (pageWidth - bylineSize.width) / 2,
                                  y: bylineY))

    }

    /// Song card layout. Title + subtitle at top, then each phrase as
    /// a row of colored note-circles above white syllable-boxes (the
    /// box outline takes the note's color). Drops the piano and QWERTY
    /// diagrams entirely — the note circles are the legend.
    private static func renderSong(_ chart: Chart, in ctx: CGContext) {
        // Small-print title + subtitle at the BOTTOM of the card —
        // the song notes/lyrics dominate the page; the song name is
        // just the foot stamp.
        let titleWidth = pageWidth - 2 * marginX
        let dotIndex = chart.title.lastIndex(of: ".")
        let titleText: String = {
            if let dot = dotIndex { return String(chart.title[..<dot]) }
            return chart.title
        }()
        let titleAttr = NSAttributedString(string: titleText, attributes: [
            .font: bodyFont(size: 11, bold: true),
            .foregroundColor: dark,
            .kern: 0.5,
        ])
        let titleH = ceil(titleAttr.size().height)
        let titleW = ceil(titleAttr.size().width)

        let subAttr = NSAttributedString(string: chart.subtitle, attributes: [
            .font: bodyFont(size: 7.5),
            .foregroundColor: gray,
        ])
        let subBounds = subAttr.boundingRect(
            with: CGSize(width: titleWidth, height: .greatestFiniteMagnitude),
            options: [.usesLineFragmentOrigin, .usesFontLeading])
        let subH = ceil(subBounds.height)

        let footBot: CGFloat = cardInset + 12
        let subY = footBot
        let titleY = subY + subH + 4
        drawWrappedText(in: ctx, attr: subAttr,
                        rect: CGRect(x: marginX, y: subY,
                                      width: titleWidth, height: subH + 2))
        drawText(in: ctx, text: titleAttr,
                 origin: CGPoint(x: (pageWidth - titleW) / 2, y: titleY))

        let y = pageHeight - marginTop - 4
        let footTop = titleY + titleH + 10   // phrases stop above here

        // ── Responsive layout ──────────────────────────────────────
        // Flatten all phrases into a single stream of UNITS — each is
        // either a (note,syllable) pair or a phrase-separator bar.
        // Greedy-wrap the stream at any chosen scale, then binary-search
        // for the LARGEST scale where the laid-out height still fits
        // inside the available card area. The result fills the card
        // for short songs and shrinks gracefully for long ones.
        enum Unit {
            case pair(note: String, syl: String)
            case sep   // phrase-break bar
        }
        var units: [Unit] = []
        for (i, phrase) in chart.songPhrases.enumerated() {
            if i > 0 { units.append(.sep) }
            for (n, s) in phrase { units.append(.pair(note: n, syl: s)) }
        }
        guard !units.isEmpty else { return }

        // Default un-scaled metrics.
        let dCircleD:  CGFloat = 26
        let dBoxH:     CGFloat = 22
        let dStackGap: CGFloat = 4
        let dColGap:   CGFloat = 4
        let dRowGap:   CGFloat = 10
        let dBoxPadX:  CGFloat = 7
        let dNoteSize: CGFloat = 16.0
        let dSylSize:  CGFloat = 12.0
        let dSepW:     CGFloat = 18    // horizontal bar separator width

        let availTop = y
        let availBot = footTop                // stay clear of small-print foot
        let availH = availTop - availBot
        let availW = pageWidth - 2 * marginX

        // Layout function: returns (rows of placed units, total height).
        // `placed` for each row is an array of (unit, x, width). The row
        // height = circle + gap + box. Vertical gap between rows = rowGap.
        struct Placed { let unit: Unit; let x: CGFloat; let w: CGFloat }
        func layout(scale: CGFloat) -> (rows: [[Placed]], h: CGFloat) {
            let circleD = dCircleD * scale
            let sylFont = bodyFont(size: dSylSize * scale, bold: true)
            let boxPadX = dBoxPadX * scale
            let colGap  = dColGap  * scale
            let sepW    = dSepW    * scale
            var rows: [[Placed]] = []
            var current: [Placed] = []
            var x: CGFloat = 0
            for u in units {
                let w: CGFloat
                switch u {
                case .pair(_, let s):
                    let txt = (s as NSString).size(withAttributes: [.font: sylFont]).width
                    w = max(circleD, ceil(txt) + boxPadX * 2)
                case .sep:
                    w = sepW
                }
                let gap = current.isEmpty ? 0 : colGap
                if x + gap + w > availW && !current.isEmpty {
                    // wrap; drop trailing separator on the wrapped row
                    if case .sep = current.last?.unit { current.removeLast() }
                    rows.append(current)
                    current = []
                    x = 0
                }
                // skip leading separator at the start of a new row
                if current.isEmpty {
                    if case .sep = u { continue }
                    current.append(Placed(unit: u, x: 0, w: w))
                    x = w
                } else {
                    current.append(Placed(unit: u, x: x + gap, w: w))
                    x += gap + w
                }
            }
            if !current.isEmpty {
                if case .sep = current.last?.unit { current.removeLast() }
                rows.append(current)
            }
            let rowH = circleD + dStackGap * scale + dBoxH * scale
            let h = CGFloat(rows.count) * rowH
                + CGFloat(max(0, rows.count - 1)) * dRowGap * scale
            return (rows, h)
        }

        // Binary-search for the largest scale that fits the height.
        var lo: CGFloat = 0.35
        var hi: CGFloat = 2.6
        for _ in 0..<22 {
            let s = (lo + hi) / 2
            let (_, h) = layout(scale: s)
            if h > availH { hi = s } else { lo = s }
        }
        let scale = lo
        let (rows, totalH) = layout(scale: scale)

        let circleD   = dCircleD * scale
        let boxH      = dBoxH    * scale
        let stackGap  = dStackGap * scale
        let rowGap    = dRowGap   * scale
        let cornerR: CGFloat = max(2.5, 4 * scale)
        let noteFont  = berkeleyMono(size: dNoteSize * scale, weight: .bold)
        let sylFont   = bodyFont(size: dSylSize * scale, bold: true)

        // Vertically center the laid-out block in the available area.
        var rowY = availTop - max(0, (availH - totalH) / 2)
        for row in rows {
            // Horizontally center this row.
            let rowW = (row.last?.x ?? 0) + (row.last?.w ?? 0)
            let xOffset = (pageWidth - rowW) / 2
            let circleBottomY = rowY - circleD
            let boxBottomY = circleBottomY - stackGap - boxH

            for placed in row {
                let x = xOffset + placed.x
                let cw = placed.w
                let cx = x + cw / 2
                switch placed.unit {
                case .pair(let note, let syl):
                    let color = noteColorForLetter(note)
                    // Circle — filled note color with a darker stroke
                    // ring (~35% darker hue) so the note pops as a
                    // bordered chip.
                    let circleR = CGRect(x: cx - circleD/2, y: circleBottomY,
                                          width: circleD, height: circleD)
                    ctx.setFillColor(color.cgColor)
                    ctx.addEllipse(in: circleR)
                    ctx.fillPath()
                    let ring = NSColor(
                        srgbRed:   color.redComponent   * 0.62,
                        green:     color.greenComponent * 0.62,
                        blue:      color.blueComponent  * 0.62,
                        alpha:     1)
                    let ringW = max(1.0, scale * 1.6)
                    ctx.setStrokeColor(ring.cgColor)
                    ctx.setLineWidth(ringW)
                    ctx.strokeEllipse(in: circleR.insetBy(dx: ringW/2, dy: ringW/2))
                    let lum = (0.299 * color.redComponent +
                               0.587 * color.greenComponent +
                               0.114 * color.blueComponent)
                    let letterColor: NSColor = lum > 0.62
                        ? NSColor(srgbRed: 0.10, green: 0.08, blue: 0.10, alpha: 1)
                        : .white
                    drawCenteredText(in: ctx, text: note.uppercased(),
                                      center: CGPoint(x: cx, y: circleR.midY),
                                      font: noteFont, color: letterColor)
                    // Box — subtle tinted fill in the note's color so
                    // the syllable inherits its note's tint, with a
                    // slightly darker border in the same hue.
                    let boxR = CGRect(x: x, y: boxBottomY, width: cw, height: boxH)
                    let boxPath = CGPath(roundedRect: boxR, cornerWidth: cornerR,
                                          cornerHeight: cornerR, transform: nil)
                    ctx.setFillColor(color.withAlphaComponent(0.14).cgColor)
                    ctx.addPath(boxPath)
                    ctx.fillPath()
                    ctx.setStrokeColor(color.withAlphaComponent(0.75).cgColor)
                    ctx.setLineWidth(max(0.6, scale * 0.9))
                    ctx.addPath(boxPath)
                    ctx.strokePath()
                    drawCenteredText(in: ctx, text: syl,
                                      center: CGPoint(x: cx, y: boxR.midY),
                                      font: sylFont, color: dark)
                case .sep:
                    // Horizontal phrase-divider bar. Sits at the
                    // syllable-box vertical center, spans most of the
                    // separator's width, in soft gray.
                    let barH = max(1.0, 1.6 * scale)
                    let barW = cw * 0.7
                    let barX = cx - barW / 2
                    let barY = boxBottomY + boxH / 2 - barH / 2
                    ctx.saveGState()
                    ctx.setFillColor(NSColor.black.withAlphaComponent(0.32).cgColor)
                    ctx.fill(CGRect(x: barX, y: barY, width: barW, height: barH))
                    ctx.restoreGState()
                }
            }
            rowY = boxBottomY - rowGap
        }
    }

    /// Look up the notepat color for a QWERTY letter (c, d, e, …, h, i, j, …).
    /// Naturals return their own color; sharps return the natural-to-the-left
    /// color (same rule the menuband piano uses).
    private static func noteColorForLetter(_ letter: String) -> NSColor {
        for (midi, l) in Piano.notepatLetter where l == letter {
            if Piano.isWhite(midi) { return Piano.noteColor(midi: midi) }
            var left = midi - 1
            while !Piano.isWhite(left) { left -= 1 }
            return Piano.noteColor(midi: left)
        }
        return gray
    }
}
