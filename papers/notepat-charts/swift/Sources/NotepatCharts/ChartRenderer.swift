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
}

struct BodyParagraph {
    /// Inline runs — each chunk is either plain text or bold accent.
    let runs: [Run]
    let topSpacing: CGFloat   // extra space above this paragraph
    enum Run {
        case text(String)
        case bold(String)
        case key(String)      // typeset in Berkeley Mono, accent color
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

    // AC palette — pulled from `ac-paper-cards.sty`.
    static let pink   = NSColor(srgbRed: 205/255, green:  92/255, blue: 155/255, alpha: 1)
    static let dark   = NSColor(srgbRed:  18/255, green:  16/255, blue:  18/255, alpha: 1)
    static let gray   = NSColor(srgbRed: 119/255, green: 119/255, blue: 119/255, alpha: 1)
    static let muted  = NSColor(srgbRed: 168/255, green: 168/255, blue: 172/255, alpha: 1)

    static func renderPDF(charts: [Chart], to url: URL) {
        var mediaBox = CGRect(x: 0, y: 0, width: pageWidth, height: pageHeight)
        guard let ctx = CGContext(url as CFURL, mediaBox: &mediaBox, nil) else {
            FileHandle.standardError.write(
                Data("Failed to open PDF context\n".utf8))
            return
        }
        for chart in charts {
            ctx.beginPage(mediaBox: &mediaBox)
            renderChart(chart, in: ctx)
            ctx.endPage()
        }
        ctx.closePDF()
    }

    static func renderChart(_ chart: Chart, in ctx: CGContext) {
        // Pastel background — set per-chart so the page reads warm and
        // the piano's white keys pop forward.
        ctx.setFillColor(chart.background.cgColor)
        ctx.fill(CGRect(x: 0, y: 0, width: pageWidth, height: pageHeight))

        // Header: big title, then a one-line subtitle. (No page-count
        // chrome — the footer carries the card number.)
        var y = pageHeight - marginTop

        // ── Title (big YWFT Processing). Wraps within the inner width.
        let titleFont = displayFont(size: 26)
        // Title gets a colored "." accent in pink. Build attributed
        // string with the trailing period in pink.
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

        // ── Subtitle (wraps if it exceeds the inner width).
        y -= 2
        let subAttr = NSAttributedString(string: chart.subtitle, attributes: [
            .font: bodyFont(size: 9.5),
            .foregroundColor: gray,
        ])
        let subWidth = pageWidth - 2 * marginX
        let subBounds = subAttr.boundingRect(
            with: CGSize(width: subWidth, height: .greatestFiniteMagnitude),
            options: [.usesLineFragmentOrigin, .usesFontLeading])
        let subH = ceil(subBounds.height)
        y -= subH
        drawWrappedText(in: ctx,
                        attr: subAttr,
                        rect: CGRect(x: marginX, y: y, width: subWidth, height: subH + 2))

        // ── Piano — horizontal hero block.
        // 17 whites in our range. With 14pt per white the strip
        // measures 238pt — fits inside the inner 252pt width.
        let whiteW: CGFloat = 14.0
        let layout = Piano.defaultLayout(whiteWidth: whiteW)
        let whites = Piano.whiteList()
        let pianoW = CGFloat(whites.count) * whiteW
        let pianoX = (pageWidth - pianoW) / 2

        // Sit the piano a comfortable gap below the subtitle.
        y -= 14
        let pianoTopY = y                    // top of white key
        let pianoBottomY = pianoTopY - layout.whiteH
        Piano.draw(in: ctx,
                   origin: CGPoint(x: pianoX, y: pianoBottomY),
                   layout: layout,
                   highlights: chart.highlights)
        y = pianoBottomY - 10

        // ── QWERTY map — physical-key reference under the piano. Same
        // color language as the menuband popover: naturals in their
        // note color, sharps dark with a colored inner ring.
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
        y = qY - 12

        // ── Body paragraphs.
        let bodyFontReg  = bodyFont(size: 9.5)
        let bodyFontBold = bodyFont(size: 9.5, bold: true)
        let bodyFontKey  = berkeleyMono(size: 9.0, weight: .bold)

        let bodyWidth = pageWidth - 2 * marginX

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
                }
            }
            // Measure how tall the wrapped text will be in `bodyWidth`.
            let bounds = m.boundingRect(
                with: CGSize(width: bodyWidth, height: .greatestFiniteMagnitude),
                options: [.usesLineFragmentOrigin, .usesFontLeading])
            let h = ceil(bounds.height)
            y -= h
            let rect = CGRect(x: marginX, y: y, width: bodyWidth, height: h + 2)
            drawWrappedText(in: ctx, attr: m, rect: rect)
        }

        // ── Footer (mono, muted, centered).
        let footerAttr = NSAttributedString(string: chart.footer, attributes: [
            .font: berkeleyMono(size: 6.5, weight: .bold),
            .foregroundColor: muted,
            .kern: 1.4,
        ])
        let fSize = footerAttr.size()
        drawText(in: ctx, text: footerAttr,
                 origin: CGPoint(x: (pageWidth - fSize.width) / 2,
                                  y: marginBot - fSize.height / 2))
    }

    private static func drawText(in ctx: CGContext,
                                  text: NSAttributedString,
                                  origin: CGPoint) {
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(cgContext: ctx, flipped: false)
        text.draw(at: origin)
        NSGraphicsContext.restoreGraphicsState()
    }
}
