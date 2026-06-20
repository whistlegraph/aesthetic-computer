// ContactSheet — headless render of a large grid of randomized-axes "regarde"
// lockups from the from-scratch skeleton engine. Light + dark, 128 cells each,
// the SAME 128 axis-sets in both so they're comparable.
//   swift run GlyphWizard contact <outDir>
import AppKit
import CoreGraphics

enum ContactSheet {
    static let COUNT = 128, COLS = 8
    static var ROWS: Int { (COUNT + COLS - 1) / COLS }   // 16
    static let cellW: CGFloat = 480, cellH: CGFloat = 280, top: CGFloat = 76

    static func run(outDir: String) {
        let axes = (0..<COUNT).map { _ in randomAxes() }
        let dir = URL(fileURLWithPath: outDir, isDirectory: true)
        for theme in ["dark", "light"] {
            let rep = sheet(theme: theme, axes: axes)
            let out = dir.appendingPathComponent("regarde-logotype-contact-\(theme).png")
            if let png = rep.representation(using: .png, properties: [:]) {
                try? png.write(to: out)
                FileHandle.standardError.write("wrote \(out.path) (\(rep.pixelsWide)×\(rep.pixelsHigh))\n".data(using: .utf8)!)
            }
        }
    }

    static func randomAxes() -> Axes {
        var ax = Axes()
        ax.weight = CGFloat.random(in: 26...216)
        ax.contrast = [0, 0, CGFloat.random(in: 0...0.72)].randomElement()!
        ax.width = CGFloat.random(in: 0.68...1.34)
        ax.slant = CGFloat([0, 0, Double.random(in: 0...18)].randomElement()!)
        ax.xHeight = CGFloat.random(in: 0.88...1.14)
        ax.tracking = CGFloat.random(in: -18...78)
        ax.roundEnds = Bool.random()
        return ax
    }

    static func sheet(theme: String, axes: [Axes]) -> NSBitmapImageRep {
        let W = Int(cellW * CGFloat(COLS)), H = Int(cellH * CGFloat(ROWS) + top)
        let dark = (theme == "dark")
        let bg = dark ? NSColor(red: 0.047, green: 0.047, blue: 0.051, alpha: 1) : NSColor(red: 0.969, green: 0.965, blue: 0.957, alpha: 1)
        let fg = dark ? NSColor(white: 0.96, alpha: 1) : NSColor(red: 0.047, green: 0.047, blue: 0.051, alpha: 1)
        let cap = dark ? NSColor(white: 0.55, alpha: 1) : NSColor(white: 0.45, alpha: 1)
        let line = dark ? NSColor(white: 0.14, alpha: 1) : NSColor(white: 0.86, alpha: 1)
        let red = NSColor(red: 0.898, green: 0.141, blue: 0.165, alpha: 1)

        let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: W, pixelsHigh: H,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
        let ctx = NSGraphicsContext.current!.cgContext

        bg.setFill(); ctx.fill(CGRect(x: 0, y: 0, width: W, height: H))
        NSString(string: "regarde ■   ·   GLYPHWIZARD CONTACT SHEET   ·   128 from-scratch skeleton variations   ·   \(theme)")
            .draw(at: NSPoint(x: 28, y: CGFloat(H) - 50),
                  withAttributes: [.font: NSFont.monospacedSystemFont(ofSize: 17, weight: .bold), .foregroundColor: fg])

        let glyphs = Skeletons.base()
        for i in 0..<min(COUNT, axes.count) {
            let c = i % COLS, r = i / COLS
            let x = CGFloat(c) * cellW
            let y = CGFloat(ROWS - 1 - r) * cellH
            let cell = CGRect(x: x, y: y, width: cellW, height: cellH)
            line.setStroke(); ctx.stroke(cell.insetBy(dx: 0.5, dy: 0.5), width: 1)
            drawLockup(ctx, in: cell, glyphs: glyphs, ax: axes[i], fg: fg, red: red)
            let a = axes[i]
            NSString(string: String(format: "w%.0f c%.2f %.2f× %.0f° tr%.0f", a.weight, a.contrast, a.width, a.slant, a.tracking))
                .draw(at: NSPoint(x: x + 18, y: y + 14),
                      withAttributes: [.font: NSFont.monospacedSystemFont(ofSize: 11, weight: .regular), .foregroundColor: cap])
        }

        NSGraphicsContext.restoreGraphicsState()
        return rep
    }

    static func drawLockup(_ ctx: CGContext, in cell: CGRect, glyphs: [Character: Glyph], ax: Axes, fg: NSColor, red: NSColor) {
        let word = "regarde"
        let scale = (cell.height * 0.42) / Metrics.em
        let originX = cell.minX + 30, originY = cell.minY + cell.height * 0.46
        let tanS = tan(ax.slant * .pi / 180)
        func toView(_ p: CGPoint, _ penX: CGFloat) -> CGPoint {
            let gx = p.x * ax.width + penX, gy = p.y * ax.xHeight
            return CGPoint(x: originX + (gx + tanS * gy) * scale, y: originY + gy * scale)
        }
        fg.setFill()
        var penX: CGFloat = 0, lastEnd = originX
        for ch in word {
            guard let g = glyphs[ch] else { continue }
            let path = CGMutablePath()
            for loop in Expander.contours(g, ax) {
                guard let f = loop.first else { continue }
                path.move(to: toView(f, penX))
                for q in loop.dropFirst() { path.addLine(to: toView(q, penX)) }
                path.closeSubpath()
            }
            ctx.addPath(path); ctx.fillPath(using: .winding)
            lastEnd = toView(CGPoint(x: g.advance, y: 0), penX).x
            penX += g.advance * ax.width + ax.tracking
        }
        let sq = Metrics.xHeight * 0.3 * scale * ax.xHeight
        red.setFill(); ctx.fill(CGRect(x: lastEnd + 20, y: originY, width: sq, height: sq))
    }
}
