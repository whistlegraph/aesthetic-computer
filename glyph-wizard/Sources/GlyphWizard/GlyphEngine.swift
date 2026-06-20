// GlyphEngine — the parametric skeleton model + stroke expander.
//
// A Glyph is a set of Strokes (skeleton centerlines). The expander offsets
// each centerline by a pen whose width varies with the local tangent angle
// (vertical strokes thick, horizontals thinner = contrast), producing filled
// contours. Closed skeletons (bowls) become annuli (outer loop + reversed
// inner loop) so counters stay open under nonzero winding.
import Foundation
import CoreGraphics

enum Metrics {
    static let em: CGFloat = 1000
    static let xHeight: CGFloat = 520
    static let ascender: CGFloat = 740
    static let descender: CGFloat = -210
}

struct Axes {
    var weight: CGFloat = 96      // pen width in font units (vertical strokes)
    var contrast: CGFloat = 0.0   // 0..0.85 — how much thinner horizontals get
    var width: CGFloat = 1.0      // horizontal scale
    var slant: CGFloat = 0.0      // degrees (italic shear)
    var xHeight: CGFloat = 1.0    // vertical scale of the lowercase
    var tracking: CGFloat = 0.0   // extra font units between glyphs
    var roundEnds: Bool = true    // round vs flat stroke terminals
}

struct Stroke { var pts: [CGPoint]; var closed: Bool }
struct Glyph { var advance: CGFloat; var strokes: [Stroke] }

// ── skeleton construction helpers ───────────────────────────────────────
func seg(_ a: CGPoint, _ b: CGPoint, _ steps: Int = 6) -> Stroke {
    var p = [CGPoint]()
    for i in 0...steps { let t = CGFloat(i)/CGFloat(steps); p.append(CGPoint(x: a.x+(b.x-a.x)*t, y: a.y+(b.y-a.y)*t)) }
    return Stroke(pts: p, closed: false)
}
func poly(_ pts: [CGPoint]) -> Stroke { Stroke(pts: pts, closed: false) }
func ring(cx: CGFloat, cy: CGFloat, rx: CGFloat, ry: CGFloat, steps: Int = 44) -> Stroke {
    var p = [CGPoint]()
    for i in 0..<steps { let a = 2*CGFloat.pi*CGFloat(i)/CGFloat(steps); p.append(CGPoint(x: cx+rx*cos(a), y: cy+ry*sin(a))) }
    return Stroke(pts: p, closed: true)
}
func arc(cx: CGFloat, cy: CGFloat, rx: CGFloat, ry: CGFloat, from a0: CGFloat, to a1: CGFloat, steps: Int = 16) -> Stroke {
    var p = [CGPoint]()
    for i in 0...steps { let t = CGFloat(i)/CGFloat(steps); let a = (a0+(a1-a0)*t)*CGFloat.pi/180; p.append(CGPoint(x: cx+rx*cos(a), y: cy+ry*sin(a))) }
    return Stroke(pts: p, closed: false)
}

// ── the expander ────────────────────────────────────────────────────────
enum Expander {
    static func penWidth(_ angle: CGFloat, _ ax: Axes) -> CGFloat {
        let verticalness = abs(sin(angle))                 // 1 = vertical, 0 = horizontal
        return ax.weight * (1 - ax.contrast * (1 - verticalness))
    }
    static func contours(_ g: Glyph, _ ax: Axes) -> [[CGPoint]] {
        var out = [[CGPoint]]()
        for s in g.strokes where s.pts.count >= 2 {
            let n = s.pts.count
            var left = [CGPoint](), right = [CGPoint]()
            for i in 0..<n {
                let prev = s.closed ? s.pts[(i-1+n)%n] : s.pts[max(i-1,0)]
                let next = s.closed ? s.pts[(i+1)%n] : s.pts[min(i+1,n-1)]
                var tx = next.x-prev.x, ty = next.y-prev.y
                let len = max((tx*tx+ty*ty).squareRoot(), 0.0001); tx/=len; ty/=len
                let nx = -ty, ny = tx                       // left normal
                let hw = penWidth(atan2(next.y-prev.y, next.x-prev.x), ax) / 2
                left.append(CGPoint(x: s.pts[i].x+nx*hw, y: s.pts[i].y+ny*hw))
                right.append(CGPoint(x: s.pts[i].x-nx*hw, y: s.pts[i].y-ny*hw))
            }
            if s.closed {
                out.append(left)                            // outer loop
                out.append(Array(right.reversed()))         // inner loop (reversed = hole)
            } else {
                out.append(left + Array(right.reversed()))  // single filled stroke
            }
        }
        return out
    }
    static func path(_ g: Glyph, _ ax: Axes) -> CGPath {
        let p = CGMutablePath()
        for loop in contours(g, ax) {
            guard let f = loop.first else { continue }
            p.move(to: f); for q in loop.dropFirst() { p.addLine(to: q) }; p.closeSubpath()
        }
        return p
    }
}

// ── the starting skeletons for r·e·g·a·d (v0.1 — author/refine in-app) ───
enum Skeletons {
    static func base() -> [Character: Glyph] {
        var g = [Character: Glyph]()
        let xh = Metrics.xHeight, asc = Metrics.ascender, desc = Metrics.descender

        // r — stem + shoulder arm
        g["r"] = Glyph(advance: 400, strokes: [
            seg(CGPoint(x: 150, y: 0), CGPoint(x: 150, y: xh), 10),
            poly([CGPoint(x: 150, y: xh-110), CGPoint(x: 250, y: xh+10), CGPoint(x: 360, y: xh-10)])
        ])
        // e — bowl + crossbar (aperture comes in a later pass)
        g["e"] = Glyph(advance: 560, strokes: [
            ring(cx: 290, cy: 270, rx: 250, ry: 260),
            seg(CGPoint(x: 60, y: 300), CGPoint(x: 520, y: 300), 8)
        ])
        // g — single-story bowl + descender tail
        g["g"] = Glyph(advance: 560, strokes: [
            ring(cx: 290, cy: 320, rx: 230, ry: 210),
            poly([CGPoint(x: 500, y: 360), CGPoint(x: 500, y: -40), CGPoint(x: 400, y: desc), CGPoint(x: 230, y: desc+30)])
        ])
        // a — single-story bowl + right stem
        g["a"] = Glyph(advance: 560, strokes: [
            ring(cx: 270, cy: 240, rx: 240, ry: 250),
            seg(CGPoint(x: 500, y: 0), CGPoint(x: 500, y: xh-40), 8)
        ])
        // d — bowl + ascending stem
        g["d"] = Glyph(advance: 650, strokes: [
            ring(cx: 300, cy: 260, rx: 250, ry: 260),
            seg(CGPoint(x: 560, y: 0), CGPoint(x: 560, y: asc), 12)
        ])
        // space
        g[" "] = Glyph(advance: 320, strokes: [])
        return g
    }
}
