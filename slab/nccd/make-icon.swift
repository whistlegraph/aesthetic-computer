// make-icon.swift — draw the app icon and write Resources/AppIcon.icns.
//   swift make-icon.swift      (run from slab/nccd; needs iconutil)
// A near-black tile, the chunky pixel "NCCd" wordmark (NCC in the wiki's cyan,
// the d in the icing's pink), and under it a donut with four discs on it for
// the circles.
import AppKit

let letters: [(rows: [String], pink: Bool)] = [
    (["XX..XX", "XXX.XX", "XXXXXX", "XXXXXX", "XX.XXX", "XX..XX", "XX..XX"], false),
    ([".XXXX", "XXXXX", "XX...", "XX...", "XX...", "XXXXX", ".XXXX"], false),
    ([".XXXX", "XXXXX", "XX...", "XX...", "XX...", "XXXXX", ".XXXX"], false),
    (["...XX", "...XX", ".XXXX", "XXXXX", "XX.XX", "XXXXX", ".XXXX"], true),
]
func hex(_ h: String) -> NSColor {
    let v = UInt32(h, radix: 16)!
    return NSColor(srgbRed: CGFloat((v >> 16) & 255) / 255, green: CGFloat((v >> 8) & 255) / 255, blue: CGFloat(v & 255) / 255, alpha: 1)
}

func render(_ px: Int) -> Data {
    let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: px, pixelsHigh: px, bitsPerSample: 8, samplesPerPixel: 4,
                               hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
    let s = CGFloat(px) / 1024
    // the tile, on Apple's icon grid: 824 square, 100 inset, 185 corner
    hex("07100C").setFill()
    NSBezierPath(roundedRect: NSRect(x: 100 * s, y: 100 * s, width: 824 * s, height: 824 * s), xRadius: 185 * s, yRadius: 185 * s).fill()
    // wordmark: "NCCd" in chunky pixels, 24 columns by 7 rows, a darker copy a half pixel under it
    let c = 23 * s, ox = (CGFloat(px) - 24 * c) / 2, top = 850 * s
    for pass in 0..<2 {
        var x0 = 0
        for letter in letters {
            let base = hex(letter.pink ? "FF6FB8" : "00C4FD")
            (pass == 0 ? (base.blended(withFraction: 0.55, of: .black) ?? base) : base).setFill()
            let d = pass == 0 ? c * 0.5 : 0
            for (y, row) in letter.rows.enumerated() {
                for (x, ch) in row.enumerated() where ch == "X" {
                    NSRect(x: ox + CGFloat(x0 + x) * c + d, y: top - CGFloat(y + 1) * c - d, width: c, height: c).fill()
                }
            }
            x0 += letter.rows[0].count + 1
        }
    }
    // the donut: dough, icing with a wavy edge, an open hole
    let mid = NSPoint(x: 512 * s, y: 385 * s), R = 235 * s, hole: CGFloat = 0.40
    func ring(_ outer: @escaping (CGFloat) -> CGFloat, _ inner: @escaping (CGFloat) -> CGFloat) -> NSBezierPath {
        let p = NSBezierPath()
        for radius in [outer, inner] {
            for step in 0...180 {
                let a = CGFloat(step) / 180 * 2 * .pi
                let pt = NSPoint(x: mid.x + radius(a) * cos(a), y: mid.y + radius(a) * sin(a))
                step == 0 ? p.move(to: pt) : p.line(to: pt)
            }
            p.close()
        }
        p.windingRule = .evenOdd
        return p
    }
    hex("E6B873").setFill()
    ring({ _ in R }, { _ in R * hole }).fill()
    hex("FF9BCF").setFill()
    ring({ a in R * (0.905 + 0.035 * sin(11 * a)) }, { a in R * (hole + 0.095 + 0.022 * sin(7 * a + 1)) }).fill()
    // the circles: four discs spread out on it
    let track = R * (1 + hole) / 2 + R * 0.02, r = R * 0.215
    for (i, c) in ["0A64D8", "FF5C80", "FF4DFA", "00D080"].enumerated() {
        let a = (135 - CGFloat(i) * 90) * .pi / 180
        let p = NSPoint(x: mid.x + track * cos(a), y: mid.y + track * sin(a))
        NSColor.white.setFill()
        NSBezierPath(ovalIn: NSRect(x: p.x - r - 7 * s, y: p.y - r - 7 * s, width: 2 * r + 14 * s, height: 2 * r + 14 * s)).fill()
        hex(c).setFill()
        NSBezierPath(ovalIn: NSRect(x: p.x - r, y: p.y - r, width: 2 * r, height: 2 * r)).fill()
    }
    NSGraphicsContext.restoreGraphicsState()
    return rep.representation(using: .png, properties: [:])!
}

let set = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent("NCCd.iconset")
try? FileManager.default.removeItem(at: set)
try! FileManager.default.createDirectory(at: set, withIntermediateDirectories: true)
for (name, px) in [("16x16", 16), ("16x16@2x", 32), ("32x32", 32), ("32x32@2x", 64), ("128x128", 128), ("128x128@2x", 256),
                   ("256x256", 256), ("256x256@2x", 512), ("512x512", 512), ("512x512@2x", 1024)] {
    try! render(px).write(to: set.appendingPathComponent("icon_\(name).png"))
}
let p = Process()
p.executableURL = URL(fileURLWithPath: "/usr/bin/iconutil")
p.arguments = ["-c", "icns", set.path, "-o", "Resources/AppIcon.icns"]
try! p.run()
p.waitUntilExit()
print(p.terminationStatus == 0 ? "wrote Resources/AppIcon.icns" : "iconutil failed")
