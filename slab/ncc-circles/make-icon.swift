// make-icon.swift — draw the app icon and write Resources/AppIcon.icns.
//   swift make-icon.swift      (run from slab/ncc-circles; needs iconutil)
// A near-black tile, the block-character NCC wordmark in the wiki's cyan,
// and four discs on a ring beneath it for the circles.
import AppKit

let rows: [[(Int, Character)]] = [
    [(0, "▛"), (1, "▚"), (5, "▌"), (9, "▞"), (10, "▔"), (11, "▔"), (12, "▚"), (16, "▞"), (17, "▔"), (18, "▔"), (19, "▚")],
    [(0, "▌"), (2, "▚"), (5, "▌"), (8, "▞"), (15, "▞")],
    [(0, "▌"), (3, "▚"), (5, "▌"), (8, "▚"), (15, "▚")],
    [(0, "▌"), (4, "▚"), (5, "▌"), (9, "▚"), (10, "▂"), (11, "▂"), (12, "▞"), (16, "▚"), (17, "▂"), (18, "▂"), (19, "▞")],
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
    // wordmark: 200x64 units, drawn 3.1x, top-left origin flipped by hand
    let k = 3.1 * s, w = 10 * k, h = 16 * k
    let ox = (CGFloat(px) - 200 * k) / 2, top = 720 * s
    hex("00C4FD").setFill()
    for (row, cells) in rows.enumerated() {
        for (col, ch) in cells {
            let parts: [(CGFloat, CGFloat, CGFloat, CGFloat)]
            switch ch {
            case "▛": parts = [(0, 0, 1, 0.5), (0, 0.5, 0.5, 0.5)]
            case "▚": parts = [(0, 0, 0.5, 0.5), (0.5, 0.5, 0.5, 0.5)]
            case "▞": parts = [(0.5, 0, 0.5, 0.5), (0, 0.5, 0.5, 0.5)]
            case "▌": parts = [(0, 0, 0.5, 1)]
            case "▔": parts = [(0, 0, 1, 0.125)]
            case "▂": parts = [(0, 0.75, 1, 0.25)]
            default: parts = []
            }
            for (px_, py, pw, ph) in parts {
                let x = ox + CGFloat(col) * w + px_ * w
                let yTop = top - CGFloat(row) * h - py * h
                NSRect(x: x, y: yTop - ph * h, width: pw * w, height: ph * h).fill()
            }
        }
    }
    // the circles: four discs on one ring
    let mid = NSPoint(x: 512 * s, y: 330 * s), rx = 190 * s, ry = 96 * s, r = 50 * s
    hex("D2F3E1").withAlphaComponent(0.45).setStroke()
    let ring = NSBezierPath(ovalIn: NSRect(x: mid.x - rx, y: mid.y - ry, width: 2 * rx, height: 2 * ry))
    ring.lineWidth = 7 * s
    ring.stroke()
    for (i, c) in ["00D080", "FF4DFA", "FF5C80", "0A84C1"].enumerated() {
        let a = (45 + CGFloat(i) * 90) * .pi / 180
        let p = NSPoint(x: mid.x + rx * cos(a), y: mid.y + ry * sin(a))
        hex("07100C").setFill()
        NSBezierPath(ovalIn: NSRect(x: p.x - r - 8 * s, y: p.y - r - 8 * s, width: 2 * r + 16 * s, height: 2 * r + 16 * s)).fill()
        hex(c).setFill()
        NSBezierPath(ovalIn: NSRect(x: p.x - r, y: p.y - r, width: 2 * r, height: 2 * r)).fill()
    }
    NSGraphicsContext.restoreGraphicsState()
    return rep.representation(using: .png, properties: [:])!
}

let set = URL(fileURLWithPath: NSTemporaryDirectory()).appendingPathComponent("NCCCircles.iconset")
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
