import AppKit
import QuartzCore

// Font selection and RockCharLayer copied from Slab PromptSigilOverlay.swift.
func playfulRockFont(_ pt: CGFloat) -> NSFont {
    for n in ["Comic Sans MS Bold", "ComicSansMS-Bold", "Chalkboard SE Bold", "ChalkboardSE-Bold"] {
        if let f = NSFont(name: n, size: pt) { return f }
    }
    return NSFont.systemFont(ofSize: pt, weight: .heavy)
}

/// One character of a rock's name — a layer that draws its attributed string
/// (bubble lettering: fill + stroke + hard shadow), so each letter can carry
/// its own static jitter and wiggle animation, MacPal-style.
final class RockCharLayer: CALayer {
    var attr: NSAttributedString?
    var inset: CGFloat = 0
    override func draw(in ctx: CGContext) {
        guard let attr = attr else { return }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(cgContext: ctx, flipped: false)
        attr.draw(at: NSPoint(x: inset, y: inset))
        NSGraphicsContext.restoreGraphicsState()
    }
}


// Render native Prox lettering as a transparent Retina image for the desktop.
// Arguments carry only a public display label; credentials never enter this helper.
let text = CommandLine.arguments.dropFirst().first ?? "Credits —"
guard text.count <= 100 else { exit(1) }
let pointSize = min(48, max(12, Double(CommandLine.arguments.dropFirst(2).first ?? "20") ?? 20))
func render(_ text: String, _ pointSize: Double) -> (Data, CGFloat, CGFloat, CGFloat) {
let font = playfulRockFont(pointSize)
let inset: CGFloat = 9
let size = (text as NSString).size(withAttributes: [.font: font])
let width = ceil(size.width + inset * 2), height = ceil(size.height + inset * 2)
let scale: CGFloat = 2
guard let bitmap = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: Int(width * scale), pixelsHigh: Int(height * scale), bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0), let context = NSGraphicsContext(bitmapImageRep: bitmap) else { exit(1) }
NSGraphicsContext.saveGraphicsState()
NSGraphicsContext.current = context
context.cgContext.scaleBy(x: scale, y: scale)
// Layered translucent echoes under the same white, thin-outlined Prox face.
let echoes: [(NSColor, CGFloat, CGFloat)] = [
    (NSColor.systemCyan.withAlphaComponent(0.28), 4.5, -3),
    (NSColor.systemPurple.withAlphaComponent(0.40), 3, -2),
    (NSColor.systemPink.withAlphaComponent(0.65), 1.5, -1.5)
]
for (color, x, y) in echoes {
    NSAttributedString(string: text, attributes: [.font: font, .foregroundColor: color]).draw(at: NSPoint(x: inset + x, y: inset + y))
}
let shadow = NSShadow()
shadow.shadowColor = NSColor.systemPink.withAlphaComponent(0.5)
shadow.shadowBlurRadius = 0
shadow.shadowOffset = NSSize(width: 1.5, height: -1.5)
let glyph = RockCharLayer()
glyph.inset = inset
glyph.attr = NSAttributedString(string: text, attributes: [
    .font: font,
    .foregroundColor: NSColor.white.withAlphaComponent(0.94),
    .strokeColor: NSColor(white: 0.08, alpha: 1),
    .strokeWidth: -3.5,
    .shadow: shadow
])
glyph.draw(in: context.cgContext)
NSGraphicsContext.restoreGraphicsState()
guard let data = bitmap.representation(using: .png, properties: [:]) else { exit(1) }
return (data, width, height, size.width)
}
if CommandLine.arguments.dropFirst(3).first == "--glyphs" {
    let letters: [[String: Any]] = text.map { character in
        let (png, width, height, advance) = render(String(character), pointSize)
        return ["image": "data:image/png;base64," + png.base64EncodedString(), "width": width, "height": height, "advance": advance]
    }
    let data = try JSONSerialization.data(withJSONObject: ["glyphs": letters])
    FileHandle.standardOutput.write(data)
} else {
    FileHandle.standardOutput.write(render(text, pointSize).0)
}
