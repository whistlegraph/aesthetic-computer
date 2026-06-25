import AppKit

// Render real SF Symbols (Apple's own menu-bar glyphs) to crisp tinted PNGs —
// no stretching (natural aspect), no hand-drawn SVG. Ink = #16161a (light bar).
let ink = NSColor(red: 0x16/255.0, green: 0x16/255.0, blue: 0x1a/255.0, alpha: 1)
let OUT = NSString(string: CommandLine.arguments[1]).expandingTildeInPath

func render(_ symbol: String, to file: String, pointSize: CGFloat, weight: NSFont.Weight = .regular) {
    let cfg = NSImage.SymbolConfiguration(pointSize: pointSize, weight: weight)
    guard let base = NSImage(systemSymbolName: symbol, accessibilityDescription: nil),
          let img = base.withSymbolConfiguration(cfg) else {
        FileHandle.standardError.write(Data("MISSING symbol \(symbol)\n".utf8)); return
    }
    let size = img.size
    let scale: CGFloat = 4
    guard let rep = NSBitmapImageRep(bitmapDataPlanes: nil,
        pixelsWide: Int((size.width*scale).rounded()), pixelsHigh: Int((size.height*scale).rounded()),
        bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
        colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0) else { return }
    rep.size = size
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
    let r = NSRect(origin: .zero, size: size)
    img.draw(in: r)
    ink.set(); r.fill(using: .sourceAtop)               // tint template → ink
    NSGraphicsContext.restoreGraphicsState()
    let path = "\(OUT)/\(file)"
    try? rep.representation(using: .png, properties: [:])!.write(to: URL(fileURLWithPath: path))
    print("wrote \(file)  \(rep.pixelsWide)x\(rep.pixelsHigh)  aspect \(String(format: "%.3f", size.width/size.height))")
}

// Menu-bar right cluster + Apple logo, all real SF Symbols.
render("apple.logo",   to: "sf-apple.png",   pointSize: 36, weight: .regular)
render("switch.2",     to: "sf-control.png", pointSize: 30, weight: .regular)  // Control Center
render("wifi",         to: "sf-wifi.png",    pointSize: 30, weight: .regular)
render("battery.100",  to: "sf-battery.png", pointSize: 30, weight: .regular)
