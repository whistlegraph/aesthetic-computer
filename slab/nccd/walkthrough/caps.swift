import AppKit
// draw each caption as a 1920x120 strip: white monospace words, centred, on the bar's colour
let dir = CommandLine.arguments[1]
for f in try! FileManager.default.contentsOfDirectory(atPath: dir) where f.hasSuffix(".txt") {
    let text = try! String(contentsOfFile: dir + "/" + f, encoding: .utf8)
    let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: 1920, pixelsHigh: 120, bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
    NSGraphicsContext.saveGraphicsState()
    NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
    NSColor(srgbRed: 0.043, green: 0.165, blue: 0.22, alpha: 1).setFill()
    NSRect(x: 0, y: 0, width: 1920, height: 120).fill()
    var size: CGFloat = 46
    var s = NSAttributedString(string: text, attributes: [.font: NSFont.monospacedSystemFont(ofSize: size, weight: .semibold), .foregroundColor: NSColor.white])
    while s.size().width > 1840 { size -= 2; s = NSAttributedString(string: text, attributes: [.font: NSFont.monospacedSystemFont(ofSize: size, weight: .semibold), .foregroundColor: NSColor.white]) }
    s.draw(at: NSPoint(x: (1920 - s.size().width) / 2, y: (120 - s.size().height) / 2))
    NSGraphicsContext.restoreGraphicsState()
    try! rep.representation(using: .png, properties: [:])!.write(to: URL(fileURLWithPath: dir + "/" + f.replacingOccurrences(of: ".txt", with: ".png")))
}
