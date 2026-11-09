import AppKit

/// Main-thread bitmap cache. Draw closures run once per artwork/scale, not
/// again when AppKit composites an NSImage drawing handler.
final class RasterArtworkCache<Key: Hashable> {
    private struct EntryKey: Hashable {
        let artwork: Key
        let width, height, scaleX, scaleY: CGFloat
        let flipped: Bool
    }
    private var images: [EntryKey: NSImage] = [:]
    private var bytes = 0
    private let maximumBytes = 8 * 1024 * 1024
    private let capacity: Int
    private(set) var rasterizations = 0
    var count: Int { images.count }
    // Used by render comparisons; bypass must preserve the original drawing.
    var enabled = true

    init(capacity: Int = 256) { self.capacity = capacity }
    func removeAll() { images.removeAll(keepingCapacity: true); bytes = 0 }

    func draw(_ artwork: Key, in rect: NSRect, padding: CGFloat = 0,
              paint: () -> Void) {
        guard enabled, let current = NSGraphicsContext.current,
              rect.width > 0, rect.height > 0 else { paint(); return }
        let ctm = current.cgContext.ctm
        let sx = max(1, hypot(ctm.a, ctm.b)), sy = max(1, hypot(ctm.c, ctm.d))
        let area = rect.insetBy(dx: -padding, dy: -padding)
        let key = EntryKey(artwork: artwork, width: area.width, height: area.height,
                           scaleX: sx, scaleY: sy, flipped: current.isFlipped)
        if let image = images[key] {
            image.draw(in: area, from: .zero, operation: .sourceOver, fraction: 1,
                       respectFlipped: true, hints: [.interpolation: NSImageInterpolation.high])
            return
        }
        let w = Int(ceil(area.width * sx)), h = Int(ceil(area.height * sy))
        guard w <= 4096, h <= 4096, w * h * 4 <= maximumBytes,
              let rep = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: w, pixelsHigh: h,
                bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
                colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0),
              let bitmap = NSGraphicsContext(bitmapImageRep: rep) else { paint(); return }
        rep.size = area.size
        NSGraphicsContext.saveGraphicsState()
        let cg = bitmap.cgContext
        cg.scaleBy(x: CGFloat(w) / area.width, y: CGFloat(h) / area.height)
        if current.isFlipped {
            cg.translateBy(x: 0, y: area.height)
            cg.scaleBy(x: 1, y: -1)
        }
        cg.translateBy(x: -area.minX, y: -area.minY)
        NSGraphicsContext.current = NSGraphicsContext(cgContext: cg, flipped: current.isFlipped)
        paint()
        NSGraphicsContext.restoreGraphicsState()
        let image = NSImage(size: area.size)
        image.addRepresentation(rep)
        if images.count >= capacity || bytes + w * h * 4 > maximumBytes { removeAll() }
        bytes += w * h * 4
        images[key] = image
        rasterizations += 1
        image.draw(in: area, from: .zero, operation: .sourceOver, fraction: 1,
                   respectFlipped: true, hints: [.interpolation: NSImageInterpolation.high])
    }
}

/// Resolve dynamic AppKit colors before using them as bitmap identity.
func artworkColorKey(_ color: NSColor) -> [CGFloat] {
    guard let c = color.usingColorSpace(.deviceRGB) else { return [] }
    return [c.redComponent, c.greenComponent, c.blueComponent, c.alphaComponent]
}

/// Freeze a frame into pixels before handing it to AppKit. The source may
/// contain a drawing handler that reads live renderer state; the result does
/// not retain that handler or repeat its work on later compositing passes.
func rasterizedArtwork(_ source: NSImage, scale: CGFloat) -> NSImage {
    let size = source.size
    guard size.width > 0, size.height > 0, scale.isFinite, scale > 0 else { return source }
    let width = ceil(size.width * scale), height = ceil(size.height * scale)
    guard width <= 8192, height <= 8192, width * height * 4 <= 32 * 1024 * 1024,
          let rep = NSBitmapImageRep(bitmapDataPlanes: nil,
            pixelsWide: Int(width), pixelsHigh: Int(height), bitsPerSample: 8,
            samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0),
          let context = NSGraphicsContext(bitmapImageRep: rep) else { return source }
    rep.size = size
    NSGraphicsContext.saveGraphicsState()
    defer { NSGraphicsContext.restoreGraphicsState() }
    context.cgContext.scaleBy(x: width / size.width, y: height / size.height)
    NSGraphicsContext.current = context
    source.draw(in: NSRect(origin: .zero, size: size), from: .zero,
                operation: .sourceOver, fraction: 1, respectFlipped: true, hints: nil)
    let result = NSImage(size: size)
    result.addRepresentation(rep)
    result.isTemplate = source.isTemplate
    return result
}
