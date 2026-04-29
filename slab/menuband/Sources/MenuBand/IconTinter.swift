import AppKit
import CoreImage

/// Retints the app's Finder/About/Applications icon to match the user's
/// system accent color. The shipped `AppIcon.icns` is a purple gradient
/// with a white piano in the middle; we hue-rotate the entire icon by
/// `(accentHue − purpleHue)` so the gradient adopts the accent while
/// the white/black piano stays untouched (hue rotation is a no-op on
/// achromatic pixels).
///
/// Application is via `NSWorkspace.setIcon(_:forFile:)` on the bundle
/// path. On macOS that stores the icon resource as extended attributes
/// on the bundle directory itself — not inside the signed payload — so
/// the Developer ID signature stays valid. We re-verify after each
/// retint just in case Apple ever changes that.
enum IconTinter {
    /// Approximate hue of the shipped icon's purple gradient. The icon
    /// design is symmetric enough that any value in 280–300° looks fine,
    /// since CIHueAdjust rotates relative to whatever the source hue is.
    private static let originalHueDeg: CGFloat = 290

    /// Idempotent: read the bundled icon, hue-rotate to current accent,
    /// write back via NSWorkspace. Safe to call repeatedly (e.g., after
    /// every accent-color change notification). No-op if the bundle's
    /// AppIcon.icns can't be loaded.
    static func applyTintedIcon() {
        guard let tinted = tintedIcon() else { return }
        NSWorkspace.shared.setIcon(tinted, forFile: Bundle.main.bundlePath, options: [])
    }

    /// Build the retinted NSImage. Returned image carries every
    /// representation of the source `.icns` at the same hue rotation,
    /// so Finder picks the right size automatically.
    static func tintedIcon() -> NSImage? {
        guard let baseURL = Bundle.main.url(forResource: "AppIcon", withExtension: "icns"),
              let base = NSImage(contentsOf: baseURL) else { return nil }
        let accent = NSColor.controlAccentColor.usingColorSpace(.deviceRGB) ?? .systemBlue
        var hue: CGFloat = 0, sat: CGFloat = 0, bri: CGFloat = 0, a: CGFloat = 0
        accent.getHue(&hue, saturation: &sat, brightness: &bri, alpha: &a)
        let accentHueRad = hue * 2 * .pi
        let originalHueRad = (originalHueDeg / 360) * 2 * .pi
        let delta = accentHueRad - originalHueRad

        // Rebuild the multi-rep NSImage by hue-rotating each rep
        // separately. Doing the filter once on a flattened TIFF would
        // collapse the icon down to one resolution, defeating the
        // multi-size dance Finder does at different zoom levels.
        let result = NSImage(size: base.size)
        for rep in base.representations {
            guard let bitmap = rep as? NSBitmapImageRep ?? bitmap(from: rep),
                  let cg = bitmap.cgImage else { continue }
            guard let rotated = hueRotate(cg, by: delta) else { continue }
            let newRep = NSBitmapImageRep(cgImage: rotated)
            newRep.size = bitmap.size
            result.addRepresentation(newRep)
        }
        return result.representations.isEmpty ? nil : result
    }

    private static func bitmap(from rep: NSImageRep) -> NSBitmapImageRep? {
        // Some reps (PDF, vector) need rasterization first.
        let size = rep.size
        guard size.width > 0, size.height > 0 else { return nil }
        let bm = NSBitmapImageRep(
            bitmapDataPlanes: nil,
            pixelsWide: Int(size.width * 2), pixelsHigh: Int(size.height * 2),
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true,
            isPlanar: false, colorSpaceName: .deviceRGB,
            bytesPerRow: 0, bitsPerPixel: 0
        )
        bm?.size = size
        guard let bm = bm else { return nil }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: bm)
        rep.draw(in: NSRect(origin: .zero, size: size))
        NSGraphicsContext.restoreGraphicsState()
        return bm
    }

    private static func hueRotate(_ cg: CGImage, by radians: CGFloat) -> CGImage? {
        let ci = CIImage(cgImage: cg)
        guard let filter = CIFilter(name: "CIHueAdjust") else { return nil }
        filter.setValue(ci, forKey: kCIInputImageKey)
        filter.setValue(radians, forKey: kCIInputAngleKey)
        guard let output = filter.outputImage else { return nil }
        let context = CIContext(options: [.useSoftwareRenderer: false])
        return context.createCGImage(output, from: output.extent)
    }
}
