import AppKit
import ImageIO
import UniformTypeIdentifiers

/// Renders (and content-caches) the macOS desktop picture used by
/// theme-by-status: a flat, solid status color filling the whole screen.
/// One image per aggregate status × appearance; the fill is that status's
/// page color (same family as the themed terminals, so desktop and
/// terminals read as one tone). Mirrors `IconRenderer`'s house style (a
/// stateless CoreGraphics enum), but writes a PNG to disk because macOS
/// wallpaper takes a file path, not a color.
enum DesktopTint {
    /// Ensure the flat-color PNG named `name` filled with `color`
    /// (AppleScript 0–65535 RGB) exists, and return its path.
    /// Content-addressed: the color is hashed into the filename, so a
    /// different averaged color regenerates rather than serving a stale
    /// image. Returns nil if the render or file write fails.
    static func ensure(name: String, color: (Int, Int, Int)) -> String? {
        // A flat fill needs no resolution to look right, so render small
        // and let macOS scale it — keeps the cache tiny and the write
        // instant regardless of display size.
        let w = 256
        let h = 256
        // `renderVersion` is bumped whenever the image design changes so a
        // redesign invalidates the cache instead of serving stale PNGs.
        let renderVersion = 3
        var hasher = Hasher()
        hasher.combine(renderVersion)
        hasher.combine(color.0); hasher.combine(color.1); hasher.combine(color.2)
        let key = String(UInt(bitPattern: hasher.finalize()), radix: 36)

        let dir = Paths.desktopWallpaperDir
        let path = "\(dir)/\(name)-\(key).png"
        if FileManager.default.fileExists(atPath: path) { return path }
        try? FileManager.default.createDirectory(
            atPath: dir, withIntermediateDirectories: true)

        guard let cg = render(width: w, height: h, color: color) else { return nil }
        let url = URL(fileURLWithPath: path)
        guard let dest = CGImageDestinationCreateWithURL(
            url as CFURL, UTType.png.identifier as CFString, 1, nil)
        else { return nil }
        CGImageDestinationAddImage(dest, cg, nil)
        return CGImageDestinationFinalize(dest) ? path : nil
    }

    private static func render(width: Int, height: Int,
                               color: (Int, Int, Int)) -> CGImage? {
        let cs = CGColorSpace(name: CGColorSpace.sRGB) ?? CGColorSpaceCreateDeviceRGB()
        guard let ctx = CGContext(
            data: nil, width: width, height: height,
            bitsPerComponent: 8, bytesPerRow: 0, space: cs,
            bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue)
        else { return nil }
        ctx.setFillColor(
            red: CGFloat(color.0) / 65535,
            green: CGFloat(color.1) / 65535,
            blue: CGFloat(color.2) / 65535,
            alpha: 1)
        ctx.fill(CGRect(x: 0, y: 0, width: width, height: height))
        return ctx.makeImage()
    }
}
