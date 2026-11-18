// Media.swift — pictures, videos and files on the sheet.
//
// Every image is downsampled once to a thumbnail no larger than the sheet can
// show, then cached. A video gives its first frame with a play mark. Anything
// else shows as a named file. All of it opens in its own app on click.

import AppKit
import AVFoundation
import ImageIO

enum Media {
    private static let cache = NSCache<NSString, NSImage>()
    private static let videoTypes: Set<String> = ["mp4", "mov", "m4v", "webm"]
    private static let imageTypes: Set<String> = ["jpg", "jpeg", "png", "gif", "heic", "webp", "tiff", "bmp"]

    static func isVideo(_ path: String) -> Bool { videoTypes.contains((path as NSString).pathExtension.lowercased()) }
    static func isImage(_ path: String) -> Bool { imageTypes.contains((path as NSString).pathExtension.lowercased()) }

    // A thumbnail at most `maxPixel` on its long side, or nil when the file is not a picture or video.
    static func thumb(_ path: String, maxPixel: Int = 900) -> NSImage? {
        if let hit = cache.object(forKey: path as NSString) { return hit }
        let url = URL(fileURLWithPath: path)
        var cg: CGImage?
        if isVideo(path) {
            let gen = AVAssetImageGenerator(asset: AVURLAsset(url: url))
            gen.appliesPreferredTrackTransform = true
            gen.maximumSize = CGSize(width: maxPixel, height: maxPixel)
            cg = try? gen.copyCGImage(at: CMTime(seconds: 0.1, preferredTimescale: 600), actualTime: nil)
        } else if let src = CGImageSourceCreateWithURL(url as CFURL, nil) {
            // Works by content, so a picture with a wrong or missing extension still shows.
            let opts: [CFString: Any] = [kCGImageSourceCreateThumbnailFromImageAlways: true,
                                         kCGImageSourceCreateThumbnailWithTransform: true,
                                         kCGImageSourceThumbnailMaxPixelSize: maxPixel]
            cg = CGImageSourceCreateThumbnailAtIndex(src, 0, opts as CFDictionary)
        }
        guard let cg else { return nil }
        let img = NSImage(cgImage: cg, size: NSSize(width: cg.width, height: cg.height))
        cache.setObject(img, forKey: path as NSString)
        return img
    }

    // The size to draw at: inside the box, never enlarged, aspect kept.
    static func fit(_ size: NSSize, maxWidth: CGFloat, maxHeight: CGFloat) -> NSSize {
        guard size.width > 0, size.height > 0 else { return .zero }
        // Thumbnails are in pixels; show at half so they stay sharp on a Retina screen.
        let natural = NSSize(width: size.width / 2, height: size.height / 2)
        let k = min(1, maxWidth / natural.width, maxHeight / natural.height)
        return NSSize(width: (natural.width * k).rounded(), height: (natural.height * k).rounded())
    }

    // A video frame with a play mark in the middle.
    static func withPlayMark(_ img: NSImage, size: NSSize) -> NSImage {
        let out = NSImage(size: size)
        out.lockFocus()
        img.draw(in: NSRect(origin: .zero, size: size))
        let d = min(size.width, size.height, 120) * 0.4
        let r = NSRect(x: (size.width - d) / 2, y: (size.height - d) / 2, width: d, height: d)
        NSColor.black.withAlphaComponent(0.55).setFill()
        NSBezierPath(ovalIn: r).fill()
        let tri = NSBezierPath()
        tri.move(to: NSPoint(x: r.minX + d * 0.38, y: r.minY + d * 0.27))
        tri.line(to: NSPoint(x: r.minX + d * 0.38, y: r.maxY - d * 0.27))
        tri.line(to: NSPoint(x: r.maxX - d * 0.26, y: r.midY))
        tri.close()
        NSColor.white.setFill()
        tri.fill()
        out.unlockFocus()
        return out
    }

    static func sizeLabel(_ path: String) -> String {
        let bytes = ((try? FileManager.default.attributesOfItem(atPath: path))?[.size] as? NSNumber)?.int64Value ?? 0
        guard bytes > 0 else { return "" }
        return ByteCountFormatter.string(fromByteCount: bytes, countStyle: .file)
    }

    static func duration(_ path: String) -> String {
        let s = CMTimeGetSeconds(AVURLAsset(url: URL(fileURLWithPath: path)).duration)
        guard s.isFinite, s > 0 else { return "" }
        return String(format: "%d:%02d", Int(s) / 60, Int(s) % 60)
    }
}
