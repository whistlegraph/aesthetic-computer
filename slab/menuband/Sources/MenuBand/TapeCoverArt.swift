import AppKit

/// Per-recording **album art** Finder icon. Each take gets a bold generative
/// cover — a vivid gradient field with a few big soft shapes and the take's
/// LENGTH set large across the middle. Every recording draws its own random
/// hue + composition, so a Desktop full of takes looks like a stack of records.
/// No waveform, no cassette.
///
/// Attached via `NSWorkspace.shared.setIcon(_:forFile:options:)` after the WAV
/// is written; the icon rides the file's resource fork across Finder copies.
enum TapeCoverArt {
    static let canvasSize = CGSize(width: 512, height: 512)

    /// `waveform` is accepted for call-site compatibility but no longer drawn.
    /// Renders into an offscreen bitmap context (NOT `lockFocus`) so it's safe
    /// to build off the main thread — the take is saved on a background queue.
    static func makeIcon(date: Date,
                          duration: TimeInterval,
                          waveform: [Float] = []) -> NSImage {
        let w = Int(canvasSize.width), h = Int(canvasSize.height)
        guard let rep = NSBitmapImageRep(
                bitmapDataPlanes: nil, pixelsWide: w, pixelsHigh: h,
                bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true,
                isPlanar: false, colorSpaceName: .deviceRGB,
                bytesPerRow: 0, bitsPerPixel: 0),
              let ctx = NSGraphicsContext(bitmapImageRep: rep) else {
            return NSImage(size: canvasSize)
        }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = ctx
        draw(date: date, duration: duration,
             rect: NSRect(origin: .zero, size: canvasSize))
        ctx.flushGraphics()
        NSGraphicsContext.restoreGraphicsState()
        let img = NSImage(size: canvasSize)
        img.addRepresentation(rep)
        return img
    }

    private static func draw(date: Date, duration: TimeInterval, rect: NSRect) {
        NSColor.clear.setFill()
        rect.fill()

        // Rounded-square cover.
        let cover = rect.insetBy(dx: 26, dy: 26)
        let clip = NSBezierPath(roundedRect: cover, xRadius: 44, yRadius: 44)

        // Drop shadow so the cover sits above the Finder background.
        NSGraphicsContext.saveGraphicsState()
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.black.withAlphaComponent(0.35)
        shadow.shadowOffset = NSSize(width: 0, height: -8)
        shadow.shadowBlurRadius = 22
        shadow.set()
        NSColor.black.setFill()
        clip.fill()
        NSGraphicsContext.restoreGraphicsState()

        NSGraphicsContext.saveGraphicsState()
        clip.addClip()

        // One random base hue defines the whole cover.
        let base = CGFloat.random(in: 0..<1)
        func hue(_ shift: CGFloat, _ s: CGFloat, _ b: CGFloat, _ a: CGFloat = 1) -> NSColor {
            NSColor(hue: (base + shift).truncatingRemainder(dividingBy: 1),
                    saturation: s, brightness: b, alpha: a)
        }

        // Gradient field.
        NSGradient(colors: [hue(0.00, 0.70, 0.88), hue(0.09, 0.88, 0.44)])?
            .draw(in: cover, angle: CGFloat.random(in: 15...75))

        // A few big soft discs in complementary hues — the "art".
        for _ in 0..<Int.random(in: 3...5) {
            let r = CGFloat.random(in: cover.width * 0.16 ... cover.width * 0.48)
            let cx = CGFloat.random(in: cover.minX ... cover.maxX)
            let cy = CGFloat.random(in: cover.minY ... cover.maxY)
            hue(CGFloat.random(in: 0.33...0.62),
                CGFloat.random(in: 0.5...0.9),
                CGFloat.random(in: 0.75...1.0),
                CGFloat.random(in: 0.16...0.32)).setFill()
            NSBezierPath(ovalIn: NSRect(x: cx - r, y: cy - r, width: r * 2, height: r * 2)).fill()
        }

        // A bold accent band.
        let bandH = CGFloat.random(in: 24...58)
        let bandY = CGFloat.random(in: cover.minY ... (cover.maxY - bandH))
        hue(0.5, 0.9, 0.96, 0.45).setFill()
        NSBezierPath(rect: NSRect(x: cover.minX, y: bandY,
                                   width: cover.width, height: bandH)).fill()

        // Vignette for depth (dark toward the edges).
        NSGradient(colors: [.clear, NSColor.black.withAlphaComponent(0.30)])?
            .draw(in: cover, relativeCenterPosition: NSPoint(x: 0, y: 0))

        // LENGTH — the hero. Big, bold, centered, auto-fit to width.
        let mins = Int(duration) / 60
        let secs = Int(duration) % 60
        let durStr = String(format: "%d:%02d", mins, secs)
        let para = NSMutableParagraphStyle()
        para.alignment = .center
        let textShadow = NSShadow()
        textShadow.shadowColor = NSColor.black.withAlphaComponent(0.5)
        textShadow.shadowOffset = NSSize(width: 0, height: -3)
        textShadow.shadowBlurRadius = 12
        let durFont = fittedBlackFont(durStr, maxWidth: cover.width - 56, start: 180)
        let durAttrs: [NSAttributedString.Key: Any] = [
            .font: durFont,
            .foregroundColor: NSColor.white,
            .paragraphStyle: para,
            .shadow: textShadow,
            .kern: -2,
        ]
        let ds = (durStr as NSString).size(withAttributes: durAttrs)
        (durStr as NSString).draw(
            in: NSRect(x: cover.minX, y: cover.midY - ds.height / 2 + 8,
                       width: cover.width, height: ds.height),
            withAttributes: durAttrs)

        // Wordmark + date, small along the bottom.
        let foot = "MENU BAND · \(footDate(date))"
        let footAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 20, weight: .bold),
            .foregroundColor: NSColor.white.withAlphaComponent(0.85),
            .kern: 2,
            .paragraphStyle: para,
        ]
        let fs = (foot as NSString).size(withAttributes: footAttrs)
        (foot as NSString).draw(
            in: NSRect(x: cover.minX, y: cover.minY + 26,
                       width: cover.width, height: fs.height),
            withAttributes: footAttrs)

        NSGraphicsContext.restoreGraphicsState()

        // Subtle bright inner edge.
        NSColor.white.withAlphaComponent(0.14).setStroke()
        clip.lineWidth = 2
        clip.stroke()
    }

    /// Largest `.black` system font at which `s` fits `maxWidth`.
    private static func fittedBlackFont(_ s: String, maxWidth: CGFloat, start: CGFloat) -> NSFont {
        var size = start
        while size > 44 {
            let f = NSFont.systemFont(ofSize: size, weight: .black)
            if (s as NSString).size(withAttributes: [.font: f]).width <= maxWidth { return f }
            size -= 6
        }
        return NSFont.systemFont(ofSize: 44, weight: .black)
    }

    private static let footFormatter: DateFormatter = {
        let f = DateFormatter()
        f.dateFormat = "MMM d"
        return f
    }()
    private static func footDate(_ d: Date) -> String {
        footFormatter.string(from: d).uppercased()
    }
}
