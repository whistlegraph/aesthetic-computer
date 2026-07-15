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

        // Rounded-square cover — big radius to echo the Menu Band app icon.
        let cover = rect.insetBy(dx: 26, dy: 26)
        let clip = NSBezierPath(roundedRect: cover, xRadius: 104, yRadius: 104)

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

        // Menu Band purple→magenta gradient. A small per-take hue drift keeps a
        // shelf of takes varied while staying unmistakably Menu Band.
        let drift = CGFloat.random(in: -0.035...0.035)
        func mb(_ h: CGFloat, _ s: CGFloat, _ b: CGFloat, _ a: CGFloat = 1) -> NSColor {
            NSColor(hue: (0.80 + h + drift).truncatingRemainder(dividingBy: 1),
                    saturation: s, brightness: b, alpha: a)
        }
        NSGradient(colors: [mb(0.03, 0.72, 0.88), mb(-0.05, 0.86, 0.40)])?
            .draw(in: cover, angle: 125)

        // Soft magenta sheen, upper-left (matches the icon's light).
        mb(0.04, 0.50, 1.0, 0.16).setFill()
        let gr = cover.width * 0.62
        NSBezierPath(ovalIn: NSRect(x: cover.minX - gr * 0.15, y: cover.maxY - gr * 0.65,
                                    width: gr, height: gr)).fill()

        // The identity: a piano-keys strip across the middle, a couple of keys lit.
        let stripH = cover.height * 0.24
        let strip = NSRect(x: cover.minX + cover.width * 0.11, y: cover.midY - stripH / 2,
                           width: cover.width * 0.78, height: stripH)
        drawKeyboardStrip(in: strip)

        let para = NSMutableParagraphStyle()
        para.alignment = .center

        // LENGTH — bold, in the upper third above the keys.
        let mins = Int(duration) / 60
        let secs = Int(duration) % 60
        let durStr = String(format: "%d:%02d", mins, secs)
        let textShadow = NSShadow()
        textShadow.shadowColor = NSColor.black.withAlphaComponent(0.5)
        textShadow.shadowOffset = NSSize(width: 0, height: -3)
        textShadow.shadowBlurRadius = 12
        let durFont = fittedBlackFont(durStr, maxWidth: cover.width - 120, start: 120)
        let durAttrs: [NSAttributedString.Key: Any] = [
            .font: durFont,
            .foregroundColor: NSColor.white,
            .paragraphStyle: para,
            .shadow: textShadow,
            .kern: -2,
        ]
        let ds = (durStr as NSString).size(withAttributes: durAttrs)
        let durMidY = (strip.maxY + cover.maxY) / 2
        (durStr as NSString).draw(
            in: NSRect(x: cover.minX, y: durMidY - ds.height / 2,
                       width: cover.width, height: ds.height),
            withAttributes: durAttrs)

        // Wordmark + date, centered in the lower third below the keys.
        let foot = "MENU BAND · \(footDate(date))"
        let footAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 22, weight: .bold),
            .foregroundColor: NSColor.white.withAlphaComponent(0.9),
            .kern: 2,
            .paragraphStyle: para,
        ]
        let fs = (foot as NSString).size(withAttributes: footAttrs)
        let footMidY = (cover.minY + strip.minY) / 2
        (foot as NSString).draw(
            in: NSRect(x: cover.minX, y: footMidY - fs.height / 2,
                       width: cover.width, height: fs.height),
            withAttributes: footAttrs)

        NSGraphicsContext.restoreGraphicsState()

        // Subtle bright inner edge.
        NSColor.white.withAlphaComponent(0.14).setStroke()
        clip.lineWidth = 2
        clip.stroke()
    }

    /// A stylized piano-keys strip — the Menu Band identity motif. White keys
    /// with black keys on top; a couple of white keys lit warm for per-take
    /// variety (echoes the menubar's lit keys).
    private static func drawKeyboardStrip(in rect: NSRect) {
        let corner = rect.height * 0.16
        let bg = NSBezierPath(roundedRect: rect, xRadius: corner, yRadius: corner)

        // Ivory backing with a soft drop shadow so the strip floats.
        NSGraphicsContext.saveGraphicsState()
        let sh = NSShadow()
        sh.shadowColor = NSColor.black.withAlphaComponent(0.35)
        sh.shadowOffset = NSSize(width: 0, height: -6)
        sh.shadowBlurRadius = 16
        sh.set()
        NSColor(srgbRed: 0.96, green: 0.95, blue: 0.90, alpha: 1).setFill()
        bg.fill()
        NSGraphicsContext.restoreGraphicsState()

        NSGraphicsContext.saveGraphicsState()
        bg.addClip()
        let whiteCount = 7
        let kw = rect.width / CGFloat(whiteCount)
        var lit = Set<Int>()
        while lit.count < 2 { lit.insert(Int.random(in: 0..<whiteCount)) }
        for i in 0..<whiteCount {
            let kx = rect.minX + CGFloat(i) * kw
            if lit.contains(i) {
                NSColor(srgbRed: 1.0, green: 0.83, blue: 0.36, alpha: 0.92).setFill()
                NSRect(x: kx, y: rect.minY, width: kw, height: rect.height).fill()
            }
            if i > 0 {
                let sep = NSBezierPath()
                sep.move(to: NSPoint(x: kx, y: rect.minY))
                sep.line(to: NSPoint(x: kx, y: rect.maxY))
                sep.lineWidth = 2
                NSColor.black.withAlphaComponent(0.16).setStroke()
                sep.stroke()
            }
        }
        // Black keys between the appropriate whites (C-D-E-F-G-A-B pattern).
        let blackAfter = [0, 1, 3, 4, 5]
        let bkW = kw * 0.60, bkH = rect.height * 0.62
        NSColor(srgbRed: 0.10, green: 0.10, blue: 0.12, alpha: 1).setFill()
        for i in blackAfter where i < whiteCount - 1 {
            let cx = rect.minX + CGFloat(i + 1) * kw
            NSBezierPath(roundedRect: NSRect(x: cx - bkW / 2, y: rect.maxY - bkH,
                                             width: bkW, height: bkH),
                         xRadius: bkW * 0.16, yRadius: bkW * 0.16).fill()
        }
        NSGraphicsContext.restoreGraphicsState()
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
