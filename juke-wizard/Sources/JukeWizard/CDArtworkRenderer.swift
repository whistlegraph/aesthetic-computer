import AppKit

/// Turns cover art into a printed compact disc: exposed dark edge, subtle
/// data rings, translucent plastic hub, a true transparent die-cut, and a
/// restrained specular sheen. The result is cached by callers before spin.
enum CDArtworkRenderer {
    /// Default disc when no cover art exists: the user's live macOS accent
    /// color molded into glossy translucent polycarbonate.
    static func accentDisc(side: CGFloat) -> NSImage {
        let image = NSImage(size: NSSize(width: side, height: side))
        image.lockFocus()
        NSGraphicsContext.current?.imageInterpolation = .high
        let accent = NSColor.controlAccentColor.usingColorSpace(.sRGB) ?? .controlAccentColor
        let outer = NSRect(x: side * 0.025, y: side * 0.025,
                           width: side * 0.95, height: side * 0.95)

        // Dark molded edge, then a rounded accent face with directional depth.
        (accent.shadow(withLevel: 0.58) ?? .black).setFill()
        NSBezierPath(ovalIn: outer).fill()
        let face = outer.insetBy(dx: side * 0.018, dy: side * 0.018)
        let facePath = NSBezierPath(ovalIn: face)
        let hi = accent.highlight(withLevel: 0.48) ?? accent
        let lo = accent.shadow(withLevel: 0.34) ?? accent
        NSGradient(colors: [hi, accent, lo],
                   atLocations: [0, 0.48, 1], colorSpace: .sRGB)?.draw(in: facePath, angle: -55)

        NSGraphicsContext.current?.saveGraphicsState()
        facePath.addClip()
        // Data rings catch the light without introducing another hue.
        for (fraction, alpha) in [(0.88, 0.24), (0.71, 0.15), (0.54, 0.10)] {
            let inset = face.width * (1 - fraction) / 2
            NSColor.white.withAlphaComponent(alpha).setStroke()
            let ring = NSBezierPath(ovalIn: face.insetBy(dx: inset, dy: inset))
            ring.lineWidth = max(0.35, side * 0.006)
            ring.stroke()
        }
        // Broad plastic streak plus a sharp top-left specular glint.
        let sheen = NSBezierPath()
        sheen.move(to: NSPoint(x: face.minX + face.width * 0.03, y: face.maxY * 0.84))
        sheen.line(to: NSPoint(x: face.minX + face.width * 0.37, y: face.maxY))
        sheen.line(to: NSPoint(x: face.maxX, y: face.minY + face.height * 0.36))
        sheen.line(to: NSPoint(x: face.maxX, y: face.minY + face.height * 0.13))
        sheen.close()
        NSGradient(starting: NSColor.white.withAlphaComponent(0.48),
                   ending: NSColor.white.withAlphaComponent(0.02))?.draw(in: sheen, angle: -38)
        NSColor.white.withAlphaComponent(0.82).setStroke()
        let glint = NSBezierPath()
        glint.move(to: NSPoint(x: face.minX + face.width * 0.18, y: face.maxY - side * 0.06))
        glint.curve(to: NSPoint(x: face.minX + face.width * 0.50, y: face.maxY - side * 0.015),
                    controlPoint1: NSPoint(x: face.minX + face.width * 0.27, y: face.maxY),
                    controlPoint2: NSPoint(x: face.minX + face.width * 0.41, y: face.maxY))
        glint.lineWidth = max(0.45, side * 0.018)
        glint.stroke()
        NSGraphicsContext.current?.restoreGraphicsState()

        // Clear raised hub and a true transparent die-cut center.
        let hubSide = side * 0.21
        let hub = NSRect(x: side / 2 - hubSide / 2, y: side / 2 - hubSide / 2,
                         width: hubSide, height: hubSide)
        NSColor.white.withAlphaComponent(0.34).setFill()
        NSBezierPath(ovalIn: hub).fill()
        NSColor.white.withAlphaComponent(0.58).setStroke()
        let hubEdge = NSBezierPath(ovalIn: hub.insetBy(dx: side * 0.008, dy: side * 0.008))
        hubEdge.lineWidth = max(0.4, side * 0.007)
        hubEdge.stroke()
        let holeSide = side * 0.078
        let hole = NSRect(x: side / 2 - holeSide / 2, y: side / 2 - holeSide / 2,
                          width: holeSide, height: holeSide)
        NSGraphicsContext.current?.compositingOperation = .clear
        NSBezierPath(ovalIn: hole).fill()
        NSGraphicsContext.current?.compositingOperation = .sourceOver

        NSColor.white.withAlphaComponent(0.66).setStroke()
        let rim = NSBezierPath(ovalIn: outer.insetBy(dx: side * 0.006, dy: side * 0.006))
        rim.lineWidth = max(0.45, side * 0.009)
        rim.stroke()
        image.unlockFocus()
        image.isTemplate = false
        return image
    }

    static func disc(from art: NSImage, side: CGFloat, shadow: Bool = false) -> NSImage {
        let image = NSImage(size: NSSize(width: side, height: side))
        image.lockFocus()
        NSGraphicsContext.current?.imageInterpolation = .high
        let pad = side * (shadow ? 0.055 : 0.025)
        let outer = NSRect(x: pad, y: pad, width: side - pad * 2, height: side - pad * 2)

        if shadow {
            NSGraphicsContext.current?.saveGraphicsState()
            let s = NSShadow()
            s.shadowBlurRadius = side * 0.035
            s.shadowOffset = NSSize(width: 0, height: -side * 0.012)
            s.shadowColor = NSColor.black.withAlphaComponent(0.48)
            s.set()
            NSColor.black.setFill()
            NSBezierPath(ovalIn: outer).fill()
            NSGraphicsContext.current?.restoreGraphicsState()
        }

        // Black polycarbonate edge, then cover art printed just inside it.
        NSColor.black.withAlphaComponent(0.92).setFill()
        NSBezierPath(ovalIn: outer).fill()
        let face = outer.insetBy(dx: side * 0.012, dy: side * 0.012)
        NSGraphicsContext.current?.saveGraphicsState()
        NSBezierPath(ovalIn: face).addClip()
        let scale = max(face.width / max(1, art.size.width), face.height / max(1, art.size.height))
        let artSize = NSSize(width: art.size.width * scale, height: art.size.height * scale)
        art.draw(in: NSRect(x: face.midX - artSize.width / 2,
                           y: face.midY - artSize.height / 2,
                           width: artSize.width, height: artSize.height))

        // Thin iridescent data rings keep the cover visible while reading CD.
        for (fraction, color, width) in [
            (0.92, NSColor.white.withAlphaComponent(0.30), side * 0.008),
            (0.80, NSColor.systemCyan.withAlphaComponent(0.16), side * 0.006),
            (0.68, NSColor.systemPink.withAlphaComponent(0.13), side * 0.005),
        ] {
            let inset = face.width * (1 - fraction) / 2
            color.setStroke()
            let ring = NSBezierPath(ovalIn: face.insetBy(dx: inset, dy: inset))
            ring.lineWidth = max(0.35, width)
            ring.stroke()
        }

        // A diagonal plastic sheen, deliberately translucent and quiet.
        let sheen = NSBezierPath()
        sheen.move(to: NSPoint(x: face.minX + face.width * 0.10, y: face.maxY))
        sheen.line(to: NSPoint(x: face.minX + face.width * 0.39, y: face.maxY))
        sheen.line(to: NSPoint(x: face.maxX, y: face.minY + face.height * 0.27))
        sheen.line(to: NSPoint(x: face.maxX, y: face.minY + face.height * 0.05))
        sheen.close()
        NSGradient(starting: NSColor.white.withAlphaComponent(0.30),
                   ending: NSColor.white.withAlphaComponent(0.015))?.draw(in: sheen, angle: -35)
        NSGraphicsContext.current?.restoreGraphicsState()

        // Clear polycarbonate hub + real die-cut center.
        let hubSide = side * 0.19
        let hub = NSRect(x: side / 2 - hubSide / 2, y: side / 2 - hubSide / 2,
                         width: hubSide, height: hubSide)
        NSColor.white.withAlphaComponent(0.28).setFill()
        NSBezierPath(ovalIn: hub).fill()
        NSColor.black.withAlphaComponent(0.36).setStroke()
        let hubEdge = NSBezierPath(ovalIn: hub.insetBy(dx: side * 0.007, dy: side * 0.007))
        hubEdge.lineWidth = max(0.45, side * 0.006)
        hubEdge.stroke()
        let holeSide = side * 0.075
        let hole = NSRect(x: side / 2 - holeSide / 2, y: side / 2 - holeSide / 2,
                          width: holeSide, height: holeSide)
        NSGraphicsContext.current?.compositingOperation = .clear
        NSBezierPath(ovalIn: hole).fill()
        NSGraphicsContext.current?.compositingOperation = .sourceOver
        NSColor.white.withAlphaComponent(0.68).setStroke()
        let holeEdge = NSBezierPath(ovalIn: hole.insetBy(dx: -side * 0.005, dy: -side * 0.005))
        holeEdge.lineWidth = max(0.45, side * 0.007)
        holeEdge.stroke()

        NSColor.white.withAlphaComponent(0.50).setStroke()
        let rim = NSBezierPath(ovalIn: outer.insetBy(dx: side * 0.006, dy: side * 0.006))
        rim.lineWidth = max(0.45, side * 0.008)
        rim.stroke()
        image.unlockFocus()
        image.isTemplate = false
        return image
    }
}
