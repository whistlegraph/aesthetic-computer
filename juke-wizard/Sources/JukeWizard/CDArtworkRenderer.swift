import AppKit

/// Turns cover art into a printed compact disc: exposed dark edge, subtle
/// data rings, translucent plastic hub, a true transparent die-cut, and a
/// restrained specular sheen. The result is cached by callers before spin.
enum CDArtworkRenderer {
    private static func fan(center: NSPoint, radius: CGFloat,
                            startAngle: CGFloat, endAngle: CGFloat) -> NSBezierPath {
        let path = NSBezierPath()
        path.move(to: center)
        path.appendArc(withCenter: center, radius: radius,
                       startAngle: startAngle, endAngle: endAngle)
        path.close()
        return path
    }

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
        let outerPath = NSBezierPath(ovalIn: outer)
        let outline = accent.shadow(withLevel: 0.78) ?? .black
        outline.setFill()
        outerPath.fill()
        outline.withAlphaComponent(0.96).setStroke()
        outerPath.lineWidth = max(0.9, side * 0.052)
        outerPath.stroke()
        let face = outer.insetBy(dx: side * 0.018, dy: side * 0.018)
        let facePath = NSBezierPath(ovalIn: face)
        let hi = accent.highlight(withLevel: 0.48) ?? accent
        let lo = accent.shadow(withLevel: 0.34) ?? accent
        NSGradient(colors: [hi, accent, lo],
                   atLocations: [0, 0.48, 1], colorSpace: .sRGB)?.draw(in: facePath, angle: -55)

        NSGraphicsContext.current?.saveGraphicsState()
        facePath.addClip()
        // A couple of data rings catch the light without becoming noise at 21 pt.
        for (fraction, alpha) in [(0.88, 0.24), (0.66, 0.11)] {
            let inset = face.width * (1 - fraction) / 2
            NSColor.white.withAlphaComponent(alpha + 0.10).setStroke()
            let ring = NSBezierPath(ovalIn: face.insetBy(dx: inset, dy: inset))
            ring.lineWidth = max(0.5, side * 0.012)
            ring.stroke()
        }

        // CD gloss fans radiate from the hub, following the disc's circular tracks.
        // Layering a wide soft fan and a narrower bright fan gives the angular edges
        // a natural fade without doing any per-frame drawing while the disc spins.
        let center = NSPoint(x: face.midX, y: face.midY)
        let radius = face.width * 0.56
        NSColor.white.withAlphaComponent(0.15).setFill()
        let broadFan = fan(center: center, radius: radius, startAngle: 58, endAngle: 132)
        broadFan.fill()
        NSColor.white.withAlphaComponent(0.30).setStroke()
        broadFan.lineWidth = max(0.45, side * 0.021)
        broadFan.stroke()
        NSGradient(starting: NSColor.white.withAlphaComponent(0.08),
                   ending: NSColor.white.withAlphaComponent(0.53))?
            .draw(in: fan(center: center, radius: radius,
                          startAngle: 76, endAngle: 116), angle: 90)
        // A quiet counter-reflection keeps the accent disc monochromatic.
        NSColor.white.withAlphaComponent(0.09).setFill()
        let counterFan = fan(center: center, radius: radius, startAngle: 238, endAngle: 270)
        counterFan.fill()
        outline.withAlphaComponent(0.22).setStroke()
        counterFan.lineWidth = max(0.4, side * 0.018)
        counterFan.stroke()

        // Sharp outer glint sells the molded plastic at menu-bar scale.
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
        let hubPath = NSBezierPath(ovalIn: hub)
        NSColor.white.withAlphaComponent(0.38).setFill()
        hubPath.fill()
        outline.withAlphaComponent(0.82).setStroke()
        hubPath.lineWidth = max(0.75, side * 0.040)
        hubPath.stroke()
        NSColor.white.withAlphaComponent(0.88).setStroke()
        let hubEdge = NSBezierPath(ovalIn: hub.insetBy(dx: side * 0.008, dy: side * 0.008))
        hubEdge.lineWidth = max(0.5, side * 0.020)
        hubEdge.stroke()
        let holeSide = side * 0.078
        let hole = NSRect(x: side / 2 - holeSide / 2, y: side / 2 - holeSide / 2,
                          width: holeSide, height: holeSide)
        NSGraphicsContext.current?.compositingOperation = .clear
        NSBezierPath(ovalIn: hole).fill()
        NSGraphicsContext.current?.compositingOperation = .sourceOver
        outline.withAlphaComponent(0.92).setStroke()
        let holeEdge = NSBezierPath(ovalIn: hole.insetBy(dx: -side * 0.008, dy: -side * 0.008))
        holeEdge.lineWidth = max(0.7, side * 0.034)
        holeEdge.stroke()

        NSColor.white.withAlphaComponent(0.88).setStroke()
        let rim = NSBezierPath(ovalIn: outer.insetBy(dx: side * 0.006, dy: side * 0.006))
        rim.lineWidth = max(0.55, side * 0.024)
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
        let outerPath = NSBezierPath(ovalIn: outer)
        NSColor.black.withAlphaComponent(0.96).setFill()
        outerPath.fill()
        NSColor.black.withAlphaComponent(0.98).setStroke()
        outerPath.lineWidth = max(0.9, side * 0.052)
        outerPath.stroke()
        let face = outer.insetBy(dx: side * 0.012, dy: side * 0.012)
        NSGraphicsContext.current?.saveGraphicsState()
        NSBezierPath(ovalIn: face).addClip()
        let scale = max(face.width / max(1, art.size.width), face.height / max(1, art.size.height))
        let artSize = NSSize(width: art.size.width * scale, height: art.size.height * scale)
        art.draw(in: NSRect(x: face.midX - artSize.width / 2,
                           y: face.midY - artSize.height / 2,
                           width: artSize.width, height: artSize.height))

        // Thin data rings keep the cover visible while reading CD.
        for (fraction, color, width) in [
            (0.92, NSColor.white.withAlphaComponent(0.30), side * 0.008),
            (0.70, NSColor.white.withAlphaComponent(0.11), side * 0.005),
        ] {
            let inset = face.width * (1 - fraction) / 2
            color.setStroke()
            let ring = NSBezierPath(ovalIn: face.insetBy(dx: inset, dy: inset))
            ring.lineWidth = max(0.5, width * 1.45)
            ring.stroke()
        }

        // Classic hub-radiating plastic gloss. The broad white fan reads clearly at
        // menu-bar size; the narrow opposite spectral fan hints at CD diffraction.
        let center = NSPoint(x: face.midX, y: face.midY)
        let radius = face.width * 0.56
        NSColor.white.withAlphaComponent(0.12).setFill()
        let broadFan = fan(center: center, radius: radius, startAngle: 58, endAngle: 132)
        broadFan.fill()
        NSColor.white.withAlphaComponent(0.32).setStroke()
        broadFan.lineWidth = max(0.45, side * 0.021)
        broadFan.stroke()
        NSGradient(starting: NSColor.white.withAlphaComponent(0.05),
                   ending: NSColor.white.withAlphaComponent(0.36))?
            .draw(in: fan(center: center, radius: radius,
                          startAngle: 76, endAngle: 116), angle: 90)

        NSColor.systemCyan.withAlphaComponent(0.12).setFill()
        fan(center: center, radius: radius, startAngle: 236, endAngle: 248).fill()
        NSColor.systemPink.withAlphaComponent(0.10).setFill()
        fan(center: center, radius: radius, startAngle: 248, endAngle: 259).fill()
        NSColor.systemYellow.withAlphaComponent(0.09).setFill()
        let spectralEdge = fan(center: center, radius: radius, startAngle: 259, endAngle: 269)
        spectralEdge.fill()
        NSColor.black.withAlphaComponent(0.30).setStroke()
        let spectralFan = fan(center: center, radius: radius, startAngle: 236, endAngle: 269)
        spectralFan.lineWidth = max(0.4, side * 0.018)
        spectralFan.stroke()
        NSGraphicsContext.current?.restoreGraphicsState()

        // Clear polycarbonate hub + real die-cut center.
        let hubSide = side * 0.19
        let hub = NSRect(x: side / 2 - hubSide / 2, y: side / 2 - hubSide / 2,
                         width: hubSide, height: hubSide)
        let hubPath = NSBezierPath(ovalIn: hub)
        NSColor.white.withAlphaComponent(0.34).setFill()
        hubPath.fill()
        NSColor.black.withAlphaComponent(0.84).setStroke()
        hubPath.lineWidth = max(0.75, side * 0.040)
        hubPath.stroke()
        NSColor.white.withAlphaComponent(0.76).setStroke()
        let hubEdge = NSBezierPath(ovalIn: hub.insetBy(dx: side * 0.007, dy: side * 0.007))
        hubEdge.lineWidth = max(0.5, side * 0.020)
        hubEdge.stroke()
        let holeSide = side * 0.075
        let hole = NSRect(x: side / 2 - holeSide / 2, y: side / 2 - holeSide / 2,
                          width: holeSide, height: holeSide)
        NSGraphicsContext.current?.compositingOperation = .clear
        NSBezierPath(ovalIn: hole).fill()
        NSGraphicsContext.current?.compositingOperation = .sourceOver
        NSColor.black.withAlphaComponent(0.92).setStroke()
        let holeEdge = NSBezierPath(ovalIn: hole.insetBy(dx: -side * 0.005, dy: -side * 0.005))
        holeEdge.lineWidth = max(0.7, side * 0.034)
        holeEdge.stroke()

        NSColor.white.withAlphaComponent(0.76).setStroke()
        let rim = NSBezierPath(ovalIn: outer.insetBy(dx: side * 0.006, dy: side * 0.006))
        rim.lineWidth = max(0.55, side * 0.024)
        rim.stroke()
        image.unlockFocus()
        image.isTemplate = false
        return image
    }
}
