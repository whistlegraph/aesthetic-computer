import AppKit

enum TrackDrumIcon {
    static func image(size: CGFloat = 1024,
                      accent: NSColor = .controlAccentColor) -> NSImage {
        let canvas = NSSize(width: size, height: size)
        return NSImage(size: canvas, flipped: false) { rect in
            draw(in: rect, accent: accent)
            return true
        }
    }

    private static func draw(in rect: NSRect, accent: NSColor) {
        let scale = min(rect.width, rect.height) / 1024
        func s(_ value: CGFloat) -> CGFloat { value * scale }
        let accent = accent.usingColorSpace(.deviceRGB) ?? .systemPink

        NSColor.clear.setFill()
        rect.fill()

        func path(_ box: NSRect, radius: CGFloat) -> NSBezierPath {
            NSBezierPath(roundedRect: box, xRadius: s(radius), yRadius: s(radius))
        }
        func shifted(_ box: NSRect, y: CGFloat) -> NSRect {
            box.offsetBy(dx: 0, dy: s(y))
        }
        func fillGradient(_ shape: NSBezierPath, in box: NSRect,
                          top: NSColor, bottom: NSColor) {
            NSGraphicsContext.saveGraphicsState()
            shape.addClip()
            NSGradient(starting: top, ending: bottom)?.draw(in: box, angle: -90)
            NSGraphicsContext.restoreGraphicsState()
        }
        func raised(_ shape: NSBezierPath, in box: NSRect,
                    top: NSColor, bottom: NSColor,
                    shadowY: CGFloat = -10, blur: CGFloat = 14,
                    edgeWidth: CGFloat = 7) {
            NSGraphicsContext.saveGraphicsState()
            let shadow = NSShadow()
            shadow.shadowColor = NSColor.black.withAlphaComponent(0.48)
            shadow.shadowBlurRadius = s(blur)
            shadow.shadowOffset = NSSize(width: 0, height: s(shadowY))
            shadow.set()
            bottom.setFill()
            shape.fill()
            NSGraphicsContext.restoreGraphicsState()
            fillGradient(shape, in: box, top: top, bottom: bottom)
            NSColor.black.withAlphaComponent(0.48).setStroke()
            shape.lineWidth = s(edgeWidth)
            shape.stroke()
            NSColor.white.withAlphaComponent(0.18).setStroke()
            shape.lineWidth = s(2.5)
            shape.stroke()
        }

        // A physical slab seen slightly from above. The lower copy is the
        // chassis thickness; the raised material zones preserve TrackDrum's
        // exact outside-to-center instrument map in an icon-shaped footprint.
        let pad = NSRect(x: rect.minX + s(46), y: rect.minY + s(72),
                         width: s(932), height: s(884))
        let body = path(pad, radius: 210)
        let bodyBase = path(shifted(pad, y: -36), radius: 210)
        let accentBase = accent.blended(withFraction: 0.58, of: .black)
            ?? NSColor(srgbRed: 0.20, green: 0.04, blue: 0.16, alpha: 1)

        NSGraphicsContext.saveGraphicsState()
        let bodyShadow = NSShadow()
        bodyShadow.shadowColor = NSColor.black.withAlphaComponent(0.62)
        bodyShadow.shadowBlurRadius = s(38)
        bodyShadow.shadowOffset = NSSize(width: 0, height: -s(24))
        bodyShadow.set()
        accentBase.setFill()
        bodyBase.fill()
        NSGraphicsContext.restoreGraphicsState()
        accentBase.setFill()
        bodyBase.fill()

        let creamTop = NSColor(srgbRed: 1.00, green: 0.96, blue: 0.83, alpha: 1)
        let creamBottom = NSColor(srgbRed: 0.83, green: 0.72, blue: 0.52, alpha: 1)
        raised(body, in: pad, top: creamTop, bottom: creamBottom,
               shadowY: -8, blur: 12, edgeWidth: 8)

        func zone(_ inset: CGFloat, radius: CGFloat) -> (NSRect, NSBezierPath) {
            let box = pad.insetBy(dx: s(inset), dy: s(inset))
            return (box, path(box, radius: radius))
        }

        let (hatBox, hat) = zone(58, radius: 166)
        let (wireBox, snareWire) = zone(176, radius: 116)
        let (tomBox, tom) = zone(262, radius: 80)
        let (kickBox, kick) = zone(338, radius: 50)
        let sageTop = NSColor(srgbRed: 0.82, green: 0.87, blue: 0.70, alpha: 1)
        let sageBottom = NSColor(srgbRed: 0.49, green: 0.57, blue: 0.38, alpha: 1)
        let terraTop = NSColor(srgbRed: 0.91, green: 0.59, blue: 0.40, alpha: 1)
        let terraBottom = NSColor(srgbRed: 0.55, green: 0.27, blue: 0.18, alpha: 1)
        let ochreTop = NSColor(srgbRed: 0.95, green: 0.79, blue: 0.49, alpha: 1)
        let ochreBottom = NSColor(srgbRed: 0.63, green: 0.43, blue: 0.20, alpha: 1)
        let umberTop = NSColor(srgbRed: 0.59, green: 0.36, blue: 0.24, alpha: 1)
        let umberBottom = NSColor(srgbRed: 0.24, green: 0.12, blue: 0.08, alpha: 1)
        raised(hat, in: hatBox, top: sageTop, bottom: sageBottom)
        raised(snareWire, in: wireBox, top: terraTop, bottom: terraBottom)

        NSGraphicsContext.saveGraphicsState()
        snareWire.addClip()
        let wires = NSBezierPath()
        stride(from: pad.minX - pad.height,
               through: pad.maxX, by: s(28)).forEach { x in
            wires.move(to: NSPoint(x: x, y: pad.minY))
            wires.line(to: NSPoint(x: x + pad.height, y: pad.maxY))
        }
        NSColor.black.withAlphaComponent(0.14).setStroke()
        wires.lineWidth = s(4)
        wires.stroke()
        NSGraphicsContext.restoreGraphicsState()

        raised(tom, in: tomBox, top: ochreTop, bottom: ochreBottom,
               shadowY: -8, blur: 12)
        raised(kick, in: kickBox, top: umberTop, bottom: umberBottom,
               shadowY: -7, blur: 10)

        // Two restrained registration points make this read as the live
        // touch surface, not a cassette, even at Dock size.
        let touches = [
            NSPoint(x: pad.minX + pad.width * 0.36,
                    y: pad.minY + pad.height * 0.68),
            NSPoint(x: pad.minX + pad.width * 0.65,
                    y: pad.minY + pad.height * 0.36),
        ]
        let tether = NSBezierPath()
        tether.move(to: touches[0])
        tether.line(to: touches[1])
        NSGraphicsContext.saveGraphicsState()
        let tetherShadow = NSShadow()
        tetherShadow.shadowColor = NSColor.black.withAlphaComponent(0.55)
        tetherShadow.shadowBlurRadius = s(8)
        tetherShadow.shadowOffset = NSSize(width: 0, height: -s(5))
        tetherShadow.set()
        accent.withAlphaComponent(0.64).setStroke()
        tether.lineWidth = s(12)
        tether.stroke()
        NSGraphicsContext.restoreGraphicsState()
        for point in touches {
            let radius = s(20)
            let dot = NSBezierPath(ovalIn: NSRect(
                x: point.x - radius, y: point.y - radius,
                width: radius * 2, height: radius * 2
            ))
            NSGraphicsContext.saveGraphicsState()
            let glow = NSShadow()
            glow.shadowColor = accent.withAlphaComponent(0.80)
            glow.shadowBlurRadius = s(12)
            glow.shadowOffset = .zero
            glow.set()
            NSColor.white.withAlphaComponent(0.98).setFill()
            dot.fill()
            NSGraphicsContext.restoreGraphicsState()
            accent.setStroke()
            dot.lineWidth = s(5)
            dot.stroke()
        }
    }
}
