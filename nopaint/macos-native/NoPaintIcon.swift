import AppKit

enum NoPaintIcon {
    static func image(size: CGFloat = 1024) -> NSImage {
        NSImage(size: NSSize(width: size, height: size), flipped: false) { rect in
            draw(in: rect)
            return true
        }
    }

    private static func draw(in rect: NSRect) {
        let scale = min(rect.width, rect.height) / 1024
        func s(_ value: CGFloat) -> CGFloat { value * scale }
        func rounded(_ box: NSRect, _ radius: CGFloat) -> NSBezierPath {
            NSBezierPath(roundedRect: box, xRadius: s(radius), yRadius: s(radius))
        }
        func gradient(_ shape: NSBezierPath, box: NSRect, top: NSColor, bottom: NSColor) {
            NSGraphicsContext.saveGraphicsState()
            shape.addClip()
            NSGradient(starting: top, ending: bottom)?.draw(in: box, angle: -90)
            NSGraphicsContext.restoreGraphicsState()
        }
        func raised(_ shape: NSBezierPath, box: NSRect, top: NSColor, bottom: NSColor,
                    shadowY: CGFloat = -12, blur: CGFloat = 18) {
            NSGraphicsContext.saveGraphicsState()
            let shadow = NSShadow()
            shadow.shadowColor = NSColor.black.withAlphaComponent(0.50)
            shadow.shadowBlurRadius = s(blur)
            shadow.shadowOffset = NSSize(width: 0, height: s(shadowY))
            shadow.set()
            bottom.setFill()
            shape.fill()
            NSGraphicsContext.restoreGraphicsState()
            gradient(shape, box: box, top: top, bottom: bottom)
            NSColor.black.withAlphaComponent(0.48).setStroke()
            shape.lineWidth = s(7)
            shape.stroke()
            NSColor.white.withAlphaComponent(0.18).setStroke()
            shape.lineWidth = s(2.5)
            shape.stroke()
        }

        NSColor.clear.setFill()
        rect.fill()

        let bodyBox = NSRect(x: s(48), y: s(74), width: s(928), height: s(882))
        let body = rounded(bodyBox, 206)
        let baseBox = bodyBox.offsetBy(dx: 0, dy: -s(38))
        let base = rounded(baseBox, 206)
        let baseColor = NSColor(srgbRed: 0.26, green: 0.08, blue: 0.16, alpha: 1)
        NSGraphicsContext.saveGraphicsState()
        let bodyShadow = NSShadow()
        bodyShadow.shadowColor = NSColor.black.withAlphaComponent(0.64)
        bodyShadow.shadowBlurRadius = s(38)
        bodyShadow.shadowOffset = NSSize(width: 0, height: -s(24))
        bodyShadow.set()
        baseColor.setFill()
        base.fill()
        NSGraphicsContext.restoreGraphicsState()
        baseColor.setFill()
        base.fill()
        raised(body, box: bodyBox,
               top: NSColor(srgbRed: 1.00, green: 0.96, blue: 0.83, alpha: 1),
               bottom: NSColor(srgbRed: 0.82, green: 0.71, blue: 0.51, alpha: 1))

        let faceBox = bodyBox.insetBy(dx: s(58), dy: s(58))
        let face = rounded(faceBox, 154)
        raised(face, box: faceBox,
               top: NSColor(srgbRed: 0.20, green: 0.19, blue: 0.23, alpha: 1),
               bottom: NSColor(srgbRed: 0.07, green: 0.06, blue: 0.09, alpha: 1),
               shadowY: -8, blur: 12)

        let canvasBox = NSRect(x: s(132), y: s(354), width: s(760), height: s(470))
        let canvas = rounded(canvasBox, 54)
        raised(canvas, box: canvasBox,
               top: NSColor(srgbRed: 0.99, green: 0.94, blue: 0.77, alpha: 1),
               bottom: NSColor(srgbRed: 0.77, green: 0.66, blue: 0.45, alpha: 1),
               shadowY: -8, blur: 10)

        NSGraphicsContext.saveGraphicsState()
        canvas.addClip()
        let field = NSGradient(colors: [
            NSColor(srgbRed: 0.99, green: 0.43, blue: 0.54, alpha: 1),
            NSColor(srgbRed: 0.99, green: 0.75, blue: 0.22, alpha: 1),
            NSColor(srgbRed: 0.22, green: 0.78, blue: 0.72, alpha: 1),
        ])
        field?.draw(in: canvasBox, angle: -18)

        let strokes: [(NSColor, NSPoint, NSPoint, CGFloat)] = [
            (.white.withAlphaComponent(0.82), NSPoint(x: s(176), y: s(700)), NSPoint(x: s(742), y: s(456)), 50),
            (NSColor(srgbRed: 0.18, green: 0.12, blue: 0.35, alpha: 0.72), NSPoint(x: s(274), y: s(430)), NSPoint(x: s(792), y: s(700)), 38),
            (NSColor(srgbRed: 1.00, green: 0.91, blue: 0.18, alpha: 0.90), NSPoint(x: s(214), y: s(568)), NSPoint(x: s(666), y: s(650)), 24),
        ]
        for (color, start, end, width) in strokes {
            let line = NSBezierPath()
            line.move(to: start)
            line.curve(to: end,
                       controlPoint1: NSPoint(x: start.x + s(180), y: start.y - s(110)),
                       controlPoint2: NSPoint(x: end.x - s(190), y: end.y + s(90)))
            line.lineCapStyle = .round
            line.lineWidth = s(width)
            color.setStroke()
            line.stroke()
        }
        NSGraphicsContext.restoreGraphicsState()

        let noBox = NSRect(x: s(132), y: s(168), width: s(284), height: s(132))
        let paintBox = NSRect(x: s(440), y: s(168), width: s(452), height: s(132))
        drawButton(noBox, radius: s(42),
                   top: NSColor(srgbRed: 1.00, green: 0.34, blue: 0.36, alpha: 1),
                   bottom: NSColor(srgbRed: 0.68, green: 0.07, blue: 0.12, alpha: 1),
                   label: "No", fontSize: s(59))
        drawButton(paintBox, radius: s(42),
                   top: NSColor(srgbRed: 0.36, green: 0.90, blue: 0.38, alpha: 1),
                   bottom: NSColor(srgbRed: 0.05, green: 0.48, blue: 0.15, alpha: 1),
                   label: "Paint", fontSize: s(54))
    }

    private static func drawButton(_ box: NSRect, radius: CGFloat, top: NSColor,
                                   bottom: NSColor, label: String, fontSize: CGFloat) {
        let base = NSBezierPath(roundedRect: box.offsetBy(dx: 0, dy: -box.height * 0.11),
                                xRadius: radius, yRadius: radius)
        bottom.blended(withFraction: 0.45, of: .black)?.setFill()
        base.fill()

        let face = NSBezierPath(roundedRect: box, xRadius: radius, yRadius: radius)
        NSGraphicsContext.saveGraphicsState()
        let shadow = NSShadow()
        shadow.shadowColor = .black.withAlphaComponent(0.48)
        shadow.shadowBlurRadius = box.height * 0.09
        shadow.shadowOffset = NSSize(width: 0, height: -box.height * 0.07)
        shadow.set()
        bottom.setFill()
        face.fill()
        NSGraphicsContext.restoreGraphicsState()
        NSGraphicsContext.saveGraphicsState()
        face.addClip()
        NSGradient(starting: top, ending: bottom)?.draw(in: box, angle: -90)
        NSGraphicsContext.restoreGraphicsState()
        NSColor.black.withAlphaComponent(0.50).setStroke()
        face.lineWidth = box.height * 0.045
        face.stroke()

        let paragraph = NSMutableParagraphStyle()
        paragraph.alignment = .center
        let attributes: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: fontSize, weight: .heavy),
            .foregroundColor: NSColor.white,
            .strokeColor: NSColor.black.withAlphaComponent(0.25),
            .strokeWidth: -2.5,
            .paragraphStyle: paragraph,
        ]
        let size = label.size(withAttributes: attributes)
        label.draw(in: NSRect(x: box.minX, y: box.midY - size.height * 0.54,
                              width: box.width, height: size.height * 1.2),
                   withAttributes: attributes)
    }
}
