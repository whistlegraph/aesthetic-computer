import AppKit

// The WaveWizard mascot — pointy purple hat (with stars), round beige
// face with simple eyes, white beard, wand with a star tip. The wand
// star glows yellow while recording, cyan while listening, purple idle.
final class MascotView: NSView {
    enum Mood { case idle, listening, recording, success, error }
    var mood: Mood = .idle { didSet { needsDisplay = true } }

    override var wantsDefaultClipping: Bool { false }

    override func draw(_ dirtyRect: NSRect) {
        guard let ctx = NSGraphicsContext.current?.cgContext else { return }
        let w = bounds.width, h = bounds.height

        // ── Hat — pointy purple triangle, with brim
        let hatColor = NSColor(calibratedHue: 0.76, saturation: 0.72, brightness: 0.55, alpha: 1)
        let brimColor = NSColor(calibratedHue: 0.76, saturation: 0.55, brightness: 0.32, alpha: 1)
        let hat = NSBezierPath()
        hat.move(to: NSPoint(x: w * 0.50, y: h * 0.97))
        hat.line(to: NSPoint(x: w * 0.22, y: h * 0.58))
        hat.line(to: NSPoint(x: w * 0.78, y: h * 0.58))
        hat.close()
        hatColor.setFill(); hat.fill()
        let brim = NSBezierPath(roundedRect: NSRect(x: w * 0.13, y: h * 0.52, width: w * 0.74, height: h * 0.06), xRadius: 3, yRadius: 3)
        brimColor.setFill(); brim.fill()
        // hat sparkles
        NSColor.white.withAlphaComponent(0.85).setFill()
        for (cx, cy, r) in [(0.43, 0.80, 2.5), (0.56, 0.72, 2.0), (0.40, 0.66, 1.5), (0.58, 0.85, 1.8)] {
            let s = NSBezierPath(ovalIn: NSRect(x: w * CGFloat(cx) - CGFloat(r), y: h * CGFloat(cy) - CGFloat(r), width: CGFloat(r) * 2, height: CGFloat(r) * 2))
            s.fill()
        }

        // ── Face — round beige
        let faceRect = NSRect(x: w * 0.27, y: h * 0.18, width: w * 0.46, height: h * 0.36)
        NSColor(calibratedRed: 0.96, green: 0.83, blue: 0.66, alpha: 1).setFill()
        NSBezierPath(ovalIn: faceRect).fill()

        // ── Eyes (taller when recording — the wizard is "watching")
        NSColor.black.setFill()
        let eyeH: CGFloat = (mood == .recording) ? 6 : 4
        let eyeY: CGFloat = h * 0.40
        NSBezierPath(ovalIn: NSRect(x: w * 0.40, y: eyeY, width: 4, height: eyeH)).fill()
        NSBezierPath(ovalIn: NSRect(x: w * 0.56, y: eyeY, width: 4, height: eyeH)).fill()
        // cheek glow when success
        if mood == .success {
            NSColor.systemPink.withAlphaComponent(0.35).setFill()
            NSBezierPath(ovalIn: NSRect(x: w * 0.34, y: h * 0.32, width: 8, height: 5)).fill()
            NSBezierPath(ovalIn: NSRect(x: w * 0.58, y: h * 0.32, width: 8, height: 5)).fill()
        }

        // ── Beard — white, hanging from below the face
        let beard = NSBezierPath()
        beard.move(to: NSPoint(x: w * 0.30, y: h * 0.26))
        beard.curve(to: NSPoint(x: w * 0.50, y: h * 0.02),
                    controlPoint1: NSPoint(x: w * 0.32, y: h * 0.08),
                    controlPoint2: NSPoint(x: w * 0.45, y: -h * 0.01))
        beard.curve(to: NSPoint(x: w * 0.70, y: h * 0.26),
                    controlPoint1: NSPoint(x: w * 0.55, y: -h * 0.01),
                    controlPoint2: NSPoint(x: w * 0.68, y: h * 0.08))
        beard.curve(to: NSPoint(x: w * 0.30, y: h * 0.26),
                    controlPoint1: NSPoint(x: w * 0.60, y: h * 0.30),
                    controlPoint2: NSPoint(x: w * 0.40, y: h * 0.30))
        beard.close()
        NSColor(white: 0.96, alpha: 1).setFill()
        beard.fill()
        NSColor(white: 0.75, alpha: 0.5).setStroke()
        beard.lineWidth = 1
        beard.stroke()

        // ── Wand — tilted to upper-right with a star at the tip
        ctx.saveGState()
        ctx.translateBy(x: w * 0.78, y: h * 0.32)
        ctx.rotate(by: -0.45)
        // staff
        let staff = NSBezierPath()
        staff.move(to: .zero)
        staff.line(to: NSPoint(x: 0, y: h * 0.30))
        staff.lineWidth = 3
        NSColor(calibratedRed: 0.42, green: 0.26, blue: 0.12, alpha: 1).setStroke()
        staff.stroke()
        // star tip
        let starColor: NSColor
        switch mood {
        case .recording: starColor = .systemYellow
        case .listening: starColor = .systemCyan
        case .success:   starColor = .systemGreen
        case .error:     starColor = .systemRed
        case .idle:      starColor = NSColor(calibratedHue: 0.76, saturation: 0.6, brightness: 0.85, alpha: 1)
        }
        // glow halo
        if mood == .recording || mood == .listening || mood == .success {
            starColor.withAlphaComponent(0.30).setFill()
            NSBezierPath(ovalIn: NSRect(x: -16, y: h * 0.30 - 16, width: 32, height: 32)).fill()
        }
        starColor.setFill()
        starPath(center: NSPoint(x: 0, y: h * 0.32), radius: 9, points: 5).fill()
        ctx.restoreGState()
    }

    private func starPath(center c: NSPoint, radius r: CGFloat, points: Int) -> NSBezierPath {
        let p = NSBezierPath()
        let pi = CGFloat.pi
        for i in 0..<(points * 2) {
            let angle = -pi / 2 + pi * CGFloat(i) / CGFloat(points)
            let rr = i % 2 == 0 ? r : r * 0.45
            let x = c.x + cos(angle) * rr
            let y = c.y + sin(angle) * rr
            if i == 0 { p.move(to: NSPoint(x: x, y: y)) }
            else      { p.line(to: NSPoint(x: x, y: y)) }
        }
        p.close()
        return p
    }
}
