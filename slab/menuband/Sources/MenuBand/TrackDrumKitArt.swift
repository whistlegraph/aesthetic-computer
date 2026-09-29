import AppKit

/// Pad artwork for the drum-machine kits. The Menu Band membrane keeps its
/// own earthy rings in TrackpadDrumSkinPad; Electro draws the same five
/// contours in neon over scanlines, so a kit change reads at a glance while
/// every zone stays where the sound puts it.
enum TrackDrumKitArt {
    typealias Kit = MenuBandPercussion.DrumKit

    // MARK: Palette

    private static func rgb(_ hex: UInt32, _ alpha: CGFloat = 1) -> NSColor {
        NSColor(srgbRed: CGFloat((hex >> 16) & 0xFF) / 255,
                green: CGFloat((hex >> 8) & 0xFF) / 255,
                blue: CGFloat(hex & 0xFF) / 255, alpha: alpha)
    }

    /// Fill per zone, kick (center) → click (rim).
    static func fills(for kit: Kit, dark: Bool) -> [NSColor] {
        switch kit {
        case .menuBand:
            return []
        case .electro:
            // 909 SIMMONS SNARE 606 BELL — neon from the center out.
            return dark
                ? [0x7A1466, 0x4B2390, 0x1F4FA8, 0x0F7C8C, 0x5F8A12].map { rgb($0) }
                : [0xE0409F, 0x8E5BE0, 0x4A86F0, 0x3CC6D4, 0xA6D63A].map { rgb($0) }
        }
    }

    static func chassis(for kit: Kit, dark: Bool) -> NSColor {
        switch kit {
        case .menuBand: return .clear
        case .electro: return dark ? rgb(0x0A0A14) : rgb(0x18182A)
        }
    }

    // MARK: Drawing

    /// Draw a drum-machine kit's material inside `body` (already clipped by caller).
    static func drawMaterial(_ kit: Kit, in chart: NSRect, body: NSBezierPath,
                             dark: Bool) {
        guard let spec = MenuBandPercussion.spec(for: kit) else { return }
        let fills = fills(for: kit, dark: dark)
        chassis(for: kit, dark: dark).setFill()
        body.fill()

        // Electro: the membrane's contours in neon over scanlines.
        let insets: [CGFloat] = [0, 4.8, 14.4, 21.6, 28]   // click → kick
        for (step, inset) in insets.enumerated() {
            let zone = NSBezierPath(
                roundedRect: chart.insetBy(dx: inset, dy: inset),
                xRadius: max(2, 8 - inset * 0.16), yRadius: max(2, 8 - inset * 0.16))
            // insets run outside-in; fills run kick-first.
            fills[fills.count - 1 - step].withAlphaComponent(0.85).setFill()
            zone.fill()
            rgb(0xFFFFFF, dark ? 0.35 : 0.5).setStroke()
            zone.lineWidth = 0.7
            zone.stroke()
        }
        NSGraphicsContext.saveGraphicsState()
        body.addClip()
        let lines = NSBezierPath()
        stride(from: chart.minY + 1, through: chart.maxY, by: 3).forEach { y in
            lines.move(to: NSPoint(x: chart.minX, y: y))
            lines.line(to: NSPoint(x: chart.maxX, y: y))
        }
        NSColor.black.withAlphaComponent(0.22).setStroke()
        lines.lineWidth = 0.6
        lines.stroke()
        NSGraphicsContext.restoreGraphicsState()
        label(spec.pads[0].name, at: NSPoint(x: chart.midX, y: chart.midY),
              on: fills[0])
        badge(kit.number, in: chart, on: chassis(for: kit, dark: dark))
    }

    private static func isLight(_ color: NSColor) -> Bool {
        guard let c = color.usingColorSpace(.sRGB) else { return false }
        return 0.299 * c.redComponent + 0.587 * c.greenComponent
            + 0.114 * c.blueComponent > 0.6
    }

    private static func label(_ text: String, at point: NSPoint, on fill: NSColor) {
        let ink = isLight(fill) ? NSColor.black.withAlphaComponent(0.72)
                                : NSColor.white.withAlphaComponent(0.88)
        let attributes: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 6, weight: .heavy),
            .foregroundColor: ink,
            .kern: 0.15,
        ]
        let string = NSAttributedString(string: text, attributes: attributes)
        let size = string.size()
        string.draw(at: NSPoint(x: point.x - size.width / 2, y: point.y - size.height / 2))
    }

    /// Kit number in the top-right corner, inside the rounded rim.
    static func badge(_ number: Int, in chart: NSRect, on chassis: NSColor) {
        let diameter: CGFloat = 10
        let rect = NSRect(x: chart.maxX - diameter - 3.5, y: chart.maxY - diameter - 3.5,
                          width: diameter, height: diameter)
        let disc = NSBezierPath(ovalIn: rect)
        NSColor.white.withAlphaComponent(0.92).setFill()
        disc.fill()
        chassis.setStroke()
        disc.lineWidth = 0.8
        disc.stroke()
        let string = NSAttributedString(string: "\(number)", attributes: [
            .font: NSFont.monospacedDigitSystemFont(ofSize: 7, weight: .heavy),
            .foregroundColor: NSColor.black,
        ])
        let size = string.size()
        string.draw(at: NSPoint(x: rect.midX - size.width / 2,
                                y: rect.midY - size.height / 2))
    }
}
