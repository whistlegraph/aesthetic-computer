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
        case .salamander:
            return dark
                ? [0x594535, 0x765844, 0x807969, 0x88703A, 0xAD9254].map { rgb($0) }
                : [0xB39672, 0xC9AC84, 0xDED5BF, 0xC7A357, 0xE2C47A].map { rgb($0) }
        case .handPercussion:
            return dark
                ? [0x63382A, 0x875034, 0xA36943, 0x455F46, 0x947749].map { rgb($0) }
                : [0xAE6445, 0xD28A54, 0xE8B377, 0x7BA379, 0xC4A264].map { rgb($0) }
        case .cc:
            // Will's library: cool sample-pad greys with one hot ring.
            return dark
                ? [0x2E3440, 0x434C5E, 0x8F1D3A, 0x4C566A, 0x5E6B80].map { rgb($0) }
                : [0x8A93A6, 0xA7B0C2, 0xE04466, 0xC0C8D8, 0xD8DEE9].map { rgb($0) }
        }
    }

    static func chassis(for kit: Kit, dark: Bool) -> NSColor {
        switch kit {
        case .menuBand: return .clear
        case .electro: return dark ? rgb(0x0A0A14) : rgb(0x18182A)
        case .salamander: return rgb(0x332C25)
        case .handPercussion: return rgb(0x3F2A20)
        case .cc: return dark ? rgb(0x15171C) : rgb(0x23262E)
        }
    }

    // MARK: Drawing

    /// Draw a drum-machine kit's material inside `body` (already clipped by caller).
    static func drawMaterial(_ kit: Kit, in chart: NSRect, body: NSBezierPath,
                             dark: Bool) {
        guard MenuBandPercussion.spec(for: kit) != nil else { return }
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
        guard kit == .electro else { return }
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
    }
}
