import AppKit
import CoreText

/// Loads + vends the bundled Bravura music font (SMuFL) so the
/// notation staff can draw real engraving glyphs — clefs, noteheads,
/// flags, rests, time-signature digits — instead of hand-rolled
/// bezier approximations. Bravura is the reference SMuFL font (SIL
/// OFL; see Resources/Bravura-OFL.txt) and is the same typeface
/// Verovio renders the exported PDF with, so the live staff and the
/// PDF read identically.
enum Bravura {
    /// SMuFL recommended glyph codepoints, by role. (The Private Use
    /// Area assignments are the SMuFL standard, stable across fonts.)
    enum Glyph: Character {
        case gClef            = "\u{E050}"   // treble clef
        case noteheadBlack    = "\u{E0A4}"   // filled head (quarter & shorter)
        case noteheadHalf     = "\u{E0A3}"   // open head + space for stem
        case noteheadWhole    = "\u{E0A2}"   // whole note (no stem)
        case flag8thUp        = "\u{E240}"
        case flag8thDown      = "\u{E241}"
        case flag16thUp       = "\u{E242}"
        case flag16thDown     = "\u{E243}"
        case accidentalSharp  = "\u{E262}"
        case accidentalFlat   = "\u{E260}"
        case accidentalNatural = "\u{E261}"
        case restWhole        = "\u{E4E3}"
        case restHalf         = "\u{E4E4}"
        case restQuarter      = "\u{E4E5}"
        case rest8th          = "\u{E4E6}"
        case rest16th         = "\u{E4E7}"
        case timeSig0         = "\u{E080}"   // 1..9 follow contiguously
        case timeSigCommon    = "\u{E08A}"   // 4/4 "C"
        case timeSigCut       = "\u{E08B}"   // cut time
    }

    /// Time-signature digit glyph for a single digit 0–9.
    static func timeSigDigit(_ d: Int) -> Character {
        let base = UnicodeScalar("\u{E080}").value
        let clamped = UInt32(max(0, min(9, d)))
        return Character(UnicodeScalar(base + clamped)!)
    }

    private static let fontName = "Bravura"
    private static var registered = false

    /// Register the bundled OTF with CoreText (once) so `NSFont(name:)`
    /// can find it even though it isn't installed system-wide.
    static func ensureRegistered() {
        guard !registered else { return }
        registered = true
        guard let url = Bundle.module.url(forResource: "Bravura",
                                          withExtension: "otf") else {
            NSLog("Bravura: Bravura.otf missing from bundle")
            return
        }
        var err: Unmanaged<CFError>?
        if !CTFontManagerRegisterFontsForURL(url as CFURL, .process, &err) {
            // Already-registered is fine (e.g. a prior popover open);
            // anything else we log but soldier on with the fallback.
            NSLog("Bravura: register failed: \(String(describing: err))")
        }
    }

    /// An `NSFont` of the music face at `size` points. Falls back to
    /// the system font if registration somehow failed, so callers
    /// never crash (they just draw tofu, which is visible in testing).
    static func font(ofSize size: CGFloat) -> NSFont {
        ensureRegistered()
        return NSFont(name: fontName, size: size)
            ?? NSFont.systemFont(ofSize: size)
    }

    /// Draw a single SMuFL glyph with its left edge at `origin.x` and
    /// its baseline at `origin.y`. Bravura's glyphs are designed on a
    /// "staff space = 0.25 em" metric, so a font whose point size is
    /// `4 * staffSpace` makes one staff space == `lineSpacing`.
    static func draw(_ glyph: Glyph, at origin: NSPoint,
                     staffSpace: CGFloat, color: NSColor) {
        draw(String(glyph.rawValue), at: origin,
             staffSpace: staffSpace, color: color)
    }

    static func draw(_ text: String, at origin: NSPoint,
                     staffSpace: CGFloat, color: NSColor) {
        let f = font(ofSize: staffSpace * 4)
        let attrs: [NSAttributedString.Key: Any] = [
            .font: f, .foregroundColor: color,
        ]
        NSAttributedString(string: text, attributes: attrs)
            .draw(at: NSPoint(x: origin.x, y: origin.y))
    }

    /// Advance width of a glyph string at the given staff-space size —
    /// used to place a notehead's stem flush to its right edge and to
    /// center time-signature digits.
    static func width(_ text: String, staffSpace: CGFloat) -> CGFloat {
        let f = font(ofSize: staffSpace * 4)
        return NSAttributedString(string: text, attributes: [.font: f])
            .size().width
    }

    /// Vertical offset from a glyph's baseline to the visual center of
    /// a notehead, so we can place a head centered on a staff line.
    /// Bravura noteheads sit ~0.5 staff-space above the baseline.
    static func noteheadCenterOffset(staffSpace: CGFloat) -> CGFloat {
        staffSpace * 0.5
    }
}
