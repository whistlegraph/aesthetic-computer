import AppKit

/// Swift port of the notepat per-note ROYGBIV palette
/// (system/public/aesthetic.computer/lib/note-colors.mjs). Sharps and
/// flats are intentionally black so the chord readout matches the look
/// of a real notepat strip — naturals carry the chromatic identity,
/// accidentals read as the gaps between them.
enum NoteColors {
    private static let base: [Character: NSColor] = [
        "c": rgb(255, 50,  50),
        "d": rgb(255, 160, 0),
        "e": rgb(255, 230, 0),
        "f": rgb(50,  200, 50),
        "g": rgb(50,  120, 255),
        "a": rgb(130, 50,  200),
        "b": rgb(180, 80,  255),
    ]
    private static let dayglo: [Character: NSColor] = [
        "c": rgb(255, 40,  80),
        "d": rgb(255, 180, 0),
        "e": rgb(255, 255, 50),
        "f": rgb(50,  255, 100),
        "g": rgb(50,  200, 255),
        "a": rgb(180, 50,  255),
        "b": rgb(255, 80,  255),
    ]
    private static let muted: [Character: NSColor] = [
        "c": rgb(139, 26,  26),
        "d": rgb(180, 100, 0),
        "e": rgb(180, 150, 0),
        "f": rgb(20,  90,  20),
        "g": rgb(20,  60,  120),
        "a": rgb(50,  0,   90),
        "b": rgb(90,  30,  150),
    ]
    private static let black = NSColor.black

    /// Pitch-class → letter (natural) + sharp flag. Pitch classes use
    /// 0=C..11=B. Sharps are merged into the natural to the left
    /// (e.g., C# → "c" + sharp), matching how notepat treats them.
    private static let letters: [Character] = [
        "c","c","d","d","e","f","f","g","g","a","a","b",
    ]
    private static let isSharp: [Bool] = [
        false, true, false, true, false, false, true, false, true, false, true, false,
    ]

    /// Color for a pitch class at the given octave (4 = base octave).
    /// Sharps return black regardless of octave.
    static func color(pitchClass: Int, octave: Int = 4) -> NSColor {
        let pc = ((pitchClass % 12) + 12) % 12
        if isSharp[pc] { return black }
        let letter = letters[pc]
        let delta = octave - 4
        if delta >= 1 {
            return dayglo[letter] ?? base[letter] ?? .white
        } else if delta <= -1 {
            return muted[letter] ?? base[letter] ?? .white
        } else {
            return base[letter] ?? .white
        }
    }

    /// True for sharps/flats (black-key pitch classes).
    static func isAccidental(pitchClass: Int) -> Bool {
        let pc = ((pitchClass % 12) + 12) % 12
        return isSharp[pc]
    }

    /// Pick a foreground color that reads against the given background —
    /// black on bright naturals, white on the all-black sharps and on
    /// the muted lower-octave palette.
    static func textColor(on background: NSColor) -> NSColor {
        let rgb = background.usingColorSpace(.deviceRGB) ?? background
        let lum = 0.299 * rgb.redComponent
                + 0.587 * rgb.greenComponent
                + 0.114 * rgb.blueComponent
        return lum > 0.55 ? .black : .white
    }

    private static func rgb(_ r: Int, _ g: Int, _ b: Int) -> NSColor {
        NSColor(deviceRed: CGFloat(r) / 255.0,
                green: CGFloat(g) / 255.0,
                blue: CGFloat(b) / 255.0,
                alpha: 1.0)
    }
}
