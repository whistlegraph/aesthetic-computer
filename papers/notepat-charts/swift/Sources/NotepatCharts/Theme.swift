import AppKit

/// One place for every color that is not a note color. Renderers read
/// `ChartRenderer.theme` and never name a literal; `--dark` swaps the
/// instance before the deck renders. Tokens are semantic (surface, ink
/// tier, separator, key body) so light and dark stay the same design.
///
/// Light is the paper brand (papers/ac-paper-cards.sty: acpink, acpurple
/// #7850B4, acgray #777). Dark is the house dark used by the prompt
/// (disks/theme.mjs), the aesel renderer (aesel/src/render.mjs) and the
/// slab terminal profiles (slab/bin/slab-aesthetic-profiles.swift): the
/// purple ground #463264 with white ink, soft #DCB4FF, muted #AA96CD,
/// bar #5F4687, handle #FF64FF, and the deeper grounds #33285A / #2A2038
/// for frame and sharps. Elevation by lightness, hairline separators,
/// shadows pulled back, note colors the only other saturation.
struct Theme {
    // Surfaces
    let frame: NSColor            // card frame around the face
    let faceBase: NSColor         // what a card's pastel blends toward
    let faceTint: CGFloat         // how much pastel survives (1 = as given)
    let edge: NSColor             // hairline on the card's outer edge
    let shadowAlpha: CGFloat
    let elevated: NSColor         // boxes sitting on the face
    // Accent + hands
    let accent: NSColor           // phrase brackets, the title's dot
    let handRight: NSColor
    let handLeft: NSColor
    // Ink tiers
    let ink: NSColor
    let ink2: NSColor
    let ink3: NSColor
    let separator: NSColor
    let separatorStrong: NSColor
    // QWERTY caps (also the sharps on hand-row cards)
    let capNatural: NSColor, capNaturalLetter: NSColor, capNaturalBorder: NSColor
    let capSharp: NSColor,   capSharpLetter: NSColor,   capSharpBorder: NSColor
    let capShadow: NSColor
    // Piano
    let pianoWhite: NSColor, pianoWhiteDim: NSColor, pianoWhiteStroke: NSColor
    let pianoWhiteLetter: NSColor, pianoWhiteLetterDim: NSColor, pianoWhiteNote: NSColor
    let pianoBlack: NSColor, pianoBlackDim: NSColor, pianoBlackStroke: NSColor
    let pianoBlackLetter: NSColor, pianoBlackNote: NSColor
    let pianoExtWhite: NSColor, pianoExtWhiteStroke: NSColor, pianoExtWhiteLetter: NSColor, pianoExtWhiteNote: NSColor
    let pianoExtBlack: NSColor, pianoExtBlackLetter: NSColor, pianoExtBlackNote: NSColor
    let swatchEdge: NSColor
    // Keycaps on hand-row cards: how far the face blends toward the note color
    let keyTint: CGFloat
    let keyStrokeToward: NSColor  // note color leans this way for its outline
    let keyStrokeAmount: CGFloat
    // Score pills: left-hand pill opacity
    let pillLeftAlpha: CGFloat
    let dotAlpha: CGFloat
    let coverFullBleed: Bool      // light: illustration bleeds; dark: framed picture

    /// A card face from the deck's pastel.
    func face(_ pastel: NSColor) -> NSColor {
        let p = pastel.usingColorSpace(.sRGB) ?? pastel
        return faceTint >= 1 ? p : (faceBase.blended(withFraction: faceTint, of: p) ?? faceBase)
    }

    static func rgb(_ r: CGFloat, _ g: CGFloat, _ b: CGFloat, _ a: CGFloat = 1) -> NSColor {
        NSColor(srgbRed: r / 255, green: g / 255, blue: b / 255, alpha: a)
    }

    static let light = Theme(
        frame: .white,
        faceBase: .white, faceTint: 1,
        edge: rgb(120, 80, 180), shadowAlpha: 0.62,
        elevated: NSColor.white.withAlphaComponent(0.55),
        accent: rgb(205, 92, 155), handRight: rgb(205, 92, 155), handLeft: rgb(60, 120, 230),
        ink: rgb(18, 16, 18), ink2: rgb(119, 119, 119), ink3: rgb(168, 168, 172),
        separator: rgb(18, 16, 18, 0.14), separatorStrong: rgb(18, 16, 18, 0.55),
        capNatural: .white, capNaturalLetter: rgb(18, 16, 18), capNaturalBorder: rgb(110, 104, 120),
        capSharp: rgb(18, 16, 18), capSharpLetter: .white, capSharpBorder: .black,
        capShadow: NSColor.black.withAlphaComponent(0.18),
        pianoWhite: .white, pianoWhiteDim: rgb(248, 246, 252, 0.6), pianoWhiteStroke: rgb(120, 114, 130),
        pianoWhiteLetter: rgb(18, 16, 18), pianoWhiteLetterDim: NSColor.black.withAlphaComponent(0.30),
        pianoWhiteNote: NSColor.black.withAlphaComponent(0.55),
        pianoBlack: rgb(18, 16, 18), pianoBlackDim: rgb(92, 90, 100), pianoBlackStroke: .black,
        pianoBlackLetter: .white, pianoBlackNote: NSColor.white.withAlphaComponent(0.78),
        pianoExtWhite: rgb(252, 244, 220), pianoExtWhiteStroke: rgb(200, 180, 110),
        pianoExtWhiteLetter: rgb(138, 115, 48), pianoExtWhiteNote: rgb(168, 152, 85),
        pianoExtBlack: rgb(90, 72, 24), pianoExtBlackLetter: rgb(248, 231, 160), pianoExtBlackNote: rgb(216, 200, 120, 0.9),
        swatchEdge: NSColor.black.withAlphaComponent(0.22),
        keyTint: 0.42, keyStrokeToward: rgb(18, 16, 18), keyStrokeAmount: 0.3,
        pillLeftAlpha: 0.42, dotAlpha: 0.55,
        coverFullBleed: true)

    static let dark = Theme(
        frame: rgb(42, 32, 56),                       // stale ground #2A2038
        faceBase: rgb(70, 50, 100), faceTint: 0.14,   // blank ground #463264, a breath of the card's pastel
        edge: rgb(120, 80, 180), shadowAlpha: 0.42,   // acpurple hairline
        elevated: rgb(95, 70, 135, 0.45),             // bar #5F4687
        accent: rgb(255, 100, 255), handRight: rgb(255, 100, 255), handLeft: rgb(120, 200, 255),  // handle, inbox
        ink: .white, ink2: rgb(220, 180, 255), ink3: rgb(170, 150, 205),                          // text, soft, muted
        separator: rgb(255, 255, 255, 0.18), separatorStrong: rgb(220, 180, 255, 0.6),
        capNatural: rgb(220, 180, 255), capNaturalLetter: rgb(40, 30, 90), capNaturalBorder: rgb(120, 80, 180),
        capSharp: rgb(28, 20, 40), capSharpLetter: rgb(220, 180, 255), capSharpBorder: .black,
        capShadow: NSColor.black.withAlphaComponent(0.45),
        pianoWhite: rgb(220, 180, 255), pianoWhiteDim: rgb(185, 154, 224, 0.7), pianoWhiteStroke: rgb(42, 32, 56),
        pianoWhiteLetter: rgb(40, 30, 90), pianoWhiteLetterDim: rgb(40, 30, 90, 0.35),
        pianoWhiteNote: rgb(40, 30, 90, 0.6),
        pianoBlack: rgb(28, 20, 40), pianoBlackDim: rgb(70, 60, 90), pianoBlackStroke: .black,
        pianoBlackLetter: .white, pianoBlackNote: rgb(255, 255, 255, 0.72),
        pianoExtWhite: rgb(150, 125, 190), pianoExtWhiteStroke: rgb(95, 70, 135),
        pianoExtWhiteLetter: rgb(40, 30, 90), pianoExtWhiteNote: rgb(40, 30, 90, 0.6),
        pianoExtBlack: rgb(50, 36, 70), pianoExtBlackLetter: rgb(220, 180, 255), pianoExtBlackNote: rgb(220, 180, 255, 0.8),
        swatchEdge: NSColor.black.withAlphaComponent(0.45),
        keyTint: 0.55, keyStrokeToward: .white, keyStrokeAmount: 0.3,
        pillLeftAlpha: 0.6, dotAlpha: 0.75,
        coverFullBleed: false)
}
