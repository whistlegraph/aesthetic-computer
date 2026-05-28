import AppKit
import Foundation

let outPath: String = CommandLine.arguments.count > 1
    ? CommandLine.arguments[1]
    : "./notepat-charts.pdf"
let outURL = URL(fileURLWithPath: outPath)

// ── Music helpers ──────────────────────────────────────────────────
// Use a 1-octave anchor at MIDI 60 (middle C) for chord/interval
// charts; cassette-style colored swatch ties each interval/chord
// shape to the same starting note color.

let intervalAccent = NSColor(srgbRed: 205/255, green:  92/255, blue: 155/255, alpha: 1)  // ac-pink
let intervalRoot   = NSColor(srgbRed:  18/255, green:  16/255, blue:  18/255, alpha: 1)  // ink

/// Build a highlight map that marks `root` as a labeled active key and
/// each note in `offsets` (semitones above the root) as a secondary
/// active. All other keys in the visible range get a dim treatment so
/// the lesson notes pop.
func makeShape(root: Int,
               offsets: [Int],
               rootColor: NSColor = intervalRoot,
               accentColor: NSColor = intervalAccent) -> [Int: Piano.Highlight] {
    var h: [Int: Piano.Highlight] = [:]
    for midi in Piano.firstMidi...Piano.lastMidi {
        h[midi] = .dimmed
    }
    h[root] = .active(rootColor)
    for off in offsets {
        let m = root + off
        if m >= Piano.firstMidi && m <= Piano.lastMidi {
            h[m] = .active(accentColor)
        }
    }
    return h
}

/// Rainbow palette for the interval card. Pulled out as a top-level
/// function so Swift's type-checker doesn't choke on an inline literal.
func intervalsHighlight() -> [Int: Piano.Highlight] {
    var h: [Int: Piano.Highlight] = [:]
    for m in Piano.firstMidi...Piano.lastMidi { h[m] = .dimmed }
    var palette: [NSColor] = []
    palette.append(NSColor(srgbRed: 255/255, green:  60/255, blue:  60/255, alpha: 1))   // P1
    palette.append(NSColor(srgbRed: 255/255, green: 110/255, blue:  60/255, alpha: 1))   // m2
    palette.append(NSColor(srgbRed: 255/255, green: 170/255, blue:  60/255, alpha: 1))   // M2
    palette.append(NSColor(srgbRed: 255/255, green: 220/255, blue:  60/255, alpha: 1))   // m3
    palette.append(NSColor(srgbRed: 220/255, green: 235/255, blue:  60/255, alpha: 1))   // M3
    palette.append(NSColor(srgbRed: 140/255, green: 220/255, blue:  60/255, alpha: 1))   // P4
    palette.append(NSColor(srgbRed:  60/255, green: 200/255, blue: 130/255, alpha: 1))   // TT
    palette.append(NSColor(srgbRed:  60/255, green: 160/255, blue: 230/255, alpha: 1))   // P5
    palette.append(NSColor(srgbRed:  80/255, green: 110/255, blue: 230/255, alpha: 1))   // m6
    palette.append(NSColor(srgbRed: 130/255, green:  80/255, blue: 220/255, alpha: 1))   // M6
    palette.append(NSColor(srgbRed: 180/255, green:  80/255, blue: 220/255, alpha: 1))   // m7
    palette.append(NSColor(srgbRed: 220/255, green:  80/255, blue: 200/255, alpha: 1))   // M7
    palette.append(NSColor(srgbRed: 230/255, green:  80/255, blue: 130/255, alpha: 1))   // P8
    for i in 0...12 {
        h[60 + i] = .active(palette[i])
    }
    return h
}

// ── Charts ─────────────────────────────────────────────────────────

var charts: [Chart] = []

/// Walking accent — bright color used to highlight the practice
/// sequence's notes (so the eye can follow the line).
let walkColor = NSColor(srgbRed: 60/255, green: 160/255, blue: 230/255, alpha: 1)

// Soft pastel page tints. Cycled per card so the deck feels like a
// flip-book of colored notepad cards, while the piano whites still
// pop forward against the warm canvas.
let bgWarmCream = NSColor(srgbRed: 252/255, green: 246/255, blue: 238/255, alpha: 1)
let bgPaleRose  = NSColor(srgbRed: 252/255, green: 240/255, blue: 242/255, alpha: 1)
let bgPalePeach = NSColor(srgbRed: 254/255, green: 240/255, blue: 226/255, alpha: 1)
let bgPaleLemon = NSColor(srgbRed: 252/255, green: 250/255, blue: 226/255, alpha: 1)
let bgPaleMint  = NSColor(srgbRed: 232/255, green: 250/255, blue: 238/255, alpha: 1)
let bgPaleSky   = NSColor(srgbRed: 232/255, green: 244/255, blue: 252/255, alpha: 1)
let bgPaleLilac = NSColor(srgbRed: 242/255, green: 238/255, blue: 252/255, alpha: 1)
let bgPaleSand  = NSColor(srgbRed: 248/255, green: 244/255, blue: 232/255, alpha: 1)

// ───── 0. Cover ──────────────────────────────────────────────
// renderCover() hardcodes the byline and lays everything out
// anchored, so the cover entry only carries title + subtitle.
charts.append(Chart(
    title: "Learn Notepat",
    subtitle: "a flip deck for the QWERTY-as-piano instrument inside aesthetic.computer.",
    highlights: [:],
    body: [],
    footer: "",
    isCover: true,
    backStyle: .pattern   // cover's back = brand mark, not its own title
))

// ───── 1. Layout legend ──────────────────────────────────────
charts.append(Chart(
    title: "Standard Notepat",
    subtitle: "notes that name themselves — your QWERTY row is a piano.",
    highlights: [:],
    body: [
        BodyParagraph(runs: [
            .text("whites spell themselves — "),
            .key("a b c d e f g"),
            .text(" play A through G. the second octave continues alphabetically "),
            .key("h i j k l m n"),
            .text(" for C5 through B5."),
        ], topSpacing: 0),
        BodyParagraph(runs: [
            .text("sharps sit one row up — "),
            .key("q w e r t y u o p"),
            .text(" carry the black notes; "),
            .key("s"),
            .text(" tucks D♯ inside the home row."),
        ], topSpacing: 6),
        BodyParagraph(runs: [
            .bold("range. "),
            .text("29 semitones, B♭3 → D5. wide enough to comp with your left and noodle with your right."),
        ], topSpacing: 6),
    ],
    footer: "NOTEPAT · 1"
))

// ───── 2. Intervals — all 12 from C4 ─────────────────────────
// Highlight every white + black up to the octave above C as a single
// stack; readers count steps from C.
charts.append(Chart(
    title: "Intervals",
    subtitle: "distance, in semitones, between two notes. count up from C.",
    highlights: intervalsHighlight(),
    body: [
        BodyParagraph(runs: [
            .text("12 steps span an octave."),
        ], topSpacing: 0),
        BodyParagraph(runs: [
            .text("naturals: "),
            .key("c→d"), .text(" M2 · "), .key("c→e"), .text(" M3 · "),
            .key("c→f"), .text(" P4 · "), .key("c→g"), .text(" P5 · "),
            .key("c→a"), .text(" M6 · "), .key("c→b"), .text(" M7."),
        ], topSpacing: 6),
        BodyParagraph(runs: [
            .text("flats: "),
            .key("c→v"), .text(" m2 · "), .key("c→s"), .text(" m3 · "),
            .key("c→w"), .text(" TT · "), .key("c→r"), .text(" m6 · "),
            .key("c→q"), .text(" m7 · "), .key("c→h"), .text(" P8."),
        ], topSpacing: 4),
    ],
    footer: "NOTEPAT · 2"
))

// ───── 3. Major triad ────────────────────────────────────────
charts.append(Chart(
    title: "Major Triad",
    subtitle: "root · major 3rd · perfect 5th — 0, 4, 7 semitones.",
    highlights: makeShape(root: 60, offsets: [4, 7]),
    body: [
        BodyParagraph(runs: [
            .text("C major on the notepat: "),
            .key("c"), .text(" + "), .key("e"), .text(" + "), .key("g"),
            .text(". skip a white, skip a white."),
        ], topSpacing: 0),
        BodyParagraph(runs: [
            .text("transpose by sliding: G = "), .key("g i b"),
            .text(", F = "), .key("f a h"),
            .text(", D = "), .key("d w a"),
            .text("."),
        ], topSpacing: 6),
        BodyParagraph(runs: [
            .bold("feel. "),
            .text("bright, resolved, the default chord of major-key songs."),
        ], topSpacing: 6),
    ],
    footer: "NOTEPAT · 3"
))

// ───── 4. Minor triad ────────────────────────────────────────
charts.append(Chart(
    title: "Minor Triad",
    subtitle: "root · minor 3rd · perfect 5th — 0, 3, 7 semitones.",
    highlights: makeShape(root: 60, offsets: [3, 7]),
    body: [
        BodyParagraph(runs: [
            .text("C minor: "),
            .key("c"), .text(" + "), .key("s"), .text(" + "), .key("g"),
            .text(". flatten the 3rd of major — drop "),
            .key("e"), .text(" to "), .key("s"),
            .text(" (D♯ = E♭)."),
        ], topSpacing: 0),
        BodyParagraph(runs: [
            .text("A minor (no sharps): "),
            .key("a"), .text(" + "), .key("c"), .text(" + "), .key("e"),
            .text(". E minor: "), .key("e g b"),
            .text("."),
        ], topSpacing: 6),
        BodyParagraph(runs: [
            .bold("feel. "),
            .text("pensive, weighty — the workhorse of pop, folk, and lament."),
        ], topSpacing: 6),
    ],
    footer: "NOTEPAT · 4"
))

// ───── 5. Diminished + augmented ─────────────────────────────
charts.append(Chart(
    title: "Diminished and Augmented",
    subtitle: "the two altered triads: shrink or stretch the 5th.",
    highlights: {
        // Show DIM as primary (C-E♭-G♭ = 0,3,6) using one color, and
        // AUG (C-E-G♯ = 0,4,8) as a secondary tint so both shapes
        // appear on one card.
        var h: [Int: Piano.Highlight] = [:]
        for m in Piano.firstMidi...Piano.lastMidi { h[m] = .dimmed }
        let dimColor = NSColor(srgbRed: 110/255, green: 130/255, blue: 220/255, alpha: 1)
        let augColor = NSColor(srgbRed: 220/255, green: 100/255, blue:  60/255, alpha: 1)
        h[60] = .active(intervalRoot)
        h[63] = .active(dimColor)   // E♭
        h[66] = .active(dimColor)   // G♭
        h[64] = .active(augColor)   // E
        h[68] = .active(augColor)   // G♯
        return h
    }(),
    body: [
        BodyParagraph(runs: [
            .bold("diminished "),
            .text("(0·3·6): "),
            .key("c s w"),
            .text(". minor 3rd + minor 3rd. anxious, unresolved."),
        ], topSpacing: 0),
        BodyParagraph(runs: [
            .bold("augmented "),
            .text("(0·4·8): "),
            .key("c e r"),
            .text(". major 3rd + major 3rd. dreamlike, suspended."),
        ], topSpacing: 6),
        BodyParagraph(runs: [
            .text("both split the octave evenly — every voicing is its own inversion. transpose by moving one note up or down 1 semitone."),
        ], topSpacing: 6),
    ],
    footer: "NOTEPAT · 5"
))

// ───── 6. Seventh chords ─────────────────────────────────────
charts.append(Chart(
    title: "Seventh Chords",
    subtitle: "stack one more 3rd on a triad — four notes for color.",
    highlights: makeShape(root: 60, offsets: [4, 7, 11]),  // CMaj7
    body: [
        BodyParagraph(runs: [
            .bold("CMaj7 "),
            .text("("), .key("c e g b"), .text(") = major triad + M7. lush, cinematic."),
        ], topSpacing: 0),
        BodyParagraph(runs: [
            .bold("C7 "),
            .text("("), .key("c e g q"), .text(") = major triad + m7. blues. wants to move."),
        ], topSpacing: 4),
        BodyParagraph(runs: [
            .bold("Cm7 "),
            .text("("), .key("c s g q"), .text(") = minor triad + m7. smooth, jazz lounge."),
        ], topSpacing: 4),
        BodyParagraph(runs: [
            .bold("Cø7 "),
            .text("("), .key("c s w q"), .text(") = half-diminished. minor with a flat 5."),
        ], topSpacing: 4),
    ],
    footer: "NOTEPAT · 6"
))

// ───── 7. Circle of fifths ───────────────────────────────────
// Show C → G as P5 highlight; mention the circle traversal.
charts.append(Chart(
    title: "Circle of Fifths",
    subtitle: "go up a perfect 5th — add one sharp. round trip = 12.",
    highlights: makeShape(root: 60, offsets: [7],
                          accentColor: NSColor(srgbRed: 60/255, green: 160/255, blue: 230/255, alpha: 1)),
    body: [
        BodyParagraph(runs: [
            .text("from "), .key("c"), .text(" → "), .key("g"),
            .text(" (7 semitones up). G has 1 sharp (F♯ = "), .key("w"), .text(")."),
        ], topSpacing: 0),
        BodyParagraph(runs: [
            .text("keep going: "),
            .key("c → g → d → a → e → b"),
            .text(". each step adds one sharp; the sharp order is "),
            .key("w r s y u t '"),
            .text(" (F♯ C♯ G♯ D♯ A♯ E♯ B♯)."),
        ], topSpacing: 6),
        BodyParagraph(runs: [
            .text("counterclockwise = perfect 4th = add one flat. 12 steps either way and you're home."),
        ], topSpacing: 6),
    ],
    footer: "NOTEPAT · 7"
))

// ───── 8a. Linear chromatic walk-up ──────────────────────────
// Show ALL keys in the C4..C5 octave highlighted in order so the
// player can practice walking up one semitone at a time.
charts.append(Chart(
    title: "Chromatic Walk",
    subtitle: "every key, in order, C4 up to C5 — 12 semitones, then back.",
    highlights: {
        var h: [Int: Piano.Highlight] = [:]
        for m in Piano.firstMidi...Piano.lastMidi { h[m] = .dimmed }
        for m in 60...72 { h[m] = .active(walkColor) }
        return h
    }(),
    body: [
        BodyParagraph(runs: [
            .bold("up.  "),
            .key("c v d s e f w g r a q b h"),
            .text("."),
        ], topSpacing: 0),
        BodyParagraph(runs: [
            .bold("down.  "),
            .key("h b q a r g w f e s d v c"),
            .text("."),
        ], topSpacing: 6),
        BodyParagraph(runs: [
            .text("hands stay on home row + top row. every QWERTY position rises by one semitone — black notes are always the row above their natural."),
        ], topSpacing: 6),
    ],
    footer: "NOTEPAT · 8"
))

// ───── 8b. Diatonic scales ───────────────────────────────────
// Major + natural minor scale shapes you can walk up and down on
// the home row.
charts.append(Chart(
    title: "Scales to Walk",
    subtitle: "two diatonic shapes — eight notes up, then back down.",
    highlights: {
        // C major (whites only in our range): 60 62 64 65 67 69 71 72.
        var h: [Int: Piano.Highlight] = [:]
        for m in Piano.firstMidi...Piano.lastMidi { h[m] = .dimmed }
        for m in [60, 62, 64, 65, 67, 69, 71, 72] {
            h[m] = .active(walkColor)
        }
        return h
    }(),
    body: [
        BodyParagraph(runs: [
            .bold("C major.  "),
            .key("c d e f g a b h"),
            .text(" → "),
            .key("h b a g f e d c"),
            .text(". all whites, all home-row."),
        ], topSpacing: 0),
        BodyParagraph(runs: [
            .bold("A minor.  "),
            .key("a b c d e f g a"),
            .text(" → reverse. same keys as C major, starts on "),
            .key("a"), .text("."),
        ], topSpacing: 6),
        BodyParagraph(runs: [
            .bold("C minor.  "),
            .key("c d s f g r q h"),
            .text(". one flat note in the row above ("),
            .key("s"), .text(" / "), .key("r"), .text(" / "), .key("q"),
            .text(")."),
        ], topSpacing: 6),
    ],
    footer: "NOTEPAT · 9"
))

// ───── 8c. Interval drill ────────────────────────────────────
// Practice interval-by-interval up from C: P1, m2, M2, m3, M3, …
charts.append(Chart(
    title: "Interval Drill",
    subtitle: "play each pair as a leap up from C — feel the distance.",
    highlights: intervalsHighlight(),
    body: [
        BodyParagraph(runs: [
            .key("c→c"), .text(" P1  ·  "),
            .key("c→v"), .text(" m2  ·  "),
            .key("c→d"), .text(" M2  ·  "),
            .key("c→s"), .text(" m3"),
        ], topSpacing: 0),
        BodyParagraph(runs: [
            .key("c→e"), .text(" M3  ·  "),
            .key("c→f"), .text(" P4  ·  "),
            .key("c→w"), .text(" TT  ·  "),
            .key("c→g"), .text(" P5"),
        ], topSpacing: 4),
        BodyParagraph(runs: [
            .key("c→r"), .text(" m6  ·  "),
            .key("c→a"), .text(" M6  ·  "),
            .key("c→q"), .text(" m7  ·  "),
            .key("c→b"), .text(" M7"),
        ], topSpacing: 4),
        BodyParagraph(runs: [
            .key("c→h"), .text(" P8 — full octave. play each pair down too: drop the upper note, hold the lower, and reverse."),
        ], topSpacing: 6),
    ],
    footer: "NOTEPAT · 10"
))

// ───── 9. Common progressions ────────────────────────────────
charts.append(Chart(
    title: "Progressions",
    subtitle: "chord-shape sequences your fingers can memorize.",
    highlights: makeShape(root: 60, offsets: [4, 7]),   // start on C major
    body: [
        BodyParagraph(runs: [
            .bold("I – IV – V "),
            .text("(C major key): "),
            .key("[c e g]"), .text(" → "),
            .key("[f a h]"), .text(" → "),
            .key("[g i b]"),
            .text(". rock + folk default."),
        ], topSpacing: 0),
        BodyParagraph(runs: [
            .bold("ii – V – I "),
            .text("(jazz turnaround): "),
            .key("[d f a]"), .text(" → "),
            .key("[g i b]"), .text(" → "),
            .key("[c e g]"), .text("."),
        ], topSpacing: 4),
        BodyParagraph(runs: [
            .bold("I – V – vi – IV "),
            .text("(pop forever): "),
            .key("[c e g]"), .text(" → "),
            .key("[g i b]"), .text(" → "),
            .key("[a c e]"), .text(" → "),
            .key("[f a h]"), .text("."),
        ], topSpacing: 4),
    ],
    footer: "NOTEPAT · 11"
))

// ============================================================
// SONG CARDS — one melody per card. Lyrics from the public-domain
// folk corpus (see papers/arxiv-folk-songs). Pitched in C-major-
// friendly keys so every key on the card maps to a notepat natural.
// ============================================================

// Highlight helper: dim everything in range, leave the named MIDI
// notes at their default notepat coloring (so each note pops in its
// own natural color while non-melody keys drain to gray).
func songHighlights(_ midis: [Int]) -> [Int: Piano.Highlight] {
    var h: [Int: Piano.Highlight] = [:]
    for m in Piano.firstMidi...Piano.lastMidi { h[m] = .dimmed }
    for m in midis { h[m] = .normal }
    return h
}

// MIDI shortcuts for the C-major / G-major naturals these songs use.
let mC4 = 60, mD4 = 62, mE4 = 64, mF4 = 65, mG4 = 67, mA4 = 69, mB4 = 71
let mC5 = 72, mD5 = 74, mE5 = 76, mG5 = 79

// Build a vertically-aligned (key, syllable) pair line. Each column is
// padded to the wider of (key, syllable) so the two strings sit in
// equal-width Berkeley-Mono columns and every note glyph lands above
// the syllable it plays.
func aligned(_ pairs: [(String, String)]) -> (keys: String, lyrics: String) {
    var k = ""
    var l = ""
    for (note, syl) in pairs {
        let w = max(note.count, syl.count) + 1
        k += note.padding(toLength: w, withPad: " ", startingAt: 0)
        l += syl.padding(toLength: w, withPad: " ", startingAt: 0)
    }
    return (k, l)
}

// Convert a list of phrases (each a list of (note, syllable) pairs)
// into stacked BodyParagraphs: key-line then lyric-line, repeating per
// phrase with a small top gap between phrases.
func songLines(_ phrases: [[(String, String)]]) -> [BodyParagraph] {
    var out: [BodyParagraph] = []
    for (i, phrase) in phrases.enumerated() {
        let a = aligned(phrase)
        out.append(BodyParagraph(runs: [.key(a.keys)], topSpacing: i == 0 ? 0 : 8))
        out.append(BodyParagraph(runs: [.mono(a.lyrics)], topSpacing: 0))
    }
    return out
}

// ───── 12. Twinkle Twinkle Little Star ───────────────────────
charts.append(Chart(
    title: "Twinkle Twinkle",
    subtitle: "english nursery rhyme. C major. all white keys. play with one finger.",
    highlights: [:],
    body: [],
    footer: "",
    isSong: true,
    songPhrases: [
        [("c","twin-"), ("c","-kle"), ("g","twin-"), ("g","-kle"), ("a","li-"), ("a","-ttle"), ("g","star")],
        [("f","how"),   ("f","i"),    ("e","won-"), ("e","-der"), ("d","what"),("d","you"),  ("c","are")],
        [("g","up"),    ("g","a-"),   ("f","-bove"),("f","the"),  ("e","world"),("e","so"),  ("d","high")],
        [("g","like"),  ("g","a"),    ("f","dia-"), ("f","-mond"),("e","in"), ("e","the"),  ("d","sky")],
    ]
))

// ───── 13. Row Row Row Your Boat (round) ─────────────────────
charts.append(Chart(
    title: "Row Row Row Your Boat",
    subtitle: "english round. play it; have a friend start 4 beats behind. instant two-part.",
    highlights: [:],
    body: [],
    footer: "",
    isSong: true,
    songPhrases: [
        [("c","row"), ("c","row"), ("c","row"), ("d","your"),("e","boat")],
        [("e","gent-"),("d","-ly"),("e","down"),("f","the"), ("g","stream")],
        [("h","mer-"),("h","-ri-"),("h","-ly"), ("g","mer-"),("g","-ri-"),("g","-ly")],
        [("e","mer-"),("e","-ri-"),("e","-ly"), ("c","mer-"),("c","-ri-"),("c","-ly")],
        [("g","life"),("f","is"), ("e","but"), ("d","a"),   ("c","dream")],
    ]
))

// ───── 15. Frère Jacques (french round) ──────────────────────
charts.append(Chart(
    title: "Frere Jacques",
    subtitle: "french round. C major. each phrase plays twice; start a friend 4 beats behind.",
    highlights: [:],
    body: [],
    footer: "",
    isSong: true,
    songPhrases: [
        [("c","frè-"), ("d","-re"), ("e","jac-"), ("c","-ques")],
        [("e","dor-"), ("f","-mez"),("g","vous")],
        [("g","son-"), ("a","-nez"),("g","les"), ("f","ma-"), ("e","-ti-"),("c","-nes")],
        [("c","ding"), ("g","ding"),("c","dong")],
    ]
))

// ───── 16. Mary Had a Little Lamb ────────────────────────────
charts.append(Chart(
    title: "Mary Had a Little Lamb",
    subtitle: "english nursery rhyme. only four notes — c d e g. taught to most kids first.",
    highlights: [:],
    body: [],
    footer: "",
    isSong: true,
    songPhrases: [
        [("e","ma-"),("d","-ry"),("c","had"),("d","a"), ("e","lit-"),("e","-tle"),("e","lamb")],
        [("d","lit-"),("d","-tle"),("d","lamb"),("e","lit-"),("g","-tle"),("g","lamb")],
        [("e","ma-"),("d","-ry"),("c","had"),("d","a"), ("e","lit-"),("e","-tle"),("e","lamb")],
        [("d","its"),("d","fleece"),("e","was"),("d","white"),("c","as snow")],
    ]
))

// ───── 17. Ode to Joy (Beethoven 9th) ────────────────────────
charts.append(Chart(
    title: "Ode to Joy",
    subtitle: "beethoven, ninth symphony. five notes — c d e f g — and you've got the EU anthem.",
    highlights: [:],
    body: [],
    footer: "",
    isSong: true,
    songPhrases: [
        [("e","joy-"),("e","-ful"),("f","joy-"),("g","-ful"),("g","we"),("f","a-"),("e","-dore"),("d","thee")],
        [("c","god"),("c","of"), ("d","glo-"),("e","-ry"), ("e","lord"),("d","of"),("d","love")],
        [("e","hearts"),("e","un-"),("f","-fold"),("g","like"),("g","flow'rs"),("f","be-"),("e","-fore"),("d","thee")],
        [("c","prais-"),("c","-ing"),("d","thee,"),("e","their"),("d","sun"),("c","a-"),("c","-bove")],
    ]
))

// ───── 18. Happy Birthday ────────────────────────────────────
// (Public domain since 2016 — Warner/Chappell lost the case.)
charts.append(Chart(
    title: "Happy Birthday",
    subtitle: "public domain since 2016. wide range — climbs to upper G for the friend's name.",
    highlights: [:],
    body: [],
    footer: "",
    isSong: true,
    songPhrases: [
        [("g","hap-"),("g","-py"),("a","birth-"),("g","-day"),("h","to"),("b","you")],
        [("g","hap-"),("g","-py"),("a","birth-"),("g","-day"),("i","to"),("h","you")],
        [("g","hap-"),("g","-py"),("l","birth-"),("j","-day"),("h","dear"),("b","___")],
        [("f","hap-"),("f","-py"),("j","birth-"),("h","-day"),("i","to"),("h","you")],
    ]
))

// ───── 14. Amazing Grace (pentatonic) ────────────────────────
// Melody encoded in G-major pentatonic — the 5 notes G A B D E. The
// song starts on a pickup D below the tonic, then climbs. No sharps.
charts.append(Chart(
    title: "Amazing Grace",
    subtitle: "american pentatonic. five notes — g a b d e — zero sharps. pickup on the low d.",
    highlights: [:],
    body: [],
    footer: "",
    isSong: true,
    songPhrases: [
        [("d","a-"), ("g","-ma-"),("b","-zing"),("g","grace"),("b","how"),("a","sweet")],
        [("g","the"),("b","sound"),("i","that"),("j","saved"),("i","a")],
        [("b","wretch"),("g","like"),("b","me")],
    ]
))

// ── Pastel rotation per card ───────────────────────────────────────
let palette: [NSColor] = [
    bgWarmCream, bgPaleRose, bgPalePeach, bgPaleLemon,
    bgPaleMint,  bgPaleSky,  bgPaleLilac, bgPaleSand,
    bgWarmCream, bgPaleMint, bgPaleSky,
    // song cards — let each get a distinct pastel
    bgPaleRose,  bgPalePeach, bgPaleLemon,
    bgPaleMint,  bgPaleSky,   bgPaleLilac, bgPaleSand,
]
for i in charts.indices {
    charts[i].background = palette[i % palette.count]
}

// ── Render the whole deck. ─────────────────────────────────────────
// Cover keys illustration: prefer the gpt-image-2 illy-cover.png if it
// exists, fall back to the legacy keys-strip.png crop.
let coverDir = outURL.deletingLastPathComponent()
for candidate in ["illy-cover.png", "keys-strip.png"] {
    let p = coverDir.appendingPathComponent(candidate).path
    if FileManager.default.fileExists(atPath: p) {
        ChartRenderer.coverKeysImagePath = p
        break
    }
}
ChartRenderer.renderPDF(charts: charts, to: outURL)
print("wrote \(outURL.path) — \(charts.count) cards")
