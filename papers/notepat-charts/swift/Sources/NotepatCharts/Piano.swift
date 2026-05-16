import AppKit
import CoreGraphics

/// Piano keyboard renderer matching the menuband / notepat visual
/// vocabulary: white keys carry colored note-tab swatches at the bottom
/// (matching `slides/notepat-keymap/template.html` and the menuband
/// inline piano), black keys overlay between adjacent whites. Each key
/// also shows its QWERTY notepat letter on top and the note name below.
///
/// Coordinates are in PDF points (1pt = 1/72in). Caller sizes the rect.
enum Piano {

    // ── Notepat key map (MIDI → QWERTY letter). Mirrors the table in
    //    slab/menuband/Sources/MenuBand/KeyboardIconRenderer.swift.
    static let notepatLetter: [Int: String] = [
        58: "z", 59: "x", 60: "c", 61: "v", 62: "d", 63: "s",
        64: "e", 65: "f", 66: "w", 67: "g", 68: "r", 69: "a",
        70: "q", 71: "b", 72: "h", 73: "t", 74: "i", 75: "y",
        76: "j", 77: "k", 78: "u", 79: "l", 80: "o", 81: "m",
        82: "p", 83: "n", 84: ";", 85: "'", 86: "]",
    ]

    /// Notepat note color (white-key tab swatch). Mirrors the palette
    /// in template.html / KeyboardIconRenderer for instant visual
    /// continuity with the menuband piano + notepat web piece.
    static func noteColor(midi: Int) -> NSColor {
        switch midi {
        // Octave 4 (lower hand).
        case 60: return NSColor(srgbRed: 255/255, green:  50/255, blue:  50/255, alpha: 1)  // C — red
        case 62: return NSColor(srgbRed: 255/255, green: 160/255, blue:   0/255, alpha: 1)  // D — orange
        case 64: return NSColor(srgbRed: 255/255, green: 230/255, blue:   0/255, alpha: 1)  // E — yellow
        case 65: return NSColor(srgbRed:  50/255, green: 200/255, blue:  50/255, alpha: 1)  // F — green
        case 67: return NSColor(srgbRed:  50/255, green: 120/255, blue: 255/255, alpha: 1)  // G — blue
        case 69: return NSColor(srgbRed: 130/255, green:  50/255, blue: 200/255, alpha: 1)  // A — purple
        case 71: return NSColor(srgbRed: 180/255, green:  80/255, blue: 255/255, alpha: 1)  // B — violet
        // Octave 5 (upper hand) — dayglo variants.
        case 72: return NSColor(srgbRed: 255/255, green:  40/255, blue:  80/255, alpha: 1)  // C5
        case 74: return NSColor(srgbRed: 255/255, green: 180/255, blue:   0/255, alpha: 1)  // D5
        case 76: return NSColor(srgbRed: 255/255, green: 255/255, blue:  50/255, alpha: 1)  // E5
        case 77: return NSColor(srgbRed:  50/255, green: 255/255, blue: 100/255, alpha: 1)  // F5
        case 79: return NSColor(srgbRed:  50/255, green: 200/255, blue: 255/255, alpha: 1)  // G5
        case 81: return NSColor(srgbRed: 180/255, green:  50/255, blue: 255/255, alpha: 1)  // A5
        case 83: return NSColor(srgbRed: 255/255, green:  80/255, blue: 255/255, alpha: 1)  // B5
        default: return .clear
        }
    }

    /// The note's identity color for ANY key: whites use their own
    /// swatch color, sharps borrow the natural immediately to their
    /// left (matching the QWERTY map). This is the single color a key
    /// lights up in when a chart marks it active — there is no separate
    /// "selection" color; the note color IS the highlight.
    static func accentColor(midi: Int) -> NSColor {
        if isWhite(midi) { return noteColor(midi: midi) }
        var left = midi - 1
        while !isWhite(left) { left -= 1 }
        return noteColor(midi: left)
    }

    /// Drained gray for keys a chart dims out of the lesson.
    static let dimGray = NSColor(srgbRed: 150/255, green: 150/255, blue: 156/255, alpha: 1)

    // ── MIDI helpers ────────────────────────────────────────────────
    static func isWhite(_ midi: Int) -> Bool {
        switch midi % 12 {
        case 0, 2, 4, 5, 7, 9, 11: return true
        default: return false
        }
    }

    static let noteNames = ["C", "C♯", "D", "D♯", "E", "F", "F♯", "G", "G♯", "A", "A♯", "B"]

    static func noteName(midi: Int) -> String {
        let octave = midi / 12 - 1
        let name = noteNames[midi % 12]
        return "\(name)\(octave)"
    }

    static func noteNameShort(midi: Int) -> String {
        return noteNames[midi % 12]
    }

    // ── Range we render: MIDI 58 (B♭3) through MIDI 86 (D5).
    static let firstMidi = 58
    static let lastMidi  = 86

    /// Notepat's "core" range — the alphabetic keys that don't reach
    /// for extras. C4..B5 inclusive plus the C♯..A♯ blacks inside it.
    /// Used to tint extension keys (X, Z, ;, ', ]) as muted.
    static func isExtended(midi: Int) -> Bool {
        return midi < 60 || midi > 83
    }

    /// White keys in [first..last], in order, with positional index.
    static func whiteList(first: Int = firstMidi, last: Int = lastMidi) -> [Int] {
        var out: [Int] = []
        for m in first...last where isWhite(m) { out.append(m) }
        return out
    }

    // MARK: - Highlight specification
    //
    // A chart can override per-key appearance by passing in a map of
    // MIDI → Highlight. Default is "no override" (use the natural
    // notepat coloring). Active = the key participates in the lesson
    // being shown (e.g. notes in a triad); dimmed = key is in range
    // but irrelevant to this chart.
    enum Highlight {
        case normal
        case active(NSColor)   // override the keyswatch color + add a glow
        case dimmed
    }

    // MARK: - Drawing

    struct Layout {
        let whiteW: CGFloat
        let whiteH: CGFloat
        let blackW: CGFloat
        let blackH: CGFloat
        let cornerR: CGFloat
        let qwertyFontSize: CGFloat
        let noteFontSize: CGFloat
        let swatchH: CGFloat
    }

    static func defaultLayout(whiteWidth: CGFloat) -> Layout {
        let whiteW = whiteWidth
        // Tall keys (~5.4× the width) so the QWERTY letter and note-name
        // caption have clear vertical room in the exposed band below
        // the black keys. Black keys stay ~55% of white height so the
        // exposed band ends up ~45% of the key — enough for two stacked
        // labels with a comfortable gap.
        let whiteH = whiteW * 5.4
        return Layout(
            whiteW: whiteW,
            whiteH: whiteH,
            blackW: whiteW * 0.62,
            blackH: whiteH * 0.55,
            cornerR: whiteW * 0.10,
            qwertyFontSize: whiteW * 0.70,
            noteFontSize: whiteW * 0.30,
            swatchH: whiteH * 0.07
        )
    }

    /// Draw the piano centered horizontally inside `bounds.width`, with
    /// the bottom of the white keys flush with `bounds.minY`.
    static func draw(in ctx: CGContext,
                     origin: CGPoint,
                     layout: Layout,
                     first: Int = firstMidi,
                     last: Int = lastMidi,
                     highlights: [Int: Highlight] = [:],
                     showLetters: Bool = true,
                     showNoteNames: Bool = true) {

        let whites = whiteList(first: first, last: last)
        var indexByMidi: [Int: Int] = [:]
        for (i, m) in whites.enumerated() { indexByMidi[m] = i }

        // ── White keys ──────────────────────────────────────────────
        for (i, midi) in whites.enumerated() {
            let x = origin.x + CGFloat(i) * layout.whiteW
            let y = origin.y
            let r = CGRect(x: x, y: y, width: layout.whiteW, height: layout.whiteH)
            drawWhiteKey(in: ctx, rect: r, layout: layout,
                          midi: midi,
                          highlight: highlights[midi] ?? .normal,
                          showLetter: showLetters,
                          showNoteName: showNoteNames)
        }

        // ── Black keys ──────────────────────────────────────────────
        // Each black key sits between two whites. We anchor it on the
        // RIGHT edge of its "left white" (the natural to its left in
        // the chromatic scale). Centered over the boundary so it
        // straddles both adjacent whites the way piano blacks do.
        for midi in first...last where !isWhite(midi) {
            var leftWhite = midi - 1
            while !isWhite(leftWhite) { leftWhite -= 1 }
            guard let leftIdx = indexByMidi[leftWhite] else { continue }
            let centerX = origin.x + CGFloat(leftIdx + 1) * layout.whiteW
            let x = centerX - layout.blackW / 2
            let y = origin.y + layout.whiteH - layout.blackH
            let r = CGRect(x: x, y: y, width: layout.blackW, height: layout.blackH)
            drawBlackKey(in: ctx, rect: r, layout: layout,
                          midi: midi,
                          highlight: highlights[midi] ?? .normal,
                          showLetter: showLetters,
                          showNoteName: showNoteNames)
        }
    }

    private static func drawWhiteKey(in ctx: CGContext,
                                      rect: CGRect,
                                      layout: Layout,
                                      midi: Int,
                                      highlight: Highlight,
                                      showLetter: Bool,
                                      showNoteName: Bool) {
        let isExt = isExtended(midi: midi)
        let isDimmed: Bool = if case .dimmed = highlight { true } else { false }

        // Body fill — soft cream for normal whites, faded for extended
        // or dimmed.
        ctx.saveGState()
        let bodyFill: NSColor = if isExt {
            NSColor(srgbRed: 252/255, green: 244/255, blue: 220/255, alpha: 1.0)
        } else if isDimmed {
            NSColor(srgbRed: 248/255, green: 246/255, blue: 252/255, alpha: 0.6)
        } else {
            NSColor.white
        }
        let stroke: NSColor = isExt
            ? NSColor(srgbRed: 200/255, green: 180/255, blue: 110/255, alpha: 1.0)
            : NSColor(srgbRed: 120/255, green: 114/255, blue: 130/255, alpha: 1.0)

        let path = CGPath(roundedRect: rect,
                          cornerWidth: layout.cornerR,
                          cornerHeight: layout.cornerR,
                          transform: nil)
        ctx.setFillColor(bodyFill.cgColor)
        ctx.addPath(path)
        ctx.fillPath()
        ctx.setStrokeColor(stroke.cgColor)
        ctx.setLineWidth(0.5)
        ctx.addPath(path)
        ctx.strokePath()
        ctx.restoreGState()

        let isActive: Bool = if case .active = highlight { true } else { false }

        // Active keys glow: a soft wash of the note's own color flooded
        // up the key body so a lit note reads at a glance.
        if isActive && !isExt {
            ctx.saveGState()
            ctx.addPath(path)
            ctx.clip()
            ctx.setFillColor(noteColor(midi: midi).withAlphaComponent(0.20).cgColor)
            ctx.fill(rect)
            ctx.restoreGState()
        }

        // Color swatch tab at the bottom. Lit (normal/active) keys carry
        // their full note color; dimmed keys drain to a colorless gray
        // so the active notes are the only color on a chord card.
        if !isExt {
            let swatchRect = CGRect(
                x: rect.minX,
                y: rect.minY,
                width: rect.width,
                height: layout.swatchH
            )
            ctx.saveGState()
            ctx.addPath(CGPath(roundedRect: swatchRect,
                               cornerWidth: layout.cornerR,
                               cornerHeight: layout.cornerR,
                               transform: nil))
            ctx.clip()
            let swatchColor = isDimmed
                ? dimGray.withAlphaComponent(0.45)
                : noteColor(midi: midi)
            ctx.setFillColor(swatchColor.cgColor)
            ctx.fill(swatchRect.insetBy(dx: 0, dy: -layout.cornerR))
            ctx.setStrokeColor(NSColor.black.withAlphaComponent(isDimmed ? 0.08 : 0.22).cgColor)
            ctx.setLineWidth(0.4)
            ctx.move(to: CGPoint(x: rect.minX, y: rect.minY + layout.swatchH))
            ctx.addLine(to: CGPoint(x: rect.maxX, y: rect.minY + layout.swatchH))
            ctx.strokePath()
            ctx.restoreGState()
        }

        // Active emphasis — a bold ring in the note's OWN color (no
        // separate selection hue).
        if isActive {
            ctx.saveGState()
            ctx.setStrokeColor(accentColor(midi: midi).cgColor)
            ctx.setLineWidth(2.2)
            ctx.addPath(CGPath(roundedRect: rect.insetBy(dx: 1.1, dy: 1.1),
                               cornerWidth: layout.cornerR,
                               cornerHeight: layout.cornerR,
                               transform: nil))
            ctx.strokePath()
            ctx.restoreGState()
        }

        // Visible (exposed) zone on the white key — i.e. the region
        // NOT covered by overlapping black keys. Stack the labels
        // with breathing room: QWERTY letter hugs the bottom edge of
        // the black-key area, note name hugs the top of the swatch,
        // and a ≥2pt gap separates them.
        let exposedH = rect.height - layout.blackH
        let letterCenterY = rect.minY + exposedH - layout.qwertyFontSize * 0.50
        let noteCenterY   = rect.minY + layout.swatchH + layout.noteFontSize * 0.65

        if showLetter, let letter = notepatLetter[midi] {
            let color: NSColor = isExt
                ? NSColor(srgbRed: 138/255, green: 115/255, blue:  48/255, alpha: 1.0)
                : (isDimmed
                    ? NSColor.black.withAlphaComponent(0.30)
                    : NSColor(srgbRed:  18/255, green:  16/255, blue:  18/255, alpha: 1.0))
            drawCenteredText(in: ctx,
                              text: letter.uppercased(),
                              center: CGPoint(x: rect.midX, y: letterCenterY),
                              font: berkeleyMono(size: layout.qwertyFontSize, weight: .bold),
                              color: color)
        }

        if showNoteName {
            let color: NSColor = isExt
                ? NSColor(srgbRed: 168/255, green: 152/255, blue:  85/255, alpha: 1.0)
                : NSColor.black.withAlphaComponent(0.55)
            drawCenteredText(in: ctx,
                              text: noteName(midi: midi),
                              center: CGPoint(x: rect.midX, y: noteCenterY),
                              font: berkeleyMono(size: layout.noteFontSize, weight: .bold),
                              color: color)
        }
    }

    private static func drawBlackKey(in ctx: CGContext,
                                      rect: CGRect,
                                      layout: Layout,
                                      midi: Int,
                                      highlight: Highlight,
                                      showLetter: Bool,
                                      showNoteName: Bool) {
        let isExt = isExtended(midi: midi)
        let isDimmed: Bool = if case .dimmed = highlight { true } else { false }
        let isActive: Bool = if case .active = highlight { true } else { false }
        // Fully opaque in every state — a translucent black key let the
        // two white-key edge strokes underneath bleed through as a
        // vertical line down its middle. Dimmed = solid mid-gray.
        let body: NSColor = if isExt {
            NSColor(srgbRed:  90/255, green:  72/255, blue:  24/255, alpha: 1.0)
        } else if isDimmed {
            NSColor(srgbRed:  92/255, green:  90/255, blue: 100/255, alpha: 1.0)
        } else {
            NSColor(srgbRed:  18/255, green:  16/255, blue:  18/255, alpha: 1.0)
        }
        ctx.saveGState()
        let path = CGPath(roundedRect: rect,
                          cornerWidth: layout.cornerR * 0.8,
                          cornerHeight: layout.cornerR * 0.8,
                          transform: nil)
        ctx.setFillColor(body.cgColor)
        ctx.addPath(path)
        ctx.fillPath()
        ctx.setStrokeColor(NSColor.black.cgColor)
        ctx.setLineWidth(0.5)
        ctx.addPath(path)
        ctx.strokePath()
        ctx.restoreGState()

        // Active sharp lights up in its own note color (the natural to
        // its left): flood the cap, then a brighter ring on top.
        if isActive {
            let lit = accentColor(midi: midi)
            ctx.saveGState()
            ctx.addPath(path)
            ctx.clip()
            ctx.setFillColor(lit.withAlphaComponent(0.92).cgColor)
            ctx.fill(rect)
            ctx.restoreGState()
            ctx.saveGState()
            ctx.setStrokeColor(lit.cgColor)
            ctx.setLineWidth(2.2)
            ctx.addPath(CGPath(roundedRect: rect.insetBy(dx: 1.1, dy: 1.1),
                               cornerWidth: layout.cornerR * 0.8,
                               cornerHeight: layout.cornerR * 0.8,
                               transform: nil))
            ctx.strokePath()
            ctx.restoreGState()
        }

        // QWERTY letter — centered in the upper third of the black key.
        if showLetter, let letter = notepatLetter[midi] {
            let labelY = rect.midY + rect.height * 0.20
            let color: NSColor = isExt
                ? NSColor(srgbRed: 248/255, green: 231/255, blue: 160/255, alpha: 1.0)
                : NSColor.white
            drawCenteredText(in: ctx,
                              text: letter.uppercased(),
                              center: CGPoint(x: rect.midX, y: labelY),
                              font: berkeleyMono(size: layout.qwertyFontSize * 0.78, weight: .bold),
                              color: color)
        }

        // Note name — small caption near the bottom of the black key.
        if showNoteName {
            let labelY = rect.minY + layout.noteFontSize * 0.9
            let color: NSColor = isExt
                ? NSColor(srgbRed: 216/255, green: 200/255, blue: 120/255, alpha: 0.9)
                : NSColor.white.withAlphaComponent(0.78)
            drawCenteredText(in: ctx,
                              text: noteName(midi: midi),
                              center: CGPoint(x: rect.midX, y: labelY),
                              font: berkeleyMono(size: layout.noteFontSize * 0.86, weight: .bold),
                              color: color)
        }
    }
}
