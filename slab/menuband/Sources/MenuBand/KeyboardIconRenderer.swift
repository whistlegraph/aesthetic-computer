import AppKit

// Tiny piano rendered into a single menubar status item with a flat
// text-only header. Layout (left to right):
//
//   [pad][picker][gap][piano keys (A3..D6, A3 hidden)][gap][MIDI][pad]
//
// Hit-testing returns .toggleMIDI / .pickInstrument / .note(midi) so the
// drag-and-tap interaction in AppDelegate can route accordingly. Header
// buttons are flat (no fill, no border) so they read like native menubar
// text. Keys are skeuomorphic: white gradient + dark-accent black gradient.
enum KeyboardIconRenderer {
    /// Updated by AppDelegate.updateIcon() before each render so the renderer
    /// can pick the right letter labels and active-range without threading
    /// the keymap through every static method's signature.
    static var activeKeymap: Keymap = .notepat

    /// Adaptive sizing for menubar overflow. AppDelegate progressively
    /// shrinks this when the status item can't fit, and tries to expand
    /// back when there's room. Render math (pianoWidth, imageSize, hit
    /// rects, slot positions) all derive from `lastMidi`.
    enum DisplayLayout {
        case full        // C4..B5 — 2 octaves (default)
        case oneOctave   // C4..B4 — 1 octave fallback
        case compact     // chip only, no piano keys

        var smaller: DisplayLayout? {
            switch self {
            case .full: return .oneOctave
            case .oneOctave: return .compact
            case .compact: return nil
            }
        }
        var larger: DisplayLayout? {
            switch self {
            case .compact: return .oneOctave
            case .oneOctave: return .full
            case .full: return nil
            }
        }
    }
    static var displayLayout: DisplayLayout = .full

    // Render area shrinks with the layout. Compact has no piano keys at
    // all — `lastMidi < firstMidi` makes whiteList() empty.
    static let firstMidi: Int = 60                 // C4 (middle C)
    static var lastMidi: Int {
        switch displayLayout {
        case .full:      return 83                 // B5 — full 2 octaves
        case .oneOctave: return 71                 // B4 — single octave
        case .compact:   return firstMidi - 1      // empty range
        }
    }

    /// Active subset of the visible range that's drawn + interactive.
    /// Capped by both the keymap range (Ableton tops out at E5/76) and the
    /// current display layout. Returns nil when no keys are displayable.
    static var activeRange: ClosedRange<Int>? {
        let keymapMax = activeKeymap == .ableton ? 76 : 83
        let upper = Swift.min(keymapMax, lastMidi)
        return upper >= firstMidi ? firstMidi...upper : nil
    }

    @inline(__always)
    private static func isActive(_ midi: Int) -> Bool {
        activeRange?.contains(midi) ?? false
    }

    /// Right-align the active keys in Ableton mode so the negative space
    /// (the unmapped 4 whites past E5) sits on the *left* — visually it
    /// reads as "the rightmost portion of the 2-octave area is the playable
    /// keymap." `imageSize` doesn't change, so the popover anchor stays put.
    static var activeSlotOffset: Int {
        let allWhites = whiteList()
        guard let range = activeRange else { return 0 }
        let activeCount = allWhites.filter { range.contains($0) }.count
        return allWhites.count - activeCount
    }

    // Piano key sizes — wide whites give a generous hit area between black
    // keys (the exposed-white-between-blacks zone is whiteW - blackW).
    static let whiteW: CGFloat = 23.0
    static let whiteH: CGFloat = 21.0
    static let blackW: CGFloat = 13.5
    static let blackH: CGFloat = 12.0  // shorter so the white-below strip is
                                       // tall enough to drag across easily
    static let pad: CGFloat = 0.5

    // Settings — simple monochrome music note that reads like a native
    // status-bar icon. Click → popup menu with TYPE / MIDI / Instrument /
    // About.
    static let settingsW: CGFloat = 18.0
    static let settingsH: CGFloat = 21.0
    static let settingsGap: CGFloat = 12.0
    /// Reserved space on the right of the icon for the voice-number
    /// badge to flow into when the program has 2+ digits. The music
    /// note + settingsHitRect stay anchored where they were; this
    /// pad just gives multi-digit numbers somewhere to grow without
    /// clipping at the image edge.
    static let voiceBadgeRightPad: CGFloat = 12.0

    enum HitResult: Equatable {
        case openSettings
        case note(UInt8)
    }

    // Letter labels keyed by MIDI note, per layout. The renderer picks
    // between these via `activeKeymap` so the menubar piano shows the
    // correct QWERTY hint for whichever mode you're in.
    private static let labelByMidiNotepat: [Int: String] = [
        58: "z", 59: "x", 60: "c", 61: "v", 62: "d", 63: "s",
        64: "e", 65: "f", 66: "w", 67: "g", 68: "r", 69: "a",
        70: "q", 71: "b", 72: "h", 73: "t", 74: "i", 75: "y",
        76: "j", 77: "k", 78: "u", 79: "l", 80: "o", 81: "m",
        82: "p", 83: "n", 84: ";", 85: "'", 86: "]",
    ]

    // Ableton Live's M-mode QWERTY mapping: A=C, W=C#, S=D, E=D#, D=E,
    // F=F, T=F#, G=G, Y=G#, H=A, U=A#, J=B, K=C+1, O=C#+1, L=D+1,
    // P=D#+1, ;=E+1. Mirrors `MenuBandLayout.semitoneByKeyCodeAbleton`
    // with middle C anchored at MIDI 60.
    private static let labelByMidiAbleton: [Int: String] = [
        60: "a", 61: "w", 62: "s", 63: "e", 64: "d",
        65: "f", 66: "t", 67: "g", 68: "y", 69: "h",
        70: "u", 71: "j", 72: "k", 73: "o", 74: "l",
        75: "p", 76: ";",
    ]

    static var labelByMidi: [Int: String] {
        activeKeymap == .ableton ? labelByMidiAbleton : labelByMidiNotepat
    }

    @inline(__always)
    private static func isWhite(_ midi: Int) -> Bool {
        switch midi % 12 {
        case 0, 2, 4, 5, 7, 9, 11: return true
        default: return false
        }
    }

    private static func whiteList() -> [Int] {
        guard lastMidi >= firstMidi else { return [] }
        return (firstMidi...lastMidi).filter { isWhite($0) }
    }

    private static var pianoWidth: CGFloat {
        CGFloat(whiteList().count) * whiteW
    }

    static var imageSize: NSSize {
        let totalW = ceil(pad + pianoWidth + settingsGap + settingsW + pad + voiceBadgeRightPad)
        let totalH = ceil(whiteH + pad * 2)
        return NSSize(width: totalW, height: totalH)
    }

    private static var pianoOriginX: CGFloat { pad }

    /// Settings chip's visual rect IS its hit-test rect — they're identical
    /// so the user gets visual feedback wherever the click lands.
    private static var settingsRect: NSRect { settingsHitRect }

    static func image(litNotes: Set<UInt8>,
                      enabled: Bool,
                      typeMode: Bool = false,
                      melodicProgram: UInt8 = 0,
                      hovered: HitResult? = nil,
                      letterAlpha: ((UInt8) -> CGFloat)? = nil,
                      slideOffsetX: CGFloat = 0,
                      settingsFlash: CGFloat = 0) -> NSImage {
        let whites = whiteList()
        var whiteIndex: [Int: Int] = [:]
        for (i, m) in whites.enumerated() { whiteIndex[m] = i }
        let size = imageSize

        let img = NSImage(size: size, flipped: false) { _ in

            // Octave slide: translate the WHOLE icon (piano + settings
            // chip) by the requested offset so the entire menubar
            // board scrolls left/right as one unit when the user
            // changes octave. This is intentionally OUTSIDE any
            // sub-save state so it affects every subsequent draw.
            if abs(slideOffsetX) > 0.01 {
                let xform = NSAffineTransform()
                xform.translateX(by: slideOffsetX, yBy: 0)
                xform.concat()
            }
            // Piano.
            NSGraphicsContext.saveGraphicsState()
            // Dark-mode awareness: in light mode the piano reads as
            // a real piano (white keys white, black keys dark
            // accent). In dark mode we swap the relationship — white
            // keys go a soft macOS dark-gray, black keys flip to a
            // brighter accent so they still pop above the white
            // keys. Lit (active) state always rides the accent
            // palette so a pressed key contrasts both modes.
            let isDark = NSApp.effectiveAppearance.bestMatch(
                from: [.aqua, .darkAqua]) == .darkAqua
            let lit = NSColor.controlAccentColor.highlight(withLevel: 0.30)
                ?? NSColor.controlAccentColor
            let groove = NSColor.black.withAlphaComponent(isDark ? 0.85 : 0.55)
            let whiteHi: NSColor
            let whiteLo: NSColor
            let blackHi: NSColor
            let blackLo: NSColor
            if isDark {
                // Soft macOS dark-gray for the "white" keys.
                whiteHi = NSColor(white: 0.20, alpha: 1.0)
                whiteLo = NSColor(white: 0.13, alpha: 1.0)
                // Brighter accent for the "black" keys so they
                // stand out above the dark grays.
                blackHi = NSColor.controlAccentColor.highlight(withLevel: 0.10)
                    ?? NSColor.controlAccentColor
                blackLo = NSColor.controlAccentColor.highlight(withLevel: 0.30)
                    ?? NSColor.controlAccentColor
            } else {
                whiteHi = NSColor.white
                whiteLo = NSColor(white: 0.88, alpha: 1.0)
                blackHi = NSColor.controlAccentColor.shadow(withLevel: 0.30)
                    ?? NSColor.controlAccentColor
                blackLo = NSColor.controlAccentColor.shadow(withLevel: 0.55)
                    ?? NSColor.controlAccentColor
            }

            // "Edges" of the *active* range — used for the rounded outer
            // corners. Active range may be right-aligned (Ableton) so the
            // outer corners sit on the first/last active white in the slot
            // layout, not the geometric ends of the render area.
            let activeWhites = whites.filter { isActive($0) }
            let leftmostMidi = activeWhites.first ?? firstMidi
            let rightmostMidi = activeWhites.last ?? lastMidi
            let slotOffset = activeSlotOffset
            for (idx, m) in whites.enumerated() {
                if !isActive(m) { continue }   // negative space — skip draw
                let rect = whiteRect(at: idx + slotOffset)
                let isLit = litNotes.contains(UInt8(m))
                let isHover = hovered == .note(UInt8(m))
                let isLeftmost = (m == leftmostMidi)
                let isRightmost = (m == rightmostMidi)
                let path = roundedKeyPath(
                    rect: rect,
                    tl: isLeftmost ? 2.5 : 0,
                    tr: isRightmost ? 2.5 : 0,
                    br: isRightmost ? 2.5 : 0,
                    bl: isLeftmost ? 2.5 : 0
                )
                if isLit {
                    lit.setFill()
                    path.fill()
                } else {
                    NSGradient(starting: whiteHi, ending: whiteLo)!.draw(in: path, angle: -90)
                }
                if isHover && !isLit {
                    NSColor.controlAccentColor.withAlphaComponent(0.50).setFill()
                    path.fill()
                }
                groove.setStroke()
                path.lineWidth = 0.7
                path.stroke()
                // Lit keys always wear their letter at full opacity (a
                // mouse-press tap reveals just that letter). For unlit
                // keys, the per-key `letterAlpha` closure drives a wave
                // fade-in / fade-out that ripples outward from whichever
                // key the user is currently playing. Falls back to the
                // legacy binary `typeMode` rendering when no closure is
                // supplied (e.g., previews that don't drive animation).
                if let letter = labelByMidi[m] {
                    let a: CGFloat
                    if isLit {
                        a = 1.0
                    } else if let closure = letterAlpha {
                        a = closure(UInt8(m))
                    } else {
                        a = typeMode ? 1.0 : 0.0
                    }
                    if a > 0.01 {
                        drawWhiteLabel(letter, in: rect, lit: isLit, alpha: a)
                    }
                }
            }
            for m in firstMidi...lastMidi where !isWhite(m) {
                if !isActive(m) { continue }   // negative space
                var leftWhite = m - 1
                while !isWhite(leftWhite) { leftWhite -= 1 }
                guard let leftIdx = whiteIndex[leftWhite] else { continue }
                let rect = blackRect(rightOfWhiteIndex: leftIdx + slotOffset)
                let isLit = litNotes.contains(UInt8(m))
                let isHover = hovered == .note(UInt8(m))
                let path = roundedKeyPath(rect: rect, tl: 0, tr: 0, br: 1.2, bl: 1.2)
                if isLit {
                    lit.setFill()
                    path.fill()
                } else {
                    NSGradient(starting: blackHi, ending: blackLo)!.draw(in: path, angle: -90)
                }
                if isHover && !isLit {
                    NSColor.white.withAlphaComponent(0.20).setFill()
                    path.fill()
                }
                groove.setStroke()
                path.lineWidth = 0.6
                path.stroke()
                if let letter = labelByMidi[m] {
                    let a: CGFloat
                    if isLit {
                        a = 1.0
                    } else if let closure = letterAlpha {
                        a = closure(UInt8(m))
                    } else {
                        a = typeMode ? 1.0 : 0.0
                    }
                    if a > 0.01 {
                        drawBlackLabel(letter, in: rect, lit: isLit, alpha: a)
                    }
                }
            }
            NSGraphicsContext.restoreGraphicsState()

            // Single settings chip — glyph + color reflect MIDI/DAW state.
            // MIDI on → `waveform` tinted accent (signal flowing to DAW);
            // MIDI off → `slider.horizontal.3` in label color (generic).
            drawSettingsChip(in: settingsRect, hoverRect: settingsHitRect,
                             midiOn: enabled,
                             hovered: hovered == .openSettings,
                             flash: settingsFlash,
                             voiceNumber: Int(melodicProgram))
            return true
        }
        img.isTemplate = false
        return img
    }

    /// Public so the popover can anchor its arrow at the latch — point
    /// at the visible icon, not the wider hit area, so the arrow lines
    /// up directly below the music note glyph.
    static var settingsRectPublic: NSRect { settingsIconRect }

    // Settings button hit area extends from the piano's right edge
    // all the way to the image's right edge — including the voice-
    // badge pad — so clicking either the music-note glyph OR the
    // badge digits opens the popover. The icon is positioned via
    // `settingsIconRect` directly (not via this hit rect) so the
    // glyph stays put even though the hit zone now covers the pad.
    private static var settingsHitRect: NSRect {
        let leftX = pianoOriginX + pianoWidth
        let rightX = imageSize.width
        return NSRect(x: leftX, y: 0, width: rightX - leftX, height: imageSize.height)
    }

    /// Visible icon bounds — a pill-sized region centered on the music
    /// note glyph itself. Used both as the hover-highlight backdrop and
    /// as the popover anchor so the popover arrow points right at the
    /// icon. Width tracks `settingsW`; height tracks `settingsH`.
    private static var settingsIconRect: NSRect {
        let w = settingsW
        let h = settingsH
        // Anchor the icon to the image's "old" right edge — i.e.
        // before the voice-badge pad was added. Otherwise the
        // music-note glyph would drift right whenever the badge pad
        // claims its slice of the image.
        let oldRight = imageSize.width - voiceBadgeRightPad
        let cx = oldRight - w / 2 - pad
        let cy = imageSize.height / 2
        return NSRect(x: cx - w / 2, y: cy - h / 2, width: w, height: h)
    }

    // MARK: - Hit testing

    static func hit(at point: NSPoint) -> HitResult? {
        if settingsHitRect.contains(point) { return .openSettings }
        let whites = whiteList()
        var whiteIndex: [Int: Int] = [:]
        for (i, m) in whites.enumerated() { whiteIndex[m] = i }
        let slotOffset = activeSlotOffset
        // Black-key hit area = the visual blackRect. 1:1 mapping with what
        // the user sees on screen — clicking on visible black triggers black,
        // clicking visible white triggers white. Inactive (negative-space)
        // keys are non-interactive.
        for m in firstMidi...lastMidi where !isWhite(m) {
            if !isActive(m) { continue }
            var leftWhite = m - 1
            while !isWhite(leftWhite) { leftWhite -= 1 }
            guard let leftIdx = whiteIndex[leftWhite] else { continue }
            if blackRect(rightOfWhiteIndex: leftIdx + slotOffset).contains(point) {
                return .note(UInt8(m))
            }
        }
        // White keys: y is unbounded so any cursor-y inside the button maps
        // to the white at that x — even when the menubar adds an extra pixel
        // or two of padding above/below the image, those edge clicks still
        // register as the underlying white. Black-band check above already
        // claims the black-key region; everything else falls through here.
        for (idx, m) in whites.enumerated() {
            if !isActive(m) { continue }
            let r = whiteRect(at: idx + slotOffset)
            let relaxed = NSRect(x: r.minX, y: -100,
                                 width: r.width, height: 200)
            if relaxed.contains(point) { return .note(UInt8(m)) }
        }
        return nil
    }

    /// Public lookup so callers (drag handler) can compute cursor-relative
    /// expression — e.g. y→velocity, x→pan within the active key's bounds.
    static func keyRect(for midi: UInt8) -> NSRect? {
        let m = Int(midi)
        let whites = whiteList()
        let slotOffset = activeSlotOffset
        if isWhite(m) {
            guard let idx = whites.firstIndex(of: m) else { return nil }
            return whiteRect(at: idx + slotOffset)
        } else {
            var leftWhite = m - 1
            while !isWhite(leftWhite) { leftWhite -= 1 }
            guard let leftIdx = whites.firstIndex(of: leftWhite) else { return nil }
            return blackRect(rightOfWhiteIndex: leftIdx + slotOffset)
        }
    }

    /// Drag-friendly hit test for piano keys only. Vertically forgiving (y
    /// can be anywhere); horizontally tolerates a small overshoot past the
    /// leftmost/rightmost white key so a drag rolling past the edge keeps
    /// the edge key sounding.
    static func noteAt(_ point: NSPoint) -> UInt8? {
        let whites = whiteList()
        let activeWhites = whites.filter { isActive($0) }
        guard !activeWhites.isEmpty else { return nil }
        let slotOffset = activeSlotOffset
        // Active keys occupy slots [slotOffset, slotOffset + activeWhites.count - 1].
        let leftEdge  = pianoOriginX + CGFloat(slotOffset) * whiteW
        let rightEdge = pianoOriginX + CGFloat(slotOffset + activeWhites.count) * whiteW
        let edgeTolerance: CGFloat = whiteW * 0.6
        guard point.x >= leftEdge - edgeTolerance,
              point.x < rightEdge + edgeTolerance else { return nil }

        // Black-key band: matches the visual blackRect exactly. Inactive
        // black keys are negative space — skipped.
        let blackYMin = pad + (whiteH - blackH)
        if point.x >= leftEdge && point.x < rightEdge && point.y >= blackYMin {
            var whiteIndex: [Int: Int] = [:]
            for (i, m) in whites.enumerated() { whiteIndex[m] = i }
            for m in firstMidi...lastMidi where !isWhite(m) {
                if !isActive(m) { continue }
                var leftWhite = m - 1
                while !isWhite(leftWhite) { leftWhite -= 1 }
                guard let leftIdx = whiteIndex[leftWhite] else { continue }
                let rect = blackRect(rightOfWhiteIndex: leftIdx + slotOffset)
                if point.x >= rect.minX && point.x < rect.maxX { return UInt8(m) }
            }
        }
        // White by slot within the active range; overshoot clamps to the
        // outermost active white.
        let clampedX = max(leftEdge, min(rightEdge - 0.001, point.x))
        let localCol = Int((clampedX - leftEdge) / whiteW)
        let clamped = max(0, min(activeWhites.count - 1, localCol))
        return UInt8(activeWhites[clamped])
    }

    // MARK: - Layout helpers

    private static func whiteRect(at index: Int) -> NSRect {
        let x = pianoOriginX + CGFloat(index) * whiteW
        return NSRect(x: x, y: pad, width: whiteW, height: whiteH)
    }

    private static func blackRect(rightOfWhiteIndex idx: Int) -> NSRect {
        let xCenter = pianoOriginX + CGFloat(idx + 1) * whiteW
        let x = xCenter - blackW / 2.0
        let y = pad + (whiteH - blackH)
        return NSRect(x: x, y: y, width: blackW, height: blackH)
    }

    private static func roundedKeyPath(rect: NSRect, tl: CGFloat, tr: CGFloat,
                                       br: CGFloat, bl: CGFloat) -> NSBezierPath {
        let path = NSBezierPath()
        let minX = rect.minX, maxX = rect.maxX, minY = rect.minY, maxY = rect.maxY
        path.move(to: NSPoint(x: minX + bl, y: minY))
        path.line(to: NSPoint(x: maxX - br, y: minY))
        if br > 0 {
            path.appendArc(withCenter: NSPoint(x: maxX - br, y: minY + br),
                           radius: br, startAngle: 270, endAngle: 360)
        }
        path.line(to: NSPoint(x: maxX, y: maxY - tr))
        if tr > 0 {
            path.appendArc(withCenter: NSPoint(x: maxX - tr, y: maxY - tr),
                           radius: tr, startAngle: 0, endAngle: 90)
        }
        path.line(to: NSPoint(x: minX + tl, y: maxY))
        if tl > 0 {
            path.appendArc(withCenter: NSPoint(x: minX + tl, y: maxY - tl),
                           radius: tl, startAngle: 90, endAngle: 180)
        }
        path.line(to: NSPoint(x: minX, y: minY + bl))
        if bl > 0 {
            path.appendArc(withCenter: NSPoint(x: minX + bl, y: minY + bl),
                           radius: bl, startAngle: 180, endAngle: 270)
        }
        path.close()
        return path
    }

    // MARK: - Key labels

    private static func drawWhiteLabel(_ text: String, in rect: NSRect, lit: Bool, alpha: CGFloat = 1.0) {
        guard alpha > 0.01 else { return }
        // Lit cells always wear pure-white labels (over the bright
        // accent fill). Unlit cells need to flip with the
        // appearance: dark-text in light mode (over a near-white
        // keycap) becomes light-text in dark mode (over a dark-gray
        // keycap).
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        let unlitBase = isDark
            ? NSColor(white: 0.85, alpha: 1.0)
            : NSColor(white: 0.28, alpha: 1.0)
        let base: NSColor = lit ? .white : unlitBase
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 9.0, weight: .heavy),
            .foregroundColor: base.withAlphaComponent(alpha),
        ]
        let str = NSAttributedString(string: text, attributes: attrs)
        let size = str.size()
        // White key labels sit a couple pixels off the bottom — high
        // enough that the descender on `j` doesn't kiss the menubar
        // edge, low enough that the letters feel anchored in the
        // bottom of the key rather than floating mid-cell.
        str.draw(at: NSPoint(x: rect.midX - size.width / 2,
                             y: rect.minY + 1.8))
    }

    private static func drawBlackLabel(_ text: String, in rect: NSRect, lit: Bool, alpha: CGFloat = 1.0) {
        guard alpha > 0.01 else { return }
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 8.0, weight: .heavy),
            .foregroundColor: NSColor.white.withAlphaComponent(0.96 * alpha),
        ]
        let str = NSAttributedString(string: text, attributes: attrs)
        let size = str.size()
        str.draw(at: NSPoint(x: rect.midX - size.width / 2,
                             y: rect.midY - size.height / 2))
    }

    /// Prefer Processing's bundled typeface for that Processing-IDE look. The
    /// font name varies across releases; try a few. Falls back to the system
    /// monospaced font (heavy) so the UI stays legible if it isn't installed.
    private static func processingFont(size: CGFloat) -> NSFont {
        let names = [
            "ProcessingSansPro-Bold",
            "Processing-Sans-Pro-Bold",
            "Processing Sans Pro Bold",
            "ProcessingSansPro-Regular",
            "Processing Sans Pro",
            "Processing",
        ]
        for n in names {
            if let f = NSFont(name: n, size: size) { return f }
        }
        return NSFont.monospacedSystemFont(ofSize: size, weight: .heavy)
    }

    // MARK: - Header buttons (flat, hover-aware)

    private static func drawHoverBackdrop(in rect: NSRect, hovered: Bool) {
        guard hovered else { return }
        let r = rect.insetBy(dx: 1.0, dy: 1.5)
        let path = NSBezierPath(roundedRect: r, xRadius: 3, yRadius: 3)
        NSColor.labelColor.withAlphaComponent(0.10).setFill()
        path.fill()
    }

    /// Music-notation chip — `music.note.list` reads like notes on a staff
    /// (a tiny scroll of music), more on-brand for a menubar instrument
    /// than a generic settings glyph. Tints accent + brightens when MIDI
    /// is sending to a DAW so you can see port status at a glance.
    ///
    /// Alternates considered: "music.quarternote.3", "pianokeys",
    /// "music.note", "scroll", "speaker.wave.2", "waveform".
    private static func drawSettingsChip(in _: NSRect, hoverRect _: NSRect,
                                         midiOn: Bool, hovered: Bool,
                                         flash: CGFloat = 0,
                                         voiceNumber: Int = 0) {
        // Standard systray pill: hover/click paints a soft rounded
        // backdrop centered on the icon glyph (NOT the full hit area, so
        // the piano-side empty space stays unhighlighted). Same look as
        // any other status-bar item.
        let iconBox = settingsIconRect
        if hovered {
            // Pill encompasses the music note AND the voice-badge
            // pad as one target — hovering either part highlights
            // the same chip, so the user gets unified feedback.
            let pill = NSRect(
                x: iconBox.minX - 1,
                y: iconBox.minY + 1,
                width: (iconBox.width + Self.voiceBadgeRightPad + 1),
                height: iconBox.height - 2
            )
            let path = NSBezierPath(roundedRect: pill, xRadius: 4, yRadius: 4)
            NSColor.labelColor.withAlphaComponent(0.12).setFill()
            path.fill()
        }
        let alpha: CGFloat = hovered ? 1.0 : (midiOn ? 1.0 : 0.78)
        let baseColor: NSColor = midiOn
            ? NSColor.controlAccentColor
            : NSColor.labelColor.withAlphaComponent(alpha)
        // Blend toward pure white based on `flash` (0..1). Used to
        // signal "activity" when the user taps an octave key or
        // plays a note — the music note icon briefly gets brighter
        // before settling back.
        let f = max(0, min(1, flash))
        let color = (f > 0.001)
            ? baseColor.blended(withFraction: f, of: .white) ?? baseColor
            : baseColor
        drawTintedSymbol("music.note.list", in: iconBox, pointSize: 13.0, color: color)
        // Voice-number subscript: tiny digits in the bottom-right
        // corner. The first digit sits where the single-digit case
        // looked good; additional digits FLOW RIGHT from there
        // (instead of growing leftward across the music-note glyph).
        // Single digit is the visual anchor; multi-digit values
        // extend toward / past the menubar slot's right edge.
        // Skipped for program 0 (default Acoustic Grand) so the
        // unmodified state stays uncluttered.
        if voiceNumber > 0 {
            let label = String(voiceNumber)
            // Negative kerning tightens the digits so multi-digit
            // values feel more like a tag than spaced text.
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedDigitSystemFont(ofSize: 7, weight: .heavy),
                .foregroundColor: color,
                .kern: -0.4,
            ]
            let str = NSAttributedString(string: label, attributes: attrs)
            // Width of a single "0" — anchor for the first digit.
            let oneDigit = NSAttributedString(string: "0", attributes: [
                .font: NSFont.monospacedDigitSystemFont(ofSize: 7, weight: .heavy),
            ]).size()
            // Nudge the first digit a small tad LEFT so it visually
            // hugs the music-note glyph; remaining digits flow
            // rightward into the reserved badge pad.
            let leftX = iconBox.maxX - oneDigit.width - 1
            str.draw(at: NSPoint(x: leftX, y: iconBox.minY - 1))
        }
    }

    private static func drawInstrumentLabel(in rect: NSRect, hoverRect: NSRect,
                                            program: UInt8, hovered: Bool) {
        drawHoverBackdrop(in: hoverRect, hovered: hovered)
        let safeIdx = max(0, min(127, Int(program)))
        let abbrev = GeneralMIDI.familyAbbrev(for: program)
        let label = String(format: "%@ %03d", abbrev, safeIdx)
        let alpha: CGFloat = hovered ? 1.0 : 0.82
        let attrs: [NSAttributedString.Key: Any] = [
            .font: processingFont(size: 10.0),
            .foregroundColor: NSColor.labelColor.withAlphaComponent(alpha),
            .kern: 0.2,
        ]
        let str = NSAttributedString(string: label, attributes: attrs)
        let size = str.size()
        str.draw(at: NSPoint(x: rect.midX - size.width / 2,
                             y: rect.midY - size.height / 2))
    }

    private static func drawTypeButton(in rect: NSRect, hoverRect: NSRect,
                                       on: Bool, hovered: Bool) {
        drawHoverBackdrop(in: hoverRect, hovered: hovered)
        let alpha: CGFloat = hovered ? 1.0 : 0.78
        let activeColor = NSColor.controlAccentColor.highlight(withLevel: 0.30)
            ?? NSColor.controlAccentColor
        let color: NSColor = on ? activeColor : NSColor.labelColor.withAlphaComponent(alpha)
        drawQwertyMap(in: rect, color: color)
    }

    private static func drawMIDIButton(in rect: NSRect, hoverRect: NSRect,
                                       on: Bool, hovered: Bool) {
        drawHoverBackdrop(in: hoverRect, hovered: hovered)
        let alpha: CGFloat = hovered ? 1.0 : 0.78
        let activeColor = NSColor.controlAccentColor.highlight(withLevel: 0.30)
            ?? NSColor.controlAccentColor
        let color: NSColor = on ? activeColor : NSColor.labelColor.withAlphaComponent(alpha)

        // Layout: [music note] [tiny "midi"] inline, centered together.
        let iconSize: CGFloat = 11.0
        let textGap: CGFloat = 1.6
        let textAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 6.5, weight: .heavy),
            .foregroundColor: color,
            .kern: 0.2,
        ]
        let textStr = NSAttributedString(string: "midi", attributes: textAttrs)
        let textSize = textStr.size()
        let totalW = iconSize + textGap + textSize.width
        let startX = rect.midX - totalW / 2
        let iconRect = NSRect(x: startX, y: rect.midY - iconSize / 2,
                              width: iconSize, height: iconSize)
        drawTintedSymbol("music.note", in: iconRect, pointSize: 10.0, color: color)
        textStr.draw(at: NSPoint(x: startX + iconSize + textGap,
                                 y: rect.midY - textSize.height / 2))
    }

    /// Tiny stylized QWERTY layout — three staggered rows of small rounded
    /// rects, like a typewriter keyboard from above.
    private static func drawQwertyMap(in rect: NSRect, color: NSColor) {
        color.setFill()
        let rows = [4, 4, 3]
        let rowOffsets: [CGFloat] = [0.0, 0.5, 1.0]  // typewriter stagger
        let keySize: CGFloat = 1.9
        let hSpace: CGFloat = 0.5
        let vSpace: CGFloat = 0.6
        let maxKeys = rows.max() ?? 4
        let gridW = CGFloat(maxKeys) * keySize + CGFloat(maxKeys - 1) * hSpace
        let gridH = CGFloat(rows.count) * keySize + CGFloat(rows.count - 1) * vSpace
        let originX = rect.midX - gridW / 2
        let originY = rect.midY - gridH / 2
        for (rIdx, count) in rows.enumerated() {
            let y = originY + CGFloat(rows.count - 1 - rIdx) * (keySize + vSpace)
            let xOff = rowOffsets[rIdx] * (keySize + hSpace)
            for col in 0..<count {
                let x = originX + xOff + CGFloat(col) * (keySize + hSpace)
                let r = NSRect(x: x, y: y, width: keySize, height: keySize)
                NSBezierPath(roundedRect: r, xRadius: 0.35, yRadius: 0.35).fill()
            }
        }
    }

    /// Render an SF Symbol tinted to an arbitrary color. Templates alone
    /// don't reliably take a fill color when drawn outside a button, so we
    /// composite the symbol into a fresh image and use sourceIn to colour it.
    private static func drawTintedSymbol(_ name: String, in rect: NSRect,
                                         pointSize: CGFloat, color: NSColor) {
        guard let base = NSImage(systemSymbolName: name, accessibilityDescription: nil) else { return }
        let config = NSImage.SymbolConfiguration(pointSize: pointSize, weight: .bold)
        let configured = base.withSymbolConfiguration(config) ?? base
        let size = configured.size

        let tinted = NSImage(size: size)
        tinted.lockFocus()
        configured.draw(in: NSRect(origin: .zero, size: size),
                        from: .zero, operation: .sourceOver, fraction: 1.0)
        color.set()
        NSRect(origin: .zero, size: size).fill(using: .sourceIn)
        tinted.unlockFocus()

        let drawRect = NSRect(
            x: rect.midX - size.width / 2,
            y: rect.midY - size.height / 2,
            width: size.width, height: size.height
        )
        tinted.draw(in: drawRect)
    }
}
