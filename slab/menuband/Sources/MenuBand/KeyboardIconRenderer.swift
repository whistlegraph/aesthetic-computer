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
    static let firstMidi: Int = 60   // C4 (middle C)
    static let lastMidi: Int = 83    // B5 — stop at the last QWERTY-letter key
                                     // (skip ;, ', ] which sit beyond the row)

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
    static let settingsGap: CGFloat = 4.0

    enum HitResult: Equatable {
        case openSettings
        case note(UInt8)
    }

    // Notepat layout key letters keyed by MIDI note. A3 (57) has no label
    // because notepat assigns it to the Control modifier rather than a letter.
    static let labelByMidi: [Int: String] = [
        58: "z", 59: "x", 60: "c", 61: "v", 62: "d", 63: "s",
        64: "e", 65: "f", 66: "w", 67: "g", 68: "r", 69: "a",
        70: "q", 71: "b", 72: "h", 73: "t", 74: "i", 75: "y",
        76: "j", 77: "k", 78: "u", 79: "l", 80: "o", 81: "m",
        82: "p", 83: "n", 84: ";", 85: "'", 86: "]",
    ]

    @inline(__always)
    private static func isWhite(_ midi: Int) -> Bool {
        switch midi % 12 {
        case 0, 2, 4, 5, 7, 9, 11: return true
        default: return false
        }
    }

    private static func whiteList() -> [Int] {
        (firstMidi...lastMidi).filter { isWhite($0) }
    }

    private static var pianoWidth: CGFloat {
        CGFloat(whiteList().count) * whiteW
    }

    static var imageSize: NSSize {
        let totalW = ceil(pad + pianoWidth + settingsGap + settingsW + pad)
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
                      hovered: HitResult? = nil) -> NSImage {
        let whites = whiteList()
        var whiteIndex: [Int: Int] = [:]
        for (i, m) in whites.enumerated() { whiteIndex[m] = i }
        let size = imageSize

        let img = NSImage(size: size, flipped: false) { _ in

            // Piano.
            NSGraphicsContext.saveGraphicsState()
            // Touched / active = a brighter version of the user's accent
            // color (controlAccentColor lightened ~25% toward white) so the
            // pressed state pops without leaving the accent palette.
            let lit = NSColor.controlAccentColor.highlight(withLevel: 0.30)
                ?? NSColor.controlAccentColor
            let groove = NSColor.black.withAlphaComponent(0.55)
            let whiteHi = NSColor.white
            let whiteLo = NSColor(white: 0.88, alpha: 1.0)
            let blackHi = NSColor.controlAccentColor.shadow(withLevel: 0.30) ?? NSColor.controlAccentColor
            let blackLo = NSColor.controlAccentColor.shadow(withLevel: 0.55) ?? NSColor.controlAccentColor

            let leftmostMidi = firstMidi      // C4 is the leftmost drawn white
            let rightmostMidi = lastMidi      // B5
            for (idx, m) in whites.enumerated() {
                let rect = whiteRect(at: idx)
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
                if typeMode, let letter = labelByMidi[m] {
                    drawWhiteLabel(letter, in: rect, lit: isLit)
                }
            }
            for m in firstMidi...lastMidi where !isWhite(m) {
                var leftWhite = m - 1
                while !isWhite(leftWhite) { leftWhite -= 1 }
                guard let leftIdx = whiteIndex[leftWhite] else { continue }
                let rect = blackRect(rightOfWhiteIndex: leftIdx)
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
                if typeMode, let letter = labelByMidi[m] {
                    drawBlackLabel(letter, in: rect, lit: isLit)
                }
            }
            NSGraphicsContext.restoreGraphicsState()

            // Single accent-colored settings chip with a tiny dropdown
            // chevron — voice/qwerty/midi all live behind this one click.
            drawSettingsChip(in: settingsRect, hoverRect: settingsHitRect,
                             anyActive: enabled || typeMode,
                             hovered: hovered == .openSettings)
            return true
        }
        img.isTemplate = false
        return img
    }

    /// Public so the popover can anchor its arrow at the latch.
    static var settingsRectPublic: NSRect { settingsHitRect }

    // Settings button hit area extends from piano's right edge to the image
    // edge so the entire right-side region is one big click target.
    private static var settingsHitRect: NSRect {
        let leftX = pad + pianoWidth
        let rightX = imageSize.width
        return NSRect(x: leftX, y: 0, width: rightX - leftX, height: imageSize.height)
    }

    // MARK: - Hit testing

    static func hit(at point: NSPoint) -> HitResult? {
        if settingsHitRect.contains(point) { return .openSettings }
        let whites = whiteList()
        var whiteIndex: [Int: Int] = [:]
        for (i, m) in whites.enumerated() { whiteIndex[m] = i }
        // Black-key hit area = the visual blackRect. 1:1 mapping with what
        // the user sees on screen — clicking on visible black triggers black,
        // clicking visible white triggers white.
        for m in firstMidi...lastMidi where !isWhite(m) {
            var leftWhite = m - 1
            while !isWhite(leftWhite) { leftWhite -= 1 }
            guard let leftIdx = whiteIndex[leftWhite] else { continue }
            if blackRect(rightOfWhiteIndex: leftIdx).contains(point) {
                return .note(UInt8(m))
            }
        }
        // White keys: y is unbounded so any cursor-y inside the button maps
        // to the white at that x — even when the menubar adds an extra pixel
        // or two of padding above/below the image, those edge clicks still
        // register as the underlying white. Black-band check above already
        // claims the black-key region; everything else falls through here.
        for (idx, m) in whites.enumerated() {
            let r = whiteRect(at: idx)
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
        if isWhite(m) {
            guard let idx = whites.firstIndex(of: m) else { return nil }
            return whiteRect(at: idx)
        } else {
            var leftWhite = m - 1
            while !isWhite(leftWhite) { leftWhite -= 1 }
            guard let leftIdx = whites.firstIndex(of: leftWhite) else { return nil }
            return blackRect(rightOfWhiteIndex: leftIdx)
        }
    }

    /// Drag-friendly hit test for piano keys only. Vertically forgiving (y
    /// can be anywhere); horizontally tolerates a small overshoot past the
    /// leftmost/rightmost white key so a drag rolling past the edge keeps
    /// the edge key sounding.
    static func noteAt(_ point: NSPoint) -> UInt8? {
        let whites = whiteList()
        let leftEdge = pianoOriginX
        let rightEdge = pianoOriginX + CGFloat(whites.count) * whiteW
        let edgeTolerance: CGFloat = whiteW * 0.6
        guard point.x >= leftEdge - edgeTolerance,
              point.x < rightEdge + edgeTolerance else { return nil }

        // Black-key band: matches the visual blackRect exactly.
        let blackYMin = pad + (whiteH - blackH)
        if point.x >= leftEdge && point.x < rightEdge && point.y >= blackYMin {
            var whiteIndex: [Int: Int] = [:]
            for (i, m) in whites.enumerated() { whiteIndex[m] = i }
            for m in firstMidi...lastMidi where !isWhite(m) {
                var leftWhite = m - 1
                while !isWhite(leftWhite) { leftWhite -= 1 }
                guard let leftIdx = whiteIndex[leftWhite] else { continue }
                let rect = blackRect(rightOfWhiteIndex: leftIdx)
                if point.x >= rect.minX && point.x < rect.maxX { return UInt8(m) }
            }
        }
        // White by column, clamping x into the piano range so overshoot maps
        // to the leftmost/rightmost key.
        let clampedX = max(leftEdge, min(rightEdge - 0.001, point.x))
        let col = Int((clampedX - leftEdge) / whiteW)
        let clamped = max(0, min(whites.count - 1, col))
        return UInt8(whites[clamped])
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

    private static func drawWhiteLabel(_ text: String, in rect: NSRect, lit: Bool) {
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 9.0, weight: .heavy),
            .foregroundColor: lit ? NSColor.white : NSColor(white: 0.28, alpha: 1.0),
        ]
        let str = NSAttributedString(string: text, attributes: attrs)
        let size = str.size()
        str.draw(at: NSPoint(x: rect.midX - size.width / 2, y: rect.minY + 0.4))
    }

    private static func drawBlackLabel(_ text: String, in rect: NSRect, lit: Bool) {
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 8.0, weight: .heavy),
            .foregroundColor: NSColor.white.withAlphaComponent(0.96),
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

    /// `slider.horizontal.3` — three audio-mixer-style sliders, generic
    /// enough to cover TYPE / MIDI / Instrument / Octave / About without
    /// committing to one specific musical concept. Flat monochrome, blends
    /// with native menubar icons. Tints accent when any mode is active.
    ///
    /// Alternates: "ellipsis", "gearshape", "waveform", "music.note",
    /// "speaker.wave.2", "metronome", "switch.2".
    private static func drawSettingsChip(in rect: NSRect, hoverRect _: NSRect,
                                         anyActive: Bool, hovered: Bool) {
        let alpha: CGFloat = hovered ? 1.0 : 0.78
        let color: NSColor = anyActive
            ? NSColor.controlAccentColor
            : NSColor.labelColor.withAlphaComponent(alpha)
        drawTintedSymbol("slider.horizontal.3", in: rect, pointSize: 11.0, color: color)
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
