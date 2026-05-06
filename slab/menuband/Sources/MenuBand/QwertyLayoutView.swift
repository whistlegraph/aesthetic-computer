import AppKit

/// Tiny QWERTY keyboard map drawn into the popover. Three rows of
/// keycaps in the macOS layout (top row offset 0, home row 0.5, bottom
/// row 1.0) — same row-offset logic the QWERTY-pan uses. Note-mapped
/// keys are tinted with the current voice's family color so the user
/// can see at a glance which physical keys play notes; currently-held
/// keys light up with the accent color.
final class QwertyLayoutView: NSView {
    /// Hardware key codes currently held by the user. Driven from the
    /// controller's `heldKeyCodes` snapshot. Updating triggers a
    /// redraw.
    var litKeyCodes: Set<UInt16> = [] {
        didSet { needsDisplay = true }
    }
    /// Keymap variant — controls which key codes are colored as
    /// "note-mapped" (Notepat = 2-octave layout, Ableton = Live's
    /// M-mode QWERTY).
    var keymap: Keymap = .notepat {
        didSet { needsDisplay = true }
    }
    /// Current voice's family color — used to tint note-mapped
    /// keycaps so the keymap reads as part of the chosen instrument.
    var voiceColor: NSColor = .controlAccentColor {
        didSet { needsDisplay = true }
    }

    /// Pointer-driven key event. Fires on mouseDown over a cap (isDown=true),
    /// when the cursor drags off that cap onto another (isDown=false for the
    /// old, true for the new), and on mouseUp (isDown=false). The popover
    /// wires this to `MenuBandController.handleLocalKey` so taps/drags on
    /// the layout map play the same notes the physical keyboard would.
    var onKey: ((_ keyCode: UInt16, _ isDown: Bool) -> Void)?

    /// Hardware key code currently held by the pointer, if any. Lets us
    /// release-then-press when a drag crosses into a new cap and clean
    /// up on mouseUp.
    private var heldByPointer: UInt16?

    // 5 rows of 14pt caps + 4 row gaps of 1pt = 74pt + a touch of
    // breathing room. (Was 60pt back when there were 4 rows.)
    static let intrinsicSize = NSSize(width: 180, height: 76)
    /// Multiplier applied to all keycap dimensions + label font size.
    /// Default 1.0 = popover layout (compact). The floating play
    /// palette sets a larger value so the keymap fills the overlay.
    var scale: CGFloat = 1.0 {
        didSet {
            invalidateIntrinsicContentSize()
            needsDisplay = true
        }
    }
    override var intrinsicContentSize: NSSize {
        NSSize(width: Self.intrinsicSize.width * scale,
               height: Self.intrinsicSize.height * scale)
    }
    private var scaledKeySize: CGFloat { Self.keySize * scale }
    private var scaledKeyGap: CGFloat { Self.keyGap * scale }
    private var scaledCornerRadius: CGFloat { Self.cornerRadius * scale }
    private var scaledLabelFontSize: CGFloat { 8.5 * scale }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setFrameSize(Self.intrinsicSize)
    }
    required init?(coder: NSCoder) { fatalError() }

    override var isFlipped: Bool { false }
    override var mouseDownCanMoveWindow: Bool { false }

    /// Cap descriptor — `width` is in standard-key units (1.0 = a
    /// regular letter cap; shift = 1.5, space = 5.0). `altLabel`
    /// stacks above `label` (smaller) so caps that double as
    /// shifted-glyph controls (`,` / `<`, `.` / `>`) read like the
    /// printing on a real keycap.
    private struct Cap {
        let kc: UInt16
        let label: String
        let altLabel: String?
        let width: CGFloat
        init(_ kc: UInt16, _ label: String, alt: String? = nil, width: CGFloat = 1.0) {
            self.kc = kc
            self.label = label
            self.altLabel = alt
            self.width = width
        }
    }

    /// Five-row macOS QWERTY layout — keys identified by `kVK_*`
    /// codes, mirroring `MenuBandLayout.panByKeyCode` so the visual
    /// position reflects the keypad-driven pan. Row 0 holds the
    /// number row (1-0, voice picker); row 3 includes the notepat
    /// octave keys (`,` and `.`); row 4 holds shift / space / shift
    /// so the linger-mode and metronome-toggle keys are visible
    /// alongside the note caps.
    private static let rows: [[Cap]] = [
        // Number row: 1 2 3 4 5 6 7 8 9 0  (voice picker — typing
        // a digit selects the GM voice with that 1-based index;
        // 0 is MIDI passthrough).
        [Cap(18, "1"), Cap(19, "2"), Cap(20, "3"), Cap(21, "4"), Cap(23, "5"),
         Cap(22, "6"), Cap(26, "7"), Cap(28, "8"), Cap(25, "9"), Cap(29, "0")],
        // Top row: q w e r t y u i o p ]
        [Cap(12, "q"), Cap(13, "w"), Cap(14, "e"), Cap(15, "r"), Cap(17, "t"),
         Cap(16, "y"), Cap(32, "u"), Cap(34, "i"), Cap(31, "o"), Cap(35, "p"), Cap(30, "]")],
        // Home row: a s d f g h j k l ; '
        [Cap(0, "a"), Cap(1, "s"), Cap(2, "d"), Cap(3, "f"), Cap(5, "g"),
         Cap(4, "h"), Cap(38, "j"), Cap(40, "k"), Cap(37, "l"), Cap(41, ";"), Cap(39, "'")],
        // Bottom row: z x c v b n m , .
        // The , and . keys double as octave shift via their shifted
        // glyphs (< and >), so render both labels stacked on those
        // caps — matches what's printed on a real keyboard.
        [Cap(6, "z"), Cap(7, "x"), Cap(8, "c"), Cap(9, "v"), Cap(11, "b"),
         Cap(45, "n"), Cap(46, "m"), Cap(43, ",", alt: "<"), Cap(47, ".", alt: ">")],
        // Modifier / space row: shift   space   shift — no glyph
        // on the space bar; the wide blank cap reads as space.
        [Cap(56, "⇧", width: 1.6), Cap(49, "", width: 5.5), Cap(60, "⇧", width: 1.6)],
    ]
    private static let rowOffsets: [CGFloat] = [0, 0, 0.5, 1.0, 0.0]
    /// Hardware key codes for the number row (1-0). Used by the
    /// renderer to pick a voice-control accent style — same muted-
    /// accent treatment the octave keys get, since both rows are
    /// "control" rather than "note".
    private static let voiceKeyCodes: Set<UInt16> =
        [18, 19, 20, 21, 23, 22, 26, 28, 25, 29]
    private static let keySize: CGFloat = 14
    private static let keyGap: CGFloat = 1
    private static let cornerRadius: CGFloat = 2.5

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        forEachVisibleCap { cap, rect in
            let st = semitone(cap.kc)
            let octaves = MenuBandLayout.octaveKeyCodes(for: keymap)
            let isOctaveKey = (cap.kc == octaves.down || cap.kc == octaves.up)
            let isVoiceKey = Self.voiceKeyCodes.contains(cap.kc)
            let black = (st.map { Self.isBlackKey(semitone: $0) }) ?? false
            let isLit = litKeyCodes.contains(cap.kc)
            drawKeycap(rect: rect, label: cap.label, altLabel: cap.altLabel,
                        mapped: st != nil, isBlack: black, lit: isLit,
                        // Both octave keys and voice (number-row) keys
                        // are "control" caps that get the same muted-
                        // accent treatment so the user can tell at a
                        // glance which presses change state vs play
                        // a note.
                        isOctaveKey: isOctaveKey || isVoiceKey)
        }
    }

    /// Walk the visible caps in draw-order, handing each its layout
    /// rect. Hit testing and drawing share this so a cap rendered on
    /// screen is exactly the cap a click on that pixel resolves to.
    /// Skips unmapped Ableton keys (matching the draw-time hide rule)
    /// so a click on empty space doesn't accidentally trigger a key
    /// that isn't there.
    private func forEachVisibleCap(_ body: (_ cap: Cap, _ rect: NSRect) -> Void) {
        let r = bounds
        let kSize = scaledKeySize
        let kGap = scaledKeyGap
        let totalRows = CGFloat(Self.rows.count)
        let rowSpan = totalRows * kSize + (totalRows - 1) * kGap
        let topY = r.midY + rowSpan / 2 - kSize
        let octaves = MenuBandLayout.octaveKeyCodes(for: keymap)
        // Pre-compute the home row's total span as the centering
        // reference so all rows share the same horizontal anchor.
        // Home row is now at index 2 (number → q → home → z → mod).
        let homeRowIdx = 2
        let homeRow = Self.rows[homeRowIdx]
        let homeWidth = homeRow.reduce(0.0) { $0 + $1.width * kSize } +
            CGFloat(homeRow.count - 1) * kGap +
            Self.rowOffsets[homeRowIdx] * kSize
        let centerX = r.midX
        for (rIdx, row) in Self.rows.enumerated() {
            let y = topY - CGFloat(rIdx) * (kSize + kGap)
            let xOffset = Self.rowOffsets[rIdx] * kSize
            // Sum the row's visible widths so we can right-truncate
            // it (Ableton hides unmapped letters) and still center
            // the row's caps relative to the home row's left edge.
            // For the modifier row (4) we instead center on the
            // panel midline so shift / space / shift sit centered.
            var rowWidth: CGFloat = 0
            var visibleCount = 0
            for cap in row {
                let st = semitone(cap.kc)
                let isOct = (cap.kc == octaves.down || cap.kc == octaves.up)
                if keymap == .ableton && st == nil && !isOct
                    && !Self.isModifierKey(cap.kc) { continue }
                rowWidth += cap.width * kSize
                visibleCount += 1
            }
            if visibleCount > 0 {
                rowWidth += CGFloat(visibleCount - 1) * kGap
            }
            let leftX: CGFloat
            if rIdx == Self.rows.count - 1 {
                // Modifier row centers on the panel midline so the
                // shift / space / shift cluster reads as the bottom
                // of a real keyboard.
                leftX = centerX - rowWidth / 2
            } else {
                leftX = centerX - homeWidth / 2 + xOffset
            }
            var cursorX = leftX
            for cap in row {
                let st = semitone(cap.kc)
                let isOct = (cap.kc == octaves.down || cap.kc == octaves.up)
                if keymap == .ableton && st == nil && !isOct
                    && !Self.isModifierKey(cap.kc) { continue }
                let w = cap.width * kSize
                let kr = NSRect(x: cursorX, y: y, width: w, height: kSize)
                body(cap, kr)
                cursorX += w + kGap
            }
        }
    }

    /// Shift / space — non-note caps that should always render even
    /// in keymap variants that hide unmapped letters.
    private static func isModifierKey(_ kc: UInt16) -> Bool {
        kc == 49 || kc == 56 || kc == 60
    }

    /// Hit-test a point in view coordinates against the visible caps.
    /// Returns the matching key code or nil if the point misses every
    /// cap (gaps between keys, empty area around the layout).
    private func keyCode(at point: NSPoint) -> UInt16? {
        var hit: UInt16?
        forEachVisibleCap { cap, rect in
            if hit == nil && rect.contains(point) { hit = cap.kc }
        }
        return hit
    }

    // MARK: - Pointer events
    //
    // mouseDown / mouseDragged / mouseUp fire onKey so a tap on a cap
    // plays the same note the matching physical key would. Drag rolls
    // across caps with proper release-then-press so chords don't get
    // stuck and notes glissando smoothly.

    override func mouseDown(with event: NSEvent) {
        let p = convert(event.locationInWindow, from: nil)
        guard let kc = keyCode(at: p) else { return }
        heldByPointer = kc
        onKey?(kc, true)
    }

    override func mouseDragged(with event: NSEvent) {
        let p = convert(event.locationInWindow, from: nil)
        let kc = keyCode(at: p)
        if kc == heldByPointer { return }
        if let prev = heldByPointer { onKey?(prev, false) }
        heldByPointer = kc
        if let new = kc { onKey?(new, true) }
    }

    override func mouseUp(with event: NSEvent) {
        if let prev = heldByPointer { onKey?(prev, false) }
        heldByPointer = nil
    }

    /// Returns the semitone offset for a key in the active keymap, or
    /// nil if unmapped. Used to know whether the key plays a note +
    /// whether that note is a white or black piano key.
    private func semitone(_ kc: UInt16) -> Int? {
        guard kc < 128 else { return nil }
        let table = (keymap == .ableton)
            ? MenuBandLayout.semitoneByKeyCodeAbleton
            : MenuBandLayout.semitoneByKeyCode
        let v = table[Int(kc)]
        return (v == Int8.min) ? nil : Int(v)
    }

    /// Black keys = sharps/flats: pitch classes 1, 3, 6, 8, 10
    /// (C#, D#, F#, G#, A#). White = the rest. We mod by 12 against
    /// middle C (semitone 0 → C natural) so the mapping holds across
    /// octave shifts.
    private static func isBlackKey(semitone: Int) -> Bool {
        let pc = ((semitone % 12) + 12) % 12
        switch pc {
        case 1, 3, 6, 8, 10: return true
        default:             return false
        }
    }

    private func drawKeycap(rect: NSRect, label: String,
                             altLabel: String? = nil,
                             mapped: Bool, isBlack: Bool, lit: Bool,
                             isOctaveKey: Bool = false) {
        let path = NSBezierPath(roundedRect: rect,
                                 xRadius: scaledCornerRadius,
                                 yRadius: scaledCornerRadius)
        // Fills mirror the menubar piano: white-key-mapped letters
        // get a near-white cap, black-key-mapped letters get a
        // near-black cap. Octave-shift keys get a muted accent fill
        // so they read as "control" rather than note. Unmapped
        // letters stay outline-only. Lit overrides everything with
        // accent color so the user can see what they're playing in
        // real time. The instrument's family color stays out of the
        // picture so the keymap reads as "this is which physical
        // keys play piano keys" rather than another voice-colored
        // ornament.
        if lit {
            NSColor.controlAccentColor.withAlphaComponent(0.85).setFill()
            path.fill()
        } else if isOctaveKey && !mapped {
            NSColor.controlAccentColor.withAlphaComponent(0.18).setFill()
            path.fill()
        } else if mapped && isBlack {
            NSColor(white: 0.08, alpha: 1.0).setFill()
            path.fill()
        } else if mapped {
            NSColor(white: 0.92, alpha: 1.0).setFill()
            path.fill()
        }
        let stroke: NSColor
        if lit {
            stroke = NSColor.controlAccentColor
        } else if isOctaveKey && !mapped {
            stroke = NSColor.controlAccentColor.withAlphaComponent(0.65)
        } else {
            stroke = NSColor.labelColor.withAlphaComponent(mapped ? 0.45 : 0.30)
        }
        stroke.setStroke()
        path.lineWidth = 0.7
        path.stroke()
        // Letter glyph centered in the cap, color follows the piano-
        // key convention: white caps get black text, black caps get
        // white text. Octave-shift caps wear the accent color.
        let textColor: NSColor
        if lit {
            textColor = .black
        } else if isOctaveKey && !mapped {
            textColor = .controlAccentColor
        } else if mapped && isBlack {
            textColor = .white
        } else if mapped {
            textColor = NSColor(white: 0.10, alpha: 1.0)
        } else {
            textColor = NSColor.labelColor.withAlphaComponent(0.55)
        }
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: scaledLabelFontSize, weight: .heavy),
            .foregroundColor: textColor,
        ]
        let s = NSAttributedString(string: label, attributes: attrs)
        let size = s.size()
        if let altLabel = altLabel, !altLabel.isEmpty {
            // Stacked label — shifted glyph (alt) on top, base glyph
            // (label) below, mirroring what's printed on a real
            // keycap. Both labels are scaled down a touch so the
            // pair fits inside the small cap without clipping.
            let stackedFontSize = scaledLabelFontSize * 0.78
            let stackedAttrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: stackedFontSize, weight: .heavy),
                .foregroundColor: textColor,
            ]
            let stackedBase = NSAttributedString(string: label, attributes: stackedAttrs)
            let stackedAlt = NSAttributedString(string: altLabel, attributes: stackedAttrs)
            let baseSz = stackedBase.size()
            let altSz = stackedAlt.size()
            // Glyph extents (a comma's tight ascent vs. an angle
            // bracket's full ascent) make `.size().height` lie
            // about visual height; cap font ascender for spacing.
            let lineH = stackedFontSize * 0.95
            let stackGap: CGFloat = -lineH * 0.25  // overlap to compress
            let totalH = lineH + stackGap + lineH
            let topGlyphBaselineY = rect.midY + totalH / 2 - lineH * 0.85
            let bottomGlyphBaselineY = topGlyphBaselineY - lineH - stackGap
            stackedAlt.draw(at: NSPoint(x: rect.midX - altSz.width / 2,
                                         y: topGlyphBaselineY))
            stackedBase.draw(at: NSPoint(x: rect.midX - baseSz.width / 2,
                                          y: bottomGlyphBaselineY))
        } else {
            s.draw(at: NSPoint(x: rect.midX - size.width / 2,
                               y: rect.midY - size.height / 2 - 0.5))
        }
    }
}
// rebuild marker 1777878530
