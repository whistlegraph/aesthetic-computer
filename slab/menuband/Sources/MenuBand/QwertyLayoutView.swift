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

    static let intrinsicSize = NSSize(width: 180, height: 46)
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

    /// Three-row macOS QWERTY layout — keys identified by `kVK_*`
    /// codes, mirroring `MenuBandLayout.panByKeyCode` so the visual
    /// position reflects the keypad-driven pan.
    private static let rows: [[(kc: UInt16, label: String)]] = [
        // Top row: q w e r t y u i o p ]
        [(12, "q"), (13, "w"), (14, "e"), (15, "r"), (17, "t"),
         (16, "y"), (32, "u"), (34, "i"), (31, "o"), (35, "p"), (30, "]")],
        // Home row: a s d f g h j k l ; '
        [(0, "a"), (1, "s"), (2, "d"), (3, "f"), (5, "g"),
         (4, "h"), (38, "j"), (40, "k"), (37, "l"), (41, ";"), (39, "'")],
        // Bottom row: z x c v b n m
        [(6, "z"), (7, "x"), (8, "c"), (9, "v"), (11, "b"),
         (45, "n"), (46, "m")],
    ]
    private static let rowOffsets: [CGFloat] = [0, 0.5, 1.0]
    private static let keySize: CGFloat = 14
    private static let keyGap: CGFloat = 1
    private static let cornerRadius: CGFloat = 2.5

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        forEachVisibleCap { cap, rect in
            let st = semitone(cap.kc)
            let octaves = MenuBandLayout.octaveKeyCodes(for: keymap)
            let isOctaveKey = (cap.kc == octaves.down || cap.kc == octaves.up)
            let black = (st.map { Self.isBlackKey(semitone: $0) }) ?? false
            let isLit = litKeyCodes.contains(cap.kc)
            drawKeycap(rect: rect, label: cap.label,
                        mapped: st != nil, isBlack: black, lit: isLit,
                        isOctaveKey: isOctaveKey)
        }
    }

    /// Walk the visible caps in draw-order, handing each its layout
    /// rect. Hit testing and drawing share this so a cap rendered on
    /// screen is exactly the cap a click on that pixel resolves to.
    /// Skips unmapped Ableton keys (matching the draw-time hide rule)
    /// so a click on empty space doesn't accidentally trigger a key
    /// that isn't there.
    private func forEachVisibleCap(_ body: (_ cap: (kc: UInt16, label: String), _ rect: NSRect) -> Void) {
        let r = bounds
        let kSize = scaledKeySize
        let kGap = scaledKeyGap
        let totalRows = CGFloat(Self.rows.count)
        let rowSpan = totalRows * kSize + (totalRows - 1) * kGap
        let topY = r.midY + rowSpan / 2 - kSize
        let homeRowSpan = CGFloat(Self.rows[1].count) * kSize +
            CGFloat(Self.rows[1].count - 1) * kGap +
            Self.rowOffsets[1] * kSize
        let leftX = r.midX - homeRowSpan / 2
        let octaves = MenuBandLayout.octaveKeyCodes(for: keymap)
        for (rIdx, row) in Self.rows.enumerated() {
            let y = topY - CGFloat(rIdx) * (kSize + kGap)
            let xOffset = Self.rowOffsets[rIdx] * kSize
            for (cIdx, cap) in row.enumerated() {
                let st = semitone(cap.kc)
                let isOctaveKey = (cap.kc == octaves.down || cap.kc == octaves.up)
                if keymap == .ableton && st == nil && !isOctaveKey { continue }
                let x = leftX + xOffset + CGFloat(cIdx) * (kSize + kGap)
                let kr = NSRect(x: x, y: y, width: kSize, height: kSize)
                body(cap, kr)
            }
        }
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
        s.draw(at: NSPoint(x: rect.midX - size.width / 2,
                           y: rect.midY - size.height / 2 - 0.5))
    }
}
