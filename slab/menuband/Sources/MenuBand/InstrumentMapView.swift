import AppKit
import CoreVideo

/// Tiny numeric flat-map of all 128 General MIDI programs. 8 columns × 16
/// rows, one cell per program — number-only, family-colored background.
/// No scrolling, no names: a glance shows the whole instrument set.
/// Click a cell → onCommit; the popover commits the program and plays a
/// preview note in it.
///
/// Doubles as a low-resolution audio-reactive LED matrix: while the popover
/// is visible, each column shows the recent peak amplitude of one slice of
/// the live waveform, lighting cells from the bottom upward as a vintage
/// segmented meter. The grid stays clickable underneath.
///
/// Replaces the scrollable named-list because the named list ate the
/// popover's vertical real estate. The numeric map is ~224 × 224 px and
/// fits comfortably alongside the rest of the controls.
final class InstrumentListView: NSView {
    /// Set by the popover so the view can snapshot the synth's tap ring.
    weak var menuBand: MenuBandController?
    static let cols = 8
    static let rows = 16
    static let cellW: CGFloat = 28
    static let cellH: CGFloat = 14
    /// Special "instrument 0" addressable slot that lives above the
    /// 1-128 grid. Picking it engages MIDI passthrough mode (notes go
    /// out the virtual port instead of the internal synth). Replaces
    /// the old in-popover MIDI toggle switch with a visual peer of the
    /// patch slots, so the chooser is the single addressing surface.
    static let midiOutH: CGFloat = 18
    static let midiOutGap: CGFloat = 3

    static let preferredWidth:  CGFloat = cellW * CGFloat(cols)    // 224
    static let preferredHeight: CGFloat = midiOutH + midiOutGap
        + cellH * CGFloat(rows)                                    // 245

    var selectedProgram: UInt8 = 0 { didSet { needsDisplay = true } }
    private(set) var hoveredProgram: UInt8?
    /// Lit when the controller's `midiMode` is on. Drives the
    /// MIDI-OUT cell's filled/outlined appearance and tints the
    /// rest of the grid as deselected.
    var midiModeActive: Bool = false { didSet { needsDisplay = true } }
    var onCommit: ((Int) -> Void)?
    /// Fires when the user clicks the MIDI-OUT cell at the top of the
    /// grid. The popover wires this to `menuBand.toggleMIDIMode()`.
    var onMidiOutCommit: (() -> Void)?
    /// Fires whenever the hovered cell changes (including transitions to
    /// "no hover" → nil). Drives the controller's hover-preview note for
    /// sonic browsing.
    var onHover: ((Int?) -> Void)?
    /// Fires when an arrow key is processed. Reports the direction (0=←,
    /// 1=→, 2=↓, 3=↑) and whether the key is going down (true) or up
    /// (false). Used by the popover so the on-screen arrow-keys hint
    /// can highlight the specific direction while held.
    var onArrowKey: ((Int, Bool) -> Void)?
    /// Forwarded keyDown / keyUp for non-arrow keys. The popover wires
    /// this to `menuBand.handleLocalKey` so notepat / Ableton letter
    /// keys still play notes while the user is browsing the voice
    /// palette. Returns true when the key has been consumed as a
    /// music event.
    var onMusicKey: ((UInt16, Bool, Bool, NSEvent.ModifierFlags) -> Bool)?

    private var trackingArea: NSTrackingArea?

    override var isFlipped: Bool { true }   // top-down rows, reading order
    /// Cells are clickable + drag-target — let `panel.isMovableByWindowBackground`
    /// kick in only on truly empty surfaces, not on the chooser.
    override var mouseDownCanMoveWindow: Bool { false }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setFrameSize(NSSize(width: Self.preferredWidth, height: Self.preferredHeight))
    }
    required init?(coder: NSCoder) { fatalError() }

    override var intrinsicContentSize: NSSize {
        NSSize(width: Self.preferredWidth, height: Self.preferredHeight)
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let ta = trackingArea { removeTrackingArea(ta) }
        let ta = NSTrackingArea(
            rect: bounds,
            options: [.mouseEnteredAndExited, .mouseMoved,
                      .activeAlways, .inVisibleRect],
            owner: self, userInfo: nil
        )
        addTrackingArea(ta)
        trackingArea = ta
    }

    // MARK: - Geometry

    private static var gridYOffset: CGFloat { midiOutH + midiOutGap }

    private func cellRect(program p: Int) -> NSRect {
        let col = p % Self.cols
        let row = p / Self.cols
        return NSRect(x: CGFloat(col) * Self.cellW,
                      y: Self.gridYOffset + CGFloat(row) * Self.cellH,
                      width: Self.cellW,
                      height: Self.cellH)
    }

    /// Full-width "0 MIDI OUT" cell at the top of the chooser. Hit-test
    /// is exclusive of the patch grid below.
    private var midiOutRect: NSRect {
        NSRect(x: 0, y: 0, width: bounds.width, height: Self.midiOutH)
    }

    private func program(at point: NSPoint) -> Int? {
        guard bounds.contains(point) else { return nil }
        let yInGrid = point.y - Self.gridYOffset
        guard yInGrid >= 0 else { return nil }
        let col = Int(point.x / Self.cellW)
        let row = Int(yInGrid / Self.cellH)
        guard col >= 0, col < Self.cols, row >= 0, row < Self.rows else { return nil }
        let p = row * Self.cols + col
        return p < 128 ? p : nil
    }

    private func isMidiOutHit(_ point: NSPoint) -> Bool {
        midiOutRect.contains(point)
    }

    /// Hand-picked color per GM family (16 families × 8 programs each;
    /// each row of the 8-col grid is one family). RGB values mirror
    /// standard CSS named colors so the timbre→color mapping reads
    /// intuitively: pianos are ivory, brass is gold, strings firebrick,
    /// synth leads magenta, etc. Compared to the previous hue
    /// gradient, this gives every family a *meaning* — a glance at the
    /// grid tells you what kind of instrument lives there.
    static let familyPalette: [NSColor] = [
        NSColor(srgbRed: 1.00, green: 0.94, blue: 0.84, alpha: 1), // 0  Piano             — ivory
        NSColor(srgbRed: 0.28, green: 0.82, blue: 0.80, alpha: 1), // 1  Chromatic Perc.   — mediumturquoise
        NSColor(srgbRed: 0.58, green: 0.44, blue: 0.86, alpha: 1), // 2  Organ             — mediumpurple
        NSColor(srgbRed: 0.80, green: 0.52, blue: 0.25, alpha: 1), // 3  Guitar            — peru
        NSColor(srgbRed: 0.10, green: 0.10, blue: 0.44, alpha: 1), // 4  Bass              — midnightblue
        NSColor(srgbRed: 0.70, green: 0.13, blue: 0.13, alpha: 1), // 5  Strings           — firebrick
        NSColor(srgbRed: 1.00, green: 0.41, blue: 0.71, alpha: 1), // 6  Ensemble          — hotpink
        NSColor(srgbRed: 1.00, green: 0.84, blue: 0.00, alpha: 1), // 7  Brass             — gold
        NSColor(srgbRed: 0.13, green: 0.55, blue: 0.13, alpha: 1), // 8  Reed              — forestgreen
        NSColor(srgbRed: 0.53, green: 0.81, blue: 0.98, alpha: 1), // 9  Pipe              — lightskyblue
        NSColor(srgbRed: 1.00, green: 0.00, blue: 1.00, alpha: 1), // 10 Synth Lead        — magenta
        NSColor(srgbRed: 0.87, green: 0.63, blue: 0.87, alpha: 1), // 11 Synth Pad         — plum
        NSColor(srgbRed: 0.12, green: 0.56, blue: 1.00, alpha: 1), // 12 Synth Effects     — dodgerblue
        NSColor(srgbRed: 1.00, green: 0.39, blue: 0.28, alpha: 1), // 13 Ethnic            — tomato
        NSColor(srgbRed: 0.41, green: 0.41, blue: 0.41, alpha: 1), // 14 Percussive        — dimgray
        NSColor(srgbRed: 0.24, green: 0.70, blue: 0.44, alpha: 1), // 15 Sound Effects     — mediumseagreen
    ]

    /// Public color lookup for any GM program. Each family has a base
    /// hue (see `familyPalette`); each of the 8 voices inside a family
    /// gets its own shade derived from that base by walking
    /// brightness + saturation across the 8 slots. So every program
    /// (0–127) renders as a distinct, visually distinguishable color
    /// while still reading as a member of its family row.
    static func colorForProgram(_ p: Int) -> NSColor {
        let safe = max(0, min(127, p))
        let base = familyPalette[safe / 8]
        let slot = safe % 8
        // Walk brightness across slots: dimmest at slot 0, brightest at
        // slot 7. Range ±22% from the base. Saturation alternates so
        // adjacent shades never collapse to the same tonal value.
        let bDelta = (CGFloat(slot) - 3.5) / 3.5 * 0.22       // −0.22…+0.22
        let sDelta: CGFloat = (slot % 2 == 0) ? -0.10 : 0.10  // alternating
        // Convert to HSB in sRGB, shift, clamp, return.
        guard let hsb = base.usingColorSpace(.sRGB) else { return base }
        var h: CGFloat = 0, s: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
        hsb.getHue(&h, saturation: &s, brightness: &b, alpha: &a)
        let s2 = max(0.05, min(1.0, s + sDelta))
        let b2 = max(0.10, min(1.0, b + bDelta))
        return NSColor(hue: h, saturation: s2, brightness: b2, alpha: a)
    }

    private func familyColor(forProgram p: Int) -> NSColor {
        Self.colorForProgram(p)
    }

    // MARK: - Drawing

    /// Bee-vision typography: program numbers grow toward the
    /// selected cell and shrink toward the edges of the grid. The
    /// selected cell renders biggest with a heavy weight; cells one
    /// step away render slightly smaller; faraway cells fall to the
    /// minimum size so the eye is drawn to the active voice.
    private static func numberFont(forDistance d: CGFloat, isSelected: Bool) -> NSFont {
        if isSelected {
            return NSFont.monospacedDigitSystemFont(ofSize: 13, weight: .black)
        }
        // Continuous falloff so neighbors of any direction get
        // proportional emphasis. Tuned so dist 1 ≈ 11pt, dist 4 ≈ 9pt
        // and the floor (dist 8+) stays at the original 8.5pt.
        let size = max(8.5, 12.0 - d * 0.85)
        let weight: NSFont.Weight = d <= 1.5 ? .bold : (d <= 3.0 ? .semibold : .medium)
        return NSFont.monospacedDigitSystemFont(ofSize: size, weight: weight)
    }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        // MIDI OUT cell (slot 0) — accent-filled when active, outlined
        // when inactive. Draws first so the patch grid renders below.
        let midiR = midiOutRect
        if midiR.intersects(dirtyRect) {
            let accent = NSColor.controlAccentColor
            let cap = NSBezierPath(roundedRect: midiR.insetBy(dx: 1.75, dy: 1.5),
                                   xRadius: 3, yRadius: 3)
            if midiModeActive {
                accent.withAlphaComponent(0.85).setFill()
                cap.fill()
                accent.setStroke()
                cap.lineWidth = 1.4
                cap.stroke()
            } else {
                accent.withAlphaComponent(0.10).setFill()
                cap.fill()
                accent.withAlphaComponent(0.55).setStroke()
                cap.lineWidth = 0.8
                cap.stroke()
            }
            let labelText = "0  MIDI OUT"
            let labelColor: NSColor = midiModeActive ? .white : .labelColor
            let labelAttrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: 10.5, weight: .semibold),
                .foregroundColor: labelColor.withAlphaComponent(midiModeActive ? 1.0 : 0.85),
                .kern: 0.4,
            ]
            let str = NSAttributedString(string: labelText, attributes: labelAttrs)
            let size = str.size()
            str.draw(at: NSPoint(x: midiR.midX - size.width / 2,
                                 y: midiR.midY - size.height / 2))
        }

        let selectedRow = Int(selectedProgram) / Self.cols
        let selectedCol = Int(selectedProgram) % Self.cols

        for p in 0..<128 {
            let r = cellRect(program: p)
            guard r.intersects(dirtyRect) else { continue }
            let fam = familyColor(forProgram: p)
            let row = p / Self.cols
            let col = p % Self.cols
            let dx = CGFloat(col - selectedCol)
            let dy = CGFloat(row - selectedRow)
            let dist = sqrt(dx * dx + dy * dy)
            let isSelected = (selectedProgram == UInt8(p))

            if isSelected {
                // Selected cell: solid family color so the chosen
                // voice reads as the brightest cell in the grid.
                fam.withAlphaComponent(0.85).setFill()
                NSBezierPath(rect: r).fill()
            } else {
                // Family-tinted bed at low opacity so the grid still
                // reads as 16 rainbow rows.
                fam.withAlphaComponent(0.20).setFill()
                NSBezierPath(rect: r).fill()
                if hoveredProgram == UInt8(p) {
                    NSColor.controlAccentColor.withAlphaComponent(0.35).setFill()
                    NSBezierPath(rect: r).fill()
                }
            }

            // Chiclet-style keycap outline.
            let capPath = NSBezierPath(roundedRect: r.insetBy(dx: 1.75, dy: 1.5),
                                        xRadius: 2.5, yRadius: 2.5)
            // Selected cell gets a brighter ring so it reads as the
            // visual focus even without the radial pulse on top.
            let strokeAlpha: CGFloat = isSelected ? 1.0 : 0.65
            fam.withAlphaComponent(strokeAlpha).setStroke()
            capPath.lineWidth = isSelected ? 1.4 : 0.8
            capPath.stroke()

            // Bee-vision program number — biggest at the selected
            // cell, tapering with distance. Shadow on the selected
            // cell so the giant glyph lifts cleanly off the bg.
            let font = Self.numberFont(forDistance: dist, isSelected: isSelected)
            let textColor: NSColor
            let textAlpha: CGFloat
            if isSelected {
                textColor = .white
                textAlpha = 1.0
            } else {
                textColor = NSColor.labelColor
                // Distant cells fade toward gridroom-tone so the
                // selected one stays visually loudest.
                textAlpha = max(0.45, 0.95 - dist * 0.10)
            }
            var attrs: [NSAttributedString.Key: Any] = [
                .font: font,
                .foregroundColor: textColor.withAlphaComponent(textAlpha),
            ]
            if isSelected {
                let shadow = NSShadow()
                shadow.shadowColor = NSColor.black.withAlphaComponent(0.6)
                shadow.shadowOffset = NSSize(width: 0, height: -1)
                shadow.shadowBlurRadius = 2
                attrs[.shadow] = shadow
            }
            // Display 1-based labels (1-128) — internal program index
            // stays 0-127 for synth/MIDI compatibility. Slot 0 is the
            // virtual "MIDI OUT" address (handled by the toggle below
            // the grid); patches occupy 1-128.
            let str = NSAttributedString(string: String(p + 1), attributes: attrs)
            let size = str.size()
            str.draw(at: NSPoint(x: r.midX - size.width / 2,
                                 y: r.midY - size.height / 2))
        }
    }

    // MARK: - Mouse

    // Hover preview is press-gated: passive mouse-over does NOT light cells
    // or trigger preview audio. The user has to mouseDown first; while held,
    // dragging across cells lights/sounds each one and unlights it on the
    // way out. mouseUp stops the sound and commits the cell under the cursor.
    private var dragging = false

    override func mouseEntered(with event: NSEvent) {
        guard dragging else { return }
        updateHover(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseMoved(with event: NSEvent) {
        guard dragging else { return }
        updateHover(at: convert(event.locationInWindow, from: nil))
    }
    override func mouseExited(with event: NSEvent) {
        guard dragging else { return }
        if let prev = hoveredProgram {
            hoveredProgram = nil
            setNeedsDisplay(cellRect(program: Int(prev)))
            onHover?(nil)
        }
    }

    private func updateHover(at point: NSPoint) {
        let p = program(at: point).map { UInt8($0) }
        if p != hoveredProgram {
            let prev = hoveredProgram
            hoveredProgram = p
            if let prev = prev { setNeedsDisplay(cellRect(program: Int(prev))) }
            if let p = p { setNeedsDisplay(cellRect(program: Int(p))) }
            onHover?(p.map { Int($0) })
        }
    }

    override func mouseDown(with event: NSEvent) {
        // Take key focus on click so arrow-key navigation works
        // immediately after the user picks an initial cell.
        window?.makeFirstResponder(self)
        let pt = convert(event.locationInWindow, from: nil)
        // MIDI OUT cell — slot 0. Click toggles MIDI passthrough mode
        // via the controller. Bypasses the drag/preview path because
        // there's no audible preview to start.
        if isMidiOutHit(pt) {
            onMidiOutCommit?()
            return
        }
        dragging = true
        if let p = program(at: pt) {
            // Treat the press as a hover-into-this-cell so the preview note
            // and lit highlight start immediately on click.
            if hoveredProgram != UInt8(p) {
                let prev = hoveredProgram
                hoveredProgram = UInt8(p)
                if let prev = prev { setNeedsDisplay(cellRect(program: Int(prev))) }
                setNeedsDisplay(cellRect(program: p))
            }
            onHover?(p)
        }
    }

    override func mouseDragged(with event: NSEvent) {
        guard dragging else { return }
        updateHover(at: convert(event.locationInWindow, from: nil))
    }

    override func mouseUp(with event: NSEvent) {
        guard dragging else { return }
        dragging = false
        let pt = convert(event.locationInWindow, from: nil)
        // Release stops the preview note + clears the lit highlight, then
        // commits the cell that was under the cursor at release time. The
        // commit path will re-light the cell as the *selected* program.
        let prevHover = hoveredProgram
        hoveredProgram = nil
        if let prev = prevHover { setNeedsDisplay(cellRect(program: Int(prev))) }
        onHover?(nil)
        if let p = program(at: pt) {
            onCommit?(p)
        }
    }

    /// No-op kept for API compatibility with the popover (was needed when
    /// the list was scrollable; the numeric map shows everything at once).
    func scrollProgramIntoView(_ program: UInt8, animated: Bool = false) {}

    // MARK: - Keyboard

    override var acceptsFirstResponder: Bool { true }

    /// Arrow-key navigation across the 8 × 16 grid. Each move fires
    /// `onHover` for the new cell — same path as mouse-drag preview
    /// — so the held preview note moves with you and the chrome
    /// retints in real time. Releasing the key commits the cell that's
    /// currently selected (mirrors mouseDown→drag→mouseUp).
    override func keyDown(with event: NSEvent) {
        let cur = Int(selectedProgram)
        var next = cur
        var dir = -1
        // Digit keys address slots directly: '0' picks MIDI OUT, '1'-'9'
        // pick programs 0-8 (display 1-9). Auto-repeat is suppressed so
        // a held digit doesn't re-toggle MIDI mode every tick. Multi-
        // digit entry for patches 10-128 isn't wired yet — single-digit
        // covers the common "0/1 quick toggle" case the user described.
        if !event.isARepeat,
           !event.modifierFlags.contains(.shift),
           let ch = event.charactersIgnoringModifiers, ch.count == 1,
           let digit = Int(ch), (0...9).contains(digit) {
            if digit == 0 {
                onMidiOutCommit?()
            } else {
                // onCommit's existing path turns MIDI off (if on) before
                // setting the program — same path the chooser click
                // uses, so the keyboard "1" matches "click slot 1".
                onCommit?(digit - 1)
            }
            return
        }
        switch event.keyCode {
        case 123: next = cur - 1;        dir = 0   // ←
        case 124: next = cur + 1;        dir = 1   // →
        case 125: next = cur + Self.cols; dir = 2  // ↓
        case 126: next = cur - Self.cols; dir = 3  // ↑
        default:
            // Not an arrow — forward to the music-key handler so
            // notepat letter keys still sound notes while the
            // popover holds key focus on this view. Pass auto-repeat
            // through here (the controller handles its own dedupe).
            let consumed = onMusicKey?(event.keyCode, true,
                                       event.isARepeat,
                                       event.modifierFlags) ?? false
            if !consumed { super.keyDown(with: event) }
            return
        }
        // Arrow path: ignore auto-repeat so each physical press moves
        // the selection by exactly one cell. The preview note started
        // on first-press still sustains as long as the key is held
        // (release in keyUp), satisfying "hold preview while held"
        // without bundling movement into auto-repeat.
        if event.isARepeat { return }
        next = max(0, min(127, next))
        onArrowKey?(dir, true)
        if next != cur {
            // Update the lit cell + fire hover. The popover's onHover
            // handler already does the heavy lifting: stop the previous
            // preview note, retint chip + visualizer, start a new
            // preview note in the new program.
            selectedProgram = UInt8(next)
            onHover?(next)
        }
    }

    override func keyUp(with event: NSEvent) {
        switch event.keyCode {
        case 123, 124, 125, 126:
            let dir: Int
            switch event.keyCode {
            case 123: dir = 0
            case 124: dir = 1
            case 125: dir = 2
            default:  dir = 3
            }
            onArrowKey?(dir, false)
            // Release ends the preview and commits the currently-
            // selected cell (same mouseUp semantics).
            onHover?(nil)
            onCommit?(Int(selectedProgram))
        default:
            let consumed = onMusicKey?(event.keyCode, false, false,
                                       event.modifierFlags) ?? false
            if !consumed { super.keyUp(with: event) }
        }
    }
}
