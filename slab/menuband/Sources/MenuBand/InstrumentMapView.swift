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

    static let preferredWidth:  CGFloat = cellW * CGFloat(cols)    // 224
    static let preferredHeight: CGFloat = cellH * CGFloat(rows)    // 224

    var selectedProgram: UInt8 = 0 { didSet { needsDisplay = true } }
    private(set) var hoveredProgram: UInt8?
    var onCommit: ((Int) -> Void)?
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

    // MARK: - Visualizer state
    /// One smoothed display level per column (0…1), driving the
    /// per-column LED bars that bloom outward from the grid midline.
    private var columnPeaks = [Float](repeating: 0, count: cols)
    /// Per-tick raw RMS per column, before gain + smoothing.
    private var columnLevels = [Float](repeating: 0, count: cols)
    /// Reusable buffer the synth fills during snapshotWaveform.
    private var sampleScratch = [Float](repeating: 0, count: 1024)
    /// Auto-gain envelope.
    private var smoothedPeak: Float = 0.05
    /// Slow blink phase for the selected cell — independent of audio
    /// so the chosen instrument still breathes during silence.
    private var blinkPhase: Double = 0
    private var visualizerLink: CVDisplayLink?
    private var hasCaptureLease = false
    private var pendingTickLock = NSLock()
    private var pendingTick = false

    override var isFlipped: Bool { true }   // top-down rows, reading order
    /// Cells are clickable + drag-target — let `panel.isMovableByWindowBackground`
    /// kick in only on truly empty surfaces, not on the chooser.
    override var mouseDownCanMoveWindow: Bool { false }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        setFrameSize(NSSize(width: Self.preferredWidth, height: Self.preferredHeight))
    }
    required init?(coder: NSCoder) { fatalError() }

    deinit { stopVisualizer() }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if window == nil {
            stopVisualizer()
        } else {
            startVisualizer()
        }
    }

    private func startVisualizer() {
        guard visualizerLink == nil, menuBand != nil else { return }
        var link: CVDisplayLink?
        guard CVDisplayLinkCreateWithActiveCGDisplays(&link) == kCVReturnSuccess,
              let link = link else { return }
        let opaque = Unmanaged.passUnretained(self).toOpaque()
        CVDisplayLinkSetOutputCallback(link, { _, _, _, _, _, ctx -> CVReturn in
            guard let ctx = ctx else { return kCVReturnSuccess }
            let view = Unmanaged<InstrumentListView>.fromOpaque(ctx).takeUnretainedValue()
            // Coalesce pending ticks the same way WaveformView does — slow
            // main runloop shouldn't build a backlog of stale draws.
            view.pendingTickLock.lock()
            if view.pendingTick { view.pendingTickLock.unlock(); return kCVReturnSuccess }
            view.pendingTick = true
            view.pendingTickLock.unlock()
            DispatchQueue.main.async { view.tickVisualizer() }
            return kCVReturnSuccess
        }, opaque)
        guard CVDisplayLinkStart(link) == kCVReturnSuccess else { return }
        menuBand?.setWaveformCaptureEnabled(true)
        hasCaptureLease = true
        visualizerLink = link
    }

    private func stopVisualizer() {
        if let link = visualizerLink {
            CVDisplayLinkStop(link)
            visualizerLink = nil
        }
        if hasCaptureLease {
            menuBand?.setWaveformCaptureEnabled(false)
            hasCaptureLease = false
        }
        pendingTickLock.lock()
        pendingTick = false
        pendingTickLock.unlock()
        for c in 0..<columnPeaks.count { columnPeaks[c] = 0 }
        blinkPhase = 0
    }

    private func tickVisualizer() {
        pendingTickLock.lock()
        pendingTick = false
        pendingTickLock.unlock()
        guard let mb = menuBand else { return }
        guard window?.isVisible == true, !isHiddenOrHasHiddenAncestor else { return }
        mb.synthSnapshotWaveform(into: &sampleScratch)
        let perCol = sampleScratch.count / Self.cols
        guard perCol > 0 else { return }
        var framePeak: Float = 0
        for c in 0..<Self.cols {
            let base = c * perCol
            var sumSq: Float = 0
            for i in 0..<perCol {
                let s = sampleScratch[base + i]
                sumSq += s * s
            }
            let rms = sqrtf(sumSq / Float(perCol))
            columnLevels[c] = rms
            if rms > framePeak { framePeak = rms }
        }
        if framePeak > smoothedPeak {
            smoothedPeak = framePeak
        } else {
            smoothedPeak = max(0.05, smoothedPeak * 0.92 + framePeak * 0.08)
        }
        let gain = 0.95 / smoothedPeak
        let attack: Float = 0.55
        let decay:  Float = 0.18
        var changed = false
        for c in 0..<Self.cols {
            let raw = Swift.min(1.0, columnLevels[c] * gain)
            let prev = columnPeaks[c]
            let alpha = (raw > prev) ? attack : decay
            let next = prev * (1.0 - alpha) + raw * alpha
            if abs(next - prev) > 0.005 {
                columnPeaks[c] = next
                changed = true
            }
        }
        // Steady ~1.6Hz breathe phase for the selected cell, independent
        // of audio so the chosen voice keeps gently pulsing during silence.
        blinkPhase += 1.0 / 60.0 * 1.6
        if blinkPhase > 1000 { blinkPhase -= 1000 }
        if changed || Int(blinkPhase * 15) % 4 == 0 {
            needsDisplay = true
        }
    }

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

    private func cellRect(program p: Int) -> NSRect {
        let col = p % Self.cols
        let row = p / Self.cols
        return NSRect(x: CGFloat(col) * Self.cellW,
                      y: CGFloat(row) * Self.cellH,
                      width: Self.cellW,
                      height: Self.cellH)
    }

    private func program(at point: NSPoint) -> Int? {
        guard bounds.contains(point) else { return nil }
        let col = Int(point.x / Self.cellW)
        let row = Int(point.y / Self.cellH)
        guard col >= 0, col < Self.cols, row >= 0, row < Self.rows else { return nil }
        let p = row * Self.cols + col
        return p < 128 ? p : nil
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
        let selectedRow = Int(selectedProgram) / Self.cols
        let selectedCol = Int(selectedProgram) % Self.cols
        // Selected cell breathes in alpha at ~1.6 Hz regardless of
        // audio; on top of that, the loudest column's level boosts the
        // gain so the chosen voice pulses with playing too.
        let blinkAmount = 0.5 + 0.5 * CGFloat(sin(blinkPhase * 2 * .pi))
        let amp = CGFloat(columnPeaks.max() ?? 0)

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
                // Selected cell: family color pulsing toward white,
                // tracking both the steady blink and audio amplitude.
                let pulse = min(1.0, 0.55 + blinkAmount * 0.30 + amp * 0.5)
                let bg = fam.blended(withFraction: amp * 0.4, of: .white) ?? fam
                bg.withAlphaComponent(pulse).setFill()
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
            let str = NSAttributedString(string: String(p), attributes: attrs)
            let size = str.size()
            str.draw(at: NSPoint(x: r.midX - size.width / 2,
                                 y: r.midY - size.height / 2))
        }
        drawColumnBars(in: dirtyRect)
    }

    /// Per-column center-out LED bars — each column's smoothed peak
    /// maps to a half-height of lit cells, blooming above and below
    /// the grid's midline. Lit cells glow in a saturated derivative
    /// of their family color so the meter reads as the family palette
    /// firing up. The selected cell is skipped (the main draw loop
    /// already paints it with a brighter pulse).
    private func drawColumnBars(in dirtyRect: NSRect) {
        let halfRows = Self.rows / 2
        for c in 0..<Self.cols {
            let peak = columnPeaks[c]
            let litHalf = Int((CGFloat(peak) * CGFloat(halfRows)).rounded(.up))
            guard litHalf > 0 else { continue }
            for offset in 0..<litHalf {
                let intensity = 0.65 - 0.30 * (CGFloat(offset) / CGFloat(halfRows))
                for row in [halfRows - 1 - offset, halfRows + offset] {
                    guard row >= 0, row < Self.rows else { continue }
                    let p = row * Self.cols + c
                    guard p >= 0, p < 128 else { continue }
                    if UInt8(p) == selectedProgram { continue }
                    let r = cellRect(program: p)
                    guard r.intersects(dirtyRect) else { continue }
                    saturatedGlow(for: p, alpha: intensity).setFill()
                    NSBezierPath(rect: r.insetBy(dx: 1.75, dy: 1.5)).fill()
                }
            }
        }
    }

    /// Take the cell's family color and crank saturation + brightness so
    /// the lit overlay reads as a glowing version of the cell's own hue.
    /// Returns nil-safe via fallback to the base family color if HSB
    /// conversion fails (shouldn't happen for sRGB-defined palette
    /// entries, but guards against future palette changes).
    private func saturatedGlow(for program: Int, alpha: CGFloat) -> NSColor {
        let base = Self.colorForProgram(program)
        guard let hsb = base.usingColorSpace(.sRGB) else {
            return base.withAlphaComponent(alpha)
        }
        var h: CGFloat = 0, s: CGFloat = 0, b: CGFloat = 0, a: CGFloat = 0
        hsb.getHue(&h, saturation: &s, brightness: &b, alpha: &a)
        // Push saturation toward 1, brightness toward 1 — same hue, much
        // hotter rendering. Half-step toward max to keep colors that are
        // already vivid (magenta, gold) from clipping into nonsense.
        let s2 = s + (1 - s) * 0.85
        let b2 = b + (1 - b) * 0.55
        return NSColor(hue: h, saturation: s2, brightness: b2, alpha: alpha)
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
        dragging = true
        // Take key focus on click so arrow-key navigation works
        // immediately after the user picks an initial cell.
        window?.makeFirstResponder(self)
        let pt = convert(event.locationInWindow, from: nil)
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
