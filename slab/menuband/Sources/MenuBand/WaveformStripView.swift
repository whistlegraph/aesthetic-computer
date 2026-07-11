import AppKit

/// Static lit-up raster display for the popover and the full-screen keymap
/// view — a fixed grid of cells on dark display glass, like a hardware VFD /
/// dot-matrix scope. The waveform never scrolls: a write cursor sweeps
/// left→right ECG-style, lighting each column's cells in place and wrapping
/// back to the left edge when it hits the right. Holding space (reverse
/// replay) freezes the writes and sweeps an orange playhead backward over the
/// already-lit columns — nothing slides, nothing snaps back on release.
///
/// The glass stays dark in BOTH appearances (a lit display embedded in light
/// chrome, not a white scope), which is also what fixes the washed-out light
/// mode the old scrolling strip had.
///
/// Lightweight (Core Graphics, polled ~30 fps only while on screen) and
/// cacheDisplay-safe, so the headless App Store captures render it for real.
final class WaveformStripView: NSView {
    weak var menuBand: MenuBandController?

    /// Optional tint for the lit cells. `nil` → the system accent (the
    /// popover default); the full-screen overlay feeds the active voice's
    /// family color so the display matches its per-voice framing. Rewind
    /// still flips to orange regardless of this.
    var tintColor: NSColor? {
        didSet { needsDisplay = true }
    }

    /// When false the display stops capturing and clears to an empty grid —
    /// used while the host panel is hidden or MIDI mode owns the output.
    var isLive: Bool = true {
        didSet {
            guard isLive != oldValue, window != nil else { return }
            if isLive { start() } else { freezeToBaseline() }
        }
    }

    /// Draw the recessed rounded plate + accent frame. The popover wants it
    /// (the strip IS the visual there); the full-screen overlay turns it off
    /// so the glass fills square into the bezel's own rounded clip.
    var drawsPlate: Bool = true {
        didSet { needsDisplay = true }
    }

    /// Fired when the display is clicked. The popover wires this to open the
    /// full keymap view — same action as the "Keymap" button — so the live
    /// scope doubles as a keymap affordance.
    var onClick: (() -> Void)?

    override func mouseDown(with event: NSEvent) {
        if onClick != nil { onClick?() } else { super.mouseDown(with: event) }
    }

    /// Pointing-hand cursor so the display reads as clickable.
    override func resetCursorRects() {
        if onClick != nil { addCursorRect(bounds, cursor: .pointingHand) }
    }

    /// One frame's worth of the tap ring, reduced to a single min/max
    /// column. Sized to cover MORE than one 30 fps frame even at 96 kHz
    /// (96000/30 = 3200 samples) so consecutive columns don't skip audio
    /// between them.
    private var samples = [Float](repeating: 0, count: 4096)

    // ── Fixed raster model ──────────────────────────────────────────────────
    /// Per-column min/max amplitude, sized to the on-screen column count.
    /// `written[c]` marks columns that carry real data (unwritten columns show
    /// only the faint off-grid). `cursor` is the ECG write head: it advances
    /// one column per 30 fps tick and wraps, overwriting the oldest column.
    private var gridMin: [Float] = []
    private var gridMax: [Float] = []
    private var written: [Bool] = []
    private var cursor = 0
    /// Raster geometry — chunky cells, low-res on purpose so it reads as a
    /// hardware display, not an anti-aliased plot. Sized for the popover by
    /// default; `setCellGeometry` scales it up for the full-screen visualizer,
    /// where the same 5 pt cells would read as a fine dot screen from across
    /// the room instead of a wall of LEDs.
    private var cellStep: CGFloat = 5     // px between cell origins
    private var cellSize: CGFloat = 3.5   // lit cell square
    private static let inset: CGFloat = 2 // glass inner margin

    /// Resize the LEDs. Forces a full raster rebuild (the column/row counts
    /// fall out of the cell pitch), so the sweep restarts from a clean grid.
    func setCellGeometry(step: CGFloat, size: CGFloat) {
        guard step != cellStep || size != cellSize else { return }
        cellStep = step
        cellSize = size
        cols = 0                 // defeat updateGridGeometry's no-op guard
        rows = 0
        updateGridGeometry()
        needsDisplay = true
    }
    private var cols = 0
    private var rows = 0
    /// Every cell of the raster, precomputed once per geometry change. The
    /// off-grid paints as ONE batched `CGContext.fill` of this list rather than
    /// a per-frame blit of a screen-sized bitmap — at full-screen sizes that
    /// bitmap was several megabytes copied 30× a second for a picture that
    /// never changes.
    private var offRects: [CGRect] = []
    /// Scratch buffer for one column's lit cells, reused every frame so the
    /// draw loop allocates nothing.
    private var columnRects: [CGRect] = []
    private var timer: Timer?
    /// While reversing: eased column offset of the playhead back from the
    /// cursor, driven by the audio's actual reverse progress.
    private var scrubOffset: CGFloat = 0
    private var wasReversing = false

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
    }
    required init?(coder: NSCoder) { fatalError() }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if window != nil, isLive { start() } else { stop() }
    }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        updateGridGeometry()
    }

    /// Recompute the raster from the current bounds. Rows are odd so a center
    /// baseline row exists; buffers reset on a size change (resize is rare —
    /// a fresh sweep is cleaner than remapping history).
    private func updateGridGeometry() {
        let w = bounds.width - Self.inset * 2
        let h = bounds.height - Self.inset * 2
        let newCols = max(0, Int(w / cellStep))
        var newRows = max(3, Int(h / cellStep))
        if newRows % 2 == 0 { newRows -= 1 }
        guard newCols != cols || newRows != rows else { return }
        cols = newCols
        rows = newRows
        gridMin = .init(repeating: 0, count: cols)
        gridMax = .init(repeating: 0, count: cols)
        written = .init(repeating: false, count: cols)
        cursor = 0
        columnRects.reserveCapacity(rows)
        offRects = []
        offRects.reserveCapacity(cols * rows)
        for c in 0..<cols {
            let x = cellX(c)
            for r in 0..<rows {
                offRects.append(CGRect(x: x, y: cellY(r), width: cellSize, height: cellSize))
            }
        }
        needsDisplay = true
    }

    private func start() {
        guard timer == nil else { return }
        menuBand?.setWaveformCaptureEnabled(true)
        let t = Timer(timeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in
            self?.tick()
        }
        t.tolerance = 1.0 / 90.0
        RunLoop.main.add(t, forMode: .common)
        timer = t
    }

    private func stop() {
        timer?.invalidate(); timer = nil
        menuBand?.setWaveformCaptureEnabled(false)
    }

    /// Stop capturing and clear the raster so the display shows the empty
    /// off-grid (used when `isLive` is switched off).
    private func freezeToBaseline() {
        stop()
        for i in 0..<cols { written[i] = false; gridMin[i] = 0; gridMax[i] = 0 }
        cursor = 0
        scrubOffset = 0
        needsDisplay = true
    }

    deinit { timer?.invalidate() }

    /// Seed the raster with a believable synthetic note so the display paints
    /// what it really would when a note plays, in the HEADLESS App Store
    /// captures (`--render-popover` / `--render-keymap`), where no audio
    /// engine runs and the live `tick()` path never writes columns.
    ///
    /// Reading left→right is forward in time, exactly like the live sweep:
    /// a little pre-strike silence, a sharp ATTACK transient, then an
    /// exponentially DECAYING sustain that trails up to the write cursor —
    /// the picture of "a note is ringing out," with the not-yet-swept columns
    /// to the right showing only the off-grid. No-op at runtime; only the
    /// capture calls it.
    /// Fill the scope's raster from REAL audio — one peak level per column,
    /// oldest to newest, with the write cursor `cursorAt` (0…1) across the
    /// strip. Promo renders scrub the actual mix through here so the LED
    /// display shows the music that's playing, rather than the synthetic
    /// stand-in below. No-op at runtime; only the capture calls it.
    /// `scrubBack` (0…1) poses the reverse playhead: 0 sits it at the write
    /// cursor, 1 sweeps it fully back through the columns just played. `draw`
    /// only paints the orange indicator when the controller's `isRewinding` is
    /// set, so a reverse capture poses both.
    func seedWaveform(levels: [Float], cursorAt: Double, scrubBack: Double = 0) {
        updateGridGeometry()
        guard cols > 8, !levels.isEmpty else { return }
        let fillEnd = max(1, min(cols, Int((Double(cols) * cursorAt).rounded())))
        for c in 0..<cols {
            guard c < fillEnd else { written[c] = false; continue }
            let f = fillEnd > 1 ? Double(c) / Double(fillEnd - 1) : 0
            let level = levels[min(levels.count - 1, Int(f * Double(levels.count - 1)))]
            // Tiny floor so even silent columns tick the center baseline row,
            // the same way the synthetic seed does.
            gridMax[c] = max(0.012, level)
            gridMin[c] = min(-0.012, -level * 0.85)
            written[c] = true
        }
        cursor = fillEnd % cols
        scrubOffset = CGFloat(max(0, min(1, scrubBack))) * CGFloat(fillEnd)
        wasReversing = scrubBack > 0
        needsDisplay = true
    }

    func seedSyntheticWaveform(columns: Int = 220) {
        _ = columns   // raster fills from its own geometry, not a history cap
        updateGridGeometry()
        guard cols > 8 else { return }
        // Deterministic value-noise so the capture is reproducible run-to-run
        // (no RNG seeding) yet looks irregular like real sampled audio.
        func noise(_ x: Float) -> Float {
            let s = sinf(x * 12.9898) * 43758.5453
            return (s - s.rounded(.down)) * 2 - 1   // → [-1, 1)
        }
        let fillEnd = Int(Float(cols) * 0.72)        // cursor lands here
        let onsetIdx = Float(fillEnd) * 0.30         // strike ~1/3 into the sweep
        let decayCols = Float(fillEnd) - onsetIdx
        for c in 0..<cols {
            guard c < fillEnd else { written[c] = false; continue }
            let pos = Float(c)
            let since = pos - onsetIdx
            var env: Float
            if since < 0 {
                env = 0
            } else {
                let attack = min(1, since / 2)
                let decay = 0.45 + 0.55 * expf(-since / decayCols * 2.0)
                env = attack * decay
            }
            let carrier = sinf(pos * 0.9) * 0.55 + sinf(pos * 0.37) * 0.25
            let grain = noise(pos) * 0.22
            let sample = carrier + grain
            let amp = env * 0.92
            // Tiny floor so even silent columns tick the center baseline row.
            gridMax[c] = max(0.012, abs(sample) * amp)
            gridMin[c] = min(-0.012, -abs(sample * 0.85 + grain * 0.5) * amp)
            written[c] = true
        }
        cursor = fillEnd % cols
        needsDisplay = true
    }

    private func tick() {
        guard cols > 0 else { updateGridGeometry(); return }
        let reversing = menuBand?.isRewinding ?? false
        if reversing {
            // Freeze the sweep; position the playhead from the ACTUAL reverse
            // playback progress (exact, drift-free). The player's progress is
            // polled at 30 fps and can land unevenly, so ease toward the
            // audio-derived offset (snap on the first reverse frame) — the
            // indicator glides while the lit columns stay put.
            let windowCols = CGFloat(menuBand?.rewindWindowSeconds ?? 1.5) * 30.0
            let progress = menuBand?.rewindProgress() ?? 0
            let target = CGFloat(progress) * windowCols
            scrubOffset = wasReversing ? scrubOffset + (target - scrubOffset) * 0.4 : target
        } else {
            // Live sweep: reduce this frame to one min/max column, write it at
            // the cursor, advance, wrap (overwriting the oldest column).
            menuBand?.synthSnapshotWaveform(into: &samples)
            var mn: Float = 0, mx: Float = 0
            for s in samples {
                if s < mn { mn = s }
                if s > mx { mx = s }
            }
            gridMin[cursor] = min(-0.012, mn)
            gridMax[cursor] = max(0.012, mx)
            written[cursor] = true
            cursor = (cursor + 1) % cols
            scrubOffset = 0
        }
        wasReversing = reversing
        needsDisplay = true
    }

    // ── Drawing ─────────────────────────────────────────────────────────────

    /// Dark display glass — the same in light and dark appearance. A lit
    /// display is dark by nature; embedding it unchanged in light chrome is
    /// what makes it read as hardware.
    private static let glass = NSColor(srgbRed: 0.035, green: 0.045, blue: 0.042, alpha: 1.0)

    /// Full-bleed glass is opaque, so AppKit can skip compositing whatever is
    /// behind us — worth real time when "behind us" is the entire desktop.
    /// The popover's plate has rounded corners, so it isn't.
    override var isOpaque: Bool { !drawsPlate }

    private func cellX(_ c: Int) -> CGFloat {
        Self.inset + (bounds.width - Self.inset * 2 - CGFloat(cols) * cellStep) / 2
            + CGFloat(c) * cellStep
    }
    private func cellY(_ r: Int) -> CGFloat {
        Self.inset + (bounds.height - Self.inset * 2 - CGFloat(rows) * cellStep) / 2
            + CGFloat(r) * cellStep
    }

    override func draw(_ dirtyRect: NSRect) {
        let bounds = self.bounds
        guard bounds.width > 6, bounds.height > 6 else { return }
        if cols == 0 { updateGridGeometry() }
        guard cols > 0, let ctx = NSGraphicsContext.current?.cgContext else { return }
        let reversing = menuBand?.isRewinding ?? false
        let accent = NSColor.controlAccentColor
        let tint = tintColor ?? accent

        // Glass. Rounded + accent-framed as the popover's recessed plate;
        // square when the host bezel supplies the framing.
        if drawsPlate {
            let plate = NSBezierPath(roundedRect: bounds.insetBy(dx: 0.5, dy: 0.5),
                                     xRadius: 4, yRadius: 4)
            Self.glass.setFill(); plate.fill()
            accent.withAlphaComponent(0.55).setStroke(); plate.lineWidth = 1.0; plate.stroke()
        } else {
            ctx.setFillColor(Self.glass.cgColor)
            ctx.fill(bounds)
        }

        // Faint off-grid — the raster itself, always visible. One batched fill
        // of every cell; Quartz walks the rect list far faster than we can
        // issue the fills one at a time.
        ctx.setFillColor(tint.withAlphaComponent(0.09).cgColor)
        ctx.fill(offRects)

        let halfRows = (rows - 1) / 2
        let centerRow = halfRows
        // Playhead column while reversing: eased columns back from the cursor,
        // wrapped onto the raster.
        let playheadCol = reversing
            ? ((cursor - 1 - Int(scrubOffset.rounded())) % cols + cols) % cols
            : -1
        // Columns the reverse has already consumed (between playhead and
        // cursor) dim — the static-raster equivalent of the old fade-out.
        func consumed(_ c: Int) -> Bool {
            guard reversing else { return false }
            let back = ((cursor - 1 - c) % cols + cols) % cols
            return back < Int(scrubOffset.rounded())
        }

        // Perceptual amplitude lift: master output rarely peaks past ~0.3
        // after the limiter, so a LINEAR map lit barely one cell ("mostly
        // flat"). Modest gain + square-root loudness curve fills the raster
        // the way a hardware VFD meter would, and still saturates cleanly at
        // true full scale.
        func lift(_ v: Float) -> CGFloat {
            CGFloat(sqrtf(Swift.min(1, Swift.max(0, v) * 2.2)))
        }

        // Age of a column: 0 is the one just written, cols-1 the oldest — the
        // one sitting immediately AHEAD of the write cursor, about to be
        // overwritten.
        func age(_ c: Int) -> CGFloat {
            CGFloat(((cursor - 1 - c) % cols + cols) % cols)
        }
        // Phosphor decay. Without this the ring buffer never appears to clear:
        // a column written a full sweep ago paints exactly as bright as one
        // written this frame, so the wall just sits there full, quietly
        // rewriting itself in place. Fading by age turns the sweep into a comet
        // tail — the trace burns in at the head and dims to almost nothing by
        // the time the cursor comes back around to erase it.
        let oldest = CGFloat(max(1, cols - 1))
        func glow(_ c: Int) -> CGFloat {
            let t = age(c) / oldest                    // 0 fresh → 1 stale
            return 0.06 + 0.88 * pow(1 - t, 1.7)       // ease-out; never fully black
        }

        // Lit cells, column by column — center baseline outward. The column
        // under the write cursor (live) / playhead (reversing) burns brighter.
        // One batched fill per column instead of one per cell: the full-screen
        // wall repaints thousands of LEDs at 30 fps, and the per-cell fill is
        // what made it work harder than it needed to.
        for c in 0..<cols where written[c] {
            let up = min(halfRows, Int((lift(gridMax[c]) * CGFloat(halfRows)).rounded()))
            let down = min(halfRows, Int((lift(-gridMin[c]) * CGFloat(halfRows)).rounded()))
            let isHead = reversing ? c == playheadCol : c == (cursor - 1 + cols) % cols
            let base = reversing && (isHead || consumed(c)) ? NSColor.systemOrange : tint
            // Reverse keeps its own flat shading (the raster is frozen, so age
            // means nothing there); the live sweep decays.
            let alpha: CGFloat
            if reversing {
                alpha = isHead ? 1.0 : (consumed(c) ? 0.30 : 0.85)
            } else {
                alpha = isHead ? 1.0 : glow(c)
            }
            ctx.setFillColor(base.withAlphaComponent(alpha).cgColor)
            let x = cellX(c)
            columnRects.removeAll(keepingCapacity: true)
            for r in (centerRow - down)...(centerRow + up) {
                columnRects.append(CGRect(x: x, y: cellY(r), width: cellSize, height: cellSize))
            }
            ctx.fill(columnRects)
        }

        // Cursor / playhead column marker — a dim full-height lit column so
        // the write head reads even over silence. Orange during reverse.
        let headCol = reversing ? playheadCol : cursor
        if headCol >= 0, headCol < cols {
            let marker = (reversing ? NSColor.systemOrange : accent).withAlphaComponent(0.30)
            ctx.setFillColor(marker.cgColor)
            let x = cellX(headCol)
            columnRects.removeAll(keepingCapacity: true)
            for r in 0..<rows {
                columnRects.append(CGRect(x: x, y: cellY(r), width: cellSize, height: cellSize))
            }
            ctx.fill(columnRects)
        }
    }
}
