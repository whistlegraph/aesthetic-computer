import AppKit

/// Compact live audio scope for the popover, above the instrument name —
/// modeled on native notepat's top strip. A bright vertical **playhead bar
/// sits at the center**; chunky waveform bars are born AT the playhead and
/// march outward: to the LEFT while playing live, and to the RIGHT while the
/// spacebar reverse-replay scrubs the buffer backward (drift flips like
/// notepat's `waveDriftSpeed`). Short window, low-res/bold bars so it reads
/// at a glance and you can watch the rewind direction.
///
/// Lightweight (Core Graphics, polled ~30 fps only while on screen).
final class WaveformStripView: NSView {
    weak var menuBand: MenuBandController?

    /// Optional tint for the live scope. `nil` → the system accent (the
    /// popover default); the full-screen overlay feeds the active voice's
    /// family color so the scope matches its per-voice framing. Rewind still
    /// flips to orange regardless of this.
    var tintColor: NSColor? {
        didSet { needsDisplay = true }
    }

    /// When false the strip stops capturing and clears to a flat baseline —
    /// used while the host panel is hidden or MIDI mode owns the output.
    var isLive: Bool = true {
        didSet {
            guard isLive != oldValue, window != nil else { return }
            if isLive { start() } else { freezeToBaseline() }
        }
    }

    /// Draw the recessed black plate + frame. The popover wants it (the strip
    /// IS the visual there); the full-screen overlay turns it off so the scope
    /// paints straight onto the bezel/glass it's already embedded in.
    var drawsPlate: Bool = true {
        didSet { needsDisplay = true }
    }

    /// Fired when the strip is clicked. The popover wires this to open the
    /// full keymap view — same action as the "Keymap" button — so the live
    /// scope doubles as a keymap affordance.
    var onClick: (() -> Void)?

    override func mouseDown(with event: NSEvent) {
        if onClick != nil { onClick?() } else { super.mouseDown(with: event) }
    }

    /// Pointing-hand cursor so the strip reads as clickable.
    override func resetCursorRects() {
        if onClick != nil { addCursorRect(bounds, cursor: .pointingHand) }
    }

    /// One frame's worth of the tap ring, reduced to a single min/max
    /// column. Sized to cover MORE than one 30 fps frame even at 96 kHz
    /// (96000/30 = 3200 samples) so consecutive columns don't skip audio
    /// between them.
    private var samples = [Float](repeating: 0, count: 4096)
    /// Rolling history (index 0 oldest, last = newest). New live columns are
    /// pushed at the center; reverse playback freezes pushing and scrubs the
    /// frozen buffer back through the center line.
    private var colMin: [Float] = []
    private var colMax: [Float] = []
    private static let historyCap = 2800   // ≥ 90 s at 30 fps so the reverse scrub covers the whole window
    /// Bar geometry — chunky + bold, low-res on purpose.
    private static let barStep: CGFloat = 5   // px between bars
    private static let barWidth: CGFloat = 3
    private var timer: Timer?
    /// While reversing: how many columns the playhead has scrubbed back from
    /// the newest (advances ~1/frame so it tracks the 30 fps capture rate).
    private var scrubPos: CGFloat = 0
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

    /// Stop capturing and clear the buffer so the scope shows a clean flat
    /// baseline (used when `isLive` is switched off).
    private func freezeToBaseline() {
        stop()
        colMin.removeAll(); colMax.removeAll()
        scrubPos = 0
        needsDisplay = true
    }

    deinit { timer?.invalidate() }

    /// Seed the rolling history with a believable synthetic note so the strip
    /// paints what it really would when a note plays, in the HEADLESS App Store
    /// capture (`--render-popover`), where no audio engine runs and the live
    /// `tick()` path never accumulates columns. Pure Core Graphics — the
    /// strip's `draw(_:)` consumes `colMin`/`colMax` exactly as for live audio.
    ///
    /// MATCHES draw()'s geometry exactly. Live draw walks
    /// `bar(n-1-i, x: cx - i*step)`: the NEWEST column (`n-1`) lands on the
    /// bright center playhead, and older columns scroll LEFT (oldest at the far
    /// left). Nothing is drawn RIGHT of the playhead — that's the not-yet-played
    /// future, so the seed leaves it empty by virtue of stopping at `n-1`.
    ///
    /// Reading left→right is forward in time: far-left near-silence (before the
    /// strike) → a sharp ATTACK transient → an exponentially DECAYING sustain
    /// that trails up to the center playhead. So the column AT the needle is the
    /// freshest, partly-decayed sustain — exactly the picture of "a note is
    /// ringing out." Bars are chunky min/max pairs centered on the baseline,
    /// like a real low-res scope. No-op at runtime; only the capture calls it.
    func seedSyntheticWaveform(columns: Int = 220) {
        let n = max(8, min(Self.historyCap, columns))
        colMin = []; colMax = []
        colMin.reserveCapacity(n); colMax.reserveCapacity(n)
        // Deterministic value-noise so the capture is reproducible run-to-run
        // (no RNG seeding) yet looks irregular like real sampled audio.
        func noise(_ x: Float) -> Float {
            let s = sinf(x * 12.9898) * 43758.5453
            return (s - s.rounded(.down)) * 2 - 1   // → [-1, 1)
        }
        // Only a window near the NEWEST end is ever on screen: draw() shows
        // ~`(width/2)/barStep` columns left of the center playhead (≈20–26 for
        // the popover strip). Place the strike INSIDE that visible window so the
        // captured frame reads silence → attack → decay rather than just a flat
        // decayed tail. ~22 columns back from the newest puts the onset a little
        // left of center with the ring-out trailing to the playhead, and the
        // pre-strike silence filling the visible far left.
        let visibleCols: Float = 22
        // Strike near the LEFT edge of the visible window so the whole ring-out
        // fills the on-screen span: a little pre-strike silence at the far left,
        // then a tall attack, then a gentle decay still carrying real amplitude
        // all the way to the center playhead (a note actively ringing — not one
        // that's already faded to the baseline before reaching the needle).
        let onsetIdx = Float(n - 1) - visibleCols * 0.9    // ≈20 cols before newest
        // Slow decay across the visible window so amplitude at the playhead is
        // still a healthy fraction of the peak.
        let decayCols = visibleCols * 1.4
        for i in 0..<n {
            let pos = Float(i)
            let x = pos
            // ADSR-ish envelope in note time. `since` = columns since the
            // strike (negative = pre-strike silence, growing positive = ringing
            // out toward the playhead).
            let since = pos - onsetIdx
            var env: Float
            if since < 0 {
                // Pre-strike: flat silence baseline at the far left.
                env = 0
            } else {
                // Fast attack (≈2 columns) into a gentle exponential decay —
                // the unmistakable shape of a plucked / struck instrument, with
                // a sustain floor so the tail keeps body up to the playhead.
                let attack = min(1, since / 2)
                let decay = 0.45 + 0.55 * expf(-since / decayCols * 2.0)
                env = attack * decay
            }
            // Sample value: a voiced carrier (gives the bars a periodic, musical
            // body rather than pure hiss) plus a little grain for texture.
            let carrier = sinf(x * 0.9) * 0.55 + sinf(x * 0.37) * 0.25
            let grain = noise(x) * 0.22
            let sample = (carrier + grain)            // ≈ [-1, 1]
            // Chunky min/max column centered on the baseline. Slight asymmetry
            // so top/bottom aren't a perfect mirror, like real audio.
            let amp = env * 0.92
            let top = abs(sample) * amp
            let bot = abs(sample * 0.85 + grain * 0.5) * amp
            // Tiny silence floor so even dead columns tick a 1-px baseline bar
            // (matches draw()'s `max(1.5, …)` height floor visually).
            colMax.append(max(0.012, top))
            colMin.append(min(-0.012, -bot))
        }
        needsDisplay = true
    }

    private func tick() {
        let reversing = menuBand?.isRewinding ?? false
        if reversing {
            // Freeze capture; position the playhead from the ACTUAL reverse
            // playback progress (exact, drift-free) instead of a free-running
            // counter — so the visual scrub stays locked to the audio. scrub
            // sweeps 0 → windowCols as progress goes 0 → 1.
            let windowCols = Double((menuBand?.rewindWindowSeconds ?? 1.5)) * 30.0
            let progress = menuBand?.rewindProgress() ?? 0
            let target = CGFloat(progress * windowCols)
            // The player's progress is polled at 30 fps and can land unevenly,
            // which made the scrub jump/skip on the x axis. Ease toward the
            // audio-derived target (snap on the first reverse frame) so the
            // playhead glides smoothly while staying locked to the audio.
            scrubPos = wasReversing ? scrubPos + (target - scrubPos) * 0.4 : target
        } else {
            // Live: reduce this frame to one min/max column, push it (it's
            // drawn at the center and the older columns drift left).
            menuBand?.synthSnapshotWaveform(into: &samples)
            var mn: Float = 0, mx: Float = 0
            for s in samples {
                if s < mn { mn = s }
                if s > mx { mx = s }
            }
            colMin.append(mn); colMax.append(mx)
            if colMin.count > Self.historyCap { colMin.removeFirst(); colMax.removeFirst() }
        }
        wasReversing = reversing
        needsDisplay = true
    }

    override func draw(_ dirtyRect: NSRect) {
        let r = bounds
        guard r.width > 6, r.height > 6 else { return }
        let reversing = menuBand?.isRewinding ?? false
        let accent = NSColor.controlAccentColor
        let base = tintColor ?? accent
        let color = reversing ? NSColor.systemOrange : base   // the signal bars
        // The frame (outline) + needle read as the accent color regardless of
        // the bar signal color, so the scope always has a consistent accent
        // frame even while the bars flip orange during reverse.
        let isDark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua

        // Recessed screen — appearance-aware so light mode gets a light plate
        // instead of the hardcoded near-black (which only read in dark mode).
        // Skipped when embedded in a host bezel that already supplies the
        // framing (the full-screen overlay).
        if drawsPlate {
            let plate = NSBezierPath(roundedRect: r.insetBy(dx: 0.5, dy: 0.5),
                                     xRadius: 4, yRadius: 4)
            (isDark ? NSColor.black.withAlphaComponent(0.55)
                    : NSColor(white: 0.90, alpha: 0.85)).setFill()
            plate.fill()
            accent.withAlphaComponent(0.55).setStroke(); plate.lineWidth = 1.0; plate.stroke()
        }

        let mid = r.midY
        let cx = r.midX
        // Bars are mirrored around `mid`, so a full-scale (±1) sample must stay
        // within HALF the strip height. Scale to ~0.9 of that half (1px inner
        // margin top + bottom) so even peak audio never pokes past the recessed
        // plate's frame. (Previously `r.height * 0.72` measured the FULL height,
        // letting loud columns overflow above/below the strip.)
        let halfInset = max(1, r.height / 2 - 1)
        let amp = halfInset * 0.9
        let n = colMin.count
        let step = Self.barStep

        func bar(_ idx: Int, x: CGFloat, alpha: CGFloat) {
            guard x >= 1, x <= r.width - 1 else { return }
            var top = mid - CGFloat(max(-1, min(1, colMax[idx]))) * amp
            var bot = mid - CGFloat(max(-1, min(1, colMin[idx]))) * amp
            // Hard clamp to the strip's inner frame — belt-and-suspenders so no
            // data path (live or seeded) can ever draw outside the plate.
            let topLimit = r.maxY - 1, botLimit = r.minY + 1
            top = max(botLimit, min(topLimit, top))
            bot = max(botLimit, min(topLimit, bot))
            let h = max(1.5, abs(bot - top))   // floor so silence still ticks
            color.withAlphaComponent(alpha).setFill()
            NSBezierPath(rect: NSRect(x: x - Self.barWidth / 2, y: min(top, bot),
                                      width: Self.barWidth, height: h)).fill()
        }

        if reversing, n > 0 {
            // Scrub ONLY the reverse window (the same span the audio plays),
            // rightward through the center playhead. At scrub 0 the window's
            // newest sample is at center; as scrub advances the center tracks
            // progressively older samples — exactly what the one-shot reverse
            // is sounding — and already-played (newer) bars pass the middle
            // line to the right and fade out ("consumed").
            let windowSec = menuBand?.rewindWindowSeconds ?? 1.5
            let windowCols = max(1, Int(windowSec * 30))   // strip captures at 30 fps
            for i in 0..<min(windowCols, n) {
                let idx = n - 1 - i            // i = 0 → newest (window start)
                let x = cx + (scrubPos - CGFloat(i)) * step
                let past = x - cx
                let alpha: CGFloat = past > 0 ? max(0, 0.95 - past / (r.width / 2)) : 0.95
                bar(idx, x: x, alpha: alpha)
            }
        } else {
            // Live: newest generated AT the center, older columns drift LEFT.
            let visible = max(0, Int((r.width / 2 - 3) / step))
            for i in 0..<min(visible, n) {
                bar(n - 1 - i, x: cx - CGFloat(i) * step, alpha: 0.95)
            }
        }

        // Center playhead needle — the accent color, full height.
        let needle = NSBezierPath(rect: NSRect(x: cx - 1, y: 2,
                                               width: 2, height: r.height - 4))
        accent.withAlphaComponent(1.0).setFill(); needle.fill()
    }
}
