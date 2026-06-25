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
    private static let historyCap = 400    // ~13 s ceiling at 30 fps (≥ rewind ring)
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
        if window != nil { start() } else { stop() }
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

    deinit { timer?.invalidate() }

    /// Seed the rolling history with a believable synthetic waveform so the
    /// strip paints real-looking bars in the HEADLESS App Store capture
    /// (`--render-popover`), where no audio engine is running and the live
    /// `tick()` path never accumulates columns. Pure Core Graphics — the
    /// strip's `draw(_:)` consumes `colMin`/`colMax` exactly as it would for
    /// live audio.
    ///
    /// Reads as a scope mid-playback: the LEFT portion is a flat horizontal
    /// line of silence (a centered baseline at ±~0), then the waveform "turns
    /// on" and fills the rest with a noisy, randomish played signal centered on
    /// that baseline — a sound that just started. Newest columns are drawn at
    /// the center playhead, oldest off to the left, so the silence sits at the
    /// far-left tail and the active audio leads up to the needle.
    /// No-op at runtime; only the capture path calls it.
    func seedSyntheticWaveform(columns: Int = 220) {
        let n = max(8, min(Self.historyCap, columns))
        colMin = []; colMax = []
        colMin.reserveCapacity(n); colMax.reserveCapacity(n)
        // i = 0 is oldest (far left); the last column sits at the center
        // playhead. Keep the first ~40% flat (silence), then ramp the activity
        // in so the audio reads as "just started playing."
        let silenceFraction: Float = 0.4
        // Deterministic value-noise so the capture is reproducible run-to-run
        // (no RNG seeding needed) yet looks irregular like real samples.
        func noise(_ x: Float) -> Float {
            let s = sinf(x * 12.9898) * 43758.5453
            return (s - s.rounded(.down)) * 2 - 1   // → [-1, 1)
        }
        for i in 0..<n {
            let t = Float(i) / Float(n - 1)
            if t < silenceFraction {
                // Flat baseline: a dead-center line (tiny floor so the strip
                // still ticks a 1-px silence bar rather than vanishing).
                colMax.append(0.015)
                colMin.append(-0.015)
                continue
            }
            // Ease the amplitude up from the silence edge so the onset isn't a
            // hard wall — a quick attack into a sustained, noisy signal.
            let local = (t - silenceFraction) / (1 - silenceFraction)
            let attack = min(1, local * 4)
            let x = Float(i)
            // Random-ish sample value: white-ish noise shaped by a low carrier
            // so it has body instead of reading as pure static.
            let body = sinf(x * 0.5) * 0.45
            let grain = noise(x) * 0.55 + noise(x * 2.3) * 0.25
            let top = (abs(body) * 0.5 + abs(grain)) * attack
            let bot = (abs(noise(x * 1.7)) * 0.7 + abs(body) * 0.4) * attack
            colMax.append(max(0.02, top))
            colMin.append(min(-0.02, -bot))
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
            scrubPos = CGFloat(progress * windowCols)
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
        let color = reversing ? NSColor.systemOrange : accent

        // Recessed screen.
        let plate = NSBezierPath(roundedRect: r.insetBy(dx: 0.5, dy: 0.5),
                                 xRadius: 4, yRadius: 4)
        NSColor.black.withAlphaComponent(0.55).setFill(); plate.fill()
        color.withAlphaComponent(0.55).setStroke(); plate.lineWidth = 1.0; plate.stroke()

        let mid = r.midY
        let cx = r.midX
        let amp = r.height * 0.72   // bigger / less tiny
        let n = colMin.count
        let step = Self.barStep

        func bar(_ idx: Int, x: CGFloat, alpha: CGFloat) {
            guard x >= 1, x <= r.width - 1 else { return }
            let top = mid - CGFloat(max(-1, min(1, colMax[idx]))) * amp
            let bot = mid - CGFloat(max(-1, min(1, colMin[idx]))) * amp
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

        // Center playhead bar — bright, full height.
        let needle = NSBezierPath(rect: NSRect(x: cx - 1, y: 2,
                                               width: 2, height: r.height - 4))
        color.withAlphaComponent(1.0).setFill(); needle.fill()
    }
}
