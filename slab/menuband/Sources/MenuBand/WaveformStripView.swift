import AppKit

/// Compact live audio strip for the popover, shown above the instrument
/// name. Instead of redrawing one instantaneous snapshot (which just
/// flickers in place), it accumulates a SCROLLING min/max envelope — one
/// column per frame, newest on the right, older columns drifting left — so
/// you watch the recent buffer scroll by, the way native notepat's top
/// strip does. ▶ while playing live (forward); while the spacebar reverse-
/// replay is sounding the strip flips ◀ + orange and the trace mirrors so
/// you can see it scrub backward through the buffer. Makes spacebar-rewind
/// debuggable at a glance.
///
/// Lightweight (Core Graphics, polled at ~30 fps only while on screen).
final class WaveformStripView: NSView {
    weak var menuBand: MenuBandController?

    /// One frame's worth of the tap ring (~46 ms at 44.1k); we reduce it to
    /// a single min/max column per tick.
    private var samples = [Float](repeating: 0, count: 2048)
    /// Rolling per-column envelope. ~256 columns × ~33 ms/frame ≈ 8 s of
    /// scrolling history (close to the 12 s rewind ring). Index 0 is oldest.
    private static let columnCount = 256
    private var colMin = [Float](repeating: 0, count: columnCount)
    private var colMax = [Float](repeating: 0, count: columnCount)
    private var timer: Timer?

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

    private func tick() {
        menuBand?.synthSnapshotWaveform(into: &samples)
        var mn: Float = 0, mx: Float = 0
        for s in samples {
            if s < mn { mn = s }
            if s > mx { mx = s }
        }
        // Push one new column on the right, drop the oldest on the left.
        colMin.removeFirst(); colMin.append(mn)
        colMax.removeFirst(); colMax.append(mx)
        needsDisplay = true
    }

    override func draw(_ dirtyRect: NSRect) {
        let r = bounds
        guard r.width > 4, r.height > 4 else { return }
        let reversing = menuBand?.isRewinding ?? false
        let accent = NSColor.controlAccentColor
        let color = reversing ? NSColor.systemOrange : accent

        // Recessed screen — opaque enough to read over the glass + when silent.
        let plate = NSBezierPath(roundedRect: r.insetBy(dx: 0.5, dy: 0.5),
                                 xRadius: 4, yRadius: 4)
        NSColor.black.withAlphaComponent(0.55).setFill(); plate.fill()
        color.withAlphaComponent(0.6).setStroke(); plate.lineWidth = 1.0; plate.stroke()

        let mid = r.midY
        let zero = NSBezierPath()
        zero.move(to: NSPoint(x: 3, y: mid)); zero.line(to: NSPoint(x: r.width - 3, y: mid))
        color.withAlphaComponent(0.25).setStroke(); zero.lineWidth = 0.5; zero.stroke()

        let n = Self.columnCount
        let inset: CGFloat = 4
        let usableW = r.width - inset * 2
        let amp = r.height * 0.44
        // Scrolling min/max bars. Newest column on the RIGHT in live mode; in
        // reverse the column order is mirrored so the trace runs the other
        // way (you see the buffer scrub backward).
        color.withAlphaComponent(0.95).setStroke()
        let bars = NSBezierPath()
        bars.lineWidth = 1.0
        for i in 0..<n {
            let col = reversing ? (n - 1 - i) : i
            let x = inset + usableW * CGFloat(i) / CGFloat(n - 1)
            let top = mid - CGFloat(max(-1, min(1, colMax[col]))) * amp
            let bot = mid - CGFloat(max(-1, min(1, colMin[col]))) * amp
            // Ensure at least a 0.5px tick even on near-silence so the
            // scroll is visible as a moving baseline texture.
            bars.move(to: NSPoint(x: x, y: min(top, bot)))
            bars.line(to: NSPoint(x: x, y: max(top, bot) + 0.5))
        }
        bars.stroke()

        // Direction glyph in the corner the trace flows toward.
        let glyph = NSAttributedString(
            string: reversing ? "◀" : "▶",
            attributes: [
                .font: NSFont.systemFont(ofSize: 9, weight: .bold),
                .foregroundColor: color,
            ])
        let gs = glyph.size()
        let gx = reversing ? 4 : r.width - gs.width - 4
        glyph.draw(at: NSPoint(x: gx, y: mid - gs.height / 2))
    }
}
