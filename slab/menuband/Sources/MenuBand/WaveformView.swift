import AppKit

/// Bottom-anchored audio bars. Single CAShapeLayer with one combined path
/// of all bar rects, single monochrome fill — no gradient, no glow, no
/// peak-hold decay. Plain 60 Hz Timer on `.common` runloop drives the
/// path update. Designed to be as cheap as possible per frame: read
/// samples, compute 32 peaks, build path, swap path. Hidden when MIDI
/// mode is on (synth silent there).
final class WaveformView: NSView {
    weak var menuBand: MenuBandController?

    private static let barCount = 32
    private static let barGap: CGFloat = 2
    private static let snapshotSize = 512   // samples we look at per frame

    private var samples = [Float](repeating: 0, count: snapshotSize)
    private let barLayer = CAShapeLayer()
    private var refreshTimer: Timer?

    var isLive: Bool = false {
        didSet {
            if isLive { startTimer() } else { stopTimer() }
        }
    }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.backgroundColor = NSColor.black.withAlphaComponent(0.92).cgColor
        layer?.cornerRadius = 8
        barLayer.fillColor = NSColor.systemTeal.cgColor
        barLayer.strokeColor = nil
        barLayer.actions = ["path": NSNull()] // no implicit anim on path
        layer?.addSublayer(barLayer)
    }
    required init?(coder: NSCoder) { fatalError() }

    override func layout() {
        super.layout()
        barLayer.frame = bounds
    }

    deinit { stopTimer() }

    private func startTimer() {
        stopTimer()
        let t = Timer(timeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in
            self?.tick()
        }
        // .common so it fires while the user is interacting with menus,
        // dragging, etc. Without this, the timer can stall to ~12 Hz.
        RunLoop.main.add(t, forMode: .common)
        refreshTimer = t
    }

    private func stopTimer() {
        refreshTimer?.invalidate()
        refreshTimer = nil
    }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if window == nil { stopTimer() }
    }

    private func tick() {
        guard let m = menuBand else { return }
        m.synthSnapshotWaveform(into: &samples)

        let w = bounds.width
        let h = bounds.height
        guard w > 0, h > 0 else { return }

        let n = Self.barCount
        let chunkSize = samples.count / n
        let barW = (w - Self.barGap * CGFloat(n - 1)) / CGFloat(n)
        let stride = barW + Self.barGap
        let gain: CGFloat = 2.5  // typical synth peak ~0.3–0.4, push toward full height

        let path = CGMutablePath()
        for b in 0..<n {
            var peak: Float = 0
            let base = b * chunkSize
            for i in 0..<chunkSize {
                let a = abs(samples[base + i])
                if a > peak { peak = a }
            }
            let lvl = Swift.min(1.0, CGFloat(peak) * gain)
            // Bottom-anchored: y=0 is bottom (NSView default coord space).
            let bh = Swift.max(1.5, lvl * h)
            let bx = CGFloat(b) * stride
            path.addRect(CGRect(x: bx, y: 0, width: barW, height: bh))
        }

        barLayer.path = path
    }
}
