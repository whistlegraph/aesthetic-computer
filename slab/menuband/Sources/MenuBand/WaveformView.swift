import AppKit

/// Live audio visualizer for the local synth output. Single antialiased
/// line, stroked with a horizontal rainbow gradient (CAGradientLayer +
/// CAShapeLayer mask) so it reads as a "visualizer" rather than a flat
/// scope. Hidden when MIDI mode is on (DAW handles audio there; the
/// local mixer is silent).
///
/// Layer-based draw path: the path is updated on a 60 Hz timer with
/// implicit animations off, so each frame snaps cleanly to the new
/// shape with no tweening. Cheap enough to keep the popover snappy.
final class WaveformView: NSView {
    weak var menuBand: MenuBandController?

    private static let sampleCount = 512
    private var samples = [Float](repeating: 0, count: sampleCount)

    private let gradientLayer = CAGradientLayer()
    private let lineMask = CAShapeLayer()
    private let glowLayer = CAShapeLayer()

    private var refreshTimer: Timer?
    private var hue: CGFloat = 0  // slowly rotated each frame for shimmer

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
        layer?.borderWidth = 0.5
        layer?.borderColor = NSColor.white.withAlphaComponent(0.10).cgColor

        // A soft glow under the line — lower opacity, wider stroke.
        glowLayer.fillColor = nil
        glowLayer.lineWidth = 4.5
        glowLayer.lineJoin = .round
        glowLayer.lineCap = .round
        glowLayer.strokeColor = NSColor.white.withAlphaComponent(0.20).cgColor
        glowLayer.shadowColor = NSColor.systemTeal.cgColor
        glowLayer.shadowOpacity = 0.55
        glowLayer.shadowRadius = 6
        glowLayer.shadowOffset = .zero
        layer?.addSublayer(glowLayer)

        // Sharp foreground line, masked by the rainbow gradient.
        lineMask.fillColor = nil
        lineMask.lineWidth = 1.5
        lineMask.lineJoin = .round
        lineMask.lineCap = .round
        lineMask.strokeColor = NSColor.black.cgColor

        gradientLayer.colors = [
            NSColor.systemPink.cgColor,
            NSColor.systemOrange.cgColor,
            NSColor.systemYellow.cgColor,
            NSColor.systemGreen.cgColor,
            NSColor.systemTeal.cgColor,
            NSColor.systemPurple.cgColor,
        ]
        gradientLayer.startPoint = CGPoint(x: 0.0, y: 0.5)
        gradientLayer.endPoint   = CGPoint(x: 1.0, y: 0.5)
        gradientLayer.mask = lineMask
        layer?.addSublayer(gradientLayer)
    }
    required init?(coder: NSCoder) { fatalError() }

    override var wantsUpdateLayer: Bool { true }

    override func layout() {
        super.layout()
        gradientLayer.frame = bounds
        glowLayer.frame = bounds
        lineMask.frame = bounds
    }

    private func startTimer() {
        stopTimer()
        // 60 Hz. Path update is GPU-accelerated; CPU cost is the sample
        // walk + path build (512 line segments) — negligible.
        refreshTimer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in
            self?.tick()
        }
        if let t = refreshTimer { RunLoop.main.add(t, forMode: .common) }
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
        let midY = h * 0.5

        // Auto-gain: scale to ~85% of half-height by peak. Floor the peak
        // so silence doesn't blow up to look like noise.
        var peak: Float = 0.05
        for s in samples {
            let a = abs(s)
            if a > peak { peak = a }
        }
        let scale = CGFloat(0.88) / CGFloat(min(max(peak, 0.05), 1.0))

        let path = CGMutablePath()
        let n = samples.count
        guard n > 1 else { return }
        let step = w / CGFloat(n - 1)
        for i in 0..<n {
            let x = CGFloat(i) * step
            let y = midY - CGFloat(samples[i]) * midY * scale
            if i == 0 {
                path.move(to: CGPoint(x: x, y: y))
            } else {
                path.addLine(to: CGPoint(x: x, y: y))
            }
        }

        // Rotate the gradient hue slowly so quiet content still looks alive.
        hue += 0.0035
        if hue > 1.0 { hue -= 1.0 }
        let h0 = hue
        let h1 = (hue + 0.16).truncatingRemainder(dividingBy: 1.0)
        let h2 = (hue + 0.33).truncatingRemainder(dividingBy: 1.0)
        let h3 = (hue + 0.50).truncatingRemainder(dividingBy: 1.0)
        let h4 = (hue + 0.66).truncatingRemainder(dividingBy: 1.0)
        let h5 = (hue + 0.83).truncatingRemainder(dividingBy: 1.0)
        let cgcolor = { (hue: CGFloat) -> CGColor in
            NSColor(hue: hue, saturation: 0.85, brightness: 1.0, alpha: 1.0).cgColor
        }

        CATransaction.begin()
        CATransaction.setDisableActions(true)
        gradientLayer.colors = [cgcolor(h0), cgcolor(h1), cgcolor(h2),
                                cgcolor(h3), cgcolor(h4), cgcolor(h5)]
        lineMask.path = path
        glowLayer.path = path
        CATransaction.commit()
    }
}
