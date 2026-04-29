import AppKit

/// Bottom-anchored audio bars synced to the display's vsync via
/// CVDisplayLink — Timer-on-runloop was throttling to ~12 Hz inside the
/// NSPopover window's mode. CVDisplayLink fires at the screen's refresh
/// rate (60 Hz on most Macs, 120 on ProMotion) regardless of run-loop
/// scheduling. Single CAShapeLayer + one combined path of 32 bar rects,
/// monochrome systemTeal fill, no decay. Hidden when MIDI mode is on.
final class WaveformView: NSView {
    weak var menuBand: MenuBandController?

    private static let barCount = 32
    private static let barGap: CGFloat = 2
    private static let snapshotSize = 512

    private var samples = [Float](repeating: 0, count: snapshotSize)
    private let barLayer = CAShapeLayer()
    private var displayLink: CVDisplayLink?

    /// `true` while a `tick()` dispatch is queued to main but hasn't yet
    /// run. CVDisplayLink fires every vsync; if the main thread is briefly
    /// busy we MUST drop frames instead of stacking them up — a backlog of
    /// dispatch_main blocks turns into post-busy stutter that reads as
    /// "the visualizer is laggy."
    private var tickPending = false
    private let tickLock = NSLock()

    /// Smoothed peak across recent frames so the auto-gain doesn't strobe
    /// — when a transient hits, gain snaps; in silence, gain bleeds back
    /// over half a second so the next note pops.
    private var smoothedPeak: Float = 0.05

    var isLive: Bool = false {
        didSet {
            if isLive { startLink() } else { stopLink() }
        }
    }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.backgroundColor = NSColor.black.withAlphaComponent(0.92).cgColor
        layer?.cornerRadius = 8
        // Match the menubar piano's lit color: bars use the system accent.
        // Re-applied in `viewDidChangeEffectiveAppearance` so changing the
        // user's accent in System Settings updates the bars without restart.
        barLayer.fillColor = NSColor.controlAccentColor.cgColor
        barLayer.strokeColor = nil
        barLayer.actions = ["path": NSNull()]
        layer?.addSublayer(barLayer)
    }
    required init?(coder: NSCoder) { fatalError() }

    override func layout() {
        super.layout()
        barLayer.frame = bounds
    }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        // Accent color is appearance-derived — re-pull when the user flips
        // light/dark or changes the system accent in Settings.
        barLayer.fillColor = NSColor.controlAccentColor.cgColor
    }

    deinit { stopLink() }

    private func startLink() {
        stopLink()
        var link: CVDisplayLink?
        CVDisplayLinkCreateWithActiveCGDisplays(&link)
        guard let link = link else { return }
        let opaque = Unmanaged.passUnretained(self).toOpaque()
        CVDisplayLinkSetOutputCallback(link, { _, _, _, _, _, ctx in
            guard let ctx = ctx else { return kCVReturnSuccess }
            let view = Unmanaged<WaveformView>.fromOpaque(ctx).takeUnretainedValue()
            // Coalesce: only queue a main-thread tick if we don't already
            // have one waiting. Without this, every vsync queues a tick
            // even when main is too busy to service them, so a brief stall
            // turns into a long chain of catch-up frames that reads as lag.
            view.tickLock.lock()
            let alreadyPending = view.tickPending
            view.tickPending = true
            view.tickLock.unlock()
            if alreadyPending { return kCVReturnSuccess }
            DispatchQueue.main.async { view.tick() }
            return kCVReturnSuccess
        }, opaque)
        CVDisplayLinkStart(link)
        displayLink = link
    }

    private func stopLink() {
        if let link = displayLink {
            CVDisplayLinkStop(link)
            displayLink = nil
        }
    }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        if window == nil { stopLink() }
    }

    private func tick() {
        tickLock.lock()
        tickPending = false
        tickLock.unlock()
        guard let m = menuBand else { return }
        m.synthSnapshotWaveform(into: &samples)

        let w = bounds.width
        let h = bounds.height
        guard w > 0, h > 0 else { return }

        // Per-bar peak amplitude.
        let n = Self.barCount
        let chunkSize = samples.count / n
        var framePeak: Float = 0
        var levels = [Float](repeating: 0, count: n)
        for b in 0..<n {
            var peak: Float = 0
            let base = b * chunkSize
            for i in 0..<chunkSize {
                let a = abs(samples[base + i])
                if a > peak { peak = a }
            }
            levels[b] = peak
            if peak > framePeak { framePeak = peak }
        }

        // Auto-gain normalization. Track a smoothed peak — when current
        // frame is louder, jump up immediately so attack reads; when
        // quieter, decay over ~½ s so a sustained-quiet note still pushes
        // bars high. Floor at 0.05 so we never amplify the noise floor to
        // full scale.
        if framePeak > smoothedPeak {
            smoothedPeak = framePeak
        } else {
            smoothedPeak = max(0.05, smoothedPeak * 0.92 + framePeak * 0.08)
        }
        let gain = CGFloat(0.95) / CGFloat(smoothedPeak)

        // Build the bar path.
        let barW = (w - Self.barGap * CGFloat(n - 1)) / CGFloat(n)
        let stride = barW + Self.barGap
        let path = CGMutablePath()
        for b in 0..<n {
            let normalized = Swift.min(1.0, CGFloat(levels[b]) * gain)
            let bh = Swift.max(1.5, normalized * h)
            let bx = CGFloat(b) * stride
            path.addRect(CGRect(x: bx, y: 0, width: barW, height: bh))
        }
        // Disable implicit animations on the path swap — the layer's
        // `actions` dict already nullifies "path", but wrapping the
        // assignment in a no-action transaction is a belt-and-suspenders
        // guarantee that no 0.25 s default fade sneaks in.
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        barLayer.path = path
        CATransaction.commit()
    }
}
