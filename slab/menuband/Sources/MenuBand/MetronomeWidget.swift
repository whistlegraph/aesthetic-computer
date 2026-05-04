import AppKit

/// Compact metronome widget for the popover title row: a custom-drawn
/// metronome body + a real swinging needle (animates when playing),
/// with a tiny BPM readout and `<` / `>` BPM-step arrows below.
///
/// Tapping the body toggles play/pause; tapping the arrows steps BPM
/// by 1 (default range 40…240). The actual click→sound wiring is up
/// to whatever metronome engine eventually drives `isPlaying` and
/// `bpm`; this view just owns the visuals + interaction.
final class MetronomeWidget: NSView {
    /// External setters drive the visuals — both the BPM readout and
    /// the swing tempo. Setting `bpm` while playing reschedules the
    /// needle animation to match the new beat.
    var bpm: Int = 120 {
        didSet {
            bpm = max(Self.minBPM, min(Self.maxBPM, bpm))
            updateBPMLabel()
            if abs(bpmSlider.doubleValue - Double(bpm)) > 0.5 {
                bpmSlider.doubleValue = Double(bpm)
            }
            if isPlaying { restartSwing() }
        }
    }

    /// Drives the needle animation. Off → needle freezes upright;
    /// on → needle swings ±25° at the current BPM.
    var isPlaying: Bool = false {
        didSet {
            guard isPlaying != oldValue else { return }
            isPlaying ? startSwing() : stopSwing()
        }
    }

    /// Tint color for the metronome body, needle, and arrows. The
    /// popover tints to white so the widget pops against the
    /// translucent title row.
    var tint: NSColor = .white {
        didSet { needsDisplay = true }
    }

    var onToggle: (() -> Void)?
    var onBPMChange: ((Int) -> Void)?

    private let bodyButton = NSButton()
    private let bpmLabel = NSTextField(labelWithString: "")
    private let bpmSlider = NSSlider()
    private let needleLayer = CAShapeLayer()

    private static let minBPM = 40
    private static let maxBPM = 240
    private static let bodySize = NSSize(width: 18, height: 20)
    private static let stepperWidth: CGFloat = 38
    private static let bodyToStepperGap: CGFloat = 4
    /// Needle pivots at the bottom-center of the body, swings between
    /// ±maxSwingAngle. 25° is roughly the visual swing of a real
    /// wind-up metronome's pendulum.
    private static let maxSwingAngle: CGFloat = 25.0 * .pi / 180.0

    override var intrinsicContentSize: NSSize {
        NSSize(
            width: Self.bodySize.width + Self.bodyToStepperGap + Self.stepperWidth,
            height: Self.bodySize.height
        )
    }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.masksToBounds = false
        setFrameSize(intrinsicContentSize)
        buildSubviews()
        configureNeedleLayer()
        updateBPMLabel()
    }

    required init?(coder: NSCoder) { fatalError() }

    override var isFlipped: Bool { false }

    override var mouseDownCanMoveWindow: Bool { false }

    // MARK: - Layout

    private func buildSubviews() {
        // Body button — invisible NSButton overlaid on the metronome
        // body region so the system handles hit testing + click
        // feedback. The actual graphic is drawn in `draw(_:)`.
        bodyButton.isBordered = false
        bodyButton.title = ""
        bodyButton.target = self
        bodyButton.action = #selector(bodyClicked)
        bodyButton.translatesAutoresizingMaskIntoConstraints = false
        addSubview(bodyButton)

        bpmLabel.font = NSFont.monospacedDigitSystemFont(ofSize: 8, weight: .bold)
        bpmLabel.textColor = .white
        bpmLabel.alignment = .center
        bpmLabel.translatesAutoresizingMaskIntoConstraints = false
        addSubview(bpmLabel)

        bpmSlider.minValue = Double(Self.minBPM)
        bpmSlider.maxValue = Double(Self.maxBPM)
        bpmSlider.doubleValue = Double(bpm)
        bpmSlider.controlSize = .mini
        bpmSlider.target = self
        bpmSlider.action = #selector(sliderChanged(_:))
        bpmSlider.toolTip = "Tempo"
        bpmSlider.translatesAutoresizingMaskIntoConstraints = false
        addSubview(bpmSlider)

        NSLayoutConstraint.activate([
            // Body sits flush left, vertically centered with the row.
            bodyButton.leadingAnchor.constraint(equalTo: leadingAnchor),
            bodyButton.centerYAnchor.constraint(equalTo: centerYAnchor),
            bodyButton.widthAnchor.constraint(equalToConstant: Self.bodySize.width),
            bodyButton.heightAnchor.constraint(equalToConstant: Self.bodySize.height),

            // BPM number floats above the slider, both anchored to the
            // right of the body. Compact stack so the whole widget
            // stays inside the row's vertical bounds.
            bpmLabel.leadingAnchor.constraint(equalTo: bodyButton.trailingAnchor, constant: Self.bodyToStepperGap),
            bpmLabel.trailingAnchor.constraint(equalTo: trailingAnchor),
            bpmLabel.topAnchor.constraint(equalTo: topAnchor),
            bpmLabel.heightAnchor.constraint(equalToConstant: 9),

            bpmSlider.leadingAnchor.constraint(equalTo: bpmLabel.leadingAnchor),
            bpmSlider.trailingAnchor.constraint(equalTo: bpmLabel.trailingAnchor),
            bpmSlider.topAnchor.constraint(equalTo: bpmLabel.bottomAnchor, constant: 1),
            bpmSlider.bottomAnchor.constraint(lessThanOrEqualTo: bottomAnchor),
        ])
    }

    private func configureNeedleLayer() {
        // The body's drawing is in this view's `draw(_:)`. The needle
        // is a CAShapeLayer so it can rotate cheaply via implicit
        // animation. Hosted in a sublayer with anchor at bottom-center
        // so a rotation pivots around the hinge, not the geometric
        // mid. Body sits flush-left and vertically centered, so the
        // needle pivot's x = body.midX (= bodySize.width / 2), y =
        // body.minY + small inset.
        guard let layer = self.layer else { return }
        let body = bodyRect
        let pivotX = body.midX
        let pivotY = body.minY + 2
        let needleLength = Self.bodySize.height - 5
        let host = CALayer()
        host.bounds = NSRect(x: 0, y: 0, width: 4, height: needleLength)
        host.anchorPoint = NSPoint(x: 0.5, y: 0.0)  // bottom-center
        host.position = NSPoint(x: pivotX, y: pivotY)

        let needlePath = NSBezierPath()
        needlePath.move(to: NSPoint(x: 2, y: 0))
        needlePath.line(to: NSPoint(x: 2, y: needleLength))
        needleLayer.path = needlePath.cgPath
        needleLayer.strokeColor = NSColor.white.cgColor
        needleLayer.lineWidth = 1.4
        needleLayer.lineCap = .round
        needleLayer.fillColor = nil
        needleLayer.frame = host.bounds
        // Counterweight bob at the tip — small filled disc so the
        // needle reads as a real metronome arm, not a tick mark.
        let bob = CAShapeLayer()
        let bobR: CGFloat = 1.6
        let bobPath = NSBezierPath(ovalIn: NSRect(
            x: 2 - bobR,
            y: needleLength - bobR,
            width: bobR * 2,
            height: bobR * 2
        ))
        bob.path = bobPath.cgPath
        bob.fillColor = NSColor.white.cgColor
        bob.frame = host.bounds
        host.addSublayer(needleLayer)
        host.addSublayer(bob)
        layer.addSublayer(host)
        self.needleHost = host
    }

    private weak var needleHost: CALayer?
    private var tickTimer: Timer?
    /// Pre-loaded system click sound. NSSound("Tink") is the closest
    /// thing macOS has to a wood-block click; pre-loading dodges the
    /// per-tick disk hit. Falls back silently if the sound is missing
    /// (theme override etc.) — the visual swing still works.
    private static let tickSound: NSSound? = NSSound(named: NSSound.Name("Tink"))

    // MARK: - Actions

    @objc private func bodyClicked() {
        isPlaying.toggle()
        onToggle?()
    }

    @objc private func sliderChanged(_ sender: NSSlider) {
        let next = Int(sender.doubleValue.rounded())
        guard next != bpm else { return }
        bpm = next
        onBPMChange?(bpm)
    }

    private func updateBPMLabel() {
        bpmLabel.stringValue = String(bpm)
    }

    // MARK: - Drawing

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        let body = bodyRect
        // Trapezoidal metronome body — wide at the bottom, narrow at
        // the top, like a pyramid wind-up case.
        let path = NSBezierPath()
        let topInset: CGFloat = body.width * 0.30
        path.move(to: NSPoint(x: body.minX + topInset, y: body.maxY))
        path.line(to: NSPoint(x: body.maxX - topInset, y: body.maxY))
        path.line(to: NSPoint(x: body.maxX, y: body.minY))
        path.line(to: NSPoint(x: body.minX, y: body.minY))
        path.close()
        path.lineWidth = 1.2
        tint.setStroke()
        path.stroke()
        // Base — short horizontal line at the very bottom so the body
        // reads as sitting on a stand.
        NSColor.clear.setFill()
    }

    private var bodyRect: NSRect {
        let y = (bounds.height - Self.bodySize.height) / 2
        return NSRect(x: 0, y: y, width: Self.bodySize.width, height: Self.bodySize.height)
    }

    // MARK: - Needle animation

    private static let swingAnimationKey = "needleSwing"

    private func startSwing() {
        restartSwing()
    }

    private func stopSwing() {
        needleHost?.removeAnimation(forKey: Self.swingAnimationKey)
        // Snap needle upright on stop — implicit transaction handles
        // the smooth interpolation back to angle 0.
        needleHost?.setAffineTransform(.identity)
        tickTimer?.invalidate()
        tickTimer = nil
    }

    private func restartSwing() {
        guard let host = needleHost else { return }
        host.removeAnimation(forKey: Self.swingAnimationKey)
        // One full swing = beat-tick-tock; a 120 BPM tempo means two
        // beats per second, so a left↔right cycle every 1.0 s. Period
        // = 60 / bpm * 2 (one full back-and-forth = two beats).
        let period = 60.0 / Double(bpm) * 2.0
        let anim = CAKeyframeAnimation(keyPath: "transform.rotation.z")
        anim.values = [0, Self.maxSwingAngle, 0, -Self.maxSwingAngle, 0]
        anim.keyTimes = [0, 0.25, 0.5, 0.75, 1]
        anim.duration = period
        anim.repeatCount = .infinity
        anim.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
        host.add(anim, forKey: Self.swingAnimationKey)

        // Audible tick aligned to the swing extremes — one click per
        // beat (= twice per swing period). Timer fires 0.25·period
        // after start so the first click lands when the needle is
        // first at full extension, then every period/2 thereafter.
        tickTimer?.invalidate()
        let beatInterval = period / 2.0
        let timer = Timer(timeInterval: beatInterval, repeats: true) { [weak self] _ in
            self?.playClick()
        }
        timer.tolerance = beatInterval * 0.05
        // Fire the first beat aligned to the needle peak (a quarter-
        // period after the swing starts). RunLoop.add with `.common`
        // so menu/event tracking doesn't suspend the ticks.
        timer.fireDate = Date(timeIntervalSinceNow: period * 0.25)
        RunLoop.main.add(timer, forMode: .common)
        tickTimer = timer
    }

    private func playClick() {
        // NSSound.play() restarts from the beginning if already mid-
        // playback — exactly what we want for rapid retriggers.
        Self.tickSound?.stop()
        Self.tickSound?.play()
    }
}

private extension NSBezierPath {
    /// `CGPath` bridge — AppKit's NSBezierPath has a `cgPath` getter
    /// only on macOS 14+. Hand-roll the conversion so this widget
    /// keeps working on macOS 11+.
    var cgPath: CGPath {
        let path = CGMutablePath()
        var points = [NSPoint](repeating: .zero, count: 3)
        for i in 0..<elementCount {
            let type = element(at: i, associatedPoints: &points)
            switch type {
            case .moveTo:
                path.move(to: points[0])
            case .lineTo:
                path.addLine(to: points[0])
            case .curveTo:
                path.addCurve(to: points[2], control1: points[0], control2: points[1])
            case .closePath:
                path.closeSubpath()
            default:
                continue
            }
        }
        return path
    }
}
