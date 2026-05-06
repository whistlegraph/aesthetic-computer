import AppKit

/// Compact metronome widget for the popover title row: trapezoidal
/// wind-up body + a real swinging needle (animates when playing or
/// when scrubbing) and a BPM number readout to its right.
///
/// Interaction (kept from the redesigned dial — only the visuals
/// reverted to the original wind-up graphic):
/// - Drag horizontally → scrub BPM. Each BPM step plays a quiet
///   "Pop" detent click and gives the needle a small nudge so the
///   widget reads as a real ratcheting dial.
/// - Click (≤2 pt motion) → toggle play/pause; while playing the
///   needle swings continuously at the current BPM.
final class MetronomeWidget: NSView {
    // MARK: - Public API

    var bpm: Int = 120 {
        didSet {
            bpm = max(Self.minBPM, min(Self.maxBPM, bpm))
            updateBPMLabel()
            if isPlaying { restartSwing() }
        }
    }

    var isPlaying: Bool = false {
        didSet {
            guard isPlaying != oldValue else { return }
            isPlaying ? startSwing() : stopSwing()
            KeyboardIconRenderer.metronomeOn = isPlaying
            onRunningChanged?()
        }
    }
    var onRunningChanged: (() -> Void)?

    var tint: NSColor = .labelColor {
        didSet {
            applyTintToNeedle()
            needsDisplay = true
        }
    }

    var onToggle: (() -> Void)?
    var onBPMChange: ((Int) -> Void)?
    var onTick: (() -> Void)?

    // MARK: - Layout constants

    private static let minBPM = 40
    private static let maxBPM = 240
    private static let bodySize = NSSize(width: 18, height: 20)
    private static let bodyToLabelGap: CGFloat = 1
    /// Needle pivots at the bottom-center of the body, swings between
    /// ±maxSwingAngle. 25° approximates a real wind-up metronome.
    private static let maxSwingAngle: CGFloat = 25.0 * .pi / 180.0
    private static let swingAnimationKey = "needleSwing"
    /// 1 pt of drag = ~0.5 BPM so the dial feels precise.
    private static let pointsPerBPM: CGFloat = 2

    override var intrinsicContentSize: NSSize {
        // Trapezoid stacked on top of a small BPM number — vertical
        // layout so the metronome can sit centered in the popover
        // title row directly above the staff's beat-marker bars.
        NSSize(
            width: max(Self.bodySize.width, 26),
            height: Self.bodySize.height + Self.bodyToLabelGap + 11
        )
    }

    override var isFlipped: Bool { false }
    override var mouseDownCanMoveWindow: Bool { false }

    private let bpmLabel = NSTextField(labelWithString: "")
    private let needleLayer = CAShapeLayer()
    private let bobLayer = CAShapeLayer()
    private weak var needleHost: CALayer?
    private var tickTimer: Timer?
    /// Beat sound — one click per BPM tick while playing.
    private static let beatSound: NSSound? = NSSound(named: NSSound.Name("Tink"))
    /// Quieter detent click while scrubbing the BPM by drag.
    private static let scrubClickSound: NSSound? = {
        let s = NSSound(named: NSSound.Name("Pop"))
        s?.volume = 0.18
        return s
    }()

    // MARK: - Init

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

    private func buildSubviews() {
        // BPM number sits directly BELOW the trapezoid — vertical
        // stack so the trapezoid lines up over the staff's
        // beat-marker bars and the number reads as its caption.
        bpmLabel.font = NSFont.monospacedDigitSystemFont(ofSize: 9, weight: .bold)
        bpmLabel.textColor = .labelColor
        bpmLabel.alignment = .center
        bpmLabel.translatesAutoresizingMaskIntoConstraints = false
        addSubview(bpmLabel)
        let trackingOptions: NSTrackingArea.Options = [
            .mouseEnteredAndExited, .activeInActiveApp,
            .inVisibleRect, .cursorUpdate,
        ]
        addTrackingArea(NSTrackingArea(
            rect: bounds, options: trackingOptions, owner: self, userInfo: nil
        ))
        NSLayoutConstraint.activate([
            bpmLabel.centerXAnchor.constraint(equalTo: centerXAnchor),
            bpmLabel.bottomAnchor.constraint(equalTo: bottomAnchor),
            bpmLabel.heightAnchor.constraint(equalToConstant: 11),
        ])
    }

    // MARK: - Cursor

    override func cursorUpdate(with event: NSEvent) {
        // pointing-hand on the trapezoid (it reads as a "tap to
        // toggle"), resize-leftRight on the BPM label area
        // (drag-to-scrub).
        let p = convert(event.locationInWindow, from: nil)
        if bodyRect.contains(p) {
            NSCursor.pointingHand.set()
        } else {
            NSCursor.resizeLeftRight.set()
        }
    }

    // MARK: - Mouse drag → BPM scrub, click → toggle play

    private var dragStartLocation: NSPoint?
    private var dragStartBPM: Int = 0

    override func mouseDown(with event: NSEvent) {
        let p = convert(event.locationInWindow, from: nil)
        dragStartLocation = p
        dragStartBPM = bpm
    }

    override func mouseDragged(with event: NSEvent) {
        guard let start = dragStartLocation else {
            super.mouseDragged(with: event); return
        }
        let p = convert(event.locationInWindow, from: nil)
        let dx = p.x - start.x
        let target = dragStartBPM + Int((dx / Self.pointsPerBPM).rounded())
        let clamped = max(Self.minBPM, min(Self.maxBPM, target))
        if clamped != bpm {
            bpm = clamped
            onBPMChange?(clamped)
            playScrubClick()
            nudgeNeedle()
        }
        NSCursor.resizeLeftRight.set()
    }

    override func mouseUp(with event: NSEvent) {
        let p = convert(event.locationInWindow, from: nil)
        let drag = dragStartLocation
        dragStartLocation = nil
        guard let drag = drag else { return }
        let moved = abs(p.x - drag.x) > 2 || abs(p.y - drag.y) > 2
        if moved {
            // Drag-to-scrub finished — needle snaps back and the
            // metronome starts ticking automatically if it wasn't
            // already running. If the user wanted to silence it
            // they can click to toggle off.
            if !isPlaying {
                isPlaying = true
                onToggle?()
            }
        } else {
            // Sloppy-click threshold (≤2pt motion) → toggle.
            isPlaying.toggle()
            onToggle?()
        }
    }

    // MARK: - Drawing

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        let body = bodyRect
        // Trapezoidal metronome body — wide at the bottom, narrow at
        // the top, like a wind-up case.
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
    }

    /// Center of the metronome's trapezoid in this view's
    /// coordinate space — the popover uses this as the X anchor
    /// for staff beat-marker bars so each tick literally drops
    /// out of the metronome's needle.
    var needleAnchorPoint: NSPoint {
        let body = bodyRect
        return NSPoint(x: body.midX, y: body.minY)
    }

    private var bodyRect: NSRect {
        // Trapezoid pinned to the TOP of the widget; the BPM
        // number lives below it. Centered horizontally so the
        // needle's screen position is the widget's centerline.
        let y = bounds.height - Self.bodySize.height
        let x = (bounds.width - Self.bodySize.width) / 2
        return NSRect(x: x, y: y, width: Self.bodySize.width, height: Self.bodySize.height)
    }

    // MARK: - Needle

    private func configureNeedleLayer() {
        guard let layer = self.layer else { return }
        let body = bodyRect
        let pivotX = body.midX
        let pivotY = body.minY + 2
        let needleLength = Self.bodySize.height - 5
        let host = CALayer()
        host.bounds = NSRect(x: 0, y: 0, width: 4, height: needleLength)
        host.anchorPoint = NSPoint(x: 0.5, y: 0.0)
        host.position = NSPoint(x: pivotX, y: pivotY)

        let needlePath = NSBezierPath()
        needlePath.move(to: NSPoint(x: 2, y: 0))
        needlePath.line(to: NSPoint(x: 2, y: needleLength))
        needleLayer.path = needlePath.cgPath
        needleLayer.lineWidth = 1.4
        needleLayer.lineCap = .round
        needleLayer.fillColor = nil
        needleLayer.frame = host.bounds
        let bobR: CGFloat = 1.6
        let bobPath = NSBezierPath(ovalIn: NSRect(
            x: 2 - bobR, y: needleLength - bobR,
            width: bobR * 2, height: bobR * 2
        ))
        bobLayer.path = bobPath.cgPath
        bobLayer.frame = host.bounds
        host.addSublayer(needleLayer)
        host.addSublayer(bobLayer)
        layer.addSublayer(host)
        self.needleHost = host
        applyTintToNeedle()
    }

    private func applyTintToNeedle() {
        effectiveAppearance.performAsCurrentDrawingAppearance {
            let cg = tint.cgColor
            needleLayer.strokeColor = cg
            bobLayer.fillColor = cg
        }
    }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        applyTintToNeedle()
        needsDisplay = true
    }

    // MARK: - Swing animation (continuous while playing)

    private func startSwing() { restartSwing() }

    private func stopSwing() {
        needleHost?.removeAnimation(forKey: Self.swingAnimationKey)
        needleHost?.setAffineTransform(.identity)
        tickTimer?.invalidate()
        tickTimer = nil
    }

    private func restartSwing() {
        guard let host = needleHost else { return }
        host.removeAnimation(forKey: Self.swingAnimationKey)
        let period = 60.0 / Double(bpm) * 2.0
        let anim = CAKeyframeAnimation(keyPath: "transform.rotation.z")
        anim.values = [0, Self.maxSwingAngle, 0, -Self.maxSwingAngle, 0]
        anim.keyTimes = [0, 0.25, 0.5, 0.75, 1]
        anim.duration = period
        anim.repeatCount = .infinity
        // Pendulum motion: max velocity at the zero crossings,
        // momentary pause at each extreme. Per-segment ease curves
        // give that natural physical feel — center → extreme
        // decelerates (ease-out) and extreme → center accelerates
        // (ease-in). A single global ease made the start of every
        // cycle look paused instead of swinging.
        let easeOut = CAMediaTimingFunction(name: .easeOut)
        let easeIn = CAMediaTimingFunction(name: .easeIn)
        anim.timingFunctions = [easeOut, easeIn, easeOut, easeIn]
        host.add(anim, forKey: Self.swingAnimationKey)

        tickTimer?.invalidate()
        let beatInterval = period / 2.0
        let timer = Timer(timeInterval: beatInterval, repeats: true) { [weak self] _ in
            self?.playBeat()
        }
        timer.tolerance = beatInterval * 0.05
        // Animation goes 0 → +amp → 0 → -amp → 0 over `period`,
        // so the needle is at CENTER (angle = 0) at t = 0,
        // period/2, period, … Schedule the first beat at t =
        // period/2 so the click lands on a zero crossing instead
        // of at the swing extremes — the user hears the tick the
        // moment the needle is upright, not at the corners.
        timer.fireDate = Date(timeIntervalSinceNow: period * 0.5)
        RunLoop.main.add(timer, forMode: .common)
        tickTimer = timer
    }

    /// One-shot needle nudge — used during scrubbing so the
    /// little ticker visibly ticks each time the BPM steps.
    private func nudgeNeedle() {
        guard let host = needleHost, !isPlaying else { return }
        // Override any pending nudge so consecutive scrub steps
        // chain into a continuous wobble.
        host.removeAnimation(forKey: Self.swingAnimationKey)
        let nudge = CAKeyframeAnimation(keyPath: "transform.rotation.z")
        let amp = Self.maxSwingAngle * 0.55
        nudge.values = [0, amp, 0, -amp, 0]
        nudge.keyTimes = [0, 0.25, 0.5, 0.75, 1]
        nudge.duration = 0.18
        nudge.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
        host.add(nudge, forKey: Self.swingAnimationKey)
    }

    // MARK: - Sounds

    private func playBeat() {
        Self.beatSound?.stop()
        Self.beatSound?.play()
        onTick?()
    }

    private func playScrubClick() {
        Self.scrubClickSound?.stop()
        Self.scrubClickSound?.play()
    }

    // MARK: - Helpers

    private func updateBPMLabel() {
        bpmLabel.stringValue = String(bpm)
    }
}

private extension NSBezierPath {
    /// CGPath bridge — NSBezierPath's `cgPath` is macOS 14+. Roll
    /// it ourselves so the widget keeps working on macOS 11+.
    var cgPath: CGPath {
        let path = CGMutablePath()
        var points = [NSPoint](repeating: .zero, count: 3)
        for i in 0..<elementCount {
            let type = element(at: i, associatedPoints: &points)
            switch type {
            case .moveTo: path.move(to: points[0])
            case .lineTo: path.addLine(to: points[0])
            case .curveTo: path.addCurve(to: points[2], control1: points[0], control2: points[1])
            case .closePath: path.closeSubpath()
            default: continue
            }
        }
        return path
    }
}
