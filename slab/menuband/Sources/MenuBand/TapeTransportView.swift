import AppKit

/// Larger cassette deck for the popover. Mirrors the inline status-item
/// cassette but renders at a roomier size, with the full transport row
/// (REW / STOP / PLAY / FFWD / REC / EJECT) below the cassette window
/// and a 1:30 position counter on the left.
///
/// Reference: Nakamichi Dragon's chunky beveled transport buttons +
/// Walkman TPS-L2 silver/blue body + TDK SA gold label stripe. Buttons
/// use SF Symbols with the standardized IEC 60417 glyphs (`backward.fill`,
/// `stop.fill`, `play.fill`, `forward.fill`, `record.circle.fill`,
/// `eject.fill`) so the row reads as media transport regardless of
/// locale.
final class TapeTransportView: NSView {
    weak var menuBand: MenuBandController? {
        didSet {
            // Bring the visible state in line with whatever the
            // controller is doing right now — view may be installed
            // mid-recording (rare but possible if the popover opens
            // while REC is engaged).
            refresh()
        }
    }

    private let cassetteView = TapeCassetteView()
    private let positionLabel = NSTextField(labelWithString: "00:00 / 01:30")
    private let stateLabel = NSTextField(labelWithString: "")

    private let rewindButton = NSButton()
    private let stopButton = NSButton()
    private let playButton = NSButton()
    private let ffwdButton = NSButton()
    private let recButton = NSButton()
    private let ejectButton = NSButton()

    private var observerToken: NSObjectProtocol?
    /// 12fps redraw timer that fires while the tape is recording or
    /// playing, so the cassette reels turn smoothly and the position
    /// counter ticks without us re-publishing notifications at frame
    /// rate. Invalidated when the tape returns to idle.
    private var refreshTimer: Timer?

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.backgroundColor = NSColor.clear.cgColor
        setUp()
        startObserving()
    }
    required init?(coder: NSCoder) { nil }

    deinit {
        if let t = observerToken { NotificationCenter.default.removeObserver(t) }
        refreshTimer?.invalidate()
    }

    // MARK: - Layout

    private func setUp() {
        cassetteView.translatesAutoresizingMaskIntoConstraints = false
        addSubview(cassetteView)

        positionLabel.font = NSFont.monospacedDigitSystemFont(ofSize: 11, weight: .medium)
        positionLabel.textColor = .secondaryLabelColor
        positionLabel.translatesAutoresizingMaskIntoConstraints = false
        addSubview(positionLabel)

        stateLabel.font = NSFont.systemFont(ofSize: 10, weight: .semibold)
        stateLabel.textColor = .tertiaryLabelColor
        stateLabel.alignment = .right
        stateLabel.translatesAutoresizingMaskIntoConstraints = false
        addSubview(stateLabel)

        let symbolConfig = NSImage.SymbolConfiguration(pointSize: 13, weight: .semibold)
        configureTransport(button: rewindButton,
                           symbol: "backward.fill",
                           tooltip: "Rewind to start",
                           action: #selector(rewindClicked(_:)))
        configureTransport(button: stopButton,
                           symbol: "stop.fill",
                           tooltip: "Stop",
                           action: #selector(stopClicked(_:)))
        configureTransport(button: playButton,
                           symbol: "play.fill",
                           tooltip: "Play",
                           tint: NSColor(srgbRed: 90/255, green: 180/255,
                                          blue: 90/255, alpha: 1),
                           action: #selector(playClicked(_:)))
        configureTransport(button: ffwdButton,
                           symbol: "forward.fill",
                           tooltip: "Jump to end",
                           action: #selector(ffwdClicked(_:)))
        configureTransport(button: recButton,
                           symbol: "record.circle.fill",
                           tooltip: "Record",
                           tint: NSColor(srgbRed: 220/255, green: 60/255,
                                          blue: 60/255, alpha: 1),
                           action: #selector(recClicked(_:)))
#if MAC_APP_STORE
        let ejectTooltip = "Save tape… (or drag the cassette)"
#else
        let ejectTooltip = "Save tape to Desktop (or drag the cassette)"
#endif
        configureTransport(button: ejectButton,
                           symbol: "eject.fill",
                           tooltip: ejectTooltip,
                           action: #selector(ejectClicked(_:)))

        // Apply the buttons + symbol config in one pass.
        for button in [rewindButton, stopButton, playButton,
                        ffwdButton, recButton, ejectButton] {
            button.image = button.image?
                .withSymbolConfiguration(symbolConfig)
            addSubview(button)
        }

        let transportStack = NSStackView(views: [
            rewindButton, stopButton, playButton,
            ffwdButton, recButton, ejectButton,
        ])
        transportStack.spacing = 8
        transportStack.distribution = .fillEqually
        transportStack.alignment = .centerY
        transportStack.orientation = .horizontal
        transportStack.translatesAutoresizingMaskIntoConstraints = false
        addSubview(transportStack)

        NSLayoutConstraint.activate([
            cassetteView.topAnchor.constraint(equalTo: topAnchor, constant: 4),
            cassetteView.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 6),
            cassetteView.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -6),
            cassetteView.heightAnchor.constraint(equalToConstant: 56),

            positionLabel.topAnchor.constraint(equalTo: cassetteView.bottomAnchor, constant: 4),
            positionLabel.leadingAnchor.constraint(equalTo: cassetteView.leadingAnchor, constant: 2),

            stateLabel.centerYAnchor.constraint(equalTo: positionLabel.centerYAnchor),
            stateLabel.trailingAnchor.constraint(equalTo: cassetteView.trailingAnchor, constant: -2),

            transportStack.topAnchor.constraint(equalTo: positionLabel.bottomAnchor, constant: 6),
            transportStack.leadingAnchor.constraint(equalTo: cassetteView.leadingAnchor),
            transportStack.trailingAnchor.constraint(equalTo: cassetteView.trailingAnchor),
            transportStack.heightAnchor.constraint(equalToConstant: 28),
            transportStack.bottomAnchor.constraint(equalTo: bottomAnchor, constant: -4),
        ])
    }

    private func configureTransport(button: NSButton,
                                    symbol: String,
                                    tooltip: String,
                                    tint: NSColor = NSColor.labelColor,
                                    action: Selector) {
        button.translatesAutoresizingMaskIntoConstraints = false
        button.image = NSImage(systemSymbolName: symbol,
                                accessibilityDescription: tooltip)
        button.bezelStyle = .recessed
        button.isBordered = true
        button.imagePosition = .imageOnly
        button.contentTintColor = tint
        button.target = self
        button.action = action
        button.toolTip = tooltip
    }

    // MARK: - State sync

    private func startObserving() {
        observerToken = NotificationCenter.default.addObserver(
            forName: .menuBandTapeChanged,
            object: nil,
            queue: .main
        ) { [weak self] _ in
            self?.refresh()
        }
        refresh()
    }

    /// Pull current state from the controller and update everything
    /// the UI shows. Drives both the cassette draw and the button
    /// enable states; cheap enough to call every animation tick.
    func refresh() {
        guard let menuBand = menuBand else { return }
        let tape = menuBand.tape
        cassetteView.recording = (tape.state == .recording)
        cassetteView.playing   = (tape.state == .playing)
        cassetteView.fillFraction = CGFloat(
            tape.durationSeconds / MenuBandTape.maxDurationSeconds)
        cassetteView.playheadFraction = CGFloat(
            tape.positionSeconds / max(0.001, tape.durationSeconds))
        cassetteView.micHot = (tape.state == .recording) &&
            (MenuBandSampleVoice.micAuthorizationStatus() == .authorized)
        cassetteView.needsDisplay = true

        let pos = formatTime(tape.positionSeconds)
        let dur = formatTime(tape.durationSeconds)
        let cap = formatTime(MenuBandTape.maxDurationSeconds)
        positionLabel.stringValue = "\(pos) / \(cap)"
        switch tape.state {
        case .recording:
            stateLabel.stringValue = "● REC"
            stateLabel.textColor = NSColor(srgbRed: 220/255, green: 60/255,
                                            blue: 60/255, alpha: 1)
        case .playing:
            stateLabel.stringValue = "▶ PLAY"
            stateLabel.textColor = NSColor(srgbRed: 70/255, green: 150/255,
                                            blue: 70/255, alpha: 1)
        case .paused:
            stateLabel.stringValue = "‖ PAUSED"
            stateLabel.textColor = .secondaryLabelColor
        case .idle:
            stateLabel.stringValue = tape.hasRecording
                ? "tape: \(dur)"
                : "empty tape"
            stateLabel.textColor = .tertiaryLabelColor
        }

        // Button enable rules — REW/STOP/PLAY/FFWD/EJECT need a
        // recording; REC is always live (toggles).
        let hasRec = tape.hasRecording
        let isPlaying = tape.state == .playing
        rewindButton.isEnabled = hasRec
        stopButton.isEnabled = (tape.state != .idle) || isPlaying
        playButton.isEnabled = hasRec && !isPlaying
        ffwdButton.isEnabled = hasRec
        ejectButton.isEnabled = hasRec
        // REC button visual — tinted red, glows brighter while
        // armed. Apple's `.recessed` bezel doesn't expose a "filled"
        // active style, but we can flip the symbol between
        // `record.circle.fill` and `largecircle.fill.circle` to make
        // the toggle obvious.
        recButton.image = NSImage(
            systemSymbolName: tape.state == .recording
                ? "largecircle.fill.circle" : "record.circle.fill",
            accessibilityDescription: "Record")?
            .withSymbolConfiguration(NSImage.SymbolConfiguration(
                pointSize: 13, weight: .semibold))

        // Keep the reels turning while the tape is active. The
        // controller broadcasts `menuBandTapeChanged` only on state
        // transitions, not at frame rate — that's where this timer
        // earns its keep.
        let needsTick = (tape.state == .recording || tape.state == .playing)
        if needsTick && refreshTimer == nil {
            let timer = Timer(timeInterval: 1.0 / 12.0, repeats: true) {
                [weak self] _ in self?.refresh()
            }
            RunLoop.main.add(timer, forMode: .common)
            refreshTimer = timer
        } else if !needsTick {
            refreshTimer?.invalidate()
            refreshTimer = nil
        }
    }

    private func formatTime(_ seconds: Double) -> String {
        let total = max(0, min(seconds, MenuBandTape.maxDurationSeconds))
        let m = Int(total) / 60
        let s = Int(total) % 60
        return String(format: "%02d:%02d", m, s)
    }

    // MARK: - Actions

    @objc private func rewindClicked(_ sender: Any?) { menuBand?.rewindTape() }
    @objc private func stopClicked(_ sender: Any?)   { menuBand?.stopTape() }
    @objc private func playClicked(_ sender: Any?)   { menuBand?.playTape() }
    @objc private func ffwdClicked(_ sender: Any?)   { menuBand?.tapeSeekToEnd() }
    @objc private func recClicked(_ sender: Any?)    { menuBand?.toggleTapeRecording() }
    @objc private func ejectClicked(_ sender: Any?) {
        // Click-to-eject: write WAV then reveal in Finder. The
        // drag-out gesture from the inline cassette stays the primary
        // export path (no extra Finder window pop); the popover button
        // is a fallback for users who don't think to drag the menubar
        // cassette.
        guard let url = menuBand?.ejectTape() else { return }
        NSWorkspace.shared.activateFileViewerSelecting([url])
    }
}

/// Cassette artwork for the popover. Same skeuomorph language as the
/// inline status-item drawing, but at a larger size so the label-card
/// text + reel motion is properly visible. Subclasses NSView (rather
/// than reusing `KeyboardIconRenderer.drawTapeWidget`) so we get a
/// stable target for the popover's draw cycle without coupling the
/// transport panel to the renderer's static state.
final class TapeCassetteView: NSView {
    var recording = false
    var playing = false
    var fillFraction: CGFloat = 0
    var playheadFraction: CGFloat = 0
    var micHot = false

    private var startTime: CFTimeInterval = CACurrentMediaTime()

    override var wantsUpdateLayer: Bool { false }
    override var isFlipped: Bool { false }

    override func draw(_ dirtyRect: NSRect) {
        let isDark = NSApp.effectiveAppearance.bestMatch(
            from: [.aqua, .darkAqua]) == .darkAqua
        let rect = bounds.insetBy(dx: 1, dy: 1)

        // Body — same navy/silver gradient as the inline widget, just
        // taller so the reels read clearly.
        let bodyHi: NSColor
        let bodyLo: NSColor
        if isDark {
            bodyHi = NSColor(srgbRed:  88/255, green:  98/255, blue: 112/255, alpha: 1)
            bodyLo = NSColor(srgbRed:  42/255, green:  50/255, blue:  60/255, alpha: 1)
        } else {
            bodyHi = NSColor(srgbRed:  98/255, green: 118/255, blue: 152/255, alpha: 1)
            bodyLo = NSColor(srgbRed:  52/255, green:  68/255, blue:  98/255, alpha: 1)
        }
        let bodyPath = NSBezierPath(roundedRect: rect, xRadius: 6, yRadius: 6)
        NSGradient(colors: [bodyHi, bodyLo],
                   atLocations: [0.0, 1.0],
                   colorSpace: .sRGB)?.draw(in: bodyPath, angle: -90)
        NSColor.white.withAlphaComponent(isDark ? 0.18 : 0.30).setStroke()
        bodyPath.lineWidth = 0.8
        bodyPath.stroke()

        // Label card — cream stripe across the top.
        let labelInset: CGFloat = 6
        let labelH = rect.height * 0.32
        let labelRect = NSRect(x: rect.minX + labelInset,
                                y: rect.maxY - labelH - 5,
                                width: rect.width - labelInset * 2,
                                height: labelH)
        let labelHi: NSColor = isDark
            ? NSColor(srgbRed: 230/255, green: 220/255, blue: 195/255, alpha: 1)
            : NSColor(srgbRed: 250/255, green: 245/255, blue: 225/255, alpha: 1)
        let labelLo: NSColor = isDark
            ? NSColor(srgbRed: 195/255, green: 180/255, blue: 150/255, alpha: 1)
            : NSColor(srgbRed: 230/255, green: 220/255, blue: 195/255, alpha: 1)
        let labelPath = NSBezierPath(roundedRect: labelRect,
                                     xRadius: 2, yRadius: 2)
        NSGradient(colors: [labelHi, labelLo],
                   atLocations: [0.0, 1.0],
                   colorSpace: .sRGB)?.draw(in: labelPath, angle: -90)

        // Label text — wordmark on the cassette. "MENU BAND" in the
        // small caps that real cassette labels favored, plus a thin
        // gold stripe below.
        let title = "MENU BAND • 90"
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 9, weight: .heavy),
            .foregroundColor: NSColor(srgbRed: 30/255, green: 30/255,
                                       blue: 40/255, alpha: 0.85),
            .kern: 1.0,
        ]
        let titleSize = (title as NSString).size(withAttributes: attrs)
        let titleRect = NSRect(x: labelRect.midX - titleSize.width / 2,
                                y: labelRect.midY - titleSize.height / 2 + 1,
                                width: titleSize.width,
                                height: titleSize.height)
        (title as NSString).draw(in: titleRect, withAttributes: attrs)

        // Gold stripe — color-codes the deck state (red REC, green
        // PLAY, gold idle), same idiom as the inline widget so the
        // two surfaces feel like one instrument.
        let stripeColor: NSColor
        if recording {
            stripeColor = NSColor(srgbRed: 220/255, green: 60/255, blue: 60/255, alpha: 1)
        } else if playing {
            stripeColor = NSColor(srgbRed: 90/255, green: 180/255, blue: 90/255, alpha: 1)
        } else {
            stripeColor = NSColor(srgbRed: 210/255, green: 175/255, blue: 80/255, alpha: 1)
        }
        let stripeRect = NSRect(x: labelRect.minX, y: labelRect.minY,
                                 width: labelRect.width, height: 2.5)
        stripeColor.setFill()
        NSBezierPath(rect: stripeRect).fill()

        // Progress tick across the stripe — REC fills toward the
        // right; PLAY moves a discrete marker.
        let frac = recording
            ? min(1, max(0, fillFraction))
            : min(1, max(0, playheadFraction))
        if frac > 0.001 {
            let tickW: CGFloat = 3.0
            let tickX = stripeRect.minX + frac * (stripeRect.width - tickW)
            NSColor.white.withAlphaComponent(0.85).setFill()
            NSBezierPath(rect: NSRect(x: tickX, y: stripeRect.minY - 1.0,
                                       width: tickW, height: 4.5)).fill()
        }

        // Reels.
        let reelAreaY = rect.minY + 6
        let reelAreaH = labelRect.minY - reelAreaY - 4
        let reelR = min(reelAreaH * 0.45, 14)
        let reelCY = reelAreaY + reelAreaH / 2
        let leftCX = rect.midX - reelR * 1.8
        let rightCX = rect.midX + reelR * 1.8

        let phaseRaw = CACurrentMediaTime() - startTime
        let speed: CGFloat
        if recording { speed = 4.5 }
        else if playing { speed = 3.2 }
        else { speed = 0.0 }
        let phase = CGFloat(phaseRaw) * speed

        drawReel(at: NSPoint(x: leftCX, y: reelCY), radius: reelR,
                 rotation: phase, isDark: isDark)
        drawReel(at: NSPoint(x: rightCX, y: reelCY), radius: reelR,
                 rotation: -phase + 0.5, isDark: isDark)

        // Mic LED on the cassette — orange dot inside the label
        // upper-right, lit while mic is hot.
        if micHot {
            let micR: CGFloat = 2.5
            let micRect = NSRect(x: labelRect.maxX - micR * 2 - 4,
                                  y: labelRect.maxY - micR * 2 - 3,
                                  width: micR * 2, height: micR * 2)
            NSColor(srgbRed: 255/255, green: 150/255, blue: 40/255,
                    alpha: 0.95).setFill()
            NSBezierPath(ovalIn: micRect).fill()
            // Tiny soft halo so the LED reads as glowing, not as a
            // flat dot.
            NSColor(srgbRed: 255/255, green: 150/255, blue: 40/255,
                    alpha: 0.25).setFill()
            NSBezierPath(ovalIn: micRect.insetBy(dx: -2, dy: -2)).fill()
        }

        // REC LED — bigger pulsing red dot between the reels.
        if recording {
            let dotR: CGFloat = 4
            let pulse = 0.5 + 0.5 * sin(CGFloat(phaseRaw) * 6.0)
            let alpha: CGFloat = 0.7 + 0.3 * pulse
            let dotRect = NSRect(x: rect.midX - dotR,
                                  y: reelCY - dotR,
                                  width: dotR * 2, height: dotR * 2)
            NSColor(srgbRed: 235/255, green: 50/255, blue: 50/255,
                    alpha: alpha).setFill()
            NSBezierPath(ovalIn: dotRect).fill()
        }
    }

    private func drawReel(at center: NSPoint,
                          radius: CGFloat,
                          rotation: CGFloat,
                          isDark: Bool) {
        let windowRect = NSRect(x: center.x - radius - 1,
                                 y: center.y - radius - 1,
                                 width: (radius + 1) * 2,
                                 height: (radius + 1) * 2)
        NSColor.black.withAlphaComponent(isDark ? 0.55 : 0.7).setFill()
        NSBezierPath(ovalIn: windowRect).fill()

        let reelRect = NSRect(x: center.x - radius,
                               y: center.y - radius,
                               width: radius * 2, height: radius * 2)
        NSColor(srgbRed: 70/255, green: 55/255, blue: 45/255,
                alpha: 1.0).setFill()
        NSBezierPath(ovalIn: reelRect).fill()

        // Hexagonal hub.
        let hubR = radius * 0.55
        let hub = NSBezierPath()
        for i in 0..<6 {
            let theta = rotation + CGFloat(i) * (.pi / 3)
            let x = center.x + cos(theta) * hubR
            let y = center.y + sin(theta) * hubR
            if i == 0 { hub.move(to: NSPoint(x: x, y: y)) }
            else      { hub.line(to: NSPoint(x: x, y: y)) }
        }
        hub.close()
        NSColor(srgbRed: 200/255, green: 195/255, blue: 185/255,
                alpha: 1.0).setFill()
        hub.fill()

        // Drive-spindle hole.
        let pinR: CGFloat = max(1, radius * 0.12)
        let pinRect = NSRect(x: center.x - pinR, y: center.y - pinR,
                              width: pinR * 2, height: pinR * 2)
        NSColor.black.withAlphaComponent(0.85).setFill()
        NSBezierPath(ovalIn: pinRect).fill()

        // Light-catch spoke for rotation legibility.
        let spoke = NSBezierPath()
        let halfTheta: CGFloat = 0.18
        let inner = hubR * 0.95
        let outer = radius * 0.92
        let cL = cos(rotation - halfTheta), sL = sin(rotation - halfTheta)
        let cR = cos(rotation + halfTheta), sR = sin(rotation + halfTheta)
        spoke.move(to: NSPoint(x: center.x + cL * inner,
                                y: center.y + sL * inner))
        spoke.line(to: NSPoint(x: center.x + cL * outer,
                                y: center.y + sL * outer))
        spoke.line(to: NSPoint(x: center.x + cR * outer,
                                y: center.y + sR * outer))
        spoke.line(to: NSPoint(x: center.x + cR * inner,
                                y: center.y + sR * inner))
        spoke.close()
        NSColor.white.withAlphaComponent(0.45).setFill()
        spoke.fill()
    }
}
