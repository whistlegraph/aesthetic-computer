import AppKit

/// Thin floating waveform strip that slides down from beneath the menubar
/// piano when notes are played, and slides back up after a configurable
/// idle delay. Hidden whenever the settings popover or floating piano
/// palette is visible to avoid visual clutter.
///
/// The panel sits one level below `NSWindow.Level.mainMenu` so the menubar
/// itself occludes it. The slide animation moves the panel from behind the
/// menubar downward into view, and reverses to hide.
final class MenuBarWaveformStrip {
    private let menuBand: MenuBandController
    private let waveformView = WaveformView()
    private let waveformBezel = NSView()
    private var panel: NSPanel?
    private weak var statusButton: NSStatusBarButton?

    /// How long to keep the strip visible after the last note ends.
    private let hideDelay: TimeInterval = 2.0
    private var hideWorkItem: DispatchWorkItem?

    /// Strip height in points.
    private static let stripHeight: CGFloat = 36

    /// Animation duration in seconds.
    private static let slideDuration: TimeInterval = 0.22

    /// Window level just below the menubar so the menubar occludes the
    /// strip while it's tucked behind.
    private static let stripLevel = NSWindow.Level(rawValue: NSWindow.Level.mainMenu.rawValue - 1)

    /// CVDisplayLink-driven slide animation. NSAnimationContext + animator()
    /// proxy doesn't reliably move borderless non-activating panels, so we
    /// drive the frame ourselves at display refresh rate.
    private var slideLink: CVDisplayLink?
    private var slideStartTime: CFTimeInterval = 0
    private var slideFromY: CGFloat = 0
    private var slideToY: CGFloat = 0
    private var slideCompletion: (() -> Void)?
    private var isSliding = false

    /// True while the strip panel is on screen (visible or animating in).
    var isShown: Bool { panel?.isVisible == true }

    /// External suppression — callers set this when the popover or floating
    /// palette opens. While suppressed, `showIfNeeded` is a no-op and any
    /// visible strip is immediately hidden.
    var suppressed: Bool = false {
        didSet {
            guard suppressed != oldValue else { return }
            if suppressed { hide(animated: false) }
        }
    }

    init(menuBand: MenuBandController) {
        self.menuBand = menuBand
        waveformView.menuBand = menuBand
        waveformBezel.wantsLayer = true
        waveformBezel.layer?.cornerRadius = 6
        waveformBezel.layer?.backgroundColor = NSColor(white: 0.06, alpha: 1.0).cgColor
        waveformBezel.layer?.borderWidth = 1
    }

    /// Pre-build the panel so the first note doesn't pay construction cost.
    func warmUp() {
        if panel == nil { buildPanel() }
    }

    deinit {
        stopSlide()
    }

    // MARK: - Show / hide

    /// Call whenever `litNotes` becomes non-empty (a note starts).
    func showIfNeeded() {
        guard !suppressed else { return }
        cancelPendingHide()
        if !isShown && !isSliding { show() }
    }

    /// Call whenever `litNotes` becomes empty (all notes released).
    func scheduleHide() {
        cancelPendingHide()
        let work = DispatchWorkItem { [weak self] in
            self?.hide(animated: true)
        }
        hideWorkItem = work
        DispatchQueue.main.asyncAfter(deadline: .now() + hideDelay, execute: work)
    }

    /// Immediately tear down (app termination, etc.).
    func dismiss() {
        cancelPendingHide()
        stopSlide()
        waveformView.isLive = false
        panel?.orderOut(nil)
    }

    /// Store the button reference so positioning works from inside show().
    func reposition(statusItemButton: NSStatusBarButton?) {
        statusButton = statusItemButton
        guard let panel = panel, panel.isVisible, !isSliding else { return }
        positionPanel(panel)
    }

    // MARK: - Geometry

    /// Compute the target frame for the strip: directly below the menubar,
    /// aligned to the piano key region of the status item.
    private func targetFrame() -> NSRect? {
        guard let button = statusButton,
              let buttonWindow = button.window else { return nil }

        let imgSize = KeyboardIconRenderer.imageSize
        let bb = button.bounds
        let xOff = (bb.width - imgSize.width) / 2.0

        let pianoOriginX = xOff + KeyboardIconRenderer.pad
        let pianoWidth = imgSize.width - KeyboardIconRenderer.settingsW
            - KeyboardIconRenderer.settingsGap - KeyboardIconRenderer.pad * 2

        let localRect = NSRect(x: pianoOriginX, y: 0, width: pianoWidth, height: bb.height)
        let windowRect = button.convert(localRect, to: nil)
        let screenRect = buttonWindow.convertToScreen(windowRect)

        return NSRect(
            x: screenRect.origin.x,
            y: screenRect.origin.y - Self.stripHeight,
            width: screenRect.width,
            height: Self.stripHeight
        )
    }

    // MARK: - Slide animation

    private func show() {
        if panel == nil { buildPanel() }
        guard let panel = panel, let target = targetFrame() else { return }

        // Start tucked behind the menubar (origin shifted up by strip height).
        let hiddenY = target.origin.y + target.height
        panel.setFrame(NSRect(x: target.origin.x, y: hiddenY,
                              width: target.width, height: target.height),
                       display: true)
        applyAppearanceToVisualizer()
        applyWaveformTint()
        waveformView.isLive = true
        panel.orderFrontRegardless()

        // Slide down to the target position.
        startSlide(fromY: hiddenY, toY: target.origin.y) { [weak self] in
            // Ensure we land exactly at the target.
            panel.setFrameOrigin(target.origin)
            self?.isSliding = false
        }
    }

    private func hide(animated: Bool) {
        cancelPendingHide()
        stopSlide()
        guard let panel = panel, panel.isVisible else {
            waveformView.isLive = false
            return
        }
        if !animated {
            waveformView.isLive = false
            panel.orderOut(nil)
            return
        }
        // Slide back up behind the menubar.
        let currentY = panel.frame.origin.y
        let hiddenY = currentY + panel.frame.height
        startSlide(fromY: currentY, toY: hiddenY) { [weak self] in
            self?.waveformView.isLive = false
            panel.orderOut(nil)
            self?.isSliding = false
        }
    }

    private func startSlide(fromY: CGFloat, toY: CGFloat, completion: @escaping () -> Void) {
        stopSlide()
        isSliding = true
        slideFromY = fromY
        slideToY = toY
        slideStartTime = CACurrentMediaTime()
        slideCompletion = completion

        var link: CVDisplayLink?
        CVDisplayLinkCreateWithActiveCGDisplays(&link)
        guard let link = link else {
            // Fallback: jump immediately if CVDisplayLink fails.
            panel?.setFrameOrigin(NSPoint(x: panel?.frame.origin.x ?? 0, y: toY))
            completion()
            return
        }
        let opaque = Unmanaged.passUnretained(self).toOpaque()
        CVDisplayLinkSetOutputCallback(link, { _, _, _, _, _, ctx -> CVReturn in
            guard let ctx = ctx else { return kCVReturnSuccess }
            let strip = Unmanaged<MenuBarWaveformStrip>.fromOpaque(ctx).takeUnretainedValue()
            DispatchQueue.main.async { strip.tickSlide() }
            return kCVReturnSuccess
        }, opaque)
        CVDisplayLinkStart(link)
        slideLink = link
    }

    private func tickSlide() {
        guard isSliding, let panel = panel else {
            stopSlide()
            return
        }
        let elapsed = CACurrentMediaTime() - slideStartTime
        let t = min(1.0, elapsed / Self.slideDuration)
        // Ease-out cubic for show, ease-in cubic for hide.
        let eased: CGFloat
        if slideToY < slideFromY {
            // Sliding down (show): ease-out
            let f = 1.0 - t
            eased = CGFloat(1.0 - f * f * f)
        } else {
            // Sliding up (hide): ease-in
            eased = CGFloat(t * t * t)
        }
        let currentY = slideFromY + (slideToY - slideFromY) * eased
        panel.setFrameOrigin(NSPoint(x: panel.frame.origin.x, y: currentY))

        if t >= 1.0 {
            let completion = slideCompletion
            stopSlide()
            completion?()
        }
    }

    private func stopSlide() {
        if let link = slideLink {
            CVDisplayLinkStop(link)
            slideLink = nil
        }
        slideCompletion = nil
    }

    private func cancelPendingHide() {
        hideWorkItem?.cancel()
        hideWorkItem = nil
    }

    // MARK: - Panel construction

    private func buildPanel() {
        let p = NSPanel(
            contentRect: NSRect(origin: .zero, size: NSSize(width: 200, height: Self.stripHeight)),
            styleMask: [.borderless, .nonactivatingPanel],
            backing: .buffered,
            defer: false
        )
        p.isOpaque = true
        p.backgroundColor = .black
        p.hasShadow = true
        p.level = Self.stripLevel
        p.collectionBehavior = [.transient, .ignoresCycle]
        p.hidesOnDeactivate = false
        p.canHide = false
        p.isMovable = false
        p.animationBehavior = .none
        p.acceptsMouseMovedEvents = false

        waveformView.translatesAutoresizingMaskIntoConstraints = false
        waveformBezel.translatesAutoresizingMaskIntoConstraints = false
        let content = NSView()
        content.wantsLayer = true
        content.layer?.backgroundColor = NSColor.clear.cgColor
        p.contentView = content
        p.contentView!.addSubview(waveformBezel)
        waveformBezel.addSubview(waveformView)
        let bezelInset: CGFloat = 5
        NSLayoutConstraint.activate([
            waveformBezel.leadingAnchor.constraint(equalTo: p.contentView!.leadingAnchor),
            waveformBezel.trailingAnchor.constraint(equalTo: p.contentView!.trailingAnchor),
            waveformBezel.topAnchor.constraint(equalTo: p.contentView!.topAnchor),
            waveformBezel.bottomAnchor.constraint(equalTo: p.contentView!.bottomAnchor),
            waveformView.leadingAnchor.constraint(equalTo: waveformBezel.leadingAnchor, constant: bezelInset),
            waveformView.trailingAnchor.constraint(equalTo: waveformBezel.trailingAnchor, constant: -bezelInset),
            waveformView.topAnchor.constraint(equalTo: waveformBezel.topAnchor, constant: bezelInset),
            waveformView.bottomAnchor.constraint(equalTo: waveformBezel.bottomAnchor, constant: -bezelInset),
        ])
        applyAppearanceToVisualizer()
        applyWaveformTint()

        panel = p
    }

    private func positionPanel(_ panel: NSPanel) {
        guard let target = targetFrame() else { return }
        panel.setFrame(target, display: true)
    }

    private func applyAppearanceToVisualizer() {
        let isDark = NSApp.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        waveformView.setLightMode(!isDark)
        if isDark {
            waveformBezel.layer?.backgroundColor = NSColor(white: 0.06, alpha: 1.0).cgColor
        } else {
            waveformBezel.layer?.backgroundColor = NSColor(white: 0.82, alpha: 1.0).cgColor
        }
    }

    private func applyWaveformTint() {
        if menuBand.midiMode {
            waveformView.setDotMatrix(MenuBandPopoverViewController.midiDotPattern)
            waveformView.setBaseColor(.controlAccentColor)
            waveformBezel.layer?.borderColor = NSColor.controlAccentColor
                .withAlphaComponent(0.55).cgColor
        } else {
            waveformView.setDotMatrix(nil)
            let safe = max(0, min(127, Int(menuBand.melodicProgram)))
            let familyColor = InstrumentListView.colorForProgram(safe)
            waveformView.setBaseColor(familyColor)
            waveformBezel.layer?.borderColor = familyColor
                .withAlphaComponent(0.55).cgColor
        }
    }
}
