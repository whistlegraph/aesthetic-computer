import AppKit

/// "Fluoddity TV" — a little floating viewer that pops up when the popover
/// closes while the Fluoddity backend is active, so the swarm stays visible
/// while you play. The screen is the most recent note's 128×16 trail field
/// drawn chunky through the indigo ramp, with the 40 particles riding on
/// top; below it live the breeding options, labeled in words this time:
/// New Species rerolls the 80-float genome, Mutate steps the lineage by the
/// slider's amount. Non-activating so clicking it never steals focus from
/// whatever instrument surface is being played.
///
/// Lifecycle contract (wired in AppDelegate): `showPopover` hides the TV
/// (the popover's own FLUODDITY strip covers it), `closePopover` shows it
/// again if the backend is still active, and the TV hides itself when it
/// notices the backend has moved on. Closing it by hand just hides it
/// until the next popover close.
final class FluoddityTV {
    static let shared = FluoddityTV()

    private var panel: NSPanel?
    private var screen: FluodTVScreenView?
    private var amountSlider: NSSlider?
    private weak var menuBand: MenuBandController?
    private var timer: Timer?

    var isVisible: Bool { panel?.isVisible == true }

    func show(menuBand: MenuBandController) {
        self.menuBand = menuBand
        if panel == nil { buildPanel() }
        guard let panel, let screen else { return }
        screen.fieldSource = { [weak menuBand] in menuBand?.fluoddityFieldSnapshot() }
        screen.particleSource = { [weak menuBand] in menuBand?.fluoddityParticleSnapshot() }
        if !panel.setFrameUsingName("FluoddityTV") {
            // First run: tuck under the menu bar at the screen's top right.
            if let vis = NSScreen.main?.visibleFrame {
                panel.setFrameTopLeftPoint(NSPoint(
                    x: vis.maxX - panel.frame.width - 12, y: vis.maxY - 8))
            }
        }
        panel.orderFront(nil)
        startTimer()
    }

    func hide() {
        timer?.invalidate(); timer = nil
        panel?.saveFrame(usingName: "FluoddityTV")
        panel?.orderOut(nil)
    }

    private func startTimer() {
        timer?.invalidate()
        let t = Timer.scheduledTimer(withTimeInterval: 1.0 / 24.0,
                                     repeats: true) { [weak self] _ in
            guard let self else { return }
            // The TV is only meaningful while Fluoddity owns notes — if the
            // user picked another instrument (from anywhere), sign off.
            guard let m = self.menuBand, m.instrumentBackend == .fluoddity,
                  self.panel?.isVisible == true else {
                self.hide()
                return
            }
            self.screen?.needsDisplay = true
        }
        t.tolerance = 0.01
        RunLoop.current.add(t, forMode: .common)
        timer = t
    }

    private func buildPanel() {
        let screenH: CGFloat = 128
        let controlsH: CGFloat = 40
        let contentW: CGFloat = 392
        let content = NSView(frame: NSRect(x: 0, y: 0, width: contentW,
                                           height: screenH + controlsH))

        let screenView = FluodTVScreenView(frame: NSRect(
            x: 0, y: controlsH, width: contentW, height: screenH))
        screenView.autoresizingMask = [.width, .height]
        content.addSubview(screenView)
        screen = screenView

        let newSpecies = NSButton(title: "🎲 New Species", target: self,
                                  action: #selector(newSpeciesClicked))
        let mutate = NSButton(title: "🧬 Mutate", target: self,
                              action: #selector(mutateClicked))
        for b in [newSpecies, mutate] {
            b.bezelStyle = .rounded
            b.controlSize = .small
            b.font = NSFont.systemFont(ofSize: 11)
        }
        newSpecies.toolTip = "Reroll the whole 80-parameter rule genome — a brand-new instrument. Applies from the next note."
        mutate.toolTip = "Evolve the current genome one step (by the slider's amount). Applies from the next note."

        let slider = NSSlider(value: 0.15, minValue: 0.02, maxValue: 0.5,
                              target: nil, action: nil)
        slider.controlSize = .mini
        slider.toolTip = "Mutation amount — nudge left for family resemblance, right for wilder children"
        amountSlider = slider

        newSpecies.sizeToFit(); mutate.sizeToFit()
        var x: CGFloat = 10
        let midY = (controlsH - newSpecies.frame.height) / 2
        newSpecies.setFrameOrigin(NSPoint(x: x, y: midY)); x += newSpecies.frame.width + 8
        mutate.setFrameOrigin(NSPoint(x: x, y: midY)); x += mutate.frame.width + 10
        slider.frame = NSRect(x: x, y: (controlsH - 16) / 2,
                              width: contentW - x - 12, height: 16)
        slider.autoresizingMask = [.width]
        content.addSubview(newSpecies)
        content.addSubview(mutate)
        content.addSubview(slider)

        let p = NSPanel(contentRect: NSRect(origin: .zero, size: content.frame.size),
                        styleMask: [.titled, .closable, .utilityWindow,
                                    .nonactivatingPanel],
                        backing: .buffered, defer: false)
        p.title = "Fluoddity"
        p.isReleasedWhenClosed = false
        p.level = .floating
        p.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary]
        p.becomesKeyOnlyIfNeeded = true
        p.isMovableByWindowBackground = true
        p.contentView = content
        panel = p
    }

    @objc private func newSpeciesClicked() {
        menuBand?.reseedFluoddity()
        screen?.flash()
    }

    @objc private func mutateClicked() {
        menuBand?.mutateFluoddity(amount: Float(amountSlider?.doubleValue ?? 0.15))
        screen?.flash()
    }
}

/// The TV's screen: trail-field heat picture + particle dots. All data
/// arrives through tear-tolerant snapshot closures — this view never
/// touches audio state directly.
final class FluodTVScreenView: NSView {
    var fieldSource: (() -> [Float]?)?
    var particleSource: (() -> [Float]?)?
    private var flashUntil: TimeInterval = 0

    /// Brief white blink acknowledging a breeding action (the audible
    /// change only lands on the next note, so the eye gets told now).
    func flash() {
        flashUntil = ProcessInfo.processInfo.systemUptime + 0.12
        needsDisplay = true
    }

    override var isOpaque: Bool { true }

    override func draw(_ dirtyRect: NSRect) {
        NSColor.black.setFill()
        bounds.fill()

        let w = 128, h = 16
        guard let field = fieldSource?(), field.count >= w * h * 2 else {
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.monospacedSystemFont(ofSize: 11, weight: .medium),
                .foregroundColor: NSColor.systemIndigo.withAlphaComponent(0.85),
            ]
            let s = NSAttributedString(string: "play a note", attributes: attrs)
            let size = s.size()
            s.draw(at: NSPoint(x: bounds.midX - size.width / 2,
                               y: bounds.midY - size.height / 2))
            return
        }

        var mags = [Float](repeating: 0, count: w * h)
        var peak: Float = 1e-6
        for i in 0..<(w * h) {
            let fx = field[i * 2], fy = field[i * 2 + 1]
            let m = (fx * fx + fy * fy).squareRoot()
            mags[i] = m
            if m > peak { peak = m }
        }
        var rgba = [UInt8](repeating: 0, count: w * h * 4)
        for i in 0..<(w * h) {
            let t = (mags[i] / peak).squareRoot()
            let r: Float, g: Float, b: Float
            if t < 0.6 {
                let u = t / 0.6
                r = 10 + u * 84; g = 8 + u * 84; b = 30 + u * 200
            } else {
                let u = (t - 0.6) / 0.4
                r = 94 + u * 146; g = 92 + u * 146; b = 230 + u * 25
            }
            rgba[i * 4 + 0] = UInt8(min(255, r))
            rgba[i * 4 + 1] = UInt8(min(255, g))
            rgba[i * 4 + 2] = UInt8(min(255, b))
            rgba[i * 4 + 3] = 255
        }
        let cs = CGColorSpaceCreateDeviceRGB()
        let image: CGImage? = rgba.withUnsafeMutableBytes { buf in
            guard let ctx = CGContext(
                data: buf.baseAddress, width: w, height: h,
                bitsPerComponent: 8, bytesPerRow: w * 4, space: cs,
                bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue)
            else { return nil }
            return ctx.makeImage()
        }
        if let image, let cg = NSGraphicsContext.current?.cgContext {
            cg.saveGState()
            cg.interpolationQuality = .none
            cg.draw(image, in: bounds)
            cg.restoreGState()
        }

        // The swarm itself: px is the scan (x) axis, py transverse.
        if let particles = particleSource?() {
            NSColor.white.withAlphaComponent(0.85).setFill()
            var i = 0
            while i + 1 < particles.count {
                let x = CGFloat(particles[i]) * bounds.width
                let y = CGFloat(particles[i + 1]) * bounds.height
                NSBezierPath(ovalIn: NSRect(x: x - 1.5, y: y - 1.5,
                                            width: 3, height: 3)).fill()
                i += 2
            }
        }

        if ProcessInfo.processInfo.systemUptime < flashUntil {
            NSColor.white.withAlphaComponent(0.35).setFill()
            bounds.fill()
        }
    }
}
