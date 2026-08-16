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
        screen.tableSource = { [weak menuBand] in menuBand?.fluoddityTableSnapshot() }
        if !panel.setFrameUsingName("FluoddityTV") {
            // First run: tuck under the menu bar at the screen's top right.
            if let vis = NSScreen.main?.visibleFrame {
                panel.setFrameTopLeftPoint(NSPoint(
                    x: vis.maxX - panel.frame.width - 12, y: vis.maxY - 8))
            }
        }
        panel.orderFront(nil)
        menuBand.setFluoddityVisualLiveliness(true)
        startTimer()
    }

    func hide() {
        timer?.invalidate(); timer = nil
        menuBand?.setFluoddityVisualLiveliness(false)
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
    /// The scan table — the wavetable the ear is hearing right now.
    var tableSource: (() -> [Float]?)?
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
        // Two zones: the ecosystem strip on top at an honest-ish aspect
        // (16 transverse rows should read as a ribbon, not be smeared to
        // fill the screen), and below it the oscilloscope of the scan
        // table — the very wavetable those trails are being heard as.
        let stripH = (bounds.height * 0.45).rounded()
        let strip = NSRect(x: 0, y: bounds.height - stripH,
                           width: bounds.width, height: stripH)
        let scope = NSRect(x: 0, y: 0, width: bounds.width,
                           height: bounds.height - stripH - 1)

        if let image, let cg = NSGraphicsContext.current?.cgContext {
            cg.saveGState()
            cg.interpolationQuality = .low   // organic smoke, not bar-mush
            cg.draw(image, in: strip)
            cg.restoreGState()
        }

        // The swarm itself: px is the scan (x) axis, py transverse.
        if let particles = particleSource?() {
            NSColor.white.withAlphaComponent(0.9).setFill()
            var i = 0
            while i + 1 < particles.count {
                let x = strip.minX + CGFloat(particles[i]) * strip.width
                let y = strip.minY + CGFloat(particles[i + 1]) * strip.height
                NSBezierPath(ovalIn: NSRect(x: x - 1.25, y: y - 1.25,
                                            width: 2.5, height: 2.5)).fill()
                i += 2
            }
        }

        // Oscilloscope: one cycle of the instrument, normalized per frame.
        if let table = tableSource?(), table.count >= w, scope.height > 8 {
            var peakT: Float = 1e-6
            for v in table where abs(v) > peakT { peakT = abs(v) }
            NSColor.systemIndigo.withAlphaComponent(0.25).setStroke()
            let mid = NSBezierPath()
            mid.move(to: NSPoint(x: scope.minX, y: scope.midY))
            mid.line(to: NSPoint(x: scope.maxX, y: scope.midY))
            mid.lineWidth = 1
            mid.stroke()
            let trace = NSBezierPath()
            let amp = scope.height * 0.44
            for i in 0..<w {
                let x = scope.minX + CGFloat(i) / CGFloat(w - 1) * scope.width
                let y = scope.midY + CGFloat(table[i] / peakT) * amp
                if i == 0 { trace.move(to: NSPoint(x: x, y: y)) }
                else { trace.line(to: NSPoint(x: x, y: y)) }
            }
            trace.lineWidth = 1.5
            NSColor(calibratedRed: 0.78, green: 0.77, blue: 1.0,
                    alpha: 0.95).setStroke()
            trace.stroke()
        }

        if ProcessInfo.processInfo.systemUptime < flashUntil {
            NSColor.white.withAlphaComponent(0.35).setFill()
            bounds.fill()
        }
    }
}
