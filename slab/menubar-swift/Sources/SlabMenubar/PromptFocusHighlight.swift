import AppKit
import ApplicationServices
import QuartzCore

@_silgen_name("_AXUIElementGetWindow")
private func _FocusAXUIElementGetWindow(
    _ element: AXUIElement,
    _ windowID: UnsafeMutablePointer<CGWindowID>
) -> AXError

/// A tiny render-server-driven particle field. Swift updates geometry/theme
/// only when the prompt-rock controller already ticks; Core Animation moves
/// the particles without a per-frame app timer.
private final class PromptParticleView: NSView {
    private let emitters = (0..<4).map { _ in CAEmitterLayer() }
    private var color = NSColor.systemGreen
    private var installed = false
    private var active = false
    private var distributionSeed: UInt64 = 0
    private var density: Float = 1
    private var edgeWeights = [Float](repeating: 0.25, count: 4)
    var emissionRate: Float = 7 { didSet { applyBirthRate() } }
    var isEmitting: Bool {
        get { active }
        set { active = newValue; applyBirthRate() }
    }

    func installLayers() {
        guard !installed, let root = layer else { return }
        installed = true
        root.isGeometryFlipped = false
        root.masksToBounds = true

        for emitter in emitters {
            // A `.line` source only honors emitterSize.width, which collapses
            // vertical left/right sources to a point. A 1–3 px rectangle works
            // as the same edge source in both orientations.
            emitter.emitterShape = .rectangle
            emitter.emitterMode = .surface
            emitter.renderMode = .additive
            emitter.actions = ["frame": NSNull(), "position": NSNull(),
                               "bounds": NSNull(), "emitterPosition": NSNull(),
                               "emitterSize": NSNull(), "birthRate": NSNull()]
            root.addSublayer(emitter)
        }
        updateCells()
    }

    func configure(subject: CGRect, color nextColor: NSColor, seed: UInt64) {
        if nextColor != color || seed != distributionSeed {
            color = nextColor
            distributionSeed = seed
            density = Float(0.72 + unit(lane: 90) * 0.26)
            updateCells()
        }
        let inset: CGFloat = 12
        let sources = [
            CGRect(x: subject.minX + inset, y: subject.minY - 2,
                   width: max(12, subject.width - inset * 2), height: 3),
            CGRect(x: subject.maxX - 1, y: subject.minY + inset,
                   width: 3, height: max(12, subject.height - inset * 2)),
            CGRect(x: subject.minX + inset, y: subject.maxY - 1,
                   width: max(12, subject.width - inset * 2), height: 3),
            CGRect(x: subject.minX - 2, y: subject.minY + inset,
                   width: 3, height: max(12, subject.height - inset * 2)),
        ]
        let lengths = sources.enumerated().map { $0.offset % 2 == 0 ? $0.element.width : $0.element.height }
        // Bias each side by the prox seed instead of drawing a mechanically
        // even frame. The normalized result stays equally sparse overall, but
        // each terminal gets a recognisable little constellation.
        let weightedLengths = lengths.enumerated().map { index, length in
            length * (0.58 + unit(lane: UInt64(100 + index)) * 0.84)
        }
        let weightedPerimeter = max(1, weightedLengths.reduce(0, +))
        edgeWeights = weightedLengths.map { Float($0 / weightedPerimeter) }
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        for (i, source) in sources.enumerated() {
            let emitter = emitters[i]
            emitter.frame = bounds
            emitter.emitterPosition = CGPoint(x: source.midX, y: source.midY)
            emitter.emitterSize = CGSize(width: source.width, height: source.height)
        }
        CATransaction.commit()
        applyBirthRate()
    }

    private func applyBirthRate() {
        for (i, emitter) in emitters.enumerated() {
            emitter.birthRate = active ? emissionRate * density * edgeWeights[i] : 0
        }
    }

    /// SplitMix64 gives each prox-name seed stable, well-separated controls
    /// without turning the animation itself into a repeating canned sequence.
    private func unit(lane: UInt64) -> CGFloat {
        var z = distributionSeed &+ lane &* 0x9E3779B97F4A7C15
        z = (z ^ (z >> 30)) &* 0xBF58476D1CE4E5B9
        z = (z ^ (z >> 27)) &* 0x94D049BB133111EB
        z ^= z >> 31
        return CGFloat(z & 0xFFFF) / CGFloat(0xFFFF)
    }

    private func updateCells() {
        // AppKit layer coordinates are y-up here. Give every edge one strict
        // outward vector and keep accelerating along it, so this reads as four
        // directional sprays rather than a perimeter of drifting sparkles.
        let vectors: [(angle: CGFloat, dx: CGFloat, dy: CGFloat)] = [
            (-.pi / 2, 0, -1), // bottom → down
            (0,         1, 0), // right  → right
            (.pi / 2,   0, 1), // top    → up
            (.pi,      -1, 0), // left   → left
        ]
        let palette = starPalette()
        let colorWeights: [Float] = [0.46, 0.34, 0.20]
        for (i, emitter) in emitters.enumerated() {
            let vector = vectors[i]
            let edgeLane = UInt64(i * 40)
            let acceleration = 26 + unit(lane: edgeLane + 1) * 28
            emitter.emitterCells = palette.enumerated().flatMap { colorIndex, starColor -> [CAEmitterCell] in
                let lane = edgeLane + UInt64(colorIndex * 12)
                let cell = CAEmitterCell()
                cell.contents = Self.particleImage
                cell.color = starColor.cgColor
                cell.redRange = 0.12
                cell.greenRange = 0.12
                cell.blueRange = 0.12
                cell.redSpeed = Float((unit(lane: lane + 30) - 0.5) * 0.24)
                cell.greenSpeed = Float((unit(lane: lane + 31) - 0.5) * 0.24)
                cell.blueSpeed = Float((unit(lane: lane + 32) - 0.5) * 0.24)
                cell.birthRate = colorWeights[colorIndex]
                // Short, varied lives make individual glints wink through the
                // longer flow instead of reading as a continuous dot stream.
                cell.lifetime = Float(0.9 + unit(lane: lane + 2) * 0.75)
                cell.lifetimeRange = Float(0.35 + unit(lane: lane + 3) * 0.40)
                cell.velocity = 25 + unit(lane: lane + 4) * 19
                cell.velocityRange = 11 + unit(lane: lane + 5) * 15
                cell.xAcceleration = vector.dx * acceleration
                cell.yAcceleration = vector.dy * acceleration
                cell.emissionLongitude = vector.angle + (unit(lane: lane + 6) - 0.5) * 0.42
                // Broad but still outward-facing: random tangential motion
                // makes the composition breathe without crossing the window.
                cell.emissionRange = 0.72 + unit(lane: lane + 7) * 0.72
                cell.scale = 0.14 + unit(lane: lane + 8) * 0.06
                cell.scaleRange = 0.035 + unit(lane: lane + 9) * 0.025
                cell.scaleSpeed = -(0.008 + unit(lane: lane + 10) * 0.018)
                cell.spin = 0
                cell.spinRange = .pi * 2
                cell.alphaRange = 0.16
                cell.alphaSpeed = -Float(0.32 + unit(lane: lane + 11) * 0.24)

                // A rarer, short-lived specular cell flashes over the soft
                // moving star. Its crisp rays briefly bloom beyond the halo,
                // giving each colour its own unsynchronised little wink.
                let glint = CAEmitterCell()
                glint.contents = Self.glintImage
                glint.color = starColor.withAlphaComponent(0.96).cgColor
                glint.redRange = 0.20
                glint.greenRange = 0.20
                glint.blueRange = 0.20
                glint.redSpeed = Float((unit(lane: lane + 33) - 0.5) * 0.42)
                glint.greenSpeed = Float((unit(lane: lane + 34) - 0.5) * 0.42)
                glint.blueSpeed = Float((unit(lane: lane + 35) - 0.5) * 0.42)
                glint.birthRate = colorWeights[colorIndex] * 0.34
                glint.lifetime = Float(0.18 + unit(lane: lane + 20) * 0.22)
                glint.lifetimeRange = 0.10
                glint.velocity = 18 + unit(lane: lane + 21) * 12
                glint.velocityRange = 8
                glint.xAcceleration = vector.dx * acceleration
                glint.yAcceleration = vector.dy * acceleration
                glint.emissionLongitude = cell.emissionLongitude
                glint.emissionRange = cell.emissionRange
                glint.scale = 0.16 + unit(lane: lane + 22) * 0.09
                glint.scaleRange = 0.045
                glint.scaleSpeed = -0.22
                glint.spinRange = .pi * 0.5
                glint.alphaRange = 0.05
                glint.alphaSpeed = -1.7
                return [cell, glint]
            }
            installNeonFlicker(on: emitter, lane: edgeLane + 60)
        }
    }

    /// Independent, hard-edged intensity skips make each side behave like a
    /// tiny imperfect neon circuit. Brief dips and rebounds read as flicker;
    /// long full-bright stretches keep the field calm and legible.
    private func installNeonFlicker(on emitter: CAEmitterLayer, lane: UInt64) {
        let animation = CAKeyframeAnimation(keyPath: "opacity")
        animation.values = [1.0, 0.96, 0.42, 1.0, 0.82, 1.0, 0.58, 1.0, 1.0]
        animation.keyTimes = [0.0, 0.19, 0.205, 0.23, 0.54,
                              0.565, 0.59, 0.625, 1.0].map(NSNumber.init)
        animation.calculationMode = .discrete
        animation.duration = 1.8 + unit(lane: lane) * 2.4
        animation.beginTime = CACurrentMediaTime() - unit(lane: lane + 1) * animation.duration
        animation.repeatCount = .infinity
        animation.isRemovedOnCompletion = false
        emitter.add(animation, forKey: "prox-neon-flicker")
    }

    /// Preserve the terminal theme as the dominant note, then bend two smaller
    /// notes around the colour wheel. Additive overlap mixes them into new
    /// colours against the desktop instead of painting one flat neon tint.
    private func starPalette() -> [NSColor] {
        guard let rgb = color.usingColorSpace(.deviceRGB) else {
            return [color.withAlphaComponent(0.88),
                    NSColor.systemPink.withAlphaComponent(0.75),
                    NSColor(deviceHue: 0.52, saturation: 0.78,
                            brightness: 1, alpha: 0.67)]
        }
        var hue: CGFloat = 0
        var saturation: CGFloat = 0
        var brightness: CGFloat = 0
        var alpha: CGFloat = 0
        rgb.getHue(&hue, saturation: &saturation, brightness: &brightness, alpha: &alpha)
        let offsets: [CGFloat] = [0, 0.12, 0.56]
        let alphas: [CGFloat] = [0.88, 0.75, 0.67]
        return zip(offsets, alphas).map { offset, starAlpha in
            let shiftedHue = (hue + offset).truncatingRemainder(dividingBy: 1)
            return NSColor(deviceHue: shiftedHue,
                           saturation: max(0.58, min(0.94, saturation * 0.88 + 0.12)),
                           brightness: max(0.88, brightness),
                           alpha: starAlpha)
        }
    }

    private static let particleImage: CGImage? = {
        // The texture carries a broad halo and a four-point core; emitter scale
        // reduces the whole thing to a two-to-four-pixel flickering star.
        let size = CGSize(width: 24, height: 24)
        guard let context = CGContext(
            data: nil, width: Int(size.width), height: Int(size.height),
            bitsPerComponent: 8, bytesPerRow: Int(size.width) * 4,
            space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue),
              let gradient = CGGradient(
                colorsSpace: CGColorSpaceCreateDeviceRGB(),
                colors: [NSColor.white.cgColor,
                         NSColor.white.withAlphaComponent(0.82).cgColor,
                         NSColor.white.withAlphaComponent(0.22).cgColor,
                         NSColor.clear.cgColor] as CFArray,
                locations: [0, 0.14, 0.48, 1]) else { return nil }
        let center = CGPoint(x: size.width / 2, y: size.width / 2)
        context.drawRadialGradient(
            gradient, startCenter: center, startRadius: 0,
            endCenter: center, endRadius: size.width / 2,
            options: [.drawsAfterEndLocation])
        context.setFillColor(NSColor.white.withAlphaComponent(0.94).cgColor)
        context.move(to: CGPoint(x: center.x, y: center.y - 6))
        context.addLine(to: CGPoint(x: center.x + 1.15, y: center.y - 1.15))
        context.addLine(to: CGPoint(x: center.x + 6, y: center.y))
        context.addLine(to: CGPoint(x: center.x + 1.15, y: center.y + 1.15))
        context.addLine(to: CGPoint(x: center.x, y: center.y + 6))
        context.addLine(to: CGPoint(x: center.x - 1.15, y: center.y + 1.15))
        context.addLine(to: CGPoint(x: center.x - 6, y: center.y))
        context.addLine(to: CGPoint(x: center.x - 1.15, y: center.y - 1.15))
        context.closePath()
        context.fillPath()
        return context.makeImage()
    }()

    /// A crisp, long-rayed companion used only for very brief specular pops.
    private static let glintImage: CGImage? = {
        let size = CGSize(width: 32, height: 32)
        guard let context = CGContext(
            data: nil, width: Int(size.width), height: Int(size.height),
            bitsPerComponent: 8, bytesPerRow: Int(size.width) * 4,
            space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue),
              let gradient = CGGradient(
                colorsSpace: CGColorSpaceCreateDeviceRGB(),
                colors: [NSColor.white.cgColor,
                         NSColor.white.withAlphaComponent(0.42).cgColor,
                         NSColor.clear.cgColor] as CFArray,
                locations: [0, 0.24, 1]) else { return nil }
        let center = CGPoint(x: size.width / 2, y: size.height / 2)
        context.drawRadialGradient(
            gradient, startCenter: center, startRadius: 0,
            endCenter: center, endRadius: 9,
            options: [.drawsAfterEndLocation])
        context.setFillColor(NSColor.white.cgColor)
        context.move(to: CGPoint(x: center.x, y: center.y - 14))
        context.addLine(to: CGPoint(x: center.x + 1.2, y: center.y - 1.8))
        context.addLine(to: CGPoint(x: center.x + 7, y: center.y))
        context.addLine(to: CGPoint(x: center.x + 1.2, y: center.y + 1.8))
        context.addLine(to: CGPoint(x: center.x, y: center.y + 14))
        context.addLine(to: CGPoint(x: center.x - 1.2, y: center.y + 1.8))
        context.addLine(to: CGPoint(x: center.x - 7, y: center.y))
        context.addLine(to: CGPoint(x: center.x - 1.2, y: center.y - 1.8))
        context.closePath()
        context.fillPath()
        return context.makeImage()
    }()
}

private final class PromptParticleEffect {
    let panel: NSPanel
    let view: PromptParticleView

    init(panel: NSPanel, view: PromptParticleView) {
        self.panel = panel
        self.view = view
    }

    func close() {
        view.isEmitting = false
        // `orderOut` only hides the surface. A normal-level borderless panel
        // can remain registered with Window Server after its terminal dies,
        // where macOS window tiling exposes it as a blank full-screen ghost.
        // Closing retires the surface before the final strong reference goes.
        panel.close()
    }
}

/// Theme-coloured particles beneath every live prompt window. Geometry and
/// theme data come from PromptSigilOverlayController's existing tty binding;
/// no extra polling, window capture, or per-prompt AX observer is introduced.
/// The selected prompt emits a little more strongly, but no outline is drawn.
final class PromptFocusHighlight {
    static let shared = PromptFocusHighlight()

    private var effects: [Int: PromptParticleEffect] = [:]
    private var running = false

    /// Transparent desktop-sized particle canvases are visual children of the
    /// terminal wall, not occluders. Prompt rocks exclude exactly these own
    /// windows from their normal-window ownership snapshot while still letting
    /// real Slab previews and cards cover a rock.
    var transparentWindowIDs: Set<Int> {
        Set(effects.values.map { $0.panel.windowNumber })
    }

    private init() {}

    func start() {
        guard !running else { return }
        running = true
        refreshNow()
    }

    func stop() {
        running = false
        effects.values.forEach { $0.close() }
        effects.removeAll()
    }

    func refreshNow() {
        precondition(Thread.isMainThread)
        guard running else { return }

        // tty bindings intentionally decay slowly across transient AppleScript
        // failures, but a closed terminal must retire its particle surface on
        // the next animation tick. Window Server is authoritative for that
        // lifecycle: never keep or create an effect for a stale binding.
        let visibleTerminalIDs = onScreenTerminalWindowIDs()
        let targets = PromptSigilOverlayController.shared.promptParticleTargets
            .filter { visibleTerminalIDs.contains($0.windowID) }
        let liveIDs = Set(targets.map(\.windowID))
        let focusedID = focusedWindowID()
        let stackAnchorID = frontmostWindowID(in: liveIDs)

        // Reap first so a dead full-desktop surface cannot be raised above a
        // surviving terminal during this refresh.
        let staleIDs = effects.keys.filter { !liveIDs.contains($0) }
        for id in staleIDs {
            effects.removeValue(forKey: id)?.close()
        }

        for target in targets {
            let effect = effects[target.windowID] ?? makeEffect(for: target.windowID)
            effects[target.windowID] = effect

            let windowFrame = appKitFrame(for: target.frame)
            // Keep the render canvas fixed in global desktop coordinates.
            // Moving only the emitter positions lets particles already in
            // flight retain their world position while the terminal moves.
            let requested = desktopFrame()
            if effect.panel.frame != requested {
                effect.panel.setFrame(requested, display: false)
            }

            // AppKit can constrain a panel at a display edge; always derive
            // the source from the actual resulting panel frame.
            let actual = effect.panel.frame
            let subject = CGRect(
                x: windowFrame.minX - actual.minX,
                y: windowFrame.minY - actual.minY,
                width: windowFrame.width,
                height: windowFrame.height)
            effect.view.configure(subject: subject, color: target.color, seed: target.seed)
            effect.view.emissionRate = target.windowID == focusedID ? 18 : 8
            effect.view.isEmitting = true
            // All particle fields share one place immediately above the
            // frontmost tracked terminal. They therefore paint across the
            // complete terminal wall, while unrelated windows already above
            // that terminal remain above the particles too.
            if let anchor = stackAnchorID {
                effect.panel.order(.above, relativeTo: anchor)
            } else {
                effect.panel.orderOut(nil)
            }
        }

    }

    private func makeEffect(for windowID: Int) -> PromptParticleEffect {
        let panel = NSPanel(contentRect: .zero,
                            styleMask: [.borderless, .nonactivatingPanel],
                            backing: .buffered, defer: false)
        panel.isOpaque = false
        panel.backgroundColor = .clear
        panel.hasShadow = false
        // Share the ordinary window layer so refreshNow can insert the field
        // above the terminal wall without forcing it over unrelated apps.
        panel.level = .normal
        panel.ignoresMouseEvents = true
        panel.hidesOnDeactivate = false
        // `.transient` keeps this visual-only canvas out of Mission Control
        // and macOS window-tiling candidates. It is never a user window.
        panel.collectionBehavior = [.canJoinAllSpaces, .stationary, .transient,
                                    .ignoresCycle, .fullScreenAuxiliary]
        let view = PromptParticleView()
        panel.contentView = view
        view.wantsLayer = true
        view.layer?.backgroundColor = NSColor.clear.cgColor
        view.installLayers()
        return PromptParticleEffect(panel: panel, view: view)
    }

    private func focusedWindowID() -> Int? {
        guard let focused = WindowNav.focusedWindow() else { return nil }
        var id = CGWindowID(0)
        guard _FocusAXUIElementGetWindow(focused, &id) == .success, id != 0 else { return nil }
        return Int(id)
    }

    /// Current on-screen, normal-level Terminal/iTerm windows. Prompt-rock
    /// tty bindings may briefly outlive a closed window; this direct Window
    /// Server census prevents those stale ids from retaining particle panels.
    private func onScreenTerminalWindowIDs() -> Set<Int> {
        let terminalPIDs = Set(NSWorkspace.shared.runningApplications.compactMap { app -> pid_t? in
            switch app.bundleIdentifier {
            case "com.apple.Terminal", "com.googlecode.iterm2":
                return app.processIdentifier
            default:
                return nil
            }
        })
        guard !terminalPIDs.isEmpty,
              let infos = CGWindowListCopyWindowInfo(
                [.optionOnScreenOnly], kCGNullWindowID) as? [[String: Any]]
        else { return [] }
        return Set(infos.compactMap { info -> Int? in
            guard let layer = info[kCGWindowLayer as String] as? Int, layer == 0,
                  let pid = info[kCGWindowOwnerPID as String] as? pid_t,
                  terminalPIDs.contains(pid),
                  let number = info[kCGWindowNumber as String] as? Int
            else { return nil }
            return number
        })
    }

    /// CGWindowList is front-to-back. Pick the first tracked terminal so all
    /// particle panels can be inserted above the complete terminal wall.
    private func frontmostWindowID(in ids: Set<Int>) -> Int? {
        guard !ids.isEmpty,
              let infos = CGWindowListCopyWindowInfo(
                [.optionOnScreenOnly], kCGNullWindowID) as? [[String: Any]]
        else { return nil }
        for info in infos {
            guard let layer = info[kCGWindowLayer as String] as? Int, layer == 0,
                  let number = info[kCGWindowNumber as String] as? Int,
                  ids.contains(number) else { continue }
            return number
        }
        return nil
    }

    private func appKitFrame(for cgFrame: CGRect) -> CGRect {
        let desktopTop = NSScreen.screens.map(\.frame.maxY).max() ?? 0
        return CGRect(x: cgFrame.minX, y: desktopTop - cgFrame.maxY,
                      width: cgFrame.width, height: cgFrame.height)
    }

    private func desktopFrame() -> CGRect {
        NSScreen.screens.reduce(CGRect.null) { $0.union($1.frame) }
    }
}
