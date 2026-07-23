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
            density = Float(0.82 + unit(lane: 90) * 0.36)
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
        let perimeter = max(1, lengths.reduce(0, +))
        edgeWeights = lengths.map { Float($0 / perimeter) }
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
        for (i, emitter) in emitters.enumerated() {
            let vector = vectors[i]
            let lane = UInt64(i * 12)
            let acceleration = 26 + unit(lane: lane + 1) * 28
            let cell = CAEmitterCell()
            cell.contents = Self.particleImage
            cell.color = color.withAlphaComponent(0.84).cgColor
            cell.birthRate = 1
            cell.lifetime = Float(1.7 + unit(lane: lane + 2) * 0.8)
            cell.lifetimeRange = Float(0.55 + unit(lane: lane + 3) * 0.65)
            cell.velocity = 29 + unit(lane: lane + 4) * 21
            cell.velocityRange = 13 + unit(lane: lane + 5) * 18
            cell.xAcceleration = vector.dx * acceleration
            cell.yAcceleration = vector.dy * acceleration
            cell.emissionLongitude = vector.angle + (unit(lane: lane + 6) - 0.5) * 0.42
            // Broad but still outward-facing: random tangential motion makes
            // the perimeter feel turbulent without sending particles back
            // through the terminal that emitted them.
            cell.emissionRange = 0.72 + unit(lane: lane + 7) * 0.72
            cell.scale = 0.32 + unit(lane: lane + 8) * 0.23
            cell.scaleRange = 0.16 + unit(lane: lane + 9) * 0.18
            cell.scaleSpeed = -(0.04 + unit(lane: lane + 10) * 0.10)
            cell.spin = 0
            cell.spinRange = .pi * 2
            cell.alphaSpeed = -Float(0.25 + unit(lane: lane + 11) * 0.22)
            emitter.emitterCells = [cell]
        }
    }

    private static let particleImage: CGImage? = {
        let size = CGSize(width: 12, height: 12)
        guard let context = CGContext(
            data: nil, width: Int(size.width), height: Int(size.height),
            bitsPerComponent: 8, bytesPerRow: Int(size.width) * 4,
            space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue),
              let gradient = CGGradient(
                colorsSpace: CGColorSpaceCreateDeviceRGB(),
                colors: [NSColor.white.cgColor,
                         NSColor.white.withAlphaComponent(0.5).cgColor,
                         NSColor.clear.cgColor] as CFArray,
                locations: [0, 0.36, 1]) else { return nil }
        let center = CGPoint(x: size.width / 2, y: size.width / 2)
        context.drawRadialGradient(
            gradient, startCenter: center, startRadius: 0,
            endCenter: center, endRadius: size.width / 2,
            options: [.drawsAfterEndLocation])
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
        panel.orderOut(nil)
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

        let targets = PromptSigilOverlayController.shared.promptParticleTargets
        let liveIDs = Set(targets.map(\.windowID))
        let focusedID = focusedWindowID()
        let stackAnchorID = frontmostWindowID(in: liveIDs)

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
            effect.view.emissionRate = target.windowID == focusedID ? 24 : 11
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

        let staleIDs = effects.keys.filter { !liveIDs.contains($0) }
        for id in staleIDs {
            effects.removeValue(forKey: id)?.close()
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
        panel.collectionBehavior = [.canJoinAllSpaces, .stationary,
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
