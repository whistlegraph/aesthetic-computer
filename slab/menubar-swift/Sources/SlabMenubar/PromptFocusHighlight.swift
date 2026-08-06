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
    private let glow = CAShapeLayer()
    private let emitter = CAEmitterLayer()
    private var source = CGRect.zero
    private var color = NSColor.systemGreen
    private var installed = false
    private var active = false
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

        glow.shadowOpacity = 0.70
        glow.shadowRadius = 9
        glow.shadowOffset = .zero
        glow.actions = ["path": NSNull(), "fillColor": NSNull(),
                        "shadowColor": NSNull()]
        root.addSublayer(glow)

        emitter.emitterShape = .line
        emitter.emitterMode = .surface
        emitter.renderMode = .additive
        emitter.actions = ["frame": NSNull(), "position": NSNull(),
                           "bounds": NSNull(), "emitterPosition": NSNull(),
                           "emitterSize": NSNull(), "birthRate": NSNull()]
        root.addSublayer(emitter)
        updateCells()
    }

    func configure(subject: CGRect, color nextColor: NSColor) {
        source = CGRect(x: subject.minX + 12, y: subject.minY - 2,
                        width: max(12, subject.width - 24), height: 3)
        if nextColor != color {
            color = nextColor
            updateCells()
        }
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        glow.frame = bounds
        glow.path = CGPath(roundedRect: source, cornerWidth: 1.5,
                           cornerHeight: 1.5, transform: nil)
        glow.fillColor = color.withAlphaComponent(0.42).cgColor
        glow.shadowColor = color.cgColor
        emitter.frame = bounds
        emitter.emitterPosition = CGPoint(x: source.midX, y: source.minY)
        emitter.emitterSize = CGSize(width: source.width, height: 1)
        CATransaction.commit()
    }

    private func applyBirthRate() {
        emitter.birthRate = active ? emissionRate : 0
    }

    private func updateCells() {
        let cell = CAEmitterCell()
        cell.contents = Self.particleImage
        cell.color = color.withAlphaComponent(0.84).cgColor
        cell.birthRate = 1
        cell.lifetime = 1.85
        cell.lifetimeRange = 0.55
        cell.velocity = 27
        cell.velocityRange = 11
        // Radiate from the prompt in every direction; a light downward pull
        // keeps the field attached to the window without collapsing it back
        // into the narrow one-way stream this replaced.
        cell.yAcceleration = -8
        cell.emissionLongitude = 0
        cell.emissionRange = .pi * 2
        cell.scale = 0.43
        cell.scaleRange = 0.16
        cell.scaleSpeed = -0.10
        cell.alphaSpeed = -0.39
        emitter.emitterCells = [cell]
    }

    private static let particleImage: CGImage? = {
        let size = CGSize(width: 12, height: 16)
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
                locations: [0, 0.36, 1]) else { return nil }
        context.scaleBy(x: 1, y: size.height / size.width)
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

    private static let horizontalPad: CGFloat = 14
    private static let dropDepth: CGFloat = 130
    private static let topOverlap: CGFloat = 12
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
            let requested = CGRect(
                x: windowFrame.minX - Self.horizontalPad,
                y: windowFrame.minY - Self.dropDepth,
                width: windowFrame.width + Self.horizontalPad * 2,
                height: Self.dropDepth + Self.topOverlap)
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
            effect.view.configure(subject: subject, color: target.color)
            effect.view.emissionRate = target.windowID == focusedID ? 14 : 6
            effect.view.isEmitting = true
            // Raise only inside the desktop-underlay level. Every ordinary
            // terminal, preview, and app window remains above this panel, so
            // a prompt's falling light can never paint across other content.
            effect.panel.orderFrontRegardless()
        }

    }

    private func makeEffect(for windowID: Int) -> PromptParticleEffect {
        let panel = NSPanel(contentRect: .zero,
                            styleMask: [.borderless, .nonactivatingPanel],
                            backing: .buffered, defer: false)
        panel.isOpaque = false
        panel.backgroundColor = .clear
        panel.hasShadow = false
        // This is an UNDERLAY, unlike the floating Prompt Rock itself. Put it
        // one level above the desktop wallpaper but below desktop icons and
        // the entire normal-window stack. Ordering below one foreign Terminal
        // window is insufficient: other lower terminals can still wind up
        // beneath the panel and get painted over.
        panel.level = NSWindow.Level(
            Int(CGWindowLevelForKey(.desktopWindow)) + 1)
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

    private func appKitFrame(for cgFrame: CGRect) -> CGRect {
        let desktopTop = NSScreen.screens.map(\.frame.maxY).max() ?? 0
        return CGRect(x: cgFrame.minX, y: desktopTop - cgFrame.maxY,
                      width: cgFrame.width, height: cgFrame.height)
    }
}
