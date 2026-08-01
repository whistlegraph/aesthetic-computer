import AppKit
import QuartzCore
import SceneKit

private struct PalsSpec {
    let x: CGFloat
    let scale: CGFloat
    let riseSeconds: TimeInterval
    let turnSeconds: TimeInterval
    let phase: CGFloat
    let sway: CGFloat
    let reverse: Bool
}

private final class PalsWallpaperView: NSView, SCNSceneRendererDelegate {
    private struct Placement {
        let model: SCNNode
        var x: CGFloat
        var y: CGFloat
        let width: CGFloat
        let height: CGFloat
    }

    private static let stageHalfHeight: CGFloat = 2.35
    private static let markScale: CGFloat = 0.90
    private static let specs = [
        PalsSpec(x: -0.82, scale: 0.62, riseSeconds: 34, turnSeconds: 23, phase: 0.02, sway: 0.030, reverse: false),
        PalsSpec(x: -0.42, scale: 1.00, riseSeconds: 42, turnSeconds: 31, phase: 0.14, sway: 0.044, reverse: true),
        PalsSpec(x: 0.00, scale: 1.30, riseSeconds: 48, turnSeconds: 38, phase: 0.27, sway: 0.032, reverse: false),
        PalsSpec(x: 0.42, scale: 0.75, riseSeconds: 36, turnSeconds: 25, phase: 0.39, sway: 0.046, reverse: true),
        PalsSpec(x: 0.82, scale: 1.10, riseSeconds: 44, turnSeconds: 34, phase: 0.51, sway: 0.034, reverse: false),
        PalsSpec(x: -0.62, scale: 1.18, riseSeconds: 46, turnSeconds: 36, phase: 0.63, sway: 0.038, reverse: true),
        PalsSpec(x: -0.20, scale: 0.55, riseSeconds: 31, turnSeconds: 20, phase: 0.74, sway: 0.050, reverse: false),
        PalsSpec(x: 0.22, scale: 0.90, riseSeconds: 40, turnSeconds: 28, phase: 0.85, sway: 0.028, reverse: true),
        PalsSpec(x: 0.68, scale: 0.68, riseSeconds: 35, turnSeconds: 22, phase: 0.94, sway: 0.042, reverse: false),
    ]

    private let background = CAGradientLayer()
    private let sceneView = SCNView(frame: .zero)
    private let scene = SCNScene()
    private let camera = SCNNode()
    private var models: [SCNNode] = []
    private var materials: [SCNMaterial] = []
    private var stageAspect: CGFloat = 1.6
    private var motionEpoch: TimeInterval?
    private var modelWidth: CGFloat = 1
    private var modelHeight: CGFloat = 1

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        let root = CALayer()
        root.masksToBounds = true
        layer = root
        root.addSublayer(background)

        sceneView.frame = bounds
        sceneView.autoresizingMask = [.width, .height]
        sceneView.scene = scene
        sceneView.delegate = self
        sceneView.backgroundColor = .clear
        sceneView.antialiasingMode = .multisampling2X
        sceneView.preferredFramesPerSecond = 30
        sceneView.rendersContinuously = true
        sceneView.isPlaying = true
        addSubview(sceneView)
        NotificationCenter.default.addObserver(
            self, selector: #selector(systemColorsDidChange),
            name: NSColor.systemColorsDidChangeNotification, object: nil)

        loadModel()
        buildCameraAndLights()
        updateAppearance(animated: false)
    }

    required init?(coder: NSCoder) { nil }

    deinit { NotificationCenter.default.removeObserver(self) }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        guard let scale = window?.backingScaleFactor else { return }
        sceneView.layer?.contentsScale = scale
    }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        updateAppearance(animated: true)
    }

    @objc private func systemColorsDidChange() {
        applyAccentColor(animated: true)
    }

    override func layout() {
        super.layout()
        background.frame = bounds
        guard bounds.width > 0, bounds.height > 0 else { return }

        let aspect = bounds.width / bounds.height
        stageAspect = aspect
        let halfHeight = Self.stageHalfHeight
        camera.camera?.orthographicScale = Double(halfHeight)
    }

    func pauseAnimations() {
        scene.isPaused = true
        sceneView.rendersContinuously = false
    }

    func resumeAnimations() {
        scene.isPaused = false
        sceneView.rendersContinuously = true
        sceneView.isPlaying = true
    }

    private func loadModel() {
        guard let url = Bundle.main.url(
                  forResource: "pals-mesh", withExtension: "usdc",
                  subdirectory: "PalsModel"),
              let imported = try? SCNScene(url: url, options: nil)
        else {
            fputs("could not load bundled Pals GLB geometry\n", stderr)
            return
        }

        let prototype = SCNNode()
        for child in imported.rootNode.childNodes {
            child.removeFromParentNode()
            prototype.addChildNode(child)
        }
        applyOriginalGLBMaterials(to: prototype)
        let (lo, hi) = prototype.boundingBox
        modelWidth = max(CGFloat(hi.x - lo.x), 0.001)
        modelHeight = max(CGFloat(hi.y - lo.y), 0.001)
        prototype.pivot = SCNMatrix4MakeTranslation(
            (lo.x + hi.x) * 0.5,
            (lo.y + hi.y) * 0.5,
            (lo.z + hi.z) * 0.5)

        for spec in Self.specs {
            let model = prototype.clone()
            let scale = spec.scale * Self.markScale
            model.scale = SCNVector3(scale, scale, scale)
            model.eulerAngles = SCNVector3(-0.055, spec.phase * .pi * 2, 0)
            let angle = (spec.reverse ? -1 : 1) * CGFloat.pi * 2
            let turn = SCNAction.rotateBy(x: 0, y: angle, z: 0, duration: spec.turnSeconds)
            turn.timingMode = .linear
            model.runAction(.repeatForever(turn), forKey: "fullResolutionTurn")
            scene.rootNode.addChildNode(model)
            models.append(model)
        }
    }

    func renderer(_ renderer: SCNSceneRenderer, updateAtTime time: TimeInterval) {
        if motionEpoch == nil { motionEpoch = time }
        let elapsed = time - (motionEpoch ?? time)
        let halfHeight = Self.stageHalfHeight
        let halfWidth = halfHeight * stageAspect
        let padding: CGFloat = 0.13
        var candidates: [Placement] = []

        for (model, spec) in zip(models, Self.specs) {
            let raw = CGFloat(elapsed / spec.riseSeconds) + spec.phase
            let progress = raw - floor(raw)
            let scale = spec.scale * Self.markScale
            let width = modelWidth * scale * 1.08
            let height = modelHeight * scale * 1.08
            let margin = height * 0.62
            let low = -halfHeight - margin
            let high = halfHeight + margin
            let buoyant = progress * 0.75 + progress * progress * 0.25
            let xLimit = max(0, halfWidth - width * 0.5 - padding)
            let desiredX = spec.x * halfWidth
                + sin(progress * .pi * 2 + spec.phase * .pi) * halfWidth * spec.sway
            let x = min(max(desiredX, -xLimit), xLimit)
            candidates.append(Placement(
                model: model, x: x, y: low + (high - low) * buoyant,
                width: width, height: height))
        }

        candidates.sort { $0.y < $1.y }
        var placed: [Placement] = []
        for var candidate in candidates {
            var passes = 0
            while let collision = placed.first(where: {
                abs(candidate.x - $0.x) < (candidate.width + $0.width) * 0.5 + padding
                    && abs(candidate.y - $0.y) < (candidate.height + $0.height) * 0.5 + padding
            }), passes < placed.count + 1 {
                candidate.y = collision.y
                    + (candidate.height + collision.height) * 0.5 + padding
                passes += 1
            }
            placed.append(candidate)
        }

        for placement in placed {
            placement.model.position = SCNVector3(placement.x, placement.y, 0)
        }
    }

    private func applyOriginalGLBMaterials(to model: SCNNode) {
        model.enumerateChildNodes { node, _ in
            for material in node.geometry?.materials ?? [] {
                material.fillMode = .lines
                material.lightingModel = .constant
                material.diffuse.contents = NSColor.controlAccentColor
                material.emission.contents = nil
                material.multiply.contents = nil
                material.normal.contents = nil
                material.metalness.contents = nil
                material.roughness.contents = nil
                material.specular.contents = nil
                material.transparency = 0.68
                if !materials.contains(where: { $0 === material }) {
                    materials.append(material)
                }
            }
        }
        applyAccentColor(animated: false)
    }

    private func applyAccentColor(animated: Bool) {
        let accent = NSColor.controlAccentColor.usingColorSpace(.sRGB) ?? .systemBlue
        let faded = accent.blended(withFraction: 0.34, of: .white) ?? accent
        SCNTransaction.begin()
        SCNTransaction.animationDuration = animated ? 0.45 : 0
        for material in materials {
            material.diffuse.contents = faded
        }
        SCNTransaction.commit()
    }

    private func buildCameraAndLights() {
        camera.camera = SCNCamera()
        camera.camera?.usesOrthographicProjection = true
        camera.camera?.wantsHDR = false
        camera.position = SCNVector3(0, 0, 4)
        scene.rootNode.addChildNode(camera)
        sceneView.pointOfView = camera

    }

    private func updateAppearance(animated: Bool) {
        applyAccentColor(animated: animated)
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let accent = NSColor.controlAccentColor.usingColorSpace(.sRGB) ?? .systemBlue
        let bases: [NSColor] = dark ? [
            NSColor(srgbRed: 0.010, green: 0.018, blue: 0.080, alpha: 1),
            NSColor(srgbRed: 0.028, green: 0.072, blue: 0.225, alpha: 1),
            NSColor(srgbRed: 0.105, green: 0.082, blue: 0.300, alpha: 1),
        ] : [
            NSColor(srgbRed: 0.92, green: 0.96, blue: 1.00, alpha: 1),
            NSColor(srgbRed: 0.74, green: 0.84, blue: 0.96, alpha: 1),
            NSColor(srgbRed: 0.84, green: 0.83, blue: 0.98, alpha: 1),
        ]
        let strengths: [CGFloat] = dark ? [0.07, 0.12, 0.10] : [0.08, 0.13, 0.10]
        let colors = zip(bases, strengths).map {
            $0.0.blended(withFraction: $0.1, of: accent) ?? $0.0
        }
        CATransaction.begin()
        CATransaction.setAnimationDuration(animated ? 0.65 : 0)
        background.startPoint = CGPoint(x: 0.03, y: 0.94)
        background.endPoint = CGPoint(x: 0.98, y: 0.05)
        background.colors = colors.map(\.cgColor)
        background.locations = [0.0, 0.56, 1.0]
        CATransaction.commit()
    }
}

private final class WallpaperDelegate: NSObject, NSApplicationDelegate {
    private var windows: [NSWindow] = []
    private var views: [PalsWallpaperView] = []

    func applicationDidFinishLaunching(_ notification: Notification) {
        rebuildWindows()
        NotificationCenter.default.addObserver(self, selector: #selector(rebuildWindows),
            name: NSApplication.didChangeScreenParametersNotification, object: nil)
        NSWorkspace.shared.notificationCenter.addObserver(self, selector: #selector(screensDidSleep),
            name: NSWorkspace.screensDidSleepNotification, object: nil)
        NSWorkspace.shared.notificationCenter.addObserver(self, selector: #selector(screensDidWake),
            name: NSWorkspace.screensDidWakeNotification, object: nil)
    }

    @objc private func rebuildWindows() {
        windows.forEach { $0.close() }
        windows.removeAll()
        views.removeAll()
        let dark = NSApp.effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        for screen in NSScreen.screens {
            let window = NSWindow(contentRect: screen.frame, styleMask: [.borderless],
                                  backing: .buffered, defer: false, screen: screen)
            window.level = NSWindow.Level(rawValue: Int(CGWindowLevelForKey(.desktopIconWindow)) - 1)
            window.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]
            window.ignoresMouseEvents = true
            window.hasShadow = false
            window.isOpaque = true
            window.backgroundColor = dark
                ? NSColor(srgbRed: 0.01, green: 0.02, blue: 0.08, alpha: 1)
                : NSColor(srgbRed: 0.92, green: 0.96, blue: 1.00, alpha: 1)
            window.isReleasedWhenClosed = false
            let view = PalsWallpaperView(frame: NSRect(origin: .zero, size: screen.frame.size))
            view.autoresizingMask = [.width, .height]
            window.contentView = view
            window.orderFrontRegardless()
            let pixelsWide = Int(screen.frame.width * screen.backingScaleFactor)
            let pixelsHigh = Int(screen.frame.height * screen.backingScaleFactor)
            print("screen=\(pixelsWide)x\(pixelsHigh) scale=\(screen.backingScaleFactor) " +
                  "window=\(window.windowNumber) level=\(window.level.rawValue) renderer=live-glb")
            fflush(stdout)
            windows.append(window)
            views.append(view)
        }
    }

    @objc private func screensDidSleep() { views.forEach { $0.pauseAnimations() } }
    @objc private func screensDidWake() { views.forEach { $0.resumeAnimations() } }
}

let app = NSApplication.shared
private let delegate = WallpaperDelegate()
app.setActivationPolicy(.accessory)
app.delegate = delegate
app.run()
