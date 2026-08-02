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
    // Tiny drifting glyphs: roughly quarter-icon to half-icon height on the
    // standard 1408×881 desktop, retaining only gentle size variation.
    private static let markScale: CGFloat = 0.10
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
        PalsSpec(x: -0.94, scale: 0.82, riseSeconds: 39, turnSeconds: 29, phase: 0.08, sway: 0.035, reverse: true),
        PalsSpec(x: -0.72, scale: 0.48, riseSeconds: 29, turnSeconds: 19, phase: 0.20, sway: 0.055, reverse: false),
        PalsSpec(x: -0.52, scale: 0.88, riseSeconds: 43, turnSeconds: 33, phase: 0.32, sway: 0.026, reverse: true),
        PalsSpec(x: -0.08, scale: 0.70, riseSeconds: 37, turnSeconds: 24, phase: 0.45, sway: 0.047, reverse: false),
        PalsSpec(x: 0.10, scale: 1.08, riseSeconds: 50, turnSeconds: 41, phase: 0.57, sway: 0.024, reverse: true),
        PalsSpec(x: 0.34, scale: 0.52, riseSeconds: 30, turnSeconds: 18, phase: 0.68, sway: 0.052, reverse: false),
        PalsSpec(x: 0.54, scale: 0.96, riseSeconds: 45, turnSeconds: 35, phase: 0.79, sway: 0.031, reverse: true),
        PalsSpec(x: 0.78, scale: 0.58, riseSeconds: 33, turnSeconds: 21, phase: 0.89, sway: 0.044, reverse: false),
        PalsSpec(x: 0.96, scale: 0.78, riseSeconds: 41, turnSeconds: 30, phase: 0.98, sway: 0.037, reverse: true),
    ]

    private let background = CAGradientLayer()
    private let fogOverlay = CAGradientLayer()
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
        sceneView.antialiasingMode = .multisampling4X
        sceneView.preferredFramesPerSecond = 30
        sceneView.rendersContinuously = true
        sceneView.isPlaying = true
        addSubview(sceneView)
        fogOverlay.isOpaque = false
        root.addSublayer(fogOverlay)
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
        fogOverlay.frame = bounds
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
        // Treat the display edges as a crop, not a frame.  Pals near either
        // side deliberately overscan by almost half their width so the live
        // field reads full-bleed instead of as an inset/letterboxed stage.
        let padding: CGFloat = 0.13
        var candidates: [Placement] = []

        for (model, spec) in zip(models, Self.specs) {
            let raw = CGFloat(elapsed / spec.riseSeconds) + spec.phase
            let progress = raw - floor(raw)
            let scale = spec.scale * Self.markScale
            let width = modelWidth * scale * 1.08
            let height = modelHeight * scale * 1.08
            // Use the full rotating footprint, not just a fraction of model
            // height.  A Pal therefore clears the crop completely before its
            // progress wraps from top to bottom—no one-frame edge pop/flicker.
            let margin = hypot(width, height) + padding
            let low = -halfHeight - margin
            let high = halfHeight + margin
            let buoyant = progress * 0.75 + progress * progress * 0.25
            let overscanWidth = halfWidth + width * 0.45
            let desiredX = spec.x * overscanWidth
                + sin(progress * .pi * 2 + spec.phase * .pi) * halfWidth * spec.sway
            candidates.append(Placement(
                model: model, x: desiredX, y: low + (high - low) * buoyant,
                width: width, height: height))
        }

        // Do not solve collisions frame-by-frame: a change in vertical order
        // makes that solver jump an entire mark in one frame.  Overlap is part
        // of the field; independent continuous paths keep every edge stable.
        for placement in candidates {
            placement.model.position = SCNVector3(placement.x, placement.y, 0)
        }
    }

    private func applyOriginalGLBMaterials(to model: SCNNode) {
        model.enumerateChildNodes { node, _ in
            for material in node.geometry?.materials ?? [] {
                material.fillMode = .fill
                material.lightingModel = .lambert
                material.diffuse.contents = NSColor.controlAccentColor
                material.emission.contents = nil
                material.multiply.contents = nil
                material.normal.contents = nil
                material.metalness.contents = 0.0
                material.roughness.contents = 1.0
                material.specular.contents = NSColor.black
                // Opaque surfaces avoid SceneKit's transparency sorting and
                // the stippled/dancing silhouettes it produces at the crop.
                material.transparency = 1.0
                if !materials.contains(where: { $0 === material }) {
                    materials.append(material)
                }
            }
        }
        applyAccentColor(animated: false)
    }

    private func applyAccentColor(animated: Bool) {
        let accent = NSColor.controlAccentColor.usingColorSpace(.sRGB) ?? .systemBlue
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let backgroundBase = dark
            ? NSColor(srgbRed: 0.028, green: 0.072, blue: 0.225, alpha: 1)
            : NSColor(srgbRed: 0.74, green: 0.84, blue: 0.96, alpha: 1)
        // Match the active middle gradient stop, including its system-accent
        // tint, then split the final Pals colour exactly 50/50 with the accent.
        let activeBackground = backgroundBase.blended(
            withFraction: dark ? 0.12 : 0.13, of: accent) ?? backgroundBase
        let faded = accent.blended(withFraction: 0.5, of: activeBackground) ?? accent
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

        let key = SCNNode()
        key.light = SCNLight()
        key.light?.type = .directional
        key.light?.intensity = 820
        key.light?.temperature = 7200
        key.eulerAngles = SCNVector3(-0.65, -0.55, -0.15)
        scene.rootNode.addChildNode(key)

        let ambient = SCNNode()
        ambient.light = SCNLight()
        ambient.light?.type = .ambient
        ambient.light?.intensity = 360
        ambient.light?.color = NSColor(srgbRed: 0.64, green: 0.73, blue: 1.0, alpha: 1)
        scene.rootNode.addChildNode(ambient)

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
        // Atmospheric haze is composited after SceneKit instead of using
        // depth fog.  It therefore shares the field palette and cannot form
        // a differently coloured fringe around antialiased geometry.
        fogOverlay.startPoint = CGPoint(x: 0.12, y: 0.98)
        fogOverlay.endPoint = CGPoint(x: 0.88, y: 0.02)
        fogOverlay.colors = dark ? [
            NSColor(srgbRed: 0.30, green: 0.43, blue: 0.78, alpha: 0.05).cgColor,
            NSColor(srgbRed: 0.48, green: 0.54, blue: 0.88, alpha: 0.14).cgColor,
            NSColor(srgbRed: 0.20, green: 0.32, blue: 0.68, alpha: 0.07).cgColor,
        ] : [
            NSColor(srgbRed: 0.88, green: 0.94, blue: 1.00, alpha: 0.08).cgColor,
            NSColor(srgbRed: 0.76, green: 0.84, blue: 0.98, alpha: 0.18).cgColor,
            NSColor(srgbRed: 0.90, green: 0.90, blue: 1.00, alpha: 0.10).cgColor,
        ]
        fogOverlay.locations = [0.0, 0.52, 1.0]
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
