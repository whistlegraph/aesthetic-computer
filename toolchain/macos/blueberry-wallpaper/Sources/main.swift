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

/// Optional emblem in place of the bundled Pals mesh, tinted and lit exactly
/// like it: a model file (obj, usdz, usdc, usda, dae, scn) drifts as geometry;
/// a PNG (alpha = the mark) drifts as a flat plane. Resolved from, in order,
/// `--emblem <file>`, `$BLUEBERRY_WALLPAPER_EMBLEM`, then the first of
/// `~/.config/blueberry-wallpaper/emblem.{obj,usdz,usdc,usda,dae,scn,png}`;
/// `--emblem-scale <factor>` (or `$BLUEBERRY_WALLPAPER_EMBLEM_SCALE`) sizes
/// the marks relative to the Pals field. Nothing else about the field changes.
private enum Emblem {
    static let modelExtensions = ["obj", "usdz", "usdc", "usda", "dae", "scn"]

    static let path: String? = {
        let args = CommandLine.arguments
        if let i = args.firstIndex(of: "--emblem"), i + 1 < args.count { return args[i + 1] }
        if let env = ProcessInfo.processInfo.environment["BLUEBERRY_WALLPAPER_EMBLEM"], !env.isEmpty {
            return env
        }
        let dir = NSString(string: "~/.config/blueberry-wallpaper").expandingTildeInPath
        for ext in modelExtensions + ["png"] {
            let candidate = "\(dir)/emblem.\(ext)"
            if FileManager.default.fileExists(atPath: candidate) { return candidate }
        }
        return nil
    }()

    static var isModel: Bool {
        guard let path else { return false }
        return modelExtensions.contains((path as NSString).pathExtension.lowercased())
    }

    static let scale: CGFloat = {
        let args = CommandLine.arguments
        var raw: String?
        if let i = args.firstIndex(of: "--emblem-scale"), i + 1 < args.count { raw = args[i + 1] }
        raw = raw ?? ProcessInfo.processInfo.environment["BLUEBERRY_WALLPAPER_EMBLEM_SCALE"]
        guard let raw, let value = Double(raw), value > 0 else { return 1 }
        return CGFloat(value)
    }()

    static var renderer: String {
        path == nil ? "live-glb" : (isModel ? "emblem-model" : "emblem-image")
    }
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

    private let background = CALayer()
    private let sceneView = SCNView(frame: .zero)
    private let scene = SCNScene()
    private let camera = SCNNode()
    private var models: [SCNNode] = []
    private var materials: [SCNMaterial] = []
    private var stageAspect: CGFloat = 1.6
    private var motionEpoch: TimeInterval?
    private var modelWidth: CGFloat = 1
    private var modelHeight: CGFloat = 1
    private var lastDark: Bool?

    // ── slab status tint ──────────────────────────────────────────────────
    // SlabMenubar publishes its aggregate prompt colour (theme-by-status) as
    // current-color.json plus a distributed notification; the backdrop follows
    // that tone so the field breathes with the themed terminals. Until slab
    // has spoken once, the system-accent wash stands in.
    private static let tintFile = NSString(
        string: "~/.local/share/slab/wallpaper/desktop/current-color.json").expandingTildeInPath
    private static let tintNote = Notification.Name(
        "computer.aesthetic.slab.desktop-tint.changed")
    private var statusTint: NSColor?

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
        // The backdrop is drawn by SceneKit itself (scene.background), so the
        // view is opaque and mark edges resolve against the real backdrop
        // instead of against a transparent surface composited later — the
        // latter left tinted alpha fringes along every silhouette.
        sceneView.backgroundColor = .black
        sceneView.antialiasingMode = .multisampling4X
        sceneView.preferredFramesPerSecond = 60
        sceneView.rendersContinuously = true
        sceneView.isPlaying = true
        addSubview(sceneView)
        NotificationCenter.default.addObserver(
            self, selector: #selector(systemColorsDidChange),
            name: NSColor.systemColorsDidChangeNotification, object: nil)
        statusTint = Self.readTintFile()
        DistributedNotificationCenter.default().addObserver(
            self, selector: #selector(tintDidChange(_:)),
            name: Self.tintNote, object: nil)

        loadModel()
        buildCameraAndLights()
        updateAppearance(animated: false)
    }

    required init?(coder: NSCoder) { nil }

    deinit {
        NotificationCenter.default.removeObserver(self)
        DistributedNotificationCenter.default().removeObserver(self)
    }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        updateAppearance(animated: false)
        guard let scale = window?.backingScaleFactor else { return }
        // Render at no less than 2× and let the compositor downsample: on a
        // 1× display 4× MSAA alone still shows stair-steps along the
        // silhouettes, and supersampling on top of it smooths them out.
        sceneView.layer?.contentsScale = max(scale, 2)
    }

    /// Posted after the system appearance flips. macOS keeps a light/dark
    /// treatment for the translucent menu bar's backdrop — the 74pt band it
    /// draws above every desktop-level window — and that treatment can wedge
    /// on the old appearance: blueberry sat in light mode with a dark shade
    /// pressing down on a light field (2026-09-25). Only a desktop-level
    /// window coming or going made WindowServer derive it again, so the
    /// delegate rebuilds the windows once the flip has settled.
    static let appearanceFlipped = Notification.Name(
        "computer.aesthetic.blueberry-wallpaper.appearance-flipped")

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        updateAppearance(animated: true)
        NotificationCenter.default.post(name: Self.appearanceFlipped, object: self)
    }

    @objc private func systemColorsDidChange() {
        applyAccentColor(animated: true)
    }

    @objc private func tintDidChange(_ note: Notification) {
        statusTint = Self.color(from: note.userInfo) ?? Self.readTintFile()
        updateAppearance(animated: true)
    }

    private static func readTintFile() -> NSColor? {
        guard let data = FileManager.default.contents(atPath: tintFile),
              let obj = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any]
        else { return nil }
        return color(from: obj)
    }

    private static func color(from info: [AnyHashable: Any]?) -> NSColor? {
        guard let r = (info?["red"] as? NSNumber)?.doubleValue,
              let g = (info?["green"] as? NSNumber)?.doubleValue,
              let b = (info?["blue"] as? NSNumber)?.doubleValue
        else { return nil }
        return NSColor(srgbRed: r / 65535, green: g / 65535, blue: b / 65535, alpha: 1)
    }

    /// The live backdrop tone: slab's status tint when published, otherwise
    /// the accent-tinted wash.
    private func backdropColor() -> NSColor {
        if let tint = statusTint { return tint }
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let accent = NSColor.controlAccentColor.usingColorSpace(.sRGB) ?? .systemBlue
        let base = dark
            ? NSColor(srgbRed: 0.028, green: 0.072, blue: 0.225, alpha: 1)
            : NSColor(srgbRed: 0.74, green: 0.84, blue: 0.96, alpha: 1)
        return base.blended(withFraction: dark ? 0.12 : 0.13, of: accent) ?? base
    }

    /// The backdrop as a soft vertical gradient around that tone. The top
    /// keeps the mode's own direction — lightest up top in light mode,
    /// darkest in dark mode — so the translucent menu bar sits on the tone
    /// macOS expects; the field eases the other way toward the bottom.
    private func backdropGradient() -> (top: NSColor, bottom: NSColor) {
        let tone = backdropColor().usingColorSpace(.sRGB) ?? backdropColor()
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        if dark {
            return (tone.blended(withFraction: 0.22, of: .black) ?? tone,
                    tone.blended(withFraction: 0.14, of: .white) ?? tone)
        }
        return (tone.blended(withFraction: 0.16, of: .white) ?? tone,
                tone.blended(withFraction: 0.12, of: .black) ?? tone)
    }

    /// A tall, narrow gradient image; SceneKit stretches it over the view.
    private static func gradientImage(top: NSColor, bottom: NSColor) -> NSImage {
        let size = NSSize(width: 4, height: 512)
        let image = NSImage(size: size)
        image.lockFocus()
        NSGradient(starting: bottom, ending: top)?
            .draw(in: NSRect(origin: .zero, size: size), angle: 90)
        image.unlockFocus()
        return image
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
        let prototype: SCNNode
        if let emblemPath = Emblem.path, !Emblem.isModel {
            guard let emblem = emblemPrototype(path: emblemPath) else {
                fputs("could not load emblem image at \(emblemPath)\n", stderr)
                return
            }
            prototype = emblem
        } else {
            let url: URL?
            if let emblemPath = Emblem.path {
                url = URL(fileURLWithPath: emblemPath)
            } else {
                url = Bundle.main.url(forResource: "pals-mesh", withExtension: "usdc",
                                      subdirectory: "PalsModel")
            }
            guard let url, let imported = try? SCNScene(url: url, options: nil) else {
                fputs("could not load geometry at \(url?.path ?? "bundled Pals GLB")\n", stderr)
                return
            }
            prototype = SCNNode()
            for child in imported.rootNode.childNodes {
                child.removeFromParentNode()
                prototype.addChildNode(child)
            }
            applyOriginalGLBMaterials(to: prototype)
            if Emblem.isModel, Emblem.scale != 1 {
                prototype.scale = SCNVector3(Emblem.scale, Emblem.scale, Emblem.scale)
            }
        }
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

    /// A flat, double-sided plane carrying the emblem's alpha as its mask.
    /// Unlit, so it reads as one flat tone — the same accent/backdrop blend
    /// the mesh wears — and turns like the mesh does.
    private func emblemPrototype(path: String) -> SCNNode? {
        guard let image = NSImage(contentsOfFile: path), image.size.height > 0 else { return nil }
        let aspect = image.size.width / image.size.height
        // The Pals mesh spans 1.9 × 1.24 model units before `markScale`; a
        // 1.6-unit-tall emblem covers about the same footprint, so the specs'
        // sizes carry over unchanged.
        let height: CGFloat = 1.6 * Emblem.scale
        let plane = SCNPlane(width: height * aspect, height: height)
        let material = SCNMaterial()
        material.lightingModel = .constant
        material.diffuse.contents = NSColor.controlAccentColor
        material.transparent.contents = image
        material.transparencyMode = .aOne
        material.isDoubleSided = true
        material.writesToDepthBuffer = false
        plane.materials = [material]
        materials.append(material)
        let node = SCNNode()
        node.addChildNode(SCNNode(geometry: plane))
        applyAccentColor(animated: false)
        return node
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
        // Split the final Pals colour exactly 50/50 between the accent and
        // whatever backdrop is live, so the marks always sit in its tone.
        let faded = accent.blended(withFraction: 0.5, of: backdropColor()) ?? accent
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
        let gradient = backdropGradient()
        scene.background.contents = Self.gradientImage(top: gradient.top, bottom: gradient.bottom)
        // macOS uses the opaque desktop window's backing color when choosing
        // menu-bar contrast. Keep it on the top tone, including appearance
        // flips after launch; a stale light backing creates a pale top scrim
        // with black menu text over an otherwise dark desktop.
        window?.backgroundColor = gradient.top
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let flipped = lastDark != nil && lastDark != dark
        lastDark = dark
        // The menu-bar compositor caches contrast for a desktop window even
        // after its backing color changes. Re-present it once per mode flip.
        if flipped, let window {
            window.orderOut(nil)
            DispatchQueue.main.async { window.orderFrontRegardless() }
        }
        CATransaction.begin()
        CATransaction.setAnimationDuration(animated ? 0.65 : 0)
        background.backgroundColor = gradient.top.cgColor
        CATransaction.commit()
    }
}

private final class WallpaperDelegate: NSObject, NSApplicationDelegate {
    private var windows: [NSWindow] = []
    private var views: [PalsWallpaperView] = []

    /// Pending post-flip rebuild; every flip cancels the last so a run of
    /// appearance changes ends in exactly one rebuild.
    private var flipRebuild: DispatchWorkItem?

    func applicationDidFinishLaunching(_ notification: Notification) {
        rebuildWindows()
        NotificationCenter.default.addObserver(self, selector: #selector(rebuildWindows),
            name: NSApplication.didChangeScreenParametersNotification, object: nil)
        NotificationCenter.default.addObserver(self, selector: #selector(appearanceFlipped),
            name: PalsWallpaperView.appearanceFlipped, object: nil)
        NSWorkspace.shared.notificationCenter.addObserver(self, selector: #selector(screensDidSleep),
            name: NSWorkspace.screensDidSleepNotification, object: nil)
        NSWorkspace.shared.notificationCenter.addObserver(self, selector: #selector(screensDidWake),
            name: NSWorkspace.screensDidWakeNotification, object: nil)
    }

    /// Let slab's tint, the desktop picture and the Dock settle first
    /// (slab restarts the Dock on a flip), then make WindowServer look again.
    @objc private func appearanceFlipped() {
        flipRebuild?.cancel()
        let work = DispatchWorkItem { [weak self] in
            print("appearance flipped — rebuilding windows so the menu bar backdrop re-derives")
            fflush(stdout)
            self?.rebuildWindows()
        }
        flipRebuild = work
        DispatchQueue.main.asyncAfter(deadline: .now() + 2.5, execute: work)
    }

    @objc private func rebuildWindows() {
        // New field first, old field after: the swap never shows the bare
        // desktop picture, and the window coming in plus the one going out
        // each give WindowServer its cue to re-derive the menu bar backdrop.
        let retired = windows
        windows.removeAll()
        views.removeAll()
        defer { retired.forEach { $0.close() } }
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
                  "window=\(window.windowNumber) level=\(window.level.rawValue) renderer=\(Emblem.renderer)")
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
