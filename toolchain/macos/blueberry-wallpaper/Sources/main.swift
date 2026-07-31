import AppKit
import QuartzCore
import SceneKit

private let bundleID = "computer.aesthetic.blueberry-wallpaper"
private let slabTintNotification = Notification.Name("computer.aesthetic.slab.desktop-tint.changed")

private struct SlabTint: Decodable {
    let red: Int
    let green: Int
    let blue: Int

    static func current() -> NSColor? {
        let slabHome = ProcessInfo.processInfo.environment["SLAB_HOME"]
            ?? "\(NSHomeDirectory())/.local/share/slab"
        let url = URL(fileURLWithPath: "\(slabHome)/wallpaper/desktop/current-color.json")
        guard let data = try? Data(contentsOf: url),
              let tint = try? JSONDecoder().decode(Self.self, from: data)
        else { return nil }
        return NSColor(srgbRed: CGFloat(tint.red) / 65535,
                       green: CGFloat(tint.green) / 65535,
                       blue: CGFloat(tint.blue) / 65535, alpha: 1)
    }
}

private func accentField(dark: Bool) -> NSColor {
    let target: NSColor = dark ? .black : .white
    let amount: CGFloat = dark ? 0.68 : 0.72
    return NSColor.controlAccentColor.blended(withFraction: amount, of: target)
        ?? NSColor.controlAccentColor
}

private struct MarkSpec {
    let x: CGFloat
    let phase: Double
    let width: CGFloat
    let riseSeconds: Double
    let spinSeconds: Double
    let sway: CGFloat
    let reverse: Bool
    let variant: Int
}

private struct LiveMark {
    let node: SCNNode
    let spec: MarkSpec
}

private final class PalsWallpaperView: NSView, SCNSceneRendererDelegate {
    private static let verticalSpan: CGFloat = 12
    private static let peakOpacity: CGFloat = 0.48
    /// A toroidal blue-noise layout: vertical neighbors jump seven of the
    /// eighteen horizontal lanes. Every mark shares one rise period, so that
    /// separation remains invariant rather than drifting into collisions.
    private static let palsSpecs: [MarkSpec] = (0..<18).map { i in
        let lane = (i * 7) % 18
        let widths: [CGFloat] = [52, 64, 76, 58, 84, 68]
        return MarkSpec(
            // The outer lanes sit directly on the display boundary so those
            // meshes crop naturally and the field reads as continuing beyond
            // the screen instead of stopping at an inset margin.
            x: CGFloat(lane) / 17,
            phase: Double(i) / 18,
            width: widths[i % widths.count],
            riseSeconds: 54,
            spinSeconds: Double(22 + (i * 5) % 17),
            sway: CGFloat(6 + (i * 3) % 7),
            reverse: i.isMultiple(of: 2),
            variant: i % 5)
    }
    private let sceneView: SCNView
    private let scene = SCNScene()
    private var marks: [LiveMark] = []
    private var aspect: CGFloat = 1
    private var lastSize = CGSize.zero
    private var loadedDark: Bool?

    override init(frame: NSRect) {
        sceneView = SCNView(frame: frame)
        super.init(frame: frame)
        let isNeo = ProcessInfo.processInfo.hostName
            .lowercased().split(separator: ".").first == "neo"
        wantsLayer = true
        sceneView.autoresizingMask = [.width, .height]
        sceneView.scene = scene
        sceneView.delegate = self
        sceneView.isPlaying = true
        // Neo shares an interactive pointer seat and must never let an
        // always-on desktop render contend with input. It keeps the same scene
        // at half cadence and without MSAA; Blueberry has the full treatment.
        sceneView.preferredFramesPerSecond = isNeo ? 15 : 30
        sceneView.antialiasingMode = isNeo ? .none : .multisampling2X
        sceneView.rendersContinuously = true
        sceneView.backgroundColor = .clear
        addSubview(sceneView)
        buildScene()
        updateAppearance()
    }

    required init?(coder: NSCoder) { nil }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        updateAppearance()
    }

    override func layout() {
        super.layout()
        guard bounds.size != lastSize, bounds.height > 0 else { return }
        lastSize = bounds.size
        aspect = bounds.width / bounds.height
        resizeMarks()
    }

    func refreshAccent() { updateAppearance(force: true) }
    func refreshPromptColor() { updateAppearance(force: true) }
    func pauseAnimations() { sceneView.isPlaying = false }
    func resumeAnimations() { sceneView.isPlaying = true }

    private func buildScene() {
        let camera = SCNNode()
        camera.camera = SCNCamera()
        camera.camera?.usesOrthographicProjection = true
        camera.camera?.orthographicScale = Self.verticalSpan
        camera.camera?.wantsHDR = false
        camera.position = SCNVector3(0, 0, 10)
        scene.rootNode.addChildNode(camera)
        sceneView.pointOfView = camera

        let ambient = SCNNode()
        ambient.light = SCNLight()
        ambient.light?.type = .ambient
        ambient.light?.intensity = 180
        scene.rootNode.addChildNode(ambient)
        let key = SCNNode()
        key.light = SCNLight()
        key.light?.type = .directional
        key.light?.intensity = 520
        key.eulerAngles = SCNVector3(-0.55, 0.62, 0)
        scene.rootNode.addChildNode(key)
        let rim = SCNNode()
        rim.light = SCNLight()
        rim.light?.type = .omni
        rim.light?.intensity = 240
        rim.position = SCNVector3(-2, 2, 6)
        scene.rootNode.addChildNode(rim)

        guard let modelURL = Bundle.main.url(forResource: "pals-mesh", withExtension: "usdc"),
              let imported = try? SCNScene(url: modelURL, options: nil)
        else {
            fputs("missing live Pals model\n", stderr)
            return
        }
        let template = normalizedTemplate(from: imported)
        for spec in Self.palsSpecs {
            let node = template.clone()
            makeMaterialsUnique(in: node)
            scene.rootNode.addChildNode(node)
            marks.append(LiveMark(node: node, spec: spec))
        }
    }

    private func normalizedTemplate(from imported: SCNScene) -> SCNNode {
        let content = SCNNode()
        for child in imported.rootNode.childNodes { content.addChildNode(child.clone()) }
        let (lo, hi) = content.boundingBox
        content.position = SCNVector3(-(lo.x + hi.x) / 2,
                                      -(lo.y + hi.y) / 2,
                                      -(lo.z + hi.z) / 2)
        let span = max(max(hi.x - lo.x, hi.y - lo.y), hi.z - lo.z)
        let scale = 1 / max(span, 0.001)
        content.scale = SCNVector3(scale, scale, scale)
        let outer = SCNNode()
        outer.addChildNode(content)
        outer.eulerAngles.x = -0.08
        return outer
    }

    private func makeMaterialsUnique(in node: SCNNode) {
        node.enumerateChildNodes { child, _ in
            guard let geometry = child.geometry else { return }
            geometry.materials = geometry.materials.map { ($0.copy() as? SCNMaterial) ?? $0 }
        }
    }

    private func resizeMarks() {
        for mark in marks {
            let pixels = min(mark.spec.width * 0.85, bounds.width * 0.26)
            let world = pixels / bounds.height * Self.verticalSpan
            mark.node.scale = SCNVector3(world, world, world)
        }
    }

    private func updateAppearance(force: Bool = false) {
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        if !force, loadedDark == dark { return }
        loadedDark = dark
        SCNTransaction.begin()
        SCNTransaction.animationDuration = force ? 0.65 : 0
        scene.background.contents = accentField(dark: dark)
        let palette = materialPalette(dark: dark)
        for mark in marks {
            let color = palette[mark.spec.variant % palette.count]
            mark.node.enumerateChildNodes { child, _ in
                for material in child.geometry?.materials ?? [] {
                    material.lightingModel = .physicallyBased
                    material.diffuse.contents = color
                    material.multiply.contents = NSColor.white
                    material.roughness.contents = 0.42
                    material.metalness.contents = 0.04
                    material.specular.contents = NSColor(white: 0.55, alpha: 1)
                }
            }
        }
        SCNTransaction.commit()
    }

    private func materialPalette(dark: Bool) -> [NSColor] {
        let accent = NSColor.controlAccentColor.usingColorSpace(.sRGB)
            ?? NSColor.controlAccentColor
        let prompt = SlabTint.current() ?? accent
        let promptMix = accent.blended(withFraction: 0.42, of: prompt) ?? accent
        let base = promptMix.blended(withFraction: 0.18, of: .systemTeal) ?? promptMix
        let shade: NSColor = dark ? NSColor(white: 0.72, alpha: 1) : .black
        return [
            base.blended(withFraction: dark ? 0.10 : 0.24, of: shade) ?? base,
            base.blended(withFraction: dark ? 0.18 : 0.31, of: shade) ?? base,
            base.blended(withFraction: 0.24, of: .systemBlue) ?? base,
            base.blended(withFraction: 0.26, of: .systemTeal) ?? base,
            base.blended(withFraction: 0.18, of: .systemIndigo) ?? base,
        ]
    }

    func renderer(_ renderer: any SCNSceneRenderer, updateAtTime time: TimeInterval) {
        let screenHeight = max(bounds.height, 1)
        let worldWidth = Self.verticalSpan * aspect
        for mark in marks {
            let spec = mark.spec
            var progress = time / spec.riseSeconds + spec.phase
            progress -= floor(progress)
            let width = min(spec.width * 0.85, bounds.width * 0.26)
            let margin = width / screenHeight * Self.verticalSpan * 0.75
            let low = -Self.verticalSpan / 2 - margin
            let high = Self.verticalSpan / 2 + margin
            let sway = spec.sway / screenHeight * Self.verticalSpan
            let x = (spec.x - 0.5) * worldWidth
                + sway * sin(CGFloat(progress) * .pi * 4)
            let y = low + (high - low) * CGFloat(progress)
            mark.node.position = SCNVector3(x, y, 0)
            let edge = min(CGFloat(progress) / 0.08,
                           CGFloat(1 - progress) / 0.10, 1)
            mark.node.opacity = max(0, edge) * Self.peakOpacity
            let direction: CGFloat = spec.reverse ? -1 : 1
            let turn = CGFloat(time / spec.spinSeconds) * .pi * 2 * direction
            mark.node.eulerAngles.y = turn
        }
    }
}

private final class WallpaperDelegate: NSObject, NSApplicationDelegate {
    private var windows: [NSWindow] = []
    private var views: [PalsWallpaperView] = []

    func applicationDidFinishLaunching(_ notification: Notification) {
        rebuildWindows()
        NotificationCenter.default.addObserver(
            self, selector: #selector(systemColorsDidChange),
            name: NSColor.systemColorsDidChangeNotification, object: nil)
        DistributedNotificationCenter.default().addObserver(
            self, selector: #selector(promptColorDidChange),
            name: slabTintNotification, object: nil)
        NotificationCenter.default.addObserver(
            self, selector: #selector(rebuildWindows),
            name: NSApplication.didChangeScreenParametersNotification, object: nil)
        NSWorkspace.shared.notificationCenter.addObserver(
            self, selector: #selector(screensDidSleep),
            name: NSWorkspace.screensDidSleepNotification, object: nil)
        NSWorkspace.shared.notificationCenter.addObserver(
            self, selector: #selector(screensDidWake),
            name: NSWorkspace.screensDidWakeNotification, object: nil)
    }

    @objc private func systemColorsDidChange() { views.forEach { $0.refreshAccent() } }
    @objc private func promptColorDidChange() { views.forEach { $0.refreshPromptColor() } }

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
            window.backgroundColor = accentField(dark: dark)
            window.isReleasedWhenClosed = false
            let view = PalsWallpaperView(frame: NSRect(origin: .zero, size: screen.frame.size))
            view.autoresizingMask = [.width, .height]
            window.contentView = view
            window.orderFrontRegardless()
            print("screen=\(Int(screen.frame.width))x\(Int(screen.frame.height)) "
                + "scale=\(screen.backingScaleFactor) window=\(window.windowNumber) "
                + "level=\(window.level.rawValue) renderer=live-scenekit "
                + "pals=18")
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
