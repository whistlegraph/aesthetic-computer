import AppKit
import Metal
import QuartzCore
import SceneKit

/// `--flag value` on the command line, else the environment variable.
private func option(_ flag: String, _ env: String) -> String? {
    let args = CommandLine.arguments
    if let i = args.firstIndex(of: flag), i + 1 < args.count { return args[i + 1] }
    let value = ProcessInfo.processInfo.environment[env]
    return value?.isEmpty == false ? value : nil
}

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
        if let given = option("--emblem", "BLUEBERRY_WALLPAPER_EMBLEM") { return given }
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
        guard let raw = option("--emblem-scale", "BLUEBERRY_WALLPAPER_EMBLEM_SCALE"),
              let value = Double(raw), value > 0 else { return 1 }
        return CGFloat(value)
    }()

    /// Glass soap-bubble marks: on for a model emblem (the Fuser mark),
    /// forced with `--bubbles` or `$BLUEBERRY_WALLPAPER_BUBBLES=1`.
    static let bubbles: Bool = {
        if CommandLine.arguments.contains("--no-bubbles") { return false }
        if CommandLine.arguments.contains("--bubbles") { return true }
        if ProcessInfo.processInfo.environment["BLUEBERRY_WALLPAPER_BUBBLES"] == "1" { return true }
        return isModel
    }()

    static var renderer: String {
        path == nil ? "live-glb" : (isModel ? "emblem-model" : "emblem-image")
    }
}

/// Frame pacing and antialiasing.
private enum Quality {
    /// A whole number of refreshes per frame, never under 60 fps when the
    /// display can do it: 144 Hz → 72, 120 → 60, 60 → 60. Asking a 144 Hz
    /// display for 60 alternates two- and three-refresh frames, which judders.
    static func framesPerSecond(for screen: NSScreen?) -> Int {
        let refresh = max(screen?.maximumFramesPerSecond ?? 60, 1)
        return refresh / max(1, refresh / 60)
    }

    /// Full-scene antialiasing on 1× displays: render at twice the backing
    /// resolution and let the compositor average each 2×2 block down, so
    /// shader edges, textures and specular glints are smoothed as well as
    /// silhouettes (which MSAA alone covers). Retina displays already have
    /// the pixels. `--supersample <factor>` or
    /// `$BLUEBERRY_WALLPAPER_SUPERSAMPLE` overrides; 1 turns it off.
    static func supersample(scale: CGFloat, size: CGSize) -> CGFloat {
        if let raw = option("--supersample", "BLUEBERRY_WALLPAPER_SUPERSAMPLE"),
           let value = Double(raw), value >= 1, value <= 3 { return CGFloat(value) }
        // Past ~16 MP (a 1× 4K panel doubled) the render target outweighs it.
        let doubled = size.width * scale * 2 * size.height * scale * 2
        return scale < 1.5 && doubled <= 16_000_000 ? 2 : 1
    }

    /// Multisamples per pixel on top of that: `--msaa <1|2|4>` or
    /// `$BLUEBERRY_WALLPAPER_MSAA`.
    static let sampleCount: Int = {
        if let raw = option("--msaa", "BLUEBERRY_WALLPAPER_MSAA"),
           let value = Int(raw), [1, 2, 4].contains(value) { return value }
        return 4
    }()
}

/// Counts frames as they finish and says how the pacing went: soon after
/// launch, again at a minute, then every ten.
private struct FrameMeter {
    private var last: Double = 0
    private var start: Double = 0
    private var frames = 0
    private var late = 0
    private var worst: Double = 0
    private var reports = 0

    /// A sleep is not a late frame.
    mutating func rest() { last = 0 }

    mutating func mark(target: Int) -> String? {
        let now = CACurrentMediaTime()
        defer { last = now }
        guard last > 0 else { start = now; return nil }
        let gap = now - last
        frames += 1
        worst = max(worst, gap)
        if gap > 1.5 / Double(target) { late += 1 }
        let window = now - start
        guard window >= (reports == 0 ? 10 : reports == 1 ? 50 : 600) else { return nil }
        defer { reports += 1; start = now; frames = 0; late = 0; worst = 0 }
        return String(format: "frames: %.1f fps (target %d) · worst %.1f ms · late %d/%d",
                      Double(frames) / window, target, worst * 1000, late, frames)
    }
}

/// The scene's surface: an SCNRenderer drawing into a Metal layer this view
/// owns. SCNView picks its own drawable size and redraw cadence; owning the
/// layer makes the supersampled size, the sample count and the frame clock
/// explicit — one draw per display-link tick, on the main thread, so the
/// scene and the Core Animation labels over it are stepped together.
private final class StageView: NSView {
    let renderer: SCNRenderer
    private let device: MTLDevice
    private let queue: MTLCommandQueue
    private let metal = CAMetalLayer()
    private var color: MTLTexture?
    private var depth: MTLTexture?
    /// Drawable pixels per backing pixel (see Quality.supersample).
    var supersample: CGFloat = 1 { didSet { needsLayout = true } }
    var drawableCount = 3 { didSet { metal.maximumDrawableCount = drawableCount } }

    init?(scene: SCNScene) {
        guard let device = MTLCreateSystemDefaultDevice(), let queue = device.makeCommandQueue() else { return nil }
        self.device = device
        self.queue = queue
        renderer = SCNRenderer(device: device, options: nil)
        renderer.scene = scene
        super.init(frame: .zero)
        metal.device = device
        // SceneKit shades in linear light; the sRGB target encodes on write.
        metal.pixelFormat = .bgra8Unorm_srgb
        metal.colorspace = CGColorSpace(name: CGColorSpace.sRGB)
        metal.framebufferOnly = true
        metal.isOpaque = true
        metal.contentsGravity = .resize
        metal.actions = ["bounds": NSNull(), "position": NSNull(), "contents": NSNull()]
        wantsLayer = true
        layer = metal
    }

    required init?(coder: NSCoder) { nil }

    override func layout() {
        super.layout()
        let scale = (window?.backingScaleFactor ?? 1) * supersample
        let size = CGSize(width: (bounds.width * scale).rounded(), height: (bounds.height * scale).rounded())
        guard size.width > 0, size.height > 0, size != metal.drawableSize else { return }
        metal.contentsScale = scale
        metal.drawableSize = size
        color = nil
        depth = nil
    }

    /// Multisample targets live only in tile memory: the pass resolves into
    /// the drawable and nothing else is kept, so 4× over a doubled 1440p
    /// frame costs no RAM.
    private func target(_ format: MTLPixelFormat) -> MTLTexture? {
        let d = MTLTextureDescriptor.texture2DDescriptor(
            pixelFormat: format, width: Int(metal.drawableSize.width),
            height: Int(metal.drawableSize.height), mipmapped: false)
        d.textureType = Quality.sampleCount > 1 ? .type2DMultisample : .type2D
        d.sampleCount = Quality.sampleCount
        d.usage = .renderTarget
        d.storageMode = device.hasUnifiedMemory ? .memoryless : .private
        return device.makeTexture(descriptor: d)
    }

    /// Draw the scene as it stands at `time` and queue it for the display.
    func draw(at time: CFTimeInterval) {
        guard metal.drawableSize.width > 0, let drawable = metal.nextDrawable() else { return }
        let multisampled = Quality.sampleCount > 1
        if color == nil, multisampled { color = target(metal.pixelFormat) }
        if depth == nil { depth = target(.depth32Float) }
        guard let depth, let commands = queue.makeCommandBuffer() else { return }
        let pass = MTLRenderPassDescriptor()
        if let color {
            pass.colorAttachments[0].texture = color
            pass.colorAttachments[0].resolveTexture = drawable.texture
            pass.colorAttachments[0].storeAction = .multisampleResolve
        } else {
            pass.colorAttachments[0].texture = drawable.texture
            pass.colorAttachments[0].storeAction = .store
        }
        pass.colorAttachments[0].loadAction = .clear
        pass.colorAttachments[0].clearColor = MTLClearColor(red: 0, green: 0, blue: 0, alpha: 1)
        pass.depthAttachment.texture = depth
        pass.depthAttachment.loadAction = .clear
        pass.depthAttachment.storeAction = .dontCare
        pass.depthAttachment.clearDepth = 1
        renderer.render(atTime: time, viewport: CGRect(origin: .zero, size: metal.drawableSize),
                        commandBuffer: commands, passDescriptor: pass)
        commands.present(drawable)
        commands.commit()
    }
}

private final class PalsWallpaperView: NSView, SCNSceneRendererDelegate {
    // SCNNode.clone shares geometry/materials. Keep one imported prototype
    // across displays and appearance rebuilds, not one import per window.
    private static var sharedPrototype: SCNNode?

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
    private let scene = SCNScene()
    private lazy var stage = StageView(scene: scene)
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

    // ── beat ──────────────────────────────────────────────────────────────
    // A listener (beat-listener.swift, or anything else that hears the room)
    // publishes {bpm, beatAt, energy, updatedAt} as beat.json plus a
    // distributed notification; the marks swell on each beat and settle back
    // when the beat goes quiet. Nothing listens here; this only follows.
    private static let beatFile = NSString(
        string: "~/.local/share/slab/wallpaper/beat.json").expandingTildeInPath
    private static let beatNote = Notification.Name("computer.aesthetic.slab.beat.changed")
    private struct Beat { let bpm: Double; let beatAt: Double; let energy: Double; let updatedAt: Double }
    private var beat: Beat?
    private var pulse: CGFloat = 0

    // ── vibe ──────────────────────────────────────────────────────────────
    // Menu Band's beat bus also carries a vibe: `vibeRed/Green/Blue` (sRGB
    // 0–1), the color of what's being played, and `glow` (0–1), how much is
    // going on. The backdrop and the Pals drift toward the color; four
    // corner glows breathe with the level and flash on beats. Everything
    // eases, so a chord change is a tide, not a cut.
    private struct Vibe { let color: NSColor; let glow: Double; let at: Double }
    private var vibe: Vibe?
    private var vibeColor: (r: CGFloat, g: CGFloat, b: CGFloat) = (0.4, 0.5, 1.0)
    private var vibeLevel: CGFloat = 0
    private var vibeMix: CGFloat = 0
    private var appliedVibeMix: CGFloat = -1
    private var appliedVibeColor: (r: CGFloat, g: CGFloat, b: CGFloat) = (-1, -1, -1)
    private var cornerGlows: [CAGradientLayer] = []
    private var cornerFlash: [CGFloat] = [0, 0, 0, 0]
    private var lastBeatAt: Double = 0
    private var beatCount = 0
    private let fishery = FisheryLayer()
    private let school = FishSchool()
    private let reef = Reef()
    /// Fish and reef live in tank coordinates; this node slides them so the
    /// screen shows its own stretch of a tank wider than itself.
    private let world = SCNNode()
    private var link: CADisplayLink?
    private var lastTick: Double?
    private var glowColor: (r: CGFloat, g: CGFloat, b: CGFloat) = (-1, -1, -1)
    private var glowDark: Bool?
    private var water = NSColor.black
    private var stocked = false

    // ── morphing water ────────────────────────────────────────────────────
    // A stocked field in the dark is lit water whose colour never sits
    // still: the hue walks the whole wheel every five minutes, and slow
    // currents push it a little either way across the screen. The colour is
    // made in a fragment shader on one plane behind everything, so it costs
    // the main thread nothing; the haze, the marks' tint and the menu bar's
    // backing follow the same hue a step at a time.
    private static let hueLap: Double = 300
    private let waterPlane = SCNNode()
    private let waterFilm = SCNMaterial()
    private var waterHue: CGFloat = 0.75
    private var appliedHue: CGFloat = -1
    private var environmentTone: NSColor?

    private var targetFPS = 60
    private var meter = FrameMeter()

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        let root = CALayer()
        root.masksToBounds = true
        layer = root
        root.addSublayer(background)

        // The backdrop is drawn by SceneKit itself (scene.background), so the
        // stage is opaque and mark edges resolve against the real backdrop
        // instead of against a transparent surface composited later — the
        // latter left tinted alpha fringes along every silhouette.
        if let stage {
            stage.frame = bounds
            stage.autoresizingMask = [.width, .height]
            stage.renderer.delegate = self
            addSubview(stage)
        } else {
            fputs("no Metal device; the field stays empty\n", stderr)
        }
        NotificationCenter.default.addObserver(
            self, selector: #selector(systemColorsDidChange),
            name: NSColor.systemColorsDidChangeNotification, object: nil)
        statusTint = Self.readTintFile()
        DistributedNotificationCenter.default().addObserver(
            self, selector: #selector(tintDidChange(_:)),
            name: Self.tintNote, object: nil)
        beat = Self.readBeatFile()
        DistributedNotificationCenter.default().addObserver(
            self, selector: #selector(beatDidChange(_:)),
            name: Self.beatNote, object: nil)

        loadModel()
        buildCameraAndLights()
        buildWater()
        buildCornerGlows()
        scene.rootNode.addChildNode(world)
        world.addChildNode(school.root)
        school.badge = Self.sharedPrototype
        world.addChildNode(reef.root)
        fishery.link = TankLink.shared
        fishery.zPosition = 20
        layer?.addSublayer(fishery)
        updateAppearance(animated: false)
    }

    required init?(coder: NSCoder) { nil }

    deinit {
        link?.invalidate()
        NotificationCenter.default.removeObserver(self)
        DistributedNotificationCenter.default().removeObserver(self)
    }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        // Tick on the display's own refresh instead of a free-running timer,
        // so every frame of motion lands on a vsync — no judder.
        targetFPS = Quality.framesPerSecond(for: window?.screen)
        if window != nil, link == nil {
            let link = displayLink(target: self, selector: #selector(step(_:)))
            let rate = Float(targetFPS)
            link.preferredFrameRateRange = CAFrameRateRange(minimum: rate, maximum: rate, preferred: rate)
            link.add(to: .main, forMode: .common)
            self.link = link
        }
        updateAppearance(animated: false)
        guard let scale = window?.backingScaleFactor else { return }
        // Retina renders at native backing resolution; a 1× display renders
        // doubled and is averaged down (see Quality.supersample). 4× MSAA
        // rides on top for the silhouettes.
        let supersample = Quality.supersample(scale: scale, size: bounds.size)
        stage?.supersample = supersample
        fishery.scale = scale
        // A front/back drawable pair is enough at 60 Hz and saves a
        // full-screen IOSurface (~19 MB on Blueberry). Faster displays get
        // the third so a late composite never stalls the next frame.
        stage?.drawableCount = targetFPS > 60 || supersample > 1 ? 3 : 2
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

    @objc private func beatDidChange(_ note: Notification) {
        let info = note.userInfo ?? Self.readBeatInfo()
        beat = Self.beat(from: info)
        if let v = Self.vibe(from: info) { vibe = v }
    }

    private static func readBeatInfo() -> [AnyHashable: Any]? {
        guard let data = FileManager.default.contents(atPath: beatFile),
              let obj = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any]
        else { return nil }
        return obj
    }

    private static func vibe(from info: [AnyHashable: Any]?) -> Vibe? {
        guard let r = (info?["vibeRed"] as? NSNumber)?.doubleValue,
              let g = (info?["vibeGreen"] as? NSNumber)?.doubleValue,
              let b = (info?["vibeBlue"] as? NSNumber)?.doubleValue
        else { return nil }
        let glow = (info?["glow"] as? NSNumber)?.doubleValue ?? 0.5
        let at = (info?["updatedAt"] as? NSNumber)?.doubleValue ?? Date().timeIntervalSince1970
        return Vibe(color: NSColor(srgbRed: min(1, max(0, r)), green: min(1, max(0, g)),
                                   blue: min(1, max(0, b)), alpha: 1),
                    glow: min(1, max(0, glow)), at: at)
    }

    /// Four radial glows anchored on the corners, screen-blended over the
    /// field and the Pals so they read as light spilling in from the room's
    /// corners, not paint. Radial only — straight-edged washes along the
    /// sides read as a letterbox, so there are none.
    private func buildCornerGlows() {
        for _ in 0..<4 {
            let g = CAGradientLayer()
            g.type = .radial
            g.startPoint = CGPoint(x: 0.5, y: 0.5)
            g.endPoint = CGPoint(x: 1, y: 1)
            // A long, soft foot: bright only right in the corner, gone well
            // before the far edge of the glow, so no rim ever shows.
            g.locations = [0, 0.22, 0.55, 1]
            g.opacity = 0
            g.zPosition = 10
            g.actions = ["opacity": NSNull(), "colors": NSNull(), "bounds": NSNull(), "position": NSNull()]
            layer?.addSublayer(g)
            cornerGlows.append(g)
        }
    }

    private func layoutCornerGlows() {
        guard cornerGlows.count == 4 else { return }
        let side = max(bounds.width, bounds.height) * 1.3
        let corners = [CGPoint(x: 0, y: 0), CGPoint(x: bounds.width, y: 0),
                       CGPoint(x: bounds.width, y: bounds.height), CGPoint(x: 0, y: bounds.height)]
        for (g, c) in zip(cornerGlows, corners) {
            g.bounds = CGRect(x: 0, y: 0, width: side, height: side)
            g.position = c
        }
    }

    /// Once per frame on the main thread: ease toward the published vibe,
    /// drive the corner glows, swim the fishery, and re-tint the backdrop
    /// when the color has moved. `now` is the moment the frame is shown.
    /// Easing constants are per-frame at 60 Hz, rescaled to the real step.
    private func vibeTick(now: Double) {
        let dt = CGFloat(min(0.1, max(0, now - (lastTick ?? now))))
        lastTick = now
        let ease = { (k: CGFloat) -> CGFloat in 1 - pow(1 - k, dt * 60) }
        var targetLevel: CGFloat = 0
        if let vibe {
            let age = now - vibe.at
            // Hold for a beat, then let the room go quiet over ~2 s.
            targetLevel = CGFloat(vibe.glow) * CGFloat(exp(-max(0, age - 0.3) / 1.6))
            // The color moves fast: a new chord is a new room within a
            // couple of frames, not a slow tide.
            let c = vibe.color
            vibeColor.r += (c.redComponent - vibeColor.r) * ease(0.35)
            vibeColor.g += (c.greenComponent - vibeColor.g) * ease(0.35)
            vibeColor.b += (c.blueComponent - vibeColor.b) * ease(0.35)
        }
        vibeLevel += (targetLevel - vibeLevel) * ease(targetLevel > vibeLevel ? 0.7 : 0.1)
        // Every hit flashes the outline; corners alternate pairs — odd beats
        // bottom-left and top-right hard, even beats the other pair — so
        // the room flickers rather than just pulsing.
        if let beat, beat.beatAt != lastBeatAt {
            lastBeatAt = beat.beatAt
            beatCount += 1
            let pair = beatCount % 2
            for i in 0..<4 {
                let hit = CGFloat(beat.energy) * (i % 2 == pair ? 1.0 : 0.45)
                cornerFlash[i] = max(cornerFlash[i], hit)
            }
        }
        let fade = pow(0.80, dt * 60)
        for i in 0..<4 { cornerFlash[i] *= fade }

        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        // Recolor only when the vibe has moved; a quiet room touches nothing.
        if glowDark != dark || abs(vibeColor.r - glowColor.r) + abs(vibeColor.g - glowColor.g)
            + abs(vibeColor.b - glowColor.b) > 0.004 {
            glowDark = dark
            glowColor = vibeColor
            let color = NSColor(srgbRed: vibeColor.r, green: vibeColor.g, blue: vibeColor.b, alpha: 1)
            // In the dark the glows stay saturated — colored light in blackness,
            // never a white-out; in the light they lift toward white.
            let bright = dark ? color : (color.blended(withFraction: 0.35, of: .white) ?? color)
            let colors = [bright.withAlphaComponent(dark ? 0.9 : 1.0).cgColor,
                          color.withAlphaComponent(dark ? 0.5 : 0.6).cgColor,
                          color.withAlphaComponent(dark ? 0.14 : 0.18).cgColor,
                          color.withAlphaComponent(0).cgColor]
            for g in cornerGlows {
                g.colors = colors
                g.compositingFilter = dark ? "screenBlendMode" : "multiplyBlendMode"
            }
        }
        for (i, g) in cornerGlows.enumerated() {
            // A slow breath per corner so the four never sit at one level.
            let breath = 0.75 + 0.25 * CGFloat(sin(now * 0.7 + Double(i) * 1.6))
            let level = min(1, vibeLevel * 0.9 * breath + cornerFlash[i] * 1.0)
            // A dark glow leaves the tree, so the compositor skips its
            // full-screen blend pass entirely.
            let hidden = level < 0.003
            if g.isHidden != hidden { g.isHidden = hidden }
            if !hidden { g.opacity = Float(level * (dark ? 1.0 : 0.7)) }
        }
        CATransaction.commit()
        fishery.tick(now: now, dark: dark)
        if fishery.stocked != stocked {
            stocked = fishery.stocked
            updateAppearance(animated: true)
        }
        // Everything that follows the water re-tints about once a second;
        // at this pace a step is a 250th of the wheel and cannot be seen.
        waterHue = CGFloat(now.truncatingRemainder(dividingBy: Self.hueLap) / Self.hueLap)
        if dark, stocked, abs(waterHue - appliedHue) > 0.004 { updateAppearance(animated: false) }

        // Backdrop and Pals follow the color as the level rises — at most a
        // ~70 % pull so the slab tone still shows through underneath.
        vibeMix += (vibeLevel * (dark ? 0.5 : 0.7) - vibeMix) * ease(0.3)
        let moved = abs(vibeMix - appliedVibeMix) > 0.01
            || abs(vibeColor.r - appliedVibeColor.r) + abs(vibeColor.g - appliedVibeColor.g)
               + abs(vibeColor.b - appliedVibeColor.b) > 0.03
        if moved, vibeMix > 0.005 || appliedVibeMix > 0.005 {
            appliedVibeMix = vibeMix
            appliedVibeColor = vibeColor
            updateAppearance(animated: false)
        }
    }

    private static func readBeatFile() -> Beat? {
        guard let data = FileManager.default.contents(atPath: beatFile),
              let obj = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any]
        else { return nil }
        return beat(from: obj)
    }

    private static func beat(from info: [AnyHashable: Any]?) -> Beat? {
        guard let bpm = (info?["bpm"] as? NSNumber)?.doubleValue, bpm > 30, bpm < 300,
              let beatAt = (info?["beatAt"] as? NSNumber)?.doubleValue
        else { return nil }
        let energy = (info?["energy"] as? NSNumber)?.doubleValue ?? 1
        let updatedAt = (info?["updatedAt"] as? NSNumber)?.doubleValue ?? beatAt
        return Beat(bpm: bpm, beatAt: beatAt, energy: min(max(energy, 0), 1), updatedAt: updatedAt)
    }

    /// 0 when the room is quiet; otherwise a swell that peaks on each beat and
    /// decays through the bar. Smoothed so a dropped frame never snaps a mark.
    private func beatPulse(now: Double) -> CGFloat {
        var target: CGFloat = 0
        if let beat, now - beat.updatedAt < 4 {
            let period = 60 / beat.bpm
            var phase = (now - beat.beatAt).truncatingRemainder(dividingBy: period) / period
            if phase < 0 { phase += 1 }
            target = CGFloat(exp(-phase * 6) * beat.energy) * 0.38
        }
        pulse += (target - pulse) * (target > pulse ? 0.55 : 0.12)
        return pulse
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
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        var tone = restingBackdropColor()
        // The water's middle tone; the shader paints the field itself.
        if dark, stocked { tone = NSColor(hue: waterHue, saturation: 0.72, brightness: 0.34, alpha: 1) }
        guard vibeMix > 0.005 else { return tone }
        var vibe = NSColor(srgbRed: vibeColor.r, green: vibeColor.g, blue: vibeColor.b, alpha: 1)
        // Keep the mode. Dark is an abyss: the field itself stays near
        // black and only hints at the color — the color lives in the corner
        // glows and the Pals, behind and in front of the dark. Light takes
        // a pastel of it, so menu-bar contrast never flips mid-song.
        vibe = (dark ? vibe.blended(withFraction: 0.86, of: .black)
                     : vibe.blended(withFraction: 0.55, of: .white)) ?? vibe
        return tone.blended(withFraction: vibeMix, of: vibe) ?? tone
    }

    private func restingBackdropColor() -> NSColor {
        if let tint = statusTint { return tint }
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let accent = NSColor.controlAccentColor.usingColorSpace(.sRGB) ?? .systemBlue
        let base = dark
            ? NSColor(srgbRed: 0.012, green: 0.030, blue: 0.095, alpha: 1)
            : NSColor(srgbRed: 0.74, green: 0.84, blue: 0.96, alpha: 1)
        return base.blended(withFraction: dark ? 0.08 : 0.13, of: accent) ?? base
    }

    /// The backdrop as a soft vertical gradient around that tone. The top
    /// keeps the mode's own direction — lightest up top in light mode,
    /// darkest in dark mode — so the translucent menu bar sits on the tone
    /// macOS expects; the field eases the other way toward the bottom.
    private func backdropGradient() -> (top: NSColor, bottom: NSColor) {
        let tone = backdropColor().usingColorSpace(.sRGB) ?? backdropColor()
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        if dark, stocked {
            // Water: dim under the menu bar, opening up toward the floor.
            return (tone.blended(withFraction: 0.42, of: .black) ?? tone,
                    tone.blended(withFraction: 0.10, of: .white) ?? tone)
        }
        if dark {
            // Deeper than before: the bottom no longer lifts toward white,
            // so the whole field reads as depth rather than a lit panel.
            return (tone.blended(withFraction: 0.45, of: .black) ?? tone,
                    tone.blended(withFraction: 0.10, of: .black) ?? tone)
        }
        return (tone.blended(withFraction: 0.16, of: .white) ?? tone,
                tone.blended(withFraction: 0.12, of: .black) ?? tone)
    }

    private static func waterEnvironment(tone: NSColor) -> NSImage {
        let size = NSSize(width: 64, height: 32)
        let image = NSImage(size: size)
        image.lockFocus()
        let surface = NSColor(srgbRed: 0.78, green: 0.92, blue: 1.0, alpha: 1)
        let mid = tone.blended(withFraction: 0.35, of: .white) ?? tone
        let deep = tone.blended(withFraction: 0.7, of: .black) ?? tone
        NSGradient(colors: [deep, mid, surface], atLocations: [0, 0.55, 1], colorSpace: .sRGB)?
            .draw(in: NSRect(origin: .zero, size: size), angle: 90)
        image.unlockFocus()
        return image
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
        layoutCornerGlows()
        // A tank pane: the fishery spans the whole world, shifted so this
        // screen's stretch of it lands on the glass.
        let tank = TankConfig.current
        let worldW = max(tank?.width ?? bounds.width, bounds.width)
        let offset = tank?.offset ?? 0
        fishery.viewport = CGRect(x: offset, y: 0, width: bounds.width, height: bounds.height)
        fishery.frame = CGRect(x: -offset, y: 0, width: worldW, height: bounds.height)
        guard bounds.width > 0, bounds.height > 0 else { return }
        let k = Self.stageHalfHeight / (bounds.height / 2)
        world.position = SCNVector3(-(offset + bounds.width / 2 - worldW / 2) * k, 0, 0)
        waterFilm.setValue(Float(offset / bounds.width), forKey: "paneX")

        let aspect = bounds.width / bounds.height
        stageAspect = aspect
        let halfHeight = Self.stageHalfHeight
        camera.camera?.orthographicScale = Double(halfHeight)
        if let plane = waterPlane.geometry as? SCNPlane {
            plane.width = halfHeight * aspect * 2
            plane.height = halfHeight * 2
        }
    }

    /// Simulate for the moment this frame reaches the glass, not for
    /// whenever the callback happened to run.
    @objc private func step(_ link: CADisplayLink) {
        vibeTick(now: Date().timeIntervalSince1970 + (link.targetTimestamp - CACurrentMediaTime()))
        stage?.draw(at: link.targetTimestamp)
    }

    /// A window being replaced: stop everything this view drives. The
    /// display link retains its target, so without this every rebuild left
    /// a whole view behind, still ticking and holding its scene.
    func retire() {
        link?.invalidate()
        link = nil
        stage?.renderer.delegate = nil
        stage?.renderer.scene = nil
        school.retire()
    }

    func pauseAnimations() {
        link?.isPaused = true
        scene.isPaused = true
    }

    func resumeAnimations() {
        lastTick = nil
        meter.rest()
        scene.isPaused = false
        link?.isPaused = false
    }

    private func loadModel() {
        let prototype: SCNNode
        if let shared = Self.sharedPrototype {
            prototype = shared
            prototype.enumerateChildNodes { node, _ in
                for material in node.geometry?.materials ?? [] {
                    if !materials.contains(where: { $0 === material }) {
                        materials.append(material)
                    }
                }
            }
            applyAccentColor(animated: false)
        } else if let emblemPath = Emblem.path, !Emblem.isModel {
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
        }
        let (lo, hi) = prototype.boundingBox
        modelWidth = max(CGFloat(hi.x - lo.x), 0.001)
        modelHeight = max(CGFloat(hi.y - lo.y), 0.001)
        prototype.pivot = SCNMatrix4MakeTranslation(
            (lo.x + hi.x) * 0.5,
            (lo.y + hi.y) * 0.5,
            (lo.z + hi.z) * 0.5)
        Self.sharedPrototype = prototype

        for spec in Self.specs {
            let model = prototype.clone()
            let scale = spec.scale * Self.markScale
            model.scale = SCNVector3(scale, scale, scale)
            model.eulerAngles = SCNVector3(-0.055, spec.phase * .pi * 2, 0)
            scene.rootNode.addChildNode(model)
            models.append(model)
        }
    }

    func renderer(_ renderer: SCNSceneRenderer, didRenderScene scene: SCNScene, atTime time: TimeInterval) {
        guard let line = meter.mark(target: targetFPS) else { return }
        let view = renderer.currentViewport
        print("\(line) · \(Int(view.width))x\(Int(view.height)) \(Quality.sampleCount)xMSAA")
        fflush(stdout)
    }

    func renderer(_ renderer: SCNSceneRenderer, updateAtTime time: TimeInterval) {
        waterFilm.setValue(Float(waterHue), forKey: "baseHue")
        waterFilm.setValue(Float(vibeMix), forKey: "vibeMix")
        waterFilm.setValue(NSValue(scnVector3: SCNVector3(vibeColor.r, vibeColor.g, vibeColor.b)), forKey: "vibe")
        // Inside the renderer's own update, scene changes land in this
        // frame; made outside it they wait a run loop for their transaction.
        school.water = water
        school.sync(fishery.poses, size: fishery.bounds.size, halfHeight: Self.stageHalfHeight)
        reef.sync(fishery.reef, size: fishery.bounds.size, halfHeight: Self.stageHalfHeight)
        // Wall clock, so the water of every pane in a tank flows as one.
        waterFilm.setValue(Float(Date().timeIntervalSince1970.truncatingRemainder(dividingBy: 20000)), forKey: "clock")
        if motionEpoch == nil { motionEpoch = time }
        let elapsed = time - (motionEpoch ?? time)
        let halfHeight = Self.stageHalfHeight
        let halfWidth = halfHeight * stageAspect
        // Treat the display edges as a crop, not a frame.  Pals near either
        // side deliberately overscan by almost half their width so the live
        // field reads full-bleed instead of as an inset/letterboxed stage.
        let padding: CGFloat = 0.13
        let swell = 1 + beatPulse(now: Date().timeIntervalSince1970)

        for (model, spec) in zip(models, Self.specs) {
            let turn = CGFloat(elapsed / spec.turnSeconds) * .pi * 2
            model.eulerAngles = SCNVector3(-0.055,
                spec.phase * .pi * 2 + (spec.reverse ? -turn : turn), 0)
            let raw = CGFloat(elapsed / spec.riseSeconds) + spec.phase
            let progress = raw - floor(raw)
            let scale = spec.scale * Self.markScale
            let shown = scale * swell * (Emblem.isModel ? Emblem.scale : 1)
            model.scale = SCNVector3(shown, shown, shown)
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
            model.position = SCNVector3(desiredX, low + (high - low) * buoyant, 0)
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
        // The field is accent shaded, so retain none of the imported model's
        // texture maps. All mesh instances and displays share this material.
        let material = SCNMaterial()
        material.lightingModel = .lambert
        material.diffuse.contents = NSColor.controlAccentColor
        material.metalness.contents = 0.0
        material.roughness.contents = 1.0
        material.specular.contents = NSColor.black
        if Emblem.bubbles { Self.makeBubble(material) }
        model.enumerateChildNodes { node, _ in
            if let geometry = node.geometry {
                geometry.materials = [material]
            }
        }
        materials = [material]
        applyAccentColor(animated: false)
    }

    /// Soap-film glass: clear through the middle, a bright rim, and a
    /// thin-film rainbow that slides with the viewing angle as the mark turns.
    /// The accent still tints the film. One-sided and depth-read only, so
    /// each mark shows its own front shell without fighting its neighbours.
    private static func makeBubble(_ material: SCNMaterial) {
        material.lightingModel = .blinn
        material.specular.contents = NSColor.white
        material.shininess = 0.92
        material.isDoubleSided = false
        material.writesToDepthBuffer = false
        material.blendMode = .alpha
        material.transparencyMode = .singleLayer
        material.shaderModifiers = [.fragment: """
        float3 n = normalize(_surface.normal);
        float3 v = normalize(_surface.view);
        float facing = abs(dot(n, v));
        float rim = pow(1.0 - facing, 2.0);
        float t = scn_frame.time;
        float film = rim * 2.6 + n.y * 0.9 + n.x * 0.5 + t * 0.12;
        float3 iri = 0.5 + 0.5 * cos(6.28318 * (float3(0.0, 0.33, 0.67) + film));
        float3 tint = _output.color.rgb;
        float3 glass = mix(tint * 0.35, iri, 0.55 + 0.45 * rim) * (0.25 + rim * 1.1);
        float glint = pow(saturate(dot(reflect(-v, n), normalize(float3(-0.4, 0.7, 0.6)))), 48.0);
        _output.color.rgb = glass + glint * 1.4 + pow(rim, 5.0) * 0.5;
        _output.color.a = saturate(0.08 + rim * 0.9 + glint);
        """]
    }

    private func applyAccentColor(animated: Bool) {
        let accent = NSColor.controlAccentColor.usingColorSpace(.sRGB) ?? .systemBlue
        // Split the final Pals colour exactly 50/50 between the accent and
        // whatever backdrop is live, so the marks always sit in its tone.
        var faded = accent.blended(withFraction: 0.5, of: backdropColor()) ?? accent
        if vibeMix > 0.005 {
            let vibe = NSColor(srgbRed: vibeColor.r, green: vibeColor.g, blue: vibeColor.b, alpha: 1)
            faded = faded.blended(withFraction: vibeMix * 0.8, of: vibe) ?? faded
        }
        SCNTransaction.begin()
        SCNTransaction.animationDuration = animated ? 0.45 : 0
        for material in materials {
            material.diffuse.contents = faded
        }
        SCNTransaction.commit()
    }

    /// One plane behind the whole stage, coloured per pixel. Hue: the
    /// walking base, bent by four slow sines and a little by height.
    /// Brightness: dim under the menu bar, opening toward the floor. A
    /// grain of noise breaks the banding an 8-bit dark gradient shows.
    private func buildWater() {
        waterFilm.lightingModel = .constant
        waterFilm.readsFromDepthBuffer = false
        waterFilm.writesToDepthBuffer = false
        waterFilm.shaderModifiers = [.fragment: """
        #pragma arguments
        float baseHue;
        float vibeMix;
        float3 vibe;
        float paneX;
        float clock;
        #pragma body
        // paneX: this screen's place in a tank, in screen widths, so the
        // flow carries straight across the seam.
        float2 uv = _surface.diffuseTexcoord + float2(paneX, 0.0);
        float t = clock * 0.05;
        float2 eddy = float2(0.5 + 0.35 * sin(t * 0.7), 0.5 + 0.35 * cos(t * 0.5));
        float flow = sin(uv.x * 2.3 + t * 1.3) + sin(uv.y * 1.9 - t)
                   + sin((uv.x + uv.y) * 1.4 + t * 0.6) + sin(length(uv - eddy) * 3.2 - t * 0.8);
        float hue = fract(baseHue + flow * 0.04 + (uv.y - 0.5) * 0.12);
        float3 wheel = saturate(abs(fract(hue + float3(0.0, 2.0 / 3.0, 1.0 / 3.0)) * 6.0 - 3.0) - 1.0);
        float value = mix(0.19, 0.40, uv.y) + flow * 0.012;
        float3 c = value * mix(float3(1.0), wheel, 0.74);
        // With music on, the room's colour, kept deep, takes its share.
        c = mix(c, vibe * 0.14, vibeMix);
        c += (fract(sin(dot(uv, float2(12.9898, 78.233))) * 43758.5453) - 0.5) / 255.0;
        // Authored in display values; the target wants linear light.
        _output.color.rgb = pow(max(c, 0.0), float3(2.2));
        _output.color.a = 1.0;
        """]
        let plane = SCNPlane(width: 1, height: 1)
        plane.materials = [waterFilm]
        waterPlane.geometry = plane
        waterPlane.position = SCNVector3(0, 0, -40)
        waterPlane.renderingOrder = -1000
        waterPlane.isHidden = true
        scene.rootNode.addChildNode(waterPlane)
    }

    private func buildCameraAndLights() {
        camera.camera = SCNCamera()
        // Glows and specular glints bleed softly, like light in water.
        camera.camera?.wantsHDR = false
        // HDR is only for bloom: no auto exposure, which would lift the
        // dark field and blow the fish out to white.
        camera.camera?.wantsExposureAdaptation = false
        camera.camera?.exposureOffset = 0
        camera.camera?.averageGray = 0.18
        camera.camera?.whitePoint = 1
        camera.camera?.minimumExposure = 0
        camera.camera?.maximumExposure = 0
        camera.camera?.bloomIntensity = 0
        camera.camera?.bloomThreshold = 0.9
        camera.camera?.bloomBlurRadius = 8
        camera.camera?.usesOrthographicProjection = true
        camera.position = SCNVector3(0, 0, 4)
        scene.rootNode.addChildNode(camera)
        stage?.renderer.pointOfView = camera

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
        water = backdropColor()
        let gradient = backdropGradient()
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        appliedHue = waterHue
        // The plane covers the gradient, so there is none to redraw then.
        waterPlane.isHidden = !(dark && stocked)
        if waterPlane.isHidden {
            scene.background.contents = Self.gradientImage(top: gradient.top, bottom: gradient.bottom)
        }
        // Global light: a lat-long sky the physically based fish and the
        // bubbles reflect — bright surface water above, the room's tone in
        // the middle, the deep below.
        // SceneKit refilters the whole environment whenever it changes, which
        // drops a frame; morphing water keeps one fixed sky in a hue-less
        // deep blue rather than refiltering on every step of the wheel.
        let sky: NSColor? = waterPlane.isHidden ? water : nil
        if sky != environmentTone || scene.lightingEnvironment.contents == nil {
            environmentTone = sky
            scene.lightingEnvironment.contents = Self.waterEnvironment(
                tone: sky ?? NSColor(srgbRed: 0.10, green: 0.16, blue: 0.34, alpha: 1))
        }
        scene.lightingEnvironment.intensity = 0.7
        // macOS uses the opaque desktop window's backing color when choosing
        // menu-bar contrast. Keep it on the top tone, including appearance
        // flips after launch; a stale light backing creates a pale top scrim
        // with black menu text over an otherwise dark desktop.
        window?.backgroundColor = gradient.top
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

    /// Coalesce compositor repairs after appearance changes and wake.
    private var backdropRebuild: DispatchWorkItem?

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
        scheduleBackdropRebuild(reason: "appearance flipped")
    }

    /// Two passes: the 2.5 s one catches the usual flip, the late one catches
    /// a flip that keeps settling (neo 2026-11-05 sat in Dark under a light
    /// band after the 2.5 s rebuild had already run; a restart cleared it).
    private func scheduleBackdropRebuild(reason: String) {
        backdropRebuild?.cancel()
        let work = DispatchWorkItem { [weak self] in
            print("\(reason) — rebuilding windows so the menu bar backdrop re-derives")
            fflush(stdout)
            self?.rebuildWindows()
            let late = DispatchWorkItem { [weak self] in
                print("\(reason) — late rebuild")
                fflush(stdout)
                self?.rebuildWindows()
            }
            self?.backdropRebuild = late
            DispatchQueue.main.asyncAfter(deadline: .now() + 8, execute: late)
        }
        backdropRebuild = work
        DispatchQueue.main.asyncAfter(deadline: .now() + 2.5, execute: work)
    }

    @objc private func rebuildWindows() {
        // New field first, old field after: the swap never shows the bare
        // desktop picture, and the window coming in plus the one going out
        // each give WindowServer its cue to re-derive the menu bar backdrop.
        let retired = windows
        let retiredViews = views
        windows.removeAll()
        views.removeAll()
        defer {
            retiredViews.forEach { $0.retire() }
            retired.forEach { $0.close() }
        }
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

    @objc private func screensDidSleep() {
        backdropRebuild?.cancel()
        backdropRebuild = nil
        views.forEach { $0.pauseAnimations() }
    }

    @objc private func screensDidWake() {
        views.forEach { $0.resumeAnimations() }
        // WindowServer can restore the wrong menu-bar treatment without an
        // appearance change. Neo reproduced this in Dark with a dark tint:
        // restarting Dock did nothing; recreating this window repaired it.
        // Rebuild after wake settles, also reloading Slab's persisted tint.
        scheduleBackdropRebuild(reason: "screens woke")
    }
}

let app = NSApplication.shared
private let delegate = WallpaperDelegate()
app.setActivationPolicy(.accessory)
app.delegate = delegate
app.run()
