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
    private var edgeGlows: [CAGradientLayer] = []
    private var cornerFlash: [CGFloat] = [0, 0, 0, 0]
    private var edgeFlash: CGFloat = 0
    private var lastBeatAt: Double = 0
    private var beatCount = 0
    private var vibeTimer: Timer?

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
        sceneView.antialiasingMode = .multisampling2X
        sceneView.preferredFramesPerSecond = 30
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
        beat = Self.readBeatFile()
        DistributedNotificationCenter.default().addObserver(
            self, selector: #selector(beatDidChange(_:)),
            name: Self.beatNote, object: nil)

        loadModel()
        buildCameraAndLights()
        buildCornerGlows()
        updateAppearance(animated: false)
        let timer = Timer(timeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in
            self?.vibeTick()
        }
        timer.tolerance = 0.01
        RunLoop.main.add(timer, forMode: .common)
        vibeTimer = timer
    }

    required init?(coder: NSCoder) { nil }

    deinit {
        vibeTimer?.invalidate()
        NotificationCenter.default.removeObserver(self)
        DistributedNotificationCenter.default().removeObserver(self)
    }

    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        updateAppearance(animated: false)
        guard let scale = window?.backingScaleFactor else { return }
        // Native backing resolution avoids quadrupling render-target memory
        // on 1× external displays. MSAA still smooths the geometry edges.
        sceneView.layer?.contentsScale = scale
        // The slow desktop field needs only a front/back drawable pair.
        // A third full-screen Retina IOSurface costs ~19 MB on Blueberry.
        if let metalLayer = sceneView.layer as? CAMetalLayer {
            metalLayer.maximumDrawableCount = 2
        }
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

    /// Four radial glows anchored on the corners plus four edge washes —
    /// the outline — screen-blended over the field and the Pals so they
    /// read as light spilling in, not paint.
    private func buildCornerGlows() {
        for _ in 0..<4 {
            let g = CAGradientLayer()
            g.type = .radial
            g.startPoint = CGPoint(x: 0.5, y: 0.5)
            g.endPoint = CGPoint(x: 1, y: 1)
            g.locations = [0, 0.45, 1]
            g.opacity = 0
            g.zPosition = 10
            g.actions = ["opacity": NSNull(), "colors": NSNull(), "bounds": NSNull(), "position": NSNull()]
            layer?.addSublayer(g)
            cornerGlows.append(g)
        }
        // Edges: bottom, right, top, left — each fades inward from its edge.
        let dirs: [(CGPoint, CGPoint)] = [
            (CGPoint(x: 0.5, y: 1), CGPoint(x: 0.5, y: 0)),   // bottom (layer y is flipped)
            (CGPoint(x: 1, y: 0.5), CGPoint(x: 0, y: 0.5)),   // right
            (CGPoint(x: 0.5, y: 0), CGPoint(x: 0.5, y: 1)),   // top
            (CGPoint(x: 0, y: 0.5), CGPoint(x: 1, y: 0.5)),   // left
        ]
        for (start, end) in dirs {
            let g = CAGradientLayer()
            g.type = .axial
            g.startPoint = start
            g.endPoint = end
            g.locations = [0, 0.35, 1]
            g.opacity = 0
            g.zPosition = 11
            g.actions = ["opacity": NSNull(), "colors": NSNull(), "bounds": NSNull(), "position": NSNull()]
            layer?.addSublayer(g)
            edgeGlows.append(g)
        }
    }

    private func layoutCornerGlows() {
        guard cornerGlows.count == 4, edgeGlows.count == 4 else { return }
        let side = max(bounds.width, bounds.height) * 0.95
        let corners = [CGPoint(x: 0, y: 0), CGPoint(x: bounds.width, y: 0),
                       CGPoint(x: bounds.width, y: bounds.height), CGPoint(x: 0, y: bounds.height)]
        for (g, c) in zip(cornerGlows, corners) {
            g.bounds = CGRect(x: 0, y: 0, width: side, height: side)
            g.position = c
        }
        let w = bounds.width, h = bounds.height
        let thick = min(w, h) * 0.22
        edgeGlows[0].frame = CGRect(x: 0, y: 0, width: w, height: thick)
        edgeGlows[1].frame = CGRect(x: w - thick, y: 0, width: thick, height: h)
        edgeGlows[2].frame = CGRect(x: 0, y: h - thick, width: w, height: thick)
        edgeGlows[3].frame = CGRect(x: 0, y: 0, width: thick, height: h)
    }

    /// 30 Hz on the main thread: ease toward the published vibe, drive the
    /// corner glows, and re-tint the backdrop when the color has moved.
    private func vibeTick() {
        let now = Date().timeIntervalSince1970
        var targetLevel: CGFloat = 0
        if let vibe {
            let age = now - vibe.at
            // Hold for a beat, then let the room go quiet over ~3 s.
            // Hold for a beat, then let the room go quiet over ~2 s.
            targetLevel = CGFloat(vibe.glow) * CGFloat(exp(-max(0, age - 0.3) / 1.6))
            // The color moves fast: a new chord is a new room within a
            // couple of frames, not a slow tide.
            let c = vibe.color
            vibeColor.r += (c.redComponent - vibeColor.r) * 0.35
            vibeColor.g += (c.greenComponent - vibeColor.g) * 0.35
            vibeColor.b += (c.blueComponent - vibeColor.b) * 0.35
        }
        vibeLevel += (targetLevel - vibeLevel) * (targetLevel > vibeLevel ? 0.7 : 0.1)
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
            edgeFlash = max(edgeFlash, CGFloat(beat.energy))
        }
        for i in 0..<4 { cornerFlash[i] *= 0.80 }
        edgeFlash *= 0.76

        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let color = NSColor(srgbRed: vibeColor.r, green: vibeColor.g, blue: vibeColor.b, alpha: 1)
        let bright = color.blended(withFraction: 0.35, of: .white) ?? color
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        for (i, g) in cornerGlows.enumerated() {
            g.colors = [bright.withAlphaComponent(0.95).cgColor,
                        color.withAlphaComponent(0.45).cgColor,
                        color.withAlphaComponent(0).cgColor]
            g.compositingFilter = dark ? "screenBlendMode" : "multiplyBlendMode"
            // A slow breath per corner so the four never sit at one level.
            let breath = 0.75 + 0.25 * CGFloat(sin(now * 0.7 + Double(i) * 1.6))
            let level = min(1, vibeLevel * 0.9 * breath + cornerFlash[i] * 1.0)
            g.opacity = Float(level * (dark ? 1.0 : 0.7))
        }
        for g in edgeGlows {
            g.colors = [bright.withAlphaComponent(0.9).cgColor,
                        color.withAlphaComponent(0.35).cgColor,
                        color.withAlphaComponent(0).cgColor]
            g.compositingFilter = dark ? "screenBlendMode" : "multiplyBlendMode"
            let level = min(1, vibeLevel * 0.45 + edgeFlash * 0.95)
            g.opacity = Float(level * (dark ? 1.0 : 0.7))
        }
        CATransaction.commit()

        // Backdrop and Pals follow the color as the level rises — at most a
        // ~70 % pull so the slab tone still shows through underneath.
        vibeMix += (vibeLevel * 0.7 - vibeMix) * 0.3
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
        let tone = restingBackdropColor()
        guard vibeMix > 0.005 else { return tone }
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        var vibe = NSColor(srgbRed: vibeColor.r, green: vibeColor.g, blue: vibeColor.b, alpha: 1)
        // Keep the mode: a dark field takes a deep version of the color, a
        // light one a pastel, so menu-bar contrast never flips mid-song.
        vibe = (dark ? vibe.blended(withFraction: 0.62, of: .black)
                     : vibe.blended(withFraction: 0.55, of: .white)) ?? vibe
        return tone.blended(withFraction: vibeMix, of: vibe) ?? tone
    }

    private func restingBackdropColor() -> NSColor {
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
        layoutCornerGlows()
        guard bounds.width > 0, bounds.height > 0 else { return }

        let aspect = bounds.width / bounds.height
        stageAspect = aspect
        let halfHeight = Self.stageHalfHeight
        camera.camera?.orthographicScale = Double(halfHeight)
    }

    func pauseAnimations() {
        scene.isPaused = true
        sceneView.rendersContinuously = false
        vibeTimer?.fireDate = .distantFuture
    }

    func resumeAnimations() {
        scene.isPaused = false
        sceneView.rendersContinuously = true
        sceneView.isPlaying = true
        vibeTimer?.fireDate = Date()
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

    func renderer(_ renderer: SCNSceneRenderer, updateAtTime time: TimeInterval) {
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
        model.enumerateChildNodes { node, _ in
            if let geometry = node.geometry {
                geometry.materials = [material]
            }
        }
        materials = [material]
        applyAccentColor(animated: false)
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

    private func scheduleBackdropRebuild(reason: String) {
        backdropRebuild?.cancel()
        let work = DispatchWorkItem { [weak self] in
            print("\(reason) — rebuilding windows so the menu bar backdrop re-derives")
            fflush(stdout)
            self?.rebuildWindows()
        }
        backdropRebuild = work
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
