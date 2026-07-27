import AppKit
import QuartzCore

private let bundleID = "computer.aesthetic.blueberry-wallpaper"

private struct MarkSpec {
    let x: CGFloat
    let phase: CGFloat
    let width: CGFloat
    let riseSeconds: TimeInterval
    let spinSeconds: TimeInterval
    let sway: CGFloat
    let reverse: Bool
    let variant: Int
}

private final class MarkLayers {
    let container = CALayer()
    let sprites = [CALayer(), CALayer()]
    let spec: MarkSpec
    init(spec: MarkSpec) { self.spec = spec }
}

private final class PalsWallpaperView: NSView {
    private static let markScale: CGFloat = 0.85
    private static let columns = 8
    private static let rows = 3
    private static let frameCount = 24
    private static let peakOpacity: Float = 0.68
    private static let specs = [
        MarkSpec(x: 0.11, phase: 0.04, width: 205, riseSeconds: 41, spinSeconds: 22, sway: 34, reverse: false, variant: 0),
        MarkSpec(x: 0.31, phase: 0.18, width: 310, riseSeconds: 52, spinSeconds: 31, sway: 48, reverse: true, variant: 1),
        MarkSpec(x: 0.55, phase: 0.31, width: 178, riseSeconds: 36, spinSeconds: 18, sway: 27, reverse: false, variant: 2),
        MarkSpec(x: 0.76, phase: 0.46, width: 255, riseSeconds: 47, spinSeconds: 28, sway: 39, reverse: true, variant: 3),
        MarkSpec(x: 0.88, phase: 0.62, width: 145, riseSeconds: 33, spinSeconds: 16, sway: 21, reverse: false, variant: 4),
        MarkSpec(x: 0.43, phase: 0.72, width: 225, riseSeconds: 44, spinSeconds: 24, sway: 33, reverse: true, variant: 0),
        MarkSpec(x: 0.66, phase: 0.86, width: 170, riseSeconds: 38, spinSeconds: 20, sway: 25, reverse: false, variant: 1),
        MarkSpec(x: 0.20, phase: 0.93, width: 138, riseSeconds: 31, spinSeconds: 15, sway: 18, reverse: true, variant: 3),
    ]

    private let background = CAGradientLayer()
    private var marks: [MarkLayers] = []
    private var sheets: [String: CGImage] = [:]
    private var loadedAppearance = ""
    private var lastSize = CGSize.zero

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        let root = CALayer()
        root.masksToBounds = true
        layer = root
        background.startPoint = CGPoint(x: 0.03, y: 0.94)
        background.endPoint = CGPoint(x: 0.98, y: 0.05)
        root.addSublayer(background)
        buildMarks()
        updateAppearance(animated: false)
    }

    required init?(coder: NSCoder) { nil }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        updateAppearance(animated: true)
    }

    override func layout() {
        super.layout()
        background.frame = bounds
        guard bounds.size != lastSize else { return }
        lastSize = bounds.size
        placeMarks()
    }

    func pauseAnimations() {
        guard let root = layer, root.speed != 0 else { return }
        let paused = root.convertTime(CACurrentMediaTime(), from: nil)
        root.speed = 0
        root.timeOffset = paused
    }

    func resumeAnimations() {
        guard let root = layer, root.speed == 0 else { return }
        let paused = root.timeOffset
        root.speed = 1
        root.timeOffset = 0
        root.beginTime = 0
        root.beginTime = root.convertTime(CACurrentMediaTime(), from: nil) - paused
    }

    private func loadSheets(appearance: String) {
        guard loadedAppearance != appearance else { return }
        sheets.removeAll(keepingCapacity: true)
        loadedAppearance = appearance
        for variant in 0..<5 {
            let name = "pals-\(appearance)-\(variant)"
            guard let url = Bundle.main.url(forResource: name, withExtension: "png"),
                  let image = NSImage(contentsOf: url),
                  let cg = image.cgImage(forProposedRect: nil, context: nil, hints: nil)
            else { continue }
            sheets[name] = cg
        }
    }

    private func buildMarks() {
        guard let root = layer else { return }
        for spec in Self.specs {
            let mark = MarkLayers(spec: spec)
            mark.container.opacity = Self.peakOpacity
            for sprite in mark.sprites {
                sprite.contentsGravity = .resizeAspectFill
                sprite.contentsScale = 2
                mark.container.addSublayer(sprite)
            }
            root.addSublayer(mark.container)
            marks.append(mark)
        }
    }

    private func placeMarks() {
        let now = CACurrentMediaTime()
        for mark in marks {
            let spec = mark.spec
            let width = min(spec.width * Self.markScale, bounds.width * 0.26)
            let size = CGSize(width: width, height: width)
            mark.container.bounds = CGRect(origin: .zero, size: size)
            mark.sprites.forEach { $0.frame = mark.container.bounds }

            let x = bounds.width * spec.x
            let lowY = -width * 0.75
            let highY = bounds.height + width * 0.75
            let span = highY - lowY
            mark.container.position = CGPoint(x: x, y: lowY + span * spec.phase)

            let path = CGMutablePath()
            path.move(to: CGPoint(x: x, y: lowY))
            path.addCurve(to: CGPoint(x: x - spec.sway * 0.55, y: lowY + span * 0.38),
                          control1: CGPoint(x: x + spec.sway, y: lowY + span * 0.12),
                          control2: CGPoint(x: x - spec.sway, y: lowY + span * 0.27))
            path.addCurve(to: CGPoint(x: x + spec.sway * 0.34, y: lowY + span * 0.72),
                          control1: CGPoint(x: x + spec.sway * 0.60, y: lowY + span * 0.49),
                          control2: CGPoint(x: x - spec.sway * 0.45, y: lowY + span * 0.63))
            path.addCurve(to: CGPoint(x: x, y: highY),
                          control1: CGPoint(x: x + spec.sway, y: lowY + span * 0.82),
                          control2: CGPoint(x: x - spec.sway * 0.55, y: lowY + span * 0.94))

            let rise = CAKeyframeAnimation(keyPath: "position")
            rise.path = path
            rise.duration = spec.riseSeconds
            rise.repeatCount = .infinity
            rise.calculationMode = .paced
            rise.timingFunction = CAMediaTimingFunction(name: .linear)
            rise.beginTime = now - spec.riseSeconds * Double(spec.phase)
            rise.isRemovedOnCompletion = false
            mark.container.add(rise, forKey: "rise")

            let fade = CAKeyframeAnimation(keyPath: "opacity")
            fade.values = [0.0, Self.peakOpacity, Self.peakOpacity, 0.0]
            fade.keyTimes = [0.0, 0.08, 0.90, 1.0]
            fade.duration = spec.riseSeconds
            fade.repeatCount = .infinity
            fade.beginTime = rise.beginTime
            fade.isRemovedOnCompletion = false
            mark.container.add(fade, forKey: "edgeFade")

            let rects = frameRects(reverse: spec.reverse)
            let followingRects = Array(rects.dropFirst()) + [rects[0]]
            let phaseIndex = Int(CGFloat(Self.frameCount - 1) * spec.phase)
            let turnStart = now - spec.spinSeconds * Double(spec.phase)
            addTurn(rects: rects, to: mark.sprites[0], initialIndex: phaseIndex,
                    duration: spec.spinSeconds, beginTime: turnStart, key: "meshyTurnA")
            addTurn(rects: followingRects, to: mark.sprites[1], initialIndex: phaseIndex,
                    duration: spec.spinSeconds, beginTime: turnStart, key: "meshyTurnB")

            // Cross-dissolve each sampled angle into the next. Core Animation performs
            // this on the compositor while both layers share the same decoded texture.
            let frameDuration = spec.spinSeconds / Double(Self.frameCount)
            addFrameBlend(to: mark.sprites[0], from: 1, to: 0,
                          duration: frameDuration, beginTime: turnStart, key: "blendA")
            addFrameBlend(to: mark.sprites[1], from: 0, to: 1,
                          duration: frameDuration, beginTime: turnStart, key: "blendB")
        }
    }

    private func addTurn(rects: [CGRect], to sprite: CALayer, initialIndex: Int,
                         duration: TimeInterval, beginTime: CFTimeInterval, key: String) {
        sprite.contentsRect = rects[initialIndex]
        let turn = CAKeyframeAnimation(keyPath: "contentsRect")
        turn.values = rects.map(NSValue.init(rect:))
        turn.duration = duration
        turn.repeatCount = .infinity
        turn.calculationMode = .discrete
        turn.beginTime = beginTime
        turn.isRemovedOnCompletion = false
        sprite.add(turn, forKey: key)
    }

    private func addFrameBlend(to sprite: CALayer, from: Float, to: Float,
                               duration: TimeInterval, beginTime: CFTimeInterval, key: String) {
        sprite.opacity = from
        let blend = CABasicAnimation(keyPath: "opacity")
        blend.fromValue = from
        blend.toValue = to
        blend.duration = duration
        blend.repeatCount = .infinity
        blend.timingFunction = CAMediaTimingFunction(name: .linear)
        blend.beginTime = beginTime
        blend.isRemovedOnCompletion = false
        sprite.add(blend, forKey: key)
    }

    private func frameRects(reverse: Bool) -> [CGRect] {
        let sequence = reverse ? Array((0..<Self.frameCount).reversed()) : Array(0..<Self.frameCount)
        return sequence.map { frame in
            let column = frame % Self.columns
            let row = frame / Self.columns
            return CGRect(x: CGFloat(column) / CGFloat(Self.columns),
                          y: CGFloat(row) / CGFloat(Self.rows),
                          width: 1 / CGFloat(Self.columns),
                          height: 1 / CGFloat(Self.rows))
        }
    }

    private func updateAppearance(animated: Bool) {
        let dark = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
        let key = dark ? "dark" : "light"
        loadSheets(appearance: key)
        let colors: [NSColor] = dark ? [
            NSColor(srgbRed: 0.010, green: 0.018, blue: 0.080, alpha: 1),
            NSColor(srgbRed: 0.028, green: 0.072, blue: 0.225, alpha: 1),
            NSColor(srgbRed: 0.105, green: 0.082, blue: 0.300, alpha: 1),
        ] : [
            NSColor(srgbRed: 0.92, green: 0.96, blue: 1.00, alpha: 1),
            NSColor(srgbRed: 0.74, green: 0.84, blue: 0.96, alpha: 1),
            NSColor(srgbRed: 0.84, green: 0.83, blue: 0.98, alpha: 1),
        ]
        CATransaction.begin()
        CATransaction.setAnimationDuration(animated ? 0.65 : 0)
        background.colors = colors.map(\.cgColor)
        background.locations = [0.0, 0.56, 1.0]
        for mark in marks {
            let sheet = sheets["pals-\(key)-\(mark.spec.variant)"]
            mark.sprites.forEach { $0.contents = sheet }
        }
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
            print("screen=\(Int(screen.frame.width))x\(Int(screen.frame.height)) " +
                  "scale=\(screen.backingScaleFactor) window=\(window.windowNumber) " +
                  "level=\(window.level.rawValue) renderer=meshy-sprites")
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
