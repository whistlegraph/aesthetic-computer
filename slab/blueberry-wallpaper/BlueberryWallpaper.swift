import AppKit
import QuartzCore

private struct MarkSpec {
    let phase: Double
    let width: CGFloat
    let riseSeconds: Double
    let spinSeconds: Double
    let sway: CGFloat
    let reverse: Bool
    let variant: Int
}

private final class MarkLayers {
    let container = CALayer()
    let sprites: [CALayer]
    let spec: MarkSpec

    init(spec: MarkSpec, images: [CGImage], scale: CGFloat) {
        self.spec = spec
        let image = images[spec.variant % images.count]
        sprites = (0..<2).map { _ in
            let layer = CALayer()
            layer.contents = image
            layer.contentsGravity = .resizeAspect
            layer.contentsScale = scale
            layer.opacity = 0
            return layer
        }
        sprites.forEach(container.addSublayer)
        sprites[0].opacity = 1
    }
}

private final class PalsWallpaperView: NSView {
    private let background = CAGradientLayer()
    private var marks: [MarkLayers] = []
    private var loadedAppearance: NSAppearance.Name?
    private var lastSize = CGSize.zero

    private let specs: [MarkSpec] = [
        .init(phase: 0.03, width: 94,  riseSeconds: 31, spinSeconds: 2.4, sway: 70, reverse: false, variant: 0),
        .init(phase: 0.16, width: 58,  riseSeconds: 25, spinSeconds: 2.8, sway: 48, reverse: true,  variant: 2),
        .init(phase: 0.29, width: 122, riseSeconds: 38, spinSeconds: 3.2, sway: 95, reverse: false, variant: 4),
        .init(phase: 0.41, width: 72,  riseSeconds: 28, spinSeconds: 2.6, sway: 56, reverse: true,  variant: 1),
        .init(phase: 0.54, width: 108, riseSeconds: 35, spinSeconds: 3.0, sway: 82, reverse: false, variant: 3),
        .init(phase: 0.66, width: 48,  riseSeconds: 23, spinSeconds: 2.2, sway: 42, reverse: true,  variant: 0),
        .init(phase: 0.77, width: 82,  riseSeconds: 30, spinSeconds: 2.9, sway: 64, reverse: false, variant: 2),
        .init(phase: 0.88, width: 64,  riseSeconds: 27, spinSeconds: 2.5, sway: 50, reverse: true,  variant: 4),
    ]

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        layer = CALayer()
        layer?.masksToBounds = true
        layer?.addSublayer(background)
        rebuild()
    }

    required init?(coder: NSCoder) { nil }

    override func layout() {
        super.layout()
        guard bounds.size != lastSize else { return }
        lastSize = bounds.size
        rebuild()
    }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        let appearance = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua])
        guard appearance != loadedAppearance else { return }
        rebuild()
    }

    private var dark: Bool {
        effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
    }

    private func loadImages() -> [CGImage] {
        (0..<5).compactMap { index in
            guard let url = Bundle.main.url(forResource: "pals-\(dark ? "dark" : "light")-\(index)",
                                            withExtension: "png"),
                  let image = NSImage(contentsOf: url) else { return nil }
            return image.cgImage(forProposedRect: nil, context: nil, hints: nil)
        }
    }

    private func rebuild() {
        guard bounds.width > 0, bounds.height > 0 else { return }
        loadedAppearance = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua])
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        background.frame = bounds
        background.startPoint = CGPoint(x: 0, y: 1)
        background.endPoint = CGPoint(x: 1, y: 0)
        background.colors = dark
            ? [NSColor(srgbRed: 0.045, green: 0.09, blue: 0.16, alpha: 1).cgColor,
               NSColor(srgbRed: 0.12, green: 0.07, blue: 0.19, alpha: 1).cgColor]
            : [NSColor(srgbRed: 0.72, green: 0.84, blue: 0.98, alpha: 1).cgColor,
               NSColor(srgbRed: 0.90, green: 0.86, blue: 1.00, alpha: 1).cgColor]
        marks.forEach { $0.container.removeFromSuperlayer() }
        marks.removeAll()

        let images = loadImages()
        guard images.count == 5 else { CATransaction.commit(); return }
        let scale = window?.backingScaleFactor ?? NSScreen.main?.backingScaleFactor ?? 2
        for (index, spec) in specs.enumerated() {
            let mark = MarkLayers(spec: spec, images: images, scale: scale)
            // Each source PNG is an 8×3 atlas of square rotation frames.
            mark.container.bounds = CGRect(x: 0, y: 0, width: spec.width, height: spec.width)
            mark.container.anchorPoint = CGPoint(x: 0.5, y: 0.5)
            mark.sprites.forEach { $0.frame = mark.container.bounds }
            layer?.addSublayer(mark.container)
            animate(mark, index: index)
            marks.append(mark)
        }
        CATransaction.commit()
    }

    private func animate(_ mark: MarkLayers, index: Int) {
        let spec = mark.spec
        let now = layer?.convertTime(CACurrentMediaTime(), from: nil) ?? CACurrentMediaTime()
        let x = bounds.width * CGFloat(0.08 + (Double(index) * 0.137).truncatingRemainder(dividingBy: 0.84))
        let low = -mark.container.bounds.height
        let high = bounds.height + mark.container.bounds.height
        let path = CGMutablePath()
        path.move(to: CGPoint(x: x, y: low))
        path.addCurve(to: CGPoint(x: x + spec.sway, y: low + (high - low) * 0.34),
                      control1: CGPoint(x: x - spec.sway, y: low + (high - low) * 0.12),
                      control2: CGPoint(x: x + spec.sway * 1.3, y: low + (high - low) * 0.22))
        path.addCurve(to: CGPoint(x: x - spec.sway * 0.4, y: low + (high - low) * 0.68),
                      control1: CGPoint(x: x + spec.sway * 0.6, y: low + (high - low) * 0.46),
                      control2: CGPoint(x: x - spec.sway * 1.2, y: low + (high - low) * 0.57))
        path.addCurve(to: CGPoint(x: x, y: high),
                      control1: CGPoint(x: x + spec.sway * 0.7, y: low + (high - low) * 0.80),
                      control2: CGPoint(x: x - spec.sway * 0.5, y: low + (high - low) * 0.92))

        let rise = CAKeyframeAnimation(keyPath: "position")
        rise.path = path
        rise.duration = spec.riseSeconds
        rise.repeatCount = .infinity
        rise.calculationMode = .paced
        rise.timingFunction = CAMediaTimingFunction(name: .linear)
        rise.beginTime = now - spec.phase * spec.riseSeconds
        mark.container.add(rise, forKey: "rise")

        // Decode the 8×3 atlas at 7.5–11 fps (depending on mark size). The
        // previous long cycle exposed individual frames as steps; one complete
        // turn now uses every rendered angle over roughly 2–3 seconds.
        var rects: [NSValue] = []
        for raw in 0..<24 {
            let frame = spec.reverse ? 23 - raw : raw
            let column = frame % 8
            let row = frame / 8
            rects.append(NSValue(rect: CGRect(x: CGFloat(column) / 8,
                                              y: CGFloat(2 - row) / 3,
                                              width: 1.0 / 8, height: 1.0 / 3)))
        }
        rects.append(rects[0])
        for (index, sprite) in mark.sprites.enumerated() {
            let frames = CAKeyframeAnimation(keyPath: "contentsRect")
            frames.values = rects
            frames.keyTimes = (0..<rects.count).map { NSNumber(value: Double($0) / Double(rects.count - 1)) }
            frames.duration = spec.spinSeconds
            frames.repeatCount = .infinity
            frames.calculationMode = .discrete
            frames.beginTime = now - spec.phase * spec.spinSeconds + Double(index) * spec.spinSeconds / 48
            sprite.add(frames, forKey: "atlasRotation")

            // The second atlas layer is half a frame ahead and crossfades at
            // display rate, softening the remaining 24-frame quantization.
            let blend = CABasicAnimation(keyPath: "opacity")
            blend.fromValue = index == 0 ? 0.82 : 0.18
            blend.toValue = index == 0 ? 0.18 : 0.82
            blend.duration = spec.spinSeconds / 24
            blend.autoreverses = true
            blend.repeatCount = .infinity
            blend.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
            blend.beginTime = frames.beginTime
            sprite.add(blend, forKey: "frameBlend")
        }
    }
}

private final class WallpaperDelegate: NSObject, NSApplicationDelegate {
    private var windows: [NSWindow] = []

    func applicationDidFinishLaunching(_ notification: Notification) {
        NSApp.setActivationPolicy(.prohibited)
        rebuildWindows()
        let center = NotificationCenter.default
        center.addObserver(self, selector: #selector(rebuildWindows),
                           name: NSApplication.didChangeScreenParametersNotification, object: nil)
        center.addObserver(self, selector: #selector(rebuildWindows),
                           name: NSWorkspace.screensDidWakeNotification, object: nil)
    }

    @objc private func rebuildWindows() {
        windows.forEach { $0.close() }
        windows = NSScreen.screens.map { screen in
            let window = NSWindow(contentRect: screen.frame, styleMask: .borderless,
                                  backing: .buffered, defer: false, screen: screen)
            window.level = NSWindow.Level(rawValue: Int(CGWindowLevelForKey(.desktopWindow)) + 1)
            window.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle]
            window.backgroundColor = .clear
            window.isOpaque = true
            window.hasShadow = false
            window.ignoresMouseEvents = true
            window.isReleasedWhenClosed = false
            window.contentView = PalsWallpaperView(frame: NSRect(origin: .zero, size: screen.frame.size))
            window.orderFrontRegardless()
            return window
        }
    }
}

private let application = NSApplication.shared
private let delegate = WallpaperDelegate()
application.delegate = delegate
application.run()
