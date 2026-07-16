import AppKit
import QuartzCore

/// The visual punctuation on a zoom-lens acquisition: one crisp edge flash,
/// a hot bloom, and a brief spray of flame-like embers around the target.
///
/// This deliberately does not capture or redraw the window. It is a transparent,
/// click-through Core Animation layer that rides through the Window Server's
/// native zoom with the subject, so the lens keeps its low latency and never
/// asks for Screen Recording permission.
enum ZoomSpecialMove {
    private static var panel: NSPanel?
    private static var sequence = 0

    static func fire(around cgFrame: CGRect, on screen: NSScreen) {
        precondition(Thread.isMainThread)
        let window = panel ?? makePanel()
        panel = window
        window.setFrame(screen.frame, display: true)

        guard let root = window.contentView?.layer else { return }
        root.sublayers?.forEach { $0.removeFromSuperlayer() }

        let localFrame = appKitFrame(for: cgFrame)
            .offsetBy(dx: -screen.frame.minX, dy: -screen.frame.minY)
        let padding: CGFloat = 42
        let container = CALayer()
        container.frame = localFrame.insetBy(dx: -padding, dy: -padding)
        root.addSublayer(container)

        let subject = container.bounds.insetBy(dx: padding, dy: padding)
        let radius = min(18, min(subject.width, subject.height) * 0.06)
        let path = CGPath(roundedRect: subject, cornerWidth: radius,
                          cornerHeight: radius, transform: nil)

        // Wide hot bloom: the soft outer body of the move.
        let bloom = CAShapeLayer()
        bloom.path = path
        bloom.fillColor = NSColor.clear.cgColor
        bloom.strokeColor = NSColor(srgbRed: 1.0, green: 0.20, blue: 0.015,
                                    alpha: 0.92).cgColor
        bloom.lineWidth = 7
        bloom.shadowColor = NSColor(srgbRed: 1.0, green: 0.08, blue: 0.0,
                                    alpha: 1).cgColor
        bloom.shadowOpacity = 0.95
        bloom.shadowRadius = 28
        bloom.shadowOffset = .zero
        container.addSublayer(bloom)

        // A one-pixel pale rim supplies the "sharpen" sensation without
        // replacing the actual window pixels with a captured texture.
        let edge = CAShapeLayer()
        edge.path = path
        edge.fillColor = NSColor.clear.cgColor
        edge.strokeColor = NSColor(srgbRed: 1.0, green: 0.92, blue: 0.60,
                                   alpha: 1).cgColor
        edge.lineWidth = 1.5
        edge.shadowColor = NSColor.white.cgColor
        edge.shadowOpacity = 0.9
        edge.shadowRadius = 4
        edge.shadowOffset = .zero
        container.addSublayer(edge)

        let emitter = CAEmitterLayer()
        emitter.frame = container.bounds
        emitter.emitterPosition = CGPoint(x: subject.midX, y: subject.midY)
        emitter.emitterSize = subject.size
        emitter.emitterShape = .rectangle
        emitter.emitterMode = .outline
        emitter.renderMode = .additive
        emitter.emitterCells = [flameCell(), emberCell()]
        container.addSublayer(emitter)

        window.alphaValue = 1
        window.orderFrontRegardless()
        animate(layer: bloom, peak: 1.0, duration: 0.72)
        animate(layer: edge, peak: 1.0, duration: 0.48)

        // Emit hard for a few frames, then leave only the particles already in
        // flight. A sequence token prevents an older burst hiding a newer one.
        sequence += 1
        let mySequence = sequence
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.18) {
            emitter.birthRate = 0
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.95) {
            guard sequence == mySequence else { return }
            window.orderOut(nil)
            root.sublayers?.forEach { $0.removeFromSuperlayer() }
        }
    }

    private static func animate(layer: CALayer, peak: Float, duration: CFTimeInterval) {
        let animation = CAKeyframeAnimation(keyPath: "opacity")
        animation.values = [0, peak, peak * 0.72, 0]
        animation.keyTimes = [0, 0.08, 0.28, 1]
        animation.timingFunctions = [
            CAMediaTimingFunction(name: .easeOut),
            CAMediaTimingFunction(name: .linear),
            CAMediaTimingFunction(name: .easeOut),
        ]
        animation.duration = duration
        layer.opacity = 0
        layer.add(animation, forKey: "special-move")
    }

    private static func flameCell() -> CAEmitterCell {
        let cell = CAEmitterCell()
        cell.contents = flameParticle
        cell.birthRate = 92
        cell.lifetime = 0.62
        cell.lifetimeRange = 0.22
        cell.velocity = 92
        cell.velocityRange = 44
        cell.emissionLongitude = .pi / 2
        cell.emissionRange = .pi / 7
        cell.scale = 0.17
        cell.scaleRange = 0.08
        cell.scaleSpeed = -0.10
        cell.alphaSpeed = -1.18
        cell.spinRange = 0.8
        return cell
    }

    private static func emberCell() -> CAEmitterCell {
        let cell = CAEmitterCell()
        cell.contents = emberParticle
        cell.birthRate = 48
        cell.lifetime = 0.82
        cell.lifetimeRange = 0.32
        cell.velocity = 126
        cell.velocityRange = 72
        cell.emissionLongitude = .pi / 2
        cell.emissionRange = .pi / 3
        cell.scale = 0.075
        cell.scaleRange = 0.045
        cell.scaleSpeed = -0.035
        cell.alphaSpeed = -0.92
        cell.spin = 1.1
        cell.spinRange = 2.4
        return cell
    }

    private static let flameParticle: CGImage? = particleImage(
        size: CGSize(width: 28, height: 44),
        inner: NSColor(srgbRed: 1.0, green: 0.98, blue: 0.62, alpha: 0.96),
        middle: NSColor(srgbRed: 1.0, green: 0.25, blue: 0.015, alpha: 0.72))

    private static let emberParticle: CGImage? = particleImage(
        size: CGSize(width: 16, height: 16),
        inner: NSColor(srgbRed: 1.0, green: 1.0, blue: 0.78, alpha: 1),
        middle: NSColor(srgbRed: 1.0, green: 0.12, blue: 0.0, alpha: 0.82))

    private static func particleImage(size: CGSize, inner: NSColor,
                                      middle: NSColor) -> CGImage? {
        let width = Int(size.width)
        let height = Int(size.height)
        guard let context = CGContext(
            data: nil, width: width, height: height, bitsPerComponent: 8,
            bytesPerRow: width * 4, space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue),
              let gradient = CGGradient(
                colorsSpace: CGColorSpaceCreateDeviceRGB(),
                colors: [inner.cgColor, middle.cgColor, NSColor.clear.cgColor] as CFArray,
                locations: [0, 0.34, 1]) else { return nil }
        let centre = CGPoint(x: size.width / 2, y: size.height * 0.42)
        context.scaleBy(x: 1, y: size.height / size.width)
        context.drawRadialGradient(
            gradient,
            startCenter: CGPoint(x: centre.x, y: centre.y * size.width / size.height),
            startRadius: 0,
            endCenter: CGPoint(x: centre.x, y: centre.y * size.width / size.height),
            endRadius: size.width / 2,
            options: [.drawsAfterEndLocation])
        return context.makeImage()
    }

    private static func appKitFrame(for cgFrame: CGRect) -> CGRect {
        let desktopTop = NSScreen.screens.map(\.frame.maxY).max() ?? 0
        return CGRect(x: cgFrame.minX, y: desktopTop - cgFrame.maxY,
                      width: cgFrame.width, height: cgFrame.height)
    }

    private static func makePanel() -> NSPanel {
        let window = NSPanel(
            contentRect: .zero,
            styleMask: [.borderless, .nonactivatingPanel],
            backing: .buffered,
            defer: false)
        window.isOpaque = false
        window.backgroundColor = .clear
        window.hasShadow = false
        window.level = .screenSaver
        window.ignoresMouseEvents = true
        window.hidesOnDeactivate = false
        window.collectionBehavior = [.canJoinAllSpaces, .stationary,
                                     .ignoresCycle, .fullScreenAuxiliary]
        let view = NSView()
        view.wantsLayer = true
        window.contentView = view
        return window
    }
}
