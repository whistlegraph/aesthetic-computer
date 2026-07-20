import AppKit
import QuartzCore

/// A tiny, click-through fountain below JukeWizard's menu-bar CD. It borrows
/// MenuBand's purple/pink/chartreuse note language, but exists for only two
/// seconds and uses a single Core Animation emitter (no display-link work).
enum MenuBarNoteBurst {
    private static var windows: [NSWindow] = []
    private static var lastBurst = Date.distantPast

    static func emit(from button: NSStatusBarButton) {
        guard Date().timeIntervalSince(lastBurst) > 0.28,
              let hostWindow = button.window else { return }
        lastBurst = Date()
        let inWindow = button.convert(button.bounds, to: nil)
        let anchor = hostWindow.convertToScreen(inWindow)
        let size = NSSize(width: 170, height: 135)
        let frame = NSRect(x: anchor.midX - size.width / 2,
                           y: anchor.minY - size.height + 3,
                           width: size.width, height: size.height)
        let window = NSWindow(contentRect: frame, styleMask: .borderless,
                              backing: .buffered, defer: false)
        window.isOpaque = false
        window.backgroundColor = .clear
        window.hasShadow = false
        window.ignoresMouseEvents = true
        window.level = .statusBar
        window.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle]

        let view = NSView(frame: NSRect(origin: .zero, size: size))
        view.wantsLayer = true
        view.layer?.backgroundColor = NSColor.clear.cgColor
        window.contentView = view

        let emitter = CAEmitterLayer()
        emitter.frame = view.bounds
        emitter.emitterPosition = CGPoint(x: view.bounds.midX, y: view.bounds.maxY - 4)
        emitter.emitterShape = .point
        emitter.emitterMode = .points
        emitter.renderMode = .unordered
        let colors: [NSColor] = [
            NSColor(red: 167/255, green: 139/255, blue: 250/255, alpha: 1),
            NSColor(red: 255/255, green: 107/255, blue: 157/255, alpha: 1),
            NSColor(red: 158/255, green: 212/255, blue: 80/255, alpha: 1),
        ]
        emitter.emitterCells = colors.enumerated().map { index, color in
            let cell = CAEmitterCell()
            cell.contents = noteImage(color: color, glyphIndex: index)
            cell.birthRate = 7
            cell.lifetime = 1.7
            cell.lifetimeRange = 0.25
            cell.velocity = 42
            cell.velocityRange = 22
            cell.emissionLongitude = -.pi / 2
            cell.emissionRange = .pi / 3
            cell.yAcceleration = -54
            cell.xAcceleration = CGFloat(index - 1) * 5
            cell.scale = 0.72
            cell.scaleRange = 0.18
            cell.scaleSpeed = -0.08
            cell.alphaSpeed = -0.52
            cell.spin = 0.35
            cell.spinRange = 0.7
            return cell
        }
        view.layer?.addSublayer(emitter)
        windows.append(window)
        window.orderFrontRegardless()
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.055) { emitter.birthRate = 0 }
        DispatchQueue.main.asyncAfter(deadline: .now() + 2.15) {
            window.orderOut(nil)
            windows.removeAll { $0 === window }
        }
    }

    private static func noteImage(color: NSColor, glyphIndex: Int) -> CGImage? {
        let glyphs = ["♪", "♫", "♩"]
        let side: CGFloat = 24
        let image = NSImage(size: NSSize(width: side, height: side))
        image.lockFocus()
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.black.withAlphaComponent(0.72)
        shadow.shadowBlurRadius = 0
        shadow.shadowOffset = NSSize(width: 1.2, height: -1.2)
        shadow.set()
        let text = NSAttributedString(string: glyphs[glyphIndex % glyphs.count], attributes: [
            .font: NSFont.systemFont(ofSize: 16, weight: .bold),
            .foregroundColor: color,
        ])
        let textSize = text.size()
        text.draw(at: NSPoint(x: (side - textSize.width) / 2,
                              y: (side - textSize.height) / 2))
        image.unlockFocus()
        return image.cgImage(forProposedRect: nil, context: nil, hints: nil)
    }
}
