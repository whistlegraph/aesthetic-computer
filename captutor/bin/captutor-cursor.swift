import AppKit
import CoreGraphics
import Foundation

// Captutor's filmed pointer. Browser input remains in CDP; this process only
// paints a native, click-through overlay at the same global screen coordinate.

private struct Command: Decodable {
    let op: String
    let x: CGFloat?
    let y: CGFloat?
    let durationMs: Double?
}

private struct Particle {
    var position: CGPoint
    var velocity: CGVector
    var bornAt: TimeInterval
    var lifetime: TimeInterval
    var radius: CGFloat
    var color: NSColor
}

private final class CursorView: NSView {
    var pointer = CGPoint.zero
    var isPointerVisible = false
    var isPressed = false
    var particles: [Particle] = []

    override var isFlipped: Bool { true }
    override var isOpaque: Bool { false }

    override func draw(_ dirtyRect: NSRect) {
        guard let context = NSGraphicsContext.current?.cgContext else { return }
        context.clear(bounds)

        let now = ProcessInfo.processInfo.systemUptime
        for particle in particles {
            let age = max(0, now - particle.bornAt)
            let progress = min(1, age / particle.lifetime)
            let alpha = CGFloat(pow(1 - progress, 1.65))
            let center = CGPoint(
                x: particle.position.x + particle.velocity.dx * age,
                y: particle.position.y + particle.velocity.dy * age + CGFloat(age * age) * 22
            )
            context.setFillColor(particle.color.withAlphaComponent(alpha * 0.82).cgColor)
            context.addEllipse(in: CGRect(
                x: center.x - particle.radius,
                y: center.y - particle.radius,
                width: particle.radius * 2,
                height: particle.radius * 2
            ))
            context.fillPath()
        }

        guard isPointerVisible else { return }

        // The path begins at (0, 0): pointer is the actual click hotspot, not
        // the centre of an oversized cursor box. Its proportions follow the
        // familiar macOS arrow while the dark face and light rim read cleanly
        // on both Fuser's pale canvas and its darker controls.
        context.saveGState()
        context.translateBy(x: pointer.x, y: pointer.y)
        let pressedScale: CGFloat = isPressed ? 0.92 : 1
        context.scaleBy(x: pressedScale, y: pressedScale)

        let path = CGMutablePath()
        path.move(to: CGPoint(x: 0, y: 0))
        path.addLine(to: CGPoint(x: 1.6, y: 22.8))
        path.addLine(to: CGPoint(x: 7.3, y: 17.1))
        path.addLine(to: CGPoint(x: 12.0, y: 27.0))
        path.addLine(to: CGPoint(x: 17.0, y: 24.6))
        path.addLine(to: CGPoint(x: 12.3, y: 15.0))
        path.addLine(to: CGPoint(x: 20.4, y: 14.2))
        path.closeSubpath()

        context.setShadow(offset: CGSize(width: 0.7, height: 1.4), blur: 2.4,
                          color: NSColor.black.withAlphaComponent(0.55).cgColor)
        context.setFillColor(NSColor(calibratedWhite: 0.055, alpha: 1).cgColor)
        context.addPath(path)
        context.fillPath()
        context.setShadow(offset: .zero, blur: 0, color: nil)
        context.setStrokeColor(NSColor(calibratedWhite: 0.98, alpha: 0.98).cgColor)
        context.setLineWidth(2.25)
        context.setLineJoin(.round)
        context.addPath(path)
        context.strokePath()

        // A restrained cool hairline gives the familiar pointer a little life
        // without turning it into an annotation or obscuring the target.
        context.setStrokeColor(NSColor(calibratedRed: 0.42, green: 0.78, blue: 1, alpha: 0.72).cgColor)
        context.setLineWidth(0.62)
        context.addPath(path)
        context.strokePath()
        context.restoreGState()
    }
}

private struct DisplaySurface {
    let displayID: CGDirectDisplayID
    let quartzFrame: CGRect
    let window: NSWindow
    let view: CursorView
}

private final class CursorController {
    private var surfaces: [DisplaySurface] = []
    private var activeSurface: Int?
    private var currentGlobal: CGPoint?
    private var moveStartGlobal = CGPoint.zero
    private var moveTargetGlobal = CGPoint.zero
    private var moveStartedAt: TimeInterval = 0
    private var moveDuration: TimeInterval = 0
    private var lastTrailGlobal: CGPoint?
    private var timer: Timer?

    init() {
        rebuildSurfaces()
        timer = Timer(timeInterval: 1.0 / 120.0, repeats: true) { [weak self] _ in
            self?.tick()
        }
        RunLoop.main.add(timer!, forMode: .common)
    }

    deinit { timer?.invalidate() }

    private func rebuildSurfaces() {
        for screen in NSScreen.screens {
            guard let number = screen.deviceDescription[NSDeviceDescriptionKey("NSScreenNumber")] as? NSNumber else {
                continue
            }
            let displayID = CGDirectDisplayID(number.uint32Value)
            let window = NSWindow(
                contentRect: screen.frame,
                styleMask: .borderless,
                backing: .buffered,
                defer: false,
                screen: screen
            )
            let view = CursorView(frame: CGRect(origin: .zero, size: screen.frame.size))
            window.contentView = view
            window.backgroundColor = .clear
            window.isOpaque = false
            window.hasShadow = false
            window.ignoresMouseEvents = true
            // Borderless accessory windows otherwise default to sharingState
            // 0 and disappear from ScreenCaptureKit. The cursor is presentation
            // UI, so make its pixels explicitly capture-visible while keeping
            // the surface click-through and absent from window cycling.
            window.sharingType = .readOnly
            window.level = NSWindow.Level(rawValue: Int(CGWindowLevelForKey(.statusWindow)) + 1)
            window.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle, .fullScreenAuxiliary]
            window.isReleasedWhenClosed = false
            surfaces.append(DisplaySurface(
                displayID: displayID,
                quartzFrame: CGDisplayBounds(displayID),
                window: window,
                view: view
            ))
        }
    }

    private func surfaceIndex(containing point: CGPoint) -> Int? {
        surfaces.firstIndex(where: { $0.quartzFrame.contains(point) }) ?? surfaces.indices.first
    }

    private func localPoint(_ global: CGPoint, on surface: DisplaySurface) -> CGPoint {
        // CGDisplayBounds and the command protocol both use a top-left origin.
        // CursorView is flipped, so subtracting the display origin preserves the
        // exact coordinate, including on a secondary display.
        CGPoint(x: global.x - surface.quartzFrame.minX,
                y: global.y - surface.quartzFrame.minY)
    }

    private func setGlobalPoint(_ global: CGPoint, emitTrail: Bool) {
        guard let index = surfaceIndex(containing: global) else { return }
        if activeSurface != index {
            if let old = activeSurface {
                surfaces[old].view.isPointerVisible = false
                surfaces[old].view.needsDisplay = true
                surfaces[old].window.orderOut(nil)
            }
            activeSurface = index
            surfaces[index].window.orderFrontRegardless()
        }

        let surface = surfaces[index]
        let local = localPoint(global, on: surface)
        surface.view.pointer = local
        surface.view.isPointerVisible = true
        currentGlobal = global

        if emitTrail {
            let distance = lastTrailGlobal.map { hypot(global.x - $0.x, global.y - $0.y) } ?? 100
            if distance >= 13 {
                emitParticle(at: local, in: surface.view, burst: false)
                lastTrailGlobal = global
            }
        }
        surface.view.needsDisplay = true
    }

    func move(to target: CGPoint, durationMs: Double) {
        let fallback = surfaces.first.map {
            CGPoint(x: $0.quartzFrame.midX, y: $0.quartzFrame.midY)
        } ?? target
        moveStartGlobal = currentGlobal ?? fallback
        moveTargetGlobal = target
        moveStartedAt = ProcessInfo.processInfo.systemUptime
        moveDuration = max(0, durationMs / 1000)
        if currentGlobal == nil { setGlobalPoint(moveStartGlobal, emitTrail: false) }
        if moveDuration == 0 { setGlobalPoint(target, emitTrail: true) }
    }

    func setPressed(_ pressed: Bool) {
        guard let index = activeSurface else { return }
        surfaces[index].view.isPressed = pressed
        surfaces[index].view.needsDisplay = true
    }

    func click() {
        guard let index = activeSurface else { return }
        let surface = surfaces[index]
        for _ in 0..<9 { emitParticle(at: surface.view.pointer, in: surface.view, burst: true) }
        surface.view.needsDisplay = true
    }

    func hide() {
        for surface in surfaces {
            surface.view.isPointerVisible = false
            surface.view.particles.removeAll()
            surface.view.needsDisplay = true
            surface.window.orderOut(nil)
        }
        activeSurface = nil
        currentGlobal = nil
        lastTrailGlobal = nil
    }

    private func emitParticle(at point: CGPoint, in view: CursorView, burst: Bool) {
        let palette = [
            NSColor(calibratedRed: 0.43, green: 0.82, blue: 1.00, alpha: 1),
            NSColor(calibratedRed: 0.73, green: 0.61, blue: 1.00, alpha: 1),
            NSColor(calibratedRed: 1.00, green: 0.77, blue: 0.42, alpha: 1),
        ]
        let angle = CGFloat.random(in: burst ? 0...(2 * .pi) : (0.35 * .pi)...(0.78 * .pi))
        let speed = CGFloat.random(in: burst ? 24...68 : 8...22)
        let origin = CGPoint(
            x: point.x + CGFloat.random(in: burst ? -2...3 : 2...7),
            y: point.y + CGFloat.random(in: burst ? -2...3 : 8...18)
        )
        view.particles.append(Particle(
            position: origin,
            velocity: CGVector(dx: cos(angle) * speed, dy: sin(angle) * speed),
            bornAt: ProcessInfo.processInfo.systemUptime,
            lifetime: Double.random(in: burst ? 0.34...0.52 : 0.22...0.34),
            radius: CGFloat.random(in: burst ? 1.35...2.35 : 0.8...1.45),
            color: palette.randomElement()!
        ))
    }

    private func tick() {
        let now = ProcessInfo.processInfo.systemUptime
        if moveDuration > 0, now < moveStartedAt + moveDuration {
            let t = min(1, max(0, (now - moveStartedAt) / moveDuration))
            let eased = t < 0.5
                ? 4 * t * t * t
                : 1 - pow(-2 * t + 2, 3) / 2
            setGlobalPoint(CGPoint(
                x: moveStartGlobal.x + (moveTargetGlobal.x - moveStartGlobal.x) * eased,
                y: moveStartGlobal.y + (moveTargetGlobal.y - moveStartGlobal.y) * eased
            ), emitTrail: true)
        } else if moveDuration > 0 {
            moveDuration = 0
            setGlobalPoint(moveTargetGlobal, emitTrail: true)
        }

        for surface in surfaces {
            let oldCount = surface.view.particles.count
            surface.view.particles.removeAll { now - $0.bornAt >= $0.lifetime }
            if oldCount != surface.view.particles.count || !surface.view.particles.isEmpty {
                surface.view.needsDisplay = true
            }
        }
    }
}

private final class AppDelegate: NSObject, NSApplicationDelegate {
    private var controller: CursorController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        controller = CursorController()
        DispatchQueue.global(qos: .userInteractive).async { [weak self] in
            while let line = readLine() {
                guard let data = line.data(using: .utf8),
                      let command = try? JSONDecoder().decode(Command.self, from: data) else { continue }
                DispatchQueue.main.async { self?.handle(command) }
            }
            DispatchQueue.main.async {
                self?.controller?.hide()
                NSApp.terminate(nil)
            }
        }
        FileHandle.standardOutput.write(Data("ready\n".utf8))
    }

    private func handle(_ command: Command) {
        switch command.op {
        case "move":
            guard let x = command.x, let y = command.y else { return }
            controller?.move(to: CGPoint(x: x, y: y), durationMs: command.durationMs ?? 0)
        case "down": controller?.setPressed(true)
        case "up": controller?.setPressed(false)
        case "click": controller?.click()
        case "hide": controller?.hide()
        case "quit":
            controller?.hide()
            NSApp.terminate(nil)
        default: break
        }
    }
}

private let app = NSApplication.shared
app.setActivationPolicy(.accessory)
private let delegate = AppDelegate()
app.delegate = delegate
app.run()
