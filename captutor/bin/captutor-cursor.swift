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
    let width: CGFloat?
    let height: CGFloat?
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
    var pointerAlpha: CGFloat = 1
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

        // Use AppKit's own arrow artwork and hotspot instead of approximating
        // the silhouette. This keeps Captutor's filmed pointer at the exact
        // proportions of the macOS cursor while preserving our native particle
        // trail and capture-visible overlay surface.
        let arrow = NSCursor.arrow
        let image = arrow.image
        let hotspot = arrow.hotSpot
        image.draw(
            in: CGRect(
                origin: CGPoint(x: pointer.x - hotspot.x, y: pointer.y - hotspot.y),
                size: image.size
            ),
            from: .zero,
            operation: .sourceOver,
            fraction: pointerAlpha * (isPressed ? 0.88 : 1),
            respectFlipped: true,
            hints: [.interpolation: NSImageInterpolation.high]
        )
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
    private var dillydallyCenterGlobal: CGPoint?
    private var dillydallyStartedAt: TimeInterval = 0
    private var dillydallyDuration: TimeInterval = 0
    private var orbitCenterGlobal: CGPoint?
    private var orbitStartGlobal = CGPoint.zero
    private var orbitStartedAt: TimeInterval = 0
    private var orbitDuration: TimeInterval = 0
    private var orbitRadius = CGSize.zero
    private var lastIntentAt: TimeInterval = 0
    private var idleBurstEmitted = false
    private var lastTrailGlobal: CGPoint?
    private var timer: Timer?
    private var systemCursorHidden = false
    private var originalSystemCursorPoint: CGPoint?

    init() {
        rebuildSurfaces()
        hideSystemCursor()
        timer = Timer(timeInterval: 1.0 / 120.0, repeats: true) { [weak self] _ in
            self?.tick()
        }
        RunLoop.main.add(timer!, forMode: .common)
    }

    deinit {
        timer?.invalidate()
        restoreSystemCursor()
    }

    private func hideSystemCursor() {
        guard !systemCursorHidden else { return }
        originalSystemCursorPoint = CGEvent(source: nil)?.location
        NSCursor.hide()
        CGAssociateMouseAndMouseCursorPosition(boolean_t(0))
        if let surface = surfaces.first {
            CGWarpMouseCursorPosition(CGPoint(
                x: surface.quartzFrame.maxX - 2,
                y: surface.quartzFrame.maxY - 2
            ))
        }
        systemCursorHidden = true
    }

    func restoreSystemCursor() {
        guard systemCursorHidden else { return }
        CGAssociateMouseAndMouseCursorPosition(boolean_t(1))
        if let point = originalSystemCursorPoint {
            CGWarpMouseCursorPosition(point)
        }
        NSCursor.unhide()
        originalSystemCursorPoint = nil
        systemCursorHidden = false
    }

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
        surface.view.pointerAlpha = 1
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
        settleDillydally()
        settleOrbit()
        let fallback = surfaces.first.map {
            CGPoint(x: $0.quartzFrame.midX, y: $0.quartzFrame.midY)
        } ?? target
        moveStartGlobal = currentGlobal ?? fallback
        moveTargetGlobal = target
        moveStartedAt = ProcessInfo.processInfo.systemUptime
        lastIntentAt = moveStartedAt
        idleBurstEmitted = false
        moveDuration = max(0, durationMs / 1000)
        if currentGlobal == nil { setGlobalPoint(moveStartGlobal, emitTrail: false) }
        if moveDuration == 0 { setGlobalPoint(target, emitTrail: true) }
    }

    func dillydally(at target: CGPoint, durationMs: Double) {
        settleOrbit()
        moveDuration = 0
        dillydallyCenterGlobal = target
        dillydallyStartedAt = ProcessInfo.processInfo.systemUptime
        dillydallyDuration = max(0, durationMs / 1000)
        lastIntentAt = dillydallyStartedAt
        idleBurstEmitted = false
        setGlobalPoint(target, emitTrail: false)
    }

    func settleDillydally() {
        guard let center = dillydallyCenterGlobal else { return }
        dillydallyCenterGlobal = nil
        dillydallyDuration = 0
        lastIntentAt = ProcessInfo.processInfo.systemUptime
        setGlobalPoint(center, emitTrail: false)
    }

    func orbit(around center: CGPoint, width: CGFloat, height: CGFloat, durationMs: Double) {
        settleDillydally()
        moveDuration = 0
        orbitCenterGlobal = center
        orbitStartGlobal = currentGlobal ?? center
        orbitStartedAt = ProcessInfo.processInfo.systemUptime
        orbitDuration = max(0.4, durationMs / 1000)
        orbitRadius = CGSize(
            width: min(76, max(13, width / 2 + 8)),
            height: min(52, max(11, height / 2 + 8))
        )
        lastIntentAt = orbitStartedAt
        idleBurstEmitted = false
    }

    func settleOrbit() {
        guard orbitCenterGlobal != nil else { return }
        orbitCenterGlobal = nil
        orbitDuration = 0
        lastIntentAt = ProcessInfo.processInfo.systemUptime
    }

    func setPressed(_ pressed: Bool) {
        guard let index = activeSurface else { return }
        surfaces[index].view.isPressed = pressed
        surfaces[index].view.needsDisplay = true
    }

    func click() {
        guard let index = activeSurface else { return }
        lastIntentAt = ProcessInfo.processInfo.systemUptime
        idleBurstEmitted = false
        surfaces[index].view.needsDisplay = true
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
        dillydallyCenterGlobal = nil
        dillydallyDuration = 0
        orbitCenterGlobal = nil
        orbitDuration = 0
        idleBurstEmitted = false
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
        if let center = orbitCenterGlobal {
            let elapsed = now - orbitStartedAt
            let t = min(1, max(0, elapsed / orbitDuration))
            // Ease from the click point onto the presentation ring, make one
            // measured clockwise lap around the clicked control's full bounds,
            // then settle back where the trusted click actually landed.
            let ringStart = CGPoint(x:center.x, y:center.y - orbitRadius.height)
            let point: CGPoint
            if t < 0.16 {
                let k = t / 0.16
                point = CGPoint(
                    x:orbitStartGlobal.x + (ringStart.x - orbitStartGlobal.x) * k,
                    y:orbitStartGlobal.y + (ringStart.y - orbitStartGlobal.y) * k
                )
            } else if t < 0.88 {
                let angle = -Double.pi / 2 + ((t - 0.16) / 0.72) * Double.pi * 2
                point = CGPoint(x:center.x + cos(angle) * orbitRadius.width,
                                y:center.y + sin(angle) * orbitRadius.height)
            } else {
                let k = (t - 0.88) / 0.12
                point = CGPoint(
                    x:ringStart.x + (orbitStartGlobal.x - ringStart.x) * k,
                    y:ringStart.y + (orbitStartGlobal.y - ringStart.y) * k
                )
            }
            setGlobalPoint(point, emitTrail:false)
            if t >= 1 {
                orbitCenterGlobal = nil
                orbitDuration = 0
                lastIntentAt = now
                setGlobalPoint(orbitStartGlobal, emitTrail:false)
            }
        } else if let center = dillydallyCenterGlobal {
            let elapsed = now - dillydallyStartedAt
            if dillydallyDuration > 0 && elapsed >= dillydallyDuration {
                settleDillydally()
            } else {
                // An asymmetric figure-eight keeps the cursor alive over a
                // blue in-progress control without implying another click.
                let phase = elapsed * 2 * Double.pi * 3.2
                setGlobalPoint(CGPoint(
                    x: center.x + sin(phase) * 3.8 + sin(phase * 0.43) * 1.2,
                    y: center.y + cos(phase * 1.7) * 1.8
                ), emitTrail: false)
            }
        } else if moveDuration > 0, now < moveStartedAt + moveDuration {
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
            lastIntentAt = now
            setGlobalPoint(moveTargetGlobal, emitTrail: true)
        }

        if dillydallyCenterGlobal == nil && moveDuration == 0,
           let index = activeSurface {
            let idleFor = now - lastIntentAt
            let fade = max(0, min(1, (idleFor - 1.6) / 0.5))
            let surface = surfaces[index]
            if fade > 0 && !idleBurstEmitted {
                idleBurstEmitted = true
                for _ in 0..<6 {
                    emitParticle(at: surface.view.pointer, in: surface.view, burst: true)
                }
            }
            surface.view.pointerAlpha = CGFloat(1 - fade)
            surface.view.isPointerVisible = fade < 1
            if fade > 0 { surface.view.needsDisplay = true }
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

    func applicationWillTerminate(_ notification: Notification) {
        controller?.restoreSystemCursor()
    }

    private func handle(_ command: Command) {
        switch command.op {
        case "move":
            guard let x = command.x, let y = command.y else { return }
            controller?.move(to: CGPoint(x: x, y: y), durationMs: command.durationMs ?? 0)
        case "dillydally":
            guard let x = command.x, let y = command.y else { return }
            controller?.dillydally(
                at: CGPoint(x: x, y: y),
                durationMs: command.durationMs ?? 0
            )
        case "orbit":
            guard let x = command.x, let y = command.y,
                  let width = command.width, let height = command.height else { return }
            controller?.orbit(around: CGPoint(x:x, y:y), width:width, height:height,
                              durationMs:command.durationMs ?? 1150)
        case "settle": controller?.settleDillydally()
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
