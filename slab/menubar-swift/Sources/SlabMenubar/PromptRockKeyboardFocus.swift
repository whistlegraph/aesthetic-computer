import AppKit
import ApplicationServices
import CoreGraphics

/// One visible prompt rock offered to the temporary global keyboard mode.
struct PromptRockKeyTarget {
    let sessionId: String
    let graphicRect: NSRect
    let accent: NSColor
    let foreground: NSColor
}

private struct PromptRockKeyAssignment {
    let target: PromptRockKeyTarget
    let label: String
    let keyCode: UInt16
}

/// Full-screen, non-activating visual focus layer. The desktop stays visible
/// beneath a quiet tint while each rock carries one oversized bubble letter.
private final class PromptRockFocusView: NSView {
    private let assignments: [PromptRockKeyAssignment]
    private let screenOrigin: NSPoint
    private var glyphLayers: [String: RockCharLayer] = [:]
    var entryFlash = true
    var exitFlash = false

    init(frame frameRect: NSRect, screenOrigin: NSPoint,
         assignments: [PromptRockKeyAssignment]) {
        self.assignments = assignments
        self.screenOrigin = screenOrigin
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.masksToBounds = false
        rebuildGlyphs()
    }

    required init?(coder: NSCoder) { fatalError("init(coder:) has not been implemented") }

    override var isOpaque: Bool { false }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        let wash: NSColor
        if exitFlash {
            wash = NSColor(srgbRed: 1.0, green: 0.10, blue: 0.15, alpha: 0.30)
        } else if entryFlash {
            wash = NSColor(srgbRed: 0.05, green: 0.40, blue: 1.0, alpha: 0.42)
        } else {
            wash = NSColor(deviceWhite: 0.02, alpha: 0.18)
        }
        wash.setFill()
        bounds.fill()
    }

    func settleEntryFlash() {
        entryFlash = false
        needsDisplay = true
    }

    func beginExitFlash() {
        exitFlash = true
        needsDisplay = true
    }

    func strike(sessionId: String) {
        guard let assignment = assignments.first(where: { $0.target.sessionId == sessionId }),
              let glyph = glyphLayers[sessionId] else { return }
        let bounce = CAKeyframeAnimation(keyPath: "transform.scale")
        bounce.values = [1.0, 1.58, 0.82, 1.10, 1.0]
        bounce.keyTimes = [0, 0.24, 0.52, 0.78, 1]
        bounce.duration = 0.34
        bounce.timingFunctions = Array(
            repeating: CAMediaTimingFunction(name: .easeInEaseOut), count: 4)
        glyph.add(bounce, forKey: "keyboardStrike")
        bleedParticles(from: glyph.position, color: assignment.target.accent)
    }

    /// Match the pet names exactly: Comic Sans/Chalkboard bubble fill, black
    /// outline, hard status-colour shadow, and independent organic motion.
    private func rebuildGlyphs() {
        glyphLayers.values.forEach { $0.removeFromSuperlayer() }
        glyphLayers.removeAll()
        guard let root = layer else { return }
        let font = playfulRockFont(40)
        let inset: CGFloat = 14
        let scale = NSScreen.main?.backingScaleFactor ?? 2
        let now = CACurrentMediaTime()

        for assignment in assignments {
            let shadow = NSShadow()
            shadow.shadowColor = assignment.target.accent
            shadow.shadowBlurRadius = 0
            shadow.shadowOffset = NSSize(width: 3, height: -3)
            let attr = NSAttributedString(
                string: assignment.label.uppercased(),
                attributes: [
                    .font: font,
                    .foregroundColor: assignment.target.foreground,
                    .strokeColor: NSColor(white: 0.08, alpha: 1),
                    .strokeWidth: -3.5,
                    .shadow: shadow,
                ])
            let measured = attr.size()
            let glyph = RockCharLayer()
            glyph.attr = attr
            glyph.inset = inset
            glyph.contentsScale = scale
            glyph.bounds = CGRect(x: 0, y: 0,
                                  width: measured.width + inset * 2,
                                  height: measured.height + inset * 2)
            glyph.position = CGPoint(
                x: assignment.target.graphicRect.midX - screenOrigin.x,
                y: assignment.target.graphicRect.midY - screenOrigin.y)
            glyph.anchorPoint = CGPoint(x: 0.5, y: 0.5)

            var seed = UInt32(truncatingIfNeeded:
                assignment.target.sessionId.utf8.reduce(2_166_136_261) {
                    ($0 ^ UInt32($1)) &* 16_777_619
                })
            seed ^= UInt32(assignment.keyCode) &* 2_654_435_761
            let phase = Double(seed % 1000) / 1000
            let tilt = (CGFloat(Int((seed >> 9) % 15)) - 7) * .pi / 180
            glyph.transform = CATransform3DMakeRotation(tilt, 0, 0, 1)
            root.addSublayer(glyph)
            glyphLayers[assignment.target.sessionId] = glyph
            glyph.setNeedsDisplay()

            let wave = CAKeyframeAnimation(keyPath: "transform.translation.y")
            wave.values = [0, 5.5, -4.0, 3.0, 0]
            wave.keyTimes = [0, 0.22, 0.52, 0.78, 1]
            wave.duration = 1.25 + phase * 0.42
            wave.beginTime = now - phase * wave.duration
            wave.repeatCount = .infinity
            wave.isAdditive = true
            glyph.add(wave, forKey: "focusWave")

            let wobble = CAKeyframeAnimation(keyPath: "transform.rotation.z")
            wobble.values = [0, 0.11, -0.09, 0.06, 0]
            wobble.keyTimes = [0, 0.25, 0.55, 0.8, 1]
            wobble.duration = 1.55 + phase * 0.55
            wobble.beginTime = now - (1 - phase) * wobble.duration
            wobble.repeatCount = .infinity
            wobble.isAdditive = true
            glyph.add(wobble, forKey: "focusWobble")
        }
    }

    private func bleedParticles(from origin: CGPoint, color: NSColor) {
        guard let root = layer else { return }
        let emitter = CAEmitterLayer()
        emitter.frame = bounds
        emitter.emitterShape = .point
        emitter.emitterMode = .points
        emitter.emitterPosition = origin
        emitter.renderMode = .additive

        let cell = CAEmitterCell()
        cell.contents = Self.particleImage
        cell.color = color.cgColor
        cell.redRange = 0.25
        cell.greenRange = 0.25
        cell.blueRange = 0.25
        cell.birthRate = 210
        cell.lifetime = 0.78
        cell.lifetimeRange = 0.25
        cell.velocity = 64
        cell.velocityRange = 34
        cell.emissionRange = .pi * 2
        cell.yAcceleration = -72
        cell.scale = 0.24
        cell.scaleRange = 0.12
        cell.scaleSpeed = -0.16
        cell.alphaSpeed = -1.15
        cell.spinRange = .pi * 2
        emitter.emitterCells = [cell]
        root.addSublayer(emitter)

        DispatchQueue.main.asyncAfter(deadline: .now() + 0.075) {
            emitter.birthRate = 0
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.25) {
            emitter.removeFromSuperlayer()
        }
    }

    private static let particleImage: CGImage? = {
        let side = 22
        guard let context = CGContext(
            data: nil, width: side, height: side,
            bitsPerComponent: 8, bytesPerRow: side * 4,
            space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue),
              let gradient = CGGradient(
                colorsSpace: CGColorSpaceCreateDeviceRGB(),
                colors: [NSColor.white.cgColor,
                         NSColor.white.withAlphaComponent(0.72).cgColor,
                         NSColor.clear.cgColor] as CFArray,
                locations: [0, 0.25, 1]) else { return nil }
        let center = CGPoint(x: side / 2, y: side / 2)
        context.drawRadialGradient(
            gradient, startCenter: center, startRadius: 0,
            endCenter: center, endRadius: CGFloat(side) / 2,
            options: [.drawsAfterEndLocation])
        return context.makeImage()
    }()
}

private final class PromptRockFocusPanel: NSPanel {
    let focusView: PromptRockFocusView

    init(screen: NSScreen, assignments: [PromptRockKeyAssignment]) {
        focusView = PromptRockFocusView(
            frame: NSRect(origin: .zero, size: screen.frame.size),
            screenOrigin: screen.frame.origin,
            assignments: assignments)
        super.init(contentRect: screen.frame,
                   styleMask: [.borderless, .nonactivatingPanel],
                   backing: .buffered, defer: false)
        isOpaque = false
        backgroundColor = .clear
        hasShadow = false
        level = NSWindow.Level(rawValue: NSWindow.Level.screenSaver.rawValue + 1)
        ignoresMouseEvents = true
        hidesOnDeactivate = false
        isReleasedWhenClosed = false
        sharingType = .readOnly
        collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary,
                              .stationary, .ignoresCycle]
        contentView = focusView
    }
}

/// Low-latency global QWERTY capture. Assigned letters and Escape are sunk so
/// the foreground application beneath Prompt Rock focus never receives them.
private final class PromptRockKeyEventTap {
    typealias Handler = (_ keyCode: UInt16, _ isDown: Bool, _ isRepeat: Bool) -> Bool

    private let handler: Handler
    private var tap: CFMachPort?
    private var source: CFRunLoopSource?
    private var thread: Thread?
    private var threadRunLoop: CFRunLoop?

    init(handler: @escaping Handler) {
        self.handler = handler
    }

    @discardableResult
    func start() -> Bool {
        guard tap == nil else { return true }
        guard ProcessInfo.processInfo.environment["SLAB_DISABLE_EVENT_TAPS"] != "1",
              AXIsProcessTrusted() else {
            NSLog("slab prompt rocks: keyboard focus needs Accessibility trust")
            return false
        }

        let mask: CGEventMask =
            (1 << CGEventType.keyDown.rawValue) |
            (1 << CGEventType.keyUp.rawValue) |
            (1 << CGEventType.tapDisabledByTimeout.rawValue) |
            (1 << CGEventType.tapDisabledByUserInput.rawValue)
        let callback: CGEventTapCallBack = { _, type, event, refcon in
            guard let refcon else { return Unmanaged.passUnretained(event) }
            let owner = Unmanaged<PromptRockKeyEventTap>.fromOpaque(refcon)
                .takeUnretainedValue()
            if type == .tapDisabledByTimeout || type == .tapDisabledByUserInput {
                if let tap = owner.tap { CGEvent.tapEnable(tap: tap, enable: true) }
                return Unmanaged.passUnretained(event)
            }
            guard type == .keyDown || type == .keyUp else {
                return Unmanaged.passUnretained(event)
            }
            let keyCode = UInt16(event.getIntegerValueField(.keyboardEventKeycode))
            let repeatPress = event.getIntegerValueField(.keyboardEventAutorepeat) != 0
            return owner.handler(keyCode, type == .keyDown, repeatPress)
                ? nil
                : Unmanaged.passUnretained(event)
        }

        guard let port = CGEvent.tapCreate(
            tap: .cgSessionEventTap,
            place: .headInsertEventTap,
            options: .defaultTap,
            eventsOfInterest: mask,
            callback: callback,
            userInfo: Unmanaged.passUnretained(self).toOpaque()) else {
            NSLog("slab prompt rocks: keyboard focus event tap creation failed")
            return false
        }
        tap = port
        let runLoopSource = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, port, 0)
        source = runLoopSource
        let inputThread = Thread { [weak self] in
            guard let self, let source = self.source, let tap = self.tap else { return }
            self.threadRunLoop = CFRunLoopGetCurrent()
            CFRunLoopAddSource(CFRunLoopGetCurrent(), source, .commonModes)
            CGEvent.tapEnable(tap: tap, enable: true)
            CFRunLoopRun()
        }
        inputThread.name = "PromptRock-KeyFocus"
        inputThread.qualityOfService = .userInteractive
        thread = inputThread
        inputThread.start()
        return true
    }

    func stop() {
        if let tap { CGEvent.tapEnable(tap: tap, enable: false) }
        if let threadRunLoop { CFRunLoopStop(threadRunLoop) }
        tap = nil
        source = nil
        thread = nil
        threadRunLoop = nil
    }

    deinit { stop() }
}

/// ⌘⌥X enters a Prompt-Rock-only global keyboard focus. Each activation
/// reshuffles physical QWERTY letters; Escape is the sole release gesture.
final class PromptRockKeyboardFocus {
    private static let escapeKeyCode: UInt16 = 53
    private static let qwerty: [(String, UInt16)] = [
        ("q", 12), ("w", 13), ("e", 14), ("r", 15), ("t", 17),
        ("y", 16), ("u", 32), ("i", 34), ("o", 31), ("p", 35),
        ("a", 0),  ("s", 1),  ("d", 2),  ("f", 3),  ("g", 5),
        ("h", 4),  ("j", 38), ("k", 40), ("l", 37),
        ("z", 6),  ("x", 7),  ("c", 8),  ("v", 9),  ("b", 11),
        ("n", 45), ("m", 46),
    ]
    private static let qwertyCodes = Set(qwerty.map(\.1))

    private let onPlay: (String) -> Void
    private var assignmentsByKey: [UInt16: [PromptRockKeyAssignment]] = [:]
    private var panels: [PromptRockFocusPanel] = []
    private(set) var isActive = false
    private lazy var keyTap = PromptRockKeyEventTap { [weak self] keyCode, isDown, isRepeat in
        guard let self else { return false }
        if keyCode == Self.escapeKeyCode {
            if isDown && !isRepeat {
                DispatchQueue.main.async { [weak self] in self?.end() }
            }
            return true
        }
        guard Self.qwertyCodes.contains(keyCode) else { return false }
        if isDown && !isRepeat {
            DispatchQueue.main.async { [weak self] in self?.play(keyCode: keyCode) }
        }
        return true
    }

    init(onPlay: @escaping (String) -> Void) {
        self.onPlay = onPlay
    }

    @discardableResult
    func begin(targets: [PromptRockKeyTarget]) -> Bool {
        guard !targets.isEmpty else { return false }
        if isActive { end(animated: false) }

        let orderedTargets = targets.sorted { $0.sessionId < $1.sessionId }
        var keyCycle = Self.qwerty.shuffled()
        var assignments: [PromptRockKeyAssignment] = []
        for (index, target) in orderedTargets.enumerated() {
            if index > 0 && index % Self.qwerty.count == 0 {
                keyCycle = Self.qwerty.shuffled()
            }
            let key = keyCycle[index % Self.qwerty.count]
            assignments.append(PromptRockKeyAssignment(
                target: target, label: key.0, keyCode: key.1))
        }
        assignmentsByKey = Dictionary(grouping: assignments, by: \.keyCode)

        guard keyTap.start() else {
            assignmentsByKey.removeAll()
            return false
        }
        isActive = true
        panels = NSScreen.screens.map { screen in
            let local = assignments.filter { screen.frame.contains(
                NSPoint(x: $0.target.graphicRect.midX, y: $0.target.graphicRect.midY)) }
            return PromptRockFocusPanel(screen: screen, assignments: local)
        }
        for panel in panels {
            panel.alphaValue = 1
            panel.orderFrontRegardless()
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.075) { [weak self] in
            guard self?.isActive == true else { return }
            self?.panels.forEach { $0.focusView.settleEntryFlash() }
        }
        return true
    }

    func end(animated: Bool = true) {
        guard isActive else { return }
        NSLog("🪨 [keyboard-focus] release")
        isActive = false
        keyTap.stop()
        assignmentsByKey.removeAll()
        let exiting = panels
        panels.removeAll()
        if !animated {
            exiting.forEach { $0.orderOut(nil) }
            return
        }
        exiting.forEach { $0.focusView.beginExitFlash() }
        NSAnimationContext.runAnimationGroup({ context in
            context.duration = 0.16
            context.timingFunction = CAMediaTimingFunction(name: .easeOut)
            exiting.forEach { $0.animator().alphaValue = 0 }
        }, completionHandler: {
            exiting.forEach { $0.orderOut(nil) }
        })
    }

    private func play(keyCode: UInt16) {
        guard isActive, let matches = assignmentsByKey[keyCode] else { return }
        for assignment in matches {
            onPlay(assignment.target.sessionId)
            panels.forEach { $0.focusView.strike(sessionId: assignment.target.sessionId) }
        }
    }
}
