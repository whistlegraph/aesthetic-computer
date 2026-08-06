import AppKit
import AVFoundation
import QuartzCore

final class TracktrampAppDelegate: NSObject, NSApplicationDelegate, NSWindowDelegate {
    private let performer = TracktrampPerformer()
    private let surface = TracktrampView(frame: NSRect(x: 0, y: 0, width: 660, height: 510))
    private var window: NSWindow?
    private var keyMonitor: Any?
    private var instrumentStarted = false
    private var isQuitting = false
    private var lastTrackpadDeviceSize: NSSize?
    private var systemColorsObserver: NSObjectProtocol?
    private var screenParametersObserver: NSObjectProtocol?

    func applicationWillFinishLaunching(_ notification: Notification) {
        // A Dock launch activates TrackDrum before its window is ready. Hide
        // the pointer at that boundary so it never flashes over the first
        // instrument frame; TracktrampView balances this on resign and quit.
        surface.syncPointerVisibility(applicationActive: true)
    }

    func applicationDidFinishLaunching(_ notification: Notification) {
        systemColorsObserver = NotificationCenter.default.addObserver(
            forName: NSColor.systemColorsDidChangeNotification,
            object: nil,
            queue: .main
        ) { [weak self] _ in
            self?.applyDynamicIcon()
        }
        let window = NSWindow(
            contentRect: surface.bounds,
            styleMask: [.borderless],
            backing: .buffered,
            defer: false
        )
        window.title = "TrackDrum"
        window.isOpaque = false
        window.backgroundColor = .clear
        window.hasShadow = false
        // Stay in ordinary app-window space. Position keeps the surface above
        // the Dock; z-order must never cover or compete with the Dock itself.
        window.level = .normal
        window.isReleasedWhenClosed = false
        window.contentView = surface
        window.delegate = self
        self.window = window
        applyPhysicalCalibration(to: window, screen: pointerScreen())
        screenParametersObserver = NotificationCenter.default.addObserver(
            forName: NSApplication.didChangeScreenParametersNotification,
            object: nil,
            queue: .main
        ) { [weak self, weak window] _ in
            guard let self, let window else { return }
            self.applyPhysicalCalibration(
                to: window,
                screen: window.screen ?? self.pointerScreen()
            )
        }

        surface.onDefocus = { [weak self] in self?.quitInstrument() }
        surface.showEscapeInstructionTemporarily()
        keyMonitor = NSEvent.addLocalMonitorForEvents(matching: .keyDown) {
            [weak self] event in
            guard event.keyCode == 53 else { return event }
            self?.quitInstrument()
            return nil
        }

        performer.onActivity = { [weak surface] touches, membrane in
            DispatchQueue.main.async {
                surface?.update(touches: touches, membrane: membrane)
            }
        }
        surface.onTrackpadFrame = { [weak performer] contacts, timestamp, callbackTime in
            performer?.receive(
                contacts: contacts,
                timestamp: timestamp,
                callbackTime: callbackTime
            )
        }
        surface.onTrackpadDeviceSize = { [weak self, weak window] size in
            guard let self, let window else { return }
            self.lastTrackpadDeviceSize = size
            self.applyPhysicalCalibration(to: window, deviceSize: size)
        }

        presentInstrumentWindow()
    }

    private func presentInstrumentWindow() {
        guard let window, !isQuitting else { return }
        applyPhysicalCalibration(to: window, screen: pointerScreen())
        // Commit a lightweight drum surface before initializing Metal, audio,
        // event taps, or the dynamic 1024 px Dock icon. A Dock click therefore
        // produces a visible window on the first AppKit presentation turn.
        window.makeKeyAndOrderFront(nil)
        window.makeFirstResponder(surface)
        NSApp.activate(ignoringOtherApps: true)
        surface.syncPointerVisibility(applicationActive: true)
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.01) { [weak self] in
            self?.startInstrumentAfterFirstFrame()
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.12) { [weak self] in
            self?.applyDynamicIcon()
        }
    }

    private func startInstrumentAfterFirstFrame() {
        guard !instrumentStarted, !isQuitting else { return }
        instrumentStarted = true
        surface.prepareRenderer()
        do {
            try performer.start()
            FocusFlashOverlay.shared.flash(rising: false)
            FocusCueBeep.shared.play(rising: true)
        } catch {
            presentStartError(error)
        }
    }

    func applicationWillTerminate(_ notification: Notification) {
        if let systemColorsObserver {
            NotificationCenter.default.removeObserver(systemColorsObserver)
            self.systemColorsObserver = nil
        }
        if let screenParametersObserver {
            NotificationCenter.default.removeObserver(screenParametersObserver)
            self.screenParametersObserver = nil
        }
        if let keyMonitor {
            NSEvent.removeMonitor(keyMonitor)
            self.keyMonitor = nil
        }
        surface.restorePointer()
        performer.stop()
    }

    func applicationDidBecomeActive(_ notification: Notification) {
        surface.syncPointerVisibility(applicationActive: window?.isVisible == true)
    }

    func applicationDidResignActive(_ notification: Notification) {
        surface.syncPointerVisibility(applicationActive: false)
        performer.releaseAllContacts()
    }

    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool {
        true
    }

    func applicationSupportsSecureRestorableState(_ app: NSApplication) -> Bool {
        true
    }

    func applicationShouldHandleReopen(_ sender: NSApplication,
                                       hasVisibleWindows flag: Bool) -> Bool {
        presentInstrumentWindow()
        return true
    }

    func windowDidChangeScreen(_ notification: Notification) {
        guard let window else { return }
        applyPhysicalCalibration(to: window, screen: window.screen ?? pointerScreen())
    }

    private func applyPhysicalCalibration(to window: NSWindow,
                                          screen: NSScreen? = nil,
                                          deviceSize: NSSize? = nil) {
        guard let screen = screen ?? window.screen ?? pointerScreen() else { return }
        let calibration = PhysicalTrackpadCalibration.current(
            deviceSize: deviceSize ?? lastTrackpadDeviceSize
        )
        surface.setTrackpadSize(calibration.surfacePoints)
        window.setContentSize(surface.preferredContentSize)
        let safeFrame = TrackDrumDockLayout.safeFrame(for: screen)
        window.setFrameOrigin(NSPoint(
            x: safeFrame.midX - window.frame.width / 2,
            y: safeFrame.minY + max(8, safeFrame.height * 0.02)
        ))
        NSLog(
            "TrackDrum 1:1: source=%@ surface=%.2fx%.2fpt",
            calibration.source,
            calibration.surfacePoints.width,
            calibration.surfacePoints.height
        )
    }

    private func applyDynamicIcon() {
        NSApp.applicationIconImage = TrackDrumIcon.image(
            accent: NSColor.controlAccentColor
        )
    }

    private func pointerScreen() -> NSScreen? {
        let pointer = NSEvent.mouseLocation
        return NSScreen.screens.first { NSMouseInRect(pointer, $0.frame, false) }
            ?? NSScreen.main
    }

    private func presentStartError(_ error: Error) {
        let alert = NSAlert()
        alert.messageText = "Audio unavailable"
        alert.informativeText = error.localizedDescription
        alert.alertStyle = .warning
        alert.addButton(withTitle: "OK")
        alert.beginSheetModal(for: window!)
    }

    private func quitInstrument() {
        guard !isQuitting else { return }
        isQuitting = true
        FocusFlashOverlay.shared.flash(rising: false)
        FocusCueBeep.shared.play(rising: false)
        surface.restorePointer()
        performer.stop()
        // Let the falling bell and red pulse finish before terminating. A Dock
        // click starts the same single, sandboxed app again; no helper process
        // or login item remains behind after Quit.
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.18) {
            NSApp.terminate(nil)
        }
    }
}

private enum TracktrampError: LocalizedError {
    case audio

    var errorDescription: String? {
        switch self {
        case .audio: return "Audio could not start."
        }
    }
}

private struct TracktrampEnergy {
    var point: CGPoint
    var level: Double
    var at: Double
}

private final class TracktrampPerformer {
    var onActivity: (([CGPoint], TrackpadMembraneSimulation.Snapshot) -> Void)?

    private let engine = AVAudioEngine()
    private let percussion = MenuBandPercussion()
    private let queue = DispatchQueue(label: "computer.aesthetic.tracktramp.performance", qos: .userInteractive)
    private var contactsByID: [Int32: CGPoint] = [:]
    private var energies: [TracktrampEnergy] = []
    private var previousTimestamp: Double = 0
    private var scratchFrames = 0
    private var scratchTravel: CGFloat = 0
    private var scratchSpeed: Double?
    private var membrane = TrackpadMembraneSimulation()
    private var visibleTouches: [CGPoint] = []
    private var displayTimer: DispatchSourceTimer?
    private var isRunning = false

    func start() throws {
        guard !isRunning else { return }
        percussion.attach(to: engine, output: engine.mainMixerNode)
        engine.mainMixerNode.outputVolume = 0.92
        engine.prepare()
        do {
            try engine.start()
        } catch {
            throw TracktrampError.audio
        }

        queue.sync {
            membrane.reset(at: CACurrentMediaTime())
            let timer = DispatchSource.makeTimerSource(queue: queue)
            timer.schedule(deadline: .now(), repeating: 1.0 / 60.0,
                           leeway: .microseconds(800))
            timer.setEventHandler { [weak self] in self?.presentFrame() }
            displayTimer = timer
            timer.resume()
        }
        isRunning = true
    }

    func stop() {
        guard isRunning else { return }
        isRunning = false
        queue.sync {
            displayTimer?.cancel()
            displayTimer = nil
            contactsByID.removeAll(keepingCapacity: true)
            visibleTouches.removeAll(keepingCapacity: true)
            energies.removeAll(keepingCapacity: true)
            previousTimestamp = 0
            resetScratch()
            percussion.silence()
        }
        engine.stop()
    }

    func receive(contacts: [TrackpadContact], timestamp: Double, callbackTime: Double) {
        guard isRunning else { return }
        queue.async { [weak self] in
            self?.process(contacts: contacts, timestamp: timestamp, callbackTime: callbackTime)
        }
    }

    func releaseAllContacts() {
        receive(
            contacts: [],
            timestamp: ProcessInfo.processInfo.systemUptime,
            callbackTime: CACurrentMediaTime()
        )
    }

    private func process(contacts: [TrackpadContact], timestamp: Double, callbackTime: Double) {
        let changes = TrackpadContactChanges.resolve(previous: contactsByID, contacts: contacts)
        let previous = contactsByID
        contactsByID = changes.activeByID
        let touches = changes.active.map(\.point)
        visibleTouches = touches
        let now = CACurrentMediaTime()
        membrane.advance(to: now, touches: touches)
        decayEnergies(now: now)

        if !changes.began.isEmpty || !changes.lifted.isEmpty {
            percussion.markTrackpadInput(at: callbackTime)
        }

        for lift in changes.lifted {
            let velocity = UInt8(max(36, Int(Double(MenuBandPercussion.surfaceVelocity(
                at: lift,
                anchors: touches,
                inertia: energy(at: lift)
            )) * 0.62)))
            percussion.playSurfaceLift(at: lift, anchors: touches, velocity: velocity, synthetic: false)
        }

        for strike in changes.began {
            let anchors = touches.filter { $0 != strike }
            let velocity = MenuBandPercussion.surfaceVelocity(
                at: strike,
                anchors: anchors,
                inertia: energy(at: strike)
            )
            percussion.playDrumSkin(strike: strike, anchors: anchors, velocity: velocity)
            membrane.impulse(
                at: strike,
                amount: 0.28 + Double(velocity) / 127.0 * 0.54
            )
            energies.append(TracktrampEnergy(
                point: strike,
                level: 0.48 + Double(velocity) / 127.0 * 0.42,
                at: now
            ))
        }

        let contactChanged = !changes.began.isEmpty || !changes.lifted.isEmpty
        if contactChanged || touches.isEmpty {
            resetScratch()
        } else if previousTimestamp > 0,
                  let movement = dominantMovement(previous: previous, current: contactsByID) {
            let dt = max(1.0 / 240.0, min(0.050, timestamp - previousTimestamp))
            scratchFrames += 1
            scratchTravel += movement.distance
            if scratchFrames >= 2, scratchTravel >= 0.012 {
                let measured = Double(movement.distance) / dt
                let alpha = 1.0 - exp(-dt / 0.040)
                let speed = scratchSpeed.map { $0 + alpha * (measured - $0) } ?? measured
                scratchSpeed = speed
                let anchors = touches.filter { $0 != movement.point }
                percussion.setDrumSkinScratch(
                    at: movement.point,
                    speed: speed,
                    anchors: anchors,
                    direction: movement.vector,
                    surfaceEnergy: energy(at: movement.point),
                    synthetic: false
                )
                energize(at: movement.point, amount: min(0.035, speed * 0.008), now: now)
            }
        }
        previousTimestamp = touches.isEmpty ? 0 : timestamp
        // The hardware callback can run well above display cadence. Rendering
        // here as well as from `presentFrame` floods AppKit/Metal with stale
        // full-size frames and makes the newest contact appear late. Audio
        // remains raw-frame driven; visuals are coalesced to the presentation
        // timer below and always read the latest touch state.
    }

    private func presentFrame() {
        let now = CACurrentMediaTime()
        membrane.advance(to: now, touches: visibleTouches)
        onActivity?(visibleTouches, membrane.snapshot())
    }

    private func resetScratch() {
        scratchFrames = 0
        scratchTravel = 0
        scratchSpeed = nil
        percussion.stopDrumSkinScratch()
    }

    private func dominantMovement(previous: [Int32: CGPoint], current: [Int32: CGPoint])
        -> (point: CGPoint, distance: CGFloat, vector: CGVector)? {
        current.compactMap { identifier, point -> (CGPoint, CGFloat, CGVector)? in
            guard let old = previous[identifier] else { return nil }
            let vector = CGVector(dx: point.x - old.x, dy: point.y - old.y)
            return (point, hypot(vector.dx * 1.64, vector.dy), vector)
        }.max { $0.1 < $1.1 }
    }

    private func decayEnergies(now: Double) {
        for index in energies.indices {
            let age = max(0, now - energies[index].at)
            energies[index].level *= exp(-age / 0.90)
            energies[index].at = now
        }
        energies.removeAll { $0.level < 0.01 }
    }

    private func energy(at point: CGPoint) -> Double {
        min(1, energies.reduce(0) { total, energy in
            let dx = Double(point.x - energy.point.x) * 1.64
            let dy = Double(point.y - energy.point.y)
            return total + energy.level * exp(-(dx * dx + dy * dy) / (2 * 0.19 * 0.19))
        })
    }

    private func energize(at point: CGPoint, amount: Double, now: Double) {
        if let index = energies.indices.min(by: {
            distanceSquared(energies[$0].point, point) < distanceSquared(energies[$1].point, point)
        }), distanceSquared(energies[index].point, point) < 0.05 {
            energies[index].level = min(1, energies[index].level + amount)
            energies[index].at = now
        } else {
            energies.append(TracktrampEnergy(point: point, level: amount, at: now))
        }
    }

    private func distanceSquared(_ a: CGPoint, _ b: CGPoint) -> Double {
        let dx = Double(a.x - b.x) * 1.64
        let dy = Double(a.y - b.y)
        return dx * dx + dy * dy
    }
}

private func playfulTrackDrumFont(_ pointSize: CGFloat) -> NSFont {
    for name in ["Comic Sans MS Bold", "ComicSansMS-Bold",
                 "Chalkboard SE Bold", "ChalkboardSE-Bold"] {
        if let font = NSFont(name: name, size: pointSize) { return font }
    }
    return NSFont.systemFont(ofSize: pointSize, weight: .heavy)
}

private final class TrackDrumCharacterLayer: CALayer {
    var attributedCharacter: NSAttributedString?
    var inset: CGFloat = 0

    override func draw(in context: CGContext) {
        guard let attributedCharacter else { return }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(cgContext: context, flipped: false)
        attributedCharacter.draw(at: NSPoint(x: inset, y: inset))
        NSGraphicsContext.restoreGraphicsState()
    }
}

/// The same MacPal-style per-character bubble lettering used by Slab's prox
/// prompt rocks, kept local so this small feedback app does not pull in Slab.
private final class TrackDrumInstructionView: NSView {
    private var instruction = "esc to quit"

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        setAccessibilityElement(true)
        setAccessibilityRole(.staticText)
        rebuildCharacters()
    }

    required init?(coder: NSCoder) {
        fatalError("TrackDrumInstructionView is code-only")
    }

    func setInstruction(_ newValue: String) {
        guard instruction != newValue else { return }
        instruction = newValue
        setAccessibilityLabel(newValue)
        rebuildCharacters()
    }

    override func layout() {
        super.layout()
        rebuildCharacters()
    }

    private func rebuildCharacters() {
        guard let root = layer else { return }
        root.sublayers?.forEach { $0.removeFromSuperlayer() }
        setAccessibilityLabel(instruction)

        let font = playfulTrackDrumFont(18)
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.controlAccentColor
        shadow.shadowBlurRadius = 0
        shadow.shadowOffset = NSSize(width: 2, height: -2)
        let scale = window?.backingScaleFactor ?? NSScreen.main?.backingScaleFactor ?? 2
        let inset: CGFloat = 7
        var characters: [TrackDrumCharacterLayer] = []
        var widths: [CGFloat] = []
        var totalWidth: CGFloat = 0

        for character in instruction {
            let attributed = NSAttributedString(string: String(character), attributes: [
                .font: font,
                .foregroundColor: NSColor.white,
                .strokeColor: NSColor(white: 0.08, alpha: 1),
                .strokeWidth: -3.5,
                .shadow: shadow,
            ])
            let size = attributed.size()
            let characterLayer = TrackDrumCharacterLayer()
            characterLayer.attributedCharacter = attributed
            characterLayer.inset = inset
            characterLayer.contentsScale = scale
            characterLayer.bounds = NSRect(x: 0, y: 0,
                                           width: size.width + inset * 2,
                                           height: size.height + inset * 2)
            characterLayer.anchorPoint = CGPoint(x: 0.5, y: 0.5)

            var hash: UInt32 = 2166136261
            for byte in "trackdrum\(characters.count)\(instruction)".utf8 {
                hash = (hash ^ UInt32(byte)) &* 16777619
            }
            let dy = CGFloat(Int(hash % 5)) / 2 - 1
            let rotation = (CGFloat(Int((hash >> 8) % 9)) - 4) * 0.9 * .pi / 180
            var transform = CATransform3DMakeTranslation(0, dy, 0)
            transform = CATransform3DRotate(transform, rotation, 0, 0, 1)
            characterLayer.transform = transform
            root.addSublayer(characterLayer)
            characters.append(characterLayer)
            widths.append(size.width)
            totalWidth += size.width
            characterLayer.setNeedsDisplay()
        }

        var x = (bounds.width - totalWidth) / 2
        let start = CACurrentMediaTime()
        let timing = Array(repeating: CAMediaTimingFunction(name: .easeInEaseOut), count: 3)
        for (index, characterLayer) in characters.enumerated() {
            characterLayer.position = CGPoint(x: x + widths[index] / 2, y: bounds.midY)
            x += widths[index]
            let begin = start + Double(index) * 0.12

            let sway = CAKeyframeAnimation(keyPath: "transform.translation.y")
            sway.values = [0, 1.2, -0.8, 0]
            sway.keyTimes = [0, 0.25, 0.75, 1]
            sway.timingFunctions = timing
            sway.duration = 1.8
            sway.repeatCount = .infinity
            sway.beginTime = begin
            sway.isAdditive = true
            characterLayer.add(sway, forKey: "wiggleY")

            let rotate = CAKeyframeAnimation(keyPath: "transform.rotation.z")
            rotate.values = [0, 1.2 * Double.pi / 180, -0.8 * Double.pi / 180, 0]
            rotate.keyTimes = [0, 0.25, 0.75, 1]
            rotate.timingFunctions = timing
            rotate.duration = 1.8
            rotate.repeatCount = .infinity
            rotate.beginTime = begin
            rotate.isAdditive = true
            characterLayer.add(rotate, forKey: "wiggleR")
        }
    }
}

private final class TracktrampView: NSView {
    private static let instructionBandHeight: CGFloat = 48
    private let surfaceClip = NSView(frame: .zero)
    private let launchSurface = TrackDrumLaunchSurfaceView(frame: .zero)
    private var menubandSurface: TracktrampMetalView?
    private let focusInstruction = TrackDrumInstructionView(frame: .zero)
    private var trackpadSize = TracktrampMetalView.logicalSize
    var onDefocus: (() -> Void)?
    var onTrackpadFrame: (([TrackpadContact], Double, Double) -> Void)?
    var onTrackpadDeviceSize: ((NSSize) -> Void)?
    private var pointerHidden = false
    private var reportedDeviceSize: NSSize?

    override var isFlipped: Bool { false }
    override var acceptsFirstResponder: Bool { true }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        allowedTouchTypes = [.indirect]
        wantsRestingTouches = true
        layer?.backgroundColor = NSColor.clear.cgColor
        layer?.masksToBounds = true
        surfaceClip.wantsLayer = true
        surfaceClip.layer?.backgroundColor = NSColor.clear.cgColor
        surfaceClip.layer?.masksToBounds = true
        addSubview(surfaceClip)
        surfaceClip.addSubview(launchSurface)
        addSubview(focusInstruction)
    }

    required init?(coder: NSCoder) {
        fatalError("TracktrampView is code-only")
    }

    func setTrackpadSize(_ size: NSSize) {
        trackpadSize = size
        needsLayout = true
        needsDisplay = true
    }

    var preferredContentSize: NSSize {
        NSSize(width: trackpadSize.width,
               height: trackpadSize.height + Self.instructionBandHeight)
    }

    func prepareRenderer() {
        guard menubandSurface == nil else { return }
        let renderer = TracktrampMetalView()
        renderer.frame = rendererRect
        surfaceClip.addSubview(renderer, positioned: .above, relativeTo: launchSurface)
        menubandSurface = renderer
    }

    func update(touches: [CGPoint], membrane: TrackpadMembraneSimulation.Snapshot) {
        menubandSurface?.update(membrane, touches: touches)
    }

    func showEscapeInstructionTemporarily() {
        let defaults = UserDefaults.standard
        let key = "trackDrumEscapeInstructionLaunchCount"
        let launchCount = defaults.integer(forKey: key) + 1
        defaults.set(launchCount, forKey: key)
        let environment = ProcessInfo.processInfo.environment
        let developerMode = environment["TRACKDRUM_DEVELOPER_MODE"] == "1"
            || Bundle.main.bundlePath.contains("/aesthetic-computer/")
        let shouldShow = developerMode || launchCount <= 15
        focusInstruction.isHidden = !shouldShow
        guard shouldShow else { return }
        DispatchQueue.main.asyncAfter(deadline: .now() + 5) { [weak focusInstruction] in
            focusInstruction?.isHidden = true
        }
    }

    func syncPointerVisibility(applicationActive: Bool) {
        let shouldHide = applicationActive
        if shouldHide, !pointerHidden {
            NSCursor.hide()
            pointerHidden = true
        } else if !shouldHide, pointerHidden {
            NSCursor.unhide()
            pointerHidden = false
        }
    }

    func restorePointer() {
        if pointerHidden {
            NSCursor.unhide()
            pointerHidden = false
        }
    }

    override func layout() {
        super.layout()
        surfaceClip.frame = physicalTrackpadRect
        launchSurface.frame = surfaceClip.bounds
        menubandSurface?.frame = rendererRect
        focusInstruction.frame = NSRect(
            x: 0,
            y: trackpadSize.height,
            width: bounds.width,
            height: Self.instructionBandHeight
        )
    }

    override func keyDown(with event: NSEvent) {
        if event.keyCode == 53 { onDefocus?() } else { super.keyDown(with: event) }
    }

    override func touchesBegan(with event: NSEvent) { reportTouches(from: event) }
    override func touchesMoved(with event: NSEvent) { reportTouches(from: event) }
    override func touchesEnded(with event: NSEvent) { reportTouches(from: event) }
    override func touchesCancelled(with event: NSEvent) { reportTouches(from: event) }

    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }

    override func hitTest(_ point: NSPoint) -> NSView? {
        bounds.contains(point) ? self : nil
    }

    private func reportTouches(from event: NSEvent) {
        let touches = event.touches(matching: .touching, in: self)
            .filter { $0.type == .indirect }
        let began = Set(event.touches(matching: .began, in: self).map {
            ObjectIdentifier($0.identity as AnyObject)
        })
        let contacts = touches.map { touch in
            let identity = ObjectIdentifier(touch.identity as AnyObject)
            return TrackpadContact(
                identifier: Int32(truncatingIfNeeded: identity.hashValue),
                point: touch.normalizedPosition,
                state: began.contains(identity) ? 3 : 4
            )
        }

        if let size = touches.first?.deviceSize,
           size.width > 0,
           size.height > 0,
           reportedDeviceSize.map({
               abs($0.width - size.width) > 0.5 || abs($0.height - size.height) > 0.5
           }) ?? true {
            reportedDeviceSize = size
            onTrackpadDeviceSize?(size)
        }

        onTrackpadFrame?(contacts, event.timestamp, CACurrentMediaTime())
    }

    private var rendererRect: NSRect {
        let marginX = 4 * trackpadSize.width / 132
        let marginY = 4 * trackpadSize.height / 80
        return surfaceClip.bounds.insetBy(dx: -marginX, dy: -marginY)
    }

    private var physicalTrackpadRect: NSRect {
        NSRect(
            x: 0,
            y: 0,
            width: trackpadSize.width,
            height: trackpadSize.height
        )
    }
}

/// Cheap first-frame stand-in for the Metal drum surface. It is deliberately
/// layer-only: no shader creation, texture rasterization, or image decoding may
/// delay the window becoming visible.
private final class TrackDrumLaunchSurfaceView: NSView {
    private let zoneColors: [NSColor] = [
        NSColor(calibratedRed: 0.48, green: 0.55, blue: 0.41, alpha: 1),
        NSColor(calibratedRed: 0.68, green: 0.31, blue: 0.22, alpha: 1),
        NSColor(calibratedRed: 0.76, green: 0.56, blue: 0.20, alpha: 1),
        NSColor(calibratedRed: 0.30, green: 0.22, blue: 0.17, alpha: 1),
    ]
    private lazy var zones: [CALayer] = zoneColors.map { color in
        let zone = CALayer()
        zone.backgroundColor = color.cgColor
        zone.cornerRadius = 8
        return zone
    }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.backgroundColor = NSColor(calibratedWhite: 0.10, alpha: 1).cgColor
        layer?.borderColor = NSColor.controlAccentColor.withAlphaComponent(0.65).cgColor
        layer?.borderWidth = 2
        layer?.cornerRadius = 12
        layer?.masksToBounds = true
        zones.forEach { layer?.addSublayer($0) }
    }

    required init?(coder: NSCoder) {
        fatalError("TrackDrumLaunchSurfaceView is code-only")
    }

    override func layout() {
        super.layout()
        let gap: CGFloat = 3
        let inner = bounds.insetBy(dx: 5, dy: 5)
        let halfWidth = (inner.width - gap) / 2
        let halfHeight = (inner.height - gap) / 2
        zones[0].frame = NSRect(x: inner.minX, y: inner.minY,
                                width: halfWidth, height: halfHeight)
        zones[1].frame = NSRect(x: inner.midX + gap / 2, y: inner.minY,
                                width: halfWidth, height: halfHeight)
        zones[2].frame = NSRect(x: inner.minX, y: inner.midY + gap / 2,
                                width: halfWidth, height: halfHeight)
        zones[3].frame = NSRect(x: inner.midX + gap / 2, y: inner.midY + gap / 2,
                                width: halfWidth, height: halfHeight)
    }
}
