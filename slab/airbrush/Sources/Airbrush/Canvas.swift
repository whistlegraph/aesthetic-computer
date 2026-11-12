import AppKit
import MetalKit
import AirbrushCore

final class BrushCursor: NSView {
    var point = Point(200, 200)
    var radius: Double = 55
    var painting = false
    var empty = true
    var trackpad = false
    var contacts: [Point] = []
    var tethers: [(Point, Point)] = []
    override var isFlipped: Bool { true }
    override func hitTest(_ point: NSPoint) -> NSView? { nil }
    override func draw(_ dirtyRect: NSRect) {
        if empty {
            let hint = trackpad
                ? "Touch to paint · every finger draws\nWASD move · Q / E zoom · Tab for mouse"
                : "Click and drag to paint\nWASD move · Q / E zoom · Tab for trackpad"
            let style = NSMutableParagraphStyle()
            style.alignment = .center
            style.lineSpacing = 7
            (hint as NSString).draw(in: NSRect(x: 12, y: max(12, bounds.midY - 30),
                width: bounds.width - 24, height: 70), withAttributes: [
                    .font: NSFont.systemFont(ofSize: 14),
                    .foregroundColor: NSColor.black.withAlphaComponent(0.5),
                    .paragraphStyle: style,
                ])
        }
        for (raw, tip) in tethers where hypot(raw.x - tip.x, raw.y - tip.y) > 1 {
            let line = NSBezierPath()
            line.move(to: NSPoint(x: raw.x, y: raw.y)); line.line(to: NSPoint(x: tip.x, y: tip.y))
            NSColor.black.withAlphaComponent(0.35).setStroke()
            line.lineWidth = 1; line.stroke()
        }
        for point in contacts.isEmpty ? [point] : contacts {
        let rect = NSRect(x: point.x - radius, y: point.y - radius, width: radius * 2, height: radius * 2)
        let outline = NSBezierPath(ovalIn: rect)
        NSColor.white.withAlphaComponent(0.85).setStroke()
        outline.lineWidth = 2.5
        outline.stroke()
        (painting ? NSColor.systemOrange : NSColor.black.withAlphaComponent(0.55)).setStroke()
        outline.lineWidth = 1
        outline.stroke()
        let center = NSBezierPath(ovalIn: NSRect(x: point.x - 1.5, y: point.y - 1.5, width: 3, height: 3))
        NSColor.black.withAlphaComponent(0.6).setFill()
        center.fill()
        }
    }
}

final class PaintCanvas: MTKView, MTKViewDelegate {
    enum InputMode { case mouse, trackpad }
    private(set) var inputMode: InputMode = .mouse
    var onModeChange: (() -> Void)?
    let painter: PaintRenderer
    let cache: PaintTileCache
    var painting = Painting()
    var brush = Brush()
    private(set) var smoothing: StrokeSmoothing = .string12
    func setSmoothing(_ mode: StrokeSmoothing) {
        resetInput(); smoothing = mode; redraw(artwork: false)
    }
    private var adjustmentTravel: Double = 0
    var seed: UInt64 = 1
    var onChange: (() -> Void)?
    var onBrushChange: (() -> Void)?
    var onError: ((Error) -> Void)?
    var dabs: [Dab] { cache.store.dabs }
    private var strokeDabEnds: [Int] = []
    private var artworkDirty = true
    private let inFlight = DispatchSemaphore(value: 2)
    private var redoStack: [Stroke] = []
    private var active: Stroke?
    private var sampler: StrokeSampler?
    private var strokeTime = 0.0
    private var strokeStartedAt = 0.0
    private var mouseStabilizer: StrokeStabilizer?
    private var keys = Set<UInt16>()
    private var mousePainting = false
    private final class Contact {
        var point: Point
        var sampler: StrokeSampler
        var time = 0.0
        let startedAt = ProcessInfo.processInfo.systemUptime
        var stabilizer: StrokeStabilizer
        init(point: Point, first: Sample, smoothing: StrokeSmoothing) {
            self.point = point; sampler = StrokeSampler(first: first)
            stabilizer = StrokeStabilizer(point: point, time: 0, mode: smoothing)
        }
    }
    private var contacts: [AnyHashable: Contact] = [:]
    private var contactOrder: [AnyHashable] = []
    private var commandZoom = Set<UInt16>()
    private var keyMonitor: Any?
    private var pointer = Point(200, 180)
    private var timer: Timer?
    private var previousTick = CACurrentMediaTime()
    private var lastPoint: Point?
    private let cursor = BrushCursor()
    private var tracking: NSTrackingArea?
    private var pointerLocked = false
    private var pointerHidden = false
    private var mouseInside = false
    var size: Point { Point(bounds.width, bounds.height) }
    override var acceptsFirstResponder: Bool { true }
    override var isFlipped: Bool { true }

    init(painter: PaintRenderer) {
        self.painter = painter
        cache = PaintTileCache(painter: painter)
        super.init(frame: .zero, device: painter.device)
        colorPixelFormat = .bgra8Unorm
        framebufferOnly = false // The completed view is copied for LOD transitions.
        isPaused = true
        enableSetNeedsDisplay = true
        delegate = self
        allowedTouchTypes = [.indirect]
        wantsRestingTouches = true
        addSubview(cursor)
        setAccessibilityLabel("Airbrush canvas")
        let timer = Timer(timeInterval: 1.0 / 120, repeats: true) { [weak self] _ in self?.tick() }
        RunLoop.main.add(timer, forMode: .common)
        self.timer = timer
        // AppKit can consume Command key-up before the responder chain. A local
        // monitor keeps held zoom continuous and stops it on either key release.
        keyMonitor = NSEvent.addLocalMonitorForEvents(matching: [.keyDown, .keyUp, .flagsChanged]) { [weak self] event in
            guard let self else { return event }
            if event.type == .flagsChanged && !event.modifierFlags.contains(.command) { self.commandZoom.removeAll() }
            let zoomKey = [UInt16(24), 27, 69, 78].contains(event.keyCode)
            if event.type == .keyUp && zoomKey { self.commandZoom.remove(event.keyCode) }
            guard zoomKey, event.type == .keyDown, event.modifierFlags.contains(.command),
                  self.window?.isKeyWindow == true, self.window?.firstResponder === self else { return event }
            self.commandZoom.insert(event.keyCode)
            return nil
        }
    }
    required init(coder: NSCoder) { fatalError("Code only") }
    deinit { timer?.invalidate(); if let keyMonitor { NSEvent.removeMonitor(keyMonitor) }; releasePointer() }

    override func layout() {
        super.layout()
        cursor.frame = bounds
        if inputMode == .mouse { syncMousePosition() }
        redraw()
    }
    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        if let tracking { removeTrackingArea(tracking) }
        tracking = NSTrackingArea(rect: .zero, options: [.mouseMoved, .mouseEnteredAndExited, .activeInKeyWindow, .inVisibleRect], owner: self)
        addTrackingArea(tracking!)
    }
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }

    func resetInput() {
        finishStroke()
        keys.removeAll()
        mousePainting = false
        contacts.removeAll(); contactOrder.removeAll()
        commandZoom.removeAll(); adjustmentTravel = 0
        lastPoint = nil
        releasePointer()
        redraw()
    }

    func toggleMode() { setMode(inputMode == .mouse ? .trackpad : .mouse) }
    private func setMode(_ mode: InputMode) {
        resetInput()
        inputMode = mode
        // Both modes meet at the same visible brush position. In trackpad
        // mode the OS pointer is parked inside this view, so raw NSTouch
        // delivery cannot wander into a neighboring pane or a control.
        warpPointerToBrush()
        if mode == .mouse { syncMousePosition() }
        else { capturePointerIfNeeded() }
        onModeChange?()
        redraw()
    }
    private func warpPointerToBrush() {
        guard let window, let screen = NSScreen.screens.first else { return }
        let old = NSPoint(x: pointer.x, y: pointer.y)
        let local = bounds.insetBy(dx: 1, dy: 1).contains(old) ? old : NSPoint(x: bounds.midX, y: bounds.midY)
        pointer = Point(local.x, local.y)
        let screenPoint = window.convertPoint(toScreen: convert(local, to: nil))
        CGWarpMouseCursorPosition(CGPoint(x: screenPoint.x, y: screen.frame.maxY - screenPoint.y))
    }
    private func capturePointerIfNeeded() {
        guard inputMode == .trackpad, window?.isKeyWindow == true,
              window?.firstResponder === self, NSApp.isActive else { return }
        if !pointerLocked {
            warpPointerToBrush()
            pointerLocked = CGAssociateMouseAndMouseCursorPosition(0) == .success
        }
        if !pointerHidden { NSCursor.hide(); pointerHidden = true }
    }
    private func releasePointer() {
        if pointerLocked { CGAssociateMouseAndMouseCursorPosition(1); pointerLocked = false }
        if pointerHidden { NSCursor.unhide(); pointerHidden = false }
    }
    private func syncMousePosition() {
        guard let window else { return }
        let local = convert(window.convertPoint(fromScreen: NSEvent.mouseLocation), from: nil)
        mouseInside = bounds.contains(local)
        if mouseInside { pointer = Point(local.x, local.y); lastPoint = pointer }
    }

    private func sample(at location: Point? = nil) -> Sample {
        var worldBrush = brush
        worldBrush.radius /= painting.camera.zoom
        return Sample(point: painting.camera.world(location ?? pointer, size: size), brush: worldBrush)
    }
    private func beginStroke() {
        guard active == nil else { return }
        let first = sample()
        active = Stroke(seed: seed, samples: [first])
        sampler = StrokeSampler(first: first)
        strokeTime = 0
        strokeStartedAt = ProcessInfo.processInfo.systemUptime
        mouseStabilizer = StrokeStabilizer(point: pointer, time: 0, mode: smoothing)
        redoStack.removeAll()
        cache.append(Spray.dabs(first, seed: seed, index: 0), live: true)
    }
    private func appendMousePoint(_ point: Point, elapsed: Double) {
        guard let strokeSeed = active?.seed else { return }
        strokeTime = max(strokeTime + 1e-6, elapsed)
        if let samples = sampler?.append(sample(at: point), at: strokeTime), !samples.isEmpty {
            let offset = active!.samples.count
            var dabs: [Dab] = []
            for (i, value) in samples.enumerated() { dabs += Spray.dabs(value, seed: strokeSeed, index: offset + i) }
            cache.append(dabs, live: true)
            active?.samples.append(contentsOf: samples)
            redraw()
        }
    }
    private func advanceMouse() {
        let elapsed = max(strokeTime + 1e-6, ProcessInfo.processInfo.systemUptime - strokeStartedAt)
        guard let filtered = mouseStabilizer?.update(pointer, at: elapsed) else { return }
        appendMousePoint(filtered, elapsed: elapsed)
    }
    private func finishContact(_ contact: Contact) {
        for point in contact.stabilizer.finish() {
            contact.time += Spray.step
            for value in contact.sampler.append(sample(at: point), at: contact.time) { depositContact(value) }
        }
    }
    func finishStroke() {
        guard active != nil else { return }
        if let tail = mouseStabilizer?.finish() {
            for point in tail { appendMousePoint(point, elapsed: strokeTime + Spray.step) }
        }
        for identity in contactOrder { if let contact = contacts[identity] { finishContact(contact) } }
        guard let active else { return }
        painting.strokes.append(active)
        strokeDabEnds.append(cache.store.count)
        self.active = nil
        sampler = nil; mouseStabilizer = nil
        onChange?()
        redraw()
    }
    private func tick() {
        let now = CACurrentMediaTime()
        let elapsed = now - previousTick
        previousTick = now
        if artworkDirty || cache.needsRefinement { needsDisplay = true }
        guard window?.isKeyWindow == true, NSApp.isActive else {
            if active != nil || !keys.isEmpty || !commandZoom.isEmpty || pointerLocked || pointerHidden { resetInput() }
            return
        }
        capturePointerIfNeeded()
        if inputMode == .mouse && keys.isDisjoint(with: [15, 3, 4, 17]) {
            let before = pointer
            syncMousePosition()
            if before != pointer { redraw(artwork: false) }
        }
        guard elapsed < 0.25 else { resetInput(); return }
        let dt = min(elapsed, 0.05)
        let dx = (keys.contains(2) ? 1.0 : 0) - (keys.contains(0) ? 1.0 : 0)
        let dy = (keys.contains(1) ? 1.0 : 0) - (keys.contains(13) ? 1.0 : 0)
        let commandIn = !commandZoom.isDisjoint(with: [24, 69])
        let commandOut = !commandZoom.isDisjoint(with: [27, 78])
        let zoomDirection = (keys.contains(14) || commandIn ? 1.0 : 0) - (keys.contains(12) || commandOut ? 1.0 : 0)
        let slow = NSEvent.modifierFlags.contains(.shift) ? 0.2 : 1.0
        let moving = dx != 0 || dy != 0 || zoomDirection != 0
        if moving {
            let length = max(1, sqrt(dx * dx + dy * dy))
            painting.camera.center = painting.camera.center + Point(dx, dy) * (320 * slow * dt / (painting.camera.zoom * length))
            painting.camera.magnify(exp(zoomDirection * 1.4 * (commandZoom.isEmpty ? slow : 1) * dt), anchor: pointer, size: size)
        }
        let spraying = inputMode == .mouse ? mouseInside && (mousePainting || keys.contains(49)) : !contacts.isEmpty
        if inputMode == .trackpad {
            advanceContacts(by: dt)
        } else if spraying {
            beginStroke()
            advanceMouse()
        } else { finishStroke() }
        if moving || spraying { redraw() }
    }

    func redraw(artwork: Bool = true) {
        if artwork { artworkDirty = true; needsDisplay = true }
        cursor.point = mouseStabilizer?.position ?? pointer
        cursor.radius = brush.radius
        cursor.painting = active != nil
        cursor.empty = cache.store.count == 0
        cursor.trackpad = inputMode == .trackpad
        cursor.contacts = contactOrder.compactMap { contacts[$0]?.stabilizer.position }
        cursor.tethers = contactOrder.compactMap { identity in
            guard let c = contacts[identity] else { return nil }
            return (c.point, c.stabilizer.position)
        }
        if let tip = mouseStabilizer?.position { cursor.tethers.append((pointer, tip)) }
        cursor.needsDisplay = true
    }
    func draw(in view: MTKView) {
        guard inFlight.wait(timeout: .now()) == .success else { return }
        guard bounds.width > 0, bounds.height > 0,
              let drawable = currentDrawable, let pass = currentRenderPassDescriptor,
              let command = painter.queue.makeCommandBuffer() else { inFlight.signal(); return }
        artworkDirty = false
        cache.encode(camera: painting.camera, size: size, pixels: drawableSize,
                     priority: active != nil ? pointer : nil, pass: pass, command: command)
        command.addCompletedHandler { [inFlight] _ in inFlight.signal() }
        command.present(drawable)
        command.commit()
    }
    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) { redraw() }

    override func keyDown(with event: NSEvent) {
        if event.modifierFlags.contains(.command) { super.keyDown(with: event); return }
        switch event.keyCode {
        case 48: if !event.isARepeat { toggleMode() }
        case 53: setMode(.mouse)
        case 0, 1, 2, 13, 12, 14, 49, 15, 3, 4, 17:
            keys.insert(event.keyCode)
            if event.keyCode == 49 && inputMode == .mouse && mouseInside { beginStroke(); redraw() }
        case 6: undoStroke()
        case 16: redoStroke()
        case 45:
            seed = Spray.bits(seed: seed, index: 0)
            onBrushChange?()
        case 33: BrushSteps.step(&brush, 0, -1); onBrushChange?(); redraw()
        case 30: BrushSteps.step(&brush, 0, 1); onBrushChange?(); redraw()
        case 29: painting.camera = Camera(); redraw()
        default: super.keyDown(with: event)
        }
    }
    override func keyUp(with event: NSEvent) {
        keys.remove(event.keyCode)
        adjustmentTravel = 0
        if event.keyCode == 49 && inputMode == .mouse { finishStroke() }
        if inputMode == .mouse && [15, 3, 4, 17].contains(event.keyCode) { syncMousePosition(); redraw() }
    }
    override func resignFirstResponder() -> Bool { resetInput(); return super.resignFirstResponder() }

    private func movePointer(_ p: Point) {
        if let previous = lastPoint {
            let delta = p.x - previous.x
            if let parameter = [UInt16(15), 4, 3, 17].firstIndex(where: { keys.contains($0) }) {
                adjustmentTravel += delta
                let steps = Int(adjustmentTravel / 24)
                if steps != 0 {
                    BrushSteps.step(&brush, parameter, steps)
                    adjustmentTravel -= Double(steps) * 24
                }
            } else { pointer = p; adjustmentTravel = 0 }
        } else { pointer = p }
        if brush.marker == true, active != nil, mousePainting || keys.contains(49) { advanceMouse() }
        lastPoint = p
        onBrushChange?()
        redraw(artwork: false)
    }
    private func mousePoint(_ event: NSEvent) -> Point {
        let p = convert(event.locationInWindow, from: nil)
        return Point(p.x, p.y)
    }
    override func mouseMoved(with event: NSEvent) {
        guard inputMode == .mouse else { return }
        mouseInside = bounds.contains(convert(event.locationInWindow, from: nil))
        movePointer(mousePoint(event))
    }
    override func mouseEntered(with event: NSEvent) {
        mouseInside = true
        if inputMode == .mouse { movePointer(mousePoint(event)) }
    }
    override func mouseExited(with event: NSEvent) {
        mouseInside = false
        if inputMode == .mouse { finishStroke() }
    }
    override func mouseDown(with event: NSEvent) {
        window?.makeFirstResponder(self)
        if inputMode == .mouse { mouseInside = true; movePointer(mousePoint(event)) }
        else { return }
        mousePainting = true
        beginStroke()
        redraw()
    }
    override func mouseDragged(with event: NSEvent) {
        guard inputMode == .mouse else { return }
        mouseInside = bounds.contains(convert(event.locationInWindow, from: nil))
        movePointer(mousePoint(event))
    }
    override func mouseUp(with event: NSEvent) {
        guard inputMode == .mouse else { return }
        movePointer(mousePoint(event))
        mousePainting = false; finishStroke()
    }
    override func scrollWheel(with event: NSEvent) {
        guard inputMode == .mouse else { return }
        painting.camera.magnify(exp(-event.scrollingDeltaY * 0.012), anchor: pointer, size: size)
        redraw()
    }
    override func magnify(with event: NSEvent) {
        guard inputMode == .mouse else { return }
        painting.camera.magnify(exp(event.magnification), anchor: pointer, size: size)
        redraw()
    }
    override func touchesBegan(with event: NSEvent) { touches(event) }
    override func touchesMoved(with event: NSEvent) { touches(event) }
    override func touchesEnded(with event: NSEvent) { touches(event) }
    override func touchesCancelled(with event: NSEvent) { resetInput() }
    // Contacts have independent paths and samplers. Their emitted knots share
    // one chronological gesture, preserving overlap order on save/replay/undo.
    private func depositContact(_ value: Sample) {
        if active == nil { active = Stroke(seed: seed); redoStack.removeAll() }
        let index = active!.samples.count
        cache.append(Spray.dabs(value, seed: active!.seed, index: index), live: true)
        active!.samples.append(value)
    }
    private func touches(_ event: NSEvent) {
        guard inputMode == .trackpad, window?.isKeyWindow == true else { return }
        // Include the hardware lift position before removing that contact.
        for touch in event.touches(matching: .ended, in: self) {
            guard let identity = touch.identity as? AnyHashable, let contact = contacts[identity] else { continue }
            contact.point = Point(touch.normalizedPosition.x * bounds.width, (1 - touch.normalizedPosition.y) * bounds.height)
            contact.time = max(contact.time + 1e-6, ProcessInfo.processInfo.systemUptime - contact.startedAt)
            let filtered = contact.stabilizer.update(contact.point, at: contact.time)
            for value in contact.sampler.append(sample(at: filtered), at: contact.time) { depositContact(value) }
        }
        let touching = event.touches(matching: .touching, in: self).filter { $0.type == .indirect && !$0.isResting }
            .sorted { $0.normalizedPosition.x == $1.normalizedPosition.x
                ? $0.normalizedPosition.y < $1.normalizedPosition.y : $0.normalizedPosition.x < $1.normalizedPosition.x }
        updateContacts(touching.compactMap { touch -> (AnyHashable, Point)? in
            guard let identity = touch.identity as? AnyHashable else { return nil }
            return (identity, Point(touch.normalizedPosition.x * bounds.width, (1 - touch.normalizedPosition.y) * bounds.height))
        })
    }
    private func advanceContacts(by dt: Double) {
        for identity in contactOrder {
            guard let contact = contacts[identity] else { continue }
            contact.time += dt
            let filtered = contact.stabilizer.update(contact.point, at: contact.time)
            for value in contact.sampler.append(sample(at: filtered), at: contact.time) { depositContact(value) }
        }
    }
    private func updateContacts(_ touching: [(AnyHashable, Point)]) {
        let present = Set(touching.map { $0.0 })
        for identity in contactOrder where !present.contains(identity) {
            if let contact = contacts[identity] { finishContact(contact) }
            contacts.removeValue(forKey: identity)
        }
        contactOrder.removeAll { !present.contains($0) }
        for (identity, point) in touching {
            if let contact = contacts[identity] {
                contact.point = point
                if brush.marker == true {
                    contact.time = max(contact.time + 1e-6, ProcessInfo.processInfo.systemUptime - contact.startedAt)
                    let filtered = contact.stabilizer.update(point, at: contact.time)
                    for value in contact.sampler.append(sample(at: filtered), at: contact.time) { depositContact(value) }
                }
            }
            else {
                let first = sample(at: point)
                contacts[identity] = Contact(point: point, first: first, smoothing: smoothing)
                contactOrder.append(identity)
                depositContact(first)
            }
        }
        if let first = contactOrder.first, let contact = contacts[first] { pointer = contact.point }
        if contacts.isEmpty { finishStroke(); lastPoint = nil }
        redraw()
    }

    static func verifyContacts() throws {
        let canvas = PaintCanvas(painter: try PaintRenderer())
        canvas.frame = NSRect(x: 0, y: 0, width: 400, height: 300)
        canvas.brush.marker = true; canvas.brush.radius = 2
        canvas.brush.hardness = 1; canvas.brush.flow = 1; canvas.brush.scatter = 0
        // Exercise the same contact lifecycle used by NSTouch, including one
        // finger lifting while another keeps drawing, then a new contact.
        canvas.updateContacts([(1, Point(40, 60)), (2, Point(340, 60))])
        precondition(canvas.dabs.count > 0, "Touch-down did not deposit immediately")
        for i in 1...12 {
            canvas.updateContacts([(1, Point(40, 60 + Double(i) * 5)), (2, Point(340, 60 + Double(i) * 5))])
            canvas.advanceContacts(by: Spray.step)
        }
        canvas.updateContacts([(2, Point(340, 150))])
        canvas.advanceContacts(by: Spray.step * 2)
        canvas.updateContacts([(2, Point(340, 160)), (3, Point(200, 250))])
        canvas.advanceContacts(by: Spray.step)
        canvas.updateContacts([])
        precondition(canvas.painting.strokes.count == 1 && canvas.active == nil)
        let stroke = canvas.painting.strokes[0]
        precondition(stroke.samples.allSatisfy { [-160.0, 140, 0].contains($0.point.x) }, "Contact paths were connected")
        let live = canvas.dabs
        let document = try Painting.decode(canvas.painting.encoded())
        precondition(document.strokes.flatMap(Spray.replay) == live, "Multitouch replay changed overlap order")
        canvas.undoStroke(); precondition(canvas.dabs.isEmpty)
        canvas.redoStroke(); precondition(canvas.dabs == live)
        canvas.updateContacts([(4, Point(20, 20))]); canvas.resetInput()
        precondition(canvas.contacts.isEmpty && canvas.active == nil, "Focus reset left a finger painting")
        canvas.setSmoothing(.balanced)
        canvas.updateContacts([(5, Point(50, 100)), (6, Point(350, 100))])
        canvas.updateContacts([(5, Point(50, 180)), (6, Point(350, 180))])
        canvas.advanceContacts(by: Spray.step)
        canvas.updateContacts([])
        let smoothed = canvas.painting.strokes.last!
        precondition(smoothed.samples.contains { abs($0.point.x + 150) < 1e-6 && abs($0.point.y - 30) < 1e-6 })
        precondition(smoothed.samples.contains { abs($0.point.x - 150) < 1e-6 && abs($0.point.y - 30) < 1e-6 })
        precondition(canvas.dabs == canvas.painting.strokes.flatMap(Spray.replay), "Adaptive contact tails changed replay order")
        print("PASS contact-down painting, independent multitouch paths, partial lift, reentry, save/replay, undo/redo, focus reset")
    }

    func undoStroke() {
        resetInput()
        if let stroke = painting.strokes.popLast() {
            redoStack.append(stroke)
            strokeDabEnds.removeLast()
            cache.truncate(to: strokeDabEnds.last ?? 0)
            redraw(); onChange?()
        }
    }
    func redoStroke() {
        resetInput()
        if let stroke = redoStack.popLast() {
            painting.strokes.append(stroke)
            cache.append(Spray.replay(stroke))
            strokeDabEnds.append(cache.store.count)
            redraw(); onChange?()
        }
    }
    func load(_ document: Painting) {
        resetInput()
        painting = document
        redoStack.removeAll()
        rebuild()
    }
    private func rebuild() {
        cache.replace([])
        strokeDabEnds.removeAll()
        for stroke in painting.strokes {
            cache.append(Spray.replay(stroke))
            strokeDabEnds.append(cache.store.count)
        }
        redraw()
    }
}
