import AppKit
import AVFoundation
import CoreGraphics
import JukeDSP

/// A floating, single-record surface for direct deck play. The platter is the
/// control: press to stop it under the hand, drag around the groove to scratch,
/// and release to resume the state it had before the touch.
final class DJPopoutDeckController: NSWindowController, NSWindowDelegate {
    private let recordView: DJRadialRecordView
    private let deckName: String
    private var displayTimer: Timer?
    private var hasPositioned = false

    init(deck: DJDeckPlayer, name: String, accent: NSColor) {
        deckName = name
        recordView = DJRadialRecordView(frame: NSRect(x: 0, y: 0, width: 350, height: 350))
        recordView.deck = deck
        recordView.accent = accent
        recordView.deckName = name

        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 350, height: 350),
            styleMask: [.borderless],
            backing: .buffered,
            defer: false
        )
        window.title = "JukeWizard · Deck \(name)"
        window.isMovableByWindowBackground = true
        window.level = .floating
        window.collectionBehavior = [.fullScreenAuxiliary, .moveToActiveSpace]
        window.minSize = NSSize(width: 260, height: 260)
        window.backgroundColor = .clear
        window.isOpaque = false
        window.hasShadow = false
        window.contentView = recordView

        super.init(window: window)
        window.delegate = self
        recordView.onClose = { [weak window] in window?.performClose(nil) }
    }

    required init?(coder: NSCoder) { fatalError() }
    deinit { displayTimer?.invalidate() }

    func show(track: Track?) {
        if let track { recordView.load(track) }
        window?.title = "JukeWizard · \(deckName) · \(track?.title ?? "record")"
        positionOnce()
        showWindow(nil)
        window?.orderFrontRegardless()
        startDisplay()
    }

    func trackChanged(_ track: Track) {
        recordView.load(track)
        window?.title = "JukeWizard · \(deckName) · \(track.title)"
    }

    private func startDisplay() {
        displayTimer?.invalidate()
        displayTimer = DJRunLoopTimer.scheduled(every: 1.0 / 60.0) { [weak self] _ in
            self?.recordView.advanceEffects()
            self?.recordView.needsDisplay = true
        }
    }

    private func positionOnce() {
        guard !hasPositioned, let window, let screen = NSScreen.main else { return }
        hasPositioned = true
        let visible = screen.visibleFrame
        let frame = window.frame
        let inset: CGFloat = 18
        let origin: NSPoint
        switch deckName {
        case "A": origin = NSPoint(x: visible.minX + inset, y: visible.maxY - frame.height - inset)
        case "B": origin = NSPoint(x: visible.maxX - frame.width - inset,
                                    y: visible.maxY - frame.height - inset)
        case "C": origin = NSPoint(x: visible.minX + inset, y: visible.minY + inset)
        default:  origin = NSPoint(x: visible.maxX - frame.width - inset, y: visible.minY + inset)
        }
        window.setFrameOrigin(origin)
    }

    func windowWillClose(_ notification: Notification) {
        recordView.cancelTrackpadLock()
        displayTimer?.invalidate()
        displayTimer = nil
    }

    func windowDidResignKey(_ notification: Notification) {
        recordView.cancelTrackpadLock()
    }
}

final class DJRadialRecordView: NSView {
    private struct Spark {
        var point: NSPoint
        var velocity: CGVector
        var life: Double
        let duration: Double
    }
    private struct GrooveTrail {
        let rotation: CGFloat
        var life: Double
        let energy: CGFloat
    }
    weak var deck: DJDeckPlayer?
    var accent: NSColor = Palette.teal
    var deckName = "A"
    var onClose: (() -> Void)?

    private var trackTitle = "record"
    private var trackDetail = "press · hold · scratch"
    private var envelope: [Float] = []
    private var loadToken = 0
    private var lastAngle: CGFloat?
    private var lastTimestamp: TimeInterval?
    private var scratchOrigin: Double = 0
    private var scratchOffset: Double = 0
    private var wasPlayingAtPress = false
    private var didDrag = false
    private var pointerScratching = false
    private var pressTimer: Timer?
    private var releaseTimer: Timer?
    private var brakeFactor = 1.0
    private var brakeActive = false
    private var pressStartedAt: TimeInterval?
    private var trackpadScratching = false
    private var trackpadOrigin: Double = 0
    private var trackpadOffset: Double = 0
    private var trackpadLastTimestamp: TimeInterval?
    private var trackpadEndTimer: Timer?
    private var touchPositions: [ObjectIdentifier: NSPoint] = [:]
    private var multitouchScratching = false
    private var multitouchOrigin: Double = 0
    private var multitouchOffset: Double = 0
    private var multitouchLastTimestamp: TimeInterval?
    private var multitouchArmed = false
    private var multitouchWasPlaying = false
    private var multitouchTravel: Double = 0
    private var trackpadLockActive = false
    private var cursorHiddenByLock = false
    private var sparks: [Spark] = []
    private var grooveTrails: [GrooveTrail] = []
    private var lastEffectTime = ProcessInfo.processInfo.systemUptime
    private var lastTrailTime: TimeInterval = 0
    private var sparkBudget: Double = 0
    private var recordCache: NSImage?
    private var shadowCache: NSImage?
    private var energyTraceCache: NSImage?
    private var cachedSize: NSSize = .zero
    private var cachedDark = false
    private var cachedMotor = false
    private var spinMomentum = 0.0
    private var centerDragMouse: NSPoint?
    private var centerDragOrigin: NSPoint?

    override var acceptsFirstResponder: Bool { true }
    override var mouseDownCanMoveWindow: Bool { false }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        allowedTouchTypes = [.indirect]
        wantsRestingTouches = true
        layer?.cornerRadius = 22
        layer?.masksToBounds = false
        setAccessibilityRole(.slider)
        setAccessibilityLabel("Floating scratch record")
        setAccessibilityHelp("Press and hold to slow the record. Double-click the vinyl for one-finger trackpad lock; press Escape to exit.")
    }

    required init?(coder: NSCoder) { fatalError() }
    deinit {
        pressTimer?.invalidate()
        releaseTimer?.invalidate()
        trackpadEndTimer?.invalidate()
        releaseTrackpadLock()
    }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        trackingAreas.forEach(removeTrackingArea)
        addTrackingArea(NSTrackingArea(rect: bounds,
                                       options: [.activeInKeyWindow, .cursorUpdate],
                                       owner: self))
    }

    override func cursorUpdate(with event: NSEvent) { NSCursor.openHand.set() }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        invalidateRecordCache()
    }

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        invalidateRecordCache()
    }

    private var center: NSPoint { NSPoint(x: bounds.midX, y: bounds.midY) }
    private var radius: CGFloat { max(1, min(bounds.width, bounds.height) * 0.44) }
    private var needleAngle: CGFloat { .pi * 0.257 }
    private var trackProgress: CGFloat {
        guard let deck, deck.duration > 0 else { return 0 }
        return CGFloat(max(0, min(1, deck.currentTime / deck.duration)))
    }
    private var needleTip: NSPoint {
        let grooveRadius = radius * (0.82 - trackProgress * 0.48)
        return NSPoint(x: center.x + cos(needleAngle) * grooveRadius,
                       y: center.y + sin(needleAngle) * grooveRadius)
    }

    func load(_ track: Track) {
        if let primpat = DJPrimpats.metadata(for: track) {
            trackTitle = "\(deckName) · \(primpat.waveform.rawValue.uppercased())"
            let number = String(format: "%.2f", primpat.frequency)
                .replacingOccurrences(of: #"0+$"#, with: "", options: .regularExpression)
                .replacingOccurrences(of: #"\.$"#, with: "", options: .regularExpression)
            trackDetail = "\(primpat.key) · \(number) Hz"
        } else {
            trackTitle = track.title
        }
        if DJPrimpats.metadata(for: track) != nil {
            // The frequency is already the useful record label.
        } else if let key = track.meta?.key, !key.isEmpty {
            trackDetail = key
        } else if let bpm = track.meta?.bpm {
            trackDetail = "\(bpm) BPM"
        } else {
            trackDetail = "press · hold · scratch"
        }
        envelope = []
        invalidateRecordCache()
        loadToken += 1
        let token = loadToken
        needsDisplay = true

        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            guard let file = try? AVAudioFile(forReading: track.url) else { return }
            let format = file.processingFormat
            let frameCount = Int(file.length)
            let channelCount = max(1, Int(format.channelCount))
            guard frameCount > 1 else { return }

            let bins = 2_880
            var env = [Float](repeating: 0, count: bins)
            let chunkFrames = AVAudioFrameCount(min(frameCount, 8192))
            guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: chunkFrames) else { return }
            var absoluteFrame = 0
            while absoluteFrame < frameCount {
                let count = AVAudioFrameCount(min(Int(chunkFrames), frameCount - absoluteFrame))
                do { try file.read(into: buffer, frameCount: count) } catch { return }
                guard let channels = buffer.floatChannelData else { return }
                let loaded = Int(buffer.frameLength)
                guard loaded > 0 else { break }
                for localFrame in 0..<loaded {
                    let frame = absoluteFrame + localFrame
                    var magnitude: Float = 0
                    for channel in 0..<channelCount {
                        let sample = channels[channel][localFrame]
                        magnitude += abs(sample)
                    }
                    magnitude /= Float(channelCount)
                    let bin = min(bins - 1, frame * bins / frameCount)
                    env[bin] = max(env[bin], magnitude)
                }
                absoluteFrame += loaded
            }
            let envMax = env.max() ?? 1
            if envMax > 0 { env = env.map { $0 / envMax } }

            DispatchQueue.main.async {
                guard let self, token == self.loadToken else { return }
                self.envelope = env
                self.invalidateRecordCache()
                self.needsDisplay = true
            }
        }
    }

    func advanceEffects() {
        let now = ProcessInfo.processInfo.systemUptime
        let elapsed = min(0.05, max(0, now - lastEffectTime))
        lastEffectTime = now
        for index in sparks.indices {
            sparks[index].point.x += sparks[index].velocity.dx * elapsed
            sparks[index].point.y += sparks[index].velocity.dy * elapsed
            sparks[index].velocity.dy -= 120 * elapsed
            sparks[index].life -= elapsed
        }
        sparks.removeAll { $0.life <= 0 }
        for index in grooveTrails.indices { grooveTrails[index].life -= elapsed }
        grooveTrails.removeAll { $0.life <= 0 }

        guard let deck else { return }
        let state = deck.visualState
        guard abs(state.motion) >= 0.002 else { return }
        let energy = min(1, max(Double(state.energy), abs(state.motion) * 0.10))
        let rotation = CGFloat(-deck.currentTime / DJPlatterGeometry.secondsPerRevolution * Double.pi * 2)
        if energy > 0.035, now - lastTrailTime > 0.035 {
            grooveTrails.append(GrooveTrail(rotation: rotation, life: 0.20,
                                             energy: CGFloat(energy)))
            if grooveTrails.count > 5 { grooveTrails.removeFirst(grooveTrails.count - 5) }
            lastTrailTime = now
        }

        sparkBudget += energy * elapsed * 38
        let tip = needleTip
        while sparkBudget >= 1, sparks.count < 24 {
            sparkBudget -= 1
            let duration = Double.random(in: 0.16...0.34)
            sparks.append(Spark(
                point: tip,
                velocity: CGVector(dx: Double.random(in: -48...52) + state.motion * 8,
                                   dy: Double.random(in: 48...116)),
                life: duration,
                duration: duration))
        }
    }

    override func draw(_ dirtyRect: NSRect) {
        let dark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        NSColor.clear.setFill()
        bounds.fill(using: .copy)

        let c = center
        let r = radius
        let rotation = CGFloat(-(deck?.currentTime ?? 0) /
            DJPlatterGeometry.secondsPerRevolution * Double.pi * 2)
        let visual = deck?.visualState ?? (motion: 0.0, energy: Float(0))
        let energy = min(CGFloat(1), max(CGFloat(visual.energy), CGFloat(abs(visual.motion)) * 0.10))

        ensureRecordCache(dark: dark)
        // The cast shadow belongs to the screen, not the rotating material.
        shadowCache?.draw(in: bounds, from: .zero, operation: .sourceOver, fraction: 1)
        NSGraphicsContext.saveGraphicsState()
        let transform = NSAffineTransform()
        transform.translateX(by: c.x, yBy: c.y)
        transform.rotate(byRadians: rotation)
        transform.translateX(by: -c.x, yBy: -c.y)
        transform.concat()

        recordCache?.draw(in: bounds, from: .zero, operation: .sourceOver, fraction: 1)
        NSGraphicsContext.restoreGraphicsState()

        drawGrooveTrails(center: c, radius: r)

        NSGraphicsContext.saveGraphicsState()
        transform.concat()
        energyTraceCache?.draw(in: bounds, from: .zero, operation: .plusLighter,
                               fraction: min(0.78, energy * 0.72))
        NSGraphicsContext.restoreGraphicsState()

        // The needle stays still while the waveform turns underneath it.
        let needle = NSBezierPath()
        needle.move(to: NSPoint(x: c.x + r * 0.18, y: c.y + r * 0.96))
        needle.line(to: needleTip)
        Palette.gold.setStroke()
        needle.lineWidth = max(2, r * 0.018)
        needle.lineCapStyle = .round
        needle.stroke()
        drawNeedleHeat(center: c, radius: r, energy: energy)
        drawSparks()

        if trackpadLockActive {
            accent.withAlphaComponent(0.88).setStroke()
            let lockRing = NSBezierPath(ovalIn: NSRect(x: c.x - r - 7, y: c.y - r - 7,
                                                       width: (r + 7) * 2, height: (r + 7) * 2))
            lockRing.lineWidth = 3
            lockRing.stroke()
        }

    }

    private func drawEnvelopeGroove(center c: NSPoint, radius r: CGFloat) {
        let values = envelope.isEmpty ? [Float](repeating: 0.55, count: 2_880) : envelope
        let revolution = DJPlatterGeometry.secondsPerRevolution
        let turns = max(1, (deck?.duration ?? revolution) / revolution)
        let path = NSBezierPath()
        for (index, value) in values.enumerated() {
            let progress = CGFloat(index) / CGFloat(max(1, values.count - 1))
            let angle = needleAngle + progress * CGFloat(turns) * .pi * 2
            let grooveRadius = r * (0.82 - progress * 0.48) + CGFloat(value) * r * 0.020
            let point = NSPoint(x: c.x + cos(angle) * grooveRadius,
                                y: c.y + sin(angle) * grooveRadius)
            index == 0 ? path.move(to: point) : path.line(to: point)
        }
        accent.withAlphaComponent(0.80).setStroke()
        path.lineWidth = max(1.15, r * 0.008)
        path.lineJoinStyle = .round
        path.stroke()
    }

    private func drawGrooveTrails(center c: NSPoint, radius r: CGFloat) {
        guard let energyTraceCache else { return }
        for trail in grooveTrails {
            NSGraphicsContext.saveGraphicsState()
            let transform = NSAffineTransform()
            transform.translateX(by: c.x, yBy: c.y)
            transform.rotate(byRadians: trail.rotation)
            transform.translateX(by: -c.x, yBy: -c.y)
            transform.concat()
            let alpha = CGFloat(max(0, trail.life / 0.20)) * trail.energy * 0.22
            energyTraceCache.draw(in: bounds, from: .zero, operation: .plusLighter,
                                  fraction: alpha)
            NSGraphicsContext.restoreGraphicsState()
        }
    }

    private func invalidateRecordCache() {
        recordCache = nil
        shadowCache = nil
        energyTraceCache = nil
    }

    private func ensureRecordCache(dark: Bool) {
        let motor = deck?.motorEnabled ?? false
        guard recordCache == nil || cachedSize != bounds.size || cachedDark != dark ||
                cachedMotor != motor else { return }
        cachedSize = bounds.size
        cachedDark = dark
        cachedMotor = motor
        let c = center
        let r = radius

        let shadowImage = NSImage(size: bounds.size)
        shadowImage.lockFocus()
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.black.withAlphaComponent(motor ? 0.70 : 0.46)
        shadow.shadowBlurRadius = motor ? 6 : 9
        shadow.shadowOffset = NSSize(width: 0, height: motor ? -10 : -7)
        shadow.set()
        NSColor.black.setFill()
        NSBezierPath(ovalIn: NSRect(x: c.x - r, y: c.y - r,
                                    width: r * 2, height: r * 2)).fill()
        shadowImage.unlockFocus()
        shadowImage.cacheMode = .always
        shadowCache = shadowImage

        let record = NSImage(size: bounds.size)
        record.lockFocus()
        Palette.deckSurface(accent, dark: dark).setFill()
        NSBezierPath(ovalIn: NSRect(x: c.x - r, y: c.y - r, width: r * 2, height: r * 2)).fill()
        drawEnvelopeGroove(center: c, radius: r)
        drawLabel(center: c, radius: r)
        record.unlockFocus()
        record.cacheMode = .always
        recordCache = record

        let trace = NSImage(size: bounds.size)
        trace.lockFocus()
        drawEnergyTrace(center: c, radius: r)
        trace.unlockFocus()
        trace.cacheMode = .always
        energyTraceCache = trace
    }

    private func drawEnergyTrace(center c: NSPoint, radius r: CGFloat) {
        let values = envelope.isEmpty ? [Float](repeating: 0.55, count: 2_880) : envelope
        let revolution = DJPlatterGeometry.secondsPerRevolution
        let turns = max(1, (deck?.duration ?? revolution) / revolution)
        let path = NSBezierPath()
        for (index, value) in values.enumerated() {
            let progress = CGFloat(index) / CGFloat(max(1, values.count - 1))
            let angle = needleAngle + progress * CGFloat(turns) * .pi * 2
            let grooveRadius = r * (0.82 - progress * 0.48) + CGFloat(value) * r * 0.020
            let point = NSPoint(x: c.x + cos(angle) * grooveRadius,
                                y: c.y + sin(angle) * grooveRadius)
            index == 0 ? path.move(to: point) : path.line(to: point)
        }
        accent.setStroke()
        path.lineWidth = max(1.4, r * 0.01)
        path.stroke()
    }

    private func drawNeedleHeat(center c: NSPoint, radius r: CGFloat, energy: CGFloat) {
        guard energy > 0.015 else { return }
        let tip = needleTip
        for ring in stride(from: 4, through: 1, by: -1) {
            let size = CGFloat(ring) * (3 + energy * 3)
            NSColor(srgbRed: 1, green: 0.18 + 0.15 * CGFloat(ring), blue: 0.02,
                    alpha: energy * (0.10 + CGFloat(5 - ring) * 0.08)).setFill()
            NSBezierPath(ovalIn: NSRect(x: tip.x - size, y: tip.y - size,
                                       width: size * 2, height: size * 2)).fill()
        }
        NSColor(calibratedRed: 1, green: 0.94, blue: 0.56, alpha: energy).setFill()
        NSBezierPath(ovalIn: NSRect(x: tip.x - 2.2, y: tip.y - 2.2, width: 4.4, height: 4.4)).fill()
    }

    private func drawSparks() {
        for spark in sparks {
            let alpha = CGFloat(max(0, spark.life / spark.duration))
            NSColor(srgbRed: 1, green: 0.32 + alpha * 0.55, blue: 0.04,
                    alpha: alpha).setFill()
            let size = 1.5 + alpha * 2.4
            NSBezierPath(ovalIn: NSRect(x: spark.point.x - size, y: spark.point.y - size,
                                       width: size * 2, height: size * 2)).fill()
        }
    }

    private func drawLabel(center c: NSPoint, radius r: CGFloat) {
        let labelR = r * 0.21
        accent.withAlphaComponent(0.94).setFill()
        NSBezierPath(ovalIn: NSRect(x: c.x - labelR, y: c.y - labelR,
                                   width: labelR * 2, height: labelR * 2)).fill()
        if deck?.motorEnabled == true {
            let spindleShadow = NSShadow()
            spindleShadow.shadowColor = NSColor.black.withAlphaComponent(0.55)
            spindleShadow.shadowBlurRadius = 4
            spindleShadow.shadowOffset = NSSize(width: 1, height: -3)
            spindleShadow.set()
            Palette.gold.setFill()
            NSBezierPath(ovalIn: NSRect(x: c.x - 5, y: c.y - 5, width: 10, height: 10)).fill()
            NSColor.white.withAlphaComponent(0.72).setFill()
            NSBezierPath(ovalIn: NSRect(x: c.x - 2.5, y: c.y + 0.5, width: 3, height: 3)).fill()
        } else {
            let dark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
            Palette.deckInk(accent, dark: dark).withAlphaComponent(0.58).setFill()
            NSBezierPath(ovalIn: NSRect(x: c.x - 3, y: c.y - 3, width: 6, height: 6)).fill()
        }

        let title = deckName as NSString
        let titleAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: max(12, r * 0.11), weight: .black),
            .foregroundColor: NSColor.white
        ]
        let titleSize = title.size(withAttributes: titleAttrs)
        title.draw(at: NSPoint(x: c.x - titleSize.width / 2,
                               y: c.y + r * 0.025), withAttributes: titleAttrs)

        let detail = trackDetail as NSString
        let detailAttrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedDigitSystemFont(ofSize: max(9, r * 0.055), weight: .bold),
            .foregroundColor: NSColor.white.withAlphaComponent(0.82)
        ]
        let detailSize = detail.size(withAttributes: detailAttrs)
        detail.draw(at: NSPoint(x: c.x - detailSize.width / 2,
                                y: c.y - r * 0.105), withAttributes: detailAttrs)
    }

    private func angle(for event: NSEvent) -> CGFloat {
        let point = convert(event.locationInWindow, from: nil)
        return atan2(point.y - center.y, point.x - center.x)
    }

    override func mouseDown(with event: NSEvent) {
        let point = convert(event.locationInWindow, from: nil)
        let distance = hypot(point.x - center.x, point.y - center.y)
        // The visible vinyl is the instrument; the translucent surround is
        // the window handle. Keep these hit regions identical to what is drawn.
        guard distance <= radius else {
            window?.performDrag(with: event)
            return
        }
        // The center sticker is a second, easy-to-find window handle. Moving
        // it never changes playback position or catches the virtual platter.
        if distance <= radius * 0.21 {
            centerDragMouse = NSEvent.mouseLocation
            centerDragOrigin = window?.frame.origin
            return
        }
        if event.clickCount == 2 {
            trackpadLockActive ? releaseTrackpadLock() : engageTrackpadLock()
            return
        }
        window?.makeFirstResponder(self)
        NSCursor.closedHand.set()
        lastAngle = angle(for: event)
        lastTimestamp = event.timestamp
        scratchOrigin = deck?.currentTime ?? 0
        scratchOffset = 0
        spinMomentum = deck?.visualState.motion ?? 0
        wasPlayingAtPress = deck?.isPlaying ?? false
        didDrag = false
        pointerScratching = false
        releaseTimer?.invalidate()
        releaseTimer = nil
        pressStartedAt = ProcessInfo.processInfo.systemUptime
        pressTimer?.invalidate()
        pressTimer = DJRunLoopTimer.scheduled(every: 1.0 / 60.0) { [weak self] _ in
            guard let self, self.wasPlayingAtPress, !self.pointerScratching,
                  let started = self.pressStartedAt else { return }
            // Same deliberate-hold threshold and 60 Hz decay as video.mjs.
            guard ProcessInfo.processInfo.systemUptime - started >= 0.09 else { return }
            self.brakeActive = true
            self.didDrag = true // a brake never becomes a tap action on lift
            self.brakeFactor *= 0.85
            if self.brakeFactor < 0.02 { self.brakeFactor = 0 }
            self.deck?.setTransportScale(self.brakeFactor)
        }
    }

    override func mouseDragged(with event: NSEvent) {
        if let startMouse = centerDragMouse, let startOrigin = centerDragOrigin {
            let mouse = NSEvent.mouseLocation
            window?.setFrameOrigin(NSPoint(x: startOrigin.x + mouse.x - startMouse.x,
                                           y: startOrigin.y + mouse.y - startMouse.y))
            return
        }
        guard let prior = lastAngle else { return }
        if !pointerScratching {
            pressTimer?.invalidate()
            pressTimer = nil
            if brakeActive {
                brakeActive = false
                brakeFactor = 1
                deck?.restoreTransportRate()
            }
            pointerScratching = true
            deck?.beginScratch()
        }
        let next = angle(for: event)
        var delta = next - prior
        if delta > .pi { delta -= .pi * 2 }
        if delta < -.pi { delta += .pi * 2 }
        let seconds = Double(-delta / (.pi * 2)) * DJPlatterGeometry.secondsPerRevolution
        let elapsed = max(1.0 / 240.0, event.timestamp - (lastTimestamp ?? event.timestamp))
        scratchOffset += seconds
        if abs(scratchOffset) > 0.008 { didDrag = true }
        deck?.scratch(to: scratchOrigin + scratchOffset, movement: seconds, elapsed: elapsed)
        lastAngle = next
        lastTimestamp = event.timestamp
        needsDisplay = true
    }

    override func mouseUp(with event: NSEvent) {
        if let startMouse = centerDragMouse {
            let mouse = NSEvent.mouseLocation
            let travelled = hypot(mouse.x - startMouse.x, mouse.y - startMouse.y)
            centerDragMouse = nil
            centerDragOrigin = nil
            if travelled < 3 {
                deck?.toggle()
                invalidateRecordCache()
                needsDisplay = true
            }
            return
        }
        lastAngle = nil
        lastTimestamp = nil
        pressStartedAt = nil
        pressTimer?.invalidate()
        pressTimer = nil
        if pointerScratching {
            pointerScratching = false
            spinMomentum = deck?.visualState.motion ?? 0
            deck?.endScratch(momentum: spinMomentum)
            beginMomentumRelease()
        } else if brakeActive {
            beginBrakeRelease()
        } else if !wasPlayingAtPress, !didDrag {
            deck?.play()
        }
        NSCursor.openHand.set()
    }

    private func beginBrakeRelease() {
        brakeActive = false
        releaseTimer?.invalidate()
        releaseTimer = DJRunLoopTimer.scheduled(every: 1.0 / 60.0) { [weak self] timer in
            guard let self else { timer.invalidate(); return }
            // video.mjs ramps toward the pre-touch rate rather than snapping.
            self.brakeFactor += (1 - self.brakeFactor) * 0.12
            self.deck?.setTransportScale(self.brakeFactor)
            if self.brakeFactor >= 0.995 {
                self.brakeFactor = 1
                self.deck?.restoreTransportRate()
                timer.invalidate()
                self.releaseTimer = nil
            }
        }
    }

    private func beginMomentumRelease() {
        releaseTimer?.invalidate()
        releaseTimer = DJRunLoopTimer.scheduled(every: 1.0 / 60.0) { [weak self] timer in
            guard let self, let deck = self.deck else { timer.invalidate(); return }
            let restingRate = deck.motorEnabled ? deck.rate : 0
            self.spinMomentum += (restingRate - self.spinMomentum) * 0.022
            deck.setTransportVelocity(self.spinMomentum)
            if abs(self.spinMomentum - restingRate) < 0.004 {
                self.spinMomentum = restingRate
                deck.motorEnabled ? deck.restoreTransportRate() : deck.setTransportVelocity(0)
                timer.invalidate()
                self.releaseTimer = nil
            }
        }
    }

    override func scrollWheel(with event: NSEvent) {
        if multitouchScratching { return }
        let point = convert(event.locationInWindow, from: nil)
        guard hypot(point.x - center.x, point.y - center.y) <= radius + 10 else {
            super.scrollWheel(with: event)
            return
        }

        if !trackpadScratching {
            releaseTimer?.invalidate()
            releaseTimer = nil
            brakeFactor = 1
            deck?.restoreTransportRate()
            trackpadScratching = true
            trackpadOrigin = deck?.currentTime ?? 0
            trackpadOffset = 0
            spinMomentum = deck?.visualState.motion ?? 0
            trackpadLastTimestamp = event.timestamp
            deck?.beginScratch()
        }

        let horizontal = event.scrollingDeltaX
        let vertical = -event.scrollingDeltaY
        let points = abs(horizontal) >= abs(vertical) ? horizontal : vertical
        let secondsPerPoint = DJPlatterGeometry.secondsPerRevolution /
            Double(max(120, radius * 2))
        let movement = -Double(points) * secondsPerPoint
        let elapsed = max(1.0 / 240.0,
                          event.timestamp - (trackpadLastTimestamp ?? event.timestamp))
        trackpadOffset += movement
        deck?.scratch(to: trackpadOrigin + trackpadOffset,
                      movement: movement, elapsed: elapsed)
        trackpadLastTimestamp = event.timestamp
        needsDisplay = true

        trackpadEndTimer?.invalidate()
        if event.phase == .ended || event.phase == .cancelled ||
            event.momentumPhase == .ended || event.momentumPhase == .cancelled {
            endTrackpadScratch()
        } else {
            // Some trackpad and wheel drivers omit explicit phase endings.
            trackpadEndTimer = DJRunLoopTimer.scheduled(every: 0.12, repeats: false) { [weak self] _ in
                self?.endTrackpadScratch()
            }
        }
    }

    private func endTrackpadScratch() {
        guard trackpadScratching else { return }
        trackpadEndTimer?.invalidate()
        trackpadEndTimer = nil
        trackpadScratching = false
        trackpadLastTimestamp = nil
        spinMomentum = deck?.visualState.motion ?? 0
        deck?.endScratch(momentum: spinMomentum)
        beginMomentumRelease()
    }

    private func touchID(_ touch: NSTouch) -> ObjectIdentifier {
        ObjectIdentifier(touch.identity)
    }

    private func platterPosition(_ touch: NSTouch) -> NSPoint {
        let normalized = touch.normalizedPosition
        return NSPoint(x: (normalized.x - 0.5) * 2,
                       y: (normalized.y - 0.5) * 2)
    }

    override func touchesBegan(with event: NSEvent) {
        for touch in event.touches(matching: .touching, in: self) {
            touchPositions[touchID(touch)] = platterPosition(touch)
        }
        guard touchPositions.count >= requiredTouchCount else {
            needsDisplay = true
            return
        }

        if trackpadScratching { endTrackpadScratch() }
        if !multitouchScratching && !multitouchArmed {
            releaseTimer?.invalidate()
            releaseTimer = nil
            pressTimer?.invalidate()
            pressTimer = nil
            brakeFactor = 1
            deck?.restoreTransportRate()
            multitouchArmed = true
            multitouchWasPlaying = deck?.isPlaying ?? false
            multitouchTravel = 0
            multitouchLastTimestamp = event.timestamp
            let started = ProcessInfo.processInfo.systemUptime
            pressTimer = DJRunLoopTimer.scheduled(every: 1.0 / 60.0) { [weak self] _ in
                guard let self, self.multitouchArmed, self.multitouchWasPlaying,
                      ProcessInfo.processInfo.systemUptime - started >= 0.09 else { return }
                self.brakeActive = true
                self.brakeFactor *= 0.85
                if self.brakeFactor < 0.02 { self.brakeFactor = 0 }
                self.deck?.setTransportScale(self.brakeFactor)
            }
        }
        needsDisplay = true
    }

    override func touchesMoved(with event: NSEvent) {
        let touches = event.touches(matching: .touching, in: self)
        var contacts: [ACPlatterContact] = []
        contacts.reserveCapacity(touches.count)

        for touch in touches {
            let id = touchID(touch)
            let current = platterPosition(touch)
            let previous = touchPositions[id] ?? current
            var contact = ACPlatterContact()
            contact.previous_x = Double(previous.x)
            contact.previous_y = Double(previous.y)
            contact.current_x = Double(current.x)
            contact.current_y = Double(current.y)
            contacts.append(contact)
            touchPositions[id] = current
        }

        guard touchPositions.count >= requiredTouchCount else {
            needsDisplay = true
            return
        }
        let elapsed = max(1.0 / 240.0,
                          event.timestamp - (multitouchLastTimestamp ?? event.timestamp))
        let movement = contacts.withUnsafeBufferPointer {
            ac_platter_contact_motion($0.baseAddress, $0.count,
                                      DJPlatterGeometry.secondsPerRevolution)
        }
        multitouchTravel += abs(movement)
        if !multitouchScratching && multitouchTravel > 0.002 {
            pressTimer?.invalidate()
            pressTimer = nil
            multitouchArmed = false
            if brakeActive {
                brakeActive = false
                brakeFactor = 1
                deck?.restoreTransportRate()
            }
            multitouchScratching = true
            multitouchOrigin = deck?.currentTime ?? 0
            multitouchOffset = 0
            spinMomentum = deck?.visualState.motion ?? 0
            deck?.beginScratch()
        }
        guard multitouchScratching else {
            multitouchLastTimestamp = event.timestamp
            needsDisplay = true
            return
        }

        multitouchOffset += movement
        deck?.scratch(to: multitouchOrigin + multitouchOffset,
                      movement: movement, elapsed: elapsed)
        multitouchLastTimestamp = event.timestamp
        needsDisplay = true
    }

    override func touchesEnded(with event: NSEvent) {
        for touch in event.touches(matching: .ended, in: self) {
            touchPositions.removeValue(forKey: touchID(touch))
        }
        finishMultitouchIfNeeded()
    }

    override func touchesCancelled(with event: NSEvent) {
        for touch in event.touches(matching: .cancelled, in: self) {
            touchPositions.removeValue(forKey: touchID(touch))
        }
        if touchPositions.count < requiredTouchCount { finishMultitouch() }
        needsDisplay = true
    }

    private func finishMultitouchIfNeeded() {
        if touchPositions.count < requiredTouchCount { finishMultitouch() }
        needsDisplay = true
    }

    private func finishMultitouch() {
        pressTimer?.invalidate()
        pressTimer = nil
        multitouchArmed = false
        multitouchLastTimestamp = nil
        if multitouchScratching {
            multitouchScratching = false
            spinMomentum = deck?.visualState.motion ?? 0
            deck?.endScratch(momentum: spinMomentum)
            beginMomentumRelease()
        } else if brakeActive {
            beginBrakeRelease()
        }
    }

    private var requiredTouchCount: Int { trackpadLockActive ? 1 : 2 }

    private func engageTrackpadLock() {
        guard !trackpadLockActive else { return }
        trackpadLockActive = true
        window?.makeKey()
        window?.makeFirstResponder(self)
        if CGAssociateMouseAndMouseCursorPosition(0) == .success {
            NSCursor.hide()
            cursorHiddenByLock = true
        }
        DJFocusFlash.shared.flash(rising: true)
        DJFocusDing.shared.play(rising: true)
        needsDisplay = true
    }

    private func releaseTrackpadLock() {
        guard trackpadLockActive || cursorHiddenByLock else { return }
        let wasActive = trackpadLockActive
        if multitouchScratching || multitouchArmed { finishMultitouch() }
        touchPositions.removeAll()
        trackpadLockActive = false
        CGAssociateMouseAndMouseCursorPosition(1)
        if cursorHiddenByLock {
            NSCursor.unhide()
            cursorHiddenByLock = false
        }
        if wasActive {
            DJFocusFlash.shared.flash(rising: false)
            DJFocusDing.shared.play(rising: false)
        }
        needsDisplay = true
    }

    func cancelTrackpadLock() { releaseTrackpadLock() }

    override func keyDown(with event: NSEvent) {
        if event.keyCode == 53, trackpadLockActive {
            releaseTrackpadLock()
            return
        }
        super.keyDown(with: event)
    }
}
