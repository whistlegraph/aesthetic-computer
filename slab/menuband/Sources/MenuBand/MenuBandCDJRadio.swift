import ACMacAudio
import AppKit
import AVFoundation
import Darwin

/// The continuous external-audio deck that lives beside (not inside) the
/// piano instrument. A source only reaches the keys after the user explicitly
/// chooses "Sample to Piano".
enum CDJRadioSource: Equatable {
    case station(RadioStation)
    case spotify

    var label: String {
        switch self {
        case .station(let station): return station.label
        case .spotify: return "SPOTIFY"
        }
    }
}

/// Shared platter feel for the status-item CD. A point of pointer travel moves
/// the needle by a small, fixed amount of tape; velocity controls varispeed.
/// Keeping this pure makes the gesture curve regression-testable without an
/// audio device.
enum MenuBandCDJScratchGesture {
    static let secondsPerPoint = 0.015

    static func signedRate(forVelocity velocity: Double) -> Float {
        guard velocity.isFinite, abs(velocity) > 0.01 else { return 0 }
        let direction: Float = velocity < 0 ? -1 : 1
        let magnitude = max(0.16, min(4.0, abs(velocity) / 120.0))
        return direction * Float(magnitude)
    }
}

/// A tiny grain player fed from the active deck's rolling PCM ring. Each drag
/// event moves a virtual needle and replaces the currently sounding grain.
/// Reading the grain backwards gives a real reverse scratch; varispeed makes
/// faster hand motion sound and pitch faster like a physical platter.
final class MenuBandCDJScratchVoice {
    private let player = AVAudioPlayerNode()
    private let varispeed = AVAudioUnitVarispeed()
    private weak var engine: AVAudioEngine?
    private var attached = false
    private var tape: AVAudioPCMBuffer?
    private var needleFrame = 0

    func attach(to engine: AVAudioEngine, output: AVAudioNode,
                format: AVAudioFormat) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(player)
        engine.attach(varispeed)
        engine.connect(player, to: varispeed, format: format)
        engine.connect(varispeed, to: output, format: format)
        attached = true
    }

    @discardableResult
    func begin(with buffer: AVAudioPCMBuffer?) -> Bool {
        end()
        guard attached, let buffer,
              buffer.frameLength > 256,
              buffer.floatChannelData != nil else { return false }
        tape = buffer
        // Leave a little forward runway while beginning at the audible live
        // edge. A rightward flick can therefore accelerate before it catches
        // the newest captured frame.
        let runway = Int(buffer.format.sampleRate * 0.18)
        needleFrame = max(0, Int(buffer.frameLength) - 1 - runway)
        return true
    }

    func scrub(deltaPoints: Double, velocity: Double) {
        guard let tape,
              let source = tape.floatChannelData,
              abs(deltaPoints) > 0.001 else { return }
        let sampleRate = tape.format.sampleRate
        let lastFrame = max(0, Int(tape.frameLength) - 1)
        let movedFrames = Int(
            deltaPoints * MenuBandCDJScratchGesture.secondsPerPoint * sampleRate)
        needleFrame = max(0, min(lastFrame, needleFrame + movedFrames))

        let signedRate = MenuBandCDJScratchGesture.signedRate(
            forVelocity: velocity)
        let direction = deltaPoints < 0 ? -1 : 1
        let rate = max(0.16, min(4.0, abs(signedRate)))
        // Consume enough source for a roughly 85 ms output grain at the
        // current hand speed. Frequent drag events replace this grain, so a
        // held platter naturally stops instead of free-running underneath.
        let requested = max(256, Int(sampleRate * 0.085 * Double(rate)))
        let available = direction > 0
            ? lastFrame - needleFrame + 1
            : needleFrame + 1
        let frameCount = min(requested, available)
        guard frameCount > 128,
              let grain = AVAudioPCMBuffer(
                pcmFormat: tape.format,
                frameCapacity: AVAudioFrameCount(frameCount)),
              let destination = grain.floatChannelData else { return }
        grain.frameLength = AVAudioFrameCount(frameCount)

        let channels = Int(tape.format.channelCount)
        let fadeFrames = min(frameCount / 2, max(1, Int(sampleRate * 0.003)))
        for channel in 0..<channels {
            for frame in 0..<frameCount {
                let sourceFrame = needleFrame + direction * frame
                var gain: Float = 1
                if frame < fadeFrames {
                    gain = Float(frame) / Float(fadeFrames)
                } else if frame >= frameCount - fadeFrames {
                    gain = Float(frameCount - 1 - frame) / Float(fadeFrames)
                }
                destination[channel][frame] = source[channel][sourceFrame] * gain
            }
        }

        varispeed.rate = rate
        player.stop()
        player.scheduleBuffer(grain, at: nil, options: .interrupts)
        player.play()
    }

    func end() {
        player.stop()
        varispeed.rate = 1
        tape = nil
        needleFrame = 0
    }
}

/// Routes `juked`'s Core Audio process into Menu Band's AVAudioEngine. The
/// process tap mutes juked's original output, then this player re-emits it on
/// Menu Band's pre-FX bus so space, echo, proximity, compression, tape, and
/// waveform capture all see the same audio.
final class MenuBandCDJSpotifyDeck {
    private let format = AVAudioFormat(
        commonFormat: .pcmFormatFloat32, sampleRate: 44_100,
        channels: 2, interleaved: false)!
    private let player = AVAudioPlayerNode()
    private let timePitch = AVAudioUnitTimePitch()
    private let mixer = AVAudioMixerNode()
    private weak var engine: AVAudioEngine?
    private var attached = false
    private var tap: AnyObject?

    private let audioLock = NSLock()
    private var converter: AVAudioConverter?
    private var converterInputFormat: AVAudioFormat?
    private let ringSeconds: Double = 12
    private lazy var ringFrames = Int(format.sampleRate * ringSeconds)
    private lazy var ring = AVAudioPCMBuffer(
        pcmFormat: format, frameCapacity: AVAudioFrameCount(ringFrames))!
    private var ringWriteFrame: Int64 = 0

    var onError: ((String) -> Void)?

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        self.engine = engine
        engine.attach(player)
        engine.attach(timePitch)
        engine.attach(mixer)
        engine.connect(player, to: timePitch, format: format)
        engine.connect(timePitch, to: mixer, format: format)
        engine.connect(mixer, to: output, format: format)
        mixer.outputVolume = 1
        ring.frameLength = AVAudioFrameCount(ringFrames)
        if let channels = ring.floatChannelData {
            for channel in 0..<Int(format.channelCount) {
                memset(channels[channel], 0, ringFrames * MemoryLayout<Float>.size)
            }
        }
        attached = true
    }

    func start(processID: pid_t) {
        guard attached else { return }
        mixer.outputVolume = 1
        guard tap == nil else { return }
        guard #available(macOS 14.2, *) else {
            onError?("CDJ Radio Spotify routing requires macOS 14.2 or newer")
            return
        }
        do {
            let processTap = ACProcessAudioTap(
                processID: processID, name: "Menu Band CDJ Radio",
                muteOriginal: true)
            processTap.onLog = { NSLog("MenuBand CDJ Radio: \($0)") }
            try processTap.start { [weak self] buffer in
                self?.ingest(buffer)
            }
            tap = processTap
        } catch {
            onError?(error.localizedDescription)
        }
    }

    func stop() {
        if #available(macOS 14.2, *), let processTap = tap as? ACProcessAudioTap {
            processTap.stop()
        }
        tap = nil
        mixer.outputVolume = 0
        player.stop()
        audioLock.lock()
        converter = nil
        converterInputFormat = nil
        audioLock.unlock()
    }

    func setPitch(semitones: Float) {
        timePitch.pitch = max(-2_400, min(2_400, semitones * 100))
    }

    func setOutputEnabled(_ enabled: Bool) {
        mixer.outputVolume = enabled ? 1 : 0
    }

    /// Return from a platter scratch at the live edge. Stopping the player
    /// drops any buffers accumulated while the live deck was gated; the next
    /// process-tap buffer starts immediately at unity rate.
    func resumeLiveOutput() {
        player.stop()
        timePitch.rate = 1
        mixer.outputVolume = 1
    }

    /// Snapshot the newest deck audio for the explicit CDJ → Piano Sampler
    /// handoff. This never changes the active instrument by itself.
    func copyRecentAudio(seconds: Double) -> AVAudioPCMBuffer? {
        audioLock.lock(); defer { audioLock.unlock() }
        let available = min(Int64(ringFrames), ringWriteFrame)
        let wanted = min(available, Int64(max(0.1, seconds) * format.sampleRate))
        guard wanted > 0,
              let output = AVAudioPCMBuffer(
                pcmFormat: format, frameCapacity: AVAudioFrameCount(wanted)),
              let source = ring.floatChannelData,
              let destination = output.floatChannelData else { return nil }
        output.frameLength = AVAudioFrameCount(wanted)
        let start = ringWriteFrame - wanted
        for channel in 0..<Int(format.channelCount) {
            for frame in 0..<Int(wanted) {
                let sourceIndex = Int((start + Int64(frame)) % Int64(ringFrames))
                destination[channel][frame] = source[channel][sourceIndex]
            }
        }
        return output
    }

    private func ingest(_ input: AVAudioPCMBuffer) {
        audioLock.lock(); defer { audioLock.unlock() }
        if converter == nil || converterInputFormat != input.format {
            converter = AVAudioConverter(from: input.format, to: format)
            converterInputFormat = input.format
        }
        guard let converter else { return }
        converter.reset()
        let ratio = format.sampleRate / input.format.sampleRate
        let capacity = AVAudioFrameCount(
            ceil(Double(input.frameLength) * ratio) + 16)
        guard let output = AVAudioPCMBuffer(
            pcmFormat: format, frameCapacity: capacity) else { return }
        var supplied = false
        var conversionError: NSError?
        let status = converter.convert(to: output, error: &conversionError) {
            _, outStatus in
            if supplied {
                outStatus.pointee = .noDataNow
                return nil
            }
            supplied = true
            outStatus.pointee = .haveData
            return input
        }
        guard status != .error, conversionError == nil, output.frameLength > 0,
              let source = output.floatChannelData,
              let destination = ring.floatChannelData else { return }

        let frames = Int(output.frameLength)
        let writeStart = ringWriteFrame
        for channel in 0..<Int(format.channelCount) {
            for frame in 0..<frames {
                let destinationIndex = Int(
                    (writeStart + Int64(frame)) % Int64(ringFrames))
                destination[channel][destinationIndex] =
                    source[channel][frame]
            }
        }
        ringWriteFrame &+= Int64(frames)
        player.scheduleBuffer(output)
        if !player.isPlaying { player.play() }
    }

    deinit { stop() }
}

/// Album artwork printed as a compact disc. Menu Band owns this renderer so
/// the CD status item no longer depends on JukeWizard's process or assets.
enum MenuBandCDArtworkRenderer {
    static func disc(from art: NSImage, side: CGFloat) -> NSImage {
        let image = NSImage(size: NSSize(width: side, height: side))
        image.lockFocus()
        NSGraphicsContext.current?.imageInterpolation = .high
        let outer = NSRect(x: side * 0.025, y: side * 0.025,
                           width: side * 0.95, height: side * 0.95)
        NSColor.black.withAlphaComponent(0.92).setFill()
        NSBezierPath(ovalIn: outer).fill()
        let face = outer.insetBy(dx: side * 0.012, dy: side * 0.012)
        NSGraphicsContext.current?.saveGraphicsState()
        NSBezierPath(ovalIn: face).addClip()
        let scale = max(face.width / max(1, art.size.width),
                        face.height / max(1, art.size.height))
        let artSize = NSSize(width: art.size.width * scale,
                             height: art.size.height * scale)
        art.draw(in: NSRect(x: face.midX - artSize.width / 2,
                            y: face.midY - artSize.height / 2,
                            width: artSize.width, height: artSize.height))
        NSColor.white.withAlphaComponent(0.26).setStroke()
        let ring = NSBezierPath(ovalIn: face.insetBy(dx: side * 0.08,
                                                     dy: side * 0.08))
        ring.lineWidth = max(0.4, side * 0.006)
        ring.stroke()
        NSGraphicsContext.current?.restoreGraphicsState()
        let hubSide = side * 0.20
        let hub = NSRect(x: side / 2 - hubSide / 2,
                         y: side / 2 - hubSide / 2,
                         width: hubSide, height: hubSide)
        NSColor.white.withAlphaComponent(0.38).setFill()
        NSBezierPath(ovalIn: hub).fill()
        let holeSide = side * 0.075
        NSGraphicsContext.current?.compositingOperation = .clear
        NSBezierPath(ovalIn: NSRect(x: side / 2 - holeSide / 2,
                                    y: side / 2 - holeSide / 2,
                                    width: holeSide, height: holeSide)).fill()
        NSGraphicsContext.current?.compositingOperation = .sourceOver
        image.unlockFocus()
        image.isTemplate = false
        return image
    }

    static func fallback(side: CGFloat) -> NSImage {
        let art = NSImage(size: NSSize(width: side, height: side))
        art.lockFocus()
        NSGradient(colors: [.systemTeal, .systemPurple])?.draw(
            in: NSRect(x: 0, y: 0, width: side, height: side), angle: 35)
        art.unlockFocus()
        return disc(from: art, side: side)
    }
}

/// Menu Band's small spinning CD status item. It is deliberately owned by
/// the Menu Band process and opens Menu Band's CDJ Radio panel when clicked.
final class MenuBandCDJStatusItem {
    private let statusItem: NSStatusItem
    private let fallback: NSImage
    private var baseImage: NSImage
    private var timer: Timer?
    private var angle: CGFloat = 0
    private var artworkTask: URLSessionDataTask?
    private var representedArtworkURL: URL?
    private let side: CGFloat = 22
    private var mouseDownPoint: NSPoint?
    private var lastDragPoint: NSPoint?
    private var lastDragTimestamp: TimeInterval = 0
    private var scratching = false
    var onClick: (() -> Void)?
    var onScratchBegin: (() -> Bool)?
    var onScratch: ((_ deltaPoints: Double, _ velocity: Double) -> Void)?
    var onScratchEnd: (() -> Void)?

    init() {
        statusItem = NSStatusBar.system.statusItem(withLength: 25)
        statusItem.autosaveName = "menuband-cdj-radio"
        fallback = MenuBandCDArtworkRenderer.fallback(side: side)
        baseImage = fallback
        if let button = statusItem.button {
            button.image = fallback
            button.imagePosition = .imageOnly
            button.imageScaling = .scaleProportionallyDown
            button.target = self
            button.action = #selector(handlePlatterEvent)
            button.sendAction(on: [.leftMouseDown, .leftMouseDragged, .leftMouseUp])
            button.toolTip = "Menu Band CDJ Radio · drag the disc to scratch"
        }
        setVisible(false)
    }

    func setVisible(_ visible: Bool) {
        statusItem.isVisible = visible
        if !visible {
            finishScratch()
            setPlaying(false)
        }
    }

    func update(title: String, artworkURL: URL?, playing: Bool) {
        statusItem.button?.toolTip = title.isEmpty
            ? "Menu Band CDJ Radio · drag the disc to scratch"
            : "CDJ Radio · \(title) · drag to scratch"
        updateArtwork(artworkURL)
        setPlaying(playing)
    }

    func setPlaying(_ playing: Bool) {
        if playing {
            guard timer == nil else { return }
            let timer = Timer(timeInterval: 1.0 / 30.0, repeats: true) {
                [weak self] _ in self?.tick()
            }
            RunLoop.main.add(timer, forMode: .common)
            self.timer = timer
        } else {
            finishScratch()
            timer?.invalidate()
            timer = nil
            angle = 0
            statusItem.button?.image = baseImage
        }
    }

    private func updateArtwork(_ url: URL?) {
        guard representedArtworkURL != url else { return }
        representedArtworkURL = url
        artworkTask?.cancel()
        artworkTask = nil
        baseImage = fallback
        statusItem.button?.image = fallback
        guard let url else { return }
        artworkTask = URLSession.shared.dataTask(with: url) {
            [weak self] data, _, _ in
            guard let self, let data, let art = NSImage(data: data) else { return }
            let disc = MenuBandCDArtworkRenderer.disc(from: art, side: self.side)
            DispatchQueue.main.async {
                guard self.representedArtworkURL == url else { return }
                self.baseImage = disc
                self.statusItem.button?.image = disc
            }
        }
        artworkTask?.resume()
    }

    @objc private func handlePlatterEvent() {
        guard let event = NSApp.currentEvent,
              let button = statusItem.button else { return }
        let point = button.convert(event.locationInWindow, from: nil)
        switch event.type {
        case .leftMouseDown:
            mouseDownPoint = point
            lastDragPoint = point
            lastDragTimestamp = event.timestamp
            scratching = false
        case .leftMouseDragged:
            guard let down = mouseDownPoint, let previous = lastDragPoint else { return }
            let totalDistance = hypot(point.x - down.x, point.y - down.y)
            if !scratching, totalDistance >= 1.5 {
                guard onScratchBegin?() == true else { return }
                scratching = true
            }
            guard scratching else { return }
            // Horizontal travel dominates, with a little vertical coupling so
            // a circular finger motion around the tiny disc still feels live.
            let delta = Double((point.x - previous.x) - (point.y - previous.y) * 0.35)
            let elapsed = max(1.0 / 240.0, event.timestamp - lastDragTimestamp)
            let velocity = delta / elapsed
            angle -= CGFloat(delta * 7.5)
            angle.formTruncatingRemainder(dividingBy: 360)
            button.image = rotated(baseImage, by: angle)
            onScratch?(delta, velocity)
            lastDragPoint = point
            lastDragTimestamp = event.timestamp
        case .leftMouseUp:
            let wasScratching = scratching
            finishScratch()
            if !wasScratching { onClick?() }
        default:
            break
        }
    }

    private func finishScratch() {
        if scratching { onScratchEnd?() }
        scratching = false
        mouseDownPoint = nil
        lastDragPoint = nil
        lastDragTimestamp = 0
    }

    private func tick() {
        guard !scratching else { return }
        angle -= 1.5
        if angle <= -360 { angle += 360 }
        statusItem.button?.image = rotated(baseImage, by: angle)
    }

    private func rotated(_ image: NSImage, by degrees: CGFloat) -> NSImage {
        let output = NSImage(size: image.size)
        output.lockFocus()
        let transform = NSAffineTransform()
        transform.translateX(by: image.size.width / 2, yBy: image.size.height / 2)
        transform.rotate(byDegrees: degrees)
        transform.translateX(by: -image.size.width / 2,
                             yBy: -image.size.height / 2)
        transform.concat()
        image.draw(at: .zero, from: NSRect(origin: .zero, size: image.size),
                   operation: .sourceOver, fraction: 1)
        output.unlockFocus()
        output.isTemplate = false
        return output
    }

    deinit {
        finishScratch()
        timer?.invalidate()
        artworkTask?.cancel()
        NSStatusBar.system.removeStatusItem(statusItem)
    }
}
