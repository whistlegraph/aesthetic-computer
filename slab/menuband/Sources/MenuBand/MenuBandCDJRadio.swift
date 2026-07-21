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
    var onClick: (() -> Void)?

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
            button.action = #selector(clicked)
            button.toolTip = "Menu Band CDJ Radio"
        }
        setVisible(false)
    }

    func setVisible(_ visible: Bool) {
        statusItem.isVisible = visible
        if !visible { setPlaying(false) }
    }

    func update(title: String, artworkURL: URL?, playing: Bool) {
        statusItem.button?.toolTip = title.isEmpty
            ? "Menu Band CDJ Radio" : "CDJ Radio · \(title)"
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

    @objc private func clicked() { onClick?() }

    private func tick() {
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
        timer?.invalidate()
        artworkTask?.cancel()
        NSStatusBar.system.removeStatusItem(statusItem)
    }
}
