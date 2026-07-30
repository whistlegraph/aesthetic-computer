import AppKit
import AVFoundation
import JukeDSP

private final class DJDeckPCMState {
    private let lock = NSLock()
    private var samples: [[Float]] = []
    private var positionFrames: Double = 0
    private var playbackRate: Double = 1
    private var scratchTargetFrames: Double?
    private var scratching = false
    private var playing = false
    private var looping = false
    private var lastOutput: [Float] = []
    private var material = ACScratchState()
    private(set) var sampleRate: Double = 44_100

    var duration: Double {
        lock.lock(); defer { lock.unlock() }
        return Double(samples.first?.count ?? 0) / sampleRate
    }
    var currentTime: Double {
        lock.lock(); defer { lock.unlock() }
        return positionFrames / sampleRate
    }
    var isPlaying: Bool {
        lock.lock(); defer { lock.unlock() }
        return playing
    }

    func load(samples: [[Float]], sampleRate: Double, looping: Bool) {
        lock.lock()
        self.samples = samples
        self.sampleRate = sampleRate
        self.looping = looping
        positionFrames = 0
        playbackRate = 1
        scratchTargetFrames = nil
        scratching = false
        playing = false
        lastOutput = [Float](repeating: 0, count: samples.count)
        ac_scratch_init(&material)
        lock.unlock()
    }

    func setPlaying(_ value: Bool) {
        lock.lock()
        if value, positionFrames >= Double(max(0, (samples.first?.count ?? 1) - 1)) { positionFrames = 0 }
        playing = value
        lock.unlock()
    }

    func setRate(_ value: Double) {
        lock.lock(); playbackRate = value; lock.unlock()
    }

    func beginScratch() {
        lock.lock()
        scratching = true
        scratchTargetFrames = positionFrames
        playbackRate = 0
        playing = true
        lock.unlock()
    }

    func seek(seconds: Double) {
        lock.lock()
        let finalFrame = Double(max(0, (samples.first?.count ?? 1) - 1))
        positionFrames = max(0, min(finalFrame, seconds * sampleRate))
        lock.unlock()
    }

    func scratch(positionSeconds: Double, velocity: Double) {
        lock.lock()
        let finalFrame = Double(max(0, (samples.first?.count ?? 1) - 1))
        scratchTargetFrames = max(0, min(finalFrame, positionSeconds * sampleRate))
        playbackRate = max(-6, min(6, velocity))
        playing = true
        lock.unlock()
    }

    func endScratch(normalRate: Double, resume: Bool) {
        lock.lock()
        scratching = false
        scratchTargetFrames = nil
        playbackRate = normalRate
        playing = resume
        lock.unlock()
    }

    func render(frameCount: AVAudioFrameCount, audioBufferList: UnsafeMutablePointer<AudioBufferList>) -> OSStatus {
        let outputs = UnsafeMutableAudioBufferListPointer(audioBufferList)
        for buffer in outputs {
            guard let data = buffer.mData else { continue }
            memset(data, 0, Int(buffer.mDataByteSize))
        }

        lock.lock()
        defer { lock.unlock() }
        guard !samples.isEmpty else { return noErr }
        let total = samples[0].count
        guard total > 1 else { return noErr }

        for frame in 0..<Int(frameCount) {
            if !playing {
                for channel in 0..<min(outputs.count, lastOutput.count) {
                    lastOutput[channel] *= 0.90
                    guard let data = outputs[channel].mData else { continue }
                    data.assumingMemoryBound(to: Float.self)[frame] = lastOutput[channel]
                }
                continue
            }
            if positionFrames < 0 || positionFrames >= Double(total - 1) {
                if looping {
                    let length = Double(total - 1)
                    positionFrames = positionFrames.truncatingRemainder(dividingBy: length)
                    if positionFrames < 0 { positionFrames += length }
                } else {
                    playing = false
                    break
                }
            }
            let error = (scratchTargetFrames ?? positionFrames) - positionFrames
            let motion = ac_scratch_motion(&material, playbackRate, error,
                                           scratching ? 1 : 0, sampleRate)
            // A stopped hand emits silence; movement resumes from the same
            // groove with a short slew instead of a discontinuous seek.
            if abs(motion) >= 0.002 {
                let lower = Int(positionFrames)
                let upper = min(total - 1, lower + 1)
                let fraction = Float(positionFrames - Double(lower))
                for channel in 0..<min(outputs.count, samples.count) {
                    guard let data = outputs[channel].mData else { continue }
                    let pointer = data.assumingMemoryBound(to: Float.self)
                    let raw = ac_scratch_cubic(
                        samples[channel][max(0, lower - 1)], samples[channel][lower],
                        samples[channel][upper], samples[channel][min(total - 1, lower + 2)], fraction)
                    lastOutput[channel] = ac_scratch_material(
                        &material, raw, Int32(channel), positionFrames, motion, scratching ? 1 : 0)
                    pointer[frame] = lastOutput[channel]
                }
            }
            positionFrames += motion
        }
        return noErr
    }
}

// Each deck renders directly from decoded PCM. Platter velocity can therefore
// drive positive or negative sample motion without a seek/restart round trip.
final class DJDeckPlayer: NSObject {
    private(set) var track: Track?
    private var engine = AVAudioEngine()
    private var sourceNode: AVAudioSourceNode?
    private var pcm = DJDeckPCMState()
    private var resumeAfterScratch = false
    private(set) var sourceBPM: Double = 120
    private(set) var targetBPM: Double = 120
    private(set) var gain: Float = 1
    var onStateChange: (() -> Void)?

    var duration: Double { pcm.duration }
    var currentTime: Double { pcm.currentTime }
    var isPlaying: Bool { pcm.isPlaying }
    var rate: Double { sourceBPM > 0 ? targetBPM / sourceBPM : 1 }

    func load(_ track: Track) {
        engine.stop()
        sourceNode = nil
        self.track = track
        sourceBPM = Double(track.meta?.bpm ?? 120)
        targetBPM = sourceBPM
        guard let file = try? AVAudioFile(forReading: track.url) else { return }
        let format = file.processingFormat
        let capacity = AVAudioFrameCount(file.length)
        guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: capacity),
              (try? file.read(into: buffer)) != nil,
              let channelData = buffer.floatChannelData else { return }
        let frameCount = Int(buffer.frameLength)
        let channels = (0..<Int(format.channelCount)).map { channel in
            Array(UnsafeBufferPointer(start: channelData[channel], count: frameCount))
        }
        pcm = DJDeckPCMState()
        pcm.load(samples: channels, sampleRate: format.sampleRate, looping: track.lane == "practice")

        engine = AVAudioEngine()
        let state = pcm
        let node = AVAudioSourceNode(format: format) { _, _, frameCount, audioBufferList -> OSStatus in
            state.render(frameCount: frameCount, audioBufferList: audioBufferList)
        }
        sourceNode = node
        engine.attach(node)
        engine.connect(node, to: engine.mainMixerNode, format: format)
        engine.mainMixerNode.outputVolume = gain
        engine.prepare()
        try? engine.start()
        applyRate()
        onStateChange?()
    }

    func toggle() { isPlaying ? pause() : play() }

    func play() {
        guard sourceNode != nil else { return }
        if !engine.isRunning { try? engine.start() }
        pcm.setRate(rate)
        pcm.setPlaying(true)
        onStateChange?()
    }

    func pause() {
        pcm.setPlaying(false)
        onStateChange?()
    }

    func seek(to time: Double) {
        pcm.seek(seconds: max(0, min(duration, time)))
    }

    func beginScratch() {
        resumeAfterScratch = isPlaying
        pcm.beginScratch()
    }

    func scratch(to time: Double, movement: Double, elapsed: Double) {
        let velocity = elapsed > 0 ? movement / elapsed : 0
        pcm.scratch(positionSeconds: time, velocity: velocity)
    }

    func endScratch() {
        pcm.endScratch(normalRate: rate, resume: resumeAfterScratch)
        onStateChange?()
    }

    func holdScratch() { pcm.setRate(0) }

    func setBPM(_ bpm: Double) {
        targetBPM = max(sourceBPM * 0.5, min(sourceBPM * 1.5, bpm))
        applyRate()
        onStateChange?()
    }

    func resetBPM() { setBPM(sourceBPM) }

    func setRate(_ value: Double) {
        targetBPM = sourceBPM * max(0.5, min(1.5, value))
        applyRate()
        onStateChange?()
    }

    func setGain(_ value: Float) {
        gain = max(0, min(1, value))
        engine.mainMixerNode.outputVolume = gain
    }

    private func applyRate() {
        pcm.setRate(max(0.5, min(2, rate)))
    }
}

enum DJPracticeTracks {
    private static let sampleRate = 48_000.0
    private static let bpm = 120

    static func make() -> [Track] {
        let specs = [
            ("Practice · Sine Kicks + Hats", 0, "practice-sine-kicks-hats-v2.wav"),
            ("Practice · Waves + Claps", 1, "practice-waves-claps-v2.wav")
        ]
        return specs.compactMap { name, variant, filename in
            guard let url = render(name: filename, variant: variant) else { return nil }
            let track = Track(url: url, lane: "practice", title: name)
            track.meta = TrackMeta(artist: "JukeWizard", backend: "C synthesis", status: "PRACTICE",
                                   updated: nil, revisions: nil, bytes: nil, durationSec: 16,
                                   bpm: bpm, key: variant == 0 ? "A1" : "A minor",
                                   releaseDate: nil, art: nil, media: nil, links: nil)
            return track
        }
    }

    private static func render(name: String, variant: Int) -> URL? {
        let fm = FileManager.default
        guard let base = fm.urls(for: .cachesDirectory, in: .userDomainMask).first else { return nil }
        let directory = base.appendingPathComponent("computer.aesthetic.jukewizard", isDirectory: true)
        try? fm.createDirectory(at: directory, withIntermediateDirectories: true)
        let url = directory.appendingPathComponent(name)
        if fm.fileExists(atPath: url.path) { return url }

        let frames = Int(sampleRate * 16)
        var left = [Float](repeating: 0, count: frames)
        var right = [Float](repeating: 0, count: frames)
        left.withUnsafeMutableBufferPointer { l in
            right.withUnsafeMutableBufferPointer { r in
                ac_practice_render(Int32(variant), l.baseAddress, r.baseAddress, frames, sampleRate, Double(bpm))
            }
        }
        guard let format = AVAudioFormat(commonFormat: .pcmFormatFloat32, sampleRate: sampleRate,
                                         channels: 2, interleaved: false),
              let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: AVAudioFrameCount(frames)),
              let channels = buffer.floatChannelData else { return nil }
        buffer.frameLength = AVAudioFrameCount(frames)
        left.withUnsafeBufferPointer { channels[0].update(from: $0.baseAddress!, count: frames) }
        right.withUnsafeBufferPointer { channels[1].update(from: $0.baseAddress!, count: frames) }
        do {
            var fileSettings = format.settings
            fileSettings[AVLinearPCMIsNonInterleaved] = false
            let file = try AVAudioFile(forWriting: url, settings: fileSettings)
            try file.write(from: buffer)
            return url
        } catch {
            try? fm.removeItem(at: url)
            return nil
        }
    }
}

final class DJPlatterView: NSView {
    weak var deck: DJDeckPlayer?
    var accent: NSColor = Palette.teal
    var deckName = "A"
    private var lastAngle: CGFloat?
    private var lastTimestamp: TimeInterval?
    private var scratchOrigin: Double = 0
    private var scratchOffset: Double = 0
    private var scratchIdleTimer: Timer?

    override var acceptsFirstResponder: Bool { true }
    override var mouseDownCanMoveWindow: Bool { false }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        setAccessibilityRole(.slider)
        setAccessibilityHelp("Drag the record clockwise or counterclockwise to scratch the track")
    }
    required init?(coder: NSCoder) { fatalError() }
    deinit { scratchIdleTimer?.invalidate() }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        trackingAreas.forEach(removeTrackingArea)
        addTrackingArea(NSTrackingArea(rect: bounds, options: [.activeInKeyWindow, .cursorUpdate], owner: self))
    }

    override func cursorUpdate(with event: NSEvent) { NSCursor.openHand.set() }

    private var center: NSPoint { NSPoint(x: bounds.midX, y: bounds.midY) }
    private var radius: CGFloat { max(1, min(bounds.width, bounds.height) / 2 - 5) }

    override func draw(_ dirtyRect: NSRect) {
        guard let context = NSGraphicsContext.current?.cgContext else { return }
        let c = center, r = radius

        context.saveGState()
        let shadow = NSShadow()
        shadow.shadowColor = NSColor.black.withAlphaComponent(0.55)
        shadow.shadowBlurRadius = 10
        shadow.shadowOffset = NSSize(width: 0, height: -3)
        shadow.set()
        NSColor(white: 0.025, alpha: 1).setFill()
        NSBezierPath(ovalIn: NSRect(x: c.x - r, y: c.y - r, width: r * 2, height: r * 2)).fill()
        context.restoreGState()

        for groove in stride(from: r * 0.34, through: r * 0.93, by: max(3, r * 0.038)) {
            NSColor(white: 0.20, alpha: 0.55).setStroke()
            let path = NSBezierPath(ovalIn: NSRect(x: c.x - groove, y: c.y - groove,
                                                   width: groove * 2, height: groove * 2))
            path.lineWidth = 0.65
            path.stroke()
        }

        let labelR = r * 0.29
        accent.withAlphaComponent(0.90).setFill()
        NSBezierPath(ovalIn: NSRect(x: c.x - labelR, y: c.y - labelR,
                                   width: labelR * 2, height: labelR * 2)).fill()
        Palette.gold.setFill()
        NSBezierPath(ovalIn: NSRect(x: c.x - 4, y: c.y - 4, width: 8, height: 8)).fill()

        let seconds = deck?.currentTime ?? 0
        let angle = CGFloat(-seconds / 1.8 * Double.pi * 2) + .pi / 2
        let marker = NSBezierPath()
        marker.move(to: NSPoint(x: c.x + cos(angle) * r * 0.42,
                                y: c.y + sin(angle) * r * 0.42))
        marker.line(to: NSPoint(x: c.x + cos(angle) * r * 0.88,
                                y: c.y + sin(angle) * r * 0.88))
        accent.setStroke()
        marker.lineWidth = max(2, r * 0.025)
        marker.lineCapStyle = .round
        marker.stroke()

        if let deck, deck.duration > 0 {
            let progress = max(0, min(1, deck.currentTime / deck.duration))
            let ring = NSBezierPath()
            ring.appendArc(withCenter: c, radius: r - 2, startAngle: 90,
                           endAngle: 90 - CGFloat(progress) * 360, clockwise: true)
            Palette.gold.setStroke()
            ring.lineWidth = 3
            ring.stroke()
        }

        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: max(15, r * 0.20), weight: .black),
            .foregroundColor: NSColor.white
        ]
        let name = deckName as NSString
        let size = name.size(withAttributes: attrs)
        name.draw(at: NSPoint(x: c.x - size.width / 2, y: c.y - size.height / 2), withAttributes: attrs)
    }

    private func angle(for event: NSEvent) -> CGFloat {
        let point = convert(event.locationInWindow, from: nil)
        return atan2(point.y - center.y, point.x - center.x)
    }

    override func mouseDown(with event: NSEvent) {
        guard hypot(convert(event.locationInWindow, from: nil).x - center.x,
                    convert(event.locationInWindow, from: nil).y - center.y) <= radius else { return }
        window?.makeFirstResponder(self)
        NSCursor.closedHand.set()
        lastAngle = angle(for: event)
        lastTimestamp = event.timestamp
        scratchOrigin = deck?.currentTime ?? 0
        scratchOffset = 0
        deck?.beginScratch()
        scratchIdleTimer?.invalidate()
        scratchIdleTimer = Timer.scheduledTimer(withTimeInterval: 0.02, repeats: true) { [weak self] _ in
            guard let self, let timestamp = self.lastTimestamp else { return }
            if ProcessInfo.processInfo.systemUptime - timestamp > 0.04 { self.deck?.holdScratch() }
        }
    }

    override func mouseDragged(with event: NSEvent) {
        guard let prior = lastAngle else { return }
        let next = angle(for: event)
        var delta = next - prior
        if delta > .pi { delta -= .pi * 2 }
        if delta < -.pi { delta += .pi * 2 }
        // At 33⅓ RPM a full turn is 1.8 seconds. Clockwise advances.
        let seconds = Double(-delta / (.pi * 2)) * 1.8
        let elapsed = max(1.0 / 240.0, event.timestamp - (lastTimestamp ?? event.timestamp))
        scratchOffset += seconds
        deck?.scratch(to: scratchOrigin + scratchOffset, movement: seconds, elapsed: elapsed)
        lastAngle = next
        lastTimestamp = event.timestamp
        needsDisplay = true
    }

    override func mouseUp(with event: NSEvent) {
        lastAngle = nil
        lastTimestamp = nil
        scratchIdleTimer?.invalidate()
        scratchIdleTimer = nil
        deck?.endScratch()
        NSCursor.openHand.set()
    }
}

// A fixed output-time window makes the two rows directly comparable. When a
// deck's rate changes, its source waveform expands or contracts so matched
// beats occupy the same horizontal distance on both rows.
final class DJWaveformOutputView: NSView {
    weak var deck: DJDeckPlayer?
    var accent: NSColor = Palette.teal
    var deckName = "A"
    private var peaks: [Float] = []
    private var peakDuration: Double = 0
    private var loadToken = 0
    private var lastX: CGFloat?
    private var lastTimestamp: TimeInterval?
    private var scratchOrigin: Double = 0
    private var scratchOffset: Double = 0
    private var scratchIdleTimer: Timer?

    override var mouseDownCanMoveWindow: Bool { false }
    private var visibleSourceSpan: Double { 16.0 * (deck?.rate ?? 1) }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        layer?.cornerRadius = 7
        layer?.masksToBounds = true
        setAccessibilityRole(.slider)
        setAccessibilityHelp("Drag left or right to scratch this output waveform")
    }
    required init?(coder: NSCoder) { fatalError() }
    deinit { scratchIdleTimer?.invalidate() }

    override func updateTrackingAreas() {
        super.updateTrackingAreas()
        trackingAreas.forEach(removeTrackingArea)
        addTrackingArea(NSTrackingArea(rect: bounds, options: [.activeInKeyWindow, .cursorUpdate], owner: self))
    }
    override func cursorUpdate(with event: NSEvent) { NSCursor.resizeLeftRight.set() }

    func load(_ track: Track) {
        loadToken += 1
        let token = loadToken
        peaks = []
        peakDuration = 0
        needsDisplay = true
        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            guard let file = try? AVAudioFile(forReading: track.url) else { return }
            let format = file.processingFormat
            let frames = AVAudioFrameCount(file.length)
            guard frames > 0,
                  let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: frames),
                  (try? file.read(into: buffer)) != nil,
                  let channels = buffer.floatChannelData else { return }
            let frameCount = Int(buffer.frameLength)
            let channelCount = Int(format.channelCount)
            let bins = 2400
            let framesPerBin = max(1, frameCount / bins)
            var output = [Float](repeating: 0, count: bins)
            for bin in 0..<bins {
                let start = bin * framesPerBin
                let end = min(frameCount, start + framesPerBin)
                var peak: Float = 0
                var frame = start
                while frame < end {
                    var sample: Float = 0
                    for channel in 0..<channelCount { sample += abs(channels[channel][frame]) }
                    peak = max(peak, sample / Float(channelCount))
                    frame += 1
                }
                output[bin] = peak
            }
            let maximum = output.max() ?? 1
            if maximum > 0 { output = output.map { $0 / maximum } }
            let duration = Double(file.length) / file.processingFormat.sampleRate
            DispatchQueue.main.async {
                guard let self, token == self.loadToken else { return }
                self.peaks = output
                self.peakDuration = duration
                self.needsDisplay = true
            }
        }
    }

    override func draw(_ dirtyRect: NSRect) {
        NSColor(white: 0.035, alpha: 1).setFill()
        bounds.fill()
        guard let deck else { return }
        let centerX = bounds.midX
        let current = deck.currentTime
        let sourceSpan = visibleSourceSpan
        let startTime = current - sourceSpan / 2
        let secondsPerPoint = sourceSpan / Double(max(1, bounds.width))

        if !peaks.isEmpty, peakDuration > 0 {
            let mid = bounds.midY
            accent.setFill()
            var x: CGFloat = 0
            while x < bounds.width {
                let time = startTime + Double(x) * secondsPerPoint
                if time >= 0, time <= peakDuration {
                    let index = min(peaks.count - 1, max(0, Int(time / peakDuration * Double(peaks.count))))
                    let amplitude = CGFloat(peaks[index]) * (mid - 4)
                    NSBezierPath(rect: NSRect(x: x, y: mid - amplitude,
                                              width: 1.5, height: amplitude * 2)).fill()
                }
                x += 2
            }
        }

        // Beat marks share output-time geometry with the waveform.
        if deck.sourceBPM > 0 {
            let beat = 60.0 / deck.sourceBPM
            var time = floor(startTime / beat) * beat
            while time <= startTime + sourceSpan {
                if time >= 0 {
                    let x = CGFloat((time - startTime) / sourceSpan) * bounds.width
                    NSColor.white.withAlphaComponent(0.18).setStroke()
                    let mark = NSBezierPath()
                    mark.move(to: NSPoint(x: x, y: 0)); mark.line(to: NSPoint(x: x, y: bounds.height))
                    mark.lineWidth = 1; mark.stroke()
                }
                time += beat
            }
        }

        Palette.gold.setStroke()
        let playhead = NSBezierPath()
        playhead.move(to: NSPoint(x: centerX, y: 0))
        playhead.line(to: NSPoint(x: centerX, y: bounds.height))
        playhead.lineWidth = 2
        playhead.stroke()

        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 14, weight: .black),
            .foregroundColor: accent
        ]
        (deckName as NSString).draw(at: NSPoint(x: 8, y: bounds.midY - 9), withAttributes: attrs)
    }

    override func mouseDown(with event: NSEvent) {
        guard let deck else { return }
        window?.makeFirstResponder(self)
        lastX = convert(event.locationInWindow, from: nil).x
        lastTimestamp = event.timestamp
        scratchOrigin = deck.currentTime
        scratchOffset = 0
        deck.beginScratch()
        scratchIdleTimer?.invalidate()
        scratchIdleTimer = Timer.scheduledTimer(withTimeInterval: 0.02, repeats: true) { [weak self] _ in
            guard let self, let timestamp = self.lastTimestamp else { return }
            if ProcessInfo.processInfo.systemUptime - timestamp > 0.04 { self.deck?.holdScratch() }
        }
    }

    override func mouseDragged(with event: NSEvent) {
        guard let deck, let prior = lastX else { return }
        let x = convert(event.locationInWindow, from: nil).x
        // Pulling the printed waveform right pulls the record backward.
        let movement = -Double(x - prior) / Double(max(1, bounds.width)) * visibleSourceSpan
        let elapsed = max(1.0 / 240.0, event.timestamp - (lastTimestamp ?? event.timestamp))
        scratchOffset += movement
        deck.scratch(to: scratchOrigin + scratchOffset, movement: movement, elapsed: elapsed)
        lastX = x
        lastTimestamp = event.timestamp
        needsDisplay = true
    }

    override func mouseUp(with event: NSEvent) {
        lastX = nil
        lastTimestamp = nil
        scratchIdleTimer?.invalidate()
        scratchIdleTimer = nil
        deck?.endScratch()
    }
}

final class DJDeckView: NSView {
    let deck = DJDeckPlayer()
    let platter = DJPlatterView(frame: .zero)
    private let deckLabel: NSTextField
    private let trackPopup = NSPopUpButton(frame: .zero, pullsDown: false)
    private let playButton = NSButton(title: "▶", target: nil, action: nil)
    private let bpmSlider = NSSlider(value: 120, minValue: 60, maxValue: 180, target: nil, action: nil)
    private let bpmLabel = NSTextField(labelWithString: "120.0 BPM")
    private let timeLabel = NSTextField(labelWithString: "0:00 / 0:00")
    private let syncButton = NSButton(title: "SYNC", target: nil, action: nil)
    private let resetButton = NSButton(title: "1×", target: nil, action: nil)
    private var tracks: [Track] = []
    var onStateChange: (() -> Void)?
    var onTrackLoaded: ((Track) -> Void)?
    var onSync: (() -> Void)?

    init(name: String, accent: NSColor) {
        deckLabel = NSTextField(labelWithString: name)
        super.init(frame: .zero)
        wantsLayer = true
        layer?.cornerRadius = 12
        layer?.borderWidth = 1
        layer?.borderColor = accent.withAlphaComponent(0.55).cgColor
        layer?.backgroundColor = NSColor.black.withAlphaComponent(0.18).cgColor

        platter.deck = deck
        platter.accent = accent
        platter.deckName = name
        platter.setAccessibilityLabel("Deck \(name) record")
        deckLabel.font = .systemFont(ofSize: 17, weight: .black)
        deckLabel.textColor = accent
        trackPopup.controlSize = .small
        trackPopup.target = self
        trackPopup.action = #selector(trackChanged)
        playButton.target = self
        playButton.action = #selector(togglePlay)
        playButton.bezelStyle = .rounded
        playButton.contentTintColor = accent
        bpmSlider.target = self
        bpmSlider.action = #selector(bpmChanged)
        bpmSlider.isContinuous = true
        bpmLabel.font = .monospacedDigitSystemFont(ofSize: 14, weight: .bold)
        bpmLabel.alignment = .center
        timeLabel.font = .monospacedDigitSystemFont(ofSize: 12, weight: .medium)
        timeLabel.textColor = .secondaryLabelColor
        syncButton.target = self
        syncButton.action = #selector(sync)
        syncButton.bezelStyle = .rounded
        syncButton.contentTintColor = accent
        syncButton.toolTip = "Match this deck's tempo and beat phase to the other deck"
        resetButton.target = self
        resetButton.action = #selector(resetBPM)
        resetButton.bezelStyle = .rounded
        resetButton.toolTip = "Reset this deck to the track's original tempo"

        [deckLabel, trackPopup, platter, playButton, bpmSlider, bpmLabel,
         timeLabel, syncButton, resetButton].forEach(addSubview)
        deck.onStateChange = { [weak self] in
            self?.refresh()
            self?.onStateChange?()
        }
    }
    required init?(coder: NSCoder) { fatalError() }

    func configure(tracks: [Track], selectedIndex: Int) {
        self.tracks = tracks
        trackPopup.removeAllItems()
        trackPopup.addItems(withTitles: tracks.map { "\($0.title) — \($0.lane)" })
        guard !tracks.isEmpty else { return }
        let index = max(0, min(tracks.count - 1, selectedIndex))
        trackPopup.selectItem(at: index)
        load(index)
    }

    func step(by offset: Int) {
        guard !tracks.isEmpty else { return }
        let next = max(0, min(tracks.count - 1, trackPopup.indexOfSelectedItem + offset))
        trackPopup.selectItem(at: next)
        load(next)
    }

    func refresh() {
        playButton.title = deck.isPlaying ? "❚❚" : "▶"
        bpmLabel.stringValue = String(format: "%.1f BPM", deck.targetBPM)
        bpmSlider.doubleValue = deck.targetBPM
        timeLabel.stringValue = "\(JukeController.mmss(deck.currentTime)) / \(JukeController.mmss(deck.duration))"
        platter.needsDisplay = true
    }

    override func layout() {
        let pad: CGFloat = 12
        deckLabel.frame = NSRect(x: pad, y: bounds.height - 31, width: 24, height: 23)
        trackPopup.frame = NSRect(x: 40, y: bounds.height - 32, width: max(90, bounds.width - 52), height: 24)

        let platterBottom: CGFloat = 91
        let platterTop = bounds.height - 39
        let diameter = max(72, min(bounds.width - pad * 2, platterTop - platterBottom))
        platter.frame = NSRect(x: (bounds.width - diameter) / 2,
                               y: platterBottom + (platterTop - platterBottom - diameter) / 2,
                               width: diameter, height: diameter)

        bpmLabel.frame = NSRect(x: 64, y: 62, width: max(80, bounds.width - 128), height: 18)
        bpmSlider.frame = NSRect(x: 65, y: 38, width: max(70, bounds.width - 130), height: 20)
        playButton.frame = NSRect(x: pad, y: 36, width: 45, height: 27)
        timeLabel.frame = NSRect(x: pad, y: 10, width: max(80, bounds.width - 132), height: 18)
        syncButton.frame = NSRect(x: bounds.width - 112, y: 8, width: 58, height: 24)
        resetButton.frame = NSRect(x: bounds.width - 50, y: 8, width: 38, height: 24)
    }

    @objc private func trackChanged() { load(trackPopup.indexOfSelectedItem) }

    private func load(_ index: Int) {
        guard index >= 0, index < tracks.count else { return }
        deck.load(tracks[index])
        onTrackLoaded?(tracks[index])
        bpmSlider.minValue = deck.sourceBPM * 0.5
        bpmSlider.maxValue = deck.sourceBPM * 1.5
        bpmSlider.doubleValue = deck.targetBPM
        bpmSlider.toolTip = "Tempo: \(Int(bpmSlider.minValue))–\(Int(bpmSlider.maxValue)) BPM"
        refresh()
    }

    @objc private func togglePlay() { deck.toggle() }
    @objc private func bpmChanged() { deck.setBPM(bpmSlider.doubleValue); refresh() }
    @objc private func sync() { onSync?() }
    @objc private func resetBPM() { deck.resetBPM(); refresh() }
}

final class DJMixerView: NSView {
    let deckA = DJDeckView(name: "A", accent: Palette.teal)
    let deckB = DJDeckView(name: "B", accent: Palette.coral)
    private let waveformA = DJWaveformOutputView(frame: .zero)
    private let waveformB = DJWaveformOutputView(frame: .zero)
    private let crossfader = NSSlider(value: 0, minValue: -1, maxValue: 1, target: nil, action: nil)
    private let crossLabel = NSTextField(labelWithString: "A 50  ·  50 B")
    private let practiceButton = NSButton(title: "PRACTICE", target: nil, action: nil)
    private var displayTimer: Timer?
    private var availableTracks: [Track] = []
    private(set) var masterVolume: Float = 0.8
    var onStateChange: (() -> Void)?

    var isPlaying: Bool { deckA.deck.isPlaying || deckB.deck.isPlaying }
    var dominantDeck: DJDeckView { crossfader.doubleValue <= 0 ? deckA : deckB }
    var dominantTitle: String { dominantDeck.deck.track?.title ?? "DJ Mix" }
    var dominantBPM: Double { dominantDeck.deck.targetBPM }

    override init(frame frameRect: NSRect) {
        super.init(frame: frameRect)
        wantsLayer = true
        crossfader.target = self
        crossfader.action = #selector(crossfadeChanged)
        crossfader.isContinuous = true
        crossLabel.font = .monospacedDigitSystemFont(ofSize: 13, weight: .bold)
        crossLabel.alignment = .center
        practiceButton.target = self
        practiceButton.action = #selector(loadPractice)
        practiceButton.bezelStyle = .rounded
        practiceButton.contentTintColor = Palette.gold
        practiceButton.toolTip = "Load the C-synthesized kick, hat, clap, and wave loops"
        waveformA.deck = deckA.deck
        waveformA.accent = Palette.teal
        waveformA.deckName = "A"
        waveformA.setAccessibilityLabel("Deck A output waveform")
        waveformB.deck = deckB.deck
        waveformB.accent = Palette.coral
        waveformB.deckName = "B"
        waveformB.setAccessibilityLabel("Deck B output waveform")
        [deckA, deckB, waveformA, waveformB, crossfader, crossLabel, practiceButton].forEach(addSubview)
        deckA.onStateChange = { [weak self] in self?.onStateChange?() }
        deckB.onStateChange = { [weak self] in self?.onStateChange?() }
        deckA.onTrackLoaded = { [weak waveformA] track in waveformA?.load(track) }
        deckB.onTrackLoaded = { [weak waveformB] track in waveformB?.load(track) }
        deckA.onSync = { [weak self] in self?.sync(self?.deckA, to: self?.deckB) }
        deckB.onSync = { [weak self] in self?.sync(self?.deckB, to: self?.deckA) }
        applyCrossfade()
    }
    required init?(coder: NSCoder) { fatalError() }
    deinit { displayTimer?.invalidate() }

    func configure(tracks: [Track], primaryIndex: Int) {
        let practice = DJPracticeTracks.make()
        availableTracks = practice + tracks
        let requested = primaryIndex + practice.count
        let first = max(0, min(max(0, availableTracks.count - 1), requested))
        let second = availableTracks.count > 1 ? (first + 1) % availableTracks.count : first
        deckA.configure(tracks: availableTracks, selectedIndex: first)
        deckB.configure(tracks: availableTracks, selectedIndex: second)
        crossfader.doubleValue = 0
        applyCrossfade()
    }

    func startDisplay() {
        displayTimer?.invalidate()
        displayTimer = Timer.scheduledTimer(withTimeInterval: 1.0 / 60.0, repeats: true) { [weak self] _ in
            self?.deckA.refresh()
            self?.deckB.refresh()
            self?.waveformA.needsDisplay = true
            self?.waveformB.needsDisplay = true
        }
    }

    func stopDisplay() { displayTimer?.invalidate(); displayTimer = nil }
    func pauseAll() { deckA.deck.pause(); deckB.deck.pause() }
    func toggleDominant() { dominantDeck.deck.toggle() }
    func stepDominant(by offset: Int) { dominantDeck.step(by: offset) }

    func setMasterVolume(_ value: Float) {
        masterVolume = max(0, min(1, value))
        applyCrossfade()
    }

    @objc private func loadPractice() {
        guard availableTracks.count >= 2 else { return }
        deckA.configure(tracks: availableTracks, selectedIndex: 0)
        deckB.configure(tracks: availableTracks, selectedIndex: 1)
        crossfader.doubleValue = 0
        applyCrossfade()
        onStateChange?()
    }

    private func sync(_ target: DJDeckView?, to reference: DJDeckView?) {
        guard let target, let reference else { return }
        let referenceBPM = reference.deck.targetBPM
        let candidates = [referenceBPM / 2, referenceBPM, referenceBPM * 2]
        let tempo = candidates
            .filter { $0 >= target.deck.sourceBPM * 0.5 && $0 <= target.deck.sourceBPM * 1.5 }
            .min { abs(log($0 / target.deck.sourceBPM)) < abs(log($1 / target.deck.sourceBPM)) }
            ?? referenceBPM
        target.deck.setBPM(tempo)

        // File time zero is the initial beat-grid anchor. The nearest target
        // beat is moved onto the reference deck's current output phase.
        let beatDuration = 60.0 / max(1, referenceBPM)
        let referenceOutputTime = reference.deck.currentTime / max(0.01, reference.deck.rate)
        let phase = referenceOutputTime.truncatingRemainder(dividingBy: beatDuration)
        let targetOutputTime = target.deck.currentTime / max(0.01, target.deck.rate)
        let beatIndex = ((targetOutputTime - phase) / beatDuration).rounded()
        let alignedOutputTime = max(0, beatIndex * beatDuration + phase)
        target.deck.seek(to: alignedOutputTime * target.deck.rate)
        if reference.deck.isPlaying { target.deck.play() }
        target.refresh()
        waveformA.needsDisplay = true
        waveformB.needsDisplay = true
        onStateChange?()
    }

    override func layout() {
        let pad: CGFloat = 4
        let gap: CGFloat = 8
        let crossHeight: CGFloat = 52
        let waveHeight: CGFloat = min(104, max(76, bounds.height * 0.22))
        let waveRow = (waveHeight - 4) / 2
        let waveBottom = crossHeight + gap
        waveformB.frame = NSRect(x: pad, y: waveBottom, width: bounds.width - pad * 2, height: waveRow)
        waveformA.frame = NSRect(x: pad, y: waveBottom + waveRow + 4,
                                 width: bounds.width - pad * 2, height: waveRow)
        let width = max(1, (bounds.width - pad * 2 - gap) / 2)
        let deckBottom = waveBottom + waveHeight + gap
        deckA.frame = NSRect(x: pad, y: deckBottom, width: width,
                             height: max(120, bounds.height - deckBottom))
        deckB.frame = NSRect(x: pad + width + gap, y: deckBottom, width: width,
                             height: max(120, bounds.height - deckBottom))
        crossLabel.frame = NSRect(x: bounds.midX - 100, y: 29, width: 200, height: 18)
        practiceButton.frame = NSRect(x: pad, y: 8, width: 86, height: 26)
        crossfader.frame = NSRect(x: max(32, bounds.midX - min(250, bounds.width * 0.32)), y: 6,
                                  width: min(500, bounds.width - 64), height: 20)
    }

    @objc private func crossfadeChanged() {
        applyCrossfade()
        onStateChange?()
    }

    private func applyCrossfade() {
        let position = max(-1, min(1, crossfader.doubleValue))
        let blend = (position + 1) / 2
        let a = cos(blend * .pi / 2)
        let b = sin(blend * .pi / 2)
        deckA.deck.setGain(Float(a) * masterVolume)
        deckB.deck.setGain(Float(b) * masterVolume)
        crossLabel.stringValue = "A \(Int((a * a * 100).rounded()))  ·  \(Int((b * b * 100).rounded())) B"
    }
}
