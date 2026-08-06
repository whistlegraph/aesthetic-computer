import AppKit
import AVFoundation
import JukeDSP

enum DJRunLoopTimer {
    @discardableResult
    static func scheduled(every interval: TimeInterval, repeats: Bool = true,
                          _ block: @escaping (Timer) -> Void) -> Timer {
        let timer = Timer(timeInterval: interval, repeats: repeats, block: block)
        RunLoop.main.add(timer, forMode: .common)
        return timer
    }
}

enum DJPlatterGeometry {
    // A slower visual platter gives each rendered groove enough radial room
    // to carry real waveform detail while preserving exact scratch mapping.
    static let secondsPerRevolution = 3.6
}

enum DJTempoAnalyzer {
    static func estimate(samples: [Float], sampleRate: Double) -> Double? {
        guard sampleRate > 0, samples.count > Int(sampleRate * 4) else { return nil }
        // Keep the onset envelope near 200 Hz. A fixed 512-frame hop made
        // tempo resolution depend heavily on the source sample rate (and could
        // quantize 128 BPM down near 125 BPM).
        let hop = max(32, Int((sampleRate / 200).rounded()))
        let limit = min(samples.count, Int(sampleRate * 90))
        var novelty: [Double] = []
        novelty.reserveCapacity(limit / hop)
        var previousEnergy = 0.0
        var frame = 0
        while frame + hop <= limit {
            var sum = 0.0
            for index in frame..<(frame + hop) {
                let sample = Double(samples[index])
                sum += sample * sample
            }
            let energy = sqrt(sum / Double(hop))
            novelty.append(max(0, energy - previousEnergy * 0.86))
            previousEnergy = energy
            frame += hop
        }
        guard novelty.count > 64 else { return nil }

        let stepsPerSecond = sampleRate / Double(hop)
        var bestLag = 0
        var bestScore = 0.0
        let minimumLag = max(2, Int((60 / 180 * stepsPerSecond).rounded()))
        let maximumLag = min(novelty.count / 2 - 1,
                             Int((60 / 70 * stepsPerSecond).rounded()))
        guard minimumLag <= maximumLag else { return nil }
        for lag in minimumLag...maximumLag {
            var score = 0.0
            for index in lag..<novelty.count { score += novelty[index] * novelty[index - lag] }
            // A slight musical-range prior resolves common half-time ties.
            let bpm = 60 * stepsPerSecond / Double(lag)
            if bpm >= 90, bpm <= 150 { score *= 1.06 }
            if score > bestScore { bestScore = score; bestLag = lag }
        }
        guard bestScore > 0.000_001, bestLag > 0 else { return nil }
        return 60 * stepsPerSecond / Double(bestLag)
    }
}

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
    var visualState: (motion: Double, energy: Float) {
        lock.lock(); defer { lock.unlock() }
        let energy = lastOutput.isEmpty
            ? 0
            : lastOutput.reduce(Float(0)) { $0 + abs($1) } / Float(lastOutput.count)
        return (material.velocity, min(1, energy * 2.5))
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
        let finalFrame = Double(max(1, (samples.first?.count ?? 1) - 1))
        var target = seconds * sampleRate
        if looping {
            target = target.truncatingRemainder(dividingBy: finalFrame)
            if target < 0 { target += finalFrame }
            positionFrames = target
        } else {
            positionFrames = max(0, min(finalFrame, target))
        }
        lock.unlock()
    }

    func scratch(positionSeconds: Double, velocity: Double) {
        lock.lock()
        let finalFrame = Double(max(1, (samples.first?.count ?? 1) - 1))
        var target = positionSeconds * sampleRate
        if looping {
            target = target.truncatingRemainder(dividingBy: finalFrame)
            if target < 0 { target += finalFrame }
        } else {
            target = max(0, min(finalFrame, target))
        }
        scratchTargetFrames = target
        playbackRate = velocity.isFinite ? velocity : 0
        playing = true
        lock.unlock()
    }

    func endScratch(normalRate: Double, resume: Bool) {
        lock.lock()
        if let target = scratchTargetFrames { positionFrames = target }
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
            var error = (scratchTargetFrames ?? positionFrames) - positionFrames
            if looping, scratching {
                let length = Double(total - 1)
                if error > length / 2 { error -= length }
                if error < -length / 2 { error += length }
            }
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
            } else {
                // A caught record decays into silence instead of dropping the
                // output buffer to zero in one sample (the old audible click).
                for channel in 0..<min(outputs.count, lastOutput.count) {
                    lastOutput[channel] *= 0.995
                    guard let data = outputs[channel].mData else { continue }
                    data.assumingMemoryBound(to: Float.self)[frame] = lastOutput[channel]
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
    private var pitchNode: AVAudioUnitTimePitch?
    private var pcm = DJDeckPCMState()
    private var resumeAfterScratch = false
    private(set) var sourceBPM: Double = 120
    private(set) var targetBPM: Double = 120
    private(set) var bpmAnalyzed = false
    private(set) var gain: Float = 1
    private(set) var pitchSemitones: Double = 0
    private(set) var motorEnabled = false
    var onStateChange: (() -> Void)?

    var duration: Double { pcm.duration }
    var currentTime: Double { pcm.currentTime }
    var isPlaying: Bool { motorEnabled }
    var visualState: (motion: Double, energy: Float) { pcm.visualState }
    var rate: Double { sourceBPM > 0 ? targetBPM / sourceBPM : 1 }

    func load(_ track: Track) {
        engine.stop()
        sourceNode = nil
        pitchNode = nil
        self.track = track
        let suppliedBPM = track.meta?.bpm
        sourceBPM = Double(suppliedBPM ?? 120)
        targetBPM = sourceBPM
        pitchSemitones = 0
        motorEnabled = false
        bpmAnalyzed = false
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
        pcm.load(samples: channels, sampleRate: format.sampleRate,
                 looping: track.lane == "practice" || track.lane == "primpats")

        if suppliedBPM == nil, let analysisChannel = channels.first {
            let trackURL = track.url
            DispatchQueue.global(qos: .utility).async { [weak self] in
                guard let estimate = DJTempoAnalyzer.estimate(samples: analysisChannel,
                                                               sampleRate: format.sampleRate) else { return }
                DispatchQueue.main.async {
                    guard let self, self.track?.url == trackURL else { return }
                    self.sourceBPM = estimate
                    self.targetBPM = estimate
                    self.bpmAnalyzed = true
                    self.applyRate()
                    self.onStateChange?()
                }
            }
        }

        engine = AVAudioEngine()
        let state = pcm
        let node = AVAudioSourceNode(format: format) { _, _, frameCount, audioBufferList -> OSStatus in
            state.render(frameCount: frameCount, audioBufferList: audioBufferList)
        }
        let pitch = AVAudioUnitTimePitch()
        pitch.pitch = 0
        sourceNode = node
        pitchNode = pitch
        engine.attach(node)
        engine.attach(pitch)
        engine.connect(node, to: pitch, format: format)
        engine.connect(pitch, to: engine.mainMixerNode, format: format)
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
        motorEnabled = true
        pcm.setRate(rate)
        pcm.setPlaying(true)
        onStateChange?()
    }

    func pause() {
        motorEnabled = false
        // A motor-off deck remains available to the hand; zero transport
        // velocity is silence until the platter is pushed or thrown.
        pcm.setRate(0)
        pcm.setPlaying(true)
        onStateChange?()
    }

    func seek(to time: Double) {
        pcm.seek(seconds: max(0, min(duration, time)))
    }

    func beginScratch() {
        resumeAfterScratch = motorEnabled
        pcm.beginScratch()
    }

    func scratch(to time: Double, movement: Double, elapsed: Double) {
        let velocity = elapsed > 0 ? movement / elapsed : 0
        pcm.scratch(positionSeconds: time, velocity: velocity)
    }

    func endScratch(momentum: Double? = nil) {
        let releaseRate = momentum ?? (motorEnabled ? rate : 0)
        pcm.endScratch(normalRate: releaseRate,
                       resume: resumeAfterScratch || abs(releaseRate) >= 0.002)
        onStateChange?()
    }

    func holdScratch() { pcm.setRate(0) }

    /// Touch-brake multiplier used by the floating record. The underlying
    /// musical/BPM rate remains unchanged and can be restored without a seek.
    func setTransportScale(_ scale: Double) {
        pcm.setRate(rate * max(0, min(1, scale)))
    }

    func restoreTransportRate() { applyRate() }

    func setTransportVelocity(_ velocity: Double) {
        guard velocity.isFinite else { return }
        pcm.setRate(velocity)
        pcm.setPlaying(true)
    }

    func setBPM(_ bpm: Double) {
        targetBPM = max(sourceBPM * 0.5, min(sourceBPM * 2.0, bpm))
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

    func setPitchSemitones(_ value: Double) {
        pitchSemitones = max(-12, min(12, value))
        pitchNode?.pitch = Float(pitchSemitones * 100)
        onStateChange?()
    }

    private func applyRate() {
        pcm.setRate(motorEnabled ? max(0.5, min(2, rate)) : 0)
    }
}

enum DJPracticeTracks {
    private static let sampleRate = 48_000.0
    private static let bpm = 120
    private static let duration = 32.0

    static func make() -> [Track] {
        let specs = [
            ("Primpats · Sine Kick", 0, "primpat-sine-kick-v4.wav", "A1"),
            ("Primpats · Closed Hat", 1, "primpat-closed-hat-v4.wav", "noise"),
            ("Primpats · Clap", 2, "primpat-clap-v4.wav", "noise"),
            ("Primpats · Wave Bass", 3, "primpat-wave-bass-v4.wav", "A1")
        ]
        return specs.compactMap { name, variant, filename, key in
            guard let url = render(name: filename, variant: variant) else { return nil }
            let track = Track(url: url, lane: "practice", title: name)
            track.meta = TrackMeta(artist: "Menu Band Juke", backend: "C synthesis", status: "PRACTICE",
                                   updated: nil, revisions: nil, bytes: nil, durationSec: duration,
                                   bpm: bpm, key: key,
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

        let frames = Int(sampleRate * duration)
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
    var canDetachRecord = true
    var onDetachRequested: ((NSPoint) -> Void)?
    private var lastAngle: CGFloat?
    private var lastTimestamp: TimeInterval?
    private var scratchOrigin: Double = 0
    private var scratchOffset: Double = 0
    private var scratchIdleTimer: Timer?
    private var dragStartScreen: NSPoint?
    private var detachGestureResolved = false
    private var detachDeniedUntil: TimeInterval = 0

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

        guard deck?.track != nil else {
            NSColor.black.withAlphaComponent(0.08).setFill()
            NSBezierPath(ovalIn: NSRect(x: c.x - r, y: c.y - r,
                                        width: r * 2, height: r * 2)).fill()
            let bed = NSBezierPath(ovalIn: NSRect(x: c.x - r + 2, y: c.y - r + 2,
                                                  width: r * 2 - 4, height: r * 2 - 4))
            bed.setLineDash([7, 7], count: 2, phase: 0)
            bed.lineWidth = 2
            accent.withAlphaComponent(0.42).setStroke()
            bed.stroke()
            let empty = "EMPTY" as NSString
            let attrs: [NSAttributedString.Key: Any] = [
                .font: NSFont.systemFont(ofSize: max(13, r * 0.12), weight: .bold),
                .foregroundColor: accent.withAlphaComponent(0.62)
            ]
            let size = empty.size(withAttributes: attrs)
            empty.draw(at: NSPoint(x: c.x - size.width / 2, y: c.y - size.height / 2),
                       withAttributes: attrs)
            return
        }

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
        let angle = CGFloat(-seconds / DJPlatterGeometry.secondsPerRevolution * Double.pi * 2) + .pi / 2
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

        if ProcessInfo.processInfo.systemUptime < detachDeniedUntil {
            Palette.coral.withAlphaComponent(0.9).setStroke()
            let denied = NSBezierPath(ovalIn: NSRect(x: c.x - r + 2, y: c.y - r + 2,
                                                     width: r * 2 - 4, height: r * 2 - 4))
            denied.lineWidth = 5
            denied.stroke()
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
        guard deck?.track != nil else { return }
        guard hypot(convert(event.locationInWindow, from: nil).x - center.x,
                    convert(event.locationInWindow, from: nil).y - center.y) <= radius else { return }
        window?.makeFirstResponder(self)
        NSCursor.closedHand.set()
        lastAngle = angle(for: event)
        lastTimestamp = event.timestamp
        scratchOrigin = deck?.currentTime ?? 0
        scratchOffset = 0
        dragStartScreen = NSEvent.mouseLocation
        detachGestureResolved = false
        deck?.beginScratch()
        scratchIdleTimer?.invalidate()
        scratchIdleTimer = DJRunLoopTimer.scheduled(every: 0.02) { [weak self] _ in
            guard let self, let timestamp = self.lastTimestamp else { return }
            if ProcessInfo.processInfo.systemUptime - timestamp > 0.04 { self.deck?.holdScratch() }
        }
    }

    override func mouseDragged(with event: NSEvent) {
        if !detachGestureResolved, let start = dragStartScreen {
            let screen = NSEvent.mouseLocation
            let travel = hypot(screen.x - start.x, screen.y - start.y)
            let local = convert(event.locationInWindow, from: nil)
            let outsideBed = hypot(local.x - center.x, local.y - center.y) > radius * 1.08
            if travel > max(52, radius * 0.46), outsideBed {
                detachGestureResolved = true
                scratchIdleTimer?.invalidate()
                scratchIdleTimer = nil
                deck?.endScratch()
                lastAngle = nil
                lastTimestamp = nil
                if canDetachRecord {
                    onDetachRequested?(screen)
                } else {
                    detachDeniedUntil = ProcessInfo.processInfo.systemUptime + 0.42
                    NSSound.beep()
                    needsDisplay = true
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.45) { [weak self] in
                        self?.needsDisplay = true
                    }
                }
                return
            }
        }
        guard !detachGestureResolved else { return }
        guard let prior = lastAngle else { return }
        let next = angle(for: event)
        var delta = next - prior
        if delta > .pi { delta -= .pi * 2 }
        if delta < -.pi { delta += .pi * 2 }
        let seconds = Double(-delta / (.pi * 2)) * DJPlatterGeometry.secondsPerRevolution
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
        if !detachGestureResolved { deck?.endScratch() }
        dragStartScreen = nil
        detachGestureResolved = false
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
    var vertical = false
    private var peaks: [Float] = []
    private var peakDuration: Double = 0
    private var loadToken = 0
    private var lastX: CGFloat?
    private var lastTimestamp: TimeInterval?
    private var scratchOrigin: Double = 0
    private var scratchOffset: Double = 0
    private var scratchIdleTimer: Timer?
    private var cachedDark: Bool?
    private var cachedSurface = NSColor.clear
    private var cachedInk = NSColor.clear

    override var mouseDownCanMoveWindow: Bool { false }
    private var visibleSourceSpan: Double { 12.0 * (deck?.rate ?? 1) }

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

    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        cachedDark = nil
        needsDisplay = true
    }

    private func updateColors(dark: Bool) {
        guard cachedDark != dark else { return }
        cachedDark = dark
        cachedSurface = Palette.deckSurface(accent, dark: dark)
        cachedInk = Palette.deckInk(accent, dark: dark)
    }

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

    func clear() {
        loadToken += 1
        peaks = []
        peakDuration = 0
        needsDisplay = true
    }

    override func draw(_ dirtyRect: NSRect) {
        let dark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        updateColors(dark: dark)
        guard let context = NSGraphicsContext.current?.cgContext else { return }
        context.setFillColor(cachedSurface.cgColor)
        context.fill(bounds)
        guard let deck else { return }
        let centerX = bounds.midX
        let centerY = bounds.midY
        let current = deck.currentTime
        let sourceSpan = visibleSourceSpan
        let startTime = current - sourceSpan / 2
        let axisLength = vertical ? bounds.height : bounds.width
        let secondsPerPoint = sourceSpan / Double(max(1, axisLength))

        if !peaks.isEmpty, peakDuration > 0 {
            let mid = vertical ? bounds.midX : bounds.midY
            var bars: [CGRect] = []
            bars.reserveCapacity(Int(axisLength / 2) + 1)
            var axis: CGFloat = 0
            while axis < axisLength {
                let time = startTime + Double(axis) * secondsPerPoint
                if time >= 0, time <= peakDuration {
                    let index = min(peaks.count - 1, max(0, Int(time / peakDuration * Double(peaks.count))))
                    let amplitude = CGFloat(peaks[index]) * (mid - 4)
                    let rect = vertical
                        ? NSRect(x: mid - amplitude, y: axis, width: amplitude * 2, height: 1.5)
                        : NSRect(x: axis, y: mid - amplitude, width: 1.5, height: amplitude * 2)
                    bars.append(rect)
                }
                axis += 2
            }
            context.setFillColor(accent.cgColor)
            context.fill(bars)
        }

        // Beat marks share output-time geometry with the waveform.
        if deck.sourceBPM > 0 {
            let beat = 60.0 / deck.sourceBPM
            var time = floor(startTime / beat) * beat
            let marks = CGMutablePath()
            while time <= startTime + sourceSpan {
                if time >= 0 {
                    let axis = CGFloat((time - startTime) / sourceSpan) * axisLength
                    if vertical {
                        marks.move(to: CGPoint(x: 0, y: axis)); marks.addLine(to: CGPoint(x: bounds.width, y: axis))
                    } else {
                        marks.move(to: CGPoint(x: axis, y: 0)); marks.addLine(to: CGPoint(x: axis, y: bounds.height))
                    }
                }
                time += beat
            }
            context.addPath(marks)
            context.setStrokeColor(cachedInk.withAlphaComponent(0.16).cgColor)
            context.setLineWidth(1)
            context.strokePath()
        }

        context.setFillColor(Palette.gold.cgColor)
        context.fill(vertical
            ? CGRect(x: 0, y: centerY - 1, width: bounds.width, height: 2)
            : CGRect(x: centerX - 1, y: 0, width: 2, height: bounds.height))
    }

    override func mouseDown(with event: NSEvent) {
        guard let deck else { return }
        window?.makeFirstResponder(self)
        let point = convert(event.locationInWindow, from: nil)
        lastX = vertical ? point.y : point.x
        lastTimestamp = event.timestamp
        scratchOrigin = deck.currentTime
        scratchOffset = 0
        deck.beginScratch()
        scratchIdleTimer?.invalidate()
        scratchIdleTimer = DJRunLoopTimer.scheduled(every: 0.02) { [weak self] _ in
            guard let self, let timestamp = self.lastTimestamp else { return }
            if ProcessInfo.processInfo.systemUptime - timestamp > 0.04 { self.deck?.holdScratch() }
        }
    }

    override func mouseDragged(with event: NSEvent) {
        guard let deck, let prior = lastX else { return }
        let point = convert(event.locationInWindow, from: nil)
        let x = vertical ? point.y : point.x
        // Pulling the printed waveform right pulls the record backward.
        let dimension = vertical ? bounds.height : bounds.width
        let movement = -Double(x - prior) / Double(max(1, dimension)) * visibleSourceSpan
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

final class DJAlignmentSurface: NSView {
    let strips: [DJWaveformOutputView]
    private let decks: [DJDeckPlayer]
    private let pitches: [NSSlider]
    private let volumes: [NSSlider]
    private let bpmLabels: [NSTextField]
    private let trackPickers: [NSPopUpButton]
    private let rateLabels: [NSTextField]
    private let volumeLabels: [NSTextField]
    private let rateButtons: [NSSegmentedControl]
    private let playButtons: [NSButton]
    private var recordChoices: [Track] = []
    private let syncButton = NSButton(title: "SYNC RATES", target: nil, action: nil)
    private let alignButton = NSButton(title: "ALIGN PEAKS", target: nil, action: nil)
    var onSyncRates: (() -> Void)?
    var onAlignPeaks: (() -> Void)?
    var onChooseTrack: ((Int, Track) -> Void)?

    init(decks: [DJDeckPlayer], names: [String], accents: [NSColor]) {
        self.decks = decks
        strips = decks.indices.map { index in
            let strip = DJWaveformOutputView(frame: .zero)
            strip.deck = decks[index]
            strip.accent = accents[index]
            strip.deckName = names[index]
            strip.vertical = true
            return strip
        }
        pitches = decks.indices.map { _ in
            NSSlider(value: 0, minValue: -12, maxValue: 12, target: nil, action: nil)
        }
        volumes = decks.indices.map { _ in
            NSSlider(value: 0.5, minValue: 0, maxValue: 1, target: nil, action: nil)
        }
        bpmLabels = decks.indices.map { _ in NSTextField(labelWithString: "120.0 BPM") }
        trackPickers = decks.indices.map { _ in NSPopUpButton(frame: .zero, pullsDown: false) }
        rateLabels = decks.indices.map { _ in NSTextField(labelWithString: "rate") }
        volumeLabels = decks.indices.map { _ in NSTextField(labelWithString: "vol") }
        rateButtons = decks.indices.map { _ in
            NSSegmentedControl(labels: ["½×", "1×", "2×"], trackingMode: .selectOne,
                               target: nil, action: nil)
        }
        playButtons = decks.indices.map { _ in NSButton(title: "▶", target: nil, action: nil) }
        super.init(frame: .zero)
        wantsLayer = true
        layer?.cornerRadius = 18
        layer?.masksToBounds = true
        for index in decks.indices {
            pitches[index].tag = index
            pitches[index].target = self
            pitches[index].action = #selector(pitchChanged(_:))
            pitches[index].isContinuous = true
            pitches[index].isVertical = false
            pitches[index].numberOfTickMarks = 25
            pitches[index].allowsTickMarkValuesOnly = false
            pitches[index].toolTip = "Deck \(names[index]) pitch, independent of rate"
            volumes[index].tag = index
            volumes[index].target = self
            volumes[index].action = #selector(volumeChanged(_:))
            volumes[index].isContinuous = true
            volumes[index].isVertical = false
            volumes[index].toolTip = "Deck \(names[index]) volume"
            bpmLabels[index].font = .monospacedDigitSystemFont(ofSize: 12, weight: .bold)
            bpmLabels[index].textColor = accents[index]
            trackPickers[index].tag = index
            trackPickers[index].target = self
            trackPickers[index].action = #selector(trackChanged(_:))
            trackPickers[index].controlSize = .small
            trackPickers[index].font = .systemFont(ofSize: 11, weight: .semibold)
            for label in [rateLabels[index], volumeLabels[index]] {
                label.font = .systemFont(ofSize: 10, weight: .medium)
                label.textColor = .secondaryLabelColor
            }
            rateButtons[index].tag = index
            rateButtons[index].target = self
            rateButtons[index].action = #selector(rateButtonChanged(_:))
            rateButtons[index].selectedSegment = 1
            rateButtons[index].controlSize = .small
            playButtons[index].tag = index
            playButtons[index].target = self
            playButtons[index].action = #selector(playChanged(_:))
            playButtons[index].bezelStyle = .inline
            playButtons[index].contentTintColor = accents[index]
            addSubview(strips[index])
            addSubview(pitches[index])
            addSubview(volumes[index])
            addSubview(bpmLabels[index])
            addSubview(trackPickers[index])
            addSubview(rateLabels[index])
            addSubview(volumeLabels[index])
            addSubview(rateButtons[index])
            addSubview(playButtons[index])
        }
        syncButton.target = self
        syncButton.action = #selector(sync)
        syncButton.bezelStyle = .rounded
        syncButton.contentTintColor = Palette.gold
        syncButton.isHidden = decks.count < 2
        alignButton.target = self
        alignButton.action = #selector(align)
        alignButton.bezelStyle = .rounded
        alignButton.contentTintColor = Palette.teal
        alignButton.isHidden = decks.count < 2
        addSubview(syncButton)
        addSubview(alignButton)
        setAccessibilityLabel("Deck alignment read strips")
    }

    required init?(coder: NSCoder) { fatalError() }

    override func draw(_ dirtyRect: NSRect) {
        let dark = effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        (dark ? NSColor(white: 0.02, alpha: 0.88)
              : Palette.cream.withAlphaComponent(0.94)).setFill()
        bounds.fill()

        let channelTop = bounds.height - 52
        let channelBottom: CGFloat = 112
        let width = bounds.width / CGFloat(max(1, strips.count))
        for index in strips.indices {
            Palette.deckSurface(strips[index].accent, dark: dark).setFill()
            NSRect(x: CGFloat(index) * width, y: channelBottom,
                   width: width, height: max(0, channelTop - channelBottom)).fill()
        }
    }

    override func layout() {
        let pad: CGFloat = 12
        let footer: CGFloat = 40
        let channelBottom: CGFloat = 112
        let column = bounds.width / CGFloat(max(1, strips.count))
        for index in strips.indices {
            let x = CGFloat(index) * column
            trackPickers[index].frame = NSRect(x: x + 2, y: bounds.height - 31,
                                               width: max(38, column - 33), height: 24)
            bpmLabels[index].frame = NSRect(x: x + 6, y: bounds.height - 49,
                                            width: max(30, column - 12), height: 16)
            playButtons[index].frame = NSRect(x: x + column - 29, y: bounds.height - 31,
                                              width: 25, height: 23)
            rateButtons[index].frame = NSRect(x: x + 4, y: footer + 3,
                                              width: max(36, column - 8), height: 24)
            let stripTop = bounds.height - 52
            strips[index].frame = NSRect(x: x, y: channelBottom,
                                         width: column, height: stripTop - channelBottom)
            rateLabels[index].stringValue = "P"
            rateLabels[index].alignment = .center
            rateLabels[index].frame = NSRect(x: x + 2, y: 90, width: 14, height: 14)
            pitches[index].frame = NSRect(x: x + 16, y: 88,
                                          width: max(24, column - 20), height: 17)
            volumeLabels[index].stringValue = "V"
            volumeLabels[index].alignment = .center
            volumeLabels[index].frame = NSRect(x: x + 2, y: 69, width: 14, height: 14)
            volumes[index].frame = NSRect(x: x + 16, y: 67,
                                          width: max(24, column - 20), height: 17)
        }
        syncButton.frame = NSRect(x: bounds.midX - 127, y: pad + 1, width: 120, height: 28)
        alignButton.frame = NSRect(x: bounds.midX + 7, y: pad + 1, width: 120, height: 28)
    }

    func refresh() {
        for index in decks.indices {
            pitches[index].doubleValue = decks[index].pitchSemitones
            volumes[index].doubleValue = Double(decks[index].gain)
            playButtons[index].title = decks[index].isPlaying ? "Ⅱ" : "▶"
            let rate = decks[index].rate
            rateButtons[index].selectedSegment = abs(rate - 0.5) < 0.01 ? 0
                : abs(rate - 1) < 0.01 ? 1
                : abs(rate - 2) < 0.01 ? 2 : -1
            rateLabels[index].toolTip = String(format: "Pitch %+.1f semitones",
                                                decks[index].pitchSemitones)
            bpmLabels[index].stringValue = String(format: "%@%.1f BPM",
                                                   decks[index].bpmAnalyzed ? "≈" : "",
                                                   decks[index].targetBPM)
        }
    }

    func setRecordChoices(_ tracks: [Track]) {
        recordChoices = tracks
        let titles = tracks.map {
            $0.title
                .replacingOccurrences(of: "Primpats · ", with: "")
                .replacingOccurrences(of: "Practice · ", with: "")
        }
        for index in trackPickers.indices {
            trackPickers[index].removeAllItems()
            trackPickers[index].addItems(withTitles: titles)
            if let url = decks[index].track?.url,
               let selected = tracks.firstIndex(where: { $0.url == url }) {
                trackPickers[index].selectItem(at: selected)
            }
        }
    }

    func load(_ track: Track, at index: Int) {
        guard strips.indices.contains(index) else { return }
        strips[index].load(track)
        if let selected = recordChoices.firstIndex(where: { $0.url == track.url }) {
            trackPickers[index].selectItem(at: selected)
        }
    }

    @objc private func trackChanged(_ sender: NSPopUpButton) {
        guard decks.indices.contains(sender.tag),
              recordChoices.indices.contains(sender.indexOfSelectedItem) else { return }
        onChooseTrack?(sender.tag, recordChoices[sender.indexOfSelectedItem])
    }

    @objc private func pitchChanged(_ sender: NSSlider) {
        guard decks.indices.contains(sender.tag) else { return }
        decks[sender.tag].setPitchSemitones(sender.doubleValue)
        rateLabels[sender.tag].toolTip = String(format: "Pitch %+.1f semitones",
                                                sender.doubleValue)
    }
    @objc private func volumeChanged(_ sender: NSSlider) {
        guard decks.indices.contains(sender.tag) else { return }
        decks[sender.tag].setGain(Float(sender.doubleValue))
    }
    @objc private func rateButtonChanged(_ sender: NSSegmentedControl) {
        guard decks.indices.contains(sender.tag) else { return }
        let multiplier = [0.5, 1.0, 2.0][max(0, sender.selectedSegment)]
        decks[sender.tag].setBPM(decks[sender.tag].sourceBPM * multiplier)
        refresh()
    }
    @objc private func playChanged(_ sender: NSButton) {
        guard decks.indices.contains(sender.tag) else { return }
        decks[sender.tag].toggle()
        refresh()
    }
    @objc private func sync() { onSyncRates?(); refresh() }
    @objc private func align() { onAlignPeaks?(); refresh() }
}

final class DJAlignmentWindowController: NSWindowController, NSWindowDelegate {
    private let surface: DJAlignmentSurface
    private var displayTimer: Timer?
    private var positioned = false
    private var displayFrame = 0

    init(decks: [DJDeckPlayer], names: [String], accents: [NSColor]) {
        surface = DJAlignmentSurface(decks: decks, names: names, accents: accents)
        let solo = decks.count == 1
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: solo ? 150 : 300, height: 400),
            styleMask: [.titled, .closable, .resizable],
            backing: .buffered, defer: false)
        window.title = "Menu Band Juke · Alignment"
        window.titleVisibility = .hidden
        window.titlebarAppearsTransparent = true
        window.isMovableByWindowBackground = true
        window.level = .floating
        window.collectionBehavior = [.fullScreenAuxiliary, .moveToActiveSpace]
        window.minSize = NSSize(width: solo ? 120 : 260, height: 340)
        window.backgroundColor = .clear
        window.isOpaque = false
        window.hasShadow = true
        window.contentView = surface
        super.init(window: window)
        window.delegate = self
    }

    required init?(coder: NSCoder) { fatalError() }
    deinit { displayTimer?.invalidate() }

    func show(tracks: [Track?]) {
        for (index, track) in tracks.enumerated() {
            if let track { surface.load(track, at: index) }
        }
        if !positioned, let window, let screen = NSScreen.main {
            positioned = true
            let visible = screen.visibleFrame
            window.setFrameOrigin(NSPoint(x: visible.midX - window.frame.width / 2,
                                          y: visible.midY - window.frame.height / 2))
        }
        showWindow(nil)
        window?.orderFrontRegardless()
        displayTimer?.invalidate()
        let timer = DJRunLoopTimer.scheduled(every: 1.0 / 60.0) { [weak self] _ in
            guard let self else { return }
            self.surface.strips.forEach { $0.needsDisplay = true }
            self.displayFrame &+= 1
            if self.displayFrame.isMultiple(of: 4) { self.surface.refresh() }
        }
        displayTimer = timer
    }

    func setSyncActions(rates: @escaping () -> Void, peaks: @escaping () -> Void) {
        surface.onSyncRates = rates
        surface.onAlignPeaks = peaks
    }

    func trackChanged(_ track: Track, at index: Int) { surface.load(track, at: index) }

    func setRecordChoices(_ tracks: [Track], onChoose: @escaping (Int, Track) -> Void) {
        surface.onChooseTrack = onChoose
        surface.setRecordChoices(tracks)
    }

    func windowWillClose(_ notification: Notification) {
        displayTimer?.invalidate()
        displayTimer = nil
    }
}

final class DJDeckView: NSView {
    private(set) var deck = DJDeckPlayer()
    let platter = DJPlatterView(frame: .zero)
    private let deckLabel: NSTextField
    private let trackPopup = NSPopUpButton(frame: .zero, pullsDown: false)
    private let playButton = NSButton(title: "▶", target: nil, action: nil)
    private let bpmSlider = NSSlider(value: 120, minValue: 60, maxValue: 180, target: nil, action: nil)
    private let bpmLabel = NSTextField(labelWithString: "120.0 BPM")
    private let timeLabel = NSTextField(labelWithString: "0:00 / 0:00")
    private let syncButton = NSButton(title: "SYNC", target: nil, action: nil)
    private let resetButton = NSButton(title: "1×", target: nil, action: nil)
    private let popoutButton = NSButton(title: "↗", target: nil, action: nil)
    private var tracks: [Track] = []
    var onStateChange: (() -> Void)?
    var onTrackLoaded: ((Track) -> Void)?
    var onSync: (() -> Void)?
    var onDetach: ((DJDeckPlayer, Track, NSPoint) -> Void)?
    var canDetachRecord = true {
        didSet {
            platter.canDetachRecord = canDetachRecord
            popoutButton.isHidden = !canDetachRecord || deck.track == nil
            platter.setAccessibilityHelp(canDetachRecord
                ? "Turn the record to scratch, or pull it off the bed to float it"
                : "Turn the record to scratch. This source must stay on the main deck")
        }
    }

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
        popoutButton.target = self
        popoutButton.action = #selector(popout)
        popoutButton.bezelStyle = .rounded
        popoutButton.contentTintColor = accent
        popoutButton.toolTip = "Pull this record off the bed into a floating deck"
        platter.onDetachRequested = { [weak self] point in _ = self?.detachRecord(at: point) }

        [deckLabel, trackPopup, platter, playButton, bpmSlider, bpmLabel,
         timeLabel, syncButton, resetButton, popoutButton].forEach(addSubview)
        installDeckCallback()
    }

    private func installDeckCallback() {
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

    func select(_ track: Track, autoplay: Bool) {
        guard let index = tracks.firstIndex(where: { $0.url == track.url }) else { return }
        trackPopup.selectItem(at: index)
        load(index)
        if autoplay { deck.play() }
    }

    func load(_ track: Track, autoplay: Bool) {
        if let index = tracks.firstIndex(where: { $0.url == track.url }) {
            trackPopup.selectItem(at: index)
        } else {
            tracks.append(track)
            trackPopup.addItem(withTitle: "\(track.title) — \(track.lane)")
            trackPopup.selectItem(at: tracks.count - 1)
        }
        deck.load(track)
        onTrackLoaded?(track)
        bpmSlider.minValue = deck.sourceBPM * 0.5
        bpmSlider.maxValue = deck.sourceBPM * 1.5
        if autoplay { deck.play() }
        refresh()
    }

    func refresh() {
        let hasTrack = deck.track != nil
        playButton.title = deck.isPlaying ? "❚❚" : "▶"
        bpmLabel.stringValue = hasTrack ? String(format: "%.1f BPM", deck.targetBPM) : "— BPM"
        bpmSlider.doubleValue = deck.targetBPM
        timeLabel.stringValue = "\(JukeController.mmss(deck.currentTime)) / \(JukeController.mmss(deck.duration))"
        playButton.isEnabled = hasTrack
        bpmSlider.isEnabled = hasTrack
        syncButton.isEnabled = hasTrack
        resetButton.isEnabled = hasTrack
        popoutButton.isHidden = !canDetachRecord || !hasTrack
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
        timeLabel.frame = NSRect(x: pad, y: 10, width: max(70, bounds.width - 170), height: 18)
        popoutButton.frame = NSRect(x: bounds.width - 150, y: 8, width: 34, height: 24)
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
    @objc private func popout() {
        let point = window.map { NSPoint(x: $0.frame.midX, y: $0.frame.midY) } ?? NSEvent.mouseLocation
        _ = detachRecord(at: point)
    }

    @discardableResult
    func detachRecord(at point: NSPoint) -> Bool {
        guard canDetachRecord, let track = deck.track else { return false }
        let floatingDeck = deck
        floatingDeck.endScratch()
        floatingDeck.onStateChange = nil
        deck = DJDeckPlayer()
        platter.deck = deck
        installDeckCallback()
        trackPopup.selectItem(at: -1)
        refresh()
        onDetach?(floatingDeck, track, point)
        return true
    }
}

/// One compact channel in the main-window mixer for a record that has been
/// pulled into its own window. Floating records remain playable directly;
/// this strip keeps their transport and level reachable from the selector.
final class DJDetachedChannelView: NSView {
    weak var controller: DJPopoutDeckController?
    private let playButton = NSButton(title: "❚❚", target: nil, action: nil)
    private let titleLabel = NSTextField(labelWithString: "")
    private let levelSlider = NSSlider(value: 1, minValue: 0, maxValue: 1,
                                       target: nil, action: nil)

    init(controller: DJPopoutDeckController) {
        self.controller = controller
        super.init(frame: .zero)
        wantsLayer = true
        layer?.cornerRadius = 7
        layer?.backgroundColor = NSColor.black.withAlphaComponent(0.14).cgColor
        titleLabel.font = .systemFont(ofSize: 10, weight: .semibold)
        titleLabel.lineBreakMode = .byTruncatingTail
        titleLabel.stringValue = controller.trackTitle
        playButton.bezelStyle = .inline
        playButton.target = self
        playButton.action = #selector(toggle)
        playButton.toolTip = "Play or pause this floating record"
        levelSlider.controlSize = .mini
        levelSlider.isContinuous = true
        levelSlider.target = self
        levelSlider.action = #selector(levelChanged)
        levelSlider.toolTip = "Floating record level"
        [titleLabel, playButton, levelSlider].forEach(addSubview)
        refresh()
    }
    required init?(coder: NSCoder) { fatalError() }

    override func layout() {
        playButton.frame = NSRect(x: 4, y: 4, width: 30, height: bounds.height - 8)
        titleLabel.frame = NSRect(x: 38, y: bounds.height - 17,
                                  width: max(30, bounds.width - 44), height: 14)
        levelSlider.frame = NSRect(x: 38, y: 2, width: max(30, bounds.width - 44), height: 14)
    }

    func refresh() { playButton.title = controller?.isPlaying == true ? "❚❚" : "▶" }
    @objc private func toggle() { controller?.toggle(); refresh() }
    @objc private func levelChanged() { controller?.setChannelGain(levelSlider.floatValue) }
}

final class DJMixerView: NSView {
    let deckA = DJDeckView(name: "A", accent: Palette.teal)
    let deckB = DJDeckView(name: "B", accent: Palette.coral)
    private let deckC = DJDeckPlayer()
    private let deckD = DJDeckPlayer()
    private let waveformA = DJWaveformOutputView(frame: .zero)
    private let waveformB = DJWaveformOutputView(frame: .zero)
    private let crossfader = NSSlider(value: 0, minValue: -1, maxValue: 1, target: nil, action: nil)
    private let crossLabel = NSTextField(labelWithString: "A 50  ·  50 B")
    private let practiceButton = NSButton(title: "PRIMPATS", target: nil, action: nil)
    private let detachedScroll = NSScrollView(frame: .zero)
    private let detachedRack = NSStackView(frame: .zero)
    private var displayTimer: Timer?
    private var availableTracks: [Track] = []
    private var primpatCount = 0
    private var practiceStartIndex = 0
    private var practiceCount = 0
    private var soloMode = false
    private var popoutA: DJPopoutDeckController?
    private var popoutB: DJPopoutDeckController?
    private var popoutC: DJPopoutDeckController?
    private var popoutD: DJPopoutDeckController?
    private var alignmentPopout: DJAlignmentWindowController?
    private var rateSyncTimer: Timer?
    private var peakAlignTimer: Timer?
    private var detachedDecks: [DJPopoutDeckController] = []
    private var detachedChannels: [DJDetachedChannelView] = []
    private var detachedSerial = 0
    private(set) var masterVolume: Float = 0.8
    var onStateChange: (() -> Void)?
    var onDetach: (() -> Void)?
    private var deckAppearance: NSAppearance?

    var isPlaying: Bool {
        deckA.deck.isPlaying || deckB.deck.isPlaying || deckC.isPlaying || deckD.isPlaying
            || detachedDecks.contains(where: { $0.isPlaying })
    }
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
        practiceButton.toolTip = "Load four one-voice primitive records"
        waveformA.deck = deckA.deck
        waveformA.accent = Palette.teal
        waveformA.deckName = "A"
        waveformA.setAccessibilityLabel("Deck A output waveform")
        waveformB.deck = deckB.deck
        waveformB.accent = Palette.coral
        waveformB.deckName = "B"
        waveformB.setAccessibilityLabel("Deck B output waveform")
        detachedRack.orientation = .horizontal
        detachedRack.alignment = .centerY
        detachedRack.spacing = 6
        detachedScroll.documentView = detachedRack
        detachedScroll.hasHorizontalScroller = true
        detachedScroll.hasVerticalScroller = false
        detachedScroll.autohidesScrollers = true
        detachedScroll.scrollerStyle = .overlay
        detachedScroll.drawsBackground = false
        detachedScroll.borderType = .noBorder
        detachedScroll.isHidden = true
        [deckA, deckB, waveformA, waveformB, crossfader, crossLabel,
         practiceButton, detachedScroll].forEach(addSubview)
        deckA.onStateChange = { [weak self] in self?.onStateChange?() }
        deckB.onStateChange = { [weak self] in self?.onStateChange?() }
        deckA.onTrackLoaded = { [weak self] track in
            self?.waveformA.load(track)
            self?.popoutA?.trackChanged(track)
            self?.alignmentPopout?.trackChanged(track, at: 0)
        }
        deckB.onTrackLoaded = { [weak self] track in
            self?.waveformB.load(track)
            self?.popoutB?.trackChanged(track)
            self?.alignmentPopout?.trackChanged(track, at: 1)
        }
        deckA.onSync = { [weak self] in self?.sync(self?.deckA, to: self?.deckB) }
        deckB.onSync = { [weak self] in self?.sync(self?.deckB, to: self?.deckA) }
        deckA.onDetach = { [weak self] deck, track, point in
            self?.waveformA.deck = self?.deckA.deck
            self?.waveformA.clear()
            self?.float(deck: deck, track: track, near: point)
        }
        deckB.onDetach = { [weak self] deck, track, point in
            self?.waveformB.deck = self?.deckB.deck
            self?.waveformB.clear()
            self?.float(deck: deck, track: track, near: point)
        }
        applyCrossfade()
    }
    required init?(coder: NSCoder) { fatalError() }
    deinit {
        displayTimer?.invalidate()
        rateSyncTimer?.invalidate()
        peakAlignTimer?.invalidate()
    }

    func configure(tracks: [Track], primaryIndex: Int) {
        soloMode = false
        updateSoloVisibility()
        let primpats = DJPrimpats.makeTracks()
        let practice = DJPracticeTracks.make()
        primpatCount = primpats.count
        practiceStartIndex = primpats.count
        practiceCount = practice.count
        availableTracks = primpats + practice + tracks
        let requested = primaryIndex + primpatCount + practiceCount
        let first = max(0, min(max(0, availableTracks.count - 1), requested))
        let second = availableTracks.count > 1 ? (first + 1) % availableTracks.count : first
        deckA.configure(tracks: availableTracks, selectedIndex: first)
        deckB.configure(tracks: availableTracks, selectedIndex: second)
        crossfader.doubleValue = 0
        applyCrossfade()
    }

    func configureSolo(tracks: [Track], primaryIndex: Int) {
        soloMode = true
        updateSoloVisibility()
        let primpats = DJPrimpats.makeTracks()
        let practice = DJPracticeTracks.make()
        primpatCount = primpats.count
        practiceStartIndex = primpats.count
        practiceCount = practice.count
        availableTracks = primpats + practice + tracks
        let requested = primaryIndex + primpatCount + practiceCount
        let first = max(0, min(max(0, availableTracks.count - 1), requested))
        deckA.configure(tracks: availableTracks, selectedIndex: first)
        crossfader.doubleValue = -1
        applyCrossfade()
    }

    func startDisplay() {
        displayTimer?.invalidate()
        displayTimer = DJRunLoopTimer.scheduled(every: 1.0 / 60.0) { [weak self] _ in
            guard let self else { return }
            self.deckA.refresh()
            self.waveformA.needsDisplay = true
            if !self.soloMode {
                self.deckB.refresh()
                self.waveformB.needsDisplay = true
            }
            self.detachedChannels.forEach { $0.refresh() }
        }
    }

    func stopDisplay() { displayTimer?.invalidate(); displayTimer = nil }
    func pauseAll() {
        deckA.deck.pause(); deckB.deck.pause(); deckC.pause(); deckD.pause()
        detachedDecks.forEach { $0.pause() }
    }
    func toggleDominant() { dominantDeck.deck.toggle() }
    func stepDominant(by offset: Int) { dominantDeck.step(by: offset) }
    func loadPrimary(_ track: Track, autoplay: Bool = true) { deckA.load(track, autoplay: autoplay) }
    @discardableResult
    func detachPrimary() -> Bool {
        let point = window.map { NSPoint(x: $0.frame.midX, y: $0.frame.midY) } ?? NSEvent.mouseLocation
        return deckA.detachRecord(at: point)
    }
    func setRecordDetachmentAllowed(_ allowed: Bool) {
        deckA.canDetachRecord = allowed
        deckB.canDetachRecord = allowed
    }

    func setMasterVolume(_ value: Float) {
        masterVolume = max(0, min(1, value))
        applyCrossfade()
        detachedDecks.forEach { $0.setMasterGain(masterVolume) }
    }

    func setAppearance(_ appearance: NSAppearance?) {
        deckAppearance = appearance
        popoutA?.window?.appearance = appearance
        popoutB?.window?.appearance = appearance
        popoutC?.window?.appearance = appearance
        popoutD?.window?.appearance = appearance
        alignmentPopout?.window?.appearance = appearance
        detachedDecks.forEach { $0.window?.appearance = appearance }
        popoutA?.window?.contentView?.needsDisplay = true
        popoutB?.window?.contentView?.needsDisplay = true
        popoutC?.window?.contentView?.needsDisplay = true
        popoutD?.window?.contentView?.needsDisplay = true
        alignmentPopout?.window?.contentView?.needsDisplay = true
    }

    private func updateSoloVisibility() {
        deckB.isHidden = soloMode
        waveformB.isHidden = soloMode
        crossfader.isHidden = soloMode
        crossLabel.isHidden = soloMode
    }

    private func float(deck: DJDeckPlayer, track: Track, near point: NSPoint) {
        detachedSerial += 1
        let controller = DJPopoutDeckController(
            deck: deck, name: String(detachedSerial), accent: Palette.teal)
        controller.setMasterGain(masterVolume)
        controller.window?.appearance = deckAppearance
        controller.trackChanged(track)
        let channel = DJDetachedChannelView(controller: controller)
        channel.translatesAutoresizingMaskIntoConstraints = false
        channel.widthAnchor.constraint(equalToConstant: 148).isActive = true
        channel.heightAnchor.constraint(equalToConstant: 38).isActive = true
        controller.onStateChange = { [weak self, weak channel] in
            channel?.refresh()
            self?.onStateChange?()
        }
        controller.onClose = { [weak self, weak controller] in
            guard let self, let controller else { return }
            self.detachedDecks.removeAll { $0 === controller }
            if let index = self.detachedChannels.firstIndex(where: { $0.controller === controller }) {
                let channel = self.detachedChannels.remove(at: index)
                self.detachedRack.removeArrangedSubview(channel)
                channel.removeFromSuperview()
            }
            self.detachedScroll.isHidden = self.detachedChannels.isEmpty
            self.needsLayout = true
            self.onStateChange?()
        }
        detachedDecks.append(controller)
        detachedChannels.append(channel)
        detachedRack.addArrangedSubview(channel)
        detachedScroll.isHidden = false
        controller.show(track: track, near: point)
        needsLayout = true
        onStateChange?()
    }

    @objc func loadPractice() { loadPrimpats() }

    func loadLibraryRecords(openPopouts: Bool = false) {
        let first = primpatCount + practiceCount
        guard availableTracks.indices.contains(first) else { return }
        let libraryCount = availableTracks.count - first
        deckA.configure(tracks: availableTracks, selectedIndex: first)
        deckB.configure(tracks: availableTracks,
                        selectedIndex: min(first + 1, availableTracks.count - 1))
        deckC.load(availableTracks[min(first + 2, availableTracks.count - 1)])
        deckD.load(availableTracks[min(first + 3, availableTracks.count - 1)])
        crossfader.doubleValue = 0
        applyCrossfade()
        if openPopouts {
            showPopoutA()
            if libraryCount > 1 { showPopoutB() }
            if libraryCount > 2 { showPopoutC() }
            if libraryCount > 3 { showPopoutD() }
            showAlignmentPopout()
            onDetach?()
        }
        onStateChange?()
    }

    func loadPrimpats(openPopouts: Bool = false) {
        guard primpatCount >= 8 else { return }
        deckA.configure(tracks: availableTracks, selectedIndex: 0)
        deckB.configure(tracks: availableTracks, selectedIndex: 2)
        deckC.load(availableTracks[4])
        deckD.load(availableTracks[7])
        crossfader.doubleValue = 0
        applyCrossfade()
        if openPopouts {
            showPopoutA()
            showPopoutB()
            showPopoutC()
            showPopoutD()
            showAlignmentPopout()
            onDetach?()
        }
        onStateChange?()
    }

    func loadBeats(openPopouts: Bool = false, autoplay: Bool = false, solo: Bool = false) {
        guard practiceCount >= 4 else { return }
        deckA.configure(tracks: availableTracks, selectedIndex: practiceStartIndex)
        if !solo {
            deckB.configure(tracks: availableTracks, selectedIndex: practiceStartIndex + 1)
            deckC.load(availableTracks[practiceStartIndex + 2])
            deckD.load(availableTracks[practiceStartIndex + 3])
        }
        crossfader.doubleValue = solo ? -1 : 0
        applyCrossfade()
        if autoplay {
            deckA.deck.play()
            if !solo {
                deckB.deck.play()
                deckC.play()
                deckD.play()
            }
        }
        if openPopouts {
            showPopoutA()
            if solo {
                showSoloAlignmentPopout()
            } else {
                showPopoutB()
                showPopoutC()
                showPopoutD()
                showAlignmentPopout()
            }
            onDetach?()
        }
        onStateChange?()
    }

    private func showPopoutA() {
        if popoutA == nil {
            popoutA = DJPopoutDeckController(deck: deckA.deck, name: "A", accent: Palette.teal)
        }
        popoutA?.window?.appearance = deckAppearance
        popoutA?.show(track: deckA.deck.track)
    }

    private func showPopoutB() {
        if popoutB == nil {
            popoutB = DJPopoutDeckController(deck: deckB.deck, name: "B", accent: Palette.coral)
        }
        popoutB?.window?.appearance = deckAppearance
        popoutB?.show(track: deckB.deck.track)
    }

    private func showPopoutC() {
        if popoutC == nil {
            popoutC = DJPopoutDeckController(deck: deckC, name: "C", accent: Palette.gold)
        }
        popoutC?.window?.appearance = deckAppearance
        popoutC?.show(track: deckC.track)
    }

    private func showPopoutD() {
        if popoutD == nil {
            popoutD = DJPopoutDeckController(deck: deckD, name: "D", accent: .systemPurple)
        }
        popoutD?.window?.appearance = deckAppearance
        popoutD?.show(track: deckD.track)
    }

    private func showAlignmentPopout() {
        if alignmentPopout == nil {
            alignmentPopout = DJAlignmentWindowController(
                decks: [deckA.deck, deckB.deck, deckC, deckD],
                names: ["A", "B", "C", "D"],
                accents: [Palette.teal, Palette.coral, Palette.gold, .systemPurple])
            alignmentPopout?.setSyncActions(
                rates: { [weak self] in
                    guard let self else { return }
                    let decks = self.audibleDecks
                    guard let reference = decks.first, decks.count > 1 else { return }
                    self.slideRates(Array(decks.dropFirst()), to: reference)
                },
                peaks: { [weak self] in
                    guard let self else { return }
                    let decks = self.audibleDecks
                    guard let reference = decks.first, decks.count > 1 else { return }
                    self.alignPeaks(Array(decks.dropFirst()), to: reference)
                })
        }
        alignmentPopout?.setRecordChoices(availableTracks) { [weak self] index, track in
            self?.replaceTrack(at: index, with: track)
        }
        alignmentPopout?.window?.appearance = deckAppearance
        alignmentPopout?.show(tracks: [deckA.deck.track, deckB.deck.track, deckC.track, deckD.track])
    }

    private func showSoloAlignmentPopout() {
        alignmentPopout = DJAlignmentWindowController(
            decks: [deckA.deck], names: ["A"], accents: [Palette.teal])
        let primitiveEnd = min(availableTracks.count, practiceStartIndex + practiceCount)
        let primitiveTracks = Array(availableTracks.prefix(primitiveEnd))
        alignmentPopout?.setRecordChoices(primitiveTracks) { [weak self] index, track in
            self?.replaceTrack(at: index, with: track)
        }
        alignmentPopout?.window?.appearance = deckAppearance
        alignmentPopout?.show(tracks: [deckA.deck.track])
    }

    private func replaceTrack(at index: Int, with track: Track) {
        let decks = [deckA.deck, deckB.deck, deckC, deckD]
        guard decks.indices.contains(index) else { return }
        let autoplay = decks[index].motorEnabled
        switch index {
        case 0:
            deckA.select(track, autoplay: autoplay)
        case 1:
            deckB.select(track, autoplay: autoplay)
        case 2:
            deckC.load(track)
            if autoplay { deckC.play() }
            popoutC?.trackChanged(track)
            alignmentPopout?.trackChanged(track, at: 2)
        default:
            deckD.load(track)
            if autoplay { deckD.play() }
            popoutD?.trackChanged(track)
            alignmentPopout?.trackChanged(track, at: 3)
        }
        applyCrossfade()
        onStateChange?()
    }

    private func sync(_ target: DJDeckView?, to reference: DJDeckView?) {
        guard let target, let reference else { return }
        slideRates([target.deck], to: reference.deck)
    }

    private var audibleDecks: [DJDeckPlayer] {
        [deckA.deck, deckB.deck, deckC, deckD].filter {
            $0.motorEnabled && $0.gain > 0.0001
        }
    }

    private func matchedTempo(for target: DJDeckPlayer, referenceBPM: Double) -> Double {
        let candidates = [referenceBPM / 2, referenceBPM, referenceBPM * 2]
        return candidates
            .filter { $0 >= target.sourceBPM * 0.5 && $0 <= target.sourceBPM * 2.0 }
            .min { abs(log($0 / target.sourceBPM)) < abs(log($1 / target.sourceBPM)) }
            ?? referenceBPM
    }

    private func slideRates(_ targets: [DJDeckPlayer], to reference: DJDeckPlayer) {
        rateSyncTimer?.invalidate()
        guard reference.motorEnabled, reference.gain > 0.0001 else { return }
        let targets = targets.filter { $0.motorEnabled && $0.gain > 0.0001 }
        guard !targets.isEmpty else { return }
        let starts = targets.map(\.targetBPM)
        let destinations = targets.map { matchedTempo(for: $0, referenceBPM: reference.targetBPM) }
        let began = ProcessInfo.processInfo.systemUptime
        let duration = 0.65
        rateSyncTimer = DJRunLoopTimer.scheduled(every: 1.0 / 30.0) { [weak self] timer in
            let raw = min(1, (ProcessInfo.processInfo.systemUptime - began) / duration)
            let eased = raw * raw * (3 - 2 * raw)
            for index in targets.indices {
                targets[index].setBPM(starts[index] + (destinations[index] - starts[index]) * eased)
            }
            if raw >= 1 {
                timer.invalidate()
                self?.rateSyncTimer = nil
            }
        }
    }

    private func alignPeaks(_ targets: [DJDeckPlayer], to reference: DJDeckPlayer) {
        peakAlignTimer?.invalidate()
        guard reference.motorEnabled, reference.gain > 0.0001 else { return }
        let targets = targets.filter { $0.motorEnabled && $0.gain > 0.0001 }
        guard !targets.isEmpty else { return }
        let beat = 60 / max(1, reference.targetBPM)
        let referenceOutput = reference.currentTime / max(0.01, reference.rate)
        let referencePhase = referenceOutput.truncatingRemainder(dividingBy: beat)
        let corrections = targets.map { target -> Double in
            let output = target.currentTime / max(0.01, target.rate)
            let phase = output.truncatingRemainder(dividingBy: beat)
            var delta = referencePhase - phase
            if delta > beat / 2 { delta -= beat }
            if delta < -beat / 2 { delta += beat }
            return delta * target.rate
        }
        let began = ProcessInfo.processInfo.systemUptime
        let duration = 0.42
        var priorEase = 0.0
        peakAlignTimer = DJRunLoopTimer.scheduled(every: 1.0 / 60.0) { [weak self] timer in
            let raw = min(1, (ProcessInfo.processInfo.systemUptime - began) / duration)
            let eased = raw * raw * (3 - 2 * raw)
            let step = eased - priorEase
            priorEase = eased
            for index in targets.indices {
                targets[index].seek(to: targets[index].currentTime + corrections[index] * step)
            }
            if raw >= 1 {
                timer.invalidate()
                self?.peakAlignTimer = nil
            }
        }
    }

    override func layout() {
        let pad: CGFloat = 4
        let gap: CGFloat = 8
        let crossHeight: CGFloat = 52
        let waveHeight: CGFloat = min(104, max(76, bounds.height * 0.22))
        let waveRow = (waveHeight - 4) / 2
        let waveBottom = crossHeight + gap
        if soloMode {
            waveformA.frame = NSRect(x: pad, y: crossHeight, width: bounds.width - pad * 2,
                                     height: waveHeight)
            deckA.frame = NSRect(x: pad, y: crossHeight + waveHeight + gap,
                                 width: bounds.width - pad * 2,
                                 height: max(160, bounds.height - crossHeight - waveHeight - gap))
            practiceButton.frame = NSRect(x: pad, y: 8, width: 86, height: 26)
            detachedScroll.frame = NSRect(x: 98, y: 3, width: max(0, bounds.width - 102), height: 44)
            detachedRack.frame = NSRect(x: 0, y: 0,
                                        width: max(detachedScroll.bounds.width,
                                                   CGFloat(detachedChannels.count) * 154),
                                        height: 38)
            return
        }
        detachedScroll.frame = .zero
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
        deckC.setGain(masterVolume * 0.42)
        deckD.setGain(masterVolume * 0.42)
        crossLabel.stringValue = "A \(Int((a * a * 100).rounded()))  ·  \(Int((b * b * 100).rounded())) B"
    }
}
