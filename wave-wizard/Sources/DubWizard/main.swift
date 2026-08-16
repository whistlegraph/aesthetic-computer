import AppKit
import AVFoundation
import AVFAudio
import CoreAudio
import DubRingBuffer

struct DubTake: Codable {
    let id: String
    let track: String
    let file: String
    let createdAt: String
    let durationSeconds: Double
    let inputDevice: String
    let inputChannel: Int
    let role: String
    let section: String
    let notes: String
}

struct DeviceInfo: Equatable {
    let id: AudioDeviceID
    let name: String
    let inputChannels: Int
    let outputChannels: Int

    /// The Scarlett Solo (and every interface like it) can carry the whole session:
    /// AVAudioEngine binds ONE device to both its input and its output.
    var canRunFullDuplex: Bool { inputChannels > 0 && outputChannels > 0 }
}

enum Hardware {
    static func uint32Property(_ id: AudioObjectID,
                               _ selector: AudioObjectPropertySelector,
                               scope: AudioObjectPropertyScope) -> UInt32? {
        var address = AudioObjectPropertyAddress(mSelector: selector, mScope: scope,
                                                 mElement: kAudioObjectPropertyElementMain)
        var value: UInt32 = 0
        var size = UInt32(MemoryLayout<UInt32>.size)
        guard AudioObjectGetPropertyData(id, &address, 0, nil, &size, &value) == noErr else { return nil }
        return value
    }

    static func sampleRate(_ id: AudioDeviceID) -> Double {
        var address = AudioObjectPropertyAddress(mSelector: kAudioDevicePropertyNominalSampleRate,
                                                 mScope: kAudioObjectPropertyScopeGlobal,
                                                 mElement: kAudioObjectPropertyElementMain)
        var value = Float64(0)
        var size = UInt32(MemoryLayout<Float64>.size)
        guard AudioObjectGetPropertyData(id, &address, 0, nil, &size, &value) == noErr else { return 0 }
        return value
    }

    static func latencyReport(_ device: DeviceInfo) -> String {
        let rate = sampleRate(device.id)
        let buffer = uint32Property(device.id, kAudioDevicePropertyBufferFrameSize,
                                    scope: kAudioObjectPropertyScopeGlobal) ?? 0
        let inputLatency = uint32Property(device.id, kAudioDevicePropertyLatency,
                                          scope: kAudioObjectPropertyScopeInput) ?? 0
        let outputLatency = uint32Property(device.id, kAudioDevicePropertyLatency,
                                           scope: kAudioObjectPropertyScopeOutput) ?? 0
        let inputSafety = uint32Property(device.id, kAudioDevicePropertySafetyOffset,
                                         scope: kAudioObjectPropertyScopeInput) ?? 0
        let outputSafety = uint32Property(device.id, kAudioDevicePropertySafetyOffset,
                                          scope: kAudioObjectPropertyScopeOutput) ?? 0
        let inputFrames = inputLatency + inputSafety + buffer
        let outputFrames = outputLatency + outputSafety + buffer
        let roundTripFrames = inputFrames + outputFrames
        func ms(_ frames: UInt32) -> String {
            guard rate > 0 else { return "unknown" }
            return String(format: "%.2f ms", Double(frames) / rate * 1000)
        }
        return """
        device: \(device.name)
        sample rate: \(Int(rate)) Hz
        buffer: \(buffer) frames (\(ms(buffer)))
        input: device \(inputLatency) + safety \(inputSafety) + buffer \(buffer) = \(inputFrames) frames (\(ms(inputFrames)))
        output: device \(outputLatency) + safety \(outputSafety) + buffer \(buffer) = \(outputFrames) frames (\(ms(outputFrames)))
        estimated round trip: \(roundTripFrames) frames (\(ms(roundTripFrames)))
        """
    }

    static func devices() -> [DeviceInfo] {
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDevices,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain
        )
        var size: UInt32 = 0
        guard AudioObjectGetPropertyDataSize(AudioObjectID(kAudioObjectSystemObject), &address, 0, nil, &size) == noErr else { return [] }
        let count = Int(size) / MemoryLayout<AudioDeviceID>.stride
        var ids = Array(repeating: AudioDeviceID(), count: count)
        guard AudioObjectGetPropertyData(AudioObjectID(kAudioObjectSystemObject), &address, 0, nil, &size, &ids) == noErr else { return [] }
        return ids.compactMap { id in
            var nameAddress = AudioObjectPropertyAddress(
                mSelector: kAudioObjectPropertyName,
                mScope: kAudioObjectPropertyScopeGlobal,
                mElement: kAudioObjectPropertyElementMain
            )
            var name: Unmanaged<CFString>?
            var nameSize = UInt32(MemoryLayout<CFString?>.size)
            guard AudioObjectGetPropertyData(id, &nameAddress, 0, nil, &nameSize, &name) == noErr,
                  let cfName = name?.takeUnretainedValue() else { return nil }
            return DeviceInfo(id: id, name: cfName as String,
                              inputChannels: channels(id, kAudioObjectPropertyScopeInput),
                              outputChannels: channels(id, kAudioObjectPropertyScopeOutput))
        }
    }

    /// Walks every buffer in the AudioBufferList. Reading only the first buffer
    /// undercounts any device that publishes its channels non-interleaved.
    static func channels(_ id: AudioDeviceID, _ scope: AudioObjectPropertyScope) -> Int {
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyStreamConfiguration,
            mScope: scope,
            mElement: kAudioObjectPropertyElementMain
        )
        var dataSize: UInt32 = 0
        guard AudioObjectGetPropertyDataSize(id, &address, 0, nil, &dataSize) == noErr, dataSize > 0 else { return 0 }
        let raw = UnsafeMutableRawPointer.allocate(byteCount: Int(dataSize),
                                                   alignment: MemoryLayout<AudioBufferList>.alignment)
        defer { raw.deallocate() }
        guard AudioObjectGetPropertyData(id, &address, 0, nil, &dataSize, raw) == noErr else { return 0 }
        let list = UnsafeMutableAudioBufferListPointer(raw.assumingMemoryBound(to: AudioBufferList.self))
        return list.reduce(0) { $0 + Int($1.mNumberChannels) }
    }

    static func inputs() -> [DeviceInfo] { devices().filter { $0.inputChannels > 0 } }

    /// Prefer a full-duplex interface (the Focusrite) so playback and capture
    /// share one clock; fall back to whatever can record.
    static func preferredInput() -> DeviceInfo? {
        let all = inputs()
        return all.first { $0.name.localizedCaseInsensitiveContains("scarlett") || $0.name.localizedCaseInsensitiveContains("focusrite") }
            ?? all.first { $0.canRunFullDuplex }
            ?? all.first
    }

    /// AVAudioEngine's input and output share a single AUHAL, so this one call
    /// sets the device for BOTH directions. Verified by reading the value back.
    ///
    /// `viaInput: false` reaches that same unit through `outputNode` instead.
    /// Necessary before the mic grant settles: `engine.inputNode` instantiates the
    /// I/O unit with input enabled, which blocks inside `AudioComponentInstanceNew`
    /// until the permission prompt is answered — on the main thread that freezes
    /// the whole UI (the window never leaves 0×0).
    @discardableResult
    static func bindEngine(_ engine: AVAudioEngine, to device: DeviceInfo, viaInput: Bool = true) -> Bool {
        guard let unit = viaInput ? engine.inputNode.audioUnit : engine.outputNode.audioUnit else { return false }
        var deviceID = device.id
        let status = AudioUnitSetProperty(unit, kAudioOutputUnitProperty_CurrentDevice,
                                          kAudioUnitScope_Global, 0, &deviceID,
                                          UInt32(MemoryLayout<AudioDeviceID>.size))
        guard status == noErr else { return false }
        var readback = AudioDeviceID(0)
        var size = UInt32(MemoryLayout<AudioDeviceID>.size)
        guard AudioUnitGetProperty(unit, kAudioOutputUnitProperty_CurrentDevice,
                                   kAudioUnitScope_Global, 0, &readback, &size) == noErr else { return false }
        return readback == device.id
    }

    /// 128 frames is ~2.7 ms at 48 kHz. Asking for 16 is below what the Scarlett
    /// will accept and leaves the graph in a state that aborts on connect.
    @discardableResult
    static func setBufferFrames(_ device: DeviceInfo, frames: UInt32 = 128) -> UInt32 {
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioDevicePropertyBufferFrameSize,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain
        )
        var value = frames
        _ = AudioObjectSetPropertyData(device.id, &address, 0, nil,
                                       UInt32(MemoryLayout<UInt32>.size), &value)
        var actual: UInt32 = 0
        var size = UInt32(MemoryLayout<UInt32>.size)
        guard AudioObjectGetPropertyData(device.id, &address, 0, nil, &size, &actual) == noErr else { return frames }
        return actual
    }
}

enum MicPermission {
    static var state: String = "checking…"

    static func request(_ done: @escaping (Bool) -> Void) {
        switch AVCaptureDevice.authorizationStatus(for: .audio) {
        case .authorized:
            state = "granted"
            done(true)
        case .denied, .restricted:
            state = "DENIED — System Settings ▸ Privacy & Security ▸ Microphone"
            done(false)
        case .notDetermined:
            AVCaptureDevice.requestAccess(for: .audio) { granted in
                state = granted ? "granted" : "DENIED — System Settings ▸ Privacy & Security ▸ Microphone"
                DispatchQueue.main.async { done(granted) }
            }
        @unknown default:
            state = "unknown"
            done(false)
        }
    }
}

final class DubSession: NSObject {
    let trackURL: URL
    let outputDir: URL

    // Rebuilt wholesale on every device change: AVAudioEngine caches the format
    // it saw at connect time, and reusing a stale graph is what was aborting.
    private(set) var engine = AVAudioEngine()
    private(set) var player = AVAudioPlayerNode()

    var trackFile: AVAudioFile?
    var recordingFile: AVAudioFile?
    var recordingFormat: AVAudioFormat?
    var graphReady = false
    var monitorAvailable = false
    var ring: OpaquePointer?
    let drainQueue = DispatchQueue(label: "dubwizard.record-drain", qos: .userInitiated)
    var drainTimer: DispatchSourceTimer?
    var recordingStarted: Date?
    var currentTakeID: String?
    var ioProcID: AudioDeviceIOProcID?
    let captureQueue = DispatchQueue(label: "dubwizard.coreaudio-input", qos: .userInteractive)
    var onState: ((String) -> Void)?
    var onDeviceInfo: ((String) -> Void)?

    var inputDevice: DeviceInfo?
    var inputChannel = 0
    var inputChannelCount = 0
    private let peakLock = NSLock()
    private var peaks: [Float] = []
    var outputVolume: Float = 0.85
    var inputGain: Float = 1.0
    var monitorVolume: Float = 0.8
    var inputSampleRate: Double = 48_000
    var monitoringEnabled = false
    private let inputScratch = UnsafeMutablePointer<Float>.allocate(capacity: 8192)

    /// Peak per hardware input channel — this is what makes a dead XLR channel
    /// (no phantom power, gain at zero) visible instead of mysterious.
    func channelPeaks() -> [Float] {
        peakLock.lock(); defer { peakLock.unlock() }
        let snapshot = peaks
        for i in peaks.indices { peaks[i] *= 0.82 }
        return snapshot
    }

    var inputPeak: Float { channelPeaks().indices.contains(inputChannel) ? channelPeaks()[inputChannel] : 0 }

    init(trackURL: URL, outputDir: URL) {
        self.trackURL = trackURL
        self.outputDir = outputDir
        super.init()
        trackFile = try? AVAudioFile(forReading: trackURL)
    }

    /// Tears the graph down and rebuilds it against `device`, in the one order
    /// CoreAudio tolerates: stop → set device → read formats → connect → prepare.
    ///
    /// `includeInput: false` builds a playback-only graph that never touches
    /// `engine.inputNode`. That matters: reading the input node while a
    /// microphone prompt is still pending BLOCKS the calling thread, and doing it
    /// on the main thread is what leaves the window stuck at 0×0 and offscreen —
    /// the "the UI is invisible" symptom.
    func rebuild(with device: DeviceInfo?, includeInput: Bool = true) {
        stopDeviceCapture()
        engine.stop()
        engine = AVAudioEngine()
        player = AVAudioPlayerNode()
        graphReady = false
        monitorAvailable = false
        inputDevice = device

        var summary: [String] = []

        if let device {
            let frames = Hardware.setBufferFrames(device)
            let bound = Hardware.bindEngine(engine, to: device, viaInput: false)
            summary.append("\(device.name) · \(frames)-frame buffer")
            if !bound { summary.append("device bind FAILED") }
            if !device.canRunFullDuplex {
                summary.append("input-only device — playback needs an aggregate device")
            }
        } else {
            summary.append("system default device")
        }

        inputChannelCount = includeInput ? (device?.inputChannels ?? 0) : 0
        peakLock.lock(); peaks = Array(repeating: 0, count: max(inputChannelCount, 1)); peakLock.unlock()
        if inputChannel >= inputChannelCount { inputChannel = 0 }

        engine.attach(player)
        let trackFormat = trackFile?.processingFormat
        engine.connect(player, to: engine.mainMixerNode, format: trackFormat)

        if includeInput, let device, startDeviceCapture(device) {
            monitorAvailable = true
            summary.append("direct CoreAudio input · \(device.inputChannels) ch")
        } else if includeInput {
            summary.append("NO INPUT FORMAT — check microphone permission")
        } else {
            summary.append("playback only — awaiting microphone permission")
        }

        // 128 frames keeps latency low without asking for a slice CoreAudio rejects.
        engine.outputNode.auAudioUnit.maximumFramesToRender = 128
        engine.mainMixerNode.outputVolume = outputVolume
        engine.prepare()
        graphReady = true

        let text = summary.joined(separator: " · ")
        DispatchQueue.main.async { [weak self] in
            self?.onDeviceInfo?(text)
            self?.onState?("Ready · \(MicPermission.state)")
        }
    }

    private func startDeviceCapture(_ device: DeviceInfo) -> Bool {
        var rateAddress = AudioObjectPropertyAddress(mSelector: kAudioDevicePropertyNominalSampleRate,
                                                     mScope: kAudioObjectPropertyScopeGlobal,
                                                     mElement: kAudioObjectPropertyElementMain)
        var rate = Float64(48_000)
        var rateSize = UInt32(MemoryLayout<Float64>.size)
        _ = AudioObjectGetPropertyData(device.id, &rateAddress, 0, nil, &rateSize, &rate)
        inputSampleRate = rate

        var proc: AudioDeviceIOProcID?
        let status = AudioDeviceCreateIOProcIDWithBlock(&proc, device.id, captureQueue) { [weak self] _, input, _, output, _ in
            self?.consumeDeviceInput(input, output: output)
        }
        guard status == noErr, let proc else { return false }
        ioProcID = proc
        guard AudioDeviceStart(device.id, proc) == noErr else {
            AudioDeviceDestroyIOProcID(device.id, proc)
            ioProcID = nil
            return false
        }
        return true
    }

    private func stopDeviceCapture() {
        guard let device = inputDevice, let proc = ioProcID else { return }
        AudioDeviceStop(device.id, proc)
        AudioDeviceDestroyIOProcID(device.id, proc)
        ioProcID = nil
    }

    private func consumeDeviceInput(_ input: UnsafePointer<AudioBufferList>,
                                    output: UnsafeMutablePointer<AudioBufferList>) {
        let buffers = UnsafeMutableAudioBufferListPointer(UnsafeMutablePointer(mutating: input))
        var channelIndex = 0
        var selectedFrames = 0
        peakLock.lock()
        for buffer in buffers {
            guard let data = buffer.mData?.assumingMemoryBound(to: Float.self) else {
                channelIndex += Int(buffer.mNumberChannels); continue
            }
            let channels = max(1, Int(buffer.mNumberChannels))
            let frames = Int(buffer.mDataByteSize) / MemoryLayout<Float>.size / channels
            for local in 0..<channels {
                let global = channelIndex + local
                var peak: Float = 0
                for frame in 0..<frames { peak = max(peak, abs(data[frame * channels + local])) }
                if global < peaks.count { peaks[global] = max(peaks[global], peak) }
                if global == inputChannel {
                    selectedFrames = min(frames, 8192)
                    if channels == 1 {
                        inputScratch.update(from: data, count: selectedFrames)
                    } else {
                        for frame in 0..<selectedFrames {
                            inputScratch[frame] = data[frame * channels + local]
                        }
                    }
                }
            }
            channelIndex += channels
        }
        peakLock.unlock()
        guard selectedFrames > 0 else { return }
        for frame in 0..<selectedFrames { inputScratch[frame] *= inputGain }
        if let ring { _ = dub_ring_write(ring, inputScratch, selectedFrames) }

        // Lowest-latency ARM preview: write the chosen XLR channel straight into
        // this device IOProc's output buffers. CoreAudio mixes it with the track
        // playback client on the same Scarlett clock.
        guard monitoringEnabled else { return }
        let outputs = UnsafeMutableAudioBufferListPointer(output)
        for buffer in outputs {
            guard let data = buffer.mData?.assumingMemoryBound(to: Float.self) else { continue }
            let channels = max(1, Int(buffer.mNumberChannels))
            let frames = min(selectedFrames,
                             Int(buffer.mDataByteSize) / MemoryLayout<Float>.size / channels)
            for frame in 0..<frames {
                for channel in 0..<channels {
                    data[frame * channels + channel] = inputScratch[frame] * monitorVolume
                }
            }
        }
    }

    func startEngineIfNeeded() throws {
        if !engine.isRunning { try engine.start() }
    }

    func startTrack() {
        guard graphReady, let file = trackFile else { onState?("Audio graph warming…"); return }
        do {
            try startEngineIfNeeded()
            player.stop()
            player.scheduleFile(file, at: nil)
            player.play()
            onState?("Playing \(trackURL.lastPathComponent)")
        } catch { onState?("Audio start failed: \(error.localizedDescription)") }
    }

    func stopTrack() {
        player.stop()
        onState?("Stopped")
    }

    func setMonitoring(_ enabled: Bool) {
        guard monitorAvailable else {
            onState?("No input available — check microphone permission")
            return
        }
        do { try startEngineIfNeeded() } catch {
            onState?("Monitor failed: \(error.localizedDescription)"); return
        }
        monitoringEnabled = enabled
        onState?(enabled ? "ARMED · direct Scarlett monitor live" : "Input monitor off")
    }

    func beginTake(role: String, section: String, notes: String) {
        guard graphReady, monitorAvailable else { onState?("No input available"); return }
        do {
            try startEngineIfNeeded()
            try FileManager.default.createDirectory(at: outputDir, withIntermediateDirectories: true)
            let id = ISO8601DateFormatter().string(from: Date()).replacingOccurrences(of: ":", with: "-")
            let url = outputDir.appendingPathComponent("\(id).caf")
            guard let monoFormat = AVAudioFormat(standardFormatWithSampleRate: inputSampleRate, channels: 1) else { return }
            recordingFormat = monoFormat
            recordingFile = try AVAudioFile(forWriting: url, settings: monoFormat.settings)
            ring = dub_ring_create(Int(inputSampleRate * 8))
            currentTakeID = id
            recordingStarted = Date()
            startDrain()
            startTrack()
            onState?("Recording dub on ch\(inputChannel + 1)…")
        } catch { onState?("Record failed: \(error.localizedDescription)") }
    }

    func endTake(role: String, section: String, notes: String) {
        guard let started = recordingStarted, let id = currentTakeID else { return }
        stopDrain()
        recordingFile = nil
        recordingFormat = nil
        if let ring { dub_ring_destroy(ring) }
        ring = nil
        stopTrack()
        let duration = Date().timeIntervalSince(started)
        let take = DubTake(id: id, track: trackURL.path,
                           file: outputDir.appendingPathComponent("\(id).caf").path,
                           createdAt: ISO8601DateFormatter().string(from: started), durationSeconds: duration,
                           inputDevice: inputDevice?.name ?? "system input", inputChannel: inputChannel + 1,
                           role: role, section: section, notes: notes)
        let indexURL = outputDir.appendingPathComponent("dubs.json")
        var takes = (try? JSONDecoder().decode([DubTake].self, from: Data(contentsOf: indexURL))) ?? []
        takes.append(take)
        if let data = try? JSONEncoder.pretty.encode(takes) { try? data.write(to: indexURL) }
        recordingStarted = nil; currentTakeID = nil
        onState?("Saved dub \(id)")
    }

    private func startDrain() {
        let timer = DispatchSource.makeTimerSource(queue: drainQueue)
        timer.schedule(deadline: .now(), repeating: .milliseconds(10), leeway: .milliseconds(2))
        timer.setEventHandler { [weak self] in self?.drainAvailable() }
        timer.resume()
        drainTimer = timer
    }

    private func stopDrain() {
        drainTimer?.cancel()
        drainTimer = nil
        drainQueue.sync { drainAvailable() }
    }

    private func drainAvailable() {
        guard let ring, let file = recordingFile, let format = recordingFormat else { return }
        var samples = [Float](repeating: 0, count: 2048)
        while dub_ring_available(ring) > 0 {
            let count = dub_ring_read(ring, &samples, samples.count)
            guard count > 0, let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: AVAudioFrameCount(count)),
                  let channel = buffer.floatChannelData else { return }
            buffer.frameLength = AVAudioFrameCount(count)
            samples.withUnsafeBufferPointer { src in
                channel[0].update(from: src.baseAddress!, count: count)
            }
            do { try file.write(from: buffer) } catch { onState?("Record write failed: \(error.localizedDescription)"); return }
        }
    }

    deinit {
        stopDeviceCapture()
        inputScratch.deallocate()
        engine.stop()
    }
}

private extension JSONEncoder {
    static var pretty: JSONEncoder { let e = JSONEncoder(); e.outputFormatting = [.prettyPrinted, .sortedKeys]; return e }
}

final class DubMascotView: NSView {
    private let jeffrey = NSImage(contentsOfFile: "/Users/jas/aesthetic-computer/slab/menuband/marketing/notepat-launch/illy-jeffrey-duo.png")
    override func draw(_ dirtyRect: NSRect) {
        let r = bounds.insetBy(dx: 8, dy: 8)
        NSColor(calibratedRed: 0.12, green: 0.09, blue: 0.20, alpha: 0.92).setFill()
        NSBezierPath(roundedRect: r, xRadius: 24, yRadius: 24).fill()
        NSColor.systemPink.withAlphaComponent(0.8).setStroke()
        let halo = NSBezierPath(ovalIn: r.insetBy(dx: 12, dy: 12)); halo.lineWidth = 2; halo.stroke()
        let mic = NSRect(x: r.midX - 17, y: r.midY - 22, width: 34, height: 52)
        NSColor(calibratedRed: 0.96, green: 0.40, blue: 0.72, alpha: 1).setFill()
        NSBezierPath(roundedRect: mic, xRadius: 17, yRadius: 17).fill()
        NSColor.white.withAlphaComponent(0.9).setFill()
        NSBezierPath(ovalIn: NSRect(x: mic.midX - 5, y: mic.midY + 5, width: 10, height: 10)).fill()
        NSColor(calibratedRed: 0.30, green: 0.90, blue: 0.95, alpha: 1).setStroke()
        let cradle = NSBezierPath(); cradle.move(to: NSPoint(x: r.midX - 30, y: r.midY - 4)); cradle.curve(to: NSPoint(x: r.midX + 30, y: r.midY - 4), controlPoint1: NSPoint(x: r.midX - 30, y: r.midY - 28), controlPoint2: NSPoint(x: r.midX + 30, y: r.midY - 28)); cradle.lineWidth = 4; cradle.stroke()
        let stem = NSBezierPath(); stem.move(to: NSPoint(x: r.midX, y: r.midY - 6)); stem.line(to: NSPoint(x: r.midX, y: r.midY - 27)); stem.move(to: NSPoint(x: r.midX - 13, y: r.midY - 28)); stem.line(to: NSPoint(x: r.midX + 13, y: r.midY - 28)); stem.lineWidth = 4; stem.stroke()
        NSColor.white.withAlphaComponent(0.9).setFill()
        for p in [NSPoint(x: r.minX + 20, y: r.maxY - 25), NSPoint(x: r.maxX - 21, y: r.midY + 22)] {
            NSBezierPath(ovalIn: NSRect(x: p.x - 3, y: p.y - 3, width: 6, height: 6)).fill()
        }
        if let jeffrey {
            let portrait = NSRect(x: r.minX + 12, y: r.minY + 10, width: 46, height: 58)
            jeffrey.draw(in: portrait, from: NSRect(x: 0, y: jeffrey.size.height * 0.42, width: jeffrey.size.width, height: jeffrey.size.height * 0.44), operation: .sourceOver, fraction: 0.9)
        }
    }
}

final class DubWaveformView: NSView {
    var liveLevel: CGFloat = 0 { didSet { needsDisplay = true } }
    var phase: CGFloat = 0 { didSet { needsDisplay = true } }
    var trackSamples: [CGFloat] = [] { didSet { needsDisplay = true } }
    var liveSamples: [CGFloat] = [] { didSet { needsDisplay = true } }
    override func draw(_ dirtyRect: NSRect) {
        let r = bounds.insetBy(dx: 1, dy: 1)
        NSColor.black.withAlphaComponent(0.22).setFill(); NSBezierPath(roundedRect: r, xRadius: 10, yRadius: 10).fill()
        let mid = r.midY
        NSColor.systemTeal.withAlphaComponent(0.8).setStroke()
        let track = NSBezierPath(); track.lineWidth = 1.5
        for i in 0..<56 { let x = r.minX + CGFloat(i) / 55 * r.width; let sample = trackSamples.isEmpty ? 0.2 : trackSamples[i % trackSamples.count]; let amp = 3 + sample * 15; track.move(to: NSPoint(x: x, y: mid - amp)); track.line(to: NSPoint(x: x, y: mid + amp)) }
        track.stroke()
        guard !liveSamples.isEmpty else { return }
        NSColor.systemPink.setStroke()
        let vocal = NSBezierPath(); vocal.lineWidth = 2
        for (i, sample) in liveSamples.enumerated() {
            let x = r.minX + CGFloat(i) / CGFloat(max(1, liveSamples.count - 1)) * r.width
            let y = mid + (i.isMultiple(of: 2) ? -1 : 1) * max(1, sample * r.height * 0.46)
            if i == 0 { vocal.move(to: NSPoint(x: x, y: y)) }
            else { vocal.line(to: NSPoint(x: x, y: y)) }
        }
        vocal.stroke()
    }
}

/// Per-hardware-channel meters. On a Scarlett Solo ch1 is the XLR mic and ch2 is
/// the instrument jack — if ch1 stays dark, the problem is phantom power or gain.
final class DubChannelMeters: NSView {
    var levels: [Float] = [] { didSet { needsDisplay = true } }
    var selected = 0 { didSet { needsDisplay = true } }
    override func draw(_ dirtyRect: NSRect) {
        let r = bounds
        NSColor.black.withAlphaComponent(0.22).setFill()
        NSBezierPath(roundedRect: r, xRadius: 8, yRadius: 8).fill()
        guard !levels.isEmpty else { return }
        let rowHeight = r.height / CGFloat(levels.count)
        for (i, level) in levels.enumerated() {
            let y = r.maxY - CGFloat(i + 1) * rowHeight
            let label = NSString(string: "\(i + 1)")
            let color: NSColor = i == selected ? .systemPink : NSColor.white.withAlphaComponent(0.35)
            label.draw(at: NSPoint(x: r.minX + 5, y: y + rowHeight / 2 - 7),
                       withAttributes: [.font: NSFont.monospacedSystemFont(ofSize: 9, weight: .bold), .foregroundColor: color])
            let barArea = NSRect(x: r.minX + 18, y: y + rowHeight * 0.28, width: r.width - 26, height: rowHeight * 0.44)
            NSColor.white.withAlphaComponent(0.10).setFill()
            NSBezierPath(roundedRect: barArea, xRadius: 3, yRadius: 3).fill()
            let filled = CGFloat(min(1, sqrt(max(0, level)))) * barArea.width
            if filled > 1 {
                (level > 0.95 ? NSColor.systemRed : color).setFill()
                NSBezierPath(roundedRect: NSRect(x: barArea.minX, y: barArea.minY, width: filled, height: barArea.height), xRadius: 3, yRadius: 3).fill()
            }
        }
    }
}

final class DubWindowController: NSWindowController {
    let session: DubSession
    let status = NSTextField(labelWithString: "")
    let device = NSTextField(labelWithString: "")
    let inputPicker = NSPopUpButton()
    let channelPicker = NSPopUpButton()
    let meters = DubChannelMeters()
    let role = NSTextField(string: "lead")
    let section = NSTextField(string: "")
    let notes = NSTextField(string: "")
    let volume = NSSlider(value: 0.85, minValue: 0, maxValue: 1, target: nil, action: nil)
    let volumeLabel = NSTextField(labelWithString: "OUT 85%")
    let inputGain = NSSlider(value: 1, minValue: 0, maxValue: 4, target: nil, action: nil)
    let inputGainLabel = NSTextField(labelWithString: "GAIN 1.0×")
    let monitorVolume = NSSlider(value: 0.8, minValue: 0, maxValue: 1.5, target: nil, action: nil)
    let monitorLabel = NSTextField(labelWithString: "MON 80%")
    let waveform = DubWaveformView()
    let record = NSButton(title: "● Record Dub", target: nil, action: nil)
    let arm = NSButton(title: "ARM", target: nil, action: nil)
    let play = NSButton(title: "▶ Play Track", target: nil, action: nil)
    let stop = NSButton(title: "■ Stop", target: nil, action: nil)
    var recording = false
    var armed = false
    var waveformTimer: Timer?
    var availableInputs: [DeviceInfo] = []

    init(session: DubSession) {
        self.session = session
        let panel = NSPanel(contentRect: NSRect(x: 0, y: 0, width: 520, height: 360),
                            styleMask: [.titled, .closable, .utilityWindow], backing: .buffered, defer: false)
        panel.title = "AesthetiVox"
        panel.isFloatingPanel = true
        panel.hidesOnDeactivate = false
        panel.level = .floating
        panel.backgroundColor = NSColor(calibratedRed: 0.07, green: 0.05, blue: 0.12, alpha: 1)
        super.init(window: panel)
        session.onState = { [weak self] message in DispatchQueue.main.async { self?.status.stringValue = message } }
        session.onDeviceInfo = { [weak self] info in DispatchQueue.main.async { self?.device.stringValue = info } }
        buildUI()
    }

    required init?(coder: NSCoder) { fatalError() }

    func buildUI() {
        guard let view = window?.contentView else { return }
        let glass = NSVisualEffectView(frame: view.bounds)
        glass.material = .hudWindow; glass.blendingMode = .behindWindow; glass.state = .active
        glass.autoresizingMask = [.width, .height]; view.addSubview(glass)
        let mascot = DubMascotView(frame: view.bounds); mascot.autoresizingMask = [.width, .height]; mascot.alphaValue = 0.22; view.addSubview(mascot, positioned: .below, relativeTo: glass)
        device.stringValue = "Binding audio device…"
        device.font = .systemFont(ofSize: 11); device.textColor = .secondaryLabelColor
        device.frame = NSRect(x: 24, y: 310, width: 470, height: 18); view.addSubview(device)

        inputPicker.frame = NSRect(x: 24, y: 242, width: 330, height: 24)
        inputPicker.target = self; inputPicker.action = #selector(inputDeviceChanged(_:))
        view.addSubview(inputPicker)
        channelPicker.frame = NSRect(x: 362, y: 242, width: 132, height: 24)
        channelPicker.target = self; channelPicker.action = #selector(inputChannelChanged(_:))
        view.addSubview(channelPicker)

        for (label, field, y) in [("role", role, 205), ("section", section, 175), ("notes", notes, 145)] as [(String, NSTextField, CGFloat)] {
            let l = NSTextField(labelWithString: label); l.frame = NSRect(x: 24, y: y + 3, width: 60, height: 20); view.addSubview(l)
            field.frame = NSRect(x: 88, y: y, width: 406, height: 24); view.addSubview(field)
        }
        waveform.frame = NSRect(x: 24, y: 96, width: 330, height: 42); view.addSubview(waveform)
        meters.frame = NSRect(x: 362, y: 96, width: 132, height: 42); view.addSubview(meters)

        let outLabel = NSTextField(labelWithString: "out")
        outLabel.frame = NSRect(x: 24, y: 74, width: 24, height: 18)
        outLabel.font = .systemFont(ofSize: 10, weight: .medium); outLabel.textColor = .secondaryLabelColor
        view.addSubview(outLabel)
        volume.frame = NSRect(x: 48, y: 70, width: 72, height: 22)
        volume.target = self; volume.action = #selector(outputVolumeChanged(_:)); volume.isContinuous = true
        view.addSubview(volume)
        volumeLabel.frame = NSRect(x: 122, y: 73, width: 58, height: 18)
        volumeLabel.font = .monospacedSystemFont(ofSize: 10, weight: .medium); volumeLabel.textColor = .secondaryLabelColor
        view.addSubview(volumeLabel)
        inputGain.frame = NSRect(x: 180, y: 70, width: 72, height: 22)
        inputGain.target = self; inputGain.action = #selector(inputGainChanged(_:)); inputGain.isContinuous = true
        view.addSubview(inputGain)
        inputGainLabel.frame = NSRect(x: 254, y: 73, width: 72, height: 18)
        inputGainLabel.font = .monospacedSystemFont(ofSize: 10, weight: .medium); inputGainLabel.textColor = .secondaryLabelColor
        view.addSubview(inputGainLabel)
        monitorVolume.frame = NSRect(x: 326, y: 70, width: 72, height: 22)
        monitorVolume.target = self; monitorVolume.action = #selector(monitorVolumeChanged(_:)); monitorVolume.isContinuous = true
        view.addSubview(monitorVolume)
        monitorLabel.frame = NSRect(x: 400, y: 73, width: 74, height: 18)
        monitorLabel.font = .monospacedSystemFont(ofSize: 10, weight: .medium); monitorLabel.textColor = .secondaryLabelColor
        view.addSubview(monitorLabel)

        play.frame = NSRect(x: 24, y: 34, width: 140, height: 30); play.target = self; play.action = #selector(playTrack); view.addSubview(play)
        stop.frame = NSRect(x: 174, y: 34, width: 90, height: 30); stop.target = self; stop.action = #selector(stopTrack); view.addSubview(stop)
        record.frame = NSRect(x: 274, y: 34, width: 150, height: 30); record.target = self; record.action = #selector(toggleRecord); view.addSubview(record)
        arm.frame = NSRect(x: 430, y: 34, width: 64, height: 30); arm.target = self; arm.action = #selector(toggleArm); arm.bezelStyle = .rounded; view.addSubview(arm)
        status.frame = NSRect(x: 24, y: 10, width: 470, height: 20); status.textColor = .secondaryLabelColor; view.addSubview(status)
        status.stringValue = "Requesting microphone access…"

        DispatchQueue.global(qos: .utility).async { [weak self] in self?.loadTrackWaveform() }
        waveformTimer = Timer.scheduledTimer(withTimeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in
            guard let self else { return }
            waveform.phase += 0.22
            let levels = session.channelPeaks()
            meters.levels = levels
            meters.selected = session.inputChannel
            waveform.liveLevel = armed && levels.indices.contains(session.inputChannel) ? CGFloat(levels[session.inputChannel]) : 0
            let sample = levels.indices.contains(session.inputChannel)
                ? CGFloat(min(1, levels[session.inputChannel] * session.inputGain)) : 0
            waveform.liveSamples.append(sample)
            if waveform.liveSamples.count > 96 { waveform.liveSamples.removeFirst() }
        }
    }

    /// Called once at launch with `includeInput: false` (window first), again once
    /// the mic grant settles, then on every device change.
    func refreshDevices(select: DeviceInfo?, includeInput: Bool = true) {
        availableInputs = Hardware.inputs()
        inputPicker.removeAllItems()
        for d in availableInputs {
            let duplex = d.canRunFullDuplex ? "" : " (in only)"
            inputPicker.addItem(withTitle: "\(d.name) · \(d.inputChannels) in\(duplex)")
        }
        let target = select ?? Hardware.preferredInput()
        if let target, let index = availableInputs.firstIndex(of: target) {
            inputPicker.selectItem(at: index)
        }
        session.rebuild(with: target, includeInput: includeInput)
        refreshChannelPicker()
    }

    private func refreshChannelPicker() {
        channelPicker.removeAllItems()
        let count = max(session.inputChannelCount, 0)
        if count == 0 {
            channelPicker.addItem(withTitle: "no input")
            channelPicker.isEnabled = false
            return
        }
        channelPicker.isEnabled = true
        let scarlett = session.inputDevice?.name.localizedCaseInsensitiveContains("scarlett") ?? false
        for c in 0..<count {
            // Scarlett Solo front panel: 1 = XLR mic, 2 = instrument jack.
            let hint = scarlett ? (c == 0 ? " mic/XLR" : c == 1 ? " inst" : "") : ""
            channelPicker.addItem(withTitle: "ch \(c + 1)\(hint)")
        }
        channelPicker.selectItem(at: min(session.inputChannel, count - 1))
    }

    private func loadTrackWaveform() {
        guard let file = try? AVAudioFile(forReading: session.trackURL) else { return }
        let format = file.processingFormat
        let total = Int(file.length)
        guard total > 0, let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: AVAudioFrameCount(total)) else { return }
        do { try file.read(into: buffer) } catch { return }
        guard let data = buffer.floatChannelData else { return }
        let stride = max(1, total / 56)
        let samples = (0..<56).map { i in
            let start = i * stride; let end = min(total, start + stride)
            var peak: Float = 0
            for j in start..<end { peak = max(peak, abs(data[0][j])) }
            return CGFloat(min(1, peak))
        }
        DispatchQueue.main.async { [weak self] in self?.waveform.trackSamples = samples }
    }

    @objc func playTrack() { session.startTrack() }
    @objc func stopTrack() { session.stopTrack() }

    @objc func outputVolumeChanged(_ sender: NSSlider) {
        let value = Float(max(0, min(1, sender.doubleValue)))
        session.outputVolume = value
        session.engine.mainMixerNode.outputVolume = value
        volumeLabel.stringValue = "OUT \(Int(value * 100))%"
    }

    @objc func inputGainChanged(_ sender: NSSlider) {
        session.inputGain = Float(sender.doubleValue)
        inputGainLabel.stringValue = String(format: "GAIN %.1f×", sender.doubleValue)
    }

    @objc func monitorVolumeChanged(_ sender: NSSlider) {
        session.monitorVolume = Float(sender.doubleValue)
        monitorLabel.stringValue = "MON \(Int(sender.doubleValue * 100))%"
    }

    @objc func inputDeviceChanged(_ sender: NSPopUpButton) {
        let index = sender.indexOfSelectedItem
        guard index >= 0, index < availableInputs.count else { return }
        armed = false; arm.title = "ARM"
        refreshDevices(select: availableInputs[index])
        status.stringValue = "Switched to \(availableInputs[index].name)"
    }

    @objc func inputChannelChanged(_ sender: NSPopUpButton) {
        let index = sender.indexOfSelectedItem
        guard index >= 0, index < max(session.inputChannelCount, 0) else { return }
        session.inputChannel = index
        meters.selected = index
        status.stringValue = "Capturing input channel \(index + 1)"
    }

    @objc func toggleArm() {
        armed.toggle()
        session.setMonitoring(armed)
        arm.title = armed ? "ARMED" : "ARM"
        status.stringValue = armed ? "Input monitor live · headphones recommended" : "Input monitor off"
    }

    @objc func toggleRecord() {
        guard armed else { status.stringValue = "Press ARM before recording"; return }
        if recording {
            session.endTake(role: role.stringValue, section: section.stringValue, notes: notes.stringValue)
            record.title = "● Record Dub"; recording = false
        } else {
            session.beginTake(role: role.stringValue, section: section.stringValue, notes: notes.stringValue)
            record.title = "■ Stop + Save"; recording = true
        }
    }
}

final class AppDelegate: NSObject, NSApplicationDelegate {
    var controller: DubWindowController?

    func applicationDidFinishLaunching(_ notification: Notification) {
        let args = CommandLine.arguments.dropFirst()
        if args.first == "--list-devices" {
            for d in Hardware.devices() {
                let duplex = d.canRunFullDuplex ? " · full-duplex" : ""
                print("\(d.name) · input \(d.inputChannels) ch · output \(d.outputChannels) ch · id \(d.id)\(duplex)")
            }
            NSApp.terminate(nil); return
        }
        if args.first == "--latency" {
            let hint = args.dropFirst().first.map { String($0) } ?? "Scarlett"
            guard let device = Hardware.devices().first(where: {
                $0.name.localizedCaseInsensitiveContains(hint)
            }) else {
                print("no device matching \(hint)")
                NSApp.terminate(nil); return
            }
            print(Hardware.latencyReport(device))
            NSApp.terminate(nil); return
        }
        // Headless level check: binds an input and reports per-channel peaks, so a
        // dead XLR channel (no phantom power, gain down) can be told apart from a
        // routing or permission fault without opening the UI.
        if args.first == "--meter" {
            let rest = Array(args.dropFirst())
            let wanted = rest.first { !$0.hasPrefix("-") }
            let reportPath = rest.first { $0.hasPrefix("--out=") }?.replacingOccurrences(of: "--out=", with: "")
            meter(deviceHint: wanted, seconds: 6, reportPath: reportPath)
            return
        }

        guard let track = args.first else {
            print("usage: DubWizard <track-audio> [dubs-output-dir]")
            print("       DubWizard --list-devices")
            print("       DubWizard --meter [device-name-fragment] [--out=/path/report.txt]")
            NSApp.terminate(nil); return
        }
        let trackURL = URL(fileURLWithPath: String(track))
        let output = args.dropFirst().first.map(URL.init(fileURLWithPath:))
            ?? trackURL.deletingLastPathComponent().appendingPathComponent("voice-takes/dubs")
        controller = DubWindowController(session: DubSession(trackURL: trackURL, outputDir: output))
        controller?.window?.isReleasedWhenClosed = false
        controller?.window?.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary]
        controller?.showWindow(nil)
        controller?.window?.center()
        controller?.window?.makeKeyAndOrderFront(nil)
        controller?.window?.orderFrontRegardless()
        NSApp.activate(ignoringOtherApps: true)

        // Touch NO audio until the mic grant is decided. Instantiating
        // AVAudioEngine's I/O unit blocks inside AudioComponentInstanceNew while a
        // permission prompt is pending — through outputNode just as much as
        // inputNode, since the unit is created with input enabled either way. Do
        // it on the main thread and the window never leaves 0×0, which reads as
        // "the app has no UI" rather than "the app is waiting on you".
        // Once TCC has decided (granted OR denied) instantiation no longer blocks,
        // so the graph is safe to build in the callback.
        MicPermission.request { [weak self] granted in
            self?.controller?.refreshDevices(select: nil, includeInput: granted)
            self?.controller?.status.stringValue = granted
                ? "Microphone granted · pick your input, then ARM"
                : "Playback only · microphone \(MicPermission.state)"
        }
    }

    // A floating utility panel can transiently report no visible windows while
    // CoreAudio/TCC changes focus. Do not interpret that as an instruction to
    // quit; DubWizard should close only from an explicit app/window action.
    func applicationShouldTerminateAfterLastWindowClosed(_ sender: NSApplication) -> Bool { false }


    private func meter(deviceHint: String?, seconds: Int, reportPath: String?) {
        var lines: [String] = []
        func emit(_ s: String) {
            print(s)
            lines.append(s)
            if let reportPath {
                try? lines.joined(separator: "\n").write(toFile: reportPath, atomically: true, encoding: .utf8)
            }
        }

        MicPermission.request { granted in
            emit("microphone: \(MicPermission.state)")
            guard granted else { emit("DONE"); NSApp.terminate(nil); return }

            let target = deviceHint.flatMap { hint in
                Hardware.inputs().first { $0.name.localizedCaseInsensitiveContains(hint) }
            } ?? Hardware.preferredInput()
            guard let target else { emit("no input device"); emit("DONE"); NSApp.terminate(nil); return }

            let session = DubSession(trackURL: URL(fileURLWithPath: "/dev/null"), outputDir: URL(fileURLWithPath: "/tmp"))
            session.rebuild(with: target)
            emit("device: \(target.name) · \(target.inputChannels) in / \(target.outputChannels) out")
            emit("engine input channels: \(session.inputChannelCount)")
            guard session.monitorAvailable else { emit("no usable input format"); emit("DONE"); NSApp.terminate(nil); return }
            do { try session.startEngineIfNeeded() } catch {
                emit("engine start failed: \(error.localizedDescription)"); emit("DONE"); NSApp.terminate(nil); return
            }
            emit("listening \(seconds)s — make noise into the mic")

            var tick = 0
            var loudest: Float = 0
            Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { timer in
                tick += 1
                let levels = session.channelPeaks()
                loudest = max(loudest, levels.max() ?? 0)
                let row = levels.enumerated()
                    .map { "ch\($0.offset + 1)=\(String(format: "%.5f", $0.element))" }
                    .joined(separator: "  ")
                emit("  t=\(tick)s  \(row)")
                if tick >= seconds {
                    timer.invalidate()
                    emit(loudest > 0.0005
                         ? "SIGNAL PRESENT — loudest \(String(format: "%.5f", loudest))"
                         : "SILENT — no signal on any channel (check 48V phantom power, gain knob, cable)")
                    emit("DONE")
                    NSApp.terminate(nil)
                }
            }
        }
    }
}

let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.setActivationPolicy(.regular)
app.run()
