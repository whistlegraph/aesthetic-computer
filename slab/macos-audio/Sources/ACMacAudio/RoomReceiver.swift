import AVFoundation
import Foundation
import Network

private struct ACBufferedAudio {
    let presentationNanos: UInt64
    let frameCount: Int
    let left: [Float]
    let right: [Float]

    var endNanos: UInt64 {
        presentationNanos + UInt64(Double(frameCount) * 1_000_000_000 / ACRoomWire.sampleRate)
    }
}

private final class ACAudioPacketStore: @unchecked Sendable {
    private let lock = NSLock()
    private var session: UInt32?
    private var chunks: [ACBufferedAudio] = []

    func insert(_ packet: ACRoomAudioPacket) {
        guard packet.sampleRate == UInt32(ACRoomWire.sampleRate), packet.channels == 2 else { return }
        let needed = Int(packet.frameCount) * 4
        guard packet.pcm16.count == needed else { return }
        var left = [Float](repeating: 0, count: Int(packet.frameCount))
        var right = left
        packet.pcm16.withUnsafeBytes { raw in
            let bytes = raw.bindMemory(to: UInt8.self)
            for frame in 0..<Int(packet.frameCount) {
                let base = frame * 4
                let l = Int16(bitPattern: UInt16(bytes[base]) | UInt16(bytes[base + 1]) << 8)
                let r = Int16(bitPattern: UInt16(bytes[base + 2]) | UInt16(bytes[base + 3]) << 8)
                left[frame] = Float(l) / Float(Int16.max)
                right[frame] = Float(r) / Float(Int16.max)
            }
        }
        let chunk = ACBufferedAudio(presentationNanos: packet.presentationNanos,
                                    frameCount: Int(packet.frameCount), left: left, right: right)
        lock.lock()
        if session != packet.session { session = packet.session; chunks.removeAll(keepingCapacity: true) }
        if chunks.last?.presentationNanos ?? 0 <= chunk.presentationNanos {
            chunks.append(chunk)
        } else {
            chunks.append(chunk)
            chunks.sort { $0.presentationNanos < $1.presentationNanos }
        }
        if chunks.count > 600 { chunks.removeFirst(chunks.count - 600) }
        lock.unlock()
    }

    func render(serverStartNanos: UInt64, frameCount: Int, channel: ACRoomWire.Channel,
                gain: Float, leftOutput: UnsafeMutablePointer<Float>,
                rightOutput: UnsafeMutablePointer<Float>) {
        leftOutput.initialize(repeating: 0, count: frameCount)
        rightOutput.initialize(repeating: 0, count: frameCount)
        let renderEnd = serverStartNanos + UInt64(Double(frameCount) * 1_000_000_000 / ACRoomWire.sampleRate)
        lock.lock()
        chunks.removeAll { $0.endNanos + 50_000_000 < serverStartNanos }
        for chunk in chunks {
            if chunk.presentationNanos >= renderEnd { break }
            if chunk.endNanos <= serverStartNanos { continue }
            let sourceStart: Int
            let outputStart: Int
            if chunk.presentationNanos < serverStartNanos {
                sourceStart = Int(Double(serverStartNanos - chunk.presentationNanos) * ACRoomWire.sampleRate / 1_000_000_000)
                outputStart = 0
            } else {
                sourceStart = 0
                outputStart = Int(Double(chunk.presentationNanos - serverStartNanos) * ACRoomWire.sampleRate / 1_000_000_000)
            }
            let count = min(chunk.frameCount - sourceStart, frameCount - outputStart)
            guard count > 0 else { continue }
            for index in 0..<count {
                let l = chunk.left[sourceStart + index]
                let r = chunk.right[sourceStart + index]
                let outL: Float
                let outR: Float
                switch channel {
                case .stereo: outL = l; outR = r
                case .left: outL = l; outR = l
                case .right: outL = r; outR = r
                case .mono:
                    let mix = (l + r) * 0.5
                    outL = mix; outR = mix
                }
                leftOutput[outputStart + index] = outL * gain
                rightOutput[outputStart + index] = outR * gain
            }
        }
        lock.unlock()
    }
}

public final class ACAudioRoomReceiver: @unchecked Sendable {
    public struct Configuration: Sendable {
        public var host: String
        public var port: UInt16
        public var name: String
        public var channel: ACRoomWire.Channel
        public var gain: Float

        public init(host: String, port: UInt16 = ACRoomWire.defaultPort,
                    name: String = Host.current().localizedName ?? "Mac",
                    channel: ACRoomWire.Channel = .stereo, gain: Float = 1) {
            self.host = host
            self.port = port
            self.name = name
            self.channel = channel
            self.gain = max(0, min(2, gain))
        }
    }

    public var onLog: (@Sendable (String) -> Void)?
    public private(set) var isRunning = false

    private let configuration: Configuration
    private let queue = DispatchQueue(label: "computer.aesthetic.audio-room.receiver", qos: .userInitiated)
    private let engine = AVAudioEngine()
    private var sourceNode: AVAudioSourceNode?
    private var connection: NWConnection?
    private var timer: DispatchSourceTimer?
    private let clock = ACClockSynchronizer()
    private let store = ACAudioPacketStore()
    private var nonce: UInt32 = 0
    private var didLogAudio = false

    public init(configuration: Configuration) { self.configuration = configuration }

    public func start() throws {
        guard !isRunning else { return }
        guard let port = NWEndpoint.Port(rawValue: configuration.port) else {
            throw ACAudioRoomError.invalidPort(configuration.port)
        }
        try startAudio()
        let connection = NWConnection(host: NWEndpoint.Host(configuration.host), port: port, using: .udp)
        self.connection = connection
        connection.stateUpdateHandler = { [weak self] state in
            switch state {
            case .ready:
                guard let self else { return }
                self.log("connected to \(self.configuration.host) as \(self.configuration.channel.name)")
                self.sendHello()
                self.sendClockRequest()
            case .failed(let error): self?.log("connection failed: \(error)")
            default: break
            }
        }
        connection.start(queue: queue)
        receive()
        let timer = DispatchSource.makeTimerSource(queue: queue)
        timer.schedule(deadline: .now() + 0.25, repeating: 1.0)
        timer.setEventHandler { [weak self] in self?.sendClockRequest() }
        timer.resume()
        self.timer = timer
        isRunning = true
    }

    public func stop() {
        timer?.cancel(); timer = nil
        connection?.cancel(); connection = nil
        engine.stop()
        if let sourceNode { engine.detach(sourceNode) }
        sourceNode = nil
        isRunning = false
    }

    private func startAudio() throws {
        let format = AVAudioFormat(commonFormat: .pcmFormatFloat32,
                                   sampleRate: ACRoomWire.sampleRate,
                                   channels: 2, interleaved: false)!
        let source = AVAudioSourceNode(format: format) { [weak self] _, timestamp, frames, outputData in
            let buffers = UnsafeMutableAudioBufferListPointer(outputData)
            guard buffers.count >= 2,
                  let left = buffers[0].mData?.assumingMemoryBound(to: Float.self),
                  let right = buffers[1].mData?.assumingMemoryBound(to: Float.self) else {
                for buffer in buffers { if let data = buffer.mData { memset(data, 0, Int(buffer.mDataByteSize)) } }
                return noErr
            }
            guard let self, let offset = self.clock.offsetNanos else {
                left.initialize(repeating: 0, count: Int(frames))
                right.initialize(repeating: 0, count: Int(frames))
                return noErr
            }
            let localNanos: UInt64
            if timestamp.pointee.mFlags.contains(.hostTimeValid) {
                localNanos = ACHostClock.nanos(fromHostTime: timestamp.pointee.mHostTime)
            } else {
                localNanos = ACHostClock.nowNanos()
            }
            let serverSigned = Int64(clamping: localNanos) + offset
            guard serverSigned >= 0 else { return noErr }
            self.store.render(serverStartNanos: UInt64(serverSigned), frameCount: Int(frames),
                              channel: self.configuration.channel,
                              gain: self.configuration.gain,
                              leftOutput: left, rightOutput: right)
            return noErr
        }
        engine.attach(source)
        engine.connect(source, to: engine.mainMixerNode, format: format)
        engine.prepare()
        try engine.start()
        sourceNode = source
    }

    private func receive() {
        connection?.receiveMessage { [weak self] data, _, _, error in
            guard let self else { return }
            let arrived = ACHostClock.nowNanos()
            if let data, let message = ACRoomMessage(data: data) {
                switch message {
                case .clockResponse(let response): self.clock.add(response, arrived: arrived)
                case .audio(let packet):
                    self.store.insert(packet)
                    if !self.didLogAudio {
                        self.didLogAudio = true
                        self.log("audio locked at \(packet.sampleRate) Hz, \(self.configuration.channel.name), gain \(self.configuration.gain)")
                    }
                default: break
                }
            }
            if error == nil { self.receive() } else { self.log("receiver stopped: \(error!)") }
        }
    }

    private func sendHello() {
        let message = ACRoomMessage.hello(.init(channel: configuration.channel, name: configuration.name))
        connection?.send(content: message.encode(), completion: .contentProcessed { _ in })
    }

    private func sendClockRequest() {
        nonce &+= 1
        let message = ACRoomMessage.clockRequest(.init(nonce: nonce, clientSendNanos: ACHostClock.nowNanos()))
        connection?.send(content: message.encode(), completion: .contentProcessed { _ in })
    }

    private func log(_ message: String) { onLog?(message) }

    deinit { stop() }
}
