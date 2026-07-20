import AVFoundation
import Foundation
import Network

public final class ACAudioRoomSender: @unchecked Sendable {
    public struct Configuration: Sendable {
        public var port: UInt16
        public var latencyMilliseconds: UInt64

        public init(port: UInt16 = ACRoomWire.defaultPort, latencyMilliseconds: UInt64 = 700) {
            self.port = port
            self.latencyMilliseconds = latencyMilliseconds
        }
    }

    public var onLog: (@Sendable (String) -> Void)?

    private let configuration: Configuration
    private let queue = DispatchQueue(label: "computer.aesthetic.audio-room.sender", qos: .userInitiated)
    private let lock = NSLock()
    private var listener: NWListener?
    private var clients: [ObjectIdentifier: NWConnection] = [:]
    private var clientDescriptions: [ObjectIdentifier: ACRoomHello] = [:]
    private var converter: AVAudioConverter?
    private var converterInputFormat: AVAudioFormat?
    private let networkFormat = AVAudioFormat(commonFormat: .pcmFormatFloat32,
                                               sampleRate: ACRoomWire.sampleRate,
                                               channels: 2, interleaved: false)!
    private var session = UInt32.random(in: 1...UInt32.max)
    private var sequence: UInt32 = 0
    private var nextPresentationNanos: UInt64 = 0
    private var lastAudioNanos: UInt64 = 0

    public init(configuration: Configuration = .init()) {
        self.configuration = configuration
    }

    public var connectedClients: [ACRoomHello] {
        lock.lock(); defer { lock.unlock() }
        return Array(clientDescriptions.values)
    }

    public func start() throws {
        guard listener == nil else { return }
        guard let port = NWEndpoint.Port(rawValue: configuration.port) else {
            throw ACAudioRoomError.invalidPort(configuration.port)
        }
        let listener = try NWListener(using: .udp, on: port)
        listener.newConnectionHandler = { [weak self] connection in self?.accept(connection) }
        listener.stateUpdateHandler = { [weak self] state in
            switch state {
            case .ready: self?.log("sender listening on UDP \(self?.configuration.port ?? 0)")
            case .failed(let error): self?.log("sender failed: \(error)")
            default: break
            }
        }
        self.listener = listener
        listener.start(queue: queue)
    }

    public func stop() {
        listener?.cancel()
        listener = nil
        lock.lock()
        let current = Array(clients.values)
        clients.removeAll()
        clientDescriptions.removeAll()
        lock.unlock()
        current.forEach { $0.cancel() }
    }

    /// Accept PCM from a Core Audio tap or any AVAudioEngine-backed app. The
    /// buffer is converted immediately because tap-owned memory expires when
    /// its callback returns.
    public func send(_ input: AVAudioPCMBuffer) {
        queue.async { [weak self] in self?.convertAndBroadcast(input) }
    }

    private func accept(_ connection: NWConnection) {
        let id = ObjectIdentifier(connection)
        lock.lock(); clients[id] = connection; lock.unlock()
        connection.stateUpdateHandler = { [weak self, weak connection] state in
            guard let self, let connection else { return }
            if case .failed = state { self.remove(connection) }
            if case .cancelled = state { self.remove(connection) }
        }
        connection.start(queue: queue)
        receive(on: connection)
    }

    private func receive(on connection: NWConnection) {
        connection.receiveMessage { [weak self, weak connection] data, _, _, error in
            guard let self, let connection else { return }
            let received = ACHostClock.nowNanos()
            if let data, let message = ACRoomMessage(data: data) {
                switch message {
                case .hello(let hello):
                    let id = ObjectIdentifier(connection)
                    self.lock.lock(); self.clientDescriptions[id] = hello; self.lock.unlock()
                    self.log("client \(hello.name) joined as \(hello.channel.name)")
                case .clockRequest(let request):
                    let sent = ACHostClock.nowNanos()
                    let response = ACRoomMessage.clockResponse(.init(
                        nonce: request.nonce, clientSendNanos: request.clientSendNanos,
                        serverReceiveNanos: received, serverSendNanos: sent))
                    connection.send(content: response.encode(), completion: .contentProcessed { _ in })
                default: break
                }
            }
            if error == nil { self.receive(on: connection) } else { self.remove(connection) }
        }
    }

    private func remove(_ connection: NWConnection) {
        let id = ObjectIdentifier(connection)
        lock.lock()
        let hello = clientDescriptions.removeValue(forKey: id)
        clients.removeValue(forKey: id)
        lock.unlock()
        if let hello { log("client \(hello.name) left") }
    }

    private func convertAndBroadcast(_ input: AVAudioPCMBuffer) {
        let inputFormat = input.format
        if converter == nil || converterInputFormat != inputFormat {
            converter = AVAudioConverter(from: inputFormat, to: networkFormat)
            converterInputFormat = inputFormat
        }
        guard let converter else { return }
        let ratio = ACRoomWire.sampleRate / inputFormat.sampleRate
        let capacity = AVAudioFrameCount(ceil(Double(input.frameLength) * ratio) + 16)
        guard let output = AVAudioPCMBuffer(pcmFormat: networkFormat, frameCapacity: capacity) else { return }
        var supplied = false
        var conversionError: NSError?
        let status = converter.convert(to: output, error: &conversionError) { _, outStatus in
            if supplied {
                outStatus.pointee = .noDataNow
                return nil
            }
            supplied = true
            outStatus.pointee = .haveData
            return input
        }
        guard status != .error, conversionError == nil, output.frameLength > 0,
              let channels = output.floatChannelData else {
            log("PCM conversion failed: \(conversionError?.localizedDescription ?? "unknown error")")
            return
        }

        let now = ACHostClock.nowNanos()
        if nextPresentationNanos == 0 || now > lastAudioNanos + 250_000_000 {
            session = UInt32.random(in: 1...UInt32.max)
            sequence = 0
            nextPresentationNanos = now + configuration.latencyMilliseconds * 1_000_000
        }
        lastAudioNanos = now

        var offset = 0
        let total = Int(output.frameLength)
        while offset < total {
            let count = min(ACRoomWire.packetFrames, total - offset)
            var pcm = Data(capacity: count * 4)
            for frame in 0..<count {
                for channel in 0..<2 {
                    let sample = max(-1, min(1, channels[channel][offset + frame]))
                    var quantized = Int16(sample * Float(Int16.max)).littleEndian
                    Swift.withUnsafeBytes(of: &quantized) { pcm.append(contentsOf: $0) }
                }
            }
            let packet = ACRoomAudioPacket(
                session: session, sequence: sequence,
                presentationNanos: nextPresentationNanos,
                sampleRate: UInt32(ACRoomWire.sampleRate), frameCount: UInt16(count),
                channels: 2, pcm16: pcm)
            broadcast(ACRoomMessage.audio(packet).encode())
            sequence &+= 1
            nextPresentationNanos += UInt64(Double(count) * 1_000_000_000 / ACRoomWire.sampleRate)
            offset += count
        }
    }

    private func broadcast(_ data: Data) {
        lock.lock(); let targets = Array(clients.values); lock.unlock()
        for client in targets {
            client.send(content: data, completion: .contentProcessed { [weak self, weak client] error in
                if error != nil, let client { self?.remove(client) }
            })
        }
    }

    private func log(_ message: String) { onLog?(message) }

    deinit { stop() }
}

public enum ACAudioRoomError: LocalizedError {
    case invalidPort(UInt16)
    case unavailable(String)
    case coreAudio(String, OSStatus)

    public var errorDescription: String? {
        switch self {
        case .invalidPort(let port): "Invalid room-audio port \(port)"
        case .unavailable(let reason): reason
        case .coreAudio(let operation, let status): "\(operation) failed (Core Audio \(status))"
        }
    }
}
