import Foundation

/// The deliberately small wire format shared by every Aesthetic Computer Mac
/// audio sender and receiver. Datagrams stay below the normal Wi-Fi MTU.
public enum ACRoomWire {
    public static let magic: UInt32 = 0x4143_524d // "ACRM"
    public static let version: UInt8 = 1
    public static let sampleRate: Double = 48_000
    public static let packetFrames = 240       // 5 ms, 960-byte stereo payload
    public static let defaultPort: UInt16 = 17_788

    public enum Kind: UInt8 {
        case hello = 1
        case clockRequest = 2
        case clockResponse = 3
        case audio = 4
    }

    public enum Channel: UInt8, CaseIterable, Sendable {
        case stereo = 0
        case left = 1
        case right = 2
        case mono = 3

        public init?(name: String) {
            switch name.lowercased() {
            case "stereo", "lr": self = .stereo
            case "left", "l": self = .left
            case "right", "r": self = .right
            case "mono", "mix", "both": self = .mono
            default: return nil
            }
        }

        public var name: String {
            switch self {
            case .stereo: "stereo"
            case .left: "left"
            case .right: "right"
            case .mono: "mono"
            }
        }
    }
}

public struct ACRoomHello: Equatable, Sendable {
    public let channel: ACRoomWire.Channel
    public let name: String

    public init(channel: ACRoomWire.Channel, name: String) {
        self.channel = channel
        self.name = name
    }
}

public struct ACRoomClockRequest: Equatable, Sendable {
    public let nonce: UInt32
    public let clientSendNanos: UInt64
}

public struct ACRoomClockResponse: Equatable, Sendable {
    public let nonce: UInt32
    public let clientSendNanos: UInt64
    public let serverReceiveNanos: UInt64
    public let serverSendNanos: UInt64
}

public struct ACRoomAudioPacket: Equatable, Sendable {
    public let session: UInt32
    public let sequence: UInt32
    public let presentationNanos: UInt64
    public let sampleRate: UInt32
    public let frameCount: UInt16
    public let channels: UInt8
    public let pcm16: Data

    public init(session: UInt32, sequence: UInt32, presentationNanos: UInt64,
                sampleRate: UInt32, frameCount: UInt16, channels: UInt8, pcm16: Data) {
        self.session = session
        self.sequence = sequence
        self.presentationNanos = presentationNanos
        self.sampleRate = sampleRate
        self.frameCount = frameCount
        self.channels = channels
        self.pcm16 = pcm16
    }
}

public enum ACRoomMessage: Equatable, Sendable {
    case hello(ACRoomHello)
    case clockRequest(ACRoomClockRequest)
    case clockResponse(ACRoomClockResponse)
    case audio(ACRoomAudioPacket)

    public func encode() -> Data {
        var data = Data()
        data.appendBE(ACRoomWire.magic)
        data.append(ACRoomWire.version)
        switch self {
        case .hello(let value):
            data.append(ACRoomWire.Kind.hello.rawValue)
            data.appendBE(UInt16(0))
            data.append(value.channel.rawValue)
            let bytes = Data(value.name.utf8.prefix(96))
            data.append(UInt8(bytes.count))
            data.append(bytes)
        case .clockRequest(let value):
            data.append(ACRoomWire.Kind.clockRequest.rawValue)
            data.appendBE(UInt16(0))
            data.appendBE(value.nonce)
            data.appendBE(value.clientSendNanos)
        case .clockResponse(let value):
            data.append(ACRoomWire.Kind.clockResponse.rawValue)
            data.appendBE(UInt16(0))
            data.appendBE(value.nonce)
            data.appendBE(value.clientSendNanos)
            data.appendBE(value.serverReceiveNanos)
            data.appendBE(value.serverSendNanos)
        case .audio(let value):
            data.append(ACRoomWire.Kind.audio.rawValue)
            data.appendBE(UInt16(0))
            data.appendBE(value.session)
            data.appendBE(value.sequence)
            data.appendBE(value.presentationNanos)
            data.appendBE(value.sampleRate)
            data.appendBE(value.frameCount)
            data.append(value.channels)
            data.append(UInt8(1)) // codec 1 = signed 16-bit interleaved PCM
            data.append(value.pcm16)
        }
        return data
    }

    public init?(data: Data) {
        var cursor = DataCursor(data)
        guard cursor.readUInt32() == ACRoomWire.magic,
              cursor.readUInt8() == ACRoomWire.version,
              let kindRaw = cursor.readUInt8(),
              let kind = ACRoomWire.Kind(rawValue: kindRaw),
              cursor.readUInt16() != nil else { return nil }

        switch kind {
        case .hello:
            guard let roleRaw = cursor.readUInt8(),
                  let role = ACRoomWire.Channel(rawValue: roleRaw),
                  let count = cursor.readUInt8(),
                  let nameData = cursor.readData(count: Int(count)),
                  let name = String(data: nameData, encoding: .utf8) else { return nil }
            self = .hello(.init(channel: role, name: name))
        case .clockRequest:
            guard let nonce = cursor.readUInt32(), let sent = cursor.readUInt64() else { return nil }
            self = .clockRequest(.init(nonce: nonce, clientSendNanos: sent))
        case .clockResponse:
            guard let nonce = cursor.readUInt32(),
                  let sent = cursor.readUInt64(),
                  let received = cursor.readUInt64(),
                  let returned = cursor.readUInt64() else { return nil }
            self = .clockResponse(.init(nonce: nonce, clientSendNanos: sent,
                                        serverReceiveNanos: received, serverSendNanos: returned))
        case .audio:
            guard let session = cursor.readUInt32(),
                  let sequence = cursor.readUInt32(),
                  let presentation = cursor.readUInt64(),
                  let rate = cursor.readUInt32(),
                  let frames = cursor.readUInt16(),
                  let channels = cursor.readUInt8(),
                  cursor.readUInt8() == 1,
                  channels == 1 || channels == 2,
                  let pcm = cursor.readData(count: Int(frames) * Int(channels) * 2),
                  cursor.isAtEnd else { return nil }
            self = .audio(.init(session: session, sequence: sequence,
                                presentationNanos: presentation, sampleRate: rate,
                                frameCount: frames, channels: channels, pcm16: pcm))
        }
    }
}

extension Data {
    fileprivate mutating func appendBE<T: FixedWidthInteger>(_ value: T) {
        var value = value.bigEndian
        Swift.withUnsafeBytes(of: &value) { append(contentsOf: $0) }
    }
}

private struct DataCursor {
    let data: Data
    var offset = 0
    var isAtEnd: Bool { offset == data.count }

    init(_ data: Data) { self.data = data }

    mutating func readUInt8() -> UInt8? {
        guard offset < data.count else { return nil }
        defer { offset += 1 }
        return data[offset]
    }

    mutating func readUInt16() -> UInt16? {
        guard let bytes = readData(count: 2) else { return nil }
        return bytes.reduce(UInt16(0)) { ($0 << 8) | UInt16($1) }
    }

    mutating func readUInt32() -> UInt32? {
        guard let bytes = readData(count: 4) else { return nil }
        return bytes.reduce(UInt32(0)) { ($0 << 8) | UInt32($1) }
    }

    mutating func readUInt64() -> UInt64? {
        guard let bytes = readData(count: 8) else { return nil }
        return bytes.reduce(UInt64(0)) { ($0 << 8) | UInt64($1) }
    }

    mutating func readData(count: Int) -> Data? {
        guard count >= 0, offset + count <= data.count else { return nil }
        defer { offset += count }
        return data.subdata(in: offset..<(offset + count))
    }
}
