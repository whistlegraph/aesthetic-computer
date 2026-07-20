import XCTest
@testable import ACMacAudio

final class RoomProtocolTests: XCTestCase {
    func testHelloRoundTrip() {
        let message = ACRoomMessage.hello(.init(channel: .right, name: "blueberry"))
        XCTAssertEqual(ACRoomMessage(data: message.encode()), message)
    }

    func testClockRoundTrip() {
        let message = ACRoomMessage.clockResponse(.init(
            nonce: 42, clientSendNanos: 10, serverReceiveNanos: 20, serverSendNanos: 21))
        XCTAssertEqual(ACRoomMessage(data: message.encode()), message)
    }

    func testAudioRoundTripAndMTU() {
        let pcm = Data(repeating: 0x7f, count: ACRoomWire.packetFrames * 2 * 2)
        let message = ACRoomMessage.audio(.init(
            session: 7, sequence: 99, presentationNanos: 123_456,
            sampleRate: 48_000, frameCount: UInt16(ACRoomWire.packetFrames),
            channels: 2, pcm16: pcm))
        let encoded = message.encode()
        XCTAssertLessThan(encoded.count, 1_300)
        XCTAssertEqual(ACRoomMessage(data: encoded), message)
    }

    func testRejectsTruncatedPacket() {
        let encoded = ACRoomMessage.hello(.init(channel: .left, name: "neo")).encode()
        XCTAssertNil(ACRoomMessage(data: encoded.dropLast()))
    }
}
