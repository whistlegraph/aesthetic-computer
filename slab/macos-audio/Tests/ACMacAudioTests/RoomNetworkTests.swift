import AVFoundation
import Network
import XCTest
@testable import ACMacAudio

final class RoomNetworkTests: XCTestCase {
    func testSenderAnswersClockAndEmitsAudioWithoutAnAudioDevice() throws {
        let port: UInt16 = 27_788
        let sender = ACAudioRoomSender(configuration: .init(port: port, latencyMilliseconds: 500))
        try sender.start()
        defer { sender.stop() }

        let clockReply = expectation(description: "clock reply")
        let audioPacket = expectation(description: "audio packet")
        let queue = DispatchQueue(label: "room-network-test")
        let connection = NWConnection(host: "127.0.0.1", port: NWEndpoint.Port(rawValue: port)!, using: .udp)

        func receive() {
            connection.receiveMessage { data, _, _, error in
                if let data, let message = ACRoomMessage(data: data) {
                    switch message {
                    case .clockResponse: clockReply.fulfill()
                    case .audio: audioPacket.fulfill()
                    default: break
                    }
                }
                if error == nil { receive() }
            }
        }

        connection.stateUpdateHandler = { state in
            guard case .ready = state else { return }
            connection.send(content: ACRoomMessage.hello(.init(channel: .left, name: "test")).encode(),
                            completion: .contentProcessed { _ in })
            connection.send(content: ACRoomMessage.clockRequest(.init(
                nonce: 1, clientSendNanos: ACHostClock.nowNanos())).encode(),
                            completion: .contentProcessed { _ in })
            let format = AVAudioFormat(commonFormat: .pcmFormatFloat32, sampleRate: 48_000,
                                       channels: 2, interleaved: false)!
            let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: 240)!
            buffer.frameLength = 240
            queue.asyncAfter(deadline: .now() + 0.2) { sender.send(buffer) }
        }
        connection.start(queue: queue)
        receive()
        wait(for: [clockReply, audioPacket], timeout: 3)
        connection.cancel()
    }
}
