import AVFoundation
import XCTest
@testable import MenuBand

final class CDJRadioTests: XCTestCase {
    func testSourceLabelsAreDeckSourcesNotInstrumentNumbers() {
        XCTAssertEqual(CDJRadioSource.station(.nts1).label, "NTS1")
        XCTAssertEqual(CDJRadioSource.spotify.label, "SPOTIFY")
    }

    func testCDJBufferLoadsIntoPianoSampler() throws {
        let format = try XCTUnwrap(AVAudioFormat(
            commonFormat: .pcmFormatFloat32, sampleRate: 48_000,
            channels: 2, interleaved: false))
        let frames = 24_000
        let buffer = try XCTUnwrap(AVAudioPCMBuffer(
            pcmFormat: format, frameCapacity: AVAudioFrameCount(frames)))
        buffer.frameLength = AVAudioFrameCount(frames)
        let channels = try XCTUnwrap(buffer.floatChannelData)
        for frame in 0..<frames {
            let sample = sinf(Float(frame) * 2 * .pi * 220 / 48_000) * 0.25
            channels[0][frame] = sample
            channels[1][frame] = sample
        }

        let sampler = MenuBandSampleVoice()
        XCTAssertTrue(sampler.loadRecording(from: buffer))
        XCTAssertTrue(sampler.hasRecording)
    }

    func testTooShortCDJBufferDoesNotReplaceSampler() throws {
        let format = try XCTUnwrap(AVAudioFormat(
            commonFormat: .pcmFormatFloat32, sampleRate: 44_100,
            channels: 2, interleaved: false))
        let buffer = try XCTUnwrap(AVAudioPCMBuffer(
            pcmFormat: format, frameCapacity: 128))
        buffer.frameLength = 128

        let sampler = MenuBandSampleVoice()
        XCTAssertFalse(sampler.loadRecording(from: buffer))
        XCTAssertFalse(sampler.hasRecording)
    }
}
