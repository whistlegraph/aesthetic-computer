import XCTest
import AVFoundation
@testable import MenuBand

final class TapeExportSnapshotTests: XCTestCase {
    private func signal(_ value: Float, frames: Int = 4096) -> AVAudioPCMBuffer {
        let format = AVAudioFormat(standardFormatWithSampleRate: 44_100, channels: 2)!
        let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: AVAudioFrameCount(frames))!
        buffer.frameLength = AVAudioFrameCount(frames)
        for ch in 0..<2 { buffer.floatChannelData![ch].initialize(repeating: value, count: frames) }
        return buffer
    }

    func testNextRecordingCannotChangeAnExportingTake() throws {
        let tape = MenuBandTape()
        tape.record(micAlreadyInMix: true, mix: ["mic": 0.5])
        tape.ingestSynth(signal(0.25))
        tape.ingestTones(signal(0.125))
        tape.stop()
        let snapshot = try XCTUnwrap(tape.snapshotForExport())
        XCTAssertTrue(snapshot === tape.snapshotForExport())
        let originalID = snapshot.takeID
        let originalDuration = snapshot.durationSeconds
        tape.record(micAlreadyInMix: false, mix: ["mic": 1])
        tape.ingestSynth(signal(0.75))
        tape.stop()
        XCTAssertNotEqual(tape.takeID, originalID)
        XCTAssertTrue(snapshot.micWasInMix)
        XCTAssertFalse(tape.micWasInMix)
        XCTAssertEqual(snapshot.durationSeconds, originalDuration)

        let finished = expectation(description: "background export")
        MenuBandTape.exportQueue.async {
            defer { finished.fulfill() }
            do {
                let take = try XCTUnwrap(snapshot.eject())
                defer {
                    try? FileManager.default.removeItem(at: take.file)
                    if let stems = take.stems { try? FileManager.default.removeItem(at: stems) }
                }
                let file = try AVAudioFile(forReading: take.file)
                let audio = AVAudioPCMBuffer(pcmFormat: file.processingFormat, frameCapacity: AVAudioFrameCount(file.length))!
                try file.read(into: audio)
                XCTAssertEqual(audio.floatChannelData![0][100], 0.25, accuracy: 0.001)
                let stems = try XCTUnwrap(take.stems)
                let mix = try JSONSerialization.jsonObject(with: Data(contentsOf: stems.appendingPathComponent("mix.json"))) as! [String: Any]
                XCTAssertEqual((mix["start"] as? [String: Double])?["mic"], 0.5)
            } catch { XCTFail("Export failed: \(error)") }
        }
        wait(for: [finished], timeout: 20)
    }

    func testLiveRecordingCannotBeSharedWithAnExporter() {
        let tape = MenuBandTape()
        tape.record()
        tape.ingestSynth(signal(0.25))
        XCTAssertNil(tape.snapshotForExport())
        tape.stop()
        XCTAssertNotNil(tape.snapshotForExport())
    }
}
