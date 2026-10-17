import AVFoundation
import XCTest
@testable import MenuBand

final class AudioTelemetryTests: XCTestCase {
    func testLiveDevicePropertiesRemainScalarsWhenStoredInAnyDictionary() throws {
        guard let device = MenuBandAudioDevices.systemDefaultOutputID(), device != 0 else {
            throw XCTSkip("No audio output device available")
        }
        let telemetry = MenuBandAudioTelemetry(enabled: false)
        for _ in 0..<3 {
            let snapshot = telemetry.deviceSnapshot(device)
            XCTAssertNotNil(snapshot["alive"] as? UInt32)
            XCTAssertNotNil(snapshot["bufferFrames"] as? UInt32)
            XCTAssertNotNil(snapshot["rate"] as? Double)
            XCTAssertNoThrow(try JSONSerialization.data(withJSONObject: snapshot))
        }
    }

    private func buffer(_ left: [Float], _ right: [Float]) -> AVAudioPCMBuffer {
        let format = AVAudioFormat(standardFormatWithSampleRate: 44_100, channels: 2)!
        let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: AVAudioFrameCount(left.count))!
        buffer.frameLength = buffer.frameCapacity
        for i in left.indices {
            buffer.floatChannelData![0][i] = left[i]
            buffer.floatChannelData![1][i] = right[i]
        }
        return buffer
    }

    func testOppositePhaseStereoIsNotReportedAsSilence() throws {
        let meter = AudioSignalMeter()
        let audio = buffer([0.5, 0.5], [-0.5, -0.5])
        meter.observe(audio.audioBufferList, frames: 2, at: 10)
        let snapshot = meter.takeSnapshot(at: 11)
        XCTAssertEqual(snapshot.peakDBFS, -6.0206, accuracy: 0.001)
        XCTAssertEqual(snapshot.rmsDBFS, -6.0206, accuracy: 0.001)
        XCTAssertEqual(snapshot.measuredBuffers, 1)
        XCTAssertEqual(snapshot.lastSignalAgeMs, 1000)
    }

    func testInvalidSamplesAreCountedWithoutPoisoningJSONOrFiniteSignal() throws {
        let meter = AudioSignalMeter()
        let audio = buffer([.nan, 0.25], [.infinity, -.infinity])
        meter.observe(audio.audioBufferList, frames: 2, at: 0)
        let snapshot = meter.takeSnapshot(at: 1)
        XCTAssertEqual(snapshot.invalidSamples, 3)
        XCTAssertEqual(snapshot.peakDBFS, -12.0412, accuracy: 0.001)
        XCTAssertNoThrow(try JSONEncoder().encode(snapshot))
    }

    func testSilentFlagIgnoresUndefinedContentsButTracksCallbacks() {
        let meter = AudioSignalMeter()
        let audio = buffer([.nan, 1], [.nan, 1])
        meter.observe(audio.audioBufferList, frames: 2, silent: true, at: 0)
        let snapshot = meter.takeSnapshot(at: 1)
        XCTAssertEqual(snapshot.callbacks, 1)
        XCTAssertEqual(snapshot.measuredBuffers, 1)
        XCTAssertEqual(snapshot.peakDBFS, -160)
        XCTAssertEqual(snapshot.invalidSamples, 0)
        XCTAssertEqual(snapshot.silentFlagBuffers, 1)
        XCTAssertNil(snapshot.lastSignalAgeMs)
    }

    func testMissingHardwareBufferIsUnknownRatherThanMeasuredSilence() {
        let meter = AudioSignalMeter()
        meter.observe(nil, frames: 16, error: true, at: 0)
        let snapshot = meter.takeSnapshot(at: 1)
        XCTAssertEqual(snapshot.callbacks, 1)
        XCTAssertEqual(snapshot.measuredBuffers, 0)
        XCTAssertEqual(snapshot.samples, 0)
        XCTAssertEqual(snapshot.renderErrors, 1)
    }

    func testIntervalCountersResetButStallAndLastSignalAgeSurvive() {
        let meter = AudioSignalMeter()
        let audio = buffer([0.5], [0])
        meter.observe(audio.audioBufferList, frames: 1, at: 10)
        _ = meter.takeSnapshot(at: 11)
        let stopped = meter.takeSnapshot(at: 13)
        XCTAssertEqual(stopped.callbacks, 0)
        XCTAssertEqual(stopped.lastCallbackAgeMs, 3000)
        XCTAssertEqual(stopped.lastSignalAgeMs, 3000)
        meter.observe(audio.audioBufferList, frames: 1, silent: true, at: 14)
        let resumed = meter.takeSnapshot(at: 15)
        XCTAssertEqual(resumed.maxGapMs, 4000)
        XCTAssertEqual(resumed.lastSignalAgeMs, 5000)
    }

    func testDiagnosticFilesRotateAndEveryLineRemainsValidJSON() throws {
        let directory = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        defer { try? FileManager.default.removeItem(at: directory) }
        let log = AudioDiagnosticLog(directory: directory, maximumBytes: 512)
        for i in 0..<40 { try log.append(["event": "snapshot", "sequence": i]) }
        let files = try FileManager.default.contentsOfDirectory(at: directory, includingPropertiesForKeys: nil)
        XCTAssertEqual(files.count, 2)
        for file in files {
            let data = try Data(contentsOf: file)
            XCTAssertLessThanOrEqual(data.count, 512)
            for line in data.split(separator: 0x0a) {
                XCTAssertNoThrow(try JSONSerialization.jsonObject(with: Data(line)))
            }
        }
        let last = try String(contentsOf: log.url).split(separator: "\n").last!
        let row = try JSONSerialization.jsonObject(with: Data(last.utf8)) as! [String: Any]
        XCTAssertEqual(row["sequence"] as? Int, 39)
    }
}
