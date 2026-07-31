import AVFoundation
import XCTest
@testable import JukeWizard

final class DJPrimpatsTests: XCTestCase {
    func testCatalogRendersDeterministicLoopableStereoTracks() throws {
        let records = DJPrimpats.make()
        XCTAssertEqual(records.count, DJPrimpats.catalog.count)
        XCTAssertEqual(Set(records.map(\.track.url)).count, records.count)

        for record in records {
            XCTAssertEqual(record.track.lane, "primpats")
            XCTAssertEqual(record.track.title, record.metadata.title)
            XCTAssertEqual(record.waveform, .sine)
            XCTAssertTrue(record.title.contains("\(frequencyLabel(record.frequency)) Hz"))

            let file = try AVAudioFile(forReading: record.track.url)
            XCTAssertEqual(file.processingFormat.channelCount, 2)
            XCTAssertEqual(file.length, AVAudioFramePosition(file.processingFormat.sampleRate * record.metadata.duration))

            let frameCount = AVAudioFrameCount(file.length)
            let buffer = try XCTUnwrap(AVAudioPCMBuffer(
                pcmFormat: file.processingFormat,
                frameCapacity: frameCount
            ))
            try file.read(into: buffer)
            let channels = try XCTUnwrap(buffer.floatChannelData)
            let last = Int(buffer.frameLength) - 1

            XCTAssertEqual(channels[0][0], 0, accuracy: 0.000_001)
            XCTAssertEqual(channels[0][0] - channels[0][last], channels[0][1] - channels[0][0], accuracy: 0.000_001)
            XCTAssertEqual(channels[0][last], channels[1][last], accuracy: 0.000_001)
        }
    }

    func testCachedTracksRetainTheirFilesAndMetadataLookup() throws {
        let first = DJPrimpats.make()
        let dates = try Dictionary(uniqueKeysWithValues: first.map {
            ($0.track.url, try $0.track.url.resourceValues(forKeys: [.contentModificationDateKey]).contentModificationDate)
        })
        let second = DJPrimpats.make()

        XCTAssertEqual(first.map(\.track.url), second.map(\.track.url))
        for record in second {
            let date = try record.track.url.resourceValues(forKeys: [.contentModificationDateKey]).contentModificationDate
            XCTAssertEqual(date, dates[record.track.url] ?? nil)
            XCTAssertEqual(DJPrimpats.metadata(for: record.track), record.metadata)
        }
    }

    private func frequencyLabel(_ frequency: Double) -> String {
        let hundredths = Int((frequency * 100).rounded())
        if hundredths.isMultiple(of: 100) { return String(hundredths / 100) }
        if hundredths.isMultiple(of: 10) { return "\(hundredths / 100).\((hundredths % 100) / 10)" }
        return "\(hundredths / 100).\(String(format: "%02d", hundredths % 100))"
    }
}
