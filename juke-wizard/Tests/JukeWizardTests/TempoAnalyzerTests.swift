import XCTest
@testable import JukeWizard

final class TempoAnalyzerTests: XCTestCase {
    func testDetectsFourOnFloorTempo() throws {
        let sampleRate = 8_000.0
        let bpm = 128.0
        let duration = 20.0
        var samples = [Float](repeating: 0, count: Int(sampleRate * duration))
        let beatFrames = Int((60 / bpm) * sampleRate)
        for start in stride(from: 0, to: samples.count, by: beatFrames) {
            for offset in 0..<min(80, samples.count - start) {
                samples[start + offset] = Float(exp(-Double(offset) / 16))
            }
        }
        let estimate = try XCTUnwrap(DJTempoAnalyzer.estimate(samples: samples, sampleRate: sampleRate))
        XCTAssertEqual(estimate, bpm, accuracy: 1.0)
    }
}
