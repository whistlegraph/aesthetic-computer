import AVFoundation
import XCTest
@testable import MenuBand

final class MenuBandAirVoiceTests: XCTestCase {
    private let rate: Double = 44_100
    private lazy var format = AVAudioFormat(
        commonFormat: .pcmFormatFloat32, sampleRate: rate,
        channels: 1, interleaved: false)!

    /// Goertzel power at one frequency over the whole loop.
    private func power(_ data: UnsafeMutablePointer<Float>, count: Int,
                       at hz: Double) -> Double {
        let k = 2 * cos(2 * Double.pi * hz / rate)
        var s0 = 0.0, s1 = 0.0, s2 = 0.0
        for i in 0..<count {
            s0 = Double(data[i]) + k * s1 - s2
            s2 = s1; s1 = s0
        }
        return s1 * s1 + s2 * s2 - k * s1 * s2
    }

    func testLoopIsTwoSecondsPeakNormalizedAndSeamless() throws {
        let loop = try XCTUnwrap(MenuBandAirVoice.makeLoop(
            pitches: [60], sampleRate: rate, format: format))
        XCTAssertEqual(Int(loop.frameLength), Int(rate * 2))
        let data = try XCTUnwrap(loop.floatChannelData?[0])
        var peak: Float = 0
        for i in 0..<Int(loop.frameLength) { peak = max(peak, abs(data[i])) }
        XCTAssertEqual(peak, 0.6, accuracy: 0.02)
        // The seam is no bigger a step than the signal takes anywhere else.
        let n = Int(loop.frameLength)
        var biggest: Float = 0
        for i in 1..<n { biggest = max(biggest, abs(data[i] - data[i - 1])) }
        XCTAssertLessThanOrEqual(abs(data[n - 1] - data[0]), biggest)
    }

    func testAirWhistlesAtTheChordItWasGiven() throws {
        // A minor: A3, C4, E4. Expect energy at each, and little in between.
        let loop = try XCTUnwrap(MenuBandAirVoice.makeLoop(
            pitches: [57, 60, 64], sampleRate: rate, format: format))
        let data = try XCTUnwrap(loop.floatChannelData?[0])
        let n = Int(loop.frameLength)
        let a3 = power(data, count: n, at: 220)
        let c4 = power(data, count: n, at: 261.63)
        let e4 = power(data, count: n, at: 329.63)
        let between = power(data, count: n, at: 300)
        let far = power(data, count: n, at: 1_000)
        for tone in [a3, c4, e4] {
            XCTAssertGreaterThan(tone, between * 8)
            XCTAssertGreaterThan(tone, far * 8)
        }
    }

    func testEmptyPitchesStillMakeAir() {
        XCTAssertNotNil(MenuBandAirVoice.makeLoop(
            pitches: [], sampleRate: rate, format: format))
    }
}
