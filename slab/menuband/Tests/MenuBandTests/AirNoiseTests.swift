import XCTest
@testable import MenuBand

final class AirNoiseTests: XCTestCase {
    func testNoiseColorsStayBoundedAndBrownIsDarkerThanWhite() {
        func analyze(_ color: MenuBandAirColor) -> (energy: Float, motion: Float) {
            var noise = MenuBandAirNoise()
            var energy: Float = 0, motion: Float = 0, last: Float = 0, sum: Float = 0
            for frame in 0..<88200 {
                let x = noise.next(color: color, sampleRate: 44100, cutoff: 6000)
                XCTAssertTrue(x.isFinite)
                XCTAssertLessThanOrEqual(abs(x), 1)
                if frame >= 44100 {
                    energy += x*x; motion += (x-last)*(x-last); sum += x
                }
                last = x
            }
            XCTAssertLessThan(abs(sum / 44100), 0.03)
            XCTAssertGreaterThan(energy / 44100, 0.001)
            return (energy, motion)
        }
        let white = analyze(.white), brown = analyze(.brown), cabin = analyze(.cabin)
        XCTAssertLessThan(brown.motion / brown.energy, white.motion / white.energy * 0.1)
        XCTAssertLessThan(cabin.motion / cabin.energy, white.motion / white.energy * 0.3)
    }
    func testLowCutoffRemovesHissAtSupportedRates() {
        for rate in [44100.0, 48000.0, 96000.0] {
            func brightness(_ cutoff: Double) -> Double {
                var noise = MenuBandAirNoise()
                var energy = 0.0, motion = 0.0, last = 0.0
                for frame in 0..<Int(rate * 2) {
                    let x = Double(noise.next(color: .cabin, sampleRate: rate, cutoff: cutoff))
                    if frame >= Int(rate) { energy += x*x; motion += (x-last)*(x-last) }
                    last = x
                }
                XCTAssertGreaterThan(energy / rate, 0.001)
                return motion / energy
            }
            XCTAssertLessThan(brightness(160), brightness(6000) * 0.05)
        }
    }
    func testFilterSweepStaysSmoothAndBounded() {
        var noise = MenuBandAirNoise(), last: Float = 0
        for frame in 0..<96000 {
            let x = noise.next(color: .cabin, sampleRate: 48000, cutoff: frame < 48000 ? 6000 : 60)
            XCTAssertTrue(x.isFinite)
            XCTAssertLessThanOrEqual(abs(x), 1)
            if (48000..<48100).contains(frame) { XCTAssertLessThan(abs(x-last), 0.1) }
            last = x
        }
        XCTAssertEqual(MenuBandAirNoise.cutoff(at: 0), 60, accuracy: 0.001)
        XCTAssertEqual(MenuBandAirNoise.cutoff(at: 1), 6000, accuracy: 0.001)
        XCTAssertEqual(MenuBandAirNoise.cutoff(at: MenuBandAirNoise.position(for: 160)), 160, accuracy: 0.001)
    }

}
