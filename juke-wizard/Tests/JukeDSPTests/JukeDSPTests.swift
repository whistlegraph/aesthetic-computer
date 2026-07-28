import XCTest
import JukeDSP

final class JukeDSPTests: XCTestCase {
    func testCubicInterpolationPreservesLinearMotion() {
        XCTAssertEqual(ac_scratch_cubic(-1, 0, 1, 2, 0.5), 0.5, accuracy: 0.000_01)
    }

    func testScratchMotionChangesDirectionWithoutJumping() {
        var state = ACScratchState()
        ac_scratch_init(&state)
        var motion = 1.0
        for _ in 0..<64 {
            motion = ac_scratch_motion(&state, -2, -2_000, 1, 48_000)
            XCTAssertTrue(motion.isFinite)
        }
        XCTAssertLessThan(motion, 0)
        XCTAssertGreaterThan(motion, -8)
    }

    func testPracticeLoopsAreFiniteAndAudible() {
        let frames = 48_000
        for variant in 0...1 {
            var left = [Float](repeating: 0, count: frames)
            var right = [Float](repeating: 0, count: frames)
            left.withUnsafeMutableBufferPointer { l in
                right.withUnsafeMutableBufferPointer { r in
                    ac_practice_render(Int32(variant), l.baseAddress, r.baseAddress,
                                       frames, 48_000, 120)
                }
            }
            XCTAssertTrue(left.allSatisfy(\.isFinite))
            XCTAssertTrue(right.allSatisfy(\.isFinite))
            XCTAssertGreaterThan(left.map { abs($0) }.max() ?? 0, 0.1)
            XCTAssertGreaterThan(right.map { abs($0) }.max() ?? 0, 0.1)
            XCTAssertLessThanOrEqual(left.map { abs($0) }.max() ?? 2, 1.001)
            XCTAssertLessThanOrEqual(right.map { abs($0) }.max() ?? 2, 1.001)
        }
    }
}
