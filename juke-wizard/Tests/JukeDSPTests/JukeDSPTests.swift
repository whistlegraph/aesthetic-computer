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

    func testScratchVelocityHasNoArtificialForwardOrReverseBoundary() {
        for velocity in [-24.0, 24.0] {
            var state = ACScratchState()
            ac_scratch_init(&state)
            var motion = state.velocity
            for _ in 0..<4_000 {
                motion = ac_scratch_motion(&state, velocity, 0, 1, 48_000)
                XCTAssertTrue(motion.isFinite)
            }
            XCTAssertEqual(motion, velocity, accuracy: 0.001)
        }
    }

    func testScratchReleasePreservesVelocityBeforeFriction() {
        var state = ACScratchState()
        ac_scratch_init(&state)
        for _ in 0..<2_000 {
            _ = ac_scratch_motion(&state, 11.5, 0, 1, 48_000)
        }
        let heldVelocity = state.velocity
        let firstReleased = ac_scratch_motion(&state, heldVelocity, 0, 0, 48_000)
        XCTAssertEqual(firstReleased, heldVelocity, accuracy: 0.000_001)
    }

    func testScratchFollowerLandsWithoutOvershooting() {
        var state = ACScratchState()
        ac_scratch_init(&state)
        var error = 2_400.0
        var previousError = error
        for _ in 0..<8_000 {
            let motion = ac_scratch_motion(&state, 0, error, 1, 48_000)
            error -= motion
            XCTAssertGreaterThanOrEqual(error, -0.000_001)
            XCTAssertLessThanOrEqual(error, previousError + 0.000_001)
            previousError = error
        }
        XCTAssertLessThan(error, 0.5) // sub-sample after 167 ms, without reversal
    }

    func testSpatialPlatterContactsUseRadiusAndCombineFingers() {
        var rim = ACPlatterContact(previous_x: 1, previous_y: 0,
                                   current_x: 0, current_y: 1)
        var center = ACPlatterContact(previous_x: 0.04, previous_y: 0,
                                      current_x: 0, current_y: 0.04)
        let rimMotion = ac_platter_contact_motion(&rim, 1, 1.8)
        let centerMotion = ac_platter_contact_motion(&center, 1, 1.8)
        XCTAssertEqual(rimMotion, -0.279, accuracy: 0.000_01)
        XCTAssertEqual(centerMotion, 0, accuracy: 0.000_01)

        let contacts = [rim, rim]
        let twoFingerMotion = contacts.withUnsafeBufferPointer {
            ac_platter_contact_motion($0.baseAddress, $0.count, 1.8)
        }
        XCTAssertGreaterThan(abs(twoFingerMotion), abs(rimMotion))
        XCTAssertLessThan(abs(twoFingerMotion), abs(rimMotion) * 2)
    }

    func testPracticeLoopsAreFiniteAndAudible() {
        let frames = 48_000
        for variant in 0...3 {
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
