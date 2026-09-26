import XCTest
@testable import MenuBand

final class TrackpadSplitSurfaceTests: XCTestCase {
    private func contact(_ id: Int32, _ x: CGFloat, _ y: CGFloat = 0.5,
                         state: Int32 = 4) -> TrackpadContact {
        TrackpadContact(identifier: id, point: CGPoint(x: x, y: y), state: state)
    }

    func testPitchHalfStretchesToFullSliderTravel() {
        XCTAssertTrue(TrackpadSplitSurface.isPitchSide(CGPoint(x: 0.49, y: 0.5)))
        XCTAssertFalse(TrackpadSplitSurface.isPitchSide(CGPoint(x: 0.5, y: 0.5)))
        let p = TrackpadSplitSurface.pitchPoint(CGPoint(x: 0.25, y: 0.9))
        XCTAssertEqual(p.x, 0.5, accuracy: 1e-9)
        XCTAssertEqual(p.y, 0.9, accuracy: 1e-9)
    }

    func testFingersAreOwnedByTheHalfTheyLandedOn() {
        var ownership = TrackpadSplitSurface.Ownership()
        let first = ownership.resolve(previous: [:], active: [
            contact(1, 0.2, state: 3), contact(2, 0.8, state: 3),
        ])
        XCTAssertEqual(first.pitch.map(\.identifier), [1])
        XCTAssertEqual(first.pitch[0].point.x, 0.4, accuracy: 1e-9)
        XCTAssertEqual(first.drumTouches, [CGPoint(x: 0.8, y: 0.5)])
        XCTAssertEqual(first.drumBegan, [CGPoint(x: 0.8, y: 0.5)])
        XCTAssertEqual(first.drumLifted, [])

        // Both fingers cross the seam. Neither changes job.
        let crossed = ownership.resolve(
            previous: [1: CGPoint(x: 0.2, y: 0.5), 2: CGPoint(x: 0.8, y: 0.5)],
            active: [contact(1, 0.7), contact(2, 0.3)]
        )
        XCTAssertEqual(crossed.pitch.map(\.identifier), [1])
        XCTAssertEqual(crossed.pitch[0].point.x, 1.4, accuracy: 1e-9)
        XCTAssertEqual(crossed.drumTouches, [CGPoint(x: 0.3, y: 0.5)])
        XCTAssertEqual(crossed.drumBegan, [])
    }

    func testDrumFingerLiftsWhereItLastWasAndIsForgotten() {
        var ownership = TrackpadSplitSurface.Ownership()
        _ = ownership.resolve(previous: [:], active: [contact(2, 0.8, state: 3)])
        let lifted = ownership.resolve(
            previous: [2: CGPoint(x: 0.3, y: 0.6)], active: []
        )
        XCTAssertEqual(lifted.drumLifted, [CGPoint(x: 0.3, y: 0.6)])
        XCTAssertFalse(ownership.drumIsHeld)
        // The same id landing on the left afterwards is a slider finger.
        let again = ownership.resolve(previous: [:], active: [contact(2, 0.1, state: 3)])
        XCTAssertEqual(again.pitch.map(\.identifier), [2])
        XCTAssertEqual(again.drumTouches, [])
    }

    func testHeldDrumTouchesReadFromTheLiveContactMap() {
        var ownership = TrackpadSplitSurface.Ownership()
        _ = ownership.resolve(previous: [:], active: [contact(1, 0.2), contact(2, 0.9)])
        let held = ownership.drumTouches(in: [
            1: CGPoint(x: 0.25, y: 0.5), 2: CGPoint(x: 0.85, y: 0.4),
        ])
        XCTAssertEqual(held, [CGPoint(x: 0.85, y: 0.4)])
    }
}
