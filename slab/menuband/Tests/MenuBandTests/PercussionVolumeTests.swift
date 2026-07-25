import XCTest
@testable import MenuBand

final class PercussionVolumeTests: XCTestCase {
    func testTapMovesTenPercentagePoints() {
        XCTAssertEqual(
            MenuBandController.steppedPercussionVolume(
                0.75, direction: 1, isRepeat: false),
            0.85,
            accuracy: 0.0001)
        XCTAssertEqual(
            MenuBandController.steppedPercussionVolume(
                0.75, direction: -1, isRepeat: false),
            0.65,
            accuracy: 0.0001)
    }

    func testKeyRepeatMovesOnePercentagePoint() {
        XCTAssertEqual(
            MenuBandController.steppedPercussionVolume(
                0.75, direction: 1, isRepeat: true),
            0.76,
            accuracy: 0.0001)
        XCTAssertEqual(
            MenuBandController.steppedPercussionVolume(
                0.75, direction: -1, isRepeat: true),
            0.74,
            accuracy: 0.0001)
    }

    func testSteppingClampsAtZeroAndOne() {
        XCTAssertEqual(
            MenuBandController.steppedPercussionVolume(
                0.99, direction: 1, isRepeat: false),
            1.0)
        XCTAssertEqual(
            MenuBandController.steppedPercussionVolume(
                0.01, direction: -1, isRepeat: false),
            0.0)
    }
}
