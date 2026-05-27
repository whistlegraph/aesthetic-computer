import XCTest
@testable import MenuBand

final class FloatSafeClampedTests: XCTestCase {
    func testValuesInsideBoundsReturnOriginalValue() {
        XCTAssertEqual(Float(0.5).safeClamped(0, 1), 0.5)
        XCTAssertEqual(Float(-2).safeClamped(-5, 5), -2)
    }

    func testValuesBelowMinimumClampToMinimum() {
        XCTAssertEqual(Float(-0.1).safeClamped(0, 1), 0)
        XCTAssertEqual(Float(-10).safeClamped(-5, 5), -5)
    }

    func testValuesAboveMaximumClampToMaximum() {
        XCTAssertEqual(Float(1.1).safeClamped(0, 1), 1)
        XCTAssertEqual(Float(10).safeClamped(-5, 5), 5)
    }

    func testBoundaryValuesReturnOriginalValue() {
        XCTAssertEqual(Float(0).safeClamped(0, 1), 0)
        XCTAssertEqual(Float(1).safeClamped(0, 1), 1)
    }

    func testInfiniteValuesClampToFiniteBounds() {
        XCTAssertEqual(Float.infinity.safeClamped(0, 1), 1)
        XCTAssertEqual((-Float.infinity).safeClamped(0, 1), 0)
    }

    func testNaNReturnsNaN() {
        XCTAssertTrue(Float.nan.safeClamped(0, 1).isNaN)
    }

    func testInvertedBoundsReturnMaximumArgument() {
        XCTAssertEqual(Float(-5).safeClamped(10, 0), 0)
        XCTAssertEqual(Float(5).safeClamped(10, 0), 0)
        XCTAssertEqual(Float(15).safeClamped(10, 0), 0)
    }
}
