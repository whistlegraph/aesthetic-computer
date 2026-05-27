import XCTest
@testable import MenuBand

final class FloatFiniteMagnitudeMultiplyTests: XCTestCase {
    func testFiniteProductsReturnProduct() {
        XCTAssertEqual(Float(3).finiteMagnitudeMultiply(2), 6)
        XCTAssertEqual(Float(-3).finiteMagnitudeMultiply(2), -6)
        XCTAssertEqual(Float(3).finiteMagnitudeMultiply(-2), -6)
        XCTAssertEqual(Float(-3).finiteMagnitudeMultiply(-2), 6)
        XCTAssertEqual(Float(0.5).finiteMagnitudeMultiply(0.25), 0.125)
    }

    func testProductsWithZeroReturnSignedZero() {
        let positiveResult = Float.greatestFiniteMagnitude.finiteMagnitudeMultiply(0)
        XCTAssertEqual(positiveResult, 0)
        XCTAssertEqual(positiveResult.sign, .plus)
        XCTAssertTrue(positiveResult.isFinite)

        let negativeResult = Float.greatestFiniteMagnitude.finiteMagnitudeMultiply(-Float.zero)
        XCTAssertEqual(negativeResult, 0)
        XCTAssertEqual(negativeResult.sign, .minus)
        XCTAssertTrue(negativeResult.isFinite)

        let negativeLeftResult = (-Float.greatestFiniteMagnitude).finiteMagnitudeMultiply(0)
        XCTAssertEqual(negativeLeftResult, 0)
        XCTAssertEqual(negativeLeftResult.sign, .minus)
        XCTAssertTrue(negativeLeftResult.isFinite)
    }

    func testSubnormalProductsReturnProduct() {
        let result = Float.leastNonzeroMagnitude.finiteMagnitudeMultiply(0.5)

        XCTAssertEqual(result, 0)
        XCTAssertTrue(result.isFinite)
    }

    func testPositiveOverflowCapsToGreatestFiniteMagnitude() {
        XCTAssertEqual(Float.greatestFiniteMagnitude.finiteMagnitudeMultiply(2), .greatestFiniteMagnitude)
        XCTAssertEqual((-Float.greatestFiniteMagnitude).finiteMagnitudeMultiply(-2), .greatestFiniteMagnitude)
    }

    func testNegativeOverflowCapsToLowestFiniteMagnitude() {
        XCTAssertEqual(Float.greatestFiniteMagnitude.finiteMagnitudeMultiply(-2), -.greatestFiniteMagnitude)
        XCTAssertEqual((-Float.greatestFiniteMagnitude).finiteMagnitudeMultiply(2), -.greatestFiniteMagnitude)
    }

    func testPositiveInfinityProductsCapToGreatestFiniteMagnitude() {
        XCTAssertEqual(Float(0.1).finiteMagnitudeMultiply(.infinity), .greatestFiniteMagnitude)
        XCTAssertEqual(Float(-0.1).finiteMagnitudeMultiply(-.infinity), .greatestFiniteMagnitude)
        XCTAssertEqual(Float.infinity.finiteMagnitudeMultiply(0.1), .greatestFiniteMagnitude)
        XCTAssertEqual((-Float.infinity).finiteMagnitudeMultiply(-0.1), .greatestFiniteMagnitude)
        XCTAssertEqual(Float.infinity.finiteMagnitudeMultiply(.infinity), .greatestFiniteMagnitude)
        XCTAssertEqual((-Float.infinity).finiteMagnitudeMultiply(-.infinity), .greatestFiniteMagnitude)
    }

    func testNegativeInfinityProductsCapToLowestFiniteMagnitude() {
        XCTAssertEqual(Float(-0.1).finiteMagnitudeMultiply(.infinity), -.greatestFiniteMagnitude)
        XCTAssertEqual(Float(0.1).finiteMagnitudeMultiply(-.infinity), -.greatestFiniteMagnitude)
        XCTAssertEqual(Float.infinity.finiteMagnitudeMultiply(-0.1), -.greatestFiniteMagnitude)
        XCTAssertEqual((-Float.infinity).finiteMagnitudeMultiply(0.1), -.greatestFiniteMagnitude)
        XCTAssertEqual(Float.infinity.finiteMagnitudeMultiply(-.infinity), -.greatestFiniteMagnitude)
        XCTAssertEqual((-Float.infinity).finiteMagnitudeMultiply(.infinity), -.greatestFiniteMagnitude)
    }

    func testZeroMultipliedByInfinityReturnsNaN() {
        XCTAssertTrue(Float(0).finiteMagnitudeMultiply(.infinity).isNaN)
        XCTAssertTrue(Float(-0).finiteMagnitudeMultiply(.infinity).isNaN)
        XCTAssertTrue(Float(0).finiteMagnitudeMultiply(-.infinity).isNaN)
        XCTAssertTrue(Float.infinity.finiteMagnitudeMultiply(0).isNaN)
        XCTAssertTrue((-Float.infinity).finiteMagnitudeMultiply(0).isNaN)
    }

    func testNaNProductsReturnNaN() {
        XCTAssertTrue(Float.nan.finiteMagnitudeMultiply(2).isNaN)
        XCTAssertTrue(Float(2).finiteMagnitudeMultiply(.nan).isNaN)
        XCTAssertTrue(Float.nan.finiteMagnitudeMultiply(.nan).isNaN)
        XCTAssertTrue(Float.nan.finiteMagnitudeMultiply(.infinity).isNaN)
    }
}
