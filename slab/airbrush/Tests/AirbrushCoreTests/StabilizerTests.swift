import XCTest
@testable import AirbrushCore

final class StabilizerTests: XCTestCase {
    func testStringRejectsJitterAndStopsWithoutTailSnap() {
        var s = StrokeStabilizer(point: Point(), time: 0, mode: .string12)
        for i in 1...120 {
            let a = Double(i) * 0.7
            XCTAssertEqual(s.update(Point(cos(a) * 4, sin(a) * 4), at: Double(i) / 120), Point())
        }
        XCTAssertEqual(s.update(Point(100, 0), at: 2), Point(88, 0))
        XCTAssertEqual(s.update(Point(88, 10), at: 3), Point(88, 0))
        XCTAssertEqual(s.update(Point(88, 40), at: 4), Point(88, 28))
        XCTAssertTrue(s.finish().isEmpty)
        XCTAssertEqual(s.position, Point(88, 28))
    }
    func testAdaptiveReducesSlowJitterAndReleasesFastMotion() {
        var s = StrokeStabilizer(point: Point(), time: 0, mode: .balanced)
        var inputEnergy = 0.0, outputEnergy = 0.0
        for i in 1...480 {
            let t = Double(i) / 240, raw = Point(t * 40, sin(t * 2 * .pi * 35) * 2)
            let output = s.update(raw, at: t)
            if i > 120 { inputEnergy += raw.y * raw.y; outputEnergy += output.y * output.y }
        }
        XCTAssertLessThan(outputEnergy, inputEnergy * 0.15)
        for i in 1...240 { _ = s.update(Point(80 + Double(i) * 5, 0), at: 2 + Double(i) / 240) }
        XCTAssertLessThan(1280 - s.position.x, 12, "Fast movement has excessive lag")
        let tail = s.finish()
        XCTAssertLessThanOrEqual(tail.count, 49)
        XCTAssertEqual(s.position, Point(1280, 0))
        XCTAssertTrue(tail.allSatisfy { $0.x <= 1280 })
    }
    func testFilterUsesElapsedTimeAndContactsStayIndependent() {
        func path(_ rate: Int) -> Point {
            var s = StrokeStabilizer(point: Point(), time: 0, mode: .balanced)
            for i in 1...rate { _ = s.update(Point(Double(i) / Double(rate) * 600, 0), at: Double(i) / Double(rate)) }
            return s.position
        }
        XCTAssertEqual(path(120).x, path(1000).x, accuracy: 1)
        var a = StrokeStabilizer(point: Point(), time: 0, mode: .string12)
        let b = StrokeStabilizer(point: Point(500, 0), time: 0, mode: .string12)
        _ = a.update(Point(100, 0), at: 1)
        XCTAssertEqual(b.position, Point(500, 0))
        var off = StrokeStabilizer(point: Point(), time: 0, mode: .off)
        XCTAssertEqual(off.update(Point(7, 13), at: 0.1), Point(7, 13))
        XCTAssertTrue(off.finish().isEmpty)
    }
}
