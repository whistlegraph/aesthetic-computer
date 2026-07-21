import AppKit
import XCTest
@testable import MenuBand

#if !MAC_APP_STORE
final class ShapedownTests: XCTestCase {
    func testFullestShapeSurvivesStaggeredFourFingerLift() {
        var memory = ShapedownGestureMemory()
        let quad = points(4)
        XCTAssertEqual(memory.update(quad), quad)
        XCTAssertEqual(memory.update(points(3)).count, 4)
        XCTAssertEqual(memory.update(points(2)).count, 4)
        XCTAssertEqual(memory.update(points(1)).count, 4)
        XCTAssertEqual(memory.release().count, 4)
        XCTAssertTrue(memory.peak.isEmpty)
    }

    func testOneFingerNeverBecomesMultiPointShape() {
        var memory = ShapedownGestureMemory()
        XCTAssertEqual(memory.update(points(1)).count, 1)
        XCTAssertEqual(memory.release().count, 1)
    }

    func testNearlyCoincidentTouchesHaveNoSoftwareDeadZone() {
        var memory = ShapedownGestureMemory()
        let close = [CGPoint(x: 100, y: 100), CGPoint(x: 100.01, y: 100.01)]
        XCTAssertEqual(memory.update(close).count, 2)
        XCTAssertEqual(memory.release(), close)
    }

    func testSinglePointFilterRejectsOneFrameTeleport() {
        var filter = ShapedownSinglePointFilter()
        let size = CGSize(width: 1_000, height: 1_000)
        XCTAssertEqual(filter.update(raw: CGPoint(x: 100, y: 100), canvasSize: size),
                       CGPoint(x: 100, y: 100))
        XCTAssertEqual(filter.update(raw: CGPoint(x: 900, y: 900), canvasSize: size),
                       CGPoint(x: 100, y: 100),
                       "an isolated raw jump must not throw the display")
    }

    func testSinglePointFilterAcceptsConfirmedFastMove() {
        var filter = ShapedownSinglePointFilter()
        let size = CGSize(width: 1_000, height: 1_000)
        _ = filter.update(raw: CGPoint(x: 100, y: 100), canvasSize: size)
        _ = filter.update(raw: CGPoint(x: 700, y: 700), canvasSize: size)
        let confirmed = filter.update(raw: CGPoint(x: 710, y: 710), canvasSize: size)
        XCTAssertGreaterThan(confirmed.x, 100)
        XCTAssertGreaterThan(confirmed.y, 100)
    }

    func testNotepatCKeySelectsBaseRed() throws {
        let color = try XCTUnwrap(ShapedownPalette.color(forKeyCode: 8)) // C
        assertRGB(color, 255, 50, 50)
    }

    func testNotepatNaturalsFollowSharedPalette() throws {
        assertRGB(try XCTUnwrap(ShapedownPalette.color(forKeyCode: 2)), 255, 160, 0) // D
        assertRGB(try XCTUnwrap(ShapedownPalette.color(forKeyCode: 3)), 50, 200, 50)  // F
        assertRGB(try XCTUnwrap(ShapedownPalette.color(forKeyCode: 5)), 50, 120, 255) // G
    }

    func testAccidentalKeyStaysBlack() throws {
        assertRGB(try XCTUnwrap(ShapedownPalette.color(forKeyCode: 9)), 0, 0, 0) // V = C#
    }

    private func points(_ count: Int) -> [CGPoint] {
        (0..<count).map { CGPoint(x: CGFloat($0 * 20), y: CGFloat($0 * 10)) }
    }

    private func assertRGB(_ color: NSColor, _ r: Int, _ g: Int, _ b: Int,
                           file: StaticString = #filePath, line: UInt = #line) {
        let rgb = color.usingColorSpace(.deviceRGB)
        XCTAssertEqual(rgb?.redComponent ?? -1, CGFloat(r) / 255, accuracy: 0.001,
                       file: file, line: line)
        XCTAssertEqual(rgb?.greenComponent ?? -1, CGFloat(g) / 255, accuracy: 0.001,
                       file: file, line: line)
        XCTAssertEqual(rgb?.blueComponent ?? -1, CGFloat(b) / 255, accuracy: 0.001,
                       file: file, line: line)
    }
}
#endif
