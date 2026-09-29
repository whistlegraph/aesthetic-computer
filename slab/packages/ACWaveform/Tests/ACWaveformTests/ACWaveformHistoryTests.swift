import XCTest
@testable import ACWaveform

final class ACWaveformHistoryTests: XCTestCase {
    func testSpanIsTwoBars() {
        XCTAssertEqual(ACWaveformHistory.span(bpm: 120), 4)
        XCTAssertEqual(ACWaveformHistory.span(bpm: 60), 8)
        XCTAssertEqual(ACWaveformHistory.span(bpm: .nan), 4)
    }

    func testSilenceDrawsNothing() {
        var history = ACWaveformHistory()
        history.appendSilence(at: 0)
        XCTAssertFalse(history.isSounding)
        XCTAssertNil(history.path(in: CGSize(width: 16, height: 100), now: 0, span: 4))
    }

    func testNewestSitsAtTheBottomAndOldestScrollsUp() throws {
        var history = ACWaveformHistory()
        history.append(low: -1, high: 1, at: 0)
        history.append(low: -0.5, high: 0.5, at: 2)
        let path = try XCTUnwrap(history.path(in: CGSize(width: 16, height: 100), now: 2, span: 4))
        var points: [CGPoint] = []
        path.applyWithBlock { element in
            if element.pointee.type != .closeSubpath { points.append(element.pointee.points[0]) }
        }
        // start, two low edges, the bottom centre, two high edges
        XCTAssertEqual(points.count, 6)
        XCTAssertEqual(points[1], CGPoint(x: 8 - 6.5, y: 50))
        XCTAssertEqual(points[2], CGPoint(x: 8 - 3.25, y: 100))
        XCTAssertEqual(points[3], CGPoint(x: 8, y: 100))
        XCTAssertEqual(points[5], CGPoint(x: 8 + 6.5, y: 50))
    }

    func testPruneDropsWhatScrolledOff() {
        var history = ACWaveformHistory()
        history.append(low: -1, high: 1, at: 0)
        history.append(low: -1, high: 1, at: 3)
        history.prune(now: 5, span: 4)
        XCTAssertEqual(history.slices.map(\.at), [3])
        history.prune(now: 10, span: 4)
        XCTAssertTrue(history.slices.isEmpty)
    }

    func testValuesAreClamped() {
        var history = ACWaveformHistory()
        history.append(low: -9, high: .infinity, at: 0)
        XCTAssertEqual(history.slices.first?.low, -1)
        XCTAssertEqual(history.slices.first?.high, 0)
    }
}
