import AppKit
import XCTest
@testable import MenuBand

/// The stage's lanes must be the trackpad's tap bands, or a burst would
/// light a different circle than the one the tap was scored against.
final class PolyrhythmStageTests: XCTestCase {
    func testLanesTileTheScreenAndMatchTapBands() {
        let bounds = NSRect(x: 0, y: 0, width: 1512, height: 982)
        for count in 1...5 {
            var covered: CGFloat = 0
            for index in 0..<count {
                let lane = PolyrhythmStageLayout.laneRect(index: index, count: count, in: bounds)
                XCTAssertEqual(lane.minX, covered, accuracy: 0.001)
                covered = lane.maxX
                // A tap anywhere in this lane's horizontal span scores on this rhythm.
                for fraction in [0.01, 0.5, 0.99] {
                    let x = Double((lane.minX + lane.width * CGFloat(fraction)) / bounds.width)
                    XCTAssertEqual(
                        PolyrhythmTrainerClock.rhythmIndex(forNormalizedX: x, rhythmCount: count),
                        index, "count \(count) lane \(index) fraction \(fraction)")
                }
            }
            XCTAssertEqual(covered, bounds.maxX, accuracy: 0.001)
        }
    }

    func testClockFitsInsideItsLane() {
        let bounds = NSRect(x: 0, y: 0, width: 1512, height: 982)
        for count in 1...5 {
            let lane = PolyrhythmStageLayout.laneRect(index: 0, count: count, in: bounds)
            let radius = PolyrhythmStageLayout.clockRadius(lane: lane, bounds: bounds)
            let center = PolyrhythmStageLayout.clockCenter(lane: lane, bounds: bounds)
            XCTAssertLessThan(radius * 2, lane.width)
            XCTAssertLessThan(center.y + radius, bounds.maxY)
            // Room under the dial for the tempo readout.
            XCTAssertGreaterThan(center.y - radius, bounds.height * 0.12)
        }
    }

    func testTapColorFollowsAccuracyOnly() {
        let lane = NSColor.systemGreen
        func tap(_ accuracy: Double) -> PolyrhythmTapFeedback {
            PolyrhythmTapFeedback(phase: 0, rhythmIndex: 0, accuracy: accuracy, opacity: 1)
        }
        XCTAssertEqual(PolyrhythmStageView.tapColor(tap(0.9), lane: lane), lane)
        XCTAssertEqual(PolyrhythmStageView.tapColor(tap(0.5), lane: lane), .systemOrange)
        XCTAssertEqual(PolyrhythmStageView.tapColor(tap(0.1), lane: lane), .systemRed)
    }
}
