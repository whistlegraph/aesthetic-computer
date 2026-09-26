import XCTest
@testable import MenuBand

final class MonitorReadCursorTests: XCTestCase {
    func testUnderrunConsumesOnlyCapturedFramesThenWaitsForFreshCushion() {
        var cursor = MonitorReadCursor()
        let short = cursor.plan(read: 100, written: 108, frames: 16, lead: 32)
        XCTAssertEqual(short.start, 100)
        XCTAssertEqual(short.count, 8)
        XCTAssertTrue(short.underrun)
        let waiting = cursor.plan(read: 108, written: 140, frames: 16, lead: 32)
        XCTAssertEqual(waiting.count, 0)
        let recovered = cursor.plan(read: 108, written: 172, frames: 16, lead: 32)
        XCTAssertEqual(recovered.start, 124)
        XCTAssertEqual(recovered.count, 16)
        XCTAssertFalse(cursor.recovering)
        XCTAssertEqual(172 - recovered.start - recovered.count, 32)
    }

    func testHoursOfUptimeKeepExactSampleAddresses() {
        var cursor = MonitorReadCursor()
        for read in [1 << 24, (1 << 24) + 1, 1 << 30, (1 << 30) + 17] {
            let plan = cursor.plan(read: read, written: read + 32, frames: 16, lead: 32)
            XCTAssertEqual(plan.start, read)
            XCTAssertEqual(plan.count, 16)
        }
    }

    func testLargeCallbackStillGetsAFullBlockWhenDroppingBacklog() {
        var cursor = MonitorReadCursor()
        let plan = cursor.plan(read: 0, written: 4096, frames: 512, lead: 32)
        XCTAssertTrue(plan.dropped)
        XCTAssertEqual(plan.count, 512)
        XCTAssertEqual(plan.start + plan.count, 4096)
    }

    func testJitterNeverReadsUnwrittenFramesOrMovesBackwards() {
        var cursor = MonitorReadCursor()
        var written = 1 << 30, read = written - 32
        for tick in 0..<100_000 {
            // Capture stalls, resumes in bursts, and changes callback size.
            let frames = [16, 32, 64, 128][(tick / 1000) % 4]
            written += tick % 17 < 3 ? 0 : (tick % 17 == 3 ? frames * 4 : frames)
            let plan = cursor.plan(read: read, written: written, frames: frames, lead: max(32, frames))
            XCTAssertGreaterThanOrEqual(plan.start, read)
            XCTAssertLessThanOrEqual(plan.start + plan.count, written)
            XCTAssertLessThanOrEqual(plan.count, frames)
            read = plan.start + plan.count
        }
    }
}
