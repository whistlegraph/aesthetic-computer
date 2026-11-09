import XCTest
import AppKit
@testable import MenuBand

final class UIResponsivenessTests: XCTestCase {
    func testBlockedVisibilityQueryDoesNotBlockMainAndKeepsOnlyLatestWidth() {
        let entered = DispatchSemaphore(value: 0)
        let release = DispatchSemaphore(value: 0)
        let queried = PerformanceSnapshot<[CGFloat]>([])
        let probe = MenuBarVisibilityProbe { width, _ in
            XCTAssertFalse(Thread.isMainThread)
            queried.update { $0.append(width) }
            if width == 1 {
                entered.signal()
                _ = release.wait(timeout: .now() + 3)
            }
            return width == 500
        }
        probe.sample(width: 1, preferredName: "menuband") { _ in XCTFail("obsolete result delivered") }
        XCTAssertEqual(entered.wait(timeout: .now() + 1), .success)
        for width in 2..<500 {
            probe.sample(width: CGFloat(width), preferredName: "menuband") { _ in
                XCTFail("intermediate layout delivered")
            }
        }
        let done = expectation(description: "latest layout")
        probe.sample(width: 500, preferredName: "menuband") { visible in
            XCTAssertTrue(Thread.isMainThread)
            XCTAssertTrue(visible)
            done.fulfill()
        }
        // Only a responsive main queue can release the blocked worker.
        DispatchQueue.main.async { release.signal() }
        wait(for: [done], timeout: 2)
        XCTAssertEqual(queried.read(), [1, 500])
        withExtendedLifetime(probe) {}
    }

    func testRepeatedVisibilityRequestUsesOneInFlightQuery() {
        let release = DispatchSemaphore(value: 0)
        let count = PerformanceSnapshot(0)
        let probe = MenuBarVisibilityProbe { _, _ in
            count.update { $0 += 1 }
            _ = release.wait(timeout: .now() + 3)
            return false
        }
        probe.sample(width: 240, preferredName: "menuband") { _ in XCTFail("superseded callback") }
        let done = expectation(description: "current callback")
        probe.sample(width: 240, preferredName: "menuband") { visible in
            XCTAssertFalse(visible)
            done.fulfill()
        }
        release.signal()
        wait(for: [done], timeout: 2)
        XCTAssertEqual(count.read(), 1)
        withExtendedLifetime(probe) {}
    }
}
