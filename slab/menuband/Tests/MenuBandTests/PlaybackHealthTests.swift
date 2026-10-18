import XCTest
@testable import MenuBand

final class PlaybackHealthTests: XCTestCase {
    func testUSBResetSuppressesCompetingRecoveryUntilStreamsSettle() {
        let health = MenuBandPlaybackHealth()
        health.begin(at: 0)
        health.setInterfaceResetting(true, at: 2)
        health.observe(invalid: 832, at: 3)
        XCTAssertNil(health.recoveryReason(at: 10, shouldRun: true, isRunning: false))
        health.setInterfaceResetting(false, at: 11)
        health.observe(invalid: 0, at: 12)
        XCTAssertNil(health.recoveryReason(at: 13, shouldRun: true, isRunning: true))
        // A real fault after the settling interval still gets repaired.
        health.observe(invalid: 64, at: 14)
        XCTAssertTrue(health.recoveryReason(at: 14, shouldRun: true, isRunning: true)!
            .contains("64 non-finite"))
    }

    func testRunningFlagCannotHideStalledOutput() {
        let health = MenuBandPlaybackHealth()
        health.begin(at: 0)
        health.observe(invalid: 0, at: 1)
        XCTAssertNil(health.recoveryReason(at: 2, shouldRun: true, isRunning: true))
        XCTAssertTrue(health.recoveryReason(at: 4, shouldRun: true, isRunning: true)!
            .contains("callbacks stopped"))
    }

    func testIdleIsAllowedButUnexpectedStopRecovers() {
        let health = MenuBandPlaybackHealth()
        health.begin(at: 0)
        XCTAssertNil(health.recoveryReason(at: 60, shouldRun: false, isRunning: false))
        XCTAssertTrue(health.recoveryReason(at: 61, shouldRun: true, isRunning: false)!
            .contains("engine stopped"))
    }

    func testInvalidSamplesTriggerRecoveryEvenIfCallbacksContinue() {
        let health = MenuBandPlaybackHealth()
        health.begin(at: 0)
        health.observe(invalid: 32, at: 2)
        health.observe(invalid: 0, at: 3)
        XCTAssertTrue(health.recoveryReason(at: 3, shouldRun: true, isRunning: true)!
            .contains("32 non-finite"))
        XCTAssertNil(health.recoveryReason(at: 4, shouldRun: true, isRunning: true))
    }

    func testFailedRecoveryBacksOffUntilWakeOrDeviceChange() {
        let health = MenuBandPlaybackHealth()
        health.begin(at: 0)
        for time in [3.0, 8, 13] {
            XCTAssertNotNil(health.recoveryReason(at: time, shouldRun: true, isRunning: false))
        }
        XCTAssertNil(health.recoveryReason(at: 100, shouldRun: true, isRunning: false))
        health.begin(at: 100, renewBudget: true)
        XCTAssertNil(health.recoveryReason(at: 101, shouldRun: true, isRunning: false))
        XCTAssertNotNil(health.recoveryReason(at: 103, shouldRun: true, isRunning: false))
    }

    func testSustainedHealthyRenderingRenewsRecoveryBudget() {
        let health = MenuBandPlaybackHealth()
        health.begin(at: 0)
        for time in [3.0, 8, 13] {
            XCTAssertNotNil(health.recoveryReason(at: time, shouldRun: true, isRunning: false))
        }
        for time in 14...25 { health.observe(invalid: 0, at: Double(time)) }
        health.observe(invalid: 1, at: 26)
        XCTAssertNotNil(health.recoveryReason(at: 26, shouldRun: true, isRunning: true))
    }
}
