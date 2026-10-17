import XCTest
@testable import MenuBand

final class InterfaceRecoveryPolicyTests: XCTestCase {
    func testStartupWaitsAndRepeatedEnumerationDoesNotResetAgain() {
        var policy = InterfaceRecoveryPolicy()
        policy.observeRoute(device: 87, affected: true, at: 0)
        XCTAssertNil(policy.takeRequest(device: 87, resetting: false, at: 2))
        XCTAssertEqual(policy.takeRequest(device: 87, resetting: false, at: 3), 87)
        for time in 4...20 {
            policy.observeRoute(device: 87, affected: true, at: Double(time))
            XCTAssertNil(policy.takeRequest(device: 87, resetting: false, at: Double(time)))
        }
    }

    func testReplugResolvesNewIDAndNeverResetsSpeakerFallback() {
        var policy = InterfaceRecoveryPolicy()
        policy.observeRoute(device: 87, affected: true, at: 0)
        policy.observeRoute(device: 73, affected: false, at: 1)
        XCTAssertNil(policy.takeRequest(device: 73, resetting: false, at: 5))
        policy.observeRoute(device: 145, affected: true, at: 6)
        XCTAssertNil(policy.takeRequest(device: 87, resetting: false, at: 9))
        XCTAssertEqual(policy.takeRequest(device: 145, resetting: false, at: 9), 145)
    }

    func testManualResetSatisfiesPendingAutomaticRecovery() {
        var policy = InterfaceRecoveryPolicy()
        policy.observeRoute(device: 87, affected: true, at: 0)
        XCTAssertNil(policy.takeRequest(device: 87, resetting: true, at: 4))
        policy.didReset(at: 4)
        XCTAssertNil(policy.takeRequest(device: 87, resetting: false, at: 7))
    }

    func testRecoveryBurstCoalescesAndRepeatedFailureIsBounded() {
        var policy = InterfaceRecoveryPolicy()
        for time in 0...4 { policy.engineRecovered(device: 87, at: Double(time)) }
        XCTAssertNil(policy.takeRequest(device: 87, resetting: false, at: 6))
        XCTAssertEqual(policy.takeRequest(device: 87, resetting: false, at: 7), 87)
        for time in [10.0, 20] {
            policy.engineRecovered(device: 87, at: time)
            XCTAssertEqual(policy.takeRequest(device: 87, resetting: false, at: time + 3), 87)
        }
        policy.engineRecovered(device: 87, at: 30)
        XCTAssertNil(policy.takeRequest(device: 87, resetting: false, at: 33))
        XCTAssertNil(policy.takeRequest(device: 87, resetting: false, at: 100))
        policy.engineRecovered(device: 87, at: 101)
        XCTAssertEqual(policy.takeRequest(device: 87, resetting: false, at: 104), 87)
    }
}
