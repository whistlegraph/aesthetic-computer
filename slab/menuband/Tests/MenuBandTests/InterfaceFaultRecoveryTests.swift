import XCTest
@testable import MenuBand

final class InterfaceFaultRecoveryTests: XCTestCase {
    func testHealthyClockNeverRequestsReset() {
        var policy = InterfaceFaultRecovery()
        for t in 0...120 {
            XCTAssertNil(policy.observe(device: 85, rate: 44_100,
                sampleTime: Double(t) * 44_100, active: true, at: Double(t)))
        }
    }

    func testUSBClockRestartRequestsOneResetAndIgnoresItsOwnAftermath() {
        var policy = InterfaceFaultRecovery()
        for t in 0...5 {
            _ = policy.observe(device: 85, rate: 44_100,
                sampleTime: Double(t) * 44_100, active: true, at: Double(t))
        }
        XCTAssertEqual(policy.observe(device: 85, rate: 44_100, sampleTime: 100,
            active: true, at: 6), "hardware clock restarted unexpectedly")
        for t in 7...20 {
            XCTAssertNil(policy.observe(device: 85, rate: 44_100,
                sampleTime: t.isMultiple(of: 2) ? 100_000 : 0, active: true, at: Double(t)))
        }
    }

    func testStalledAndUnavailableClocksNeedSustainedEvidence() {
        for clock: Double? in [100, nil] {
            var policy = InterfaceFaultRecovery()
            for t in 0...4 {
                XCTAssertNil(policy.observe(device: 85, rate: 44_100,
                    sampleTime: clock, active: true, at: Double(t)))
            }
            XCTAssertEqual(policy.observe(device: 85, rate: 44_100,
                sampleTime: clock, active: true, at: 5), "hardware clock stopped advancing")
        }
    }

    func testIdleRouteRateAndSleepChangesDoNotTriggerReset() {
        var policy = InterfaceFaultRecovery()
        for t in 0...5 {
            _ = policy.observe(device: 85, rate: 44_100, sampleTime: Double(t) * 44_100,
                               active: true, at: Double(t))
        }
        XCTAssertNil(policy.observe(device: 85, rate: 44_100, sampleTime: nil, active: false, at: 6))
        XCTAssertNil(policy.observe(device: 85, rate: 44_100, sampleTime: 0, active: true, at: 7))
        XCTAssertNil(policy.observe(device: 86, rate: 44_100, sampleTime: 0, active: true, at: 8))
        XCTAssertNil(policy.observe(device: 86, rate: 48_000, sampleTime: 0, active: true, at: 9))
        XCTAssertNil(policy.observe(device: 86, rate: 48_000, sampleTime: 0, active: true, at: 99))
    }

    func testRepeatedFaultsCannotCreateAnEndlessResetLoop() {
        var policy = InterfaceFaultRecovery()
        var requests = 0
        for t in 0..<60 {
            if policy.observe(device: 85, rate: 44_100,
                sampleTime: t.isMultiple(of: 2) ? 100_000 : 0, active: true,
                at: Double(t)) != nil { requests += 1 }
        }
        XCTAssertEqual(requests, 2)
    }
}
