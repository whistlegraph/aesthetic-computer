import XCTest
@testable import MenuBand

final class InterfaceReappearancePolicyTests: XCTestCase {
    func testLaunchWithInterfaceDoesNotReset() {
        var policy = InterfaceReappearancePolicy()
        XCTAssertFalse(policy.observe(interfacePresent: true, at: 10))
        XCTAssertFalse(policy.observe(interfacePresent: true, at: 11))
    }

    func testLaunchWithoutInterfaceThenPlugInResetsOnce() {
        var policy = InterfaceReappearancePolicy()
        XCTAssertFalse(policy.observe(interfacePresent: false, at: 10))
        XCTAssertTrue(policy.observe(interfacePresent: true, at: 12))
        XCTAssertFalse(policy.observe(interfacePresent: true, at: 13))
    }

    func testDropAndReturnResetsButRateChangesAndAuditsDoNot() {
        var policy = InterfaceReappearancePolicy()
        _ = policy.observe(interfacePresent: true, at: 10)
        // A rate flip or a default-output nudge keeps the interface bound.
        XCTAssertFalse(policy.observe(interfacePresent: true, at: 20))
        XCTAssertFalse(policy.observe(interfacePresent: false, at: 30))   // fell to speakers
        XCTAssertTrue(policy.observe(interfacePresent: true, at: 33))     // back
        XCTAssertFalse(policy.observe(interfacePresent: true, at: 36))
    }

    func testFlappingCableIsCappedAtTwoRebuildsPerMinute() {
        var policy = InterfaceReappearancePolicy()
        _ = policy.observe(interfacePresent: true, at: 0)
        var resets = 0
        for i in 0..<6 {
            let t = TimeInterval(1 + i * 8)
            _ = policy.observe(interfacePresent: false, at: t)
            if policy.observe(interfacePresent: true, at: t + 3) { resets += 1 }
        }
        XCTAssertEqual(resets, 2)
        _ = policy.observe(interfacePresent: false, at: 70)
        XCTAssertTrue(policy.observe(interfacePresent: true, at: 73))
    }
}
