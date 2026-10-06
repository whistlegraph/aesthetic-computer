import XCTest
@testable import MenuBand

final class MonitorIdleTests: XCTestCase {
    func testDisabledMonitorDoesNotOpenMicOnLaunchOrDeviceArrival() {
        for eligible in [false, true] {
            XCTAssertFalse(MenuBandSynth.shouldKeepMonitorOpen(
                wanted: false, attached: false, eligible: eligible))
        }
    }

    func testExplicitMonitoringOpensAvailableInput() {
        XCTAssertTrue(MenuBandSynth.shouldKeepMonitorOpen(
            wanted: true, attached: false, eligible: true))
    }

    func testMutingAnEstablishedInterfaceDoesNotReopenUSBStreams() {
        XCTAssertTrue(MenuBandSynth.shouldKeepMonitorOpen(
            wanted: false, attached: true, eligible: true))
    }

    func testDisconnectedInputIsReleasedEvenWhenMonitoringIsWanted() {
        XCTAssertFalse(MenuBandSynth.shouldKeepMonitorOpen(
            wanted: true, attached: true, eligible: false))
    }
}
