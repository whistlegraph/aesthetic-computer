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

    func testExplicitOffReleasesAnEstablishedInterface() {
        XCTAssertFalse(MenuBandSynth.shouldKeepMonitorOpen(
            wanted: false, attached: true, eligible: true))
    }

    func testManualOffSurvivesActivationAndDeviceReconnection() {
        for present in [true, false, true, true] {
            XCTAssertFalse(MenuBandController.monitoringSelection(
                manualChoice: false, interfacePresent: present))
        }
    }

    func testAutomaticMonitoringAndExplicitOnRemainAvailable() {
        for present in [false, true] {
            XCTAssertEqual(MenuBandController.monitoringSelection(
                manualChoice: nil, interfacePresent: present), present)
            XCTAssertTrue(MenuBandController.monitoringSelection(
                manualChoice: true, interfacePresent: present))
        }
    }

    func testDisconnectedInputIsReleasedEvenWhenMonitoringIsWanted() {
        XCTAssertFalse(MenuBandSynth.shouldKeepMonitorOpen(
            wanted: true, attached: true, eligible: false))
    }
}
