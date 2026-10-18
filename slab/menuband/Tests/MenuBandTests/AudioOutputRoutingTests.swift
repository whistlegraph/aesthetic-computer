import XCTest
@testable import MenuBand

final class AudioOutputRoutingTests: XCTestCase {
    private let speakers = MBAudioDevice(id: 73, uid: "speakers", name: "Speakers",
                                         inputChannels: 0, outputChannels: 2)
    private func scarlett(_ id: UInt32) -> MBAudioDevice {
        MBAudioDevice(id: id, uid: "scarlett", name: "Scarlett Solo USB",
                      inputChannels: 2, outputChannels: 2)
    }

    func testPinnedInterfaceReturnsWithNewIDAfterSpeakerFallback() {
        XCTAssertEqual(MenuBandAudioDevices.outputTargetID(
            devices: [speakers, scarlett(87)], pinnedUID: "scarlett", systemDefaultID: 73), 87)
        XCTAssertEqual(MenuBandAudioDevices.outputTargetID(
            devices: [speakers], pinnedUID: "scarlett", systemDefaultID: 73), 73)
        // The fallback still exists and remains the system default. The pin
        // must reclaim the interface using its new ID, without a user pick.
        XCTAssertEqual(MenuBandAudioDevices.outputTargetID(
            devices: [speakers, scarlett(157)], pinnedUID: "scarlett", systemDefaultID: 73), 157)
    }

    func testAutomaticOutputFollowsDefaultThatChangesAfterDeviceArrival() {
        XCTAssertEqual(MenuBandAudioDevices.outputTargetID(
            devices: [speakers, scarlett(87)], pinnedUID: nil, systemDefaultID: 87), 87)
        XCTAssertEqual(MenuBandAudioDevices.outputTargetID(
            devices: [speakers], pinnedUID: nil, systemDefaultID: 73), 73)
        let reconnected = [speakers, scarlett(157)]
        XCTAssertEqual(MenuBandAudioDevices.outputTargetID(
            devices: reconnected, pinnedUID: nil, systemDefaultID: 73), 73)
        // No second device-list event: only the default output changes.
        XCTAssertEqual(MenuBandAudioDevices.outputTargetID(
            devices: reconnected, pinnedUID: nil, systemDefaultID: 157), 157)
    }

    func testExplicitSpeakerPickSurvivesSystemDefaultChange() {
        XCTAssertEqual(MenuBandAudioDevices.outputTargetID(
            devices: [speakers, scarlett(157)], pinnedUID: "speakers", systemDefaultID: 157), 73)
    }

    func testUnavailableOrInputOnlyTargetsAreNotBound() {
        let mic = MBAudioDevice(id: 80, uid: "mic", name: "Microphone",
                                inputChannels: 1, outputChannels: 0)
        XCTAssertEqual(MenuBandAudioDevices.outputTargetID(
            devices: [speakers, mic], pinnedUID: "mic", systemDefaultID: 73), 73)
        XCTAssertNil(MenuBandAudioDevices.outputTargetID(
            devices: [speakers], pinnedUID: nil, systemDefaultID: 87))
        XCTAssertNil(MenuBandAudioDevices.outputTargetID(
            devices: [], pinnedUID: "scarlett", systemDefaultID: nil))
    }
}
