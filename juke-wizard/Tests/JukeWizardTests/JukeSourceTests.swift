import XCTest
@testable import MenuBandJuke

final class JukeSourceTests: XCTestCase {
    func testSourceOrderAndLabels() {
        XCTAssertEqual(JukeSource.allCases.map { $0.label(machineName: "Jeffrey’s MacBook Neo") },
                       ["Neo", "Aesthetic", "Spotify", "Apple Music"])
    }

    func testDetachmentFollowsMediaRights() {
        XCTAssertTrue(JukeSource.local.canDetachRecords)
        XCTAssertTrue(JukeSource.aesthetic.canDetachRecords)
        XCTAssertFalse(JukeSource.spotify.canDetachRecords)
        XCTAssertFalse(JukeSource.appleMusic.canDetachRecords)
    }

    func testShortMachineNamePreservesShortHostnames() {
        XCTAssertEqual(JukeSource.shortMachineName("blueberry.local"), "blueberry")
        XCTAssertEqual(JukeSource.shortMachineName("Mac.localdomain"), "Mac")
        XCTAssertEqual(JukeSource.shortMachineName(""), "This Mac")
    }
}
