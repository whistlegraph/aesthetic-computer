import XCTest
@testable import JukeWizard

final class MenuBarCDTests: XCTestCase {
    func testCreditShowsArtistBeforeTrackTitle() {
        XCTAssertEqual(
            MenuBarCD.credit(artist: "Aesthetic Dot Computer", title: "Color Test"),
            "Aesthetic Dot Computer — Color Test"
        )
    }

    func testCreditOmitsMissingFieldsAndWhitespace() {
        XCTAssertEqual(MenuBarCD.credit(artist: nil, title: "  Color Test  "), "Color Test")
        XCTAssertEqual(MenuBarCD.credit(artist: "  Aesthetic  ", title: ""), "Aesthetic")
    }
}
