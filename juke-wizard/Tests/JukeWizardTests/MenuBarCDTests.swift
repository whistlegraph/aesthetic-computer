import XCTest
// The standalone app owned MenuBarCD; Menu Band no longer builds that type.
#if canImport(JukeWizard)
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
#endif
