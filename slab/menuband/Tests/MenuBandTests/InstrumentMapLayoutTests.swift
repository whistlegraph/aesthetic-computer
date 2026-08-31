import XCTest
@testable import MenuBand

final class InstrumentMapLayoutTests: XCTestCase {
    func testCatalogueOrderIsTheDefault() {
        let defaults = UserDefaults.standard
        let key = "instrumentGridLayout"
        let previous = defaults.object(forKey: key)
        defer {
            if let previous {
                defaults.set(previous, forKey: key)
            } else {
                defaults.removeObject(forKey: key)
            }
        }

        defaults.removeObject(forKey: key)

        XCTAssertEqual(InstrumentListView.gridLayout, .catalogue)
        XCTAssertEqual(InstrumentListView.program(inSlot: 0), 0)
        XCTAssertEqual(InstrumentListView.program(inSlot: 127), 127)
    }

    func testExplicitTimbreOrderStillWorks() {
        let defaults = UserDefaults.standard
        let key = "instrumentGridLayout"
        let previous = defaults.object(forKey: key)
        defer {
            if let previous {
                defaults.set(previous, forKey: key)
            } else {
                defaults.removeObject(forKey: key)
            }
        }

        defaults.set("timbre", forKey: key)

        XCTAssertEqual(InstrumentListView.gridLayout, .timbre)
        XCTAssertEqual(InstrumentListView.program(inSlot: 0),
                       GMTimbreLayout.programAtSlot[0])
    }
}
