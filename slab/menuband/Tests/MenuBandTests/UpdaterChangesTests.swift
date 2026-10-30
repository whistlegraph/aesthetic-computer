import XCTest
@testable import MenuBand

final class UpdaterChangesTests: XCTestCase {
    func testParsesHashAndSubjectAndDropsPrefix() {
        let log = "3d9a95dfc0\tMenu Band: cc drum kit from Will's library\n997fe354da\tmonitor toggle-off no longer crashes\n"
        XCTAssertEqual(MenuBandUpdater.parseChanges(log), [
            .init(hash: "3d9a95dfc0", subject: "cc drum kit from Will's library"),
            .init(hash: "997fe354da", subject: "monitor toggle-off no longer crashes"),
        ])
    }

    func testKeepsTabsInsideSubjectAndSkipsJunk() {
        let log = "abc\ta\tb\n\nnot-a-line\n"
        XCTAssertEqual(MenuBandUpdater.parseChanges(log), [.init(hash: "abc", subject: "a\tb")])
    }

    func testEmptyLogMeansNoUpdate() {
        XCTAssertTrue(MenuBandUpdater.parseChanges("").isEmpty)
    }
}
