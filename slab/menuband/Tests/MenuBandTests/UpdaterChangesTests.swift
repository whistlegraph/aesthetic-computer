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

final class UpdaterSymlinkTests: XCTestCase {
    func testFindsDirectoriesThatDanglingSymlinksPointInto() throws {
        let fm = FileManager.default
        let root = fm.temporaryDirectory.appendingPathComponent("updater-\(UUID().uuidString)")
        defer { try? fm.removeItem(at: root) }
        let src = root.appendingPathComponent("slab/menuband/Sources/CSinger")
        try fm.createDirectory(at: src, withIntermediateDirectories: true)
        try fm.createDirectory(at: root.appendingPathComponent("here"), withIntermediateDirectories: true)
        fm.createFile(atPath: root.appendingPathComponent("here/ok.c").path, contents: Data())
        try fm.createSymbolicLink(atPath: src.appendingPathComponent("singer.c").path,
                                  withDestinationPath: "../../../../live/singer.c")
        try fm.createSymbolicLink(atPath: src.appendingPathComponent("fine.c").path,
                                  withDestinationPath: "../../../../here/ok.c")
        try fm.createSymbolicLink(atPath: src.appendingPathComponent("escape.c").path,
                                  withDestinationPath: "/nowhere/at/all.c")
        XCTAssertEqual(MenuBandUpdater.danglingTargetDirs(root: root, under: ["slab/menuband"]), ["live"])
    }
}
