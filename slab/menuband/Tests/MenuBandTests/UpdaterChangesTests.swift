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

final class UpdaterBuildInputsTests: XCTestCase {
    func testOnlyCompiledOrPackagedChangesOfferAnUpdate() throws {
        let root = FileManager.default.temporaryDirectory.appendingPathComponent("update-inputs-\(UUID())")
        try FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: root) }
        func git(_ args: String...) throws -> String {
            let p = Process()
            p.executableURL = URL(fileURLWithPath: "/usr/bin/git")
            p.arguments = ["-C", root.path] + args
            let output = Pipe()
            p.standardOutput = output
            p.standardError = output
            try p.run()
            let data = output.fileHandleForReading.readDataToEndOfFile()
            p.waitUntilExit()
            XCTAssertEqual(p.terminationStatus, 0, String(decoding: data, as: UTF8.self))
            return String(decoding: data, as: UTF8.self).trimmingCharacters(in: .whitespacesAndNewlines)
        }
        func commit(_ path: String, _ subject: String) throws {
            let file = root.appendingPathComponent(path)
            try FileManager.default.createDirectory(at: file.deletingLastPathComponent(), withIntermediateDirectories: true)
            try Data(subject.utf8).write(to: file)
            _ = try git("add", ".")
            _ = try git("commit", "-qm", subject)
        }
        _ = try git("init", "-q")
        _ = try git("config", "user.name", "Update test")
        _ = try git("config", "user.email", "update@example.invalid")
        _ = try git("config", "commit.gpgsign", "false")
        _ = try git("config", "core.hooksPath", "/dev/null")
        _ = try git("commit", "--allow-empty", "-qm", "base")
        let built = try git("rev-parse", "HEAD")
        let gitDir = root.appendingPathComponent(".git").path
        for path in [
            "slab/bin/x-mcp.mjs", "slab/menuband/marketing/post.md",
            "slab/menuband/Tests/test.swift", "live/score-bounce.c",
            "fedac/native/src/display.c", "juke-wizard/README.md",
            "slab/macos-audio/Sources/ACAudioRoomCLI/main.swift",
        ] {
            try commit(path, "social: unrelated change")
        }
        XCTAssertTrue(MenuBandUpdater.changes(gitDir: gitDir, built: built, target: "HEAD").isEmpty)
        for path in [
            "slab/menuband/Sources/MenuBand/AppDelegate.swift",
            "slab/menuband/Sources/MenuBand/Resources/piano.raw",
            "live/singer.c", "live/world/harvest.cpp", "live/world/world/harvest.h",
            "fedac/native/src/gm_synth.c", "fedac/native/samples/piano/c4.raw",
            "juke-wizard/Sources/JukeWizard/Player.swift",
            "slab/macos-audio/Sources/ACMacAudio/Output.swift",
            "slab/bin/build-lock.sh", "slab/menuband/quicklook/Preview/Info.plist",
        ] {
            let before = try git("rev-parse", "HEAD")
            try commit(path, "Menu Band: audible fix")
            XCTAssertEqual(MenuBandUpdater.changes(gitDir: gitDir, built: before, target: "HEAD").map(\.subject), ["audible fix"], path)
        }
        let before = try git("rev-parse", "HEAD")
        try commit("live/singer.c", "temporary change")
        _ = try git("revert", "--no-edit", "HEAD")
        XCTAssertTrue(MenuBandUpdater.changes(gitDir: gitDir, built: before, target: "HEAD").isEmpty)
    }
}
