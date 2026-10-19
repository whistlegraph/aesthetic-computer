import XCTest
@testable import MenuBandJuke

final class RemoteLibraryTests: XCTestCase {
    func testMissingRemoteTrackRemainsVisibleAndNotesSurvive() throws {
        let root = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: root) }
        let audio = root.appendingPathComponent("lane/out/song.mp3")
        let catalog = root.appendingPathComponent("library.json")
        let object: [String: Any] = ["tracks": [
            ["path": audio.path, "title": "Song", "remoteKey": "lane/out/song.mp3",
             "updated": "2026-10-15T12:00:00Z"],
            ["path": root.appendingPathComponent("missing-original.mp3").path]
        ]]
        try JSONSerialization.data(withJSONObject: object).write(to: catalog)
        let library = Library(inputs: [catalog.path])
        XCTAssertEqual(library.tracks.count, 1)
        let track = try XCTUnwrap(library.tracks.first)
        XCTAssertEqual(track.remoteKey, "lane/out/song.mp3")
        XCTAssertNotEqual(track.renderedAt, .distantPast)
        track.data.notes = "Keep this note while audio is uncached"
        track.save()
        XCTAssertEqual(Track.loadSidecar(for: audio).notes, track.data.notes)
        XCTAssertFalse(FileManager.default.fileExists(atPath: audio.path))
        let playlist = root.appendingPathComponent("playlist.m3u8")
        try "#EXTM3U\nlane/out/song.mp3\n".write(to: playlist, atomically: true, encoding: .utf8)
        let scoped = Library(inputs: [playlist.path], catalogPath: catalog.path)
        XCTAssertEqual(scoped.tracks.count, 1)
        XCTAssertEqual(scoped.tracks.first?.remoteKey, "lane/out/song.mp3")
        XCTAssertEqual(scoped.tracks.first?.data.notes, track.data.notes)
        let audition = root.appendingPathComponent("lane/out/song-audition.mp3")
        let remote: [String: Any] = ["tracks": [["key": "lane/out/song-audition.mp3", "path": audition.path]]]
        try JSONSerialization.data(withJSONObject: remote).write(to: root.appendingPathComponent("juke-remote.json"))
        let single = Library(inputs: [audition.path], catalogPath: catalog.path)
        XCTAssertEqual(single.tracks.first?.remoteKey, "lane/out/song-audition.mp3")
    }
}
