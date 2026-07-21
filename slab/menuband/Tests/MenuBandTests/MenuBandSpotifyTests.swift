import XCTest
@testable import MenuBand

final class MenuBandSpotifyTests: XCTestCase {
    func testDecodesSearchResultsFromJukedContract() throws {
        let data = Data(#"""
        {
          "tracks": [{
            "id": {"id": "abc123"},
            "name": "Test Track",
            "artists": [{"name": "Artist One"}, {"name": "Artist Two"}],
            "album": {"name": "Test Album"},
            "duration_ms": 183500
          }]
        }
        """#.utf8)

        let tracks = try MenuBandSpotify.decodeSearch(data)

        XCTAssertEqual(tracks, [MenuBandSpotifyTrack(
            id: "abc123", title: "Test Track",
            artists: "Artist One, Artist Two", album: "Test Album",
            duration: 183.5)])
    }

    func testDecodesNowPlayingAndArtwork() throws {
        let data = Data(#"""
        {
          "is_playing": true,
          "progress_ms": 42125,
          "item": {
            "id": "track-id",
            "name": "Now Playing",
            "artists": [{"name": "Player"}],
            "duration_ms": 240000,
            "album": {
              "name": "The Record",
              "images": [{"url": "https://example.com/cover.jpg"}]
            }
          }
        }
        """#.utf8)

        let state = try XCTUnwrap(MenuBandSpotify.decodePlayback(data))

        XCTAssertEqual(state.trackID, "track-id")
        XCTAssertEqual(state.title, "Now Playing")
        XCTAssertEqual(state.artists, "Player")
        XCTAssertEqual(state.album, "The Record")
        XCTAssertEqual(state.artworkURL?.absoluteString,
                       "https://example.com/cover.jpg")
        XCTAssertEqual(state.duration, 240)
        XCTAssertEqual(state.position, 42.125)
        XCTAssertTrue(state.isPlaying)
    }

    func testEmptyStatusDecodesAsNoPlayback() throws {
        let data = Data(#"{"is_playing":false}"#.utf8)
        XCTAssertNil(try MenuBandSpotify.decodePlayback(data))
    }
}
