import Foundation

/// A small, dependency-free NTS "now playing" client that can be dropped into
/// a macOS app.
///
/// Public NTS data provides the current live show and its artwork. Song-level
/// artist/title data is an NTS Supporter feature, so this file accepts an
/// optional authenticated provider rather than embedding credentials or
/// depending on NTS's private web implementation.
///
/// Typical use:
///
///     let client = NTSNowPlayingClient(
///         supporterTrackProvider: myAuthenticatedNTSProvider
///     )
///     client.fetch(.live1) { result in
///         if case .success(let nowPlaying) = result {
///             statusButton.image = NSImage(contentsOf: nowPlaying.artworkURL)
///             statusButton.toolTip = nowPlaying.tooltip
///         }
///     }
///
/// Without a provider, `track` is nil and `tooltip` falls back to the public
/// show title. With an entitled provider, the same result is enriched with the
/// current artist and song.

struct NTSStream: Equatable {
    /// NTS uses "1" and "2" for its live channels. Infinite Mixtapes use
    /// their mixtape alias as the stream identifier.
    let id: String
    let fallbackTitle: String
    let fallbackArtworkURL: URL?
    fileprivate let liveChannelName: String?

    static let live1 = NTSStream(
        id: "1",
        fallbackTitle: "NTS 1",
        fallbackArtworkURL: nil,
        liveChannelName: "1"
    )

    static let live2 = NTSStream(
        id: "2",
        fallbackTitle: "NTS 2",
        fallbackArtworkURL: nil,
        liveChannelName: "2"
    )

    /// Use this for an Infinite Mixtape selected elsewhere in the app. Its
    /// public title/artwork can come from the mixtape picker; an authenticated
    /// provider may then add the current track for `alias`.
    static func mixtape(alias: String, title: String, artworkURL: URL?) -> NTSStream {
        NTSStream(
            id: alias,
            fallbackTitle: title,
            fallbackArtworkURL: artworkURL,
            liveChannelName: nil
        )
    }
}

struct NTSTrack: Equatable {
    let artist: String
    let title: String
    let startedAt: Date?
}

struct NTSNowPlaying: Equatable {
    let stream: NTSStream
    let showTitle: String
    let showStartedAt: Date?
    let showEndsAt: Date?
    let artworkURL: URL?
    let track: NTSTrack?

    /// Suitable for `NSStatusBarButton.toolTip`.
    var tooltip: String {
        if let track {
            let artist = track.artist.trimmingCharacters(in: .whitespacesAndNewlines)
            let title = track.title.trimmingCharacters(in: .whitespacesAndNewlines)
            if !artist.isEmpty && !title.isEmpty { return "\(artist) — \(title)" }
            if !title.isEmpty { return title }
            if !artist.isEmpty { return artist }
        }
        return showTitle
    }
}

/// Implement this with an NTS-approved authenticated session for a signed-in
/// Supporter. The current NTS web data model identifies streams by `stream_id`
/// and represents tracks with artist names, a song title, and a start time.
///
/// Deliberately keep login, entitlement checks, cookies, and tokens in the
/// provider. Do not hard-code or copy a browser session into this source file.
protocol NTSSupporterTrackProviding {
    func currentTrack(
        forStreamID streamID: String,
        completion: @escaping (Result<NTSTrack?, Error>) -> Void
    )
}

/// Convenience adapter when an app already has an authenticated async/callback
/// function and does not need a separate provider class.
struct NTSClosureTrackProvider: NTSSupporterTrackProviding {
    typealias Fetch = (
        _ streamID: String,
        _ completion: @escaping (Result<NTSTrack?, Error>) -> Void
    ) -> Void

    let fetch: Fetch

    func currentTrack(
        forStreamID streamID: String,
        completion: @escaping (Result<NTSTrack?, Error>) -> Void
    ) {
        fetch(streamID, completion)
    }
}

final class NTSNowPlayingClient {
    private static let liveURL = URL(string: "https://www.nts.live/api/v2/live")!

    private let session: URLSession
    private let supporterTrackProvider: NTSSupporterTrackProviding?
    private let callbackQueue: DispatchQueue

    init(
        session: URLSession = .shared,
        supporterTrackProvider: NTSSupporterTrackProviding? = nil,
        callbackQueue: DispatchQueue = .main
    ) {
        self.session = session
        self.supporterTrackProvider = supporterTrackProvider
        self.callbackQueue = callbackQueue
    }

    /// Fetch public show/artwork metadata, then optionally enrich the result
    /// with the signed-in Supporter's current artist/song metadata.
    func fetch(
        _ stream: NTSStream,
        completion: @escaping (Result<NTSNowPlaying, Error>) -> Void
    ) {
        let deliver: (Result<NTSNowPlaying, Error>) -> Void = {
            [callbackQueue] result in
            callbackQueue.async { completion(result) }
        }

        guard let channelName = stream.liveChannelName else {
            let fallback = NTSNowPlaying(
                stream: stream,
                showTitle: stream.fallbackTitle,
                showStartedAt: nil,
                showEndsAt: nil,
                artworkURL: stream.fallbackArtworkURL,
                track: nil
            )
            enrich(fallback, completion: deliver)
            return
        }

        var request = URLRequest(url: Self.liveURL)
        request.setValue("application/vnd.live-list+json", forHTTPHeaderField: "Accept")

        session.dataTask(with: request) { data, response, error in
            if let error {
                deliver(.failure(error))
                return
            }
            guard
                let http = response as? HTTPURLResponse,
                (200..<300).contains(http.statusCode),
                let data
            else {
                deliver(.failure(NTSNowPlayingError.invalidResponse))
                return
            }

            do {
                let payload = try JSONDecoder().decode(LiveResponse.self, from: data)
                guard let channel = payload.results.first(where: {
                    $0.channelName == channelName
                }) else {
                    throw NTSNowPlayingError.channelNotFound(channelName)
                }

                let now = channel.now
                let publicResult = NTSNowPlaying(
                    stream: stream,
                    showTitle: now?.broadcastTitle ?? stream.fallbackTitle,
                    showStartedAt: Self.parseDate(now?.startTimestamp),
                    showEndsAt: Self.parseDate(now?.endTimestamp),
                    artworkURL: now?.embeds.details.media.bestArtworkURL
                        ?? stream.fallbackArtworkURL,
                    track: nil
                )
                self.enrich(publicResult, completion: deliver)
            } catch {
                deliver(.failure(error))
            }
        }.resume()
    }

    private func enrich(
        _ publicResult: NTSNowPlaying,
        completion: @escaping (Result<NTSNowPlaying, Error>) -> Void
    ) {
        guard let supporterTrackProvider else {
            completion(.success(publicResult))
            return
        }

        supporterTrackProvider.currentTrack(forStreamID: publicResult.stream.id) {
            result in
            switch result {
            case .success(let track):
                completion(.success(NTSNowPlaying(
                    stream: publicResult.stream,
                    showTitle: publicResult.showTitle,
                    showStartedAt: publicResult.showStartedAt,
                    showEndsAt: publicResult.showEndsAt,
                    artworkURL: publicResult.artworkURL,
                    track: track
                )))
            case .failure:
                // Public show metadata remains useful if the user is signed
                // out, the subscription lapsed, or the track feed is empty.
                completion(.success(publicResult))
            }
        }
    }

    private static func parseDate(_ value: String?) -> Date? {
        guard let value else { return nil }
        return ISO8601DateFormatter().date(from: value)
    }
}

enum NTSNowPlayingError: LocalizedError {
    case invalidResponse
    case channelNotFound(String)

    var errorDescription: String? {
        switch self {
        case .invalidResponse:
            return "NTS returned an invalid response."
        case .channelNotFound(let channel):
            return "NTS channel \(channel) was not present in the live response."
        }
    }
}

private struct LiveResponse: Decodable {
    let results: [LiveChannel]
}

private struct LiveChannel: Decodable {
    let channelName: String
    let now: LiveBroadcast?

    enum CodingKeys: String, CodingKey {
        case channelName = "channel_name"
        case now
    }
}

private struct LiveBroadcast: Decodable {
    let broadcastTitle: String
    let startTimestamp: String?
    let endTimestamp: String?
    let embeds: Embeds

    enum CodingKeys: String, CodingKey {
        case broadcastTitle = "broadcast_title"
        case startTimestamp = "start_timestamp"
        case endTimestamp = "end_timestamp"
        case embeds
    }
}

private struct Embeds: Decodable {
    let details: Details
}

private struct Details: Decodable {
    let media: Media
}

private struct Media: Decodable {
    let pictureMedium: URL?
    let pictureMediumLarge: URL?
    let backgroundMedium: URL?

    var bestArtworkURL: URL? {
        pictureMediumLarge ?? pictureMedium ?? backgroundMedium
    }

    enum CodingKeys: String, CodingKey {
        case pictureMedium = "picture_medium"
        case pictureMediumLarge = "picture_medium_large"
        case backgroundMedium = "background_medium"
    }
}
