import Foundation

struct SpotifyTrackResult: Equatable {
    let id: String
    let title: String
    let artists: String
    let album: String
    let duration: Double
}

struct SpotifyPlaybackState: Equatable {
    let trackID: String?
    let title: String
    let artists: String
    let album: String
    let artworkURL: URL?
    let duration: Double
    let position: Double
    let isPlaying: Bool
}

/// JukeWizard talks to our `juked` contract, never directly to Spotify.app or
/// a third-party library. The current engine can therefore be pinned, forked,
/// or replaced without changing the native UI.
final class JukeSpotify {
    enum BackendError: LocalizedError {
        case missing
        case failed(String)

        var errorDescription: String? {
            switch self {
            case .missing: return "juked is not installed; run slab/juked/install.sh"
            case .failed(let message): return message
            }
        }
    }

    var onState: ((SpotifyPlaybackState?) -> Void)?
    var onStatus: ((String, Bool) -> Void)?

    private let work = DispatchQueue(label: "computer.aesthetic.juked", qos: .userInitiated)
    private var timer: Timer?
    private var pollInFlight = false

    func start() {
        command(["start"]) { [weak self] result in
            switch result {
            case .success:
                self?.onStatus?("● juked headless · ready", false)
                self?.poll()
            case .failure(let error):
                self?.onStatus?("⚠ \(error.localizedDescription)", true)
            }
        }
        timer = Timer.scheduledTimer(withTimeInterval: 1.0, repeats: true) { [weak self] _ in
            self?.poll()
        }
    }

    func search(_ query: String, completion: @escaping (Result<[SpotifyTrackResult], Error>) -> Void) {
        command(["search", query]) { result in
            completion(result.flatMap { data in
                do { return .success(try Self.decodeSearch(data)) }
                catch { return .failure(error) }
            })
        }
    }

    func play(_ track: SpotifyTrackResult) { command(["play-id", track.id]); pollSoon() }
    func play(trackID: String) { command(["play-id", trackID]); pollSoon() }
    func toggle() { command(["toggle"]); pollSoon() }
    func play() { command(["play"]); pollSoon() }
    func pause() { command(["pause"]); pollSoon() }
    func next() { command(["next"]); pollSoon() }
    func previous() { command(["previous"]); pollSoon() }
    func seek(offsetMS: Int) { command(["seek", String(offsetMS)]); pollSoon() }
    func volume(percent: Int) { command(["volume", String(max(0, min(100, percent)))]) }

    /// spotify_player binds its Core Audio stream when the daemon starts.
    /// Reopen that stream on a newly selected device, then restore the staged
    /// track, position, and paused/playing state.
    func refreshOutputDevice(resuming state: SpotifyPlaybackState?) {
        command(["restart"]) { [weak self] result in
            guard let self, case .success = result else { return }
            guard let state, let trackID = state.trackID else { self.pollSoon(); return }
            self.command(["play-id", trackID]) { [weak self] result in
                guard let self, case .success = result else { return }
                let offset = max(0, Int((state.position * 1000).rounded()))
                let restorePlayState = { [weak self] in
                    guard let self else { return }
                    if !state.isPlaying { self.command(["pause"]) }
                    self.pollSoon()
                }
                if offset > 500 {
                    self.command(["seek", String(offset)]) { _ in restorePlayState() }
                } else {
                    restorePlayState()
                }
            }
        }
    }

    func daemonPID() -> pid_t? {
        guard let data = try? Self.run(["pid"]),
              let text = String(data: data, encoding: .utf8)?.trimmingCharacters(in: .whitespacesAndNewlines),
              let value = Int32(text) else { return nil }
        return value
    }

    static func trackID(from value: String) -> String? {
        if value.hasPrefix("spotify:track:") { return String(value.dropFirst("spotify:track:".count)) }
        guard let url = URL(string: value), url.host == "open.spotify.com" else { return nil }
        let parts = url.pathComponents.filter { $0 != "/" }
        guard let i = parts.firstIndex(of: "track"), i + 1 < parts.count else { return nil }
        return parts[i + 1]
    }

    private func pollSoon() {
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.35) { [weak self] in self?.poll() }
    }

    private func poll() {
        guard !pollInFlight else { return }
        pollInFlight = true
        command(["status"], reportError: false) { [weak self] result in
            guard let self else { return }
            self.pollInFlight = false
            guard case .success(let data) = result else { return }
            self.onState?(try? Self.decodePlayback(data))
        }
    }

    private func command(_ arguments: [String], reportError: Bool = true,
                         completion: ((Result<Data, Error>) -> Void)? = nil) {
        work.async {
            let result = Result { try Self.run(arguments) }
            DispatchQueue.main.async { [weak self] in
                if reportError, case .failure(let error) = result {
                    self?.onStatus?("⚠ \(error.localizedDescription)", true)
                }
                completion?(result)
            }
        }
    }

    private static func executableURL() -> URL? {
        let installed = URL(fileURLWithPath: NSHomeDirectory()).appendingPathComponent(".local/bin/juked")
        if FileManager.default.isExecutableFile(atPath: installed.path) { return installed }
        var root = URL(fileURLWithPath: #filePath)
        for _ in 0..<4 { root.deleteLastPathComponent() }
        let source = root.appendingPathComponent("slab/juked/bin/juked")
        return FileManager.default.isExecutableFile(atPath: source.path) ? source : nil
    }

    private static func run(_ arguments: [String]) throws -> Data {
        guard let executable = executableURL() else { throw BackendError.missing }
        let process = Process()
        let output = Pipe(), errors = Pipe()
        process.executableURL = executable
        process.arguments = arguments
        process.standardOutput = output
        process.standardError = errors
        try process.run()
        let data = output.fileHandleForReading.readDataToEndOfFile()
        process.waitUntilExit()
        let errorData = errors.fileHandleForReading.readDataToEndOfFile()
        guard process.terminationStatus == 0 else {
            let message = String(data: errorData.isEmpty ? data : errorData, encoding: .utf8)?
                .trimmingCharacters(in: .whitespacesAndNewlines)
            throw BackendError.failed(message?.isEmpty == false ? message! : "juked exited with status \(process.terminationStatus)")
        }
        return data
    }

    private static func decodeSearch(_ data: Data) throws -> [SpotifyTrackResult] {
        guard let root = try JSONSerialization.jsonObject(with: data) as? [String: Any],
              let tracks = root["tracks"] as? [[String: Any]] else { return [] }
        return tracks.compactMap { track in
            guard let id = spotifyID(track["id"]), let title = track["name"] as? String else { return nil }
            let artists = names(track["artists"])
            let album = (track["album"] as? [String: Any])?["name"] as? String ?? ""
            return SpotifyTrackResult(id: id, title: title, artists: artists, album: album,
                                      duration: seconds(track["duration"], milliseconds: track["duration_ms"]))
        }
    }

    private static func decodePlayback(_ data: Data) throws -> SpotifyPlaybackState? {
        let object = try JSONSerialization.jsonObject(with: data)
        guard let root = object as? [String: Any], let item = root["item"] as? [String: Any] else { return nil }
        let album = item["album"] as? [String: Any]
        let images = album?["images"] as? [[String: Any]]
        let art = (images?.first?["url"] as? String).flatMap(URL.init(string:))
        return SpotifyPlaybackState(
            trackID: spotifyID(item["id"]),
            title: item["name"] as? String ?? "Spotify",
            artists: names(item["artists"]),
            album: album?["name"] as? String ?? "",
            artworkURL: art,
            duration: seconds(item["duration"], milliseconds: item["duration_ms"]),
            position: seconds(root["progress"], milliseconds: root["progress_ms"]),
            isPlaying: root["is_playing"] as? Bool ?? false)
    }

    private static func spotifyID(_ value: Any?) -> String? {
        if let string = value as? String { return string }
        if let object = value as? [String: Any] {
            return object["id"] as? String ?? object["_id"] as? String
        }
        return nil
    }

    private static func names(_ value: Any?) -> String {
        guard let objects = value as? [[String: Any]] else { return "" }
        return objects.compactMap { $0["name"] as? String }.joined(separator: ", ")
    }

    private static func seconds(_ value: Any?, milliseconds: Any?) -> Double {
        if let ms = milliseconds as? NSNumber { return ms.doubleValue / 1000 }
        if let number = value as? NSNumber { return number.doubleValue / 1000 }
        if let duration = value as? [String: Any] {
            let secs = (duration["secs"] as? NSNumber)?.doubleValue ?? 0
            let nanos = (duration["nanos"] as? NSNumber)?.doubleValue ?? 0
            return secs + nanos / 1_000_000_000
        }
        return 0
    }

    deinit { timer?.invalidate() }
}
