import Foundation

struct MenuBandSpotifyTrack: Equatable {
    let id: String
    let title: String
    let artists: String
    let album: String
    let duration: Double
}

struct MenuBandSpotifyPlayback: Equatable {
    let trackID: String?
    let title: String
    let artists: String
    let album: String
    let artworkURL: URL?
    let duration: Double
    let position: Double
    let isPlaying: Bool
}

/// Menu Band's client for the same stable `juked` command contract used by
/// JukeWizard. Spotify.app is never automated or inspected: `juked` owns
/// librespot playback, OAuth, catalog search, and credentials, while this
/// small adapter only translates its JSON into native UI state.
final class MenuBandSpotify {
    enum BackendError: LocalizedError {
        case missing
        case failed(String)

        var errorDescription: String? {
            switch self {
            case .missing:
                return "juked is not installed; run slab/juked/install.sh"
            case .failed(let message):
                return message
            }
        }
    }

    var onState: ((MenuBandSpotifyPlayback?) -> Void)?
    var onStatus: ((String, Bool) -> Void)?

    private let work = DispatchQueue(
        label: "computer.aesthetic.menuband.juked", qos: .userInitiated)
    private var timer: Timer?
    private var pollInFlight = false
    private var started = false

    func start() {
        guard !started else {
            poll()
            return
        }
        timer?.invalidate()
        timer = nil
        started = true
        command(["start"]) { [weak self] result in
            switch result {
            case .success:
                self?.onStatus?("juked headless · ready", false)
                self?.poll()
            case .failure(let error):
                self?.started = false
                self?.timer?.invalidate()
                self?.timer = nil
                self?.onStatus?(error.localizedDescription, true)
            }
        }
        timer = Timer.scheduledTimer(withTimeInterval: 1, repeats: true) {
            [weak self] _ in self?.poll()
        }
    }

    func search(
        _ query: String,
        completion: @escaping (Result<[MenuBandSpotifyTrack], Error>) -> Void
    ) {
        command(["search", query]) { result in
            completion(result.flatMap { data in
                do { return .success(try Self.decodeSearch(data)) }
                catch { return .failure(error) }
            })
        }
    }

    func play(_ track: MenuBandSpotifyTrack) {
        command(["play-id", track.id])
        pollSoon()
    }
    func toggle() { command(["toggle"]); pollSoon() }
    func pause(completion: (() -> Void)? = nil) {
        command(["pause"]) { _ in completion?() }
        pollSoon()
    }
    func next() { command(["next"]); pollSoon() }
    func previous() { command(["previous"]); pollSoon() }
    func seek(to seconds: Double, from currentPosition: Double) {
        let offset = Int(((seconds - currentPosition) * 1_000).rounded())
        command(["seek", String(offset)])
        pollSoon()
    }

    /// Resolve the headless daemon's PID so CDJ Radio can attach a first-party
    /// Core Audio process tap and route Spotify through Menu Band's FX graph.
    func daemonPID(_ completion: @escaping (pid_t?) -> Void) {
        command(["pid"], reportError: false) { result in
            guard case .success(let data) = result,
                  let text = String(data: data, encoding: .utf8)?
                    .trimmingCharacters(in: .whitespacesAndNewlines),
                  let value = Int32(text) else {
                completion(nil)
                return
            }
            completion(value)
        }
    }

    private func pollSoon() {
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.35) {
            [weak self] in self?.poll()
        }
    }

    private func poll() {
        guard started, !pollInFlight else { return }
        pollInFlight = true
        command(["status"], reportError: false) { [weak self] result in
            guard let self else { return }
            self.pollInFlight = false
            guard case .success(let data) = result else { return }
            self.onState?(try? Self.decodePlayback(data))
        }
    }

    private func command(
        _ arguments: [String],
        reportError: Bool = true,
        completion: ((Result<Data, Error>) -> Void)? = nil
    ) {
        work.async {
            let result = Result { try Self.run(arguments) }
            DispatchQueue.main.async { [weak self] in
                if reportError, case .failure(let error) = result {
                    self?.onStatus?(error.localizedDescription, true)
                }
                completion?(result)
            }
        }
    }

    private static func executableURL() -> URL? {
        let installed = URL(fileURLWithPath: NSHomeDirectory())
            .appendingPathComponent(".local/bin/juked")
        if FileManager.default.isExecutableFile(atPath: installed.path) {
            return installed
        }

        // Source-tree fallback for local Menu Band development. Release users
        // get the installed ~/.local/bin copy from slab/juked/install.sh.
        var root = URL(fileURLWithPath: #filePath)
        for _ in 0..<4 { root.deleteLastPathComponent() }
        let source = root.appendingPathComponent("slab/juked/bin/juked")
        return FileManager.default.isExecutableFile(atPath: source.path)
            ? source : nil
    }

    private static func run(_ arguments: [String]) throws -> Data {
        guard let executable = executableURL() else {
            throw BackendError.missing
        }
        let process = Process()
        let output = Pipe()
        let errors = Pipe()
        process.executableURL = executable
        process.arguments = arguments
        process.standardOutput = output
        process.standardError = errors
        try process.run()
        let data = output.fileHandleForReading.readDataToEndOfFile()
        process.waitUntilExit()
        let errorData = errors.fileHandleForReading.readDataToEndOfFile()
        guard process.terminationStatus == 0 else {
            let message = String(
                data: errorData.isEmpty ? data : errorData, encoding: .utf8
            )?.trimmingCharacters(in: .whitespacesAndNewlines)
            throw BackendError.failed(
                message?.isEmpty == false
                    ? message!
                    : "juked exited with status \(process.terminationStatus)")
        }
        return data
    }

    static func decodeSearch(_ data: Data) throws -> [MenuBandSpotifyTrack] {
        guard
            let root = try JSONSerialization.jsonObject(with: data)
                as? [String: Any],
            let tracks = root["tracks"] as? [[String: Any]]
        else { return [] }

        return tracks.compactMap { track in
            guard
                let id = spotifyID(track["id"]),
                let title = track["name"] as? String
            else { return nil }
            let album = (track["album"] as? [String: Any])?["name"]
                as? String ?? ""
            return MenuBandSpotifyTrack(
                id: id,
                title: title,
                artists: names(track["artists"]),
                album: album,
                duration: seconds(
                    track["duration"], milliseconds: track["duration_ms"]))
        }
    }

    static func decodePlayback(_ data: Data) throws -> MenuBandSpotifyPlayback? {
        let object = try JSONSerialization.jsonObject(with: data)
        guard
            let root = object as? [String: Any],
            let item = root["item"] as? [String: Any]
        else { return nil }
        let album = item["album"] as? [String: Any]
        let images = album?["images"] as? [[String: Any]]
        let artwork = (images?.first?["url"] as? String)
            .flatMap(URL.init(string:))
        return MenuBandSpotifyPlayback(
            trackID: spotifyID(item["id"]),
            title: item["name"] as? String ?? "Spotify",
            artists: names(item["artists"]),
            album: album?["name"] as? String ?? "",
            artworkURL: artwork,
            duration: seconds(
                item["duration"], milliseconds: item["duration_ms"]),
            position: seconds(
                root["progress"], milliseconds: root["progress_ms"]),
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
        return objects.compactMap { $0["name"] as? String }
            .joined(separator: ", ")
    }

    private static func seconds(_ value: Any?, milliseconds: Any?) -> Double {
        if let ms = milliseconds as? NSNumber { return ms.doubleValue / 1_000 }
        if let number = value as? NSNumber { return number.doubleValue / 1_000 }
        if let duration = value as? [String: Any] {
            let secs = (duration["secs"] as? NSNumber)?.doubleValue ?? 0
            let nanos = (duration["nanos"] as? NSNumber)?.doubleValue ?? 0
            return secs + nanos / 1_000_000_000
        }
        return 0
    }

    deinit { timer?.invalidate() }
}
