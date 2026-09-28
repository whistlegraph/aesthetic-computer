#if !MAC_APP_STORE
import AppKit
import Foundation

/// Backs every tape take up to the signed-in Aesthetic Computer handle.
///
/// Each export lands a copy of the take in a durable queue folder, then a
/// serial utility queue drains it against `/api/menuband-takes`:
///
///     begin (takeId, file sizes) → presigned PUTs → PUT each file → commit
///
/// A queue entry is deleted only after `commit` (or when the server says the
/// take is already committed), so a take recorded offline, signed out, or
/// with an expired session waits on disk until the next drain. Drains run on
/// launch, on any ~/.ac-token change, after each take, and on a backoff timer
/// after a failure. Nothing here touches main or the audio graph.
///
/// Direct-download build only: the sandboxed App Store build can't read
/// ~/.ac-token.
///
///     ~/Library/Application Support/MenuBand/cloud-queue/<takeId>/
///         manifest.json   written last; a folder without one is ignored
///         mix.mp3 | mix.wav, tones.wav, percussion.wav, voice.wav,
///         notes.mid, mix.json, cover.png
final class MenuBandCloud {
    static let shared = MenuBandCloud()

    /// UserDefaults flag, next to the tape-deck flags. Unset = on, so a
    /// signed-in user backs up without having to find the switch.
    static let enabledDefaultsKey = "MenuBandCloudBackupEnabled"
    /// Posted on main when the queue or its status changes (Settings listens).
    static let statusChanged = Notification.Name("MenuBandCloudStatusChanged")

    static var isEnabled: Bool {
        get { UserDefaults.standard.object(forKey: enabledDefaultsKey) as? Bool ?? true }
        set {
            UserDefaults.standard.set(newValue, forKey: enabledDefaultsKey)
            if newValue { shared.drainSoon() }
        }
    }

    /// Override with MENUBAND_TAKES_URL to point a dev build at a local lith.
    private static let endpoint = URL(string:
        ProcessInfo.processInfo.environment["MENUBAND_TAKES_URL"]
            ?? "https://aesthetic.computer/api/menuband-takes")!

    private struct Manifest: Codable {
        var takeId: String
        var recordedAt: String
        var machine: String
        var duration: Double
        var program: Int?
        var bpm: Double?
        var files: [String]
    }

    private static let contentTypes: [String: String] = [
        "mix.mp3": "audio/mpeg", "mix.wav": "audio/wav",
        "tones.wav": "audio/wav", "percussion.wav": "audio/wav", "voice.wav": "audio/wav",
        "notes.mid": "audio/midi", "mix.json": "application/json", "cover.png": "image/png",
    ]

    private let queue = DispatchQueue(label: "computer.aestheticcomputer.menuband.cloud", qos: .utility)
    private let session: URLSession = {
        let config = URLSessionConfiguration.ephemeral
        config.timeoutIntervalForRequest = 60
        config.timeoutIntervalForResource = 30 * 60
        config.waitsForConnectivity = false
        return URLSession(configuration: config)
    }()

    // Queue-confined state.
    private var draining = false
    private var backoff: TimeInterval = 30
    private var retryItem: DispatchWorkItem?
    /// Takes already committed this run, so a re-export of an old take (the
    /// export cache holds four) doesn't queue it a second time.
    private var committed = Set<String>()

    private init() {}

    private var root: URL {
        FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appendingPathComponent("MenuBand/cloud-queue", isDirectory: true)
    }

    /// Call once on launch, on main.
    func start() {
        dispatchPrecondition(condition: .onQueue(.main))
        ACSession.shared.startWatching { [weak self] in
            self?.postStatus()
            self?.drainSoon()
        }
        drainSoon()
    }

    /// Takes waiting on disk. Cheap; safe from any thread.
    var pendingCount: Int {
        let dirs = (try? FileManager.default.contentsOfDirectory(
            at: root, includingPropertiesForKeys: nil)) ?? []
        return dirs.filter {
            FileManager.default.fileExists(atPath: $0.appendingPathComponent("manifest.json").path)
        }.count
    }

    // MARK: - Enqueue

    /// Copy a finished take into the queue and drain. Callable from any
    /// thread; the copies run on the cloud queue.
    func enqueue(takeID: UUID, recordedAt: Date, duration: Double, program: Int?, bpm: Double?,
                 mix: URL, stems: URL?, cover: NSImage?) {
        queue.async { [self] in
            let takeId = takeID.uuidString
            guard !committed.contains(takeId) else { return }
            let dir = root.appendingPathComponent(takeId, isDirectory: true)
            let fm = FileManager.default
            if fm.fileExists(atPath: dir.appendingPathComponent("manifest.json").path) {
                drain()
                return
            }
            do {
                try? fm.removeItem(at: dir)   // a half-written folder from a crash
                try fm.createDirectory(at: dir, withIntermediateDirectories: true)
                var files: [String] = []
                func add(_ source: URL, as name: String) {
                    guard fm.fileExists(atPath: source.path) else { return }
                    do {
                        try fm.copyItem(at: source, to: dir.appendingPathComponent(name))
                        files.append(name)
                    } catch { NSLog("MenuBand cloud: copy \(name) failed: \(error)") }
                }
                add(mix, as: mix.pathExtension.lowercased() == "mp3" ? "mix.mp3" : "mix.wav")
                if let stems {
                    for name in ["tones.wav", "percussion.wav", "voice.wav", "notes.mid", "mix.json"] {
                        add(stems.appendingPathComponent(name), as: name)
                    }
                }
                if let cover, let png = Self.pngData(cover) {
                    if (try? png.write(to: dir.appendingPathComponent("cover.png"))) != nil {
                        files.append("cover.png")
                    }
                }
                let manifest = Manifest(
                    takeId: takeId,
                    recordedAt: ISO8601DateFormatter().string(from: recordedAt),
                    machine: Host.current().localizedName ?? ProcessInfo.processInfo.hostName,
                    duration: duration, program: program, bpm: bpm, files: files)
                try JSONEncoder().encode(manifest)
                    .write(to: dir.appendingPathComponent("manifest.json"), options: .atomic)
                NSLog("MenuBand cloud: queued \(takeId) (\(files.joined(separator: ", ")))")
            } catch {
                NSLog("MenuBand cloud: queue \(takeId) failed: \(error)")
                try? fm.removeItem(at: dir)
                return
            }
            postStatus()
            drain()
        }
    }

    // MARK: - Drain

    func drainSoon() { queue.async { [self] in drain() } }

    /// Runs on `queue`. Serial: one take, one file at a time.
    private func drain() {
        guard !draining, Self.isEnabled else { return }
        guard let token = ACSession.shared.freshToken() else {
            if pendingCount > 0 { NSLog("MenuBand cloud: \(pendingCount) waiting — not signed in (run ac-login)") }
            return
        }
        let entries = loadManifests()
        guard !entries.isEmpty else { return }
        draining = true
        defer { draining = false; postStatus() }
        retryItem?.cancel()
        for (dir, manifest) in entries {
            switch upload(dir: dir, manifest: manifest, token: token) {
            case .done:
                committed.insert(manifest.takeId)
                try? FileManager.default.removeItem(at: dir)
                backoff = 30
                NSLog("MenuBand cloud: backed up \(manifest.takeId)")
                DispatchQueue.main.async { ReadyChime.shared.playBackedUp() }
            case .unauthorized:
                // Wait for a fresh ~/.ac-token; the session watcher re-drains.
                NSLog("MenuBand cloud: session rejected — waiting for ac-login")
                return
            case .failed(let why):
                NSLog("MenuBand cloud: \(manifest.takeId) failed (\(why)); retry in \(Int(backoff))s")
                scheduleRetry()
                return
            }
        }
    }

    private func scheduleRetry() {
        let item = DispatchWorkItem { [weak self] in self?.drain() }
        retryItem = item
        queue.asyncAfter(deadline: .now() + backoff, execute: item)
        backoff = min(backoff * 2, 3600)
    }

    private func loadManifests() -> [(URL, Manifest)] {
        let dirs = (try? FileManager.default.contentsOfDirectory(
            at: root, includingPropertiesForKeys: nil)) ?? []
        return dirs.compactMap { dir -> (URL, Manifest)? in
            guard let data = try? Data(contentsOf: dir.appendingPathComponent("manifest.json")),
                  let m = try? JSONDecoder().decode(Manifest.self, from: data) else { return nil }
            return (dir, m)
        }.sorted { $0.1.recordedAt < $1.1.recordedAt }
    }

    private enum Outcome { case done, unauthorized, failed(String) }

    private func upload(dir: URL, manifest: Manifest, token: String) -> Outcome {
        let fm = FileManager.default
        let files: [[String: Any]] = manifest.files.compactMap { name in
            let path = dir.appendingPathComponent(name).path
            guard let size = (try? fm.attributesOfItem(atPath: path))?[.size] as? NSNumber else { return nil }
            return ["name": name, "bytes": size.intValue]
        }
        var begin: [String: Any] = [
            "action": "begin", "takeId": manifest.takeId, "recordedAt": manifest.recordedAt,
            "machine": manifest.machine, "duration": manifest.duration, "files": files,
        ]
        if let program = manifest.program { begin["program"] = program }
        if let bpm = manifest.bpm { begin["bpm"] = bpm }

        let (status, body) = post(begin, token: token)
        if status == 401 || status == 403 { return .unauthorized }
        guard status == 200, let code = body["code"] as? String else {
            return .failed("begin \(status) \(body["error"] ?? body["message"] ?? "")")
        }
        if body["committed"] as? Bool == true { return .done }

        for upload in body["uploads"] as? [[String: Any]] ?? [] {
            guard let name = upload["name"] as? String,
                  let urlString = upload["url"] as? String,
                  let url = URL(string: urlString) else { return .failed("bad upload entry") }
            var request = URLRequest(url: url)
            request.httpMethod = upload["method"] as? String ?? "PUT"
            for (key, value) in upload["headers"] as? [String: String] ?? [:] {
                request.setValue(value, forHTTPHeaderField: key)
            }
            if request.value(forHTTPHeaderField: "Content-Type") == nil {
                request.setValue(Self.contentTypes[name] ?? "application/octet-stream",
                                 forHTTPHeaderField: "Content-Type")
            }
            let put = send(request, fromFile: dir.appendingPathComponent(name))
            guard (200..<300).contains(put) else { return .failed("PUT \(name) \(put)") }
        }

        let (commitStatus, commitBody) = post(["action": "commit", "code": code], token: token)
        if commitStatus == 401 || commitStatus == 403 { return .unauthorized }
        guard commitStatus == 200 else {
            return .failed("commit \(commitStatus) \(commitBody["missing"] ?? commitBody["error"] ?? "")")
        }
        return .done
    }

    // MARK: - HTTP (blocking; only ever on `queue`)

    private func post(_ json: [String: Any], token: String) -> (Int, [String: Any]) {
        var request = URLRequest(url: Self.endpoint)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        request.setValue("MenuBand", forHTTPHeaderField: "User-Agent")
        request.httpBody = try? JSONSerialization.data(withJSONObject: json)
        var status = 0
        var result: [String: Any] = [:]
        let done = DispatchSemaphore(value: 0)
        session.dataTask(with: request) { data, response, _ in
            status = (response as? HTTPURLResponse)?.statusCode ?? 0
            if let data, let object = try? JSONSerialization.jsonObject(with: data) as? [String: Any] {
                result = object
            }
            done.signal()
        }.resume()
        done.wait()
        return (status, result)
    }

    /// Streams the file from disk — stems never load into memory.
    private func send(_ request: URLRequest, fromFile file: URL) -> Int {
        var status = 0
        let done = DispatchSemaphore(value: 0)
        session.uploadTask(with: request, fromFile: file) { _, response, _ in
            status = (response as? HTTPURLResponse)?.statusCode ?? 0
            done.signal()
        }.resume()
        done.wait()
        return status
    }

    // MARK: - Helpers

    private func postStatus() {
        DispatchQueue.main.async { NotificationCenter.default.post(name: Self.statusChanged, object: nil) }
    }

    private static func pngData(_ image: NSImage) -> Data? {
        guard let tiff = image.tiffRepresentation,
              let rep = NSBitmapImageRep(data: tiff) else { return nil }
        return rep.representation(using: .png, properties: [:])
    }
}
#endif
