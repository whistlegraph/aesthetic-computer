// Model.swift — the listening library + per-track rating sidecar.
//
// JukeWizard is non-destructive: it never touches the audio. Everything
// you leave — a star rating, freeform notes, timestamped comments — is
// saved next to each track as <track>.juke.json, so ratings travel with
// the file and survive re-renders (a fresh render of the same name keeps
// its notes). The library can be loaded from a folder, an .m3u/.m3u8
// playlist (lane names picked up from "# --- lane ---" comments), or a
// list of individual files — that's the cross-lane queue.
import Foundation

struct Comment: Codable {
    var t: Double          // seconds into the track
    var text: String
}

struct JukeData: Codable {
    var stars: Int = 0
    var notes: String = ""
    var comments: [Comment] = []
    var updated: String = ""
}

// Release/status metadata for a /pop track (from pop/bin/pop-library.mjs).
struct TrackLinks: Codable { var spotify: String?; var apple: String?; var youtube: String?; var distrokid: String? }
struct MediaItem: Codable { var kind: String; var path: String }
struct TrackMeta: Codable {
    var artist: String?
    var backend: String?
    var status: String?
    var updated: String?
    var revisions: Int?
    var bytes: Int?
    var durationSec: Double?
    var bpm: Int?
    var key: String?
    var releaseDate: String?
    var art: String?
    var media: [MediaItem]?
    var links: TrackLinks?
}

final class Track {
    let url: URL
    let title: String
    let lane: String
    var data: JukeData
    var meta: TrackMeta?
    var liveStatus: String?
    var remoteKey: String?

    /// Called off the UI thread. The helper validates catalog identity and
    /// downloads atomically; the original path keeps ratings/notes stable.
    func materialize(pin: Bool = false) throws {
        if !pin && remoteKey == nil && FileManager.default.fileExists(atPath: url.path) { return }
        guard let remoteKey else {
            throw NSError(domain: "Juke", code: 1, userInfo: [NSLocalizedDescriptionKey: "Audio file is unavailable."])
        }
        let process = Process()
        process.executableURL = URL(fileURLWithPath: NSHomeDirectory() + "/.local/bin/ac-juke-cache")
        process.arguments = [pin ? "pin" : "fetch", remoteKey]
        let output = Pipe()
        process.standardOutput = output
        process.standardError = FileHandle.nullDevice
        try process.run()
        let data = output.fileHandleForReading.readDataToEndOfFile()
        process.waitUntilExit()
        let path = String(data: data, encoding: .utf8)?.trimmingCharacters(in: .whitespacesAndNewlines)
        guard process.terminationStatus == 0, path == url.path,
              FileManager.default.fileExists(atPath: url.path) else {
            throw NSError(domain: "Juke", code: 2, userInfo: [NSLocalizedDescriptionKey: "Download unavailable. Check your connection and try again."])
        }
    }
    func unpin() throws {
        guard let remoteKey else { return }
        let process = Process()
        process.executableURL = URL(fileURLWithPath: NSHomeDirectory() + "/.local/bin/ac-juke-cache")
        process.arguments = ["unpin", remoteKey]
        process.standardOutput = FileHandle.nullDevice
        process.standardError = FileHandle.nullDevice
        try process.run()
        process.waitUntilExit()
        guard process.terminationStatus == 0 else {
            throw NSError(domain: "Juke", code: 3, userInfo: [NSLocalizedDescriptionKey: "Could not remove offline pin."])
        }
    }
    // Cached so row drawing never stats the disk; refreshed by the
    // newest/oldest sorts so the displayed stamp always matches the order.
    var renderedAt: Date = .distantPast

    init(url: URL, lane: String, title: String? = nil) {
        self.url = url
        self.lane = lane
        self.title = title ?? url.deletingPathExtension().lastPathComponent
        self.data = Track.loadSidecar(for: url)
        refreshRenderedAt()
    }

    /// The moment this track's audio was last rendered: the file's own
    /// modification date, falling back to the library JSON's `updated`
    /// stamp for files that have vanished from disk.
    func refreshRenderedAt() {
        if let date = (try? FileManager.default.attributesOfItem(atPath: url.path))?[.modificationDate] as? Date {
            renderedAt = date
            return
        }
        if let stamp = meta?.updated {
            let iso = ISO8601DateFormatter()
            iso.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
            if let date = iso.date(from: stamp) { renderedAt = date; return }
            iso.formatOptions = [.withInternetDateTime]
            if let date = iso.date(from: stamp) { renderedAt = date; return }
        }
        renderedAt = .distantPast
    }

    var sidecarURL: URL { url.deletingPathExtension().appendingPathExtension("juke.json") }

    static func loadSidecar(for url: URL) -> JukeData {
        let s = url.deletingPathExtension().appendingPathExtension("juke.json")
        if let d = try? Data(contentsOf: s),
           let j = try? JSONDecoder().decode(JukeData.self, from: d) { return j }
        return JukeData()
    }

    func save() {
        data.updated = ISO8601DateFormatter().string(from: Date())
        let enc = JSONEncoder()
        enc.outputFormatting = [.prettyPrinted, .sortedKeys]
        if let d = try? enc.encode(data) {
            try? FileManager.default.createDirectory(at: sidecarURL.deletingLastPathComponent(), withIntermediateDirectories: true)
            try? d.write(to: sidecarURL, options: .atomic)
        }
    }
}

final class Library {
    private(set) var tracks: [Track] = []
    private var seen = Set<String>()
    private var remoteEntries: [String: LibEntry] = [:]
    static let audioExts: Set<String> = ["mp3", "wav", "flac", "m4a", "aac", "aiff", "aif"]

    init(inputs: [String], catalogPath: String? = nil) {
        let catalog = URL(fileURLWithPath: catalogPath ?? MenuBandJuke.defaultLibraryPath())
        if let data = try? Data(contentsOf: catalog),
           let lib = try? JSONDecoder().decode(LibFile.self, from: data) {
            for entry in lib.tracks where entry.remoteKey != nil {
                remoteEntries[URL(fileURLWithPath: entry.path).standardizedFileURL.path] = entry
            }
        }
        // Playlists may reference auditions omitted from the canonical index.
        let remoteCatalog = catalog.deletingLastPathComponent().appendingPathComponent("juke-remote.json")
        if let data = try? Data(contentsOf: remoteCatalog),
           let remote = try? JSONDecoder().decode(RemoteFile.self, from: data) {
            for entry in remote.tracks {
                let path = URL(fileURLWithPath: entry.path).standardizedFileURL.path
                if remoteEntries[path] == nil {
                    remoteEntries[path] = LibEntry(path: path, updated: entry.updated,
                                                   bytes: entry.bytes, remoteKey: entry.key)
                }
            }
        }
        for p in inputs { add(path: p) }
    }

    // Reorder the queue in place (JukeWizard's sort control).
    func reorder(by cmp: (Track, Track) -> Bool) { tracks.sort(by: cmp) }

    @discardableResult
    func addFile(_ url: URL, lane: String) -> Track? {
        let key = url.standardizedFileURL.path
        guard !seen.contains(key) else { return nil }
        guard Library.audioExts.contains(url.pathExtension.lowercased()) else { return nil }
        let remote = remoteEntries[key]
        guard FileManager.default.fileExists(atPath: url.path) || remote != nil else { return nil }
        seen.insert(key)
        let t = Track(url: url.standardizedFileURL, lane: lane, title: remote?.title)
        t.remoteKey = remote?.remoteKey
        t.meta = remote?.metadata
        t.refreshRenderedAt()
        tracks.append(t)
        return t
    }

    func add(path: String) {
        let url = URL(fileURLWithPath: (path as NSString).expandingTildeInPath)
        var isDir: ObjCBool = false
        guard FileManager.default.fileExists(atPath: url.path, isDirectory: &isDir) else {
            addFile(url, lane: url.deletingLastPathComponent().lastPathComponent)
            return
        }
        if isDir.boolValue {
            let lane = url.lastPathComponent
            let items = (try? FileManager.default.contentsOfDirectory(
                at: url, includingPropertiesForKeys: nil)) ?? []
            for f in items.sorted(by: { $0.lastPathComponent < $1.lastPathComponent }) {
                addFile(f, lane: lane)
            }
        } else if ["m3u", "m3u8"].contains(url.pathExtension.lowercased()) {
            loadPlaylist(url)
        } else if url.pathExtension.lowercased() == "json" {
            loadLibrary(url)
        } else {
            addFile(url, lane: url.deletingLastPathComponent().lastPathComponent)
        }
    }

    // a pop-library.json (from pop/bin/pop-library.mjs): tracks + status/meta.
    private struct LibEntry: Codable {
        var path: String; var title: String?; var artist: String?; var lane: String?
        var backend: String?; var status: String?; var updated: String?
        var revisions: Int?; var bytes: Int?; var durationSec: Double?
        var bpm: Int?; var key: String?; var releaseDate: String?
        var art: String?; var media: [MediaItem]?; var links: TrackLinks?
        var remoteKey: String?
        var metadata: TrackMeta {
            TrackMeta(artist: artist, backend: backend, status: status, updated: updated,
                      revisions: revisions, bytes: bytes, durationSec: durationSec,
                      bpm: bpm, key: key, releaseDate: releaseDate, art: art, media: media, links: links)
        }
    }
    private struct LibFile: Codable { var tracks: [LibEntry] }
    private struct RemoteEntry: Decodable {
        var key: String; var path: String; var bytes: Int?; var updated: String?
    }
    private struct RemoteFile: Decodable { var tracks: [RemoteEntry] }
    private func loadLibrary(_ url: URL) {
        guard let d = try? Data(contentsOf: url),
              let lib = try? JSONDecoder().decode(LibFile.self, from: d) else { return }
        for e in lib.tracks {
            let f = URL(fileURLWithPath: (e.path as NSString).expandingTildeInPath)
            let key = f.standardizedFileURL.path
            guard !seen.contains(key), FileManager.default.fileExists(atPath: f.path) || e.remoteKey != nil else { continue }
            seen.insert(key)
            let t = Track(url: f, lane: e.lane ?? f.deletingLastPathComponent().lastPathComponent, title: e.title)
            t.remoteKey = e.remoteKey
            t.meta = e.metadata
            t.refreshRenderedAt()
            tracks.append(t)
        }
    }

    private func loadPlaylist(_ url: URL) {
        guard let text = try? String(contentsOf: url, encoding: .utf8) else { return }
        let base = url.deletingLastPathComponent()
        var lane = "playlist"
        for raw in text.split(separator: "\n", omittingEmptySubsequences: false) {
            let line = raw.trimmingCharacters(in: .whitespaces)
            if line.hasPrefix("#") {
                // "# --- lullabies (…) ---" → lane label for following entries
                if line.hasPrefix("# ---") {
                    let inner = line.dropFirst(5)
                        .replacingOccurrences(of: "---", with: "")
                        .trimmingCharacters(in: .whitespaces)
                    if let head = inner.split(separator: " ").first { lane = String(head) }
                }
                continue
            }
            if line.isEmpty { continue }
            let f = line.hasPrefix("/") ? URL(fileURLWithPath: line)
                                        : base.appendingPathComponent(line)
            addFile(f, lane: lane)
        }
    }
}
