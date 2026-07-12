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

    init(url: URL, lane: String, title: String? = nil) {
        self.url = url
        self.lane = lane
        self.title = title ?? url.deletingPathExtension().lastPathComponent
        self.data = Track.loadSidecar(for: url)
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
        if let d = try? enc.encode(data) { try? d.write(to: sidecarURL) }
    }
}

final class Library {
    private(set) var tracks: [Track] = []
    private var seen = Set<String>()
    static let audioExts: Set<String> = ["mp3", "wav", "flac", "m4a", "aac", "aiff", "aif"]

    init(inputs: [String]) { for p in inputs { add(path: p) } }

    // Reorder the queue in place (JukeWizard's sort control).
    func reorder(by cmp: (Track, Track) -> Bool) { tracks.sort(by: cmp) }

    @discardableResult
    func addFile(_ url: URL, lane: String) -> Track? {
        let key = url.standardizedFileURL.path
        guard !seen.contains(key) else { return nil }
        guard Library.audioExts.contains(url.pathExtension.lowercased()) else { return nil }
        guard FileManager.default.fileExists(atPath: url.path) else { return nil }
        seen.insert(key)
        let t = Track(url: url, lane: lane)
        tracks.append(t)
        return t
    }

    func add(path: String) {
        let url = URL(fileURLWithPath: (path as NSString).expandingTildeInPath)
        var isDir: ObjCBool = false
        guard FileManager.default.fileExists(atPath: url.path, isDirectory: &isDir) else { return }
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
        var path: String; var title: String?; var lane: String?
        var backend: String?; var status: String?; var updated: String?
        var revisions: Int?; var bytes: Int?; var durationSec: Double?
        var bpm: Int?; var key: String?; var releaseDate: String?
        var art: String?; var media: [MediaItem]?; var links: TrackLinks?
    }
    private struct LibFile: Codable { var tracks: [LibEntry] }
    private func loadLibrary(_ url: URL) {
        guard let d = try? Data(contentsOf: url),
              let lib = try? JSONDecoder().decode(LibFile.self, from: d) else { return }
        for e in lib.tracks {
            let f = URL(fileURLWithPath: (e.path as NSString).expandingTildeInPath)
            let key = f.standardizedFileURL.path
            guard !seen.contains(key), FileManager.default.fileExists(atPath: f.path) else { continue }
            seen.insert(key)
            let t = Track(url: f, lane: e.lane ?? f.deletingLastPathComponent().lastPathComponent, title: e.title)
            t.meta = TrackMeta(backend: e.backend, status: e.status, updated: e.updated,
                               revisions: e.revisions, bytes: e.bytes, durationSec: e.durationSec,
                               bpm: e.bpm, key: e.key, releaseDate: e.releaseDate,
                               art: e.art, media: e.media, links: e.links)
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
