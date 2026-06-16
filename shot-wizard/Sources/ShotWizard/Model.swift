// Model.swift — the storyboard: a board.json with an ordered list of
// shots. Reordering and status live in the file; the controller saves
// back on every reorder. Paths resolve relative to the board's dir.
import Foundation

enum Lane: String, Codable {
    case GEN   // Seedance / generated motion
    case CAP   // screen capture
    case CARD  // designed title/number card
    case VO    // pure voiceover beat (no picture of its own)

    var glyph: String {
        switch self {
        case .GEN: return "✦"
        case .CAP: return "▣"
        case .CARD: return "▤"
        case .VO: return "♪"
        }
    }
}

struct Shot: Codable {
    var id: String
    var t0: Double
    var t1: Double
    var lane: Lane
    var vo: String?
    var source: String?    // still that seeds a GEN shot, or the CARD/CAP image
    var prompt: String?    // motion prompt (GEN) or note
    var clip: String?      // rendered mp4 for this shot
    var card: String?      // rendered still (png) for a CARD/CAP shot
    var status: String?    // "done" | "pending" | "wip"

    var dur: Double { max(0, t1 - t0) }
    var effectiveStatus: String {
        if let s = status { return s }
        return (clip != nil || card != nil) ? "done" : "pending"
    }
}

struct BoardFile: Codable {
    var title: String?
    var width: Int?
    var height: Int?
    var fps: Int?
    var voAudio: String?
    var driver: String?
    var shots: [Shot]
}

enum BoardError: LocalizedError {
    case missing(String)
    var errorDescription: String? {
        if case .missing(let what) = self { return "missing: \(what)" }
        return nil
    }
}

final class Board {
    let boardURL: URL
    let baseDir: URL
    var title: String
    var width: Int
    var height: Int
    var fps: Int
    var voAudioRel: String?
    var driverRel: String?
    private(set) var shots: [Shot]

    init(boardPath: String) throws {
        let url = URL(fileURLWithPath: boardPath)
        guard FileManager.default.fileExists(atPath: url.path) else {
            throw BoardError.missing(url.path)
        }
        self.boardURL = url
        self.baseDir = url.deletingLastPathComponent()
        let file = try JSONDecoder().decode(BoardFile.self, from: try Data(contentsOf: url))
        self.title = file.title ?? url.deletingPathExtension().lastPathComponent
        self.width = file.width ?? 1920
        self.height = file.height ?? 1080
        self.fps = file.fps ?? 30
        self.voAudioRel = file.voAudio
        self.driverRel = file.driver
        self.shots = file.shots
    }

    // Resolve a board-relative path to an absolute URL (nil → no path).
    func resolve(_ rel: String?) -> URL? {
        guard let rel, !rel.isEmpty else { return nil }
        if rel.hasPrefix("/") { return URL(fileURLWithPath: rel) }
        return baseDir.appendingPathComponent(rel).standardizedFileURL
    }

    var voAudioURL: URL? { resolve(voAudioRel) }
    var driverURL: URL? { resolve(driverRel) }

    /// The best preview asset for a shot: its rendered clip, else its card/still.
    func previewURL(_ shot: Shot) -> URL? {
        if let c = shot.clip, let u = resolve(c), FileManager.default.fileExists(atPath: u.path) { return u }
        if let c = shot.card, let u = resolve(c), FileManager.default.fileExists(atPath: u.path) { return u }
        return nil
    }

    func sourceURL(_ shot: Shot) -> URL? {
        guard let u = resolve(shot.source), FileManager.default.fileExists(atPath: u.path) else { return nil }
        return u
    }

    func move(from: Int, to: Int) {
        guard shots.indices.contains(from), to >= 0, to < shots.count, from != to else { return }
        let s = shots.remove(at: from)
        shots.insert(s, at: to)
        save()
    }

    func setStatus(_ index: Int, _ status: String) {
        guard shots.indices.contains(index) else { return }
        shots[index].status = status
        save()
    }

    func setPrompt(_ index: Int, _ prompt: String) {
        guard shots.indices.contains(index) else { return }
        shots[index].prompt = prompt
        save()
    }

    func reload() {
        guard let file = try? JSONDecoder().decode(BoardFile.self, from: Data(contentsOf: boardURL)) else { return }
        shots = file.shots
    }

    func save() {
        let file = BoardFile(title: title, width: width, height: height, fps: fps,
                             voAudio: voAudioRel, driver: driverRel, shots: shots)
        let enc = JSONEncoder()
        enc.outputFormatting = [.prettyPrinted, .withoutEscapingSlashes]
        if let data = try? enc.encode(file) { try? data.write(to: boardURL) }
    }
}
