// Model.swift — project state for ClipWizard: the track's sections
// (from struct.json), every take on disk (current + archived), and the
// picks file the assembler consumes.
import Foundation

struct StructFile: Codable {
    struct Section: Codable {
        var name: String
        var startSec: Double
        var endSec: Double
    }
    var totalSec: Double
    var sections: [Section]
}

// One entry of out/motion/shots.json — the inputs manifest the
// motion-pipeline rewrites on every run (prompt, panels, durations).
struct ShotInfo: Codable {
    var i: Int
    var name: String
    var exact: Double
    var dur: Int
    var image: String
    var endImage: String?
    var prompt: String
    var tier: String?
    var ratePerSec: Double?
}

struct Take {
    var file: String      // relative to motion dir, e.g. "archive/…v1.mp4"
    var label: String     // "current" | "v1" | …
    var url: URL
}

struct Section {
    var index: Int
    var name: String
    var start: Double
    var end: Double
    var exact: Double { end - start }
    var takes: [Take]
    var picked: String?   // relative file, nil → no take exists
}

enum ProjectError: LocalizedError {
    case missing(String)
    var errorDescription: String? {
        if case .missing(let what) = self { return "missing: \(what)" }
        return nil
    }
}

final class Project {
    let lane: String
    let slug: String
    let laneDir: URL
    let outDir: URL
    let motionDir: URL
    let archiveDir: URL
    let takesURL: URL
    let audioURL: URL
    let driverURL: URL
    let finalURL: URL
    let repoRoot: URL
    private(set) var total: Double = 0
    private(set) var sections: [Section] = []
    private(set) var shotInfo: [String: ShotInfo] = [:]   // by section name

    init(popDir: String, lane: String, slug: String) throws {
        self.lane = lane
        self.slug = slug
        let pop = URL(fileURLWithPath: popDir)
        self.repoRoot = pop.deletingLastPathComponent()
        self.laneDir = pop.appendingPathComponent(lane)
        self.outDir = laneDir.appendingPathComponent("out")
        self.motionDir = outDir.appendingPathComponent("motion")
        self.archiveDir = motionDir.appendingPathComponent("archive")
        self.takesURL = motionDir.appendingPathComponent("takes.json")
        self.audioURL = outDir.appendingPathComponent("\(slug).mp3")
        self.driverURL = laneDir.appendingPathComponent("bin/gen-motion-\(slug).mjs")
        self.finalURL = outDir.appendingPathComponent("\(slug)-motion-yt.mp4")
        let structURL = outDir.appendingPathComponent("\(slug).struct.json")
        guard FileManager.default.fileExists(atPath: structURL.path) else {
            throw ProjectError.missing(structURL.path)
        }
        guard FileManager.default.fileExists(atPath: driverURL.path) else {
            throw ProjectError.missing(driverURL.path)
        }
        try FileManager.default.createDirectory(at: archiveDir, withIntermediateDirectories: true)
        try rescan(structURL: structURL)
    }

    func rescan() { try? rescan(structURL: outDir.appendingPathComponent("\(slug).struct.json")) }

    private func rescan(structURL: URL) throws {
        let data = try Data(contentsOf: structURL)
        let st = try JSONDecoder().decode(StructFile.self, from: data)
        total = st.totalSec
        let manifestURL = motionDir.appendingPathComponent("shots.json")
        if let mData = try? Data(contentsOf: manifestURL),
           let infos = try? JSONDecoder().decode([ShotInfo].self, from: mData) {
            shotInfo = Dictionary(uniqueKeysWithValues: infos.map { ($0.name, $0) })
        }
        let picks = loadPicks()
        let archived = (try? FileManager.default.contentsOfDirectory(atPath: archiveDir.path)) ?? []
        sections = st.sections.enumerated().map { (i, s) in
            var takes: [Take] = []
            let cur = "\(slug)-shot-\(i)-\(s.name).mp4"
            let curURL = motionDir.appendingPathComponent(cur)
            if FileManager.default.fileExists(atPath: curURL.path) {
                takes.append(Take(file: cur, label: "current", url: curURL))
            }
            for f in archived.filter({ $0.hasPrefix("\(slug)-shot-\(i)-\(s.name).v") }).sorted() {
                let v = f.split(separator: ".").dropLast().last.map(String.init) ?? f
                takes.append(Take(file: "archive/\(f)", label: v,
                                  url: archiveDir.appendingPathComponent(f)))
            }
            let picked = picks[s.name] ?? takes.first?.file
            return Section(index: i, name: s.name, start: s.startSec, end: s.endSec,
                           takes: takes, picked: picked)
        }
    }

    func loadPicks() -> [String: String] {
        guard let data = try? Data(contentsOf: takesURL),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: String]
        else { return [:] }
        return obj
    }

    func pick(section: String, file: String) {
        var picks = loadPicks()
        picks[section] = file
        if let data = try? JSONSerialization.data(withJSONObject: picks, options: [.prettyPrinted, .sortedKeys]) {
            try? data.write(to: takesURL)
        }
        rescan()
    }
}
