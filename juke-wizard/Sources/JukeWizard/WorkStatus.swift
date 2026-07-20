// WorkStatus.swift — read-only awareness of music work happening around JukeWizard.
// Combines Slab's fleet agent ledger with local renderer/build processes, then
// maps each activity to a /pop lane or track. Nothing here controls an agent.
import Foundation

struct WorkActivity {
    var lane: String?
    var track: String?
    var state: String
    var detail: String
    var priority: Int
}

enum WorkStatus {
    private struct LedgerFile: Decodable { var entries: [LedgerEntry] }
    private struct LedgerEntry: Decodable {
        var name: String
        var subject: String
        var status: String
        var cwd: String
        var updated: Double
        var agentType: String?
    }

    static func snapshot(tracks: [Track]) -> [WorkActivity] {
        var result = renderProcesses(tracks: tracks)
        let fm = FileManager.default
        let root = (NSHomeDirectory() as NSString).appendingPathComponent(".config/slab/ledger")
        var files = [(root as NSString).appendingPathComponent("local.json")]
        let peers = (root as NSString).appendingPathComponent("peers")
        if let names = try? fm.contentsOfDirectory(atPath: peers) {
            files += names.filter { $0.hasSuffix(".json") }.map { (peers as NSString).appendingPathComponent($0) }
        }
        let nowMS = Date().timeIntervalSince1970 * 1000
        for file in files {
            guard let data = fm.contents(atPath: file),
                  let ledger = try? JSONDecoder().decode(LedgerFile.self, from: data) else { continue }
            for entry in ledger.entries {
                guard nowMS - entry.updated < 10 * 60 * 1000,
                      !["complete", "blank", "interrupted"].contains(entry.status) else { continue }
                let haystack = "\(entry.cwd) \(entry.subject)".lowercased()
                let lane = popLane(in: entry.cwd) ?? tracks.first(where: {
                    haystack.contains($0.title.lowercased()) || haystack.contains("/\($0.lane.lowercased())")
                })?.lane
                guard lane != nil else { continue }
                let track = tracks.first(where: { $0.lane == lane && haystack.contains($0.title.lowercased()) })?.title
                let who = "\(entry.agentType ?? "agent") \(entry.name)"
                result.append(WorkActivity(lane: lane, track: track, state: "agent \(entry.status)",
                                           detail: "\(who): \(entry.subject)", priority: 1))
            }
        }
        return dedupe(result)
    }

    private static func renderProcesses(tracks: [Track]) -> [WorkActivity] {
        let p = Process()
        let pipe = Pipe()
        p.executableURL = URL(fileURLWithPath: "/bin/ps")
        p.arguments = ["-axo", "command="]
        p.standardOutput = pipe
        p.standardError = FileHandle.nullDevice
        guard (try? p.run()) != nil else { return [] }
        // Drain before waiting. A large command list can fill the pipe while
        // waitUntilExit spins the main run loop, recursively firing this poll.
        let data = pipe.fileHandleForReading.readDataToEndOfFile()
        p.waitUntilExit()
        guard let text = String(data: data, encoding: .utf8) else { return [] }
        let markers = ["render-", "render-c.mjs", "bake.mjs", "ffmpeg", "swift build", "gen-score"]
        return text.split(separator: "\n").compactMap { raw in
            let command = String(raw)
            guard markers.contains(where: command.contains), !command.contains("JukeWizard") else { return nil }
            let lane = popLane(in: command) ?? tracks.first(where: { command.lowercased().contains($0.title.lowercased()) })?.lane
            guard let lane else { return nil }
            let track = tracks.first(where: { $0.lane == lane && command.lowercased().contains($0.title.lowercased()) })?.title
            return WorkActivity(lane: lane, track: track, state: "baking",
                                detail: track.map { "rendering \($0)" } ?? "rendering \(lane)", priority: 0)
        }
    }

    private static func popLane(in text: String) -> String? {
        guard let r = text.range(of: "/pop/") else { return nil }
        let tail = text[r.upperBound...]
        let lane = tail.prefix { $0 != "/" && !$0.isWhitespace }
        return lane.isEmpty ? nil : String(lane)
    }

    private static func dedupe(_ xs: [WorkActivity]) -> [WorkActivity] {
        var seen = Set<String>()
        return xs.sorted { $0.priority < $1.priority }.filter {
            let key = "\($0.lane ?? "")|\($0.track ?? "")|\($0.state)"
            return seen.insert(key).inserted
        }
    }
}
