import Foundation

struct StateSnapshot {
    var lidClosed: Bool = false
    var sleepDisabled: Bool = false
    var activePrompts: Int = 0
    var activeSubagents: Int = 0
    var ambientActive: Bool = false
    var muted: Bool = false
    var tailnetPeers: [TailnetPeer] = []
    var claudeSessions: [ClaudeSession] = []

    var totalActive: Int { activePrompts + activeSubagents }
    var hasWork: Bool { totalActive > 0 }
    var awaitingCount: Int { claudeSessions.filter { $0.state == .awaiting }.count }
    var anyAwaiting: Bool { awaitingCount > 0 }
    var anyActive: Bool { claudeSessions.contains(where: { $0.state != .stale }) }

    var statusLine: String {
        if anyAwaiting { return "\(awaitingCount) awaiting · \(totalActive) active" }
        if ambientActive && hasWork { return "ambient — \(totalActive) active" }
        if ambientActive { return "ambient" }
        if !hasWork { return "idle" }
        if lidClosed && !sleepDisabled { return "\(totalActive) active · sleep not disabled" }
        if lidClosed { return "\(totalActive) active · lid closed" }
        return "\(totalActive) active"
    }

    static func gather() -> StateSnapshot {
        var s = StateSnapshot()
        s.lidClosed = parseLidState()
        s.sleepDisabled = parseSleepDisabled()
        s.activePrompts = countFiles(in: Paths.activePromptsDir)
        s.activeSubagents = countFiles(in: Paths.activeSubagentsDir)
        s.ambientActive = FileManager.default.fileExists(atPath: Paths.ambientFlag)
        s.muted = FileManager.default.fileExists(atPath: Paths.muteFlag)
        s.tailnetPeers = TailnetPeer.query()
        s.claudeSessions = ClaudeSessionReader.active()
        return s
    }

    private static func parseLidState() -> Bool {
        guard let out = ShellRunner.output(
            "/usr/sbin/ioreg",
            args: ["-r", "-k", "AppleClamshellState", "-d", "4"],
            timeout: 2
        ) else { return false }
        for line in out.split(separator: "\n") {
            if line.contains("AppleClamshellState") && !line.contains("Change") {
                return line.contains("Yes")
            }
        }
        return false
    }

    private static func parseSleepDisabled() -> Bool {
        guard let out = ShellRunner.output("/usr/bin/pmset", args: ["-g"], timeout: 2) else { return false }
        for rawLine in out.split(separator: "\n") {
            let line = rawLine.trimmingCharacters(in: .whitespaces)
            if line.hasPrefix("SleepDisabled") {
                return line.contains("1")
            }
        }
        return false
    }

    private static func countFiles(in dir: String) -> Int {
        guard let contents = try? FileManager.default.contentsOfDirectory(atPath: dir) else { return 0 }
        return contents.filter { !$0.hasPrefix(".") }.count
    }
}
