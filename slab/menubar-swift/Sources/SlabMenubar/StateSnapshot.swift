import Foundation

/// One live /pop render — an audio, illy, or video render publishing a
/// progress heartbeat into ~/.ac-pop-renders/ (written by
/// pop/lib/render-progress.mjs). The menu shows a temporary progress
/// bar per render and drops it the instant its heartbeat file is gone.
struct PopRender {
    var id: String
    var type: String        // "audio" | "illy" | "video"
    var label: String
    var pct: Int?           // 0…100, or nil for an indeterminate render
    var done: Int?          // e.g. frame 142, panel 3
    var total: Int?         // e.g. of 240, of 11
    var startedAt: Double   // ms since epoch
}

/// How aggressively to shrink the tile font. Far = "comfortable from
/// typical distance" (the auto-fit baseline). Near = ~60% — denser when
/// you're sitting close. Tiny = ~40% — for cramming many panes onto one
/// screen at the edge of legibility.
enum TextSize {
    case far, near, tiny
}

struct StateSnapshot {
    var lidClosed: Bool = false
    var sleepDisabled: Bool = false
    var activePrompts: Int = 0
    var activeSubagents: Int = 0
    var ambientActive: Bool = false
    var muted: Bool = false
    var autoTile: Bool = false
    var textSize: TextSize = .far
    var themeByStatus: Bool = false
    var forceBright: Bool = false
    var tailnetPeers: [TailnetPeer] = []
    var claudeSessions: [ClaudeSession] = []
    /// Live /pop renders with progress heartbeats — one temporary
    /// progress bar each in the menu (audio / illy / video).
    var popRenders: [PopRender] = []
    /// The configured iMessage contact has unread inbound AND theme-by-status
    /// is on. Set by AppDelegate (not gather()) from the imsg poll — the
    /// whole status surface (polygon icon + themed terminals) then carries a
    /// shared "she texted" accent until the thread is read.
    var messageWaiting: Bool = false
    /// True when slab-call-record is actively capturing a meeting WAV.
    /// Populated from ~/.ac-meeting-recording.json — the recorder script
    /// owns the state file; the menubar only reads it.
    var callRecording: Bool = false
    /// Path to the active recording WAV when callRecording is true.
    /// Empty otherwise.
    var callRecordingPath: String = ""

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
        s.autoTile = FileManager.default.fileExists(atPath: Paths.autoTileFlag)
        // Tiny wins over near if both flags somehow coexist on disk —
        // matches the menu handlers, which only ever leave one set.
        if FileManager.default.fileExists(atPath: Paths.tinyTextFlag) {
            s.textSize = .tiny
        } else if FileManager.default.fileExists(atPath: Paths.nearTextFlag) {
            s.textSize = .near
        } else {
            s.textSize = .far
        }
        s.themeByStatus = FileManager.default.fileExists(atPath: Paths.themeByStatusFlag)
        s.forceBright = FileManager.default.fileExists(atPath: Paths.forceBrightFlag)
        s.tailnetPeers = TailnetPeer.query()
        s.claudeSessions = ClaudeSessionReader.active()
        s.popRenders = readPopRenders()
        let (rec, recPath) = readCallRecordingState()
        s.callRecording = rec
        s.callRecordingPath = recPath
        return s
    }

    /// Inspect ~/.ac-meeting-recording.json. The file exists only while
    /// slab-call-record has a live ffmpeg subprocess; we additionally
    /// verify the PID is alive (kill -0) so a stale file from a crashed
    /// recorder doesn't keep the menu pinned to "recording" forever.
    private static func readCallRecordingState() -> (Bool, String) {
        let path = Paths.meetingRecordingState
        guard FileManager.default.fileExists(atPath: path),
              let data = try? Data(contentsOf: URL(fileURLWithPath: path)),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
        else { return (false, "") }
        let pid = (obj["pid"] as? Int) ?? 0
        if pid > 0 && kill(pid_t(pid), 0) != 0 && errno == ESRCH {
            // Recorder died — clean up so we don't lie to the user.
            try? FileManager.default.removeItem(atPath: path)
            return (false, "")
        }
        let wav = (obj["wav"] as? String) ?? ""
        return (true, wav)
    }

    /// Read the /pop render progress heartbeats from ~/.ac-pop-renders/.
    /// Cheap — a small dir of tiny JSON files — and gather() already
    /// runs off the main tick. Sweeps stale files (writer pid gone, or
    /// heartbeat older than 120 s) so a crashed render leaves no ghost.
    ///
    /// Internal (not private) so AppDelegate can do a fast pop-only
    /// re-poll while the menu is open without paying for a full gather().
    static func readPopRenders() -> [PopRender] {
        let dir = FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent(".ac-pop-renders")
        guard let entries = try? FileManager.default.contentsOfDirectory(
            at: dir, includingPropertiesForKeys: nil) else { return [] }
        let now = Date().timeIntervalSince1970 * 1000
        var out: [PopRender] = []
        for url in entries where url.pathExtension == "json" {
            guard let data = try? Data(contentsOf: url),
                  let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
            else { continue }
            let updatedAt = (obj["updatedAt"] as? Double) ?? 0
            let pid = (obj["pid"] as? Int) ?? 0
            let dead = pid > 0 && kill(pid_t(pid), 0) != 0 && errno == ESRCH
            if now - updatedAt > 120_000 || dead {
                try? FileManager.default.removeItem(at: url)
                continue
            }
            let pctRaw = obj["pct"]
            let doneRaw = obj["done"]
            let totalRaw = obj["total"]
            out.append(PopRender(
                id: (obj["id"] as? String) ?? url.lastPathComponent,
                type: (obj["type"] as? String) ?? "render",
                label: (obj["label"] as? String) ?? "",
                pct: (pctRaw is NSNull) ? nil : (pctRaw as? Int),
                done: (doneRaw is NSNull) ? nil : (doneRaw as? Int),
                total: (totalRaw is NSNull) ? nil : (totalRaw as? Int),
                startedAt: (obj["startedAt"] as? Double) ?? 0))
        }
        return out.sorted { $0.startedAt < $1.startedAt }
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
