import Foundation

/// One live /pop render — an audio, illy, or video render publishing a
/// progress heartbeat into ~/.ac-pop-renders/ (written by
/// pop/lib/render-progress.mjs). The menu shows a temporary progress
/// bar per render and drops it the instant its heartbeat file is gone.
struct PopRender {
    var id: String
    var type: String        // "audio" | "illy" | "video"
    var label: String
    /// Claude session that launched the render (render-progress.mjs resolves
    /// it from the process tree). Lets that session's row go pink/rendering.
    var sessionId: String = ""
    var pct: Int?           // 0…100, or nil for an indeterminate render
    var done: Int?          // e.g. frame 142, panel 3
    var total: Int?         // e.g. of 240, of 11
    var startedAt: Double   // ms since epoch
    /// Resident memory of the writer pid, sampled at read time via
    /// proc_pid_rusage — driver process only, not its ffmpeg/python
    /// children (those surface in the system hogs list instead).
    var rssMB: Int? = nil
}

/// One heavyweight process for the "System" hogs submenu.
struct SystemHog {
    var pid: Int
    var rssMB: Int
    var cpu: Double
    var name: String
}

/// Whole-machine telemetry for the 8 GB Neo — answers "what's cutting
/// into RAM/CPU right now" from the menubar instead of Activity Monitor.
struct SystemStats {
    var memFreePct: Int = 0      // kern.memorystatus_level (what memory_pressure -Q prints)
    var swapUsedMB: Int = 0
    var swapTotalMB: Int = 0
    var loadAvg: Double = 0      // 1-minute load
    var hogs: [SystemHog] = []   // top processes by resident memory
    /// Red-flag threshold: the machine is about to start thrashing.
    var pressure: Bool {
        memFreePct > 0 && memFreePct < 15
            || swapTotalMB > 0 && swapUsedMB * 10 >= swapTotalMB * 9
    }
}

/// How aggressively to shrink the tile font. Far = "comfortable from
/// typical distance" (the auto-fit baseline). Near = ~60% — denser when
/// you're sitting close. Tiny = ~40% — for cramming many panes onto one
/// screen at the edge of legibility.
enum TextSize {
    case far, near, tiny
}

/// Deskflow KVM status for the menubar. All machine-specific identity
/// (role/label/agent) is read from the untracked `~/.config/slab/deskflow.json`
/// so tracked code stays generic — same convention as the iMessage bridge.
struct DeskflowState {
    var configured: Bool = false   // a deskflow.json with "enabled": true exists
    var running: Bool = false      // a deskflow-core process is alive
    var role: String = ""          // "server" | "client" (display only)
    var label: String = "Deskflow"
    var agent: String = ""         // launchd label, e.g. computer.aesthetic.deskflow
}

/// One assigned, incomplete Asana task as rendered in the menu. The URL is the
/// task's permalink so a click opens it in the browser; due dates drive a small
/// overdue/today marker.
struct AsanaTask {
    var name: String
    var url: String
    var due: String = ""       // "YYYY-MM-DD" or "" if undated
    var overdue: Bool = false
    var today: Bool = false
}

/// A project bucket holding the user's assigned tasks. Projectless tasks land
/// in a synthetic "Inbox" bucket (the helper does the grouping).
struct AsanaProject {
    var name: String
    var tasks: [AsanaTask]
}

/// Asana task state for the menubar, polled off-main via `slab/bin/asana`
/// (mirrors the iMessage/mail bridges). All identity (account, token, project
/// names) comes from Asana at runtime via the untracked config — nothing
/// personal lives in tracked code. Present-but-unconfigured machines show a
/// one-line "set up" hint instead of the task tree.
struct AsanaState {
    var configured: Bool = false
    var label: String = "Asana: —"   // menu parent title from the helper
    var user: String = ""
    var count: Int = 0
    var projects: [AsanaProject] = []
}

/// One deployed worker within an environment (e.g. `api` in staging).
struct DeployApp {
    var name: String
    var state: String       // "deployed" | "building" | "failing"
    var url: String = ""
}

/// One watched environment (e.g. staging→staging, production→main), rolled up
/// from its Workers Builds check-runs on the branch tip.
struct DeployEnv {
    var env: String
    var state: String = "none"   // deployed | building | failing | none | error
    var sha: String = ""
    var message: String = ""
    var apps: [DeployApp] = []
    var url: String = ""
}

/// Deploy status for the menubar, polled off-main via `slab/bin/deploy-status`
/// (mirrors the Asana bridge). Watched repos + token come from the untracked
/// config — nothing repo-specific lives in tracked code. Unconfigured machines
/// show a one-line "set up" hint instead of the environment tree.
struct DeployStatusState {
    var configured: Bool = false
    var label: String = "Deploy: —"   // menu parent title from the helper
    var target: String = ""
    var envs: [DeployEnv] = []
    var url: String = ""
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
    var promptSigils: Bool = false
    var forceBright: Bool = false
    var preferIterm: Bool = false
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
    /// Deskflow KVM — present only on machines with a deskflow.json; the
    /// menu shows a status line + Start/Stop/Restart for the LaunchAgent.
    var deskflow: DeskflowState = DeskflowState()
    /// Whole-machine RAM/swap/load + top hogs, refreshed each gather()
    /// tick so resource squeezes are visible the moment the menu opens.
    var system: SystemStats = SystemStats()
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
    /// Pure "resting" idle — nothing open and nothing pending. This is the
    /// state the menubar shows as the slow, colour-flowing spinning line
    /// (instead of a static glyph). messageWaiting is folded onto the
    /// snapshot by AppDelegate after gather(), so it's authoritative here.
    var idleResting: Bool {
        claudeSessions.isEmpty && !messageWaiting && !ambientActive && !hasWork
    }

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
        s.promptSigils = FileManager.default.fileExists(atPath: Paths.promptSigilsFlag)
        s.forceBright = FileManager.default.fileExists(atPath: Paths.forceBrightFlag)
        s.preferIterm = FileManager.default.fileExists(atPath: Paths.preferItermFlag)
        s.deskflow = readDeskflow()
        s.tailnetPeers = TailnetPeer.query()
        s.claudeSessions = ClaudeSessionReader.active()
        s.popRenders = readPopRenders()
        s.system = readSystemStats()
        let (rec, recPath) = readCallRecordingState()
        s.callRecording = rec
        s.callRecordingPath = recPath
        return s
    }

    /// Read Deskflow KVM status. Returns an unconfigured state on machines
    /// without a `~/.config/slab/deskflow.json` (or with "enabled": false),
    /// so the menu item only appears where Deskflow is actually wired.
    /// "running" is a cheap `pgrep` for a live deskflow-core — runs off the
    /// main tick inside gather(), alongside the existing ioreg/pmset forks.
    private static func readDeskflow() -> DeskflowState {
        var d = DeskflowState()
        guard let data = FileManager.default.contents(atPath: Paths.deskflowConfig),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              (obj["enabled"] as? Bool) ?? false
        else { return d }
        d.configured = true
        d.role = (obj["role"] as? String) ?? ""
        d.label = (obj["label"] as? String) ?? "Deskflow"
        d.agent = (obj["agent"] as? String) ?? ""
        let pids = ShellRunner.output("/usr/bin/pgrep", args: ["-f", "deskflow-core"], timeout: 2)
        d.running = !(pids?.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty ?? true)
        return d
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
                sessionId: (obj["sessionId"] as? String) ?? "",
                pct: (pctRaw is NSNull) ? nil : (pctRaw as? Int),
                done: (doneRaw is NSNull) ? nil : (doneRaw as? Int),
                total: (totalRaw is NSNull) ? nil : (totalRaw as? Int),
                startedAt: (obj["startedAt"] as? Double) ?? 0,
                rssMB: residentMB(of: pid)))
        }
        return out.sorted { $0.startedAt < $1.startedAt }
    }

    /// Resident set size of a same-user process in MB, or nil if the pid
    /// is gone / not ours. In-process syscall — no fork, safe per tick.
    private static func residentMB(of pid: Int) -> Int? {
        guard pid > 0 else { return nil }
        var usage = rusage_info_current()
        let rc = withUnsafeMutablePointer(to: &usage) { ptr -> Int32 in
            ptr.withMemoryRebound(to: (rusage_info_t?).self, capacity: 1) {
                proc_pid_rusage(pid_t(pid), RUSAGE_INFO_CURRENT, $0)
            }
        }
        guard rc == 0 else { return nil }
        return Int(usage.ri_resident_size / 1_048_576)
    }

    /// Whole-machine telemetry. The free percentage is
    /// kern.memorystatus_level — the same number `memory_pressure -Q`
    /// prints — and swap comes from vm.swapusage; both are in-process
    /// sysctls. The hogs list is one `ps` fork (memory-sorted), which is
    /// in line with the ioreg/pmset/pgrep forks gather() already pays.
    private static func readSystemStats() -> SystemStats {
        var stats = SystemStats()

        var level: Int32 = 0
        var levelSize = MemoryLayout<Int32>.size
        if sysctlbyname("kern.memorystatus_level", &level, &levelSize, nil, 0) == 0 {
            stats.memFreePct = Int(level)
        }

        var swap = xsw_usage()
        var swapSize = MemoryLayout<xsw_usage>.size
        if sysctlbyname("vm.swapusage", &swap, &swapSize, nil, 0) == 0 {
            stats.swapUsedMB = Int(swap.xsu_used / 1_048_576)
            stats.swapTotalMB = Int(swap.xsu_total / 1_048_576)
        }

        var loads = [Double](repeating: 0, count: 3)
        if getloadavg(&loads, 3) >= 1 {
            stats.loadAvg = loads[0]
        }

        if let out = ShellRunner.output(
            "/bin/ps", args: ["axm", "-o", "rss=,pcpu=,pid=,comm="], timeout: 2) {
            for line in out.split(separator: "\n").prefix(6) {
                let cols = line.trimmingCharacters(in: .whitespaces)
                    .split(separator: " ", maxSplits: 3, omittingEmptySubsequences: true)
                guard cols.count == 4,
                      let rssKB = Int(cols[0]),
                      let cpu = Double(cols[1]),
                      let pid = Int(cols[2])
                else { continue }
                let name = (String(cols[3]) as NSString).lastPathComponent
                stats.hogs.append(SystemHog(
                    pid: pid, rssMB: rssKB / 1024, cpu: cpu, name: name))
            }
        }
        return stats
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
