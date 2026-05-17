import Foundation
import Darwin

/// Per-Claude-session state. Mirrors the menubar's `ClaudeSession.State` so
/// the screensaver paints the same colors as the menubar polygon icon.
///
/// - `blank`    SessionStart fired, no prompt yet — a fresh window.
/// - `working`  Claude is actively running a turn.
/// - `complete` turn finished; alive but idle, no attention needed.
/// - `awaiting` paused mid-task, needs the user (permission / continue).
/// - `stale`    claude_pid is gone; marker not yet reaped by the menubar.
enum SlabSessionState {
    case blank, working, complete, awaiting, stale
}

struct SlabSession {
    let sessionId: String
    let claudePid: Int
    let updated: Date
    var state: SlabSessionState
}

/// Read-only snapshot of "the status of our prompts" plus a couple of slab
/// mode flags. This is a trimmed port of the menubar's `StateSnapshot` /
/// `ClaudeSessionReader`: file reads only, **no process spawning** (the
/// sandboxed screensaver host can't reliably fork) and **no mutation** (the
/// menubar owns reaping; the screensaver only observes).
struct SlabStatus {
    var sessions: [SlabSession] = []
    var activePrompts: Int = 0
    var activeSubagents: Int = 0
    var ambientActive: Bool = false
    var muted: Bool = false
    var stateDirReadable: Bool = false

    var working: Int  { sessions.filter { $0.state == .working  }.count }
    var awaiting: Int { sessions.filter { $0.state == .awaiting }.count }
    var complete: Int { sessions.filter { $0.state == .complete }.count }
    var blank: Int    { sessions.filter { $0.state == .blank    }.count }
    var stale: Int    { sessions.filter { $0.state == .stale    }.count }
    var anyAwaiting: Bool { awaiting > 0 }
    var anyLive: Bool { sessions.contains { $0.state != .stale } }

    /// One-line summary, matching the spirit of the menubar's `statusLine`.
    var statusLine: String {
        if !stateDirReadable { return "slab" }
        if anyAwaiting { return "\(awaiting) awaiting · \(sessions.count) active" }
        if ambientActive && !sessions.isEmpty { return "ambient — \(sessions.count) active" }
        if ambientActive { return "ambient" }
        if sessions.isEmpty { return "idle" }
        return "\(sessions.count) active"
    }

    private static var stateDir: String { "\(RealHome.path)/.local/share/slab/state" }

    static func gather() -> SlabStatus {
        var s = SlabStatus()
        let fm = FileManager.default
        let activeDir = "\(stateDir)/active-prompts"
        let awaitingDir = "\(stateDir)/awaiting-prompts"

        s.ambientActive = fm.fileExists(atPath: "/tmp/slab-ambient-active")
        s.muted = fm.fileExists(atPath: "\(stateDir)/muted")
        s.activeSubagents = countFiles("\(stateDir)/active-subagents")

        guard let names = try? fm.contentsOfDirectory(atPath: activeDir) else {
            // Either no work, or the sandbox blocked the read. Either way we
            // degrade gracefully to a clean idle render.
            return s
        }
        s.stateDirReadable = true

        var awaitingMsgs: [String: String] = [:]
        if let an = try? fm.contentsOfDirectory(atPath: awaitingDir) {
            for name in an where !name.hasPrefix(".") {
                let msg = (try? String(contentsOfFile: "\(awaitingDir)/\(name)", encoding: .utf8))?
                    .trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
                awaitingMsgs[name] = msg.isEmpty ? "awaiting input" : msg
            }
        }

        for name in names where !name.hasPrefix(".") {
            guard let sess = parse(path: "\(activeDir)/\(name)", fallbackId: name) else { continue }
            var v = sess
            if v.claudePid > 0 && !pidAlive(v.claudePid) {
                v.state = .stale            // observe only — never unlink
            } else if let msg = awaitingMsgs[v.sessionId] {
                v.state = msg.lowercased().hasPrefix("turn complete") ? .complete : .awaiting
            } else if v.state != .blank {
                v.state = .working
            }
            s.sessions.append(v)
        }
        s.activePrompts = s.sessions.count

        // Attention first, then calm, then fresh, then dead — same banding
        // as the menubar so the polygon edges read consistently.
        s.sessions.sort { a, b in
            func rank(_ st: SlabSessionState) -> Int {
                switch st {
                case .awaiting: return 0
                case .complete: return 1
                case .working:  return 2
                case .blank:    return 3
                case .stale:    return 4
                }
            }
            let ra = rank(a.state), rb = rank(b.state)
            return ra != rb ? ra < rb : a.updated > b.updated
        }
        return s
    }

    private static func countFiles(_ dir: String) -> Int {
        guard let c = try? FileManager.default.contentsOfDirectory(atPath: dir) else { return 0 }
        return c.filter { !$0.hasPrefix(".") }.count
    }

    private static func parse(path: String, fallbackId: String) -> SlabSession? {
        guard let data = try? Data(contentsOf: URL(fileURLWithPath: path)) else { return nil }
        let mtime = (try? FileManager.default.attributesOfItem(atPath: path)[.modificationDate] as? Date) ?? Date()
        if data.isEmpty {
            return SlabSession(sessionId: fallbackId, claudePid: 0, updated: mtime, state: .working)
        }
        guard let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else { return nil }
        let iso = ISO8601DateFormatter()
        let updated: Date = {
            if let str = obj["updated"] as? String, let d = iso.date(from: str) { return d }
            return mtime
        }()
        let state: SlabSessionState = ((obj["state"] as? String) == "blank") ? .blank : .working
        return SlabSession(
            sessionId: (obj["session_id"] as? String) ?? fallbackId,
            claudePid: (obj["claude_pid"] as? Int) ?? 0,
            updated: updated,
            state: state
        )
    }

    /// `kill(pid, 0)` is a bare syscall (no fork), so it's safe inside the
    /// screensaver sandbox. EPERM means alive-but-not-ours.
    private static func pidAlive(_ pid: Int) -> Bool {
        guard pid > 0 else { return false }
        return kill(pid_t(pid), 0) == 0 || errno == EPERM
    }
}
