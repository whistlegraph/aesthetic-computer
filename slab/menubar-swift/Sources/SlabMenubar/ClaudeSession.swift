import Foundation

struct ClaudeSession {
    /// `working`  — Claude is actively running a turn.
    /// `complete` — Stop fired ("turn complete"); session is alive but idle,
    ///              waiting for the next user prompt. Calm, no attention needed.
    /// `awaiting` — Notification fired; Claude paused mid-task and needs the
    ///              user (permission prompt, idle-on-input). Attention!
    /// `stale`    — claude_pid is gone; about to be reaped.
    enum State { case working, complete, awaiting, stale }

    let sessionId: String
    let cwd: String
    let subject: String
    /// 4–8 word topic summary written by `claude-prompt-log.sh` for use as
    /// a live Terminal title. Empty for older active-prompts files; callers
    /// fall back to `shortSubject`.
    let summary: String
    let tty: String
    let claudePid: Int
    let updated: Date
    var state: State
    var awaitingMessage: String?

    var shortSubject: String {
        let trimmed = subject.trimmingCharacters(in: .whitespacesAndNewlines)
        if trimmed.count <= 60 { return trimmed }
        let idx = trimmed.index(trimmed.startIndex, offsetBy: 57)
        return trimmed[..<idx] + "…"
    }

    /// Prefer the hook's pre-truncated summary; fall back to the longer
    /// subject when the hook hasn't populated it yet.
    var titleString: String {
        let s = summary.trimmingCharacters(in: .whitespacesAndNewlines)
        return s.isEmpty ? shortSubject : s
    }

    var cwdLabel: String {
        let url = URL(fileURLWithPath: cwd)
        return url.lastPathComponent.isEmpty ? cwd : url.lastPathComponent
    }
}

enum ClaudeSessionReader {
    static func active() -> [ClaudeSession] {
        let fm = FileManager.default
        let activeDir = Paths.activePromptsDir
        let awaitingDir = Paths.awaitingPromptsDir

        guard let names = try? fm.contentsOfDirectory(atPath: activeDir) else {
            // No active dir means no awaiting can be valid either — sweep
            // any orphan awaiting files so they don't surface later.
            reapOrphanAwaiting(awaitingDir: awaitingDir, validIds: [])
            return []
        }

        var awaitingMessages: [String: String] = [:]
        if let awaitingNames = try? fm.contentsOfDirectory(atPath: awaitingDir) {
            for name in awaitingNames where !name.hasPrefix(".") {
                let path = "\(awaitingDir)/\(name)"
                let msg = (try? String(contentsOfFile: path, encoding: .utf8))?
                    .trimmingCharacters(in: .whitespacesAndNewlines)
                awaitingMessages[name] = (msg?.isEmpty == false ? msg : "awaiting input")
            }
        }

        // Reap markers whose claude_pid is observably dead (terminal closed
        // mid-session, no Stop hook fired). Pid liveness is checked BEFORE
        // the awaiting branch so a closed tab carrying a stale awaiting
        // marker doesn't keep pulsing red until another session's Stop hook
        // gets around to running the janitor.
        var sessions: [ClaudeSession] = []
        var liveIds: Set<String> = []
        for name in names where !name.hasPrefix(".") {
            let path = "\(activeDir)/\(name)"
            guard let session = parse(path: path, fallbackId: name) else {
                // Unparseable file with a real name: leave it alone — could
                // be an in-flight write from claude-prompt-log.sh.
                continue
            }
            var s = session

            if s.claudePid > 0 && !pidAlive(s.claudePid) {
                // Dead pid → reap active marker and any matching awaiting
                // marker. Best-effort: ignore unlink errors (concurrent
                // janitor in claude-stop.sh may have beaten us here).
                try? fm.removeItem(atPath: path)
                try? fm.removeItem(atPath: "\(awaitingDir)/\(s.sessionId)")
                continue
            }

            if let msg = awaitingMessages[s.sessionId] {
                // claude-stop.sh writes the literal string "turn complete"
                // (idle-after-turn). claude-notify.sh writes the actual
                // permission/notification message. Distinguish so the user
                // sees a calm cue for "done with the turn" vs a louder cue
                // for "i need you to continue".
                if msg.lowercased().hasPrefix("turn complete") {
                    s.state = .complete
                } else {
                    s.state = .awaiting
                }
                s.awaitingMessage = msg
            } else {
                s.state = .working
            }
            sessions.append(s)
            liveIds.insert(s.sessionId)
        }

        reapOrphanAwaiting(awaitingDir: awaitingDir, validIds: liveIds)

        // Sort: attention-needed first (awaiting), then complete, then
        // working, all most-recent first within each band.
        return sessions.sorted { a, b in
            func rank(_ st: ClaudeSession.State) -> Int {
                switch st {
                case .awaiting: return 0
                case .complete: return 1
                case .working:  return 2
                case .stale:    return 3
                }
            }
            let ra = rank(a.state), rb = rank(b.state)
            if ra != rb { return ra < rb }
            return a.updated > b.updated
        }
    }

    /// Drop awaiting-prompts entries that no longer have a live active
    /// counterpart. Mirrors the orphan sweep in claude-stop.sh so the
    /// menubar can self-heal without waiting on another session's Stop.
    private static func reapOrphanAwaiting(awaitingDir: String, validIds: Set<String>) {
        let fm = FileManager.default
        guard let names = try? fm.contentsOfDirectory(atPath: awaitingDir) else { return }
        for name in names where !name.hasPrefix(".") {
            if validIds.contains(name) { continue }
            try? fm.removeItem(atPath: "\(awaitingDir)/\(name)")
        }
    }

    private static func parse(path: String, fallbackId: String) -> ClaudeSession? {
        guard let data = try? Data(contentsOf: URL(fileURLWithPath: path)) else { return nil }

        // Empty marker: synthesize a minimal session record.
        if data.isEmpty {
            let mtime = (try? FileManager.default.attributesOfItem(atPath: path)[.modificationDate] as? Date) ?? Date()
            return ClaudeSession(
                sessionId: fallbackId,
                cwd: "",
                subject: "(no subject)",
                summary: "",
                tty: "",
                claudePid: 0,
                updated: mtime,
                state: .working,
                awaitingMessage: nil
            )
        }

        guard let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else {
            return nil
        }

        let formatter = ISO8601DateFormatter()
        let updated: Date = {
            if let s = obj["updated"] as? String, let d = formatter.date(from: s) { return d }
            return (try? FileManager.default.attributesOfItem(atPath: path)[.modificationDate] as? Date) ?? Date()
        }()

        return ClaudeSession(
            sessionId: (obj["session_id"] as? String) ?? fallbackId,
            cwd: (obj["cwd"] as? String) ?? "",
            subject: (obj["subject"] as? String) ?? "(no subject)",
            summary: (obj["summary"] as? String) ?? "",
            tty: (obj["tty"] as? String) ?? "",
            claudePid: (obj["claude_pid"] as? Int) ?? 0,
            updated: updated,
            state: .working,
            awaitingMessage: nil
        )
    }

    private static func pidAlive(_ pid: Int) -> Bool {
        guard pid > 0 else { return false }
        return kill(pid_t(pid), 0) == 0 || errno == EPERM
    }
}
