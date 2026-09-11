import Foundation

/// Generic alias — a tracked agent session is no longer Claude-specific
/// (Codex and Easel sessions flow through the same markers + reducer).
/// New code should
/// prefer `AgentSession`; the old name stays valid so existing call sites
/// (overlay, snapshot, menu) keep compiling.
typealias AgentSession = ClaudeSession

struct ClaudeSession {
    /// `blank`    — SessionStart fired but no UserPromptSubmit yet; window
    ///              is open with the welcome screen, awaiting the first input.
    ///              Painted in the macOS appearance color (pure white/black) so
    ///              fresh windows read as "blank pages" until you type.
    /// `working`  — Claude is actively running a turn.
    /// `complete` — Stop fired ("turn complete"); session is alive but idle,
    ///              waiting for the next user prompt. Calm, no attention needed.
    /// `awaiting` — Notification fired; Claude paused mid-task and needs the
    ///              user (permission prompt, idle-on-input). Attention!
    /// `interrupted` — the marker says `working` but no tool is mid-flight and
    ///              there's been no hook activity for a while: Claude was
    ///              interrupted (Esc) and is idle at the prompt. Claude Code
    ///              fires NO hook on interrupt, so we infer it from staleness
    ///              + the running-tool heartbeat (a long real tool keeps the
    ///              marker fresh and stays `working`/green).
    /// `stale`    — claude_pid is gone; about to be reaped.
    /// `rendering` — the turn is done but a long render this session launched
    ///              (a ~/.ac-pop-renders heartbeat carrying its sessionId) is
    ///              still running. Pink — between working-green and
    ///              awaiting-amber: not idle, the machine is cooking.
    enum State { case blank, working, rendering, complete, awaiting, interrupted, stale }

    /// Seconds of hook silence (no tool start/stop, no prompt) before a
    /// `working` session is treated as interrupted. Generous so normal
    /// between-tool thinking never trips it.
    static let interruptIdleSeconds: TimeInterval = 45
    /// A running-tool marker older than this is assumed orphaned (the tool was
    /// interrupted mid-flight, so PostToolUse never fired) and no longer pins
    /// the session green. Far beyond any real single tool call.
    static let runningToolMaxAge: TimeInterval = 900

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
    /// When this prox first appeared on this host. Unlike `updated`, this does
    /// not advance on prompts/tool heartbeats, so hover copy can say how long
    /// the actual prompt window has been alive.
    var started: Date = Date()
    /// Native Claude/Codex transcript, when the launcher/hook supplied it.
    /// Prox memoir inference reads a bounded tail off-main; the UI never opens
    /// this file itself.
    var transcriptPath: String = ""
    var state: State
    var awaitingMessage: String?
    /// Absolute path to this session's iTerm2 background-image wallpaper,
    /// resolved off-main during refresh (instant cache probe; empty until
    /// the async generator has produced one). Empty → leave bg image unset.
    var wallpaper: String = ""
    /// Sticky per-session emoji (see TitleEmoji) prefixed to the window
    /// title and menu row so the eye can re-find a session after the tiler
    /// shuffles the grid. Stamped during refresh; "" until the first prompt.
    var emoji: String = ""
    /// Number of subagents currently in-flight under this session — both
    /// `Task`-tool agents (per-session markers from `claude-tool-pre.sh`) and
    /// live Workflow-tool agents (counted from the workflow journals). Drawn
    /// as one dot per subagent along this session's polygon edge.
    var subagentCount: Int = 0
    /// Non-empty when this session actually lives on a remote box (e.g.
    /// jasellite) and is only mirrored here by the remote-claude bridge,
    /// which copies the marker's `remote_host`. Drives the 🛰 menu badge so a
    /// remote session reads distinctly from a local one.
    var remoteHost: String = ""

    /// Which interface owns this session — "claude" (default), "codex", or
    /// "easel".
    /// Read from the marker's `agent_type`; drives per-agent labels/tooltips
    /// in the menu. The state/color engine is agent-agnostic, so this is
    /// display-only.
    var agentType: String = "claude"

    /// Easel owns its UI while using Codex app-server as its current
    /// provider bridge. Provider-thread operations therefore share Codex's
    /// transcript and resume path without presenting the Codex UI.
    var isCodexBacked: Bool {
        agentType == "codex" || agentType == "easel"
    }

    /// Optional hardware/platform destination for this prompt (for example
    /// `xbox`). The active-prompt marker owns this metadata so the
    /// platform-target-awareness-identifier-badge follows the session across
    /// prompt changes, titles, and terminal retiling without name-based rules.
    var platformTarget: String = ""

    /// Where a phone can reach the piece this session is holding, as a bare
    /// host+path (`aesthetic.computer/prompt~…`) with no scheme. Only
    /// Easel mints one, and only once its piece exists, so an empty
    /// string is the ordinary case and means "no scannable destination yet".
    /// The prompt rock turns this into a QR surface; everything else ignores it.
    var scanURL: String = ""

    /// The piece this session is writing, with its extension — `balozo.mjs`.
    /// Only Easel sets it, and it names the rock: a session that is
    /// holding a piece should be addressable by that piece's name rather than
    /// by a second unrelated word drawn from its session id.
    var piece: String = ""

    /// Native provider thread id. Claude uses `sessionId`; Codex's tracked
    /// wrapper has its own rock id, so the watcher records the rollout id here.
    var providerSessionId: String = ""
    var loopboyContact: String = ""
    var loopboyState: String = ""
    var loopboyResponse: String = ""
    var nudgeScreen: String = ""

    /// True for sessions running on another machine, surfaced via the bridge.
    var isRemote: Bool { !remoteHost.isEmpty }

    /// Human label for the owning interface.
    var agentLabel: String {
        if agentType == "easel" { return "Easel" }
        return agentType.isEmpty
            ? "Claude"
            : agentType.prefix(1).uppercased() + agentType.dropFirst()
    }

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
            } else if s.state == .blank {
                // Marker says blank (SessionStart wrote it, no UserPromptSubmit
                // has overwritten it yet) — preserve so applyTerminalDecor
                // paints the appearance-matched bg.
                // (no-op: keep s.state == .blank)
            } else if s.agentType == "easel",
                      s.state == .complete || s.state == .awaiting || s.state == .interrupted {
                // Easel receives app-server lifecycle events
                // directly, so its marker can state this transition without
                // waiting for hook-derived side channels.
            } else if isInterrupted(sessionId: s.sessionId, markerPath: path) {
                // No awaiting marker, but no tool is running and the session
                // has gone quiet → interrupted (Esc) and idle. NOT green.
                s.state = .interrupted
            } else {
                s.state = .working
            }
            s.subagentCount = subagentCount(for: s.sessionId)
            sessions.append(s)
            liveIds.insert(s.sessionId)
        }

        reapOrphanAwaiting(awaitingDir: awaitingDir, validIds: liveIds)

        // Sort: attention-needed first (awaiting), then complete, then
        // working, then blank (fresh window, nothing to look at), all
        // most-recent first within each band.
        return sessions.sorted { a, b in
            func rank(_ st: ClaudeSession.State) -> Int {
                switch st {
                case .awaiting:    return 0
                case .interrupted: return 1
                case .complete:    return 2
                case .rendering:   return 3   // busy, nothing to read yet
                case .working:     return 4
                case .blank:       return 5
                case .stale:       return 6
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

    /// In-flight subagents for a session: per-session `Task` markers plus live
    /// Workflow-tool agents. Cheap (a couple of directory reads + small journal
    /// scans); runs off-main during the snapshot refresh.
    static func subagentCount(for sessionId: String) -> Int {
        guard !sessionId.isEmpty else { return 0 }
        let fm = FileManager.default
        var count = 0
        // Task-tool agents: one marker file per agent under the session subdir.
        let taskDir = "\(Paths.activeSubagentsDir)/\(sessionId)"
        if let files = try? fm.contentsOfDirectory(atPath: taskDir) {
            count += files.lazy.filter { !$0.hasPrefix(".") }.count
        }
        count += activeWorkflowAgents(for: sessionId)
        return count
    }

    /// Workflow-tool agents don't fire the PreToolUse hook, so count them live
    /// from each workflow run's journal: `started` events minus `result`
    /// events = agents currently running.
    private static func activeWorkflowAgents(for sessionId: String) -> Int {
        let fm = FileManager.default
        let projectsDir = "\(NSHomeDirectory())/.claude/projects"
        guard let projects = try? fm.contentsOfDirectory(atPath: projectsDir) else { return 0 }
        var active = 0
        for proj in projects where !proj.hasPrefix(".") {
            let wfDir = "\(projectsDir)/\(proj)/\(sessionId)/subagents/workflows"
            guard let runs = try? fm.contentsOfDirectory(atPath: wfDir) else { continue }
            for run in runs where run.hasPrefix("wf_") {
                let journal = "\(wfDir)/\(run)/journal.jsonl"
                guard let text = try? String(contentsOfFile: journal, encoding: .utf8) else { continue }
                let started = text.components(separatedBy: "\"type\":\"started\"").count - 1
                let done = text.components(separatedBy: "\"type\":\"result\"").count - 1
                if started > done { active += started - done }
            }
        }
        return active
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
        let attrs = try? FileManager.default.attributesOfItem(atPath: path)
        let started: Date = {
            if let s = obj["started_at"] as? String, let d = formatter.date(from: s) { return d }
            return (attrs?[.creationDate] as? Date) ?? updated
        }()

        // Hook-backed clients still derive most state from the side-channel
        // markers. Easel receives direct app-server lifecycle events
        // and can publish exact idle/interrupted state itself.
        let agentType = (obj["agent_type"] as? String) ?? "claude"
        let parsedState: ClaudeSession.State = {
            switch obj["state"] as? String {
            case "blank": return .blank
            case "complete" where agentType == "easel": return .complete
            case "awaiting" where agentType == "easel": return .awaiting
            case "interrupted" where agentType == "easel": return .interrupted
            default: return .working
            }
        }()

        var session = ClaudeSession(
            sessionId: (obj["session_id"] as? String) ?? fallbackId,
            cwd: (obj["cwd"] as? String) ?? "",
            subject: (obj["subject"] as? String) ?? "(no subject)",
            summary: (obj["summary"] as? String) ?? "",
            tty: (obj["tty"] as? String) ?? "",
            // Prefer the generic `agent_pid`; fall back to the legacy
            // `claude_pid` so existing Claude markers keep reaping.
            claudePid: (obj["agent_pid"] as? Int) ?? (obj["claude_pid"] as? Int) ?? 0,
            updated: updated,
            state: parsedState,
            awaitingMessage: nil
        )
        session.remoteHost = (obj["remote_host"] as? String) ?? ""
        session.agentType = agentType
        session.providerSessionId = (obj["provider_session_id"] as? String)
            ?? (obj["codex_session_id"] as? String)
            ?? (session.agentType == "claude" ? session.sessionId : "")
        session.started = started
        session.loopboyContact = (obj["loopboy_contact"] as? String) ?? ""
        session.loopboyState = (obj["loopboy_state"] as? String) ?? ""
        session.loopboyResponse = (obj["loopboy_response"] as? String) ?? ""
        session.nudgeScreen = (obj["nudge_screen"] as? String) ?? ""
        session.scanURL = (obj["scan_url"] as? String) ?? ""
        session.piece = (obj["piece"] as? String) ?? ""
        return session
    }

    private static func pidAlive(_ pid: Int) -> Bool {
        guard pid > 0 else { return false }
        return kill(pid_t(pid), 0) == 0 || errno == EPERM
    }

    private static func mtime(_ path: String) -> Date? {
        try? FileManager.default.attributesOfItem(atPath: path)[.modificationDate] as? Date
    }

    /// A `working` session is INTERRUPTED when no tool is actively running and
    /// the active-prompt marker hasn't been touched (by a tool heartbeat) for
    /// a while. The tool hooks (`claude-tool-heartbeat.sh`) `touch` the marker
    /// on every PreToolUse/PostToolUse and keep a `running-tools/<sid>` flag
    /// for the duration of a call — so a long legit tool stays green, but an
    /// Esc'd, idle session flips out of green even though Claude fires no
    /// interrupt hook.
    private static func isInterrupted(sessionId: String, markerPath: String) -> Bool {
        let now = Date()
        // A fresh running-tool flag means a tool is genuinely mid-flight.
        let toolPath = "\(Paths.runningToolsDir)/\(sessionId)"
        if let tm = mtime(toolPath), now.timeIntervalSince(tm) < ClaudeSession.runningToolMaxAge {
            return false
        }
        // Otherwise: idle if the marker (bumped by every tool hook) is stale.
        guard let mm = mtime(markerPath) else { return false }
        return now.timeIntervalSince(mm) > ClaudeSession.interruptIdleSeconds
    }
}
