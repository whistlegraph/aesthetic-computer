// HOOKS, WITHOUT A SHELL.
//
// `slab-menubar hook <event>` is the Claude Code hook surface reimplemented
// inside the app binary. It exists for one reason: the shell hooks in
// `slab/bin/claude-*.sh` reach for `jq` thirteen times and `python3` three
// times, and neither ships with macOS. On the studio Macs that is invisible —
// brew put them there years ago. On a stranger's Mac, a hook that shells out
// to a missing `jq` writes no marker, and a Slab with no markers has no rocks,
// no colours and no titles, which is the whole of what it is for.
//
// So the DMG build routes hooks here instead. `JSONSerialization` replaces
// `jq`, `sysctl` replaces `ps`, and the only requirement left is the app the
// user already dragged to Applications. The settings fragment written by
// `FirstRun` points at this binary by absolute path, so the hooks keep working
// no matter what is or is not on `PATH` inside a login shell.
//
// This is deliberately ONLY the state half of the shell hooks. The audio,
// the lid ambient and the sleep scheduling stay in `slab/bin` where the repo
// install can reach them, because the menubar already synthesizes its own
// status cues (see PromptStatusSound) and a downloaded Slab has no sound
// assets to play. The marker protocol below is the contract both halves
// share; if one side changes shape, the rocks stop reading.
//
// Every path exits 0. A hook that fails a turn is worse than a hook that
// quietly does nothing, and Claude Code surfaces a non-zero hook as an error
// in the middle of somebody's work.

import Foundation

enum HookCLI {
    /// Marker layout under `$SLAB_HOME` (default `~/.local/share/slab`).
    /// These four directories ARE the protocol — `StateSnapshot` reads them
    /// on a two-second timer and everything visible follows from what it finds.
    private static var stateRoot: String { Paths.slabHome }
    private static var activeDir: String { "\(stateRoot)/state/active-prompts" }
    private static var awaitingDir: String { "\(stateRoot)/state/awaiting-prompts" }
    private static var subagentDir: String { "\(stateRoot)/state/active-subagents" }
    private static var runningDir: String { "\(stateRoot)/state/running-tools" }

    /// Returns true when the arguments were a hook invocation, so `main`
    /// can exit before any menubar bootstrap happens.
    static func handleIfPresent(_ args: [String]) -> Bool {
        guard args.count >= 2, args[1] == "hook" else { return false }
        let event = args.count >= 3 ? args[2] : ""
        let payload = readPayload()

        switch event {
        case "prompt":        promptSubmitted(payload)
        case "pre":           toolPre(payload)
        case "post":          toolPost(payload)
        case "stop":          stopped(payload)
        case "subagent-stop": subagentStopped(payload)
        case "notify":        notified(payload)
        default:              break
        }
        return true
    }

    // MARK: - Events

    /// UserPromptSubmit. The turn begins: (re)write the session's marker in
    /// the `working` state and clear anything the previous turn left behind.
    private static func promptSubmitted(_ payload: [String: Any]) {
        guard let session = string(payload, "session_id"), !session.isEmpty else { return }
        mkdirs([activeDir, awaitingDir])

        let prompt = string(payload, "prompt") ?? ""
        let now = timestamp()
        // `started_at` survives across the turns of one session so the menu
        // can show how long a thread has been alive, not just this turn.
        let startedAt = existingMarker(session)?["started_at"] as? String ?? now
        let pid = claudePID()

        let marker: [String: Any] = [
            "session_id": session,
            "cwd": string(payload, "cwd") ?? "",
            "subject": String(prompt.prefix(140)),
            "summary": summarize(prompt),
            "tty": controllingTTY(of: pid) ?? "",
            "claude_pid": pid,
            "agent_pid": pid,
            "agent_type": "claude",
            "started_at": startedAt,
            "updated": now,
            "state": "working",
        ]
        write(marker, to: "\(activeDir)/\(session)")

        // The terminal's own title bar, set straight on the controlling TTY
        // with OSC 0. "Theme by status" overrides this when it is on; when it
        // is off this is the only thing naming the window.
        if let tty = marker["tty"] as? String, !tty.isEmpty {
            let summary = marker["summary"] as? String ?? ""
            if !summary.isEmpty {
                let escaped = summary.replacingOccurrences(of: "\u{07}", with: " ")
                try? "\u{1B}]0;\(escaped)\u{07}".write(
                    toFile: "/dev/\(tty)", atomically: false, encoding: .utf8)
            }
        }

        // The user answered, so nothing is awaiting and no tool is mid-flight.
        remove("\(awaitingDir)/\(session)")
        remove("\(runningDir)/\(session)")
    }

    /// PreToolUse. A `Task` opens a subagent marker; every tool refreshes the
    /// session's mtime so a long tool call never reads as a stalled thread.
    private static func toolPre(_ payload: [String: Any]) {
        guard let session = string(payload, "session_id"), !session.isEmpty else { return }
        mkdirs([runningDir])
        touch("\(activeDir)/\(session)", onlyIfPresent: true)
        touch("\(runningDir)/\(session)")

        if string(payload, "tool_name") == "Task" {
            let dir = "\(subagentDir)/\(session)"
            mkdirs([dir])
            let name = "\(Int(Date().timeIntervalSince1970))-\(getpid())-\(Int.random(in: 0..<32768))"
            FileManager.default.createFile(atPath: "\(dir)/\(name)", contents: nil)
        }
    }

    /// PostToolUse. The tool returned, so the session is thinking again.
    private static func toolPost(_ payload: [String: Any]) {
        guard let session = string(payload, "session_id"), !session.isEmpty else { return }
        touch("\(activeDir)/\(session)", onlyIfPresent: true)
        remove("\(runningDir)/\(session)")
    }

    /// SubagentStop. Pop the OLDEST marker rather than a named one: the hook
    /// payload does not say which `Task` finished, and for a count that is
    /// the only thing anyone reads, oldest-first keeps the tally honest.
    private static func subagentStopped(_ payload: [String: Any]) {
        let session = string(payload, "session_id") ?? ""
        let dir = "\(subagentDir)/\(session.isEmpty ? "_global" : session)"
        let fm = FileManager.default
        guard let names = try? fm.contentsOfDirectory(atPath: dir) else { return }
        let oldest = names
            .map { (path: "\(dir)/\($0)", date: modified("\(dir)/\($0)")) }
            .sorted { $0.date < $1.date }
            .first
        if let oldest { remove(oldest.path) }
    }

    /// Stop. The turn is over: mark the session as awaiting a human, then
    /// sweep markers whose process is gone — a session killed with its window
    /// never gets to clean up after itself, and a rock for a dead terminal is
    /// the one piece of furniture nobody can get rid of.
    private static func stopped(_ payload: [String: Any]) {
        mkdirs([activeDir, awaitingDir])
        if let session = string(payload, "session_id"), !session.isEmpty {
            try? "turn complete\n".write(
                toFile: "\(awaitingDir)/\(session)", atomically: true, encoding: .utf8)
            remove("\(runningDir)/\(session)")
        }
        reapDeadSessions()
    }

    /// Notification. Claude is asking for something; the message itself
    /// becomes the awaiting marker's contents so the menu can show it.
    private static func notified(_ payload: [String: Any]) {
        guard let session = string(payload, "session_id"), !session.isEmpty else { return }
        mkdirs([awaitingDir])
        let message = string(payload, "message") ?? "awaiting input"
        try? "\(message)\n".write(
            toFile: "\(awaitingDir)/\(session)", atomically: true, encoding: .utf8)
    }

    // MARK: - Reaping

    /// Drop any active marker whose recorded pid is no longer alive, plus any
    /// awaiting marker with no active marker left beside it.
    private static func reapDeadSessions() {
        let fm = FileManager.default
        for name in (try? fm.contentsOfDirectory(atPath: activeDir)) ?? [] {
            let path = "\(activeDir)/\(name)"
            guard let marker = json(at: path) else { continue }
            let pid = (marker["agent_pid"] as? Int) ?? (marker["claude_pid"] as? Int) ?? 0
            guard pid > 0, !isAlive(pid_t(pid)) else { continue }
            remove(path)
            remove("\(awaitingDir)/\(name)")
            remove("\(runningDir)/\(name)")
        }
        for name in (try? fm.contentsOfDirectory(atPath: awaitingDir)) ?? [] {
            if !fm.fileExists(atPath: "\(activeDir)/\(name)") {
                remove("\(awaitingDir)/\(name)")
            }
        }
    }

    // MARK: - Process inspection
    //
    // `sysctl` rather than `ps`: a hook runs on every prompt and every tool
    // call, and walking a process tree by forking `ps` eight times is eight
    // process spawns on a path that should be invisible.

    /// Walk up from this process looking for the `claude` that owns the turn,
    /// stopping at pid 1.
    ///
    /// The fallback is the IMMEDIATE parent, not the last ancestor the walk
    /// reached. Whatever pid lands in the marker is what `reapDeadSessions`
    /// later tests for liveness, and a distant ancestor is typically the
    /// terminal or `launchd` — both of which outlive the session, leaving a
    /// rock on screen that nothing can ever clear.
    private static func claudePID() -> Int {
        let own = getpid()
        let immediateParent = procInfo(own)?.parent ?? own
        var pid = own
        for _ in 0..<8 {
            guard let info = procInfo(pid) else { break }
            let parent = info.parent
            if parent <= 1 { break }
            pid = parent
            if let parentInfo = procInfo(parent),
               parentInfo.command.lowercased().contains("claude") {
                return Int(parent)
            }
        }
        return Int(immediateParent)
    }

    private static func procInfo(_ pid: pid_t) -> (parent: pid_t, command: String, tdev: dev_t)? {
        var mib: [Int32] = [CTL_KERN, KERN_PROC, KERN_PROC_PID, pid]
        var info = kinfo_proc()
        var size = MemoryLayout<kinfo_proc>.stride
        let result = sysctl(&mib, u_int(mib.count), &info, &size, nil, 0)
        guard result == 0, size > 0 else { return nil }
        let command = withUnsafePointer(to: info.kp_proc.p_comm) {
            $0.withMemoryRebound(to: CChar.self, capacity: MemoryLayout.size(ofValue: $0.pointee)) {
                String(cString: $0)
            }
        }
        return (info.kp_eproc.e_ppid, command, info.kp_eproc.e_tdev)
    }

    /// The `ttysNNN` this turn is happening in, which is the terminal window
    /// the rock has to sit on top of.
    ///
    /// Asks the named process first, then falls back to our own. A controlling
    /// terminal is inherited, so the hook process itself always knows the
    /// right answer, while an ancestor far enough up the tree may have crossed
    /// a session boundary and lost it.
    private static func controllingTTY(of pid: Int) -> String? {
        if let override = ProcessInfo.processInfo.environment["SLAB_TERMINAL_TTY"],
           !override.isEmpty { return override }
        return terminalName(of: pid_t(pid)) ?? terminalName(of: getpid())
    }

    private static func terminalName(of pid: pid_t) -> String? {
        guard let info = procInfo(pid) else { return nil }
        // NODEV (-1) means the process has no controlling terminal at all,
        // which is the normal case for an agent launched outside a window.
        guard info.tdev != -1 else { return nil }
        guard let name = devname(info.tdev, S_IFCHR) else { return nil }
        return String(cString: name)
    }

    private static func isAlive(_ pid: pid_t) -> Bool {
        // Signal 0 tests for existence without delivering anything. EPERM
        // means the process is there but owned by somebody else, which for
        // our purposes still counts as alive.
        if kill(pid, 0) == 0 { return true }
        return errno == EPERM
    }

    // MARK: - Small helpers

    private static func readPayload() -> [String: Any] {
        let data = FileHandle.standardInput.readDataToEndOfFile()
        guard !data.isEmpty,
              let parsed = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
        else { return [:] }
        return parsed
    }

    private static func string(_ payload: [String: Any], _ key: String) -> String? {
        payload[key] as? String
    }

    private static func existingMarker(_ session: String) -> [String: Any]? {
        json(at: "\(activeDir)/\(session)")
    }

    private static func json(at path: String) -> [String: Any]? {
        guard let data = FileManager.default.contents(atPath: path),
              let parsed = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
        else { return nil }
        return parsed
    }

    private static func write(_ object: [String: Any], to path: String) {
        guard let data = try? JSONSerialization.data(withJSONObject: object) else { return }
        try? data.write(to: URL(fileURLWithPath: path), options: .atomic)
    }

    private static func mkdirs(_ paths: [String]) {
        for path in paths {
            try? FileManager.default.createDirectory(
                atPath: path, withIntermediateDirectories: true)
        }
    }

    private static func remove(_ path: String) {
        try? FileManager.default.removeItem(atPath: path)
    }

    private static func touch(_ path: String, onlyIfPresent: Bool = false) {
        let fm = FileManager.default
        if fm.fileExists(atPath: path) {
            try? fm.setAttributes([.modificationDate: Date()], ofItemAtPath: path)
        } else if !onlyIfPresent {
            fm.createFile(atPath: path, contents: nil)
        }
    }

    private static func modified(_ path: String) -> Date {
        let attrs = try? FileManager.default.attributesOfItem(atPath: path)
        return (attrs?[.modificationDate] as? Date) ?? .distantPast
    }

    private static func timestamp() -> String {
        let formatter = DateFormatter()
        formatter.locale = Locale(identifier: "en_US_POSIX")
        formatter.timeZone = TimeZone(identifier: "UTC")
        formatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ss'Z'"
        return formatter.string(from: Date())
    }

    /// The window title: at most seven words and 48 characters, with an
    /// ellipsis when either limit bites. Matches `claude-prompt-log.sh`'s awk
    /// so a repo install and a downloaded one name windows identically.
    private static func summarize(_ prompt: String) -> String {
        let flattened = prompt
            .replacingOccurrences(of: "\n", with: " ")
            .replacingOccurrences(of: "\r", with: " ")
            .replacingOccurrences(of: "\t", with: " ")
        let words = flattened.split(separator: " ").map(String.init)
        guard !words.isEmpty else { return "" }
        let kept = Array(words.prefix(7))
        var out = kept.joined(separator: " ")
        if out.count > 48 {
            out = String(out.prefix(45)) + "…"
        } else if words.count > kept.count {
            out += "…"
        }
        return out
    }
}
