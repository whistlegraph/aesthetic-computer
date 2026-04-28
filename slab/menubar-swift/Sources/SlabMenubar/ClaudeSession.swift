import Foundation

struct ClaudeSession {
    enum State { case working, awaiting, stale }

    let sessionId: String
    let cwd: String
    let subject: String
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

        var sessions: [ClaudeSession] = []
        for name in names where !name.hasPrefix(".") {
            let path = "\(activeDir)/\(name)"
            guard let session = parse(path: path, fallbackId: name) else { continue }
            var s = session
            if let msg = awaitingMessages[s.sessionId] {
                s.state = .awaiting
                s.awaitingMessage = msg
            } else if pidAlive(s.claudePid) || s.claudePid == 0 {
                s.state = .working
            } else {
                s.state = .stale
            }
            sessions.append(s)
        }

        return sessions.sorted { a, b in
            if a.state == .awaiting && b.state != .awaiting { return true }
            if a.state != .awaiting && b.state == .awaiting { return false }
            return a.updated > b.updated
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
