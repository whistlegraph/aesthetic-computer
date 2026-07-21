import Darwin
import Foundation

/// Persistent description of one prompt that has been parked with `zzz`.
/// The provider thread survives; its process sleeps while the terminal remains
/// open with a colored, copyable resume receipt.
struct ZzzEntry: Codable {
    let version: Int
    let id: String
    let providerSessionId: String
    let agentType: String
    let cwd: String
    let subject: String
    let summary: String
    let tty: String
    let parkedAt: Date
    let lastActiveAt: Date
    let reason: String

    var shortSubject: String {
        let clean = subject.trimmingCharacters(in: .whitespacesAndNewlines)
        if clean.count <= 60 { return clean }
        return String(clean.prefix(57)) + "…"
    }
}

struct ZzzConfiguration: Codable {
    // Process termination must be an explicit user choice. A missing config
    // file used to opt every fresh install into auto-zzz, which made Codex
    // windows disappear after only 20 minutes with no visible receipt.
    var enabled: Bool = false
    var idleMinutes: Double = 60

    var idleSeconds: TimeInterval { max(60, idleMinutes * 60) }
}

/// Small JSON store shared by Slab and the `zzz` harness. One file per prompt
/// keeps writes atomic and makes a sleeping entry recoverable after a crash.
enum ZzzStore {
    private static let decoder: JSONDecoder = {
        let d = JSONDecoder()
        d.dateDecodingStrategy = .iso8601
        return d
    }()

    private static let encoder: JSONEncoder = {
        let e = JSONEncoder()
        e.dateEncodingStrategy = .iso8601
        e.outputFormatting = [.prettyPrinted, .sortedKeys]
        return e
    }()

    static func configuration() -> ZzzConfiguration {
        guard let data = FileManager.default.contents(atPath: Paths.zzzConfig),
              let config = try? decoder.decode(ZzzConfiguration.self, from: data)
        else { return ZzzConfiguration() }
        return config
    }

    static func setEnabled(_ enabled: Bool) {
        var config = configuration()
        config.enabled = enabled
        let dir = (Paths.zzzConfig as NSString).deletingLastPathComponent
        try? FileManager.default.createDirectory(atPath: dir,
                                                 withIntermediateDirectories: true)
        guard let data = try? encoder.encode(config) else { return }
        try? data.write(to: URL(fileURLWithPath: Paths.zzzConfig), options: .atomic)
    }

    static func entries() -> [ZzzEntry] {
        let fm = FileManager.default
        guard let names = try? fm.contentsOfDirectory(atPath: Paths.zzzDir) else { return [] }
        return names.compactMap { name in
            guard name.hasSuffix(".json"), !name.hasPrefix(".") else { return nil }
            let path = "\(Paths.zzzDir)/\(name)"
            guard let data = fm.contents(atPath: path) else { return nil }
            return try? decoder.decode(ZzzEntry.self, from: data)
        }.sorted { $0.parkedAt > $1.parkedAt }
    }

    static func write(_ entry: ZzzEntry) -> Bool {
        let fm = FileManager.default
        do {
            try fm.createDirectory(atPath: Paths.zzzDir, withIntermediateDirectories: true)
            let data = try encoder.encode(entry)
            try data.write(to: URL(fileURLWithPath: path(for: entry.id)), options: .atomic)
            return true
        } catch {
            NSLog("💤 [zzz] could not persist %@: %@", entry.id, String(describing: error))
            return false
        }
    }

    static func remove(_ id: String) {
        try? FileManager.default.removeItem(atPath: path(for: id))
    }

    private static func path(for id: String) -> String {
        let safe = id.replacingOccurrences(of: "/", with: "_")
        return "\(Paths.zzzDir)/\(safe).json"
    }
}

/// Central idle reaper. `zzz` deliberately means "park", not "forget": write
/// the wake record first, then end the process and leave its terminal receipt.
final class ZzzManager {
    static let shared = ZzzManager()

    private let queue = DispatchQueue(label: "slab.zzz")
    private var parking: Set<String> = []
    private var lastSweep = Date.distantPast
    private let sweepInterval: TimeInterval = 30

    private init() {}

    /// Called by Slab's ordinary refresh. Work is serialized off-main and
    /// throttled so the feature adds no per-frame or per-window polling cost.
    func tick(sessions: [ClaudeSession]) {
        let now = Date()
        guard now.timeIntervalSince(lastSweep) >= sweepInterval else { return }
        lastSweep = now
        let config = ZzzStore.configuration()
        guard config.enabled else { return }
        queue.async { [weak self] in
            self?.autoPark(sessions: sessions, config: config)
        }
    }

    /// Manual Slab-menu sibling of auto-zzz. It is intentionally limited to
    /// already-idle sessions; the explicit CLI has `--force` for emergencies.
    func parkIdle(sessionId: String, sessions: [ClaudeSession], completion: @escaping (String) -> Void) {
        guard let session = sessions.first(where: { $0.sessionId == sessionId }) else {
            completion("Prompt is no longer live.")
            return
        }
        queue.async { [weak self] in
            guard let self else { return }
            let result = self.park(session, reason: "manual", requireAge: nil)
            DispatchQueue.main.async { completion(result) }
        }
    }

    private func autoPark(sessions: [ClaudeSession], config: ZzzConfiguration) {
        for candidate in sessions {
            guard eligible(candidate, minimumIdle: config.idleSeconds) == nil else { continue }

            // Re-read markers immediately before the destructive half. A user
            // may have submitted a prompt since the background snapshot; a
            // fresh working/awaiting state must always win that race.
            guard let live = ClaudeSessionReader.active().first(where: {
                $0.sessionId == candidate.sessionId
            }), eligible(live, minimumIdle: config.idleSeconds) == nil else { continue }
            _ = park(live, reason: "idle-\(Int(config.idleMinutes))m",
                     requireAge: config.idleSeconds)
        }
    }

    /// Returns nil when eligible, otherwise a human-readable reason.
    private func eligible(_ session: ClaudeSession, minimumIdle: TimeInterval?) -> String? {
        if session.isRemote { return "remote prompt" }
        if session.state != .complete && session.state != .interrupted { return "prompt is not idle" }
        if session.subagentCount > 0 { return "subagents are still running" }
        if session.claudePid <= 0 || session.tty.isEmpty { return "missing pid/tty" }
        if session.cwd.isEmpty { return "missing working directory" }
        if session.agentType == "codex" && session.providerSessionId.isEmpty {
            return "Codex provider thread is not known yet"
        }
        if loopboySessionIds().contains(session.sessionId) { return "Loopboy-bound prompt" }
        if let seconds = minimumIdle,
           Date().timeIntervalSince(session.updated) < seconds { return "idle timer has not elapsed" }
        return nil
    }

    private func park(_ session: ClaudeSession, reason: String,
                      requireAge: TimeInterval?) -> String {
        if let why = eligible(session, minimumIdle: requireAge) { return "Not zzz'd: \(why)." }
        if parking.contains(session.sessionId) { return "Prompt is already entering zzz." }
        parking.insert(session.sessionId)
        defer { parking.remove(session.sessionId) }

        let provider = session.agentType == "codex"
            ? session.providerSessionId : session.sessionId
        let entry = ZzzEntry(
            version: 1,
            id: session.sessionId,
            providerSessionId: provider,
            agentType: session.agentType,
            cwd: session.cwd,
            subject: session.subject,
            summary: session.summary,
            tty: session.tty,
            parkedAt: Date(),
            lastActiveAt: session.updated,
            reason: reason
        )
        guard ZzzStore.write(entry) else { return "Could not write the zzz wake record." }

        let pid = pid_t(session.claudePid)
        if Self.pidAlive(pid) {
            _ = kill(pid, SIGTERM)
            Thread.sleep(forTimeInterval: 1.2)
        }
        if Self.pidAlive(pid) {
            _ = kill(pid, SIGKILL)
            Thread.sleep(forTimeInterval: 0.15)
        }
        if Self.pidAlive(pid) {
            ZzzStore.remove(session.sessionId)
            return "Could not stop pid \(session.claudePid); wake record was rolled back."
        }

        // The old implementation closed the whole tab here, which erased all
        // evidence that ZZZ acted. Keep the terminal and leave a colored,
        // copyable command after the agent has restored normal terminal mode.
        Self.leaveResumeReceipt(entry)
        let fm = FileManager.default
        try? fm.removeItem(atPath: "\(Paths.activePromptsDir)/\(session.sessionId)")
        try? fm.removeItem(atPath: "\(Paths.awaitingPromptsDir)/\(session.sessionId)")
        try? fm.removeItem(atPath: "\(Paths.runningToolsDir)/\(session.sessionId)")
        NSLog("💤 [zzz] parked %@ %@ (%@)", session.agentType,
              session.sessionId, reason)
        let selector = String(session.sessionId.prefix(8))
        return "zzz'd \(session.shortSubject). Resume with: zzz resume \(selector)"
    }

    private func loopboySessionIds() -> Set<String> {
        guard let data = FileManager.default.contents(atPath: Paths.loopboyConfig),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let loops = obj["loops"] as? [String: Any] else { return [] }
        return Set(loops.values.compactMap {
            ($0 as? [String: Any])?["sessionId"] as? String
        })
    }

    private static func pidAlive(_ pid: pid_t) -> Bool {
        guard pid > 0 else { return false }
        return kill(pid, 0) == 0 || errno == EPERM
    }

    private static func leaveResumeReceipt(_ entry: ZzzEntry) {
        let ttyName = (entry.tty as NSString).lastPathComponent
        let allowed = CharacterSet.alphanumerics.union(CharacterSet(charactersIn: "._-"))
        guard ttyName.hasPrefix("tty"),
              ttyName.unicodeScalars.allSatisfy({ allowed.contains($0) }) else { return }
        let selector = String(entry.id.prefix(8))
        let receipt = """
        \r
        \u{001B}[1;38;5;213m💤  ZZZ parked this prompt\u{001B}[0m\r
        \u{001B}[2m\(entry.shortSubject)\u{001B}[0m\r
        \u{001B}[1;38;5;51mResume: zzz resume \(selector)\u{001B}[0m\r

        """
        guard let data = receipt.data(using: .utf8),
              let handle = FileHandle(forWritingAtPath: "/dev/\(ttyName)") else { return }
        handle.seekToEndOfFile()
        handle.write(data)
        try? handle.close()
    }
}
