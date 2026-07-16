// Ledger.swift — the fleet's advertised handle ledger.
//
// Each slab machine advertises a compact list of its live handles (Claude
// sessions + headless agents) as `host:name` — e.g. neo:geb, blueberry:flock,
// panda:iris. Peers cache each other's ledgers over the tailnet, so an agent
// resolving a `host:name` reference does an O(1) local lookup instead of an
// SSH+find crawl.
//
// Three moving parts, all here:
//   • publishLocal — turns this machine's sessions (+ an advertise drop-dir for
//     non-tty agents) into ~/.config/slab/ledger/local.json, kept with a small
//     history log for provenance.
//   • LedgerHTTPServer — serves that JSON over the tailnet, bound to THIS
//     machine's tailscale IP only (tailnet is the auth perimeter, matching
//     slab/flock's posture — no bearer token, no public bind).
//   • fetchPeers — pulls each online peer's ledger onto disk (peers/<host>.json)
//     off the main thread, failure-tolerant, so resolves stay local + instant.
//
// The overlay stays local-only: rocks are never rendered for remote machines.
// The only remote control action is an allowlisted Claude/Codex launch; the
// ledger never accepts an arbitrary executable or shell command.
import Foundation

// One advertised handle. `name` is the stable session/thread pet-name (and
// matches the local overlay); `seed` is the evolving prompt-sensitive visual
// identity in hex so the current rock can be re-rendered anywhere.
struct LedgerEntry: Codable, Equatable {
    var id: String
    var host: String
    var name: String
    var subject: String
    var status: String     // working | awaiting | complete | rendering | blank | interrupted | agent-defined
    var kind: String       // "session" | "agent"
    var seed: String       // hex of the sigil seed
    var cwd: String
    var updated: Double     // ms since epoch
    // Owning CLI agent ("claude" | "codex" | …). Optional so ledgers published
    // by older peers (no field) still decode; nil is treated as "claude".
    var agentType: String?
}

struct Ledger: Codable {
    var host: String
    var ip: String
    var updatedAt: Double
    var entries: [LedgerEntry]
}

final class LedgerStore {
    static let shared = LedgerStore()

    static let port: UInt16 = 5252
    static let peerFetchInterval: TimeInterval = 10   // don't hammer the tailnet every 2s tick

    private let queue = DispatchQueue(label: "slab.ledger")
    private var server: LedgerHTTPServer?
    private var lastPeerFetch = Date.distantPast
    private var lastPublishedFingerprint = ""
    private var selfHost = ""
    private var selfIP = ""

    /// Transient "someone is reading this handle right now" marks, keyed by the
    /// session id. Set when a poke beacon lands; the rock overlay reads these
    /// each frame to blink + spin faster, and they decay after `observeWindow`.
    /// Renewed on repeat pokes so sustained attention keeps the rock lively.
    private var observed: [String: (by: String, until: Date)] = [:]
    static let observeWindow: TimeInterval = 4
    /// Posted (main queue) with userInfo["id"] the moment a poke lands, so the
    /// overlay loop wakes from idle and shows the reaction without waiting for
    /// the next lazy tick.
    static let observedNote = Notification.Name("slab.ledger.observed")

    // ── on-disk layout (kept: survives restarts) ─────────────────────────
    static var dir: String { "\(Paths.home)/.config/slab/ledger" }
    static var localFile: String { "\(dir)/local.json" }
    static var peersDir: String { "\(dir)/peers" }
    static var advertiseDir: String { "\(dir)/advertise" }   // non-tty agents drop JSON here
    static var historyLog: String { "\(dir)/history.jsonl" }

    private let encoder: JSONEncoder = {
        let e = JSONEncoder()
        e.outputFormatting = [.prettyPrinted, .sortedKeys]
        return e
    }()

    // ── lifecycle ────────────────────────────────────────────────────────
    /// Ensure the ledger dirs exist and start serving on the tailscale IP.
    /// Safe to call once at launch; the server rebinds if the IP appears later.
    func start() {
        let fm = FileManager.default
        for d in [Self.dir, Self.peersDir, Self.advertiseDir] {
            try? fm.createDirectory(atPath: d, withIntermediateDirectories: true)
        }
        queue.async { [weak self] in self?.ensureServer() }
    }

    func stop() { queue.async { [weak self] in self?.server?.stop(); self?.server = nil } }

    /// Called each refresh tick (off-main). Publishes this machine's ledger and,
    /// on its own slower cadence, refreshes the peer cache. Never blocks the UI.
    func tick(sessions: [ClaudeSession], peers: [TailnetPeer]) {
        queue.async { [weak self] in
            guard let self else { return }
            self.ensureServer()
            self.publishLocal(sessions: sessions)
            if Date().timeIntervalSince(self.lastPeerFetch) >= Self.peerFetchInterval {
                self.lastPeerFetch = Date()
                self.fetchPeers(peers.filter { $0.online && !$0.ip.isEmpty })
            }
        }
    }

    // ── serving ──────────────────────────────────────────────────────────
    private func ensureServer() {
        let id = LedgerStore.selfIdentity()
        selfHost = id.host
        selfIP = id.ip
        guard !selfIP.isEmpty else { return }          // no tailnet yet — try again next tick
        if let s = server, s.boundIP == selfIP { return }
        server?.stop()
        let s = try? LedgerHTTPServer(ip: selfIP, port: Self.port)
        s?.onPoke = { [weak self] body in self?.receivePoke(body) }
        s?.onLaunch = { body in Self.launchPrompt(body) }
        server = s
    }

    /// Start one allowlisted interactive agent in Terminal.app. The ledger is
    /// bound only to the tailnet IP; this endpoint deliberately accepts no
    /// executable or shell text from the caller. A caller chooses only the
    /// agent, an initial prompt, and a working directory inside this user's
    /// home folder.
    private static func launchPrompt(_ body: [String: Any]) -> [String: Any] {
        let agent = ((body["agent"] as? String) ?? "").lowercased()
        guard agent == "claude" || agent == "codex" else {
            return ["ok": false, "error": "agent must be claude or codex"]
        }

        let prompt = (body["prompt"] as? String) ?? ""
        guard prompt.count <= 4_000 else {
            return ["ok": false, "error": "prompt exceeds 4000 characters"]
        }

        let requested = (body["cwd"] as? String).flatMap { $0.isEmpty ? nil : $0 }
            ?? Paths.acRepo
        guard requested.hasPrefix("/") else {
            return ["ok": false, "error": "cwd must be an absolute path"]
        }
        let fm = FileManager.default
        var isDir: ObjCBool = false
        guard fm.fileExists(atPath: requested, isDirectory: &isDir), isDir.boolValue else {
            return ["ok": false, "error": "cwd does not exist or is not a directory"]
        }
        let home = URL(fileURLWithPath: Paths.home).standardizedFileURL
            .resolvingSymlinksInPath().path
        let cwd = URL(fileURLWithPath: requested).standardizedFileURL
            .resolvingSymlinksInPath().path
        guard cwd == home || cwd.hasPrefix(home + "/") else {
            return ["ok": false, "error": "cwd must stay inside the target user's home folder"]
        }

        let binary = agent == "codex"
            ? "\(Paths.slabBin)/codex-slab"
            : "\(Paths.home)/.local/bin/claude"
        guard fm.isExecutableFile(atPath: binary) else {
            return ["ok": false, "error": "\(agent) launcher is not installed"]
        }

        var command = "cd \(shellQuote(cwd)) && exec \(shellQuote(binary))"
        if !prompt.isEmpty { command += " \(shellQuote(prompt))" }

        // Hand Terminal a one-shot executable document through LaunchServices.
        // This avoids the macOS Automation permission required by AppleScript
        // `tell application "Terminal"`, which would otherwise leave a fresh
        // headless prompt host waiting behind a TCC dialog. Every dynamic field
        // in `command` was shell-quoted above; the script is private to the user
        // and removes itself as soon as Terminal starts it.
        let launchDir = "\(Paths.slabHome)/launches"
        do {
            try fm.createDirectory(atPath: launchDir, withIntermediateDirectories: true,
                                   attributes: [.posixPermissions: 0o700])
        } catch {
            return ["ok": false, "error": "could not create launch directory"]
        }
        let launchPath = "\(launchDir)/\(UUID().uuidString).command"
        let script = "#!/bin/zsh\nrm -f \(shellQuote(launchPath))\n\(command)\n"
        do {
            try script.write(toFile: launchPath, atomically: true, encoding: .utf8)
            try fm.setAttributes([.posixPermissions: 0o700], ofItemAtPath: launchPath)
        } catch {
            try? fm.removeItem(atPath: launchPath)
            return ["ok": false, "error": "could not stage Terminal launch"]
        }

        let proc = Process()
        proc.executableURL = URL(fileURLWithPath: "/usr/bin/open")
        proc.arguments = ["-a", "Terminal", launchPath]
        let err = Pipe()
        proc.standardOutput = Pipe()
        proc.standardError = err
        do {
            try proc.run()
            proc.waitUntilExit()
        } catch {
            try? fm.removeItem(atPath: launchPath)
            return ["ok": false, "error": "could not start Terminal.app"]
        }
        guard proc.terminationStatus == 0 else {
            try? fm.removeItem(atPath: launchPath)
            let detail = String(data: err.fileHandleForReading.readDataToEndOfFile(),
                                encoding: .utf8)?.trimmingCharacters(in: .whitespacesAndNewlines)
            return ["ok": false, "error": detail?.isEmpty == false ? detail! : "Terminal.app rejected launch"]
        }
        let by = String(((body["by"] as? String) ?? "prox").prefix(100))
        NSLog("🪨 [ledger] %@ launched %@ in %@", by, agent, cwd)
        return ["ok": true, "host": selfIdentity().host, "agent": agent, "cwd": cwd]
    }

    private static func shellQuote(_ value: String) -> String {
        "'" + value.replacingOccurrences(of: "'", with: "'\\''") + "'"
    }

    // ── observed (poke) state ────────────────────────────────────────────
    /// A peer resolved one of our handles. Mark the matching session observed
    /// (renewing the decay window), log it as kept provenance, and wake the
    /// overlay so the rock reacts immediately. `id` is preferred; `name` is the
    /// fallback when only the pet-name was referenced.
    private func receivePoke(_ body: [String: Any]) {
        let by = (body["by"] as? String) ?? "someone"
        let pokeId = (body["id"] as? String) ?? ""
        let pokeName = (body["name"] as? String) ?? ""
        let sid = sessionId(forId: pokeId, name: pokeName)
        guard !sid.isEmpty else { return }
        queue.async { [weak self] in
            guard let self else { return }
            self.observed[sid] = (by, Date().addingTimeInterval(Self.observeWindow))
        }
        appendReference(sid: sid, name: pokeName, by: by)
        DispatchQueue.main.async {
            NotificationCenter.default.post(name: Self.observedNote,
                                            object: nil, userInfo: ["id": sid])
        }
    }

    /// Local event sources (iMessage, timers, host integrations) use the same
    /// observed path as a fleet `/poke`, without an HTTP loopback. Binding by
    /// session id keeps the target stable even while its prompt and rock form
    /// evolve.
    func pokeLocal(sessionId: String, by: String) {
        receivePoke(["id": sessionId, "by": by])
    }

    /// Live observed record for a session, or nil once the window has decayed.
    /// Thread-safe; the overlay controller calls this each frame.
    func observation(for sessionId: String) -> (by: String, remaining: TimeInterval)? {
        queue.sync {
            guard let o = observed[sessionId] else { return nil }
            let remaining = o.until.timeIntervalSinceNow
            if remaining <= 0 { observed.removeValue(forKey: sessionId); return nil }
            return (o.by, remaining)
        }
    }

    private func sessionId(forId id: String, name: String) -> String {
        let entries = (try? JSONDecoder().decode(
            Ledger.self, from: Data(contentsOf: URL(fileURLWithPath: Self.localFile))))?.entries ?? []
        if !id.isEmpty, entries.contains(where: { $0.id == id }) { return id }
        if !name.isEmpty, let e = entries.first(where: { $0.name == name }) { return e.id }
        return id
    }

    private func appendReference(sid: String, name: String, by: String) {
        let line = ["ts": Date().timeIntervalSince1970 * 1000, "host": selfHost,
                    "event": "referenced", "id": sid, "name": name, "by": by] as [String: Any]
        guard let data = try? JSONSerialization.data(withJSONObject: line),
              let text = String(data: data, encoding: .utf8) else { return }
        let path = Self.historyLog
        if let fh = FileHandle(forWritingAtPath: path) {
            fh.seekToEndOfFile(); fh.write(Data((text + "\n").utf8)); try? fh.close()
        } else {
            try? (text + "\n").write(toFile: path, atomically: true, encoding: .utf8)
        }
    }

    // ── local publish (sessions + advertised agents) ─────────────────────
    private func publishLocal(sessions: [ClaudeSession]) {
        var entries: [LedgerEntry] = sessions.map { s in
            let seed = SigilRenderer.seed(for: s.sessionId + "\u{1}" + s.subject)
            return LedgerEntry(
                id: s.sessionId,
                host: selfHost,
                name: SigilRenderer.name(forSessionId: s.sessionId),
                subject: s.titleString,
                status: statusName(s.state),
                kind: "session",
                seed: String(format: "%016llx", seed),
                cwd: s.cwd,
                updated: s.updated.timeIntervalSince1970 * 1000,
                agentType: s.agentType)
        }
        entries.append(contentsOf: advertisedAgents())

        let ledger = Ledger(host: selfHost, ip: selfIP,
                            updatedAt: Date().timeIntervalSince1970 * 1000,
                            entries: entries)
        guard let data = try? encoder.encode(ledger) else { return }
        try? data.write(to: URL(fileURLWithPath: Self.localFile), options: [.atomic])
        recordHistory(entries)
    }

    /// Non-tty agents advertise by dropping a small JSON file in advertiseDir:
    ///   { "id": "iris", "name": "iris", "subject": "flock weaver", "status": "working" }
    /// Stale drops (writer says a pid that's gone, or mtime > 5 min) are swept so
    /// a crashed agent leaves no ghost handle.
    private func advertisedAgents() -> [LedgerEntry] {
        let fm = FileManager.default
        guard let names = try? fm.contentsOfDirectory(atPath: Self.advertiseDir) else { return [] }
        let now = Date()
        var out: [LedgerEntry] = []
        for name in names where name.hasSuffix(".json") {
            let path = "\(Self.advertiseDir)/\(name)"
            guard let data = fm.contents(atPath: path),
                  let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
            else { continue }
            let pid = (obj["pid"] as? Int) ?? 0
            let dead = pid > 0 && kill(pid_t(pid), 0) != 0 && errno == ESRCH
            let mtime = (try? fm.attributesOfItem(atPath: path)[.modificationDate] as? Date) ?? now
            if dead || now.timeIntervalSince(mtime) > 300 {
                try? fm.removeItem(atPath: path)
                continue
            }
            let id = (obj["id"] as? String) ?? (name as NSString).deletingPathExtension
            let handle = (obj["name"] as? String) ?? id
            let subject = (obj["subject"] as? String) ?? ""
            let seed = SigilRenderer.seed(for: id + "\u{1}" + subject)
            out.append(LedgerEntry(
                id: id, host: selfHost, name: handle, subject: subject,
                status: (obj["status"] as? String) ?? "running",
                kind: "agent", seed: String(format: "%016llx", seed),
                cwd: (obj["cwd"] as? String) ?? "",
                updated: mtime.timeIntervalSince1970 * 1000,
                agentType: (obj["agent_type"] as? String)))
        }
        return out
    }

    /// Append one line to the history log whenever the advertised set changes —
    /// kept provenance the CLI or a human can walk back through. Trimmed so the
    /// log never grows without bound.
    private func recordHistory(_ entries: [LedgerEntry]) {
        let fingerprint = entries.map { "\($0.id):\($0.name):\($0.status)" }
            .sorted().joined(separator: ",")
        guard fingerprint != lastPublishedFingerprint else { return }
        lastPublishedFingerprint = fingerprint
        let line = ["ts": Date().timeIntervalSince1970 * 1000, "host": selfHost,
                    "handles": entries.map { ["host": $0.host, "name": $0.name,
                                              "id": $0.id, "status": $0.status] }] as [String: Any]
        guard let data = try? JSONSerialization.data(withJSONObject: line),
              let text = String(data: data, encoding: .utf8) else { return }
        let path = Self.historyLog
        if let fh = FileHandle(forWritingAtPath: path) {
            fh.seekToEndOfFile(); fh.write(Data((text + "\n").utf8)); try? fh.close()
        } else {
            try? (text + "\n").write(toFile: path, atomically: true, encoding: .utf8)
        }
        trimHistory(path)
    }

    private func trimHistory(_ path: String) {
        guard let text = try? String(contentsOfFile: path, encoding: .utf8) else { return }
        let lines = text.split(separator: "\n", omittingEmptySubsequences: true)
        guard lines.count > 500 else { return }
        let kept = lines.suffix(500).joined(separator: "\n") + "\n"
        try? kept.write(toFile: path, atomically: true, encoding: .utf8)
    }

    // ── peer fetch (off-main, failure-tolerant) ──────────────────────────
    private func fetchPeers(_ peers: [TailnetPeer]) {
        for peer in peers {
            guard let url = URL(string: "http://\(peer.ip):\(Self.port)/ledger") else { continue }
            var req = URLRequest(url: url)
            req.timeoutInterval = 2
            URLSession.shared.dataTask(with: req) { data, resp, _ in
                guard let data, let http = resp as? HTTPURLResponse, http.statusCode == 200,
                      let ledger = try? JSONDecoder().decode(Ledger.self, from: data)
                else { return }
                // Re-wrap with fetch provenance, then persist the peer cache —
                // named by the ledger's own host so the file reads as peers/neo.json.
                var obj = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any] ?? [:]
                obj["fetchedAt"] = Date().timeIntervalSince1970 * 1000
                obj["fetchedFrom"] = peer.ip
                guard let out = try? JSONSerialization.data(withJSONObject: obj,
                                                            options: [.prettyPrinted, .sortedKeys])
                else { return }
                let host = ledger.host.isEmpty ? peer.hostname : ledger.host
                let safe = host.replacingOccurrences(of: "/", with: "_")
                let path = "\(LedgerStore.peersDir)/\(safe).json"
                try? out.write(to: URL(fileURLWithPath: path), options: [.atomic])
            }.resume()
        }
    }

    // ── helpers ──────────────────────────────────────────────────────────
    private func statusName(_ s: ClaudeSession.State) -> String {
        switch s {
        case .blank:       return "blank"
        case .working:     return "working"
        case .rendering:   return "rendering"
        case .complete:    return "complete"
        case .awaiting:    return "awaiting"
        case .interrupted: return "interrupted"
        case .stale:       return "stale"
        }
    }

    /// This machine's tailnet identity (hostname + IPv4) from `tailscale status
    /// --json` → Self. Matches the naming peers advertise, so host keys line up
    /// fleet-wide (neo, blueberry, …).
    static func selfIdentity() -> (host: String, ip: String) {
        // Host is the SHORT OS hostname (neo, blueberry) — the name the fleet
        // references, not tailscale's device label ("Jeffrey's MacBook Neo").
        // IP is this machine's tailscale v4, for binding + advertising.
        let raw = ProcessInfo.processInfo.hostName
        let host = raw.split(separator: ".").first.map { $0.lowercased() } ?? raw.lowercased()
        var ip = ""
        if let ts = Tools.resolve("tailscale"),
           let out = ShellRunner.output(ts, args: ["status", "--json"], timeout: 2),
           let data = out.data(using: .utf8),
           let json = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
           let me = json["Self"] as? [String: Any] {
            let ips = (me["TailscaleIPs"] as? [String]) ?? []
            ip = ips.first { $0.contains(".") } ?? ""
        }
        return (host, ip)
    }
}

// ── minimal tailnet HTTP server ──────────────────────────────────────────
// Serves the local ledger JSON, bound to a single IP (the tailscale address).
// One connection at a time is plenty for a ledger that peers poll every ~10s.
// Modeled on ACLogin's loopback SocketListener, but bound to the tailnet IP.
final class LedgerHTTPServer {
    let boundIP: String
    /// Called on `POST /poke` with the decoded JSON body — the owner marks the
    /// referenced handle "observed".
    var onPoke: (([String: Any]) -> Void)?
    /// Called on POST /launch. The callback owns validation and returns a
    /// compact JSON-safe result dictionary.
    var onLaunch: (([String: Any]) -> [String: Any])?
    private var fd: Int32 = -1
    private let queue = DispatchQueue(label: "slab.ledger.http")
    private var running = false

    init(ip: String, port: UInt16) throws {
        boundIP = ip
        fd = socket(AF_INET, SOCK_STREAM, 0)
        guard fd >= 0 else { throw NSError(domain: "slab.ledger.socket", code: Int(errno)) }
        var yes: Int32 = 1
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &yes, socklen_t(MemoryLayout<Int32>.size))
        // macOS can retain the previous listener's tailnet 4-tuple through
        // TIME_WAIT after an in-place app upgrade. REUSEPORT lets the freshly
        // signed replacement reclaim 5252 immediately instead of disappearing
        // from prox for up to a minute.
        setsockopt(fd, SOL_SOCKET, SO_REUSEPORT, &yes, socklen_t(MemoryLayout<Int32>.size))

        var addr = sockaddr_in()
        addr.sin_family = sa_family_t(AF_INET)
        addr.sin_port = port.bigEndian
        addr.sin_addr.s_addr = inet_addr(ip)   // tailnet IP only — not INADDR_ANY
        let bound = withUnsafePointer(to: &addr) {
            $0.withMemoryRebound(to: sockaddr.self, capacity: 1) {
                bind(fd, $0, socklen_t(MemoryLayout<sockaddr_in>.size))
            }
        }
        guard bound == 0, listen(fd, 8) == 0 else {
            close(fd); fd = -1
            throw NSError(domain: "slab.ledger.bind", code: Int(errno))
        }
        running = true
        queue.async { [weak self] in self?.acceptLoop() }
    }

    func stop() { running = false; if fd >= 0 { close(fd); fd = -1 } }

    private func acceptLoop() {
        while running {
            let client = accept(fd, nil, nil)
            if client < 0 { break }
            handle(client)
        }
    }

    private func handle(_ client: Int32) {
        defer { close(client) }
        // A 2s recv timeout so a stalled client can never wedge the accept loop.
        var tv = timeval(tv_sec: 2, tv_usec: 0)
        setsockopt(client, SOL_SOCKET, SO_RCVTIMEO, &tv, socklen_t(MemoryLayout<timeval>.size))

        // Read headers, then — for a POST — keep reading until the declared
        // Content-Length of body bytes has arrived. URLSession sends the header
        // block and body in SEPARATE segments, so a single read() misses the
        // body; curl happens to send both in one. Loop until complete.
        var data = Data()
        var buf = [UInt8](repeating: 0, count: 8192)
        let marker = Data("\r\n\r\n".utf8)
        var bodyStart: Int?
        var contentLength = 0
        while true {
            let n = read(client, &buf, buf.count)
            if n <= 0 { break }
            data.append(contentsOf: buf[0..<n])
            if bodyStart == nil, let r = data.range(of: marker) {
                bodyStart = r.upperBound
                let head = String(decoding: data[0..<r.lowerBound], as: UTF8.self)
                for line in head.split(separator: "\r\n") where line.lowercased().hasPrefix("content-length:") {
                    contentLength = Int(line.split(separator: ":")[1]
                        .trimmingCharacters(in: .whitespaces)) ?? 0
                }
            }
            if let bs = bodyStart, data.count - bs >= contentLength { break }
        }
        let raw = String(decoding: data, as: UTF8.self)
        let line = raw.split(separator: "\r\n", maxSplits: 1).first.map(String.init) ?? ""

        // POST /poke — a peer read one of our handles. Decode the JSON body and
        // mark it observed; reply {"ok":true}.
        if line.hasPrefix("POST"), line.contains("/poke") {
            if let bs = bodyStart, bs <= data.count,
               let obj = try? JSONSerialization.jsonObject(with: data[bs...]) as? [String: Any] {
                onPoke?(obj)
            }
            respond(client, body: Data("{\"ok\":true}".utf8))
            return
        }

        // POST /launch — start one fixed Claude/Codex launcher in Terminal.
        // The callback rejects arbitrary binaries, paths outside HOME, and
        // oversized prompts; this HTTP layer only handles framing.
        if line.hasPrefix("POST"), line.contains("/launch") {
            let obj: [String: Any]
            if let bs = bodyStart, bs <= data.count,
               let decoded = try? JSONSerialization.jsonObject(with: data[bs...]) as? [String: Any] {
                obj = decoded
            } else {
                obj = [:]
            }
            let result = onLaunch?(obj) ?? ["ok": false, "error": "launcher unavailable"]
            let body = (try? JSONSerialization.data(withJSONObject: result, options: [.sortedKeys]))
                ?? Data("{\"ok\":false,\"error\":\"encoding failed\"}".utf8)
            respond(client, body: body)
            return
        }

        // Anything else (GET /ledger) → the current local ledger JSON.
        let body = (try? Data(contentsOf: URL(fileURLWithPath: LedgerStore.localFile)))
            ?? Data("{\"host\":\"\",\"entries\":[]}".utf8)
        respond(client, body: body)
    }

    private func respond(_ client: Int32, body: Data) {
        let header = "HTTP/1.0 200 OK\r\n"
            + "Content-Type: application/json\r\n"
            + "Content-Length: \(body.count)\r\n"
            + "Connection: close\r\n\r\n"
        var out = Data(header.utf8); out.append(body)
        _ = out.withUnsafeBytes { write(client, $0.baseAddress, $0.count) }
    }
}
