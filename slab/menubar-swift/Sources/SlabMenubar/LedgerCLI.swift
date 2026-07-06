// LedgerCLI.swift — the scriptable resolver an agent calls to turn a
// `host:name` reference into a concrete handle, reading only the kept on-disk
// cache (local + peers). No network, no running menubar required, O(small):
//
//   slab-menubar ledger resolve neo:geb      → matching handle(s) as JSON
//   slab-menubar ledger resolve iris         → search every host
//   slab-menubar ledger dump                 → merged fleet ledger
//   slab-menubar ledger hosts                → advertised hosts + counts
//
// main.swift calls handleIfPresent() before any AppKit bootstrap, so the same
// binary doubles as a CLI without ever spawning a menubar.
import Foundation

enum LedgerCLI {
    /// Returns true if argv was a ledger subcommand (caller then exits). A
    /// no-match / bad-usage still returns true — it's ours to answer.
    static func handleIfPresent(_ argv: [String]) -> Bool {
        guard argv.count >= 2, argv[1] == "ledger" else { return false }
        let sub = argv.count >= 3 ? argv[2] : "resolve"
        switch sub {
        case "resolve":
            let ref = argv.count >= 4 ? argv[3] : ""
            resolve(ref)
        case "dump":
            printJSON(load().map(encodeEntry))
        case "hosts":
            var counts: [String: Int] = [:]
            for e in load() { counts[e.host, default: 0] += 1 }
            printJSON(counts.keys.sorted().map { ["host": $0, "handles": counts[$0] ?? 0] as [String: Any] })
        default:
            FileHandle.standardError.write(Data("usage: ledger [resolve <host:name> | dump | hosts]\n".utf8))
        }
        return true
    }

    // ── resolution ─────────────────────────────────────────────────────
    private static func resolve(_ ref: String) {
        var host = ""
        var query = ref
        if let colon = ref.firstIndex(of: ":") {
            host = String(ref[..<colon]).lowercased()
            query = String(ref[ref.index(after: colon)...])
        }
        let q = slug(query)
        let matches = load().filter { e in
            if !host.isEmpty && e.host.lowercased() != host { return false }
            if q.isEmpty { return true }
            return slug(e.name) == q
                || slug(e.id).hasPrefix(q)
                || slug(e.name).hasPrefix(q)
                || slug(e.subject).contains(q)
        }
        // Exact name hits first — the most direct read of a `host:name` ref.
        let ranked = matches.sorted { a, b in
            let ax = slug(a.name) == q ? 0 : 1
            let bx = slug(b.name) == q ? 0 : 1
            if ax != bx { return ax < bx }
            return a.updated > b.updated
        }
        poke(ranked)                      // tell each owner its handle was read
        printJSON(ranked.map(encodeEntry))
    }

    /// Fire a best-effort "poke" beacon to each matched handle's owner so its
    /// rock reacts to being read. Owner IP comes from the cached ledger; sends
    /// are concurrent with a short overall cap so a slow/offline owner never
    /// stalls the resolve. Set SLAB_POKE_BY to identify as an agent (iris,
    /// hermes, …) rather than the bare hostname.
    private static func poke(_ entries: [LedgerEntry]) {
        let ips = hostIPs()
        let by = ProcessInfo.processInfo.environment["SLAB_POKE_BY"]
            ?? ProcessInfo.processInfo.hostName.replacingOccurrences(of: ".local", with: "")
        let group = DispatchGroup()
        for e in entries {
            guard let ip = ips[e.host.lowercased()], !ip.isEmpty,
                  let url = URL(string: "http://\(ip):\(LedgerStore.port)/poke") else { continue }
            var req = URLRequest(url: url)
            req.httpMethod = "POST"
            req.timeoutInterval = 2
            req.setValue("application/json", forHTTPHeaderField: "Content-Type")
            let payload = ["id": e.id, "name": e.name, "by": by,
                           "ts": Date().timeIntervalSince1970 * 1000] as [String: Any]
            req.httpBody = try? JSONSerialization.data(withJSONObject: payload)
            group.enter()
            URLSession.shared.dataTask(with: req) { _, _, _ in group.leave() }.resume()
        }
        _ = group.wait(timeout: .now() + 2)   // let the beacons flush before we exit
    }

    // ── cache loading (local + peers) ───────────────────────────────────
    private static func load() -> [LedgerEntry] {
        var all: [LedgerEntry] = []
        all.append(contentsOf: entries(atPath: LedgerStore.localFile))
        if let names = try? FileManager.default.contentsOfDirectory(atPath: LedgerStore.peersDir) {
            for n in names where n.hasSuffix(".json") {
                all.append(contentsOf: entries(atPath: "\(LedgerStore.peersDir)/\(n)"))
            }
        }
        return all
    }

    private static func entries(atPath path: String) -> [LedgerEntry] {
        guard let data = FileManager.default.contents(atPath: path),
              let ledger = try? JSONDecoder().decode(Ledger.self, from: data)
        else { return [] }
        return ledger.entries
    }

    /// host → tailscale IP, gleaned from every cached ledger (local + peers).
    /// Lets the resolver reach an owner's `/poke` endpoint without a discovery
    /// fork of its own.
    private static func hostIPs() -> [String: String] {
        var map: [String: String] = [:]
        var paths = [LedgerStore.localFile]
        if let names = try? FileManager.default.contentsOfDirectory(atPath: LedgerStore.peersDir) {
            paths += names.filter { $0.hasSuffix(".json") }.map { "\(LedgerStore.peersDir)/\($0)" }
        }
        for p in paths {
            guard let data = FileManager.default.contents(atPath: p),
                  let ledger = try? JSONDecoder().decode(Ledger.self, from: data),
                  !ledger.host.isEmpty, !ledger.ip.isEmpty else { continue }
            map[ledger.host.lowercased()] = ledger.ip
        }
        return map
    }

    // ── output ──────────────────────────────────────────────────────────
    private static func encodeEntry(_ e: LedgerEntry) -> [String: Any] {
        ["id": e.id, "host": e.host, "name": e.name, "subject": e.subject,
         "status": e.status, "kind": e.kind, "seed": e.seed, "cwd": e.cwd,
         "handle": "\(e.host):\(e.name)"]
    }

    private static func printJSON(_ obj: Any) {
        guard let data = try? JSONSerialization.data(
            withJSONObject: obj, options: [.prettyPrinted, .sortedKeys]),
              let text = String(data: data, encoding: .utf8) else { print("[]"); return }
        print(text)
    }

    /// lowercase, non-alphanumerics → single '-', trimmed. Turns "flock theme"
    /// and "Flock-Theme!" into the same "flock-theme" a reference would use.
    private static func slug(_ s: String) -> String {
        var out = ""
        var lastDash = true
        for ch in s.lowercased() {
            if ch.isLetter || ch.isNumber { out.append(ch); lastDash = false }
            else if !lastDash { out.append("-"); lastDash = true }
        }
        if out.hasSuffix("-") { out.removeLast() }
        return out
    }
}
