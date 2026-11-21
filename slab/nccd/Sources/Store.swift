// Store.swift — the message log and the people ledger.
//
// signal-cli keeps no history, so the app keeps its own: one append-only file
// per circle under the app's support directory, a record to a line. Reactions
// are records in the same file, folded onto their message when the log loads.
//
// Each line is sealed (AES-256-GCM) with a key that lives in the login
// Keychain, as Signal Desktop keeps its database key. A copy of the file is
// not a copy of the chat.

import AppKit
import CryptoKit
import Foundation

struct Preview: Codable {
    var url: String
    var title: String = ""
    var description: String = ""
    var image: String?        // the preview picture on disk
}

struct Msg: Codable {
    var ts: Int64             // sent timestamp, unix ms — Signal's message id
    var author: String        // sender ACI
    var name: String?         // sender name as Signal gave it
    var body: String = ""
    var files: [String]?      // attachment paths on disk
    var fileNames: [String]?  // their original names, same order
    var previews: [Preview]?  // link previews the sender attached
    var quote: String?        // the quoted text, when this is a reply
    var kind: String?         // nil = message, "reaction"
    var target: Int64?        // reaction: the message it lands on
    var emoji: String?
    var remove: Bool?
    var expires: Int64?       // unix ms when a disappearing message goes
}

// ─── the vault ───────────────────────────────────────────────────────────────

// The key that seals the logs. It is made on the first run and kept in the
// login Keychain, which lets this app read it and asks before another does.
enum Vault {
    private static let service = Paths.id + ".log"
    private static let mark = "1:"   // the format of a sealed line

    // nil when the Keychain does not give the key. The app then keeps what
    // arrives in memory and writes nothing.
    static let key: SymmetricKey? = {
        let q: [CFString: Any] = [kSecClass: kSecClassGenericPassword, kSecAttrService: service, kSecAttrAccount: "key"]
        var read = q
        read[kSecReturnData] = true
        var out: CFTypeRef?
        let status = SecItemCopyMatching(read as CFDictionary, &out)
        if status == errSecSuccess, let d = out as? Data, d.count == 32 { return SymmetricKey(data: d) }
        guard status == errSecItemNotFound else { return nil }   // refused, or locked: never make a second key
        let fresh = SymmetricKey(size: .bits256)
        var add = q
        add[kSecValueData] = fresh.withUnsafeBytes { Data($0) }
        add[kSecAttrLabel] = "NCCd chat log key"
        return SecItemAdd(add as CFDictionary, nil) == errSecSuccess ? fresh : nil
    }()

    // The circle's id is bound into the seal, so a line cannot be moved to another log.
    static func seal(_ plain: Data, for groupId: String) -> String? {
        guard let key, let box = try? AES.GCM.seal(plain, using: key, authenticating: Data(groupId.utf8)),
              let all = box.combined else { return nil }
        return mark + all.base64EncodedString()
    }

    static func open(_ line: Substring, for groupId: String) -> Data? {
        guard let key, line.hasPrefix(mark), let all = Data(base64Encoded: String(line.dropFirst(mark.count))),
              let box = try? AES.GCM.SealedBox(combined: all) else { return nil }
        return try? AES.GCM.open(box, using: key, authenticating: Data(groupId.utf8))
    }

    // Only the owner reads it.
    static func lock(_ url: URL, dir: Bool = false) {
        try? FileManager.default.setAttributes([.posixPermissions: dir ? 0o700 : 0o600], ofItemAtPath: url.path)
    }
}

final class CircleLog {
    let groupId: String
    private(set) var msgs: [Msg] = []
    private(set) var reactions: [Int64: [String: Set<String>]] = [:]  // ts → emoji → authors
    private var seen = Set<String>()
    private var records: [Msg] = []     // every record of the file, to write it again
    private var closed: [String] = []   // lines this key does not open; they are kept as they are
    private let file: URL

    init(groupId: String) {
        self.groupId = groupId
        let h = SHA256.hash(data: Data(groupId.utf8)).prefix(8).map { String(format: "%02x", $0) }.joined()
        file = Paths.log.appendingPathComponent(h + ".jsonl")
        guard let text = try? String(contentsOf: file, encoding: .utf8) else { return }
        let dec = JSONDecoder()
        var plain = false   // a log from before the seal
        for line in text.split(separator: "\n") {
            let data: Data?
            if line.hasPrefix("{") { data = Data(line.utf8); plain = true }
            else { data = Vault.open(line, for: groupId) }
            guard let data, let m = try? dec.decode(Msg.self, from: data) else {
                if !line.hasPrefix("{") { closed.append(String(line)) }
                continue
            }
            if fold(m) { records.append(m) }
        }
        msgs.sort { $0.ts < $1.ts }
        if !prune(), plain { rewrite() }
    }

    // Returns false when the record was already in the log.
    @discardableResult
    func add(_ m: Msg) -> Bool {
        guard fold(m) else { return false }
        records.append(m)
        if let last = msgs.last, msgs.count > 1, last.ts < msgs[msgs.count - 2].ts {
            msgs.sort { $0.ts < $1.ts }
        }
        if let d = try? JSONEncoder().encode(m), let line = Vault.seal(d, for: groupId) {
            if let h = try? FileHandle(forWritingTo: file) {
                h.seekToEndOfFile()
                h.write(Data((line + "\n").utf8))
                try? h.close()
            } else {
                try? Data((line + "\n").utf8).write(to: file)
                Vault.lock(file)
            }
        }
        return true
    }

    // A message its sender deleted for everyone. Returns true when it was here.
    @discardableResult
    func remove(ts: Int64, author: String) -> Bool {
        drop { $0.ts == ts && $0.author == author }
    }

    // Disappearing messages that are past their time go. Returns true when any went.
    @discardableResult
    func prune(now: Int64 = Int64(Date().timeIntervalSince1970 * 1000)) -> Bool {
        drop { ($0.expires ?? .max) <= now }
    }

    // Takes messages out of the sheet and the file, with their reactions and their files.
    private func drop(_ gone: (Msg) -> Bool) -> Bool {
        let out = msgs.filter(gone)
        guard !out.isEmpty else { return false }
        let times = Set(out.map { $0.ts })
        msgs.removeAll(where: gone)
        records.removeAll { $0.kind == "reaction" ? times.contains($0.target ?? 0) : gone($0) }
        for t in times { reactions[t] = nil }
        // Only files the app itself was given: a path from anywhere else is left alone.
        let own = [Config.shared.signalConfigDir.appendingPathComponent("attachments").path,
                   Paths.root.appendingPathComponent("media").path]
        for path in out.flatMap({ ($0.files ?? []) + ($0.previews ?? []).compactMap { $0.image } })
        where own.contains(where: { path.hasPrefix($0 + "/") }) {
            try? FileManager.default.removeItem(atPath: path)
        }
        rewrite()
        return true
    }

    // The whole file again, every line sealed. Without the key the file stays as it is.
    private func rewrite() {
        guard Vault.key != nil else { return }
        let enc = JSONEncoder()
        let lines = closed + records.compactMap { m in (try? enc.encode(m)).flatMap { Vault.seal($0, for: groupId) } }
        guard lines.count == closed.count + records.count else { return }
        try? Data((lines.joined(separator: "\n") + (lines.isEmpty ? "" : "\n")).utf8).write(to: file, options: .atomic)
        Vault.lock(file)
    }

    @discardableResult
    private func fold(_ m: Msg) -> Bool {
        if m.kind == "reaction" {
            guard let t = m.target, let e = m.emoji else { return false }
            let key = "r\(t)|\(m.author)|\(e)|\(m.remove == true)|\(m.ts)"
            guard seen.insert(key).inserted else { return false }
            if m.remove == true { reactions[t]?[e]?.remove(m.author) }
            else { reactions[t, default: [:]][e, default: []].insert(m.author) }
            return true
        }
        guard seen.insert("\(m.ts)|\(m.author)").inserted else { return false }
        msgs.append(m)
        return true
    }

    func unread(since: Int64, me: String) -> Int {
        msgs.reversed().prefix { $0.ts > since }.filter { $0.author != me }.count
    }
}

// ─── people ──────────────────────────────────────────────────────────────────

struct Person {
    var name: String
    var color: NSColor?
    var badge: NSImage?
    var roles: [String] = []   // "Infra Circle: Secretary"
}

final class People {
    private var byACI: [String: Person] = [:]
    private var contacts: [String: String] = [:]

    init() {
        guard let p = Config.shared.conf.peopleLedger,
              let d = try? Data(contentsOf: Paths.expand(p)),
              let j = try? JSONSerialization.jsonObject(with: d) as? [String: Any],
              let people = j["people"] as? [String: [String: Any]] else { return }
        // Badge paths in the ledger are relative to the club directory.
        let base = Paths.expand(p).deletingLastPathComponent().deletingLastPathComponent()
        for (_, e) in people {
            guard let aci = (e["signal"] as? [String: Any])?["aci"] as? String else { continue }
            var person = Person(name: (e["name"] as? String) ?? "?")
            if let hex = (e["wiki"] as? [String: Any])?["color"] as? String { person.color = NSColor(hex: hex) }
            if let b = e["badge"] as? String {
                person.badge = NSImage(contentsOf: base.appendingPathComponent(b))
            }
            person.roles = (e["roles"] as? [String]) ?? []
            byACI[aci.lowercased()] = person
        }
    }

    func setContacts(_ list: [[String: Any]]) {
        for c in list {
            guard let u = c["uuid"] as? String else { continue }
            let profile = c["profile"] as? [String: Any]
            let given = (profile?["givenName"] as? String) ?? ""
            let family = (profile?["familyName"] as? String) ?? ""
            let n = [c["name"] as? String, [given, family].filter { !$0.isEmpty }.joined(separator: " ")]
                .compactMap { $0 }.first { !$0.isEmpty }
            if let n { contacts[u.lowercased()] = n }
        }
    }

    var wikiColors: [String: String] = [:]   // lowercase first name → hex, from the wiki

    func person(_ aci: String, fallback: String?) -> Person {
        if let p = byACI[aci.lowercased()] { return p }
        let n = contacts[aci.lowercased()] ?? fallback ?? "someone"
        var p = Person(name: n.isEmpty ? "someone" : n)
        if let first = p.name.split(separator: " ").first, let hex = wikiColors[first.lowercased()] {
            p.color = NSColor(hex: hex)
        }
        return p
    }

    // Role holders for one circle: [(role, person)] from "<key> Circle: <role>".
    func roles(forKey key: String) -> [(String, Person)] {
        var out: [(String, Person)] = []
        for p in byACI.values {
            for r in p.roles {
                let parts = r.split(separator: ":", maxSplits: 1).map { $0.trimmingCharacters(in: .whitespaces) }
                if parts.count == 2, parts[0].lowercased().hasPrefix(key.lowercased()) { out.append((parts[1], p)) }
            }
        }
        return out.sorted { $0.0 < $1.0 }
    }
}

extension NSColor {
    convenience init?(hex: String) {
        let h = hex.trimmingCharacters(in: CharacterSet(charactersIn: "#"))
        guard h.count == 6, let v = UInt32(h, radix: 16) else { return nil }
        self.init(srgbRed: CGFloat((v >> 16) & 0xFF) / 255, green: CGFloat((v >> 8) & 0xFF) / 255,
                  blue: CGFloat(v & 0xFF) / 255, alpha: 1)
    }
}
