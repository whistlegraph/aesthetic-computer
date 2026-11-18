// Store.swift — the message log and the people ledger.
//
// signal-cli keeps no history, so the app keeps its own: one append-only JSONL
// file per circle under the app's support directory. Reactions are records in
// the same file, folded onto their message when the log loads.

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
}

final class CircleLog {
    let groupId: String
    private(set) var msgs: [Msg] = []
    private(set) var reactions: [Int64: [String: Set<String>]] = [:]  // ts → emoji → authors
    private var seen = Set<String>()
    private let file: URL

    init(groupId: String) {
        self.groupId = groupId
        let h = SHA256.hash(data: Data(groupId.utf8)).prefix(8).map { String(format: "%02x", $0) }.joined()
        file = Paths.log.appendingPathComponent(h + ".jsonl")
        guard let text = try? String(contentsOf: file, encoding: .utf8) else { return }
        let dec = JSONDecoder()
        for line in text.split(separator: "\n") {
            if let m = try? dec.decode(Msg.self, from: Data(line.utf8)) { fold(m) }
        }
        msgs.sort { $0.ts < $1.ts }
    }

    // Returns false when the record was already in the log.
    @discardableResult
    func add(_ m: Msg) -> Bool {
        guard fold(m) else { return false }
        if let last = msgs.last, msgs.count > 1, last.ts < msgs[msgs.count - 2].ts {
            msgs.sort { $0.ts < $1.ts }
        }
        if let d = try? JSONEncoder().encode(m), let line = String(data: d, encoding: .utf8) {
            if let h = try? FileHandle(forWritingTo: file) {
                h.seekToEndOfFile()
                h.write(Data((line + "\n").utf8))
                try? h.close()
            } else {
                try? Data((line + "\n").utf8).write(to: file)
            }
        }
        return true
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
