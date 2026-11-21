// Wiki.swift — the club wiki (Outline) beside the circles.
//
// Each circle gets the pages of the wiki collection that carries its name;
// a circle with no collection of its own shows the whole wiki. The wiki is
// opened by the NCC login (Login.swift): the token is the wiki session that
// login leaves, kept in the login Keychain. An API key set by hand
// (menu → Set Wiki Key…) works in its place.

import AppKit
import Security

// One line of the wiki column: a collection heading (no id) or a page at a depth.
struct WikiRow {
    let depth: Int
    let title: String
    let id: String?
}

final class Wiki {
    let base: String
    private let service = Paths.id + ".wiki"
    private var collections: [(id: String, name: String)] = []
    private(set) var userColors: [String: String] = [:]   // lowercase first name → hex
    private var collectionColors: [String: String] = [:]  // collection id → hex

    init() { base = Config.shared.conf.wikiURL ?? "https://wiki.nela.computer" }

    var token: String? {
        if ["1", "ncc"].contains(ProcessInfo.processInfo.environment["NCC_LOGGED_OUT"] ?? "") { return nil }   // test aid
        if let t = ProcessInfo.processInfo.environment["NCC_WIKI_TOKEN"], !t.isEmpty { return t }
        var q: [CFString: Any] = [kSecClass: kSecClassGenericPassword, kSecAttrService: service,
                                  kSecAttrAccount: base, kSecReturnData: true]
        var out: CFTypeRef?
        guard SecItemCopyMatching(q as CFDictionary, &out) == errSecSuccess, let d = out as? Data else { return nil }
        q.removeAll()
        return String(data: d, encoding: .utf8)
    }

    func setToken(_ t: String) {
        let q: [CFString: Any] = [kSecClass: kSecClassGenericPassword, kSecAttrService: service, kSecAttrAccount: base]
        SecItemDelete(q as CFDictionary)
        guard !t.isEmpty else { return }
        var add = q
        add[kSecValueData] = Data(t.utf8)
        SecItemAdd(add as CFDictionary, nil)
    }

    private func call(_ method: String, _ body: [String: Any], done: @escaping ([String: Any]?) -> Void) {
        guard let token, let url = URL(string: base + "/api/" + method) else { return done(nil) }
        var req = URLRequest(url: url)
        req.httpMethod = "POST"
        req.timeoutInterval = 20
        req.setValue("Bearer " + token, forHTTPHeaderField: "Authorization")
        req.setValue("application/json", forHTTPHeaderField: "Content-Type")
        req.setValue("nccd", forHTTPHeaderField: "User-Agent")
        req.httpBody = try? JSONSerialization.data(withJSONObject: body)
        URLSession.shared.dataTask(with: req) { data, _, _ in
            let j = data.flatMap { try? JSONSerialization.jsonObject(with: $0) as? [String: Any] }
            DispatchQueue.main.async { done(j) }
        }.resume()
    }

    // Collections and member colours, once per launch.
    func load(done: @escaping (Bool) -> Void) {
        call("collections.list", ["limit": 100]) { j in
            guard let list = j?["data"] as? [[String: Any]] else { return done(false) }
            self.collections = list.compactMap { c in
                guard let id = c["id"] as? String, let name = c["name"] as? String else { return nil }
                if let color = c["color"] as? String { self.collectionColors[id] = color }
                return (id, name)
            }
            self.call("users.list", ["limit": 100]) { j in
                for u in (j?["data"] as? [[String: Any]]) ?? [] {
                    guard let name = u["name"] as? String, let color = u["color"] as? String,
                          let first = name.split(separator: " ").first else { continue }
                    self.userColors[first.lowercased()] = color
                }
                done(true)
            }
        }
    }

    // The wiki's own full-text search: the pages, and the words round what was found.
    func search(_ query: String, done: @escaping ([(id: String, title: String, context: String)]) -> Void) {
        call("documents.search", ["query": query, "limit": 25]) { j in
            let rows = (j?["data"] as? [[String: Any]]) ?? []
            done(rows.compactMap { r in
                guard let d = r["document"] as? [String: Any], let id = d["id"] as? String else { return nil }
                let context = ((r["context"] as? String) ?? "")
                    .replacingOccurrences(of: "<[^>]+>", with: "", options: .regularExpression)
                return (id, (d["title"] as? String) ?? "(untitled)", context)
            })
        }
    }

    // Who the wiki says is signed in.
    func me(done: @escaping (String?) -> Void) {
        call("auth.info", [:]) { j in
            done(((j?["data"] as? [String: Any])?["user"] as? [String: Any])?["name"] as? String)
        }
    }

    // The collections whose name carries the circle's title ("Infra" → "Infrastructure Zone").
    func collections(for title: String) -> [(id: String, name: String)] {
        let key = title.lowercased()
        return collections.filter { $0.name.lowercased().contains(key) }
    }

    var allCollections: [(id: String, name: String)] { collections }

    // The colour the wiki gives a circle's collection, when it has one.
    func color(for title: String) -> String? {
        collections(for: title).compactMap { collectionColors[$0.id] }.first
    }

    // The page tree of some collections, in the order their editors keep them.
    // With more than one collection each gets a heading row.
    func tree(_ cols: [(id: String, name: String)], done: @escaping ([WikiRow]) -> Void) {
        var parts: [String: [WikiRow]] = [:]
        var left = cols.count
        guard left > 0 else { return done([]) }
        for c in cols {
            call("collections.documents", ["id": c.id]) { j in
                var rows: [WikiRow] = []
                func walk(_ nodes: [[String: Any]], _ depth: Int) {
                    for n in nodes {
                        guard let id = n["id"] as? String else { continue }
                        rows.append(WikiRow(depth: depth, title: (n["title"] as? String) ?? "(untitled)", id: id))
                        walk((n["children"] as? [[String: Any]]) ?? [], depth + 1)
                    }
                }
                walk((j?["data"] as? [[String: Any]]) ?? [], cols.count > 1 ? 1 : 0)
                parts[c.id] = rows
                left -= 1
                guard left == 0 else { return }
                var out: [WikiRow] = []
                for c in cols {
                    let rows = parts[c.id] ?? []
                    if cols.count > 1 {
                        if rows.isEmpty { continue }
                        out.append(WikiRow(depth: 0, title: c.name, id: nil))
                    }
                    out += rows
                }
                done(out)
            }
        }
    }

    // A page by its UUID or by the slug at the end of its address.
    func page(_ id: String, done: @escaping ((id: String, title: String, text: String, path: String)?) -> Void) {
        call("documents.info", ["id": id]) { j in
            guard let d = j?["data"] as? [String: Any], let text = d["text"] as? String else { return done(nil) }
            done(((d["id"] as? String) ?? id, (d["title"] as? String) ?? "", text, (d["url"] as? String) ?? ""))
        }
    }

    // Pictures on wiki pages, by the address the page gives them. One of the
    // wiki's own (a path, or an address on the wiki's host) is asked for with
    // the member's key; any other is fetched bare, so the key never leaves the wiki.
    private var pictures: [String: NSImage] = [:]
    private var asked: Set<String> = []
    func picture(_ address: String) -> NSImage? { pictures[address] }

    func fetchPictures(_ addresses: [String], each: @escaping () -> Void) {
        for address in addresses where !asked.contains(address) {
            asked.insert(address)
            let own = address.hasPrefix("/") || address.hasPrefix(base + "/")
            guard let url = URL(string: address.hasPrefix("/") ? base + address : address),
                  url.scheme == "https" else { continue }
            var req = URLRequest(url: url)
            req.timeoutInterval = 30
            if own, let token { req.setValue("Bearer " + token, forHTTPHeaderField: "Authorization") }
            URLSession.shared.dataTask(with: req) { data, _, _ in
                guard let data, let img = NSImage(data: data), img.size.width > 0 else { return }
                DispatchQueue.main.async {
                    self.pictures[address] = img
                    each()
                }
            }.resume()
        }
    }

    // The wiki's own front door: the page with this title, if the club keeps one.
    func startPage(titled title: String, done: @escaping (String?) -> Void) {
        call("documents.search_titles", ["query": title]) { j in
            let hits = (j?["data"] as? [[String: Any]]) ?? []
            let exact = hits.first { (($0["title"] as? String) ?? "").caseInsensitiveCompare(title) == .orderedSame }
            done(exact?["id"] as? String)
        }
    }
}
