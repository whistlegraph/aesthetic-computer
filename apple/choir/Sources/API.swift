import Foundation

struct Post: Codable, Identifiable, Hashable {
    let id: String
    let handle: String
    let text: String
    let tags: [String]
    let links: [String]
    let replyTo: String?
    let replies: Int
    let lastReplyAt: String?
    let when: String
    let deleted: Bool
    var preview: ArticleMeta? = nil   // news items arrive with their card
    var kind: String? = nil           // "opportunity" for open calls
    var date: Date { Post.iso.date(from: when) ?? .distantPast }
    private static let iso: ISO8601DateFormatter = { let f = ISO8601DateFormatter(); f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]; return f }()
}
struct TagCount: Codable, Hashable { let tag: String; let count: Int }
struct Me: Codable {
    struct Prefs: Codable { let notifications: Bool; var chat: Bool? = nil; var discover: Bool? = nil }
    let handle: String?
    let prefs: Prefs
}

/// The choir API at aesthetic.computer/api/choir (system/netlify/functions/choir.mjs).
enum API {
    static let base = URL(string: "https://aesthetic.computer/api/choir")!
    enum Failure: LocalizedError {
        case status(Int, String)
        var errorDescription: String? { if case .status(_, let m) = self { return m }; return nil }
    }

    static func request(_ method: String, _ query: String, body: Data? = nil, token: String?) -> URLRequest {
        var r = URLRequest(url: URL(string: base.absoluteString + query)!)
        r.httpMethod = method
        r.timeoutInterval = 20
        r.setValue(Trail.session, forHTTPHeaderField: "x-choir-session")
        if let token { r.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization") }
        if let body { r.httpBody = body; r.setValue("application/json", forHTTPHeaderField: "Content-Type") }
        return r
    }

    static func run<T: Decodable>(_ r: URLRequest) async throws -> T {
        let (data, response) = try await URLSession.shared.data(for: r)
        let status = (response as? HTTPURLResponse)?.statusCode ?? 0
        guard status == 200 else {
            let message = (try? JSONSerialization.jsonObject(with: data) as? [String: Any])?["error"] as? String
            throw Failure.status(status, message ?? "Something went wrong (\(status))")
        }
        return try JSONDecoder().decode(T.self, from: data)
    }

    static func get<T: Decodable>(_ query: String, token: String? = nil) async throws -> T {
        try await run(request("GET", query, token: token))
    }
    static func send<B: Encodable, T: Decodable>(_ method: String, _ query: String, _ body: B, token: String?) async throws -> T {
        try await run(request(method, query, body: try JSONEncoder().encode(body), token: token))
    }

    struct Feed: Decodable { let posts: [Post]; let next: String? }
    struct Thread: Decodable { let root: Post; let replies: [Post] }
    struct Tags: Decodable { let tags: [TagCount] }

    static func feed(tag: String?) async throws -> [Post] {
        let q = tag.map { "?tag=" + ($0.addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed) ?? $0) } ?? ""
        let f: Feed = try await get(q); return f.posts
    }
    static func thread(_ id: String) async throws -> Thread { try await get("?thread=\(id)") }
    static func news() async throws -> [Post] { let f: Feed = try await get("?news=1&limit=50"); return f.posts }
    static func tags() async throws -> [TagCount] { let t: Tags = try await get("?tags=1"); return t.tags }

    struct NewPost: Encodable { let text: String; let tags: [String]; let replyTo: String? }
    struct Posted: Decodable { let post: Post }
    static func post(_ p: NewPost, token: String?) async throws -> Post {
        let r: Posted = try await send("POST", "", p, token: token); return r.post
    }
    struct Deleted: Decodable { let deleted: Bool }
    static func delete(_ id: String, token: String?) async throws {
        let _: Deleted = try await run(request("DELETE", "?id=\(id)", token: token))
    }
}

/// The live trail (choir-events). One session id per launch ties these to the server's own.
enum Trail {
    static let session = UUID().uuidString
    static func send(_ name: String, _ props: [String: Any] = [:]) {
        let body: [String: Any] = ["name": name, "props": props, "session": session, "surface": "ios"]
        guard let data = try? JSONSerialization.data(withJSONObject: body) else { return }
        let r = API.request("POST", "?event=1", body: data, token: nil)
        Task.detached { _ = try? await URLSession.shared.data(for: r) }
    }
}

enum Tag {
    /// Same spelling rule as the server: "Free & For Sale" → "free-and-for-sale".
    static func normalize(_ raw: String) -> String {
        let s = raw.lowercased().replacingOccurrences(of: "&", with: "and")
        let dashed = s.unicodeScalars.map { CharacterSet.alphanumerics.contains($0) && $0.isASCII ? String($0) : "-" }.joined()
        return String(dashed.split(separator: "-").joined(separator: "-").prefix(32))
    }
}
