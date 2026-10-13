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
    var attachments: [Attachment]? = nil
    /// Never in the future: a phone clock a little behind the server read "in 0s".
    var date: Date { min(Post.iso.date(from: when) ?? .distantPast, .now) }
    private static let iso: ISO8601DateFormatter = { let f = ISO8601DateFormatter(); f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]; return f }()
}
/// A file in the "choir" Spaces bucket, attached to a post.
struct Attachment: Codable, Hashable {
    let url: String
    let type: String
    let size: Int
    let name: String
    var width: Int? = nil
    var height: Int? = nil
    var duration: Double? = nil      // voice notes
    var transcript: String? = nil    // voice notes, transcribed on the phone
    var isImage: Bool { type.hasPrefix("image/") }
    var isVoice: Bool { type.hasPrefix("audio/") && duration != nil }
}

struct TagCount: Codable, Hashable { let tag: String; let count: Int }
struct Me: Codable {
    struct Prefs: Codable { let notifications: Bool; var chat: Bool? = nil; var discover: Bool? = nil }
    struct Agreed: Codable { let current: Bool; var banned: Bool? = nil }
    let handle: String?
    let prefs: Prefs
    var terms: Agreed? = nil   // choir.ac/terms, once per version
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

    struct NewPost: Encodable { let text: String; let tags: [String]; let replyTo: String?; var attachments: [Attachment]? = nil }

    /// Ask for a 15-minute upload link, PUT the bytes to the choir bucket, return the attachment.
    static func upload(_ data: Data, name: String, type: String, width: Int? = nil, height: Int? = nil,
                       duration: Double? = nil, transcript: String? = nil, token: String?) async throws -> Attachment {
        struct Ask: Encodable { let name: String; let type: String; let size: Int }
        struct Signed: Decodable { let uploadUrl: String; let url: String; let headers: [String: String] }
        let signed: Signed = try await send("POST", "?upload=1", Ask(name: name, type: type, size: data.count), token: token)
        var put = URLRequest(url: URL(string: signed.uploadUrl)!)
        put.httpMethod = "PUT"; put.timeoutInterval = 120
        for (k, v) in signed.headers { put.setValue(v, forHTTPHeaderField: k) }
        let (_, resp) = try await URLSession.shared.upload(for: put, from: data)
        guard (resp as? HTTPURLResponse)?.statusCode == 200 else { throw Failure.status((resp as? HTTPURLResponse)?.statusCode ?? 0, "Upload failed") }
        return Attachment(url: signed.url, type: type, size: data.count, name: name, width: width, height: height,
                          duration: duration, transcript: transcript)
    }
    struct Posted: Decodable { let post: Post }
    static func post(_ p: NewPost, token: String?) async throws -> Post {
        let r: Posted = try await send("POST", "", p, token: token); return r.post
    }
    /// "2 here · 3 members · 4 invited". Signed in, it also says you're here.
    struct Here: Decodable { let members: Int; let here: Int; let invited: Int; let joined: Int }
    static func here(token: String?) async throws -> Here { try await get("?here=1", token: token) }

    /// Each member has a few invites (Fía, unlimited): a code and a choir.ac link.
    struct Invite: Decodable, Identifiable, Hashable { let code: String; let url: String; let name: String?; let at: String; let joined: String?; var id: String { code } }
    struct Invites: Decodable { let invites: [Invite]; let left: Int? }
    static func invites(token: String?) async throws -> Invites { try await get("?invites=1", token: token) }
    struct Made: Decodable { let _id: String; let url: String; let left: Int? }
    static func invite(name: String, token: String?) async throws -> Made {
        struct Ask: Encodable { let name: String }
        return try await send("POST", "?invite=1", Ask(name: name), token: token)
    }
    struct Claimed: Decodable { let from: String }
    static func claim(_ code: String, token: String?) async throws -> Claimed {
        struct Ask: Encodable { let code: String }
        return try await send("POST", "?claim=1", Ask(code: code), token: token)
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
