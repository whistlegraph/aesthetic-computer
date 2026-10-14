import Foundation
import Combine

/// One card of the network feed, as GET mime.ac/api/mime?feed=1 returns it.
struct Post: Codable, Identifiable, Equatable {
    struct LetterRGB: Codable, Equatable { let r: Double; let g: Double; let b: Double }
    struct File: Codable, Equatable { let name: String?; let type: String; let url: String; let poster: String?; let alt: String? }
    struct Media: Codable, Equatable {
        let kind: String; let code: String?; let title: String?; let url: String?
        let packed: Bool?; let aspect: String?
        let version: Int?
    }
    struct Embed: Codable, Equatable { let kind: String; let id: String; let start: Int?; let url: String?; let caption: String? }
    let embed: Embed?
    struct Origin: Codable, Equatable { let name: String; let url: String? }
    let code: String
    let name: String?
    let nameColors: [LetterRGB?]?
    let text: String?
    let when: String?
    let board: String?
    let file: File?
    let media: Media?
    let version: Int?
    let origin: Origin?
    var id: String { code }

    /// The work's global identifier, retaining its media prefix.
    var command: String {
        if isNES { return "nes~" + self.code }
        if ["application/x-doom-wad", "application/x-doom"].contains(mimeType) { return "doom~" + self.code }
        guard let code = media?.code, !code.isEmpty else { return self.code }
        switch media?.kind {
        case "painting": return "#" + code
        case "tape": return "!" + code
        case "kidlisp": return "$" + code
        case "mood": return "mood~" + code
        case "chat": return "chat~" + code
        default: return code
        }
    }
    var mimeType: String { file?.type ?? board ?? "text/plain" }
    /// Meaning is separate from the bytes' media type.
    var typeLabel: String {
        switch media?.kind {
        case "mood": return "mood"
        case "chat": return "chat"
        default: return mimeType
        }
    }
    var playable: Bool {
        switch body {
        case .runtime(_, kind: "oskiewar"): return false
        case .runtime, .packed, .video: return true
        default: return false
        }
    }
    var isNES: Bool {
        ["application/x-nes-rom", "application/x-nes", "application/x-nintendo-nes-rom"].contains(mimeType) || file?.name?.lowercased().hasSuffix(".nes") == true
    }
    var isMood: Bool { media?.kind == "mood" }

    /// What draws the card.
    enum Body { case image(URL), video(URL), runtime(URL, kind: String), packed(URL), words(String), document(URL) }
    var body: Body {
        let base = URL(string: "https://mime.ac")!
        func absolute(_ s: String) -> URL? { URL(string: s, relativeTo: base)?.absoluteURL }
        if let embed, embed.kind == "youtube", embed.id.range(of: "^[A-Za-z0-9_-]{11}$", options: .regularExpression) != nil {
            var player = URLComponents(string: "https://aesthetic.computer/mime-youtube/")!
            player.queryItems = [.init(name: "v", value: embed.id), .init(name: "start", value: String(min(86400, max(0, embed.start ?? 0)))), .init(name: "warm", value: "1")]
            return .runtime(player.url!, kind: "youtube")
        }
        if isMood { return .words(text ?? "") }
        if let media, let code = media.code {
            switch media.kind {
            case "kidlisp":
                var player = URLComponents(string: "https://aesthetic.computer/mime-play/")!
                player.queryItems = [.init(name: "piece", value: "$" + code), .init(name: "warm", value: "1")]
                return .runtime(player.url!, kind: "kidlisp")
            case "piece":
                // A registered piece runs at its handle address (@jeffrey/loredi),
                // which the feed hands over as the post's URL; the code alone does not boot.
                let path = media.url.flatMap { URL(string: $0) }.flatMap { $0.host == "aesthetic.computer" && $0.path.count > 1 ? $0.path : nil } ?? "/" + code
                var player = URLComponents(string: "https://aesthetic.computer/mime-play/")!
                player.queryItems = [.init(name: "piece", value: String(path.dropFirst())), .init(name: "warm", value: "1")]
                return .runtime(player.url!, kind: "piece")
            case "wgpiece": return .runtime(absolute(media.url ?? "") ?? base, kind: "wgpiece")
            case "oskiewar":
                if var player = media.url.flatMap({ URLComponents(string: $0) }),
                   player.scheme == "https", player.host == "oskiewar.com" {
                    player.queryItems = (player.queryItems ?? []).filter { $0.name != "warm" } + [.init(name: "warm", value: "1")]
                    if let url = player.url { return .runtime(url, kind: "oskiewar") }
                }
            case "whistlegraph" where media.packed == true:
                if let url = media.url.flatMap(absolute) { return .packed(url) }
            default: break
            }
        }
        guard let file, let url = absolute(file.url) else { return .words(text ?? "") }
        if ["application/x-doom-wad", "application/x-doom"].contains(file.type) {
            var player = URLComponents(string: "https://aesthetic.computer/doom-player/")!
            player.queryItems = [.init(name: "preview", value: "1"), .init(name: "mime", value: "1"), .init(name: "file", value: url.absoluteString)]
            return .runtime(player.url!, kind: "doom")
        }
        if isNES {
            var player = URLComponents(string: "https://aesthetic.computer/nes-player/")!
            player.queryItems = [.init(name: "warm", value: "1"), .init(name: "file", value: url.absoluteString)]
            return .runtime(player.url!, kind: "nes")
        }
        if file.type.hasPrefix("image/") { return .image(url) }
        if file.type.hasPrefix("video/") { return .video(url) }
        return .document(url)
    }
}

/// The feed for this launch: its own seed, so this launch keeps one order.
@MainActor final class Feed: ObservableObject {
    @Published private(set) var posts: [Post] = []
    @Published private(set) var failed = false
    @Published private(set) var loaded = false
    @Published private(set) var layout: MimeLayout = {
        if let data = UserDefaults.standard.data(forKey: "mime-layout-v1"), let cached = MimeLayout.decode(data) { return cached }
        return .bundled
    }()
    /// Authors this viewer hid; their cards never enter this phone's feed.
    private var hidden = Set(UserDefaults.standard.stringArray(forKey: "mime-hidden-v1") ?? [])
    private var page = 0, hasMore = true, loading = false
    private let seed = UUID().uuidString

    init() {
        #if DEBUG
        if ProcessInfo.processInfo.environment["MIME_MOOD_FIXTURE"] != nil {
            let json = """
            [{"code":"mood_fixture","name":"@fixture","board":"text/plain","text":"A little room for a mood.","media":{"kind":"mood","code":"abc"}},
             {"code":"chat_fixture","name":"@fixture","board":"text/plain","text":"Hello from chat.","media":{"kind":"chat","code":"xyz"}}]
            """
            posts = try! JSONDecoder().decode([Post].self, from: Data(json.utf8))
            loaded = true; hasMore = false
        }
        if ProcessInfo.processInfo.environment["MIME_INTERACTION_FIXTURE"] == "1" {
            let json = """
            [{"code":"fixture-first","name":"@fixture","media":{"kind":"wgpiece","code":"wgFirst","url":"https://example.invalid/mime-fixture/first"}},
             {"code":"fixture-second","name":"@fixture","media":{"kind":"wgpiece","code":"wgSecond","url":"https://example.invalid/mime-fixture/second"}}]
            """
            posts = try! JSONDecoder().decode([Post].self, from: Data(json.utf8))
            loaded = true; hasMore = false
        }
        if ProcessInfo.processInfo.environment["MIME_CREATION_FIXTURE"] != nil {
            let data = Data("[{\"code\":\"fixture-png\",\"name\":\"@fixture\",\"board\":\"image/png\",\"text\":\"PNG\"},{\"code\":\"fixture-piece\",\"name\":\"@fixture\",\"board\":\"text/javascript\",\"text\":\"AC piece\"}]".utf8)
            posts = try! JSONDecoder().decode([Post].self, from: data)
            if ProcessInfo.processInfo.environment["MIME_CREATION_FIXTURE"] == "doom" {
                posts = [try! JSONDecoder().decode(Post.self, from: Data("{\"code\":\"fixture-wad\",\"name\":\"@fixture\",\"file\":{\"name\":\"empty-room.wad\",\"type\":\"application/x-doom-wad\",\"url\":\"https://aesthetic.computer/doom-player/examples/empty-room.wad\"}}".utf8))]
            }
            loaded = true; hasMore = false
            if ["remix", "progress"].contains(ProcessInfo.processInfo.environment["MIME_CREATION_FIXTURE"] ?? "") {
                let json = """
                [{"code":"fixture-remix-piece","name":"@original","version":7,"text":"Source piece","file":{"name":"original.ac","type":"text/javascript","url":"https://example.invalid/original.ac"}},
                 {"code":"fixture-remix-png","name":"@painter","version":3,"file":{"name":"Blue.png","type":"image/png","url":"https://example.invalid/blue.png"}},
                 {"code":"fixture-remix-wad","name":"@builder","version":2,"file":{"name":"Room.wad","type":"application/x-doom-wad","url":"https://aesthetic.computer/doom-player/examples/empty-room.wad"}}]
                """
                posts = try! JSONDecoder().decode([Post].self, from: Data(json.utf8))
            }
        }
        #endif
    }

    private struct Page: Decodable { let recent: [Post]; let hasMore: Bool }

    func include(_ post: Post) {
        posts.removeAll { $0.id == post.id }
        posts.insert(post, at: 0)
    }

    func retry() async {
        guard !loading else { return }
        hasMore = true
        if posts.isEmpty { page = 0 }
        await loadMore()
    }

    func refreshLayout() async {
        let url = URL(string: "https://aesthetic.computer/mime-layout.json")!
        do {
            let (data, response) = try await URLSession.shared.data(for: URLRequest(url: url, cachePolicy: .reloadIgnoringLocalCacheData, timeoutInterval: 8))
            guard (response as? HTTPURLResponse)?.statusCode == 200, let value = MimeLayout.decode(data) else { return }
            layout = value
            UserDefaults.standard.set(data, forKey: "mime-layout-v1")
        } catch { /* Preserve the last valid layout when offline. */ }
    }

    func hide(_ name: String) {
        hidden.insert(name)
        UserDefaults.standard.set(Array(hidden), forKey: "mime-hidden-v1")
        posts.removeAll { $0.name == name }
        if posts.count < 4 { Task { await loadMore() } }
    }

    func loadMore() async {
        guard hasMore, !loading else { return }
        loading = true; defer { loading = false }
        var components = URLComponents(string: "https://mime.ac/api/mime")!
        components.queryItems = [.init(name: "feed", value: "1"), .init(name: "page", value: String(page)), .init(name: "seed", value: seed)]
        do {
            let (data, response) = try await URLSession.shared.data(from: components.url!)
            guard (response as? HTTPURLResponse)?.statusCode == 200 else { throw URLError(.badServerResponse) }
            let next = try JSONDecoder().decode(Page.self, from: data)
            let known = Set(posts.map(\.code))
            posts += next.recent.filter { !known.contains($0.code) && !hidden.contains($0.name ?? "") }
            hasMore = next.hasMore; page += 1; failed = false; loaded = true
        } catch { failed = posts.isEmpty }
    }
}
