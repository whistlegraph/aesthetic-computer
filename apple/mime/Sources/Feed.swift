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
    let code: String
    let name: String?
    let nameColors: [LetterRGB?]?
    let text: String?
    let when: String?
    let board: String?
    let file: File?
    let media: Media?
    let version: Int?
    var id: String { code }

    /// The code you can enter in AC: #painting, !tape, $kidlisp, a piece by name.
    var command: String {
        if ["application/x-doom-wad", "application/x-doom"].contains(mimeType) { return "doom~" + self.code }
        guard let code = media?.code, !code.isEmpty else { return self.code }
        switch media?.kind {
        case "painting": return "#" + code
        case "tape": return "!" + code
        case "kidlisp": return "$" + code
        case "mood": return "mood~" + code
        default: return code
        }
    }
    var mimeType: String { file?.type ?? board ?? "text/plain" }
    var isMood: Bool { media?.kind == "mood" }

    /// What draws the card.
    enum Body { case image(URL), video(URL), runtime(URL, kind: String), packed(URL), words(String), document(URL) }
    var body: Body {
        let base = URL(string: "https://mime.ac")!
        func absolute(_ s: String) -> URL? { URL(string: s, relativeTo: base)?.absoluteURL }
        if isMood { return .words(text ?? "") }
        if let media, let code = media.code {
            switch media.kind {
            case "kidlisp": return .runtime(URL(string: "https://aesthetic.computer/$\(code)?nogap=true&nolabel=true&noauth=true")!, kind: "kidlisp")
            case "piece": return .runtime(URL(string: "https://aesthetic.computer/\(code)?nogap=true&nolabel=true&noauth=true")!, kind: "piece")
            case "wgpiece": return .runtime(absolute(media.url ?? "") ?? base, kind: "wgpiece")
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
    private var page = 0, hasMore = true, loading = false
    private let seed = UUID().uuidString

    init() {
        #if DEBUG
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
            posts += next.recent.filter { !known.contains($0.code) }
            hasMore = next.hasMore; page += 1; failed = false; loaded = true
        } catch { failed = posts.isEmpty }
    }
}
