import Foundation

/// One card of the network feed, as GET mime.ac/api/mime?feed=1 returns it.
struct Post: Decodable, Identifiable, Equatable {
    struct File: Decodable, Equatable { let name: String?; let type: String; let url: String; let poster: String?; let alt: String? }
    struct Media: Decodable, Equatable {
        let kind: String; let code: String?; let title: String?; let url: String?
        let packed: Bool?; let aspect: String?
    }
    let code: String
    let name: String?
    let text: String?
    let when: String?
    let board: String?
    let file: File?
    let media: Media?
    var id: String { code }

    /// The code you can enter in AC: #painting, !tape, $kidlisp, a piece by name.
    var command: String {
        if let title = media?.title, !title.isEmpty { return title }
        guard let code = media?.code else { return media?.kind ?? "post" }
        switch media?.kind {
        case "painting": return "#" + code
        case "tape": return "!" + code
        case "kidlisp": return "$" + code
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
        if file.type.hasPrefix("image/") { return .image(url) }
        if file.type.hasPrefix("video/") { return .video(url) }
        return .document(url)
    }
}

/// The feed for this launch: its own seed, so this launch keeps one order.
@MainActor final class Feed: ObservableObject {
    @Published private(set) var posts: [Post] = []
    @Published private(set) var failed = false
    private var page = 0, hasMore = true, loading = false
    private let seed = UUID().uuidString

    private struct Page: Decodable { let recent: [Post]; let hasMore: Bool }

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
            hasMore = next.hasMore; page += 1; failed = false
        } catch { failed = posts.isEmpty }
    }
}
