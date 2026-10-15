import Foundation

enum AppRoute {
    static let prompt = "https://aesthetic.computer/prompt"

    // MIME owns these actions. Keep its post/version pair intact across apps.
    static func mimeFallback(for url: URL) -> URL? {
        guard url.scheme?.lowercased() == "mime", url.user == nil, url.password == nil,
              url.port == nil, url.path.isEmpty, url.fragment == nil,
              let parts = URLComponents(url: url, resolvingAgainstBaseURL: false) else { return nil }
        let query = parts.queryItems ?? []
        guard Set(query.map(\.name)).count == query.count else { return nil }
        switch url.host {
        case "create":
            guard query.allSatisfy({ $0.name == "type" }),
                  query.allSatisfy({ ($0.value ?? "").range(of: "^[A-Za-z0-9.+_-]+/[A-Za-z0-9.+_-]+$", options: .regularExpression) != nil }) else { return nil }
        case "remix":
            guard query.count == 2,
                  let post = query.first(where: { $0.name == "post" })?.value,
                  post.range(of: "^[A-Za-z0-9_-]{1,100}$", options: .regularExpression) != nil,
                  let version = query.first(where: { $0.name == "version" })?.value,
                  let number = Int(version), number >= 0 else { return nil }
            return URL(string: "https://mime.ac/" + post)
        default: return nil
        }
        return URL(string: "https://mime.ac/")
    }

    static func notificationDestination(_ payload: [AnyHashable: Any]) -> URL? {
        if payload["kind"] as? String == "tell" {
            var parts = URLComponents(string: "https://aesthetic.computer/mail")!
            if let id = payload["tellId"] as? String,
               id.range(of: "^[a-fA-F0-9]{24}$", options: .regularExpression) != nil {
                parts.queryItems = [URLQueryItem(name: "letter", value: id)]
            }
            return parts.url
        }
        guard let piece = payload["piece"] as? String, !piece.isEmpty,
              let url = URL(string: "aesthetic://" + piece),
              let destination = destination(for: url) else { return nil }
        return URL(string: destination)
    }

    static func windowTitle(for url: URL?) -> String {
        guard let url, url.scheme == "https", url.host == "aesthetic.computer" else {
            return "aesthetic.computer"
        }
        let piece = String(url.path.drop(while: { $0 == "/" }))
        return piece.isEmpty ? "prompt" : piece
    }

    static func destination(for url: URL) -> String? {
        if url.scheme?.lowercased() == "aesthetic" {
            // Preserve piece case and sigils instead of using URL.host, which
            // treats @ as credentials and can normalize a piece's name.
            let raw = url.absoluteString
            guard let separator = raw.range(of: "://") else { return nil }
            let piece = String(raw[separator.upperBound...])
            guard !piece.hasPrefix("/"), !piece.contains("\\") else { return nil }
            return piece.isEmpty ? prompt : "https://aesthetic.computer/" + piece
        }
        guard url.scheme == "https", url.host == "aesthetic.computer",
              url.user == nil, url.password == nil, url.port == nil || url.port == 443 else { return nil }
        return url.absoluteString
    }
}
