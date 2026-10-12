import Foundation

enum AppRoute {
    static let prompt = "https://aesthetic.computer/prompt"

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
