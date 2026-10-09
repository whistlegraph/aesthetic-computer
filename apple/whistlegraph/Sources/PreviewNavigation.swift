import Foundation

/// These WebViews render artwork; account and purchase UI have separate native
/// entry points. Resource requests are unaffected by this document allowlist.
enum PreviewNavigation {
    enum Document: String { case workspace = "/index.html", story = "/story.html" }

    enum BridgeScope { case workspace, artwork, none }
    static func bridge(_ url: URL?, mainFrame: Bool, document: Document) -> BridgeScope {
        guard allows(url, mainFrame: mainFrame, document: document) else { return .none }
        if mainFrame { return document == .workspace ? .workspace : .none }
        return url?.scheme == "https" ? .artwork : .none
    }

    static func allows(_ url: URL?, mainFrame: Bool?, document: Document) -> Bool {
        guard let url, let mainFrame, url.user == nil, url.password == nil, url.port == nil else { return false }
        if mainFrame {
            guard url.scheme == "walkieware", url.host == "app", url.path == document.rawValue else { return false }
            if document == .story { return url.query == nil }
            // The storage origin stays put across the Whistlegraph rename.
            return query(url) == ["whistlegraph": "1"] || query(url) == ["walkie": "1"]
        }
        // Creating an iframe can first navigate its empty document.
        if url.absoluteString == "about:blank" { return true }
        // The runtime rewrites its own URL after boot (the piece's path, flags reordered, noplot
        // dropped), so the artwork frame is known by host + the private preview flags, never by
        // the exact URL: matching the launch URL alone dropped every message after boot (2026-10-09).
        guard url.scheme == "https", url.host == "aesthetic.computer", let flags = query(url) else { return false }
        let piece = url.path.dropFirst()
        guard url.path.hasPrefix("/"), !piece.contains("/"),
              piece.allSatisfy({ $0.isLetter || $0.isNumber || "._-".contains($0) }) else { return false }
        let required = ["noauth": "true", "nogap": "true", "nolabel": "true", "preview": "walkieware"]
        for (key, value) in required where flags[key] != value { return false }
        for (key, value) in flags where required[key] == nil { guard key == "noplot", value == "true" else { return false } }
        return true
    }
    private static func query(_ url: URL) -> [String: String]? {
        guard let items = URLComponents(url: url, resolvingAgainstBaseURL: false)?.queryItems else { return nil }
        var result: [String: String] = [:]
        for item in items {
            guard result[item.name] == nil, let value = item.value else { return nil }
            result[item.name] = value
        }
        return result
    }
}
