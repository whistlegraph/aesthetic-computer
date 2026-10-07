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
            return document == .workspace ? [["whistlegraph": "1"], ["walkie": "1"]].contains(query(url) ?? [:]) : url.query == nil
        }
        // Creating an iframe can first navigate its empty document.
        if url.absoluteString == "about:blank" { return true }
        return url.scheme == "https" && url.host == "aesthetic.computer" && url.path == "/wipe"
            && ["whistlegraph", "walkieware"].contains(where: { name in
                query(url) == ["noauth": "true", "noplot": "true", "nogap": "true", "nolabel": "true", "preview": name]
            })
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
