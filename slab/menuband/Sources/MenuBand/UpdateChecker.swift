import Foundation

/// Polls a small JSON manifest hosted on the AC CDN to decide whether
/// there's a newer Menu Band release than the one running. The manifest
/// lives at `assets.aesthetic.computer/menuband/latest.json` and is the
/// only side-effect of a release — bumping its `version` field is what
/// triggers the in-popover update banner.
///
/// Manifest schema (kept tiny so the file caches well on Cloudflare):
/// ```json
/// { "version": "0.1", "url": "https://aesthetic.computer/menuband",
///   "notes": "First release." }
/// ```
enum UpdateChecker {
    static let manifestURL = URL(string: "https://assets.aesthetic.computer/menuband/latest.json")!

    struct VersionInfo: Codable {
        let version: String
        let url: String?
        let notes: String?
    }

    /// In-memory cache. Refresh policy: bypass when older than 1h. The
    /// request itself uses `reloadIgnoringLocalCacheData` so the OS HTTP
    /// cache doesn't pin a stale manifest after a release.
    private static var cached: VersionInfo?
    private static var lastCheck: Date?

    static func currentVersion() -> String {
        (Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String) ?? "0.0"
    }

    /// Fetch the latest manifest. Calls back on the main thread. Returns
    /// nil on network/parse failure (silent fallback — we don't want to
    /// pester the user when their wi-fi is down).
    static func fetchLatest(completion: @escaping (VersionInfo?) -> Void) {
        if let info = cached, let ts = lastCheck, Date().timeIntervalSince(ts) < 3600 {
            completion(info)
            return
        }
        var req = URLRequest(url: manifestURL)
        req.cachePolicy = .reloadIgnoringLocalCacheData
        req.timeoutInterval = 8
        URLSession.shared.dataTask(with: req) { data, _, _ in
            var parsed: VersionInfo?
            if let data = data {
                parsed = try? JSONDecoder().decode(VersionInfo.self, from: data)
            }
            if let info = parsed {
                cached = info
                lastCheck = Date()
            }
            DispatchQueue.main.async { completion(parsed) }
        }.resume()
    }

    /// `true` iff `latest > current` under naive dotted-integer ordering.
    /// "0.2" > "0.1", "1.0" > "0.99", "0.1.1" > "0.1". Missing components
    /// count as 0 so "0.1" and "0.1.0" compare equal.
    static func isNewer(_ latest: String, than current: String) -> Bool {
        let l = latest.split(separator: ".").map { Int($0) ?? 0 }
        let c = current.split(separator: ".").map { Int($0) ?? 0 }
        let count = Swift.max(l.count, c.count)
        for i in 0..<count {
            let lv = i < l.count ? l[i] : 0
            let cv = i < c.count ? c[i] : 0
            if lv > cv { return true }
            if lv < cv { return false }
        }
        return false
    }
}
