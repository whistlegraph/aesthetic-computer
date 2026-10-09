import Foundation

/// "Is the build I'm running behind main?" — the fleet question, answered
/// in the menubar. install.sh stamps the git commit, its count on the branch
/// and the branch name into Info.plist (`MBGitCommit`, `MBGitCommitCount`,
/// `MBGitBranch`). lith publishes the same two facts for the revision it is
/// serving (`/.commit-ref`, `/.commit-count`, written by lith/deploy.fish and
/// lith/webhook.sh). Commit counts on main only grow, so `served − built` is
/// how far behind this build is; equal refs mean exactly current. When the
/// build is behind, the music-note chip wears a small orange branch mark
/// (`KeyboardIconRenderer.behindMainBy`), and the status-item tooltip says
/// by how much. Not in the App Store build (no git there) and silent for
/// `swift run` dev binaries (no stamp).
enum RevisionChecker {
    static let refURL = URL(string: "https://aesthetic.computer/.commit-ref")!
    static let countURL = URL(string: "https://aesthetic.computer/.commit-count")!
    /// Half an hour — a deploy lands, the next check shows the mark.
    static let interval: TimeInterval = 30 * 60

    static var buildCommit: String? {
        Bundle.main.infoDictionary?["MBGitCommit"] as? String
    }
    static var buildCommitCount: Int? {
        guard let s = Bundle.main.infoDictionary?["MBGitCommitCount"] else { return nil }
        if let n = s as? Int { return n }
        return Int((s as? String) ?? "")
    }
    static var buildBranch: String? {
        Bundle.main.infoDictionary?["MBGitBranch"] as? String
    }

    /// Commits behind the served main; 0 = current; nil = unknown (not
    /// stamped, offline, or lith predates `.commit-count`).
    private(set) static var behindBy: Int?
    private(set) static var servedCommit: String?
    static var onChange: (() -> Void)?
    private static var timer: Timer?

    /// Human line for the tooltip, or nil when there is nothing to say.
    static var statusLine: String? {
        guard let behind = behindBy, behind > 0 else { return nil }
        let short = buildCommit.map { String($0.prefix(10)) } ?? "?"
        return "build \(short) is \(behind) commit\(behind == 1 ? "" : "s") behind main — rebuild"
    }

    static func start() {
        #if MAC_APP_STORE
        return
        #else
        guard timer == nil, buildCommit != nil, buildCommitCount != nil else { return }
        DispatchQueue.main.asyncAfter(deadline: .now() + 8) { check() }
        timer = Timer.scheduledTimer(withTimeInterval: interval, repeats: true) { _ in check() }
        #endif
    }

    static func check() {
        guard let built = buildCommit, let builtCount = buildCommitCount else { return }
        let group = DispatchGroup()
        var ref: String?
        var count: Int?
        func fetch(_ url: URL, _ into: @escaping (String) -> Void) {
            group.enter()
            var req = URLRequest(url: url, cachePolicy: .reloadIgnoringLocalAndRemoteCacheData,
                                 timeoutInterval: 15)
            req.setValue("no-cache", forHTTPHeaderField: "Cache-Control")
            URLSession.shared.dataTask(with: req) { data, resp, _ in
                defer { group.leave() }
                guard let data, let http = resp as? HTTPURLResponse, http.statusCode == 200,
                      let s = String(data: data, encoding: .utf8) else { return }
                into(s.trimmingCharacters(in: .whitespacesAndNewlines))
            }.resume()
        }
        fetch(refURL) { ref = $0 }
        fetch(countURL) { count = Int($0) }
        group.notify(queue: .main) {
            let before = behindBy
            servedCommit = ref
            if let ref, ref == built {
                behindBy = 0
            } else if let count {
                behindBy = max(0, count - builtCount)
            } else {
                behindBy = nil
            }
            if let behind = behindBy {
                NSLog("MenuBand revision: build \(built.prefix(10)) (#\(builtCount), \(buildBranch ?? "?")) vs main \(ref?.prefix(10) ?? "?") (#\(count.map(String.init) ?? "?")) → behind by \(behind)")
            }
            if before != behindBy { onChange?() }
        }
    }
}
