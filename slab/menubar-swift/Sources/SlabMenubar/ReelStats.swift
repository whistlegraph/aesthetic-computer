// ReelStats — the data half of the Reels player (ReelsPlayerView.swift):
// what the oskiewar reel factory knows about each rendered reel, read
// strictly from its own local JSON records — no network, no tokens, ever.
// Two sources merge on reel id:
//
//   • the publish ledger — one post per publish; `refreshInsights` hangs
//     each post's Instagram numbers on it later. `insights: null` means
//     Meta has not measured yet, and that nil is preserved all the way to
//     the view (which renders "—", never a fake zero).
//   • <queue>/<id>/reel.json — the staged render's local media + meta
//     (mp4 path, duration, dimensions, size, caption). The queue is
//     machine-local build output and may be missing entirely; a reel with
//     no staged copy still lists from the ledger alone, and vice versa.
//
// Both trees live outside this repo. Paths default from Paths.acRepo and
// can be repointed per host in untracked ~/.config/slab/reels.json:
//   { "ledger": "/abs/path/ledger.json", "queue": "/abs/path/queue" }
import Foundation

extension Paths {
    /// Untracked per-host override for the ledger + queue locations —
    /// same convention as imsgConfig: plain local paths, never secrets.
    static var reelsConfig: String { "\(home)/.config/slab/reels.json" }
}

/// One post's Instagram numbers, exactly the ledger's `insights` object
/// (publish.mjs `reelMetrics`). Every field is optional because Meta only
/// returns a metric once it has computed it; nil reads as "not measured".
/// All numerics decode as Double so a drifting int/float wire shape can't
/// break the whole ledger read.
struct ReelInsights: Decodable {
    var views: Double?
    var reach: Double?
    var likes: Double?
    var comments: Double?
    var saved: Double?
    var shares: Double?
    var totalInteractions: Double?
    var reposts: Double?
    var avgWatchTimeMs: Double?
    var viewTotalTimeMs: Double?
    var skipRate: Double?

    enum CodingKeys: String, CodingKey {
        case views, reach, likes, comments, saved, shares, reposts
        case totalInteractions = "total_interactions"
        case avgWatchTimeMs = "ig_reels_avg_watch_time"
        case viewTotalTimeMs = "ig_reels_video_view_total_time"
        case skipRate = "reels_skip_rate"
    }
}

/// One ledger entry — a single publish of a reel. The same reel id can
/// appear more than once (re-published with a different round/audio).
///
/// Fields are optional past `nothing`: the oskiewar factory stamps every post
/// with `mode`/`id`/`segment`, while ig.mjs's per-account ledgers carry only
/// what a hand-published reel knows (media id, caption, permalink). A missing
/// `id` is normal there, so identity falls back to the media id — see
/// `resolvedID`. Requiring either would make one ledger's shape fail the whole
/// decode and silently empty the window.
struct ReelPost: Decodable {
    var mode: String?
    var id: String?
    var segment: String?
    var day: String?
    var publishedAt: String?
    var mediaId: String?
    var permalink: String?
    var caption: String?
    var audioName: String?
    var source: String?
    var urls: [String: String]?
    var insights: ReelInsights?
    var insightsAt: String?

    /// Which account published it. Not in the file — stamped from the ledger
    /// it was read out of, so one merged list can still say where a reel went.
    var account: String = ""

    enum CodingKeys: String, CodingKey {
        case mode, id, segment, day, publishedAt, mediaId, permalink
        case caption, audioName, source, urls, insights, insightsAt
    }

    /// Stable identity across both ledger shapes.
    var resolvedID: String { id ?? mediaId ?? permalink ?? "" }

    /// The oskiewar factory records dry runs too and marks the real ones
    /// `live`; the per-account ledgers only ever record real publishes, so an
    /// absent `mode` means live rather than unknown.
    var isLive: Bool { (mode ?? "live") == "live" }

    /// `day` is a factory field. Elsewhere the publish timestamp carries it.
    var resolvedDay: String { day ?? String((publishedAt ?? "").prefix(10)) }
}

/// A ledger to merge, and the account to stamp on everything inside it.
struct ReelLedgerSource {
    var account: String
    var path: String
}

/// The staged render's `meta` block (ffprobe-derived).
struct ReelMeta: Decodable {
    var width: Double?
    var height: Double?
    var seconds: Double?
    var megabytes: Double?
    var fps: Double?
}

/// The slice of <queue>/<id>/reel.json the player needs.
struct ReelQueueEntry: Decodable {
    var id: String
    var segment: String?
    var segmentName: String?
    var day: String?
    var caption: String?
    var audioName: String?
    var builtAt: String?
    var files: [String: String]?
    var meta: ReelMeta?
}

/// One reel as the player sees it: staged media (when this machine has the
/// render) + the latest live post (when it has been published). Either half
/// may be absent.
struct Reel: Identifiable {
    let id: String
    var account: String
    var segment: String
    var day: String
    var segmentName: String?
    var caption: String?
    var audioName: String?
    var localVideo: String?      // playable mp4 on disk, or nil
    var thumbnail: String?       // small local jpg for the row, or nil
    var meta: ReelMeta?
    var post: ReelPost?          // latest live post for this id
    var postCount: Int           // live publishes sharing this id

    /// What to call it in a list. Factory ids already read as names
    /// (`2026-08-13-s0-retro`), but an ig.mjs reel is keyed by an opaque media
    /// id — so prefer its render's filename there and leave the raw id to the
    /// detail pane, which is where you go when you need to match it to Meta.
    var displayTitle: String {
        if let name = post?.source?.split(separator: "/").last { return String(name) }
        return id
    }

    var insights: ReelInsights? { post?.insights }
    var permalink: URL? { post?.permalink.flatMap(URL.init(string:)) }
    /// The published CDN copy — offered as an openable link, never fetched.
    var publishedURL: URL? { post?.urls?["reel"].flatMap(URL.init(string:)) }
}

/// A segmentReport() row (publish.mjs), computed the same way: live posts
/// with insights only, every post counted (re-publishes included), nils
/// summed as zero, viewsPerPost rounded, interactionRate in % to 2 places.
struct ReelSegmentRow: Identifiable {
    let segment: String
    var posts = 0
    var views = 0.0
    var reach = 0.0
    var interactions = 0.0
    var id: String { segment }
    var viewsPerPost: Int { posts > 0 ? Int((views / Double(posts)).rounded()) : 0 }
    var interactionRate: Double { views > 0 ? (interactions / views * 100) : 0 }
}

enum ReelStats {
    /// { "ledger": …, "queue": … } from the untracked per-host config.
    private static func config() -> [String: String] {
        guard let data = FileManager.default.contents(atPath: Paths.reelsConfig),
              let json = try? JSONSerialization.jsonObject(with: data) as? [String: String]
        else { return [:] }
        return json
    }

    static var ledgerPath: String {
        config()["ledger"] ?? "\(Paths.acRepo)/xbox/live/marketing/ledger.json"
    }

    static var queueRoot: String {
        config()["queue"] ?? "\(Paths.acRepo)/tmp/oskiewar-reels/queue"
    }

    /// Every ledger the window merges: the oskiewar factory's, plus one per
    /// account from `social/instagram/<account>-ledger.json` (ig.mjs writes
    /// these). Discovered by listing that directory rather than by a hardcoded
    /// account list, so provisioning a new account puts its reels in the window
    /// without touching this file. `ledger` in the per-host config still
    /// repoints the factory ledger; nothing here needs a token.
    static var ledgerSources: [ReelLedgerSource] {
        var sources = [ReelLedgerSource(account: "oskiewar", path: ledgerPath)]
        let dir = "\(Paths.acRepo)/social/instagram"
        let names = (try? FileManager.default.contentsOfDirectory(atPath: dir)) ?? []
        for name in names.sorted() where name.hasSuffix("-ledger.json") {
            let account = String(name.dropLast("-ledger.json".count))
            guard account != "oskiewar" else { continue }  // the factory's wins
            sources.append(ReelLedgerSource(account: account, path: "\(dir)/\(name)"))
        }
        return sources
    }

    /// Merge ledger + queue into the player's rows, newest first. Missing
    /// files are normal (fresh checkout, cleaned tmp) and read as empty.
    static func load() -> (reels: [Reel], segments: [ReelSegmentRow]) {
        let posts = livePosts()
        let staged = queueEntries()

        // Latest live post per id (ISO timestamps order lexically).
        var latest: [String: ReelPost] = [:]
        var count: [String: Int] = [:]
        for post in posts {
            let key = post.resolvedID
            guard !key.isEmpty else { continue }
            count[key, default: 0] += 1
            if (latest[key]?.publishedAt ?? "") <= (post.publishedAt ?? "") {
                latest[key] = post
            }
        }

        var reels: [String: Reel] = [:]
        for entry in staged {
            let dir = "\(queueRoot)/\(entry.id)"
            reels[entry.id] = Reel(
                id: entry.id,
                account: latest[entry.id]?.account ?? "oskiewar",
                segment: entry.segment ?? "",
                day: entry.day ?? "",
                segmentName: entry.segmentName,
                caption: entry.caption,
                audioName: entry.audioName,
                localVideo: firstExisting([entry.files?["reel"], "\(dir)/reel.mp4"]),
                thumbnail: firstExisting([entry.files?["thumbnail"],
                                          "\(dir)/thumbnail-10-percent.jpg",
                                          entry.files?["cover"], "\(dir)/cover.jpg"]),
                meta: entry.meta,
                post: latest[entry.id],
                postCount: count[entry.id] ?? 0)
        }
        for (id, post) in latest where reels[id] == nil {
            // A ledger-only reel still plays when its `source` render survives
            // on this machine — that path is repo-relative in ig.mjs ledgers.
            let local = post.source.map { $0.hasPrefix("/") ? $0 : "\(Paths.acRepo)/\($0)" }
            reels[id] = Reel(
                id: id,
                account: post.account,
                segment: post.segment ?? "",
                day: post.resolvedDay,
                segmentName: nil,
                caption: post.caption,
                audioName: post.audioName,
                localVideo: firstExisting([local]),
                thumbnail: nil, meta: nil,
                post: post,
                postCount: count[id] ?? 0)
        }

        // Newest first. Factory ids lead with the day, ig.mjs ids are opaque
        // media ids, so sort on the publish date and fall back to the id.
        let sorted = reels.values.sorted {
            let a = $0.post?.publishedAt ?? $0.day, b = $1.post?.publishedAt ?? $1.day
            return a == b ? $0.id > $1.id : a > b
        }
        return (sorted, segmentReport(posts))
    }

    /// Posts that actually went out, across every ledger, each stamped with
    /// the account it came from. A ledger that is absent or unreadable
    /// contributes nothing — absence is "nothing published", not an error, and
    /// one malformed file must not take the other accounts down with it.
    private static func livePosts() -> [ReelPost] {
        struct Ledger: Decodable { var posts: [ReelPost]? }
        return ledgerSources.flatMap { source -> [ReelPost] in
            guard let data = FileManager.default.contents(atPath: source.path),
                  let ledger = try? JSONDecoder().decode(Ledger.self, from: data)
            else { return [] }
            return (ledger.posts ?? []).filter(\.isLive).map { post in
                var stamped = post
                stamped.account = source.account
                return stamped
            }
        }
    }

    /// Every readable <queue>/<id>/reel.json. A directory without one (or
    /// with a malformed one) is a render in progress — skipped, not fatal.
    private static func queueEntries() -> [ReelQueueEntry] {
        let fm = FileManager.default
        guard let names = try? fm.contentsOfDirectory(atPath: queueRoot) else { return [] }
        return names.compactMap { name in
            guard let data = fm.contents(atPath: "\(queueRoot)/\(name)/reel.json") else { return nil }
            return try? JSONDecoder().decode(ReelQueueEntry.self, from: data)
        }
    }

    /// Mirrors publish.mjs segmentReport(): per-market rollup over live
    /// posts that have insights. Segment is a factory concept, so posts from
    /// the per-account ledgers roll up under their account name instead —
    /// otherwise every one of them lands in a single unlabeled row.
    private static func segmentReport(_ posts: [ReelPost]) -> [ReelSegmentRow] {
        var rows: [String: ReelSegmentRow] = [:]
        for post in posts {
            guard let insights = post.insights else { continue }
            let named = post.segment ?? ""
            let segment = named.isEmpty ? post.account : named
            var row = rows[segment] ?? ReelSegmentRow(segment: segment)
            row.posts += 1
            row.views += insights.views ?? 0
            row.reach += insights.reach ?? 0
            row.interactions += insights.totalInteractions ?? 0
            rows[segment] = row
        }
        return rows.values.sorted { $0.views > $1.views }
    }

    private static func firstExisting(_ paths: [String?]) -> String? {
        paths.compactMap { $0 }.first { FileManager.default.fileExists(atPath: $0) }
    }
}

/// The live model behind the window: reloads itself when the ledger is
/// rewritten (an insights refresh) or the queue gains a reel, so the open
/// panel tracks the factory without polling. Tiny files, main-queue reads.
final class ReelsStore: ObservableObject {
    @Published var reels: [Reel] = []
    @Published var segments: [ReelSegmentRow] = []
    @Published var loadedAt: Date?

    private var watchers: [DispatchSourceFileSystemObject] = []
    private var reloadPending = false

    init() {
        reload()
        watch()
    }

    deinit { unwatch() }

    func reload() {
        let loaded = ReelStats.load()
        reels = loaded.reels
        segments = loaded.segments
        loadedAt = Date()
    }

    private func watch() {
        // Ledgers are rewritten in place (a publish, or an insights refresh);
        // the queue root gains/loses subdirectories. Watch every one that
        // exists today — a ledger created later is picked up on the next
        // reload, since re-arming re-reads the source list.
        for path in ReelStats.ledgerSources.map(\.path) + [ReelStats.queueRoot] {
            let fd = Darwin.open(path, O_EVTONLY)
            guard fd >= 0 else { continue }
            let source = DispatchSource.makeFileSystemObjectSource(
                fileDescriptor: fd, eventMask: [.write, .rename, .delete, .extend],
                queue: .main)
            source.setEventHandler { [weak self] in self?.scheduleReload() }
            source.setCancelHandler { Darwin.close(fd) }
            source.resume()
            watchers.append(source)
        }
    }

    private func unwatch() {
        for watcher in watchers { watcher.cancel() }
        watchers = []
    }

    /// Writers replace files non-atomically — debounce past the last event,
    /// then reload and re-arm on the (possibly new) inodes.
    private func scheduleReload() {
        if reloadPending { return }
        reloadPending = true
        unwatch()
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) { [weak self] in
            guard let self = self else { return }
            self.reloadPending = false
            self.reload()
            self.watch()
        }
    }
}
