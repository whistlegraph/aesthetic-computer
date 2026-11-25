import AppKit
import QuartzCore
import ImageIO

/// 🔥 The oven, live: what aesthetic.computer's oven (oven/server.mjs) bakes
/// flies across the field as it comes out, the bakes in the oven sit in its
/// eight slots with their preview frame and progress, and a small table says
/// how well it is going.
///
/// Fed straight from the oven's public firehose, `wss://oven.aesthetic.computer/ws`
/// (its message types are documented above `wss` in oven/server.mjs). A
/// snapshot (no `type`, keyed on `grabs`) holds `grabs.recent` — the last 20
/// finished grabs with `cdnUrl`, `duration`, `size`, `status` — plus
/// `grabs.active`, `grabProgress` with an 80² JPEG preview per bake, and
/// `concurrency`; log lines come as `{logEntry}`.
///
/// Two shapes of that stream are read:
///   - the newer oven sends a snapshot only on a state change, moves inside a
///     stage as small `{type:'grab:progress'}` ticks (a `preview` when the
///     frame changed), marks `grabs.active[].state` queued|baking with
///     `bakeStart`, counts `concurrency.baking/queued`, and carries its own
///     `stats` (since start and the last hour, bake and queue-wait
///     percentiles). Those are shown as the oven counts them.
///   - the older one sends a whole snapshot on every tick and no `stats`;
///     then the slots and table are worked out here from the snapshots and
///     the cache log lines, as before.
///
/// `--oven right|left|off` or `$BLUEBERRY_WALLPAPER_OVEN` places it (right by
/// default); `--no-oven` turns it off. Only the main screen draws it unless
/// `$BLUEBERRY_WALLPAPER_OVEN_SCREENS=all`. `$BLUEBERRY_WALLPAPER_OVEN_URL`
/// points it at another oven.
enum OvenConfig {
    enum Side { case off, right, left }
    static let side: Side = {
        let args = CommandLine.arguments
        var raw = ProcessInfo.processInfo.environment["BLUEBERRY_WALLPAPER_OVEN"]
        if let i = args.firstIndex(of: "--oven"), i + 1 < args.count { raw = args[i + 1] }
        if args.contains("--no-oven") { raw = "off" }
        switch raw?.lowercased() {
        case "0", "off", "no", "false": return .off
        case "left": return .left
        default: return .right
        }
    }()
    static let everyScreen = ProcessInfo.processInfo.environment["BLUEBERRY_WALLPAPER_OVEN_SCREENS"] == "all"
    static let url = URL(string: ProcessInfo.processInfo.environment["BLUEBERRY_WALLPAPER_OVEN_URL"]
                         ?? "wss://oven.aesthetic.computer/ws")!
}

/// One finished grab, as the oven reports it in `grabs.recent`.
struct OvenGrab {
    let id: String, piece: String, format: String, status: String, source: String
    /// Unix seconds.
    let completedAt: Double
    /// Seconds from the request being taken to the upload finishing: queue
    /// wait and bake together (the oven sets `startTime` before queueing).
    let duration: Double
    let size: Int
    let url: String?
    /// The oven's own test (oven/grab-duds.mjs): flagged, or under 1 KB.
    var dud: Bool
    /// Seconds in a slot, without the queue wait (newer ovens only, else 0).
    var bakeTime: Double = 0
    var ok: Bool { status == "complete" }
}

/// The newer oven's own counts (grabber.getGrabStats), since it started and
/// over the last hour, with bake and queue-wait percentiles in seconds over
/// its last few hundred bakes.
struct OvenStats {
    /// Unix seconds the counts start from (the oven process starting).
    var since: Double = 0
    var total: [String: Double] = [:]
    var hour: [String: Double] = [:]
    var bakeP50: Double?, bakeP90: Double?, queueP50: Double?, queueP90: Double?
    var samples = 0
}

/// A bake in the oven right now: `grabs.active` joined with `grabProgress`.
struct OvenBake {
    let id: String, piece: String, format: String
    /// When it took a slot (`bakeStart`), or when it was asked for on an older oven.
    let startTime: Double
    var stage: String
    var percent: Double
}

/// A decoded grab: up to a dozen frames, small, for a looping card.
final class OvenMedia {
    let frames: [CGImage]
    let duration: Double
    init(frames: [CGImage], duration: Double) { self.frames = frames; self.duration = duration }
}

/// The socket, the rolling record and the image cache, shared by every
/// screen. Socket work and parsing happen on `wire`; the record and the
/// cache are only touched on the main thread.
final class OvenFeed: NSObject, URLSessionWebSocketDelegate {
    static let shared = OvenFeed()

    // ── main-thread state ────────────────────────────────────────────────
    /// Bumps whenever anything below changes.
    private(set) var version = 0
    private(set) var grabs: [String: OvenGrab] = [:]
    /// Ids of grabs seen finishing while connected, newest last, numbered so
    /// each screen can take its own.
    private(set) var arrivals: [(seq: Int, id: String)] = []
    private var arrivalSeq = 0
    private(set) var bakes: [OvenBake] = []
    private(set) var previews: [String: CGImage] = [:]
    private(set) var busy = 0, slots = 8, queueDepth = 0
    private(set) var queued: [String] = []
    /// `💾 Cache hit` / `🔄 Cache miss` log lines from the icon and preview
    /// path (getCachedOrGenerate), unix seconds.
    private(set) var cacheEvents: [(at: Double, hit: Bool)] = []
    private(set) var ovenVersion = ""
    private(set) var uptime: Double = 0, uptimeAt: Double = 0
    private(set) var connected = false
    private(set) var lastMessage: Double = 0
    /// When the record began: the oldest grab the first snapshot held, or the
    /// moment of connecting if the oven had held fewer than its 20.
    private(set) var recordSince: Double = 0
    private var seeded = false
    /// The oven's own counts; nil from an older oven that sends none.
    private(set) var stats: OvenStats?
    /// The newest `grab:progress` tick per bake, so a snapshot parsed a
    /// moment after it arrived cannot wind a bar back.
    private var ticks: [String: (at: Double, stage: String, percent: Double)] = [:]

    // ── wire ─────────────────────────────────────────────────────────────
    private let wire = DispatchQueue(label: "computer.aesthetic.blueberry-wallpaper.oven", qos: .utility)
    private lazy var session: URLSession = {
        let ops = OperationQueue()
        ops.maxConcurrentOperationCount = 1
        ops.underlyingQueue = wire
        let config = URLSessionConfiguration.default
        config.urlCache = nil
        config.requestCachePolicy = .reloadIgnoringLocalCacheData
        return URLSession(configuration: config, delegate: self, delegateQueue: ops)
    }()
    private var task: URLSessionWebSocketTask?
    private var backoff: Double = 2
    private var started = false
    private var heardOnTask = false
    private var previewKeys: [String: Int] = [:]
    private var watchdog: DispatchSourceTimer?
    private var wireLastMessage: Double = 0

    func start() {
        guard !started else { return }
        started = true
        wire.async { self.connect() }
        let timer = DispatchSource.makeTimerSource(queue: wire)
        timer.schedule(deadline: .now() + 60, repeating: 60)
        timer.setEventHandler { [weak self] in self?.checkAlive() }
        timer.resume()
        watchdog = timer
    }

    private func connect() {
        task?.cancel(with: .goingAway, reason: nil)
        let t = session.webSocketTask(with: OvenConfig.url)
        t.maximumMessageSize = 16 << 20
        task = t
        heardOnTask = false
        t.resume()
        listen(t)
    }

    private func listen(_ t: URLSessionWebSocketTask) {
        t.receive { [weak self] result in
            guard let self, t === self.task else { return }
            switch result {
            case .success(let message):
                switch message {
                case .string(let s): self.handle(Data(s.utf8))
                case .data(let d): self.handle(d)
                @unknown default: break
                }
                self.listen(t)
            case .failure:
                self.dropped(t)
            }
        }
    }

    func urlSession(_ session: URLSession, webSocketTask: URLSessionWebSocketTask,
                    didCloseWith closeCode: URLSessionWebSocketTask.CloseCode, reason: Data?) {
        dropped(webSocketTask)
    }

    func urlSession(_ session: URLSession, task: URLSessionTask, didCompleteWithError error: Error?) {
        if let ws = task as? URLSessionWebSocketTask { dropped(ws) }
    }

    /// Reconnect with doubling backoff (2 s to 5 min, jittered); a socket
    /// that delivered anything starts the ladder again.
    private func dropped(_ t: URLSessionWebSocketTask) {
        guard t === task else { return }
        task = nil
        if heardOnTask { backoff = 2 }
        let delay = backoff * Double.random(in: 0.8...1.25)
        backoff = min(300, backoff * 2)
        print("oven: socket dropped, again in \(Int(delay))s"); fflush(stdout)
        DispatchQueue.main.async { self.connected = false; self.version += 1 }
        wire.asyncAfter(deadline: .now() + delay) { [weak self] in
            guard let self, self.task == nil else { return }
            self.connect()
        }
    }

    /// The oven only speaks when something changes; a quiet hour is normal.
    /// After ten silent minutes, ask; a dead socket fails the ping.
    private func checkAlive() {
        guard let t = task, Date().timeIntervalSince1970 - wireLastMessage > 600 else { return }
        t.sendPing { [weak self] error in
            guard let self else { return }
            self.wire.async {
                if error != nil { self.dropped(t) } else { self.wireLastMessage = Date().timeIntervalSince1970 }
            }
        }
    }

    // ── parsing (wire) ───────────────────────────────────────────────────

    private struct Parsed {
        var recent: [OvenGrab] = []
        var bakes: [OvenBake] = []
        var previews: [String: CGImage] = [:]
        var queued: [String] = []
        var busy = 0, slots = 8, queueDepth = 0
        var version = ""
        var uptime: Double = 0
        var logs: [(Double, Bool)] = []
        var stats: OvenStats?
        /// When the message arrived (it may be parsed up to 0.25 s later).
        var at: Double = 0
    }

    private static func num(_ v: Any?) -> Double {
        if let n = v as? NSNumber { return n.doubleValue }
        if let s = v as? String, let d = Double(s) { return d }
        return 0
    }
    /// A number that may be null (a percentile with no samples yet).
    private static func maybe(_ v: Any?) -> Double? {
        if let n = v as? NSNumber { return n.doubleValue }
        return nil
    }

    private static let iso: ISO8601DateFormatter = {
        let f = ISO8601DateFormatter()
        f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return f
    }()

    /// Epoch milliseconds, or an ISO string (records reloaded from Mongo), to unix seconds.
    private static func time(_ v: Any?) -> Double {
        if let s = v as? String {
            if let d = iso.date(from: s) { return d.timeIntervalSince1970 }
            if let d = ISO8601DateFormatter().date(from: s) { return d.timeIntervalSince1970 }
            return 0
        }
        let ms = num(v)
        return ms > 1e11 ? ms / 1000 : ms
    }

    private static func cacheLine(_ entry: [String: Any]) -> (Double, Bool)? {
        guard let msg = entry["msg"] as? String else { return nil }
        let hit = msg.hasPrefix("Cache hit:")
        guard hit || msg.hasPrefix("Cache miss:") else { return nil }
        return (time(entry["time"]), hit)
    }

    /// A bake broadcasts a whole snapshot (~18 KB) about five times a second,
    /// and eight bakes eight times that. Snapshots are whole state, so only
    /// the newest is parsed, at most four times a second; log lines (small)
    /// are taken one by one.
    private var latest: Data?
    private var latestAt: Double = 0
    private var parseScheduled = false
    private var lastParse: Double = 0
    private static let logPrefix = Data(#"{"logEntry""#.utf8)
    private static let typePrefix = Data(#"{"type""#.utf8)

    /// Log lines and `type` messages are small and taken one by one; any
    /// other message is a snapshot only if it has `grabs` (checked on parse).
    private func handle(_ data: Data) {
        let now = Date().timeIntervalSince1970
        wireLastMessage = now
        heardOnTask = true
        if data.starts(with: Self.logPrefix) { return parse(data) }
        if data.starts(with: Self.typePrefix) { return typed(data, now: now) }
        latest = data
        latestAt = now
        guard !parseScheduled else { return }
        parseScheduled = true
        wire.asyncAfter(deadline: .now() + max(0, 0.25 - (now - lastParse))) { [weak self] in
            guard let self else { return }
            self.parseScheduled = false
            self.lastParse = Date().timeIntervalSince1970
            if let d = self.latest { self.latest = nil; self.parse(d, at: self.latestAt) }
        }
    }

    /// `{type:'grab:progress'}` moves one bake's bar, stage and preview;
    /// every other `type` (os:build-progress, os:new-build) is not ours.
    private func typed(_ data: Data, now: Double) {
        guard let obj = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any],
              obj["type"] as? String == "grab:progress", let id = obj["grabId"] as? String else {
            DispatchQueue.main.async { self.heard(now) }
            return
        }
        let stage = obj["stage"] as? String
        let percent = obj["percent"] == nil ? nil : Self.num(obj["percent"])
        var image: CGImage?
        if let b64 = obj["preview"] as? String { image = decodePreview(id, b64) }
        DispatchQueue.main.async { self.progress(id, stage: stage, percent: percent, preview: image, at: now) }
    }

    /// A preview is decoded once per new frame, not once per message (wire).
    private func decodePreview(_ id: String, _ b64: String) -> CGImage? {
        let key = b64.hashValue
        guard previewKeys[id] != key, let bytes = Data(base64Encoded: b64),
              let src = CGImageSourceCreateWithData(bytes as CFData, nil),
              let image = CGImageSourceCreateImageAtIndex(src, 0, nil) else { return nil }
        previewKeys[id] = key
        return image
    }

    private func parse(_ data: Data, at: Double = 0) {
        let now = Date().timeIntervalSince1970
        guard let obj = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any] else { return }
        if let entry = obj["logEntry"] as? [String: Any] {
            guard let line = Self.cacheLine(entry) else {
                DispatchQueue.main.async { self.heard(now) }
                return
            }
            DispatchQueue.main.async { self.heard(now); self.cacheEvents.append((line.0, line.1)); self.version += 1 }
            return
        }
        guard let grabs = obj["grabs"] as? [String: Any] else {
            DispatchQueue.main.async { self.heard(now) }
            return
        }
        var p = Parsed()
        p.version = obj["version"] as? String ?? ""
        p.uptime = Self.num(obj["uptime"]) / 1000
        for g in grabs["recent"] as? [[String: Any]] ?? [] {
            guard let id = g["id"] as? String else { continue }
            let size = Int(Self.num(g["size"]))
            var url = g["cdnUrl"] as? String
            if url?.isEmpty == true { url = nil }
            let completed = Self.time(g["completedAt"])
            let start = Self.time(g["startTime"])
            var duration = Self.num(g["duration"]) / 1000
            if duration <= 0, completed > 0, start > 0 { duration = completed - start }
            p.recent.append(OvenGrab(
                id: id, piece: g["piece"] as? String ?? "?", format: g["format"] as? String ?? "",
                status: g["status"] as? String ?? "", source: g["source"] as? String ?? "",
                completedAt: completed > 0 ? completed : start, duration: duration, size: size, url: url,
                dud: g["dud"] as? Bool == true || (size > 0 && size < 1024),
                bakeTime: Self.num(g["bakeMs"]) / 1000))
        }
        let progress = obj["grabProgress"] as? [String: [String: Any]] ?? [:]
        let active = grabs["active"] as? [[String: Any]] ?? []
        // A newer oven says which grabs are baking (`state`, `bakeStart`);
        // an older one only says a grab has reached a progress stage.
        let stated = active.contains { $0["state"] is String }
        var live = Set<String>()
        for g in active {
            guard let id = g["id"] as? String else { continue }
            let pr = progress[id]
            let stage = pr?["stage"] as? String ?? ""
            if stated {
                guard g["state"] as? String == "baking" else { continue }
            } else if stage.isEmpty { continue }
            live.insert(id)
            let bakeStart = Self.time(g["bakeStart"])
            p.bakes.append(OvenBake(
                id: id, piece: g["piece"] as? String ?? "?", format: g["format"] as? String ?? "",
                startTime: bakeStart > 0 ? bakeStart : Self.time(g["startTime"]),
                stage: stage, percent: Self.num(pr?["percent"])))
            if let b64 = pr?["previewFrame"] as? String, let image = decodePreview(id, b64) { p.previews[id] = image }
        }
        for id in previewKeys.keys where !live.contains(id) { previewKeys[id] = nil }
        p.bakes.sort { $0.startTime < $1.startTime }
        p.queued = (grabs["queue"] as? [[String: Any]] ?? []).compactMap { $0["piece"] as? String }
        // `concurrency.active` is decremented after the last broadcast of a
        // finished grab, so an idle oven can read 1/8 until something else
        // happens; count the bakes (or the newer oven's own `baking`).
        p.busy = p.bakes.count
        p.queueDepth = p.queued.count
        if let c = obj["concurrency"] as? [String: Any] {
            p.slots = max(1, Int(Self.num(c["max"])))
            if c["baking"] != nil, c["queued"] != nil {
                p.busy = Int(Self.num(c["baking"]))
                p.queueDepth = Int(Self.num(c["queued"]))
            } else {
                p.queueDepth = max(p.queueDepth, Int(Self.num(c["queueDepth"])))
            }
        }
        if let st = obj["stats"] as? [String: Any] {
            var s = OvenStats()
            s.since = Self.time(st["since"])
            for (k, v) in st { if let n = v as? NSNumber { s.total[k] = n.doubleValue } }
            for (k, v) in st["hour"] as? [String: Any] ?? [:] { if let n = v as? NSNumber { s.hour[k] = n.doubleValue } }
            s.bakeP50 = Self.maybe(st["bakeMsP50"]).map { $0 / 1000 }
            s.bakeP90 = Self.maybe(st["bakeMsP90"]).map { $0 / 1000 }
            s.queueP50 = Self.maybe(st["queueMsP50"]).map { $0 / 1000 }
            s.queueP90 = Self.maybe(st["queueMsP90"]).map { $0 / 1000 }
            s.samples = Int(Self.num(st["samples"]))
            p.stats = s
        }
        p.at = at > 0 ? at : now
        for entry in obj["recentLogs"] as? [[String: Any]] ?? [] {
            if let line = Self.cacheLine(entry) { p.logs.append(line) }
        }
        DispatchQueue.main.async { self.apply(p, now: now) }
    }

    // ── main ─────────────────────────────────────────────────────────────

    private func heard(_ now: Double) {
        lastMessage = now
        if !connected { connected = true; version += 1 }
    }

    /// One `grab:progress` tick: fold it into the bake it belongs to. A tick
    /// for a bake no snapshot has shown yet is kept until one does.
    private func progress(_ id: String, stage: String?, percent: Double?, preview: CGImage?, at: Double) {
        heard(at)
        let old = ticks[id]
        let tick = (at: at, stage: stage ?? old?.stage ?? "", percent: percent ?? old?.percent ?? 0)
        ticks[id] = tick
        if let i = bakes.firstIndex(where: { $0.id == id }) {
            if !tick.stage.isEmpty { bakes[i].stage = tick.stage }
            bakes[i].percent = tick.percent
        }
        if let preview, bakes.contains(where: { $0.id == id }) { previews[id] = preview }
        version += 1
    }

    private func apply(_ p: Parsed, now: Double) {
        heard(now)
        let first = !seeded
        for g in p.recent {
            let known = grabs[g.id] != nil
            grabs[g.id] = g
            // Only what finished while we watched flies in as new; a
            // reconnect after a long gap brings no burst of old ones.
            if !known, !first, g.completedAt > now - 180 {
                print("oven: out \(g.piece) \(g.status) \(Int(g.duration))s \(g.size)B\(g.dud ? " dud" : "")"); fflush(stdout)
                arrivalSeq += 1
                arrivals.append((arrivalSeq, g.id))
            }
        }
        if arrivals.count > 32 { arrivals.removeFirst(arrivals.count - 32) }
        if first {
            seeded = true
            // The oven holds its last 20; fewer means that is all there was.
            let oldest = p.recent.map(\.completedAt).filter { $0 > 0 }.min() ?? now
            recordSince = p.recent.count >= 20 ? oldest : 0
        }
        if !p.logs.isEmpty, cacheEvents.isEmpty { cacheEvents = p.logs.sorted { $0.0 < $1.0 } }
        // A day of grabs, at most 600.
        if grabs.count > 600 || version % 64 == 0 {
            let cut = now - 86_400
            grabs = grabs.filter { $0.value.completedAt > cut }
            if grabs.count > 600 {
                let keep = grabs.values.sorted { $0.completedAt > $1.completedAt }.prefix(600)
                grabs = Dictionary(uniqueKeysWithValues: keep.map { ($0.id, $0) })
            }
        }
        cacheEvents.removeAll { $0.at < now - 3_600 }
        var bakes = p.bakes
        let live = Set(p.bakes.map(\.id))
        // A tick that came in after this snapshot did is newer than it.
        ticks = ticks.filter { live.contains($0.key) || $0.value.at > now - 5 }
        for i in bakes.indices {
            guard let t = ticks[bakes[i].id], t.at > p.at else { continue }
            if !t.stage.isEmpty { bakes[i].stage = t.stage }
            bakes[i].percent = t.percent
        }
        self.bakes = bakes
        stats = p.stats
        previews = previews.filter { live.contains($0.key) }
        for (id, image) in p.previews { previews[id] = image }
        queued = p.queued
        busy = p.busy; slots = p.slots; queueDepth = p.queueDepth
        if !p.version.isEmpty { ovenVersion = p.version }
        uptime = p.uptime; uptimeAt = now
        version += 1
    }

    // ── media ────────────────────────────────────────────────────────────

    private static let mediaCap = 30
    private static let inFlightCap = 2
    private var media: [String: OvenMedia?] = [:]
    private var mediaOrder: [String] = []
    private var waiting: [String] = []
    private var loading = Set<String>()
    private var inFlight = 0
    private lazy var fetcher: URLSession = {
        let config = URLSessionConfiguration.ephemeral
        config.urlCache = nil
        config.timeoutIntervalForRequest = 20
        config.timeoutIntervalForResource = 40
        config.httpMaximumConnectionsPerHost = 2
        return URLSession(configuration: config)
    }()

    /// The decoded grab, nil while it is on its way, `.some(nil)` when it
    /// would not load. Two downloads at a time; thirty kept, oldest out.
    func media(_ url: String) -> OvenMedia?? {
        if let known = media[url] {
            if let i = mediaOrder.firstIndex(of: url) { mediaOrder.remove(at: i) }
            mediaOrder.append(url)
            return known
        }
        if !loading.contains(url) {
            loading.insert(url)
            waiting.append(url)
            pump()
        }
        return nil
    }

    func forget(_ url: String) { if let i = waiting.firstIndex(of: url) { waiting.remove(at: i); loading.remove(url) } }

    private func pump() {
        while inFlight < Self.inFlightCap, !waiting.isEmpty {
            let url = waiting.removeLast()   // newest asked first
            guard let u = URL(string: url) else { loading.remove(url); continue }
            inFlight += 1
            fetcher.dataTask(with: u) { [weak self] data, response, _ in
                let okay = (response as? HTTPURLResponse).map { (200..<300).contains($0.statusCode) } ?? false
                let decoded = okay && (data?.count ?? 0) < 8 << 20 ? data.flatMap { Self.decode($0) } : nil
                DispatchQueue.main.async {
                    guard let self else { return }
                    self.inFlight -= 1
                    self.loading.remove(url)
                    self.media[url] = .some(decoded)
                    self.mediaOrder.append(url)
                    while self.mediaOrder.count > Self.mediaCap {
                        self.media[self.mediaOrder.removeFirst()] = nil
                    }
                    self.version += 1
                    self.pump()
                }
            }.resume()
        }
    }

    private static func delay(_ src: CGImageSource, _ i: Int) -> Double {
        guard let props = CGImageSourceCopyPropertiesAtIndex(src, i, nil) as? [CFString: Any] else { return 0 }
        for dict in [kCGImagePropertyWebPDictionary, kCGImagePropertyGIFDictionary, kCGImagePropertyPNGDictionary] {
            guard let d = props[dict] as? [CFString: Any] else { continue }
            for key in [kCGImagePropertyWebPUnclampedDelayTime, kCGImagePropertyWebPDelayTime,
                        kCGImagePropertyGIFUnclampedDelayTime, kCGImagePropertyGIFDelayTime,
                        kCGImagePropertyAPNGUnclampedDelayTime, kCGImagePropertyAPNGDelayTime] {
                if let v = d[key] as? Double, v > 0 { return v }
            }
        }
        return 0
    }

    /// Up to twelve evenly spaced frames, at most 112 px a side.
    static func decode(_ data: Data) -> OvenMedia? {
        guard let src = CGImageSourceCreateWithData(data as CFData, [kCGImageSourceShouldCache: false] as CFDictionary)
        else { return nil }
        let n = CGImageSourceGetCount(src)
        guard n > 0 else { return nil }
        let take = min(n, 12)
        var total = 0.0
        for i in 0..<n { total += delay(src, i) }
        let options = [kCGImageSourceCreateThumbnailFromImageAlways: true,
                       kCGImageSourceCreateThumbnailWithTransform: true,
                       kCGImageSourceShouldCacheImmediately: true,
                       kCGImageSourceThumbnailMaxPixelSize: 112] as CFDictionary
        var frames: [CGImage] = []
        for k in 0..<take {
            let i = take == 1 ? 0 : k * n / take
            if let image = CGImageSourceCreateThumbnailAtIndex(src, i, options) { frames.append(image) }
        }
        guard !frames.isEmpty else { return nil }
        return OvenMedia(frames: frames, duration: total > 0 ? total : Double(n) / 12)
    }
}

/// The oven on one screen: the mouth (slots and table) at one side, and the
/// finished grabs flying away from it across the field. Motion is all Core
/// Animation; `tick` only launches cards and redraws text when the feed moves.
final class OvenLayer: CALayer {
    var enabled = false {
        didSet { isHidden = !enabled; if !enabled { panel = .null } }
    }
    var scale: CGFloat = 2 {
        didSet { if scale != oldValue { setScale(self) } }
    }
    private var dark = true

    private final class Card {
        let root = CALayer()
        let media = CALayer()
        let caption = HaloTextLayer()
        var until: CFTimeInterval = 0
        var id = ""
    }

    private struct Slot {
        let box = CALayer()
        let image = CALayer()
        let track = CALayer()
        let fill = CALayer()
        let tag = HaloTextLayer()
    }

    private let title = HaloTextLayer()
    private let table = HaloTextLayer()
    private let queueLine = HaloTextLayer()
    private var slotLayers: [Slot] = []
    private let lane = CALayer()
    private var cards: [Card] = []
    private var drawnVersion = -1
    private struct Shape: Equatable { var dark = true, size = CGSize.zero, scale: CGFloat = 0 }
    private var drawnShape = Shape(scale: -1)
    private var checkedAt: Double = 0
    private var drawnAt: Double = 0
    private var tableText = ""
    private var titleText = ""
    private var queueText = ""
    private var takenSeq = 0
    private var pending: [(id: String, since: CFTimeInterval)] = []
    private var lastLaunch: CFTimeInterval = 0
    private var historyCursor = 0
    /// Where cards leave the mouth, and the band they fly in.
    private var mouth = CGPoint.zero
    private var laneSpan: CGFloat = 0
    private var slotSide: CGFloat = 0
    /// The panel (title, slots, queue line, table) in this layer's
    /// coordinates, padded; `.null` when nothing is drawn. Fishery keeps its
    /// walkers out of it.
    private(set) var panel: CGRect = .null

    private static let still: [String: CAAction] = [
        "position": NSNull(), "bounds": NSNull(), "opacity": NSNull(), "transform": NSNull(),
        "contents": NSNull(), "string": NSNull(), "foregroundColor": NSNull(), "backgroundColor": NSNull(),
        "hidden": NSNull(), "frame": NSNull(), "borderColor": NSNull(), "borderWidth": NSNull(),
        "cornerRadius": NSNull(), "sublayers": NSNull(),
    ]
    private static let darkHalo = NSColor.black.cgColor
    private static let lightHalo = NSColor(srgbRed: 1.0, green: 0.42, blue: 0.78, alpha: 1).cgColor
    /// Oven heat: fresh out is orange, the slots glow amber, duds go grey,
    /// failures red.
    private static let hot = NSColor(srgbRed: 1.00, green: 0.55, blue: 0.22, alpha: 1)
    private static let amber = NSColor(srgbRed: 1.00, green: 0.78, blue: 0.32, alpha: 1)
    private static let red = NSColor(srgbRed: 1.00, green: 0.36, blue: 0.30, alpha: 1)

    override init() {
        super.init()
        actions = Self.still
        isHidden = true
        lane.actions = Self.still
        addSublayer(lane)
        for t in [title, table, queueLine] { addSublayer(Self.text(t)) }
        for _ in 0..<8 { addSlot() }
    }
    override init(layer: Any) { super.init(layer: layer) }
    required init?(coder: NSCoder) { nil }

    private func addSlot() {
        let s = Slot()
        for l in [s.box, s.image, s.track, s.fill] { l.actions = Self.still }
        s.image.magnificationFilter = .nearest
        s.image.masksToBounds = true
        s.box.addSublayer(s.image)
        addSublayer(s.box)
        addSublayer(s.track)
        s.track.addSublayer(s.fill)
        addSublayer(Self.text(s.tag))
        slotLayers.append(s)
    }

    private static func text(_ t: HaloTextLayer) -> HaloTextLayer {
        t.actions = still
        t.pixelated = PixelFont.available
        t.contentsScale = 2
        t.alignmentMode = .left
        t.truncationMode = .end
        t.isWrapped = false
        return t
    }

    private func setScale(_ layer: CALayer) {
        for sub in layer.sublayers ?? [] {
            sub.contentsScale = scale
            setScale(sub)
        }
    }

    func retire() {
        for c in cards { c.root.removeAllAnimations(); c.media.removeAllAnimations() }
    }

    // ── type ─────────────────────────────────────────────────────────────

    private var u: CGFloat { max(1, bounds.height / 850) }
    private var tu: CGFloat { u * 0.7 }
    private var ts: CGFloat { tu * PixelFont.boost }

    private func ink(_ alpha: CGFloat = 1) -> NSColor {
        dark ? NSColor(white: 0.96, alpha: alpha) : NSColor(srgbRed: 0.05, green: 0.07, blue: 0.16, alpha: alpha)
    }

    /// The pixel face at a whole multiple of its cell, as Fishery sets it.
    private func face(_ size: CGFloat, _ weight: NSFont.Weight = .bold) -> NSFont {
        let system = NSFont.monospacedDigitSystemFont(ofSize: size, weight: weight)
        guard let base = PixelFont.font(scale: scale) else { return system }
        let cells = max(1, min(3, (size * scale / 16 + 0.25).rounded(.down)))
        return NSFont(descriptor: base.fontDescriptor, size: base.pointSize * cells) ?? base
    }

    private func words(_ s: String, _ size: CGFloat, _ color: NSColor, _ weight: NSFont.Weight = .bold) -> NSAttributedString {
        NSAttributedString(string: s, attributes: [.font: face(size, weight), .foregroundColor: color])
    }

    private static func span(_ s: Double) -> String {
        s < 10 ? String(format: "%.1fs", s) : s < 90 ? "\(Int(s.rounded()))s" : "\(Int(s / 60))m\(Int(s) % 60)s"
    }
    private static func bytes(_ n: Double) -> String {
        n < 1024 ? "\(Int(n))B" : n < 1_048_576 ? "\(Int((n / 1024).rounded()))K" : String(format: "%.1fM", n / 1_048_576)
    }
    private static func quantile(_ xs: [Double], _ q: Double) -> Double {
        guard !xs.isEmpty else { return 0 }
        let s = xs.sorted()
        return s[min(s.count - 1, Int((Double(s.count - 1) * q).rounded()))]
    }

    // ── frame ────────────────────────────────────────────────────────────

    func tick(now: Double, dark: Bool) {
        guard enabled, bounds.width > 0 else { return }
        // Called every display frame: most frames compare a few numbers and leave.
        let feed = OvenFeed.shared
        feed.start()
        let shape = Shape(dark: dark, size: bounds.size, scale: scale)
        let relayout = shape != drawnShape
        guard relayout || feed.version != drawnVersion || now - drawnAt > 10 || now - checkedAt > 0.25 else { return }
        checkedAt = now
        if relayout { self.dark = dark; drawnShape = shape }
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        // Text moves only with the feed, and once every ten seconds for the
        // hour window sliding on.
        if relayout || feed.version != drawnVersion || now - drawnAt > 10 {
            drawnVersion = feed.version
            drawnAt = now
            layoutMouth(now: now, feed: feed, force: relayout)
        }
        launch(feed: feed)
        CATransaction.commit()
    }

    private func layoutMouth(now: Double, feed: OvenFeed, force: Bool) {
        let w = bounds.width, h = bounds.height
        let right = OvenConfig.side != .left
        let pad = 16 * u
        let n = min(8, max(1, feed.slots))
        let gap = (3 * u).rounded()
        let panelW = min(max(240 * ts, w * 0.2), 380 * ts)
        let side = min(((panelW - CGFloat(n - 1) * gap) / CGFloat(n)).rounded(.down), 40 * u)
        slotSide = side
        let rowW = CGFloat(n) * side + CGFloat(n - 1) * gap
        let x0 = right ? w - pad - rowW : pad
        let slotY = (h * 0.5).rounded()
        mouth = CGPoint(x: right ? x0 - 10 * u : x0 + rowW + 10 * u, y: slotY + side / 2)
        laneSpan = right ? x0 - 10 * u : w - (x0 + rowW + 10 * u)
        let halo = dark ? Self.darkHalo : Self.lightHalo
        let haveData = feed.lastMessage > 0 || !feed.grabs.isEmpty
        for l in [title, table, queueLine] as [CALayer] { l.isHidden = !haveData }

        // Slots: one per oven slot, a live bake shows its latest preview
        // frame and a progress bar; an idle slot is an outline.
        for (i, s) in slotLayers.enumerated() {
            let show = haveData && i < n
            for l in [s.box, s.track, s.tag] as [CALayer] { l.isHidden = !show }
            guard show else { continue }
            let bake = i < feed.bakes.count ? feed.bakes[i] : nil
            let x = x0 + CGFloat(i) * (side + gap)
            s.box.frame = CGRect(x: x, y: slotY, width: side, height: side)
            s.box.cornerRadius = 3 * u
            s.box.borderWidth = max(1, u)
            s.image.frame = s.box.bounds.insetBy(dx: max(1, u), dy: max(1, u))
            s.track.frame = CGRect(x: x, y: slotY - 5 * u, width: side, height: max(2, 2.5 * u))
            s.track.cornerRadius = s.track.frame.height / 2
            s.fill.cornerRadius = s.track.cornerRadius
            if let bake {
                s.box.borderColor = Self.amber.cgColor
                s.box.backgroundColor = Self.hot.withAlphaComponent(0.18).cgColor
                s.image.contents = feed.previews[bake.id]
                s.track.backgroundColor = ink(0.18).cgColor
                s.fill.backgroundColor = Self.amber.cgColor
                s.fill.frame = CGRect(x: 0, y: 0, width: side * CGFloat(min(100, max(4, bake.percent)) / 100),
                                      height: s.track.frame.height)
                let stage = String((bake.stage.isEmpty ? "…" : bake.stage).prefix(4))
                let tagKey = "\(bake.id)\(stage)\(dark)"
                if s.tag.name != tagKey {
                    s.tag.name = tagKey
                    s.tag.string = words(stage, max(9, 9 * tu), ink(0.6), .semibold)
                }
                s.tag.isHidden = side < 26 * u
                s.tag.frame = CGRect(x: x, y: slotY - 6 * u - 12 * ts, width: side + 8 * u, height: 12 * ts)
                s.tag.halo = halo
            } else {
                s.box.borderColor = ink(0.22).cgColor
                s.box.backgroundColor = nil
                s.image.contents = nil
                s.track.backgroundColor = ink(0.08).cgColor
                s.fill.frame = .zero
                s.tag.isHidden = true
            }
        }

        // Title over the slots: busy of max, and how far the oven is behind.
        let offline = feed.lastMessage > 0 && !feed.connected
        let up = feed.uptime + (feed.connected ? now - feed.uptimeAt : 0)
        let upText = up > 86_400 ? String(format: "%.1fd", up / 86_400) : up > 3_600 ? "\(Int(up / 3_600))h" : "\(Int(up / 60))m"
        let titleKey = "\(feed.busy)/\(feed.slots) \(feed.queueDepth) \(offline) \(upText) \(dark) \(scale)"
        if force || titleKey != titleText {
            titleText = titleKey
            let s = NSMutableAttributedString(attributedString: words("oven ", max(11, 13 * tu), Self.hot, .heavy))
            if offline {
                s.append(words("offline · reconnecting", max(11, 11 * tu), Self.red))
            } else {
                s.append(words("\(feed.busy)/\(feed.slots) busy", max(11, 11 * tu), feed.busy > 0 ? Self.amber : ink(0.7)))
                s.append(words(" · queue \(feed.queueDepth)", max(11, 11 * tu),
                               feed.queueDepth > 0 ? Self.amber : ink(0.55), .semibold))
                if up > 0 { s.append(words(" · up \(upText)", max(11, 11 * tu), ink(0.4), .semibold)) }
            }
            title.string = s
        }
        let titleW = ceil((title.string as? NSAttributedString)?.size().width ?? 0) + 4
        title.frame = CGRect(x: right ? x0 + rowW - titleW : x0, y: slotY + side + 5 * u, width: titleW, height: 16 * ts)
        title.halo = halo

        // Next up, under the bars: what is waiting its turn.
        let wait = feed.queued.prefix(4).joined(separator: " ")
        let qText = wait.isEmpty ? "" : "next " + wait + (feed.queued.count > 4 ? " +\(feed.queued.count - 4)" : "")
        if force || qText != queueText {
            queueText = qText
            queueLine.string = words(qText, max(9, 10 * tu), ink(0.5), .semibold)
        }
        queueLine.isHidden = !haveData || qText.isEmpty
        let qW = min(rowW, ceil((queueLine.string as? NSAttributedString)?.size().width ?? 0) + 4)
        let tagRoom = slotLayers.contains { !$0.tag.isHidden } ? 14 * ts : 0
        queueLine.frame = CGRect(x: right ? x0 + rowW - qW : x0, y: slotY - 8 * u - tagRoom - 12 * ts, width: qW, height: 13 * ts)
        queueLine.halo = halo

        layoutTable(now: now, feed: feed, right: right, edge: right ? x0 + rowW : x0,
                    top: queueLine.frame.minY - (queueLine.isHidden ? -12 * ts : 4 * u), force: force)
        var box = CGRect.null
        for l in [title, queueLine, table] as [CALayer] + slotLayers.flatMap({ [$0.box, $0.track, $0.tag] as [CALayer] })
            where !l.isHidden { box = box.union(l.frame) }
        panel = !enabled || !haveData || box.isNull ? .null : box.insetBy(dx: -8 * u, dy: -6 * u)
    }

    /// The newer oven's table: its own counts, as it counts them.
    ///   made     finished bakes in the hour (duds are made too), then good ones
    ///   failed   bakes that ended in an error, of all that ended
    ///   bake     time in a slot, p50 · p90 over its last few hundred bakes
    ///   queue    the wait for a slot, same window
    ///   grab cache  asks answered from an earlier capture, of all asks
    ///   icon cache  icons and previews served from cache
    private func statRows(_ st: OvenStats, now: Double, feed: OvenFeed) -> [(String, String, NSColor)] {
        var rows: [(String, String, NSColor)] = []
        let h = { (k: String) in Int(st.hour[k] ?? 0) }
        // The oven counts from its start; a younger oven has less than an hour.
        let age = st.since > 0 ? now - st.since : 3_600
        let window = age < 3_600 ? (age < 60 ? "<1m" : "\(Int(age / 60))m") : "1h"
        let made = h("completed"), duds = h("duds"), failed = h("failed")
        rows.append(("made · \(window)", made == 0 ? "none" : "\(made)" + (duds > 0 ? " · \(made - duds) good" : ""),
                     made > 0 ? Self.hot : ink(0.6)))
        if duds > 0 { rows.append(("duds · \(window)", "\(duds) of \(made)", ink(0.75))) }
        if made + failed > 0 {
            rows.append(("failed · \(window)", "\(failed) of \(made + failed)", failed > 0 ? Self.red : ink(0.5)))
        }
        if let p50 = st.bakeP50 {
            rows.append(("bake", "\(Self.span(p50))" + (st.bakeP90.map { " · p90 \(Self.span($0))" } ?? ""), Self.amber))
        }
        if let p50 = st.queueP50 {
            rows.append(("queue wait", "\(Self.span(p50))" + (st.queueP90.map { " · p90 \(Self.span($0))" } ?? ""),
                         p50 > 5 ? Self.amber : ink(0.85)))
        }
        let hits = h("cacheHits"), asks = hits + h("cacheMisses")
        if asks > 0 {
            let dedup = h("deduped")
            rows.append(("grab cache · \(window)", "\(Int((Double(hits) / Double(asks) * 100).rounded()))% of \(asks)"
                            + (dedup > 0 ? " · \(dedup) joined" : ""), ink(0.85)))
        }
        let iconHits = h("iconCacheHits"), icons = iconHits + h("iconCacheMisses")
        if icons > 0 {
            rows.append(("icon cache · \(window)", "\(Int((Double(iconHits) / Double(icons) * 100).rounded()))% of \(icons)",
                         ink(0.85)))
        }
        // No size in the oven's counts: the median of the recent grabs seen.
        let good = feed.grabs.values.filter { $0.ok && !$0.dud }.sorted { $0.completedAt > $1.completedAt }.prefix(50)
        if !good.isEmpty {
            rows.append(("size", Self.bytes(Self.quantile(good.map { Double($0.size) }, 0.5)) + " median", ink(0.85)))
        }
        return rows
    }

    /// The efficiency table: every row is computed from what the stream sent.
    private func layoutTable(now: Double, feed: OvenFeed, right: Bool, edge: CGFloat, top: CGFloat, force: Bool) {
        if let st = feed.stats { return drawTable(statRows(st, now: now, feed: feed), right: right, edge: edge, top: top, force: force) }
        // An older oven sends no counts: work them out from the snapshots.
        let all = feed.grabs.values.sorted { $0.completedAt > $1.completedAt }
        let hour = all.filter { $0.completedAt > now - 3_600 }
        let sample = Array(all.prefix(50))
        let done = sample.filter(\.ok)
        let good = done.filter { !$0.dud }
        var rows: [(String, String, NSColor)] = []
        let partial = feed.recordSince > now - 3_600 ? "≥" : ""
        let okHour = hour.filter(\.ok).count
        let dudHour = hour.filter { $0.ok && $0.dud }.count
        rows.append(("made · 1h", hour.isEmpty ? "none" : "\(partial)\(okHour)" + (okHour == hour.count ? "" : " of \(hour.count)")
                        + (dudHour > 0 ? " · \(okHour - dudHour) good" : ""),
                     okHour > 0 ? Self.hot : ink(0.6)))
        if !done.isEmpty {
            let d = done.map(\.duration).filter { $0 > 0 }
            rows.append(("turnaround", "\(Self.span(Self.quantile(d, 0.5))) · p90 \(Self.span(Self.quantile(d, 0.9)))", Self.amber))
        }
        if !good.isEmpty {
            rows.append(("size", Self.bytes(Self.quantile(good.map { Double($0.size) }, 0.5)) + " median", ink(0.85)))
        }
        if !sample.isEmpty {
            let duds = done.filter(\.dud).count
            let failed = sample.count - done.count
            rows.append(("duds", "\(duds) of \(sample.count)", duds > 0 ? ink(0.75) : ink(0.5)))
            rows.append(("failed", "\(failed) of \(sample.count)", failed > 0 ? Self.red : ink(0.5)))
        }
        let cache = feed.cacheEvents.filter { $0.at > now - 3_600 }
        if !cache.isEmpty {
            let hits = cache.filter(\.hit).count
            rows.append(("icon cache · 1h", "\(Int((Double(hits) / Double(cache.count) * 100).rounded()))% of \(cache.count)",
                         ink(0.85)))
        }
        drawTable(rows, right: right, edge: edge, top: top, force: force)
    }

    private func drawTable(_ rows: [(String, String, NSColor)], right: Bool, edge: CGFloat, top: CGFloat, force: Bool) {
        let text = rows.map { "\($0.0)\t\($0.1)" }.joined(separator: "\n") + "\(dark)\(scale)"
        table.isHidden = rows.isEmpty || table.isHidden
        guard !rows.isEmpty else { return }
        if force || text != tableText {
            tableText = text
            let pixel = PixelFont.font(scale: scale)
            let font = pixel.map { $0 } ?? NSFont.monospacedSystemFont(ofSize: max(10, 10 * tu), weight: .medium)
            table.pixelated = pixel != nil
            let px = 1 / max(1, scale.rounded())
            let snap = { (v: CGFloat) in pixel == nil ? v : (v / px).rounded() * px }
            let pad = snap(pixel == nil ? 5 * tu : 4 * px)
            let gap = snap(pixel == nil ? 5 * tu : 3 * px)
            let keyW = ceil(rows.map { ($0.0 as NSString).size(withAttributes: [.font: font]).width }.max() ?? 8)
            let ascent = ceil(CTFontGetAscent(font))
            let lineH = ceil(ascent + CTFontGetDescent(font))
            var valueW: CGFloat = 0
            table.cells = rows.map { (k, v, color) in
                let value = NSAttributedString(string: v, attributes: [.font: font, .foregroundColor: color])
                valueW = max(valueW, ceil(value.size().width))
                return (NSAttributedString(string: k, attributes: [.font: font, .foregroundColor: ink(0.5)]), value)
            }
            let inset = snap(pixel == nil ? gap * 0.9 : 2 * px)
            let width = pad + keyW + gap * 2 + valueW + pad + px
            let height = inset * 2 + CGFloat(rows.count) * lineH + CGFloat(rows.count - 1) * gap + px
            table.string = nil
            table.bounds = CGRect(x: 0, y: 0, width: width, height: height)
            table.topInset = inset
            table.cellLeft = pad
            table.cellValueX = pad + keyW + gap * 2
            table.cellLineHeight = lineH
            table.cellAscent = ascent
            table.gridRows = rows.count
            table.gridRowStep = lineH + gap
            table.gridGap = gap
            table.gridColumnX = pad + keyW + gap
            table.gridColor = ink(0.2).cgColor
            table.borderWidth = 1
            table.borderColor = ink(0.22).cgColor
            table.cornerRadius = 2
            table.halo = dark ? Self.darkHalo : Self.lightHalo
        }
        let b = table.bounds
        table.frame = CGRect(x: right ? edge - b.width : edge, y: top - b.height, width: b.width, height: b.height)
    }

    // ── flight ───────────────────────────────────────────────────────────

    /// New grabs leave the mouth as soon as their image is in, a few seconds
    /// apart; with nothing new, the recent ones come round again, smaller
    /// and dimmer, one every eight seconds.
    private func launch(feed: OvenFeed) {
        guard laneSpan > 120 * u else { return }
        let t = CACurrentMediaTime()
        for a in feed.arrivals where a.seq > takenSeq {
            takenSeq = a.seq
            pending.append((a.id, t))
        }
        if pending.count > 8 { pending.removeFirst(pending.count - 8) }
        // Ask for every pending image now, so they arrive in parallel.
        for p in pending { if let url = feed.grabs[p.id]?.url { _ = feed.media(url) } }
        if let next = pending.first, t - lastLaunch > 2.5 {
            let grab = feed.grabs[next.id]
            let loaded = grab?.url.flatMap { feed.media($0) }
            if grab == nil || grab?.url == nil || loaded != nil || t - next.since > 15 {
                pending.removeFirst()
                if let grab { fly(grab, media: loaded ?? nil, fresh: true, at: t) }
            }
            return
        }
        guard pending.isEmpty, t - lastLaunch > 6 else { return }
        // Duds are counted in the table; only real media comes round again.
        let history = feed.grabs.values.filter { $0.url != nil && $0.ok && !$0.dud }
            .sorted { $0.completedAt > $1.completedAt }.prefix(20)
        guard !history.isEmpty else { return }
        let flying = Set(cards.filter { $0.until > t }.map(\.id))
        for _ in 0..<history.count {
            historyCursor = (historyCursor + 1) % history.count
            let g = history[history.index(history.startIndex, offsetBy: historyCursor)]
            guard !flying.contains(g.id), let url = g.url else { continue }
            if let m = feed.media(url) {
                fly(g, media: m, fresh: false, at: t)
                return
            }
        }
    }

    private func card(at t: CFTimeInterval) -> Card {
        if let free = cards.first(where: { $0.until <= t }) { return free }
        let c = Card()
        for l in [c.root, c.media] { l.actions = Self.still }
        c.media.magnificationFilter = .nearest
        c.media.masksToBounds = true
        c.root.addSublayer(c.media)
        c.root.addSublayer(Self.text(c.caption))
        c.root.contentsScale = scale
        c.media.contentsScale = scale
        c.caption.contentsScale = scale
        lane.addSublayer(c.root)
        cards.append(c)
        return c
    }

    private func fly(_ g: OvenGrab, media: OvenMedia?, fresh: Bool, at t: CFTimeInterval) {
        guard cards.count < 14 || cards.contains(where: { $0.until <= t }) else { return }
        lastLaunch = t
        let c = card(at: t)
        c.id = g.id
        // Depth: a hash of the id picks size, height in the band and speed.
        var hash: UInt64 = 1469598103934665603
        for b in g.id.utf8 { hash = (hash ^ UInt64(b)) &* 1099511628211 }
        let r1 = CGFloat(hash & 0xffff) / 65535, r2 = CGFloat((hash >> 16) & 0xffff) / 65535
        let depth = fresh ? 1 : 0.55 + 0.35 * r1
        let side = ((fresh ? 88 : 72) * u * depth).rounded()
        let right = OvenConfig.side != .left
        let band = 150 * u
        let y0 = mouth.y
        let y1 = mouth.y + (r2 - 0.5) * band + (fresh ? 0 : (r1 - 0.5) * 40 * u)
        let from = CGPoint(x: right ? mouth.x - side / 2 : mouth.x + side / 2, y: y0)
        let to = CGPoint(x: right ? -side : bounds.width + side, y: y1)
        let speed = (fresh ? 52 : 26 + 18 * depth) * u
        let duration = Double(abs(to.x - from.x) / speed)
        c.until = t + duration + 0.2

        c.root.removeAllAnimations()
        c.media.removeAllAnimations()
        c.root.bounds = CGRect(x: 0, y: 0, width: side, height: side + 13 * ts)
        c.root.position = to
        c.root.opacity = 0
        c.root.zPosition = depth
        c.media.frame = CGRect(x: 0, y: 13 * ts, width: side, height: side)
        c.media.cornerRadius = 4 * u
        c.media.borderWidth = fresh ? max(1, 1.5 * u) : max(1, u)
        let dim = g.dud || !g.ok
        c.media.borderColor = (g.ok ? (fresh ? Self.hot : ink(0.35)) : Self.red).cgColor
        c.media.backgroundColor = ink(0.08).cgColor
        if let frames = media?.frames, frames.count > 1, !g.dud {
            c.media.contents = frames[0]
            let loop = CAKeyframeAnimation(keyPath: "contents")
            loop.values = frames
            loop.calculationMode = .discrete
            loop.duration = max(0.6, min(6, media?.duration ?? 1))
            loop.repeatCount = .infinity
            c.media.add(loop, forKey: "loop")
        } else {
            c.media.contents = media?.frames.first
        }
        // Caption: code, then how long it took and what it weighs.
        let caption = NSMutableAttributedString(attributedString:
            words(g.piece + " ", max(9, 10 * tu), dim ? ink(0.55) : (fresh ? Self.hot : ink(0.85)), .heavy))
        // Bake time when the oven says it (queue wait left out), else request to upload.
        let took = g.bakeTime > 0 ? g.bakeTime : g.duration
        let tail = !g.ok ? g.status : g.dud ? "dud" : "\(Self.span(took)) \(Self.bytes(Double(g.size)))"
        caption.append(words(tail, max(9, 9 * tu), g.ok ? ink(0.55) : Self.red, .semibold))
        c.caption.string = caption
        c.caption.halo = dark ? Self.darkHalo : Self.lightHalo
        c.caption.frame = CGRect(x: 0, y: 0, width: max(side, ceil(caption.size().width) + 4), height: 12 * ts)

        let path = CGMutablePath()
        path.move(to: from)
        // Out of the mouth level, then easing into its lane.
        path.addCurve(to: to, control1: CGPoint(x: from.x + (to.x - from.x) * 0.25, y: y0),
                      control2: CGPoint(x: from.x + (to.x - from.x) * 0.55, y: y1))
        let move = CAKeyframeAnimation(keyPath: "position")
        move.path = path
        move.calculationMode = .paced
        let peak: Float = dim ? 0.5 : fresh ? 1 : 0.62 + 0.3 * Float(depth - 0.55)
        let fade = CAKeyframeAnimation(keyPath: "opacity")
        fade.values = [0, peak, peak, 0]
        fade.keyTimes = [0, 0.04, 0.88, 1]
        var parts: [CAAnimation] = [move, fade]
        if fresh {
            let pop = CAKeyframeAnimation(keyPath: "transform.scale")
            pop.values = [0.3, 1.25, 1, 1]
            pop.keyTimes = [0, NSNumber(value: 0.5 / duration), NSNumber(value: 1.1 / duration), 1]
            parts.append(pop)
        }
        let group = CAAnimationGroup()
        group.animations = parts
        group.duration = duration
        group.beginTime = 0
        c.root.add(group, forKey: "fly")
    }
}
