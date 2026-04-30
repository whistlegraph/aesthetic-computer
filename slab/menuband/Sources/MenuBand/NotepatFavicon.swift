import AppKit

/// Lazily-loaded notepat.com favicon, cached on disk. The Notepat
/// mode button in the popover uses this as its glyph so the brand
/// is always live — whatever favicon the site is reporting today is
/// what shows up on the button.
///
/// Cache strategy:
/// 1. On first access we synchronously return whatever's on disk
///    (or `nil` if we haven't fetched yet).
/// 2. In parallel, we kick off an async refresh from the network.
///    When it lands, the file is overwritten and observers fire.
/// 3. Subsequent app launches read the cached file immediately,
///    refreshing in the background if it's older than the TTL.
enum NotepatFavicon {
    private static let url = URL(string: "https://notepat.com/favicon.ico")!
    /// 24h refresh interval — the favicon doesn't change often, but
    /// a long-running session shouldn't drift forever.
    private static let refreshInterval: TimeInterval = 24 * 3600

    /// Cached image (in-memory). Refreshes automatically on first
    /// access if the disk cache is missing or stale.
    static var image: NSImage? {
        if let cached = cachedImage { return cached }
        // First access — load from disk and trigger refresh.
        loadFromDisk()
        triggerRefreshIfNeeded()
        return cachedImage
    }

    private static var cachedImage: NSImage?
    private static var lastDiskMTime: Date?
    private static var refreshInFlight = false

    private static var cacheURL: URL {
        let fm = FileManager.default
        let base = (try? fm.url(for: .applicationSupportDirectory,
                                in: .userDomainMask,
                                appropriateFor: nil,
                                create: true)) ?? fm.temporaryDirectory
        let dir = base.appendingPathComponent("MenuBand", isDirectory: true)
        try? fm.createDirectory(at: dir, withIntermediateDirectories: true)
        return dir.appendingPathComponent("notepat-favicon.ico")
    }

    private static func loadFromDisk() {
        guard let data = try? Data(contentsOf: cacheURL) else { return }
        cachedImage = NSImage(data: data)
        let attrs = try? FileManager.default.attributesOfItem(atPath: cacheURL.path)
        lastDiskMTime = attrs?[.modificationDate] as? Date
    }

    private static func triggerRefreshIfNeeded() {
        let stale = (lastDiskMTime.map { Date().timeIntervalSince($0) } ?? .infinity)
            > refreshInterval
        guard stale, !refreshInFlight else { return }
        refreshInFlight = true
        let task = URLSession.shared.dataTask(with: url) { data, _, _ in
            defer { refreshInFlight = false }
            guard let data = data, NSImage(data: data) != nil else { return }
            try? data.write(to: cacheURL, options: .atomic)
            DispatchQueue.main.async {
                cachedImage = NSImage(data: data)
                lastDiskMTime = Date()
                NotificationCenter.default.post(name: .notepatFaviconLoaded,
                                                 object: nil)
            }
        }
        task.resume()
    }

    /// Posted on the main queue when a fresh favicon lands. Observers
    /// (e.g. the popover) can re-bind their image to the latest cache.
    static let didLoadNotification = Notification.Name.notepatFaviconLoaded
}

extension Notification.Name {
    static let notepatFaviconLoaded = Notification.Name("NotepatFaviconLoaded")
}
