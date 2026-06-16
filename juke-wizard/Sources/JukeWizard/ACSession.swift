// ACSession.swift — canonical shared AC session reader for the macOS app suite.
//
// SOURCE OF TRUTH: shared/swift/ACSession.swift. The standalone SwiftPM apps
// (date-wizard, wave-wizard, clip-wizard, juke-wizard, slab menubar, menuband)
// each keep a COPY of this file in their own Sources/ — there is no shared SPM
// target across them. When you change this file, re-copy it into each consumer
// (a one-liner: `cp shared/swift/ACSession.swift <app>/Sources/<App>/`).
//
// One sign-in serves the whole suite. The AC stack (`ac-login` CLI, ac-os, the
// AC Electron app) writes a single session token at ~/.ac-token:
//
//   { "access_token": "<jwe>", "refresh_token": "...", "id_token": "...",
//     "expires_at": <ms-epoch>,
//     "user": { "handle": "jeffrey", "email": "...", "sub": "auth0|…",
//               "name": "...", "picture": "..." } }
//
// Use `access_token` as the Authorization Bearer for aesthetic.computer APIs.
// For display use `handle` (show "@handle"); never surface email/name (PII).
//
// "Broadcast" = the shared file. ACSession.shared.startWatching { … } fires the
// instant ~/.ac-token changes (atomic-write aware — it watches the parent
// directory), so a sign-in/out in ANY app (or the Electron tray) updates every
// running app live, with no restart and no polling. A best-effort
// NSDistributedNotification ("computer.aesthetic.session.changed") is also
// posted/observed for instant Swift↔Swift refresh.
import Foundation

final class ACSession {
    static let shared = ACSession()

    static let didChangeNotification =
        Notification.Name("computer.aesthetic.session.changed")

    // ~/.ac-token
    static var tokenURL: URL {
        FileManager.default.homeDirectoryForCurrentUser
            .appendingPathComponent(".ac-token")
    }

    // ── on-disk shape ────────────────────────────────────────────────
    private struct User: Codable {
        var handle: String?
        var email: String?
        var sub: String?
        var name: String?
        var picture: String?
    }
    private struct TokenFile: Codable {
        var access_token: String?
        var refresh_token: String?
        var id_token: String?
        var expires_at: Double?      // ms-epoch
        var user: User?
    }

    private func load() -> TokenFile? {
        guard let data = try? Data(contentsOf: Self.tokenURL) else { return nil }
        return try? JSONDecoder().decode(TokenFile.self, from: data)
    }

    // ── public read surface ──────────────────────────────────────────

    enum State { case signedIn, expired, signedOut }

    var state: State {
        guard let tf = load(), let t = tf.access_token, !t.isEmpty else { return .signedOut }
        if let exp = tf.expires_at, exp <= Date().timeIntervalSince1970 * 1000 { return .expired }
        return .signedIn
    }

    /// The current valid access token, or nil if missing/unparseable/expired.
    func token() -> String? {
        guard let tf = load(), let t = tf.access_token, !t.isEmpty else { return nil }
        if let exp = tf.expires_at, exp <= Date().timeIntervalSince1970 * 1000 { return nil }
        return t
    }

    /// The AC @handle (without the leading "@"), or nil. Safe to display.
    var handle: String? { load()?.user?.handle }

    /// "@handle" for display, falling back to a neutral label (never email/PII).
    var displayName: String? { handle.map { $0.hasPrefix("@") ? $0 : "@\($0)" } }

    /// Auth0 subject id (for API calls that key off the user).
    var sub: String? { load()?.user?.sub }

    /// expires_at in ms-epoch, if known.
    var expiresAt: Double? { load()?.expires_at }

    // ── live broadcast (file-watch + distributed notification) ───────
    // We watch BOTH the parent directory and the file itself, because writers
    // differ: `ac-login` overwrites ~/.ac-token IN PLACE (fs.writeFile → same
    // inode → a directory event does NOT fire, but the file's .write does),
    // while `ac-login logout` / atomic replacers delete/rename the file (the
    // file watch goes stale → only the directory event fires). The directory
    // watch also re-arms the file watch when the token reappears.
    private var dirSource: DispatchSourceFileSystemObject?
    private var dirFD: Int32 = -1
    private var fileSource: DispatchSourceFileSystemObject?
    private var fileFD: Int32 = -1
    private var observers: [UUID: () -> Void] = [:]
    private var distributedObserver: NSObjectProtocol?

    /// Register a callback that fires (on the main queue) whenever the shared
    /// session changes — sign-in, sign-out, refresh. Returns a token you can
    /// pass to `stopWatching` (or ignore; everything is torn down on dealloc).
    @discardableResult
    func startWatching(_ onChange: @escaping () -> Void) -> UUID {
        let id = UUID()
        observers[id] = onChange
        installDirectoryWatchIfNeeded()
        installFileWatchIfNeeded()
        installDistributedObserverIfNeeded()
        return id
    }

    func stopWatching(_ id: UUID) { observers[id] = nil }

    private func installDirectoryWatchIfNeeded() {
        guard dirSource == nil else { return }
        let dir = Self.tokenURL.deletingLastPathComponent()
        let fd = open(dir.path, O_EVTONLY)
        guard fd >= 0 else { return }
        dirFD = fd
        let src = DispatchSource.makeFileSystemObjectSource(
            fileDescriptor: fd, eventMask: [.write, .rename, .delete], queue: .main)
        src.setEventHandler { [weak self] in
            // A directory change may mean the token was (re)created/replaced —
            // (re)arm the file watch, then report.
            self?.installFileWatchIfNeeded()
            self?.fireChanged()
        }
        src.setCancelHandler { [weak self] in
            if let fd = self?.dirFD, fd >= 0 { close(fd); self?.dirFD = -1 }
        }
        src.resume()
        dirSource = src
    }

    private func installFileWatchIfNeeded() {
        guard fileSource == nil else { return }
        let fd = open(Self.tokenURL.path, O_EVTONLY)
        guard fd >= 0 else { return }   // file not present yet — dir watch will re-arm
        fileFD = fd
        let src = DispatchSource.makeFileSystemObjectSource(
            fileDescriptor: fd, eventMask: [.write, .extend, .rename, .delete, .revoke], queue: .main)
        src.setEventHandler { [weak self] in
            guard let self else { return }
            let data = src.data
            self.fireChanged()
            // File was replaced/removed → this watch is stale; tear down so the
            // directory watch can re-arm a fresh one.
            if !data.intersection([.rename, .delete, .revoke]).isEmpty { src.cancel() }
        }
        src.setCancelHandler { [weak self] in
            if let fd = self?.fileFD, fd >= 0 { close(fd); self?.fileFD = -1 }
            self?.fileSource = nil
        }
        src.resume()
        fileSource = src
    }

    private func installDistributedObserverIfNeeded() {
        guard distributedObserver == nil else { return }
        distributedObserver = DistributedNotificationCenter.default().addObserver(
            forName: Self.didChangeNotification, object: nil, queue: .main
        ) { [weak self] _ in self?.notifyObservers() }
    }

    // Debounce burst of fs events; only notify on a real token-string change.
    private var lastTokenSeen: String?
    private var debounceItem: DispatchWorkItem?
    private func fireChanged() {
        debounceItem?.cancel()
        let item = DispatchWorkItem { [weak self] in
            guard let self else { return }
            let now = self.load()?.access_token
            if now != self.lastTokenSeen {
                self.lastTokenSeen = now
                self.notifyObservers()
            }
        }
        debounceItem = item
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.25, execute: item)
    }

    private func notifyObservers() { for cb in observers.values { cb() } }

    /// Tell other AC apps the session changed (call after sign-in/out you drive).
    func broadcastChanged() {
        DistributedNotificationCenter.default().postNotificationName(
            Self.didChangeNotification, object: nil, userInfo: nil,
            deliverImmediately: true)
    }

    // ── sign-in helper ───────────────────────────────────────────────
    /// Launch `ac-login` in Terminal so the user can sign in without leaving
    /// the app. (Re)writes ~/.ac-token on success; the file-watch picks it up.
    func runAcLogin() {
        let script = "tell application \"Terminal\"\nactivate\ndo script \"ac-login\"\nend tell"
        let task = Process()
        task.executableURL = URL(fileURLWithPath: "/usr/bin/osascript")
        task.arguments = ["-e", script]
        try? task.run()
    }
}
