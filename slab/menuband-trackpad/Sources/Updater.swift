// Updater — over-the-air self-update for the TrackDrum helper.
//
// TrackDrum is a background agent with no window and no menu: nobody ever
// "opens" it to be told an update exists, and it is the half of Menu Band a
// sandboxed App Store build cannot ship for itself. So it updates itself, the
// way MacPal's UpdatePlugin does (macpal/Sources/UpdatePlugin.swift — same
// manifest shape, same hash check, same swap).
//
// `./release.sh` uploads the notarized zip plus a manifest beside the DMG:
//
//   https://assets.aesthetic.computer/menuband/trackdrum-latest.json
//     { "version": "1.3", "url": "…/TrackDrum-for-Menu-Band-1.3.zip", "sha256": "…" }
//
// A check runs a couple of minutes after launch and then hourly. A newer
// version is downloaded, its sha256 verified against the manifest (TLS + hash
// = provenance), unpacked with ditto so the notarized signature survives
// byte-for-byte, and swapped into place; the helper then relaunches itself.
// Only an installed helper self-updates — a build running out of the repo's
// build/ directory leaves itself alone.

import AppKit
import CryptoKit

final class TrackDrumUpdater {
    private static let manifest =
        "https://assets.aesthetic.computer/menuband/trackdrum-latest.json"
    private static let bundleName = "TrackDrum for Menu Band.app"
    private static let executableName = "MenuBandTrackpad"

    private let firstCheck: TimeInterval = 120
    private let checkEvery: TimeInterval = 3600
    private var timer: Timer?
    private var busy = false

    /// Nothing here touches the trackpad loop: the check is a URLSession task
    /// on a background queue, and the only main-thread work is the relaunch.
    func start() {
        guard Self.installedAppURL() != nil else {
            NSLog("TrackDrum update: not an installed copy, self-update off")
            return
        }
        let timer = Timer(timeInterval: firstCheck, repeats: false) { [weak self] _ in
            self?.check()
            self?.scheduleHourly()
        }
        RunLoop.main.add(timer, forMode: .common)
        self.timer = timer
    }

    func stop() {
        timer?.invalidate()
        timer = nil
    }

    private func scheduleHourly() {
        timer?.invalidate()
        let timer = Timer(timeInterval: checkEvery, repeats: true) { [weak self] _ in
            self?.check()
        }
        RunLoop.main.add(timer, forMode: .common)
        self.timer = timer
    }

    // ── the update pipeline, one stage per method ──────────────────────────
    private func check() {
        guard !busy, Self.installedAppURL() != nil else { return }
        // Cache-bust so the CDN or URLCache never pins us to a stale manifest.
        let stamp = "\(Int(Date().timeIntervalSince1970))-\(ProcessInfo.processInfo.processIdentifier)"
        guard let url = URL(string: "\(Self.manifest)?t=\(stamp)") else { return }
        busy = true
        var request = URLRequest(url: url)
        request.timeoutInterval = 20
        request.cachePolicy = .reloadIgnoringLocalCacheData
        URLSession.shared.dataTask(with: request) { [weak self] data, _, _ in
            guard let self else { return }
            guard let data,
                  let object = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  let version = object["version"] as? String,
                  let zipURL = (object["url"] as? String).flatMap(URL.init(string:)),
                  let sha256 = object["sha256"] as? String
            else { self.busy = false; return }
            let current = Bundle.main
                .infoDictionary?["CFBundleShortVersionString"] as? String ?? "0"
            guard Self.isNewer(version, than: current) else { self.busy = false; return }
            NSLog("TrackDrum update: %@ → %@, downloading %@",
                  current, version, zipURL.lastPathComponent)
            self.download(zipURL, sha256: sha256, version: version)
        }.resume()
    }

    private func download(_ url: URL, sha256: String, version: String) {
        URLSession.shared.downloadTask(with: url) { [weak self] temp, _, error in
            guard let self else { return }
            guard let temp, error == nil else {
                NSLog("TrackDrum update: download failed — %@",
                      error?.localizedDescription ?? "?")
                self.busy = false
                return
            }
            self.install(zip: temp, sha256: sha256, version: version)
        }.resume()
    }

    private func install(zip: URL, sha256 expected: String, version: String) {
        defer { busy = false }
        guard let data = try? Data(contentsOf: zip) else { return }
        let sha = SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined()
        guard sha == expected.lowercased() else {
            NSLog("TrackDrum update: sha256 mismatch, refusing (got %@…)",
                  String(sha.prefix(12)))
            return
        }

        let fm = FileManager.default
        let work = fm.temporaryDirectory
            .appendingPathComponent("TrackDrum-update-\(version)")
        try? fm.removeItem(at: work)
        try? fm.createDirectory(at: work, withIntermediateDirectories: true)

        // ditto -xk preserves the signed bundle exactly as notarized.
        let unzip = Process()
        unzip.executableURL = URL(fileURLWithPath: "/usr/bin/ditto")
        unzip.arguments = ["-xk", zip.path, work.path]
        try? unzip.run()
        unzip.waitUntilExit()

        let fresh = work.appendingPathComponent(Self.bundleName)
        let freshBinary = fresh
            .appendingPathComponent("Contents/MacOS/\(Self.executableName)")
        guard fm.fileExists(atPath: freshBinary.path),
              let installed = Self.installedAppURL()
        else {
            NSLog("TrackDrum update: unpacked bundle looks wrong, refusing")
            return
        }

        // Swap: slide the running bundle aside (a same-volume rename is safe
        // while it executes), land the new one at the same path, then relaunch.
        let aside = work.appendingPathComponent("TrackDrum-old.app")
        do {
            do { try fm.moveItem(at: installed, to: aside) } catch {
                // Cross-volume tmp? Slide aside next to the install instead.
                let sibling = installed.deletingLastPathComponent()
                    .appendingPathComponent(".TrackDrum-old-\(version).app")
                try? fm.removeItem(at: sibling)
                try fm.moveItem(at: installed, to: sibling)
            }
            try fm.moveItem(at: fresh, to: installed)
        } catch {
            NSLog("TrackDrum update: swap failed — %@", "\(error)")
            return
        }
        NSLog("TrackDrum update: installed %@, relaunching", version)
        Self.relaunch(installed)
    }

    /// The helper's own bundle, but only where a real install lives — a repo
    /// build/ copy must never overwrite itself with a release.
    private static func installedAppURL() -> URL? {
        let url = Bundle.main.bundleURL
        let userApps = NSString(string: "~/Applications/").expandingTildeInPath + "/"
        let installed = url.path.hasPrefix("/Applications/")
            || url.path.hasPrefix(userApps)
        return (installed && url.lastPathComponent == bundleName) ? url : nil
    }

    static func isNewer(_ candidate: String, than current: String) -> Bool {
        let a = candidate.split(separator: ".").map { Int($0) ?? 0 }
        let b = current.split(separator: ".").map { Int($0) ?? 0 }
        for i in 0..<max(a.count, b.count) {
            let x = i < a.count ? a[i] : 0
            let y = i < b.count ? b[i] : 0
            if x != y { return x > y }
        }
        return false
    }

    /// Menu Band reconnects to the helper on its own, so a bare re-open is
    /// enough — there is no launch agent for this bundle. The helper shell
    /// outlives our exit so the new copy starts after this process is gone.
    static func relaunch(_ app: URL) {
        let script = """
        sleep 1
        /usr/bin/open -na "\(app.path)"
        """
        let process = Process()
        process.executableURL = URL(fileURLWithPath: "/bin/bash")
        process.arguments = ["-c", script]
        try? process.run()
        DispatchQueue.main.async { NSApp.terminate(nil) }
    }
}
