// UpdatePlugin — over-the-air self-update, so a new MacPal reaches every pal
// (Fía's star included) without a dmg riding over iMessage.
//
// @jeffrey publishes with `node macpal/release.mjs`, which uploads the
// notarized zip plus a tiny manifest to the releases bucket:
//
//   https://releases.aesthetic.computer/macpal/latest.json
//     { "version": "0.2.0", "url": "…/MacPal-0.2.0.zip", "sha256": "…" }
//
// The pal checks a couple of minutes after launch and then hourly. A newer
// version is downloaded to a temp dir, its sha256 verified against the
// manifest (TLS + hash = provenance), unzipped with ditto, and swapped into
// place: the old bundle slides aside, the new one lands at the same path, and
// the pal relaunches itself — through the launch agent when one manages it,
// plain `open` otherwise (Fía's star). Only an installed pal self-updates;
// a dev build running from the repo's build/ dir leaves itself alone.

import AppKit
import CryptoKit

final class UpdatePlugin: NSObject, PalPlugin {
    private let manifest = "https://releases.aesthetic.computer/macpal/latest.json"
    private let firstCheck = 120          // core ticks at ~1Hz → ~2 min in
    private let checkEvery = 3600         // then hourly
    private var tickCount = 0
    private var busy = false

    func attach(to c: PalController) {}

    func tick() {
        defer { tickCount += 1 }
        let due = tickCount == firstCheck ||
            (tickCount > firstCheck && (tickCount - firstCheck) % checkEvery == 0)
        if due { check() }
    }

    // ── the update pipeline, one stage per method ──────────────────────────
    private func check() {
        guard !busy, installedAppURL() != nil else { return }
        // Cache-bust so a CDN or URLCache never pins us to a stale manifest.
        guard let url = URL(string: "\(manifest)?t=\(tickCount)-\(ProcessInfo.processInfo.processIdentifier)")
        else { return }
        busy = true
        var req = URLRequest(url: url)
        req.timeoutInterval = 20
        req.cachePolicy = .reloadIgnoringLocalCacheData
        URLSession.shared.dataTask(with: req) { [weak self] data, _, _ in
            guard let self = self else { return }
            guard let data = data,
                  let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  let version = obj["version"] as? String,
                  let zipURL = (obj["url"] as? String).flatMap(URL.init(string:)),
                  let sha256 = obj["sha256"] as? String
            else { self.busy = false; return }
            let current = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "0"
            guard Self.isNewer(version, than: current) else { self.busy = false; return }
            print("MacPal update: \(current) → \(version), downloading \(zipURL.lastPathComponent)")
            self.download(zipURL, sha256: sha256, version: version)
        }.resume()
    }

    private func download(_ url: URL, sha256: String, version: String) {
        URLSession.shared.downloadTask(with: url) { [weak self] tmp, _, err in
            guard let self = self else { return }
            guard let tmp = tmp, err == nil else {
                print("MacPal update: download failed — \(err?.localizedDescription ?? "?")")
                self.busy = false; return
            }
            self.install(zip: tmp, sha256: sha256, version: version)
        }.resume()
    }

    private func install(zip: URL, sha256 expected: String, version: String) {
        defer { busy = false }
        guard let data = try? Data(contentsOf: zip) else { return }
        let sha = SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined()
        guard sha == expected.lowercased() else {
            print("MacPal update: sha256 mismatch, refusing (got \(sha.prefix(12))…)")
            return
        }

        let fm = FileManager.default
        let work = fm.temporaryDirectory.appendingPathComponent("MacPal-update-\(version)")
        try? fm.removeItem(at: work)
        try? fm.createDirectory(at: work, withIntermediateDirectories: true)

        // ditto -xk preserves the signed bundle exactly as notarized.
        let unzip = Process()
        unzip.executableURL = URL(fileURLWithPath: "/usr/bin/ditto")
        unzip.arguments = ["-xk", zip.path, work.path]
        try? unzip.run(); unzip.waitUntilExit()

        let fresh = work.appendingPathComponent("MacPal.app")
        guard fm.fileExists(atPath: fresh.appendingPathComponent("Contents/MacOS/MacPal").path),
              let installed = installedAppURL()
        else { print("MacPal update: unpacked bundle looks wrong, refusing"); return }

        // Swap: slide the running bundle aside (a same-volume rename is safe
        // while it executes), land the new one at the same path, then relaunch.
        let aside = work.appendingPathComponent("MacPal-old.app")
        do {
            do { try fm.moveItem(at: installed, to: aside) } catch {
                // Cross-volume tmp? Slide aside next to the install instead.
                let sib = installed.deletingLastPathComponent()
                    .appendingPathComponent(".MacPal-old-\(version).app")
                try? fm.removeItem(at: sib)
                try fm.moveItem(at: installed, to: sib)
            }
            try fm.moveItem(at: fresh, to: installed)
        } catch {
            print("MacPal update: swap failed — \(error)"); return
        }
        print("MacPal update: installed \(version), relaunching")
        Self.relaunch(installed)
    }

    /// The pal's own bundle, but only when it lives in an Applications folder —
    /// a repo build/ pal must never overwrite itself with a release.
    private func installedAppURL() -> URL? {
        let u = Bundle.main.bundleURL
        let inApplications = u.path.hasPrefix("/Applications/") ||
            u.path.hasPrefix(NSString(string: "~/Applications/").expandingTildeInPath + "/")
        return (inApplications && u.lastPathComponent == "MacPal.app") ? u : nil
    }

    static func isNewer(_ v: String, than cur: String) -> Bool {
        let a = v.split(separator: ".").map { Int($0) ?? 0 }
        let b = cur.split(separator: ".").map { Int($0) ?? 0 }
        for i in 0..<max(a.count, b.count) {
            let x = i < a.count ? a[i] : 0
            let y = i < b.count ? b[i] : 0
            if x != y { return x > y }
        }
        return false
    }

    /// Restart as the launch agent when one manages this pal (neo, blueberry),
    /// else a plain re-open (Fía's star). The helper shell outlives our exit.
    /// launchd's first respawn after a bundle swap flakes to "spawn failed"
    /// now and then, so kickstart gets a few tries — each verified with pgrep,
    /// since kickstart can exit 0 and still fail to spawn — before `open`
    /// carries the fallback.
    static func relaunch(_ app: URL) {
        let uid = getuid()
        let script = """
        sleep 1
        for i in 1 2 3; do
          /bin/launchctl kickstart -k gui/\(uid)/computer.aesthetic.macpal 2>/dev/null
          sleep 2
          /usr/bin/pgrep -xq MacPal && exit 0
        done
        /usr/bin/open -na "\(app.path)"
        """
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/bin/bash")
        p.arguments = ["-c", script]
        try? p.run()
        DispatchQueue.main.async { NSApp.terminate(nil) }
    }
}
