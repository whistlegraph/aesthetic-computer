// Updater.swift — NCCd updates itself, the way MacPal and TrackDrum do
// (macpal/Sources/UpdatePlugin.swift: same manifest shape, hash check, swap).
//
// `./release.sh` puts the notarized app, zipped, beside a manifest:
//
//   https://assets.aesthetic.computer/nccd/latest.json
//     { "version": "0.3.1", "url": "…/NCCd-0.3.1.zip", "sha256": "…" }
//
// A check runs half a minute after launch and then every half hour (and from
// the menu). A newer version is downloaded and must pass three checks before
// it replaces anything: its sha256 is the manifest's, its code signature is
// whole, and it is signed by the same Developer ID team as the copy that is
// running. It is unpacked with ditto so the notarized signature survives byte
// for byte, swapped into place, and the app starts again on the same circle
// with the words still in the composer. Only an installed copy updates itself;
// a build run from the repo's build/ folder leaves itself alone.

import AppKit
import CryptoKit
import Security

final class Updater {
    // Test aid: NCC_UPDATE_MANIFEST names another manifest (a file:// one will
    // do) and brings the first check forward. The three checks still apply, so
    // it cannot make the app take a bundle that another developer signed.
    private static let override = ProcessInfo.processInfo.environment["NCC_UPDATE_MANIFEST"]
    static let manifest = override ?? "https://assets.aesthetic.computer/nccd/latest.json"
    private static let bundleName = "NCCd.app"

    private var timer: Timer?
    private var busy = false
    var onStatus: ((String) -> Void)?          // a line for the sheet's status row
    var readyToRestart: (() -> Bool)?          // false while the member is typing
    var willRestart: (() -> Void)?             // save what the next launch restores

    func start() {
        guard Self.installedAppURL() != nil else { return }
        DispatchQueue.main.asyncAfter(deadline: .now() + (Self.override == nil ? 30 : 6)) { [weak self] in
            guard let self else { return }
            self.check()
            let t = Timer(timeInterval: 1800, repeats: true) { [weak self] _ in self?.check() }
            RunLoop.main.add(t, forMode: .common)
            self.timer = t
        }
    }

    private func say(_ s: String) { DispatchQueue.main.async { self.onStatus?(s) } }

    // `asked` = the member chose Check for Updates, so every outcome gets a line.
    func check(asked: Bool = false) {
        guard !busy else { return }
        guard Self.installedAppURL() != nil else {
            if asked { say("this copy runs from a build folder; it does not update itself") }
            return
        }
        // A new address each time, so no cache pins the app to an old manifest.
        let stamp = Self.manifest.hasPrefix("file:") ? "" : "?t=\(Int(Date().timeIntervalSince1970))"
        guard let url = URL(string: Self.manifest + stamp) else { return }
        busy = true
        var req = URLRequest(url: url)
        req.timeoutInterval = 20
        req.cachePolicy = .reloadIgnoringLocalCacheData
        URLSession.shared.dataTask(with: req) { [weak self] data, _, _ in
            guard let self else { return }
            guard let data, let j = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  let version = j["version"] as? String, let sha = j["sha256"] as? String,
                  let zip = (j["url"] as? String).flatMap(URL.init(string:)),
                  zip.scheme == "https" || (Self.override != nil && zip.isFileURL) else {
                self.busy = false
                if asked { self.say("the update server did not answer") }
                return
            }
            let current = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "0"
            guard Self.isNewer(version, than: current) else {
                self.busy = false
                if asked { self.say("NCCd \(current) is the newest") }
                return
            }
            self.say("getting NCCd \(version)…")
            URLSession.shared.downloadTask(with: zip) { [weak self] temp, _, _ in
                guard let self else { return }
                guard let temp else { self.busy = false; return self.say("the update did not download") }
                self.install(zip: temp, sha256: sha, version: version)
            }.resume()
        }.resume()
    }

    private func install(zip: URL, sha256 expected: String, version: String) {
        defer { busy = false }
        guard let data = try? Data(contentsOf: zip) else { return }
        let sha = SHA256.hash(data: data).map { String(format: "%02x", $0) }.joined()
        guard sha == expected.lowercased() else { return say("update refused: the download is not the published file") }

        let fm = FileManager.default
        let work = fm.temporaryDirectory.appendingPathComponent("NCCd-update-\(version)")
        try? fm.removeItem(at: work)
        try? fm.createDirectory(at: work, withIntermediateDirectories: true)
        let unzip = Process()               // ditto keeps the signed bundle exactly as notarized
        unzip.executableURL = URL(fileURLWithPath: "/usr/bin/ditto")
        unzip.arguments = ["-xk", zip.path, work.path]
        try? unzip.run()
        unzip.waitUntilExit()

        let fresh = work.appendingPathComponent(Self.bundleName)
        guard fm.fileExists(atPath: fresh.appendingPathComponent("Contents/MacOS/NCCd").path),
              let installed = Self.installedAppURL() else { return say("update refused: the bundle looks wrong") }
        guard Self.signedByThisTeam(fresh) else { return say("update refused: it is not signed by NCCd's developer") }

        // Wait for a pause in the typing, then swap and start again.
        DispatchQueue.main.async {
            self.say("NCCd \(version) is ready; it restarts in a moment")
            self.swapWhenQuiet(fresh: fresh, installed: installed, work: work, version: version, tries: 0)
        }
    }

    private func swapWhenQuiet(fresh: URL, installed: URL, work: URL, version: String, tries: Int) {
        if readyToRestart?() == false, tries < 150 {          // at most five minutes
            DispatchQueue.main.asyncAfter(deadline: .now() + 2) {
                self.swapWhenQuiet(fresh: fresh, installed: installed, work: work, version: version, tries: tries + 1)
            }
            return
        }
        let fm = FileManager.default
        // Slide the running bundle aside (a rename on one volume is safe while
        // it runs), land the new one at the same path.
        do {
            do { try fm.moveItem(at: installed, to: work.appendingPathComponent("NCCd-old.app")) } catch {
                let sibling = installed.deletingLastPathComponent().appendingPathComponent(".NCCd-old-\(version).app")
                try? fm.removeItem(at: sibling)
                try fm.moveItem(at: installed, to: sibling)
            }
            try fm.moveItem(at: fresh, to: installed)
        } catch {
            return say("the update could not be put in place: \(error.localizedDescription)")
        }
        willRestart?()
        // A shell outlives this process, so the new copy starts once it is gone.
        // It comes forward only if this one was in front.
        let p = Process()
        p.executableURL = URL(fileURLWithPath: "/bin/bash")
        p.arguments = ["-c", "sleep 1; /usr/bin/open -n\(NSApp.isActive ? "" : "g") \"\(installed.path)\""]
        try? p.run()
        NSApp.terminate(nil)
    }

    // The new bundle's signature is whole, and its Developer ID team is the
    // running copy's own. A build with no team (ad hoc) accepts no update.
    static func signedByThisTeam(_ app: URL) -> Bool {
        var me: SecCode?
        var mine: SecStaticCode?
        var info: CFDictionary?
        guard SecCodeCopySelf([], &me) == errSecSuccess, let me,
              SecCodeCopyStaticCode(me, [], &mine) == errSecSuccess, let mine,
              SecCodeCopySigningInformation(mine, SecCSFlags(rawValue: kSecCSSigningInformation), &info) == errSecSuccess,
              let team = (info as? [String: Any])?[kSecCodeInfoTeamIdentifier as String] as? String,
              team.allSatisfy({ $0.isLetter || $0.isNumber }) else { return false }
        var requirement: SecRequirement?
        let text = "anchor apple generic and certificate leaf[subject.OU] = \"\(team)\""
            + " and identifier \"\(Bundle.main.bundleIdentifier ?? "")\""
        var fresh: SecStaticCode?
        guard SecRequirementCreateWithString(text as CFString, [], &requirement) == errSecSuccess, let requirement,
              SecStaticCodeCreateWithPath(app as CFURL, [], &fresh) == errSecSuccess, let fresh else { return false }
        let flags = SecCSFlags(rawValue: kSecCSCheckAllArchitectures | kSecCSCheckNestedCode | kSecCSStrictValidate)
        return SecStaticCodeCheckValidity(fresh, flags, requirement) == errSecSuccess
    }

    // This bundle, but only where an install lives.
    private static func installedAppURL() -> URL? {
        let url = Bundle.main.bundleURL
        let userApps = NSString(string: "~/Applications/").expandingTildeInPath + "/"
        let installed = url.path.hasPrefix("/Applications/") || url.path.hasPrefix(userApps)
        return installed && url.lastPathComponent == bundleName ? url : nil
    }

    static func isNewer(_ candidate: String, than current: String) -> Bool {
        let a = candidate.split(separator: ".").map { Int($0) ?? 0 }
        let b = current.split(separator: ".").map { Int($0) ?? 0 }
        for i in 0..<max(a.count, b.count) {
            let x = i < a.count ? a[i] : 0, y = i < b.count ? b[i] : 0
            if x != y { return x > y }
        }
        return false
    }
}
