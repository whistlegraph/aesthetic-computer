// ArtPlugin — the glyph-art twin of AffirmationsPlugin. Where that plugin pulls
// a caption, this one pulls the star's *poses* and hot-swaps them live, so
// @jeffrey can restyle Fía's star from the wire without shipping a new build.
//
// The star polls an aesthetic.computer endpoint
//   GET /api/macpal-art?to=<recipient>  →  { rev, poses: { <name>: "<svg>" } }
// every ~45s; when `rev` bumps, each pose SVG is written to
//   ~/Library/Application Support/MacPal/art/<name>.svg
// (which PalConfig.posePaths prefers over the bundle art), the star reloads and
// pops the new pose, and a compact status is reported back to
//   POST /api/macpal-art  { report: { rev, poses: [{name,bytes,hash,source}] } }
// so the device round-trip (?from=device) reflects what's actually on screen.
//
// @jeffrey pushes with: node macpal/art.mjs face.svg --to fia

import AppKit
import CryptoKit

final class ArtPlugin: NSObject, PalPlugin {
    private let recipient: String
    private let host: String
    private let artDir: String
    private let revFile: String
    private weak var c: PalController?

    private var lastRev = -1
    private var polling = false
    private var reported = false
    private var tickCount = 0
    private let pollEvery = 45         // core ticks at ~1Hz → ~45s, like the caption

    init(recipient: String, host: String, supportDir: String) {
        self.recipient = recipient
        self.host = host.hasSuffix("/") ? String(host.dropLast()) : host
        self.artDir = supportDir + "/art"
        self.revFile = supportDir + "/art/.rev"
        super.init()
        try? FileManager.default.createDirectory(atPath: artDir, withIntermediateDirectories: true)
        // Remember the rev we last applied so a relaunch doesn't re-download art
        // that's already on disk (the files persist between runs).
        if let raw = try? String(contentsOfFile: revFile, encoding: .utf8),
           let r = Int(raw.trimmingCharacters(in: .whitespacesAndNewlines)) {
            lastRev = r
        }
    }

    func attach(to controller: PalController) { c = controller }

    func tick() {
        if tickCount % pollEvery == 0 { poll() }
        tickCount += 1
    }

    // ── network ────────────────────────────────────────────────────────────
    private func poll() {
        guard !polling, let url = URL(string: "\(host)/api/macpal-art?to=\(recipient)") else { return }
        polling = true
        var req = URLRequest(url: url)
        req.timeoutInterval = 12
        req.cachePolicy = .reloadIgnoringLocalCacheData
        URLSession.shared.dataTask(with: req) { [weak self] data, _, _ in
            guard let self = self else { return }
            defer { self.polling = false }
            guard let data = data,
                  let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
                  let rev = (obj["rev"] as? Int) ?? (obj["rev"] as? NSNumber)?.intValue
            else { return }
            let poses = obj["poses"] as? [String: String] ?? [:]
            if rev != self.lastRev {
                DispatchQueue.main.async { self.apply(rev: rev, poses: poses) }
            } else if !self.reported {
                // Already in sync from a prior launch — still report once so the
                // device slot shows this star is alive and on the right rev.
                DispatchQueue.main.async { self.report(rev: rev) }
            }
        }.resume()
    }

    // Reconcile the art dir to exactly the server's poses (write new/changed,
    // delete removed), then hot-swap and report.
    private func apply(rev: Int, poses: [String: String]) {
        let fm = FileManager.default
        let want = Set(poses.keys.map { $0 + ".svg" })
        if let existing = try? fm.contentsOfDirectory(atPath: artDir) {
            for f in existing where f.hasSuffix(".svg") && !want.contains(f) {
                try? fm.removeItem(atPath: artDir + "/" + f)
            }
        }
        for (name, svg) in poses {
            try? svg.write(toFile: artDir + "/" + name + ".svg", atomically: true, encoding: .utf8)
        }
        lastRev = rev
        try? "\(rev)".write(toFile: revFile, atomically: true, encoding: .utf8)
        c?.reloadArt()
        report(rev: rev)
    }

    // A compact status of the poses on screen — no SVG bodies, so the endpoint
    // accepts it unauthenticated (the star has no admin token).
    private func report(rev: Int) {
        guard let paths = c?.currentArtPaths(),
              let url = URL(string: "\(host)/api/macpal-art") else { return }
        var out: [[String: Any]] = []
        for p in paths {
            guard let data = FileManager.default.contents(atPath: p) else { continue }
            let hash = SHA256.hash(data: data).prefix(8).map { String(format: "%02x", $0) }.joined()
            out.append([
                "name": ArtPlugin.poseName(p),
                "bytes": data.count,
                "hash": hash,
                "source": p.hasPrefix(artDir) ? "wire" : "bundle",
            ])
        }
        let body: [String: Any] = ["to": recipient, "report": ["rev": rev, "poses": out]]
        guard let json = try? JSONSerialization.data(withJSONObject: body) else { return }
        var req = URLRequest(url: url)
        req.httpMethod = "POST"
        req.setValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpBody = json
        req.timeoutInterval = 12
        URLSession.shared.dataTask(with: req) { [weak self] _, _, _ in self?.reported = true }.resume()
    }

    // A pose file's name: star-glyph-2.svg → glyph-2, star-glyph-sing.svg → sing,
    // art/glyph.svg → glyph. Mirrors the CLI's mapping.
    static func poseName(_ path: String) -> String {
        var b = (path as NSString).lastPathComponent
        if b.hasSuffix(".svg") { b = String(b.dropLast(4)) }
        if b.hasPrefix("star-") { b = String(b.dropFirst(5)) }
        return b == "glyph-sing" ? "sing" : b
    }
}
