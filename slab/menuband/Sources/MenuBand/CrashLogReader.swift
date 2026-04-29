import Foundation

/// Reads MenuBand crash reports from `~/Library/Logs/DiagnosticReports/`,
/// where macOS deposits them automatically (.ips on Sonoma+, .crash on
/// older). The popover surfaces the count + a "Send" button so the user
/// can opt in to uploading them to the lith endpoint at
/// `aesthetic.computer/menuband-logs`. We never auto-upload.
enum CrashLogReader {
    static let directoryPath: String = ("~/Library/Logs/DiagnosticReports" as NSString).expandingTildeInPath

    static let lithEndpoint = URL(string: "https://aesthetic.computer/menuband-logs")!

    /// MenuBand crash reports from the *currently-installed* build, newest
    /// first. Filter cutoff = the executable's mtime, which install.sh
    /// updates on every reinstall — so crashes from older, since-fixed
    /// builds (very common during heavy dev iteration) don't pollute
    /// the count surfaced to the user.
    static func recentLogs() -> [URL] {
        let dirURL = URL(fileURLWithPath: directoryPath)
        guard let entries = try? FileManager.default.contentsOfDirectory(
            at: dirURL,
            includingPropertiesForKeys: [.contentModificationDateKey],
            options: [.skipsHiddenFiles]
        ) else {
            return []
        }
        let installedAt = bundleInstalledAt()
        return entries
            .filter { url in
                let name = url.lastPathComponent
                guard name.hasPrefix("MenuBand-") else { return false }
                let ext = url.pathExtension
                guard ext == "ips" || ext == "crash" else { return false }
                let mtime = (try? url.resourceValues(forKeys: [.contentModificationDateKey]))?
                    .contentModificationDate ?? .distantPast
                return mtime > installedAt
            }
            .sorted { (a, b) in
                let ad = (try? a.resourceValues(forKeys: [.contentModificationDateKey]))?
                    .contentModificationDate ?? .distantPast
                let bd = (try? b.resourceValues(forKeys: [.contentModificationDateKey]))?
                    .contentModificationDate ?? .distantPast
                return ad > bd
            }
    }

    /// mtime of the running executable. install.sh `cp`s the new binary
    /// into place on every reinstall, so this advances with each build.
    private static func bundleInstalledAt() -> Date {
        guard let exe = Bundle.main.executableURL else { return .distantPast }
        return (try? exe.resourceValues(forKeys: [.contentModificationDateKey]))?
            .contentModificationDate ?? .distantPast
    }

    /// Upload one crash report to lith. Calls back on the main thread.
    static func upload(_ url: URL,
                       version: String,
                       completion: @escaping (Bool, String) -> Void) {
        guard let body = try? String(contentsOf: url, encoding: .utf8),
              let data = body.data(using: .utf8) else {
            DispatchQueue.main.async { completion(false, "couldn't read \(url.lastPathComponent)") }
            return
        }
        var req = URLRequest(url: lithEndpoint)
        req.httpMethod = "POST"
        req.setValue("text/plain; charset=utf-8", forHTTPHeaderField: "Content-Type")
        req.setValue(url.lastPathComponent, forHTTPHeaderField: "X-Menuband-Filename")
        req.setValue(version, forHTTPHeaderField: "X-Menuband-Version")
        req.timeoutInterval = 12
        req.httpBody = data
        URLSession.shared.dataTask(with: req) { _, resp, err in
            DispatchQueue.main.async {
                if let err = err {
                    completion(false, err.localizedDescription)
                    return
                }
                let code = (resp as? HTTPURLResponse)?.statusCode ?? 0
                if (200..<300).contains(code) {
                    completion(true, "HTTP \(code)")
                } else {
                    completion(false, "HTTP \(code)")
                }
            }
        }.resume()
    }
}
