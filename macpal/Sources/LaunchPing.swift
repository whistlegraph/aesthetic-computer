// LaunchPing — one anonymous "MacPal opened" count per launch.
//
// POSTs {app: "macpal", version, platform: "mac", install, fresh} to
// https://aesthetic.computer/api/app-open. `install` is a random UUID minted
// on first launch and kept in UserDefaults (not the Keychain) on purpose, so
// it goes when the app's data does; `fresh` is true only on that launch.
// Nothing here identifies the person: no account, handle, device, name or
// address rides along (see toolchain/analytics/VISITS.md).
//
// Fire-and-forget: no retries, errors swallowed, 10s ceiling, ephemeral
// session. Skipped in DEBUG builds, and when the `acLaunchPingDisabled`
// default is set:  defaults write <bundle id> acLaunchPingDisabled -bool YES

import Foundation

enum LaunchPing {
    private static var sent = false

    static func send() {
        #if DEBUG
        return
        #else
        let d = UserDefaults.standard
        // build.sh never defines DEBUG, so a repo build/ pal is told apart by
        // where it runs from, the way UpdatePlugin's installedAppURL does.
        let path = Bundle.main.bundleURL.path
        let installed = path.hasPrefix("/Applications/") ||
            path.hasPrefix(NSString(string: "~/Applications/").expandingTildeInPath + "/")
        guard installed, !sent, !d.bool(forKey: "acLaunchPingDisabled"),
              let version = Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String,
              version.range(of: #"^\d+(\.\d+){0,3}$"#, options: .regularExpression) != nil,
              let url = URL(string: "https://aesthetic.computer/api/app-open")
        else { return }
        sent = true

        var install = d.string(forKey: "acInstallID") ?? ""
        let fresh = UUID(uuidString: install) == nil
        if fresh {
            install = UUID().uuidString.lowercased()
            d.set(install, forKey: "acInstallID")
        }

        let body: [String: Any] = [
            "app": "macpal", "version": version, "platform": "mac",
            "install": install, "fresh": fresh,
        ]
        var req = URLRequest(url: url)
        req.httpMethod = "POST"
        req.setValue("application/json", forHTTPHeaderField: "Content-Type")
        req.httpBody = try? JSONSerialization.data(withJSONObject: body)

        let config = URLSessionConfiguration.ephemeral
        config.timeoutIntervalForRequest = 10
        config.timeoutIntervalForResource = 10
        let session = URLSession(configuration: config)
        session.dataTask(with: req) { _, _, _ in }.resume()
        session.finishTasksAndInvalidate()
        #endif
    }
}
