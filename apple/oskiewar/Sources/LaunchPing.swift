// LaunchPing — one POST to aesthetic.computer/api/app-open per launch, so we
// can count opens and new installs. What goes out: the app's id, its version,
// the platform (ios / ipados / mac), a random install id minted on the first
// launch, and whether this is that first launch. Nothing names the person,
// their device or their account (see toolchain/analytics/VISITS.md).
// The id lives in UserDefaults rather than the Keychain on purpose: deleting
// the app deletes it. Off in DEBUG builds and when "acLaunchPingDisabled" is set.
import Foundation
#if canImport(UIKit)
import UIKit
#endif

@MainActor enum LaunchPing {
    private static var sent = false

    static func send(_ app: String) {
        #if !DEBUG
        let defaults = UserDefaults.standard
        guard !sent, !defaults.bool(forKey: "acLaunchPingDisabled") else { return }
        sent = true
        let version = (Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "")
            .filter { "0123456789.".contains($0) }
        guard !version.isEmpty else { return }
        let fresh = defaults.string(forKey: "acInstallID") == nil
        if fresh { defaults.set(UUID().uuidString.lowercased(), forKey: "acInstallID") }
        let body: [String: Any] = ["app": app, "version": version, "platform": platform,
                                   "install": defaults.string(forKey: "acInstallID") ?? "", "fresh": fresh]
        var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/app-open")!,
                                 timeoutInterval: 10)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try? JSONSerialization.data(withJSONObject: body)
        let session = URLSession(configuration: .ephemeral)
        session.dataTask(with: request) { _, _, _ in }.resume() // fire and forget
        session.finishTasksAndInvalidate()
        #endif
    }

    private static var platform: String {
        #if os(macOS) || targetEnvironment(macCatalyst)
        return "mac"
        #else
        return UIDevice.current.userInterfaceIdiom == .pad ? "ipados" : "ios"
        #endif
    }
}
