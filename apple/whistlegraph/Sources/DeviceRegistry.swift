import Foundation
import UIKit

/// Reports this device to the AC network device registry (POST /api/app-device):
/// app build and version, model, iOS version, and, when signed in, the account
/// (verified server-side from the bearer token). Each activation counts as an open.
/// The device id is identifierForVendor, which survives reinstalls while any
/// computer.aesthetic app remains installed. Fixtures and UI tests never
/// register; a debug build on a real phone does, like a release build, unless
/// launched with WHISTLEGRAPH_DEVICE_REPORTS=0.
@MainActor enum DeviceRegistry {
    enum Event: String { case open, seen, login, logout, push }

    /// `push`: an APNs registration dictionary, or NSNull() when notifications were turned off.
    static func report(_ event: Event, account: WhistlegraphAccount?, push: Any? = nil) {
        guard reportsAllowed, let deviceId = UIDevice.current.identifierForVendor?.uuidString else { return }
        let info = Bundle.main.infoDictionary ?? [:]
        var body: [String: Any] = [
            "app": "whistlegraph", "deviceId": deviceId, "event": event.rawValue,
            "platform": UIDevice.current.userInterfaceIdiom == .pad ? "ipados" : "ios",
            "model": machine, "os": UIDevice.current.systemVersion, "label": UIDevice.current.model,
        ]
        if let version = info["CFBundleShortVersionString"] as? String { body["version"] = version }
        if let build = info["CFBundleVersion"] as? String { body["build"] = build }
        if let push { body["push"] = push }
        Task {
            // Logout is reported after the Keychain sign-in is gone; it unbinds unsigned.
            let token = event == .logout ? nil : try? await account?.token()
            var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/app-device")!, timeoutInterval: 10)
            request.httpMethod = "POST"
            request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            if let token { request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization") }
            request.httpBody = try? JSONSerialization.data(withJSONObject: body)
            _ = try? await URLSession.shared.data(for: request) // best effort; the next open retries
        }
    }

    static var reportsAllowed: Bool {
        let env = ProcessInfo.processInfo.environment
        if env["WHISTLEGRAPH_DEVICE_REPORTS"] == "0" || env["WHISTLEGRAPH_DEVICE_REPORTS"] == "1" { return env["WHISTLEGRAPH_DEVICE_REPORTS"] == "1" }
        #if DEBUG
        return !NativeScreenFixture.enabled && env["WHISTLEGRAPH_ACCOUNT_ENTRY_TEST"] != "1" && env["WHISTLEGRAPH_NATIVE_SCREEN_FIXTURE"] == nil && env["WALKIE_NATIVE_SCREEN_FIXTURE"] == nil
        #else
        return true
        #endif
    }

    /// Hardware identifier such as "iPhone15,3".
    private static let machine: String = {
        var info = utsname(); uname(&info)
        return withUnsafeBytes(of: &info.machine) { raw in
            String(decoding: raw.prefix(while: { $0 != 0 }), as: UTF8.self)
        }
    }()
}
