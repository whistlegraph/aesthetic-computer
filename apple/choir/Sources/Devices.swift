import UIKit
import UserNotifications

/// This phone in the AC network device registry (POST /api/app-device, app
/// "choir"): build, model, iOS, the signed-in account, and the APNs token once
/// notifications are allowed. The server pushes new messages and Discover
/// items to each member's devices (system/backend/choir-push.mjs).
@MainActor enum Devices {
    enum Event: String { case open, login, logout, push }
    weak static var account: Account?

    static var environment: String {
        #if DEBUG
        return "sandbox"
        #else
        return "production"
        #endif
    }

    /// `push`: an APNs registration, or NSNull() when notifications were turned off.
    static func report(_ event: Event, push: Any? = nil) {
        guard let deviceId = UIDevice.current.identifierForVendor?.uuidString else { return }
        let info = Bundle.main.infoDictionary ?? [:]
        var body: [String: Any] = [
            "app": "choir", "deviceId": deviceId, "event": event.rawValue, "platform": "ios",
            "model": machine, "os": UIDevice.current.systemVersion, "label": UIDevice.current.model,
        ]
        if let v = info["CFBundleShortVersionString"] as? String { body["version"] = v }
        if let b = info["CFBundleVersion"] as? String { body["build"] = b }
        if let push { body["push"] = push }
        Task {
            let token = event == .logout ? nil : await account?.token()
            var r = URLRequest(url: URL(string: "https://aesthetic.computer/api/app-device")!, timeoutInterval: 10)
            r.httpMethod = "POST"
            r.setValue("application/json", forHTTPHeaderField: "Content-Type")
            if let token { r.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization") }
            r.httpBody = try? JSONSerialization.data(withJSONObject: body)
            _ = try? await URLSession.shared.data(for: r) // best effort; the next open retries
        }
    }

    /// On every activation: re-register (tokens change) when allowed.
    static func refresh() async {
        let status = await UNUserNotificationCenter.current().notificationSettings().authorizationStatus
        if [.authorized, .provisional, .ephemeral].contains(status) { UIApplication.shared.registerForRemoteNotifications() }
    }

    /// Asked once a person can post, never at launch. Returns whether allowed.
    @discardableResult
    static func ask() async -> Bool {
        let center = UNUserNotificationCenter.current()
        let status = await center.notificationSettings().authorizationStatus
        if status == .denied { return false }
        let granted = status == .notDetermined
            ? ((try? await center.requestAuthorization(options: [.alert, .sound, .badge])) ?? false)
            : true
        Trail.send(granted ? "push:allowed" : "push:declined")
        if granted { UIApplication.shared.registerForRemoteNotifications() }
        return granted
    }

    static func registered(_ token: Data) {
        let hex = token.map { String(format: "%02x", $0) }.joined()
        report(.push, push: ["kind": "apns", "token": hex, "env": environment, "topic": "computer.aesthetic.choir"])
    }

    private static let machine: String = {
        var info = utsname(); uname(&info)
        return withUnsafeBytes(of: &info.machine) { String(decoding: $0.prefix(while: { $0 != 0 }), as: UTF8.self) }
    }()
}

/// Where a tapped notification wants to go: "/thread/<id>" or "/discover".
@MainActor final class Router: ObservableObject {
    static let shared = Router()
    @Published var thread: String?
    @Published var discover = false
    func open(_ url: String) {
        if url == "/discover" { discover = true }
        else if url.hasPrefix("/thread/") { thread = String(url.dropFirst("/thread/".count)) }
    }
}

final class AppDelegate: NSObject, UIApplicationDelegate, UNUserNotificationCenterDelegate {
    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil) -> Bool {
        UNUserNotificationCenter.current().delegate = self
        return true
    }
    func application(_ application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: Data) {
        Task { @MainActor in Devices.registered(deviceToken) }
    }
    func application(_ application: UIApplication, didFailToRegisterForRemoteNotificationsWithError error: Error) {
        Task { @MainActor in Trail.send("push:register-failed", ["why": error.localizedDescription]) }
    }
    // Show banners while choir is open too, except for the room you're looking at.
    func userNotificationCenter(_ center: UNUserNotificationCenter, willPresent notification: UNNotification) async -> UNNotificationPresentationOptions {
        [.banner, .list, .sound]
    }
    func userNotificationCenter(_ center: UNUserNotificationCenter, didReceive response: UNNotificationResponse) async {
        guard let url = response.notification.request.content.userInfo["url"] as? String else { return }
        Trail.send("push:opened", ["url": url])
        await MainActor.run { Router.shared.open(url) }
    }
}
