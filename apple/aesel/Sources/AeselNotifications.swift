import Foundation
import UserNotifications
#if os(macOS)
import AppKit
#else
import UIKit
#endif

/// Native notifications for the Aesel GUI, and its row in the AC network device
/// registry (POST /api/app-device). A turn that finishes, fails or waits for an
/// approval while Aesel is in the background posts a local notification;
/// clicking it brings that thread forward. Permission is asked once, when the
/// first turn starts. Debug builds stay silent unless AESEL_DEVICE_REPORTS=1.
@MainActor enum AeselNotifications {
    private static let askedKey = "aesel.notifications.asked"
    static let center = Delegate()

    private static var enabled: Bool {
        #if DEBUG
        return ProcessInfo.processInfo.environment["AESEL_DEVICE_REPORTS"] == "1"
        #else
        return true
        #endif
    }

    private static var appActive: Bool {
        #if os(macOS)
        return NSApplication.shared.isActive
        #else
        return UIApplication.shared.applicationState == .active
        #endif
    }

    /// Called with every session event before the renderer sees it.
    static func observe(_ event: [String: Any], session: Session, token: () -> String?) {
        guard enabled else { return }
        switch event["type"] as? String {
        case "signedIn": AeselDevices.report(.login, token: token())
        case "signedOut": AeselDevices.report(.logout, token: nil)
        case "approval":
            guard let approval = event["approval"] as? [String: Any], approval["id"] != nil else { return }
            let detail = approval["detail"] as? String ?? (approval["params"] as? [String: Any])?["command"] as? String ?? ""
            post(title: approval["title"] as? String ?? "Aesel needs your approval", body: detail, thread: session.currentThreadID, kind: "approval")
        case "bridge":
            let params = event["params"] as? [String: Any] ?? [:]
            switch event["method"] as? String {
            case "turn/started": askOnce()
            case "turn/completed":
                let turn = params["turn"] as? [String: Any] ?? [:]
                let status = turn["status"] as? String
                if status == "interrupted" { return }
                let failed = status == "failed" || turn["error"] != nil
                let message = (turn["error"] as? [String: Any])?["message"] as? String
                let reply = session.entries.last(where: { $0.kind == .ac })?.text ?? ""
                post(title: failed ? "Aesel stopped with a problem" : "Aesel is done",
                     body: failed ? (message ?? "The turn failed.") : reply, thread: session.currentThreadID, kind: failed ? "failed" : "done")
            default: break
            }
        default: break
        }
    }

    /// `aesel://notify?title=…&body=…&kind=…&from=tui` from the Aesel TUI, which
    /// already checked that its terminal is not frontmost. Asks for permission
    /// first if this app never has, so TUI-only use still gets notifications.
    static func handle(_ url: URL) {
        guard url.scheme == "aesel", url.host == "notify",
              let items = URLComponents(url: url, resolvingAgainstBaseURL: false)?.queryItems else { return }
        let value = { (name: String) in items.first(where: { $0.name == name })?.value ?? "" }
        let title = String(value("title").prefix(120)), kind = String(value("kind").prefix(16))
        guard !title.isEmpty else { return }
        Task {
            let center = UNUserNotificationCenter.current()
            if await center.notificationSettings().authorizationStatus == .notDetermined {
                UserDefaults.standard.set(true, forKey: askedKey)
                _ = try? await center.requestAuthorization(options: [.alert, .sound])
            }
            let content = UNMutableNotificationContent()
            content.title = title
            content.subtitle = "Aesel TUI"
            content.body = String(value("body").prefix(240))
            content.sound = .default
            content.threadIdentifier = "tui"
            try? await center.add(UNNotificationRequest(identifier: "tui:\(kind)", content: content, trigger: nil))
        }
    }

    private static func askOnce() {
        guard !UserDefaults.standard.bool(forKey: askedKey) else { return }
        UserDefaults.standard.set(true, forKey: askedKey)
        Task {
            let granted = (try? await UNUserNotificationCenter.current().requestAuthorization(options: [.alert, .sound])) ?? false
            if granted { registerRemote() }
        }
    }

    /// Remote push lets the network reach this Mac while Aesel is closed. Only the
    /// Developer ID build carries the entitlement; elsewhere registration fails quietly.
    static func registerRemoteIfAllowed() async {
        guard enabled else { return }
        let status = await UNUserNotificationCenter.current().notificationSettings().authorizationStatus
        if status == .authorized || status == .provisional { registerRemote() }
    }

    private static func registerRemote() {
        #if os(macOS)
        NSApplication.shared.registerForRemoteNotifications()
        #else
        UIApplication.shared.registerForRemoteNotifications()
        #endif
    }

    /// The APNs token goes to the device registry with the bundle this build runs as.
    static func registered(deviceToken: Data) {
        let hex = deviceToken.map { String(format: "%02x", $0) }.joined()
        #if DEBUG
        let environment = "sandbox"
        #else
        let environment = "production"
        #endif
        AeselDevices.report(.push, token: SessionHost.currentToken(), push: [
            "kind": "apns", "token": hex, "env": environment, "topic": Bundle.main.bundleIdentifier ?? "",
        ])
    }

    private static func post(title: String, body: String, thread: String, kind: String) {
        guard !appActive else { return }
        let content = UNMutableNotificationContent()
        content.title = title
        content.body = String(body.trimmingCharacters(in: .whitespacesAndNewlines).prefix(240))
        content.sound = .default
        content.threadIdentifier = thread
        content.userInfo = ["thread": thread, "kind": kind]
        // One notification per thread and kind: a newer "done" replaces the last.
        let request = UNNotificationRequest(identifier: "\(thread):\(kind)", content: content, trigger: nil)
        UNUserNotificationCenter.current().add(request)
    }

    final class Delegate: NSObject, UNUserNotificationCenterDelegate {
        func install() { UNUserNotificationCenter.current().delegate = self }
        func userNotificationCenter(_ center: UNUserNotificationCenter, willPresent notification: UNNotification) async -> UNNotificationPresentationOptions {
            [.banner, .list, .sound]
        }
        func userNotificationCenter(_ center: UNUserNotificationCenter, didReceive response: UNNotificationResponse) async {
            guard let thread = response.notification.request.content.userInfo["thread"] as? String else { return }
            await MainActor.run { SessionHost.focus(thread: thread) }
        }
    }
}

/// The GUI's row in the AC network device registry.
@MainActor enum AeselDevices {
    enum Event: String { case open, login, logout, push }
    private static var opened = false

    /// macOS has no identifierForVendor; keep one id for this install.
    private static var deviceId: String {
        #if os(iOS)
        if let id = UIDevice.current.identifierForVendor?.uuidString { return id }
        #endif
        let defaults = UserDefaults.standard
        if let id = defaults.string(forKey: "aesel.deviceId") { return id }
        let id = UUID().uuidString
        defaults.set(id, forKey: "aesel.deviceId")
        return id
    }

    static func reportOpenOnce(token: String?) {
        guard !opened else { return }
        opened = true
        report(.open, token: token)
    }

    static func report(_ event: Event, token: String?, push: [String: Any]? = nil) {
        #if DEBUG
        guard ProcessInfo.processInfo.environment["AESEL_DEVICE_REPORTS"] == "1" else { return }
        #endif
        let info = Bundle.main.infoDictionary ?? [:]
        var body: [String: Any] = ["app": "aesel", "deviceId": deviceId, "event": event.rawValue, "label": "Aesel"]
        #if os(macOS)
        body["platform"] = "mac"
        body["os"] = ProcessInfo.processInfo.operatingSystemVersionString
        body["model"] = sysctl("hw.model")
        #else
        body["platform"] = UIDevice.current.userInterfaceIdiom == .pad ? "ipados" : "ios"
        body["os"] = UIDevice.current.systemVersion
        body["model"] = sysctl("hw.machine")
        #endif
        if let version = info["CFBundleShortVersionString"] as? String { body["version"] = version }
        if let build = info["CFBundleVersion"] as? String, build.allSatisfy(\.isNumber) { body["build"] = build }
        if let push { body["push"] = push }
        var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/app-device")!, timeoutInterval: 10)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        if let token, event != .logout { request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization") }
        request.httpBody = try? JSONSerialization.data(withJSONObject: body)
        Task { _ = try? await URLSession.shared.data(for: request) } // best effort
    }

    private static func sysctl(_ name: String) -> String {
        var size = 0
        sysctlbyname(name, nil, &size, nil, 0)
        guard size > 0 else { return "" }
        var bytes = [CChar](repeating: 0, count: size)
        sysctlbyname(name, &bytes, &size, nil, 0)
        return String(cString: bytes)
    }
}
