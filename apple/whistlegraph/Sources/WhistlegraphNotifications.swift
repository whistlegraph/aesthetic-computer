import UIKit
import UserNotifications

/// Push notifications through the AC network device registry. The APNs token
/// is reported with the device (/api/app-device, event "push"); the server sends
/// with shared/push.mjs sendToTarget to this device, this person, or a group.
/// Permission is asked once, after the first saved piece, never at launch.
@MainActor enum WhistlegraphNotifications {
    private static let askedKey = "whistlegraph-notifications-asked"
    private static let registeredKey = "whistlegraph-notifications-registered"

    /// Debug builds are signed with a development profile, so APNs issues sandbox tokens.
    static var environment: String {
        #if DEBUG
        return "sandbox"
        #else
        return "production"
        #endif
    }

    /// Fixtures and UI tests never prompt; a real phone does, like DeviceRegistry.
    private static var enabled: Bool { DeviceRegistry.reportsAllowed }

    /// On every activation: APNs tokens can change, so re-register when allowed;
    /// when the person has turned notifications off, stop targeting this device.
    static func refresh(account: WhistlegraphAccount) async {
        guard enabled else { return }
        let status = await UNUserNotificationCenter.current().notificationSettings().authorizationStatus
        switch status {
        case .authorized, .provisional, .ephemeral:
            UIApplication.shared.registerForRemoteNotifications()
        case .denied where UserDefaults.standard.bool(forKey: registeredKey):
            UserDefaults.standard.set(false, forKey: registeredKey)
            DeviceRegistry.report(.push, account: account, push: NSNull())
        default: break
        }
    }

    static func askAfterFirstPiece(account: WhistlegraphAccount) async {
        guard enabled, !UserDefaults.standard.bool(forKey: askedKey) else { return }
        guard await UNUserNotificationCenter.current().notificationSettings().authorizationStatus == .notDetermined else { return }
        UserDefaults.standard.set(true, forKey: askedKey)
        let granted = (try? await UNUserNotificationCenter.current().requestAuthorization(options: [.alert, .sound, .badge])) ?? false
        DeviceActionLog.shared.record(.notifications, granted ? .succeeded : .declined)
        if granted { UIApplication.shared.registerForRemoteNotifications() }
    }

    static func registered(token: Data, account: WhistlegraphAccount?) {
        UserDefaults.standard.set(true, forKey: registeredKey)
        let hex = token.map { String(format: "%02x", $0) }.joined()
        DeviceRegistry.report(.push, account: account, push: ["kind": "apns", "token": hex, "env": environment])
    }
}

final class WhistlegraphAppDelegate: NSObject, UIApplicationDelegate, UNUserNotificationCenterDelegate {
    weak var session: WhistlegraphSession?

    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil) -> Bool {
        UNUserNotificationCenter.current().delegate = self
        return true
    }

    func application(_ application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: Data) {
        Task { @MainActor in WhistlegraphNotifications.registered(token: deviceToken, account: session?.account) }
    }

    func application(_ application: UIApplication, didFailToRegisterForRemoteNotificationsWithError error: Error) {
        Task { @MainActor in DeviceActionLog.shared.recordError(.notifications, error) }
    }

    // Show notifications while Whistlegraph is open, too.
    func userNotificationCenter(_ center: UNUserNotificationCenter, willPresent notification: UNNotification) async -> UNNotificationPresentationOptions {
        [.banner, .list, .sound]
    }

    // A notification may carry `url: "/<piece>"`; tapping it opens that piece.
    func userNotificationCenter(_ center: UNUserNotificationCenter, didReceive response: UNNotificationResponse) async {
        guard let url = response.notification.request.content.userInfo["url"] as? String else { return }
        await MainActor.run { session?.openFromNotification(url) }
    }
}
