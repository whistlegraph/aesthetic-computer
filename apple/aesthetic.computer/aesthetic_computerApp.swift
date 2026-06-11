import SwiftUI
import UserNotifications
import WebKit

@main
struct aesthetic_computerApp: App {
    @UIApplicationDelegateAdaptor(AppDelegate.self) var delegate

    var body: some Scene {
        WindowGroup {
            ContentView()
        }
    }

}

// 🔔 Push notifications arrive straight from APNs now — no Firebase.
// The raw device token is handed to the WebView, where bios.mjs registers it
// with /api/register-push-token (kind: "apns") and the server fans out topic
// + per-device sends itself (see shared/push.mjs).
class AppDelegate: NSObject, UIApplicationDelegate {
    static var shared: AppDelegate?
    var appWebView: WKWebView?
    private var apnsToken: String?

    override init() {
        super.init()
        AppDelegate.shared = self
    }

    func application(
        _ application: UIApplication,
        didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?
    ) -> Bool {
        // 🔔 Register for remote notifications. This shows a permission dialog on first run, to
        // show the dialog at a more appropriate time move this registration accordingly.
        UNUserNotificationCenter.current().delegate = self

        let authOptions: UNAuthorizationOptions = [.alert, .badge, .sound]
        UNUserNotificationCenter.current().requestAuthorization(
            options: authOptions,
            completionHandler: { _, _ in }
        )

        application.registerForRemoteNotifications()

        return true
    }

    func application(
        _ application: UIApplication,
        didFailToRegisterForRemoteNotificationsWithError error: Error
    ) {
        print("Unable to register for remote notifications: \(error.localizedDescription)")
    }

    func application(
        _ application: UIApplication,
        didRegisterForRemoteNotificationsWithDeviceToken deviceToken: Data
    ) {
        let token = deviceToken.map { String(format: "%02x", $0) }.joined()
        print("📱 🔔 APNs token retrieved: \(token)")
        apnsToken = token
        deliverPushTokenToWebView(token)
    }

    // Called from the WebView bridge when the user types "notifs".
    func triggerSubscribe() {
        if let token = apnsToken {
            deliverPushTokenToWebView(token)
        } else {
            UIApplication.shared.registerForRemoteNotifications()
        }
    }

    // Called from the WebView bridge when the user types "nonotifs".
    func triggerUnsubscribe() {
        let script = "window.iOSUnregisterPushToken && window.iOSUnregisterPushToken();"
        appWebView?.evaluateJavaScript(script, completionHandler: nil)
    }

    private func deliverPushTokenToWebView(_ token: String, attempt: Int = 0) {
        // Retry a few times so we don't lose the token if the WebView hasn't
        // finished loading AC's bios.mjs yet (iOSReceivePushToken lives there).
        let script = "window.iOSReceivePushToken && window.iOSReceivePushToken('\(token)');"
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            guard let webView = self.appWebView else {
                if attempt < 20 {
                    DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                        self.deliverPushTokenToWebView(token, attempt: attempt + 1)
                    }
                }
                return
            }
            webView.evaluateJavaScript(script) { _, error in
                if let error = error {
                    print("📱 🔔 Failed to hand token to WebView: \(error)")
                    if attempt < 20 {
                        DispatchQueue.main.asyncAfter(deadline: .now() + 0.5) {
                            self.deliverPushTokenToWebView(token, attempt: attempt + 1)
                        }
                    }
                } else {
                    print("📱 🔔 Handed APNs token to WebView (attempt \(attempt)).")
                }
            }
        }
    }

}

extension AppDelegate: UNUserNotificationCenterDelegate {

    // Notification arrives while the app is open.
    func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        willPresent notification: UNNotification
    ) async
    -> UNNotificationPresentationOptions
    {
        print(notification.request.content.userInfo)  // Print full message.

        // Change this to your preferred presentation option.
        return [[.sound]]  // return [[.banner, .sound]]
    }

    // Notification tapped while the app wasn't open — jump to its piece.
    func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        didReceive response: UNNotificationResponse
    ) async {
        let userInfo = response.notification.request.content.userInfo

        if let pieceData = userInfo["piece"] as? String {
            if pieceData != "" {
                let script = "iOSAppSwitchPiece('\(pieceData)');"
                appWebView?.evaluateJavaScript(script, completionHandler: nil)
            }
        }

        print(userInfo)  // Print full message.
    }
}
