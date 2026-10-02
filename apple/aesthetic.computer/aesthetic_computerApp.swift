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

// Push permission is requested only by `notifs`. Native opt-out also stops APNs
// immediately; the web bridge retries server cleanup if the device is offline.
@MainActor
class AppDelegate: NSObject, UIApplicationDelegate {
    static var shared: AppDelegate?
    weak var appWebView: WKWebView?
    private let webViews = NSHashTable<WKWebView>.weakObjects()
    private let optedInKey = "acNotificationsOptedIn"
    private let tokenKey = "acNotificationToken"
    private var revision = 0
    private var pendingSubscribe: Int?
    private var optedIn: Bool {
        get { UserDefaults.standard.bool(forKey: optedInKey) }
        set { UserDefaults.standard.set(newValue, forKey: optedInKey) }
    }
    private var apnsToken: String? {
        get { UserDefaults.standard.string(forKey: tokenKey) }
        set { UserDefaults.standard.set(newValue, forKey: tokenKey) }
    }

    override init() {
        super.init()
        AppDelegate.shared = self
    }

    func application(
        _ application: UIApplication,
        didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?
    ) -> Bool {
        UNUserNotificationCenter.current().delegate = self
        if optedIn {
            // Refresh an existing opt-in silently. Never request permission here.
            let current = revision
            Task {
                let settings = await UNUserNotificationCenter.current().notificationSettings()
                guard revision == current, optedIn else { return }
                if settings.authorizationStatus == .authorized || settings.authorizationStatus == .provisional {
                    application.registerForRemoteNotifications()
                }
            }
        } else {
            // Includes upgrades from the former automatic launch-time subscription.
            application.unregisterForRemoteNotifications()
        }
        return true
    }

    func application(_ application: UIApplication, didFailToRegisterForRemoteNotificationsWithError error: Error) {
        finishSubscribe(ok: false)
    }

    func application(_ application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: Data) {
        apnsToken = deviceToken.map { String(format: "%02x", $0) }.joined()
        if let webView = appWebView { synchronizePush(in: webView) }
    }

    func pushBridgeReady(in webView: WKWebView) {
        webViews.add(webView)
        appWebView = webView
        synchronizePush(in: webView)
    }

    func triggerSubscribe(in webView: WKWebView) {
        guard pendingSubscribe == nil else { return }
        webViews.add(webView)
        appWebView = webView
        revision += 1
        let current = revision
        pendingSubscribe = current
        Task {
            let center = UNUserNotificationCenter.current()
            let settings = await center.notificationSettings()
            guard revision == current else { return }
            var granted = settings.authorizationStatus == .authorized || settings.authorizationStatus == .provisional
            if settings.authorizationStatus == .notDetermined {
                do { granted = try await center.requestAuthorization(options: [.alert, .badge, .sound]) }
                catch { finishSubscribe(ok: false); return }
            }
            guard revision == current else { return }
            guard granted else {
                optedIn = false
                pendingSubscribe = nil
                UIApplication.shared.unregisterForRemoteNotifications()
                for view in webViews.allObjects { synchronizePush(in: view) }
                notificationAlert("Notifications are off", "Allow notifications in iOS Settings, then enter ‘notifs’ again.", settings: true)
                return
            }
            optedIn = true
            UIApplication.shared.registerForRemoteNotifications()
            // A missing APNs callback must not leave the command stuck forever.
            try? await Task.sleep(nanoseconds: 25_000_000_000)
            if pendingSubscribe == current { finishSubscribe(ok: false) }
        }
    }

    func triggerUnsubscribe(in webView: WKWebView) {
        webViews.add(webView)
        appWebView = webView
        revision += 1
        let current = revision
        pendingSubscribe = nil
        optedIn = false
        UIApplication.shared.unregisterForRemoteNotifications()
        Task {
            var cleaned = true
            for view in webViews.allObjects {
                guard revision == current else { return }
                let ok = await sendPushState(in: view, enabled: false, token: apnsToken)
                cleaned = cleaned && ok
            }
            guard revision == current else { return }
            if cleaned { apnsToken = nil }
            notificationAlert("Notifications off", cleaned
                ? "Enter ‘notifs’ to turn them on again."
                : "Notifications are off on this device. Server cleanup will retry when you reconnect.")
        }
    }

    private func synchronizePush(in webView: WKWebView) {
        let current = revision
        let enabled = optedIn
        let token = apnsToken
        guard !enabled || token != nil else { return }
        Task {
            guard revision == current else { return }
            let ok = await sendPushState(in: webView, enabled: enabled, token: token)
            guard revision == current else { return }
            if enabled { finishSubscribe(ok: ok) }
            else if ok { apnsToken = nil }
        }
    }

    private func sendPushState(in webView: WKWebView, enabled: Bool, token: String?) async -> Bool {
        guard webView.url?.scheme == "https", webView.url?.host == "aesthetic.computer" else { return false }
        return await withCheckedContinuation { continuation in
            webView.callAsyncJavaScript("""
                if (window.iOSPushBridgeVersion !== 2) return {ok: false};
                return enabled ? await window.iOSReceivePushToken(token)
                    : await window.iOSUnregisterPushToken(token);
                """, arguments: ["enabled": enabled, "token": token ?? ""], in: nil, in: .page) { result in
                if case .success(let value) = result {
                    let response = value as? [String: Any]
                    continuation.resume(returning: response?["ok"] as? Bool == true && response?["enabled"] as? Bool == enabled)
                } else { continuation.resume(returning: false) }
            }
        }
    }

    private func finishSubscribe(ok: Bool) {
        guard pendingSubscribe == revision else { return }
        pendingSubscribe = nil
        notificationAlert(ok ? "Notifications on" : "Couldn’t finish subscribing", ok
            ? "You’ll receive screams and moods. Enter ‘nonotifs’ to turn them off."
            : "Check your connection and enter ‘notifs’ again.")
    }

    private func notificationAlert(_ title: String, _ message: String, settings: Bool = false) {
        let alert = UIAlertController(title: title, message: message, preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .cancel))
        if settings {
            alert.addAction(UIAlertAction(title: "Settings", style: .default) { _ in
                if let url = URL(string: UIApplication.openSettingsURLString) { UIApplication.shared.open(url) }
            })
        }
        var controller = appWebView?.window?.rootViewController
        while let presented = controller?.presentedViewController { controller = presented }
        controller?.present(alert, animated: true)
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
        return optedIn ? [.sound] : []
    }

    // Notification tapped while the app wasn't open — jump to its piece.
    func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        didReceive response: UNNotificationResponse
    ) async {
        let userInfo = response.notification.request.content.userInfo

        if let pieceData = userInfo["piece"] as? String {
            if pieceData != "" {
                guard let webView = appWebView, webView.url?.host == "aesthetic.computer",
                      webView.url?.scheme == "https" else { return }
                webView.callAsyncJavaScript("window.iOSAppSwitchPiece?.(piece);",
                    arguments: ["piece": pieceData], in: nil, in: .page)
            }
        }

    }
}
