import SwiftUI
import UserNotifications
import FirebaseCore
import FirebaseMessaging
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

class AppDelegate: NSObject, UIApplicationDelegate {
    let gcmMessageIDKey = "gcm.message_id"
    
    static var shared: AppDelegate?
    var appWebView: WKWebView?
    
    override init() {
        super.init()
        AppDelegate.shared = self
    }
    
    func application(
        _ application: UIApplication,
        didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?
    ) -> Bool {
        FirebaseApp.configure()
        Messaging.messaging().delegate = self
        
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
        didReceiveRemoteNotification userInfo: [AnyHashable: Any]
    ) async
    -> UIBackgroundFetchResult
    {
        // If you are receiving a notification message while your app is in the background,
        // this callback will not be fired till the user taps on the notification launching the application.
        
        // TODO: Handle data of notification
        Messaging.messaging().appDidReceiveMessage(userInfo)  // With swizzling disabled you must inform messaging, for Analytics
        // Print message ID.
        
        if let messageID = userInfo[gcmMessageIDKey] {
            print("Message ID: \(messageID)")
        }
        
        print(userInfo)  // Print full message.
        
        return UIBackgroundFetchResult.newData
    }
    
    func application(
        _ application: UIApplication,
        didFailToRegisterForRemoteNotificationsWithError error: Error
    ) {
        print("Unable to register for remote notifications: \(error.localizedDescription)")
    }
    
    // This function is added here only for debugging purposes, and can be removed if swizzling is enabled.
    // If swizzling is disabled then this function must be implemented so that the APNs token can be paired to
    // the FCM registration token.
    func application(
        _ application: UIApplication,
        didRegisterForRemoteNotificationsWithDeviceToken deviceToken: Data
    ) {
        print("APNs token retrieved: \(deviceToken)")
        Messaging.messaging().apnsToken = deviceToken  // With swizzling disabled you must set the APNs token here.
        //TOPICS for Notifications, decided when the app first boots on a phone
        subscribeToTopics()
    }
    func subscribeToTopics() {
        Messaging.messaging().subscribe(toTopic: "scream") { error in
            print("Subscribed to scream topic")
        }
        Messaging.messaging().subscribe(toTopic: "mood") { error in
            print("Subscribed to mood topic")
        }
    }
    func unsubscribe(_ application: UIApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: Data) {
        // Unsubscribing from the 'scream' topic
        Messaging.messaging().unsubscribe(fromTopic: "scream") { error in
            if let error = error {
                print("Error unsubscribing from scream topic: \(error)")
            } else {
                print("Unsubscribed from scream topic")
            }
        }
        // Unsubscribing from the 'mood' topic
        Messaging.messaging().unsubscribe(fromTopic: "mood") { error in
            if let error = error {
                print("Error unsubscribing from mood topic: \(error)")
            } else {
                print("Unsubscribed from mood topic")
            }
        }
    }
    func triggerSubscribe() {
        subscribeToTopics()
    }
    func triggerUnsubscribe() {
        unsubscribe(UIApplication.shared, didRegisterForRemoteNotificationsWithDeviceToken: Data())
    }

}

extension AppDelegate: UNUserNotificationCenterDelegate {
    
    // Receive displayed notifications for iOS 10 (or later) devices.
    // App notification while open
    func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        willPresent notification: UNNotification
    ) async
    -> UNNotificationPresentationOptions
    {
        let userInfo = notification.request.content.userInfo
        
        Messaging.messaging().appDidReceiveMessage(userInfo)  // With swizzling disabled you must let Messaging know about the message, for Analytics
        
        if let messageID = userInfo[gcmMessageIDKey] {
            print("Message ID: \(messageID)")  // Print message ID.
        }
        
        print(userInfo)  // Print full message.
        
        // Change this to your preferred presentation option.
        return [[.sound]]  // return [[.banner, .sound]]
    }
    //App notification when app isn't open, you get banner and tap
    func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        didReceive response: UNNotificationResponse
    ) async {
        let userInfo = response.notification.request.content.userInfo
        
        // Print message ID.
        if let messageID = userInfo[gcmMessageIDKey] {
            print("Message ID: \(messageID)")
        }
        // Print Data String
        if let pieceData = userInfo["piece"] as? String {
            if pieceData != ""{
                let script = "iOSAppSwitchPiece('\(pieceData)');"
                appWebView?.evaluateJavaScript(script, completionHandler: nil)
            }
        }
        
        Messaging.messaging().appDidReceiveMessage(userInfo)  // With swizzling disabled you must let Messaging know about the message, for Analytics
        // Print full message.
        print(userInfo)
    }
}

extension AppDelegate: MessagingDelegate {
    func messaging(_ messaging: Messaging, didReceiveRegistrationToken fcmToken: String?) {
        print("Firebase registration token: \(String(describing: fcmToken))")
        let dataDict: [String: String] = ["token": fcmToken ?? ""]
        NotificationCenter.default.post(
            name: Notification.Name("FCMToken"),
            object: nil,
            userInfo: dataDict
        )
        // Hand the token to the WebView so the AC runtime can POST it to
        // /api/register-push-token against the logged-in user.
        // This callback is fired at each app startup and whenever a new token is generated.
        guard let token = fcmToken, !token.isEmpty else { return }
        deliverPushTokenToWebView(token)
    }

    private func deliverPushTokenToWebView(_ token: String, attempt: Int = 0) {
        // Retry a few times so we don't lose the token if the WebView hasn't
        // finished loading AC's bios.mjs yet (iOSReceivePushToken lives there).
        let escaped = token.replacingOccurrences(of: "\\", with: "\\\\")
                           .replacingOccurrences(of: "'", with: "\\'")
        let script = "window.iOSReceivePushToken && window.iOSReceivePushToken('\(escaped)', 'ios');"
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
                    print("📱 🔔 Handed FCM token to WebView (attempt \(attempt)).")
                }
            }
        }
    }
}
