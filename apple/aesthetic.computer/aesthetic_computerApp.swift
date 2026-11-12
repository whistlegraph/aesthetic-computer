import SwiftUI
import UserNotifications
import WebKit
#if os(macOS)
import AppKit
import UniformTypeIdentifiers
#endif

@main
struct aesthetic_computerApp: App {
    #if os(macOS)
    @NSApplicationDelegateAdaptor(AppDelegate.self) var delegate
    #else
    @UIApplicationDelegateAdaptor(AppDelegate.self) var delegate
    #endif

    var body: some Scene {
        #if os(macOS)
        WindowGroup("Aesthetic Computer", id: "prompt") {
            ContentView()
                .frame(width: MacPromptLatencyProbe.enabled ? 640 : nil,
                       height: MacPromptLatencyProbe.enabled ? 480 : nil)
                .frame(minWidth: 0, minHeight: 0)
        }
        .windowResizability(MacPromptLatencyProbe.enabled ? .contentSize : .contentMinSize)
        .defaultSize(width: 900, height: 700)
        .commands { ComputerWindowCommands() }
        #else
        WindowGroup { ContentView() }
        #endif
    }

}

#if os(macOS)
private struct ComputerWindowCommands: Commands {
    @Environment(\.openWindow) private var openWindow
    @FocusedObject private var navigation: AppNavigation?

    var body: some Commands {
        CommandGroup(replacing: .newItem) {
            Button("New Window") { openWindow(id: "prompt") }
                .keyboardShortcut("n")
            Button("Prompt") { navigation?.prompt() }
                .keyboardShortcut("l").disabled(navigation == nil)
            Button("Reload") { navigation?.reload() }
                .keyboardShortcut("r").disabled(navigation == nil)
            Button("Hard Reload") { navigation?.hardReload() }
                .keyboardShortcut("r", modifiers: [.command, .shift]).disabled(navigation == nil)
            Button("Open Audio…") {
                let panel = NSOpenPanel()
                panel.allowedContentTypes = [.audio]
                panel.begin { response in
                    if response == .OK, let url = panel.url { PopPlayer.shared.openFile(url) }
                }
            }.keyboardShortcut("o")
        }
    }
}
#endif

// Push permission is requested by `notifs` or a one-time offer after sign-in. Native opt-out also stops APNs
// immediately; the web bridge retries server cleanup if the device is offline.
@MainActor
class AppDelegate: NSObject, ACApplicationDelegate {
    static var shared: AppDelegate?
    weak var appWebView: WKWebView?
    #if os(macOS)
    var openPromptWindow: (() -> Void)?
    #endif
    private let webViews = NSHashTable<WKWebView>.weakObjects()
    private let optedInKey = "acNotificationsOptedIn"
    private let tokenKey = "acNotificationToken"
    private let offeredKey = "acNotificationsOffered"
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

    #if os(macOS)
    func applicationDidFinishLaunching(_ notification: Notification) {
        startNotifications()
        NSApp.activate(ignoringOtherApps: true)
    }

    func applicationShouldHandleReopen(_ sender: NSApplication, hasVisibleWindows flag: Bool) -> Bool {
        if let window = sender.keyWindow ?? sender.mainWindow ?? sender.windows.first(where: { $0.canBecomeMain }) {
            window.deminiaturize(nil)
            window.makeKeyAndOrderFront(nil)
        }
        else { openPromptWindow?() }
        return false
    }

    func application(_ application: NSApplication, open urls: [URL]) {
        for url in urls {
            if url.isFileURL { PopPlayer.shared.openFile(url) }
            else { openDestination(url) }
        }
        if let window = application.keyWindow ?? application.mainWindow {
            window.deminiaturize(nil)
            window.makeKeyAndOrderFront(nil)
        }
        application.activate(ignoringOtherApps: true)
    }
    #else
    func application(
        _ application: UIApplication,
        didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?
    ) -> Bool {
        startNotifications()
        ACDevices.report(.open)
        return true
    }
    #endif

    private func openDestination(_ url: URL) {
        #if os(macOS)
        if let navigation = AppNavigation.active { navigation.open(url) }
        else if AppRoute.destination(for: url) != nil {
            AppNavigation.pendingURL = url
            openPromptWindow?()
        }
        #else
        AppNavigation.shared.open(url)
        #endif
    }

    private func startNotifications() {
        UNUserNotificationCenter.current().delegate = self
        if optedIn {
            // Refresh an existing opt-in silently. Never request permission here.
            let current = revision
            Task {
                let settings = await UNUserNotificationCenter.current().notificationSettings()
                guard revision == current, optedIn else { return }
                if settings.authorizationStatus == .authorized || settings.authorizationStatus == .provisional {
                    ACApplication.shared.registerForRemoteNotifications()
                }
            }
        } else {
            // Includes upgrades from the former automatic launch-time subscription.
            ACApplication.shared.unregisterForRemoteNotifications()
        }
    }

    func application(_ application: ACApplication, didFailToRegisterForRemoteNotificationsWithError error: Error) {
        finishSubscribe(ok: false)
    }

    func application(_ application: ACApplication, didRegisterForRemoteNotificationsWithDeviceToken deviceToken: Data) {
        apnsToken = deviceToken.map { String(format: "%02x", $0) }.joined()
        if optedIn, let apnsToken { ACDevices.report(.push, push: ACDevices.apns(apnsToken)) }
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
                ACApplication.shared.unregisterForRemoteNotifications()
                for view in webViews.allObjects { synchronizePush(in: view) }
                notificationAlert("Notifications are off", "Allow notifications in system settings, then enter ‘notifs’ again.", settings: true)
                return
            }
            optedIn = true
            ACApplication.shared.registerForRemoteNotifications()
            // A missing APNs callback must not leave the command stuck forever.
            // Long enough for the web bridge's retried registration.
            try? await Task.sleep(nanoseconds: 35_000_000_000)
            if pendingSubscribe == current { finishSubscribe(ok: false) }
        }
    }

    // Asked once per install, after sign-in, and only while iOS hasn't asked
    // yet. "Not now" leaves `notifs` as the way in; nothing re-prompts.
    func offerNotifications(in webView: WKWebView) {
        #if os(iOS)
        guard !optedIn, pendingSubscribe == nil, !UserDefaults.standard.bool(forKey: offeredKey) else { return }
        Task {
            let settings = await UNUserNotificationCenter.current().notificationSettings()
            guard settings.authorizationStatus == .notDetermined else {
                UserDefaults.standard.set(true, forKey: offeredKey)
                return
            }
            // Let the signed-in page settle before interrupting it.
            try? await Task.sleep(nanoseconds: 3_000_000_000)
            guard !optedIn, pendingSubscribe == nil, !UserDefaults.standard.bool(forKey: offeredKey) else { return }
            UserDefaults.standard.set(true, forKey: offeredKey)
            let alert = UIAlertController(title: "Turn on notifications?",
                message: "Get a note when someone sends you mail, a mood or a scream. You can turn them off anytime with ‘nonotifs’.",
                preferredStyle: .alert)
            alert.addAction(UIAlertAction(title: "Not now", style: .cancel))
            alert.addAction(UIAlertAction(title: "Turn on", style: .default) { _ in
                self.triggerSubscribe(in: webView)
            })
            var controller = webView.window?.rootViewController
            while let presented = controller?.presentedViewController { controller = presented }
            controller?.present(alert, animated: true)
        }
        #endif
    }

    func triggerUnsubscribe(in webView: WKWebView) {
        webViews.add(webView)
        appWebView = webView
        revision += 1
        let current = revision
        pendingSubscribe = nil
        optedIn = false
        ACApplication.shared.unregisterForRemoteNotifications()
        ACDevices.report(.push, push: NSNull())
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
            ? "You’ll receive screams, moods, and mail when signed in. Enter ‘nonotifs’ to turn them off on this device."
            : "Check your connection and enter ‘notifs’ again.")
    }

    private func notificationAlert(_ title: String, _ message: String, settings: Bool = false) {
        #if os(macOS)
        let alert = NSAlert()
        alert.messageText = title
        alert.informativeText = message
        alert.addButton(withTitle: "OK")
        if let window = appWebView?.window { alert.beginSheetModal(for: window) }
        else { alert.runModal() }
        #else
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
        #endif
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
        return optedIn ? [.banner, .sound] : []
    }

    // Notification tapped while the app wasn't open — jump to its piece.
    func userNotificationCenter(
        _ center: UNUserNotificationCenter,
        didReceive response: UNNotificationResponse
    ) async {
        // AppNavigation outlives the web view. Cold launches and offline starts
        // retain the destination until ContentView can load it.
        if let url = AppRoute.notificationDestination(response.notification.request.content.userInfo) {
            openDestination(url)
        }
    }
}

/// This install's row in the AC network device registry (POST /api/app-device),
/// the same table choir, Whistlegraph and Aesel report to. Anonymous: the
/// account lives in the web view, so rows count devices, not people.
@MainActor enum ACDevices {
    enum Event: String { case open, push }

    static func apns(_ token: String) -> [String: Any] {
        #if DEBUG
        let env = "sandbox"
        #else
        let env = "production"
        #endif
        return ["kind": "apns", "token": token, "env": env]
    }

    static func report(_ event: Event, push: Any? = nil) {
        #if os(iOS)
        guard let deviceId = UIDevice.current.identifierForVendor?.uuidString else { return }
        let info = Bundle.main.infoDictionary ?? [:]
        let pad = UIDevice.current.userInterfaceIdiom == .pad
        var body: [String: Any] = ["app": "aestheticcomputer", "deviceId": deviceId, "event": event.rawValue,
                                   "platform": pad ? "ipados" : "ios", "label": pad ? "iPad" : "iPhone",
                                   "os": UIDevice.current.systemVersion, "model": machine()]
        if let version = info["CFBundleShortVersionString"] as? String { body["version"] = version }
        if let build = info["CFBundleVersion"] as? String, build.allSatisfy(\.isNumber) { body["build"] = build }
        if let push { body["push"] = push }
        var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/app-device")!, timeoutInterval: 10)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try? JSONSerialization.data(withJSONObject: body)
        Task { _ = try? await URLSession.shared.data(for: request) } // best effort
        #endif
    }

    private static func machine() -> String {
        var info = utsname()
        uname(&info)
        return withUnsafeBytes(of: &info.machine) { String(decoding: $0.prefix { $0 != 0 }, as: UTF8.self) }
    }
}

#if os(macOS)
// pointer lock shim: WKWebView exposes requestPointerLock but never grants it
// (the request rejects with pointerlockerror), so the page's API is replaced
// in every aesthetic.computer frame, the dool iframe included. The frame that
// asks gets the cursor hidden and pinned, and raw mouse deltas arrive as
// mousemove events with movementX/Y until Escape, exitPointerLock, a resign
// of the app or key window, or a navigation.
@MainActor enum PointerLock {
    private(set) static var active = false
    private static weak var webView: WKWebView?
    private static var frame: WKFrameInfo?
    private static var monitors: [Any] = []
    private static var observers: [NSObjectProtocol] = []
    private static var dx = 0.0, dy = 0.0, flushing = false

    static let script = """
        (() => {
          const bridge = window.webkit?.messageHandlers?.acPointerLock;
          if (!bridge) return;
          let locked = null;
          Object.defineProperty(Document.prototype, 'pointerLockElement', { configurable: true, get: () => locked });
          const change = () => document.dispatchEvent(new Event('pointerlockchange'));
          Element.prototype.requestPointerLock = function () {
            locked = this;
            bridge.postMessage({ type: 'pointerlock', on: true });
            change();
            return Promise.resolve();
          };
          Document.prototype.exitPointerLock = function () {
            if (!locked) return;
            locked = null;
            bridge.postMessage({ type: 'pointerlock', on: false });
            change();
          };
          window.__acPointerMove = (dx, dy) => {
            if (!locked) return;
            const e = new MouseEvent('mousemove', { bubbles: true, movementX: dx, movementY: dy });
            Object.defineProperty(e, 'movementX', { value: dx });
            Object.defineProperty(e, 'movementY', { value: dy });
            locked.dispatchEvent(e);
          };
        })();
        """

    static func handle(_ message: WKScriptMessage) {
        guard message.frameInfo.securityOrigin.protocol == "https",
              message.frameInfo.securityOrigin.host == "aesthetic.computer",
              let body = message.body as? [String: Any], let on = body["on"] as? Bool else { return }
        if on, let view = message.webView { lock(view, frame: message.frameInfo) }
        else if !on { release(notifyPage: false) }
    }

    private static func lock(_ view: WKWebView, frame info: WKFrameInfo) {
        release(notifyPage: true) // another frame took over; tell the old one
        webView = view
        frame = info
        active = true
        NSCursor.hide()
        _ = CGAssociateMouseAndMouseCursorPosition(0)
        let mouse = NSEvent.addLocalMonitorForEvents(matching: [.mouseMoved, .leftMouseDragged, .rightMouseDragged,
                                                                .otherMouseDragged]) { event in
            MainActor.assumeIsolated { move(event.deltaX, event.deltaY) }
            return event // clicks and drags still reach the page
        }
        let keys = NSEvent.addLocalMonitorForEvents(matching: .keyDown) { event in
            if event.keyCode == 53 { MainActor.assumeIsolated { release(notifyPage: true) } } // Escape
            return event
        }
        monitors = [mouse, keys].compactMap { $0 }
        let center = NotificationCenter.default
        observers = [
            center.addObserver(forName: NSApplication.didResignActiveNotification, object: nil, queue: .main) { _ in
                MainActor.assumeIsolated { release(notifyPage: true) }
            },
            center.addObserver(forName: NSWindow.didResignKeyNotification, object: view.window, queue: .main) { _ in
                MainActor.assumeIsolated { release(notifyPage: true) }
            },
        ]
    }

    /// `notifyPage` runs `document.exitPointerLock()` in the locked frame so
    /// its `pointerlockchange` fires; the page's own exit call arrives here
    /// as `on: false` and finds nothing left to do.
    static func release(notifyPage: Bool) {
        guard active else { return }
        active = false
        monitors.forEach(NSEvent.removeMonitor)
        monitors = []
        observers.forEach(NotificationCenter.default.removeObserver)
        observers = []
        dx = 0
        dy = 0
        _ = CGAssociateMouseAndMouseCursorPosition(1)
        NSCursor.unhide()
        if notifyPage, let webView, let frame {
            webView.evaluateJavaScript("document.exitPointerLock()", in: frame, in: .page) { _ in }
        }
        webView = nil
        frame = nil
    }

    // Deltas from one run-loop pass are summed into a single IPC to the page.
    private static func move(_ x: CGFloat, _ y: CGFloat) {
        dx += x
        dy += y
        guard !flushing else { return }
        flushing = true
        DispatchQueue.main.async { flush() }
    }

    private static func flush() {
        flushing = false
        guard active, let webView, let frame, dx != 0 || dy != 0 else { return }
        let js = "window.__acPointerMove(\(dx),\(dy))"
        dx = 0
        dy = 0
        webView.evaluateJavaScript(js, in: frame, in: .page) { result in
            // The frame navigated away or reloaded while locked: the cursor must not stay pinned.
            if case .failure = result { Task { @MainActor in release(notifyPage: false) } }
        }
    }
}
#endif
