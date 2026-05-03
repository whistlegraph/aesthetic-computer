import SwiftUI
import WebKit
import Network
import Combine

let grey: CGFloat = 32/255

// MARK: - Long-lived network monitor
//
// The previous implementation cancelled NWPathMonitor on the first .satisfied
// path, which meant a mid-session connectivity drop was invisible to the app.
// This monitor stays alive for the whole process so the UI can react to
// every transition (online → offline → online).

final class AppNetworkMonitor: ObservableObject {
    @Published var isOnline: Bool? = nil
    private let monitor = NWPathMonitor()
    private let queue = DispatchQueue(label: "AppNetworkMonitor")

    init() {
        monitor.pathUpdateHandler = { [weak self] path in
            let online = path.status == .satisfied
            DispatchQueue.main.async {
                self?.isOnline = online
            }
        }
        monitor.start(queue: queue)
    }

    deinit { monitor.cancel() }
}

// MARK: - Boot status / watchdog
//
// Tracks whether the WebView's JS runtime is making forward progress. The
// runtime sends a heartbeat (window.iOSReportBootHeartbeat) at boot and
// periodically afterwards via webkit.messageHandlers.iOSApp. If we don't
// hear from it for STALL_TIMEOUT seconds while a navigation is in flight,
// we surface a "Reload" overlay so the user can recover without force-quitting.
//
// Bumping `reloadTrigger` causes WebView.updateUIView to issue a fresh load
// (with a per-launch _iosbust query param). `forceLiveTrigger` asks the
// view to attempt the live URL even if the network monitor is offline —
// useful for the "Tap to retry" button on offline.html when the monitor
// hasn't flipped yet.

final class BootStatus: ObservableObject {
    @Published var stalled: Bool = false
    @Published var lastError: String? = nil
    @Published var reloadTrigger: Int = 0
    @Published var forceLiveTrigger: Int = 0

    private var lastHeartbeatAt = Date()
    private var watchdog: Timer? = nil
    private var watchdogActive = false
    private(set) var isReady = false

    private let stallTimeout: TimeInterval = 25
    private let pollInterval: TimeInterval = 5

    func startWatchdog() {
        stopWatchdog()
        lastHeartbeatAt = Date()
        isReady = false
        watchdogActive = true
        DispatchQueue.main.async {
            self.stalled = false
            self.lastError = nil
        }
        let timer = Timer.scheduledTimer(withTimeInterval: pollInterval, repeats: true) { [weak self] _ in
            guard let self = self, self.watchdogActive else { return }
            let elapsed = Date().timeIntervalSince(self.lastHeartbeatAt)
            if elapsed > self.stallTimeout {
                DispatchQueue.main.async {
                    if !self.stalled { self.stalled = true }
                }
            }
        }
        RunLoop.main.add(timer, forMode: .common)
        watchdog = timer
    }

    func stopWatchdog() {
        watchdogActive = false
        watchdog?.invalidate()
        watchdog = nil
    }

    func heartbeat(ready: Bool = false) {
        lastHeartbeatAt = Date()
        if ready {
            isReady = true
            stopWatchdog()
        }
        DispatchQueue.main.async {
            if self.stalled { self.stalled = false }
        }
    }

    func setError(_ msg: String) {
        DispatchQueue.main.async {
            self.lastError = msg
        }
    }

    func requestReload() {
        stopWatchdog()
        DispatchQueue.main.async {
            self.stalled = false
            self.lastError = nil
            self.reloadTrigger += 1
        }
    }

    func requestForceLive() {
        stopWatchdog()
        DispatchQueue.main.async {
            self.stalled = false
            self.lastError = nil
            self.forceLiveTrigger += 1
        }
    }
}

// MARK: - Coordinator
//
// Owns: WKScriptMessageHandler (JS bridge), WKNavigationDelegate (load
// success/failure), WKUIDelegate (media permissions). Holds a weak ref to
// BootStatus so heartbeats / errors flow upward.

class Coordinator: NSObject, WKScriptMessageHandler, WKUIDelegate, WKNavigationDelegate {
    weak var bootStatus: BootStatus?
    private(set) var lastLoadedKey: String? = nil

    init(bootStatus: BootStatus) {
        self.bootStatus = bootStatus
        super.init()
    }

    func setLoadedKey(_ key: String) { lastLoadedKey = key }

    // MARK: media permissions
    func webView(_ webView: WKWebView,
                 decideMediaCapturePermissionsFor origin: WKSecurityOrigin,
                 initiatedBy frame: WKFrameInfo,
                 type: WKMediaCaptureType) async -> WKPermissionDecision {
        return .grant
    }

    // MARK: navigation
    func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!) {
        // Only watchdog real network loads; offline.html doesn't have JS that
        // can heartbeat back, so we'd false-alarm on the static offline page.
        if let url = webView.url, url.scheme == "https" || url.scheme == "http" {
            bootStatus?.startWatchdog()
        }
    }

    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
        bootStatus?.setError("Load failed — \(error.localizedDescription)")
    }

    func webView(_ webView: WKWebView,
                 didFailProvisionalNavigation navigation: WKNavigation!,
                 withError error: Error) {
        bootStatus?.setError("Cannot reach aesthetic.computer\n\(error.localizedDescription)")
    }

    // MARK: pull-to-refresh
    @objc func handleRefresh(_ sender: UIRefreshControl) {
        bootStatus?.requestReload()
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
            sender.endRefreshing()
        }
    }

    // MARK: JS → native messages
    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        if message.name == "iOSAppLog" {
            print("JavaScript Log: \(message.body)")
            return
        }

        guard message.name == "iOSApp", let jsonString = message.body as? String else { return }
        guard let data = jsonString.data(using: .utf8) else { return }
        guard let dictionary = try? JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] else {
            print("📱 iOSApp message: failed to parse \(jsonString)")
            return
        }

        guard let type = dictionary["type"] as? String else { return }

        switch type {
        case "boot:heartbeat":
            bootStatus?.heartbeat()
        case "boot:ready":
            bootStatus?.heartbeat(ready: true)
        case "reload":
            bootStatus?.requestReload()
        case "reload-online":
            // Force-load the live URL even if the network monitor still says
            // offline (used by the offline.html "Tap to retry" button — the
            // monitor can lag behind a real reconnect by a few seconds).
            bootStatus?.requestForceLive()
        case "notifications":
            if let body = dictionary["body"] as? Bool, body == true {
                AppDelegate.shared?.triggerSubscribe()
                showAlert(title: "SUBSCRIBED", message: "You have successfully subscribed to notifications. Type \"nonotifs\" to unsubscribe.")
            } else if let body = dictionary["body"] as? Bool, body == false {
                AppDelegate.shared?.triggerUnsubscribe()
                showAlert(title: "UNSUBSCRIBED", message: "You have successfully unsubscribed from notifications. Type \"notifs\" to subscribe.")
            }
        case "url":
            if let urlString = dictionary["body"] as? String, let url = URL(string: urlString) {
                UIApplication.shared.open(url, options: [:], completionHandler: nil)
            }
        default:
            print("Unhandled type: \(type)")
        }
    }

    func showAlert(title: String, message: String) {
        let alert = UIAlertController(title: title, message: message, preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default, handler: nil))
        if let windowScene = UIApplication.shared.connectedScenes.first as? UIWindowScene,
           let rootViewController = windowScene.windows.first?.rootViewController {
            var currentController = rootViewController
            while let presentedController = currentController.presentedViewController {
                currentController = presentedController
            }
            currentController.present(alert, animated: true, completion: nil)
        }
    }
}

// MARK: - WebView

struct WebView: UIViewRepresentable {
    var url: String
    var reloadTrigger: Int
    let bootStatus: BootStatus

    func makeCoordinator() -> Coordinator {
        Coordinator(bootStatus: bootStatus)
    }

    func makeUIView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        let userScript = WKUserScript(
            source: "console.log = function() { window.webkit.messageHandlers.iOSAppLog.postMessage([...arguments].join(' ')); }",
            injectionTime: .atDocumentStart,
            forMainFrameOnly: false
        )
        config.allowsInlineMediaPlayback = true
        config.userContentController.addUserScript(userScript)
        config.userContentController.add(context.coordinator, name: "iOSAppLog")
        config.userContentController.add(context.coordinator, name: "iOSApp")

        // 🧹 Wipe every cache surface that has been observed to keep stale
        // /aesthetic.computer/*.mjs (boot, bios, disk, ...) alive between
        // launches. Cookies + localStorage are preserved so the user stays
        // logged in.
        let cacheTypes: Set<String> = [
            WKWebsiteDataTypeDiskCache,
            WKWebsiteDataTypeMemoryCache,
            WKWebsiteDataTypeFetchCache,
            WKWebsiteDataTypeOfflineWebApplicationCache,
            WKWebsiteDataTypeServiceWorkerRegistrations,
            WKWebsiteDataTypeIndexedDBDatabases,
            WKWebsiteDataTypeWebSQLDatabases,
        ]
        WKWebsiteDataStore.default().removeData(
            ofTypes: cacheTypes,
            modifiedSince: .distantPast
        ) {}
        URLCache.shared.removeAllCachedResponses()

        let webView = WKWebView(frame: .zero, configuration: config)
        webView.backgroundColor = UIColor(red: grey, green: grey, blue: grey, alpha: 1)
        webView.isOpaque = false
        webView.uiDelegate = context.coordinator
        webView.navigationDelegate = context.coordinator
        webView.customUserAgent = "Aesthetic"

        // 👆 Forward touches immediately (default UIScrollView behaviour
        // delays the first touch by ~150ms which swallows the tap that
        // opens the AC prompt on first launch).
        webView.scrollView.delaysContentTouches = false
        webView.scrollView.canCancelContentTouches = false

        // Pull-to-refresh: tug the page down to force-reload. Lifesaver when
        // a flaky cell connection has stranded the runtime mid-fetch.
        let refresh = UIRefreshControl()
        refresh.tintColor = .lightGray
        refresh.addTarget(context.coordinator, action: #selector(Coordinator.handleRefresh(_:)), for: .valueChanged)
        webView.scrollView.refreshControl = refresh
        webView.scrollView.bounces = true
        webView.scrollView.alwaysBounceVertical = true

        AppDelegate.shared?.appWebView = webView
        return webView
    }

    func updateUIView(_ webView: WKWebView, context: Context) {
        // Gate reloads to actual changes so flipping unrelated SwiftUI state
        // (overlay visibility, etc.) doesn't kick a fresh load every time.
        let key = "\(url)|\(reloadTrigger)"
        if context.coordinator.lastLoadedKey == key { return }
        context.coordinator.setLoadedKey(key)

        // 🚫 Belt + braces against stale modules: per-launch ?_iosbust=<ts>
        // defeats CDN/SW cache keys that ignore cache-control. AC's runtime
        // ignores unknown query params on the root.
        guard var components = URLComponents(string: url) else { return }
        if components.scheme == "http" || components.scheme == "https" {
            var items = components.queryItems ?? []
            items.append(URLQueryItem(
                name: "_iosbust",
                value: String(Int(Date().timeIntervalSince1970))
            ))
            components.queryItems = items
        }
        guard let bustedURL = components.url else { return }
        let request = URLRequest(
            url: bustedURL,
            cachePolicy: .reloadIgnoringLocalAndRemoteCacheData,
            timeoutInterval: 30
        )
        webView.load(request)
    }
}

// MARK: - Recovery overlay

struct StalledOverlay: View {
    let message: String
    let onReload: () -> Void

    var body: some View {
        VStack(spacing: 12) {
            Spacer()
            VStack(spacing: 10) {
                Text(message)
                    .font(.system(size: 14, weight: .medium))
                    .foregroundColor(.white)
                    .multilineTextAlignment(.center)
                    .padding(.horizontal, 12)
                Button(action: onReload) {
                    Text("Reload")
                        .font(.system(size: 15, weight: .semibold))
                        .foregroundColor(.black)
                        .padding(.vertical, 8)
                        .padding(.horizontal, 22)
                        .background(Color.yellow)
                        .cornerRadius(6)
                }
            }
            .padding(.vertical, 14)
            .padding(.horizontal, 16)
            .background(Color.black.opacity(0.85))
            .cornerRadius(10)
            .padding(.horizontal, 24)
            .padding(.bottom, 28)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .bottom)
        .allowsHitTesting(true)
    }
}

// MARK: - ContentView

struct ContentView: View {
    @StateObject private var monitor = AppNetworkMonitor()
    @StateObject private var bootStatus = BootStatus()
    @Environment(\.scenePhase) private var scenePhase
    @State private var lastBackgroundedAt: Date? = nil
    @State private var lastForceLiveTrigger: Int = 0

    private let liveURL = "https://aesthetic.computer"
    private var offlineURL: String {
        Bundle.main.url(forResource: "offline", withExtension: "html", subdirectory: "html")?.absoluteString ?? ""
    }

    /// The URL we want the WebView to display right now.
    private var resolvedURL: String {
        // forceLive overrides the monitor briefly — used by the offline page
        // "Tap to retry" button when the user knows they're back online but
        // the system path monitor hasn't caught up.
        if bootStatus.forceLiveTrigger > 0 { return liveURL }
        guard let isOnline = monitor.isOnline else { return liveURL } // optimistic before first monitor update
        return isOnline ? liveURL : offlineURL
    }

    var body: some View {
        GeometryReader { geometry in
            ZStack {
                Color(red: grey, green: grey, blue: grey).ignoresSafeArea()

                if monitor.isOnline != nil || bootStatus.forceLiveTrigger > 0 {
                    WebView(
                        url: resolvedURL,
                        reloadTrigger: bootStatus.reloadTrigger,
                        bootStatus: bootStatus
                    )
                }

                if bootStatus.stalled || bootStatus.lastError != nil {
                    StalledOverlay(
                        message: bootStatus.lastError ?? "This is taking longer than expected."
                    ) {
                        bootStatus.requestReload()
                    }
                }
            }
            .padding(.bottom, geometry.safeAreaInsets.bottom > 0 ? 24 : 0)
            .background(Color(red: grey, green: grey, blue: grey))
            .ignoresSafeArea(.keyboard, edges: .bottom)
            .onChange(of: scenePhase) { newPhase in
                handleScenePhase(newPhase)
            }
            .onChange(of: monitor.isOnline) { newValue in
                // Re-load when connectivity returns mid-session so the user
                // doesn't have to tap reload manually after a flaky cell tower
                // hands them back a connection.
                if newValue == true {
                    bootStatus.requestReload()
                }
            }
            .onChange(of: bootStatus.forceLiveTrigger) { newValue in
                // forceLive bumped → cause WebView.updateUIView to refire by
                // also bumping reloadTrigger (URL alone wouldn't change if we
                // were already on liveURL).
                if newValue != lastForceLiveTrigger {
                    lastForceLiveTrigger = newValue
                    bootStatus.requestReload()
                }
            }
        }
    }

    private func handleScenePhase(_ phase: ScenePhase) {
        switch phase {
        case .background:
            lastBackgroundedAt = Date()
        case .active:
            // After a long sleep the WebView often holds a stale runtime
            // (sockets timed out, modules half-loaded). A fresh load is
            // cheaper than debugging which subsystem gave up.
            if let bg = lastBackgroundedAt {
                let elapsed = Date().timeIntervalSince(bg)
                if elapsed > 300 {  // 5 minutes
                    bootStatus.requestReload()
                }
            }
            lastBackgroundedAt = nil
        default:
            break
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
