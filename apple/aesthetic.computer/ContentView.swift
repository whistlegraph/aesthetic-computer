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

@MainActor class Coordinator: NSObject, WKScriptMessageHandler, WKUIDelegate, WKNavigationDelegate {
    weak var bootStatus: BootStatus?
    private(set) var lastLoadedKey: String? = nil
    #if os(macOS)
    private var urlObservation: NSKeyValueObservation?

    func observePiece(in webView: WKWebView) {
        urlObservation = webView.observe(\.url, options: [.initial, .new]) { [weak webView] _, _ in
            Task { @MainActor in
                guard let webView else { return }
                let title = AppRoute.windowTitle(for: webView.url)
                AppNavigation.shared.windowTitle = title
                webView.window?.title = title
            }
        }
    }
    #endif

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
        return origin.protocol == "https" && origin.host == "aesthetic.computer" ? .prompt : .deny
    }

    // MARK: navigation
    func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!) {
        LaunchPing.loaded(false)
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

    func webView(_ webView: WKWebView, didCommit navigation: WKNavigation!) {
        #if os(macOS)
        // Give the live document keyboard focus without waiting for every
        // subresource to finish (audio/streams can keep a piece loading).
        webView.window?.makeFirstResponder(webView)
        #endif
    }

    func webViewWebContentProcessDidTerminate(_ webView: WKWebView) {
        bootStatus?.setError("The page stopped. Reload to continue.")
    }

    #if os(iOS)
    // MARK: pull-to-refresh
    @objc func handleRefresh(_ sender: UIRefreshControl) {
        bootStatus?.requestReload()
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) {
            sender.endRefreshing()
        }
    }

    #endif

    // MARK: JS → native messages
    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
        if message.name == "iOSAppLog" {
            NSLog("AC WebKit: %@", String(describing: message.body))
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
        case "window:color":
            guard message.frameInfo.isMainFrame, message.frameInfo.securityOrigin.protocol == "https",
                  message.frameInfo.securityOrigin.host == "aesthetic.computer",
                  let webView = message.webView, let rgb = dictionary["body"] as? [Double] else { return }
            ApplePlatform.colorWindow(rgb, webView: webView)
        #if os(macOS)
        case "frame:meter":
            guard message.frameInfo.isMainFrame, message.frameInfo.securityOrigin.protocol == "https",
                  message.frameInfo.securityOrigin.host == "aesthetic.computer",
                  let body = dictionary["body"] as? [String: Any] else { return }
            FrameMeter.shared.update(body)
        #endif
        case "boot:heartbeat":
            bootStatus?.heartbeat()
        case "boot:ready":
            bootStatus?.heartbeat(ready: true)
            if message.frameInfo.isMainFrame, message.frameInfo.securityOrigin.protocol == "https",
               message.frameInfo.securityOrigin.host == "aesthetic.computer" {
                LaunchPing.loaded(true)
                #if os(macOS)
                // Autoplay is allowed in our native host. Warm the worklet
                // before the first curtain click or key so its feedback isn't
                // lost while the audio graph is still being constructed.
                message.webView?.evaluateJavaScript("window.AC?.startAudio?.()") { _, error in
                    if let error { NSLog("AC audio startup: %@", error.localizedDescription) }
                }
                MacPromptLatencyProbe.run(in: message.webView)
                FrameMeter.shared.sync(message.webView)
                #endif
            }
        case "usage:canvas":
            if message.frameInfo.isMainFrame, message.frameInfo.securityOrigin.protocol == "https",
               message.frameInfo.securityOrigin.host == "aesthetic.computer" {
                LaunchPing.interacted()
            }
        case "reload":
            bootStatus?.requestReload()
        case "reload-online":
            // Force-load the live URL even if the network monitor still says
            // offline (used by the offline.html "Tap to retry" button — the
            // monitor can lag behind a real reconnect by a few seconds).
            bootStatus?.requestForceLive()
        case "notifications:ready", "notifications":
            guard message.frameInfo.isMainFrame, message.frameInfo.securityOrigin.protocol == "https",
                  message.frameInfo.securityOrigin.host == "aesthetic.computer",
                  let webView = message.webView else { return }
            if type == "notifications:ready" {
                AppDelegate.shared?.pushBridgeReady(in: webView)
            } else if let enabled = dictionary["body"] as? Bool {
                if enabled { AppDelegate.shared?.triggerSubscribe(in: webView) }
                else { AppDelegate.shared?.triggerUnsubscribe(in: webView) }
            }
        case "pop:open":
            guard message.frameInfo.isMainFrame, message.frameInfo.securityOrigin.protocol == "https",
                  message.frameInfo.securityOrigin.host == "aesthetic.computer" else { return }
            let slug = (dictionary["body"] as? String).flatMap { $0.isEmpty ? nil : $0 }
            Task { @MainActor in PopPlayer.shared.open(slug: slug) }
        case "url":
            if let urlString = dictionary["body"] as? String, let url = URL(string: urlString) {
                ApplePlatform.openExternal(url)
            }
        default:
            print("Unhandled type: \(type)")
        }
    }

}

// MARK: - WebView

struct WebView: ACWebViewRepresentable {
    var url: String
    var reloadTrigger: Int
    let bootStatus: BootStatus

    func makeCoordinator() -> Coordinator {
        Coordinator(bootStatus: bootStatus)
    }

    func makeWebView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        #if os(macOS)
        MacPromptLatencyProbe.configure(config)
        #endif
        let userScript = WKUserScript(
            source: """
                for (const level of ['log', 'warn', 'error', 'info']) {
                    const original = console[level].bind(console);
                    console[level] = (...args) => {
                        original(...args);
                        window.webkit.messageHandlers.iOSAppLog.postMessage(level + ': ' + args.map(String).join(' '));
                    };
                }
                window.addEventListener('error', event => console.error(event.message));
                window.addEventListener('unhandledrejection', event => console.error(event.reason));
                """,
            injectionTime: .atDocumentStart,
            forMainFrameOnly: false
        )
        #if os(iOS)
        config.allowsInlineMediaPlayback = true
        #endif
        config.mediaTypesRequiringUserActionForPlayback = []
        config.userContentController.addUserScript(userScript)
        config.userContentController.add(context.coordinator, name: "iOSAppLog")
        config.userContentController.add(context.coordinator, name: "iOSApp")
        // One boolean for a trusted canvas touch. No keys, coordinates or URLs.
        config.userContentController.addUserScript(WKUserScript(source: """
            (() => {
              let sent = false;
              document.addEventListener('visibilitychange', () => { if (!document.hidden) sent = false; });
              document.addEventListener('pointerdown', e => {
                if (sent || !e.isTrusted || !(e.target instanceof HTMLCanvasElement)) return;
                sent = true;
                window.webkit.messageHandlers.iOSApp.postMessage(JSON.stringify({type:'usage:canvas'}));
              }, {capture:true, passive:true});
            })();
            """, injectionTime: .atDocumentStart, forMainFrameOnly: true))

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
        config.websiteDataStore.removeData(
            ofTypes: cacheTypes,
            modifiedSince: .distantPast
        ) {}
        URLCache.shared.removeAllCachedResponses()

        #if os(macOS)
        config.applicationNameForUserAgent = "AestheticMac"
        config.userContentController.addUserScript(WKUserScript(
            source: "window.acNativeWindowColor = true;",
            injectionTime: .atDocumentStart, forMainFrameOnly: true))
        // The native canvas always uses AC's SVG pointer, including after
        // focus/click handlers request `auto`, `pointer`, or `none` inline.
        config.userContentController.addUserScript(WKUserScript(source: """
            const acCursorStyle = document.createElement('style');
            acCursorStyle.textContent = `
              html, body, body * {
                cursor: url('/aesthetic.computer/cursors/precise.svg') 12 12, crosshair !important;
              }
            `;
            document.documentElement.appendChild(acCursorStyle);
            """, injectionTime: .atDocumentEnd, forMainFrameOnly: true))
        #endif
        #if os(macOS)
        let webView = ACNativeWebView(frame: .zero, configuration: config)
        #else
        let webView = WKWebView(frame: .zero, configuration: config)
        #endif
        #if os(iOS)
        webView.backgroundColor = UIColor(red: grey, green: grey, blue: grey, alpha: 1)
        webView.isOpaque = false
        #endif
        webView.uiDelegate = context.coordinator
        webView.navigationDelegate = context.coordinator
        #if os(macOS)
        // Keep WebKit's desktop identity; exact "Aesthetic" means iPhone in AC.
        webView.customUserAgent = nil
        webView.focusRingType = .none
        context.coordinator.observePiece(in: webView)
        webView.underPageBackgroundColor = NSColor(white: grey, alpha: 1)
        #else
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

        #endif
        AppDelegate.shared?.appWebView = webView
        return webView
    }

    func updateWebView(_ webView: WKWebView, context: Context) {
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
            items.removeAll { $0.name == "nogap" }
            items.append(URLQueryItem(name: "nogap", value: "true"))
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
    #if os(macOS)
    @ObservedObject private var windowTheme = AppleWindowTheme.shared
    #endif
    @ObservedObject private var navigation = AppNavigation.shared
    @StateObject private var monitor = AppNetworkMonitor()
    @StateObject private var bootStatus = BootStatus()
    @ObservedObject private var pop = PopPlayer.shared
    @Environment(\.scenePhase) private var scenePhase
    @State private var lastBackgroundedAt: Date? = nil
    @State private var lastForceLiveTrigger: Int = 0
    @State private var usageScene = UUID()

    private var liveURL: String { navigation.url }
    private var backdrop: Color {
        #if os(macOS)
        Color(nsColor: windowTheme.color)
        #else
        Color(red: grey, green: grey, blue: grey)
        #endif
    }
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
                backdrop.ignoresSafeArea()

                if monitor.isOnline != nil || bootStatus.forceLiveTrigger > 0 {
                    WebView(
                        url: resolvedURL,
                        reloadTrigger: bootStatus.reloadTrigger + navigation.revision,
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
            .background(backdrop)
            #if os(macOS)
            .navigationTitle(navigation.windowTitle)
            #endif
            .ignoresSafeArea(.keyboard, edges: .bottom)
            .sheet(isPresented: $pop.isPresented) {
                PopPlayerView()
                    #if os(macOS)
                    .frame(minWidth: 440, minHeight: 500)
                    #endif
            }
            #if os(iOS)
            .onOpenURL { url in
                if url.isFileURL { pop.openFile(url) }
                else { navigation.open(url) }
            }
            #endif
            .onAppear { handleScenePhase(scenePhase) }
            .onDisappear { LaunchPing.remove(scene: usageScene) }
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
            LaunchPing.inactive(scene: usageScene, background: true)
            lastBackgroundedAt = Date()
        case .active:
            LaunchPing.active(scene: usageScene)
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
        case .inactive:
            LaunchPing.inactive(scene: usageScene, background: false)
        @unknown default:
            break
        }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}
