import SwiftUI
import WebKit
import Network
import Combine
#if os(macOS)
import AppKit
#endif

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
// the page is brought back in a fresh instance. Why it is being brought back
// is kept in `trouble`, which the recovery line (below ContentView) reads out.
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

    // 🫀 The live watch: after boot, a page that keeps pulsing (AC's runtime
    // says `boot:heartbeat` every 8 s, and holds it while nothing in view is
    // drawing: bios `acStuck`) is brought back when the pulse stops for
    // `liveTimeout`. It starts at the first pulse after `boot:ready`, so a
    // page that never pulses after boot (rax standing alone) is not watched,
    // and it only judges while the app is in front and the window can be
    // seen, since a page behind other windows may be throttled to silence.
    // A notepat frozen under a hidden rax (2026-11-22) stayed frozen.
    private let liveTimeout: TimeInterval = 30
    private var liveWatch: Timer? = nil
    /// The app is in front (ContentView's scene phase). Back in front, the
    /// silence while away is forgiven.
    var appActive = true {
        didSet { if appActive && !oldValue { lastHeartbeatAt = Date() } }
    }

    private func startLiveWatch() {
        stopLiveWatch()
        lastHeartbeatAt = Date()
        let timer = Timer.scheduledTimer(withTimeInterval: pollInterval, repeats: true) { [weak self] _ in
            guard let self, self.isReady, self.appActive, Self.windowSeen() else {
                self?.lastHeartbeatAt = Date() // Silence while unseen does not count.
                return
            }
            if Date().timeIntervalSince(self.lastHeartbeatAt) > self.liveTimeout {
                self.stopLiveWatch()
                DispatchQueue.main.async { if !self.stalled { self.stalled = true } }
            }
        }
        RunLoop.main.add(timer, forMode: .common)
        liveWatch = timer
    }

    private func stopLiveWatch() {
        liveWatch?.invalidate()
        liveWatch = nil
    }

    /// The app's front window is on screen and not covered.
    private static func windowSeen() -> Bool {
        #if os(macOS)
        guard NSApp.isActive, let window = NSApp.keyWindow ?? NSApp.mainWindow else { return false }
        return window.occlusionState.contains(.visible)
        #else
        return true
        #endif
    }

    /// Why the page is being brought back, for the recovery line. Nil on a
    /// normal launch and on a reload somebody asked for.
    struct Trouble: Equatable {
        var reason: String
        /// A load that could not be made, not a page that went quiet.
        var failed = false
        /// When the trouble began: the line waits a moment before it shows.
        var since = Date()
        /// The next try, when the last one was too recent to try again now.
        var retryAt: Date? = nil

        /// Nothing for a recovery that is over in a moment.
        func shows(at now: Date) -> Bool {
            retryAt != nil || now.timeIntervalSince(since) > 1.5
        }

        func line(at now: Date) -> String {
            if reason == BootStatus.offline { return "offline: waiting for the network" }
            let wait = retryAt.map { max(1, Int($0.timeIntervalSince(now).rounded(.up))) }
            if failed {
                return "could not load: \(reason) · trying again" + (wait.map { " in \($0) s" } ?? "")
            }
            if let wait { return "\(reason) · reloading in \(wait) s" }
            return "reloading: \(reason)"
        }
    }
    @Published var trouble: Trouble? = nil
    static let offline = "offline"
    /// The last recovery, wherever it was started from: at most one every 20 s.
    private(set) var lastRecoveryAt = Date.distantPast
    private var retry: DispatchWorkItem?

    /// A recovery starts now: say why. The trouble's first moment is kept
    /// through every further try.
    func recovering(_ reason: String, failed: Bool = false) {
        retry?.cancel()
        retry = nil
        lastRecoveryAt = Date()
        trouble = Trouble(reason: reason, failed: failed, since: trouble?.since ?? Date())
    }

    /// Too soon to try again: say why and when, then try.
    func wait(_ reason: String, failed: Bool, seconds: TimeInterval, then go: @escaping () -> Void) {
        retry?.cancel()
        stopWatchdog() // The retry is the way back now, not a second stall.
        trouble = Trouble(reason: reason, failed: failed, since: trouble?.since ?? Date(),
                          retryAt: Date().addingTimeInterval(seconds))
        let item = DispatchWorkItem { [weak self] in
            guard let self, self.trouble != nil else { return }
            self.retry = nil
            go()
        }
        retry = item
        DispatchQueue.main.asyncAfter(deadline: .now() + seconds, execute: item)
    }

    /// The page is up, or an ordinary load took over: no line, no retry.
    func settle() {
        retry?.cancel()
        retry = nil
        if trouble != nil { trouble = nil }
    }

    /// A load that failed, in a few plain words.
    func fail(_ error: Error) {
        let e = error as NSError
        // A load a newer one took the place of, or one we turned away
        // ourselves, is no failure.
        if e.domain == NSURLErrorDomain, e.code == NSURLErrorCancelled { return }
        if e.domain == "WebKitErrorDomain", e.code == 102 { return }
        fail(Self.words(for: e))
    }

    func fail(_ reason: String) {
        stopWatchdog()
        setError(reason)
    }

    static func words(for e: NSError) -> String {
        if e.domain == NSURLErrorDomain {
            switch e.code {
            case NSURLErrorNotConnectedToInternet, NSURLErrorDataNotAllowed,
                 NSURLErrorInternationalRoamingOff:
                return offline
            case NSURLErrorTimedOut: return "the server took too long"
            case NSURLErrorCannotFindHost, NSURLErrorDNSLookupFailed: return "the server was not found"
            case NSURLErrorCannotConnectToHost: return "the server did not answer"
            case NSURLErrorNetworkConnectionLost: return "the connection was lost"
            case NSURLErrorSecureConnectionFailed, NSURLErrorServerCertificateUntrusted,
                 NSURLErrorServerCertificateHasBadDate, NSURLErrorServerCertificateNotYetValid,
                 NSURLErrorServerCertificateHasUnknownRoot:
                return "no secure connection"
            default: break
            }
        }
        var words = e.localizedDescription.trimmingCharacters(in: .whitespacesAndNewlines)
        while words.hasSuffix(".") { words.removeLast() }
        if words.count > 56 { words = String(words.prefix(55)) + "…" }
        return words.prefix(1).lowercased() + words.dropFirst()
    }

    func startWatchdog() {
        stopWatchdog()
        stopLiveWatch() // A new load: it is watched from its boot again.
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
        } else if isReady && liveWatch == nil {
            startLiveWatch() // A pulse after boot: this page keeps pulsing.
        }
        DispatchQueue.main.async {
            if self.stalled { self.stalled = false }
            if ready { self.settle() }
        }
    }

    func setError(_ msg: String) {
        DispatchQueue.main.async {
            self.lastError = msg
        }
    }

    func requestReload() {
        stopWatchdog()
        stopLiveWatch()
        DispatchQueue.main.async {
            self.stalled = false
            self.lastError = nil
            self.settle()
            self.reloadTrigger += 1
        }
    }

    func clear() {
        DispatchQueue.main.async {
            self.stalled = false
            self.lastError = nil
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
    let navigation: AppNavigation
    private(set) var lastLoadedKey: String? = nil
    private var lastTermination: Date?
    #if os(macOS)
    private var urlObservation: NSKeyValueObservation?
    lazy var stats = ProcessStatsSampler(navigation: navigation)

    func observePiece(in webView: WKWebView) {
        urlObservation = webView.observe(\.url, options: [.initial, .new]) { [weak webView, weak self] _, _ in
            Task { @MainActor in
                guard let webView else { return }
                // A page whose process has ended no longer says where it was.
                if let url = webView.url, url.scheme == "https" { self?.navigation.lastPage = url.absoluteString }
                let title = AppRoute.windowTitle(for: webView.url)
                self?.navigation.windowTitle = title
                webView.window?.title = title
            }
        }
    }
    #endif

    init(bootStatus: BootStatus, navigation: AppNavigation) {
        self.bootStatus = bootStatus
        self.navigation = navigation
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
    func webView(_ webView: WKWebView, decidePolicyFor action: WKNavigationAction,
                 decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        guard let url = action.request.url, url.scheme?.lowercased() == "mime" else {
            decisionHandler(.allow)
            return
        }
        // Hand off links from our main document, never an embedded piece iframe.
        let source = action.sourceFrame.securityOrigin
        if action.sourceFrame.isMainFrame, source.protocol == "https",
           ["aesthetic.computer", "mime.ac"].contains(source.host),
           AppRoute.mimeFallback(for: url) != nil {
            ApplePlatform.openExternal(url)
        }
        decisionHandler(.cancel)
    }

    func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!) {
        #if os(macOS)
        PointerLock.release(notifyPage: false) // the locked frame is going away
        #endif
        LaunchPing.loaded(false)
        // Only watchdog real network loads; offline.html doesn't have JS that
        // can heartbeat back, so we'd false-alarm on the static offline page.
        if let url = webView.url, url.scheme == "https" || url.scheme == "http" {
            bootStatus?.startWatchdog()
        }
    }

    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
        bootStatus?.fail(error)
    }

    func webView(_ webView: WKWebView,
                 didFailProvisionalNavigation navigation: WKNavigation!,
                 withError error: Error) {
        bootStatus?.fail(error)
    }

    func webView(_ webView: WKWebView, didCommit navigation: WKNavigation!) {
        #if os(macOS)
        LaunchTrace.mark("commit")
        // Give the live document keyboard focus without waiting for every
        // subresource to finish (audio/streams can keep a piece loading).
        webView.window?.makeFirstResponder(webView)
        #endif
        self.navigation.committed()
    }

    func webViewWebContentProcessDidTerminate(_ webView: WKWebView) {
        #if os(macOS)
        PointerLock.release(notifyPage: false)
        #endif
        // Come back on our own, in a fresh instance — unless it keeps dying.
        // Twice in 10 s waits out the 20 s between recoveries (see `recover`).
        if let last = lastTermination, Date().timeIntervalSince(last) < 10 {
            bootStatus?.fail("the page's process keeps ending")
        } else {
            bootStatus?.recovering("the page's process ended")
            navigation.hardReload()
        }
        lastTermination = Date()
    }

    #if os(macOS)
    /// Start the page's audio graph, then look once more. Every fresh page
    /// gets this at its `boot:ready`: a first launch, a reload, a new
    /// instance after a stall or an ended process. The first call makes the
    /// AudioContext and asks it to run; the second, 1.5 s on, asks again if
    /// it is still not running (it was asked once and never checked before).
    /// A page with no `window.AC` (rax opened on its own) answers "none" and
    /// is left alone.
    private func startAudio(in webView: WKWebView?, again: Bool = true) {
        let js = "(() => { const a = window.AC?.startAudio?.(); return a ? String(a.state) : 'none'; })()"
        webView?.evaluateJavaScript(js) { [weak self, weak webView] state, error in
            if let error { NSLog("AC audio startup: %@", error.localizedDescription) }
            guard again, (state as? String) != "running", (state as? String) != "none" else { return }
            DispatchQueue.main.asyncAfter(deadline: .now() + 1.5) {
                self?.startAudio(in: webView, again: false)
            }
        }
    }
    #endif

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
        #if os(macOS)
        if message.name == "acPointerLock" {
            PointerLock.handle(message)
            return
        }
        if message.name == "fullscreen" {
            // rax's full-screen word (F, or a pad's View button, which WebKit
            // takes for no gesture): the window itself fills the screen.
            // Element fullscreen is off here, so the page cannot know it is
            // full and says `on: true` again to leave: that also toggles.
            guard message.frameInfo.securityOrigin.protocol == "https",
                  message.frameInfo.securityOrigin.host == "aesthetic.computer",
                  let window = message.webView?.window else { return }
            let on = ((message.body as? [String: Any])?["on"] as? Bool) ?? true
            if on || window.styleMask.contains(.fullScreen) { window.toggleFullScreen(nil) }
            return
        }
        #endif

        guard message.name == "iOSApp", let jsonString = message.body as? String else { return }
        guard let data = jsonString.data(using: .utf8) else { return }
        guard let dictionary = try? JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] else {
            print("📱 iOSApp message: failed to parse \(jsonString)")
            return
        }

        guard let type = dictionary["type"] as? String else { return }

        switch type {
        #if os(macOS)
        case "cursor:hidden":
            // The page hides the pointer (rax's world): the native cursor
            // stops reasserting itself until the page says otherwise.
            guard message.frameInfo.isMainFrame, message.frameInfo.securityOrigin.protocol == "https",
                  message.frameInfo.securityOrigin.host == "aesthetic.computer",
                  let webView = message.webView as? ACNativeWebView else { return }
            webView.pageHidesCursor = (dictionary["body"] as? Bool) ?? false
        #endif
        case "window:color":
            guard message.frameInfo.isMainFrame, message.frameInfo.securityOrigin.protocol == "https",
                  message.frameInfo.securityOrigin.host == "aesthetic.computer",
                  let webView = message.webView, let rgb = dictionary["body"] as? [Double] else { return }
            ApplePlatform.colorWindow(rgb, webView: webView)
        #if os(macOS)
        case "fps:hud":
            // The fps readout is up: sample memory and CPU for it.
            guard message.frameInfo.isMainFrame, let webView = message.webView else { return }
            if (dictionary["body"] as? Bool) == true { stats.start(webView) } else { stats.stop() }
        #endif
        case "boot:heartbeat":
            #if os(macOS)
            LaunchTrace.mark("firstHeartbeat")
            #endif
            bootStatus?.heartbeat()
        case "boot:ready":
            bootStatus?.heartbeat(ready: true)
            if message.frameInfo.isMainFrame, message.frameInfo.securityOrigin.protocol == "https",
               message.frameInfo.securityOrigin.host == "aesthetic.computer" {
                navigation.booted()
                LaunchPing.loaded(true)
                #if os(macOS)
                // Autoplay is allowed in our native host. Warm the worklet
                // before the first curtain click or key so its feedback isn't
                // lost while the audio graph is still being constructed.
                startAudio(in: message.webView)
                MacPromptLatencyProbe.run(in: message.webView)
                LaunchTrace.ready(message.webView)
                message.webView?.evaluateJavaScript("window.acFrameMeter?.(false)") { _, _ in }
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
        case "notifications:ready", "notifications", "notifications:offer":
            guard message.frameInfo.isMainFrame, message.frameInfo.securityOrigin.protocol == "https",
                  message.frameInfo.securityOrigin.host == "aesthetic.computer",
                  let webView = message.webView else { return }
            if type == "notifications:ready" {
                AppDelegate.shared?.pushBridgeReady(in: webView)
            } else if type == "notifications:offer" {
                AppDelegate.shared?.offerNotifications(in: webView)
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
            guard message.frameInfo.isMainFrame, message.frameInfo.securityOrigin.protocol == "https",
                  message.frameInfo.securityOrigin.host == "aesthetic.computer" else { return }
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
    let navigation: AppNavigation

    func makeCoordinator() -> Coordinator {
        Coordinator(bootStatus: bootStatus, navigation: navigation)
    }

    func makeWebView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        #if os(macOS)
        LaunchTrace.mark("makeWebView")
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
        // One boolean for a trusted touch on the piece. No keys, coordinates or URLs.
        // Layers stack over the canvas (piece DOM content, the modal), so any
        // target inside the runtime's wrapper counts, not only a <canvas>.
        config.userContentController.addUserScript(WKUserScript(source: """
            (() => {
              let sent = false;
              document.addEventListener('visibilitychange', () => { if (!document.hidden) sent = false; });
              document.addEventListener('pointerdown', e => {
                if (sent || !e.isTrusted || !(e.target instanceof Element) || !e.target.closest('#aesthetic-computer')) return;
                sent = true;
                window.webkit.messageHandlers.iOSApp.postMessage(JSON.stringify({type:'usage:canvas'}));
              }, {capture:true, passive:true});
            })();
            """, injectionTime: .atDocumentStart, forMainFrameOnly: true))

        // 🧹 Wipe the legacy cache surfaces at launch. Cookies + localStorage
        // are preserved so the user stays logged in. The HTTP disk cache, the
        // service worker and its module cache now survive: sw.js revalidates
        // bios, boot and every lib/ module against lith on each boot (a 304
        // when unchanged) and never caches pieces, so they no longer pin stale
        // modules — and wiping them made every launch and refresh a cold boot
        // (~2.3 s vs ~0.6 s warm), and raced the first page load (a deleted
        // service worker failing a module import mid-boot).
        let cacheTypes: Set<String> = [
            WKWebsiteDataTypeMemoryCache,
            WKWebsiteDataTypeOfflineWebApplicationCache,
            WKWebsiteDataTypeIndexedDBDatabases,
            WKWebsiteDataTypeWebSQLDatabases,
        ]
        config.websiteDataStore.removeData(
            ofTypes: cacheTypes,
            modifiedSince: .distantPast
        ) {}
        URLCache.shared.removeAllCachedResponses()

        #if os(macOS)
        // 🪪 The suite's shared login (~/.ac-token), handed to the page before it boots.
        if let script = SharedSession.userScript() { config.userContentController.addUserScript(script) }
        config.applicationNameForUserAgent = "AestheticMac"
        config.userContentController.addUserScript(WKUserScript(
            source: "window.acNativeWindowColor = true;",
            injectionTime: .atDocumentStart, forMainFrameOnly: true))
        // Every frame, so the rax iframe can capture the mouse for mouse-look
        // (WKWebView never grants the Pointer Lock API itself).
        config.userContentController.add(context.coordinator, name: "acPointerLock")
        // Every frame as well: rax asks for the window to fill the screen.
        config.userContentController.add(context.coordinator, name: "fullscreen")
        config.userContentController.addUserScript(WKUserScript(
            source: PointerLock.script, injectionTime: .atDocumentStart, forMainFrameOnly: false))
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
        webView.navigation = navigation
        #else
        let webView = WKWebView(frame: .zero, configuration: config)
        #endif
        #if os(iOS)
        webView.backgroundColor = UIColor(red: grey, green: grey, blue: grey, alpha: 1)
        webView.isOpaque = false
        #endif
        navigation.webView = webView
        webView.uiDelegate = context.coordinator
        webView.navigationDelegate = context.coordinator
        #if os(macOS)
        // Keep WebKit's desktop identity; exact "Aesthetic" means iPhone in AC.
        webView.customUserAgent = nil
        webView.focusRingType = .none
        // No white sheet of WebKit's own: the page is transparent until its
        // piece paints (body.nogap), so the backdrop behind shows instead.
        webView.setValue(false, forKey: "drawsBackground")
        // Safari's Web Inspector can attach to the native app (Develop menu →
        // this Mac → Aesthetic Computer): Timelines, Graphics and Layers for
        // the real WKWebView, which has no CDP (2026-11-08).
        if #available(macOS 13.3, *) { webView.isInspectable = true }
        ClaudeIn.shared.attach(webView)
        context.coordinator.observePiece(in: webView)
        if let rgb = navigation.pageColor {
            webView.underPageBackgroundColor = NSColor(srgbRed: rgb[0] / 255,
                green: rgb[1] / 255, blue: rgb[2] / 255, alpha: 1)
        } else {
            webView.underPageBackgroundColor = NSColor(white: grey, alpha: 1)
        }
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
            items.removeAll { $0.name == "nogap" || $0.name == "_iosbust" }
            items.append(URLQueryItem(name: "nogap", value: "true"))
            items.append(URLQueryItem(
                name: "_iosbust",
                value: String(Int(Date().timeIntervalSince1970))
            ))
            components.queryItems = items
        }
        guard let bustedURL = components.url else { return }
        // The shell is per-request HTML and _iosbust already makes its URL new;
        // a reload-ignoring-cache policy would also force every module and
        // asset of the page past the cache.
        // A hard reload skips the cache for the document only.
        let request = URLRequest(
            url: bustedURL,
            cachePolicy: navigation.bypassCache ? .reloadIgnoringLocalCacheData : .useProtocolCachePolicy,
            timeoutInterval: 30
        )
        navigation.bypassCache = false
        #if os(macOS)
        LaunchTrace.mark("load")
        #endif
        webView.load(request)
    }
}

// MARK: - ContentView

struct ContentView: View {
    #if os(macOS)
    @StateObject private var navigation = AppNavigation()
    @Environment(\.openWindow) private var openWindow
    #else
    @ObservedObject private var navigation = AppNavigation.shared
    #endif
    @StateObject private var monitor = AppNetworkMonitor()
    @StateObject private var bootStatus = BootStatus()
    @ObservedObject private var pop = PopPlayer.shared
    @Environment(\.scenePhase) private var scenePhase
    @State private var lastBackgroundedAt: Date? = nil
    @State private var lastForceLiveTrigger: Int = 0
    @State private var wasOffline = false
    @State private var usageScene = UUID()

    private var liveURL: String { navigation.url }
    private var backdrop: Color {
        #if os(macOS)
        // The page's own last colour, else the runtime's grey.
        if let rgb = navigation.pageColor {
            return Color(.sRGB, red: rgb[0] / 255, green: rgb[1] / 255, blue: rgb[2] / 255)
        }
        return Color(red: grey, green: grey, blue: grey)
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
                        bootStatus: bootStatus, navigation: navigation
                    )
                    .id(navigation.instance) // A new instance is a new web process.
                }

                if let frame = navigation.handoff {
                    Image(acImage: frame)
                        .resizable()
                        .interpolation(.none)
                        .allowsHitTesting(false)
                }

                // The offline page says its own piece; the line is for the rest.
                if let trouble = bootStatus.trouble, monitor.isOnline != false {
                    RecoveryLine(trouble: trouble)
                }
            }
            .padding(.bottom, geometry.safeAreaInsets.bottom > 0 ? 24 : 0)
            .background(backdrop)
            #if os(macOS)
            .navigationTitle(navigation.windowTitle)
            .focusedSceneObject(navigation)
            .onAppear { AppDelegate.shared?.openPromptWindow = { openWindow(id: "prompt") } }
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
                // Only offline → online: the monitor's first report (unknown →
                // online) at launch used to reload the page mid-load.
                if newValue == false { wasOffline = true; bootStatus.settle() }
                if newValue == true, wasOffline {
                    wasOffline = false
                    bootStatus.requestReload()
                }
            }
            // A stalled boot or a failed load recovers on its own, quietly, in
            // a fresh instance — no overlay (retired 2026-11-11), only the
            // recovery line saying why; at most every 20 s so a server that's
            // down isn't hammered.
            .onChange(of: bootStatus.stalled) { stalled in
                if stalled { recover("the page stopped answering") }
            }
            .onChange(of: bootStatus.lastError) { error in
                if let error { recover(error, failed: true) }
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

    private func recover(_ reason: String, failed: Bool = false) {
        bootStatus.clear()
        let go = { [bootStatus, navigation] in
            bootStatus.recovering(reason, failed: failed)
            navigation.hardReload()
        }
        // Too soon after the last one: the try is kept for when the 20 s are
        // up (it used to be dropped, and the next stall picked it up).
        let wait = 20 - Date().timeIntervalSince(bootStatus.lastRecoveryAt)
        if wait > 0 { bootStatus.wait(reason, failed: failed, seconds: wait, then: go) } else { go() }
    }

    private func handleScenePhase(_ phase: ScenePhase) {
        switch phase {
        case .background:
            bootStatus.appActive = false
            LaunchPing.inactive(scene: usageScene, background: true)
            lastBackgroundedAt = Date()
        case .active:
            bootStatus.appActive = true
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
            bootStatus.appActive = false
            LaunchPing.inactive(scene: usageScene, background: false)
        @unknown default:
            break
        }
    }
}

// MARK: - Recovery line
//
// One quiet line, low in the corner, while the page is being brought back or
// could not be: why, and when the next try is. It is not the overlay retired
// on 2026-11-11: it covers nothing, takes no clicks or keys, and is gone the
// moment the page says `boot:ready`. A recovery over in 1.5 s shows nothing.
struct RecoveryLine: View {
    let trouble: BootStatus.Trouble
    @State private var now = Date()
    private let tick = Timer.publish(every: 0.5, on: .main, in: .common).autoconnect()

    var body: some View {
        Group {
            if trouble.shows(at: now) {
                Text(trouble.line(at: now))
                    .font(.system(size: 11, design: .monospaced))
                    .foregroundColor(Color.white.opacity(0.9))
                    .lineLimit(1)
                    .padding(.horizontal, 9)
                    .padding(.vertical, 4)
                    .background(Capsule().fill(Color.black.opacity(0.55)))
            }
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity, alignment: .bottomLeading)
        .padding(10)
        .allowsHitTesting(false)
        .onReceive(tick) { now = $0 }
    }
}

struct ContentView_Previews: PreviewProvider {
    static var previews: some View {
        ContentView()
    }
}

#if os(macOS)
import CryptoKit

// MARK: - 🪪 The shared AC login
//
// The AC suite (ac-login, the desktop app, the MCP servers, slab) shares one
// login in ~/.ac-token. This app takes it as they do: the page is handed the
// session in `session-aesthetic`, the form boot.mjs reads, before it boots.
// An expired token is renewed with its refresh token and written back, as
// toolchain/mcp/ac-token.mjs does, so every tool goes on reading one file.
// A sign-in made in the page itself is left alone: the shared session is
// written only where there is none, or where the one there was ours.
enum SharedSession {
    static let home = String(cString: getpwuid(getuid()).pointee.pw_dir)
    static var file: String { "\(home)/.ac-token" }
    private static let domain = "hi.aesthetic.computer"
    private static let client = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt"

    static func read() -> [String: Any]? {
        guard let data = FileManager.default.contents(atPath: file) else { return nil }
        return (try? JSONSerialization.jsonObject(with: data)) as? [String: Any]
    }
    static func expiry(_ raw: [String: Any]) -> Double { (raw["expires_at"] as? NSNumber)?.doubleValue ?? 0 }
    static func live(_ raw: [String: Any]) -> Bool { expiry(raw) > Date().timeIntervalSince1970 * 1000 + 60_000 }

    /// `session-aesthetic` as boot.mjs takes it: URI-encoded base64 JSON.
    static func param(_ raw: [String: Any]) -> String? {
        guard let token = raw["access_token"] as? String,
              let user = raw["user"] as? [String: Any], let sub = user["sub"] as? String,
              let json = try? JSONSerialization.data(withJSONObject: [
                "accessToken": token, "account": ["label": user["email"] as? String ?? "", "id": sub],
              ]) else { return nil }
        return json.base64EncodedString().addingPercentEncoding(withAllowedCharacters: .alphanumerics)
    }

    /// The page's side: written where no session is, or where ours was, and
    /// never over a newer one or in the middle of a sign-in's return.
    static func js(_ raw: [String: Any]) -> String? {
        guard let param = param(raw) else { return nil }
        let expires = Int(expiry(raw))
        return """
        (() => {
          try {
            if (!/(^|\\.)(aesthetic\\.computer|prompt\\.ac)$/.test(location.hostname)) return false;
            if (/[?&](code|state)=/.test(location.search)) return false;
            const had = localStorage.getItem('session-aesthetic'), ours = Number(localStorage.getItem('ac-shared-session') || 0);
            if ((had && !ours) || \(expires) <= Date.now() || \(expires) <= ours) return false;
            localStorage.setItem('session-aesthetic', '\(param)');
            localStorage.setItem('ac-shared-session', '\(expires)');
            return true;
          } catch { return false; }
        })();
        """
    }

    static func userScript() -> WKUserScript? {
        guard let raw = read(), live(raw), let source = js(raw) else { return nil }
        return WKUserScript(source: source, injectionTime: .atDocumentStart, forMainFrameOnly: true)
    }

    /// Renew an expired token and write the file back. → the new record.
    static func renew() async -> [String: Any]? {
        guard var raw = read(), !live(raw), let refresh = raw["refresh_token"] as? String,
              let url = URL(string: "https://\(domain)/oauth/token") else { return nil }
        var request = URLRequest(url: url)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "content-type")
        request.httpBody = try? JSONSerialization.data(withJSONObject: [
            "grant_type": "refresh_token", "client_id": client, "refresh_token": refresh,
        ])
        guard let (data, response) = try? await URLSession.shared.data(for: request),
              (response as? HTTPURLResponse)?.statusCode == 200,
              let next = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any],
              let access = next["access_token"] as? String else { return nil }
        raw["access_token"] = access
        if let rotated = next["refresh_token"] as? String { raw["refresh_token"] = rotated }
        if let id = next["id_token"] as? String { raw["id_token"] = id }
        let seconds = (next["expires_in"] as? NSNumber)?.doubleValue ?? 3600
        raw["expires_at"] = Int(Date().timeIntervalSince1970 * 1000 + seconds * 1000)
        guard let out = try? JSONSerialization.data(withJSONObject: raw, options: [.prettyPrinted]),
              (try? out.write(to: URL(fileURLWithPath: file), options: [.atomic])) != nil else { return nil }
        try? FileManager.default.setAttributes([.posixPermissions: 0o600], ofItemAtPath: file)
        return raw
    }
}

// MARK: - 🦀 Claude in
//
// A door on this Mac's loopback for an agent working with whoever is at the
// keys: it can ask where the app is, run a line of JavaScript in the page and
// take a picture of it. The Electron app has Chromium's debugging port for
// this; a WKWebView has none, so this is the native app's.
//
// The door is 127.0.0.1 only, on a port the system picks, and every request
// must carry a secret made at launch. Port and secret are written to
// ~/.ac-claude-in.json (mode 600), so only this user's own tools can knock;
// a request with an Origin (a web page's) is refused. It also keeps the
// shared login fresh while the app runs.
final class ClaudeIn {
    static let shared = ClaudeIn()
    private weak var webView: WKWebView?
    private var listener: NWListener?
    private var timer: Timer?
    private let secret = SymmetricKey(size: .bits256).withUnsafeBytes { Data($0).map { String(format: "%02x", $0) }.joined() }
    private var card: String { "\(SharedSession.home)/.ac-claude-in.json" }

    func attach(_ webView: WKWebView) {
        self.webView = webView
        if listener == nil { listen() }
        if timer == nil {
            timer = Timer.scheduledTimer(withTimeInterval: 300, repeats: true) { [weak self] _ in self?.keepSession() }
            keepSession()
        }
    }

    /// An expired shared login is renewed; the page is told, and loaded again
    /// once if it had booted without one.
    private func keepSession() {
        Task { @MainActor in
            guard let raw = await SharedSession.renew(), let source = SharedSession.js(raw), let webView = self.webView else { return }
            guard let wrote = try? await webView.evaluateJavaScript(source) as? Bool, wrote else { return }
            if let signedIn = try? await webView.evaluateJavaScript("!!window.acUSER") as? Bool, !signedIn { webView.reload() }
        }
    }

    private func listen() {
        let parameters = NWParameters.tcp
        parameters.requiredLocalEndpoint = .hostPort(host: "127.0.0.1", port: .any)
        guard let listener = try? NWListener(using: parameters) else { return }
        self.listener = listener
        listener.stateUpdateHandler = { [weak self] state in
            guard let self, case .ready = state, let port = listener.port?.rawValue else { return }
            let record: [String: Any] = ["port": Int(port), "secret": self.secret, "pid": Int(ProcessInfo.processInfo.processIdentifier),
                                         "app": Bundle.main.bundleIdentifier ?? "", "at": Int(Date().timeIntervalSince1970 * 1000)]
            guard let data = try? JSONSerialization.data(withJSONObject: record) else { return }
            FileManager.default.createFile(atPath: self.card, contents: data, attributes: [.posixPermissions: 0o600])
        }
        listener.newConnectionHandler = { [weak self] connection in
            connection.start(queue: .main)
            self?.read(connection, Data())
        }
        listener.start(queue: .main)
    }

    private func read(_ connection: NWConnection, _ soFar: Data) {
        connection.receive(minimumIncompleteLength: 1, maximumLength: 1 << 16) { [weak self] chunk, _, done, error in
            guard let self else { return connection.cancel() }
            var data = soFar
            if let chunk { data.append(chunk) }
            if data.count > 1 << 20 { return connection.cancel() }
            guard let split = data.range(of: Data("\r\n\r\n".utf8)) else {
                return done || error != nil ? connection.cancel() : self.read(connection, data)
            }
            let head = String(decoding: data[..<split.lowerBound], as: UTF8.self).components(separatedBy: "\r\n")
            var headers: [String: String] = [:]
            for line in head.dropFirst() {
                guard let colon = line.firstIndex(of: ":") else { continue }
                headers[line[..<colon].lowercased()] = line[line.index(after: colon)...].trimmingCharacters(in: .whitespaces)
            }
            let body = data[split.upperBound...]
            if body.count < Int(headers["content-length"] ?? "0") ?? 0 {
                return done || error != nil ? connection.cancel() : self.read(connection, data)
            }
            let request = (head.first ?? "").split(separator: " ").map(String.init)
            self.answer(connection, method: request.first ?? "", path: request.count > 1 ? request[1] : "", headers: headers, body: Data(body))
        }
    }

    private func send(_ connection: NWConnection, _ status: String, _ type: String, _ body: Data) {
        let head = "HTTP/1.1 \(status)\r\nContent-Type: \(type)\r\nContent-Length: \(body.count)\r\nCache-Control: no-store\r\nConnection: close\r\n\r\n"
        connection.send(content: Data(head.utf8) + body, completion: .contentProcessed { _ in connection.cancel() })
    }
    private func json(_ connection: NWConnection, _ status: String, _ value: [String: Any]) {
        let data = (try? JSONSerialization.data(withJSONObject: value, options: [.fragmentsAllowed])) ?? Data("{}".utf8)
        send(connection, status, "application/json", data)
    }

    private func answer(_ connection: NWConnection, method: String, path: String, headers: [String: String], body: Data) {
        let given = Data((headers["authorization"] ?? "").utf8), wanted = Data("Bearer \(secret)".utf8)
        guard headers["origin"] == nil, given.count == wanted.count,
              zip(given, wanted).reduce(UInt8(0), { $0 | ($1.0 ^ $1.1) }) == 0 else {
            return json(connection, "403 Forbidden", ["ok": false, "error": "refused"])
        }
        guard let webView else { return json(connection, "503 Service Unavailable", ["ok": false, "error": "no page"]) }
        switch (method, path) {
        case ("GET", "/info"):
            json(connection, "200 OK", [
                "ok": true, "app": Bundle.main.bundleIdentifier ?? "",
                "version": Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "",
                "url": webView.url?.absoluteString ?? "", "title": webView.title ?? "",
                "width": Int(webView.bounds.width), "height": Int(webView.bounds.height),
            ])
        case ("POST", "/eval"):
            // The body is the inside of an async function: `return` what is wanted.
            webView.callAsyncJavaScript(String(decoding: body, as: UTF8.self), arguments: [:], in: nil, in: .page) { [weak self] result in
                switch result {
                case .success(let value):
                    let plain = JSONSerialization.isValidJSONObject([value]) ? value : String(describing: value)
                    self?.json(connection, "200 OK", ["ok": true, "value": plain])
                case .failure(let error):
                    let detail = (error as NSError).userInfo["WKJavaScriptExceptionMessage"] as? String ?? error.localizedDescription
                    self?.json(connection, "200 OK", ["ok": false, "error": detail])
                }
            }
        case ("GET", "/shot"):
            webView.takeSnapshot(with: nil) { [weak self] image, _ in
                guard let tiff = image?.tiffRepresentation, let bitmap = NSBitmapImageRep(data: tiff),
                      let png = bitmap.representation(using: .png, properties: [:]) else {
                    self?.json(connection, "500 Internal Server Error", ["ok": false, "error": "no picture"]); return
                }
                self?.send(connection, "200 OK", "image/png", png)
            }
        default:
            json(connection, "404 Not Found", ["ok": false, "error": "no such door"])
        }
    }
}
#endif
