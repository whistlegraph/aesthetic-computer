import SwiftUI
import WebKit
import AVKit

// AirPlay projection, two ways. Unlike AC OS and the Xbox (which receive the
// source and run it themselves), the piece runs here on the phone in a second
// runtime and the TV just shows it. The phone keeps its controls; its own
// preview goes quiet so the TV copy is the one you hear.
// - Screen Mirroring: Whistlegraph claims the mirrored screen with its own
//   window, live, full-screen at the TV's size.
// - The AirPlay button: the second runtime records itself (live-tape.js) into an
//   HLS stream on the Wi-Fi (LiveStream) and AVPlayer hands it to the Apple TV,
//   which plays the piece a few seconds behind, like a video.
@MainActor final class AirPlayProjector: NSObject, ObservableObject, WKScriptMessageHandler, WKNavigationDelegate {
    static let shared = AirPlayProjector()
    /// An external screen is attached (Screen Mirroring is on).
    @Published private(set) var connected = false
    /// The piece fills that screen. False after Stop, when the TV mirrors the phone again.
    @Published private(set) var projecting = false
    /// The stream is recording, waiting for or playing on an AirPlay TV.
    @Published private(set) var streaming = false
    /// The stream has enough to play.
    @Published private(set) var streamReady = false
    /// The stream is playing on the Apple TV rather than silently on the phone.
    @Published private(set) var external = false
    let player = AVPlayer()
    private let stream = LiveStream()
    private var camera: UIWindow?
    private var generation = 0
    private var watchers: [NSObjectProtocol] = []
    private var externalWatch: NSKeyValueObservation?
    /// The TV sheet is open, so a stream should be ready for the AirPlay button.
    private var wanted = false
    @Published private(set) var screenName = ""
    /// Called whenever `projecting` flips, so the phone preview can mute or unmute.
    var onProjectingChanged: (() -> Void)?
    private var scene: UIWindowScene?
    private var window: UIWindow?
    private var view: WKWebView?
    private var frame: WKFrameInfo?
    private var source = ""
    private var rendered = ""
    private var request = 0
    private var stopped = false

    #if DEBUG
    /// A moving circle and the wall clock, so the delay on the TV can be read off the screen.
    static let testSource = "export function paint({ wipe, ink, screen, paintCount }) {\n  wipe(20, 16, 48);\n  const t = Number(paintCount) / 30;\n  ink(255, 120, 200).circle(screen.width / 2 + Math.cos(t) * screen.width / 4, screen.height / 2, screen.height / 6, true);\n  ink(255).write(new Date().toISOString().slice(17, 22), { center: \"x\", y: screen.height - 24 });\n}"
    /// Simulator check of the AirPlay stream: WHISTLEGRAPH_HLS_TEST=1 streams the test piece at launch.
    func streamTest() {
        guard ProcessInfo.processInfo.environment["WHISTLEGRAPH_HLS_TEST"] == "1" else { return }
        source = Self.testSource
        want()
    }
    #endif
    /// Every painted checkpoint of the current piece, whether or not a TV is attached.
    func update(_ source: String) {
        guard !source.isEmpty else { return }
        self.source = source
        render()
    }

    override init() {
        super.init()
        player.allowsExternalPlayback = true
        // Mirroring shows the live window instead; the stream is for the AirPlay button.
        player.usesExternalPlaybackWhileExternalScreenIsActive = false
        player.isMuted = true
        externalWatch = player.observe(\.isExternalPlaybackActive, options: [.initial, .new]) { [weak self] player, _ in
            let active = player.isExternalPlaybackActive
            Task { @MainActor in self?.externalChanged(active) }
        }
        watchers.append(NotificationCenter.default.addObserver(forName: AVAudioSession.routeChangeNotification, object: nil, queue: .main) { [weak self] _ in
            Task { @MainActor in self?.refreshName() }
        })
        stream.onReady = { [weak self] in self?.play() }
    }

    func attach(_ scene: UIWindowScene) {
        stopStream()
        self.scene = scene
        connected = true
        #if DEBUG
        // Simulator check (I/O › External Displays) without signing in.
        if ProcessInfo.processInfo.environment["WHISTLEGRAPH_AIRPLAY_TEST"] == "1", source.isEmpty { source = Self.testSource }
        #endif
        refreshName()
        if !stopped { show() }
        DeviceActionLog.shared.record(.projection, .presented, control: .tv)
    }
    func detach(_ scene: UIWindowScene) {
        guard scene === self.scene else { return }
        hide()
        self.scene = nil; connected = false; screenName = ""; stopped = false
        DeviceActionLog.shared.record(.projection, .dismissed, control: .tv)
        if wanted { startStream() }
    }

    // MARK: The AirPlay stream

    /// The TV sheet opened: have a stream playing so picking an Apple TV sends the picture.
    func want() { wanted = true; if !connected { startStream() } }
    /// The TV sheet closed: keep the stream only while a TV is playing it.
    func release() { wanted = false; if !external { stopStream() } }
    func stopStream() {
        guard streaming else { return }
        if let frame, let view { Task { _ = try? await view.callAsyncJavaScript("window.whistlegraphLiveTape?.stop()", arguments: [:], in: frame, contentWorld: .page) } }
        player.pause(); player.replaceCurrentItem(with: nil)
        stream.stop()
        teardown()
        camera?.isHidden = true; camera = nil
        streaming = false; streamReady = false
        setProjecting(window != nil || external)
        DeviceActionLog.shared.record(.projection, .ended, control: .tv)
    }
    func startStream() {
        guard !streaming, window == nil else { return }
        do { try stream.start() } catch { NSLog("AirPlay stream could not listen: %@", error.localizedDescription); return }
        // The camera runtime sits under the app's own window: on screen as far as
        // WebKit knows, so it keeps painting, but never seen. 640×360 points is 16:9.
        guard let scene = UIApplication.shared.connectedScenes.compactMap({ $0 as? UIWindowScene }).first(where: { $0.session.role == .windowApplication }) else { return }
        let view = makeView(tape: true)
        let controller = UIViewController()
        controller.view.backgroundColor = .black
        view.frame = CGRect(x: 0, y: 0, width: 640, height: 360)
        controller.view.addSubview(view)
        let camera = UIWindow(windowScene: scene)
        camera.frame = view.frame
        camera.windowLevel = UIWindow.Level(rawValue: UIWindow.Level.normal.rawValue - 1)
        camera.isUserInteractionEnabled = false
        camera.rootViewController = controller
        camera.isHidden = false
        self.camera = camera; self.view = view
        streaming = true
        refreshName()
        DeviceActionLog.shared.record(.projection, .started, control: .tv)
    }
    private func startTape() {
        guard streaming, let frame, let view else { return }
        generation += 1
        Task { _ = try? await view.callAsyncJavaScript("window.whistlegraphLiveTape?.start(generation)", arguments: ["generation": generation], in: frame, contentWorld: .page) }
    }
    private func play() {
        guard streaming, player.currentItem == nil, let url = stream.url else { return }
        let item = AVPlayerItem(url: url)
        // Sit as close to the live edge as the player will go.
        item.configuredTimeOffsetFromLive = CMTime(seconds: LiveStream.target * 2, preferredTimescale: 600)
        item.automaticallyPreservesTimeOffsetFromLive = true
        item.preferredForwardBufferDuration = LiveStream.target
        player.automaticallyWaitsToMinimizeStalling = false
        player.replaceCurrentItem(with: item)
        player.play()
        streamReady = true
        #if DEBUG
        NSLog("AirPlay stream: %@", url.absoluteString)
        #endif
    }
    private func externalChanged(_ active: Bool) {
        external = active
        // Silent on the phone; full volume once it is the TV playing.
        player.isMuted = !active
        refreshName()
        setProjecting(window != nil || active)
        if active { DeviceActionLog.shared.record(.projection, .presented, control: .tv) }
        else if !wanted { stopStream() }
    }
    /// Gives the TV back to mirroring the phone, without ending Screen Mirroring itself.
    func stop() { stopped = true; hide() }
    func resume() { stopped = false; if scene != nil { show() } }

    private func show() {
        guard let scene, window == nil else { return }
        let view = makeView()
        let controller = UIViewController()
        controller.view.backgroundColor = .black
        view.frame = controller.view.bounds
        view.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        controller.view.addSubview(view)
        let window = UIWindow(windowScene: scene)
        window.rootViewController = controller
        window.isHidden = false
        self.window = window; self.view = view
        setProjecting(true)
    }
    private func hide() {
        teardown()
        window?.isHidden = true
        window = nil
        setProjecting(external)
    }
    private func teardown() {
        view?.configuration.userContentController.removeScriptMessageHandler(forName: "whistlegraph")
        view?.navigationDelegate = nil
        view?.stopLoading()
        view?.removeFromSuperview()
        view = nil; frame = nil; rendered = ""
    }
    private func setProjecting(_ value: Bool) {
        guard projecting != value else { return }
        projecting = value
        // Sleeping the phone ends mirroring, so keep it awake while the TV is showing.
        UIApplication.shared.isIdleTimerDisabled = value
        onProjectingChanged?()
    }
    private func refreshName() {
        guard connected || streaming else { return }
        let route = AVAudioSession.sharedInstance().currentRoute.outputs.first { $0.portType == .airPlay }
        screenName = route?.portName ?? "the TV"
    }

    private func makeView(tape: Bool = false) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.websiteDataStore = .nonPersistent()
        config.setURLSchemeHandler(WhistlegraphBundle(), forURLScheme: WhistlegraphBundle.storageScheme)
        config.mediaTypesRequiringUserActionForPlayback = []
        config.allowsInlineMediaPlayback = true
        config.preferences.inactiveSchedulingPolicy = .none
        config.userContentController.add(self, name: "whistlegraph")
        config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphPixelSize = \(WhistlegraphPreview.savedPixelSize);", injectionTime: .atDocumentStart, forMainFrameOnly: false))
        config.userContentController.addUserScript(WKUserScript(source: WhistlegraphPreview.script, injectionTime: .atDocumentStart, forMainFrameOnly: false))
        if tape { config.userContentController.addUserScript(WKUserScript(source: LiveTape.script, injectionTime: .atDocumentStart, forMainFrameOnly: false)) }
        let view = WKWebView(frame: .zero, configuration: config)
        view.navigationDelegate = self
        view.isOpaque = false; view.backgroundColor = .black
        view.scrollView.isScrollEnabled = false
        view.scrollView.contentInsetAdjustmentBehavior = .never
        // story.html is the bare piece host: one full-screen frame, no corner label, no chrome.
        view.load(URLRequest(url: URL(string: "\(WhistlegraphBundle.storageScheme)://app/story.html")!))
        return view
    }
    private func render() {
        guard let view, let frame, !source.isEmpty, source != rendered else { return }
        request += 1
        let arguments: [String: Any] = ["source": source, "request": request]
        rendered = source
        Task { _ = try? await view.callAsyncJavaScript("window.AC?.setMasterVolume(1); window.whistlegraphRender(source, 'whistlegraph-airplay', request); window.AC?.startAudio?.();", arguments: arguments, in: frame, contentWorld: .page) }
    }

    func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        decisionHandler(PreviewNavigation.allows(navigationAction.request.url,
            mainFrame: navigationAction.targetFrame?.isMainFrame, document: .story) ? .allow : .cancel)
    }
    func webView(_ webView: WKWebView, decidePolicyFor navigationResponse: WKNavigationResponse, decisionHandler: @escaping (WKNavigationResponsePolicy) -> Void) {
        decisionHandler(PreviewNavigation.allows(navigationResponse.response.url,
            mainFrame: navigationResponse.isForMainFrame, document: .story) ? .allow : .cancel)
    }
    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        guard message.webView === view,
              PreviewNavigation.bridge(message.frameInfo.request.url, mainFrame: message.frameInfo.isMainFrame, document: .story) == .artwork,
              let body = message.body as? [String: Any] else { return }
        switch body["action"] as? String {
        case "previewReady": frame = message.frameInfo; rendered = ""; render(); startTape()
        case "liveTape":
            guard streaming, let generation = body["generation"] as? Int, generation == self.generation else { return }
            switch body["kind"] as? String {
            case "chunk": if let value = body["data"] as? String, let data = Data(base64Encoded: value) { stream.append(data, generation: generation) }
            case "error": NSLog("AirPlay stream encoding failed: %@", body["error"] as? String ?? "")
            default: break
            }
        default: break
        }
    }
}

/// The external-display scene. iOS hands Whistlegraph the mirrored screen here.
final class AirPlaySceneDelegate: UIResponder, UIWindowSceneDelegate {
    func scene(_ scene: UIScene, willConnectTo session: UISceneSession, options connectionOptions: UIScene.ConnectionOptions) {
        guard let scene = scene as? UIWindowScene else { return }
        Task { @MainActor in AirPlayProjector.shared.attach(scene) }
    }
    func sceneDidDisconnect(_ scene: UIScene) {
        guard let scene = scene as? UIWindowScene else { return }
        Task { @MainActor in AirPlayProjector.shared.detach(scene) }
    }
}

/// The system AirPlay picker.
struct AirPlayRoutePicker: UIViewRepresentable {
    func makeUIView(context: Context) -> AVRoutePickerView {
        let picker = AVRoutePickerView()
        picker.prioritizesVideoDevices = true
        picker.activeTintColor = .systemBlue
        return picker
    }
    func updateUIView(_ view: AVRoutePickerView, context: Context) {}
}

/// The AirPlay section of the TV sheet.
struct AirPlaySection: View {
    @ObservedObject var projector = AirPlayProjector.shared
    var body: some View {
        Section {
            if projector.connected {
                if projector.projecting {
                    Label("Projecting to \(projector.screenName)", systemImage: "airplayvideo").accessibilityIdentifier("airplay-status")
                    Button("Stop projecting", role: .destructive) { projector.stop() }.accessibilityIdentifier("airplay-stop")
                } else {
                    Label("\(projector.screenName) is mirroring your phone", systemImage: "airplayvideo")
                    Button("Show the piece on \(projector.screenName)") { projector.resume() }.accessibilityIdentifier("airplay-resume")
                }
            } else if projector.external {
                Label("Playing on \(projector.screenName)", systemImage: "airplayvideo").accessibilityIdentifier("airplay-status")
                Button("Stop", role: .destructive) { projector.stopStream() }.accessibilityIdentifier("airplay-stop")
            } else {
                HStack {
                    VStack(alignment: .leading, spacing: 2) {
                        Text("Choose an Apple TV")
                        Text(projector.streamReady ? "Ready" : "Getting the picture ready…")
                            .font(.caption).foregroundStyle(.secondary).accessibilityIdentifier("airplay-stream-status")
                    }
                    Spacer()
                    AirPlayRoutePicker().frame(width: 44, height: 44).accessibilityLabel("AirPlay")
                }
            }
        } header: { Text("AirPlay · Apple TV") } footer: {
            Text(projector.connected
                 ? "Screen Mirroring shows the piece live. Your phone keeps its controls, and the sound plays on the TV."
                 : "The piece plays on the TV a few seconds behind your phone, with its sound. For no delay, use Screen Mirroring in Control Center.")
        }
        .onAppear { projector.want() }
        .onDisappear { projector.release() }
    }
}
