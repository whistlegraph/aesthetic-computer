import SwiftUI
import WebKit
import AVKit

// AirPlay projection: when the phone mirrors to an Apple TV or an AirPlay TV,
// Whistlegraph claims that screen with its own window and shows only the
// current piece, full-screen at the TV's size. Unlike AC OS and the Xbox (which
// receive the source and run it themselves), the piece runs here on the phone in
// a second runtime and the TV just shows it. The phone keeps its controls; its
// own preview goes quiet so the TV copy is the one you hear.
@MainActor final class AirPlayProjector: NSObject, ObservableObject, WKScriptMessageHandler, WKNavigationDelegate {
    static let shared = AirPlayProjector()
    /// An external screen is attached (Screen Mirroring is on).
    @Published private(set) var connected = false
    /// The piece fills that screen. False after Stop, when the TV mirrors the phone again.
    @Published private(set) var projecting = false
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

    /// Every painted checkpoint of the current piece, whether or not a TV is attached.
    func update(_ source: String) {
        guard !source.isEmpty else { return }
        self.source = source
        render()
    }

    func attach(_ scene: UIWindowScene) {
        self.scene = scene
        connected = true
        #if DEBUG
        // Simulator check (I/O › External Displays) without signing in.
        if ProcessInfo.processInfo.environment["WHISTLEGRAPH_AIRPLAY_TEST"] == "1", source.isEmpty {
            source = "export function paint({ wipe, ink, screen, paintCount }) {\n  wipe(20, 16, 48);\n  const t = Number(paintCount) / 30;\n  ink(255, 120, 200).circle(screen.width / 2 + Math.cos(t) * screen.width / 4, screen.height / 2, screen.height / 6, true);\n  ink(255).write(\"airplay\", { center: \"x\", y: screen.height - 24 });\n}"
        }
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
        view?.configuration.userContentController.removeScriptMessageHandler(forName: "walkie")
        view?.navigationDelegate = nil
        view?.stopLoading()
        window?.isHidden = true
        window = nil; view = nil; frame = nil; rendered = ""
        setProjecting(false)
    }
    private func setProjecting(_ value: Bool) {
        guard projecting != value else { return }
        projecting = value
        // Sleeping the phone ends mirroring, so keep it awake while the TV is showing.
        UIApplication.shared.isIdleTimerDisabled = value
        onProjectingChanged?()
    }
    private func refreshName() {
        let route = AVAudioSession.sharedInstance().currentRoute.outputs.first { $0.portType == .airPlay }
        screenName = route?.portName ?? "the TV"
    }

    private func makeView() -> WKWebView {
        let config = WKWebViewConfiguration()
        config.websiteDataStore = .nonPersistent()
        config.setURLSchemeHandler(WhistlegraphBundle(), forURLScheme: WhistlegraphBundle.storageScheme)
        config.mediaTypesRequiringUserActionForPlayback = []
        config.allowsInlineMediaPlayback = true
        config.preferences.inactiveSchedulingPolicy = .none
        config.userContentController.add(self, name: "walkie")
        config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphPixelSize = \(WhistlegraphPreview.savedPixelSize);", injectionTime: .atDocumentStart, forMainFrameOnly: false))
        config.userContentController.addUserScript(WKUserScript(source: WhistlegraphPreview.script, injectionTime: .atDocumentStart, forMainFrameOnly: false))
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
        if body["action"] as? String == "previewReady" { frame = message.frameInfo; rendered = ""; render() }
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
            } else {
                HStack {
                    Text("Choose an Apple TV or AirPlay TV")
                    Spacer()
                    AirPlayRoutePicker().frame(width: 44, height: 44).accessibilityLabel("AirPlay")
                }
            }
        } header: { Text("AirPlay · Apple TV") } footer: {
            Text("If the picker only sends sound, open Control Center, tap Screen Mirroring and pick the TV. The piece fills the TV while your phone keeps its controls, and the sound plays there.")
        }
    }
}
