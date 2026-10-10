import SwiftUI
import WebKit

// Story playback owns its runtime so a slide cannot replace generation/review pixels.
@MainActor final class StoryPreview: NSObject, WKScriptMessageHandler, WKNavigationDelegate {
    weak var session: WhistlegraphSession?
    private(set) var view: WKWebView!
    private var frame: WKFrameInfo?
    private var version: Int?
    private var source = ""
    private var request = 0
    private var painted = -1
    /// An off-screen recorder listens here instead of through the session.
    var onPainted: ((Int) -> Void)?
    var onTape: (([String: Any]) -> Void)?
    let standalone: Bool
    init(session: WhistlegraphSession, standalone: Bool = false) {
        self.session = session; self.standalone = standalone
        super.init()
        let config = WKWebViewConfiguration()
        config.websiteDataStore = .nonPersistent()
        config.setURLSchemeHandler(WhistlegraphBundle(), forURLScheme: "walkieware")
        config.mediaTypesRequiringUserActionForPlayback = []
        config.allowsInlineMediaPlayback = true
        config.preferences.inactiveSchedulingPolicy = .none
        config.userContentController.add(self, name: "walkie")
        config.userContentController.addUserScript(WKUserScript(source: "window.__whistlegraphPixelSize = \(WhistlegraphPreview.savedPixelSize);", injectionTime: .atDocumentStart, forMainFrameOnly: false))
        config.userContentController.addUserScript(WKUserScript(source: WhistlegraphPreview.script, injectionTime: .atDocumentStart, forMainFrameOnly: false))
        config.userContentController.addUserScript(WKUserScript(source: StoryTape.script, injectionTime: .atDocumentStart, forMainFrameOnly: false))
        view = WKWebView(frame: .zero, configuration: config)
        view.navigationDelegate = self
        view.isOpaque = false; view.backgroundColor = .clear
        view.scrollView.isScrollEnabled = false
        view.scrollView.contentInsetAdjustmentBehavior = .never
        view.load(URLRequest(url: URL(string: "walkieware://app/story.html")!))
    }
    func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        decisionHandler(PreviewNavigation.allows(navigationAction.request.url,
            mainFrame: navigationAction.targetFrame?.isMainFrame, document: .story) ? .allow : .cancel)
    }
    func webView(_ webView: WKWebView, decidePolicyFor navigationResponse: WKNavigationResponse, decisionHandler: @escaping (WKNavigationResponsePolicy) -> Void) {
        decisionHandler(PreviewNavigation.allows(navigationResponse.response.url,
            mainFrame: navigationResponse.isForMainFrame, document: .story) ? .allow : .cancel)
    }
    func present(version: Int, source: String) {
        self.version = version; self.source = source; request += 1; if !standalone { session?.storyStatus = "Loading story runtime" }; render()
    }
    private func render() {
        guard let frame, version != nil else { return }
        let arguments: [String: Any] = ["source": source, "request": request]
        Task {
            do { _ = try await view.callAsyncJavaScript("window.AC?.setMasterVolume(1); window.whistlegraphRender(source, 'whistlegraph-story', request);", arguments: arguments, in: frame, contentWorld: .page); session?.storyStatus = "Story render sent" }
            catch { if !standalone { session?.storyStatus = error.localizedDescription } }
        }
    }
    func stop() {
        version = nil; request += 1
        guard let frame else { return }
        Task { _ = try? await view.callAsyncJavaScript("window.AC?.setMasterVolume(0);", arguments: [:], in: frame, contentWorld: .page) }
    }
    func tape(_ action: String, arguments: [String: Any]) async throws {
        guard let frame else { throw URLError(.resourceUnavailable) }
        _ = try await view.callAsyncJavaScript("if (!window.whistlegraphStoryTape) throw Error('Canvas tape is unavailable'); await window.whistlegraphStoryTape[action](value ?? id);", arguments: ["action":action,"value":arguments["value"] ?? NSNull(),"id":arguments["id"] ?? NSNull()], in: frame, contentWorld: .page)
    }
    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        guard PreviewNavigation.bridge(message.frameInfo.request.url, mainFrame: message.frameInfo.isMainFrame, document: .story) == .artwork, let body = message.body as? [String: Any] else { return }
        if body["action"] as? String == "previewReady" { frame = message.frameInfo; if !standalone { session?.storyStatus = "Story runtime ready" }; render() }
        if body["action"] as? String == "storyTape" { if let onTape { onTape(body) } else { session?.storyTapeEvent?(body) } }
        if body["action"] as? String == "previewEvent", let event = body["event"] as? [String: Any],
           event["kind"] as? String == "painted", event["requestID"] as? Int == request,
           event["sourceHash"] as? String == VisualCapture.hash(source), painted != request, let version {
            painted = request
            if let onPainted { onPainted(version) }
            else { session?.storyStatus = "Story painted"; session?.narratedVersion = version; session?.narratedFrame += 1 }
        }
    }
}
struct StoryWorkspace: UIViewRepresentable {
    let player: StoryPreview
    func makeUIView(context: Context) -> WKWebView { player.view }
    func updateUIView(_ view: WKWebView, context: Context) {}
}

struct StoryRecorderHost: UIViewRepresentable {
    let view: WKWebView
    func makeUIView(context: Context) -> WKWebView { view }
    func updateUIView(_ view: WKWebView, context: Context) {}
}
