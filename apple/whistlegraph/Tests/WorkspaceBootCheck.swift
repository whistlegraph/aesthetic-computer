import AppKit
import WebKit

// Run the shipped module graph in WebKit under the phone's custom origin.
// No credentials, inference, or persisted user data are used.
@MainActor final class WorkspaceBootCheck: NSObject, WKURLSchemeHandler, WKScriptMessageHandler, WKNavigationDelegate {
    let root: URL
    var view: WKWebView!
    var shellReady = false, snapshotReady = false, accountReady = false
    var expectedHead: Int?
    init(root: URL) { self.root = root; super.init() }
    func start() throws {
        let config = WKWebViewConfiguration()
        config.websiteDataStore = .nonPersistent()
        config.setURLSchemeHandler(self, forURLScheme: "walkieware")
        config.userContentController.add(self, name: "whistlegraph")
        var guides: [String: String] = [:]
        for name in ["pieces.md", "screen.md", "hand.md", "kidlisp.md", "api.json"] {
            guides["/easel/context/" + name] = try String(contentsOf: root.appendingPathComponent("easel/context/" + name), encoding: .utf8)
        }
        let json = String(data: try JSONSerialization.data(withJSONObject: guides), encoding: .utf8)!
        config.userContentController.addUserScript(WKUserScript(source: "window.addEventListener('error',e=>webkit.messageHandlers.whistlegraph.postMessage({action:'startupError',text:e.message}));window.addEventListener('unhandledrejection',e=>webkit.messageHandlers.whistlegraph.postMessage({action:'startupError',text:String(e.reason)}));window.__whistlegraphNativeShell=true;window.__aeselGuides=\(json);", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        if CommandLine.arguments.count > 2 {
            let seed = try Data(contentsOf: URL(fileURLWithPath: CommandLine.arguments[2]))
            let state = try JSONSerialization.jsonObject(with: seed) as! [String: String]
            if let ledger = state["whistlegraph-source-versions"] ?? state["walkieware-source-versions"],
               let data = ledger.data(using: .utf8), let value = try? JSONSerialization.jsonObject(with: data) as? [String: Any] {
                expectedHead = value["head"] as? Int
            }
            let encoded = String(data: try JSONSerialization.data(withJSONObject: state), encoding: .utf8)!
            config.userContentController.addUserScript(WKUserScript(source: "for(const [key,value] of Object.entries(\(encoded)))localStorage.setItem(key,value);", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        view = WKWebView(frame: CGRect(x: 0, y: 0, width: 430, height: 900), configuration: config)
        view.navigationDelegate = self
        view.load(URLRequest(url: URL(string: "walkieware://app/index.html?whistlegraph=1")!))
    }
    func webView(_ webView: WKWebView, start task: WKURLSchemeTask) {
        guard let url = task.request.url, url.host == "app" else { task.didFailWithError(URLError(.badURL)); return }
        let file = root.appendingPathComponent(String(url.path.dropFirst())).standardizedFileURL
        guard file.path.hasPrefix(root.path + "/"), let data = try? Data(contentsOf: file) else {
            print("MISSING", url.path); task.didFailWithError(URLError(.fileDoesNotExist)); return
        }
        let mime = ["html":"text/html", "js":"text/javascript", "mjs":"text/javascript", "json":"application/json", "ttf":"font/ttf"][file.pathExtension] ?? "text/plain"
        task.didReceive(URLResponse(url: url, mimeType: mime, expectedContentLength: data.count, textEncodingName: "utf-8"))
        task.didReceive(data); task.didFinish()
    }
    func webView(_ webView: WKWebView, stop task: WKURLSchemeTask) {}
    func webView(_ webView: WKWebView, decidePolicyFor action: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        decisionHandler(PreviewNavigation.allows(action.request.url, mainFrame: action.targetFrame?.isMainFrame, document: .workspace) ? .allow : .cancel)
    }
    func webView(_ webView: WKWebView, decidePolicyFor response: WKNavigationResponse, decisionHandler: @escaping (WKNavigationResponsePolicy) -> Void) {
        decisionHandler(PreviewNavigation.allows(response.response.url, mainFrame: response.isForMainFrame, document: .workspace) ? .allow : .cancel)
    }
    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        guard let body = message.body as? [String: Any], let action = body["action"] as? String else { return }
        switch action {
        case "startupError": print("FAIL:", body["text"] ?? "startup error"); exit(1)
        case "ready": shellReady = true
        case "snapshot":
            if let expectedHead {
                let snapshot = body["snapshot"] as? [String: Any]
                guard snapshot?["head"] as? Int == expectedHead else { print("FAIL: restored version changed"); exit(1) }
            }
            snapshotReady = true
        case "account": view.evaluateJavaScript("window.whistlegraphEngineEvent({kind:'account',token:''})")
        case "accountState": accountReady = body["status"] as? String == "signedOut"
        default: break
        }
        if shellReady && snapshotReady && accountReady { print("PASS: WebKit loaded the bundled workspace and reached signed-out account entry"); exit(0) }
    }
}

@main struct BootCheck {
    @MainActor static func main() throws {
        let app = NSApplication.shared
        app.setActivationPolicy(.prohibited)
        let root = URL(fileURLWithPath: CommandLine.arguments[1]).standardizedFileURL
        let check = WorkspaceBootCheck(root: root)
        try check.start()
        DispatchQueue.main.asyncAfter(deadline: .now() + 20) { print("FAIL: WebKit startup timed out",check.shellReady,check.snapshotReady,check.accountReady); exit(1) }
        withExtendedLifetime(check) { app.run() }
    }
}
