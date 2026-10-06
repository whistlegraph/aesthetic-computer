import WebKit

// WebKit owns this adapter; the app root owns the session. Teardown cannot
// retain or replace the session, and ordinary SwiftUI updates never reload it.
@MainActor
final class WorkspaceCoordinator: NSObject, WKScriptMessageHandler, WKNavigationDelegate {
    weak var session: WhistlegraphSession?
    init(session: WhistlegraphSession) { self.session = session }
    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        session?.userContentController(controller, didReceive: message)
    }
    func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        decisionHandler(PreviewNavigation.allows(navigationAction.request.url,
            mainFrame: navigationAction.targetFrame?.isMainFrame, document: .workspace) ? .allow : .cancel)
    }
    func webView(_ webView: WKWebView, decidePolicyFor navigationResponse: WKNavigationResponse, decisionHandler: @escaping (WKNavigationResponsePolicy) -> Void) {
        decisionHandler(PreviewNavigation.allows(navigationResponse.response.url,
            mainFrame: navigationResponse.isForMainFrame, document: .workspace) ? .allow : .cancel)
    }
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) { session?.webView(webView, didFinish: navigation) }
    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) { session?.webView(webView, didFailProvisionalNavigation: navigation, withError: error) }
    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) { session?.webView(webView, didFail: navigation, withError: error) }
    func webViewWebContentProcessDidTerminate(_ webView: WKWebView) { session?.webViewWebContentProcessDidTerminate(webView) }
}
