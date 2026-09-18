import SwiftUI
import WebKit

/// Desktop rich replies, packaged locally; the agent host remains separate.
struct AeselNotebook: UIViewRepresentable {
    let session: Session
    var paint = Paint.base
    @Binding var height: CGFloat
    var openLink: (URL) -> Void

    final class Coordinator: NSObject, WKNavigationDelegate, WKScriptMessageHandler {
        let scheme = BundleSchemeHandler()
        var ready = false
        var payload = "{}"
        var lastPayload = ""
        var openLink: (URL) -> Void
        var reportHeight: (CGFloat) -> Void = { _ in }
        init(openLink: @escaping (URL) -> Void) { self.openLink = openLink }
        func render(_ view: WKWebView) {
            guard ready, payload != lastPayload else { return }
            lastPayload = payload
            view.evaluateJavaScript("window.updatePhoneNotebook(\(payload));", completionHandler: nil)
        }
        func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
            ready = true
            lastPayload = ""
            render(webView)
        }
        func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
            guard message.frameInfo.isMainFrame,
                  message.webView?.url?.scheme == "aesel-bundle",
                  let body = message.body as? [String: Any] else { return }
            if let height = body["height"] as? Double, height.isFinite, height >= 0, height < 200_000 {
                reportHeight(CGFloat(height))
                return
            }
            guard let text = body["url"] as? String,
                  let url = URL(string: text), ["http", "https"].contains(url.scheme?.lowercased() ?? "") else { return }
            openLink(url)
        }
        func webView(_ webView: WKWebView, decidePolicyFor action: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
            if action.request.url?.scheme == "aesel-bundle" { decisionHandler(.allow) }
            else { decisionHandler(.cancel) }
        }
    }

    func makeCoordinator() -> Coordinator { Coordinator(openLink: openLink) }
    func makeUIView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.setURLSchemeHandler(context.coordinator.scheme, forURLScheme: "aesel-bundle")
        config.userContentController.add(context.coordinator, name: "notebook")
        let view = WKWebView(frame: .zero, configuration: config)
        view.navigationDelegate = context.coordinator
        view.isOpaque = false
        view.backgroundColor = .clear
        view.scrollView.backgroundColor = .clear
        view.scrollView.isScrollEnabled = false
        view.scrollView.contentInsetAdjustmentBehavior = .never
        view.load(URLRequest(url: URL(string: "aesel-bundle://app/easel/phone/notebook.html")!))
        return view
    }
    func updateUIView(_ view: WKWebView, context: Context) {
        var entries = session.entries.filter { $0.kind != .edit }.map { entry in
            ["id": entry.id.uuidString, "kind": entry.kind == .you ? "user" : entry.kind == .ac ? "assistant" : entry.kind == .bad ? "error" : "notice", "text": entry.text]
        }
        if let fatal = session.fatal { entries.append(["id": "fatal", "kind": "error", "text": fatal]) }
        if entries.isEmpty {
            entries.append(["id": "welcome", "kind": "notice", "text": session.signedIn ? "what shall we make?" : "sign in to make something."])
        }
        let colors: [[Int]] = session.handleColors.compactMap { value in
            guard value.count == 7, let rgb = UInt32(value.dropFirst(), radix: 16) else { return nil }
            return [Int((rgb >> 16) & 255), Int((rgb >> 8) & 255), Int(rgb & 255)]
        }
        let payload: [String: Any] = ["entries": entries, "handle": session.handle, "colors": colors, "theme": paint.css,
                                     "busy": session.busy, "activity": session.busy ? session.status : ""]
        if let data = try? JSONSerialization.data(withJSONObject: payload, options: [.sortedKeys]),
           let json = String(data: data, encoding: .utf8) {
            context.coordinator.payload = json
            context.coordinator.openLink = openLink
            let height = $height
            context.coordinator.reportHeight = { value in
                DispatchQueue.main.async { if height.wrappedValue != value { height.wrappedValue = value } }
            }
            context.coordinator.render(view)
        }
    }
    static func dismantleUIView(_ view: WKWebView, coordinator: Coordinator) {
        view.configuration.userContentController.removeScriptMessageHandler(forName: "notebook")
    }
}
