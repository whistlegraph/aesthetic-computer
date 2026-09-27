import SwiftUI
import WebKit

/// Desktop rich replies, packaged locally; the agent host remains separate.
struct AeselNotebook: AeselWebViewRepresentable {
    let session: Session
    var automation: AeselAutomation?
    var paint = Paint.base
    var exclusion: [String: CGFloat] = [:]
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
    func makeWebView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.setURLSchemeHandler(context.coordinator.scheme, forURLScheme: "aesel-bundle")
        config.userContentController.add(context.coordinator, name: "notebook")
        #if os(macOS)
        let view = NotebookWebView(frame: .zero, configuration: config)
        #else
        let view = WKWebView(frame: .zero, configuration: config)
        #endif
        view.navigationDelegate = context.coordinator
        automation?.notebook = view
        ApplePlatform.configureEmbeddedView(view)
        #if os(macOS)
        view.setValue(false, forKey: "drawsBackground")
        #endif
        view.load(URLRequest(url: URL(string: "aesel-bundle://app/easel/phone/notebook.html")!))
        return view
    }
    func updateWebView(_ view: WKWebView, context: Context) {
        ApplePlatform.setAppearance(view, colorScheme: paint.css["colorScheme"] == "light" ? .light : .dark)
        var entries = session.displayedEntries.filter { $0.kind != .edit }.map { entry in
            ["id": entry.id.uuidString, "kind": entry.kind == .you ? "user" : entry.kind == .ac ? "assistant" : entry.kind == .bad ? "error" : "notice", "text": entry.text]
        }
        if let fatal = session.fatal { entries.append(["id": "fatal", "kind": "error", "text": fatal]) }
        let colors: [[Int]] = session.handleColors.compactMap { value in
            guard value.count == 7, let rgb = UInt32(value.dropFirst(), radix: 16) else { return nil }
            return [Int((rgb >> 16) & 255), Int((rgb >> 8) & 255), Int(rgb & 255)]
        }
        let payload: [String: Any] = ["entries": entries, "handle": session.handle, "colors": colors, "theme": paint.css,
                                     "exclusion": exclusion, "busy": session.busy, "activity": session.busy ? session.status : ""]
        if let data = try? JSONSerialization.data(withJSONObject: payload, options: [.sortedKeys]),
           let json = String(data: data, encoding: .utf8) {
            context.coordinator.payload = json
            context.coordinator.openLink = openLink
            let height = $height
            context.coordinator.reportHeight = { value in
                DispatchQueue.main.async { let snapped = ceil(value / 24) * 24; if height.wrappedValue != snapped { height.wrappedValue = snapped } }
            }
            context.coordinator.render(view)
        }
    }
    static func dismantleWebView(_ view: WKWebView, coordinator: Coordinator) {
        view.configuration.userContentController.removeScriptMessageHandler(forName: "notebook")
    }
}
