import SwiftUI
import WebKit

/// A live AC runtime for the card in view: KidLisp and pieces load
/// aesthetic.computer directly, Whistlegraph pieces load /mime-play, packed
/// pieces their own HTML. Only the focused card mounts one. Autoplay is
/// permitted and the runtime is asked to start its audio, as the
/// Whistlegraph app's feed does. Touches go to the pager until `interactive`.
struct PieceRuntime: UIViewRepresentable {
    let url: URL
    let interactive: Bool
    var onReady: () -> Void = {}

    final class Coordinator: NSObject, WKNavigationDelegate {
        var onReady: () -> Void = {}
        func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
            Self.startAudio(webView)
            onReady()
        }
        /// AC.startAudio for pieces; /mime-play forwards ac:start-audio to the runtime it hosts.
        static func startAudio(_ webView: WKWebView) {
            webView.evaluateJavaScript("window.AC?.startAudio?.(); window.postMessage({type:'ac:start-audio'}, '*');", completionHandler: nil)
        }
        // Links inside a piece open in Safari, never inside the feed.
        func webView(_ webView: WKWebView, decidePolicyFor action: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
            if action.navigationType == .linkActivated, let url = action.request.url {
                UIApplication.shared.open(url); decisionHandler(.cancel); return
            }
            decisionHandler(.allow)
        }
    }
    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeUIView(context: Context) -> WKWebView {
        let config = WKWebViewConfiguration()
        config.allowsInlineMediaPlayback = true
        config.mediaTypesRequiringUserActionForPlayback = []
        config.websiteDataStore = .nonPersistent() // pieces never see a signed-in session
        let view = WKWebView(frame: .zero, configuration: config)
        view.isOpaque = false
        view.backgroundColor = .black
        view.scrollView.isScrollEnabled = false
        view.scrollView.contentInsetAdjustmentBehavior = .never
        view.navigationDelegate = context.coordinator
        context.coordinator.onReady = onReady
        view.load(URLRequest(url: url))
        return view
    }

    func updateUIView(_ view: WKWebView, context: Context) {
        view.isUserInteractionEnabled = interactive
        if interactive { Coordinator.startAudio(view) }
    }
}
