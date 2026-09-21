import SwiftUI
import WebKit

/// Preview the current draft through AC's existing dropped-JavaScript path.
/// It does not depend on a successful public upload or a cached handle route.
struct PieceView: View {
    let url: URL
    let source: String
    @State private var failure: String?
    @State private var attempt = 0
    @Environment(\.paint) private var paint
    var body: some View {
        ZStack {
            PieceWebView(url: url, source: source, failure: $failure).id(attempt)
                .onChange(of: url) { _, _ in failure = nil }
                .onChange(of: source) { _, _ in failure = nil }
            if let failure {
                VStack(spacing: 10) {
                    Text("Preview could not load").font(Paint.font(20))
                    Text(failure).font(Paint.font(16)).multilineTextAlignment(.center)
                    Button("/retry") { self.failure = nil; attempt += 1 }
                        .font(Paint.font(18)).foregroundStyle(paint.you).buttonStyle(AeselButtonStyle())
                }.padding().frame(maxWidth: .infinity, maxHeight: .infinity)
                    .background(paint.deep)
            }
        }
    }
}

private struct PieceWebView: AeselWebViewRepresentable {
    let url: URL
    let source: String
    @Environment(\.colorScheme) private var colorScheme

    @Binding var failure: String?
    final class Coordinator: NSObject, WKNavigationDelegate, WKScriptMessageHandler {
        var failure: Binding<String?>
        init(failure: Binding<String?>) { self.failure = failure }
        func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage) {
            guard message.frameInfo.isMainFrame, let text = message.body as? String else { return }
            failure.wrappedValue = text
        }
        func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
            if (error as NSError).code != NSURLErrorCancelled { failure.wrappedValue = error.localizedDescription }
        }
        func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
            if (error as NSError).code != NSURLErrorCancelled { failure.wrappedValue = error.localizedDescription }
        }
        var source = ""
        var requestedURL: URL?
        func update(_ view: WKWebView) {
            guard let data = try? JSONSerialization.data(withJSONObject: [source]),
                  let json = String(data: data, encoding: .utf8) else { return }
            view.evaluateJavaScript("window.__aeselSource = \(json)[0]; window.__aeselRender?.();") { [weak self] _, error in
                if let error { self?.failure.wrappedValue = error.localizedDescription }
            }
        }
        func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
            update(webView)
        }
    }

    func makeCoordinator() -> Coordinator { Coordinator(failure: $failure) }

    func makeWebView(context: Context) -> WKWebView {
        let configuration = WKWebViewConfiguration()
        configuration.userContentController.add(context.coordinator, name: "previewFailure")
        #if os(iOS)
        configuration.allowsInlineMediaPlayback = true
        #endif
        configuration.mediaTypesRequiringUserActionForPlayback = []
        configuration.userContentController.addUserScript(WKUserScript(source: """
        (() => {
          let ready = false, rendered = '', failed = false;
          const reportFailure = message => {
            if (failed) return;
            failed = true;
            window.webkit.messageHandlers.previewFailure.postMessage(String(message).slice(0, 500));
          };
          window.addEventListener('error', event => {
            if (event.message) reportFailure(event.message);
          });
          window.addEventListener('unhandledrejection', event => {
            reportFailure(event.reason?.message || 'The preview encountered an unexpected error.');
          });
          setTimeout(() => {
            if (!ready) reportFailure('The AC runtime did not become ready. Check your internet connection and retry.');
          }, 45000);
          window.__aeselRender = () => {
            const source = window.__aeselSource;
            if (!ready || !window.acSEND || !source || source === rendered) return;
            rendered = source;
            window.acSEND({type: 'dropped:piece', content: {name: 'aesel-preview', source, isKidLisp: false}});
          };
          window.addEventListener('message', event => {
            if (!ready && event.data?.type === 'ready') {
              ready = true;
              window.__aeselRender();
            }
          });
          const poll = setInterval(() => {
            if (window.preloaded && window.acSEND) {
              ready = true;
              window.__aeselRender();
              clearInterval(poll);
            }
          }, 250);
          setTimeout(() => clearInterval(poll), 45000);
        })();
        """, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        let view = WKWebView(frame: .zero, configuration: configuration)
        view.navigationDelegate = context.coordinator
        ApplePlatform.configureEmbeddedView(view)
        return view
    }

    static func dismantleWebView(_ view: WKWebView, coordinator: Coordinator) {
        view.configuration.userContentController.removeScriptMessageHandler(forName: "previewFailure")
    }

    func updateWebView(_ view: WKWebView, context: Context) {
        ApplePlatform.setAppearance(view, colorScheme: colorScheme)
        context.coordinator.source = url.path.hasPrefix("/@") ? "" : source
        if context.coordinator.requestedURL != url {
            context.coordinator.requestedURL = url
            view.load(URLRequest(url: url, cachePolicy: .reloadIgnoringLocalCacheData))
        } else { context.coordinator.update(view) }
    }
}
