import SwiftUI
import WebKit

/// One runtime retained while moving between the corner and expanded preview.
@MainActor
final class PiecePreview: NSObject, ObservableObject, WKNavigationDelegate {
    @Published var failure: String?
    @Published var background: Color?
    @Published var hasAudio = false
    @Published var volume: Double = 1 { didSet { updateSource() } }
    private var source = ""
    private var requestedURL: URL?
    private let messages = PreviewMessages()
    lazy var view: WKWebView = makeView()

    private func makeView() -> WKWebView {
        let configuration = WKWebViewConfiguration()
        configuration.websiteDataStore = .nonPersistent()
        messages.owner = self
        configuration.userContentController.add(messages, name: "previewFailure")
        configuration.userContentController.add(messages, name: "previewBackdrop")
        configuration.userContentController.add(messages, name: "previewAudio")
        #if os(iOS)
        configuration.allowsInlineMediaPlayback = true
        #endif
        configuration.mediaTypesRequiringUserActionForPlayback = []
        configuration.userContentController.addUserScript(WKUserScript(source: """
        (() => {
          let ready = false, rendered = '', failed = false, startupTimedOut = false;
          let audioSource = '', lastSound = -Infinity, audioVisible = false;
          setInterval(() => {
            if (audioSource !== window.__aeselSource) {
              audioSource = window.__aeselSource; lastSound = -Infinity; audioVisible = false;
            }
            let waveform = [];
            try { waveform = window.AC?.readOutputWaveform?.(128) || []; } catch { /* Audio can detach while a piece reloads. */ }
            if (waveform.some(value => Math.abs(value) > 0.001)) lastSound = performance.now();
            // Hold through short musical rests; keep mute reachable after muting.
            const active = performance.now() - lastSound < 3000 || (audioVisible && window.__aeselVolume === 0);
            if (active !== audioVisible) {
              audioVisible = active;
              window.webkit.messageHandlers.previewAudio.postMessage({active, source: audioSource || ''});
            }
          }, 200);
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
            if (!ready) {
              startupTimedOut = true;
              reportFailure('The AC runtime did not become ready. Check your internet connection and retry.');
            }
          }, 45000);
          window.__aeselRender = () => {
            window.AC?.setMasterVolume?.(window.__aeselVolume ?? 1);
            const source = window.__aeselSource;
            if (!ready || !window.acSEND || !source || source === rendered) return;
            rendered = source;
            const flags = new URLSearchParams();
            const query = new URLSearchParams(location.search);
            for (const key of ['nogap', 'nolabel', 'autoreload']) flags.set(key, 'true');
            for (const key of ['preview', 'icon']) if (query.has(key)) flags.set(key, query.get(key));
            window.acSEND({type: 'dropped:piece', content: {name: 'aesel-preview', source, search: flags.toString(), isKidLisp: false}});
          };
          const becomeReady = () => {
            if (ready) return;
            ready = true;
            if (startupTimedOut) failed = false;
            window.webkit.messageHandlers.previewFailure.postMessage({ready: true});
            window.__aeselRender();
          };
          window.addEventListener('message', event => {
            if (!ready && event.data?.type === 'ready') {
              becomeReady();
            }
          });
          const poll = setInterval(() => {
            if (window.preloaded && window.acSEND) {
              becomeReady();
              clearInterval(poll);
            }
          }, 250);
          setTimeout(() => clearInterval(poll), 45000);
        })();
        """, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        if let url = Bundle.main.url(forResource: "preview-continuity", withExtension: "js"),
           let script = try? String(contentsOf: url, encoding: .utf8) {
            configuration.userContentController.addUserScript(WKUserScript(source: script, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        let web = WKWebView(frame: .zero, configuration: configuration)
        web.navigationDelegate = self
        ApplePlatform.configureEmbeddedView(web)
        #if os(macOS)
        web.setValue(false, forKey: "drawsBackground")
        #endif
        return web
    }

    func update(url: URL, source: String, scheme: ColorScheme) {
        ApplePlatform.setAppearance(view, colorScheme: scheme)
        if self.source != source { failure = nil; hasAudio = false }
        self.source = source
        if requestedURL != url {
            failure = nil; requestedURL = url
            view.load(URLRequest(url: url, cachePolicy: .reloadIgnoringLocalCacheData))
        } else { updateSource() }
    }
    private func updateSource() {
        guard let data = try? JSONSerialization.data(withJSONObject: [source]),
              let json = String(data: data, encoding: .utf8) else { return }
        view.evaluateJavaScript("window.__aeselVolume = \(volume); window.__aeselSource = \(json)[0]; window.__aeselRender?.();")
    }
    func setBackdrop(_ rgb: [Double]) {
        guard rgb.count == 3, rgb.allSatisfy({ $0.isFinite && (0...255).contains($0) }) else { return }
        background = Color(red: rgb[0] / 255, green: rgb[1] / 255, blue: rgb[2] / 255)
        let color = AeselColor(red: rgb[0] / 255, green: rgb[1] / 255, blue: rgb[2] / 255, alpha: 1)
        view.underPageBackgroundColor = color
        #if os(macOS)
        view.wantsLayer = true
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        view.layer?.backgroundColor = color.cgColor
        CATransaction.commit()
        #else
        view.backgroundColor = color
        view.scrollView.backgroundColor = color
        #endif
    }
    func setAudio(_ active: Bool, source: String) {
        if self.source == source { hasAudio = active }
    }
    func reload() { failure = nil; hasAudio = false; view.reload() }
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) { updateSource() }
    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
        if (error as NSError).code != NSURLErrorCancelled { failure = error.localizedDescription }
    }
    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
        if (error as NSError).code != NSURLErrorCancelled { failure = error.localizedDescription }
    }
    func webViewWebContentProcessDidTerminate(_ webView: WKWebView) {
        failure = "The preview stopped. Reload to resume your saved piece."
    }
}

@MainActor
private final class PreviewMessages: NSObject, WKScriptMessageHandler {
    weak var owner: PiecePreview?
    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        guard message.frameInfo.isMainFrame else { return }
        if message.name == "previewAudio", let audio = message.body as? [String: Any],
           let active = audio["active"] as? Bool, let source = audio["source"] as? String {
            owner?.setAudio(active, source: source)
        } else if message.name == "previewBackdrop", let rgb = message.body as? [Double] {
            owner?.setBackdrop(rgb)
        } else if message.name == "previewFailure", let text = message.body as? String {
            owner?.failure = text
        } else if message.name == "previewFailure", let status = message.body as? [String: Bool], status["ready"] == true,
                  owner?.failure == "The AC runtime did not become ready. Check your internet connection and retry." {
            owner?.failure = nil
        }
    }
}

struct PieceView: View {
    let url: URL
    let source: String
    @ObservedObject var preview: PiecePreview
    @Environment(\.paint) private var paint
    var body: some View {
        ZStack(alignment: .topTrailing) {
            PieceWebView(url: url, source: source, preview: preview)
            if let failure = preview.failure {
                VStack(spacing: 10) {
                    Text(failure).font(Paint.font(14)).multilineTextAlignment(.center)
                    Button("Reload preview") { preview.reload() }
                }.padding().frame(maxWidth: .infinity, maxHeight: .infinity).background(paint.deep)
            }
        }.background(preview.background ?? paint.bg)
    }
}
private struct PieceWebView: AeselWebViewRepresentable {
    let url: URL
    let source: String
    let preview: PiecePreview
    @Environment(\.colorScheme) private var colorScheme
    func makeWebView(context: Context) -> WKWebView { preview.view }
    func updateWebView(_ view: WKWebView, context: Context) {
        preview.update(url: url, source: source, scheme: colorScheme)
    }
}
