import SwiftUI
import WebKit

/// Live AC runtimes for the cards around the one in view: KidLisp and pieces
/// load aesthetic.computer directly, Whistlegraph pieces load /mime-play,
/// packed pieces their own HTML. The pool loads the next cards ahead, lets
/// each draw its first frame, then pauses it silent, so a swipe lands on a
/// picture that is already there and only has to resume. A runtime stays
/// hidden until that first frame, so no card shows a loading page. Pieces
/// never see a signed-in session; touches reach one only while interacting.
@MainActor final class RuntimePool: NSObject, ObservableObject, WKScriptMessageHandler, WKNavigationDelegate {
    /// Cards whose runtime has drawn; a card shows its web view from then on.
    @Published private(set) var ready: Set<String> = []
    private var views: [String: WKWebView] = [:]
    private var active: String?
    /// Off-screen parking, behind the reel: a web view parked here still
    /// renders, so a piece can paint its first frame before its card shows.
    let parking: UIView = {
        let view = UIView()
        view.isUserInteractionEnabled = false
        view.clipsToBounds = true
        return view
    }()

    /// Keep runtimes for the cards from one behind to two ahead of `index`.
    func prepare(_ posts: [Post], around index: Int) {
        var keep = Set<String>()
        for i in max(0, index - 1)...min(posts.count - 1, index + 2) where index >= 0 && i >= 0 {
            guard let url = posts[i].runtimeURL else { continue }
            keep.insert(posts[i].id)
            if views[posts[i].id] == nil { load(posts[i].id, url) }
        }
        for (id, view) in views where !keep.contains(id) {
            view.stopLoading()
            view.configuration.userContentController.removeScriptMessageHandler(forName: "mime")
            view.removeFromSuperview()
            views[id] = nil
            ready.remove(id)
        }
    }

    /// The card in view plays with sound; every other runtime holds its frame.
    func focus(_ id: String?) {
        active = id
        for (key, view) in views { settle(view, playing: key == id) }
    }

    func view(for id: String) -> WKWebView? { views[id] }

    func park(_ view: WKWebView) {
        guard views.values.contains(view) else { return }
        view.frame = parking.bounds
        view.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        parking.addSubview(view)
    }

    private func load(_ id: String, _ url: URL) {
        let config = WKWebViewConfiguration()
        config.allowsInlineMediaPlayback = true
        config.mediaTypesRequiringUserActionForPlayback = []
        config.websiteDataStore = .nonPersistent()
        config.userContentController.add(WeakHandler(self), name: "mime")
        config.userContentController.addUserScript(WKUserScript(source: Self.firstFrame(id), injectionTime: .atDocumentEnd, forMainFrameOnly: true))
        let view = WKWebView(frame: parking.bounds, configuration: config)
        view.isOpaque = false
        view.backgroundColor = .clear
        view.scrollView.backgroundColor = .clear
        view.scrollView.isScrollEnabled = false
        view.scrollView.contentInsetAdjustmentBehavior = .never
        view.isUserInteractionEnabled = false
        view.navigationDelegate = self
        views[id] = view
        park(view)
        view.load(URLRequest(url: url))
    }

    private func settle(_ view: WKWebView, playing: Bool) {
        view.setAllMediaPlaybackSuspended(!playing, completionHandler: nil)
        view.evaluateJavaScript("window.__mimeSettle?.(\(playing))", completionHandler: nil)
    }

    // The runtime is the page itself (pieces, KidLisp) or the same-origin
    // frame inside /mime-play. Once it has booted and painted a couple of
    // frames it reports ready; then it holds unless its card is in view.
    private static func firstFrame(_ id: String) -> String {
        """
        (() => {
          const runtime = () => { const w = window.acPAUSE ? window : document.querySelector('iframe')?.contentWindow;
            return w?.acPAUSE && w.preloaded && !document.getElementById('note') ? w : null; };
          let playing = false, drawn = false;
          window.__mimeSettle = (play) => {
            playing = play; const w = runtime(); if (!w || !drawn) return;
            if (play) { w.acRESUME(); w.AC?.startAudio?.(); window.postMessage({type: 'ac:start-audio'}, '*'); }
            else w.acPAUSE();
          };
          const started = Date.now();
          const poll = setInterval(() => {
            const w = runtime();
            // A page with no AC loop (or a slow one) still shows after a while; it just cannot hold.
            if (!w && Date.now() - started < 12000) return;
            clearInterval(poll);
            if (!w) { drawn = true; window.webkit.messageHandlers.mime.postMessage(\(String(reflecting: id))); return; }
            w.requestAnimationFrame(() => w.requestAnimationFrame(() => setTimeout(() => {
              drawn = true; window.__mimeSettle(playing);
              window.webkit.messageHandlers.mime.postMessage(\(String(reflecting: id)));
            }, 250)));
          }, 100);
        })();
        """
    }

    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        guard let id = message.body as? String, let view = views[id] else { return }
        ready.insert(id)
        settle(view, playing: id == active)
    }

    // Links inside a piece open in Safari, never inside the feed.
    func webView(_ webView: WKWebView, decidePolicyFor action: WKNavigationAction, decisionHandler: @escaping @MainActor (WKNavigationActionPolicy) -> Void) {
        if action.navigationType == .linkActivated, let url = action.request.url {
            UIApplication.shared.open(url)
            decisionHandler(.cancel); return
        }
        decisionHandler(.allow)
    }
}

/// WKUserContentController retains its handlers; this keeps the pool free.
private final class WeakHandler: NSObject, WKScriptMessageHandler {
    weak var target: WKScriptMessageHandler?
    init(_ target: WKScriptMessageHandler) { self.target = target }
    func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
        target?.userContentController(controller, didReceive: message)
    }
}

/// Where the pool's web views park, behind everything.
struct RuntimeParking: UIViewRepresentable {
    let pool: RuntimePool
    func makeUIView(context: Context) -> UIView { pool.parking }
    func updateUIView(_ view: UIView, context: Context) {}
}

/// A card's slot for its pooled runtime: it borrows the web view while the
/// card is on screen and parks it again when the card goes.
struct PieceRuntime: UIViewRepresentable {
    @ObservedObject var pool: RuntimePool
    let id: String
    let interactive: Bool

    func makeUIView(context: Context) -> UIView {
        let slot = UIView()
        slot.clipsToBounds = true
        return slot
    }

    func updateUIView(_ slot: UIView, context: Context) {
        guard let view = pool.view(for: id) else { return }
        if view.superview !== slot {
            view.frame = slot.bounds
            view.autoresizingMask = [.flexibleWidth, .flexibleHeight]
            slot.addSubview(view)
        }
        view.alpha = pool.ready.contains(id) ? 1 : 0
        view.isUserInteractionEnabled = interactive
    }

    static func dismantleUIView(_ slot: UIView, coordinator: ()) {
        for case let view as WKWebView in slot.subviews {
            view.isUserInteractionEnabled = false
            MainActor.assumeIsolated { (view.navigationDelegate as? RuntimePool)?.park(view) }
        }
    }
}

extension Post {
    /// The page a live card runs, if it runs one.
    var runtimeURL: URL? {
        switch body { case .runtime(let url, _), .packed(let url): return url; default: return nil }
    }
}
