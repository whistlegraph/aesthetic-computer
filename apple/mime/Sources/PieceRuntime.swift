import SwiftUI
import WebKit

/// Live AC runtimes for the cards around the one in view: KidLisp and pieces
/// load aesthetic.computer directly, Whistlegraph pieces load /mime-play,
/// packed pieces their own HTML. The pool loads the next cards ahead, lets
/// each draw its first frame, then pauses it silent, so a swipe lands on a
/// picture that is already there and only has to resume. A runtime stays
/// hidden until that first frame, so no card shows a loading page. Pieces
/// receive only the viewer’s public handle; touches reach the focused runtime immediately.
@MainActor final class RuntimePool: NSObject, ObservableObject, WKScriptMessageHandler, WKNavigationDelegate {
    /// Cards whose runtime has drawn; a card shows its web view from then on.
    @Published private(set) var ready: Set<String> = []
    private var views: [String: WKWebView] = [:]
    private var started: [String: Date] = [:]
    private var active: String?
    private var viewerHandle = ""
    var onInteraction: ((String, Bool) -> Void)?
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

    func setViewer(_ handle: String) {
        viewerHandle = handle
        for view in views.values { shareViewer(view) }
    }

    private func shareViewer(_ view: WKWebView) {
        guard view.url?.host == "aesthetic.computer", view.url?.path == "/mime-play",
              let data = try? JSONSerialization.data(withJSONObject: [viewerHandle]),
              let json = String(data: data, encoding: .utf8) else { return }
        view.evaluateJavaScript("window.__mimeSetViewer?.(\(json)[0])", completionHandler: nil)
    }

    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) { shareViewer(webView) }

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
        let input = RuntimeInputObserver { [weak self] holding in
            guard let self, self.active == id else { return }
            self.onInteraction?(id, holding)
        }
        view.addGestureRecognizer(input)
        views[id] = view
        started[id] = Date()
        park(view)
        #if DEBUG
        if ProcessInfo.processInfo.environment["MIME_INTERACTION_FIXTURE"] == "1", url.host == "example.invalid" {
            view.loadHTMLString("""
            <meta name="viewport" content="width=device-width,initial-scale=1">
            <style>body{margin:0;height:100vh;display:grid;place-items:center;background:#264}button{font:30px system-ui;padding:35px;border:0;border-radius:50%;background:#ffbada}</style>
            <button onclick="this.textContent='Touched '+(++window.taps)">Touch me</button>
            <script>window.taps=0;window.__mimeReady=true;</script>
            """, baseURL: url)
            return
        }
        #endif
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
          window.addEventListener('message', event => {
            if (event.source === window && event.data?.type === 'mime:interaction') {
              window.webkit.messageHandlers.mime.postMessage({id: \(String(reflecting: id)), holding: event.data.phase === 'hold'});
            }
          });
          const runtime = () => { const w = window.acPAUSE ? window : document.querySelector('iframe')?.contentWindow;
            return w?.acPAUSE && w.preloaded && !document.getElementById('note') ? w : null; };
          let playing = false, drawn = false;
          window.__mimeSettle = (play) => {
            playing = play; if (window.__mimePlayerSettle) { window.__mimePlayerSettle(play); return; }
            const w = runtime(); if (!w || !drawn) return;
            if (play) { w.acRESUME(); w.AC?.startAudio?.(); window.postMessage({type: 'ac:start-audio'}, '*'); }
            else w.acPAUSE();
          };
          const started = Date.now();
          const poll = setInterval(() => {
            if (window.__mimeReady) {
              clearInterval(poll); drawn = true; window.__mimeSettle(playing);
              window.webkit.messageHandlers.mime.postMessage(\(String(reflecting: id))); return;
            }
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
        if let input = message.body as? [String: Any], let id = input["id"] as? String, id == active, let holding = input["holding"] as? Bool {
            onInteraction?(id, holding); return
        }
        guard let id = message.body as? String, let view = views[id] else { return }
        if let start = started.removeValue(forKey: id) { print("[mime] runtime \(id) ready in \(Int(Date().timeIntervalSince(start) * 1000)) ms") }
        shareViewer(view)
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

/// Observes every touch (including nested cross-origin players) without
/// canceling or delaying WebKit's own recognizers. A held finger never idles.
private final class RuntimeInputObserver: UIGestureRecognizer, UIGestureRecognizerDelegate {
    private var held = Set<UITouch>()
    private let activity: (Bool) -> Void
    init(activity: @escaping (Bool) -> Void) {
        self.activity = activity
        super.init(target: nil, action: nil)
        delegate = self
        cancelsTouchesInView = false
        delaysTouchesBegan = false
        delaysTouchesEnded = false
    }
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent) {
        held.formUnion(touches); activity(true)
        state = state == .possible ? .began : .changed
    }
    override func touchesMoved(_ touches: Set<UITouch>, with event: UIEvent) { activity(true); state = .changed }
    override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent) {
        held.subtract(touches); activity(!held.isEmpty)
        state = held.isEmpty ? .ended : .changed
    }
    override func touchesCancelled(_ touches: Set<UITouch>, with event: UIEvent) {
        held.subtract(touches); activity(!held.isEmpty)
        state = held.isEmpty ? .cancelled : .changed
    }
    override func reset() { held.removeAll(); super.reset() }
    override func canPrevent(_ preventedGestureRecognizer: UIGestureRecognizer) -> Bool { false }
    override func canBePrevented(by preventingGestureRecognizer: UIGestureRecognizer) -> Bool { false }
    func gestureRecognizer(_ gestureRecognizer: UIGestureRecognizer, shouldRecognizeSimultaneouslyWith otherGestureRecognizer: UIGestureRecognizer) -> Bool { true }
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
