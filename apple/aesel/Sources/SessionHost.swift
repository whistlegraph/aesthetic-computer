import Foundation
import WebKit

/// Runs the shared JavaScript session in a WKWebView that is never shown.
///
/// The session is `easel/phone/session.mjs`, which builds `AcServer` out of
/// `easel/src` unmodified. Hosting it here rather than porting it to Swift is
/// the point: the agent loop, the twelve-round bound, the tool contract and the
/// guides stay one implementation shared with the desktop, and SwiftUI draws
/// the result.
///
/// The webview is what supplies the web platform the bridge is written
/// against — `fetch`, streaming `response.body.getReader()`, `TextDecoder` and
/// ES modules. JavaScriptCore has none of those.
@MainActor
final class SessionHost: NSObject {
    private var webView: WKWebView!
    private let session: Session
    private let store: SessionStore
    /// Calls that arrive before the page finishes loading. Without this, a
    /// sign-in completing during launch is silently dropped.
    private var pending: [String] = []
    private var loaded = false
    /// `AESEL_ASK`, held until the session reports it has restored.
    private var openingPrompt = ProcessInfo.processInfo.environment["AESEL_ASK"].flatMap {
        $0.isEmpty ? nil : $0
    }

    init(session: Session, store: SessionStore) {
        self.session = session
        self.store = store
        super.init()

        let controller = WKUserContentController()
        controller.add(self, name: "aesel")

        // Seeded before any module runs, so `session.mjs` can restore a piece
        // and a token on its first tick rather than after a round trip.
        if let seed = store.seedJSON() {
            controller.addUserScript(
                WKUserScript(
                    source: "globalThis.__aeselSeed = \(seed);",
                    injectionTime: .atDocumentStart,
                    forMainFrameOnly: true
                )
            )
        }

        let configuration = WKWebViewConfiguration()
        configuration.userContentController = controller
        configuration.defaultWebpagePreferences.allowsContentJavaScript = true

        webView = WKWebView(frame: .zero, configuration: configuration)
        webView.navigationDelegate = self
        webView.isHidden = true
    }

    /// The view has to be in the hierarchy or WebKit may not schedule it. It
    /// stays hidden and zero-sized; nothing is ever drawn from it.
    var attachable: WKWebView { webView }

    func start(hostURL: URL) {
        webView.load(URLRequest(url: hostURL, cachePolicy: .reloadIgnoringLocalCacheData))
    }

    // MARK: - Swift → JS

    // Every call is `void`-ed. These functions are async, and handing
    // WKWebView a Promise earns "JavaScript execution returned a result of an
    // unsupported type" — a harmless error that reads exactly like a real one.
    func ask(_ text: String) { call("void aesel.ask(\(quote(text)));") }
    func stop() { call("void aesel.stop();") }
    func newPiece() { call("void aesel.newPiece();") }
    func adopt(token: String) { call("void aesel.adoptToken(\(quote(token)));") }
    func restore() { call("void aesel.restore();") }

    private func call(_ javascript: String) {
        guard loaded else {
            pending.append(javascript)
            return
        }
        webView.evaluateJavaScript(javascript) { _, error in
            if let error { NSLog("[aesel] %@ failed: %@", javascript, String(describing: error)) }
        }
    }

    /// JSON is the only string literal both languages agree on exactly, so the
    /// argument is encoded rather than escaped by hand.
    private func quote(_ text: String) -> String {
        let data = try? JSONSerialization.data(withJSONObject: [text], options: [])
        guard let data, var encoded = String(data: data, encoding: .utf8) else { return "\"\"" }
        encoded.removeFirst()
        encoded.removeLast()
        return encoded
    }
}

extension SessionHost: WKScriptMessageHandler {
    // WebKit delivers these on the main thread already, so this assumes the
    // isolation it is given rather than hopping through a Task. That is not
    // only tidier: a Task per message could reorder them, and the transcript
    // is a stream of deltas where order is the meaning.
    nonisolated func userContentController(
        _ controller: WKUserContentController,
        didReceive message: WKScriptMessage
    ) {
        MainActor.assumeIsolated {
            guard let body = message.body as? [String: Any] else {
                NSLog("[aesel] message was not a dictionary: \(message.body)")
                return
            }
            let type = body["type"] as? String
            NSLog("[aesel] event \(type ?? "?")")

            // `didFinish` is not readiness. It fires when the document has
            // loaded, which is before the module script has evaluated, so
            // `globalThis.aesel` may not exist yet — calling restore() there
            // raced and threw "Can't find variable: aesel". The page posts
            // `ready` after it has installed that global, so that is the
            // signal, and the queue drains against a page that can answer.
            if type == "ready" {
                loaded = true
                let queued = pending
                pending.removeAll()
                for javascript in queued { call(javascript) }
                restore()
                return
            }

            // The smoke-test prompt waits for a restored session rather than a
            // fixed delay, so it cannot run before there is a piece to write.
            if type == "restored", let opening = openingPrompt {
                openingPrompt = nil
                ask(opening)
            }
            // Persistence is the host's job; everything else is the renderer's.
            if body["type"] as? String == "persist",
               let key = body["key"] as? String,
               let value = body["value"] as? String {
                store.write(key: key, value: value)
                return
            }
            session.receive(body)
        }
    }
}

extension SessionHost: WKNavigationDelegate {
    nonisolated func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        // Deliberately does not mark the host ready; see the `ready` event.
        MainActor.assumeIsolated { NSLog("[aesel] host page loaded") }
    }

    nonisolated func webView(
        _ webView: WKWebView,
        didFail navigation: WKNavigation!,
        withError error: Error
    ) {
        MainActor.assumeIsolated { session.fatal = error.localizedDescription }
    }

    nonisolated func webView(
        _ webView: WKWebView,
        didFailProvisionalNavigation navigation: WKNavigation!,
        withError error: Error
    ) {
        MainActor.assumeIsolated { session.fatal = error.localizedDescription }
    }
}

/// A session on disk. One JSON file in Documents, which is the container iOS
/// actually gives an app — there is no ~/.local/share to keep a piece in.
final class SessionStore {
    private let url: URL
    private var values: [String: String]

    init() {
        let documents = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
        url = documents.appendingPathComponent("session.json")
        if let data = try? Data(contentsOf: url),
           let decoded = try? JSONDecoder().decode([String: String].self, from: data) {
            values = decoded
        } else {
            values = [:]
        }
    }

    func write(key: String, value: String) {
        values[key] = value
        guard let data = try? JSONEncoder().encode(values) else { return }
        // Atomic so a crash mid-write cannot leave a session that parses as
        // half a piece.
        try? data.write(to: url, options: .atomic)
    }

    func seedJSON() -> String? {
        guard !values.isEmpty,
              let data = try? JSONSerialization.data(withJSONObject: values, options: []),
              let text = String(data: data, encoding: .utf8) else { return nil }
        return text
    }
}
