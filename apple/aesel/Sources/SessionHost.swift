import Foundation
import WebKit
import Security

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
    private var loginWebView: WKWebView?
    private var loginTimeout: Task<Void, Never>?
    private let bundleHandler = BundleSchemeHandler()
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

        var guides: [String: String] = [:]
        for name in ["pieces.md", "screen.md", "hand.md", "kidlisp.md", "api.json"] {
            let path = "/easel/context/\(name)"
            if let root = Bundle.main.resourceURL,
               let text = try? String(contentsOf: root.appendingPathComponent("Session" + path), encoding: .utf8) {
                guides[path] = text
            }
        }
        if let data = try? JSONSerialization.data(withJSONObject: guides), let json = String(data: data, encoding: .utf8) {
            controller.addUserScript(WKUserScript(source: "globalThis.__aeselGuides = \(json);", injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }

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
        configuration.setURLSchemeHandler(bundleHandler, forURLScheme: "aesel-bundle")
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
    func publish() { call("void aesel.publish();") }
    func stop() { call("void aesel.stop();") }
    func newPiece() { newSession(medium: "piece") }
    func newSession(medium: String) { call("void aesel.newPiece(\(quote(medium)));") }
    func resumeSession(id: String) { call("void aesel.resumeSession(\(quote(id)));") }
    func save() { call("void aesel.save();") }
    func setModel(model: String) { call("void aesel.setModel(\(quote(model))); ") }
    func setModel(id: String) { setModel(model: id) }
    func adopt(token: String) { call("void aesel.adoptToken(\(quote(token)));") }
    func restore() { call("void aesel.restore();") }

    var signInView: WKWebView {
        if let loginWebView { return loginWebView }
        let controller = WKUserContentController()
        controller.add(self, name: "acSignIn")
        controller.addUserScript(WKUserScript(source: """
        if (location.origin === 'https://aesthetic.computer') {
          let delivered = false, requested = false;
          const startLogin = new URLSearchParams(location.search).has("aeselLogin");
          setInterval(async () => {
            if (delivered) return;
            if (startLogin && !requested && window.acLOGIN) {
              requested = true;
              if (!window.auth0Client || !(await window.auth0Client.isAuthenticated())) {
                window.acLOGIN();
                return;
              }
            }
            if (!window.auth0Client) return;
            try {
              if (!(await window.auth0Client.isAuthenticated())) return;
              const token = await window.auth0Client.getTokenSilently();
              if (token) { delivered = true; window.webkit.messageHandlers.acSignIn.postMessage({token}); }
            } catch (_) { }
          }, 1000);
        }
        """, injectionTime: .atDocumentEnd, forMainFrameOnly: true))
        let config = WKWebViewConfiguration()
        config.userContentController = controller
        let view = WKWebView(frame: .zero, configuration: config)
        view.navigationDelegate = self
        view.alpha = 0
        loginWebView = view
        return view
    }

    func signIn() {
        session.showSignIn = true
        session.signInError = nil
        session.signInLoading = true
        signInView.alpha = 0
        armLoginTimeout()
        signInView.load(URLRequest(url: URL(string: "https://aesthetic.computer/?aeselLogin=1")!))
    }

    private func armLoginTimeout() {
        loginTimeout?.cancel()
        loginTimeout = Task { [weak self] in
            try? await Task.sleep(nanoseconds: 45_000_000_000)
            guard !Task.isCancelled, let self, self.session.showSignIn, self.session.signInLoading else { return }
            self.session.signInLoading = false
            self.session.signInError = "AC sign-in did not finish loading. Check your connection and retry."
        }
    }

    func signOut() {
        loginTimeout?.cancel()
        call("void aesel.signOut();")
        store.clearToken()
        session.signedIn = false
        session.handle = ""
        session.showSignIn = false
        loginWebView = nil
        WKWebsiteDataStore.default().removeData(ofTypes: WKWebsiteDataStore.allWebsiteDataTypes(), modifiedSince: .distantPast) { }
    }

    private func call(_ javascript: String) {
        guard loaded else {
            pending.append(javascript)
            return
        }
        webView.evaluateJavaScript(javascript) { _, error in
            if let error { NSLog("[aesel] JavaScript call failed: %@", String(describing: error)) }
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
                NSLog("[aesel] invalid session message")
                return
            }
            if message.name == "acSignIn" {
                guard session.showSignIn, message.webView === loginWebView,
                      message.frameInfo.isMainFrame,
                      message.frameInfo.securityOrigin.protocol == "https",
                      message.frameInfo.securityOrigin.host == "aesthetic.computer",
                      [0, 443].contains(message.frameInfo.securityOrigin.port),
                      let token = body["token"] as? String, !token.isEmpty else { return }
                loginTimeout?.cancel()
                adopt(token: token)
                session.signInLoading = false
                session.showSignIn = false
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
    nonisolated func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!) {
        MainActor.assumeIsolated {
            guard webView === loginWebView else { return }
            session.signInLoading = true
            webView.alpha = 0
            armLoginTimeout()
        }
    }

    nonisolated func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        // Deliberately does not mark the host ready; see the `ready` event.
        MainActor.assumeIsolated {
            if webView === loginWebView {
                let isCallback = webView.url?.host == "aesthetic.computer"
                session.signInLoading = isCallback
                webView.alpha = isCallback ? 0 : 1
                if isCallback { armLoginTimeout() } else { loginTimeout?.cancel() }
            } else { NSLog("[aesel] host page loaded") }
        }
    }

    nonisolated func webView(
        _ webView: WKWebView,
        didFail navigation: WKNavigation!,
        withError error: Error
    ) {
        MainActor.assumeIsolated {
            if webView === loginWebView {
                if (error as NSError).code == NSURLErrorCancelled { return }
                loginTimeout?.cancel()
                session.signInLoading = false
                session.signInError = error.localizedDescription
            } else { session.fatal = error.localizedDescription }
        }
    }

    nonisolated func webView(
        _ webView: WKWebView,
        didFailProvisionalNavigation navigation: WKNavigation!,
        withError error: Error
    ) {
        MainActor.assumeIsolated {
            if webView === loginWebView {
                if (error as NSError).code == NSURLErrorCancelled { return }
                loginTimeout?.cancel()
                session.signInLoading = false
                session.signInError = error.localizedDescription
            } else { session.fatal = error.localizedDescription }
        }
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
            if let text = values["session"], let oldData = text.data(using: .utf8),
               var record = (try? JSONSerialization.jsonObject(with: oldData)) as? [String: Any],
               record.removeValue(forKey: "token") != nil,
               let clean = try? JSONSerialization.data(withJSONObject: record),
               let cleanText = String(data: clean, encoding: .utf8) {
                values["session"] = cleanText
                if let disk = try? JSONEncoder().encode(values) { try? disk.write(to: url, options: .atomic) }
            }
        } else {
            values = [:]
        }
    }

    func write(key: String, value: String) {
        var value = value
        if key == "session", let data = value.data(using: .utf8),
           var record = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any] {
            if let token = record.removeValue(forKey: "token") as? String { saveToken(token) }
            if let clean = try? JSONSerialization.data(withJSONObject: record),
               let text = String(data: clean, encoding: .utf8) { value = text }
        }
        values[key] = value
        guard let data = try? JSONEncoder().encode(values) else { return }
        // Atomic so a crash mid-write cannot leave a session that parses as
        // half a piece.
        try? data.write(to: url, options: .atomic)
    }

    private var tokenQuery: [String: Any] {
        [kSecClass as String: kSecClassGenericPassword,
         kSecAttrService as String: "computer.aesthetic.aesel.session",
         kSecAttrAccount as String: "access-token"]
    }

    func clearToken() { SecItemDelete(tokenQuery as CFDictionary) }

    private func saveToken(_ token: String) {
        clearToken()
        guard !token.isEmpty else { return }
        var query = tokenQuery
        query[kSecValueData as String] = Data(token.utf8)
        query[kSecAttrAccessible as String] = kSecAttrAccessibleWhenUnlockedThisDeviceOnly
        let status = SecItemAdd(query as CFDictionary, nil)
        if status != errSecSuccess { NSLog("[aesel] Keychain save failed (%d); sign in again after relaunch", status) }
    }

    func seedJSON() -> String? {
        var values = values
        // Migrate earlier development sessions out of Documents on next write.
        var record = ((values["session"]?.data(using: .utf8)).flatMap {
            try? JSONSerialization.jsonObject(with: $0) as? [String: Any]
        }) ?? [:]
        var query = tokenQuery
        query[kSecReturnData as String] = true
        var result: CFTypeRef?
        if SecItemCopyMatching(query as CFDictionary, &result) == errSecSuccess,
           let data = result as? Data, let token = String(data: data, encoding: .utf8) {
            record["token"] = token
        }
        if let data = try? JSONSerialization.data(withJSONObject: record),
           let text = String(data: data, encoding: .utf8) { values["session"] = text }
        guard !values.isEmpty,
              let data = try? JSONSerialization.data(withJSONObject: values, options: []),
              let text = String(data: data, encoding: .utf8) else { return nil }
        return text
    }
}


/// Serves only immutable resources shipped with the app; no listening socket.
final class BundleSchemeHandler: NSObject, WKURLSchemeHandler {
    func webView(_ webView: WKWebView, start urlSchemeTask: WKURLSchemeTask) {
        guard let url = urlSchemeTask.request.url,
              let root = Bundle.main.resourceURL?.appendingPathComponent("Session") else { return }
        let file = root.appendingPathComponent(String(url.path.drop(while: { $0 == "/" }))).standardizedFileURL
        guard file.path.hasPrefix(root.standardizedFileURL.path + "/"), let data = try? Data(contentsOf: file) else {
            NSLog("[aesel] Missing bundle resource: %@ root: %@", file.path, root.path)
            urlSchemeTask.didFailWithError(URLError(.fileDoesNotExist)); return
        }
        let types = ["html": "text/html", "mjs": "text/javascript", "js": "text/javascript", "md": "text/plain"]
        urlSchemeTask.didReceive(URLResponse(url: url, mimeType: types[file.pathExtension] ?? "application/octet-stream", expectedContentLength: data.count, textEncodingName: "utf-8"))
        urlSchemeTask.didReceive(data)
        urlSchemeTask.didFinish()
    }
    func webView(_ webView: WKWebView, stop urlSchemeTask: WKURLSchemeTask) { }
}
