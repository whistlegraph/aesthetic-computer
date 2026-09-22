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
    let automation = AeselAutomation()
    private var webView: WKWebView!
    private var loginWebView: WKWebView?
    private var signInAttempt: NativeSignIn?
    private var signInGeneration = UUID()
    private var signInExchange: Task<Void, Never>?
    private var loginTimeout: Task<Void, Never>?
    private let bundleHandler = BundleSchemeHandler()
    private let nativeHost = NativeHostConnection()
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
        if let issue = store.issue { session.fatal = issue }

        let controller = WKUserContentController()
        controller.add(self, name: "aesel")
        controller.addScriptMessageHandler(nativeHost, contentWorld: .page, name: "aeselHost")

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
        nativeHost.sessionView = webView
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
    func exportNotebook() { call("void aesel.exportNotebook();") }
    func importNotebook(_ text: String) { call("void aesel.importNotebook(\(quote(text)));") }
    func editSource(_ source: String, threadID: String) { call("void aesel.editSource(\(quote(source)), \(quote(threadID)));") }
    func previewRevision(_ version: Int, threadID: String) { call("void aesel.previewRevision(\(version), \(quote(threadID)));") }
    func restoreRevision(_ version: Int, threadID: String) { call("void aesel.restoreRevision(\(version), \(quote(threadID)));") }
    func setAutoPublish(_ enabled: Bool) { call("void aesel.setAutoPublish(\(enabled ? "true" : "false"));") }
    func stop() { call("void aesel.stop();") }
    func newPiece() { newSession(medium: "piece") }
    func newSession(medium: String) { call("void aesel.newPiece(\(quote(medium)));") }
    func resumeSession(id: String) { call("void aesel.resumeSession(\(quote(id)));") }
    func save() { call("void aesel.save();") }
    func setDraft(_ text: String, threadID: String) {
        guard !threadID.isEmpty else { return }
        call("void aesel.setDraft(\(quote(text)), \(quote(threadID)));")
    }
    func setModel(model: String) { call("void aesel.setModel(\(quote(model))); ") }
    func setModel(id: String) { setModel(model: id) }
    func setProvider(_ id: String) { call("void aesel.setProvider(\(quote(id)));") }
    func refreshProviders() { call("void aesel.refreshProviders();") }
    func resumeHostTurn() { call("void aesel.resumeTurn();") }
    func respondToApproval(id: String, decision: String) {
        call("void aesel.respondToApproval(\(quote(id)), \(quote(decision)));")
    }
    func adopt(token: String) { call("void aesel.adoptToken(\(quote(token)));") }
    func restore() { call("void aesel.restore();") }
    func refreshCredits() { call("void aesel.refreshCredits();") }
    func accessToken() -> String? { store.token() }

    var signInView: WKWebView {
        if let loginWebView { return loginWebView }
        let config = WKWebViewConfiguration()
        let view = WKWebView(frame: .zero, configuration: config)
        view.navigationDelegate = self
        loginWebView = view
        return view
    }

    func signIn() {
        cancelSignIn()
        session.showSignIn = true
        session.signInError = nil
        session.signInLoading = true
        do {
            let attempt = try NativeSignIn()
            signInAttempt = attempt
            armLoginTimeout()
            signInView.load(URLRequest(url: attempt.url))
        } catch {
            session.signInLoading = false
            session.signInError = error.localizedDescription
        }
    }

    func cancelSignIn() {
        signInGeneration = UUID()
        signInAttempt = nil
        signInExchange?.cancel(); signInExchange = nil
        loginTimeout?.cancel()
        loginWebView?.stopLoading()
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
            let type = body["type"] as? String
            if let type, type != "persist" { automation.record("session.\(type)") }
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
                do { try store.write(key: key, value: value) }
                catch { session.fatal = "Your latest changes could not be saved. " + error.localizedDescription }
                return
            }
            if type == "diagnostic" {
                NSLog("[aesel] %@ at %@: %@", body["operation"] as? String ?? "request", body["step"] as? String ?? "response", body["message"] as? String ?? "unknown failure")
            }
            session.receive(body)
        }
    }
}

extension SessionHost: WKNavigationDelegate {
    nonisolated func webView(_ webView: WKWebView, decidePolicyFor action: WKNavigationAction,
                             decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        MainActor.assumeIsolated {
            guard webView === loginWebView else { decisionHandler(.allow); return }
            guard let url = action.request.url else { decisionHandler(.cancel); return }
            guard NativeSignIn.isCallback(url) else {
                decisionHandler(url.scheme == "https" ? .allow : .cancel); return
            }
            decisionHandler(.cancel)
            guard action.targetFrame?.isMainFrame == true, session.showSignIn, signInAttempt?.consumed != true else { return }
            do {
                guard let body = try signInAttempt?.exchangeBody(for: url) else { throw NativeSignIn.failure("Sign-in expired") }
                let generation = signInGeneration
                loginTimeout?.cancel()
                session.signInError = nil
                session.signInLoading = true
                signInExchange = Task { [weak self] in
                    do {
                        let token = try await NativeSignIn.exchange(body)
                        guard let self, !Task.isCancelled, self.signInGeneration == generation, self.session.showSignIn else { return }
                        guard self.loaded else { throw NativeSignIn.failure("The notebook is still loading. Try again.") }
                        _ = try await self.webView.callAsyncJavaScript("return await aesel.adoptToken(token);", arguments: ["token": token], in: nil, contentWorld: .page)
                        guard !Task.isCancelled, self.signInGeneration == generation, self.session.showSignIn else { return }
                        self.session.signInError = nil
                        self.session.signInLoading = false
                        self.session.showSignIn = false
                    } catch {
                        guard let self, !Task.isCancelled, self.signInGeneration == generation else { return }
                        self.session.signInLoading = false
                        self.session.signInError = error.localizedDescription
                    }
                }
            } catch {
                session.signInLoading = false
                session.signInError = error.localizedDescription
            }
        }
    }

    nonisolated func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!) {
        MainActor.assumeIsolated {
            guard webView === loginWebView, session.showSignIn, signInAttempt?.consumed != true else { return }
            session.signInError = nil
            session.signInLoading = true
            armLoginTimeout()
        }
    }

    nonisolated func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        // Deliberately does not mark the host ready; see the `ready` event.
        MainActor.assumeIsolated {
            if webView === loginWebView {
                guard session.showSignIn, signInAttempt?.consumed != true else { return }
                session.signInError = nil
                // The hosted auth page supplies its own sign-in interface.
                session.signInLoading = false
                webView.aeselOpacity = 1
                loginTimeout?.cancel()
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
                if NativeSignIn.ignoresNavigationFailure(error as NSError, callbackAccepted: signInAttempt?.consumed == true, presented: session.showSignIn) { return }
                // Only domain/code enter diagnostics; callback URLs contain secrets.
                automation.record("signin.navigationFailure.\((error as NSError).domain).\((error as NSError).code)")
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
                if NativeSignIn.ignoresNavigationFailure(error as NSError, callbackAccepted: signInAttempt?.consumed == true, presented: session.showSignIn) { return }
                // Only domain/code enter diagnostics; callback URLs contain secrets.
                automation.record("signin.navigationFailure.\((error as NSError).domain).\((error as NSError).code)")
                loginTimeout?.cancel()
                session.signInLoading = false
                session.signInError = error.localizedDescription
            } else { session.fatal = error.localizedDescription }
        }
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
        let types = ["html": "text/html", "mjs": "text/javascript", "js": "text/javascript", "md": "text/plain",
                     "css": "text/css", "woff": "font/woff", "woff2": "font/woff2", "ttf": "font/ttf", "svg": "image/svg+xml"]
        urlSchemeTask.didReceive(URLResponse(url: url, mimeType: types[file.pathExtension] ?? "application/octet-stream", expectedContentLength: data.count, textEncodingName: "utf-8"))
        urlSchemeTask.didReceive(data)
        urlSchemeTask.didFinish()
    }
    func webView(_ webView: WKWebView, stop urlSchemeTask: WKURLSchemeTask) { }
}
