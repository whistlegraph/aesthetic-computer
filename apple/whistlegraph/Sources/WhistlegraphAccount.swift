import Foundation
import WebKit
import Security
import UIKit

/// Reuses Aesel's PKCE contract; credentials remain in this app's Keychain.
@MainActor final class WhistlegraphAccount: NSObject, WKNavigationDelegate, UIAdaptivePresentationControllerDelegate {
    private let key: [String: Any] = [kSecClass as String: kSecClassGenericPassword,
        kSecAttrService as String: "computer.aesthetic.walkieware", kSecAttrAccount as String: "ac"]
    private(set) var generation = 0
    private var attempt: NativeSignIn?
    private var completion: ((Result<String, Error>) -> Void)?
    private var controller: UIViewController?
    private var exchange: Task<Void, Never>?
    private var navigationTimeout: Task<Void, Never>?
    private let identity = VerifiedAccountIdentity()

    func token() async throws -> String? {
        let expectedGeneration = generation
        var query = key; query[kSecReturnData as String] = true
        var result: CFTypeRef?
        guard SecItemCopyMatching(query as CFDictionary, &result) == errSecSuccess,
              let data = result as? Data else { DeviceActionLog.shared.record(.accountToken, .notSignedIn); return nil }
        var tokens = try JSONDecoder().decode(NativeSignIn.Tokens.self, from: data)
        if tokens.expiresAt.timeIntervalSinceNow < 60 {
            DeviceActionLog.shared.record(.accountToken, .started)
            guard let refresh = tokens.refreshToken else { return nil }
            tokens = try await NativeSignIn.refresh(refresh)
            guard generation == expectedGeneration else { return nil }
            try save(tokens)
            DeviceActionLog.shared.record(.accountToken, .succeeded)
        }
        return tokens.accessToken
    }
    // The server still authenticates every request. This subject only scopes
    // local preferences to the account instead of a mutable public handle.
    func subject() async throws -> String? {
        let expected = generation
        guard let token = try await token() else { return nil }
        guard generation == expected else { throw VerifiedAccountIdentity.Failure.changed }
        let subject = try await identity.subject(token: token, generation: expected)
        guard generation == expected else { throw VerifiedAccountIdentity.Failure.changed }
        return subject
    }
    /// Forgets the stored sign-in. The thread history stays on the device.
    func signOut() {
        DeviceActionLog.shared.record(.signOut, .succeeded)
        generation += 1
        identity.invalidate()
        SecItemDelete(key as CFDictionary)
    }
    private func save(_ tokens: NativeSignIn.Tokens) throws {
        let data = try JSONEncoder().encode(tokens)
        let update = [kSecValueData as String: data]
        var status = SecItemUpdate(key as CFDictionary, update as CFDictionary)
        if status == errSecItemNotFound {
            var item = key; item[kSecValueData as String] = data
            item[kSecAttrAccessible as String] = kSecAttrAccessibleWhenUnlockedThisDeviceOnly
            status = SecItemAdd(item as CFDictionary, nil)
        }
        guard status == errSecSuccess else { throw NativeSignIn.failure("Could not save sign-in securely.") }
    }
    func signIn(from view: WKWebView?, signUp: Bool = false, completion: @escaping (Result<String, Error>) -> Void) {
        DeviceActionLog.shared.record(.signIn, .requested)
        guard controller == nil, let host = view?.window?.rootViewController else {
            completion(.failure(NativeSignIn.failure("Sign-in could not open. Close any open sheet and try again."))); return
        }
        do {
            attempt = try NativeSignIn(signUp: signUp); self.completion = completion
            let config = WKWebViewConfiguration(); config.websiteDataStore = .nonPersistent()
            let web = WKWebView(frame: .zero, configuration: config); web.navigationDelegate = self
            let page = UIViewController(); page.view = web; page.title = signUp ? "Join Aesthetic Computer" : "Log in to Aesthetic Computer"
            page.navigationItem.leftBarButtonItem = UIBarButtonItem(barButtonSystemItem: .cancel, target: self, action: #selector(cancel))
            let navigation = UINavigationController(rootViewController: page); controller = navigation
            ActionTouchProbe.TouchObserver.authenticationPresented = true
            host.present(navigation, animated: true)
            navigation.presentationController?.delegate = self
            web.load(URLRequest(url: attempt!.url))
            armNavigationTimeout()
        } catch { completion(.failure(error)) }
    }
    @objc private func cancel() { finish(.failure(NativeSignIn.failure("Sign-in cancelled. Your words are still here."))) }
    func presentationControllerDidDismiss(_ presentationController: UIPresentationController) { cancel() }
    private func finish(_ result: Result<String, Error>) {
        ActionTouchProbe.TouchObserver.authenticationPresented = false
        switch result {
        case .success: DeviceActionLog.shared.record(.signIn, .succeeded)
        case .failure(let error): DeviceActionLog.shared.recordError(.signIn, error)
        }
        navigationTimeout?.cancel(); navigationTimeout = nil
        exchange?.cancel(); exchange = nil
        let done = completion; completion = nil; attempt = nil
        controller?.dismiss(animated: true); controller = nil; done?(result)
    }
    func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        guard let url = navigationAction.request.url else { decisionHandler(.cancel); return }
        if NativeSignIn.isCallback(url) {
            decisionHandler(.cancel)
            do {
                guard navigationAction.targetFrame?.isMainFrame == true, var attempt = attempt, !attempt.consumed else { return }
                navigationTimeout?.cancel()
                let body = try attempt.exchangeBody(for: url); self.attempt = attempt
                exchange = Task {
                    do {
                        let tokens = try await NativeSignIn.exchange(body)
                        try Task.checkCancellation(); generation += 1; identity.invalidate(); try save(tokens)
                        finish(.success(tokens.accessToken))
                    } catch { if !Task.isCancelled { finish(.failure(error)) } }
                }
            } catch { finish(.failure(error)) }
        } else { decisionHandler(url.scheme == "https" ? .allow : .cancel) }
    }
    private func armNavigationTimeout() {
        navigationTimeout?.cancel()
        navigationTimeout = Task { [weak self] in
            try? await Task.sleep(for: .seconds(30))
            guard !Task.isCancelled, let self, self.controller != nil, self.attempt?.consumed != true else { return }
            self.finish(.failure(NativeSignIn.failure("AC sign-in did not finish loading. Check your connection and try again.")))
        }
    }
    func webView(_ webView: WKWebView, didStartProvisionalNavigation navigation: WKNavigation!) {
        if attempt?.consumed != true { armNavigationTimeout() }
    }
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) { navigationTimeout?.cancel() }
    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) { navigationFailed(error) }
    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) { navigationFailed(error) }
    private func navigationFailed(_ error: Error) {
        guard !NativeSignIn.ignoresNavigationFailure(error as NSError, callbackAccepted: attempt?.consumed == true, presented: controller != nil) else { return }
        finish(.failure(NativeSignIn.failure("Could not load AC sign-in. Check your connection and try again.")))
    }

}

final class WhistlegraphBundle: NSObject, WKURLSchemeHandler {
    // Keep the installed WKWebView origin: changing it hides all local work.
    static let storageScheme = "walkieware"
    func webView(_ webView: WKWebView, start task: WKURLSchemeTask) {
        guard let url = task.request.url, url.host == "app",
              let root = Bundle.main.url(forResource: "Web", withExtension: nil) else {
            task.didFailWithError(URLError(.badURL)); return
        }
        // iPhone bundle URLs may use /var while standardization produces
        // /private/var. Canonicalize BOTH sides before the containment check.
        let canonicalRoot = root.resolvingSymlinksInPath().standardizedFileURL
        let file = canonicalRoot.appendingPathComponent(String(url.path.dropFirst())).resolvingSymlinksInPath().standardizedFileURL
        guard file.path.hasPrefix(canonicalRoot.path + "/"), let data = try? Data(contentsOf: file) else {
            print("[whistlegraph] missing bundle resource: \(url.path)")
            task.didFailWithError(URLError(.fileDoesNotExist)); return
        }
        let types = ["html":"text/html", "js":"text/javascript", "mjs":"text/javascript", "json":"application/json", "ttf":"font/ttf", "md":"text/plain"]
        task.didReceive(URLResponse(url: url, mimeType: types[file.pathExtension] ?? "application/octet-stream", expectedContentLength: data.count, textEncodingName: "utf-8"))
        task.didReceive(data); task.didFinish()
    }
    func webView(_ webView: WKWebView, stop task: WKURLSchemeTask) {}
}

struct WhistlegraphPreview {
    static let pixelSizeKey = "walkieware-pixel-size"
    // bios.mjs defaults to two CSS points per framebuffer pixel on iPhone.
    static var savedPixelSize: Int {
        let saved = UserDefaults.standard.integer(forKey: pixelSizeKey)
        return (1...4).contains(saved) ? saved : 2
    }
    static let script = """
    (() => {
      if (window === window.top || location.origin !== 'https://aesthetic.computer') return;
      window.acFORCE_NOGAP = true;
      window.whistlegraphSetPixelSize = size => {
        if (!Number.isInteger(size) || size < 1 || size > 4) return;
        window.__whistlegraphPixelSize = size;
        window.acPACK_DENSITY = size;
        window.acAutoDensityOverride = true;
        try { localStorage.setItem('ac-density', String(size)); } catch {}
        window.postMessage({type:'ac-density-change', density:size}, location.origin);
      };
      window.whistlegraphSetPixelSize(window.__whistlegraphPixelSize ?? 2);
      let ready = false, revision = 0, paintedRevision = 0, sessionID = '';
      const post = body => window.webkit.messageHandlers.whistlegraph.postMessage(body);
      if (window.__whistlegraphAudioTest) {
        let gestures = 0, checks = 0;
        window.addEventListener('pointerdown', () => gestures++);
        const probe = setInterval(() => {
          const waveform = window.AC?.readOutputWaveform?.() || [];
          const peak = waveform.reduce((value,sample) => Math.max(value,Math.abs(sample)),0);
          post({action:'audioProbe',peak,gestures,state:window.AC?.startAudio?.().state || 'unavailable',ready:!!window.audioWorkletReady});
          if (peak > 0.001 || ++checks > 240) clearInterval(probe);
        },250);
      }
      window.whistlegraphRender = async (source, threadID, renderID) => {
        if (!ready) return;
        window.AC?.startAudio?.();
        sessionID = threadID;
        const current = ++revision;
        const digest = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(source));
        if (current !== revision) return;
        const hash = [...new Uint8Array(digest)].map(b => b.toString(16).padStart(2,'0')).join('');
        window.acSEND({type:'dropped:piece',content:{name:'whistlegraph-preview',source,
          search:'noauth=true&noplot=true&nogap=true&nolabel=true',isKidLisp:false,
          aeselPreview:{sessionID,revision:current,sourceHash:hash,requestID:Number.isSafeInteger(renderID)?renderID:current}}});
      };
      window.addEventListener('aesel-preview', e => {
        if (e.detail?.sessionID !== sessionID || e.detail?.revision !== revision) return;
        if (e.detail.kind === 'painted') { if (paintedRevision === revision) return; paintedRevision = revision; }
        post({action:'previewEvent',event:e.detail});
      });
      const poll = setInterval(() => {
        if (!window.preloaded || !window.acSEND) return;
        clearInterval(poll); ready = true; window.AC?.startAudio?.(); post({action:'previewReady'});
      }, 100);
    })();
    """
}
