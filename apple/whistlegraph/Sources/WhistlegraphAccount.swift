import Foundation
import WebKit
import Security
import UIKit

/// Reuses Aesel's PKCE contract; credentials remain in this app's Keychain.
@MainActor final class WhistlegraphAccount: NSObject, WKNavigationDelegate, UIAdaptivePresentationControllerDelegate {
    private let key: [String: Any] = [kSecClass as String: kSecClassGenericPassword,
        kSecAttrService as String: "computer.aesthetic.walkieware", kSecAttrAccount as String: "ac"]
    private var attempt: NativeSignIn?
    private var completion: ((Result<String, Error>) -> Void)?
    private var controller: UIViewController?
    private var exchange: Task<Void, Never>?

    func token() async throws -> String? {
        var query = key; query[kSecReturnData as String] = true
        var result: CFTypeRef?
        guard SecItemCopyMatching(query as CFDictionary, &result) == errSecSuccess,
              let data = result as? Data else { return nil }
        var tokens = try JSONDecoder().decode(NativeSignIn.Tokens.self, from: data)
        if tokens.expiresAt.timeIntervalSinceNow < 60 {
            guard let refresh = tokens.refreshToken else { return nil }
            tokens = try await NativeSignIn.refresh(refresh)
            try save(tokens)
        }
        return tokens.accessToken
    }
    /// Forgets the stored sign-in. The thread history stays on the device.
    func signOut() {
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
    func signIn(from view: WKWebView?, completion: @escaping (Result<String, Error>) -> Void) {
        guard controller == nil, let host = view?.window?.rootViewController else { return }
        do {
            attempt = try NativeSignIn(); self.completion = completion
            let config = WKWebViewConfiguration(); config.websiteDataStore = .nonPersistent()
            let web = WKWebView(frame: .zero, configuration: config); web.navigationDelegate = self
            let page = UIViewController(); page.view = web; page.title = "Sign in to Aesthetic Computer"
            page.navigationItem.leftBarButtonItem = UIBarButtonItem(barButtonSystemItem: .cancel, target: self, action: #selector(cancel))
            let navigation = UINavigationController(rootViewController: page); controller = navigation
            host.present(navigation, animated: true)
            navigation.presentationController?.delegate = self
            web.load(URLRequest(url: attempt!.url))
        } catch { completion(.failure(error)) }
    }
    @objc private func cancel() { finish(.failure(NativeSignIn.failure("Sign-in cancelled. Your words are still here."))) }
    func presentationControllerDidDismiss(_ presentationController: UIPresentationController) { cancel() }
    private func finish(_ result: Result<String, Error>) {
        exchange?.cancel(); exchange = nil
        let done = completion; completion = nil; attempt = nil
        controller?.dismiss(animated: true); controller = nil; done?(result)
    }
    func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction, decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        guard let url = navigationAction.request.url else { decisionHandler(.cancel); return }
        if NativeSignIn.isCallback(url) {
            decisionHandler(.cancel)
            do {
                guard var attempt = attempt else { return }
                let body = try attempt.exchangeBody(for: url); self.attempt = attempt
                exchange = Task {
                    do {
                        let tokens = try await NativeSignIn.exchange(body)
                        try Task.checkCancellation(); try save(tokens)
                        finish(.success(tokens.accessToken))
                    } catch { if !Task.isCancelled { finish(.failure(error)) } }
                }
            } catch { finish(.failure(error)) }
        } else { decisionHandler(url.scheme == "https" ? .allow : .cancel) }
    }
}

final class WhistlegraphBundle: NSObject, WKURLSchemeHandler {
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
            print("[walkieware] missing bundle resource: \(url.path)")
            task.didFailWithError(URLError(.fileDoesNotExist)); return
        }
        let types = ["html":"text/html", "js":"text/javascript", "mjs":"text/javascript", "json":"application/json", "ttf":"font/ttf", "md":"text/plain"]
        task.didReceive(URLResponse(url: url, mimeType: types[file.pathExtension] ?? "application/octet-stream", expectedContentLength: data.count, textEncodingName: "utf-8"))
        task.didReceive(data); task.didFinish()
    }
    func webView(_ webView: WKWebView, stop task: WKURLSchemeTask) {}
}

struct WhistlegraphPreview {
    static let script = """
    (() => {
      if (window === window.top || location.origin !== 'https://aesthetic.computer') return;
      window.acFORCE_NOGAP = true;
      let ready = false, revision = 0, paintedRevision = 0, sessionID = '';
      const post = body => window.webkit.messageHandlers.walkie.postMessage(body);
      window.walkiewareRender = async (source, threadID) => {
        if (!ready) return;
        sessionID = threadID;
        const current = ++revision;
        const digest = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(source));
        if (current !== revision) return;
        const hash = [...new Uint8Array(digest)].map(b => b.toString(16).padStart(2,'0')).join('');
        window.acSEND({type:'dropped:piece',content:{name:'walkieware-preview',source,
          search:'noauth=true&noplot=true&nogap=true&nolabel=true',isKidLisp:false,
          aeselPreview:{sessionID,revision:current,sourceHash:hash,requestID:current}}});
      };
      window.addEventListener('aesel-preview', e => {
        if (e.detail?.sessionID !== sessionID || e.detail?.revision !== revision) return;
        if (e.detail.kind === 'painted') { if (paintedRevision === revision) return; paintedRevision = revision; }
        post({action:'previewEvent',event:e.detail});
      });
      const poll = setInterval(() => {
        if (!window.preloaded || !window.acSEND) return;
        clearInterval(poll); ready = true; post({action:'previewReady'});
      }, 100);
    })();
    """
}
