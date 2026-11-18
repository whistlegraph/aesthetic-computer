// Login.swift — the NCC sign-in, inside the app.
//
// The club's login page (Authentik) shows in the wiki pane, in a web view.
// The app starts the wiki's own sign-in (/auth/oidc); the wiki sends the member
// to the club's page and takes them back when the club has confirmed who they
// are. The wiki then sets its session token, and that token is what the app
// asks the wiki's API with. So the NCC login is exactly what opens the wiki,
// and the app needs no client of its own at the identity provider.
//
// The password is typed into the club's own page. The app reads one thing
// from the web view: the wiki's session cookie.

import AppKit
import WebKit

final class WikiLogin: NSObject, WKNavigationDelegate {
    let web = WKWebView(frame: .zero, configuration: WKWebViewConfiguration())
    private let base: String
    private var done = false
    var onToken: ((String) -> Void)?

    init(base: String) {
        self.base = base
        super.init()
        web.navigationDelegate = self
        web.translatesAutoresizingMaskIntoConstraints = false
    }

    func start() {
        guard let url = URL(string: base + "/auth/oidc") else { return }
        web.load(URLRequest(url: url))
    }

    // Each time a page settles, look for the wiki's session.
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) { look() }
    func webView(_ webView: WKWebView, didCommit navigation: WKNavigation!) { look() }

    private func look() {
        guard !done, let host = URL(string: base)?.host else { return }
        web.configuration.websiteDataStore.httpCookieStore.getAllCookies { [weak self] cookies in
            guard let self, !self.done,
                  let c = cookies.first(where: { $0.name == "accessToken" && host.hasSuffix($0.domain.trimmingCharacters(in: ["."])) }),
                  !c.value.isEmpty else { return }
            self.done = true
            self.onToken?(c.value)
        }
    }

    // Forget the club's session and the wiki's (sign out).
    static func forget(done: @escaping () -> Void) {
        WKWebsiteDataStore.default().removeData(ofTypes: WKWebsiteDataStore.allWebsiteDataTypes(),
                                                modifiedSince: .distantPast, completionHandler: done)
    }
}
