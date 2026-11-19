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

    // `ink`, `field` and `accent` are the app's colours for this theme, as CSS.
    init(base: String, ink: String, field: String, accent: String, onAccent: String) {
        self.base = base
        super.init()
        web.navigationDelegate = self
        web.translatesAutoresizingMaskIntoConstraints = false
        // the form is fitted again whenever its pane changes size
        web.postsFrameChangedNotifications = true
        NotificationCenter.default.addObserver(forName: NSView.frameDidChangeNotification, object: web, queue: .main) { [weak self] _ in self?.fit() }
        web.setValue(false, forKey: "drawsBackground")       // the pane's own colour shows behind the page
        web.configuration.userContentController.addUserScript(WKUserScript(
            source: Self.dress(ink: ink, field: field, accent: accent, onAccent: onAccent),
            injectionTime: .atDocumentEnd, forMainFrameOnly: true))
    }

    // The club's login page, dressed as part of the app. The page is made of
    // nested shadow trees (authentik's web components), which a stylesheet in
    // the document cannot reach, so the same small stylesheet is put into the
    // document and into every shadow tree, as each one appears: no page
    // background, no language picker or footer, a small logo, the app's
    // monospace face, and fields and a button in the app's colours. Nothing
    // of the form's working is touched.
    static func dress(ink: String, field: String, accent: String, onAccent: String) -> String {
        let css = """
        html, body { background: transparent !important; background-image: none !important;
          height: auto !important; min-height: 0 !important; overflow: hidden !important; }
        ak-flow-executor, .ak-v2-c-drawer, .ak-v2-c-drawer__main, .ak-v2-c-drawer__content, .ak-v2-c-drawer__body {
          height: auto !important; min-height: 0 !important; }
        form.pf-c-form > p { display: none !important; }
        .pf-c-login { min-height: 100vh !important; padding: 0 16px !important; background: transparent !important;
          display: flex !important; flex-direction: column !important; justify-content: center !important; box-sizing: border-box !important; }
        ak-locale-select, .pf-c-login__header, .pf-c-login__footer, ak-brand-links, ak-flow-inspector-button,
        .pf-c-brand, img.branding-logo, .pf-c-login__main-header, h1.pf-c-title { display: none !important; }
        p, label, span, a { white-space: nowrap !important; }
        .pf-c-login__main { background: transparent !important; box-shadow: none !important; border: 0 !important;
          margin: 0 auto !important; max-width: 440px !important; min-width: 0 !important; width: 100% !important; flex: 0 0 auto !important; }
        .pf-c-login__main-header { padding: 4px 0 8px !important; }
        img.branding-logo { height: 30px !important; max-height: 30px !important; width: auto !important; }
        .pf-c-form__group.pf-m-action { margin-top: 4px !important; }
        .pf-c-login__main-body, .pf-c-login__main-footer, .pf-c-login__main-footer-band { padding: 4px 0 !important; background: transparent !important; border: 0 !important; }
        h1.pf-c-title { font: 700 14px ui-monospace, "SF Mono", Menlo, monospace !important; margin: 0 !important; color: \(ink) !important; }
        p, label, span, a, legend { font: 12px ui-monospace, "SF Mono", Menlo, monospace !important; color: \(ink) !important; }
        .pf-c-form .pf-c-form__label .pf-c-form__label-text, .pf-c-form .pf-c-form__label, .pf-c-form label span { color: \(ink) !important; }
        .pf-c-input-group, .pf-c-form__group-control { background: transparent !important; border: 0 !important; box-shadow: none !important; }
        .pf-c-form-control::before, .pf-c-form-control::after, .pf-c-input-group::before, .pf-c-input-group::after { border: 0 !important; }
        .pf-c-form { gap: 8px !important; }
        .pf-c-form__group { margin: 0 !important; }
        input.pf-c-form-control { font: 16px ui-monospace, "SF Mono", Menlo, monospace !important; height: auto !important;
          padding: 12px 13px !important; border: 0 !important; border-radius: 7px !important; background: \(field) !important; color: \(ink) !important; }
        input.pf-c-form-control::placeholder { color: \(ink) !important; opacity: 0.5; }
        button.pf-c-button.pf-m-primary { font: 700 16px ui-monospace, "SF Mono", Menlo, monospace !important; padding: 12px !important;
          border-radius: 7px !important; background: \(accent) !important; color: \(onAccent) !important; }
        button.pf-c-button.pf-m-primary::after { border: 0 !important; }
        """
        return """
        (function () {
          if (!/^auth\\./.test(location.host)) return;      // the club's login page only, not the sites it signs in to
          var css = \(String(reflecting: css));
          function dress(root) {
            if (!root || root.__nccd) return;
            root.__nccd = true;
            var style = document.createElement("style");
            style.textContent = css;
            (root.head || root).appendChild(style);
          }
          function walk(node) {
            var all = node.querySelectorAll ? node.querySelectorAll("*") : [];
            for (var i = 0; i < all.length; i++) {
              if (all[i].shadowRoot) { dress(all[i].shadowRoot); walk(all[i].shadowRoot); }
            }
          }
          function pass() { dress(document); walk(document); }
          pass();
          var n = 0;
          var t = setInterval(function () { pass(); if (++n > 120) clearInterval(t); }, 250);   // the page builds itself for a while
        })();
        """
    }

    func start() {
        guard let url = URL(string: base + "/auth/oidc") else { return }
        web.load(URLRequest(url: url))
    }

    // Each time a page settles, look for the wiki's session.
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
        look()
        dump()
        // the page builds itself for a moment after it loads: fit it as it settles
        for wait in [0.4, 1.2, 2.5, 4.5] { DispatchQueue.main.asyncAfter(deadline: .now() + wait) { [weak self] in self?.fit() } }
    }

    // Test aid: NCC_LOGIN_DUMP=<file> writes the login page's structure (its
    // shadow trees too) a few seconds after it loads, to write the styling against.
    private func dump() {
        guard let path = ProcessInfo.processInfo.environment["NCC_LOGIN_DUMP"] else { return }
        let js = #"""
        (function () {
          function walk(node, depth) {
            var out = "";
            var kids = node.children || [];
            for (var i = 0; i < kids.length; i++) {
              var el = kids[i];
              var cls = typeof el.className === "string" && el.className.trim() ? "." + el.className.trim().split(/\s+/).join(".") : "";
              var text = el.children.length === 0 ? " '" + (el.textContent || "").trim().slice(0, 40) + "'" : "";
              var part = el.getAttribute && el.getAttribute("part") ? " part=" + el.getAttribute("part") : "";
              out += "  ".repeat(depth) + el.tagName.toLowerCase() + (el.id ? "#" + el.id : "") + cls + part + text + "\n";
              if (el.shadowRoot) out += "  ".repeat(depth + 1) + "#shadow\n" + walk(el.shadowRoot, depth + 2);
              out += walk(el, depth + 1);
            }
            return out;
          }
          return walk(document.documentElement, 0);
        })()
        """#
        DispatchQueue.main.asyncAfter(deadline: .now() + 4) { [weak self] in
            self?.web.evaluateJavaScript(js) { v, e in
                try? ((v as? String) ?? "error: \(String(describing: e))").write(toFile: path, atomically: true, encoding: .utf8)
            }
        }
    }

    // The club's login page is taller than the pane it shows in. Make the page
    // as small as it takes to show all of it, so there is nothing to scroll
    // (but never so small that it is hard to read). `zoom` is the member's own.
    var zoom: CGFloat = 1 { didSet { fit() } }
    func fit() {
        guard !done, web.bounds.height > 0 else { return }
        // The form's own size at no zoom (its fields and button, not the page round
        // them), and then the zoom at which it fills its pane: larger in a large
        // pane, smaller in a small one, and whole in both.
        let js = """
        (function(){var e=document.querySelector('ak-flow-executor');var st=e&&e.shadowRoot&&e.shadowRoot.querySelector('ak-stage-identification');
        var f=st&&st.shadowRoot&&st.shadowRoot.querySelector('form');return f?[f.scrollHeight+120,f.scrollWidth]:null;})()
        """
        web.evaluateJavaScript(js) { [weak self] v, _ in
            guard let self, let a = v as? [NSNumber], a.count == 2, a[0].doubleValue > 0 else { return }
            let tall = CGFloat(a[0].doubleValue), wide: CGFloat = 440       // the form is drawn 440 wide at no zoom
            let z = max(0.3, min(1.9, min(self.web.bounds.height / tall, self.web.bounds.width / (wide + 40)) * 0.94)) * self.zoom
            if abs(self.web.pageZoom - z) > 0.02 { self.web.pageZoom = z }
        }
    }
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
