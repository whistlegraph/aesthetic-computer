import SwiftUI
import UIKit
import WebKit

@main
struct OskiewarApp: App {
    init() { LaunchPing.send("oskiewar") }

    var body: some Scene {
        WindowGroup { GameControllerSurface().ignoresSafeArea() }
    }
}

struct GameControllerSurface: UIViewControllerRepresentable {
    func makeUIViewController(context: Context) -> GameSafetyController {
        GameSafetyController(rootView: AnyView(GameSurface().ignoresSafeArea()))
    }

    func updateUIViewController(_ controller: GameSafetyController,
                                context: Context) {}
}

final class GameSafetyController: UIHostingController<AnyView> {
    override var prefersHomeIndicatorAutoHidden: Bool { true }
    override var prefersStatusBarHidden: Bool { true }
    override var preferredScreenEdgesDeferringSystemGestures: UIRectEdge { .all }

    override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
        UIApplication.shared.isIdleTimerDisabled = true
        setNeedsUpdateOfHomeIndicatorAutoHidden()
        setNeedsUpdateOfScreenEdgesDeferringSystemGestures()
    }

    override func viewWillDisappear(_ animated: Bool) {
        UIApplication.shared.isIdleTimerDisabled = false
        super.viewWillDisappear(animated)
    }
}

// Pinning the scroll view (contentInsetAdjustmentBehavior = .never) zeroes
// env(safe-area-inset-*) inside WKWebView, so the shell cannot see the home
// indicator or the notch on its own. Report the native insets into the page;
// mac-test.html takes the deeper of env() and this injection.
final class InsetReportingWebView: WKWebView {
    override func safeAreaInsetsDidChange() {
        super.safeAreaInsetsDidChange()
        reportSafeAreaInsets()
    }

    func reportSafeAreaInsets() {
        let insets = safeAreaInsets
        evaluateJavaScript(
            "globalThis.__oskiewarSafeInsets = {top:\(insets.top)," +
            "right:\(insets.right),bottom:\(insets.bottom)," +
            "left:\(insets.left)};" +
            "globalThis.__oskiewarInsetsChanged?.();")
    }
}

struct GameSurface: UIViewRepresentable {
    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeUIView(context: Context) -> WKWebView {
        let configuration = WKWebViewConfiguration()
        configuration.defaultWebpagePreferences.allowsContentJavaScript = true
        #if DEBUG
        if ProcessInfo.processInfo.arguments.contains("--smoke-test") {
            configuration.userContentController.addUserScript(WKUserScript(source: """
                globalThis.__oskiewarSmokeErrors = [];
                addEventListener('error', e => __oskiewarSmokeErrors.push(e.message));
                addEventListener('unhandledrejection', e => __oskiewarSmokeErrors.push(String(e.reason)));
                """, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        }
        #endif
        configuration.allowsInlineMediaPlayback = true
        configuration.mediaTypesRequiringUserActionForPlayback = []
        configuration.setURLSchemeHandler(context.coordinator.handler,
                                          forURLScheme: "oskiewar")
        let view = InsetReportingWebView(frame: .zero,
                                         configuration: configuration)
        view.isMultipleTouchEnabled = true
        view.isOpaque = false
        view.backgroundColor = UIColor(red: 7/255, green: 8/255, blue: 28/255, alpha: 1)
        view.scrollView.isScrollEnabled = false
        view.scrollView.contentInsetAdjustmentBehavior = .never
        view.navigationDelegate = context.coordinator
        context.coordinator.webView = view
        let debugTap = UITapGestureRecognizer(target: context.coordinator,
                                              action: #selector(Coordinator.toggleDebug))
        debugTap.numberOfTouchesRequired = 5
        debugTap.numberOfTapsRequired = 1
        debugTap.cancelsTouchesInView = false
        view.addGestureRecognizer(debugTap)
        context.coordinator.loadProduction(in: view)
        return view
    }

    func updateUIView(_ view: WKWebView, context: Context) {}

    final class Coordinator: NSObject, WKNavigationDelegate {
        let handler = BundleSchemeHandler()
        weak var webView: WKWebView?
        private var usingFallback = false

        func loadProduction(in webView: WKWebView) {
            #if DEBUG
            if ProcessInfo.processInfo.arguments.contains("--offline") {
                loadFallback(in: webView, error: NSError(domain: "offline-test", code: 0))
                return
            }
            #endif
            let url = URL(string: "https://oskiewar.com/?touch&app=ios")!
            webView.load(URLRequest(url: url,
                                    cachePolicy: .reloadIgnoringLocalCacheData))
        }

        private func loadFallback(in webView: WKWebView, error: Error) {
            guard !usingFallback else { return }
            usingFallback = true
            print("oskiewar production unavailable; using bundled game: \(error)")
            webView.load(URLRequest(url: URL(string:
                "oskiewar://app/mac-test.html?touch&app=ios&offline")!))
        }

        @objc func toggleDebug() {
            webView?.evaluateJavaScript("globalThis.__oskiewarToggleDebug?.()")
        }

        func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) {
            webView.evaluateJavaScript("document.documentElement.style.webkitUserSelect='none';document.documentElement.style.webkitTouchCallout='none';")
            // A fresh page load starts with no injected insets; re-report so
            // the shell lays out clear of the indicator from its first frame.
            (webView as? InsetReportingWebView)?.reportSafeAreaInsets()
            #if DEBUG
            if ProcessInfo.processInfo.arguments.contains("--smoke-test") {
                DispatchQueue.main.asyncAfter(deadline: .now() + 12) { [weak webView] in
                    webView?.evaluateJavaScript("""
                        JSON.stringify({url:location.href,screen:globalThis.__oskiewarTouch?.screen,
                          release:globalThis.__oskiewarRelease,errors:globalThis.__oskiewarSmokeErrors})
                        """) { value, error in
                        let result = (value as? String) ?? "{\"error\":\"JavaScript unavailable\"}"
                        let output = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
                            .appendingPathComponent("runtime-smoke.json")
                        try? result.write(to: output, atomically: true, encoding: .utf8)
                        print("oskiewar runtime smoke: \(result)")
                    }
                }
            }
            #endif
        }

        func webView(_ webView: WKWebView, didFail navigation: WKNavigation!,
                     withError error: Error) { loadFallback(in: webView, error: error) }

        func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!,
                     withError error: Error) { loadFallback(in: webView, error: error) }
    }
}

final class BundleSchemeHandler: NSObject, WKURLSchemeHandler {
    func webView(_ webView: WKWebView, start task: WKURLSchemeTask) {
        guard let url = task.request.url,
              let root = Bundle.main.resourceURL?.appendingPathComponent("Runtime") else {
            respond(task, url: task.request.url, status: 404,
                    type: "text/plain", data: Data("not found".utf8))
            return
        }
        let path = url.path == "/" ? "mac-test.html" : String(url.path.dropFirst())
        let file = root.appendingPathComponent(path).standardizedFileURL
        guard file.path.hasPrefix(root.standardizedFileURL.path + "/"),
              let data = try? Data(contentsOf: file) else {
            respond(task, url: task.request.url, status: 404,
                    type: "text/plain", data: Data("not found".utf8))
            return
        }
        let type: String
        switch file.pathExtension {
        case "html": type = "text/html"
        case "js", "mjs": type = "text/javascript"
        case "ttf": type = "font/ttf"
        case "woff2": type = "font/woff2"
        case "json": type = "application/json"
        case "png": type = "image/png"
        case "svg": type = "image/svg+xml"
        default: type = "application/octet-stream"
        }
        respond(task, url: url, status: 200, type: type, data: data)
    }

    func webView(_ webView: WKWebView, stop task: WKURLSchemeTask) {}

    private func respond(_ task: WKURLSchemeTask, url: URL?, status: Int,
                         type: String, data: Data) {
        let response = HTTPURLResponse(url: url ?? URL(string: "oskiewar://app/")!,
            statusCode: status, httpVersion: "HTTP/1.1",
            headerFields: ["Content-Type": type, "Content-Length": "\(data.count)",
                           "Cache-Control": "no-store"])!
        task.didReceive(response)
        task.didReceive(data)
        task.didFinish()
    }
}
