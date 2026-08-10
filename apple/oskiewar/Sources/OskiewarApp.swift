import SwiftUI
import UIKit
import WebKit

@main
struct OskiewarApp: App {
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

struct GameSurface: UIViewRepresentable {
    func makeCoordinator() -> Coordinator { Coordinator() }

    func makeUIView(context: Context) -> WKWebView {
        let configuration = WKWebViewConfiguration()
        configuration.defaultWebpagePreferences.allowsContentJavaScript = true
        configuration.allowsInlineMediaPlayback = true
        configuration.mediaTypesRequiringUserActionForPlayback = []
        configuration.setURLSchemeHandler(context.coordinator.handler,
                                          forURLScheme: "oskiewar")
        let view = WKWebView(frame: .zero, configuration: configuration)
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
        }

        func webView(_ webView: WKWebView, didFail navigation: WKNavigation!,
                     withError error: Error) { loadFallback(in: webView, error: error) }

        func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!,
                     withError error: Error) { loadFallback(in: webView, error: error) }
    }
}

final class BundleSchemeHandler: NSObject, WKURLSchemeHandler {
    private let routes: [String: (String, String)] = [
        "/": ("mac-test", "html"),
        "/mac-test.html": ("mac-test", "html"),
        "/oskiewar.js": ("oskiewar", "js"),
        "/oskiewar-sfx.mjs": ("oskiewar-sfx", "mjs"),
        "/oskiewar-midi.mjs": ("oskiewar-midi", "mjs"),
        "/frame-driver.mjs": ("frame-driver", "mjs"),
        "/round-room.mjs": ("round-room", "mjs"),
        "/aesthetic.computer/dep/@akamfoad/qr/qr.mjs": ("qr", "mjs"),
        "/aesthetic.computer/lib/product-analytics.mjs": ("product-analytics", "mjs"),
        "/aesthetic.computer/lib/oskiewar-analytics.mjs": ("oskiewar-analytics", "mjs"),
        "/ComicRelief-Regular.ttf": ("ComicRelief-Regular", "ttf"),
    ]

    func webView(_ webView: WKWebView, start task: WKURLSchemeTask) {
        guard let url = task.request.url, let resource = routes[url.path],
              let file = Bundle.main.url(forResource: resource.0, withExtension: resource.1),
              let data = try? Data(contentsOf: file) else {
            respond(task, url: task.request.url, status: 404,
                    type: "text/plain", data: Data("not found".utf8))
            return
        }
        let type: String
        switch resource.1 {
        case "html": type = "text/html"
        case "js", "mjs": type = "text/javascript"
        case "ttf": type = "font/ttf"
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
