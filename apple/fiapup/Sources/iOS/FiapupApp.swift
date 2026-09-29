// fiapup for iPhone and iPad: the game's web shell, full screen, in either
// orientation. The same bundle and fiapup:// scheme as the Mac app; the page
// taps the phone through a `haptic` message handler.
//
// Launch arguments (simctl launch … -stage fetch -orientation landscape)
// open it on a staged moment, for screenshots: -stage, -seconds, -pause 1.

import UIKit
import WebKit

final class GameController: UIViewController, WKScriptMessageHandler {
  let scheme = BundleScheme()
  let taps: [String: UIImpactFeedbackGenerator] = [
    "soft": UIImpactFeedbackGenerator(style: .soft),
    "light": UIImpactFeedbackGenerator(style: .light),
    "medium": UIImpactFeedbackGenerator(style: .medium),
  ]

  override func loadView() {
    let config = WKWebViewConfiguration()
    config.setURLSchemeHandler(scheme, forURLScheme: "fiapup")
    config.allowsInlineMediaPlayback = true
    config.mediaTypesRequiringUserActionForPlayback = []
    config.userContentController.add(self, name: "haptic")
    let web = WKWebView(frame: .zero, configuration: config)
    web.isOpaque = false
    web.backgroundColor = UIColor(red: 0.77, green: 0.88, blue: 0.94, alpha: 1)
    web.scrollView.isScrollEnabled = false
    web.scrollView.bounces = false
    web.scrollView.contentInsetAdjustmentBehavior = .never   // the page reads env(safe-area-inset-*)
    if #available(iOS 16.4, *) { web.isInspectable = true }
    view = web
    var query = ["touch"]
    let args = UserDefaults.standard
    if let stage = args.string(forKey: "stage") {
      query.append("stage=\(stage)")
      if let seconds = args.string(forKey: "seconds") { query.append("seconds=\(seconds)") }
      if args.bool(forKey: "pause") { query.append("pause") }
    }
    web.load(URLRequest(url: URL(string: "fiapup://app/index.html?" + query.joined(separator: "&"))!))
  }

  override func viewDidAppear(_ animated: Bool) {
    super.viewDidAppear(animated)
    guard UserDefaults.standard.string(forKey: "orientation") == "landscape" else { return }
    if #available(iOS 16.0, *) {
      view.window?.windowScene?.requestGeometryUpdate(.iOS(interfaceOrientations: .landscapeRight))
      setNeedsUpdateOfSupportedInterfaceOrientations()
    }
  }

  func userContentController(_ controller: WKUserContentController, didReceive message: WKScriptMessage) {
    guard let kind = message.body as? String, let tap = taps[kind] else { return }
    tap.impactOccurred()
    tap.prepare()
  }

  override var prefersStatusBarHidden: Bool { true }
  override var prefersHomeIndicatorAutoHidden: Bool { true }
  override var supportedInterfaceOrientations: UIInterfaceOrientationMask {
    UIDevice.current.userInterfaceIdiom == .pad ? .all : .allButUpsideDown
  }
}

@main
final class AppDelegate: UIResponder, UIApplicationDelegate {
  var window: UIWindow?

  func application(_ application: UIApplication,
                   didFinishLaunchingWithOptions options: [UIApplication.LaunchOptionsKey: Any]?) -> Bool {
    let window = UIWindow(frame: UIScreen.main.bounds)
    window.rootViewController = GameController()
    window.makeKeyAndVisible()
    self.window = window
    return true
  }
}
