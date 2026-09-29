// The fiapup:// scheme, shared by the Mac and iOS apps: the shell, the game
// and the two scene modules it borrows ship flat in the bundle, and this
// hands them to WebKit, which won't load ES modules from file:// URLs.

import Foundation
import WebKit

final class BundleScheme: NSObject, WKURLSchemeHandler {
  static let types = ["html": "text/html", "js": "text/javascript", "mjs": "text/javascript"]

  func webView(_ webView: WKWebView, start task: WKURLSchemeTask) {
    guard let url = task.request.url else { return }
    // Resources land flat in the bundle, so live/scene3d.mjs is found by name.
    var name = url.lastPathComponent
    if name.isEmpty || name == "/" { name = "index.html" }
    let ext = (name as NSString).pathExtension
    guard let file = Bundle.main.url(forResource: (name as NSString).deletingPathExtension,
                                     withExtension: ext),
          let data = try? Data(contentsOf: file) else {
      task.didFailWithError(NSError(domain: "fiapup", code: 404,
                                    userInfo: [NSLocalizedDescriptionKey: "no \(name) in the bundle"]))
      return
    }
    let type = (Self.types[ext] ?? "application/octet-stream") + "; charset=utf-8"
    let response = HTTPURLResponse(url: url, statusCode: 200, httpVersion: "HTTP/1.1",
                                   headerFields: ["Content-Type": type, "Cache-Control": "no-store"])!
    task.didReceive(response)
    task.didReceive(data)
    task.didFinish()
  }

  func webView(_ webView: WKWebView, stop task: WKURLSchemeTask) {}
}
