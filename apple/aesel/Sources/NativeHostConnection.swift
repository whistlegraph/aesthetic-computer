import Foundation
import WebKit

/// The host credential stays native. Only the bundled session can request
/// bounded provider operations; artwork and login web views have no handler.
@MainActor
final class NativeHostConnection: NSObject, WKScriptMessageHandlerWithReply {
    weak var sessionView: WKWebView?
    private let methods: Set<String> = ["capabilities", "configure", "turn", "events", "interrupt", "approval", "approvalPolicy"]

    private struct Configuration: Decodable { let schema: Int; let url: URL; let token: String }
    private func configuration() throws -> Configuration {
        #if os(macOS)
        let directory = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appendingPathComponent(Bundle.main.bundleIdentifier ?? "computer.aesthetic.aesel.native")
        #else
        let directory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
        #endif
        let file = directory.appendingPathComponent("host.json")
        guard let data = try? Data(contentsOf: file), data.count < 8192,
              let config = try? JSONDecoder().decode(Configuration.self, from: data),
              config.schema == 1, config.token.count >= 32,
              config.url.scheme == "http", config.url.host == "127.0.0.1",
              config.url.user == nil, config.url.password == nil,
              config.url.path == "/rpc", config.url.query == nil else {
            throw failure("Start Aesel Host on this Mac to connect Claude and Codex.")
        }
        return config
    }

    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage,
                               replyHandler: @escaping (Any?, String?) -> Void) {
        guard message.webView === sessionView, message.frameInfo.isMainFrame,
              message.webView?.url?.scheme == "aesel-bundle",
              message.webView?.url?.host == "app",
              let body = message.body as? [String: Any], let method = body["method"] as? String,
              methods.contains(method), let params = body["params"] as? [String: Any] else {
            replyHandler(nil, "Unsupported host request"); return
        }
        Task {
            do {
                let config = try configuration()
                let payload = try JSONSerialization.data(withJSONObject: ["method": method, "params": params])
                guard payload.count <= 2 * 1024 * 1024 else { throw failure("Host request is too large") }
                var request = URLRequest(url: config.url)
                request.httpMethod = "POST"; request.httpBody = payload; request.timeoutInterval = 40
                request.setValue("application/json", forHTTPHeaderField: "Content-Type")
                request.setValue("Bearer \(config.token)", forHTTPHeaderField: "Authorization")
                let (data, response) = try await URLSession.shared.data(for: request)
                guard data.count <= 8 * 1024 * 1024,
                      let result = try JSONSerialization.jsonObject(with: data) as? [String: Any] else {
                    throw failure("Invalid host response")
                }
                if let error = result["error"] as? String { throw failure(error) }
                guard (response as? HTTPURLResponse)?.statusCode == 200, let value = result["result"] else {
                    throw failure("Host request failed")
                }
                replyHandler(value, nil)
            } catch { replyHandler(nil, error.localizedDescription) }
        }
    }

    private func failure(_ text: String) -> NSError {
        NSError(domain: "AeselHost", code: 1, userInfo: [NSLocalizedDescriptionKey: text])
    }
}
