import Foundation
import AppKit
import Security

struct FightWireButtons {
  let mask: Int
}

@MainActor
final class NativeMatchmaker: NSObject, URLSessionWebSocketDelegate {
  var onStatus: ((String) -> Void)?
  var onStart: ((Int) -> Void)?
  var onInput: ((FightWireButtons) -> Void)?

  private var session: URLSession!
  private var socket: URLSessionWebSocketTask?
  private var matchID: String?
  private var frame = 0

  func start() {
    guard let token = Self.loadToken() else {
      onStatus?("SIGN IN: trackpad-fighter auth <token>")
      return
    }
    session = URLSession(configuration: .default, delegate: self, delegateQueue: .main)
    let task = session.webSocketTask(with: URL(string: "wss://session-server.aesthetic.computer/")!)
    socket = task; onStatus?("CONNECTING")
    task.resume(); receive()
    send("fight:auth", ["token": token, "requestId": UUID().uuidString])
  }

  func beginLogin() {
    onStatus?("OPENING LOGIN")
    Task {
      do {
        var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/device-pair")!)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONSerialization.data(withJSONObject: ["action": "create", "kind": "browser"])
        let (data, _) = try await URLSession.shared.data(for: request)
        guard let pair = try JSONSerialization.jsonObject(with: data) as? [String: Any],
              let code = pair["code"] as? String, let secret = pair["pollSecret"] as? String else { throw LoginError.invalidResponse }
        NSWorkspace.shared.open(URL(string: "https://aesthetic.computer/api/device-pair-login?code=\(code)&kind=browser")!)
        onStatus?("FINISH LOGIN IN BROWSER")
        for _ in 0..<120 {
          try await Task.sleep(for: .seconds(2))
          var parts = URLComponents(string: "https://aesthetic.computer/api/device-pair")!
          parts.queryItems = [URLQueryItem(name: "code", value: code), URLQueryItem(name: "secret", value: secret)]
          let (pollData, response) = try await URLSession.shared.data(from: parts.url!)
          if (response as? HTTPURLResponse)?.statusCode == 410 { throw LoginError.expired }
          guard let result = try? JSONSerialization.jsonObject(with: pollData) as? [String: Any],
                result["status"] as? String == "claimed",
                let session = result["session"] as? [String: Any], let token = session["accessToken"] as? String else { continue }
          guard Self.saveToken(token) else { throw LoginError.keychain }
          onStatus?("SIGNED IN — CONNECTING"); start(); return
        }
        throw LoginError.expired
      } catch { onStatus?("LOGIN FAILED — CLICK TO RETRY") }
    }
  }

  private enum LoginError: Error { case invalidResponse, expired, keychain }

  func stop() { socket?.cancel(with: .goingAway, reason: nil); socket = nil }

  func sendInput(_ buttons: Int) {
    guard let matchID else { return }
    frame += 1
    send("fight:input", ["matchId": matchID, "frame": frame, "buttons": buttons])
  }

  nonisolated func urlSession(_ session: URLSession, webSocketTask: URLSessionWebSocketTask,
                              didOpenWithProtocol protocol: String?) {
    Task { @MainActor in self.onStatus?("AUTHENTICATING") }
  }

  private func receive() {
    socket?.receive { [weak self] result in
      Task { @MainActor in
        guard let self else { return }
        if case .success(let message) = result {
          let data: Data?
          switch message { case .string(let value): data = value.data(using: .utf8); case .data(let value): data = value; @unknown default: data = nil }
          if let data { self.handle(data) }
          self.receive()
        } else { self.onStatus?("OFFLINE — PRACTICE"); self.socket = nil }
      }
    }
  }

  private func handle(_ data: Data) {
    guard let outer = try? JSONSerialization.jsonObject(with: data) as? [String: Any], let type = outer["type"] as? String else { return }
    var content = outer["content"] as? [String: Any] ?? [:]
    if let string = outer["content"] as? String, let bytes = string.data(using: .utf8),
       let parsed = try? JSONSerialization.jsonObject(with: bytes) as? [String: Any] { content = parsed }
    switch type {
    case "fight:auth:ok":
      onStatus?("SEARCHING — PRACTICE")
      send("fight:queue:join", ["manifest": Self.manifest, "region": "us-west", "platform": "macos-native", "mode": "casual", "transport": "ws-input-v1"])
    case "fight:match:proposal":
      guard let id = content["matchId"] as? String else { return }
      matchID = id; onStatus?("OPPONENT FOUND")
      send("fight:match:accept", ["matchId": id, "manifest": Self.manifest])
    case "fight:match:start":
      matchID = content["matchId"] as? String
      onStatus?("FIGHT")
      onStart?(content["seat"] as? Int ?? 0)
    case "fight:input":
      if let buttons = content["buttons"] as? Int { onInput?(FightWireButtons(mask: buttons)) }
    case "fight:match:peer-left": onStatus?("OPPONENT LEFT")
    case "fight:error": onStatus?(String(describing: content["message"] ?? "MATCH ERROR").uppercased())
    default: break
    }
  }

  private func send(_ type: String, _ content: [String: Any]) {
    guard let data = try? JSONSerialization.data(withJSONObject: ["type": type, "content": content]),
          let text = String(data: data, encoding: .utf8) else { return }
    socket?.send(.string(text)) { _ in }
  }

  static let manifest: [String: Any] = [
    "protocolVersion": 1, "buildId": "menu-fighter-dev-2026-07-20",
    "simHash": "fight-int32-v1", "rulesHash": "freefight-v1", "contentHash": "base-roster-v1"
  ]

  static func loadToken() -> String? {
    if let value = ProcessInfo.processInfo.environment["AC_TOKEN"], !value.isEmpty { return value }
    let path = NSString(string: "~/.config/aesthetic-computer/token").expandingTildeInPath
    if let value = try? String(contentsOfFile: path, encoding: .utf8).trimmingCharacters(in: .whitespacesAndNewlines), !value.isEmpty { return value }
    var item: CFTypeRef?
    let query: [String: Any] = [kSecClass as String: kSecClassGenericPassword, kSecAttrService as String: "computer.aesthetic.menu-fighter", kSecAttrAccount as String: "access-token", kSecReturnData as String: true]
    guard SecItemCopyMatching(query as CFDictionary, &item) == errSecSuccess, let data = item as? Data else { return nil }
    return String(data: data, encoding: .utf8)
  }

  static func saveToken(_ token: String) -> Bool {
    let key: [String: Any] = [kSecClass as String: kSecClassGenericPassword, kSecAttrService as String: "computer.aesthetic.menu-fighter", kSecAttrAccount as String: "access-token"]
    SecItemDelete(key as CFDictionary)
    var value = key; value[kSecValueData as String] = Data(token.utf8)
    return SecItemAdd(value as CFDictionary, nil) == errSecSuccess
  }
}
