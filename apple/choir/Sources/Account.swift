import Foundation
import Security
import WebKit

/// Sign-in is AC's passwordless email code. The flow runs in an invisible web
/// view on aesthetic.computer (choir/auth.html), because Auth0 finishes it
/// with an iframe that must return to a registered origin. The UI is native.
@MainActor
final class Account: ObservableObject {
    struct Tokens: Codable { var access: String; var refresh: String; var expires: Date }

    @Published private(set) var tokens: Tokens?
    @Published var handle: String?
    @Published var notifications = true
    @Published var checked = false
    var signedIn: Bool { tokens != nil }

    private let bridge = OTPBridge()
    private static let keychainKey = "computer.aesthetic.choir.tokens"

    init() { tokens = Self.load() }

    func sendCode(to email: String) async throws {
        Trail.send("signin:send")
        let r = try await bridge.call("return await window.choir.send(email)", ["email": email])
        if let error = r["error"] as? String { Trail.send("signin:send-failed", ["code": r["code"] as? String ?? ""]); throw Failure(error) }
    }

    func verify(email: String, code: String) async throws {
        let r = try await bridge.call("return await window.choir.verify(email, code)", ["email": email, "code": code])
        if let error = r["error"] as? String { Trail.send("signin:failed", ["code": r["code"] as? String ?? ""]); throw Failure(error) }
        guard let access = r["access"] as? String, let expires = r["expires"] as? Double else { throw Failure("Sign-in could not finish") }
        save(Tokens(access: access, refresh: r["refresh"] as? String ?? "", expires: Date(timeIntervalSince1970: expires / 1000)))
        Trail.send("signin:ok")
        await refreshMe()
        Devices.report(.login)
        // Ask once someone can actually join in, never at launch.
        if handle != nil && notifications { await Devices.ask() }
    }

    func signOut() {
        if tokens != nil { Devices.report(.logout) }
        tokens = nil; handle = nil
        SecItemDelete(Self.query as CFDictionary)
        Trail.send("signout")
    }

    /// A live access token, refreshed a minute before it lapses.
    func token() async -> String? {
        guard var t = tokens else { return nil }
        if t.expires.timeIntervalSinceNow > 60 { return t.access }
        guard !t.refresh.isEmpty, let fresh = try? await Self.refresh(t.refresh) else { signOut(); return nil }
        t.access = fresh.0; t.expires = fresh.1
        save(t)
        return t.access
    }

    func refreshMe() async {
        defer { checked = true }
        guard signedIn else { return }
        do {
            let me: Me = try await API.get("?me=1", token: await token())
            handle = me.handle; notifications = me.prefs.notifications
        } catch API.Failure.status(401, _) { signOut() } catch {}
    }

    func setNotifications(_ on: Bool) async {
        notifications = on
        if on { await Devices.ask() }
        struct Body: Encodable { let notifications: Bool }
        struct Reply: Decodable { let prefs: Me.Prefs }
        if let r: Reply = try? await API.send("PUT", "?prefs=1", Body(notifications: on), token: await token()) { notifications = r.prefs.notifications }
    }

    // Keychain

    private static var query: [String: Any] { [kSecClass as String: kSecClassGenericPassword, kSecAttrAccount as String: keychainKey] }
    private static func load() -> Tokens? {
        var q = query; q[kSecReturnData as String] = true
        var out: AnyObject?
        guard SecItemCopyMatching(q as CFDictionary, &out) == errSecSuccess, let data = out as? Data else { return nil }
        return try? JSONDecoder().decode(Tokens.self, from: data)
    }
    private func save(_ t: Tokens) {
        tokens = t
        guard let data = try? JSONEncoder().encode(t) else { return }
        SecItemDelete(Self.query as CFDictionary)
        var q = Self.query
        q[kSecValueData as String] = data
        q[kSecAttrAccessible as String] = kSecAttrAccessibleAfterFirstUnlockThisDeviceOnly
        SecItemAdd(q as CFDictionary, nil)
    }

    private static func refresh(_ refresh: String) async throws -> (String, Date) {
        var r = URLRequest(url: URL(string: "https://hi.aesthetic.computer/oauth/token")!)
        r.httpMethod = "POST"
        r.setValue("application/json", forHTTPHeaderField: "Content-Type")
        r.httpBody = try JSONSerialization.data(withJSONObject: ["grant_type": "refresh_token", "client_id": OTPBridge.clientID, "refresh_token": refresh])
        let (data, response) = try await URLSession.shared.data(for: r)
        guard (response as? HTTPURLResponse)?.statusCode == 200,
              let v = try JSONSerialization.jsonObject(with: data) as? [String: Any],
              let access = v["access_token"] as? String else { throw Failure("Sign in again") }
        return (access, Date().addingTimeInterval(v["expires_in"] as? Double ?? 3600))
    }
}

struct Failure: LocalizedError { let text: String; init(_ t: String) { text = t }; var errorDescription: String? { text } }

/// The invisible aesthetic.computer page that runs auth0-otp.mjs.
@MainActor
final class OTPBridge: NSObject, WKScriptMessageHandler {
    static let clientID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt"
    private var web: WKWebView?
    private var ready: CheckedContinuation<Void, Error>?
    private var isReady = false

    private func view() async throws -> WKWebView {
        if let web, isReady { return web }
        let config = WKWebViewConfiguration()
        config.userContentController.add(self, name: "choirReady")
        let web = WKWebView(frame: CGRect(x: 0, y: 0, width: 1, height: 1), configuration: config)
        // Offscreen web views get throttled; keep it in the window, invisible.
        if let window = UIApplication.shared.connectedScenes.compactMap({ ($0 as? UIWindowScene)?.keyWindow }).first {
            web.alpha = 0; web.isUserInteractionEnabled = false; window.addSubview(web)
        }
        self.web = web
        web.load(URLRequest(url: URL(string: "https://aesthetic.computer/choir/auth.html")!))
        try await withCheckedThrowingContinuation { (c: CheckedContinuation<Void, Error>) in
            ready = c
            Task { try? await Task.sleep(for: .seconds(20)); if let r = self.ready { self.ready = nil; r.resume(throwing: Failure("Couldn't reach sign-in. Check your connection.")) } }
        }
        return web
    }

    func userContentController(_ c: WKUserContentController, didReceive message: WKScriptMessage) {
        isReady = true
        ready?.resume(); ready = nil
    }

    func call(_ body: String, _ args: [String: Any]) async throws -> [String: Any] {
        let web = try await view()
        let value = try await web.callAsyncJavaScript(body, arguments: args, contentWorld: .page)
        return value as? [String: Any] ?? [:]
    }
}
