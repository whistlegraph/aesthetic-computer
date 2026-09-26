import Foundation
import CryptoKit
import Security

/// Same AC client and PKCE callback contract as the Electron/CLI iteration.
/// WebKit intercepts the callback; no loopback listener or prompt page is needed.
struct NativeSignIn {
    static let clientID = "LVdZaMbyXctkGfZDnpzDATB5nR0ZhmMt"
    static let callback = "http://localhost:44233/callback"
    let signUp: Bool
    let verifier: String
    let state: String
    let created = Date()
    private(set) var consumed = false

    init(signUp: Bool = false) throws {
        self.signUp = signUp
        func random() throws -> String {
            var bytes = [UInt8](repeating: 0, count: 32)
            guard SecRandomCopyBytes(kSecRandomDefault, bytes.count, &bytes) == errSecSuccess else { throw Self.failure("Could not start sign-in") }
            return Self.base64url(Data(bytes))
        }
        verifier = try random(); state = try random()
    }
    static func base64url(_ data: Data) -> String {
        data.base64EncodedString().replacingOccurrences(of: "+", with: "-").replacingOccurrences(of: "/", with: "_").replacingOccurrences(of: "=", with: "")
    }
    var url: URL {
        var url = URLComponents(string: "https://hi.aesthetic.computer/authorize")!
        url.queryItems = ["response_type":"code", "client_id":Self.clientID, "redirect_uri":Self.callback,
                          "scope":"openid profile email", "state":state,
                          "code_challenge":Self.base64url(Data(SHA256.hash(data: Data(verifier.utf8)))),
                          "code_challenge_method":"S256", "prompt":"login"].map { URLQueryItem(name: $0.key, value: $0.value) }
        if signUp { url.queryItems?.append(URLQueryItem(name: "screen_hint", value: "signup")) }
        return url.url!
    }
    static func isCallback(_ url: URL) -> Bool {
        url.scheme == "http" && url.host == "localhost" && url.port == 44233 && url.path == "/callback" && url.user == nil && url.password == nil && url.fragment == nil
    }
    mutating func exchangeBody(for url: URL) throws -> Data {
        guard !consumed, Date().timeIntervalSince(created) < 600, Self.isCallback(url),
              let items = URLComponents(url: url, resolvingAgainstBaseURL: false)?.queryItems else { throw Self.failure("Sign-in expired. Try again.") }
        var query: [String:String] = [:]
        for item in items {
            guard query[item.name] == nil else { throw Self.failure("Invalid sign-in response") }
            query[item.name] = item.value ?? ""
        }
        guard query["state"] == state else { throw Self.failure("Sign-in response did not match this window") }
        consumed = true
        guard query["error"] == nil, let code = query["code"], !code.isEmpty else { throw Self.failure("Sign-in was cancelled or refused") }
        return try JSONSerialization.data(withJSONObject: ["grant_type":"authorization_code", "client_id":Self.clientID,
                                                           "redirect_uri":Self.callback, "code_verifier":verifier, "code":code])
    }
    static func exchange(_ body: Data) async throws -> String {
        var request = URLRequest(url: URL(string: "https://hi.aesthetic.computer/oauth/token")!)
        request.httpMethod = "POST"; request.httpBody = body; request.timeoutInterval = 30
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        let (data, response) = try await URLSession.shared.data(for: request)
        guard (response as? HTTPURLResponse)?.statusCode == 200,
              let value = try JSONSerialization.jsonObject(with: data) as? [String:Any],
              let token = value["access_token"] as? String, !token.isEmpty else { throw failure("Sign-in could not finish. Try again.") }
        return token
    }
    static func ignoresNavigationFailure(_ error: NSError, callbackAccepted: Bool, presented: Bool) -> Bool {
        // Once the callback is accepted, token exchange owns completion. WebKit's
        // cancelled redirect must not replace it with an error interstitial.
        !presented || callbackAccepted || (error.domain == NSURLErrorDomain && error.code == NSURLErrorCancelled)
    }
    static func failure(_ text: String) -> NSError { NSError(domain: "AeselSignIn", code: 1, userInfo: [NSLocalizedDescriptionKey:text]) }
}
