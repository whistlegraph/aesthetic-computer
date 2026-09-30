import Foundation
import WebKit

/// One rotation per Keychain service, shared by all notebook windows. Only
/// access tokens cross into the trusted session WebView; refresh tokens do not.
@MainActor final class NativeTokenRenewal: NSObject, WKScriptMessageHandlerWithReply {
    private static var pending: [String: (UUID, Task<NativeSignIn.Tokens, Error>)] = [:]
    let store: SessionStore
    weak var sessionView: WKWebView?
    private let refreshTokens: (String) async throws -> NativeSignIn.Tokens
    init(store: SessionStore, refresh: @escaping (String) async throws -> NativeSignIn.Tokens = NativeSignIn.refresh) { self.store = store; self.refreshTokens = refresh }

    func token(force: Bool = false) async throws -> String {
        guard let original = store.renewalRecord() else { return store.token() ?? "" }
        let saved = try JSONDecoder().decode(NativeSignIn.Tokens.self, from: original)
        if !force && saved.expiresAt.timeIntervalSinceNow > 60 { return saved.accessToken }
        guard let refresh = saved.refreshToken, !refresh.isEmpty else {
            throw NSError(domain: "AeselSignIn", code: 401, userInfo: [NSLocalizedDescriptionKey:"Sign in again to renew your AC session. Your work is saved."])
        }
        let key = store.credentialKey
        let operation: (UUID, Task<NativeSignIn.Tokens, Error>)
        if let existing = Self.pending[key] { operation = existing }
        else {
            operation = (UUID(), Task { try await refreshTokens(refresh) })
            Self.pending[key] = operation
        }
        defer { if Self.pending[key]?.0 == operation.0 { Self.pending[key] = nil } }
        do {
            let next = try await operation.1.value
            let encoded = try JSONEncoder().encode(next)
            // Another window can finish the same rotation first. Anything else
            // means sign-out/account replacement won; never resurrect it.
            guard store.renewalRecord() == original || (store.renewalRecord().flatMap { try? JSONDecoder().decode(NativeSignIn.Tokens.self, from: $0) }) == next else {
                throw NativeSignIn.failure("Your AC account changed. Try again.")
            }
            try store.saveRenewalRecord(encoded, accessToken: next.accessToken)
            return next.accessToken
        } catch {
            if (error as NSError).domain == "AeselSignIn", (error as NSError).code == 401,
               store.renewalRecord() == original { store.clearToken() }
            throw error
        }
    }

    func userContentController(_ userContentController: WKUserContentController, didReceive message: WKScriptMessage,
                               replyHandler: @escaping (Any?, String?) -> Void) {
        guard message.webView === sessionView, message.frameInfo.isMainFrame,
              let body = message.body as? [String:Any], body["method"] as? String == "token" else {
            replyHandler(nil, "Unsupported account request"); return
        }
        Task {
            do { replyHandler(["token": try await token(force: body["force"] as? Bool ?? false)], nil) }
            catch {
                let invalid = (error as NSError).domain == "AeselSignIn" && (error as NSError).code == 401
                replyHandler(["error": error.localizedDescription, "status": invalid ? 401 : 503], nil)
            }
        }
    }
}
