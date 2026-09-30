import Foundation

@main @MainActor struct TokenRenewalChecks {
    static func main() async throws {
        let root = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        let service = "computer.aesthetic.aesel.renewal-test." + UUID().uuidString
        let a = SessionStore(directory: root, tokenService: service)
        let b = SessionStore(directory: root, windowID: "second", tokenService: service)
        defer { a.clearToken(); try? FileManager.default.removeItem(at: root) }
        let old = NativeSignIn.Tokens(accessToken: "old", refreshToken: "refresh-old", expiresAt: .distantPast)
        let next = NativeSignIn.Tokens(accessToken: "new", refreshToken: "refresh-new", expiresAt: Date().addingTimeInterval(3600))
        try a.saveRenewalRecord(JSONEncoder().encode(old), accessToken: old.accessToken)
        var calls = 0
        let refresh: (String) async throws -> NativeSignIn.Tokens = { token in
            precondition(token == "refresh-old"); calls += 1
            try await Task.sleep(nanoseconds: 20_000_000)
            return next
        }
        let first = NativeTokenRenewal(store: a, refresh: refresh)
        let second = NativeTokenRenewal(store: b, refresh: refresh)
        async let t1 = first.token(); async let t2 = second.token()
        let result = try await [t1, t2]
        precondition(result == ["new", "new"] && calls == 1)
        let saved = try JSONDecoder().decode(NativeSignIn.Tokens.self, from: a.renewalRecord()!)
        precondition(saved.refreshToken == "refresh-new")
        let current = try await first.token()
        precondition(current == "new" && calls == 1)
        try b.write(key: "session", value: #"{"token":"old"}"#)
        precondition(a.token() == "new", "An old window checkpoint rolled back the access token")
        try a.saveRenewalRecord(JSONEncoder().encode(old), accessToken: old.accessToken)
        let cancelled = Task { try await first.token() }
        try await Task.sleep(nanoseconds: 5_000_000)
        b.clearToken()
        do { _ = try await cancelled.value; preconditionFailure("Refresh resurrected sign-out") } catch {}
        precondition(a.token() == nil && a.renewalRecord() == nil)
        try a.saveRenewalRecord(JSONEncoder().encode(old), accessToken: old.accessToken)
        let offline = NativeTokenRenewal(store: a, refresh: { _ in throw URLError(.notConnectedToInternet) })
        do { _ = try await offline.token(); preconditionFailure("Offline refresh succeeded") } catch {}
        precondition(a.renewalRecord() != nil && a.token() == "old")
        let revoked = NativeTokenRenewal(store: a, refresh: { _ in throw NSError(domain: "AeselSignIn", code: 401) })
        do { _ = try await revoked.token(); preconditionFailure("Revoked refresh succeeded") } catch {}
        precondition(a.renewalRecord() == nil && a.token() == nil)
        let query = URLComponents(url: try NativeSignIn().url, resolvingAgainstBaseURL: false)!.queryItems!
        precondition(query.contains { $0.name == "scope" && ($0.value?.contains("offline_access") ?? false) })
        print("Token rotation, concurrent windows, and sign-out checks passed")
    }
}
