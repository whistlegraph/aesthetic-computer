import Foundation

@main struct AccountChecks {
    @MainActor static func main() async throws {
        var saved: Data?
        var polls = 0
        let secret = String(repeating: "s", count: 43)
        let account = ACAccount(transport: { request in
            let body: [String: Any]
            if request.httpMethod == "POST" {
                let sent = try JSONSerialization.jsonObject(with: request.httpBody!) as! [String: String]
                precondition(sent == ["action": "create", "kind": "browser"])
                body = ["code": "ABC123", "pollSecret": secret]
            } else {
                let query = URLComponents(url: request.url!, resolvingAgainstBaseURL: false)!.queryItems!
                precondition(query.contains(URLQueryItem(name: "secret", value: secret)))
                polls += 1
                body = polls == 1 ? ["status": "pending"] : ["status": "claimed", "session": [
                    "accessToken": "test-only-token", "account": ["label": "@tester", "id": "test-sub"]]]
            }
            return (try JSONSerialization.data(withJSONObject: body), HTTPURLResponse(url: request.url!, statusCode: 200, httpVersion: nil, headerFields: nil)!)
        }, load: { nil }, save: { saved = $0 }, remove: { saved = nil }, delay: {})
        let (pair, url) = try await account.begin()
        precondition(!url.absoluteString.contains(secret))
        precondition(url.host == "aesthetic.computer")
        try await account.complete(pair)
        precondition(polls == 2 && account.session?.account.label == "@tester" && saved != nil)
        let restored = ACAccount(load: { saved }, save: { _ in }, remove: {})
        precondition(restored.session?.account.id == "test-sub")
        try account.signOut(); precondition(saved == nil && account.session == nil)

        var beganPoll = false, cancelledWrite = false
        let cancelled = ACAccount(transport: { request in
            beganPoll = true
            try await Task.sleep(for: .seconds(10))
            return (Data(), HTTPURLResponse(url: request.url!, statusCode: 200, httpVersion: nil, headerFields: nil)!)
        }, load: { nil }, save: { _ in cancelledWrite = true }, remove: {})
        let task = Task { @MainActor in try await cancelled.complete(pair) }
        while !beganPoll { await Task.yield() }
        task.cancel()
        do { try await task.value; preconditionFailure("Cancellation must fail") } catch is CancellationError {}
        precondition(!cancelledWrite && cancelled.session == nil)

        for fixture in ["expired", "missing-session", "invalid-secret"] {
            var wrote = false
            let invalid = ACAccount(transport: { request in
                let status = fixture == "expired" ? 410 : 200
                let json = fixture == "invalid-secret" ? "{\"code\":\"ABC123\",\"pollSecret\":\"short\"}" : "{\"status\":\"claimed\"}"
                return (Data(json.utf8), HTTPURLResponse(url: request.url!, statusCode: status, httpVersion: nil, headerFields: nil)!)
            }, load: { nil }, save: { _ in wrote = true }, remove: {}, delay: {})
            do {
                if fixture == "invalid-secret" { _ = try await invalid.begin() } else { try await invalid.complete(pair) }
                preconditionFailure("Invalid response accepted")
            } catch {}
            precondition(!wrote && invalid.session == nil)
        }
        print("AC account checks passed: browser-only pairing, secret separation, pending/claim, persistence, sign-out, cancellation, and invalid responses.")
    }
}
