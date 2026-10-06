import Foundation

/// No local data is erased until the server confirms account deletion is scheduled.
@MainActor final class AccountDeletionClient {
    struct Preview: Decodable {
        let handle: String
        let email: String
        let graceDays: Int
        let handleHoldDays: Int
        let braincells: Double
        let counts: [String: Int]
    }
    struct Schedule: Decodable {
        let state: String
        let purgeAfter: String
        let mailed: Bool?
        var localErasureError: String?
        var purgeDate: Date? {
            let formatter = ISO8601DateFormatter(); formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
            return formatter.date(from: purgeAfter) ?? ISO8601DateFormatter().date(from: purgeAfter)
        }
    }
    struct Identity { let bearer: String; let generation: Int }
    private(set) var preview: Preview?
    private var identity: Identity?
    private var confirming = false
    private let credential: () async throws -> Identity?
    private let current: (Identity) -> Bool
    private let send: (URLRequest) async throws -> (Data, URLResponse)
    private let clear: () async throws -> Void
    private let endpoint = URL(string: "https://aesthetic.computer/api/delete-erase-and-forget-me")!
    init(credential: @escaping () async throws -> Identity?, current: @escaping (Identity) -> Bool,
         send: @escaping (URLRequest) async throws -> (Data, URLResponse) = { try await URLSession.shared.data(for: $0) },
         clear: @escaping () async throws -> Void) {
        self.credential = credential; self.current = current; self.send = send; self.clear = clear
    }
    private func failure(_ text: String) -> NSError { NSError(domain: "AccountDeletion", code: 1, userInfo: [NSLocalizedDescriptionKey: text]) }
    private func request(_ method: String, identity: Identity) async throws -> Data {
        guard current(identity) else { throw failure("Your account changed. Read the deletion preview again.") }
        var request = URLRequest(url: method == "GET" ? endpoint.appending(queryItems: [.init(name: "preview", value: "")]) : endpoint)
        request.httpMethod = method; request.timeoutInterval = 45
        request.setValue("Bearer \(identity.bearer)", forHTTPHeaderField: "Authorization")
        if method == "POST" { request.httpBody = Data("{}".utf8); request.setValue("application/json", forHTTPHeaderField: "Content-Type") }
        let (data, response) = try await send(request)
        guard (response as? HTTPURLResponse)?.statusCode == 200 else {
            let body = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any]
            throw failure(body?["message"] as? String ?? "Could not complete this request. Your local work has not been erased.")
        }
        return data
    }
    func load() async throws -> Preview {
        preview = nil; identity = nil
        guard let auth = try await credential() else { throw failure("Sign in to preview account deletion.") }
        let value = try JSONDecoder().decode(Preview.self, from: await request("GET", identity: auth))
        guard current(auth), value.graceDays >= 0, value.braincells.isFinite, value.braincells >= 0 else {
            throw failure("The account preview changed. Try again.")
        }
        preview = value; identity = auth; return value
    }
    func confirm() async throws -> Schedule {
        guard !confirming else { throw failure("Deletion confirmation is already in progress.") }
        confirming = true; defer { confirming = false }
        guard preview != nil, let original = identity, current(original), let auth = try await credential(),
              auth.generation == original.generation, current(auth) else {
            throw failure("Read the deletion preview for the current account before confirming.")
        }
        var schedule = try JSONDecoder().decode(Schedule.self, from: await request("POST", identity: auth))
        guard schedule.state == "scheduled", schedule.purgeDate != nil else {
            throw failure("The server has not confirmed the deletion schedule. Your local work is still here.")
        }
        guard current(auth) else { throw failure("Deletion was scheduled for the previous account. The current account's local work was not erased.") }
        do { try await clear() }
        catch { schedule.localErasureError = "Deletion is scheduled, but some local files could not be erased: " + error.localizedDescription }
        identity = nil; preview = nil
        return schedule
    }
}
