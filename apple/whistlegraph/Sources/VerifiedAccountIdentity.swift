import Foundation

/// Access tokens may be opaque. Resolve identity with the same authenticated
/// userinfo endpoint as the web client; never infer it from JWT-shaped text.
@MainActor final class VerifiedAccountIdentity {
    enum Failure: Error { case http(Int), invalidResponse, changed }
    typealias Fetch = (URLRequest) async throws -> (Data, URLResponse)
    private var cached: (token: String, generation: Int, subject: String)?
    private var pending: (id: UUID, token: String, generation: Int, task: Task<String, Error>)?
    private let fetch: Fetch
    init(fetch: @escaping Fetch = { try await URLSession.shared.data(for: $0) }) { self.fetch = fetch }
    func invalidate() { cached = nil; pending?.task.cancel(); pending = nil }
    func subject(token: String, generation: Int) async throws -> String {
        if let cached, cached.token == token, cached.generation == generation { return cached.subject }
        if let pending, pending.token == token, pending.generation == generation {
            let subject = try await pending.task.value
            guard self.pending?.id == pending.id || (cached?.token == token && cached?.generation == generation) else { throw Failure.changed }
            return subject
        }
        invalidate()
        let id = UUID(), fetch = fetch
        let task = Task<String, Error> {
            var request = URLRequest(url: URL(string: "https://hi.aesthetic.computer/userinfo")!)
            request.timeoutInterval = 8
            request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
            request.setValue("application/json", forHTTPHeaderField: "Accept")
            DeviceActionLog.shared.record(.accountIdentity, .started)
            let (data, response) = try await fetch(request)
            try Task.checkCancellation()
            guard let response = response as? HTTPURLResponse else { throw Failure.invalidResponse }
            DeviceActionLog.shared.record(.accountIdentity, response.statusCode == 200 ? .succeeded : .httpError, [.status: response.statusCode])
            guard response.statusCode == 200 else { throw Failure.http(response.statusCode) }
            struct Identity: Decodable { let sub: String }
            guard data.count <= 64_000, let value = try? JSONDecoder().decode(Identity.self, from: data),
                  !value.sub.trimmingCharacters(in: .whitespacesAndNewlines).isEmpty, value.sub.count <= 512 else { throw Failure.invalidResponse }
            return value.sub
        }
        pending = (id, token, generation, task)
        do {
            let subject = try await task.value
            guard pending?.id == id else { throw Failure.changed }
            cached = (token, generation, subject); pending = nil
            return subject
        } catch { if pending?.id == id { pending = nil }; throw error }
    }
}
