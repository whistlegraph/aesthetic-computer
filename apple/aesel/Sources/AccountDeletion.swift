import Foundation

enum AccountDeletion {
    static func delete(token: String, session: URLSession = .shared) async throws {
        guard !token.isEmpty else { throw failure("Sign in before deleting your account.") }
        var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/delete-erase-and-forget-me")!)
        request.httpMethod = "POST"
        request.timeoutInterval = 120
        request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        let (data, response) = try await session.data(for: request)
        let status = (response as? HTTPURLResponse)?.statusCode ?? 0
        guard status == 200,
              let body = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              body["result"] as? String == "Deleted!" else {
            throw failure(status == 401 ? "Sign in again before deleting your account." : "Account deletion could not finish. Please try again.")
        }
    }

    static func failure(_ text: String) -> NSError {
        NSError(domain: "AeselAccount", code: 1, userInfo: [NSLocalizedDescriptionKey: text])
    }
}
