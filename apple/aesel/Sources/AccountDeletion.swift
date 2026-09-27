import Foundation

enum AccountDeletion {
    struct Schedule { var date: Date?; var mailed: Bool }

    /// Locks the account and schedules its deletion; returns the day the
    /// server will delete it and whether it emailed a link to keep it.
    @discardableResult
    static func delete(token: String, session: URLSession = .shared) async throws -> Schedule {
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
        return Schedule(date: (body["purgeAfter"] as? String).flatMap { ISO8601DateFormatter.fractional.date(from: $0) },
                        mailed: body["mailed"] as? Bool ?? false)
    }

    /// What deletion would remove and keep, as a few short sentences, from
    /// the server's preview. Nil when it cannot be read.
    static func preview(token: String, session: URLSession = .shared) async -> String? {
        guard !token.isEmpty,
              let url = URL(string: "https://aesthetic.computer/api/delete-erase-and-forget-me?preview") else { return nil }
        var request = URLRequest(url: url)
        request.timeoutInterval = 20
        request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        guard let (data, response) = try? await session.data(for: request),
              (response as? HTTPURLResponse)?.statusCode == 200,
              let body = try? JSONSerialization.jsonObject(with: data) as? [String: Any] else { return nil }
        return describe(body)
    }

    static func describe(_ body: [String: Any]) -> String {
        let counts = body["counts"] as? [String: Any] ?? [:]
        func n(_ key: String) -> Int { (counts[key] as? NSNumber)?.intValue ?? 0 }
        func plural(_ count: Int, _ word: String) -> String? {
            count == 0 ? nil : "\(count.formatted()) \(word)\(count == 1 ? "" : "s")"
        }
        let gone = [plural(n("paintings"), "painting"), plural(n("pieces"), "piece"), plural(n("tapes"), "tape"),
                    plural(n("moods"), "mood"), plural(n("news"), "news post"), plural(n("chat"), "chat message"),
                    n("kidlispDeleted") > 0 ? "\(n("kidlispDeleted").formatted()) KidLisp" : nil].compactMap { $0 }
        var lines = ["Deletes \(gone.isEmpty ? "your account" : gone.joined(separator: ", "))."]
        if n("kidlispKept") > 0 {
            lines.append("Keeps \(n("kidlispKept").formatted()) KidLisp without your name, because it is minted or used by others.")
        }
        let braincells = (body["braincells"] as? NSNumber)?.intValue ?? 0
        if braincells > 0 { lines.append("Loses \(braincells.formatted()) braincells.") }
        let holdDays = (body["handleHoldDays"] as? NSNumber)?.intValue ?? 90
        lines.append(body["handleGoesToSotce"] as? Bool == true
                     ? "Your handle stays with your Sotce Net account."
                     : "Nobody can take your handle for \(holdDays) days.")
        let graceDays = (body["graceDays"] as? NSNumber)?.intValue ?? 14
        let email = body["email"] as? String ?? ""
        lines.append("Locks now and deletes after \(graceDays) days." + (email.isEmpty ? "" : " A link to keep it goes to \(email)."))
        return lines.joined(separator: " ")
    }

    static func failure(_ text: String) -> NSError {
        NSError(domain: "AeselAccount", code: 1, userInfo: [NSLocalizedDescriptionKey: text])
    }
}

private extension ISO8601DateFormatter {
    static let fractional: ISO8601DateFormatter = {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return formatter
    }()
}
