import Foundation

/// Saved Loopboy routing and the live launch identity are deliberately
/// separate. A route is operational only when both agree: editing the JSON
/// registry cannot retrofit the environment, MCP headers, or reduced tool
/// surface that a guarded Loopboy receives at process launch.
struct LoopboyRoute {
    let contact: String
    let sessionId: String
    let host: String
    let name: String
    let wake: Bool
}

enum LoopboyRoutes {
    static func all() -> [String: LoopboyRoute] {
        guard let data = FileManager.default.contents(atPath: Paths.loopboyConfig),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let loops = obj["loops"] as? [String: Any] else { return [:] }
        var routes: [String: LoopboyRoute] = [:]
        for (rawContact, value) in loops {
            guard let loop = value as? [String: Any],
                  let sid = loop["sessionId"] as? String, !sid.isEmpty else { continue }
            let contact = rawContact.lowercased()
            routes[contact] = LoopboyRoute(
                contact: contact,
                sessionId: sid,
                host: (loop["host"] as? String) ?? "?",
                name: (loop["name"] as? String) ?? "?",
                wake: (loop["wake"] as? Bool) ?? false)
        }
        return routes
    }

    /// Return the contact only when the saved route and immutable launch-time
    /// marker agree on this exact session.
    static func verifiedContact(for session: ClaudeSession,
                                routes: [String: LoopboyRoute]? = nil) -> String? {
        let contact = session.loopboyContact.trimmingCharacters(in: .whitespacesAndNewlines)
            .lowercased()
        guard !contact.isEmpty,
              let route = (routes ?? all())[contact],
              route.sessionId == session.sessionId else { return nil }
        return contact
    }

    static func verifiedBySession(_ sessions: [ClaudeSession]) -> [String: String] {
        let routes = all()
        return Dictionary(uniqueKeysWithValues: sessions.compactMap { session in
            verifiedContact(for: session, routes: routes).map { (session.sessionId, $0) }
        })
    }
}
