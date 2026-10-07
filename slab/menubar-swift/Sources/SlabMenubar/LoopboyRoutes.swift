import Foundation

/// Routes must agree with the session's mutable mode (or a legacy marker).
/// A persisted OFF mode overrides old launch environments and marker writers.
struct LoopboyRoute {
    let contact: String
    let channel: String
    let sessionId: String
    let host: String
    let name: String
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
            let channel = ((loop["channel"] as? String)
                ?? (loop["event"] as? String) ?? "imessage").lowercased()
            // A route is also a channel contract. Never let a future Signal or
            // mail registration silently consume the iMessage contact bus.
            guard channel == "imessage" else { continue }
            routes[contact] = LoopboyRoute(
                contact: contact,
                channel: channel,
                sessionId: sid,
                host: (loop["host"] as? String) ?? "?",
                name: (loop["name"] as? String) ?? "?")
        }
        return routes
    }

    static func mode(for sessionId: String) -> [String: Any]? {
        guard sessionId.range(of: "^[A-Za-z0-9._-]{1,180}$", options: .regularExpression) != nil,
              sessionId != ".", sessionId != ".." else { return ["contact": ""] }
        let path = "\(Paths.slabHome)/state/loopboy-modes/\(sessionId).json"
        guard FileManager.default.fileExists(atPath: path) else { return nil }
        guard let data = FileManager.default.contents(atPath: path),
              let mode = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              (mode["sessionId"] as? String) == sessionId,
              mode["contact"] is String else { return ["contact": ""] }
        return mode
    }

    /// No launch-time contact or provider restart is required for adoption.
    static func verifiedContact(for session: ClaudeSession,
                                routes: [String: LoopboyRoute]? = nil) -> String? {
        let contact = ((mode(for: session.sessionId)?["contact"] as? String)
            ?? session.loopboyContact).trimmingCharacters(in: .whitespacesAndNewlines)
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
