import Foundation

/// A route is one declared event/source pair. Contact-only routes remain v1
/// iMessage subscriptions; custom sources never consume the iMessage bus.
struct LoopboyRoute {
    let contact: String // Route key; retained for renderer compatibility.
    let channel: String
    let source: String
    let sessionId: String
    let host: String
    let name: String
    let bindingId: String
}

enum LoopboyRoutes {
    static func validKey(_ value: String) -> Bool {
        value.range(of: "^[a-z0-9][a-z0-9_-]{0,39}$", options: .regularExpression) != nil
    }

    static func date(_ value: String) -> Date? {
        let formatter = ISO8601DateFormatter()
        formatter.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return formatter.date(from: value) ?? ISO8601DateFormatter().date(from: value)
    }

    /// Metadata only. Neither this declaration nor event payloads execute code.
    static func binding(_ obj: [String: Any]) -> [String: Any]? {
        let contact = (obj["contact"] as? String ?? "").trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        let event = (obj["event"] as? String ?? (contact.isEmpty ? "" : "imessage"))
            .trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        let source = (obj["source"] as? String ?? contact).trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        guard validKey(event), validKey(source),
              contact.isEmpty || (event == "imessage" && contact == source) else { return nil }
        var result: [String: Any] = ["event": event, "source": source,
            "contact": event == "imessage" ? source : "",
            "routeKey": event == "imessage" ? source : "\(event):\(source)"]
        if let expiry = obj["expiresAt"] {
            guard let value = expiry as? String, let end = date(value), end > Date() else { return nil }
            result["expiresAt"] = value
        }
        if let description = obj["description"] {
            guard let value = description as? String, value.count <= 240 else { return nil }
            result["description"] = value
        }
        if let config = obj["config"] {
            guard let value = config as? [String: Any],
                  let data = try? JSONSerialization.data(withJSONObject: value), data.count <= 4096 else { return nil }
            result["config"] = value
        }
        return result
    }

    static func all() -> [String: LoopboyRoute] {
        guard let data = FileManager.default.contents(atPath: Paths.loopboyConfig),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let loops = obj["loops"] as? [String: Any] else { return [:] }
        var routes: [String: LoopboyRoute] = [:]
        for (rawKey, value) in loops {
            guard var loop = value as? [String: Any],
                  let sid = loop["sessionId"] as? String, !sid.isEmpty else { continue }
            loop["event"] = loop["event"] ?? loop["channel"] ?? "imessage"
            loop["source"] = loop["source"] ?? loop["contact"] ?? rawKey.lowercased()
            guard let binding = binding(loop), let key = binding["routeKey"] as? String,
                  key == rawKey.lowercased(), let event = binding["event"] as? String,
                  let source = binding["source"] as? String else { continue }
            routes[key] = LoopboyRoute(contact: key, channel: event, source: source,
                sessionId: sid, host: loop["host"] as? String ?? "?",
                name: loop["name"] as? String ?? "?", bindingId: loop["bindingId"] as? String ?? "")
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

    /// Returns the route key (contact for iMessage, event:source otherwise).
    static func verifiedContact(for session: ClaudeSession,
                                routes: [String: LoopboyRoute]? = nil) -> String? {
        let mode = mode(for: session.sessionId)
        guard let binding = binding(mode ?? ["contact": session.loopboyContact]),
              let key = binding["routeKey"] as? String,
              let route = (routes ?? all())[key], route.sessionId == session.sessionId else { return nil }
        if let generation = mode?["bindingId"] as? String, !generation.isEmpty,
           generation != route.bindingId { return nil }
        return key
    }

    static func verifiedBySession(_ sessions: [ClaudeSession]) -> [String: String] {
        let routes = all()
        return Dictionary(uniqueKeysWithValues: sessions.compactMap { session in
            verifiedContact(for: session, routes: routes).map { (session.sessionId, $0) }
        })
    }
}
