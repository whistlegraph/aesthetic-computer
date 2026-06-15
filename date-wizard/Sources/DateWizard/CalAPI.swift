// CalAPI.swift — URLSession networking for the AesthetiCal backend.
//
// Base: https://aesthetic.computer
// All authed calls send `Authorization: Bearer <token>`.
//
//   GET    /api/cal?from=<ISO>&to=<ISO>                  → { events:[CalEvent] }     (editable)
//   POST   /api/cal {title,start,end,allDay?,note?,visibility?} → { event }
//   PUT    /api/cal {uid, ...fields}                     → { event }
//   DELETE /api/cal?uid=<uid>                            → { ok:true }
//   GET    /api/cal?feedEvents=1&from=<ISO>&to=<ISO>     → { events:[FeedEvent], errors:[...] }  (read-only)
//   GET    /api/cal?feeds=1                              → { feeds:[Feed] }
//   POST   /api/cal {feed:{url,label}}                   → { feed }
//   DELETE /api/cal?feedId=<id>                          → { ok:true }
//
// Times are ISO-8601 UTC strings on the wire; we keep them as String and
// parse to Date via ISO8601 helpers in CalEvent.
import Foundation

// ── wire models ──────────────────────────────────────────────────────

// An editable AesthetiCal event (the user owns these).
struct CalEvent: Codable {
    var uid: String
    var title: String
    var start: String          // ISO-8601 UTC
    var end: String            // ISO-8601 UTC
    var allDay: Bool?
    var rrule: String?
    var note: String?
    var visibility: String?    // "private" | "public"
}

// A read-only overlay event sourced from a connected (Google) calendar.
struct FeedEvent: Codable {
    var uid: String
    var title: String
    var start: String
    var end: String?
    var allDay: Bool?
    var note: String?
    var source: String?        // feed id
    var label: String?         // feed label
    var color: String?         // feed color (hex or css name)
    var readOnly: Bool?
}

struct FeedError: Codable {
    var id: String?
    var label: String?
    var message: String?
}

// A connected external calendar subscription.
struct Feed: Codable {
    var id: String
    var label: String?
    var url: String?
    var color: String?
    var kind: String?          // "google" | "ics"
}

// ── response envelopes ───────────────────────────────────────────────

private struct EventsEnvelope: Codable { var events: [CalEvent] }
private struct EventEnvelope: Codable { var event: CalEvent }
private struct FeedEventsEnvelope: Codable {
    var events: [FeedEvent]
    var errors: [FeedError]?
}
private struct FeedsEnvelope: Codable { var feeds: [Feed] }
private struct FeedEnvelope: Codable { var feed: Feed }
private struct OkEnvelope: Codable { var ok: Bool }
private struct MessageEnvelope: Codable { var message: String? }

// ── errors ───────────────────────────────────────────────────────────

enum CalAPIError: Error, LocalizedError {
    case unauthorized                 // 401 — token missing/expired; sign in again.
    case http(Int, String)            // any non-2xx with a server message.
    case transport(Error)             // URLSession failure.
    case decode(Error)                // JSON shape mismatch.
    case noData

    var errorDescription: String? {
        switch self {
        case .unauthorized: return "Authorization failed — run ac-login to sign in."
        case .http(let code, let msg): return "HTTP \(code): \(msg)"
        case .transport(let e): return "Network error: \(e.localizedDescription)"
        case .decode(let e): return "Decode error: \(e.localizedDescription)"
        case .noData: return "No data returned."
        }
    }
}

// ── client ───────────────────────────────────────────────────────────

final class CalAPI {
    static let base = "https://aesthetic.computer"

    private let session: URLSession
    private var token: String?

    init(token: String?) {
        self.token = token
        let cfg = URLSessionConfiguration.default
        cfg.timeoutIntervalForRequest = 20
        cfg.requestCachePolicy = .reloadIgnoringLocalCacheData
        self.session = URLSession(configuration: cfg)
    }

    func setToken(_ token: String?) { self.token = token }

    // Always-UTC ISO-8601 formatter for from/to query params.
    static func iso(_ date: Date) -> String {
        let fmt = ISO8601DateFormatter()
        fmt.timeZone = TimeZone(identifier: "UTC")
        return fmt.string(from: date)
    }

    // ── editable events ──────────────────────────────────────────────

    func fetchEvents(from: Date, to: Date,
                     completion: @escaping (Result<[CalEvent], CalAPIError>) -> Void) {
        var comps = URLComponents(string: "\(Self.base)/api/cal")!
        comps.queryItems = [
            URLQueryItem(name: "from", value: Self.iso(from)),
            URLQueryItem(name: "to", value: Self.iso(to)),
        ]
        request(url: comps.url!, method: "GET", body: nil) { result in
            completion(result.flatMap { Self.decode(EventsEnvelope.self, $0).map { $0.events } })
        }
    }

    func createEvent(title: String, start: String, end: String,
                     allDay: Bool, note: String, visibility: String,
                     completion: @escaping (Result<CalEvent, CalAPIError>) -> Void) {
        let body: [String: Any] = [
            "title": title, "start": start, "end": end,
            "allDay": allDay, "note": note, "visibility": visibility,
        ]
        request(url: URL(string: "\(Self.base)/api/cal")!, method: "POST",
                body: Self.json(body)) { result in
            completion(result.flatMap { Self.decode(EventEnvelope.self, $0).map { $0.event } })
        }
    }

    func updateEvent(uid: String, title: String, start: String, end: String,
                     allDay: Bool, note: String, visibility: String,
                     completion: @escaping (Result<CalEvent, CalAPIError>) -> Void) {
        let body: [String: Any] = [
            "uid": uid, "title": title, "start": start, "end": end,
            "allDay": allDay, "note": note, "visibility": visibility,
        ]
        request(url: URL(string: "\(Self.base)/api/cal")!, method: "PUT",
                body: Self.json(body)) { result in
            completion(result.flatMap { Self.decode(EventEnvelope.self, $0).map { $0.event } })
        }
    }

    func deleteEvent(uid: String,
                     completion: @escaping (Result<Bool, CalAPIError>) -> Void) {
        var comps = URLComponents(string: "\(Self.base)/api/cal")!
        comps.queryItems = [URLQueryItem(name: "uid", value: uid)]
        request(url: comps.url!, method: "DELETE", body: nil) { result in
            completion(result.flatMap { Self.decode(OkEnvelope.self, $0).map { $0.ok } })
        }
    }

    // ── read-only Google / feed overlay ──────────────────────────────

    func fetchFeedEvents(from: Date, to: Date,
                         completion: @escaping (Result<[FeedEvent], CalAPIError>) -> Void) {
        var comps = URLComponents(string: "\(Self.base)/api/cal")!
        comps.queryItems = [
            URLQueryItem(name: "feedEvents", value: "1"),
            URLQueryItem(name: "from", value: Self.iso(from)),
            URLQueryItem(name: "to", value: Self.iso(to)),
        ]
        request(url: comps.url!, method: "GET", body: nil) { result in
            completion(result.flatMap { Self.decode(FeedEventsEnvelope.self, $0).map { $0.events } })
        }
    }

    // ── connected calendar management ────────────────────────────────

    func fetchFeeds(completion: @escaping (Result<[Feed], CalAPIError>) -> Void) {
        var comps = URLComponents(string: "\(Self.base)/api/cal")!
        comps.queryItems = [URLQueryItem(name: "feeds", value: "1")]
        request(url: comps.url!, method: "GET", body: nil) { result in
            completion(result.flatMap { Self.decode(FeedsEnvelope.self, $0).map { $0.feeds } })
        }
    }

    func connectFeed(url feedURL: String, label: String,
                     completion: @escaping (Result<Feed, CalAPIError>) -> Void) {
        let body: [String: Any] = ["feed": ["url": feedURL, "label": label]]
        request(url: URL(string: "\(Self.base)/api/cal")!, method: "POST",
                body: Self.json(body)) { result in
            completion(result.flatMap { Self.decode(FeedEnvelope.self, $0).map { $0.feed } })
        }
    }

    func removeFeed(id: String,
                    completion: @escaping (Result<Bool, CalAPIError>) -> Void) {
        var comps = URLComponents(string: "\(Self.base)/api/cal")!
        comps.queryItems = [URLQueryItem(name: "feedId", value: id)]
        request(url: comps.url!, method: "DELETE", body: nil) { result in
            completion(result.flatMap { Self.decode(OkEnvelope.self, $0).map { $0.ok } })
        }
    }

    // ── plumbing ─────────────────────────────────────────────────────

    // Run an authed request; hops back to the main queue with raw Data.
    private func request(url: URL, method: String, body: Data?,
                         completion: @escaping (Result<Data, CalAPIError>) -> Void) {
        var req = URLRequest(url: url)
        req.httpMethod = method
        if let token, !token.isEmpty {
            req.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        }
        if let body {
            req.httpBody = body
            req.setValue("application/json", forHTTPHeaderField: "Content-Type")
        }
        let task = session.dataTask(with: req) { data, response, error in
            let done: (Result<Data, CalAPIError>) -> Void = { r in
                DispatchQueue.main.async { completion(r) }
            }
            if let error { done(.failure(.transport(error))); return }
            guard let http = response as? HTTPURLResponse else {
                done(.failure(.noData)); return
            }
            let payload = data ?? Data()
            if http.statusCode == 401 { done(.failure(.unauthorized)); return }
            guard (200..<300).contains(http.statusCode) else {
                let msg = (try? JSONDecoder().decode(MessageEnvelope.self, from: payload))?.message
                    ?? String(data: payload, encoding: .utf8) ?? ""
                done(.failure(.http(http.statusCode, msg)))
                return
            }
            done(.success(payload))
        }
        task.resume()
    }

    private static func decode<T: Decodable>(_ type: T.Type, _ data: Data) -> Result<T, CalAPIError> {
        do { return .success(try JSONDecoder().decode(T.self, from: data)) }
        catch { return .failure(.decode(error)) }
    }

    private static func json(_ dict: [String: Any]) -> Data? {
        try? JSONSerialization.data(withJSONObject: dict, options: [])
    }
}
