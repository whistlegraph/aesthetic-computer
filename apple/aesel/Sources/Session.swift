import Foundation
import Observation

/// One line in the transcript. The renderer draws these and nothing else, so
/// everything the bridge reports has to become one of them or be dropped on
/// purpose.
struct Entry: Identifiable, Equatable {
    enum Kind: Equatable {
        case you
        case ac
        case edit
        case note
        case bad
    }

    let id = UUID()
    var kind: Kind
    var text: String
}

/// What the status pill says, and in which colour.
enum Health: Equatable {
    case idle
    case working
    case live
    case failed
}

/// The whole of what SwiftUI observes. It holds no logic about turns, tools or
/// publishing — those live in `easel/phone/session.mjs`, shared with the
/// desktop. This is the projection of that session's event stream into
/// something a view can draw.
@Observable
final class Session {
    var entries: [Entry] = []
    var status: String = "starting"
    var health: Health = .idle
    var route: String = ""
    var previewURL: URL?
    var busy = false
    var signedIn = false
    var handle = ""
    /// Set when the host page itself could not start — a missing dev server is
    /// the usual cause, and a blank screen is a bad way to say so.
    var fatal: String?

    /// The `<li>` currently receiving streamed deltas. Held as an index because
    /// `Entry` is a value type and SwiftUI needs the array mutated in place.
    private var streamingIndex: Int?

    func reset() {
        entries.removeAll()
        streamingIndex = nil
    }

    func append(_ kind: Entry.Kind, _ text: String) {
        entries.append(Entry(kind: kind, text: text))
        streamingIndex = nil
    }

    // MARK: - Events from the shared session

    /// Mirrors what `app.mjs` renders in the browser, so the two clients agree
    /// on what a turn looks like.
    func receive(_ event: [String: Any]) {
        guard let type = event["type"] as? String else { return }
        switch type {
        case "ready", "restored":
            signedIn = event["signedIn"] as? Bool ?? false
            if type == "restored" { status = signedIn ? "ready" : "signed out" }

        case "signedIn":
            signedIn = true
            handle = event["handle"] as? String ?? ""
            if handle.isEmpty {
                append(.note, "Signed in, but this account has no @handle yet. Claim one at aesthetic.computer/handle to publish.")
            }

        case "piece":
            route = event["route"] as? String ?? ""

        case "preview":
            if let text = event["url"] as? String { previewURL = URL(string: text) }

        case "status":
            status = event["text"] as? String ?? status
            health = Self.health(event["kind"] as? String)

        case "busy":
            busy = event["busy"] as? Bool ?? false

        case "you":
            append(.you, event["text"] as? String ?? "")

        case "note":
            append(.note, event["text"] as? String ?? "")

        case "bad":
            append(.bad, event["text"] as? String ?? "")
            health = .failed

        case "bridge":
            bridge(event)

        default:
            // `persist` is handled by the host before it reaches here, and an
            // unknown type is ignored rather than drawn, so a new event kind
            // cannot garble the transcript.
            break
        }
    }

    /// The six notification kinds `AcServer` emits. This is Aesel's equivalent
    /// of Oskiewar's triangle stream: the contract a renderer binds to.
    private func bridge(_ event: [String: Any]) {
        let method = event["method"] as? String ?? ""
        let params = event["params"] as? [String: Any] ?? [:]

        switch method {
        case "turn/started":
            streamingIndex = nil
            status = "thinking"
            health = .working

        case "turn/progress":
            if let phase = params["phase"] as? String {
                status = phase
                health = .working
            }

        case "item/agentMessage/delta":
            let delta = params["delta"] as? String ?? ""
            if let index = streamingIndex, entries.indices.contains(index) {
                entries[index].text += delta
            } else {
                entries.append(Entry(kind: .ac, text: delta))
                streamingIndex = entries.count - 1
            }

        case "item/completed":
            guard let item = params["item"] as? [String: Any] else { break }
            let kind = item["type"] as? String
            if kind == "agentMessage" {
                // The delta stream already drew it; this is the same text
                // arriving whole, so only draw it if nothing streamed.
                if streamingIndex == nil { append(.ac, item["text"] as? String ?? "") }
                streamingIndex = nil
            } else if kind == "fileChange" {
                let state = item["status"] as? String ?? "written"
                append(state.hasPrefix("failed") ? .bad : .edit, state)
            }

        case "turn/completed":
            streamingIndex = nil
            let turn = params["turn"] as? [String: Any] ?? [:]
            if let error = turn["error"] as? [String: Any],
               let message = error["message"] as? String {
                append(.bad, message)
                status = "failed"
                health = .failed
            } else if turn["status"] as? String == "interrupted" {
                append(.note, "Stopped.")
                status = "stopped"
                health = .idle
            } else {
                status = "ready"
                health = .idle
            }

        default:
            break
        }
    }

    private static func health(_ kind: String?) -> Health {
        switch kind {
        case "working": return .working
        case "live": return .live
        case "failed": return .failed
        default: return .idle
        }
    }
}
