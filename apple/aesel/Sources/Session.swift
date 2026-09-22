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

struct ModelChoice: Identifiable {
    let id: String
    let title: String
    let premium: Bool
}

struct ProviderChoice: Identifiable {
    let id: String
    let available: Bool
    let notice: String
}

struct ProviderApproval: Identifiable {
    let id: String
    let detail: String
}

struct SessionNotice: Identifiable {
    let id = UUID()
    let text: String
    let action: String?
    let working: Bool
}

struct InspectedRevision { let version: Int; let source: String }

struct SourceRevision: Identifiable {
    let id: Int
    let reason: String
    let at: String
}

struct SessionSummary: Identifiable {
    let id: String
    let title: String
    let medium: String
    let route: String
    let updatedAt: String
}

/// The whole of what SwiftUI observes. It holds no logic about turns, tools or
/// publishing — those live in `easel/phone/session.mjs`, shared with the
/// desktop. This is the projection of that session's event stream into
/// something a view can draw.
@Observable
final class Session {
    static func command(for text: String) -> String? {
        let normalized = text.trimmingCharacters(in: .whitespacesAndNewlines).lowercased()
        let aliases = ["logout":"logout", "log out":"logout", "signout":"logout", "sign out":"logout",
                       "login":"login", "log in":"login", "signin":"login", "sign in":"login",
                       "settings":"settings", "help":"help", "stop":"stop", "publish":"publish",
                       "open":"open", "new":"new", "home":"home"]
        if normalized.hasPrefix("/") {
            let name = String(normalized.dropFirst())
            return name == "buy" ? "buy" : aliases[name]
        }
        return aliases[normalized]
    }

    static let draftPreviewURL = embeddedPreviewURL(URL(string: "https://aesthetic.computer/wipe?noauth=true")!)
    static func embeddedPreviewURL(_ url: URL) -> URL {
        guard var target = URLComponents(url: url, resolvingAgainstBaseURL: false) else { return url }
        if target.host == "prompt.ac" { target.host = "aesthetic.computer" }
        let flags = ["nogap", "nolabel", "autoreload"]
        var query = (target.queryItems ?? []).filter { !flags.contains($0.name) }
        query += flags.map { URLQueryItem(name: $0, value: "true") }
        target.queryItems = query
        return target.url ?? url
    }
    var notices: [String: SessionNotice] = [:]
    var entries: [Entry] = []
    var history: [SessionSummary] = []
    var medium = "piece"
    var model = ""
    var modelChoices: [ModelChoice] = []
    var provider = "ac"
    var providers: [ProviderChoice] = []
    var approval: ProviderApproval?
    var hostOperationID: String?
    var providerNotice: String { providers.first { $0.id == provider }?.notice ?? "" }
    var providerReady: Bool {
        provider == "ac" ? signedIn : providers.contains { $0.id == provider && $0.available }
    }
    var canStartTurn: Bool { providerReady && !busy && hostOperationID == nil && fatal == nil }
    var reportedModel = ""
    var currentThreadID = ""
    var currentSessionID: String { currentThreadID }
    var status: String = "starting"
    var health: Health = .idle
    var route: String = ""
    var previewURL: URL? = Session.draftPreviewURL
    var shareURL: URL?
    var source = ""
    var composer = ""
    var revisions: [SourceRevision] = []
    var inspectedRevision: InspectedRevision?
    var currentRevision = 0
    var pieceVersion: Int { currentRevision }
    var publishedRevision: Int?
    var autoPublish = true
    var exportJSON: String?
    var showSignIn = false
    var signInLoading = false
    var signInError: String?
    var busy = false
    var signedIn = false
    var handle = ""
    var handleColors: [String] = []
    var braincells: Double?
    var braincellDollars: Double?
    var freeDollars: Double?
    var purchasedDollars: Double?
    var creditsStatus = "Sign in to view braincells"
    /// Set when the host page itself could not start — a missing dev server is
    /// the usual cause, and a blank screen is a bad way to say so.
    var fatal: String?

    /// The `<li>` currently receiving streamed deltas. Held as an index because
    /// `Entry` is a value type and SwiftUI needs the array mutated in place.
    private var streamingIndex: Int?

    func reset() {
        notices.removeAll()
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
        case "revisionPreview":
            guard event["threadID"] as? String == currentSessionID,
                  let version = event["version"] as? Int, let source = event["source"] as? String else { return }
            inspectedRevision = InspectedRevision(version: version, source: source)
        case "export":
            exportJSON = event["json"] as? String
        case "revisions":
            revisions = (event["items"] as? [[String: Any]] ?? []).compactMap { item in
                guard let version = item["version"] as? Int else { return nil }
                return SourceRevision(id: version, reason: item["summary"] as? String ?? item["reason"] as? String ?? "", at: item["at"] as? String ?? "")
            }
            currentRevision = event["current"] as? Int ?? 0
            publishedRevision = event["published"] as? Int
            autoPublish = event["autoPublish"] as? Bool ?? true
        case "publication":
            shareURL = (event["url"] as? String).flatMap(URL.init(string:))
        case "providers":
            provider = event["selected"] as? String ?? provider
            providers = (event["choices"] as? [[String: Any]] ?? []).compactMap { value in
                guard let id = value["id"] as? String else { return nil }
                return ProviderChoice(id: id, available: value["available"] as? Bool ?? false, notice: value["notice"] as? String ?? "")
            }
        case "hostOperation":
            hostOperationID = (event["operation"] as? [String: Any])?["id"] as? String
        case "approval":
            if let value = event["approval"] as? [String: Any], let id = value["id"] as? String {
                let params = value["params"] as? [String: Any] ?? [:]
                let detail = params["command"] as? String ?? params["reason"] as? String ?? value["method"] as? String ?? "Provider action"
                approval = ProviderApproval(id: id, detail: detail)
            } else { approval = nil }
        case "credits":
            braincells = event["total"] as? Double
            let dollars = event["dollars"] as? [String: Any]
            braincellDollars = dollars?["total"] as? Double
            freeDollars = dollars?["free"] as? Double
            purchasedDollars = dollars?["purchased"] as? Double
            creditsStatus = event["status"] as? String ?? "Braincells unavailable"

        case "model":
            model = event["requested"] as? String ?? model
            reportedModel = event["reported"] as? String ?? ""
            if let choices = event["choices"] as? [[String: Any]] {
                modelChoices = choices.compactMap { choice in
                    guard let id = choice["id"] as? String else { return nil }
                    return ModelChoice(id: id, title: choice["title"] as? String ?? id, premium: choice["premium"] as? Bool ?? false)
                }
            }

        case "history":
            history = (event["items"] as? [[String: Any]] ?? []).compactMap { item in
                guard let id = item["id"] as? String else { return nil }
                return SessionSummary(id: id, title: item["title"] as? String ?? "Untitled",
                    medium: item["medium"] as? String ?? "piece", route: item["route"] as? String ?? "",
                    updatedAt: item["updatedAt"] as? String ?? "")
            }

        case "thread":
            inspectedRevision = nil
            composer = event["composer"] as? String ?? ""
            reportedModel = ""
            currentThreadID = event["id"] as? String ?? ""
            medium = event["medium"] as? String ?? "piece"
            reset()
            for entry in event["events"] as? [[String: Any]] ?? [] {
                if ["you", "note", "bad", "bridge"].contains(entry["type"] as? String ?? "") { receive(entry) }
            }

        case "hostError":
            fatal = event["text"] as? String ?? "The session could not start."

        case "ready", "restored":
            signedIn = event["signedIn"] as? Bool ?? false
            if type == "restored" { status = signedIn ? "ready" : "signed out" }

        case "signedIn":
            braincells = nil
            braincellDollars = nil
            freeDollars = nil
            purchasedDollars = nil
            creditsStatus = "Loading braincells"
            signedIn = true
            handle = event["handle"] as? String ?? ""
            if handle.isEmpty {
                append(.note, "Signed in, but this account has no @handle yet. Claim one at aesthetic.computer/handle to publish.")
            }

        case "handleColors":
            if event["handle"] as? String == handle { handleColors = event["colors"] as? [String] ?? [] }

        case "signedOut":
            braincells = nil
            braincellDollars = nil
            freeDollars = nil
            purchasedDollars = nil
            creditsStatus = "Sign in to view braincells"
            signedIn = false
            handle = ""
            handleColors = []
            status = "signed out"

        case "source":
            currentRevision = event["version"] as? Int ?? currentRevision
            source = event["source"] as? String ?? source
            previewURL = Self.draftPreviewURL

        case "piece":
            currentRevision = event["version"] as? Int ?? 0
            source = event["source"] as? String ?? source
            shareURL = nil
            previewURL = Self.draftPreviewURL
            route = event["route"] as? String ?? ""

        case "preview":
            if let text = event["url"] as? String {
                shareURL = URL(string: text)
                previewURL = Self.draftPreviewURL
            }

        case "notice":
            let scope = event["scope"] as? String ?? "connection"
            let text = event["text"] as? String ?? ""
            notices[scope] = text.isEmpty ? nil : SessionNotice(text: text, action: event["action"] as? String, working: event["working"] as? Bool ?? false)

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
            let text = event["text"] as? String ?? ""
            // Old transport failures stay in the saved history, not on the paper.
            if !text.hasPrefix("Publish failed at") && text != "Load failed" { append(.bad, text) }
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
        case "model/reported":
            model = params["requested"] as? String ?? model
            reportedModel = params["reported"] as? String ?? reportedModel

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
                if message != "Load failed" { append(.bad, message) }
                status = "failed"
                health = .failed
            } else if turn["status"] as? String == "failed" {
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
