import Foundation
import Combine
import JavaScriptCore

/// Executes the same pure interface policy as mime.ac. Swift renders its
/// semantic projection and performs the effects; no credentials enter JS.
@MainActor final class MimeInterface: ObservableObject {
    struct Projection: Codable {
        struct Brand: Codable { let name: String; let slogan: String; let title: String }
        struct Action: Codable, Identifiable { let id: String; let label: String }
        struct Account: Codable {
            let title: String; let status: String; let busy: Bool; let notice: String
            let actions: [Action]
        }
        struct Media: Codable {
            let active: Bool; let viewerHandle: String?
            enum CodingKeys: String, CodingKey { case active, viewerHandle }
            func encode(to encoder: Encoder) throws {
                var values = encoder.container(keyedBy: CodingKeys.self)
                try values.encode(active, forKey: .active)
                try values.encode(viewerHandle, forKey: .viewerHandle)
            }
        }
        struct Creation: Codable { let typeAction: String; let versionAction: String }
        let schemaVersion: Int
        let brand: Brand
        let screen: String
        let logo: Action
        let account: Account
        let media: Media
        let creation: Creation
    }
    struct Effect: Codable {
        let type: String; let signUp: Bool?
        let mimeType: String?; let post: String?; let version: Int?
    }
    private struct Result: Codable { let view: Projection; let effects: [Effect] }
    private let context: JSContext
    private let policy: JSValue
    private var state: [String: Any] = [:]
    @Published private(set) var presentation: Projection

    init(sourceURL: URL? = Bundle.main.url(forResource: "interface", withExtension: "js")) {
        guard let sourceURL, let source = try? String(contentsOf: sourceURL, encoding: .utf8),
              let context = JSContext() else { preconditionFailure("MIME interface policy is missing") }
        self.context = context
        context.evaluateScript(source, withSourceURL: sourceURL)
        guard context.exception == nil, let policy = context.objectForKeyedSubscript("MimeInterface"), !policy.isUndefined,
              let value = policy.invokeMethod("dispatch", withArguments: [[:], [:]])?.toDictionary(),
              let data = try? JSONSerialization.data(withJSONObject: value),
              let result = try? JSONDecoder().decode(Result.self, from: data), result.view.schemaVersion == 1 else {
            preconditionFailure("MIME interface policy is incompatible")
        }
        self.policy = policy
        presentation = result.view
        state = value["state"] as? [String: Any] ?? [:]
    }

    @discardableResult func send(_ type: String, values: [String: Any] = [:]) -> [Effect] {
        var event = values; event["type"] = type
        guard let value = policy.invokeMethod("dispatch", withArguments: [state, event])?.toDictionary(),
              context.exception == nil, let next = value["state"] as? [String: Any],
              let data = try? JSONSerialization.data(withJSONObject: value),
              let result = try? JSONDecoder().decode(Result.self, from: data), result.view.schemaVersion == 1 else {
            assertionFailure("MIME interface transition failed"); return []
        }
        state = next; presentation = result.view
        return result.effects
    }
}
