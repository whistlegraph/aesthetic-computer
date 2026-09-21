import Foundation

@main struct SessionStoreChecks {
    static func main() throws {
        let root = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        defer { try? FileManager.default.removeItem(at: root) }
        let store = SessionStore(directory: root)
        try store.write(key: "draft", value: "first")
        try store.write(key: "draft", value: "second")
        let reopened = SessionStore(directory: root)
        precondition(reopened.seedJSON()!.contains("second"))
        try Data("broken".utf8).write(to: root.appendingPathComponent("session.json"))
        let recovered = SessionStore(directory: root)
        precondition(recovered.issue != nil && recovered.seedJSON()!.contains("first"))
        try recovered.write(key: "draft", value: "third")
        precondition(SessionStore(directory: root).seedJSON()!.contains("third"))
        try Data("broken".utf8).write(to: root.appendingPathComponent("session.json"))
        try Data("broken".utf8).write(to: root.appendingPathComponent("session.previous.json"))
        let blocked = SessionStore(directory: root)
        do { try blocked.write(key: "draft", value: "lost"); preconditionFailure("must retain corrupt checkpoints") } catch {}
        let retained = try String(contentsOf: root.appendingPathComponent("session.json"), encoding: .utf8)
        precondition(retained == "broken")
        print("Session checkpoint recovery checks passed")
    }
}
