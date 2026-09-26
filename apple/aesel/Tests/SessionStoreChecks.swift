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
        let multi = root.appendingPathComponent("multi")
        let a = SessionStore(directory: multi)
        let b = SessionStore(directory: multi, windowID: "second")
        try a.write(key:"session", value:#"{"threadID":"a"}"#)
        try b.write(key:"session", value:#"{"threadID":"b"}"#)
        try a.write(key:"threads", value:#"{"schema":1,"items":[{"id":"a","composer":"first"}]}"#)
        try b.write(key:"threads", value:#"{"schema":1,"items":[{"id":"b","composer":"second"}]}"#)
        try a.write(key:"session", value:#"{"threadID":"a"}"#)
        try a.write(key:"threads", value:#"{"schema":1,"items":[{"id":"a","composer":"updated"}]}"#)
        let stale = SessionStore(directory: multi, windowID: "third")
        try a.write(key:"threads", value:#"{"schema":1,"items":[{"id":"a","composer":"updated again"}]}"#)
        try stale.write(key:"session", value:#"{"threadID":"c"}"#)
        try stale.write(key:"threads", value:#"{"schema":1,"items":[{"id":"a","composer":"updated"},{"id":"b","composer":"second"},{"id":"c","composer":"third"}]}"#)
        let all = SessionStore(directory: multi).seedJSON()!
        precondition(all.contains("updated again") && all.contains("second") && all.contains("third"))
        func pointer(_ store: SessionStore) throws -> String {
            let seed = try JSONSerialization.jsonObject(with: Data(store.seedJSON()!.utf8)) as! [String:String]
            let session = try JSONSerialization.jsonObject(with: Data(seed["session"]!.utf8)) as! [String:Any]
            return session["threadID"] as? String ?? ""
        }
        precondition(try! pointer(SessionStore(directory: multi)) == "a")
        precondition(try! pointer(SessionStore(directory: multi, windowID:"second")) == "b")
        precondition(try! pointer(SessionStore(directory: multi, windowID:"new")) == "")
        print("Session checkpoint recovery checks passed")
    }
}
