import Foundation

@main struct SessionNoticeChecks {
    static func main() {
        for text in ["logout", "/logout", "LOG OUT", " sign out "] { precondition(Session.command(for: text) == "logout") }
        precondition(Session.command(for: "Log in") == "login")
        for text in ["make a logout button", "stop the apple bouncing", "publish a painting", "buy"] { precondition(Session.command(for: text) == nil) }
        let session = Session()
        session.receive(["type":"notice", "scope":"publish", "text":"Retrying…", "working":true])
        precondition(session.entries.isEmpty && session.notices["publish"]?.working == true)
        session.receive(["type":"notice", "scope":"publish", "text":"Saved locally", "action":"publish"])
        precondition(session.notices.count == 1 && session.notices["publish"]?.action == "publish")
        session.receive(["type":"notice", "scope":"inference", "text":"Connection interrupted"])
        session.receive(["type":"notice", "scope":"publish", "text":""])
        precondition(session.notices["publish"] == nil && session.notices["inference"] != nil)
        session.receive(["type":"bridge", "method":"turn/completed", "params":["turn":["status":"failed"]]])
        precondition(session.entries.isEmpty && session.health == .failed)
        session.receive(["type":"thread", "id":"restored", "events":[
            ["type":"you", "text":"Keep my piece"],
            ["type":"bad", "text":"Publish failed at \"requesting upload grant\": Load failed"],
            ["type":"bridge", "method":"turn/completed", "params":["turn":["status":"failed", "error":["message":"Load failed"]]]]
        ]])
        precondition(session.notices.isEmpty && session.entries.count == 1 && session.entries[0].text == "Keep my piece")
        print("Notices replace in place, remain outside the notebook, clear on success/thread changes, and suppress legacy transport spam.")
    }
}
