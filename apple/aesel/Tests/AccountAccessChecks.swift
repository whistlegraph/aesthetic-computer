import Foundation

@main
struct AccountAccessChecks {
    static func main() {
        let session = Session()
        for provider in ["ac", "claude", "codex"] {
            session.provider = provider
            session.providers = [ProviderChoice(id: provider, available: true, notice: "")]
            session.receive(["type":"signedOut"])
            precondition(!session.accountReady && !session.canStartTurn)
            session.receive(["type":"signedIn", "handle":""])
            precondition(!session.accountReady && !session.canStartTurn)
            session.receive(["type":"signedIn", "handle":"test"])
            precondition(session.accountReady && session.canStartTurn)
            session.receive(["type":"accountRequired", "signedIn":false, "text":"Expired"])
            precondition(!session.accountReady && !session.canStartTurn)
        }
        print("All providers require AC login and a handle.")
    }
}
