import Foundation

@main struct ProviderReadinessChecks {
    static func main() {
        let session = Session()
        precondition(!session.canStartTurn)
        session.signedIn = true
        precondition(session.canStartTurn)
        session.signedIn = false
        for provider in ["claude", "codex"] {
            session.receive(["type": "providers", "selected": provider,
                             "choices": [["id": provider, "available": true]]])
            precondition(session.canStartTurn, "Local providers do not require AC login")
            session.busy = true
            precondition(!session.canStartTurn)
            session.busy = false
            session.receive(["type": "hostOperation", "operation": ["id": "pending"]])
            precondition(!session.canStartTurn, "Resolve the previous operation before another Send")
            session.receive(["type": "hostOperation"])
            precondition(session.canStartTurn)
            session.fatal = "Host page failed"
            precondition(!session.canStartTurn)
            session.fatal = nil
            session.receive(["type": "providers", "selected": provider,
                             "choices": [["id": provider, "available": false]]])
            precondition(!session.canStartTurn && session.provider == provider)
            session.signedIn = true
            precondition(!session.canStartTurn, "AC login cannot substitute for a disconnected provider")
            session.signedIn = false
        }
        session.provider = "unknown"
        precondition(!session.canStartTurn)
        print("Provider readiness preserves AC sign-in, local-provider independence, and pending-turn recovery.")
    }
}
