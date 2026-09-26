import Foundation

@main
struct ProxChecks {
    @MainActor static func main() {
        let session = Session()
        assert(AeselProx.state(for: session) == "blank")
        session.busy = true
        assert(AeselProx.state(for: session) == "working")
        session.approval = ProviderApproval(id: "permission", title: "Test", detail: "test", canAccept: true, alwaysLabel: nil, alwaysScope: nil)
        assert(AeselProx.state(for: session) == "awaiting")
        session.approval = nil
        session.busy = false
        session.source = "saved piece"
        assert(AeselProx.state(for: session) == "complete")
        session.health = .failed
        assert(AeselProx.state(for: session) == "interrupted")
        session.health = .idle
        session.fatal = "bridge unavailable"
        assert(AeselProx.state(for: session) == "interrupted")
        print("Native aesel prox lifecycle checks passed")
    }
}
