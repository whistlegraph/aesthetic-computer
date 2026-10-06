import Foundation

/// The server owns the balance. StoreKit remains the retry queue until the
/// matching AC account acknowledges a durable grant for this transaction.
@MainActor final class StoreCreditDelivery {
    nonisolated static let productID = "computer.aesthetic.walkieware.braincells.1m"
    nonisolated static let credits = 1_000_000
    struct Receipt {
        let id: String
        let productID: String
        let accountToken: UUID?
        let environment: String
        let revoked: Bool
        let jws: String
    }
    struct Credential {
        let bearer: String
        let generation: Int
    }
    enum Action { case account, redeem(String) }
    enum Outcome: Equatable {
        case added, alreadyAdded, ignored, deferred(String)
    }
    private struct Account: Decodable { let appAccountToken: UUID }
    private struct Grant: Decodable {
        let credited: Bool
        let transactionId: String
        let credits: Int
        let environment: String
    }
    private var inFlight = Set<String>()

    func deliver(_ receipt: Receipt,
                 credential: () async throws -> Credential?,
                 isCurrent: (Credential) -> Bool,
                 request: (Action, Credential) async throws -> Data,
                 finish: () async -> Void) async -> Outcome {
        guard receipt.productID == Self.productID else { return .ignored }
        guard !receipt.revoked else { return .deferred("This purchase was revoked. Contact support if the balance is incorrect.") }
        guard inFlight.insert(receipt.id).inserted else { return .ignored }
        defer { inFlight.remove(receipt.id) }
        do {
            guard let auth = try await credential(), isCurrent(auth) else {
                return .deferred("Sign in to the AC account used for this purchase to add your braincells.")
            }
            let account = try JSONDecoder().decode(Account.self, from: await request(.account, auth))
            guard isCurrent(auth) else { return .deferred("Your account changed. Sign in to the purchasing account and retry.") }
            guard receipt.accountToken == account.appAccountToken else {
                return .deferred("This purchase belongs to another AC account. Sign in to that account and retry.")
            }
            let grant = try JSONDecoder().decode(Grant.self, from: await request(.redeem(receipt.jws), auth))
            guard grant.transactionId == receipt.id, grant.credits == Self.credits,
                  ["Production", "Sandbox"].contains(grant.environment), grant.environment == receipt.environment else {
                return .deferred("AC has not confirmed this purchase. Your purchase is saved for retry.")
            }
            guard isCurrent(auth) else { return .deferred("Your account changed. Sign in to the purchasing account and retry.") }
            await finish()
            return grant.credited ? .added : .alreadyAdded
        } catch {
            return .deferred("Your purchase is saved for retry. " + error.localizedDescription)
        }
    }
}
