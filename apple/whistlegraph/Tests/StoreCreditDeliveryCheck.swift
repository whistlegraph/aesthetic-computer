import Foundation

@main struct StoreCreditDeliveryCheck {
    @MainActor static func main() async throws {
        let owner = UUID(), other = UUID()
        let ledger = StoreCreditDelivery()
        var finished = 0, requests = 0, generation = 1
        var account = owner, failNetwork = false, wrongAck = false, wrongCredits = false
        var wrongEnvironment = false, credited = true, changeOnAccount = false, changeOnGrant = false
        func receipt(product: String = StoreCreditDelivery.productID, token: UUID? = nil,
                     revoked: Bool = false) -> StoreCreditDelivery.Receipt {
            .init(id: "1234", productID: product, accountToken: token ?? owner,
                  environment: "Sandbox", revoked: revoked, jws: "signed")
        }
        let request: (StoreCreditDelivery.Action, StoreCreditDelivery.Credential) async throws -> Data = { action, _ in
            requests += 1
            if failNetwork { throw URLError(.notConnectedToInternet) }
            switch action {
            case .account:
                if changeOnAccount { generation += 1 }
                return try JSONSerialization.data(withJSONObject: ["appAccountToken": account.uuidString])
            case .redeem(let jws):
                precondition(jws == "signed")
                if changeOnGrant { generation += 1 }
                return try JSONSerialization.data(withJSONObject: ["transactionId": wrongAck ? "5678" : "1234",
                    "credits": wrongCredits ? 0 : StoreCreditDelivery.credits, "credited": credited,
                    "environment": wrongEnvironment ? "Production" : "Sandbox"])
            }
        }
        func deliver(_ value: StoreCreditDelivery.Receipt) async -> StoreCreditDelivery.Outcome {
            await ledger.deliver(value, credential: { .init(bearer: "access", generation: generation) },
                isCurrent: { $0.generation == generation }, request: request, finish: { finished += 1 })
        }
        let first = await deliver(receipt())
        precondition(first == .added && finished == 1 && requests == 2, "Durable matching grant finishes once")
        credited = false
        let repeatGrant = await deliver(receipt())
        precondition(repeatGrant == .alreadyAdded && finished == 2, "Idempotent server grant can finish recovery")
        let before = finished
        account = other; requests = 0
        _ = await deliver(receipt())
        precondition(finished == before && requests == 1, "Wrong account never redeems or finishes")
        account = owner
        failNetwork = true
        _ = await deliver(receipt())
        precondition(finished == before, "Offline purchase remains unfinished")
        failNetwork = false; wrongAck = true
        _ = await deliver(receipt())
        precondition(finished == before, "Another transaction acknowledgement cannot finish this purchase")
        wrongAck = false; wrongCredits = true
        _ = await deliver(receipt())
        precondition(finished == before, "Incomplete grant cannot finish")
        wrongCredits = false; wrongEnvironment = true
        _ = await deliver(receipt())
        precondition(finished == before, "Environment mismatch cannot finish")
        wrongEnvironment = false; changeOnAccount = true; requests = 0
        _ = await deliver(receipt())
        precondition(finished == before && requests == 1, "Account switch before redemption prevents request")
        changeOnAccount = false; changeOnGrant = true
        _ = await deliver(receipt())
        precondition(finished == before, "Account switch while grant is in flight preserves retry")
        changeOnGrant = false
        _ = await deliver(receipt(revoked: true))
        precondition(finished == before, "Revoked transaction never finishes as delivered")
        requests = 0
        let otherProduct = await deliver(receipt(product: "unrelated.product"))
        precondition(otherProduct == .ignored && requests == 0 && finished == before, "Other products stay with their owner")
        _ = await ledger.deliver(receipt(), credential: { nil }, isCurrent: { _ in true }, request: request,
            finish: { finished += 1 })
        precondition(finished == before, "Signed-out recovery waits for sign-in")
        _ = await ledger.deliver(receipt(), credential: { .init(bearer: "access", generation: generation) },
            isCurrent: { _ in true }, request: { _, _ in Data("{}".utf8) }, finish: { finished += 1 })
        precondition(finished == before, "Malformed acknowledgement cannot finish")
        let unbound = StoreCreditDelivery.Receipt(id: "1234", productID: StoreCreditDelivery.productID,
            accountToken: nil, environment: "Sandbox", revoked: false, jws: "signed")
        requests = 0
        _ = await deliver(unbound)
        precondition(finished == before && requests == 1, "Unbound transactions cannot claim another account")

        var concurrentRequestCount = 0, release: CheckedContinuation<Void, Never>?
        let concurrent = Task { @MainActor in
            await ledger.deliver(receipt(), credential: { .init(bearer: "access", generation: generation) },
                isCurrent: { _ in true }, request: { action, auth in
                    concurrentRequestCount += 1
                    if concurrentRequestCount == 1 { await withCheckedContinuation { release = $0 } }
                    return try await request(action, auth)
                }, finish: { finished += 1 })
        }
        while release == nil { await Task.yield() }
        let duplicate = await deliver(receipt())
        precondition(duplicate == .ignored, "Purchase result and transaction update cannot redeem concurrently")
        release?.resume()
        let recovered = await concurrent.value
        precondition(recovered == .alreadyAdded && finished == before + 1 && concurrentRequestCount == 2)
        print("PASS: 15 StoreKit delivery cases; only a durable, matching-account grant finishes a transaction")
    }
}
