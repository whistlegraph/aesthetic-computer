import SwiftUI
import StoreKit

@MainActor final class WhistlegraphBraincells: ObservableObject {
    @Published private(set) var product: Product?
    @Published private(set) var busy = false
    @Published private(set) var notice = ""
    @Published private(set) var storeStatus = "Loading the App Store…"
    private let endpoint = URL(string: "https://aesthetic.computer/api/whistlegraph-iap")!
    private weak var session: WhistlegraphSession?
    private let delivery = StoreCreditDelivery()
    private var listener: Task<Void, Never>?
    private var recovering = false
    private var loading = false

    deinit { listener?.cancel() }

    func start(session: WhistlegraphSession) async {
        self.session = session
        if listener == nil {
            listener = Task { [weak self] in
                for await result in StoreKit.Transaction.updates {
                    guard !Task.isCancelled else { break }
                    await self?.settle(result)
                }
            }
        }
        async let catalog: Void = load()
        await recover()
        await catalog
    }

    func load() async {
        guard !loading else { return }
        loading = true; defer { loading = false }
        do {
            let products = try await Product.products(for: [StoreCreditDelivery.productID])
            product = products.first { $0.id == StoreCreditDelivery.productID && $0.type == .consumable }
            storeStatus = product == nil ? "Braincell purchases are unavailable in the App Store right now." : ""
        } catch {
            product = nil
            storeStatus = "Could not load App Store purchases. Try again."
        }
    }

    func recover() async {
        guard !recovering else { return }
        recovering = true; defer { recovering = false }
        for await result in StoreKit.Transaction.unfinished { await settle(result) }
    }

    func accountChanged() async {
        notice = ""
        await recover()
    }

    private func credential() async throws -> StoreCreditDelivery.Credential? {
        guard let session else { return nil }
        let generation = session.account.generation
        guard let bearer = try await session.account.token(), generation == session.account.generation else { return nil }
        return .init(bearer: bearer, generation: generation)
    }
    private func isCurrent(_ credential: StoreCreditDelivery.Credential) -> Bool {
        session?.account.generation == credential.generation
    }
    private func request(_ action: StoreCreditDelivery.Action, _ credential: StoreCreditDelivery.Credential) async throws -> Data {
        var request = URLRequest(url: endpoint)
        request.httpMethod = "POST"; request.timeoutInterval = 30
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.setValue("Bearer \(credential.bearer)", forHTTPHeaderField: "Authorization")
        let body: [String: String]
        switch action {
        case .account: body = ["action": "account"]
        case .redeem(let jws): body = ["action": "redeem", "jws": jws]
        }
        request.httpBody = try JSONSerialization.data(withJSONObject: body)
        let (data, response) = try await URLSession.shared.data(for: request)
        guard let response = response as? HTTPURLResponse, response.statusCode == 200 else {
            let body = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any]
            throw NativeSignIn.failure(body?["error"] as? String ?? "AC could not confirm the purchase.")
        }
        return data
    }

    func buy() async {
        guard !busy, let product else { return }
        busy = true; notice = ""; defer { busy = false }
        do {
            guard let auth = try await credential(), isCurrent(auth) else {
                notice = "Sign in to AC before buying braincells."; return
            }
            struct Account: Decodable { let appAccountToken: UUID }
            let account = try JSONDecoder().decode(Account.self, from: await request(.account, auth))
            guard isCurrent(auth) else { notice = "Your account changed. Try again."; return }
            switch try await product.purchase(options: [.appAccountToken(account.appAccountToken)]) {
            case .success(let result): await settle(result)
            case .userCancelled: break
            case .pending: notice = "Purchase pending approval. Braincells will be added after approval."
            @unknown default: notice = "The App Store has not completed this purchase."
            }
        } catch { notice = error.localizedDescription }
    }

    private func settle(_ result: VerificationResult<StoreKit.Transaction>) async {
        guard case .verified(let transaction) = result else {
            notice = "The App Store could not verify this purchase. It remains saved for retry."; return
        }
        let receipt = StoreCreditDelivery.Receipt(id: String(transaction.id), productID: transaction.productID,
            accountToken: transaction.appAccountToken, environment: transaction.environment.rawValue,
            revoked: transaction.revocationDate != nil, jws: result.jwsRepresentation)
        let outcome = await delivery.deliver(receipt,
            credential: { try await self.credential() }, isCurrent: { self.isCurrent($0) },
            request: { try await self.request($0, $1) }, finish: { await transaction.finish() })
        switch outcome {
        case .added: notice = "1,000,000 braincells added."; session?.command("refreshBraincells")
        case .alreadyAdded: notice = "This purchase is already in your balance."; session?.command("refreshBraincells")
        case .deferred(let reason): notice = reason
        case .ignored: break
        }
    }
}

struct BraincellPurchase: View {
    @ObservedObject var purchase: WhistlegraphBraincells
    let signedIn: Bool
    var body: some View {
        if let product = purchase.product {
            Button {
                Task { await purchase.buy() }
            } label: {
                HStack {
                    Text("1,000,000 braincells")
                    Spacer()
                    if purchase.busy { ProgressView() } else { Text(product.displayPrice) }
                }
            }.disabled(purchase.busy || !signedIn).accessibilityIdentifier("brain-buy-app-store")
            Text("For AC inference. Purchased braincells do not expire.").font(.footnote).foregroundStyle(.secondary)
        } else {
            Text(purchase.storeStatus).font(.footnote).foregroundStyle(.secondary)
                .accessibilityIdentifier("brain-app-store-status")
            Button("Reload App Store purchases") { Task { await purchase.load() } }
        }
        if !purchase.notice.isEmpty { Text(purchase.notice).font(.footnote).accessibilityIdentifier("brain-purchase-notice") }
        Button("Check pending purchases") { Task { await purchase.recover() } }.disabled(purchase.busy)
        if !signedIn { Text("Sign in to AC to buy braincells.").font(.footnote).foregroundStyle(.secondary) }
    }
}
