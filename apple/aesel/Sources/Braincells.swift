import Foundation
import Observation
import StoreKit

/// Braincells bought through the App Store. StoreKit 2 signs each purchase;
/// the app hands that signed transaction to AC (`/api/easel-iap`), which
/// verifies it with Apple and credits the account, and only then is the
/// transaction finished. Anything unfinished is retried on the next launch.
@Observable
@MainActor
final class Braincells {
    static let productID = "computer.aesthetic.easel.braincells.1m"
    static let redeemURL = URL(string: "https://aesthetic.computer/api/easel-iap")!

    var product: Product?
    var busy = false
    var notice = ""
    var storeStatus = "Loading the App Store…"

    private var token: () -> String? = { nil }
    private var credited: () -> Void = {}
    private var listener: Task<Void, Never>?

    func start(token: @escaping () -> String?, credited: @escaping () -> Void) {
        self.token = token
        self.credited = credited
        guard listener == nil else { return }
        listener = Task { [weak self] in
            for await result in Transaction.updates {
                await self?.settle(result)
            }
        }
        Task { await load() }
    }

    var price: String { product?.displayPrice ?? "$4.99" }

    func load() async {
        do {
            let products = try await Product.products(for: [Self.productID])
            product = products.first
            storeStatus = product == nil ? "Braincells are not for sale here yet." : ""
        } catch {
            storeStatus = "App Store unavailable: \(error.localizedDescription)"
        }
        for await result in Transaction.unfinished { await settle(result) }
    }

    func buy() async {
        guard !busy else { return }
        guard let product else { await load(); return }
        guard token() != nil else { notice = "Sign in to AC before adding braincells."; return }
        busy = true
        notice = ""
        defer { busy = false }
        do {
            switch try await product.purchase() {
            case .success(let result): await settle(result)
            case .userCancelled: break
            case .pending: notice = "Purchase pending approval."
            @unknown default: break
            }
        } catch {
            notice = error.localizedDescription
        }
    }

    /// Redeem a verified transaction with AC, then finish it.
    private func settle(_ result: VerificationResult<Transaction>) async {
        guard case .verified(let transaction) = result else { return }
        guard transaction.productID == Self.productID, transaction.revocationDate == nil else {
            await transaction.finish()
            return
        }
        guard let token = token() else {
            notice = "Sign in to AC to add the braincells you bought."
            return
        }
        var request = URLRequest(url: Self.redeemURL)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.setValue("Bearer \(token)", forHTTPHeaderField: "Authorization")
        request.httpBody = try? JSONSerialization.data(withJSONObject: ["jws": result.jwsRepresentation])
        do {
            let (data, response) = try await URLSession.shared.data(for: request)
            let status = (response as? HTTPURLResponse)?.statusCode ?? 0
            let body = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any]
            switch status {
            case 200:
                await transaction.finish()
                notice = body?["credited"] as? Bool == true ? "1,000,000 braincells added." : "Braincells already added."
                credited()
            case 400, 401:
                // Apple would not verify it for this account; keep it for a retry after sign-in.
                notice = body?["error"] as? String ?? "Could not add braincells."
            default:
                notice = body?["error"] as? String ?? "AC is busy; the purchase will be retried."
            }
        } catch {
            notice = "Could not reach AC; the purchase will be retried."
        }
    }
}
