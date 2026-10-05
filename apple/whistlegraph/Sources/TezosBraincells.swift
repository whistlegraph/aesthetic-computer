import SwiftUI
import StoreKit

/// A checkout capability can only buy credit for its already-bound AC account.
/// The app never handles a wallet key or treats a wallet callback as payment.
@MainActor final class TezosBraincells: ObservableObject {
    @Published var available = false
    @Published var busy = false
    @Published var notice = ""
    private let endpoint = URL(string: "https://aesthetic.computer/api/easel-tezos")!
    private let storageKey = "whistlegraph-tezos-checkout"
    private struct Pending: Codable { let url: URL; let handle: String }
    private struct Response: Decodable {
        let checkoutURL: URL?
        let status: String?
        let credits: Int?
        let error: String?
    }
    func prepare() async {
        #if DEBUG
        available = true
        #else
        // External digital-goods checkout links are available in the US store.
        available = await Storefront.current?.countryCode == "USA"
        #endif
    }
    private var pending: Pending? {
        guard let data = UserDefaults.standard.data(forKey: storageKey) else { return nil }
        return try? JSONDecoder().decode(Pending.self, from: data)
    }
    private func request(_ action: String, bearer: String) async throws -> Response {
        var request = URLRequest(url: endpoint)
        request.httpMethod = "POST"; request.timeoutInterval = 30
        request.setValue("Bearer \(bearer)", forHTTPHeaderField: "Authorization")
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONSerialization.data(withJSONObject: ["action": action])
        let (data, response) = try await URLSession.shared.data(for: request)
        let result = try JSONDecoder().decode(Response.self, from: data)
        guard let http = response as? HTTPURLResponse, (200..<300).contains(http.statusCode) else {
            throw NativeSignIn.failure(result.error ?? "Could not check the Tezos payment.")
        }
        return result
    }
    func buy(session: WhistlegraphSession) async {
        guard available, !busy else { return }
        busy = true; notice = ""
        defer { busy = false }
        do {
            guard !session.snapshot.handle.isEmpty, let token = try await session.account.token() else {
                throw NativeSignIn.failure("Sign in to AC before buying braincells.")
            }
            var checkout = pending?.handle == session.snapshot.handle ? pending?.url : nil
            if checkout == nil {
                let result = try await request("create", bearer: token)
                guard let url = result.checkoutURL, url.scheme == "https", url.host == "aesthetic.computer",
                      url.path == "/braincells", url.hasDirectoryPath, let secret = url.fragment, secret.count == 64,
                      secret.allSatisfy({ $0.isHexDigit }) else { throw NativeSignIn.failure("Invalid checkout link.") }
                let saved = Pending(url: url, handle: session.snapshot.handle)
                UserDefaults.standard.set(try JSONEncoder().encode(saved), forKey: storageKey)
                checkout = url
            }
            guard let checkout, await UIApplication.shared.open(checkout) else {
                throw NativeSignIn.failure("Could not open checkout in your browser.")
            }
        } catch { notice = error.localizedDescription }
    }
    func refresh(session: WhistlegraphSession) async {
        guard !busy, let saved = pending, saved.handle == session.snapshot.handle, let secret = saved.url.fragment else { return }
        busy = true
        defer { busy = false }
        do {
            let state = try await request("status", bearer: secret)
            let payment: Response
            if state.status == "created" || state.status == "credited" || state.status == "expired" { payment = state }
            else { payment = try await request("confirm", bearer: secret) }
            if payment.status == "credited" {
                UserDefaults.standard.removeObject(forKey: storageKey)
                notice = payment.credits.map { "\($0.formatted()) braincells added." } ?? "Braincells added."
                session.command("refreshBraincells")
            } else if payment.status == "expired" {
                UserDefaults.standard.removeObject(forKey: storageKey)
                notice = "The quote expired without a confirmed payment. You can start a new checkout."
            } else if payment.status == "confirming" { notice = "Waiting for Tezos confirmations…" }
        } catch { notice = error.localizedDescription }
    }
}
