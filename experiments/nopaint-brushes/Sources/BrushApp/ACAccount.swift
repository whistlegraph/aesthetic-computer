import Foundation
import Security

struct ACIdentity: Codable { var label: String; var id: String }
struct ACLoginSession: Codable { var accessToken: String; var account: ACIdentity }
struct ACPairing: Codable { var code: String; var pollSecret: String }
struct ACPoll: Decodable { var status: String; var session: ACLoginSession? }
enum ACLoginError: LocalizedError {
    case message(String)
    var errorDescription: String? { if case .message(let text) = self { return text }; return nil }
}

enum ACKeychain {
    static var query: [String: Any] { [kSecClass as String: kSecClassGenericPassword,
        kSecAttrService as String: "computer.aesthetic.nopaint.account", kSecAttrAccount as String: "session"] }
    static func read() -> Data? {
        var q = query; q[kSecReturnData as String] = true; q[kSecMatchLimit as String] = kSecMatchLimitOne
        var item: CFTypeRef?
        guard SecItemCopyMatching(q as CFDictionary, &item) == errSecSuccess else { return nil }
        return item as? Data
    }
    static func write(_ data: Data) throws {
        let values: [String: Any] = [kSecValueData as String: data,
            kSecAttrAccessible as String: kSecAttrAccessibleWhenUnlockedThisDeviceOnly]
        let status = SecItemUpdate(query as CFDictionary, values as CFDictionary)
        if status == errSecItemNotFound {
            let added = SecItemAdd(query.merging(values) { _, new in new } as CFDictionary, nil)
            guard added == errSecSuccess else { throw ACLoginError.message("Could not save the AC session in Keychain (\(added)).") }
        } else if status != errSecSuccess { throw ACLoginError.message("Could not update the AC session in Keychain (\(status)).") }
    }
    static func remove() throws {
        let status = SecItemDelete(query as CFDictionary)
        guard status == errSecSuccess || status == errSecItemNotFound else {
            throw ACLoginError.message("Could not remove the AC session from Keychain (\(status)).")
        }
    }
}

// Uses AC's existing browser pairing boundary: AC identity only, never the
// native provisioning flow that can deliver unrelated device credentials.
@MainActor final class ACAccount {
    typealias Transport = (URLRequest) async throws -> (Data, HTTPURLResponse)
    var session: ACLoginSession?
    let transport: Transport
    let save: (Data) throws -> Void
    let remove: () throws -> Void
    let delay: () async throws -> Void
    init(transport: @escaping Transport = { request in
        let (data, response) = try await URLSession.shared.data(for: request)
        guard let http = response as? HTTPURLResponse else { throw ACLoginError.message("Invalid login response.") }
        return (data, http)
    }, load: () -> Data? = ACKeychain.read, save: @escaping (Data) throws -> Void = ACKeychain.write,
         remove: @escaping () throws -> Void = ACKeychain.remove,
         delay: @escaping () async throws -> Void = { try await Task.sleep(for: .seconds(2)) }) {
        self.transport = transport; self.save = save; self.remove = remove; self.delay = delay
        if let data = load() { session = try? JSONDecoder().decode(ACLoginSession.self, from: data) }
    }
    private func request(_ url: URL, body: [String: String]? = nil) async throws -> Data {
        var request = URLRequest(url: url, cachePolicy: .reloadIgnoringLocalCacheData, timeoutInterval: 20)
        request.setValue("Mozilla/5.0 NoPaintNative/1.0", forHTTPHeaderField: "User-Agent")
        request.setValue("application/json", forHTTPHeaderField: "Accept")
        if let body {
            request.httpMethod = "POST"; request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            request.httpBody = try JSONSerialization.data(withJSONObject: body)
        }
        let (data, response) = try await transport(request)
        try Task.checkCancellation()
        guard response.statusCode == 200 else {
            throw ACLoginError.message(response.statusCode == 410 || response.statusCode == 404
                ? "This sign-in has expired. Please try again." : "AC sign-in could not finish (HTTP \(response.statusCode)).")
        }
        return data
    }
    func begin() async throws -> (ACPairing, URL) {
        let data = try await request(URL(string: "https://aesthetic.computer/api/device-pair")!, body: ["action": "create", "kind": "browser"])
        let pair = try JSONDecoder().decode(ACPairing.self, from: data)
        guard pair.code.range(of: "^[A-Z0-9]{6}$", options: .regularExpression) != nil,
              pair.pollSecret.range(of: "^[A-Za-z0-9_-]{32,128}$", options: .regularExpression) != nil else {
            throw ACLoginError.message("AC returned an invalid pairing response.")
        }
        var link = URLComponents(string: "https://aesthetic.computer/api/device-pair-login")!
        link.queryItems = [URLQueryItem(name: "code", value: pair.code), URLQueryItem(name: "kind", value: "browser")]
        return (pair, link.url!)
    }
    func complete(_ pair: ACPairing) async throws {
        var url = URLComponents(string: "https://aesthetic.computer/api/device-pair")!
        url.queryItems = [URLQueryItem(name: "code", value: pair.code), URLQueryItem(name: "secret", value: pair.pollSecret)]
        let deadline = Date().addingTimeInterval(600)
        for _ in 0..<300 {
            try Task.checkCancellation()
            guard Date() < deadline else { break }
            let data = try await request(url.url!)
            let result = try JSONDecoder().decode(ACPoll.self, from: data)
            if result.status == "claimed" {
                guard let next = result.session, !next.accessToken.isEmpty,
                      !next.account.id.isEmpty, next.account.label.hasPrefix("@") else {
                    throw ACLoginError.message("AC returned an incomplete session.")
                }
                try Task.checkCancellation()
                try save(JSONEncoder().encode(next)); session = next; return
            }
            guard result.status == "pending" else { throw ACLoginError.message("AC sign-in was not completed.") }
            try await delay()
        }
        throw ACLoginError.message("Sign-in timed out. Please try again.")
    }
    func signOut() throws { try remove(); session = nil }
}
