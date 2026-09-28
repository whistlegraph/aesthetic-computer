import Foundation
import Security

@main struct TokenPersistenceChecks {
    static func main() throws {
        let root = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        let service = "computer.aesthetic.aesel.test." + UUID().uuidString
        let store = SessionStore(directory: root, tokenService: service)
        defer { store.clearToken(); try? FileManager.default.removeItem(at: root) }
        func reference() -> Data {
            let query: [String: Any] = [kSecClass as String: kSecClassGenericPassword,
                kSecAttrService as String: service, kSecAttrAccount as String: "access-token",
                kSecReturnPersistentRef as String: true]
            var result: CFTypeRef?
            precondition(SecItemCopyMatching(query as CFDictionary, &result) == errSecSuccess)
            return result as! Data
        }
        try store.write(key: "session", value: #"{"token":"test-token-one"}"#)
        let original = reference()
        for _ in 0..<3 { try store.write(key: "session", value: #"{"token":"test-token-one"}"#) }
        precondition(reference() == original, "Repeated saves must retain the Keychain item and its ACL")
        try store.write(key: "session", value: #"{"token":"test-token-two"}"#)
        precondition(reference() == original, "Token rotation must retain the Keychain item")
        precondition(store.token() == "test-token-two")
        let disk = try String(contentsOf: root.appendingPathComponent("session.json"), encoding: .utf8)
        precondition(!disk.contains("test-token"), "Tokens must stay out of session files")
        store.clearToken(); precondition(store.token() == nil)
        print("Keychain token persistence checks passed")
    }
}
