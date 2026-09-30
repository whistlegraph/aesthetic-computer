import Foundation
import Security

/// One atomic session file: iOS Documents or the Mac app's Application Support directory.
final class SessionStore {
    private let url: URL
    private var values: [String: String]
    private let sessionKey: String
    private let tokenService: String
    // All windows share one credential read. In particular, SwiftUI body
    // evaluation must not open another Keychain request while one is pending.
    private enum TokenRead { case pending, loaded(String?) }
    private static var tokenReads: [String: TokenRead] = [:]

    private let backup: URL
    private var writable = true
    private(set) var issue: String?

    init(directory override: URL? = nil, windowID: String = "main", tokenService: String? = nil) {
        self.tokenService = tokenService ?? Self.keychainService
        sessionKey = windowID == "main" ? "session" : "session.\(windowID)"
        #if os(macOS)
        let standard = FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask)[0]
            .appendingPathComponent(Bundle.main.bundleIdentifier ?? "computer.aesthetic.aesel.native", isDirectory: true)
        #else
        let standard = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
        #endif
        let directory = override ?? standard
        url = directory.appendingPathComponent("session.json")
        backup = directory.appendingPathComponent("session.previous.json")
        values = [:]
        do {
            try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
            if FileManager.default.fileExists(atPath: url.path) {
                do { values = try Self.read(url) }
                catch {
                    values = try Self.read(backup)
                    issue = "Recovered the previous saved notebook after an unreadable checkpoint."
                }
            } else if FileManager.default.fileExists(atPath: backup.path) {
                values = try Self.read(backup)
            }
            // Legacy JSON tokens are removed from both checkpoint generations.
            values = try Self.sanitized(values)
            if FileManager.default.fileExists(atPath: url.path), issue == nil {
                try JSONEncoder().encode(values).write(to: url, options: .atomic)
            }
            if FileManager.default.fileExists(atPath: backup.path), let previous = try? Self.read(backup) {
                try JSONEncoder().encode(Self.sanitized(previous)).write(to: backup, options: .atomic)
            }
        } catch {
            writable = false
            issue = "The saved notebook could not be opened. Its files have been retained; repair or restore them before editing."
        }
    }

    private static func read(_ file: URL) throws -> [String: String] {
        try JSONDecoder().decode([String: String].self, from: Data(contentsOf: file))
    }

    private static func sanitized(_ input: [String: String]) throws -> [String: String] {
        var result = input
        for key in result.keys.filter({ $0 == "session" || $0.hasPrefix("session.") }) {
        if let text = result[key], let data = text.data(using: .utf8) {
            guard var record = try JSONSerialization.jsonObject(with: data) as? [String: Any] else {
                throw NSError(domain: "AeselStore", code: 2, userInfo: [NSLocalizedDescriptionKey: "Invalid session checkpoint"])
            }
            record.removeValue(forKey: "token")
            result[key] = String(data: try JSONSerialization.data(withJSONObject: record), encoding: .utf8)
        }
        }
        return result
    }

    func write(key: String, value: String) throws {
        guard writable else {
            throw NSError(domain: "AeselStore", code: 1, userInfo: [NSLocalizedDescriptionKey: issue ?? "Storage unavailable"])
        }
        // Each WebKit session holds its own snapshot. Merge only changed threads
        // into the latest file so one window cannot erase another window's save.
        let localThreads = values["threads"]
        var next = (try? Self.read(url)) ?? values
        let previous = next
        if key == "threads" {
            func threads(_ text: String?) throws -> [[String: Any]] {
                guard let text, let data = text.data(using: .utf8) else { return [] }
                let json = try JSONSerialization.jsonObject(with: data) as? [String: Any]
                return json?["items"] as? [[String: Any]] ?? []
            }
            let before = try threads(values[key])
            let incoming = try threads(value)
            var merged = try threads(next[key])
            for item in incoming {
                guard let id = item["id"] as? String else { continue }
                let old = before.first { $0["id"] as? String == id }
                if let old, NSDictionary(dictionary: old).isEqual(to: item) { continue }
                merged.removeAll { $0["id"] as? String == id }
                merged.append(item)
            }
            next[key] = String(data: try JSONSerialization.data(withJSONObject: ["schema": 1, "items": merged]), encoding: .utf8)
        } else {
            next[key == "session" ? sessionKey : key] = value
        }
        next = try Self.sanitized(next)
        // Keep the last valid, credential-free checkpoint before replacing it.
        try JSONEncoder().encode(previous).write(to: backup, options: .atomic)
        try JSONEncoder().encode(next).write(to: url, options: .atomic)
        // Keep this writer's baseline for change detection, not the merged array
        // that its JavaScript has not seen yet.
        values = next
        values["threads"] = key == "threads" ? value : localThreads
        if key == "session", let data = value.data(using: .utf8),
           let record = try JSONSerialization.jsonObject(with: data) as? [String: Any],
           let token = record["token"] as? String { saveToken(token) }
    }

    private static var keychainService: String {
        #if os(macOS)
        return (Bundle.main.bundleIdentifier ?? "computer.aesthetic.aesel.native") + ".session"
        #else
        return "computer.aesthetic.aesel.session"
        #endif
    }

    private var tokenQuery: [String: Any] {
        [kSecClass as String: kSecClassGenericPassword,
         kSecAttrService as String: tokenService,
         kSecAttrAccount as String: "access-token"]
    }

    var credentialKey: String { tokenService }
    private var renewalQuery: [String: Any] {
        var query = tokenQuery; query[kSecAttrAccount as String] = "oauth-renewal"; return query
    }
    func renewalRecord() -> Data? {
        var query = renewalQuery
        query[kSecReturnData as String] = true; query[kSecMatchLimit as String] = kSecMatchLimitOne
        var item: CFTypeRef?
        guard SecItemCopyMatching(query as CFDictionary, &item) == errSecSuccess else { return nil }
        return item as? Data
    }
    func saveRenewalRecord(_ data: Data, accessToken: String) throws {
        var status = SecItemUpdate(renewalQuery as CFDictionary, [kSecValueData as String:data] as CFDictionary)
        if status == errSecItemNotFound {
            var query = renewalQuery; query[kSecValueData as String] = data
            query[kSecAttrAccessible as String] = kSecAttrAccessibleWhenUnlockedThisDeviceOnly
            status = SecItemAdd(query as CFDictionary, nil)
        }
        guard status == errSecSuccess else { throw NSError(domain: "AeselKeychain", code: Int(status), userInfo: [NSLocalizedDescriptionKey:"Could not save renewed sign-in in Keychain."]) }
        saveToken(accessToken)
    }

    func clearToken() {
        SecItemDelete(renewalQuery as CFDictionary)
        Self.tokenReads[tokenService] = .loaded(nil)
        SecItemDelete(tokenQuery as CFDictionary)
    }

    /// The signed-in AC access token, for the app's own calls to AC.
    func token() -> String? {
        if let read = Self.tokenReads[tokenService] {
            switch read {
            case .pending: return nil
            case .loaded(let value): return value
            }
        }
        Self.tokenReads[tokenService] = .pending
        var query = tokenQuery
        query[kSecReturnData as String] = true
        query[kSecMatchLimit as String] = kSecMatchLimitOne
        var item: CFTypeRef?
        guard SecItemCopyMatching(query as CFDictionary, &item) == errSecSuccess,
              let data = item as? Data, let token = String(data: data, encoding: .utf8), !token.isEmpty else {
            Self.tokenReads[tokenService] = .loaded(nil)
            return nil
        }
        Self.tokenReads[tokenService] = .loaded(token)
        return token
    }

    private func saveToken(_ token: String) {
        guard !token.isEmpty else { clearToken(); return }
        // An older notebook window may checkpoint its previous access token
        // while another window rotates credentials. Never roll the vault back.
        if let data = renewalRecord(), let record = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
           let current = record["accessToken"] as? String, current != token { return }
        // Session checkpoints repeat the token. Do not delete/recreate its
        // Keychain item: that loses the user's saved access permission.
        guard self.token() != token else { return }
        let data = Data(token.utf8)
        var status = SecItemUpdate(tokenQuery as CFDictionary,
                                   [kSecValueData as String: data] as CFDictionary)
        if status == errSecItemNotFound {
            var query = tokenQuery
            query[kSecValueData as String] = data
            query[kSecAttrAccessible as String] = kSecAttrAccessibleWhenUnlockedThisDeviceOnly
            status = SecItemAdd(query as CFDictionary, nil)
        }
        if status == errSecSuccess { Self.tokenReads[tokenService] = .loaded(token) }
        else { NSLog("[aesel] Keychain save failed (%d); sign in again after relaunch", status) }
    }

    func seedJSON() -> String? {
        var values = values
        values["session"] = values[sessionKey] ?? "{}"
        for key in values.keys.filter({ $0.hasPrefix("session.") }) { values.removeValue(forKey: key) }
        // Migrate earlier development sessions out of Documents on next write.
        var record = ((values["session"]?.data(using: .utf8)).flatMap {
            try? JSONSerialization.jsonObject(with: $0) as? [String: Any]
        }) ?? [:]
        if let token = token() { record["token"] = token }
        if let data = try? JSONSerialization.data(withJSONObject: record),
           let text = String(data: data, encoding: .utf8) { values["session"] = text }
        guard !values.isEmpty,
              let data = try? JSONSerialization.data(withJSONObject: values, options: []),
              let text = String(data: data, encoding: .utf8) else { return nil }
        return text
    }
}
