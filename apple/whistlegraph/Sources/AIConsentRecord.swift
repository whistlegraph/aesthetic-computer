import Foundation
import CryptoKit

/// Logging in is the permission: the first verified sign-in records `allowed`
/// for that account, and it covers AI creation, cloud speech and cloud narration.
/// `false` means the switch in AI & privacy was turned off and stays off.
/// A new version means the disclosure changed and the account is asked once more.
struct AIConsentRecord: Codable, Equatable {
    static let version = 2
    var version = Self.version
    var allowed: Bool? = nil
    var updatedAt = Date()
    static func key(subject: String) -> String {
        "whistlegraph-ai-consent:" + SHA256.hash(data: Data(subject.utf8)).map { String(format: "%02x", $0) }.joined()
    }
    static func read(subject: String?, defaults: UserDefaults = .standard) -> Self {
        guard let subject, !subject.isEmpty, let data = defaults.data(forKey: key(subject: subject)),
              let value = try? JSONDecoder().decode(Self.self, from: data), value.version == version else { return Self() }
        return value
    }
    func save(subject: String, defaults: UserDefaults = .standard) {
        guard !subject.isEmpty, let data = try? JSONEncoder().encode(self) else { return }
        defaults.set(data, forKey: Self.key(subject: subject))
    }
}
