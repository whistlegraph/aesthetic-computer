import Foundation
import CryptoKit

struct AIConsentRecord: Codable, Equatable {
    static let version = 1
    var version = Self.version
    var creation = false
    var cloudSpeech = false
    var cloudNarration = false
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
