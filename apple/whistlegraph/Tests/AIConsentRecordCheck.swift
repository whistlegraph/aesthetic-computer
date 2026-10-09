import Foundation

@main struct AIConsentRecordCheck {
    static func main() throws {
        let name = "whistlegraph-consent-test-" + UUID().uuidString
        let defaults = UserDefaults(suiteName: name)!
        defer { defaults.removePersistentDomain(forName: name) }
        func read(_ subject: String?) -> AIConsentRecord { AIConsentRecord.read(subject: subject, defaults: defaults) }
        precondition(read(nil).allowed == nil)
        precondition(read("alice").allowed == nil, "A new account has not decided; logging in decides")
        var record = AIConsentRecord(); record.allowed = true
        record.save(subject: "alice", defaults: defaults)
        precondition(read("alice").allowed == true, "Logging in sticks")
        precondition(read("bob").allowed == nil, "Permission never transfers to another account")
        record.allowed = false; record.save(subject: "alice", defaults: defaults)
        precondition(read("alice").allowed == false, "Switching off persists and is not undecided")
        record.allowed = true; record.version = AIConsentRecord.version - 1
        record.save(subject: "alice", defaults: defaults)
        precondition(read("alice").allowed == nil, "Changed disclosure starts over")
        defaults.set(Data("invalid".utf8), forKey: AIConsentRecord.key(subject: "alice"))
        precondition(read("alice").allowed == nil)
        let old = #"{"version":1,"creation":true,"cloudSpeech":false,"cloudNarration":false,"updatedAt":0}"#
        defaults.set(Data(old.utf8), forKey: AIConsentRecord.key(subject: "legacy"))
        precondition(read("legacy").allowed == nil, "A three-toggle record starts over; the next login decides")
        print("AIConsentRecordCheck passed (8 cases)")
    }
}
