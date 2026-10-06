import Foundation

@main struct AIConsentRecordCheck {
    static func main() throws {
        let name = "whistlegraph-consent-test-" + UUID().uuidString
        let defaults = UserDefaults(suiteName: name)!
        defer { defaults.removePersistentDomain(forName: name) }
        func allOff(_ value: AIConsentRecord) -> Bool { !value.creation && !value.cloudSpeech && !value.cloudNarration }
        precondition(allOff(AIConsentRecord.read(subject: nil, defaults: defaults)))
        precondition(allOff(AIConsentRecord.read(subject: "alice", defaults: defaults)))
        var record = AIConsentRecord(); record.creation = true
        record.save(subject: "alice", defaults: defaults)
        let restored = AIConsentRecord.read(subject: "alice", defaults: defaults)
        precondition(restored.creation && !restored.cloudSpeech && !restored.cloudNarration, "Optional providers stay off")
        precondition(allOff(AIConsentRecord.read(subject: "bob", defaults: defaults)), "Consent never transfers to another account")
        record.creation = false; record.save(subject: "alice", defaults: defaults)
        precondition(allOff(AIConsentRecord.read(subject: "alice", defaults: defaults)), "Revocation persists")
        record.creation = true; record.version = AIConsentRecord.version - 1
        record.save(subject: "alice", defaults: defaults)
        precondition(allOff(AIConsentRecord.read(subject: "alice", defaults: defaults)), "Changed disclosure needs fresh permission")
        defaults.set(Data("invalid".utf8), forKey: AIConsentRecord.key(subject: "alice"))
        precondition(allOff(AIConsentRecord.read(subject: "alice", defaults: defaults)))
        print("AIConsentRecordCheck passed (7 cases)")
    }
}
