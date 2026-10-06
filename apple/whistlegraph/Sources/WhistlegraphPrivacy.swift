import SwiftUI

@MainActor final class AIConsent: ObservableObject {
    static let shared = AIConsent()
    @Published private(set) var record = AIConsentRecord()
    @Published private(set) var handle = ""
    private var subject: String?
    var changed: () -> Void = {}
    var signedIn: Bool { subject != nil && !handle.isEmpty }
    var creation: Bool { signedIn && record.creation }
    var cloudSpeech: Bool { signedIn && record.cloudSpeech }
    var cloudNarration: Bool { signedIn && record.cloudNarration }
    var bridge: [String: Any] { ["handle": handle, "creation": creation] }
    func bind(subject: String?, handle: String) {
        if self.subject != subject { StoryVoice.cancelCloudRequests() }
        self.subject = subject; self.handle = handle
        record = AIConsentRecord.read(subject: subject)
        changed()
    }
    func set(_ key: WritableKeyPath<AIConsentRecord, Bool>, _ value: Bool) {
        guard let subject, signedIn else { return }
        record[keyPath: key] = value; record.updatedAt = Date()
        record.save(subject: subject); changed()
    }
    func forget() {
        if let subject { UserDefaults.standard.removeObject(forKey: AIConsentRecord.key(subject: subject)) }
        self.subject = nil; handle = ""; record = AIConsentRecord(); changed()
    }
}

struct WhistlegraphPrivacySheet: View {
    @ObservedObject var session: WhistlegraphSession
    @ObservedObject private var consent = AIConsent.shared
    @Environment(\.dismiss) private var dismiss
    private let privacy = URL(string: "https://aesthetic.computer/privacy-policy.html")!
    var body: some View {
        List {
                Section("AI creation") {
                    Text("AC sends your prompts, speech transcripts, selected source and version context, drawings, requested sound measurements, and cropped artwork preview images to AI services to generate and check edits.")
                    Text("Hosted models use OpenRouter and the model provider you select: Anthropic, OpenAI, DeepSeek, Moonshot AI, Alibaba/Qwen, MiniMax, or Z.ai. Personal models use Anthropic or OpenAI. The Brain panel identifies the current model and service.")
                    Toggle("Allow AI creation", isOn: Binding(get: { consent.creation }, set: { consent.set(\.creation, $0) }))
                        .disabled(!consent.signedIn).accessibilityIdentifier("privacy-ai-creation")
                }
                Section("Cloud speech") {
                    Text("Optional OpenAI transcription sends microphone audio through AC or directly to OpenAI during recording and for word timing. It currently requires an eligible personal account. With this off, speech recognition stays on the device.")
                    Toggle("Allow audio to OpenAI", isOn: Binding(get: { consent.cloudSpeech }, set: { consent.set(\.cloudSpeech, $0) }))
                        .disabled(!consent.signedIn).accessibilityIdentifier("privacy-cloud-speech")
                }
                Section("Cloud narration") {
                    Text("Optional Jeffrey narration sends story captions to ElevenLabs through AC. With this off, stories use your saved recording or device speech.")
                    Toggle("Allow captions to ElevenLabs", isOn: Binding(get: { consent.cloudNarration }, set: { consent.set(\.cloudNarration, $0) }))
                        .disabled(!consent.signedIn).accessibilityIdentifier("privacy-cloud-narration")
                }
                Section {
                    Text("Turning permission off stops new requests and cancels active sending. Data already sent may remain with those services under their policies. Viewing, editing source and exporting your saved work remain available.")
                    Text("Your AC account privately stores source, version history, requests and diagnostic receipts. Saved microphone recordings remain on this phone unless cloud speech is enabled. Avoid sending sensitive personal information.")
                    Link("Privacy policy", destination: privacy).accessibilityIdentifier("privacy-policy")
                    Link("Contact support", destination: URL(string: "mailto:mail@aesthetic.computer")!)
                    if !consent.signedIn { Text("Sign in to manage this account's AI permissions.").foregroundStyle(.secondary) }
                }
            }
            .navigationTitle("AI & privacy").navigationBarTitleDisplayMode(.inline)
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
    }
}
