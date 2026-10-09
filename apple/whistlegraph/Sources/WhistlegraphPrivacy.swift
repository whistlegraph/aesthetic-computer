import SwiftUI

@MainActor final class AIConsent: ObservableObject {
    static let shared = AIConsent()
    @Published private(set) var record = AIConsentRecord()
    @Published private(set) var handle = ""
    private(set) var subject: String?
    var changed: () -> Void = {}
    var signedIn: Bool { subject != nil && !handle.isEmpty }
    /// Logging in switches this on; it covers creation, cloud speech and cloud narration.
    var allowed: Bool { signedIn && record.allowed == true }
    var decided: Bool { record.allowed != nil }
    var creation: Bool { allowed }
    var cloudSpeech: Bool { allowed }
    var cloudNarration: Bool { allowed }
    var bridge: [String: Any] { ["handle": handle, "creation": creation] }
    func bind(subject: String?, handle: String) {
        if self.subject != subject { StoryVoice.cancelCloudRequests() }
        self.subject = subject; self.handle = handle
        record = AIConsentRecord.read(subject: subject)
        changed()
    }
    func set(allowed value: Bool) {
        guard let subject, signedIn else { return }
        DeviceActionLog.shared.record(.setting, value ? .enabled : .disabled, control: .creation)
        record.allowed = value; record.updatedAt = Date()
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
                Section("AI") {
                    Text("Whistlegraph makes pieces with AI. AC sends your prompts, speech recordings and transcripts, selected source and version context, drawings, requested sound measurements, and cropped artwork preview images to AI services to generate and check edits.")
                    Text("Hosted models use OpenRouter and the model you select: Anthropic, OpenAI, DeepSeek, Moonshot AI, Alibaba/Qwen, MiniMax, or Z.ai. Finished recordings go through AC to OpenAI for transcription at 40 braincells per recorded second, daily allowance first. Optional Jeffrey narration sends story captions to ElevenLabs. The Brain panel identifies the current model and service.")
                    Toggle("Create with AI", isOn: Binding(get: { consent.allowed }, set: { consent.set(allowed: $0) }))
                        .disabled(!consent.signedIn).accessibilityIdentifier("privacy-ai-creation")
                }
                Section {
                    Text("Logging in switches this on. Switching it off stops new requests and cancels active sending; turn it back on here to create again. Data already sent may remain with those services under their policies. Viewing, editing source and exporting your saved work remain available.")
                    Text("Your AC account privately stores source, version history, requests and diagnostic receipts. It also keeps a record of each device you use Whistlegraph on — app build, device model, iOS version and when it last opened — so AC can support you and send notifications you allow. Saved microphone recordings remain on this phone unless cloud speech is enabled. Avoid sending sensitive personal information.")
                    Link("Privacy policy", destination: privacy).accessibilityIdentifier("privacy-policy")
                    Link("Contact support", destination: URL(string: "mailto:mail@aesthetic.computer")!)
                    if !consent.signedIn { Text("Sign in to manage this account's AI permissions.").foregroundStyle(.secondary) }
                }
            }
            .navigationTitle("AI & privacy").navigationBarTitleDisplayMode(.inline)
            .onAppear { DeviceActionLog.shared.record(.screen, .presented, control: .privacy) }
            .onDisappear { DeviceActionLog.shared.record(.screen, .dismissed, control: .privacy) }
            .toolbar { ToolbarItem(placement: .confirmationAction) { Button("Done") { dismiss() } } }
    }
}
