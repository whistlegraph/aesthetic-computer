import SwiftUI

@MainActor final class AIConsent: ObservableObject {
    static let shared = AIConsent()
    @Published private(set) var record = AIConsentRecord()
    @Published private(set) var handle = ""
    private(set) var subject: String?
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

// This is presented by an attempted AI action, never by launch or sign-in.
// The separate settings page below remains available for changing permission.
struct WhistlegraphAIConsentSheet: View {
    let canAllow: Bool
    let allow: () -> Void
    let decline: () -> Void
    @Environment(\.colorScheme) private var colorScheme
    @Environment(\.dynamicTypeSize) private var typeSize
    private var theme: WhistlegraphTheme { WhistlegraphTheme(phase: .ready, dark: colorScheme == .dark) }
    var body: some View {
        ScrollView {
            VStack(alignment: .leading, spacing: 14) {
                ComicTitle(text: "Create with AI?", size: 30)
                    .accessibilityAddTraits(.isHeader)
                Text("To make and check your piece, AC shares your typed and spoken words, drawings, code and version history, sound measurements and artwork previews with OpenRouter and your chosen AI provider.")
                    .font(.custom("ComicRelief-Regular", size: 18, relativeTo: .body))
                Text("Providers: Anthropic, OpenAI, DeepSeek, Moonshot AI, Alibaba (Qwen), MiniMax and Z.ai. Personal models use Anthropic or OpenAI directly.")
                    .font(.custom("ComicRelief-Regular", size: 15, relativeTo: .subheadline))
                Text("Change your choice in Brain → AI & privacy.")
                    .font(.custom("ComicRelief-Regular", size: 15, relativeTo: .subheadline))
                HStack(spacing: 12) {
                    Button("Not now", action: decline)
                        .accessibilityIdentifier("ai-consent-not-now")
                        .buttonStyle(ConsentButtonStyle(theme: theme, fill: theme.surface))
                    Button("Allow", action: allow)
                        .accessibilityIdentifier("ai-consent-allow")
                        .buttonStyle(ConsentButtonStyle(theme: theme, fill: Color(red: 0.40, green: 0.83, blue: 0.95), prominent: true))
                        .disabled(!canAllow)
                }
                Link("Privacy policy", destination: URL(string: "https://aesthetic.computer/privacy-policy.html")!)
                    .font(.custom("ComicRelief-Regular", size: 15, relativeTo: .subheadline))
                    .frame(maxWidth: .infinity).padding(.top, 2)
            }.padding(24).padding(.top, 12)
        }
        .foregroundStyle(theme.foreground).tint(theme.foreground)
        .background(theme.background.ignoresSafeArea())
        .presentationBackground(theme.background)
        .presentationDetents(typeSize.isAccessibilitySize ? [.large] : [.height(510), .large])
        .presentationDragIndicator(.visible)
        .presentationCornerRadius(30)
    }
}

private struct ConsentButtonStyle: ButtonStyle {
    let theme: WhistlegraphTheme
    let fill: Color
    var prominent = false
    func makeBody(configuration: Configuration) -> some View {
        configuration.label
            .font(.custom("ComicRelief-Bold", size: 22, relativeTo: .title3))
            .frame(maxWidth: .infinity, minHeight: 56)
            .foregroundStyle(prominent ? theme.buttonInk : theme.foreground)
            .background(fill, in: RoundedRectangle(cornerRadius: 22))
            .overlay(RoundedRectangle(cornerRadius: 22).strokeBorder(theme.foreground, lineWidth: 2))
            .opacity(configuration.isPressed ? 0.7 : 1)
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
