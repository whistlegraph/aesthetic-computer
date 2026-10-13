import SwiftUI

/// Reports and the rules (choir.ac/terms). A report goes to the server
/// (POST /api/choir?report=1), which pushes the moderator; the terms promise
/// action within 24 hours. Hiding someone also files a quiet "blocked" report,
/// so a block reaches the moderator too.
@MainActor enum Moderation {
    static let reasons = ["Harassment or hate", "Sexual or violent", "Spam or a scam", "Something else"]

    static func report(_ post: Post, reason: String, account: Account) async throws {
        struct Ask: Encodable { let id: String; let reason: String }
        struct Got: Decodable { let received: Bool }
        let _: Got = try await API.send("POST", "?report=1", Ask(id: post.id, reason: reason), token: await account.token())
        Trail.send("report", ["reason": reason])
    }

    /// Hide locally, and let the moderator know.
    static func block(_ post: Post, room: Room, account: Account) {
        room.hide(post.handle)
        guard account.signedIn, !Samples.isSample(post) else { return }
        Task { try? await report(post, reason: "blocked", account: account) }
    }

    static func agree(account: Account) async throws {
        struct Ask: Encodable { let version: String }
        struct Got: Decodable { let termsVersion: String? }
        let _: Got = try await API.send("POST", "?terms=1", Ask(version: Terms.version), token: await account.token())
        account.termsCurrent = true
        Trail.send("terms:agree")
    }
}

enum Terms { static let version = "2026-10-12" }   // matches TERMS_VERSION in system/backend/choir-moderation.mjs

extension View {
    /// The report dialog for one message: pick a reason, then a thank-you.
    func reportDialog(_ isPresented: Binding<Bool>, post: Post) -> some View {
        modifier(ReportDialog(open: isPresented, post: post))
    }
}

private struct ReportDialog: ViewModifier {
    @EnvironmentObject var account: Account
    @Binding var open: Bool
    let post: Post
    @State private var done: String?

    func body(content: Content) -> some View {
        content
            .confirmationDialog("Report this message?", isPresented: $open, titleVisibility: .visible) {
                ForEach(Moderation.reasons, id: \.self) { reason in
                    Button(reason, role: .destructive) { Task { await send(reason) } }
                }
                Button("Cancel", role: .cancel) {}
            } message: {
                Text("We look at every report within 24 hours.")
            }
            .alert(done ?? "", isPresented: Binding(get: { done != nil }, set: { if !$0 { done = nil } })) {
                Button("OK", role: .cancel) {}
            }
    }

    func send(_ reason: String) async {
        guard account.signedIn else { done = "Log in to report a message."; return }
        do {
            try await Moderation.report(post, reason: reason, account: account)
            done = "Thanks. We'll look at it within 24 hours."
        } catch { done = error.localizedDescription }
    }
}

/// Once per version of the rules, before a member's first post.
struct TermsSheet: View {
    @EnvironmentObject var account: Account
    @State private var busy = false
    @State private var error: String?

    var body: some View {
        VStack(alignment: .leading, spacing: 18) {
            SingingMark(height: 56, energy: 0.4).frame(maxWidth: .infinity)
            Text("Before you post").font(Theme.type(22, bold: true)).foregroundStyle(Theme.ink)
            VStack(alignment: .leading, spacing: 10) {
                rule("Be kind. No harassment, hate, threats or bullying.")
                rule("Nothing sexual, violent or illegal, and no spam.")
                rule("Post what's yours to share.")
                rule("To report or hide someone, long-press their @name.")
                rule("We look at every report within 24 hours. People who break the rules lose their place here.")
            }
            Link("Read the full rules at choir.ac/terms", destination: URL(string: "https://choir.ac/terms/")!)
                .font(Theme.type(14)).tint(Theme.ink)
            if let error { Text(error).font(Theme.type(13)).foregroundStyle(Theme.head) }
            Spacer()
            // Fía (23:06): "sounds good" rather than "I agree".
            Button(busy ? "One moment…" : "Sounds good") { Task { await agree() } }
                .buttonStyle(SendStyle(ready: !busy)).disabled(busy).frame(maxWidth: .infinity)
        }
        .padding(26)
        .background(RuledPaper())
        .interactiveDismissDisabled()
    }

    func rule(_ s: String) -> some View {
        // Fía (23:04): her hand-drawn asterisks, not flower emoji.
        HStack(alignment: .top, spacing: 10) { Hand("HandStar", height: 15).foregroundStyle(Theme.ink).padding(.top, 2); Text(s).font(.system(size: 16, design: .serif)).foregroundStyle(Theme.ink) }
    }

    func agree() async {
        busy = true; defer { busy = false }
        do { try await Moderation.agree(account: account) } catch { self.error = error.localizedDescription }
    }
}
