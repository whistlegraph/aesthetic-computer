import SwiftUI
import UIKit

/// Reports and hiding (App Store guideline 1.2). Tapping a card's @handle
/// offers both. A report goes to POST mime.ac/api/mime?report=1, which pushes
/// the moderator, who acts within 24 hours. Hiding someone removes their cards
/// from this phone at once and files a quiet "hidden" report, so a block
/// reaches the moderator too. Watching needs no account: a viewer reports as
/// this app install.
enum MimeModeration {
    static let reasons = ["Harassment or hate", "Sexual or violent", "Spam or a scam", "Something else"]

    static func report(_ post: Post, reason: String) async throws {
        var request = URLRequest(url: URL(string: "https://mime.ac/api/mime?report=1")!, timeoutInterval: 15)
        request.httpMethod = "POST"
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        let device = await UIDevice.current.identifierForVendor?.uuidString ?? UUID().uuidString
        request.httpBody = try JSONSerialization.data(withJSONObject: ["code": post.code, "reason": reason, "device": device])
        let (_, response) = try await URLSession.shared.data(for: request)
        guard (response as? HTTPURLResponse)?.statusCode == 200 else { throw URLError(.badServerResponse) }
    }
}

/// The handle, as a menu: report this card, or hide everything by its author.
struct HandleMenu<Label: View>: View {
    let post: Post
    let hide: (Post) -> Void
    @ViewBuilder let label: () -> Label
    @State private var done: String?

    var body: some View {
        Menu {
            Menu("Report this") {
                ForEach(MimeModeration.reasons, id: \.self) { reason in
                    Button(reason) { Task { await send(reason) } }
                }
            }
            if let name = post.name {
                Button("Hide \(name)", systemImage: "eye.slash", role: .destructive) {
                    hide(post)
                    Task { try? await MimeModeration.report(post, reason: "hidden") }
                }
            }
        } label: { label().contentShape(Rectangle()) }
        .menuStyle(.button).buttonStyle(.plain)
        .accessibilityHint("Report or hide")
        .alert(done ?? "", isPresented: Binding(get: { done != nil }, set: { if !$0 { done = nil } })) {
            Button("OK", role: .cancel) {}
        }
    }

    private func send(_ reason: String) async {
        do {
            try await MimeModeration.report(post, reason: reason)
            done = "Thanks. We look at every report within 24 hours."
        } catch { done = "That report didn't go through. Try again in a moment." }
    }
}
