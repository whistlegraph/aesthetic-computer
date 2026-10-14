import SwiftUI
import UIKit

/// Reports and hiding (App Store guideline 1.2), from the bottom of a
/// person's profile card. A report goes to POST mime.ac/api/mime?report=1,
/// which pushes the moderator, who acts within 24 hours. Hiding someone
/// removes their cards from this phone at once and files a quiet "hidden"
/// report, so a block reaches the moderator too. Watching needs no account:
/// a viewer reports as this app install.
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

/// The handle opens its person's profile card (the same card mime.ac shows).
struct HandleButton<Label: View>: View {
    let post: Post
    let hide: (Post) -> Void
    @ViewBuilder let label: () -> Label
    @State private var open = false

    var body: some View {
        Button { open = true } label: { label().contentShape(Rectangle()) }
            .buttonStyle(.plain)
            .accessibilityHint("Open their profile")
            .sheet(isPresented: $open) {
                ProfileCard(post: post) { hide(post); open = false }
                    .presentationDetents([.large])
                    .presentationDragIndicator(.visible)
                    .presentationBackground(Color.paper)
            }
    }
}
