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

/// A profile card floating over the feed, out of the handle's corner. Tap
/// around it to put it away.
struct ProfilePopover: View {
    let post: Post
    let hide: (Post) -> Void
    let close: () -> Void

    var body: some View {
        ZStack(alignment: .bottomLeading) {
            Color.black.opacity(0.35).ignoresSafeArea()
                .contentShape(Rectangle()).onTapGesture(perform: close)
                .accessibilityLabel("Close profile").accessibilityAddTraits(.isButton)
            ProfileCard(post: post) { hide(post); close() }
                .padding(.horizontal, 16).padding(.bottom, 92)
        }
    }
}
