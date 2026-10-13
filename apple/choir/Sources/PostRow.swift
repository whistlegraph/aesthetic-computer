import SwiftUI
import LinkPresentation

struct PostRow: View {
    @EnvironmentObject var account: Account
    @EnvironmentObject var room: Room
    let post: Post
    var inThread = false
    var onTag: (String?) -> Void = { _ in }
    var onChange: () -> Void = {}
    @State private var reporting = false
    @State private var confirmDelete = false

    var body: some View {
        DeskWindow(title: post.handle, detail: post.date.formatted(.relative(presentation: .numeric, unitsStyle: .narrow)), call: Samples.isSample(post) ? "sample" : post.replyTo == nil ? Dewey.call(post) : "") {
            VStack(alignment: .leading, spacing: 12) {
                if post.deleted {
                    Text("deleted").italic().foregroundStyle(.secondary)
                } else {
                    let words = shownText
                    if !words.isEmpty {
                        Text(linked(words)).font(.system(size: 17, design: .serif)).lineSpacing(4)
                            .foregroundStyle(Theme.ink).textSelection(.enabled)
                    }
                    if let link = post.links.first, let url = URL(string: link) { LinkPreview(url: url).frame(minHeight: 60) }
                }
                if post.replyTo == nil {
                    HStack(spacing: 10) {
                        Spacer()
                        if !inThread {
                            Text(post.replies == 0 ? "reply" : "\(post.replies) \(post.replies == 1 ? "reply" : "replies")")
                                .font(Theme.type(12)).foregroundStyle(Theme.ink.opacity(0.55))
                        }
                    }
                }
            }
        }
        .contextMenu {
            if !post.deleted, let url = post.links.first.flatMap(URL.init) { ShareLink(item: url) }
            if post.handle == account.handle && !post.deleted {
                Button("Delete", systemImage: "trash", role: .destructive) { confirmDelete = true }
            } else {
                Button("Hide \(post.handle)", systemImage: "eye.slash") { Moderation.block(post, room: room, account: account) }
                Button("Report", systemImage: "flag") { reporting = true }
            }
        }
        // Its own view: two dialogs on one view and only one ever shows (Delete broke).
        .background(Color.clear.reportDialog($reporting, post: post))
        .confirmationDialog("Delete this post?", isPresented: $confirmDelete) {
            Button("Delete", role: .destructive) {
                Task { try? await API.delete(post.id, token: await account.token()); Trail.send("post:delete"); onChange(); await room.load() }
            }
        }
    }

    /// The words without the link the preview card already shows.
    var shownText: String {
        guard let first = post.links.first else { return post.text }
        return post.text.replacingOccurrences(of: first, with: "").trimmingCharacters(in: .whitespacesAndNewlines)
    }

    var report: URL {
        var c = URLComponents(string: "mailto:mail@aesthetic.computer")!
        c.queryItems = [URLQueryItem(name: "subject", value: "choir report \(post.id)"),
                        URLQueryItem(name: "body", value: "Post \(post.id) by \(post.handle):\n\n\(post.text)\n\nWhat's wrong:")]
        return c.url!
    }

    func linked(_ text: String) -> AttributedString {
        var out = AttributedString(text)
        guard let detector = try? NSDataDetector(types: NSTextCheckingResult.CheckingType.link.rawValue) else { return out }
        for m in detector.matches(in: text, range: NSRange(text.startIndex..., in: text)) {
            guard let url = m.url, let r = Range(m.range, in: text), let ar = Range(r, in: out) else { continue }
            out[ar].link = url
            out[ar].underlineStyle = .single
            out[ar].foregroundColor = Color(red: 0.12, green: 0.3, blue: 0.75)
        }
        return out
    }
}

/// Apple's own rich link card, cached per URL for the session.
struct LinkPreview: UIViewRepresentable {
    let url: URL
    private static var cache: [URL: LPLinkMetadata] = [:]

    func makeUIView(context: Context) -> LPLinkView {
        let view = LPLinkView(url: url)
        if let m = Self.cache[url] { view.metadata = m; return view }
        LPMetadataProvider().startFetchingMetadata(for: url) { m, _ in
            guard let m else { return }
            DispatchQueue.main.async { Self.cache[url] = m; view.metadata = m; view.sizeToFit() }
        }
        return view
    }
    func updateUIView(_ view: LPLinkView, context: Context) {}
}
