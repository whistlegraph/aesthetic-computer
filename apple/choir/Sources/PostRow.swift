import SwiftUI
import LinkPresentation

struct PostRow: View {
    @EnvironmentObject var account: Account
    @EnvironmentObject var room: Room
    let post: Post
    var inThread = false
    var onTag: (String?) -> Void = { _ in }
    var onChange: () -> Void = {}
    @State private var confirmDelete = false

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            HStack {
                Text(post.handle).font(.subheadline.weight(.semibold))
                Spacer()
                Text(post.date, format: .relative(presentation: .numeric, unitsStyle: .narrow))
                    .font(.caption).foregroundStyle(.secondary)
            }
            if post.deleted {
                Text("deleted").italic().foregroundStyle(.secondary)
            } else {
                Text(linked(post.text)).textSelection(.enabled)
                if let link = post.links.first, let url = URL(string: link) { LinkPreview(url: url).frame(minHeight: 60) }
            }
            if post.replyTo == nil {
                HStack(spacing: 6) {
                    ForEach(post.tags, id: \.self) { t in Chip(label: "#\(t)", on: false) { onTag(t) } }
                    Spacer()
                    if !inThread {
                        Text(post.replies == 0 ? "reply" : "\(post.replies) \(post.replies == 1 ? "reply" : "replies")")
                            .font(.caption).foregroundStyle(.secondary)
                    }
                }
            }
        }
        .padding(.vertical, 4)
        .contextMenu {
            if !post.deleted, let url = post.links.first.flatMap(URL.init) { ShareLink(item: url) }
            if post.handle == account.handle && !post.deleted {
                Button("Delete", systemImage: "trash", role: .destructive) { confirmDelete = true }
            } else {
                Button("Hide \(post.handle)", systemImage: "eye.slash") { room.hide(post.handle) }
                Link(destination: report) { Label("Report", systemImage: "flag") }
            }
        }
        .confirmationDialog("Delete this post?", isPresented: $confirmDelete) {
            Button("Delete", role: .destructive) {
                Task { try? await API.delete(post.id, token: await account.token()); Trail.send("post:delete"); onChange(); await room.load() }
            }
        }
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
            out[ar].foregroundColor = .accentColor
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
