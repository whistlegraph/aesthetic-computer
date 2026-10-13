import SwiftUI

/// What a link says about itself, from AC's og-preview (title, summary, image).
struct ArticleMeta: Decodable, Hashable {
    let url: String?
    let title: String?
    let description: String?
    let image: String?
    let siteName: String?

    private static var cache: [String: ArticleMeta] = [:]
    @MainActor static func load(_ link: String) async -> ArticleMeta? {
        if let hit = cache[link] { return hit }
        var c = URLComponents(string: "https://aesthetic.computer/api/og-preview")!
        c.queryItems = [URLQueryItem(name: "url", value: link)]
        guard let (data, _) = try? await URLSession.shared.data(from: c.url!),
              let meta = try? JSONDecoder().decode(ArticleMeta.self, from: data) else { return nil }
        cache[link] = meta
        return meta
    }
}

/// A shared link as a headline: picture, source, headline, a short
/// abridgement, and a way to jump to the article.
struct HeadlineCard: View {
    let link: String
    var words: String = ""        // what the poster wrote, if anything
    var large = false
    @State private var meta: ArticleMeta?
    @Environment(\.openURL) private var openURL

    var host: String { URL(string: link)?.host()?.replacingOccurrences(of: "www.", with: "") ?? link }
    var headline: String {
        let t = meta?.title?.trimmingCharacters(in: .whitespaces) ?? ""
        return t.isEmpty || t == host ? (words.isEmpty ? host : words) : t
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            if let image = meta?.image, let url = URL(string: image) {
                AsyncImage(url: url) { $0.resizable().scaledToFill() } placeholder: { Theme.desk }
                    .frame(height: large ? 200 : 140).frame(maxWidth: .infinity).clipped()
                    .clipShape(RoundedRectangle(cornerRadius: 8))
            }
            Text((meta?.siteName ?? host).uppercased()).font(.system(size: 10, weight: .semibold)).tracking(1.2)
                .foregroundStyle(Theme.ink.opacity(0.5))
            Text(headline).font(.system(size: large ? 24 : 19, weight: .bold, design: .serif))
                .foregroundStyle(Theme.ink).fixedSize(horizontal: false, vertical: true)
            if let summary = meta?.description, !summary.isEmpty, summary != headline {
                Text(summary).font(.system(size: large ? 16 : 14, design: .serif)).foregroundStyle(Theme.ink.opacity(0.75))
                    .lineLimit(large ? 8 : 3)
            }
            if !words.isEmpty && words != headline {
                Text(words).font(.system(size: 15, design: .serif)).italic().foregroundStyle(Theme.ink.opacity(0.85))
            }
            Button { if let u = URL(string: link) { Trail.send("article:open"); openURL(u) } } label: {
                HStack(spacing: 4) { Text("Read the article"); Image(systemName: "arrow.up.right") }
                    .font(Theme.type(13, bold: true))
            }
            .buttonStyle(GlassChipStyle(on: false))
        }
        .task { meta = await ArticleMeta.load(link) }
    }
}
