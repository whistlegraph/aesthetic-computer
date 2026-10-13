import SwiftUI

/// What a link says about itself, from AC's og-preview (title, summary, image).
struct ArticleMeta: Codable, Hashable {
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
    var stored: ArticleMeta? = nil // a news item's own card, when it has one
    var opportunity = false
    var opening = false
    @State private var fetched: ArticleMeta?
    var meta: ArticleMeta? { stored ?? fetched }
    @Environment(\.openURL) private var openURL

    /// A site's front page is not an article (Fía, bug c786c7a3b9ba).
    var isHomepage: Bool {
        guard let u = URL(string: link) else { return false }
        return u.path.isEmpty || u.path == "/" || u.path.lowercased() == "/index.html"
    }
    var buttonLabel: String {
        if opportunity { return "See the call" }
        if opening { return "See the show" }
        return isHomepage ? "Visit site" : "Read the article"
    }

    var host: String { URL(string: link)?.host()?.replacingOccurrences(of: "www.", with: "") ?? link }
    var headline: String {
        let t = Self.trim(meta?.title ?? "", site: meta?.siteName, host: host)
        return t.isEmpty || t == host ? (words.isEmpty ? host : words) : t
    }

    /// "Torkwase Dyson - Announcements - e-flux" → "Torkwase Dyson". Drops trailing
    /// " - " / " | " pieces that name the site or a section of it.
    static func trim(_ title: String, site: String?, host: String) -> String {
        var parts = title.components(separatedBy: CharacterSet(charactersIn: "|")).flatMap { $0.components(separatedBy: " - ") }
            .map { $0.trimmingCharacters(in: .whitespaces) }.filter { !$0.isEmpty }
        let names = [site, host, host.components(separatedBy: ".").first].compactMap { $0?.lowercased() }
        let sections: Set<String> = ["announcements", "criticism", "notes", "news", "reviews", "features", "opinion", "magazine"]
        while parts.count > 1, let last = parts.last?.lowercased(),
              names.contains(where: { last.contains($0) || $0.contains(last) }) || sections.contains(last) || last.hasPrefix("journal #") {
            parts.removeLast()
        }
        return parts.joined(separator: " - ")
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            if let image = meta?.image, let url = URL(string: image) {
                // The frame sets the size; the image only fills it (a wide image used to
                // push the whole card past the screen edge — Fía's 22:09 screenshot).
                Theme.desk.frame(maxWidth: .infinity).frame(height: large ? 200 : 140)
                    .overlay { RemoteImage(url: url, maxPixels: 800) { $0.resizable().scaledToFill() } placeholder: { _ in Color.clear } }
                    .clipped().clipShape(RoundedRectangle(cornerRadius: 8))
            }
            HStack(spacing: 8) {
                if opening {
                    Text("OPENING").font(.system(size: 10, weight: .bold)).tracking(1.2)
                        .padding(.horizontal, 7).padding(.vertical, 3)
                        .background(Color(red: 0.24, green: 0.47, blue: 0.86), in: Capsule()).foregroundStyle(.white)
                }
                if opportunity {
                    Text("OPEN CALL").font(.system(size: 10, weight: .bold)).tracking(1.2)
                        .padding(.horizontal, 7).padding(.vertical, 3)
                        .background(Color(red: 0.2, green: 0.63, blue: 0.2), in: Capsule()).foregroundStyle(.white)
                }
                Text((meta?.siteName ?? host).uppercased()).font(.system(size: 10, weight: .semibold)).tracking(1.2)
                    .foregroundStyle(Theme.ink.opacity(0.5))
            }
            Text(headline).font(.system(size: large ? 24 : 19, weight: .bold, design: .serif))
                .foregroundStyle(Theme.ink).fixedSize(horizontal: false, vertical: true)
            if let summary = meta?.description, !summary.isEmpty, summary != headline {
                Text(summary).font(.system(size: large ? 16 : 14, design: .serif)).foregroundStyle(Theme.ink.opacity(0.75))
                    .lineLimit(large ? (opportunity ? 10 : 8) : 3)
            }
            if !words.isEmpty && words != headline && stored == nil {
                Text(words).font(.system(size: 15, design: .serif)).italic().foregroundStyle(Theme.ink.opacity(0.85))
            }
            Button { if let u = URL(string: link) { Trail.send("article:open"); openURL(u) } } label: {
                HStack(spacing: 4) { Text(buttonLabel); Image(systemName: "arrow.up.right") }
                    .font(Theme.type(13, bold: true))
            }
            .buttonStyle(GlassChipStyle(on: false))
        }
        .task { if stored == nil { fetched = await ArticleMeta.load(link) } }
    }
}

/// How a link sits inside a chat bubble: small square thumbnail, source,
/// one headline line, one line of text. Big cards live in Discover/comments.
struct CompactLink: View {
    let link: String
    var stored: ArticleMeta? = nil
    var kind: String? = nil
    @State private var fetched: ArticleMeta?
    @Environment(\.openURL) private var openURL
    var meta: ArticleMeta? { stored ?? fetched }
    var isInstagram: Bool { InstagramCard.isInstagram(link) }
    var host: String { URL(string: link)?.host()?.replacingOccurrences(of: "www.", with: "") ?? link }

    var line1: String {
        if isInstagram { return InstagramCard.handleIn(meta?.description).map { "@\($0)" } ?? "Instagram" }
        let t = HeadlineCard.trim(meta?.title ?? "", site: meta?.siteName, host: host)
        return t.isEmpty ? host : InstagramCard.decode(t)
    }
    var line2: String {
        if isInstagram { return InstagramCard.captionIn(meta?.title) }
        return InstagramCard.decode(meta?.description ?? "")
    }

    var body: some View {
        HStack(alignment: .top, spacing: 10) {
            ZStack {
                Theme.desk
                if let image = meta?.image, let url = URL(string: image) {
                    RemoteImage(url: url, maxPixels: 400) { $0.resizable().scaledToFill() } placeholder: { _ in Color.clear }
                } else {
                    Image(systemName: isInstagram ? "camera" : "link").foregroundStyle(Theme.ink.opacity(0.35))
                }
            }
            .frame(width: 72, height: 72).clipShape(RoundedRectangle(cornerRadius: 8))
            VStack(alignment: .leading, spacing: 2) {
                HStack(spacing: 5) {
                    if kind == "opportunity" { Text("OPEN CALL").font(.system(size: 9, weight: .bold)).foregroundStyle(Color(red: 0.2, green: 0.63, blue: 0.2)) }
                    if kind == "opening" { Text("OPENING").font(.system(size: 9, weight: .bold)).foregroundStyle(Color(red: 0.24, green: 0.47, blue: 0.86)) }
                    Text((isInstagram ? "INSTAGRAM" : (meta?.siteName ?? host)).uppercased())
                        .font(.system(size: 9, weight: .semibold)).tracking(1).foregroundStyle(Theme.ink.opacity(0.5)).lineLimit(1)
                }
                Text(line1).font(.system(size: 15, weight: .semibold, design: .serif)).foregroundStyle(Theme.ink).lineLimit(2)
                if !line2.isEmpty {
                    Text(line2).font(.system(size: 13, design: .serif)).foregroundStyle(Theme.ink.opacity(0.7)).lineLimit(2)
                }
            }
            Spacer(minLength: 0)
        }
        .contentShape(Rectangle())
        .onTapGesture(count: 2) { if let u = URL(string: link) { Trail.send("link:open"); openURL(u) } }
        .task { if stored == nil { fetched = await ArticleMeta.load(link) } }
    }
}
