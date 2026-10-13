import SwiftUI

/// An Instagram link is about the picture and who posted it: big square image,
/// the poster's @handle, a short caption without the hashtag wall.
struct InstagramCard: View {
    let link: String
    var large = false
    @State private var meta: ArticleMeta?
    @Environment(\.openURL) private var openURL

    static func isInstagram(_ link: String) -> Bool {
        (URL(string: link)?.host() ?? "").hasSuffix("instagram.com")
    }

    /// "20K likes, 90 comments - le_jardin_robo on July 1, 2025: …" → "le_jardin_robo"
    var handle: String? {
        let d = meta?.description ?? ""
        for pattern in [#"- ([A-Za-z0-9._]+) on [A-Z][a-z]+ \d"#, #"^([A-Za-z0-9._]+) on "#] {
            guard let re = try? NSRegularExpression(pattern: pattern),
                  let m = re.firstMatch(in: d, range: NSRange(d.startIndex..., in: d)),
                  let r = Range(m.range(at: 1), in: d) else { continue }
            return String(d[r])
        }
        return nil
    }
    /// The quoted caption, entities decoded, hashtags and trailing tags dropped.
    var caption: String {
        let raw = Self.decode(meta?.title ?? "")
        var text = raw
        if let open = raw.firstIndex(of: "\""), let close = raw.lastIndex(of: "\""), open < close {
            text = String(raw[raw.index(after: open)..<close])
        }
        let lines = text.split(separator: "\n").map { $0.trimmingCharacters(in: .whitespaces) }
            .filter { !$0.isEmpty && !$0.hasPrefix("#") }
        return lines.joined(separator: " ").replacingOccurrences(of: #"\s#\S+"#, with: "", options: .regularExpression)
    }

    static func handleIn(_ description: String?) -> String? {
        let d = description ?? ""
        for pattern in [#"- ([A-Za-z0-9._]+) on [A-Z][a-z]+ \d"#, #"^([A-Za-z0-9._]+) on "#] {
            guard let re = try? NSRegularExpression(pattern: pattern),
                  let m = re.firstMatch(in: d, range: NSRange(d.startIndex..., in: d)),
                  let r = Range(m.range(at: 1), in: d) else { continue }
            return String(d[r])
        }
        return nil
    }
    static func captionIn(_ title: String?) -> String {
        var text = decode(title ?? "")
        if let open = text.firstIndex(of: "\""), let close = text.lastIndex(of: "\""), open < close {
            text = String(text[text.index(after: open)..<close])
        }
        let lines = text.split(separator: "\n").map { $0.trimmingCharacters(in: .whitespaces) }.filter { !$0.isEmpty && !$0.hasPrefix("#") }
        return lines.joined(separator: " ").replacingOccurrences(of: #"\s#\S+"#, with: "", options: .regularExpression)
    }

    static func decode(_ s: String) -> String {
        var out = s
        // Numeric entities, decimal and hex (Instagram writes emoji as &#x1f994;).
        if let re = try? NSRegularExpression(pattern: #"&#(x?)([0-9a-fA-F]+);"#) {
            for m in re.matches(in: out, range: NSRange(out.startIndex..., in: out)).reversed() {
                guard let r = Range(m.range, in: out), let hexR = Range(m.range(at: 1), in: out), let numR = Range(m.range(at: 2), in: out),
                      let v = UInt32(out[numR], radix: out[hexR].isEmpty ? 10 : 16), let scalar = Unicode.Scalar(v) else { continue }
                out.replaceSubrange(r, with: String(Character(scalar)))
            }
        }
        for (k, v) in ["&#064;": "@", "&amp;": "&", "&quot;": "\"", "&#039;": "'", "&#x27;": "'", "&lt;": "<", "&gt;": ">"] { out = out.replacingOccurrences(of: k, with: v) }
        return out
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 10) {
            Group {
                if let image = meta?.image, let url = URL(string: image) {
                    AsyncImage(url: url) { $0.resizable().scaledToFill() } placeholder: { Theme.desk }
                } else {
                    Theme.desk.overlay(Image(systemName: "camera").font(.largeTitle).foregroundStyle(Theme.ink.opacity(0.3)))
                }
            }
            .aspectRatio(1, contentMode: .fit).frame(maxWidth: .infinity)
            .clipShape(RoundedRectangle(cornerRadius: 10))
            .onTapGesture { open() }
            HStack(spacing: 6) {
                Image(systemName: "camera.circle.fill").foregroundStyle(Theme.ink.opacity(0.7))
                Text(handle.map { "@\($0)" } ?? "Instagram").font(.system(size: large ? 17 : 15, weight: .bold))
                    .foregroundStyle(Theme.ink)
                Spacer()
                Button(action: open) {
                    HStack(spacing: 3) { Text("Open"); Image(systemName: "arrow.up.right") }.font(Theme.type(12, bold: true))
                }
                .buttonStyle(GlassChipStyle())
            }
            if !caption.isEmpty {
                Text(caption).font(.system(size: large ? 15 : 14, design: .serif)).foregroundStyle(Theme.ink.opacity(0.8))
                    .lineLimit(large ? 6 : 2)
            }
        }
        .task { meta = await ArticleMeta.load(link) }
    }

    func open() { if let u = URL(string: link) { Trail.send("instagram:open"); openURL(u) } }
}
