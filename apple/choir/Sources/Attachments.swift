import SwiftUI
import PhotosUI
import UniformTypeIdentifiers

/// Something picked but not yet sent.
struct Pending: Identifiable {
    let id = UUID()
    let data: Data
    let name: String
    let type: String
    var width: Int? = nil
    var height: Int? = nil
    var thumbnail: UIImage? = nil
}

enum Picks {
    static let maxBytes = 25 * 1024 * 1024
    /// Photos become JPEG (HEIC isn't everywhere), at most 2048 px on the long side.
    static func photo(_ item: PhotosPickerItem) async -> Pending? {
        guard let data = try? await item.loadTransferable(type: Data.self), let image = UIImage(data: data) else { return nil }
        let long = max(image.size.width, image.size.height), scale = min(1, 2048 / max(long, 1))
        let size = CGSize(width: image.size.width * scale, height: image.size.height * scale)
        let format = UIGraphicsImageRendererFormat(); format.scale = 1
        let resized = UIGraphicsImageRenderer(size: size, format: format).image { _ in image.draw(in: CGRect(origin: .zero, size: size)) }
        guard let jpg = resized.jpegData(compressionQuality: 0.85) else { return nil }
        return Pending(data: jpg, name: "photo-\(Int(Date().timeIntervalSince1970)).jpg", type: "image/jpeg",
                       width: Int(size.width), height: Int(size.height), thumbnail: resized)
    }
    static func file(_ url: URL) -> Pending? {
        let scoped = url.startAccessingSecurityScopedResource(); defer { if scoped { url.stopAccessingSecurityScopedResource() } }
        guard let data = try? Data(contentsOf: url), data.count <= maxBytes else { return nil }
        let type = UTType(filenameExtension: url.pathExtension)?.preferredMIMEType ?? "application/octet-stream"
        let image = type.hasPrefix("image/") ? UIImage(data: data) : nil
        return Pending(data: data, name: url.lastPathComponent, type: type,
                       width: image.map { Int($0.size.width) }, height: image.map { Int($0.size.height) }, thumbnail: image)
    }
    static let fileTypes: [UTType] = [.pdf, .image, .audio, .movie, .plainText, .zip]
}

/// Chips above the text box for what will be sent.
struct PendingStrip: View {
    @Binding var items: [Pending]
    var body: some View {
        ScrollView(.horizontal, showsIndicators: false) {
            HStack(spacing: 8) {
                ForEach(items) { p in
                    ZStack(alignment: .topTrailing) {
                        Group {
                            if let t = p.thumbnail { Image(uiImage: t).resizable().scaledToFill() }
                            else { VStack(spacing: 2) { Image(systemName: p.type == "application/pdf" ? "doc.richtext" : "doc"); Text(p.name).font(.system(size: 9)).lineLimit(2) }.padding(4).foregroundStyle(Theme.ink) }
                        }
                        .frame(width: 56, height: 56).background(Theme.paper).clipShape(RoundedRectangle(cornerRadius: 8))
                        Button { items.removeAll { $0.id == p.id } } label: { Image(systemName: "xmark.circle.fill").symbolRenderingMode(.palette).foregroundStyle(.white, .black.opacity(0.6)) }
                            .offset(x: 6, y: -6).accessibilityLabel("Remove")
                    }
                }
            }
            .padding(.horizontal, 14).padding(.top, 6)
        }
    }
}

/// Attachments inside a message: modest photo thumbnails, files as rows.
struct AttachmentsView: View {
    let items: [Attachment]
    @Environment(\.openURL) private var openURL
    var images: [Attachment] { items.filter(\.isImage) }
    var files: [Attachment] { items.filter { !$0.isImage } }

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            if images.count == 1, let a = images.first {
                photo(a).frame(maxWidth: 220, maxHeight: 220)
                    .aspectRatio(CGFloat(a.width ?? 1) / CGFloat(max(a.height ?? 1, 1)), contentMode: .fit)
            } else if images.count > 1 {
                LazyVGrid(columns: [GridItem(.fixed(108), spacing: 4), GridItem(.fixed(108), spacing: 4)], spacing: 4) {
                    ForEach(images, id: \.url) { photo($0).frame(width: 108, height: 108) }
                }
            }
            ForEach(files, id: \.url) { a in
                Button { if let u = URL(string: a.url) { Trail.send("attachment:open"); openURL(u) } } label: {
                    HStack(spacing: 8) {
                        Image(systemName: a.type == "application/pdf" ? "doc.richtext.fill" : a.type.hasPrefix("audio/") ? "waveform" : a.type.hasPrefix("video/") ? "film" : "doc.fill")
                            .font(.title3).foregroundStyle(Theme.head)
                        VStack(alignment: .leading, spacing: 1) {
                            Text(a.name).font(.system(size: 14, weight: .semibold)).lineLimit(1)
                            Text(ByteCountFormatter.string(fromByteCount: Int64(a.size), countStyle: .file)).font(.system(size: 11)).opacity(0.6)
                        }
                        Spacer(minLength: 0)
                    }
                    .foregroundStyle(Theme.ink).padding(8).background(Theme.desk.opacity(0.5), in: RoundedRectangle(cornerRadius: 8))
                }
                .buttonStyle(.plain)
            }
        }
    }

    func photo(_ a: Attachment) -> some View {
        AsyncImage(url: URL(string: a.url)) { $0.resizable().scaledToFill() } placeholder: { Theme.desk }
            .clipShape(RoundedRectangle(cornerRadius: 8))
            .contentShape(Rectangle())
            .onTapGesture { if let u = URL(string: a.url) { Trail.send("attachment:open"); openURL(u) } }
    }
}
