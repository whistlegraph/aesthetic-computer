import SwiftUI
import PhotosUI
import UniformTypeIdentifiers

/// Something picked but not yet sent.
struct Pending: Identifiable {
    let id = UUID()
    var data: Data
    var original: UIImage? = nil     // unfiltered, so filters can be swapped
    var filter: PhotoFilter = .original
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
        return Pending(data: jpg, original: resized, name: "photo-\(Int(Date().timeIntervalSince1970)).jpg", type: "image/jpeg",
                       width: Int(size.width), height: Int(size.height), thumbnail: resized)
    }
    /// A camera shot: kept unfiltered as the original, sent with its filter baked in.
    static func shot(_ image: UIImage, filter: PhotoFilter) -> Pending? {
        let long = max(image.size.width, image.size.height), scale = min(1, 2048 / max(long, 1))
        let size = CGSize(width: image.size.width * scale, height: image.size.height * scale)
        let format = UIGraphicsImageRendererFormat(); format.scale = 1
        let resized = UIGraphicsImageRenderer(size: size, format: format).image { _ in image.draw(in: CGRect(origin: .zero, size: size)) }
        let filtered = filter.apply(resized)
        guard let jpg = filtered.jpegData(compressionQuality: 0.85) else { return nil }
        return Pending(data: jpg, original: resized, filter: filter, name: "choir-\(Int(Date().timeIntervalSince1970)).jpg",
                       type: "image/jpeg", width: Int(size.width), height: Int(size.height), thumbnail: filtered)
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
    @State private var filtering: Pending?
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
                        .overlay(alignment: .bottomLeading) {
                            if p.original != nil {
                                Image(systemName: p.filter == .original ? "camera.filters" : "sparkles")
                                    .font(.system(size: 10, weight: .bold)).padding(3)
                                    .background(.white.opacity(0.85), in: Circle()).padding(3)
                            }
                        }
                        .onTapGesture { if p.original != nil { filtering = p } }
                        Button { items.removeAll { $0.id == p.id } } label: { Image(systemName: "xmark.circle.fill").symbolRenderingMode(.palette).foregroundStyle(.white, .black.opacity(0.6)) }
                            .offset(x: 6, y: -6).accessibilityLabel("Remove")
                    }
                }
            }
            .padding(.horizontal, 14).padding(.top, 6)
        }
        .sheet(item: $filtering) { p in
            if let original = p.original {
                FilterSheet(original: original, current: p.filter) { image, filter in
                    guard let i = items.firstIndex(where: { $0.id == p.id }), let jpg = image.jpegData(compressionQuality: 0.85) else { return }
                    items[i].data = jpg; items[i].thumbnail = image; items[i].filter = filter
                }
            }
        }
    }
}

/// Attachments inside a message: photos sized from their own proportions,
/// files as rows. Tapping a photo opens it full screen.
struct AttachmentsView: View {
    let items: [Attachment]
    @Environment(\.openURL) private var openURL
    @State private var viewing: Attachment?
    var images: [Attachment] { items.filter(\.isImage) }
    var files: [Attachment] { items.filter { !$0.isImage } }

    /// One photo: 240 pt wide, height from its aspect, at most 300 pt.
    static func size(_ a: Attachment) -> CGSize {
        let ratio = CGFloat(a.height ?? 4) / CGFloat(max(a.width ?? 3, 1))
        let w: CGFloat = 240
        let h = min(300, max(120, w * ratio))
        return CGSize(width: h == 300 ? 300 / ratio : w, height: h)
    }

    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            if images.count == 1, let a = images.first {
                let sz = Self.size(a)
                photo(a).frame(width: sz.width, height: sz.height)
            } else if images.count > 1 {
                LazyVGrid(columns: [GridItem(.fixed(118), spacing: 4), GridItem(.fixed(118), spacing: 4)], spacing: 4) {
                    ForEach(images, id: \.url) { photo($0).frame(width: 118, height: 118) }
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
                    .foregroundStyle(Theme.ink).padding(8).frame(maxWidth: 240)
                    .background(Theme.desk.opacity(0.5), in: RoundedRectangle(cornerRadius: 8))
                }
                .buttonStyle(.plain)
            }
        }
        .fullScreenCover(item: $viewing) { PhotoViewer(attachment: $0) }
    }

    func photo(_ a: Attachment) -> some View {
        AsyncImage(url: URL(string: a.url), transaction: Transaction(animation: .easeOut(duration: 0.2))) { phase in
            if let image = phase.image { image.resizable().scaledToFill() }
            else { Theme.desk.opacity(0.6).overlay(ProgressView().opacity(phase.error == nil ? 1 : 0)) }
        }
        .clipShape(RoundedRectangle(cornerRadius: 12))
        .contentShape(RoundedRectangle(cornerRadius: 12))
        .onTapGesture { Trail.send("photo:view"); viewing = a }
    }
}

extension Attachment: Identifiable { var id: String { url } }

/// Full screen: pinch to zoom, double-tap to fit, swipe down to close.
struct PhotoViewer: View {
    let attachment: Attachment
    @Environment(\.dismiss) private var dismiss
    @State private var zoom: CGFloat = 1
    @State private var lastZoom: CGFloat = 1
    @State private var drag: CGSize = .zero

    var body: some View {
        ZStack(alignment: .topTrailing) {
            Color.black.opacity(1 - min(abs(drag.height) / 400, 0.6)).ignoresSafeArea()
            AsyncImage(url: URL(string: attachment.url)) { phase in
                if let image = phase.image { image.resizable().scaledToFit() } else { ProgressView().tint(.white) }
            }
            .scaleEffect(zoom).offset(y: zoom == 1 ? drag.height : 0)
            .gesture(MagnificationGesture()
                .onChanged { zoom = max(1, min(5, lastZoom * $0)) }
                .onEnded { _ in lastZoom = zoom })
            .simultaneousGesture(DragGesture()
                .onChanged { if zoom == 1 { drag = $0.translation } }
                .onEnded { v in if zoom == 1 && abs(v.translation.height) > 120 { dismiss() } else { withAnimation(.spring) { drag = .zero } } })
            .onTapGesture(count: 2) { withAnimation(.spring) { zoom = zoom > 1 ? 1 : 2.5; lastZoom = zoom } }
            .frame(maxWidth: .infinity, maxHeight: .infinity)
            HStack(spacing: 10) {
                ShareLink(item: URL(string: attachment.url)!) { Image(systemName: "square.and.arrow.up") }.buttonStyle(GlassChipStyle())
                Button { dismiss() } label: { Image(systemName: "xmark") }.buttonStyle(GlassChipStyle())
            }
            .padding()
        }
    }
}
