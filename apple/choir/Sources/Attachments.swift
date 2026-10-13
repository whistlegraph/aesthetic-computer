import SwiftUI
import PhotosUI
import UniformTypeIdentifiers

/// Something picked but not yet sent.
struct Pending: Identifiable {
    let id = UUID()
    var data: Data
    var original: UIImage? = nil     // unfiltered, so filters can be swapped
    var filter: PhotoFilter = .original
    var name: String
    var type: String
    var width: Int? = nil
    var height: Int? = nil
    var thumbnail: UIImage? = nil
    var duration: Double? = nil
    var transcript: String? = nil
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
        guard let (data, type, ext) = encode(filtered, filter: filter) else { return nil }
        return Pending(data: data, original: resized, filter: filter, name: "choir-\(Int(Date().timeIntervalSince1970)).\(ext)",
                       type: type, width: Int(filtered.size.width), height: Int(filtered.size.height), thumbnail: filtered)
    }

    /// The daisy keeps its transparency (PNG, ≤1080 px: a 1440 one weighed
    /// 1.5 MB and crawled in over cellular); everything else is JPEG.
    static func encode(_ image: UIImage, filter: PhotoFilter) -> (Data, String, String)? {
        if filter == .daisyIris {
            let side = min(1080, image.size.width * image.scale)
            let format = UIGraphicsImageRendererFormat(); format.scale = 1; format.opaque = false
            let small = UIGraphicsImageRenderer(size: CGSize(width: side, height: side), format: format)
                .image { _ in image.draw(in: CGRect(x: 0, y: 0, width: side, height: side)) }
            return small.pngData().map { ($0, "image/png", "png") }
        }
        return image.jpegData(compressionQuality: 0.85).map { ($0, "image/jpeg", "jpg") }
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
                            else if let d = p.duration {
                                VStack(spacing: 2) { Image(systemName: "waveform"); Text(RecordingBar.clock(d)).font(.system(size: 10, weight: .semibold)) }.foregroundStyle(Theme.ink)
                            }
                            else { VStack(spacing: 2) { Image(systemName: p.type == "application/pdf" ? "doc.richtext" : "doc"); Text(p.name).font(.system(size: 9)).lineLimit(2) }.padding(4).foregroundStyle(Theme.ink) }
                        }
                        .frame(width: 56, height: 56).background(Theme.paper).clipShape(RoundedRectangle(cornerRadius: 8))
                        .overlay(alignment: .bottomLeading) {
                            if p.original != nil {
                                Hand(p.filter == .original ? "FlowerO" : "FlowerDot", height: 12).foregroundStyle(Theme.ink).padding(3)
                                    .background(.white.opacity(0.85), in: Circle()).padding(3)
                            }
                        }
                        .onTapGesture { if p.original != nil { filtering = p } }
                        Button { items.removeAll { $0.id == p.id } } label: {
                            Hand("HandX", height: 9).foregroundStyle(.white).padding(5).background(.black.opacity(0.6), in: Circle())
                        }
                            .offset(x: 6, y: -6).accessibilityLabel("Remove")
                    }
                }
            }
            .padding(.horizontal, 14).padding(.top, 6)
        }
        .sheet(item: $filtering) { p in
            if let original = p.original {
                FilterSheet(original: original, current: p.filter) { image, filter in
                    guard let i = items.firstIndex(where: { $0.id == p.id }), let (data, type, ext) = Picks.encode(image, filter: filter) else { return }
                    items[i].data = data; items[i].thumbnail = image; items[i].filter = filter; items[i].type = type
                    items[i].width = Int(image.size.width); items[i].height = Int(image.size.height)
                    items[i].name = (items[i].name as NSString).deletingPathExtension + "." + ext
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
    var files: [Attachment] { items.filter { !$0.isImage && !$0.isVoice } }
    var voices: [Attachment] { items.filter(\.isVoice) }

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
            ForEach(voices, id: \.url) { VoiceNoteView(note: $0) }
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
        RemoteImage(url: URL(string: a.url), maxPixels: 900) { $0.resizable().scaledToFill() } placeholder: { failed in
            Theme.desk.opacity(0.6).overlay(FlowerSpinner(size: 26).opacity(failed ? 0 : 1))
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
            RemoteImage(url: URL(string: attachment.url), maxPixels: 3000) { $0.resizable().scaledToFit() } placeholder: { _ in
                ProgressView().tint(.white)
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
                ShareLink(item: URL(string: attachment.url)!) { Hand("HandArrow", height: 16) }.buttonStyle(GlassChipStyle())
                    .accessibilityLabel("Share")
                Button { dismiss() } label: { Hand("HandX", height: 16) }.buttonStyle(GlassChipStyle()).accessibilityLabel("Close")
            }
            .padding()
        }
    }
}
