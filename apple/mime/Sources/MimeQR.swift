import SwiftUI
import CoreImage.CIFilterBuiltins

extension Post {
    /// The card's mime.ac address, as the web feed writes it (postPath in
    /// system/public/mime/index.html): /wg… for Whistlegraph pieces, /p/<code> otherwise.
    var shareURL: URL {
        let kind = media?.kind ?? "", short = media?.code ?? ""
        let id = ["mood", "chat"].contains(kind) && !short.isEmpty ? kind + "~" + short
            : ["wgpiece", "whistlegraph"].contains(kind) && short.range(of: #"^wg[A-Za-z0-9]+$"#, options: .regularExpression) != nil ? short : code
        let path = id.range(of: #"^wg[A-Za-z0-9]+$"#, options: .regularExpression) != nil
            ? "/" + id : "/p/" + (id.addingPercentEncoding(withAllowedCharacters: .alphanumerics.union(.init(charactersIn: "-_.~"))) ?? id)
        return URL(string: "https://mime.ac" + path)!
    }
}

/// Tapping a card's code: its mime.ac address as a full-screen QR, for another
/// phone to scan. Tap anywhere to close.
struct MimeQR: View {
    let post: Post
    let close: () -> Void

    var body: some View {
        VStack(spacing: 22) {
            PalText(text: post.command, size: 30, accent: Color(hex: 0xED629D))
            if let image = Self.code(post.shareURL.absoluteString) {
                Image(uiImage: image).interpolation(.none).resizable().scaledToFit()
                    .padding(18).background(.white, in: RoundedRectangle(cornerRadius: 18))
                    .padding(.horizontal, 28)
                    .accessibilityLabel("QR code for \(post.shareURL.absoluteString)")
            }
            Text(post.shareURL.absoluteString.replacingOccurrences(of: "https://", with: ""))
                .font(.custom("ComicRelief-Bold", size: 17)).foregroundStyle(.white.opacity(0.8))
                .lineLimit(1).minimumScaleFactor(0.6).padding(.horizontal, 28)
            ShareLink(item: post.shareURL) {
                Label("Share", systemImage: "square.and.arrow.up").font(.custom("ComicRelief-Bold", size: 17))
                    .padding(.horizontal, 18).padding(.vertical, 10).background(.white.opacity(0.14), in: Capsule())
            }.tint(.white)
        }
        .frame(maxWidth: .infinity, maxHeight: .infinity)
        .background(Color.paper.ignoresSafeArea())
        .contentShape(Rectangle())
        .onTapGesture(perform: close)
        .accessibilityAction(.escape, close)
        .accessibilityIdentifier("mime-qr")
    }

    static func code(_ text: String) -> UIImage? {
        let filter = CIFilter.qrCodeGenerator()
        filter.message = Data(text.utf8)
        filter.correctionLevel = "M"
        guard let output = filter.outputImage,
              let cg = CIContext().createCGImage(output, from: output.extent) else { return nil }
        return UIImage(cgImage: cg)
    }
}
