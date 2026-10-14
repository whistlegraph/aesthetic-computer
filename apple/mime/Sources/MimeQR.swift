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

/// Tapping a card's code slides its mime.ac address out of the top-left
/// corner as a small QR, like KidLisp's corner QR in AC: crisp cells on a
/// white quiet zone, the short address under it. Tap it to tuck it away.
struct CornerQR: View {
    let post: Post
    let close: () -> Void

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            if let image = Self.code(post.shareURL.absoluteString) {
                Image(uiImage: image).interpolation(.none).resizable()
                    .frame(width: 168, height: 168)
                    .padding(8).background(.white, in: RoundedRectangle(cornerRadius: 6))
                    .shadow(color: .black.opacity(0.35), radius: 10, y: 4)
                    .accessibilityLabel("QR code for \(post.shareURL.absoluteString)")
            }
            PalText(text: post.shareURL.absoluteString.replacingOccurrences(of: "https://", with: ""), size: 14, accent: Color(hex: 0xED629D), animated: false)
        }
        .contentShape(Rectangle())
        .onTapGesture(perform: close)
        .contextMenu { ShareLink(item: post.shareURL) }
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
