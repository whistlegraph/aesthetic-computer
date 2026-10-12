import Foundation

// Data-only presentation contract shared with mime.ac. Unknown schemas and
// invalid values leave the last valid layout in place; no remote code runs.
struct MimeLayout: Codable, Equatable {
    let schemaVersion: Int
    let revision: String
    let identifierSize: Double
    let handleSize: Double
    let mimeTypeSize: Double
    let captionSize: Double
    let titleSize: Double
    let padding: Double
    let showCaption: Bool
    let showTitle: Bool
    let showOrigin: Bool

    var isValid: Bool {
        schemaVersion == 1 && !revision.isEmpty && revision.count <= 80 &&
        [identifierSize, handleSize, captionSize, titleSize].allSatisfy { (12...32).contains($0) } &&
        (10...24).contains(mimeTypeSize) && (8...32).contains(padding)
    }
    static func decode(_ data: Data) -> MimeLayout? {
        guard data.count <= 4096, let layout = try? JSONDecoder().decode(Self.self, from: data), layout.isValid else { return nil }
        return layout
    }
    static let standard = MimeLayout(schemaVersion: 1, revision: "2026-10-12.1",
        identifierSize: 21, handleSize: 21, mimeTypeSize: 21, captionSize: 17,
        titleSize: 14, padding: 14, showCaption: false, showTitle: false, showOrigin: true)
    static var bundled: MimeLayout {
        guard let url = Bundle.main.url(forResource: "mime-layout", withExtension: "json"),
              let data = try? Data(contentsOf: url), let layout = decode(data) else { return standard }
        return layout
    }
}
