import Foundation

enum PreviewFormat: String, CaseIterable, Identifiable {
    case portrait = "2:3"
    case tall = "9:16"
    case square = "1:1"
    case landscape = "4:3"
    case wide = "16:9"
    var id: String { rawValue }
    static let preference = "whistlegraph-preview-format"
    static var saved: Self { UserDefaults.standard.string(forKey: preference).flatMap(Self.init(rawValue:)) ?? .portrait }
    var aspect: CGFloat {
        switch self {
        case .portrait: return 2 / 3
        case .tall: return 9 / 16
        case .square: return 1
        case .landscape: return 4 / 3
        case .wide: return 16 / 9
        }
    }
    func fit(width: CGFloat, height: CGFloat) -> CGSize {
        let w = max(1, min(width, height * aspect))
        return CGSize(width: w, height: w / aspect)
    }
}
