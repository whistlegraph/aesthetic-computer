import Foundation

/// The recording bank inside custom instrument `1. These IDs are persisted.
enum PianoSource: Int, CaseIterable {
    case salamander = 0, steinway, yamahaUpright

    static let didChange = Notification.Name("MenuBandPianoSourceDidChange")

    var label: String {
        switch self {
        case .salamander: return "Salamander Grand"
        case .steinway: return "Steinway B"
        case .yamahaUpright: return "Yamaha Upright"
        }
    }

    var numberedLabel: String { "\(rawValue + 1) · \(label)" }

    var anchorNotes: [Int] {
        switch self {
        case .salamander: return Array(stride(from: 21, through: 96, by: 3))
        case .steinway: return Array(stride(from: 22, through: 104, by: 2))
        case .yamahaUpright: return [24, 31, 36, 43, 48, 55, 60, 67, 72, 79, 84, 91, 96]
        }
    }

    func bankURLs() -> [URL] {
        if self == .salamander { return MenuBandACPianoVoice.bankURLs() }
        let prefix = self == .steinway ? "piano-steinway" : "piano-yamaha"
        return anchorNotes.compactMap {
            Bundle.appResources.url(forResource: "\(prefix)-\($0)", withExtension: "raw")
        }
    }

    var isAvailable: Bool {
        Self.availability[self] ?? false
    }

    private static let availability: [PianoSource: Bool] = Dictionary(uniqueKeysWithValues: allCases.map { source in
        let notes = Set(source.bankURLs().compactMap { midiNote(in: $0) })
        return (source, notes == Set(source.anchorNotes))
    })

    static func midiNote(in url: URL) -> Int? {
        Int(url.deletingPathExtension().lastPathComponent.split(separator: "-").last ?? "")
    }

    func stepped(by delta: Int) -> PianoSource {
        let available = Self.allCases.filter { $0.isAvailable }
        guard !available.isEmpty else { return self }
        let index = available.firstIndex(of: self) ?? 0
        return available[((index + delta) % available.count + available.count) % available.count]
    }
}
