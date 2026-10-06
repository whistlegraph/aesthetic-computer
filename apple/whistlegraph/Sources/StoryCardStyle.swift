import Foundation

enum StoryCardStyle {
    // Dark enough for white captions and the interface's cyan/pink lettering.
    static let backgrounds = [
        [58, 38, 89], [19, 71, 77], [95, 44, 42], [31, 52, 103],
        [53, 74, 38], [91, 33, 67], [22, 75, 57], [90, 57, 30]
    ]
    static func background(code: String, version: Int) -> [Int] {
        let seed = code.unicodeScalars.reduce(0) { ($0 + Int($1.value)) % backgrounds.count }
        return backgrounds[(seed + max(0, version) % backgrounds.count) % backgrounds.count]
    }
    static func cssBackground(code: String, version: Int) -> String {
        "rgb(" + background(code: code, version: version).map(String.init).joined(separator: ",") + ")"
    }
}
