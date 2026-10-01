import Foundation

struct PlaybackWord: Decodable {
    let text: String
    let atMs: Double
    let durationMs: Double

    static func at(_ milliseconds: Double, in words: [PlaybackWord]) -> String {
        guard milliseconds.isFinite else { return "" }
        return words.last(where: {
            $0.atMs.isFinite && $0.durationMs.isFinite && $0.durationMs > 0 &&
            milliseconds >= $0.atMs && milliseconds < $0.atMs + $0.durationMs
        })?.text ?? ""
    }
}
