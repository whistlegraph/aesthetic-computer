import Foundation

struct PlaybackWord: Decodable {
    let text: String
    let atMs: Double
    let durationMs: Double

    static func range(at milliseconds: Double, in words: [PlaybackWord], text: String) -> NSRange? {
        let sentence = text as NSString
        var offset = 0
        for word in words {
            let range = sentence.range(of: word.text, options: .caseInsensitive, range: NSRange(location: offset, length: sentence.length - offset))
            guard range.location != NSNotFound else { continue }
            if milliseconds >= word.atMs && milliseconds < word.atMs + word.durationMs { return range }
            offset = NSMaxRange(range)
        }
        return nil
    }
    static func at(_ milliseconds: Double, in words: [PlaybackWord]) -> String {
        guard milliseconds.isFinite else { return "" }
        return words.last(where: {
            $0.atMs.isFinite && $0.durationMs.isFinite && $0.durationMs > 0 &&
            milliseconds >= $0.atMs && milliseconds < $0.atMs + $0.durationMs
        })?.text ?? ""
    }
}
