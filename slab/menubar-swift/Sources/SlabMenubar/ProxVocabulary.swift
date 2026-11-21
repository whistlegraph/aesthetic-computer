import AppKit

/// The house words of a session: names the person typed that no dictionary
/// knows (dool, aesel, lith, kidlisp). A language model reads those as typos
/// and "corrects" them to the nearest English word, so `dool` comes back as
/// `doom`. This finds them in what was actually said, hands them to the model
/// as spellings to copy, and puts back any the model swapped anyway.
enum ProxVocabulary {
    private static let word = try! NSRegularExpression(pattern: "[A-Za-z][A-Za-z0-9]{2,}")

    /// Names the dictionary cannot be trusted on. macOS knows `dool` as an old
    /// Scots word, so a spell check alone lets the one that started this
    /// through. Only words that are not everyday English belong here: `slab`
    /// and `easel` need no guarding and would only invite false repairs.
    /// A seat adds its own, one per line, in `~/.config/slab/lexicon.txt`.
    private static let house: Set<String> = [
        "acin", "aesel", "aesthetivox", "amail", "botbop", "capur", "compushloy",
        "dool", "fuser", "hellsine", "hopalong", "illy", "jasellite", "kidlisp",
        "lith", "loopboy", "macpal", "mathnom", "menuband", "nela", "notepat",
        "nullnoise", "oskie", "oskiewar", "prox", "pruttivox", "slidecop",
        "sotce", "tiat", "whistlegraph", "wordcrust",
    ]

    private static var lexicon: Set<String> {
        let path = "\(Paths.home)/.config/slab/lexicon.txt"
        guard let text = try? String(contentsOfFile: path, encoding: .utf8) else { return house }
        let mine = text.split(whereSeparator: \.isNewline)
            .map { $0.trimmingCharacters(in: .whitespaces).lowercased() }
            .filter { !$0.isEmpty && !$0.hasPrefix("#") }
        return house.union(mine)
    }

    /// Function words a summary may introduce on its own; never a lost name.
    private static let plain: Set<String> = [
        "also", "been", "does", "done", "from", "have", "into", "just", "like",
        "list", "made", "make", "more", "most", "much", "need", "now", "once",
        "only", "over", "some", "such", "than", "that", "them", "then", "they",
        "this", "was", "were", "what", "when", "will", "with", "work",
    ]

    private static func words(in text: String) -> [String] {
        let ns = text as NSString
        return word.matches(in: text, range: NSRange(location: 0, length: ns.length))
            .map { ns.substring(with: $0.range) }
    }

    /// Words in `source` from the house lexicon or that the system dictionary rejects, most-used first, in the
    /// casing the session uses. Call off the main thread: the spell server is
    /// main-affine and is reached with one synchronous hop.
    static func terms(in source: String, limit: Int = 40) -> [String] {
        var counts: [String: Int] = [:]
        var casing: [String: String] = [:]
        for token in words(in: source) {
            let key = token.lowercased()
            counts[key, default: 0] += 1
            if casing[key] == nil { casing[key] = token }
        }
        let candidates = counts.keys.sorted()
        guard !candidates.isEmpty else { return [] }
        let joined = candidates.joined(separator: " ") as NSString
        var unknown: [String] = []
        let check = {
            let checker = NSSpellChecker.shared
            var at = 0
            while at < joined.length {
                let miss = checker.checkSpelling(
                    of: joined as String, startingAt: at, language: nil, wrap: false,
                    inSpellDocumentWithTag: 0, wordCount: nil)
                guard miss.location != NSNotFound, miss.length > 0 else { break }
                unknown.append(joined.substring(with: miss))
                at = miss.location + miss.length
            }
        }
        if Thread.isMainThread { check() } else { DispatchQueue.main.sync(execute: check) }
        let known = lexicon
        return Set(unknown).union(candidates.filter(known.contains))
            .filter { counts[$0] != nil }
            .sorted { (counts[$0] ?? 0, $1) > (counts[$1] ?? 0, $0) }
            .prefix(limit)
            .compactMap { casing[$0] }
    }

    /// The line that tells the model these are names, not mistakes.
    static func instruction(for terms: [String]) -> String {
        guard !terms.isEmpty else { return "" }
        return "These words are project names and are spelled correctly. Copy them letter for letter and never replace them with a similar English word: \(terms.joined(separator: ", "))."
    }

    /// Put a house word back where the model wrote its dictionary neighbour.
    /// A word is restored only when the session never used it, exactly one
    /// house word sits one edit away, and that house word is otherwise missing
    /// from the text — so a summary that already says `dool` keeps its `doom`.
    static func restore(_ text: String, terms: [String], source: String) -> String {
        guard !terms.isEmpty, !text.isEmpty else { return text }
        let said = Set(words(in: source).map { $0.lowercased() })
        let written = Set(words(in: text).map { $0.lowercased() })
        let missing = terms.filter { !written.contains($0.lowercased()) }
        guard !missing.isEmpty else { return text }

        let ns = text as NSString
        var out = text as NSString
        let matches = word.matches(in: text, range: NSRange(location: 0, length: ns.length))
        for match in matches.reversed() {
            let token = ns.substring(with: match.range)
            let lower = token.lowercased()
            guard !said.contains(lower), !plain.contains(lower) else { continue }
            let near = missing.filter { term in
                let t = term.lowercased()
                // An inflection (dools, dool's) is the model using the word, not losing it.
                guard !lower.hasPrefix(t), !t.hasPrefix(lower) else { return false }
                return distance(lower, t) == 1
            }
            guard near.count == 1, var term = near.first else { continue }
            if term == term.lowercased(), let first = token.first, first.isUppercase {
                term = term.prefix(1).uppercased() + term.dropFirst()
            }
            out = out.replacingCharacters(in: match.range, with: term) as NSString
        }
        return out as String
    }

    /// Levenshtein distance; inputs are single words, so the table stays tiny.
    static func distance(_ a: String, _ b: String) -> Int {
        let x = Array(a), y = Array(b)
        if abs(x.count - y.count) > 1 { return 2 }
        var row = Array(0...y.count)
        for i in 1...max(x.count, 1) where !x.isEmpty {
            var diagonal = row[0]
            row[0] = i
            for j in 1...max(y.count, 1) where !y.isEmpty {
                let above = row[j]
                row[j] = min(above + 1, row[j - 1] + 1, diagonal + (x[i - 1] == y[j - 1] ? 0 : 1))
                diagonal = above
            }
        }
        return row[y.count]
    }
}
