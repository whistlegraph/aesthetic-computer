import Foundation
import IOKit

/// A closed lid at cue time. The member cannot be seen, so instead of its
/// line it sings "open me" to the same notes — every line of the score,
/// captions included — until someone opens it.
extension AppDelegate {
    static func lidIsClosed() -> Bool {
        let service = IOServiceGetMatchingService(kIOMainPortDefault, IOServiceMatching("IOPMrootDomain"))
        guard service != 0 else { return false }
        defer { IOObjectRelease(service) }
        let value = IORegistryEntryCreateCFProperty(service, "AppleClamshellState" as CFString, kCFAllocatorDefault, 0)
        return (value?.takeRetainedValue() as? Bool) ?? false
    }

    /// Rewrite lyric-source text (one token per note, `-` joins syllables,
    /// ` / ` breaks lines) to "o-pen me", keeping every line's syllable count
    /// so the melody is untouched.
    static func openMeLyrics(for lyrics: String) -> String {
        lyrics.split(separator: "/", omittingEmptySubsequences: false).map { line -> String in
            let tokens = line.split(separator: " ").filter { !$0.isEmpty }
            var left = tokens.reduce(0) { $0 + $1.split(separator: "-").count }
            var out: [String] = []
            while left >= 3 { out += ["o-pen", "me"]; left -= 3 }
            if left == 2 { out.append("o-pen") } else if left == 1 { out.append("me") }
            return out.joined(separator: " ")
        }.joined(separator: " / ")
    }
}
