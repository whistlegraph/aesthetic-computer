import Foundation

/// `.mbscore` — a tiny JSON score Menu Band can auto-perform, for testing the
/// record → DMG pipeline without a human at the keyboard. Point the app at one
/// with `--mbscore <path>` and it arms record mode, plays the notes on time,
/// then hits stop so a take DMG lands on the Desktop.
///
///     {
///       "name": "tape-test",
///       "notes": [
///         { "midi": 60, "start": 0.0, "dur": 0.4 },
///         { "midi": 64, "start": 0.4, "dur": 0.4 }
///       ]
///     }
///
/// `start`/`dur` are seconds. The take ends `tailSeconds` after the last note.
struct MBScore: Decodable {
    struct Note: Decodable {
        let midi: UInt8
        let start: Double
        let dur: Double
        let velocity: UInt8?
    }
    let name: String
    let notes: [Note]
    var tailSeconds: Double = 0.5

    private enum CodingKeys: String, CodingKey { case name, notes, tailSeconds }

    static func load(_ url: URL) -> MBScore? {
        guard let data = try? Data(contentsOf: url) else { return nil }
        return try? JSONDecoder().decode(MBScore.self, from: data)
    }

    /// Total performance length: last note-end plus the tail.
    var duration: Double {
        (notes.map { $0.start + $0.dur }.max() ?? 0) + tailSeconds
    }
}
