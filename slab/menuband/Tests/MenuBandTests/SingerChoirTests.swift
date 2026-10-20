import XCTest
@testable import MenuBand

/// The self-choir's pure parts: what the band sounds at a beat, and which
/// of its tones a choir voice takes around the lead.
final class SingerChoirTests: XCTestCase {
    func testEventsParseNotesAndDottedChords() {
        let e = SingerChoir.events("62:1,r:2,62.66.69:4")
        XCTAssertEqual(e.count, 4)
        XCTAssertEqual(e[0].at, 0); XCTAssertEqual(e[0].dur, 1); XCTAssertEqual(e[0].midi, 62)
        XCTAssertEqual(e[1].at, 3); XCTAssertEqual(e[3].midi, 69); XCTAssertEqual(e[3].dur, 4)
    }

    func testChordAtBeatAndCarryThroughRests() {
        let e = SingerChoir.events("50.57.62:1,r:3,55.59.62:1")
        XCTAssertEqual(SingerChoir.chord(at: 0.5, in: e), [2, 9])      // D A (62 = D)
        XCTAssertEqual(SingerChoir.chord(at: 2.0, in: e), [2, 9])      // the rest carries the last strike
        XCTAssertEqual(SingerChoir.chord(at: 4.0, in: e), [7, 11, 2])  // G B D
        XCTAssertEqual(SingerChoir.chord(at: 1.0, in: []), [])
    }

    func testPickThirdAboveAndBelowFromTheChord() {
        // Lead on D4 (62) over a D major triad: up → F#4 (66), down → A3 (57).
        let picks = SingerChoir.pick(lead: 62, chord: [2, 6, 9], voices: ["up", "down"])
        XCTAssertEqual(picks, [66, 57])
    }

    func testPickNeverDoublesTheLeadAndSitsOutWhenNothingFits() {
        // Only the lead's own pitch class sounds: no voice has a note.
        XCTAssertEqual(SingerChoir.pick(lead: 62, chord: [2], voices: ["up", "down"]), [])
        // A fixed interval ignores the chord.
        XCTAssertEqual(SingerChoir.pick(lead: 62, chord: [], voices: ["-12"]), [50])
    }

    func testPickPrefersTheThirdOverTheFifth() {
        // G major under a G lead: up has B (4 above) and D (7 above) → B.
        XCTAssertEqual(SingerChoir.pick(lead: 67, chord: [7, 11, 2], voices: ["up"]), [71])
    }
}
