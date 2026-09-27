import XCTest
@testable import MenuBand

final class ToneTrialsTests: XCTestCase {
    func testFirstNoteIsAcceptedImmediately() {
        let trials = ToneTrials()
        trials.start(at: 0)
        trials.registerNote(60, at: 0)
        XCTAssertEqual(trials.progress, 1)
    }

    func testWrongNotesAndElapsedTimeDoNotMoveTheTrack() {
        let trials = ToneTrials()
        trials.start(at: 0)
        trials.registerNote(60, at: 3)
        trials.registerNote(63, at: 4)
        trials.update(at: 500)
        XCTAssertEqual(trials.progress, 1)
        XCTAssertEqual(trials.index, 0)
        trials.registerNote(62, at: 501)
        XCTAssertEqual(trials.progress, 2)
    }

    func testRestartReturnsToFirstScaleImmediately() {
        let trials = ToneTrials()
        trials.start(at: 5)
        trials.start(at: 0)
        XCTAssertEqual(trials.trial.title, "C Major")
        XCTAssertEqual(trials.progress, 0)
        trials.registerNote(60, at: 20)
        XCTAssertEqual(trials.progress, 1)
        trials.stop()
        trials.registerNote(60, at: 30)
        XCTAssertNil(trials.snapshot(at: 30))
        XCTAssertEqual(trials.progress, 0)
    }
}
