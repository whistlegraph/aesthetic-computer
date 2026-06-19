import XCTest
@testable import MenuBand

/// Pure-logic tests for the gamepad note mapping — no GameController needed.
/// Guards the two invariants the feature rests on: a gamepad always plays a
/// C-major scale regardless of keymap, and each note layout assigns the
/// eight buttons to the eight distinct scale degrees.
final class GamepadMappingTests: XCTestCase {

    // MARK: - C-major key-code tables

    func testCMajorKeyCodesNotepat() {
        // C D E F G A B +C  →  notepat key codes
        XCTAssertEqual(MenuBandLayout.cMajorKeyCodes(for: .notepat),
                       [8, 2, 14, 3, 5, 0, 11, 4])
    }

    func testCMajorKeyCodesAbleton() {
        XCTAssertEqual(MenuBandLayout.cMajorKeyCodes(for: .ableton),
                       [0, 1, 2, 3, 5, 4, 38, 40])
    }

    /// The key codes must resolve to the actual C-major MIDI notes
    /// (60 62 64 65 67 69 71 72) through the same lookup `playKeyEvent`
    /// uses — in both keymaps.
    func testKeyCodesProduceCMajorMidiNotes() {
        let expected: [UInt8] = [60, 62, 64, 65, 67, 69, 71, 72]
        for keymap in [Keymap.notepat, .ableton] {
            let notes = MenuBandLayout.cMajorKeyCodes(for: keymap).map {
                MenuBandLayout.midiNote(forKeyCode: $0, octaveShift: 0, keymap: keymap)
            }
            XCTAssertEqual(notes, expected.map { Optional($0) },
                           "keymap \(keymap) should play C major")
        }
    }

    func testCMajorSemitones() {
        XCTAssertEqual(MenuBandLayout.cMajorSemitones, [0, 2, 4, 5, 7, 9, 11, 12])
    }

    // MARK: - Note layouts

    func testClockwiseLayoutDegrees() {
        let l = GamepadNoteLayout.clockwise
        XCTAssertEqual(l.degree(for: .dpadLeft), 0)   // C
        XCTAssertEqual(l.degree(for: .dpadUp), 1)     // D
        XCTAssertEqual(l.degree(for: .dpadRight), 2)  // E
        XCTAssertEqual(l.degree(for: .dpadDown), 3)   // F
        XCTAssertEqual(l.degree(for: .faceWest), 4)   // G
        XCTAssertEqual(l.degree(for: .faceNorth), 5)  // A
        XCTAssertEqual(l.degree(for: .faceEast), 6)   // B
        XCTAssertEqual(l.degree(for: .faceSouth), 7)  // +C
    }

    /// Every layout must be a bijection: the eight buttons cover degrees
    /// 0...7 with no gaps or collisions, and the d-pad carries the low
    /// tetrachord (0–3) while the face buttons carry the high one (4–7).
    func testEveryLayoutIsACompletePermutation() {
        let dpad: [GamepadNoteButton] = [.dpadLeft, .dpadUp, .dpadRight, .dpadDown]
        let face: [GamepadNoteButton] = [.faceWest, .faceNorth, .faceEast, .faceSouth]
        for layout in GamepadNoteLayout.allCases {
            let degrees = GamepadNoteButton.allCases.map { layout.degree(for: $0) }
            XCTAssertEqual(Set(degrees), Set(0...7),
                           "\(layout) should cover all eight scale degrees")
            XCTAssertEqual(Set(dpad.map { layout.degree(for: $0) }), Set(0...3),
                           "\(layout) d-pad should be the low tetrachord")
            XCTAssertEqual(Set(face.map { layout.degree(for: $0) }), Set(4...7),
                           "\(layout) face buttons should be the high tetrachord")
        }
    }

    // MARK: - Defaults

    func testDefaultSchemeAndLayout() {
        // Fresh defaults (unknown raw value) fall back to the recommended
        // octave-first scheme and the chosen clockwise layout.
        XCTAssertEqual(GamepadControlScheme(rawValue: "") ?? .octaveFirst, .octaveFirst)
        XCTAssertEqual(GamepadNoteLayout(rawValue: "") ?? .clockwise, .clockwise)
    }
}
