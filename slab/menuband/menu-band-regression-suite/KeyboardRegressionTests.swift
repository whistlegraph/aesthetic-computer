import XCTest
@testable import MenuBand

/// Menu Band regression suite — the keyboard-to-note path.
///
/// Why this exists: a build once shipped where typing the normal playing keys
/// produced no notes. That failure had two possible shapes — an OS-level one
/// (the global CGEventTap needs Accessibility permission; not unit-testable) and
/// a CODE-level one (the keymap or routing stops resolving keys to notes). This
/// suite locks down the code-level shape so a regression in the mapping or the
/// percussion split can never ship silently again.
///
/// Everything here is pure logic over `MenuBandLayout` + the controller's split
/// routing — no audio engine, no event tap, no permissions. It runs on every
/// build via `swift test`.
final class KeyboardRegressionTests: XCTestCase {

    // MARK: keyCode → MIDI note (the core "a key plays a note" contract)

    /// The eight C-major playing keys must resolve to C4…C5 (MIDI 60…72) at
    /// rest. If this breaks, the keyboard is silent / wrong — exactly the
    /// regression this suite guards.
    func testCMajorKeysResolveToMiddleOctave() {
        for keymap in [Keymap.notepat, .ableton] {
            let keys = MenuBandLayout.cMajorKeyCodes(for: keymap)
            let notes = keys.map { MenuBandLayout.midiNote(forKeyCode: $0, octaveShift: 0, keymap: keymap) }
            XCTAssertEqual(notes, [60, 62, 64, 65, 67, 69, 71, 72],
                           "C-major keys must map to C4…C5 for keymap \(keymap)")
        }
    }

    /// Every C-major playing key resolves to *some* note in both keymaps — a
    /// blunt "normal keys fire" smoke check independent of exact pitches.
    func testPlayingKeysAreNeverUnmapped() {
        for keymap in [Keymap.notepat, .ableton] {
            for key in MenuBandLayout.cMajorKeyCodes(for: keymap) {
                XCTAssertNotNil(MenuBandLayout.midiNote(forKeyCode: key, octaveShift: 0, keymap: keymap),
                                "playing key \(key) must resolve to a note in \(keymap)")
            }
        }
    }

    /// Octave shift moves every resolved note by exactly ±12 semitones.
    func testOctaveShiftTransposesByTwelve() {
        let key = MenuBandLayout.cMajorKeyCodes(for: .notepat)[0]   // C
        let base = MenuBandLayout.midiNote(forKeyCode: key, octaveShift: 0)
        XCTAssertEqual(base, 60)
        XCTAssertEqual(MenuBandLayout.midiNote(forKeyCode: key, octaveShift: 1), 72)
        XCTAssertEqual(MenuBandLayout.midiNote(forKeyCode: key, octaveShift: -1), 48)
    }

    /// Out-of-range inputs return nil rather than a bogus note: keyCodes the
    /// hardware can't produce, and octave shifts that push past MIDI 0…127.
    func testOutOfRangeReturnsNil() {
        XCTAssertNil(MenuBandLayout.midiNote(forKeyCode: 200, octaveShift: 0))
        let key = MenuBandLayout.cMajorKeyCodes(for: .notepat)[0]
        XCTAssertNil(MenuBandLayout.midiNote(forKeyCode: key, octaveShift: 12),  // 60 + 144 > 127
                     "an octave shift past MIDI 127 must clamp to nil, not wrap")
    }

    // MARK: percussion split routing (guards the left-octave drum regression)

    /// Each half of the board routes to its OWN latch. A bug once made
    /// `isPercussionDisplayNote` test only the RIGHT half, so left-octave taps
    /// fell through to melodic notes even with the left split armed. Assert both
    /// halves independently. Constructs the controller WITHOUT bootstrapping
    /// audio (same as the screenshot capture does) and restores the persisted
    /// split flags so a test run never mutates the user's settings.
    func testPercussionSplitRoutesEachHalfIndependently() {
        let defaults = UserDefaults.standard
        let leftKey = KeyboardIconRenderer.percussionLeftDefaultsKey
        let rightKey = KeyboardIconRenderer.percussionRightDefaultsKey
        let savedLeft = defaults.bool(forKey: leftKey)
        let savedRight = defaults.bool(forKey: rightKey)
        defer {
            defaults.set(savedLeft, forKey: leftKey)
            defaults.set(savedRight, forKey: rightKey)
        }

        let ctrl = MenuBandController()
        let split = MenuBandLayout.lingerSplitMidi          // 72
        let leftNote = UInt8(split - 1)                     // 71 — left half
        let rightNote = UInt8(split)                        // 72 — right half

        // Left armed only: left-half taps are drums, right-half taps are not.
        ctrl.percussionLeft = true
        ctrl.percussionRight = false
        XCTAssertTrue(ctrl.isPercussionDisplayNote(leftNote),
                      "left split armed → left-octave note must be a drum (the regression)")
        XCTAssertFalse(ctrl.isPercussionDisplayNote(rightNote),
                       "right split off → right-octave note must NOT be a drum")

        // Right armed only: mirror image.
        ctrl.percussionLeft = false
        ctrl.percussionRight = true
        XCTAssertFalse(ctrl.isPercussionDisplayNote(leftNote))
        XCTAssertTrue(ctrl.isPercussionDisplayNote(rightNote))

        // Neither armed: nothing is a drum (normal melodic play).
        ctrl.percussionLeft = false
        ctrl.percussionRight = false
        XCTAssertFalse(ctrl.isPercussionDisplayNote(leftNote))
        XCTAssertFalse(ctrl.isPercussionDisplayNote(rightNote))
    }
}
