import XCTest
@testable import MenuBand

final class ChordFlagsTests: XCTestCase {
    private let leftCmd: UInt64 = 0x0010_0000 | 0x08
    private let rightCmd: UInt64 = 0x0010_0000 | 0x10
    private let leftOpt: UInt64 = 0x0008_0000 | 0x20
    private let rightOpt: UInt64 = 0x0008_0000 | 0x40
    private let ctl: UInt64 = 0x0004_0000

    func testLeftCommandChordsOnlyTheLeftHand() {
        XCTAssertEqual(MenuBandController.chordFlags(rawFlags: leftCmd, hand: .left),
                       .init(modifier: true, minor: false, sus: false, aug: false))
        XCTAssertEqual(MenuBandController.chordFlags(rawFlags: leftCmd, hand: .right),
                       .init())
    }

    func testRightOptionMinorsOnlyTheRightHand() {
        XCTAssertEqual(MenuBandController.chordFlags(rawFlags: rightOpt, hand: .right),
                       .init(modifier: true, minor: true, sus: false, aug: false))
        XCTAssertEqual(MenuBandController.chordFlags(rawFlags: rightOpt, hand: .left),
                       .init())
    }

    func testSusNeedsBothOnTheSameHand() {
        XCTAssertEqual(MenuBandController.chordFlags(rawFlags: leftCmd | leftOpt, hand: .left),
                       .init(modifier: true, minor: true, sus: true, aug: false))
        // ⌘ on one hand and ⌥ on the other: each hand hears only its own.
        XCTAssertEqual(MenuBandController.chordFlags(rawFlags: leftCmd | rightOpt, hand: .left),
                       .init(modifier: true, minor: false, sus: false, aug: false))
        XCTAssertEqual(MenuBandController.chordFlags(rawFlags: leftCmd | rightOpt, hand: .right),
                       .init(modifier: true, minor: true, sus: false, aug: false))
    }

    func testControlReachesTheWholeBoard() {
        for hand in [MenuBandController.LingerSide.left, .right, .neutral] {
            XCTAssertEqual(MenuBandController.chordFlags(rawFlags: ctl, hand: hand),
                           .init(modifier: true, minor: false, sus: false, aug: true))
        }
    }

    func testSidelessFlagsAndNonNoteKeysHearEitherSide() {
        // A synthetic event carries the mask but no device bit.
        XCTAssertEqual(MenuBandController.chordFlags(rawFlags: 0x0010_0000, hand: .right),
                       .init(modifier: true, minor: false, sus: false, aug: false))
        // ⌘-Tab and friends: the key has no hand, so any ⌘ counts.
        XCTAssertEqual(MenuBandController.chordFlags(rawFlags: leftCmd, hand: .neutral).modifier,
                       true)
    }
}
