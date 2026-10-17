import AppKit
import AVFoundation
import XCTest
@testable import MenuBand

final class PianoSourceTests: XCTestCase {
    func testBacktickOneSelectsPiano() {
        withFreshController { controller in
            key(controller, 50, []) // Backtick primes the custom instrument.
            key(controller, 18, []) // 1 selects the piano.
            drainMainQueue()
            XCTAssertEqual(controller.instrumentBackend, .acPiano)
            XCTAssertEqual(controller.pianoSource, .salamander)
        }
    }

    func testEverySourceHasItsCompletePitchMap() {
        for source in PianoSource.allCases {
            XCTAssertTrue(source.isAvailable, source.label)
            XCTAssertEqual(source.bankURLs().compactMap { PianoSource.midiNote(in: $0) }.sorted(),
                           source.anchorNotes)
        }
        XCTAssertEqual(PianoSource.yamahaUpright.stepped(by: 1), .salamander)
        XCTAssertEqual(PianoSource.salamander.stepped(by: -1), .yamahaUpright)
    }

    func testBanksRenderAcrossTheKeyboardAndSwitchWithoutOldTails() throws {
        let piano = MenuBandACPianoVoice()
        let format = try XCTUnwrap(AVAudioFormat(standardFormatWithSampleRate: 48_000, channels: 2))
        let buffer = try XCTUnwrap(AVAudioPCMBuffer(pcmFormat: format, frameCapacity: 24_000))
        buffer.frameLength = buffer.frameCapacity
        let channels = try XCTUnwrap(buffer.floatChannelData)
        for _ in 0..<2 { // Also exercise switching back to cached banks.
            for source in PianoSource.allCases {
                XCTAssertTrue(piano.selectSource(source))
                XCTAssertEqual(piano.source, source)
                piano.render(frameCount: 128, abl: buffer.mutableAudioBufferList)
                XCTAssertTrue((0..<128).allSatisfy { channels[0][$0] == 0 && channels[1][$0] == 0 },
                              "Changing a bank must clear the previous bank's voices")
                for note in [UInt8(24), 60, 84, 108] {
                    piano.panic()
                    piano.noteOn(note, velocity: 100, channel: 0)
                    piano.render(frameCount: 24_000, abl: buffer.mutableAudioBufferList)
                    var energy = 0.0
                    for index in 0..<24_000 {
                        let value = channels[0][index]
                        XCTAssertTrue(value.isFinite)
                        XCTAssertLessThanOrEqual(abs(value), 1)
                        energy += Double(value * value)
                    }
                    XCTAssertGreaterThan(energy, 0.0001, "\(source.label), MIDI \(note)")
                }
            }
        }
    }

    func testPianoAndDrumShortcutsStayIndependentInBothLayouts() {
        withFreshController { controller in
            controller.selectCustomInstrument(1)
            for trackDrum in [false, true] {
                controller.percussionRight = false
                controller.pressPercussionLatch(left: false, trackDrum: trackDrum)
                controller.selectPianoSource(.salamander)
                controller.drumKit = .electro
                key(controller, 124, .option)
                drainMainQueue()
                XCTAssertEqual(controller.pianoSource, .steinway)
                XCTAssertEqual(controller.drumKit, .electro)
                XCTAssertEqual(controller.instrumentBackend, .acPiano)
                XCTAssertEqual(controller.percussionRightTrackDrum, trackDrum)

                _ = controller.handleLocalKey(keyCode: 124, isDown: true, isRepeat: true, flags: .option)
                drainMainQueue()
                XCTAssertEqual(controller.pianoSource, .steinway)

                key(controller, 124, .shift)
                drainMainQueue()
                XCTAssertEqual(controller.drumKit, .salamander)
                XCTAssertEqual(controller.pianoSource, .steinway)
                key(controller, 123, .option)
                drainMainQueue()
                XCTAssertEqual(controller.pianoSource, .salamander)
                XCTAssertEqual(controller.drumKit, .salamander)
            }
        }
    }

    func testSourcePersistsWhenLeavingAndReturningToPiano() {
        withFreshController { controller in
            controller.selectPianoSource(.yamahaUpright)
            XCTAssertEqual(controller.instrumentBackend, .acPiano)
            controller.setACPianoBackend(false)
            controller.selectCustomInstrument(1)
            XCTAssertEqual(controller.pianoSource, .yamahaUpright)
            XCTAssertTrue(controller.voiceContextLabel.contains("3 · Yamaha Upright"))
            XCTAssertEqual(MenuBandController().pianoSource, .yamahaUpright)
        }
    }

    private func key(_ controller: MenuBandController, _ code: UInt16, _ flags: NSEvent.ModifierFlags) {
        XCTAssertTrue(controller.handleLocalKey(keyCode: code, isDown: true, isRepeat: false, flags: flags))
        XCTAssertTrue(controller.handleLocalKey(keyCode: code, isDown: false, isRepeat: false, flags: []))
    }

    private func drainMainQueue() {
        let drained = expectation(description: "Source selection applied")
        DispatchQueue.main.async { drained.fulfill() }
        wait(for: [drained], timeout: 3)
    }

    private func withFreshController(_ body: (MenuBandController) -> Void) {
        let defaults = UserDefaults.standard
        let keys = ["notepat.pianoSource", "notepat.instrumentBackend", "notepat.midiMode",
                    "notepat.percussionLeft", "notepat.percussionRight",
                    "notepat.percussionLeftTrackDrum", "notepat.percussionRightTrackDrum",
                    "notepat.trackDrumKit"]
        let saved = keys.map { ($0, defaults.object(forKey: $0)) }
        let mirror = MenuBandPercussion.DrumKit.current
        defer {
            for (key, value) in saved {
                if let value { defaults.set(value, forKey: key) }
                else { defaults.removeObject(forKey: key) }
            }
            MenuBandPercussion.DrumKit.current = mirror
        }
        for key in keys { defaults.removeObject(forKey: key) }
        body(MenuBandController())
        drainMainQueue()
    }
}
