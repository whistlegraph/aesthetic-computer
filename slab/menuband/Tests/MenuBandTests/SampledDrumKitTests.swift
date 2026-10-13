import AVFoundation
import AppKit
import XCTest
@testable import MenuBand

final class SampledDrumKitTests: XCTestCase {
    private typealias P = MenuBandPercussion

    func testBundledBanksHaveFiveAudiblePadsAndTwoAlternatingHits() throws {
        let percussion = P()
        for kit in [P.DrumKit.salamander, .handPercussion] {
            let spec = try XCTUnwrap(P.spec(for: kit), "Missing bundled bank: \(kit.label)")
            XCTAssertEqual(spec.pads.count, 5)
            for pad in spec.pads {
                XCTAssertEqual(pad.recordings.count, 2)
                let first = try XCTUnwrap(percussion.padVoices(pad, gain: 1, pan: 0).first?.recording)
                let second = try XCTUnwrap(percussion.padVoices(pad, gain: 1, pan: 0).first?.recording)
                let third = try XCTUnwrap(percussion.padVoices(pad, gain: 1, pan: 0).first?.recording)
                XCTAssertFalse(first === second)
                XCTAssertTrue(first === third)
                for hit in pad.recordings {
                    XCTAssertGreaterThan(hit.sampleRate, 0)
                    XCTAssertTrue(hit.frames.allSatisfy { $0.isFinite })
                    XCTAssertGreaterThan(hit.frames.map { abs($0) }.max() ?? 0, 0.001)
                }
                for rate in [44_100.0, 48_000.0] {
                    var voice = percussion.padVoices(pad, gain: 1, pan: 0, jitter: false)[0]
                    var energy = 0.0
                    for _ in 0..<Int(rate * 0.4) {
                        let sample = P.nextSample(&voice, pitch: 1, dt: 1 / rate)
                        XCTAssertTrue(sample.isFinite)
                        energy += sample * sample
                    }
                    XCTAssertGreaterThan(energy, 0.001, "\(kit.label) \(pad.name) at \(rate)")
                }
            }
        }
    }

    func testInterpolationAndPitchFollowSourceTime() {
        let percussion = P()
        var voice = percussion.makeVoice(.sine, 1, 1.25, 1, 0, 0.001, 0)
        voice.recording = P.RecordedHit(frames: [0, 1, 0, -1, 0], sampleRate: 4)
        let expected = [0.0, 0.5, 1, 0.5, 0, -0.5, -1, -0.5]
        for sample in expected {
            XCTAssertEqual(P.nextSample(&voice, pitch: 1, dt: 1 / 8), sample, accuracy: 0.000001)
        }
        voice.elapsed = 0
        // Half pitch takes twice as much wall time to reach the same source frame.
        for _ in 0..<8 { _ = P.nextSample(&voice, pitch: 0.5, dt: 1 / 8) }
        XCTAssertEqual(voice.elapsed, 0.5, accuracy: 0.000001)
        voice.elapsed = voice.duration + 1
        XCTAssertEqual(P.nextSample(&voice, pitch: 1, dt: 1 / 8), 0)
    }

    func testTrackDrumRoutesAllZonesToSelectedRecordings() throws {
        let percussion = P()
        for kit in [P.DrumKit.salamander, .handPercussion] {
            percussion.kit = kit
            for distance in [0.0, 0.36, 0.54, 0.78, 0.99] {
                percussion.pending.removeAll()
                percussion.playDrumSkin(strike: CGPoint(x: 0.5, y: 0.5 + distance * 0.5),
                                         anchors: [], velocity: 100)
                XCTAssertFalse(percussion.pending.isEmpty)
                XCTAssertTrue(percussion.pending.allSatisfy { $0.recording != nil })
            }
        }
        XCTAssertEqual(P.DrumKit.handPercussion.stepped(by: 1), .menuBand)
        XCTAssertEqual(P.DrumKit.menuBand.stepped(by: -1), .handPercussion)
    }

    func testKeyboardHitsAndAccentsFollowTheSelectedSampleBank() throws {
        let percussion = P()
        for kit in [P.DrumKit.salamander, .handPercussion] {
            percussion.kit = kit
            let spec = try XCTUnwrap(P.spec(for: kit))
            for drum in P.Drum.allCases {
                for accent in [false, true] {
                    percussion.pending.removeAll()
                    let group = percussion.noteOn(drum, velocity: 100, pan: 0, accent: accent)
                    XCTAssertEqual(percussion.pending.count, 1, "No synthesized accent layer in \(kit.label)")
                    let hit = try XCTUnwrap(percussion.pending.first?.recording)
                    XCTAssertTrue(spec.pads.contains { pad in pad.recordings.contains { $0 === hit } })
                    percussion.pending.removeAll()
                    percussion.noteOff(group)
                    XCTAssertTrue(percussion.pending.isEmpty, "A sampled one-shot has no synthetic release")
                }
            }
            // Familiar keyboard roles keep their position in either bank.
            for (drum, pad) in [(P.Drum.kick, 0), (.block, 1), (.snare, 2), (.hatClosed, 3), (.cowbell, 4)] {
                percussion.pending.removeAll()
                percussion.play(drum, velocity: 100, pan: 0)
                let hit = try XCTUnwrap(percussion.pending.first?.recording)
                XCTAssertTrue(spec.pads[pad].recordings.contains { $0 === hit })
            }
        }
    }

    func testChangingKitsDropsOldHeldSoundsAndReleaseBursts() {
        let percussion = P()
        let closed = percussion.noteOn(.hatClosed, velocity: 100, pan: 0)
        _ = percussion.noteOn(.hatOpen, velocity: 100, pan: 0)
        XCTAssertTrue(percussion.pending.contains { $0.held })
        percussion.kit = .salamander
        XCTAssertTrue(percussion.pending.isEmpty)
        percussion.noteOff(closed)
        XCTAssertTrue(percussion.pending.isEmpty, "No original hi-hat burst after selecting recordings")
        percussion.kit = .menuBand
        for drum in P.Drum.allCases {
            percussion.pending.removeAll()
            percussion.play(drum, velocity: 100, pan: 0)
            XCTAssertFalse(percussion.pending.isEmpty)
            XCTAssertTrue(percussion.pending.allSatisfy { $0.recording == nil })
        }
    }

    func testArrowSelectionWorksInBothKeyboardLayoutsAndIgnoresRepeat() {
        withFreshController { controller in
            for left in [false, true] {
                for trackDrum in [false, true] {
                    controller.percussionLeft = false
                    controller.percussionRight = false
                    controller.drumKit = .electro
                    XCTAssertTrue(controller.pressPercussionLatch(left: left, trackDrum: trackDrum))
                    controller.startTapNote(left ? 72 : 60) // Melody must not steal kit selection.
                    controller.stopTapNote(left ? 72 : 60)
                    sendKey(controller, code: 124)
                    XCTAssertTrue(controller.handleLocalKey(keyCode: 124, isDown: true,
                                                             isRepeat: true, flags: []))
                    drainMainQueue()
                    XCTAssertEqual(controller.drumKit, .salamander)
                    XCTAssertEqual(P.DrumKit.current, .salamander)
                    XCTAssertEqual(UserDefaults.standard.integer(forKey: "notepat.trackDrumKit"),
                                   P.DrumKit.salamander.rawValue)
                    XCTAssertEqual(left ? controller.percussionLeftTrackDrum : controller.percussionRightTrackDrum,
                                   trackDrum, "Changing a kit keeps the playing layout")
                    sendKey(controller, code: 123, flags: .shift)
                    drainMainQueue()
                    XCTAssertEqual(controller.drumKit, .electro)
                }
            }
        }
    }

    func testShiftArrowsSelectKitsWithoutAKeyboardDrumLatch() {
        withFreshController { controller in
            controller.drumKit = .menuBand
            XCTAssertFalse(controller.arrowsSelectDrumKit(shift: false))
            sendKey(controller, code: 123, flags: .shift)
            drainMainQueue()
            XCTAssertEqual(controller.drumKit, .handPercussion)
            controller.trackpadDrumSkin(strike: CGPoint(x: 0.5, y: 0.5), anchors: [], velocity: 100)
            sendKey(controller, code: 124)
            drainMainQueue()
            XCTAssertEqual(controller.drumKit, .menuBand)
            controller.startTapNote(60)
            controller.stopTapNote(60)
            XCTAssertFalse(controller.arrowsSelectDrumKit(shift: false))
        }
    }

    func testTrackDrumKeyLiftCannotCrossAKitChange() {
        withFreshController { controller in
            controller.drumKit = .menuBand
            controller.pressPercussionLatch(left: false, trackDrum: true)
            let original = controller.percussionKeyHit(displayNote: 72, velocity: 100, pan: 64)
            XCTAssertNotNil(controller.trackDrumLiftPoint(for: original))
            controller.drumKit = .salamander
            XCTAssertNil(controller.trackDrumLiftPoint(for: original))
            XCTAssertEqual(controller.percussionKeyHit(displayNote: 72, velocity: 100, pan: 64), 0)
            controller.drumKit = .menuBand
            XCTAssertNil(controller.trackDrumLiftPoint(for: original))
            let fresh = controller.percussionKeyHit(displayNote: 72, velocity: 100, pan: 64)
            XCTAssertNotNil(controller.trackDrumLiftPoint(for: fresh))
        }
    }

    private func sendKey(_ controller: MenuBandController, code: UInt16,
                         flags: NSEvent.ModifierFlags = []) {
        XCTAssertTrue(controller.handleLocalKey(keyCode: code, isDown: true, isRepeat: false, flags: flags))
        XCTAssertTrue(controller.handleLocalKey(keyCode: code, isDown: false, isRepeat: false, flags: []))
    }

    private func drainMainQueue() {
        let drained = expectation(description: "Keyboard selection dispatched to main")
        DispatchQueue.main.async { drained.fulfill() }
        wait(for: [drained], timeout: 3)
    }

    private func withFreshController(_ body: (MenuBandController) -> Void) {
        let defaults = UserDefaults.standard
        let keys = [KeyboardIconRenderer.percussionLeftDefaultsKey,
                    KeyboardIconRenderer.percussionRightDefaultsKey,
                    "notepat.percussionLeftTrackDrum", "notepat.percussionRightTrackDrum",
                    "notepat.trackDrumKit"]
        let saved = keys.map { ($0, defaults.object(forKey: $0)) }
        let mirror = P.DrumKit.current
        defer {
            for (key, value) in saved {
                if let value { defaults.set(value, forKey: key) }
                else { defaults.removeObject(forKey: key) }
            }
            P.DrumKit.current = mirror
        }
        for key in keys { defaults.removeObject(forKey: key) }
        let controller = MenuBandController() // No audio engine or global event tap.
        body(controller)
        drainMainQueue()
    }

    /// Optional listening artifacts use the same renderer and matching gains
    /// as the app: five isolated pads, then the same four-bar beat in each kit.
    func testRenderAuditions() throws {
        guard let destination = ProcessInfo.processInfo.environment["MENUBAND_DRUM_PREVIEW_DIR"] else { return }
        let folder = URL(fileURLWithPath: destination, isDirectory: true)
        try FileManager.default.createDirectory(at: folder, withIntermediateDirectories: true)
        let percussion = P()
        let rate = 48_000.0
        for kit in [P.DrumKit.salamander, .handPercussion] {
            let spec = try XCTUnwrap(P.spec(for: kit))
            let gains = percussion.padGains(for: kit)
            let format = AVAudioFormat(standardFormatWithSampleRate: rate, channels: 2)!
            let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: UInt32(rate * 16))!
            buffer.frameLength = buffer.frameCapacity
            let channels = buffer.floatChannelData!
            for c in 0..<2 { channels[c].initialize(repeating: 0, count: Int(buffer.frameLength)) }
            var events: [(Double, Int, Double)] = (0..<5).map { (Double($0), $0, 1.0) }
            for step in 0..<32 {
                let time = 6 + Double(step) * 0.25
                events.append((time, 3, step.isMultiple(of: 2) ? 0.85 : 0.6))
                if step.isMultiple(of: 8) || step % 8 == 5 { events.append((time, 0, 1)) }
                if step % 4 == 2 { events.append((time, 2, 0.9)) }
                if step % 8 == 7 { events.append((time, 1, 0.75)) }
                if step % 8 == 3 { events.append((time, 4, 0.8)) }
            }
            for (time, pad, velocity) in events {
                let voices = percussion.padVoices(spec.pads[pad], gain: gains[pad] * velocity,
                                                   pan: 0)
                for var voice in voices {
                    for frame in Int(time * rate)..<Int(buffer.frameLength) {
                        if voice.elapsed >= voice.duration { break }
                        let sample = Float(P.nextSample(&voice, pitch: 1, dt: 1 / rate) * 0.34 * 0.9)
                        channels[0][frame] += sample * voice.gainL
                        channels[1][frame] += sample * voice.gainR
                    }
                }
            }
            var peak: Float = 0
            for channel in 0..<2 {
                for frame in 0..<Int(buffer.frameLength) { peak = max(peak, abs(channels[channel][frame])) }
            }
            XCTAssertLessThan(peak, 1, "Audition clips: \(kit.label)")
            let file = try AVAudioFile(forWriting: folder.appendingPathComponent("\(kit.label).wav"),
                                      settings: format.settings)
            try file.write(from: buffer)
        }
    }
}
