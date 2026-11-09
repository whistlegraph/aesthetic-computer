import XCTest
import AppKit
import AVFoundation
import CRealtimeAudio
@testable import MenuBand

final class PerformanceIsolationTests: XCTestCase {
    func testNativeSynthRendersWithoutUIAndOverflowCannotLeaveAVoiceHeld() throws {
        let synth = MenuBandGMSynth()
        let format = try XCTUnwrap(AVAudioFormat(standardFormatWithSampleRate: 48_000, channels: 2))
        let buffer = try XCTUnwrap(AVAudioPCMBuffer(pcmFormat: format, frameCapacity: 512))
        buffer.frameLength = 512
        let samples = try XCTUnwrap(buffer.floatChannelData)[0]
        func renderEnergy() -> Float {
            synth.render(frameCount: 512, abl: buffer.mutableAudioBufferList)
            return (0..<512).reduce(0) { $0 + abs(samples[$1]) }
        }
        XCTAssertTrue(synth.noteOn(60, velocity: 100, channel: 0, program: 0))
        XCTAssertGreaterThan(renderEnergy(), 0)
        for _ in 0...mb_realtime_queue_capacity() {
            _ = synth.noteOn(60, velocity: 100, channel: 0, program: 0)
        }
        synth.noteOff(60, channel: 0) // even this overflowed release must be safe
        XCTAssertEqual(renderEnergy(), 0)
        _ = synth.noteOn(64, velocity: 100, channel: 0, program: 0)
        XCTAssertGreaterThan(renderEnergy(), 0)
        synth.panic()
        XCTAssertEqual(renderEnergy(), 0)
    }

    func testUIInvalidationsCoalesce() {
        let refresh = CoalescedUIRefresh()
        let drawn = expectation(description: "one draw for a burst of invalidations")
        drawn.assertForOverFulfill = true
        for _ in 0..<500 { refresh.request { drawn.fulfill() } }
        wait(for: [drawn], timeout: 1)
    }

    func testControlEventsAndReleaseProgressWhileMainIsBlocked() {
        // Intentionally do not pump the main run loop. Audio/control must
        // complete before the UI is allowed to consume its notifications.
        let executor = PerformanceExecutor()
        let gate = PerformanceInputGate()
        let done = DispatchSemaphore(value: 0)
        var events: [Int] = [] // written/read only on executor
        gate.activate()
        XCTAssertTrue(gate.submit(key: 0, down: true, eligible: true) {
            executor.enqueue { events.append(1) }
        })
        XCTAssertTrue(gate.submit(key: 0, down: false, eligible: false) {
            executor.enqueue { events.append(2) }
        })
        gate.close {
            executor.enqueue {
                events.append(3)
                XCTAssertEqual(events, [1, 2, 3])
                done.signal()
            }
        }
        XCTAssertFalse(gate.submit(key: 0, down: true, eligible: true) {
            XCTFail("closed focus admitted a note")
        })
        XCTAssertEqual(done.wait(timeout: .now() + 1), .success)
    }

    func testFocusCloseIsIdempotentAndRearmDoesNotOwnAnOldRelease() {
        let gate = PerformanceInputGate()
        var releases = 0
        gate.activate()
        XCTAssertTrue(gate.submit(key: 0, down: true, eligible: true) {})
        gate.close { releases += 1 }
        gate.close { releases += 1 }
        gate.activate()
        XCTAssertFalse(gate.submit(key: 0, down: false, eligible: false) {})
        XCTAssertEqual(releases, 1)
    }

    func testKeyboardControllerStagesDownAndUpWithoutMainRunloop() {
        let controller = MenuBandController() // no bootstrap/audio device
        let done = DispatchSemaphore(value: 0)
        let key = MenuBandLayout.cMajorKeyCodes(for: controller.keymap)[0]
        XCTAssertTrue(controller.handleLocalKey(keyCode: key, isDown: true,
                                                isRepeat: false, flags: []))
        controller.performance.enqueue {
            XCTAssertTrue(controller.heldKeyCodes().contains(key))
            _ = controller.handleLocalKey(keyCode: key, isDown: false,
                                          isRepeat: false, flags: [])
            XCTAssertFalse(controller.heldKeyCodes().contains(key))
            done.signal()
        }
        XCTAssertEqual(done.wait(timeout: .now() + 1), .success)
        controller.performance.flush()
    }

    func testRenderQueuePreservesOrderAcrossWraparound() {
        let queue = mb_realtime_queue_create()!
        defer { mb_realtime_queue_destroy(queue) }
        var received = MBRealtimeCommand()
        for cycle in 0..<20 {
            for n in 0..<128 {
                XCTAssertTrue(mb_realtime_queue_push(queue, MBRealtimeCommand(
                    kind: n % 2 == 0 ? 1 : 2, seed: UInt32(cycle * 128 + n),
                    program: 0, note: 60, channel: 0, velocity: 100)))
            }
            for n in 0..<128 {
                XCTAssertTrue(mb_realtime_queue_pop(queue, &received))
                XCTAssertEqual(received.seed, UInt32(cycle * 128 + n))
                XCTAssertEqual(received.kind, n % 2 == 0 ? 1 : 2)
            }
            XCTAssertFalse(mb_realtime_queue_pop(queue, &received))
        }
    }

    func testOverflowRequiresPanicBeforeAnyNewOnset() {
        let queue = mb_realtime_queue_create()!
        defer { mb_realtime_queue_destroy(queue) }
        let on = MBRealtimeCommand(kind: 1, seed: 0, program: 0,
                                   note: 60, channel: 0, velocity: 100)
        for _ in 0..<mb_realtime_queue_capacity() {
            XCTAssertTrue(mb_realtime_queue_push(queue, on))
        }
        let off = MBRealtimeCommand(kind: 2, seed: 0, program: 0,
                                    note: 60, channel: 0, velocity: 0)
        XCTAssertFalse(mb_realtime_queue_push(queue, off))
        XCTAssertFalse(mb_realtime_queue_push(queue, on))
        XCTAssertTrue(mb_realtime_queue_take_reset(queue))
        var received = MBRealtimeCommand()
        XCTAssertFalse(mb_realtime_queue_pop(queue, &received))
        XCTAssertTrue(mb_realtime_queue_push(queue, on))
        XCTAssertTrue(mb_realtime_queue_push(queue, off))
        XCTAssertTrue(mb_realtime_queue_pop(queue, &received))
        XCTAssertEqual(received.kind, 1)
        XCTAssertTrue(mb_realtime_queue_pop(queue, &received))
        XCTAssertEqual(received.kind, 2)
        XCTAssertFalse(mb_realtime_queue_take_reset(queue))
    }

    func testRenderQueueConcurrentProducerConsumer() {
        let queue = mb_realtime_queue_create()!
        defer { mb_realtime_queue_destroy(queue) }
        let producerDone = DispatchSemaphore(value: 0)
        // Each batch fits in the ring; handshake stresses wraparound and
        // memory publication without manufacturing an overflow.
        let batchReady = DispatchSemaphore(value: 0)
        let batchRead = DispatchSemaphore(value: 0)
        DispatchQueue.global().async {
            for batch in 0..<1000 {
                for i in 0..<64 {
                    XCTAssertTrue(mb_realtime_queue_push(queue, MBRealtimeCommand(
                        kind: 1, seed: UInt32(batch * 64 + i), program: 7,
                        note: 60, channel: 2, velocity: 91)))
                }
                batchReady.signal()
                batchRead.wait()
            }
            producerDone.signal()
        }
        var received = MBRealtimeCommand()
        for batch in 0..<1000 {
            XCTAssertEqual(batchReady.wait(timeout: .now() + 2), .success)
            for i in 0..<64 {
                XCTAssertTrue(mb_realtime_queue_pop(queue, &received))
                XCTAssertEqual(received.seed, UInt32(batch * 64 + i))
                XCTAssertEqual(received.velocity, 91)
            }
            batchRead.signal()
        }
        producerDone.wait()
    }
}
