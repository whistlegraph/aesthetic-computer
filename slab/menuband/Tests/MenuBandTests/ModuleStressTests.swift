import XCTest
import AppKit
import AVFoundation
@testable import MenuBand

/// Deterministic module stress: no app bootstrap, hardware output, event taps,
/// preference changes, or UI interaction. Seeds make failures reproducible.
final class ModuleStressTests: XCTestCase {
    private struct Random {
        var state: UInt64
        mutating func next(_ bound: Int) -> Int {
            state = state &* 6364136223846793005 &+ 1442695040888963407
            return Int((state >> 24) % UInt64(bound))
        }
    }

    func testRandomizedFocusOwnershipAndRelease() {
        var rng = Random(state: 0x4d4247415445)
        let gate = PerformanceInputGate()
        var open = false, held = Set<UInt16>(), dispatched = 0, released = 0
        for _ in 0..<200_000 {
            switch rng.next(20) {
            case 0:
                gate.activate(); open = true
            case 1:
                let before = released
                gate.close { released += 1 }
                XCTAssertEqual(released - before, open ? 1 : 0)
                open = false; held.removeAll()
            default:
                let key = UInt16(rng.next(128)), down = rng.next(2) == 0
                let eligible = rng.next(3) != 0
                let wanted = open && ((down && eligible) || held.contains(key))
                let before = dispatched
                let accepted = gate.submit(key: key, down: down, eligible: eligible) { dispatched += 1 }
                XCTAssertEqual(accepted, wanted)
                XCTAssertEqual(dispatched - before, wanted ? 1 : 0)
                if wanted { if down { held.insert(key) } else { held.remove(key) } }
            }
        }
        gate.close {}
    }

    func testRandomizedTrackpadOwnershipDoesNotRetainLiftedContacts() {
        var rng = Random(state: 0x4d42504144)
        var ownership = TrackpadSplitSurface.Ownership()
        var previous: [Int32: CGPoint] = [:]
        var role: [Int32: Bool] = [:]
        for _ in 0..<20_000 {
            let ids = Set((0..<10).filter { _ in rng.next(3) != 0 }.map(Int32.init))
            role = role.filter { ids.contains($0.key) }
            let contacts = ids.sorted().map { id -> TrackpadContact in
                let p = CGPoint(x: Double(rng.next(1001)) / 1000, y: Double(rng.next(1001)) / 1000)
                if role[id] == nil { role[id] = p.x < 0.5 }
                return TrackpadContact(identifier: id, point: p, state: previous[id] == nil ? 3 : 4)
            }
            let frame = ownership.resolve(previous: previous, active: contacts)
            XCTAssertEqual(ownership.pitchIDs, Set(role.filter { $0.value }.keys))
            XCTAssertEqual(ownership.drumIDs, Set(role.filter { !$0.value }.keys))
            XCTAssertTrue(ownership.pitchIDs.isDisjoint(with: ownership.drumIDs))
            XCTAssertEqual(frame.pitch.count + frame.drumTouches.count, contacts.count)
            previous = Dictionary(uniqueKeysWithValues: contacts.map { ($0.identifier, $0.point) })
        }
        _ = ownership.resolve(previous: previous, active: [])
        XCTAssertTrue(ownership.pitchIDs.isEmpty && ownership.drumIDs.isEmpty)
    }

    func testOfflineSynthRandomizedBuffersAndRepeatedTeardown() throws {
        let format = try XCTUnwrap(AVAudioFormat(standardFormatWithSampleRate: 48_000, channels: 2))
        let buffer = try XCTUnwrap(AVAudioPCMBuffer(pcmFormat: format, frameCapacity: 512))
        let channels = try XCTUnwrap(buffer.floatChannelData)
        let sizes = [0, 1, 2, 31, 64, 127, 128, 255, 256, 511, 512]
        var rng = Random(state: 0x4d4253594e5448)
        let programs = (0..<128).map(UInt8.init).filter(MenuBandGMSynth.programImplemented)
        XCTAssertFalse(programs.isEmpty)
        for _ in 0..<8 {
            weak var released: MenuBandGMSynth?
            autoreleasepool {
                let synth = MenuBandGMSynth(); released = synth
                for _ in 0..<256 {
                    for _ in 0..<8 {
                        let note = UInt8(rng.next(128)), channel = UInt8(rng.next(16))
                        switch rng.next(5) {
                        case 0, 1: _ = synth.noteOn(note, velocity: UInt8(rng.next(128)), channel: channel,
                                                  program: programs[rng.next(programs.count)])
                        case 2: synth.noteOff(note, channel: channel)
                        case 3: synth.setPitchBend(amount: Float(rng.next(2001) - 1000) / 1000)
                        default: synth.panic()
                        }
                    }
                    let n = sizes[rng.next(sizes.count)]
                    buffer.frameLength = AVAudioFrameCount(n)
                    synth.render(frameCount: n, abl: buffer.mutableAudioBufferList)
                    for c in 0..<2 {
                        XCTAssertTrue((0..<n).allSatisfy { channels[c][$0].isFinite })
                    }
                }
                synth.panic()
                buffer.frameLength = 512
                synth.render(frameCount: 512, abl: buffer.mutableAudioBufferList)
                XCTAssertTrue((0..<512).allSatisfy { channels[0][$0] == 0 && channels[1][$0] == 0 })
            }
            XCTAssertNil(released, "synth retained after its owner released it")
        }
    }

    func testConcurrentControlProducersAgainstOfflineRender() throws {
        let synth = MenuBandGMSynth()
        let format = try XCTUnwrap(AVAudioFormat(standardFormatWithSampleRate: 48_000, channels: 2))
        let buffer = try XCTUnwrap(AVAudioPCMBuffer(pcmFormat: format, frameCapacity: 128))
        buffer.frameLength = 128
        let channels = try XCTUnwrap(buffer.floatChannelData)
        let producers = DispatchGroup()
        for producer in 0..<4 {
            producers.enter()
            DispatchQueue.global(qos: .userInitiated).async {
                var rng = Random(state: UInt64(producer + 1))
                for _ in 0..<4096 {
                    let note = UInt8(rng.next(128))
                    _ = synth.noteOn(note, velocity: UInt8(rng.next(128)), channel: UInt8(producer), program: 0)
                    synth.noteOff(note, channel: UInt8(producer))
                    synth.setPitchBend(amount: Float(rng.next(2001) - 1000) / 1000)
                    if rng.next(100) == 0 { synth.panic() }
                }
                producers.leave()
            }
        }
        let deadline = ProcessInfo.processInfo.systemUptime + 30
        repeat {
            synth.render(frameCount: 128, abl: buffer.mutableAudioBufferList)
            XCTAssertTrue((0..<128).allSatisfy { channels[0][$0].isFinite && channels[1][$0].isFinite })
            if ProcessInfo.processInfo.systemUptime > deadline {
                XCTFail("control/render stress failed to finish within 30 seconds")
                break
            }
        } while producers.wait(timeout: .now()) != .success
        XCTAssertEqual(producers.wait(timeout: .now() + 5), .success)
        synth.panic()
        synth.render(frameCount: 128, abl: buffer.mutableAudioBufferList)
        XCTAssertTrue((0..<128).allSatisfy { channels[0][$0] == 0 && channels[1][$0] == 0 })
    }

    func testArtworkCacheRemainsBoundedAndReleasesAfterChurn() throws {
        _ = NSApplication.shared
        weak var released: RasterArtworkCache<Int>?
        try autoreleasepool {
            let cache = RasterArtworkCache<Int>(capacity: 32); released = cache
            let rep = try XCTUnwrap(NSBitmapImageRep(bitmapDataPlanes: nil,
                pixelsWide: 128, pixelsHigh: 64, bitsPerSample: 8, samplesPerPixel: 4,
                hasAlpha: true, isPlanar: false, colorSpaceName: .deviceRGB,
                bytesPerRow: 0, bitsPerPixel: 0))
            NSGraphicsContext.saveGraphicsState()
            defer { NSGraphicsContext.restoreGraphicsState() }
            NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: rep)
            let rect = NSRect(x: 0, y: 0, width: 128, height: 64)
            for frame in 0..<2000 {
                autoreleasepool {
                    cache.draw(frame, in: rect) {
                        NSColor(calibratedHue: CGFloat(frame % 100) / 100, saturation: 1,
                                brightness: 0.8, alpha: 1).setFill()
                        NSBezierPath(roundedRect: rect, xRadius: 4, yRadius: 4).fill()
                    }
                }
                XCTAssertLessThanOrEqual(cache.count, 32)
            }
            cache.removeAll()
            XCTAssertEqual(cache.count, 0)
        }
        XCTAssertNil(released)
    }
}
