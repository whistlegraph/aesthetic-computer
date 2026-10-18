import AVFoundation
import XCTest
@testable import MenuBand

final class AmbientAirTests: XCTestCase {
    func testDraggingActiveAirDoesNotRepeatedlyRequestEngineWork() {
        let suite = "MenuBandAmbientAirTests.\(UUID().uuidString)"
        let defaults = UserDefaults(suiteName: suite)!
        defer { defaults.removePersistentDomain(forName: suite) }
        let air = MenuBandAmbientAir(defaults: defaults)
        var activeChanges: [Bool] = []
        air.onActiveChange = { activeChanges.append($0) }
        air.setLevel(0)
        for step in 1...100 {
            air.setLevel(Float(step) / 100)
            air.setCutoff(Double(step) * 60)
            air.setColor(step.isMultiple(of: 2) ? .brown : .cabin)
        }
        air.setLevel(.nan)
        XCTAssertEqual(activeChanges, [true])
        air.setLevel(0)
        air.setLevel(-1)
        air.setLevel(0.5)
        XCTAssertEqual(activeChanges, [true, false, true])
    }

    /// Render the real source and mixer without opening audio hardware.
    private func withEngine(rate: Double = 48_000, tone: Float = 0,
                            _ check: (MenuBandAmbientAir, (Int) throws -> [Float]) throws -> Void) throws {
        let suite = "MenuBandAmbientAirTests.\(UUID().uuidString)"
        let defaults = UserDefaults(suiteName: suite)!
        defer { defaults.removePersistentDomain(forName: suite) }
        let air = MenuBandAmbientAir(defaults: defaults)
        let engine = AVAudioEngine()
        let format = AVAudioFormat(standardFormatWithSampleRate: rate, channels: 2)!
        try engine.enableManualRenderingMode(.offline, format: format, maximumFrameCount: 512)
        air.attach(to: engine, output: engine.mainMixerNode)
        let instrument = AVAudioSourceNode(format: format) { _, _, frames, output in
            for buffer in UnsafeMutableAudioBufferListPointer(output) {
                let samples = buffer.mData!.assumingMemoryBound(to: Float.self)
                for i in 0..<(Int(frames) * Int(buffer.mNumberChannels)) { samples[i] = tone }
            }
            return noErr
        }
        engine.attach(instrument)
        engine.connect(instrument, to: engine.mainMixerNode, format: format)
        try engine.start()
        defer { engine.stop() }
        let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: 512)!
        try check(air) { frames in
            var result: [Float] = []
            while result.count < frames {
                let count = AVAudioFrameCount(min(512, frames - result.count))
                let status = try engine.renderOffline(count, to: buffer)
                guard status == .success else {
                    throw NSError(domain: "AmbientAirTests.render", code: Int(status.rawValue))
                }
                let left = buffer.floatChannelData![0], right = buffer.floatChannelData![1]
                for i in 0..<Int(buffer.frameLength) {
                    XCTAssertEqual(left[i], right[i], accuracy: 0.000001)
                    XCTAssertTrue(left[i].isFinite)
                    result.append(left[i])
                }
            }
            return result
        }
    }

    func testSliderAloneProducesAirAndReturnsToSilence() throws {
        for rate in [44_100.0, 48_000.0, 96_000.0] {
            try withEngine(rate: rate) { air, render in
                XCTAssertTrue(try render(1024).allSatisfy { $0 == 0 })
                air.setLevel(0.8)
                let audible = try render(Int(rate))
                XCTAssertGreaterThan(audible.map { abs($0) }.max()!, 0.001)
                XCTAssertLessThan(audible.map { abs($0) }.max()!, 0.12)
                air.setLevel(0)
                let faded = try render(Int(rate))
                XCTAssertTrue(faded.suffix(512).allSatisfy { abs($0) < 0.000001 })
            }
        }
    }

    func testChangingAirNeverMutesOrAttenuatesTheInstrument() throws {
        try withEngine(tone: 0.1) { air, render in
            let baseline = try render(2048)
            XCTAssertEqual(baseline.last!, 0.1, accuracy: 0.000001)
            air.setLevel(0.8)
            let mixed = try render(48_000)
            XCTAssertGreaterThan(mixed.map { abs($0 - 0.1) }.max()!, 0.001)
            XCTAssertTrue(mixed.allSatisfy { abs($0 - 0.1) < 0.12 })
            air.setLevel(0)
            let restored = try render(48_000)
            XCTAssertTrue(restored.suffix(512).allSatisfy { abs($0 - 0.1) < 0.000001 })
        }
    }

    func testLeavingMenuBandFadesAirAndReturningRestoresTheSliderLevel() throws {
        try withEngine(tone: 0.1) { air, render in
            air.setLevel(0.8)
            let playing = try render(48_000)
            XCTAssertGreaterThan(playing.suffix(4800).map { abs($0 - 0.1) }.max()!, 0.001)
            air.setFocused(false)
            let faded = try render(48_000)
            // Air is gone; the instrument is untouched; the slider still reads 80%.
            XCTAssertTrue(faded.suffix(512).allSatisfy { abs($0 - 0.1) < 0.000001 })
            XCTAssertEqual(air.level, 0.8)
            XCTAssertFalse(air.isActive)
            // Fade, not a cut: the first buffers after leaving still carry Air.
            XCTAssertGreaterThan(faded.prefix(512).map { abs($0 - 0.1) }.max()!, 0.001)
            air.setFocused(true)
            let back = try render(48_000)
            XCTAssertGreaterThan(back.suffix(4800).map { abs($0 - 0.1) }.max()!, 0.001)
            XCTAssertTrue(air.isActive)
        }
    }

    func testFocusEdgesDriveEngineActivityOnlyWhenAirIsAudible() {
        let suite = "MenuBandAmbientAirTests.\(UUID().uuidString)"
        let defaults = UserDefaults(suiteName: suite)!
        defer { defaults.removePersistentDomain(forName: suite) }
        let air = MenuBandAmbientAir(defaults: defaults)
        var activeChanges: [Bool] = []
        air.onActiveChange = { activeChanges.append($0) }
        // Slider at zero: focus changes are silent to the engine.
        air.setFocused(false); air.setFocused(true)
        XCTAssertEqual(activeChanges, [])
        air.setLevel(0.5)
        air.setFocused(false)
        air.setFocused(false)
        air.setLevel(0.9)            // slider moves while away stay quiet
        air.setFocused(true)
        air.setLevel(0)              // ...and zero while in front releases
        air.setFocused(false)        // nothing left to release
        XCTAssertEqual(activeChanges, [true, false, true, false])
        XCTAssertEqual(air.level, 0)
    }

    func testRelaunchRemembersColorAndFilterButStartsSilent() {
        let suite = "MenuBandAmbientAirTests.\(UUID().uuidString)"
        let defaults = UserDefaults(suiteName: suite)!
        defer { defaults.removePersistentDomain(forName: suite) }
        let original = MenuBandAmbientAir(defaults: defaults)
        original.setLevel(0.7); original.setColor(.white); original.setCutoff(1200)
        let restored = MenuBandAmbientAir(defaults: defaults)
        XCTAssertEqual(restored.level, 0)
        XCTAssertEqual(restored.color, .white)
        XCTAssertEqual(restored.cutoff, 1200)
        restored.setLevel(.nan)
        XCTAssertEqual(restored.level, 0)
    }
}
