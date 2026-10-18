import AVFoundation
import XCTest
@testable import MenuBand

final class OutputGuardTests: XCTestCase {
    func testSilentRenderingDoesNotLookLikeAStalledEngine() throws {
        let engine = AVAudioEngine()
        let format = AVAudioFormat(standardFormatWithSampleRate: 48_000, channels: 2)!
        try engine.enableManualRenderingMode(.offline, format: format, maximumFrameCount: 512)
        let source = AVAudioSourceNode(format: format) { silent, _, _, output in
            silent.pointee = true
            for audio in UnsafeMutableAudioBufferListPointer(output) {
                if let data = audio.mData { memset(data, 0, Int(audio.mDataByteSize)) }
            }
            return noErr
        }
        engine.attach(source)
        engine.connect(source, to: engine.mainMixerNode, format: format)
        let guardNode = MenuBandOutputGuard()
        guardNode.attach(to: engine)
        try engine.start()
        defer { engine.stop() }
        guardNode.health.begin(at: 0)
        let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: 512)!
        for _ in 0..<10 { XCTAssertEqual(try engine.renderOffline(512, to: buffer), .success) }
        XCTAssertNil(guardNode.health.recoveryReason(shouldRun: true, isRunning: true))
    }

    func testFinalRenderContainsInvalidSamplesAndSurvivesEngineRecovery() throws {
        for rate in [44_100.0, 48_000.0, 96_000.0] {
            let engine = AVAudioEngine()
            let format = AVAudioFormat(standardFormatWithSampleRate: rate, channels: 2)!
            try engine.enableManualRenderingMode(.offline, format: format, maximumFrameCount: 512)
            var injectFault = false
            let source = AVAudioSourceNode(format: format) { _, _, frames, output in
                for audio in UnsafeMutableAudioBufferListPointer(output) {
                    let samples = audio.mData!.assumingMemoryBound(to: Float.self)
                    for i in 0..<Int(frames) { samples[i] = 0.125 }
                    if injectFault {
                        samples[0] = .nan; samples[1] = .infinity; samples[2] = -.infinity
                    }
                }
                return noErr
            }
            engine.attach(source)
            engine.connect(source, to: engine.mainMixerNode, format: format)
            let guardNode = MenuBandOutputGuard()
            guardNode.attach(to: engine)
            try engine.start()
            defer { engine.stop() }
            let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: 512)!
            func render() throws -> [Float] {
                XCTAssertEqual(try engine.renderOffline(512, to: buffer), .success)
                return (0..<2).flatMap { channel in
                    Array(UnsafeBufferPointer(start: buffer.floatChannelData![channel],
                                              count: Int(buffer.frameLength)))
                }
            }
            // nil graph formats deliberately follow the device. At a different
            // offline rate the converter needs a few blocks to prime its filter.
            for _ in 0..<4 { _ = try render() }
            XCTAssertTrue(try render().allSatisfy { abs($0 - 0.125) < 0.00001 }, "rate \(rate)")
            injectFault = true
            let contained = try render()
            XCTAssertTrue(contained.allSatisfy(\.isFinite))
            let reason = guardNode.health.recoveryReason(
                at: ProcessInfo.processInfo.systemUptime + 3, shouldRun: true, isRunning: true)
            XCTAssertTrue(reason?.contains("non-finite") == true)
            injectFault = false
            engine.stop()
            engine.reset()
            try engine.start()
            // The real post-render callback still runs after stop/reset/start,
            // and valid instrument audio passes through at its original gain.
            for _ in 0..<4 { _ = try render() }
            XCTAssertTrue(try render().allSatisfy { abs($0 - 0.125) < 0.00001 }, "rate \(rate)")
            injectFault = true
            XCTAssertTrue(try render().allSatisfy(\.isFinite))
        }
    }
}
