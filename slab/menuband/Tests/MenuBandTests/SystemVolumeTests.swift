import XCTest
import CoreAudio
@testable import MenuBand

#if !MAC_APP_STORE
@available(macOS 14.2, *)
final class SystemVolumeTests: XCTestCase {
    /// Buffers represent physical microphone channels followed by the tap;
    /// the renderer must never monitor those microphone samples.
    private func render(_ samples: [[Float]], widths: [UInt32], inputChannels: Int,
                        gain: Float, nextGain: Float? = nil, outputWidths: [UInt32] = [2], air: Float = 0) -> [[Float]] {
        let frames = samples.last!.count / Int(widths.last!)
        func list(_ values: [[Float]], _ channels: [UInt32]) -> UnsafeMutableAudioBufferListPointer {
            let list = AudioBufferList.allocate(maximumBuffers: values.count)
            list.unsafeMutablePointer.pointee.mNumberBuffers = UInt32(values.count)
            for i in values.indices {
                let p = UnsafeMutablePointer<Float>.allocate(capacity: values[i].count)
                p.initialize(from: values[i], count: values[i].count)
                list[i] = AudioBuffer(mNumberChannels: channels[i], mDataByteSize: UInt32(values[i].count * 4), mData: p)
            }
            return list
        }
        let input = list(samples, widths)
        let output = list(outputWidths.map { Array(repeating: Float(99), count: frames * Int($0)) }, outputWidths)
        defer {
            for b in input { b.mData!.deallocate() }; input.unsafeMutablePointer.deallocate()
            for b in output { b.mData!.deallocate() }; output.unsafeMutablePointer.deallocate()
        }
        let helper = MenuBandSystemVolumeHelper(inputChannels: inputChannels, gain: gain)
        helper.setAir(air, color: .cabin)
        if let nextGain { helper.setGain(nextGain) }
        helper.process(input: UnsafePointer(input.unsafeMutablePointer), output: output.unsafeMutablePointer)
        return output.map { Array(UnsafeBufferPointer(start: $0.mData!.assumingMemoryBound(to: Float.self), count: Int($0.mDataByteSize) / 4)) }
    }
    func testPhysicalMicrophoneNeverReachesOutput() {
        XCTAssertEqual(render([[99,99,99,99], [0.8,-0.4,0.4,-0.8]], widths: [2,2], inputChannels: 2, gain: 0.25)[0], [0.2,-0.1,0.1,-0.2])
    }
    func testPlanarTapAndPlanarOutput() {
        XCTAssertEqual(render([[99,99], [99,99], [0.8,0.4], [-0.4,-0.8]], widths: [1,1,1,1], inputChannels: 2, gain: 0.25, outputWidths: [1,1]), [[0.2,0.1],[-0.1,-0.2]])
    }
    func testSilentTapDoesNotLeakMicOrPreviousOutput() {
        XCTAssertEqual(render([[99,99,99,99], [0,0,0,0]], widths: [2,2], inputChannels: 2, gain: 0.25)[0], [0,0,0,0])
    }
    func testInvalidSamplesAreSilencedAndGainCannotAmplify() {
        XCTAssertEqual(render([[Float.nan, Float.infinity, 0.5, -0.5]], widths: [2], inputChannels: 0, gain: 9)[0], [0,0,0.5,-0.5])
    }
    func testGainChangeRampsBothChannelsTogetherAndReachesMute() {
        let result = render([Array(repeating: 1, count: 2048)], widths: [2], inputChannels: 0, gain: 0.5, nextGain: 0)[0]
        XCTAssertGreaterThan(result[0], 0.49)
        XCTAssertEqual(result[0], result[1])
        XCTAssertEqual(result.suffix(2), [0,0])
        for frame in 1..<1024 { XCTAssertLessThanOrEqual(result[frame*2], result[(frame-1)*2]) }
    }
    func testAirWorksWithoutMusicAndSystemMuteIncludesAir() {
        let silence = [Array(repeating: Float(0), count: 8192)]
        let air = render(silence, widths: [2], inputChannels: 0, gain: 0.25, air: 0.7)[0]
        XCTAssertGreaterThan(air.map { abs($0) }.max()!, 0.0001)
        XCTAssertLessThan(air.map { abs($0) }.max()!, 0.03)
        for frame in 0..<4096 { XCTAssertEqual(air[frame*2], air[frame*2+1]) }
        XCTAssertTrue(render(silence, widths: [2], inputChannels: 0, gain: 0, air: 1)[0].allSatisfy { $0 == 0 })
    }
}
#endif
