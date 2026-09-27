import AVFoundation
import XCTest
@testable import MenuBand

/// End to end on a real engine: push, let the swell ease on the main run
/// loop, and read actual samples off the bed's mixer.
final class MenuBandAirVoiceEngineTests: XCTestCase {
    func testPushProducesSoundOnALiveEngine() throws {
        let engine = AVAudioEngine()
        let voice = MenuBandAirVoice()
        voice.attach(to: engine, output: engine.mainMixerNode)
        engine.mainMixerNode.outputVolume = 0   // silent to the room
        do { try engine.start() } catch {
            throw XCTSkip("no audio output on this host: \(error)")
        }
        defer { engine.stop() }
        RunLoop.main.run(until: Date(timeIntervalSinceNow: 0.6))
        let last = engine.outputNode.lastRenderTime
        let age = last.map {
            AVAudioTime.seconds(forHostTime: mach_absolute_time())
                - AVAudioTime.seconds(forHostTime: $0.hostTime)
        }
        print("air-test: running=\(engine.isRunning) lastRenderAge=\(String(describing: age)) live=\(engine.isRenderingLive)")
        guard engine.isRunning, last != nil else { throw XCTSkip("engine never started rendering") }

        var peak: Float = 0
        let lock = NSLock()
        voice.outputNodeForTesting.installTap(onBus: 0, bufferSize: 1024, format: nil) {
            buffer, _ in
            guard let data = buffer.floatChannelData?[0] else { return }
            var p: Float = 0
            for i in 0..<Int(buffer.frameLength) { p = max(p, abs(data[i])) }
            lock.lock(); peak = max(peak, p); lock.unlock()
        }
        defer { voice.outputNodeForTesting.removeTap(onBus: 0) }

        voice.push(pitches: [60, 64, 67])
        RunLoop.main.run(until: Date(timeIntervalSinceNow: 1.2))
        lock.lock(); let heard = peak; lock.unlock()
        XCTAssertGreaterThan(heard, 0.05, "the swell should be well up after 1.2 s")

        voice.release()
        RunLoop.main.run(until: Date(timeIntervalSinceNow: 0.3))
    }
}
