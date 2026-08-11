import Foundation
import AVFoundation

/// Scarlett input 1 bridged into Menu Band's musical graph.
final class MenuBandInputMonitor {
    private let format = AVAudioFormat(
        commonFormat: .pcmFormatFloat32, sampleRate: 48_000,
        channels: 1, interleaved: false)!
    private let player = AVAudioPlayerNode()
    private let gain = AVAudioUnitEQ(numberOfBands: 0)
    private let lock = NSLock()
    private var queued = 0
    private var enabled = false

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        engine.attach(player)
        engine.attach(gain)
        gain.globalGain = 18
        engine.connect(player, to: gain, format: format)
        engine.connect(gain, to: output, format: format)
    }

    func setEnabled(_ value: Bool) {
        enabled = value
        if !value {
            player.stop()
            lock.lock(); queued = 0; lock.unlock()
        }
    }

    func ingest(_ input: AVAudioPCMBuffer) {
        guard enabled, input.frameLength > 0,
              let source = input.floatChannelData?[0],
              let mono = AVAudioPCMBuffer(
                pcmFormat: format, frameCapacity: input.frameLength),
              let destination = mono.floatChannelData?[0] else { return }
        guard abs(input.format.sampleRate - format.sampleRate) < 0.5 else {
            return
        }
        mono.frameLength = input.frameLength
        memcpy(destination, source,
               Int(input.frameLength) * MemoryLayout<Float>.size)
        lock.lock(); let backlog = queued; lock.unlock()
        if backlog > 1 {
            player.stop()
            lock.lock(); queued = 0; lock.unlock()
        }
        lock.lock(); queued += 1; lock.unlock()
        player.scheduleBuffer(mono, completionCallbackType: .dataConsumed) {
            [weak self] _ in
            guard let self else { return }
            self.lock.lock()
            self.queued = max(0, self.queued - 1)
            self.lock.unlock()
        }
        if !player.isPlaying { player.play() }
    }
}

extension Notification.Name {
    static let menuBandInputMonitoringChanged =
        Notification.Name("MenuBandInputMonitoringChanged")
}
