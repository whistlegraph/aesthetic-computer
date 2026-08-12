import Foundation
import AVFoundation

/// Direct duplex monitor: the host engine pulls Scarlett input as part of the
/// same render cycle that produces Menu Band's output. There is deliberately
/// no tap, copied buffer, player queue, or second clock domain in this path.
final class MenuBandInputMonitor {
    private let monoMixer = AVAudioMixerNode()
    private var attached = false

    var isAttached: Bool { attached }

    func attach(to engine: AVAudioEngine, output: AVAudioNode) {
        guard !attached else { return }
        let input = engine.inputNode
        let inputFormat = input.inputFormat(forBus: 0)
        guard inputFormat.channelCount > 0, inputFormat.sampleRate > 0,
              let monoFormat = AVAudioFormat(
                commonFormat: .pcmFormatFloat32,
                sampleRate: inputFormat.sampleRate,
                channels: 1, interleaved: false) else { return }
        engine.attach(monoMixer)
        engine.connect(input, to: monoMixer, format: inputFormat)
        engine.connect(monoMixer, to: output, format: monoFormat)
        monoMixer.outputVolume = 0
        attached = true
        NSLog("MenuBand monitor: direct duplex ch=\(inputFormat.channelCount) sr=\(inputFormat.sampleRate)")
    }

    /// Unwire the duplex graph so the engine goes back to being a pure
    /// output graph (no aggregate, no open microphone). Callers stop the
    /// engine around this — inputNode wiring only negotiates cleanly
    /// across a start.
    func detach(from engine: AVAudioEngine) {
        guard attached else { return }
        engine.disconnectNodeOutput(engine.inputNode)
        engine.disconnectNodeOutput(monoMixer)
        engine.detach(monoMixer)
        attached = false
    }

    func setEnabled(_ value: Bool) {
        monoMixer.outputVolume = value ? 1 : 0
    }

    /// Dry input capture still arrives through SampleVoice for the isolated
    /// tape stem. Monitoring itself is pull-driven by the direct engine graph.
    func ingest(_ input: AVAudioPCMBuffer) {}
}

extension Notification.Name {
    static let menuBandInputMonitoringChanged =
        Notification.Name("MenuBandInputMonitoringChanged")
}
