import AVFoundation
import AudioToolbox

/// Last node before hardware: contain invalid samples before CoreAudio mixes
/// this app with other clients. Taps are observers, not an output filter.
final class MenuBandOutputGuard {
    private let unit = AVAudioUnitEQ(numberOfBands: 1)
    let health = MenuBandPlaybackHealth()
    var telemetry: MenuBandAudioTelemetry?

    init() {
        unit.bands[0].bypass = true
        unit.globalGain = 0
    }

    func attach(to engine: AVAudioEngine) {
        engine.attach(unit)
        engine.connect(engine.mainMixerNode, to: unit, format: nil)
        engine.connect(unit, to: engine.outputNode, format: nil)
        let status = AudioUnitAddRenderNotify(
            unit.audioUnit, menuBandOutputGuardNotify,
            Unmanaged.passUnretained(self).toOpaque())
        if status != noErr { NSLog("MenuBand: output guard install failed: \(status)") }
        health.begin()
    }

    /// Render-thread only: no allocation, logging, dispatch or waiting.
    func inspect(_ output: UnsafeMutablePointer<AudioBufferList>, frames: UInt32, silent: Bool) {
        if let telemetry, telemetry.enabled {
            telemetry.output.observe(UnsafePointer(output), frames: Int(frames), silent: silent)
        }
        var invalid = 0
        for buffer in UnsafeMutableAudioBufferListPointer(output) {
            guard let data = buffer.mData else { continue }
            if silent {
                memset(data, 0, Int(buffer.mDataByteSize))
                continue
            }
            let samples = data.assumingMemoryBound(to: Float.self)
            let count = min(Int(frames) * Int(buffer.mNumberChannels),
                            Int(buffer.mDataByteSize) / MemoryLayout<Float>.size)
            for i in 0..<count where !samples[i].isFinite {
                samples[i] = 0
                invalid += 1
            }
        }
        health.observe(invalid: invalid, at: ProcessInfo.processInfo.systemUptime)
    }

    deinit {
        AudioUnitRemoveRenderNotify(unit.audioUnit, menuBandOutputGuardNotify,
                                    Unmanaged.passUnretained(self).toOpaque())
    }
}

private let menuBandOutputGuardNotify: AURenderCallback = {
    context, flags, _, _, frames, output in
    guard flags.pointee.contains(.unitRenderAction_PostRender),
          !flags.pointee.contains(.unitRenderAction_PostRenderError),
          let output else { return noErr }
    Unmanaged<MenuBandOutputGuard>.fromOpaque(context).takeUnretainedValue()
        .inspect(output, frames: frames,
                 silent: flags.pointee.contains(.unitRenderAction_OutputIsSilence))
    return noErr
}
