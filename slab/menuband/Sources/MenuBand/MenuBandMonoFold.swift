import AVFoundation
import AudioToolbox

/// Mono mode — the popover's "Mono" checkbox, beside "ABC".
///
/// Folds the finished stereo output down to one centred signal. It hangs a
/// post-render notify on an engine's output unit (the AUHAL that hands
/// frames to the device) and, when enabled, averages the channels in place
/// so both speakers carry the same mix. Nothing in the render graph moves:
/// no reconnects, no format changes, no engine restart — the toggle is a
/// flag the render thread reads on its next cycle. Averaging is idempotent,
/// so a notify that lands twice is harmless.
///
/// Why a notify and not a mono-format mixer: AVAudioMixerNode's stereo→mono
/// behaviour is undocumented, and reconnecting the limiter → mainMixer edge
/// on a live engine is exactly the kind of graph surgery `engineLock` exists
/// to keep rare.
enum MenuBandMonoFold {
    /// Read from the render thread, written from main. A whole-word Bool
    /// store is atomic on every Apple platform Menu Band runs on, and the
    /// worst race is one buffer folded a cycle late.
    static var enabled: Bool = false

    /// Output units that already carry the notify, by AU pointer. Keeps a
    /// repeated `start()` from stacking callbacks.
    private static var installed = Set<UnsafeMutableRawPointer>()

    /// Attach the fold to `engine`'s output unit. Safe before or after
    /// `engine.start()`; idempotent per unit. Only float32 output is
    /// folded — anything else is left alone rather than misread.
    static func install(on engine: AVAudioEngine, label: String) {
        guard let au = engine.outputNode.audioUnit else {
            debugLog("monofold(\(label)): no output audio unit")
            return
        }
        let key = UnsafeMutableRawPointer(au)
        if installed.contains(key) { return }
        let format = engine.outputNode.inputFormat(forBus: 0)
        guard format.commonFormat == .pcmFormatFloat32 else {
            NSLog("MenuBand mono fold (\(label)): output is not float32 (\(format)); not installed")
            return
        }
        let status = AudioUnitAddRenderNotify(au, menuBandMonoFoldNotify, nil)
        if status == noErr {
            installed.insert(key)
            debugLog("monofold(\(label)): installed on \(format.channelCount)ch")
        } else {
            NSLog("MenuBand mono fold (\(label)): AudioUnitAddRenderNotify failed \(status)")
        }
    }
}

/// The render notify. Fires pre- and post-render; only the post-render pass
/// carries the finished frames, and only then is the fold applied.
private func menuBandMonoFoldNotify(
    _ refCon: UnsafeMutableRawPointer,
    _ actionFlags: UnsafeMutablePointer<AudioUnitRenderActionFlags>,
    _ timestamp: UnsafePointer<AudioTimeStamp>,
    _ bus: UInt32,
    _ frameCount: UInt32,
    _ ioData: UnsafeMutablePointer<AudioBufferList>?
) -> OSStatus {
    guard actionFlags.pointee.contains(.unitRenderAction_PostRender),
          MenuBandMonoFold.enabled,
          let ioData else { return noErr }
    let buffers = UnsafeMutableAudioBufferListPointer(ioData)
    let frames = Int(frameCount)
    let byteFloor = UInt32(frames * MemoryLayout<Float>.size)

    if buffers.count >= 2 {
        // Non-interleaved: one buffer per channel. Average across all of
        // them and write the average back to each.
        var channels: [UnsafeMutablePointer<Float>] = []
        channels.reserveCapacity(buffers.count)
        for buffer in buffers {
            guard buffer.mNumberChannels == 1,
                  buffer.mDataByteSize >= byteFloor,
                  let data = buffer.mData else { return noErr }
            channels.append(data.assumingMemoryBound(to: Float.self))
        }
        let scale = 1 / Float(channels.count)
        for i in 0..<frames {
            var sum: Float = 0
            for channel in channels { sum += channel[i] }
            let mono = sum * scale
            for channel in channels { channel[i] = mono }
        }
    } else if buffers.count == 1 {
        // Interleaved: one buffer, samples striped by channel.
        let buffer = buffers[0]
        let channelCount = Int(buffer.mNumberChannels)
        guard channelCount >= 2,
              buffer.mDataByteSize >= byteFloor * UInt32(channelCount),
              let data = buffer.mData else { return noErr }
        let samples = data.assumingMemoryBound(to: Float.self)
        let scale = 1 / Float(channelCount)
        for i in 0..<frames {
            let base = i * channelCount
            var sum: Float = 0
            for c in 0..<channelCount { sum += samples[base + c] }
            let mono = sum * scale
            for c in 0..<channelCount { samples[base + c] = mono }
        }
    }
    return noErr
}
