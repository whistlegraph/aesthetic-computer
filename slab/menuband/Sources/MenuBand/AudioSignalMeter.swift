import AVFoundation

/// Scalar diagnostics only. No audio is retained, and the render thread
/// never waits for the reporting thread. Channels are measured independently
/// so opposite-phase stereo cannot be mistaken for silence.
final class AudioSignalMeter {
    private let lock = NSLock()
    private var callbacks = 0, measuredBuffers = 0, frames = 0, samples = 0
    private var invalid = 0, silentBuffers = 0, errors = 0
    private var peak = 0.0, squares = 0.0, maxGap = 0.0
    private var minFrames = Int.max, maxFrames = 0
    private var lastCallback: TimeInterval?, lastSignal: TimeInterval?

    func observe(_ buffer: AVAudioPCMBuffer) {
        guard buffer.format.commonFormat == .pcmFormatFloat32 else { return }
        observe(buffer.audioBufferList, frames: Int(buffer.frameLength))
    }

    func observe(_ output: UnsafePointer<AudioBufferList>?, frames count: Int,
                 silent: Bool = false, error: Bool = false,
                 at now: TimeInterval = ProcessInfo.processInfo.systemUptime) {
        var blockPeak = 0.0, blockSquares = 0.0
        var blockSamples = 0, blockInvalid = 0
        if !error, let output {
            for buffer in UnsafeMutableAudioBufferListPointer(UnsafeMutablePointer(mutating: output)) {
                guard let data = buffer.mData else { continue }
                let n = min(count * Int(buffer.mNumberChannels), Int(buffer.mDataByteSize) / 4)
                let values = data.assumingMemoryBound(to: Float.self)
                blockSamples += n
                if silent { continue } // Contents may be undefined when silence is flagged.
                for i in 0..<n {
                    let value = Double(values[i])
                    if value.isFinite {
                        blockPeak = max(blockPeak, abs(value))
                        blockSquares += value * value
                    } else { blockInvalid += 1 }
                }
            }
        }
        guard lock.try() else { return }
        defer { lock.unlock() }
        callbacks += 1
        frames += count
        if blockSamples > 0 { measuredBuffers += 1 }
        samples += blockSamples
        invalid += blockInvalid
        if silent { silentBuffers += 1 }
        if error { errors += 1 }
        peak = max(peak, blockPeak)
        squares += blockSquares
        minFrames = min(minFrames, count)
        maxFrames = max(maxFrames, count)
        if let lastCallback { maxGap = max(maxGap, now - lastCallback) }
        lastCallback = now
        if blockPeak > 0.00001 { lastSignal = now } // -100 dBFS
    }

    struct Snapshot: Codable {
        let callbacks, measuredBuffers, frames, samples, invalidSamples: Int
        let silentFlagBuffers, renderErrors, minFrames, maxFrames: Int
        let peakDBFS, rmsDBFS, maxGapMs: Double
        let lastCallbackAgeMs, lastSignalAgeMs: Double?
    }

    func takeSnapshot(at now: TimeInterval = ProcessInfo.processInfo.systemUptime) -> Snapshot {
        lock.lock(); defer { lock.unlock() }
        let result = Snapshot(callbacks: callbacks, measuredBuffers: measuredBuffers,
            frames: frames, samples: samples, invalidSamples: invalid,
            silentFlagBuffers: silentBuffers, renderErrors: errors,
            minFrames: minFrames == Int.max ? 0 : minFrames, maxFrames: maxFrames,
            peakDBFS: Self.db(peak), rmsDBFS: Self.db(sqrt(squares / Double(max(1, samples)))),
            maxGapMs: maxGap * 1000,
            lastCallbackAgeMs: lastCallback.map { max(0, now - $0) * 1000 },
            lastSignalAgeMs: lastSignal.map { max(0, now - $0) * 1000 })
        callbacks = 0; measuredBuffers = 0; frames = 0; samples = 0
        invalid = 0; silentBuffers = 0; errors = 0
        peak = 0; squares = 0; maxGap = 0; minFrames = Int.max; maxFrames = 0
        return result
    }

    private static func db(_ value: Double) -> Double {
        value > 0 ? max(-160, 20 * log10(value)) : -160
    }
}
