import AVFoundation
import Foundation

/// Bounded post-limiter attack meter used to balance Menu Band's live voice
/// against trackpad materials. It stores no samples: each marker measures the
/// next 120 ms, emits one compact line, then discards the accumulator.
final class MenuBandMixAnalysis {
    private struct Capture {
        let kind: String
        var frames = 0
        var sumSquares = 0.0
        var peak = 0.0
        var lowSquares = 0.0
        var midSquares = 0.0
        var highSquares = 0.0
    }

    private let markerLock = NSLock()
    private var pendingMarkers: [String] = []
    private var capture: Capture?
    private var lowState = 0.0
    private var fourKState = 0.0
    private var reportsRemaining = 96

    func mark(_ kind: String) {
        markerLock.lock()
        pendingMarkers.append(kind)
        markerLock.unlock()
    }

    func ingest(_ buffer: AVAudioPCMBuffer) {
        guard reportsRemaining > 0,
              let channels = buffer.floatChannelData else { return }
        let frameCount = Int(buffer.frameLength)
        guard frameCount > 0 else { return }
        let channelCount = max(1, Int(buffer.format.channelCount))
        let sampleRate = buffer.format.sampleRate

        markerLock.lock()
        if !pendingMarkers.isEmpty {
            let markers = pendingMarkers.reduce(into: [String]()) {
                if !$0.contains($1) { $0.append($1) }
            }
            capture = Capture(kind: markers.joined(separator: "+"))
            pendingMarkers.removeAll(keepingCapacity: true)
        }
        markerLock.unlock()
        guard var current = capture else { return }

        let lowA = 1.0 - exp(-2.0 * .pi * 250.0 / sampleRate)
        let fourKA = 1.0 - exp(-2.0 * .pi * 4_000.0 / sampleRate)
        for frame in 0..<frameCount {
            var mono = 0.0
            for channel in 0..<channelCount {
                mono += Double(channels[channel][frame])
            }
            mono /= Double(channelCount)
            lowState += lowA * (mono - lowState)
            fourKState += fourKA * (mono - fourKState)
            let low = lowState
            let mid = fourKState - lowState
            let high = mono - fourKState
            current.frames += 1
            current.sumSquares += mono * mono
            current.lowSquares += low * low
            current.midSquares += mid * mid
            current.highSquares += high * high
            current.peak = max(current.peak, abs(mono))
        }

        let targetFrames = Int(sampleRate * 0.12)
        if current.frames < targetFrames {
            capture = current
            return
        }
        capture = nil
        reportsRemaining -= 1
        let n = max(1.0, Double(current.frames))
        let rms = sqrt(current.sumSquares / n)
        let bandTotal = max(1e-15,
                            current.lowSquares + current.midSquares + current.highSquares)
        let peak = current.peak
        let peakDB = Self.db(peak)
        let rmsDB = Self.db(rms)
        let crestDB = rms > 0 ? Self.db(peak / rms) : 0
        let lowPercent = 100 * current.lowSquares / bandTotal
        let highPercent = 100 * current.highSquares / bandTotal
        let kind = current.kind
        DispatchQueue.main.async {
            NSLog(String(format:
                "MenuBand mix attack kind=%@ peak=%.2f dBFS rms=%.2f dBFS crest=%.2f dB lowPct=%.1f highPct=%.1f",
                kind, peakDB, rmsDB, crestDB, lowPercent, highPercent))
        }
    }

    private static func db(_ value: Double) -> Double {
        value > 0 ? 20 * log10(value) : -120
    }
}
