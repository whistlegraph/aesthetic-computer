import Foundation
import AVFoundation

struct RecordingTrim {
    static func bounds(energies: [Double], window: Double, duration: Double) -> (start: Double, end: Double) {
        let peak = energies.max() ?? 0
        guard peak > 0.001 else { return (0, duration) }
        let threshold = max(0.001, peak * 0.04)
        guard let first = energies.firstIndex(where: { $0 >= threshold }),
              let last = energies.lastIndex(where: { $0 >= threshold }) else { return (0, duration) }
        return (max(0, Double(first) * window - 0.08), min(duration, Double(last + 1) * window + 0.12))
    }
    static func read(_ url: URL) throws -> (start: Double, end: Double) {
        let file = try AVAudioFile(forReading: url)
        let rate = file.processingFormat.sampleRate
        let window = max(1, Int(rate * 0.02))
        guard let buffer = AVAudioPCMBuffer(pcmFormat: file.processingFormat, frameCapacity: AVAudioFrameCount(window)) else { return (0, Double(file.length) / rate) }
        var energies: [Double] = []
        while file.framePosition < file.length {
            try file.read(into: buffer, frameCount: AVAudioFrameCount(window))
            guard let samples = buffer.floatChannelData?[0], buffer.frameLength > 0 else { break }
            var energy = 0.0
            for i in 0..<Int(buffer.frameLength) { energy += Double(samples[i]) * Double(samples[i]) }
            energies.append(sqrt(energy / Double(buffer.frameLength)))
        }
        return bounds(energies: energies, window: Double(window) / rate, duration: Double(file.length) / rate)
    }
}
