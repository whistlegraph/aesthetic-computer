import Foundation
import AVFoundation

// Original microphone samples stay in Application Support, outside model requests.
// Called only on MusicalInput's serial queue; finish closes the file before playback.
final class UtteranceRecording {
    let id = UUID().uuidString
    private var file: AVAudioFile?
    private var failed = false
    private var count = 0
    static func url(_ id: String) -> URL? {
        guard UUID(uuidString: id) != nil else { return nil }
        return FileManager.default.urls(for: .applicationSupportDirectory, in: .userDomainMask).first?
            .appendingPathComponent("Utterances", isDirectory: true).appendingPathComponent(id + ".caf")
    }
    func append(_ samples: [Double], rate: Double) {
        guard !failed, rate > 0, count < Int(rate * 46), let url = Self.url(id),
              let format = AVAudioFormat(standardFormatWithSampleRate: rate, channels: 1) else { return }
        do {
            if file == nil {
                try FileManager.default.createDirectory(at: url.deletingLastPathComponent(), withIntermediateDirectories: true)
                file = try AVAudioFile(forWriting: url, settings: format.settings)
            }
            let length = min(samples.count, Int(rate * 46) - count)
            guard let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: AVAudioFrameCount(length)),
                  let channel = buffer.floatChannelData?[0] else { return }
            buffer.frameLength = AVAudioFrameCount(length)
            for i in 0..<length { channel[i] = Float(samples[i]) }
            try file?.write(from: buffer); count += length
        } catch { failed = true; file = nil; try? FileManager.default.removeItem(at: url) }
    }
    func finish() -> String? { file = nil; return !failed && count > 0 ? id : nil }
}
