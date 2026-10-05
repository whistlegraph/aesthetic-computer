import Foundation
import AVFoundation

@main struct SpeechCaptureCheck {
    static func main() throws {
        for rate in [16000.0, 24000, 44100, 48000] {
            let samples = (0..<Int(rate)).map { sin(Double($0) / rate * 2 * Double.pi * 440) * 0.3 }
            let whole = SpeechPCM().append(samples, rate: rate), stream = SpeechPCM()
            var chunks = Data()
            for i in stride(from: 0, to: samples.count, by: 317) {
                chunks.append(stream.append(Array(samples[i..<min(i + 317, samples.count)]), rate: rate))
            }
            precondition(whole == chunks, "Chunking changed the samples at \(rate) Hz")
            precondition(abs(whole.count - 48000) <= 2, "Streaming clock drift")
        }
        let root = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(at: root, withIntermediateDirectories: true)
        defer { try? FileManager.default.removeItem(at: root) }
        let url = root.appendingPathComponent("take.caf"), format = AVAudioFormat(standardFormatWithSampleRate: 48000, channels: 1)!
        let count = 820800 // 17.1 seconds, deliberately not a multiple of the decoder's block size.
        var file: AVAudioFile? = try AVAudioFile(forWriting: url, settings: format.settings)
        let buffer = AVAudioPCMBuffer(pcmFormat: format, frameCapacity: AVAudioFrameCount(count))!
        buffer.frameLength = AVAudioFrameCount(count)
        for i in 0..<count { buffer.floatChannelData![0][i] = Float(sin(Double(i) / 48000 * 2 * .pi * 440) * 0.003) }
        try file!.write(from: buffer); file = nil
        let wav = try RecordedTranscription.wav(url: url)
        precondition(wav.count == 44 + 17100 * 32, "Lost the final partial audio block")
        precondition(String(data: wav.prefix(4), encoding: .ascii) == "RIFF")
        precondition(wav.dropFirst(44).contains { $0 != 0 }, "Quiet speech was gated away")
        print("PASS: streaming resampling is chunk-invariant; quiet recording and final partial block survive")
    }
}
