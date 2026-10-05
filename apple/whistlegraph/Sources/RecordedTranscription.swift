import Foundation
import AVFoundation

struct TimedTranscript: Decodable {
    struct Word: Decodable { let text: String; let atMs: Double; let durationMs: Double }
    let transcript: String
    let words: [Word]
    var timeline: [[String: Any]] { words.map { ["text": $0.text, "atMs": $0.atMs, "durationMs": $0.durationMs] } }
}

enum RecordedTranscription {
    static func recover(_ id: String, token: String) async throws -> TimedTranscript {
        let audio = try await Task.detached(priority: .utility) { try wav(id) }.value
        try Task.checkCancellation()
        var request = URLRequest(url: URL(string: "https://help.aesthetic.computer/api/aesel/transcribe")!)
        request.httpMethod = "POST"; request.timeoutInterval = 15
        request.setValue("Bearer " + token, forHTTPHeaderField: "Authorization")
        request.setValue("application/json", forHTTPHeaderField: "Content-Type")
        request.httpBody = try JSONSerialization.data(withJSONObject: ["audio": audio.base64EncodedString()])
        let (data, response) = try await URLSession.shared.data(for: request)
        guard (response as? HTTPURLResponse)?.statusCode == 200 else { throw URLError(.badServerResponse) }
        return try JSONDecoder().decode(TimedTranscript.self, from: data)
    }
    static func wav(_ id: String) throws -> Data {
        guard let url = UtteranceRecording.url(id) else { throw URLError(.badURL) }
        return try wav(url: url)
    }
    static func wav(url: URL) throws -> Data {
        let file = try AVAudioFile(forReading: url), rate = file.processingFormat.sampleRate
        guard file.length > 0, Double(file.length) / rate <= 46 else { throw URLError(.dataLengthExceedsMaximum) }
        let buffer = AVAudioPCMBuffer(pcmFormat: file.processingFormat, frameCapacity: 16384)!
        var samples: [Float] = []; samples.reserveCapacity(Int(file.length))
        while file.framePosition < file.length {
            try file.read(into: buffer)
            guard buffer.frameLength > 0, let channel = buffer.floatChannelData?[0] else { throw URLError(.cannotDecodeContentData) }
            samples.append(contentsOf: UnsafeBufferPointer(start: channel, count: Int(buffer.frameLength)))
        }
        let count = Int(Double(samples.count) * 16000 / rate)
        var data = Data()
        func u32(_ n: UInt32) { var value = n.littleEndian; withUnsafeBytes(of: &value) { data.append(contentsOf: $0) } }
        func u16(_ n: UInt16) { var value = n.littleEndian; withUnsafeBytes(of: &value) { data.append(contentsOf: $0) } }
        data.append(Data("RIFF".utf8)); u32(UInt32(36 + count * 2)); data.append(Data("WAVEfmt ".utf8))
        u32(16); u16(1); u16(1); u32(16000); u32(32000); u16(2); u16(16)
        data.append(Data("data".utf8)); u32(UInt32(count * 2))
        for i in 0..<count {
            let position = Double(i) * rate / 16000, lo = Int(position), hi = min(lo + 1, samples.count - 1)
            let value = Double(samples[lo]) + (Double(samples[hi]) - Double(samples[lo])) * (position - Double(lo))
            u16(UInt16(bitPattern: Int16(max(-32768, min(32767, (value.isFinite ? value : 0) * 32767)))))
        }
        return data
    }
}
