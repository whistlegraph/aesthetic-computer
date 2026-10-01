import Foundation
import AVFoundation
import Speech

/// Only enabled by an explicit debug launch; never records ordinary user speech.
@MainActor enum AudioBenchmark {
    static var enabled: Bool {
        #if DEBUG
        return ProcessInfo.processInfo.environment["WALKIE_AUDIO_TEST"] == "1"
        #else
        return false
        #endif
    }
    static var fixture: String {
        let requested = ProcessInfo.processInfo.environment["WALKIE_AUDIO_FIXTURE"] ?? "basic-request"
        return ["basic-request", "sine-tone", "whistle-sweep", "mixed-request"].contains(requested) ? requested : "basic-request"
    }
    private static var runID = UUID().uuidString
    private static var start = ProcessInfo.processInfo.systemUptime
    private static var events: [[String: Any]] = []
    private static var seen = Set<String>()
    static func reset() { runID = ProcessInfo.processInfo.environment["WALKIE_RUN_ID"] ?? UUID().uuidString; start = ProcessInfo.processInfo.systemUptime; events = []; seen = [] }
    static func mark(_ name: String, fields: [String: Any] = [:]) {
        guard enabled, !seen.contains(name) else { return }
        seen.insert(name)
        let elapsed = (ProcessInfo.processInfo.systemUptime - start) * 1000
        events.append(["event": name, "ms": elapsed, "details": fields])
        let result: [String: Any] = ["model": ProcessInfo.processInfo.environment["WALKIE_MODEL"] ?? "deepseek/deepseek-v4.1-flash", "runID": runID, "fixture": fixture, "mode": "real-time injected PCM; microphone hardware excluded", "events": events]
        if let data = try? JSONSerialization.data(withJSONObject: result, options: [.prettyPrinted, .sortedKeys]),
           let directory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first {
            try? data.write(to: directory.appendingPathComponent("walkieware-benchmark.json"), options: .atomic)
        }
        print("[walkieware-benchmark] \(name) \(Int(elapsed))ms")
    }
    static func checkTranscript(_ text: String) {
        let normalized = text.lowercased().filter { $0.isLetter || $0.isWhitespace }.split(separator: " ").joined(separator: " ")
        mark("submittedTranscript", fields: ["matchesFixture": normalized == "make a pink circle", "characters": text.count, "syntheticFixtureTranscript": text])
    }
    static func replay(into request: SFSpeechAudioBufferRecognitionRequest, sound: MusicalInput, release: @escaping () -> Void) async throws {
        guard let url = Bundle.main.url(forResource: fixture, withExtension: "wav", subdirectory: "Fixtures") else {
            throw NSError(domain: "WalkiewareBenchmark", code: 1, userInfo: [NSLocalizedDescriptionKey: "Missing audio fixture"])
        }
        let file = try AVAudioFile(forReading: url)
        let began = ProcessInfo.processInfo.systemUptime
        mark("audioStarted", fields: ["durationMs": Double(file.length) / file.processingFormat.sampleRate * 1000])
        while file.framePosition < file.length {
            try Task.checkCancellation()
            let size = AVAudioFrameCount(min(320, file.length - file.framePosition))
            guard let buffer = AVAudioPCMBuffer(pcmFormat: file.processingFormat, frameCapacity: size) else { break }
            try file.read(into: buffer, frameCount: size)
            request.append(buffer)
            sound.feed(buffer)
            let target = began + Double(file.framePosition) / file.processingFormat.sampleRate
            let delay = max(0, target - ProcessInfo.processInfo.systemUptime)
            try await Task.sleep(nanoseconds: UInt64(delay * 1_000_000_000))
        }
        mark("audioEnded")
        release()
    }
}
