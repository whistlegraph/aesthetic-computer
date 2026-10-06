import AVFoundation

struct StoryAudio {
    let url: URL
    let start: Double
    let end: Double
    let original: Bool
}

@MainActor enum StoryVoice {
    static let cache = StoryCache(name: "StoryVoice", limit: 32_000_000)
    private static var erasureGeneration = 0
    private static var pending: [String: (id: UUID, task: Task<URL, Error>)] = [:]

    static func audio(for row: PieceRevision) async throws -> StoryAudio? {
        if let id = row.recordingID, let url = UtteranceRecording.url(id), FileManager.default.fileExists(atPath: url.path), let trim = try? RecordingTrim.read(url) {
            return StoryAudio(url: url, start: trim.start, end: trim.end, original: true)
        }
        guard !row.utterance.isEmpty else { return nil }
        #if DEBUG
        if NativeScreenFixture.enabled, let url = Bundle.main.url(forResource: "basic-request", withExtension: "wav", subdirectory: "Fixtures") {
            return StoryAudio(url: url, start: 0, end: try AVAudioPlayer(contentsOf: url).duration, original: false)
        }
        #endif
        let url = try await rendered(row.utterance)
        return StoryAudio(url: url, start: 0, end: try AVAudioPlayer(contentsOf: url).duration, original: false)
    }

    static func cancelCloudRequests() {
        for work in pending.values { work.task.cancel() }
        pending.removeAll()
    }
    static func erasePendingWork() {
        erasureGeneration += 1
        cancelCloudRequests()
        DeviceStorySpeech.cancelAll()
    }
    static func rendered(_ text: String) async throws -> URL {
        let generation = erasureGeneration
        guard AIConsent.shared.cloudNarration else { return try await DeviceStorySpeech.render(text) }
        let key = StoryCache.key(["jeffrey-pvc-v1", text])
        if let url = cache.find(key, ext: "mp3") { return url }
        if let work = pending[key] { return try await work.task.value }
        let requestID = UUID()
        let task = Task<URL, Error> {
            try Task.checkCancellation()
            guard generation == erasureGeneration, AIConsent.shared.cloudNarration else { throw VoiceError.unavailable }
            var request = URLRequest(url: URL(string: "https://aesthetic.computer/api/say")!)
            request.httpMethod = "POST"; request.timeoutInterval = 20
            request.setValue("application/json", forHTTPHeaderField: "Content-Type")
            request.httpBody = try JSONSerialization.data(withJSONObject: ["from": text, "provider": "jeffrey", "voice": "neutral:0", "speed": 1])
            let (data, response) = try await URLSession.shared.data(for: request)
            try Task.checkCancellation()
            guard generation == erasureGeneration, AIConsent.shared.cloudNarration else { throw CancellationError() }
            guard let response = response as? HTTPURLResponse, response.statusCode == 200, data.count > 0, data.count < 8_000_000 else { throw VoiceError.unavailable }
            let temporary = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString + ".mp3")
            defer { try? FileManager.default.removeItem(at: temporary) }
            try data.write(to: temporary, options: .atomic)
            guard try AVAudioPlayer(contentsOf: temporary).duration > 0 else { throw VoiceError.unavailable }
            return try cache.store(temporary, key: key, ext: "mp3")
        }
        pending[key] = (requestID, task)
        defer { if pending[key]?.id == requestID { pending[key] = nil } }
        do { return try await task.value }
        catch {
            guard generation == erasureGeneration, !Task.isCancelled else { throw CancellationError() }
            // Offline exports remain usable. Never cache the fallback as Jeffrey:
            // reconnecting must retry the requested voice.
            return try await DeviceStorySpeech.render(text)
        }
    }
    private enum VoiceError: Error { case unavailable }
}

@MainActor private final class DeviceStorySpeech {
    private static var active: [UUID: DeviceStorySpeech] = [:]
    private let identifier = UUID()
    private let voice = AVSpeechSynthesizer()
    static func cancelAll() {
        for speech in Array(active.values) { speech.cancel() }
    }
    private func cancel() {
        timeout?.cancel(); voice.stopSpeaking(at: .immediate); file = nil
        let pending = continuation; continuation = nil
        pending?.resume(throwing: CancellationError())
        try? FileManager.default.removeItem(at: url)
    }
    private var file: AVAudioFile?
    private var continuation: CheckedContinuation<URL, Error>?
    private var timeout: Task<Void, Never>?
    private let url = FileManager.default.temporaryDirectory.appendingPathComponent("story-voice-\(UUID()).caf")
    static func render(_ text: String) async throws -> URL { try await DeviceStorySpeech().render(text) }
    private func render(_ text: String) async throws -> URL {
        Self.active[identifier] = self
        defer { Self.active[identifier] = nil }
        return try await withCheckedThrowingContinuation { continuation in
            self.continuation = continuation
            timeout = Task {
                try? await Task.sleep(for: .seconds(15))
                guard !Task.isCancelled, let pending = self.continuation else { return }
                self.continuation = nil; self.voice.stopSpeaking(at: .immediate)
                pending.resume(throwing: NSError(domain: "StoryVoice", code: 1, userInfo: [NSLocalizedDescriptionKey: "Narration is unavailable. Try again when connected."]))
            }
            let line = AVSpeechUtterance(string: text); line.voice = AVSpeechSynthesisVoice(language: "en-US")
            voice.write(line) { buffer in
                guard let pcm = buffer as? AVAudioPCMBuffer else { return }
                Task { @MainActor in
                    guard let continuation = self.continuation else { return }
                    do {
                        if pcm.frameLength == 0 { self.timeout?.cancel(); self.continuation = nil; self.file = nil; continuation.resume(returning: self.url); return }
                        if self.file == nil { self.file = try AVAudioFile(forWriting: self.url, settings: pcm.format.settings) }
                        try self.file?.write(from: pcm)
                    } catch { self.timeout?.cancel(); self.continuation = nil; continuation.resume(throwing: error) }
                }
            }
        }
    }
}
