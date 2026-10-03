import SwiftUI
import AVFoundation

struct StoryMovie: Identifiable { let id = UUID(); let url: URL }

@MainActor final class StoryExport: ObservableObject {
    enum Stage: Equatable { case preparing, rendering, finishingVideo, narration, encoding }
    @Published private(set) var busy = false
    @Published private(set) var recording = false
    @Published private(set) var stage = Stage.preparing
    @Published private(set) var progress = 0.0
    @Published var movie: StoryMovie?
    @Published var error = ""
    private weak var session: WhistlegraphSession?
    private var operation = UUID()
    private var raw: URL?
    private var sharedURL: URL?
    private var file: FileHandle?
    private var bytes = 0
    private var tapeStarted = false
    private var previousIdleTimer: Bool?
    private var started = Date()
    private var exportSession: AVAssetExportSession?
    private var narration: [(at: Double, task: Task<(URL, Double, Bool), Error>)] = []
    private var deadline: Task<Void, Never>?

    func begin(session: WhistlegraphSession, started: @escaping () -> Void) {
        guard !busy else { return }
        self.session = session; operation = UUID(); let expected = operation
        stage = .preparing; progress = 0
        busy = true; error = ""; bytes = 0; narration = []; tapeStarted = false
        previousIdleTimer = UIApplication.shared.isIdleTimerDisabled; UIApplication.shared.isIdleTimerDisabled = true
        Task {
            do {
                let url = FileManager.default.temporaryDirectory.appendingPathComponent("story-canvas-\(expected).mp4")
                FileManager.default.createFile(atPath: url.path, contents: nil)
                raw = url; file = try FileHandle(forWritingTo: url)
                session.storyTapeEvent = { [weak self] event in self?.receive(event) }
                guard operation == expected else { return }
                self.started = Date(); recording = true; started()
                deadline = Task { [weak self] in
                    try? await Task.sleep(for: .seconds(600))
                    guard !Task.isCancelled, let self, self.operation == expected else { return }
                    self.fail("Story export exceeded ten minutes. Export a shorter branch.")
                }
            } catch { if operation == expected { fail(error.localizedDescription) } }
        }
    }
    func update(version: Int, caption: String) {
        guard busy else { return }
        Task { try? await session?.storyTape("update", arguments: ["value": ["version": version, "caption": caption]]) }
    }
    func addNarration(_ row: PieceRevision) async {
        guard recording else { return }
        do {
            try await session?.storyTape("update", arguments: ["value": ["version": row.id, "caption": row.utterance]])
            if !tapeStarted {
                try await session?.storyTape("start", arguments: ["id": operation.uuidString])
                tapeStarted = true; started = Date(); stage = .rendering
            }
        } catch { fail(error.localizedDescription); return }
        guard !row.utterance.isEmpty || row.recordingID != nil else { return }
        let at = Date().timeIntervalSince(started)
        let task = Task<(URL, Double, Bool), Error> {
            if let id = row.recordingID, let url = UtteranceRecording.url(id), FileManager.default.fileExists(atPath: url.path) {
                return (url, try RecordingTrim.read(url).start, false)
            }
            return (try await StorySpeech.render(row.utterance), 0, true)
        }
        narration.append((at, task))
    }
    func finish() {
        guard recording else { return }; recording = false
        stage = .finishingVideo
        let expected = operation
        Task { do { try await session?.storyTape("stop") } catch { if operation == expected { fail(error.localizedDescription) } } }
    }
    private func receive(_ event: [String: Any]) {
        guard busy, event["session"] as? String == operation.uuidString else { return }
        do {
            switch event["kind"] as? String {
            case "chunk":
                guard let value = event["data"] as? String, value.count <= 300_000, let data = Data(base64Encoded: value) else { throw ExportError.invalidChunk }
                bytes += data.count; guard bytes <= 256_000_000 else { throw ExportError.tooLarge }
                try file?.write(contentsOf: data)
            case "done":
                try file?.close(); file = nil
                let expected = operation
                Task {
                    do {
                        guard let raw else { throw ExportError.noVideo }
                        let output = try await mux(raw, operation: expected)
                        guard operation == expected else { try? FileManager.default.removeItem(at: output); return }
                        #if DEBUG
                        if NativeScreenFixture.enabled && NativeScreenFixture.mode == "story" {
                            let evidence = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0].appendingPathComponent("story-export-test.mp4")
                            try? FileManager.default.removeItem(at: evidence); try FileManager.default.copyItem(at: output, to: evidence)
                        }
                        #endif
                        sharedURL = output; movie = StoryMovie(url: output); cleanup(); busy = false; recording = false
                    } catch { if operation == expected { fail(error.localizedDescription) } }
                }
            case "error": fail(event["error"] as? String ?? "Canvas recording failed.")
            default: break
            }
        } catch { fail(error.localizedDescription) }
    }
    private func cleanup() {
        if let previousIdleTimer { UIApplication.shared.isIdleTimerDisabled = previousIdleTimer }; previousIdleTimer = nil
        deadline?.cancel(); try? file?.close(); file = nil
        if let raw { try? FileManager.default.removeItem(at: raw) }; raw = nil
        session?.storyTapeEvent = nil
        for item in narration { Task { if let (url, _, temporary) = try? await item.task.value, temporary { try? FileManager.default.removeItem(at: url) } } }
        narration = []
    }
    private func fail(_ message: String) { cancel(); error = message }
    func cancel() {
        operation = UUID(); exportSession?.cancelExport(); exportSession = nil
        let current = session
        Task { try? await current?.storyTape("cancel") }
        cleanup(); busy = false; recording = false
    }
    func clearMovie() { if let sharedURL { try? FileManager.default.removeItem(at: sharedURL) }; sharedURL = nil; movie = nil }
    private func checkOperation(_ expected: UUID) throws {
        guard busy, operation == expected else { throw CancellationError() }
    }
    private func mux(_ raw: URL, operation expected: UUID) async throws -> URL {
        try checkOperation(expected)
        stage = .narration; progress = 0
        let asset = AVURLAsset(url: raw), mix = AVMutableComposition()
        let duration = try await asset.load(.duration)
        try checkOperation(expected)
        guard let video = try await asset.loadTracks(withMediaType: .video).first,
              let destination = mix.addMutableTrack(withMediaType: .video, preferredTrackID: kCMPersistentTrackID_Invalid) else { throw ExportError.noVideo }
        try destination.insertTimeRange(CMTimeRange(start: .zero, duration: duration), of: video, at: .zero)
        destination.preferredTransform = try await video.load(.preferredTransform)
        let clips = narration
        for (index, clip) in clips.enumerated() {
            let (url, trim, _) = try await clip.task.value
            try checkOperation(expected)
            let audio = AVURLAsset(url: url)
            guard let track = try await audio.loadTracks(withMediaType: .audio).first,
                  let target = mix.addMutableTrack(withMediaType: .audio, preferredTrackID: kCMPersistentTrackID_Invalid) else { continue }
            let length = try await audio.load(.duration).seconds - trim
            let end = index+1 < clips.count ? clips[index+1].at : duration.seconds
            let seconds = min(length, end-clip.at, duration.seconds-clip.at)
            if seconds > 0 { try target.insertTimeRange(CMTimeRange(start: CMTime(seconds: trim, preferredTimescale: 600), duration: CMTime(seconds: seconds, preferredTimescale: 600)), of: track, at: CMTime(seconds: clip.at, preferredTimescale: 600)) }
            progress = Double(index + 1) / Double(clips.count)
        }
        try checkOperation(expected)
        guard let exporter = AVAssetExportSession(asset: mix, presetName: AVAssetExportPresetHighestQuality) else { throw ExportError.noVideo }
        let output = FileManager.default.temporaryDirectory.appendingPathComponent("Whistlegraph-\(UUID()).mp4")
        exporter.outputURL = output; exporter.outputFileType = .mp4; exporter.shouldOptimizeForNetworkUse = true; exportSession = exporter
        stage = .encoding; progress = 0
        let monitor = Task { [weak self] in
            while !Task.isCancelled {
                guard let self, self.operation == expected, self.busy else { return }
                self.progress = Double(exporter.progress)
                try? await Task.sleep(for: .milliseconds(150))
            }
        }
        defer { monitor.cancel() }
        await exporter.export()
        guard operation == expected else { try? FileManager.default.removeItem(at: output); throw CancellationError() }
        exportSession = nil
        guard exporter.status == .completed else { try? FileManager.default.removeItem(at: output); throw exporter.error ?? ExportError.noVideo }
        progress = 1
        return output
    }
    private enum ExportError: LocalizedError {
        case noVideo, invalidChunk, tooLarge
        var errorDescription: String? {
            switch self { case .noVideo: return "Could not create the MP4."; case .invalidChunk: return "Invalid canvas tape data."; case .tooLarge: return "This story is too large. Export a shorter branch." }
        }
    }
}

// Render fallback narration to an audio file, never through a microphone.
@MainActor private final class StorySpeech {
    private let voice = AVSpeechSynthesizer()
    private var file: AVAudioFile?
    private var continuation: CheckedContinuation<URL, Error>?
    private var timeout: Task<Void, Never>?
    private let url = FileManager.default.temporaryDirectory.appendingPathComponent("story-voice-\(UUID()).caf")
    static func render(_ text: String) async throws -> URL {
        let renderer = StorySpeech()
        return try await renderer.render(text)
    }
    private func render(_ text: String) async throws -> URL {
        try await withCheckedThrowingContinuation { continuation in
            self.continuation = continuation
            self.timeout = Task {
                try? await Task.sleep(for: .seconds(45))
                guard !Task.isCancelled, let pending = self.continuation else { return }
                self.continuation = nil; self.voice.stopSpeaking(at: .immediate)
                pending.resume(throwing: NSError(domain: "StorySpeech", code: 1, userInfo: [NSLocalizedDescriptionKey: "Narration rendering timed out."]))
            }
            let line = AVSpeechUtterance(string: text.isEmpty ? " " : text)
            line.rate = AVSpeechUtteranceDefaultSpeechRate; line.voice = AVSpeechSynthesisVoice(language: "en-US")
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
struct StoryShareSheet: UIViewControllerRepresentable {
    let url: URL
    func makeUIViewController(context: Context) -> UIActivityViewController { UIActivityViewController(activityItems: [url], applicationActivities: nil) }
    func updateUIViewController(_ controller: UIActivityViewController, context: Context) {}
}
