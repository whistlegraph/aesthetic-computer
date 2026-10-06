import SwiftUI
import AVFoundation

struct StoryMovie: Identifiable { let id = UUID(); let url: URL }

// One canvas encoder, one card at a time. Completed cards are durable; seeking
// discards only the unfinished card. Assembly reuses the cached MP4 tracks.
@MainActor final class StoryExport: ObservableObject {
    enum Stage: Equatable { case preparing, rendering, finishingVideo, narration, encoding }
    @Published private(set) var busy = false
    @Published private(set) var requested = false
    @Published private(set) var recording = false
    @Published private(set) var stage = Stage.preparing
    @Published private(set) var progress = 0.0
    @Published private(set) var completedCards = 0
    @Published private(set) var readyURL: URL?
    @Published var movie: StoryMovie?
    @Published var error = ""
    private let cache: StoryCache
    private static var fixtureReset = false
    private weak var session: WhistlegraphSession?
    private var rows: [PieceRevision] = []
    private var keys: [Int: String] = [:]
    private var storyKey = ""
    private var active = false
    private var operation = UUID()
    private var storyRun = UUID()
    private var currentKey: String?
    private var raw: URL?
    private var file: FileHandle?
    private var bytes = 0
    private var audio: StoryAudio?
    private var previousIdleTimer: Bool?
    private var exportSession: AVAssetExportSession?
    private var deadline: Task<Void, Never>?
    private var completion: CheckedContinuation<Void, Never>?
    private var reset: Task<Void, Never>?
    private var assembly: Task<Void, Never>?

    init(cache: StoryCache? = nil) {
        #if DEBUG
        self.cache = cache ?? StoryCache(name: NativeScreenFixture.enabled ? "StoryMovies-Fixture" : "StoryMovies")
        #else
        self.cache = cache ?? StoryCache()
        #endif
    }

    func prepare(session: WhistlegraphSession, rows: [PieceRevision]) {
        cancel(); self.session = session; self.rows = rows
        #if DEBUG
        if NativeScreenFixture.enabled, !Self.fixtureReset, ProcessInfo.processInfo.environment["WALKIE_RESET_STORY_CACHE"] == "1" {
            try? FileManager.default.removeItem(at: cache.directory); Self.fixtureReset = true
        }
        #endif
        keys = Dictionary(uniqueKeysWithValues: rows.map { row in
            (row.id, StoryCache.key(["story-card-v3-comic", session.snapshot.code, String(row.id), row.createdAt, row.utterance, row.recordingID ?? "", String(session.pixelSize), StoryCardStyle.cssBackground(code: session.snapshot.code, version: row.id)]))
        })
        storyKey = StoryCache.key(["story-movie-v2"] + rows.compactMap { keys[$0.id] })
        readyURL = cache.find(storyKey); completedCards = rows.filter { cached($0) != nil }.count
        active = true; busy = readyURL == nil; stage = .preparing; error = ""
        if readyURL == nil { assembleIfReady() }
    }
    private func cached(_ row: PieceRevision) -> URL? { keys[row.id].flatMap { cache.find($0) } }
    var firstMissingIndex: Int? { rows.firstIndex { cached($0) == nil } }
    func needsCard(at index: Int) -> Bool { rows.indices.contains(index) && cached(rows[index]) == nil }

    // Called after this exact revision paints and its narration file is ready.
    func startCard(_ row: PieceRevision, audio: StoryAudio?) async {
        guard active, readyURL == nil, cached(row) == nil, assembly == nil else { return }
        if let audio, audio.url.lastPathComponent.hasPrefix("story-voice-"), let key = keys[row.id] {
            keys[row.id] = StoryCache.key([key, "device-fallback"])
            storyKey = StoryCache.key(["story-movie-v2"] + rows.compactMap { keys[$0.id] })
        }
        discardCard()
        let expected = operation
        await reset?.value
        guard active, operation == expected else { return }
        do {
            currentKey = keys[row.id]; self.audio = audio; bytes = 0
            let url = FileManager.default.temporaryDirectory.appendingPathComponent("story-canvas-\(expected).mp4")
            FileManager.default.createFile(atPath: url.path, contents: nil)
            raw = url; file = try FileHandle(forWritingTo: url)
            session?.storyTapeEvent = { [weak self] event in self?.receive(event) }
            try await session?.storyTape("update", arguments: ["value": ["version": row.id, "caption": row.utterance, "background": StoryCardStyle.cssBackground(code: session?.snapshot.code ?? "", version: row.id)]])
            guard operation == expected, active else { return }
            try await session?.storyTape("start", arguments: ["id": expected.uuidString])
            guard operation == expected, active else { return }
            previousIdleTimer = UIApplication.shared.isIdleTimerDisabled; UIApplication.shared.isIdleTimerDisabled = true
            recording = true; busy = true; stage = .rendering
            deadline = Task { [weak self] in
                try? await Task.sleep(for: .seconds(120))
                guard !Task.isCancelled, let self, self.operation == expected else { return }
                self.fail("This card took too long to export. Try again.")
            }
        } catch { if operation == expected { fail(error.localizedDescription) } }
    }
    func pause(_ paused: Bool) {
        guard recording else { return }
        let expected = operation
        Task {
            guard operation == expected else { return }
            try? await session?.storyTape(paused ? "pause" : "resume")
        }
    }
    func finishCard() async {
        guard recording else { assembleIfReady(); return }
        recording = false; stage = .finishingVideo
        let expected = operation
        await withCheckedContinuation { continuation in
            completion = continuation
            Task {
                guard operation == expected else { return }
                do { try await session?.storyTape("stop") }
                catch { if operation == expected { fail(error.localizedDescription) } }
            }
        }
    }
    func request() {
        DeviceActionLog.shared.record(.share, .started, control: .story)
        if let readyURL { movie = StoryMovie(url: readyURL); return }
        requested = true; active = true; busy = true; error = ""
        assembleIfReady()
    }
    func needsRestart(before index: Int) -> Bool {
        !recording && assembly == nil || rows.prefix(index).contains { cached($0) == nil }
    }
    func skipCard() { if raw != nil || recording { discardCard() } }
    func discardCard() {
        operation = UUID(); exportSession?.cancelExport(); exportSession = nil
        cleanupCard()
        let current = session, earlier = reset
        reset = Task { await earlier?.value; try? await current?.storyTape("cancel") }
    }
    func cancel() {
        if requested { DeviceActionLog.shared.record(.share, .cancelled, control: .story) }
        active = false; storyRun = UUID(); assembly?.cancel(); assembly = nil
        discardCard(); busy = false; requested = false
    }
    func clearMovie() { movie = nil }
    private func cleanupCard() {
        if let previousIdleTimer { UIApplication.shared.isIdleTimerDisabled = previousIdleTimer }; previousIdleTimer = nil
        deadline?.cancel(); deadline = nil
        try? file?.close(); file = nil
        if let raw { try? FileManager.default.removeItem(at: raw) }; raw = nil
        audio = nil; currentKey = nil; session?.storyTapeEvent = nil; recording = false
        completion?.resume(); completion = nil
    }
    private func fail(_ message: String) {
        DeviceActionLog.shared.record(.share, .failed, control: .story)
        let showError = requested
        cancel()
        // Automatic preparation must not interrupt browsing with a modal alert.
        if showError { error = message }
    }
    private func receive(_ event: [String: Any]) {
        guard active, event["session"] as? String == operation.uuidString else { return }
        do {
            switch event["kind"] as? String {
            case "chunk":
                guard let value = event["data"] as? String, value.count <= 300_000, let data = Data(base64Encoded: value) else { throw ExportError.invalidChunk }
                bytes += data.count; guard bytes <= 128_000_000 else { throw ExportError.tooLarge }
                try file?.write(contentsOf: data)
            case "done":
                try file?.close(); file = nil
                let expected = operation
                guard let raw, let key = currentKey else { throw ExportError.noVideo }
                let sound = audio
                Task {
                    do {
                        let output = try await compose([(raw, sound)], expected: expected, isCard: true)
                        defer { try? FileManager.default.removeItem(at: output) }
                        guard operation == expected, active else { return }
                        _ = try cache.store(output, key: key, protecting: Set(keys.values))
                        completedCards = rows.filter { cached($0) != nil }.count
                        cleanupCard(); stage = .preparing; assembleIfReady()
                    } catch { if operation == expected { fail(error.localizedDescription) } }
                }
            case "error": fail(event["error"] as? String ?? "Canvas recording failed.")
            default: break
            }
        } catch { fail(error.localizedDescription) }
    }
    private func assembleIfReady() {
        guard active, readyURL == nil, assembly == nil, !rows.isEmpty else { return }
        let clips = rows.compactMap { cached($0) }
        guard clips.count == rows.count else { return }
        let expected = operation, run = storyRun, key = storyKey
        stage = .encoding; progress = 0
        assembly = Task {
            defer { if storyRun == run { assembly = nil } }
            do {
                let output = try await compose(clips.map { ($0, nil) }, expected: expected, isCard: false)
                defer { try? FileManager.default.removeItem(at: output) }
                guard storyRun == run, active else { return }
                let cached = try cache.store(output, key: key)
                #if DEBUG
                if NativeScreenFixture.enabled && NativeScreenFixture.mode == "story" {
                    let evidence = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0].appendingPathComponent("story-export-test.mp4")
                    try? FileManager.default.removeItem(at: evidence); try FileManager.default.copyItem(at: cached, to: evidence)
                }
                #endif
                readyURL = cached; busy = false
                if requested { requested = false; movie = StoryMovie(url: cached) }
            } catch { if storyRun == run { fail(error.localizedDescription) } }
        }
    }
    private func check(_ expected: UUID) throws {
        guard active, operation == expected, !Task.isCancelled else { throw CancellationError() }
    }
    private func compose(_ clips: [(URL, StoryAudio?)], expected: UUID, isCard: Bool) async throws -> URL {
        try check(expected)
        let mix = AVMutableComposition()
        guard let video = mix.addMutableTrack(withMediaType: .video, preferredTrackID: kCMPersistentTrackID_Invalid) else { throw ExportError.noVideo }
        var cursor = CMTime.zero
        for (url, narration) in clips {
            let asset = AVURLAsset(url: url), length = try await asset.load(.duration)
            try check(expected)
            guard let track = try await asset.loadTracks(withMediaType: .video).first else { throw ExportError.noVideo }
            try video.insertTimeRange(CMTimeRange(start: .zero, duration: length), of: track, at: cursor)
            video.preferredTransform = try await track.load(.preferredTransform)
            let soundAsset = narration.map { AVURLAsset(url: $0.url) } ?? asset
            if let sound = try await soundAsset.loadTracks(withMediaType: .audio).first,
               let target = mix.addMutableTrack(withMediaType: .audio, preferredTrackID: kCMPersistentTrackID_Invalid) {
                let trim = narration?.start ?? 0
                let soundDuration = try await soundAsset.load(.duration).seconds
                let seconds = min(length.seconds, (narration?.end ?? soundDuration) - trim)
                if seconds > 0 { try target.insertTimeRange(CMTimeRange(start: CMTime(seconds: trim, preferredTimescale: 600), duration: CMTime(seconds: seconds, preferredTimescale: 600)), of: sound, at: cursor) }
            }
            cursor = cursor + length
            guard cursor.seconds <= 600 else { throw ExportError.tooLong }
        }
        try check(expected)
        guard let exporter = AVAssetExportSession(asset: mix, presetName: isCard ? AVAssetExportPresetHighestQuality : AVAssetExportPresetPassthrough) else { throw ExportError.noVideo }
        let output = FileManager.default.temporaryDirectory.appendingPathComponent("Whistlegraph-\(UUID()).mp4")
        exporter.outputURL = output; exporter.outputFileType = .mp4; exporter.shouldOptimizeForNetworkUse = true; exportSession = exporter
        stage = .encoding; progress = 0
        let monitor = Task { [weak self] in
            while !Task.isCancelled {
                guard let self, self.operation == expected, self.active else { return }
                self.progress = Double(exporter.progress)
                try? await Task.sleep(for: .milliseconds(150))
            }
        }
        defer { monitor.cancel() }
        await exporter.export()
        do {
            try check(expected); exportSession = nil
            guard exporter.status == .completed else { throw exporter.error ?? ExportError.noVideo }
            progress = 1; return output
        } catch { try? FileManager.default.removeItem(at: output); throw error }
    }
    private enum ExportError: LocalizedError {
        case noVideo, invalidChunk, tooLarge, tooLong
        var errorDescription: String? {
            switch self {
            case .noVideo: return "Could not create the MP4."
            case .invalidChunk: return "Invalid canvas tape data."
            case .tooLarge: return "This card is too large to export."
            case .tooLong: return "Export a story shorter than ten minutes."
            }
        }
    }
}

#if canImport(UIKit)
struct StoryShareSheet: UIViewControllerRepresentable {
    let url: URL
    func makeUIViewController(context: Context) -> UIActivityViewController { UIActivityViewController(activityItems: [url], applicationActivities: nil) }
    func updateUIViewController(_ controller: UIActivityViewController, context: Context) {}
}
#endif
