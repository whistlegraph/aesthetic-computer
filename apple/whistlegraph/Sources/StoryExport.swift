import SwiftUI
import AVFoundation
import WebKit

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
        active = true; busy = readyURL == nil; stage = .preparing; error = ""; failures = [:]
        posters = [:]; for row in rows { _ = poster(for: row.id) }
        if readyURL == nil { recordMissing() }
    }
    private func cached(_ row: PieceRevision) -> URL? { keys[row.id].flatMap { cache.find($0) } }
    var firstMissingIndex: Int? { rows.firstIndex { cached($0) == nil } }

    // ---- Off-screen recording (the visible story is never touched) ----
    private var recorder: Task<Void, Never>?
    private var recorderPreview: StoryPreview?
    // ---- Posters: a still of each card, shown the instant it is selected ----
    @Published private(set) var posters: [Int: UIImage] = [:]
    func poster(for version: Int?) -> UIImage? {
        guard let version else { return nil }
        if let image = posters[version] { return image }
        guard let key = keys[version], let url = cache.find(key, ext: "png"), let image = UIImage(contentsOfFile: url.path) else { return nil }
        posters[version] = image; return image
    }
    /// Keep a still of a card from whichever runtime just painted it.
    func capturePoster(for version: Int?, from view: WKWebView) {
        guard let version, posters[version] == nil, let key = keys[version], cache.find(key, ext: "png") == nil, view.bounds.width > 0 else { return }
        let config = WKSnapshotConfiguration(); config.afterScreenUpdates = true
        view.takeSnapshot(with: config) { [weak self] image, _ in
            guard let self, let image, let data = image.pngData() else { return }
            self.posters[version] = image
            let staging = FileManager.default.temporaryDirectory.appendingPathComponent("story-poster-\(UUID()).png")
            try? data.write(to: staging); defer { try? FileManager.default.removeItem(at: staging) }
            _ = try? self.cache.store(staging, key: key, ext: "png", protecting: Set(self.keys.values))
        }
    }
    @Published private(set) var recorderView: WKWebView?
    @Published private(set) var failures: [Int: String] = [:]
    private func preview(_ session: WhistlegraphSession) -> StoryPreview {
        if let recorderPreview { return recorderPreview }
        let preview = StoryPreview(session: session, standalone: true)
        recorderPreview = preview; recorderView = preview.view
        return preview
    }
    /// Every card without a clip, in order, each with one retry. Browsing the
    /// story meanwhile is free; a card that fails twice is reported, never
    /// re-rolled on its own, and tried again on the next Export tap.
    private func recordMissing() {
        guard recorder == nil, session != nil, rows.contains(where: { cached($0) == nil }) else { assembleIfReady(); return }
        let run = storyRun
        busy = true; stage = .preparing
        recorder = Task { [weak self] in
            guard let self else { return }
            defer { if self.storyRun == run { self.recorder = nil } }
            for row in self.rows where self.cached(row) == nil {
                guard self.active, self.storyRun == run else { return }
                for attempt in 1...2 {
                    do { try await self.record(row); self.failures[row.id] = nil; break }
                    catch is CancellationError { return }
                    catch {
                        self.discardCard()
                        guard self.active, self.storyRun == run else { return }
                        if attempt == 2 { self.failures[row.id] = error.localizedDescription; DeviceActionLog.shared.record(.share, .failed, control: .story) }
                    }
                }
                self.completedCards = self.rows.filter { self.cached($0) != nil }.count
            }
            guard self.active, self.storyRun == run else { return }
            if self.rows.contains(where: { self.cached($0) == nil }) {
                self.busy = false
                if self.requested { self.requested = false; self.error = self.failures.values.first ?? "A card could not be recorded. Try again." }
            } else { self.assembleIfReady() }
        }
    }
    enum RecordError: LocalizedError {
        case notPainted, noClip
        var errorDescription: String? { self == .notPainted ? "This card did not paint." : "This card did not record." }
    }
    /// One card: present it in the off-screen runtime, wait for its paint, fetch its
    /// narration, roll tape for the narration plus the story's tail, stop, compose, cache.
    private func record(_ row: PieceRevision) async throws {
        guard let session else { throw RecordError.noClip }
        let preview = preview(session)
        let source = try await session.versionSource(row.id)
        let painted = await withCheckedContinuation { (continuation: CheckedContinuation<Bool, Never>) in
            var settled = false
            let timeout = Task { try? await Task.sleep(for: .seconds(30)); if !settled { settled = true; preview.onPainted = nil; continuation.resume(returning: false) } }
            preview.onPainted = { version in guard version == row.id, !settled else { return }; settled = true; timeout.cancel(); preview.onPainted = nil; continuation.resume(returning: true) }
            preview.present(version: row.id, source: source)
        }
        guard painted else { throw RecordError.notPainted }
        capturePoster(for: row.id, from: preview.view)
        let sound = try await StoryVoice.audio(for: row)
        if let sound, sound.url.lastPathComponent.hasPrefix("story-voice-"), let key = keys[row.id] {
            keys[row.id] = StoryCache.key([key, "device-fallback"])
            storyKey = StoryCache.key(["story-movie-v2"] + rows.compactMap { keys[$0.id] })
            if cached(row) != nil { return }
        }
        let expected = UUID(); operation = expected
        currentKey = keys[row.id]; audio = sound; bytes = 0
        let url = FileManager.default.temporaryDirectory.appendingPathComponent("story-canvas-\(expected).mp4")
        FileManager.default.createFile(atPath: url.path, contents: nil)
        raw = url; file = try FileHandle(forWritingTo: url)
        preview.onTape = { [weak self] event in self?.receive(event) }
        try await preview.tape("update", arguments: ["value": ["version": row.id, "caption": row.utterance, "background": StoryCardStyle.cssBackground(code: session.snapshot.code, version: row.id)]])
        try check(expected)
        try await preview.tape("start", arguments: ["id": expected.uuidString])
        recording = true; stage = .rendering
        // As the narrator times it: the narration, then a tail so short cards still hold four seconds.
        let narration = sound.map { max(0.5, $0.end - $0.start) } ?? 0
        try await Task.sleep(for: .seconds(narration + max(1.5, 4 - narration)))
        try check(expected)
        recording = false; stage = .finishingVideo
        await withCheckedContinuation { (continuation: CheckedContinuation<Void, Never>) in
            completion = continuation
            Task { do { try await preview.tape("stop", arguments: [:]) } catch { if self.operation == expected { self.fail(error.localizedDescription) } } }
        }
        guard cached(row) != nil else { throw RecordError.noClip }
    }
    func stop() { recorder?.cancel(); recorder = nil }
    func request() {
        DeviceActionLog.shared.record(.share, .started, control: .story)
        if let readyURL { movie = StoryMovie(url: readyURL); return }
        requested = true; active = true; busy = true; error = ""
        recordMissing()
    }
    func discardCard() {
        operation = UUID(); exportSession?.cancelExport(); exportSession = nil
        cleanupCard()
        let current = recorderPreview, earlier = reset
        reset = Task { await earlier?.value; try? await current?.tape("cancel", arguments: [:]) }
    }
    func cancel() {
        if requested { DeviceActionLog.shared.record(.share, .cancelled, control: .story) }
        active = false; storyRun = UUID(); assembly?.cancel(); assembly = nil
        recorder?.cancel(); recorder = nil
        discardCard(); busy = false; requested = false
    }
    func clearMovie() { movie = nil }
    private func cleanupCard() {
        if let previousIdleTimer { UIApplication.shared.isIdleTimerDisabled = previousIdleTimer }; previousIdleTimer = nil
        deadline?.cancel(); deadline = nil
        try? file?.close(); file = nil
        if let raw { try? FileManager.default.removeItem(at: raw) }; raw = nil
        audio = nil; currentKey = nil; recorderPreview?.onTape = nil; recording = false
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
