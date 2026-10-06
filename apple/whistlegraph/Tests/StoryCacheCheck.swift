// Headless checks use real AVFoundation composition with a deterministic MP4;
// only the WebView transport and the phone's idle timer are substituted.
import Foundation
import AVFoundation

@MainActor final class UIApplication {
    static let shared = UIApplication()
    var isIdleTimerDisabled = false
}
struct PieceRevision {
    let id: Int
    let createdAt: String
    let utterance: String
    let recordingID: String? = nil
}
struct StoryAudio { let url: URL; let start: Double; let end: Double }
@MainActor final class WhistlegraphSession {
    struct Snapshot { let code = "cache-test" }
    let snapshot = Snapshot()
    var pixelSize = 2
    var storyTapeEvent: (([String: Any]) -> Void)?
    var session = ""
    var starts = 0
    let fixture: Data
    init(fixture: Data) { self.fixture = fixture }
    func storyTape(_ action: String, arguments: [String: Any] = [:]) async throws {
        if action == "start" { session = arguments["id"] as! String; starts += 1 }
        if action == "stop" {
            for offset in stride(from: 0, to: fixture.count, by: 192_000) {
                storyTapeEvent?(["session": session, "kind": "chunk", "data": fixture.subdata(in: offset..<min(offset + 192_000, fixture.count)).base64EncodedString()])
            }
            storyTapeEvent?(["session": session, "kind": "done"])
        }
    }
}
@main struct StoryCacheCheck {
    @MainActor static func main() async throws {
        let root = FileManager.default.temporaryDirectory.appendingPathComponent("story-cache-check-" + UUID().uuidString)
        defer { try? FileManager.default.removeItem(at: root) }
        let cache = StoryCache(root: root)
        let fixture = try Data(contentsOf: URL(fileURLWithPath: CommandLine.arguments[1]))
        let session = WhistlegraphSession(fixture: fixture)
        let rows = [PieceRevision(id: 1, createdAt: "one", utterance: "first"), PieceRevision(id: 3, createdAt: "three", utterance: "second")]
        let first = StoryExport(cache: cache)
        first.prepare(session: session, rows: rows)
        assert(!first.requested && first.readyURL == nil)
        await first.startCard(rows[0], audio: nil); await first.finishCard()
        assert(first.completedCards == 1 && first.readyURL == nil)
        first.cancel()
        // A new coordinator (like relaunch) keeps the completed first card.
        let reopened = StoryExport(cache: cache)
        reopened.prepare(session: session, rows: rows)
        assert(reopened.completedCards == 1)
        await reopened.startCard(rows[0], audio: nil)
        assert(session.starts == 1, "A cached card must not start the encoder")
        await reopened.startCard(rows[1], audio: nil); await reopened.finishCard()
        for _ in 0..<300 where reopened.readyURL == nil { try await Task.sleep(for: .milliseconds(50)) }
        guard let url = reopened.readyURL else { fatalError("Assembly failed: \(reopened.error)") }
        assert(reopened.movie == nil && !reopened.busy, "Automatic export cannot open a modal")
        let duration = try await AVURLAsset(url: url).load(.duration).seconds
        let clipDuration = try await AVURLAsset(url: URL(fileURLWithPath: CommandLine.arguments[1])).load(.duration).seconds
        assert(abs(duration - clipDuration * 2) < 0.1)
        reopened.request(); assert(reopened.movie?.url == url)
        reopened.cancel()
        let warm = StoryExport(cache: cache)
        warm.prepare(session: session, rows: rows)
        assert(warm.readyURL == url && !warm.busy)
        warm.request(); assert(warm.movie?.url == url && session.starts == 2)
        warm.cancel(); session.pixelSize = 4
        warm.prepare(session: session, rows: rows)
        assert(warm.readyURL == nil && warm.completedCards == 0, "Density changes invalidate clips")
        assert(warm.firstMissingIndex == 0 && warm.needsCard(at: 1))
        await warm.startCard(rows[0], audio: nil); warm.cancel()
        assert(warm.completedCards == 0 && !warm.recording)
        session.pixelSize = 2
        let appended = rows + [PieceRevision(id: 4, createdAt: "four", utterance: "third")]
        warm.prepare(session: session, rows: appended)
        assert(warm.completedCards == 2 && warm.firstMissingIndex == 2)
        assert(!warm.needsCard(at: 0) && warm.needsCard(at: 2), "Only the new card needs rendering")
        warm.cancel()
        // Protected clips survive bounded-cache eviction; abandoned partials
        // never become hits and the filename hash includes field boundaries.
        assert(StoryCache.key(["ab", "c"]) != StoryCache.key(["a", "bc"]))
        let tiny = StoryCache(name: "tiny", limit: fixture.count + 1, root: root)
        let file = URL(fileURLWithPath: CommandLine.arguments[1])
        _ = try tiny.store(file, key: "old"); _ = try tiny.store(file, key: "new")
        assert(tiny.find("old") == nil && tiny.find("new") != nil)
        print("PASS: incremental clips, relaunch reuse, full MP4 duration, instant share, density invalidation, cancellation, bounded cache")
    }
}
