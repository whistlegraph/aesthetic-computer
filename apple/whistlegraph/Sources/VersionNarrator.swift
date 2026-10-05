import SwiftUI
import AVFoundation

@MainActor final class VersionNarrator: NSObject, ObservableObject, AVAudioPlayerDelegate {
    @Published var isPlaying = false
    @Published var isPaused = false
    @Published var utterance = ""
    @Published var currentWord = ""
    @Published var spokenRange: NSRange?
    @Published var error = ""
    @Published private(set) var index = 0
    @Published private(set) var count = 0
    @Published private(set) var progress = 0.0
    var onComplete: (() -> Void)?
    var onNarration: ((PieceRevision, StoryAudio?) async -> Void)?
    var onCardComplete: (() async -> Void)?
    var onSkip: (() -> Void)?
    var onPause: ((Bool) -> Void)?
    var shouldPlay: ((Int) -> Bool)?
    var currentVersion: Int? { versions.indices.contains(index) ? versions[index].id : nil }
    var branch: [PieceRevision] { versions }
    private var words: [PlaybackWord] = []
    private var clock: Task<Void, Never>?
    private var recordingID: String?
    private var player: AVAudioPlayer?
    private var playbackEnd = Double.infinity
    private var playbackStart = 0.0
    private var elapsed = 0.0
    private var duration = 4.0
    private var tail: Double?
    private weak var session: WhistlegraphSession?
    private var versions: [PieceRevision] = []
    private var waiting: Int?
    private var timer: Task<Void, Never>?
    private var run = UUID()
    private var completed = false
    override init() { super.init() }

    func play(_ session: WhistlegraphSession) {
        stop()
        self.session = session
        // Follow the selected branch; discarded alternatives are not story beats.
        let rows = Dictionary(uniqueKeysWithValues: session.snapshot.versions.map { ($0.id, $0) })
        var chain: [PieceRevision] = [], visited = Set<Int>(), cursor: Int? = session.snapshot.head
        while let id = cursor, let row = rows[id], visited.insert(id).inserted {
            if row.id > 0 { chain.append(row) }; cursor = row.parent
        }
        versions = chain.reversed(); count = versions.count
        guard !versions.isEmpty else { return }
        isPlaying = true; index = 0; show()
    }
    func select(_ version: Int, session: WhistlegraphSession) {
        stop(); self.session = session
        guard session.snapshot.versions.contains(where: { $0.id == version }) else { return }
        session.command("checkout", version: version)
    }
    func next() {
        guard isPlaying else { return }
        onSkip?()
        if index + 1 < count { index += 1; show() }
        else { setPaused(true); progress = 1 }
    }
    func previous() { guard isPlaying else { return }; onSkip?(); index = max(0, index - 1); show() }
    func restart() { guard isPlaying else { return }; onSkip?(); index = 0; isPaused = false; show() }
    func jump(to target: Int) { guard isPlaying, versions.indices.contains(target) else { return }; onSkip?(); index = target; isPaused = false; show() }
    func setPaused(_ paused: Bool) {
        guard isPlaying else { return }
        isPaused = paused
        onPause?(paused)
        if paused { player?.pause() }
        else if completed { restart() }
        else { player?.play() }
    }
    private func clearPlayback() {
        clock?.cancel(); timer?.cancel(); run = UUID(); waiting = nil
        player?.stop(); player = nil; currentWord = ""; spokenRange = nil
    }
    private func show() {
        clearPlayback(); completed = false; progress = 0; elapsed = 0; tail = nil
        let row = versions[index]
        words = row.words ?? []; waiting = row.id; recordingID = row.recordingID
        utterance = row.utterance; error = ""
        session?.command("presentVersion", version: row.id)
        let expected = run
        timer = Task { [weak self] in
            try? await Task.sleep(for: .seconds(30))
            guard !Task.isCancelled, let self, self.run == expected, self.waiting != nil else { return }
            self.stop(); self.error = "This version did not finish loading."
        }
    }
    func painted(_ version: Int?) {
        guard let version, waiting == version else { return }
        waiting = nil; timer?.cancel()
        let expected = run, row = versions[index]
        Task {
            let sound: StoryAudio?
            do { sound = try await StoryVoice.audio(for: row) }
            catch {
                guard run == expected, isPlaying else { return }
                stop(); self.error = "Could not prepare narration. Try again when connected."; return
            }
            guard run == expected, isPlaying else { return }
            await onNarration?(row, sound)
            guard run == expected, isPlaying else { return }
            startNarration(sound)
            if isPaused { onPause?(true) }
        }
    }
    private func startNarration(_ sound: StoryAudio?) {
        duration = 4
        do {
            let audio = AVAudioSession.sharedInstance()
            try audio.setCategory(.playback, mode: .default); try audio.setActive(true)
            if let sound {
                let recording = try AVAudioPlayer(contentsOf: sound.url)
                recording.currentTime = sound.start; playbackStart = sound.start; playbackEnd = sound.end
                duration = max(4, sound.end - sound.start + 1.5)
                if !sound.original { words = [] }
                recording.delegate = self; player = recording; recording.prepareToPlay()
                if !isPaused { recording.play() }
            } else { tail = 4 }
        } catch { stop(); self.error = "Could not play narration."; return }
        let expected = run
        clock = Task { [weak self] in
            var last = Date()
            while !Task.isCancelled {
                try? await Task.sleep(for: .milliseconds(33))
                guard !Task.isCancelled, let self, self.run == expected, self.isPlaying else { return }
                let now = Date(), delta = min(0.1, now.timeIntervalSince(last)); last = now
                guard !self.isPaused else { continue }
                self.elapsed += delta
                if let audio = self.player {
                    self.currentWord = PlaybackWord.at(audio.currentTime * 1000, in: self.words)
                    self.spokenRange = PlaybackWord.range(at: audio.currentTime * 1000, in: self.words, text: self.utterance)
                    if audio.currentTime >= self.playbackEnd { audio.stop(); self.player = nil; self.narrationFinished() }
                }
                if let remaining = self.tail {
                    self.tail = remaining - delta
                    if remaining <= 0 { self.finishCard(); return }
                }
                self.progress = min(0.98, self.elapsed / self.duration)
            }
        }
    }
    private func narrationFinished() { currentWord = ""; spokenRange = nil; tail = max(1.5, 4 - elapsed); duration = elapsed + (tail ?? 1.5) }
    private func finishCard() {
        let expected = run
        Task {
            await onCardComplete?()
            guard run == expected, isPlaying else { return }
            if let next = ((index + 1)..<count).first(where: { shouldPlay?($0) ?? true }) { index = next; show() }
            else { completed = true; isPaused = true; progress = 1; onComplete?() }
        }
    }
    nonisolated func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        Task { @MainActor [weak self] in
            guard let self, self.player === player else { return }; self.player = nil; self.narrationFinished()
        }
    }
    func stop() {
        let wasPlaying = isPlaying
        clearPlayback(); isPlaying = false; isPaused = false; recordingID = nil
        if wasPlaying { session?.command("endPresentation") }
    }
}
