import SwiftUI
import AVFoundation

@MainActor final class VersionNarrator: NSObject, ObservableObject, AVSpeechSynthesizerDelegate, AVAudioPlayerDelegate {
    @Published var isPlaying = false
    @Published var utterance = ""
    @Published var currentWord = ""
    @Published var spokenRange: NSRange?
    private var words: [PlaybackWord] = []
    private var wordClock: Task<Void, Never>?
    @Published var error = ""
    private var recordingID: String?
    private var player: AVAudioPlayer?
    private var playbackEnd = Double.infinity
    private let voice = AVSpeechSynthesizer()
    private weak var session: WalkiewareSession?
    private var versions: [PieceRevision] = []
    private var index = 0
    private var waiting: Int?
    private var spoken: AVSpeechUtterance?
    private var run = UUID()
    private var timer: Task<Void, Never>?
    override init() { super.init(); voice.delegate = self }

    func play(_ session: WalkiewareSession) {
        stop()
        self.session = session
        versions = session.snapshot.versions.filter { $0.id > 0 }.sorted { $0.id < $1.id }
        guard !versions.isEmpty else { return }
        isPlaying = true; index = 0; show(versions[0], presentation: true)
    }
    func select(_ version: Int, session: WalkiewareSession) {
        stop(); self.session = session
        guard session.snapshot.versions.contains(where: { $0.id == version }) else { return }
        session.command("checkout", version: version)
    }
    private func show(_ row: PieceRevision, presentation: Bool) {
        wordClock?.cancel(); currentWord = ""; spokenRange = nil; words = row.words ?? []
        waiting = row.id; recordingID = row.recordingID; utterance = row.utterance; error = ""
        session?.command(presentation ? "presentVersion" : "checkout", version: row.id)
        let expected = run
        timer?.cancel()
        timer = Task { [weak self] in
            try? await Task.sleep(for: .seconds(10))
            guard !Task.isCancelled, let self, self.run == expected, self.waiting != nil else { return }
            self.stop(); self.error = "This version did not finish loading."
        }
    }
    func painted(_ version: Int?) {
        guard let version, waiting == version else { return }
        waiting = nil; timer?.cancel()
        do {
            let audio = AVAudioSession.sharedInstance()
            try audio.setCategory(.playback, mode: .default)
            try audio.setActive(true)
            if let recordingID, let url = UtteranceRecording.url(recordingID), FileManager.default.fileExists(atPath: url.path) {
                let original = try AVAudioPlayer(contentsOf: url)
                let trim = try RecordingTrim.read(url)
                original.currentTime = trim.start; playbackEnd = trim.end
                original.delegate = self; player = original
                if original.play() { followWords(original); return }
            }
        } catch { self.error = "Original recording unavailable; reading the transcript." }
        guard !utterance.isEmpty else { advanceAfterPause(); return }
        let line = AVSpeechUtterance(string: utterance)
        line.rate = AVSpeechUtteranceDefaultSpeechRate
        line.voice = AVSpeechSynthesisVoice(language: "en-US")
        spoken = line; voice.speak(line)
    }
    private func followWords(_ audio: AVAudioPlayer) {
        wordClock?.cancel()
        wordClock = Task { [weak self, weak audio] in
            while !Task.isCancelled {
                guard let self, let audio, self.player === audio, audio.isPlaying else { return }
                if audio.currentTime >= self.playbackEnd {
                    audio.stop(); self.player = nil; self.advanceAfterPause(); return
                }
                self.currentWord = PlaybackWord.at(audio.currentTime * 1000, in: self.words)
                self.spokenRange = PlaybackWord.range(at: audio.currentTime * 1000, in: self.words, text: self.utterance)
                try? await Task.sleep(for: .milliseconds(30))
            }
        }
    }
    nonisolated func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, willSpeakRangeOfSpeechString characterRange: NSRange, utterance: AVSpeechUtterance) {
        let text = (utterance.speechString as NSString).substring(with: characterRange)
        Task { @MainActor [weak self] in
            guard let self, self.spoken === utterance else { return }
            self.currentWord = text; self.spokenRange = characterRange
        }
    }
    nonisolated func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didFinish utterance: AVSpeechUtterance) {
        Task { @MainActor [weak self] in
            guard let self, self.spoken === utterance else { return }
            self.spoken = nil; self.advanceAfterPause()
        }
    }
    nonisolated func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        Task { @MainActor [weak self] in
            guard let self, self.player === player else { return }
            self.player = nil; self.advanceAfterPause()
        }
    }
    private func advanceAfterPause() {
        wordClock?.cancel(); currentWord = ""; spokenRange = nil
        guard isPlaying else { return }
        let expected = run
        timer = Task { [weak self] in
            try? await Task.sleep(for: .seconds(1))
            guard !Task.isCancelled, let self, self.run == expected, self.isPlaying else { return }
            self.index += 1
            if self.index < self.versions.count { self.show(self.versions[self.index], presentation: true) }
            else { self.stop() }
        }
    }
    func stop() {
        wordClock?.cancel(); wordClock = nil; currentWord = ""
        let wasPlaying = isPlaying
        run = UUID(); timer?.cancel(); timer = nil; waiting = nil; spoken = nil
        player?.stop(); player = nil; recordingID = nil
        voice.stopSpeaking(at: .immediate); isPlaying = false
        if wasPlaying { session?.command("endPresentation") }
    }
}
