import SwiftUI
import AVFoundation
import Speech

/// Voice notes (Fía, 10-12): "send voice notes maybe with one minute limit …
/// and a transcription?" Recorded as AAC, stopped at 60 s, transcribed on the
/// phone by Apple's recognizer (on-device when the phone supports it), and sent
/// as an audio attachment carrying its duration and transcript.
@MainActor final class VoiceRecorder: NSObject, ObservableObject, AVAudioRecorderDelegate {
    static let limit: TimeInterval = 60
    @Published var recording = false
    @Published var elapsed: TimeInterval = 0
    @Published var working = false        // transcribing
    @Published var problem: String?
    private var recorder: AVAudioRecorder?
    private var tick: Timer?
    private var url: URL { FileManager.default.temporaryDirectory.appendingPathComponent("choir-voice.m4a") }
    var onFinished: ((Pending) -> Void)?

    func start() async {
        problem = nil
        guard await AVAudioApplication.requestRecordPermission() else { problem = "choir needs the microphone for voice notes."; return }
        do {
            let s = AVAudioSession.sharedInstance()
            try s.setCategory(.playAndRecord, mode: .spokenAudio, options: [.defaultToSpeaker, .allowBluetoothHFP])
            try s.setActive(true)
            try? FileManager.default.removeItem(at: url)
            let r = try AVAudioRecorder(url: url, settings: [
                AVFormatIDKey: kAudioFormatMPEG4AAC, AVSampleRateKey: 44100, AVNumberOfChannelsKey: 1, AVEncoderBitRateKey: 64000,
            ])
            r.delegate = self
            r.record(forDuration: Self.limit)
            recorder = r; recording = true; elapsed = 0
            Trail.send("voice:record")
            tick = Timer.scheduledTimer(withTimeInterval: 0.1, repeats: true) { [weak self] _ in
                Task { @MainActor in self?.elapsed = self?.recorder?.currentTime ?? 0 }
            }
        } catch { problem = "Couldn't start recording." }
    }

    func cancel() {
        tick?.invalidate(); recorder?.delegate = nil; recorder?.stop(); recorder?.deleteRecording()
        recorder = nil; recording = false; elapsed = 0
        Trail.send("voice:cancel")
    }

    func stop() { recorder?.stop() } // the delegate finishes up (also fires at the 60 s limit)

    nonisolated func audioRecorderDidFinishRecording(_ r: AVAudioRecorder, successfully flag: Bool) {
        Task { @MainActor in await self.finish(flag) }
    }

    private func finish(_ ok: Bool) async {
        tick?.invalidate(); recording = false
        let duration = recorder?.currentTime ?? elapsed
        recorder = nil
        try? AVAudioSession.sharedInstance().setActive(false, options: .notifyOthersOnDeactivation)
        guard ok, let data = try? Data(contentsOf: url), data.count > 2000 else { problem = "That recording didn't take."; return }
        let seconds = min(Self.limit, max(duration, (try? AVAudioPlayer(data: data).duration) ?? duration))
        working = true
        let text = await Self.transcribe(url)
        working = false
        Trail.send("voice:recorded", ["seconds": Int(seconds), "transcribed": text != nil])
        var p = Pending(data: data, name: "voice-\(Int(Date().timeIntervalSince1970)).m4a", type: "audio/mp4")
        p.duration = seconds; p.transcript = text
        onFinished?(p)
    }

    static func transcribe(_ url: URL) async -> String? {
        let status: SFSpeechRecognizerAuthorizationStatus = await withCheckedContinuation { c in
            SFSpeechRecognizer.requestAuthorization { c.resume(returning: $0) }
        }
        guard status == .authorized, let recognizer = SFSpeechRecognizer(), recognizer.isAvailable else { return nil }
        let request = SFSpeechURLRecognitionRequest(url: url)
        request.requiresOnDeviceRecognition = recognizer.supportsOnDeviceRecognition
        request.addsPunctuation = true
        request.shouldReportPartialResults = false
        return await withCheckedContinuation { c in
            var done = false
            recognizer.recognitionTask(with: request) { result, error in
                guard !done else { return }
                if let result, result.isFinal { done = true; c.resume(returning: result.bestTranscription.formattedString.isEmpty ? nil : result.bestTranscription.formattedString) }
                else if error != nil { done = true; c.resume(returning: nil) }
            }
        }
    }
}

/// The recording strip that replaces the text box while recording.
struct RecordingBar: View {
    @ObservedObject var recorder: VoiceRecorder
    var body: some View {
        HStack(spacing: 10) {
            Button { recorder.cancel() } label: { Hand("HandX", height: 15) }.buttonStyle(GlassChipStyle()).accessibilityLabel("Cancel recording")
            Circle().fill(Theme.head).frame(width: 10, height: 10)
                .opacity(Int(recorder.elapsed * 2) % 2 == 0 ? 1 : 0.35)
            Text(Self.clock(recorder.elapsed)).font(Theme.type(16, bold: true)).monospacedDigit()
            ProgressView(value: min(recorder.elapsed, VoiceRecorder.limit), total: VoiceRecorder.limit).tint(Theme.head)
            Text(Self.clock(VoiceRecorder.limit - recorder.elapsed)).font(Theme.type(12)).opacity(0.6).monospacedDigit()
            Button { recorder.stop() } label: { Image(systemName: "stop.fill").foregroundStyle(.white) }
                .buttonStyle(SendStyle(ready: true)).accessibilityLabel("Stop recording")
        }
        .foregroundStyle(Theme.ink)
        .padding(.horizontal, 12).padding(.vertical, 8)
    }
    static func clock(_ t: TimeInterval) -> String { let s = max(0, Int(t.rounded(.down))); return String(format: "%d:%02d", s / 60, s % 60) }
}

/// A voice note in a message: play/pause, progress, length, and its transcript.
struct VoiceNoteView: View {
    let note: Attachment
    @State private var player: AVPlayer?
    @State private var playing = false
    @State private var position: Double = 0
    @State private var observer: Any?

    var length: Double { note.duration ?? 0 }
    var body: some View {
        VStack(alignment: .leading, spacing: 6) {
            HStack(spacing: 10) {
                Button { toggle() } label: {
                    Image(systemName: playing ? "pause.fill" : "play.fill").font(.system(size: 15, weight: .bold)).foregroundStyle(.white)
                        .frame(width: 34, height: 34).background(Theme.ink, in: Circle())
                }
                .buttonStyle(.plain).accessibilityLabel(playing ? "Pause voice note" : "Play voice note")
                ProgressView(value: min(position, max(length, 0.01)), total: max(length, 0.01)).tint(Theme.head)
                Text(RecordingBar.clock(playing || position > 0 ? position : length)).font(Theme.type(12)).monospacedDigit().foregroundStyle(Theme.ink.opacity(0.7))
            }
            .frame(width: 220)
            if let t = note.transcript, !t.isEmpty {
                Text(t).font(.system(size: 14, design: .serif)).italic().foregroundStyle(Theme.ink.opacity(0.8))
                    .frame(maxWidth: 240, alignment: .leading).textSelection(.enabled)
            }
        }
        .onDisappear { player?.pause(); if let observer { player?.removeTimeObserver(observer) } }
    }

    func toggle() {
        if player == nil, let u = URL(string: note.url) {
            try? AVAudioSession.sharedInstance().setCategory(.playback, mode: .spokenAudio)
            let p = AVPlayer(url: u); p.actionAtItemEnd = .pause; player = p
            observer = p.addPeriodicTimeObserver(forInterval: CMTime(seconds: 0.2, preferredTimescale: 600), queue: .main) { t in
                if playing { position = t.seconds }
            }
            // Play once: at the end, stop and rewind (seeking alone kept it playing — a loop).
            NotificationCenter.default.addObserver(forName: AVPlayerItem.didPlayToEndTimeNotification, object: p.currentItem, queue: .main) { _ in
                Task { @MainActor in p.pause(); p.seek(to: .zero); playing = false; position = 0 }
            }
        }
        if playing { player?.pause() } else { player?.play(); Trail.send("voice:play") }
        playing.toggle()
    }
}
