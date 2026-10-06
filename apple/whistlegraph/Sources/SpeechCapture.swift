import Foundation
import AVFoundation
import Speech

// Hold-to-talk capture: the microphone feeds on-device speech recognition and
// the pitch/energy analyzer at once, and a release delivers one utterance.
// This type owns the audio session, the recognizer and the timing rules (the
// eight-second budget, the release tail, the finalize grace). It knows nothing
// about screens or web views: it reports through three closures, and the
// session maps those onto published state and the JavaScript bridge.
@MainActor final class SpeechCapture {
    /// listening · partial · sound · musicalObservation · processing · mixedFinal · error
    var hasVisualInput: () -> Bool = { false }
    var cloudSpeechEnabled: () -> Bool = { false }
    var onSpeechNotice: (String) -> Void = { _ in }
    var onSpeechCharge: () -> Void = {}
    var speechToken: () async throws -> String? = { nil }
    var onEvent: (_ kind: String, _ text: String, _ id: String) -> Void = { _, _, _ in }
    /// Latest microphone energy, for the live waveform.
    var onLevel: (_ rms: Double) -> Void = { _ in }
    /// The audio benchmark fixture finished replaying and wants the release.
    var onReplayRelease: () -> Void = {}

    /// The id of the hold in progress, or empty.
    private(set) var turn = ""

    private let engine = AVAudioEngine()
    private let recognizer = SFSpeechRecognizer(locale: Locale(identifier: "en-US"))
    private var request: SFSpeechAudioBufferRecognitionRequest?
    private var task: SFSpeechRecognitionTask?
    private var held = false
    private var maxDuration = 8.0
    func latchPerformance() {
        maxDuration = 45
        if listeningSince != nil { scheduleDeadline() }
    }
    private func scheduleDeadline() {
        deadline?.cancel()
        let id = turn, remaining = max(0, maxDuration - (listeningSince.map { Date().timeIntervalSince($0) } ?? 0))
        deadline = Task { [weak self] in
            try? await Task.sleep(for: .seconds(remaining))
            guard !Task.isCancelled, let self, self.turn == id else { return }
            self.stop()
        }
    }
    private var tapped = false
    private var listeningSince: Date?
    private var latest = ""
    private var musicalInput = MusicalInput()
    private var wordSegments: [[String: Any]] = []
    private var delivering = false
    private var recognitionFinal = false
    private var audioEnded = false
    private var replayTask: Task<Void, Never>?
    private var finishing: Task<Void, Never>?
    private var deadline: Task<Void, Never>?
    private var recognitionGraceExpired = false
    private var finalSound: [String: Any]?
    private var alignment: Task<Void, Never>?
    private var alignmentPending = false
    private var aligned: TimedTranscript?

    private func emit(_ kind: String, text: String = "", id: String? = nil) {
        if kind == "listening" { listeningSince = Date() }
        onEvent(kind, text, id ?? turn)
    }

    func start(_ id: String) {
        cancel()
        turn = id; held = true; latest = ""; wordSegments = []; delivering = false; recognitionFinal = false; audioEnded = false
        #if DEBUG
        if NativeScreenFixture.enabled && NativeScreenFixture.mode == "gestures" { emit("listening"); return }
        #endif
        musicalInput = MusicalInput()
        musicalInput.onUpdate = { [weak self] pitch, rms in
            Task { @MainActor in
                guard let self, self.turn == id, self.held else { return }
                self.onLevel(rms.isFinite ? max(0, rms) : 0)
                if let pitch, rms > 0.012 { self.emit("sound", text: "\(Int(pitch)) Hz") }
            }
        }
        musicalInput.onObservation = { [weak self] sound in
            Task { @MainActor in
                guard let self, self.turn == id, self.held else { return }
                let value: [String: Any] = ["transcript":self.latest,"words":self.wordSegments,"sound":sound]
                guard let data = try? JSONSerialization.data(withJSONObject:value), let json = String(data:data,encoding:.utf8) else { return }
                self.emit("musicalObservation", text:json)
            }
        }
        if AudioBenchmark.enabled { AudioBenchmark.reset(); AudioBenchmark.mark("holdStarted") }
        Task { [weak self] in
            guard let self else { return }
            let speech = await withCheckedContinuation { continuation in
                SFSpeechRecognizer.requestAuthorization { continuation.resume(returning: $0 == .authorized) }
            }
            guard self.turn == id, self.held else { return }
            guard speech || self.cloudSpeechEnabled() else { self.fail("Speech permission is off. Enable it in Settings."); return }
            let microphone = await AVCaptureDevice.requestAccess(for: .audio)
            guard self.turn == id, self.held else { return }
            guard microphone else { self.fail("Microphone permission is off."); return }
            let recognizer = self.recognizer
            let localSpeech = speech && recognizer?.isAvailable == true && recognizer?.supportsOnDeviceRecognition == true
            guard localSpeech || self.cloudSpeechEnabled() else {
                self.fail("On-device English speech is unavailable."); return
            }
            do {
                let session = AVAudioSession.sharedInstance()
                try session.setCategory(.playAndRecord, mode: .measurement, options: [.defaultToSpeaker, .allowBluetooth])
                try session.setActive(true)
                let request = SFSpeechAudioBufferRecognitionRequest()
                request.shouldReportPartialResults = true
                request.taskHint = .dictation
                request.requiresOnDeviceRecognition = true
                request.contextualStrings = ["Whistlegraph", "Aesthetic Computer", "garden", "flowers"]
                self.request = request
                if !AudioBenchmark.enabled {
                    let input = self.engine.inputNode
                    let format = input.outputFormat(forBus: 0)
                    guard format.sampleRate > 0, format.channelCount > 0 else {
                        self.fail("No microphone is available."); return
                    }
                    let sound = self.musicalInput
                    input.installTap(onBus: 0, bufferSize: 1024, format: format) { buffer, _ in request.append(buffer); sound.feed(buffer) }
                    self.tapped = true
                    self.engine.prepare()
                    try self.engine.start()
                }
                self.emit("listening")
                self.task = localSpeech ? recognizer?.recognitionTask(with: request) { [weak self] result, error in
                    let text = result?.bestTranscription.formattedString
                    let segments = result?.bestTranscription.segments.map { ["text": $0.substring, "atMs": $0.timestamp * 1000, "durationMs": $0.duration * 1000, "confidence": Double($0.confidence)] as [String: Any] }
                    let isFinal = result?.isFinal ?? false
                    let message = error?.localizedDescription
                    Task { @MainActor in
                        guard let self, self.turn == id else { return }
                        if let segments { self.wordSegments = Array(segments.prefix(256)) }
                        if let text { self.latest = text; self.emit("partial", text: text) }
                        if isFinal { self.recognitionFinal = true; AudioBenchmark.mark("recognitionFinal") }
                        if self.audioEnded && isFinal { self.deliver() }
                        else if self.audioEnded && message != nil && self.latest.isEmpty {
                            self.recognitionFinal = true
                            self.deliver() // Sound remains useful when speech finds no words.
                        }
                    }
                } : nil
                if AudioBenchmark.enabled {
                    let sound = self.musicalInput
                    self.replayTask = Task { [weak self] in
                        do {
                            try await AudioBenchmark.replay(into: request, sound: sound) { self?.onReplayRelease() }
                        } catch { if !Task.isCancelled { self?.fail("Audio fixture replay failed.") } }
                    }
                }
                self.scheduleDeadline()
            } catch { self.fail("Could not start the microphone. Please try again.") }
        }
    }

    private func stopAudio() {
        engine.stop()
        if tapped { engine.inputNode.removeTap(onBus: 0); tapped = false }
        request?.endAudio()
        PieceAudio.activate()
    }

    /// The thumb lifted. Ignored when `id` names a hold that is no longer this one.
    func stop(matching id: String? = nil) {
        if let id, id != turn { return }
        guard !turn.isEmpty, held else { return }
        held = false
        AudioBenchmark.mark("releaseReceived")
        guard request != nil else { cancel(); return }
        // Capture a release tail, within the eight-second recording budget. A
        // thumb lifts a beat before the last word lands; 0.25 s clipped endings.
        let elapsed = listeningSince.map { Date().timeIntervalSince($0) } ?? 8
        let tail = min(0.45, max(0, maxDuration - elapsed))
        emit("processing")
        finishing?.cancel()
        let id = turn
        finishing = Task { [weak self] in
            try? await Task.sleep(for: .seconds(tail))
            guard !Task.isCancelled, let self, self.turn == id else { return }
            self.stopAudio()
            self.audioEnded = true
            AudioBenchmark.mark("audioDrained")
            let sound = await withCheckedContinuation { continuation in self.musicalInput.finish { continuation.resume(returning: $0) } }
            guard !Task.isCancelled, self.turn == id else { return }
            self.finalSound = sound
            if !AudioBenchmark.enabled, self.cloudSpeechEnabled(), let recording = sound["recordingID"] as? String {
                self.alignmentPending = true
                self.alignment = Task { [weak self] in
                    guard let self else { return }
                    var result: TimedTranscript?
                    do { if let token = try await self.speechToken() { result = try await RecordedTranscription.recover(recording, token: token) } }
                    catch {
                        guard !Task.isCancelled, self.turn == id else { return }
                        DeviceActionLog.shared.recordError(.speech, error)
                        self.onSpeechNotice((error as? SpeechFailure)?.errorDescription ?? "Whisper is unavailable. Using device speech.")
                    }
                    guard !Task.isCancelled, self.turn == id else { return }
                    if result?.billing != nil { self.onSpeechCharge() }
                    self.aligned = result; self.alignmentPending = false; self.deliver()
                }
            }
            self.deliver()
            // endAudio lets recognition consume every queued buffer and finalize.
            // If it never does, the partial transcript is still the person's
            // words: send what was heard rather than throw the utterance away.
            try? await Task.sleep(for: .seconds(3))
            guard !Task.isCancelled, self.turn == id, !self.delivering else { return }
            self.recognitionGraceExpired = true
            AudioBenchmark.mark("recognitionTimedOut", fields: ["recognizedCharacters": self.latest.count])
            self.deliver()
        }
    }

    private func deliver() {
        guard !delivering, !turn.isEmpty, audioEnded, let sound = finalSound, !alignmentPending,
              aligned != nil || recognitionFinal || recognitionGraceExpired else { return }
        delivering = true
        let recovered = aligned?.transcript.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        let text = !recovered.isEmpty ? recovered : latest.trimmingCharacters(in: .whitespacesAndNewlines)
        // Stream arrival times are not word timestamps. Only use measured
        // alignment from the same transcript; never invent timing for deltas.
        let words = !recovered.isEmpty ? aligned!.timeline : (text == latest.trimmingCharacters(in: .whitespacesAndNewlines) ? wordSegments : [])
        let id = turn
        stopAudio()
        guard !text.isEmpty || (sound["audibleMs"] as? Double ?? 0) >= 150 || hasVisualInput() else {
            fail("I didn’t hear words or a sound. Hold to try again."); return
        }
        let value: [String: Any] = ["transcript":text,"words":words,"sound":sound]
        guard let data = try? JSONSerialization.data(withJSONObject:value), let json = String(data:data,encoding:.utf8) else { fail("Could not read sound input."); return }
        AudioBenchmark.checkTranscript(text)
        AudioBenchmark.mark("soundSubmitted", fields: value)
        cancel()
        emit("mixedFinal", text:json, id:id)
    }

    private func fail(_ message: String) {
        AudioBenchmark.mark("recognitionFailed", fields: ["message": message, "recognizedCharacters": latest.count])
        let id = turn
        cancel()
        emit("error", text: message, id: id)
    }

    func cancel() {
        alignment?.cancel(); alignment = nil
        finalSound = nil; aligned = nil; alignmentPending = false; recognitionGraceExpired = false
        turn = ""; held = false; listeningSince = nil; maxDuration = 8
        replayTask?.cancel(); replayTask = nil
        finishing?.cancel(); finishing = nil
        deadline?.cancel(); deadline = nil
        stopAudio()
        task?.cancel(); task = nil; request = nil
    }
}
