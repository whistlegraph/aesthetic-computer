import AppKit
import AVFoundation

final class WizardController: NSWindowController, NSWindowDelegate, AVAudioPlayerDelegate {
    let spec: Spec
    var currentIndex: Int = 0
    var doneIndexes: Set<Int> = []
    var skippedIndexes: Set<Int> = []

    var recorder: AVAudioRecorder?
    var player: AVAudioPlayer?
    let synth = AVSpeechSynthesizer()
    private let speechDelegate = SpeechDelegateProxy()

    var levelTimer: Timer?
    var soundDetected = false
    var silenceFrames = 0
    var maxFrames = 0
    var elapsedFrames = 0

    let onsetDb: Double
    let silenceDb: Double
    let silenceMs: Double
    let maxRecordSec: Double
    let trimSpec: TrimSpec?

    // Sequencing guard — every transition bumps this so stale async
    // callbacks (delayed dispatch, audio player finish) can no-op.
    var sequenceToken: Int = 0
    var hasGreeted: Bool = false

    // UI
    var mascot: MascotView!
    var timeline: TimelineView!
    var progressLabel: NSTextField!
    var nameLabel: NSTextField!
    var descLabel: NSTextField!
    var statusLabel: NSTextField!
    var levelMeter: NSLevelIndicator!
    var recordButton: NSButton!
    var stopButton: NSButton!
    var keepButton: NSButton!
    var retryButton: NSButton!
    var skipButton: NSButton!

    init(spec: Spec) {
        self.spec = spec
        self.onsetDb = spec.autoStop?.onsetDb ?? -22
        self.silenceDb = spec.autoStop?.silenceDb ?? -38
        self.silenceMs = spec.autoStop?.silenceDurationMs ?? 1200
        self.maxRecordSec = spec.autoStop?.maxRecordSec ?? 12
        self.trimSpec = spec.trim
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 760, height: 560),
            styleMask: [.titled, .closable, .miniaturizable],
            backing: .buffered, defer: false
        )
        window.title = "WaveWizard — \(spec.title)"
        window.center()
        super.init(window: window)
        window.delegate = self
        setupUI()
        speechDelegate.onFinish = { [weak self] token in
            DispatchQueue.main.async { self?.afterSpeak(token: token) }
        }
        synth.delegate = speechDelegate
        beginCurrent()
    }

    required init?(coder: NSCoder) { fatalError() }

    // ── UI ──────────────────────────────────────────────────────────────
    func setupUI() {
        guard let cv = window?.contentView else { return }
        cv.wantsLayer = true

        let W: CGFloat = 760
        let H: CGFloat = 560
        let pad: CGFloat = 24

        progressLabel = NSTextField(labelWithString: "")
        progressLabel.frame = NSRect(x: pad, y: H - 36, width: W - 2*pad, height: 18)
        progressLabel.font = .systemFont(ofSize: 12, weight: .medium)
        progressLabel.textColor = .secondaryLabelColor
        cv.addSubview(progressLabel)

        timeline = TimelineView(frame: NSRect(x: pad, y: H - 100, width: W - 2*pad, height: 56))
        cv.addSubview(timeline)
        timeline.update(names: spec.samples.map { $0.name }, currentIndex: 0, done: [], skipped: [])

        // mascot on left
        mascot = MascotView(frame: NSRect(x: pad, y: 140, width: 200, height: 260))
        cv.addSubview(mascot)

        // info column on right
        let infoX = pad + 220
        let infoW = W - infoX - pad
        nameLabel = NSTextField(labelWithString: "")
        nameLabel.frame = NSRect(x: infoX, y: 370, width: infoW, height: 30)
        nameLabel.font = .monospacedSystemFont(ofSize: 20, weight: .semibold)
        cv.addSubview(nameLabel)

        descLabel = NSTextField(labelWithString: "")
        descLabel.frame = NSRect(x: infoX, y: 335, width: infoW, height: 26)
        descLabel.font = .systemFont(ofSize: 15)
        descLabel.textColor = .labelColor
        cv.addSubview(descLabel)

        statusLabel = NSTextField(labelWithString: "")
        statusLabel.frame = NSRect(x: infoX, y: 295, width: infoW, height: 22)
        statusLabel.font = .systemFont(ofSize: 13)
        statusLabel.textColor = .secondaryLabelColor
        cv.addSubview(statusLabel)

        levelMeter = NSLevelIndicator(frame: NSRect(x: infoX, y: 245, width: infoW, height: 28))
        levelMeter.levelIndicatorStyle = .continuousCapacity
        levelMeter.minValue = 0; levelMeter.maxValue = 100
        levelMeter.warningValue = 75; levelMeter.criticalValue = 95
        cv.addSubview(levelMeter)

        // big red Record button (custom-drawn)
        recordButton = RedRecordButton(frame: NSRect(x: infoX, y: 150, width: infoW, height: 72))
        recordButton.target = self
        recordButton.action = #selector(recordAction)
        recordButton.title = "● Record"
        cv.addSubview(recordButton)

        stopButton = NSButton(title: "Stop", target: self, action: #selector(stopAction))
        stopButton.bezelStyle = .rounded
        stopButton.frame = NSRect(x: infoX, y: 150, width: infoW, height: 72)
        stopButton.font = .systemFont(ofSize: 18, weight: .semibold)
        cv.addSubview(stopButton)

        // decision row
        let btnY: CGFloat = 30
        let btnW: CGFloat = 140
        let btnH: CGFloat = 44
        let gap: CGFloat = 14

        keepButton = NSButton(title: "✓ Keep", target: self, action: #selector(keepAction))
        keepButton.bezelStyle = .rounded
        keepButton.keyEquivalent = "\r"
        keepButton.frame = NSRect(x: pad, y: btnY, width: btnW, height: btnH)
        keepButton.font = .systemFont(ofSize: 15, weight: .medium)
        cv.addSubview(keepButton)

        retryButton = NSButton(title: "↺ Retry", target: self, action: #selector(retryAction))
        retryButton.bezelStyle = .rounded
        retryButton.frame = NSRect(x: pad + btnW + gap, y: btnY, width: btnW, height: btnH)
        retryButton.font = .systemFont(ofSize: 15)
        cv.addSubview(retryButton)

        skipButton = NSButton(title: "→ Skip", target: self, action: #selector(skipAction))
        skipButton.bezelStyle = .rounded
        skipButton.frame = NSRect(x: pad + 2 * (btnW + gap), y: btnY, width: btnW, height: btnH)
        skipButton.font = .systemFont(ofSize: 15)
        cv.addSubview(skipButton)

        setMode(.idle)
    }

    // ── state ──────────────────────────────────────────────────────────
    enum Mode { case idle, speaking, ready, recording, trimming, playback, decision, done }
    func setMode(_ m: Mode) {
        switch m {
        case .idle:
            recordButton.isHidden = true; recordButton.isEnabled = false
            stopButton.isHidden = true
            hideDecision()
        case .speaking:
            recordButton.isHidden = true; recordButton.isEnabled = false
            stopButton.isHidden = true
            hideDecision()
            mascot.mood = .idle
        case .ready:
            recordButton.isHidden = false; recordButton.isEnabled = true
            stopButton.isHidden = true
            hideDecision()
            mascot.mood = .listening
        case .recording:
            recordButton.isHidden = true; recordButton.isEnabled = false
            stopButton.isHidden = false
            hideDecision()
            mascot.mood = .recording
        case .trimming:
            recordButton.isHidden = true
            stopButton.isHidden = true
            hideDecision()
            mascot.mood = .idle
        case .playback:
            recordButton.isHidden = true
            stopButton.isHidden = true
            hideDecision()
            mascot.mood = .success
        case .decision:
            recordButton.isHidden = true
            stopButton.isHidden = true
            showDecision()
            mascot.mood = .success
        case .done:
            recordButton.isHidden = true
            stopButton.isHidden = true
            hideDecision()
            mascot.mood = .success
        }
    }

    func hideDecision() { keepButton.isHidden = true; retryButton.isHidden = true; skipButton.isHidden = true }
    func showDecision() { keepButton.isHidden = false; retryButton.isHidden = false; skipButton.isHidden = false }

    // ── per-sample flow ─────────────────────────────────────────────────
    func beginCurrent() {
        // cancel anything in-flight, bump the sequence token
        sequenceToken += 1
        let token = sequenceToken
        synth.stopSpeaking(at: .immediate)
        levelTimer?.invalidate(); levelTimer = nil
        recorder?.stop(); recorder = nil
        player?.stop(); player = nil
        levelMeter.integerValue = 0

        guard currentIndex < spec.samples.count else { finish(); return }
        let s = spec.samples[currentIndex]
        timeline.update(names: spec.samples.map { $0.name }, currentIndex: currentIndex, done: doneIndexes, skipped: skippedIndexes)
        progressLabel.stringValue = "Sample \(currentIndex + 1) of \(spec.samples.count)"
        nameLabel.stringValue = s.name
        descLabel.stringValue = s.desc
        statusLabel.stringValue = "wizard is speaking…"
        setMode(.speaking)
        speechDelegate.token = token

        if !hasGreeted {
            hasGreeted = true
            // The wizard's opening line — sets the tone.
            let greet = AVSpeechUtterance(string: "I need your human spirit!")
            greet.rate = 0.48
            greet.pitchMultiplier = 1.05
            synth.speak(greet)
            // Then a small pause + the sample description (in the queue)
            let utt = AVSpeechUtterance(string: s.desc)
            utt.rate = 0.5
            utt.preUtteranceDelay = 0.6
            synth.speak(utt)
        } else {
            let utt = AVSpeechUtterance(string: s.desc)
            utt.rate = 0.5
            synth.speak(utt)
        }
    }

    func afterSpeak(token: Int) {
        guard token == sequenceToken else { return }
        statusLabel.stringValue = "ready — press the red button when you're set"
        setMode(.ready)
    }

    // ── recording (manual start, auto-stop on silence) ──────────────────
    @objc func recordAction() {
        let token = sequenceToken
        let outPath = "\(spec.outDir)/\(spec.samples[currentIndex].name).wav"
        let url = URL(fileURLWithPath: outPath)
        try? FileManager.default.createDirectory(at: url.deletingLastPathComponent(), withIntermediateDirectories: true)
        let settings: [String: Any] = [
            AVFormatIDKey: kAudioFormatLinearPCM,
            AVSampleRateKey: 48000,
            AVNumberOfChannelsKey: 1,
            AVLinearPCMBitDepthKey: 16,
            AVLinearPCMIsFloatKey: false,
            AVLinearPCMIsBigEndianKey: false,
        ]
        do {
            let rec = try AVAudioRecorder(url: url, settings: settings)
            rec.isMeteringEnabled = true
            rec.prepareToRecord()
            rec.record(forDuration: maxRecordSec)
            recorder = rec
        } catch {
            statusLabel.stringValue = "error: \(error.localizedDescription)"
            return
        }
        soundDetected = false
        silenceFrames = 0
        elapsedFrames = 0
        maxFrames = Int(maxRecordSec * 20)
        statusLabel.stringValue = "listening for the hit…"
        setMode(.recording)
        levelTimer = Timer.scheduledTimer(withTimeInterval: 0.05, repeats: true) { [weak self] _ in
            guard let self = self, self.sequenceToken == token else { return }
            self.tickLevel()
        }
    }

    func tickLevel() {
        guard let rec = recorder, rec.isRecording else { return }
        rec.updateMeters()
        let db = Double(rec.averagePower(forChannel: 0))
        levelMeter.integerValue = Int(min(100, max(0, (db + 60) * (100.0 / 60.0))))
        elapsedFrames += 1
        if !soundDetected {
            if db > onsetDb {
                soundDetected = true
                statusLabel.stringValue = "recording — keep going…"
            }
        } else {
            if db < silenceDb {
                silenceFrames += 1
                if silenceFrames >= Int(silenceMs / 50.0) {
                    stopRecording(); return
                }
            } else {
                silenceFrames = 0
            }
        }
        if elapsedFrames >= maxFrames { stopRecording() }
    }

    @objc func stopAction() { stopRecording() }

    func stopRecording() {
        let token = sequenceToken
        levelTimer?.invalidate(); levelTimer = nil
        recorder?.stop(); recorder = nil
        statusLabel.stringValue = "trimming + normalizing…"
        setMode(.trimming)
        let outPath = "\(spec.outDir)/\(spec.samples[currentIndex].name).wav"
        let ok = WavIO.trimNormalize(
            path: outPath,
            thresholdPctOfPeak: trimSpec?.thresholdPctOfPeak ?? 0.08,
            padHeadMs: trimSpec?.padHeadMs ?? 8,
            padTailMs: trimSpec?.padTailMs ?? 80,
            fadeMs: trimSpec?.fadeMs ?? 3,
            normalizeDb: trimSpec?.normalizeDb ?? -1.0
        )
        guard sequenceToken == token else { return }
        if !ok {
            statusLabel.stringValue = "⚠ silent or unreadable — retry?"
            mascot.mood = .error
            showDecision()
            keepButton.isEnabled = false
            return
        }
        keepButton.isEnabled = true
        playback(path: outPath)
    }

    func playback(path: String) {
        statusLabel.stringValue = "playback…"
        setMode(.playback)
        do {
            let p = try AVAudioPlayer(contentsOf: URL(fileURLWithPath: path))
            p.delegate = self
            p.play()
            player = p
        } catch {
            statusLabel.stringValue = "playback error — keep, retry, or skip?"
            setMode(.decision)
            return
        }
    }

    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            self.statusLabel.stringValue = "keep, retry, or skip?"
            self.setMode(.decision)
        }
    }

    @objc func keepAction() {
        guard currentIndex < spec.samples.count else { return }
        doneIndexes.insert(currentIndex)
        skippedIndexes.remove(currentIndex)
        currentIndex += 1
        beginCurrent()
    }

    @objc func retryAction() {
        let outPath = "\(spec.outDir)/\(spec.samples[currentIndex].name).wav"
        try? FileManager.default.removeItem(atPath: outPath)
        beginCurrent()
    }

    @objc func skipAction() {
        let outPath = "\(spec.outDir)/\(spec.samples[currentIndex].name).wav"
        try? FileManager.default.removeItem(atPath: outPath)
        skippedIndexes.insert(currentIndex)
        doneIndexes.remove(currentIndex)
        currentIndex += 1
        beginCurrent()
    }

    func finish() {
        timeline.update(names: spec.samples.map { $0.name }, currentIndex: -1, done: doneIndexes, skipped: skippedIndexes)
        progressLabel.stringValue = "✓ all samples processed"
        nameLabel.stringValue = "done!"
        descLabel.stringValue = ""
        statusLabel.stringValue = "\(doneIndexes.count) kept · \(skippedIndexes.count) skipped — close window to exit"
        setMode(.done)
        let utt = AVSpeechUtterance(string: "Thank you, your spirit nourishes the track.")
        utt.rate = 0.48
        synth.speak(utt)
    }

    func windowWillClose(_ notification: Notification) {
        levelTimer?.invalidate()
        recorder?.stop()
        player?.stop()
        synth.stopSpeaking(at: .immediate)
    }
}

// Speech delegate proxy with a sequence token so stale finish events
// don't fire onto the wrong sample.
final class SpeechDelegateProxy: NSObject, AVSpeechSynthesizerDelegate {
    var onFinish: ((Int) -> Void)?
    var token: Int = 0
    func speechSynthesizer(_ synthesizer: AVSpeechSynthesizer, didFinish utterance: AVSpeechUtterance) {
        let t = self.token
        // Fire only after the queue has fully drained (preUtteranceDelay
        // makes us see multiple didFinish in one beginCurrent — wait until
        // the synth reports not speaking before signaling ready).
        DispatchQueue.main.async {
            if !synthesizer.isSpeaking {
                self.onFinish?(t)
            }
        }
    }
}

// Custom red record button — round-rect with a red disc icon.
final class RedRecordButton: NSButton {
    override var wantsUpdateLayer: Bool { false }
    override func draw(_ dirtyRect: NSRect) {
        let r = bounds.insetBy(dx: 1, dy: 1)
        let rr = NSBezierPath(roundedRect: r, xRadius: 12, yRadius: 12)
        if isEnabled {
            NSColor(calibratedRed: 0.86, green: 0.20, blue: 0.20, alpha: 1).setFill()
        } else {
            NSColor(calibratedRed: 0.55, green: 0.30, blue: 0.30, alpha: 0.6).setFill()
        }
        rr.fill()
        // inner gradient hint
        NSColor.black.withAlphaComponent(0.20).setStroke()
        rr.lineWidth = 1
        rr.stroke()
        // record dot
        NSColor.white.setFill()
        let dot = NSBezierPath(ovalIn: NSRect(x: bounds.midX - 36, y: bounds.midY - 12, width: 24, height: 24))
        dot.fill()
        // label
        let style = NSMutableParagraphStyle()
        style.alignment = .left
        let title = NSAttributedString(string: "Record", attributes: [
            .foregroundColor: NSColor.white,
            .font: NSFont.systemFont(ofSize: 18, weight: .semibold),
            .paragraphStyle: style,
        ])
        title.draw(at: NSPoint(x: bounds.midX - 4, y: bounds.midY - 12))
    }
    override func mouseDown(with event: NSEvent) {
        guard isEnabled else { return }
        super.mouseDown(with: event)
    }
}
