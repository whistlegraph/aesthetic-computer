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
    var alignment: AlignmentView!
    var crossfader: NSSlider!
    var crossfaderLabel: NSTextField!
    var playButton: NSButton!
    var countinCheckbox: NSButton!
    var trafficLights: TrafficLightView!
    var recordLabel: NSTextField!
    // Retained for back-compat but hidden — the 3-layer hamburger inside
    // each AlignmentView pad now handles tone/arm/sample per-note.
    var modeSelector: NSSegmentedControl!
    var subtitleLabel: NSTextField!
    // Captured at the moment Record fires — used by playback() to pass
    // the right range to AlignmentView so the waveform paints only
    // across the selected note boxes.
    var lastRecordingRange: (start: Int, end: Int)? = nil
    // Crossfade playback uses two engine player nodes — recorded vocal
    // + melody reference — mixed by a horizontal slider. nil between
    // recordings.
    var cfEngine: AVAudioEngine?
    var cfVocalPlayer: AVAudioPlayerNode?
    var cfMelodyPlayer: AVAudioPlayerNode?
    var cfVocalMixer: AVAudioMixerNode?
    var cfMelodyMixer: AVAudioMixerNode?
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
    var doneButton: NSButton!

    init(spec: Spec) {
        self.spec = spec
        self.onsetDb = spec.autoStop?.onsetDb ?? -22
        self.silenceDb = spec.autoStop?.silenceDb ?? -38
        self.silenceMs = spec.autoStop?.silenceDurationMs ?? 1200
        self.maxRecordSec = spec.autoStop?.maxRecordSec ?? 12
        self.trimSpec = spec.trim
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 720, height: 260),
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

        // ── MINIMAL LAYOUT, vertical rhythm + generous whitespace ───
        //   alignment timeline at top, full width
        //   tiny dim "Sample n of m" caption
        //   sample H1 + desc + mascot beside
        //   status hint
        //   level meter
        //   Record / Stop (centered, prominent)
        //   count-in checkbox + traffic lights
        //   crossfader (after take) + decision row
        // ── ULTRA-MINIMAL LAYOUT, 4 rows ────────────────────────────
        //   row 1: alignment timeline (full width)
        //   row 2: sample H1 + mascot (right)
        //   row 3: [count-in] [traffic lights]   /// [▶ Preview]
        //   row 4: Record / Stop button (full width, draws live level
        //          meter inside while recording)
        //   row 5 (only after take): crossfader + Keep/Retry/Skip row
        // ── SINGLE-STRIP LAYOUT ────────────────────────────────────
        //   row 1: alignment timeline (full width) + tiny mode selector
        //   row 2: H1 sample-name + tiny desc + count-in
        //   row 3 (only after a take): crossfader + Keep/Retry/Skip
        let W: CGFloat = 720
        let H: CGFloat = 260           // grew from 220 → 260 to give the
        let pad: CGFloat = 18          // 3-layer hamburger stripes more
        let colW = W - 2 * pad          // vertical room (stripes ~23 px tall)

        // Background mascot — full window, faded watermark
        mascot = MascotView(frame: cv.bounds)
        mascot.autoresizingMask = [NSView.AutoresizingMask.width, NSView.AutoresizingMask.height]
        cv.addSubview(mascot)

        // Row 1 — alignment timeline + 3-mode selector + count-in
        alignment = AlignmentView(frame: NSRect(x: pad, y: H - 150, width: colW, height: 140))
        cv.addSubview(alignment)
        alignment.onRangeChanged = { [weak self] in self?.updateForRange() }
        alignment.onArmedPress   = { [weak self] i in self?.armedPressed(at: i) }
        alignment.onArmedRelease = { [weak self] i in self?.armedReleased(at: i) }
        alignment.onSamplePlay   = { [weak self] i in self?.playSampleForNote(i) }

        // Mode selector — toggles the alignment-pad click behaviour.
        //   tone   : tap a pad → audition the sine bell (default)
        //   arm    : press + hold a pad → RECORD that word
        //   sample : tap a pad → play back the recorded take for it
        modeSelector = NSSegmentedControl(labels: ["tone", "arm", "sample"],
                                           trackingMode: .selectOne,
                                           target: self,
                                           action: #selector(modeChanged(_:)))
        modeSelector.frame = NSRect(x: pad, y: H - 176, width: 220, height: 22)
        modeSelector.selectedSegment = 0
        modeSelector.controlSize = .small
        modeSelector.isHidden = true   // superseded by per-note hamburger
        cv.addSubview(modeSelector)

        // Subtitle — mirrors any TTS the wizard speaks so the singer can
        // read along (and recover if the speech is missed). Italic, low
        // contrast so it doesn't compete with the working controls.
        subtitleLabel = NSTextField(labelWithString: "")
        subtitleLabel.frame = NSRect(x: pad, y: H - 176, width: colW, height: 22)
        subtitleLabel.font = NSFontManager.shared.font(withFamily: NSFont.systemFont(ofSize: 11).familyName ?? "Helvetica",
                                                       traits: .italicFontMask, weight: 5, size: 11)
                              ?? .systemFont(ofSize: 11)
        subtitleLabel.textColor = .secondaryLabelColor
        subtitleLabel.alignment = .center
        subtitleLabel.lineBreakMode = .byTruncatingTail
        cv.addSubview(subtitleLabel)

        // Vestigial — kept allocated for the existing wiring but hidden.
        // The per-note hamburger covers TONE auditioning, and global
        // count-in / preview are no longer surfaced (@jeffrey 2026-05-29).
        countinCheckbox = NSButton(checkboxWithTitle: "count-in", target: nil, action: nil)
        countinCheckbox.frame = NSRect(x: -200, y: -200, width: 84, height: 22)
        countinCheckbox.state = .off
        countinCheckbox.isHidden = true
        cv.addSubview(countinCheckbox)
        // Count-in / status traffic lights — hidden in the simplified
        // hamburger flow (@jeffrey 2026-05-29: "remove the count in
        // traffic light").
        trafficLights = TrafficLightView(frame: NSRect(x: -200, y: -200, width: 140, height: 22))
        trafficLights.isHidden = true
        cv.addSubview(trafficLights)
        playButton = NSButton(title: "▶ Preview", target: self, action: #selector(previewMelody(_:)))
        playButton.bezelStyle = .rounded
        playButton.frame = NSRect(x: -200, y: -200, width: 100, height: 22)
        playButton.isHidden = true
        cv.addSubview(playButton)

        // Row 2 — H1 sample-name (no separate descLabel — saves a row)
        nameLabel = NSTextField(labelWithString: "")
        nameLabel.frame = NSRect(x: pad, y: H - 202, width: colW, height: 20)
        nameLabel.font = .monospacedSystemFont(ofSize: 13, weight: .semibold)
        nameLabel.textColor = .labelColor
        cv.addSubview(nameLabel)
        descLabel = NSTextField(labelWithString: "")
        descLabel.isHidden = true   // lyric is shown in the alignment row already
        cv.addSubview(descLabel)

        // Hidden vestigial widgets — kept allocated to satisfy the
        // existing wiring but invisible in the strip layout.
        progressLabel = NSTextField(labelWithString: ""); progressLabel.isHidden = true
        cv.addSubview(progressLabel)
        statusLabel = NSTextField(labelWithString: ""); statusLabel.isHidden = true
        cv.addSubview(statusLabel)
        timeline = TimelineView(frame: .zero); timeline.isHidden = true
        cv.addSubview(timeline)
        recordButton = RedRecordButton(frame: NSRect(x: -100, y: -100, width: 1, height: 1))
        recordButton.target = self
        recordButton.action = #selector(recordAction)
        recordButton.isHidden = true
        cv.addSubview(recordButton)
        stopButton = NSButton(title: "Stop", target: self, action: #selector(stopAction))
        stopButton.frame = NSRect(x: -100, y: -100, width: 1, height: 1)
        stopButton.isHidden = true
        cv.addSubview(stopButton)
        recordLabel = NSTextField(labelWithString: ""); recordLabel.isHidden = true
        cv.addSubview(recordLabel)
        levelMeter = NSLevelIndicator(frame: NSRect(x: -100, y: -100, width: 1, height: 1))
        levelMeter.isHidden = true
        cv.addSubview(levelMeter)

        // Bottom strip — decision row (only visible after a take).
        // Sized compact to fit a 220-tall window without overlap.
        let btnY: CGFloat = 12
        let btnH: CGFloat = 26
        let gap: CGFloat = 6
        // Simplified "next next next" flow: small Retry on the left,
        // big green Next/Done on the right. Skip is gone (Next already
        // advances without committing anything); Keep is gone (every
        // recorded take is auto-saved by the hamburger arm stripe).
        let retryW: CGFloat = 110
        retryButton = NSButton(title: "↺ Retry", target: self, action: #selector(retryAction))
        retryButton.bezelStyle = .rounded
        retryButton.frame = NSRect(x: pad, y: btnY, width: retryW, height: btnH)
        retryButton.font = .systemFont(ofSize: 11)
        cv.addSubview(retryButton)

        // Kept allocated but offscreen — old code paths still reference
        // them (showDecision/hideDecision, keepAction selector wiring).
        keepButton = NSButton(title: "Keep", target: self, action: #selector(keepAction))
        keepButton.keyEquivalent = "\r"
        keepButton.frame = NSRect(x: -200, y: -200, width: 1, height: 1)
        keepButton.isHidden = true
        cv.addSubview(keepButton)
        skipButton = NSButton(title: "Skip", target: self, action: #selector(skipAction))
        skipButton.frame = NSRect(x: -200, y: -200, width: 1, height: 1)
        skipButton.isHidden = true
        cv.addSubview(skipButton)

        // DONE — big, bottom-right, system-green. Label flips to
        // "Next →" while there are more lines to record; "✓ Done" on
        // the last sample (closes the window).
        let doneW: CGFloat = 140
        doneButton = NSButton(title: "Next →", target: self, action: #selector(doneAction))
        doneButton.bezelStyle = .rounded
        doneButton.keyEquivalent = "\r"
        doneButton.frame = NSRect(x: W - pad - doneW, y: btnY, width: doneW, height: btnH)
        doneButton.font = .systemFont(ofSize: 12, weight: .semibold)
        if #available(macOS 11.0, *) {
            doneButton.bezelColor = NSColor.systemGreen
        }
        doneButton.contentTintColor = NSColor.white
        cv.addSubview(doneButton)

        // Crossfader — vestigial in the simplified hamburger flow. Kept
        // allocated for back-compat (stopCrossfade / setMode wiring still
        // toggles them) but offscreen + hidden.
        crossfaderLabel = NSTextField(labelWithString: "melody  ←——  vocal")
        crossfaderLabel.frame = NSRect(x: -200, y: -200, width: 1, height: 1)
        crossfaderLabel.isHidden = true
        cv.addSubview(crossfaderLabel)
        crossfader = NSSlider(value: 0.6, minValue: 0, maxValue: 1,
                              target: self, action: #selector(crossfaderChanged(_:)))
        crossfader.frame = NSRect(x: -200, y: -200, width: 1, height: 1)
        crossfader.isContinuous = true
        crossfader.isHidden = true
        cv.addSubview(crossfader)

        setMode(.idle)
    }

    // ── state ──────────────────────────────────────────────────────────
    enum Mode { case idle, speaking, ready, recording, trimming, playback, decision, done }

    // ── theme palette ────────────────────────────────────────────────
    // Dynamic NSColor that picks light-mode / dark-mode values from
    // the system appearance. Keeps text readable in both themes by
    // pairing each tint with the system's semantic label color.
    static func dynamic(light: NSColor, dark: NSColor) -> NSColor {
        return NSColor(name: nil) { appearance in
            let isDark = appearance.bestMatch(from: [.darkAqua, .aqua]) == .darkAqua
            return isDark ? dark : light
        }
    }
    static func modeTint(for m: Mode) -> NSColor {
        // Each mode gets a pale (light-mode) and a muted-dark (dark-
        // mode) version of the same semantic hue. The colors lean
        // pastel so the actual UI elements stay legible.
        switch m {
        case .idle, .speaking:
            return dynamic(
                light: NSColor(calibratedRed: 0.97, green: 0.97, blue: 0.98, alpha: 1),
                dark:  NSColor(calibratedRed: 0.13, green: 0.14, blue: 0.17, alpha: 1))
        case .ready:
            return dynamic(
                light: NSColor(calibratedRed: 0.96, green: 0.97, blue: 0.99, alpha: 1),
                dark:  NSColor(calibratedRed: 0.14, green: 0.15, blue: 0.18, alpha: 1))
        case .recording:
            return dynamic(
                light: NSColor(calibratedRed: 0.99, green: 0.93, blue: 0.93, alpha: 1),
                dark:  NSColor(calibratedRed: 0.19, green: 0.12, blue: 0.13, alpha: 1))
        case .trimming:
            return dynamic(
                light: NSColor(calibratedRed: 0.99, green: 0.95, blue: 0.88, alpha: 1),
                dark:  NSColor(calibratedRed: 0.17, green: 0.14, blue: 0.11, alpha: 1))
        case .playback:
            return dynamic(
                light: NSColor(calibratedRed: 0.92, green: 0.96, blue: 0.99, alpha: 1),
                dark:  NSColor(calibratedRed: 0.10, green: 0.14, blue: 0.18, alpha: 1))
        case .decision:
            return dynamic(
                light: NSColor(calibratedRed: 0.93, green: 0.97, blue: 0.93, alpha: 1),
                dark:  NSColor(calibratedRed: 0.11, green: 0.16, blue: 0.13, alpha: 1))
        case .done:
            return dynamic(
                light: NSColor(calibratedRed: 0.88, green: 0.97, blue: 0.91, alpha: 1),
                dark:  NSColor(calibratedRed: 0.10, green: 0.18, blue: 0.13, alpha: 1))
        }
    }

    // ── one place that owns "what is allowed RIGHT NOW" ────────────
    // Every UI element is centrally driven by the current mode, so a
    // transition can't leave stale buttons / a stuck timeline / a
    // half-running crossfade behind. The window background tints with
    // the mode for a visceral state cue.
    func setMode(_ m: Mode) {
        // Default to all OFF — each case turns on only what belongs.
        playButton.isHidden = true
        countinCheckbox.isHidden = true
        trafficLights.isHidden = true
        crossfader.isHidden = true
        crossfaderLabel.isHidden = true
        modeSelector?.isHidden = true
        // Off-screen widgets stay hidden permanently (vestigial)
        recordButton.isHidden = true; recordButton.isEnabled = false
        stopButton.isHidden = true
        recordLabel.isHidden = true
        levelMeter.isHidden = true
        hideDecision()
        // Range selection only allowed in .ready — otherwise the
        // alignment timeline freezes. Note auditions (taps) still work
        // visually since they're driven by mouseDown which short-circuits.
        alignment.isInteractive = (m == .ready)

        // Background tint per mode — uses dynamic NSColor so the same
        // semantic hue picks a soft-light value in Light Mode and a
        // soft-dark value in Dark Mode. The system label colors keep
        // contrast either way.
        if let cv = window?.contentView {
            cv.wantsLayer = true
            cv.layer?.backgroundColor = Self.modeTint(for: m).cgColor
        }

        switch m {
        case .idle:
            break
        case .speaking:
            mascot.mood = .idle
        case .ready:
            modeSelector?.isHidden = false
            playButton.isHidden = false
            countinCheckbox.isHidden = false
            trafficLights.isHidden = false
            mascot.mood = .listening
        case .recording:
            modeSelector?.isHidden = false
            mascot.mood = .recording
        case .trimming:
            mascot.mood = .idle
        case .playback:
            // Crossfader visible during playback (controls vocal vs
            // melody balance for the just-recorded take). Decision row
            // also appears here so the user can keep/retry/skip while
            // the take loops.
            crossfader.isHidden = false
            crossfaderLabel.isHidden = false
            showDecision()
            mascot.mood = .success
        case .decision:
            crossfader.isHidden = false
            crossfaderLabel.isHidden = false
            showDecision()
            mascot.mood = .success
        case .done:
            mascot.mood = .success
        }
    }

    func hideDecision() {
        keepButton.isHidden = true; retryButton.isHidden = true
        skipButton.isHidden = true
        crossfader.isHidden = true; crossfaderLabel.isHidden = true
        stopCrossfade()
        // doneButton stays visible at all times — it's the escape hatch
        // that lets the user finish the session whenever they want.
        doneButton.isHidden = false
    }
    func showDecision() {
        keepButton.isHidden = false; retryButton.isHidden = false
        skipButton.isHidden = false; doneButton.isHidden = false
    }
    @objc func doneAction() {
        // "next next next" flow: while there's another sample, advance
        // to it. On the LAST sample, finish + close so downstream tooling
        // can pick up the takes immediately.
        if currentIndex + 1 < spec.samples.count {
            doneIndexes.insert(currentIndex)
            currentIndex += 1
            beginCurrent()
        } else {
            doneIndexes.insert(currentIndex)
            currentIndex = spec.samples.count
            finish()
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.4) { [weak self] in
                self?.window?.close()
                NSApp.terminate(nil)
            }
        }
    }

    @objc func modeChanged(_ sender: NSSegmentedControl) {
        switch sender.selectedSegment {
        case 1: alignment.clickMode = .arm
        case 2: alignment.clickMode = .sample
        default: alignment.clickMode = .tone
        }
    }

    // Per-note recording in ARM mode. Pressing a pad starts an
    // AVAudioRecorder for that note's slot; releasing stops + trims
    // and saves to a `<sample-name>-note-N.wav` file so each pad keeps
    // its own take.
    var perNoteRecorder: AVAudioRecorder?
    var perNoteIdx: Int? = nil
    var perNoteOutPath: String = ""

    func armedPressed(at idx: Int) {
        guard idx >= 0 else { return }
        perNoteIdx = idx
        let base = spec.samples[currentIndex].name
        perNoteOutPath = "\(spec.outDir)/\(base)-note-\(idx).wav"
        try? FileManager.default.createDirectory(
            at: URL(fileURLWithPath: perNoteOutPath).deletingLastPathComponent(),
            withIntermediateDirectories: true)
        let settings: [String: Any] = [
            AVFormatIDKey: kAudioFormatLinearPCM,
            AVSampleRateKey: 48000,
            AVNumberOfChannelsKey: 1,
            AVLinearPCMBitDepthKey: 16,
            AVLinearPCMIsFloatKey: false,
            AVLinearPCMIsBigEndianKey: false,
        ]
        do {
            let rec = try AVAudioRecorder(url: URL(fileURLWithPath: perNoteOutPath), settings: settings)
            rec.isMeteringEnabled = true
            rec.prepareToRecord()
            rec.record(forDuration: 6.0)
            perNoteRecorder = rec
        } catch { /* ignore */ }
    }

    func armedReleased(at idx: Int) {
        guard let rec = perNoteRecorder, perNoteIdx == idx else { return }
        rec.stop()
        perNoteRecorder = nil
        // Light trim + normalize so the per-note pad take is clean.
        _ = WavIO.trimNormalize(
            path: perNoteOutPath,
            thresholdPctOfPeak: trimSpec?.thresholdPctOfPeak ?? 0.05,
            padHeadMs: trimSpec?.padHeadMs ?? 12,
            padTailMs: trimSpec?.padTailMs ?? 60,
            fadeMs: trimSpec?.fadeMs ?? 4,
            normalizeDb: trimSpec?.normalizeDb ?? -3.0)
        perNoteIdx = nil
        // Push the freshly-recorded waveform into the alignment view so
        // its bottom (SAMPLE) stripe shows a thumbnail of the take.
        if let load = loadWavSamples(perNoteOutPath) {
            alignment.setNoteWaveform(noteIdx: idx, samples: load.samples)
        }
    }

    // SAMPLE-mode click: play back the per-note take if one exists,
    // otherwise audition the bell tone.
    func playSampleForNote(_ idx: Int) {
        let base = spec.samples[currentIndex].name
        let path = "\(spec.outDir)/\(base)-note-\(idx).wav"
        if FileManager.default.fileExists(atPath: path),
           let p = try? AVAudioPlayer(contentsOf: URL(fileURLWithPath: path)) {
            player?.stop()
            p.play()
            player = p
        } else {
            // Fallback — no take recorded yet, audition the bell.
            // Use the alignment view's CURRENT theme (which may have been
            // swapped by setScore for non-hellsine samples).
            let theme = alignment.theme[idx]
            AlignmentView.playMidi(alignment.rootMel + theme.off, beats: theme.beats)
        }
    }

    // Called by AlignmentView whenever the range selection changes.
    // Updates the record-button label + status hint so the user knows
    // whether they're tracking the full mantra or a piecemeal range.
    func updateForRange() {
        if let r = alignment.selectedRange() {
            recordButton.toolTip = "Record only notes \(r.start)–\(r.end) · saves to -range-\(r.start)-\(r.end).wav"
            statusLabel.stringValue = "range selected: notes \(r.start)–\(r.end) · click to clear, drag to extend"
        } else {
            recordButton.toolTip = "Record full mantra"
        }
    }

    // Replay the current sample's melody reference. If a range is
    // selected in the alignment view, play ONLY those notes (as the
    // built-in sine bells, beat-accurate to the THEME). Otherwise
    // play the full melodyFile.
    @objc func previewMelody(_ sender: NSButton) {
        guard currentIndex < spec.samples.count else { return }
        if alignment.selectedRange() != nil {
            alignment.auditionSelectedRange()
            return
        }
        let s = spec.samples[currentIndex]
        guard let mf = s.melodyFile, FileManager.default.fileExists(atPath: mf) else { return }
        do {
            player?.stop()
            let p = try AVAudioPlayer(contentsOf: URL(fileURLWithPath: mf))
            p.play()
            player = p
        } catch { /* ignore */ }
    }

    @objc func crossfaderChanged(_ sender: NSSlider) {
        // 0 = melody only, 1 = vocal only, 0.5 = balanced.
        // Use equal-power curve so the centre isn't a level dip.
        let t = sender.doubleValue
        let melodyGain = cos(t * .pi / 2.0)
        let vocalGain  = sin(t * .pi / 2.0)
        cfMelodyMixer?.outputVolume = Float(melodyGain)
        cfVocalMixer?.outputVolume  = Float(vocalGain)
    }

    func stopCrossfade() {
        cfVocalPlayer?.stop()
        cfMelodyPlayer?.stop()
        cfEngine?.stop()
        cfVocalPlayer = nil
        cfMelodyPlayer = nil
        cfVocalMixer = nil
        cfMelodyMixer = nil
        cfEngine = nil
    }

    // Start crossfade playback of the recorded vocal + the sample's
    // melodyFile reference. Both schedule from t=0; the slider in the
    // UI rebalances output gains in real time.
    func startCrossfade(vocalPath: String) {
        stopCrossfade()
        let melodyPath = spec.samples[currentIndex].melodyFile
        guard let vocalFile = try? AVAudioFile(forReading: URL(fileURLWithPath: vocalPath)) else { return }
        let engine = AVAudioEngine()
        let vPlayer = AVAudioPlayerNode()
        let vMixer  = AVAudioMixerNode()
        engine.attach(vPlayer); engine.attach(vMixer)
        engine.connect(vPlayer, to: vMixer, format: vocalFile.processingFormat)
        engine.connect(vMixer, to: engine.mainMixerNode, format: nil)
        vPlayer.scheduleFile(vocalFile, at: nil)
        cfVocalPlayer = vPlayer; cfVocalMixer = vMixer
        if let mp = melodyPath, FileManager.default.fileExists(atPath: mp),
           let melodyFile = try? AVAudioFile(forReading: URL(fileURLWithPath: mp)) {
            let mPlayer = AVAudioPlayerNode()
            let mMixer  = AVAudioMixerNode()
            engine.attach(mPlayer); engine.attach(mMixer)
            engine.connect(mPlayer, to: mMixer, format: melodyFile.processingFormat)
            engine.connect(mMixer, to: engine.mainMixerNode, format: nil)
            mPlayer.scheduleFile(melodyFile, at: nil)
            cfMelodyPlayer = mPlayer; cfMelodyMixer = mMixer
        }
        cfEngine = engine
        do {
            try engine.start()
            // initial mix per slider position
            crossfaderChanged(crossfader)
            cfVocalPlayer?.play()
            cfMelodyPlayer?.play()
        } catch {
            stopCrossfade()
        }
    }

    // Load mono float samples from a WAV (for waveform overlay).
    func loadWavSamples(_ path: String) -> (samples: [Float], sampleRate: Double)? {
        guard let f = try? AVAudioFile(forReading: URL(fileURLWithPath: path)) else { return nil }
        let fmt = f.processingFormat
        let frames = AVAudioFrameCount(f.length)
        guard let buf = AVAudioPCMBuffer(pcmFormat: fmt, frameCapacity: frames) else { return nil }
        do { try f.read(into: buf) } catch { return nil }
        let n = Int(buf.frameLength)
        let ch = Int(fmt.channelCount)
        guard let raw = buf.floatChannelData else { return nil }
        var out = [Float](repeating: 0, count: n)
        for i in 0..<n {
            var sum: Float = 0
            for c in 0..<ch { sum += raw[c][i] }
            out[i] = sum / Float(ch)
        }
        return (out, fmt.sampleRate)
    }

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
        alignment.setLyric(s.lyric)
        // Per-sample score override — when present, swaps the alignment
        // view's note theme from the default hellsine brass theme to the
        // sample's actual melody. Specs that omit `score` keep the
        // hellsine default for backward compatibility.
        if let sc = s.score {
            let notes = sc.notes.map { (off: $0.off, beats: $0.beats) }
            alignment.setScore(rootMel: sc.rootMel, notes: notes)
        } else {
            alignment.setScore(rootMel: AlignmentView.defaultRootMel, notes: [])
        }
        alignment.clearRecordedWaveform()
        alignment.clearRange()
        // Rehydrate per-note waveform thumbnails for any takes that
        // already exist on disk for this sample (so resuming a session
        // shows what's been recorded).
        let base = s.name
        for k in 0..<alignment.theme.count {
            let p = "\(spec.outDir)/\(base)-note-\(k).wav"
            if FileManager.default.fileExists(atPath: p),
               let load = loadWavSamples(p) {
                alignment.setNoteWaveform(noteIdx: k, samples: load.samples)
            }
        }
        // Make the alignment view first responder so ←/→/space arrive
        // here without having to click first.
        window?.makeFirstResponder(alignment)
        // Flip the Next/Done button label per position in the queue.
        let isLast = currentIndex + 1 >= spec.samples.count
        doneButton?.title = isLast ? "✓ Done" : "Next →"
        crossfader.isHidden = true
        crossfaderLabel.isHidden = true
        stopCrossfade()
        updateForRange()
        progressLabel.stringValue = "Sample \(currentIndex + 1) of \(spec.samples.count)"
        nameLabel.stringValue = s.name
        descLabel.stringValue = s.desc
        setMode(.speaking)
        speechDelegate.token = token

        // If a melodyFile is provided, play it FIRST as a reference cue
        // for the singer (jeffrey: play me the melody before I sing).
        // Then speak the desc. If absent, behave as before.
        if let mf = s.melodyFile, FileManager.default.fileExists(atPath: mf) {
            statusLabel.stringValue = "playing reference melody…"
            playPromptThenSpeak(path: mf, token: token, isFirst: !hasGreeted, desc: s.desc)
            hasGreeted = true
        } else {
            // Just announce the sample by name. The full desc reads on
            // screen — no need to TTS a paragraph for every sample.
            // First sample also skips the greeting (was "I need your
            // human spirit!" + full desc — too much chatter on launch).
            speakWithSubtitle(s.name, rate: 0.5)
            hasGreeted = true
        }
    }

    // Speak a short string AND mirror it into the on-screen subtitle so
    // the user can read along (and recover if speech is missed).
    func speakWithSubtitle(_ text: String, rate: Float = 0.5,
                            pitch: Float = 1.0, delay: TimeInterval = 0) {
        subtitleLabel?.stringValue = text
        let utt = AVSpeechUtterance(string: text)
        utt.rate = rate
        utt.pitchMultiplier = pitch
        utt.preUtteranceDelay = delay
        synth.speak(utt)
    }

    // Marks whether the currently-playing AVAudioPlayer is a melody
    // reference (true) or the post-record playback (false). Determines
    // what audioPlayerDidFinishPlaying does next.
    var playingPrompt: Bool = false
    var pendingDesc: String? = nil
    var pendingDescIsFirst: Bool = false

    func playPromptThenSpeak(path: String, token: Int, isFirst: Bool, desc: String) {
        pendingDesc = desc
        pendingDescIsFirst = isFirst
        playingPrompt = true
        do {
            let p = try AVAudioPlayer(contentsOf: URL(fileURLWithPath: path))
            p.delegate = self
            p.play()
            player = p
        } catch {
            // Fall through to speech if the file can't be loaded.
            playingPrompt = false
            speakPendingDesc()
        }
    }

    func speakPendingDesc() {
        statusLabel.stringValue = "wizard is speaking…"
        let s = currentIndex < spec.samples.count ? spec.samples[currentIndex] : nil
        pendingDesc = nil
        // Reduced verbosity — speak only the sample name. Full desc still
        // shows in the alignment row.
        speakWithSubtitle(s?.name ?? "", rate: 0.5)
    }

    func afterSpeak(token: Int) {
        guard token == sequenceToken else { return }
        statusLabel.stringValue = "ready — press the red button when you're set"
        setMode(.ready)
    }

    // Build the output WAV path for the current sample. If a range is
    // selected in the alignment view, the file gets a -range-N-M
    // suffix so partial takes don't clobber full takes. The downstream
    // gen-jeffrey-whisper pipeline can splice ranges over a full take.
    func currentSampleOutPath() -> String {
        let base = spec.samples[currentIndex].name
        if let r = alignment.selectedRange() {
            return "\(spec.outDir)/\(base)-range-\(r.start)-\(r.end).wav"
        }
        return "\(spec.outDir)/\(base).wav"
    }

    // 4-beat count-in driven by the BPM. Each beat lights the next
    // traffic light and plays a short tone (rising pitch). On the 4th
    // beat ("GO" — green) the actual recording starts.
    func runCountin(_ done: @escaping () -> Void) {
        let bpm: Double = 182
        let beat = 60.0 / bpm
        let tones: [Int] = [60, 62, 64, 72]      // C4 D4 E4 → high C5 GO
        trafficLights.clear()
        for i in 0..<4 {
            DispatchQueue.main.asyncAfter(deadline: .now() + beat * Double(i)) { [weak self] in
                self?.trafficLights.light(at: i)
                AlignmentView.playMidi(tones[i], beats: 0.4)
            }
        }
        DispatchQueue.main.asyncAfter(deadline: .now() + beat * 4) { [weak self] in
            self?.trafficLights.clear()
            done()
        }
    }

    // ── recording (manual start, auto-stop on silence) ──────────────────
    @objc func recordAction() {
        if countinCheckbox.state == .on {
            statusLabel.stringValue = "count-in…"
            runCountin { [weak self] in self?.startRecordingNow() }
            return
        }
        startRecordingNow()
    }

    func startRecordingNow() {
        let token = sequenceToken
        lastRecordingRange = alignment.selectedRange()
        let outPath = currentSampleOutPath()
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
        let outPath = currentSampleOutPath()
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
        setMode(.playback)
        statusLabel.stringValue = "playback (drag slider to balance vocal vs melody)"
        // Load samples + paint the waveform over the alignment timeline.
        // Pass the recording's range (if partial) so the waveform only
        // covers the boxes it actually represents.
        if let load = loadWavSamples(path) {
            alignment.setRecordedWaveform(samples: load.samples,
                                          sampleRate: load.sampleRate,
                                          range: lastRecordingRange)
        }
        // Crossfade playback against the melody reference.
        startCrossfade(vocalPath: path)
    }

    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        DispatchQueue.main.async { [weak self] in
            guard let self = self else { return }
            if self.playingPrompt {
                self.playingPrompt = false
                self.speakPendingDesc()
            } else {
                self.statusLabel.stringValue = "keep, retry, or skip?"
                self.setMode(.decision)
            }
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
        speakWithSubtitle("done", rate: 0.5)
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
        // Just a red circle — no text, no inner dot, no rounded-rect
        // chrome. The label is a sibling NSTextField beside the button.
        let inset: CGFloat = 2
        let r = bounds.insetBy(dx: inset, dy: inset)
        let dia = min(r.width, r.height)
        let centered = NSRect(x: r.midX - dia * 0.5, y: r.midY - dia * 0.5,
                              width: dia, height: dia)
        let circle = NSBezierPath(ovalIn: centered)
        if isEnabled {
            NSColor(calibratedRed: 0.88, green: 0.18, blue: 0.18, alpha: 1).setFill()
        } else {
            NSColor(calibratedRed: 0.55, green: 0.30, blue: 0.30, alpha: 0.5).setFill()
        }
        circle.fill()
        NSColor.black.withAlphaComponent(0.18).setStroke()
        circle.lineWidth = 1
        circle.stroke()
    }
    override func mouseDown(with event: NSEvent) {
        guard isEnabled else { return }
        super.mouseDown(with: event)
    }
}
