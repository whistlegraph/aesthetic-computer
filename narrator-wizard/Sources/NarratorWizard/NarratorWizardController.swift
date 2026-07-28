import AppKit
import AVFoundation
import AVKit
import CoreAudio

final class NarratorWizardController: NSWindowController, NSWindowDelegate, AVAudioPlayerDelegate {
    private let spec: NarrationSpec
    private let specURL: URL
    private let outURL: URL
    private let manifestURL: URL
    private var manifest: NarrationManifest
    private var index = 0
    private var recorder: AVAudioRecorder?
    private var player: AVAudioPlayer?
    private var monitorEngine: AVAudioEngine?
    private var originalInputID: AudioDeviceID?
    private var videoWindow: NSWindow?
    private var videoPlayer: AVPlayer?
    private var videoCloseObserver: NSObjectProtocol?
    private var meterTimer: Timer?
    private var pendingTake: NarrationTake?

    private let progressLabel = NSTextField(labelWithString: "")
    private let sceneLabel = NSTextField(labelWithString: "")
    private let scriptLabel = NSTextField(wrappingLabelWithString: "")
    private let statusLabel = NSTextField(labelWithString: "")
    private let imageView = NSImageView()
    private let levelMeter = NSLevelIndicator()
    private let inputPopup = NSPopUpButton()
    private let refreshInputsButton = NSButton(title: "↻ Inputs", target: nil, action: nil)
    private let monitorCheckbox = NSButton(checkboxWithTitle: "Monitor input (use headphones)", target: nil, action: nil)
    private let backButton = NSButton(title: "← Previous", target: nil, action: nil)
    private let nextButton = NSButton(title: "Next →", target: nil, action: nil)
    private let recordButton = NSButton(title: "● Record take", target: nil, action: nil)
    private let stopButton = NSButton(title: "■ Stop", target: nil, action: nil)
    private let playButton = NSButton(title: "▶ Play take", target: nil, action: nil)
    private let redoButton = NSButton(title: "↺ New take", target: nil, action: nil)
    private let keepButton = NSButton(title: "Keep & Next →", target: nil, action: nil)
    private let revealButton = NSButton(title: "Reveal recordings", target: nil, action: nil)
    private let currentVideoButton = NSButton(title: "▶ Play current cut", target: nil, action: nil)

    init(spec: NarrationSpec, specURL: URL) throws {
        self.spec = spec
        self.specURL = specURL
        let rawOut = NSString(string: spec.outDir).expandingTildeInPath
        self.outURL = rawOut.hasPrefix("/")
            ? URL(fileURLWithPath: rawOut, isDirectory: true).standardizedFileURL
            : specURL.deletingLastPathComponent().appendingPathComponent(rawOut, isDirectory: true).standardizedFileURL
        self.manifestURL = self.outURL.appendingPathComponent("manifest.json")
        try FileManager.default.createDirectory(at: outURL, withIntermediateDirectories: true)
        if let data = try? Data(contentsOf: manifestURL),
           let saved = try? JSONDecoder().decode(NarrationManifest.self, from: data) {
            var states = saved.lines
            for line in spec.lines where !states.contains(where: { $0.id == line.id }) {
                states.append(NarrationLineState(id: line.id, selectedTake: nil, takes: []))
            }
            self.manifest = NarrationManifest(
                formatVersion: 1,
                projectTitle: spec.title,
                sourceSpec: specURL.path,
                updatedAt: saved.updatedAt,
                gapMs: spec.gapMs ?? saved.gapMs,
                lines: states
            )
        } else {
            self.manifest = NarrationManifest(
                formatVersion: 1,
                projectTitle: spec.title,
                sourceSpec: specURL.path,
                updatedAt: Self.isoNow(),
                gapMs: spec.gapMs ?? 350,
                lines: spec.lines.map { NarrationLineState(id: $0.id, selectedTake: nil, takes: []) }
            )
        }

        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 980, height: 670),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered,
            defer: false
        )
        window.title = "Narrator Wizard — \(spec.title)"
        window.minSize = NSSize(width: 800, height: 560)
        window.isReleasedWhenClosed = false
        // Default is system Auto. The override exists only for deterministic
        // visual QA and screenshots; ordinary launches never set it.
        if ProcessInfo.processInfo.environment["NARRATOR_WIZARD_APPEARANCE"] == "dark" {
            window.appearance = NSAppearance(named: .darkAqua)
        } else if ProcessInfo.processInfo.environment["NARRATOR_WIZARD_APPEARANCE"] == "light" {
            window.appearance = NSAppearance(named: .aqua)
        }
        window.center()
        super.init(window: window)
        originalInputID = defaultInputID()
        window.delegate = self
        setupUI()
        DistributedNotificationCenter.default().addObserver(
            self,
            selector: #selector(systemAppearanceChanged),
            name: NSNotification.Name("AppleInterfaceThemeChangedNotification"),
            object: nil
        )
        if let firstIncomplete = spec.lines.firstIndex(where: { line in
            self.state(for: line.id).selectedTake == nil
        }) { index = firstIncomplete }
        showCurrent()
    }

    required init?(coder: NSCoder) { fatalError() }

    private static func isoNow() -> String {
        ISO8601DateFormatter().string(from: Date())
    }

    private func setupUI() {
        guard let content = window?.contentView else { return }
        content.wantsLayer = true
        applyTheme()

        let header = NSStackView(views: [progressLabel, NSView(), currentVideoButton, revealButton])
        header.orientation = .horizontal
        header.alignment = .centerY
        header.spacing = 10
        progressLabel.font = .monospacedSystemFont(ofSize: 12, weight: .medium)
        progressLabel.textColor = .secondaryLabelColor
        revealButton.target = self
        revealButton.action = #selector(revealRecordings)
        revealButton.bezelStyle = .rounded
        currentVideoButton.target = self
        currentVideoButton.action = #selector(showCurrentVideo)
        currentVideoButton.bezelStyle = .rounded
        currentVideoButton.isHidden = spec.video == nil

        imageView.imageScaling = .scaleProportionallyUpOrDown
        imageView.imageAlignment = .alignCenter
        imageView.wantsLayer = true
        imageView.layer?.cornerRadius = 14
        imageView.widthAnchor.constraint(equalToConstant: 330).isActive = true
        imageView.heightAnchor.constraint(equalToConstant: 430).isActive = true

        sceneLabel.font = .systemFont(ofSize: 18, weight: .semibold)
        sceneLabel.textColor = .secondaryLabelColor
        scriptLabel.font = .systemFont(ofSize: 31, weight: .medium)
        scriptLabel.maximumNumberOfLines = 0
        scriptLabel.lineBreakMode = .byWordWrapping
        scriptLabel.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
        statusLabel.font = .systemFont(ofSize: 13, weight: .medium)
        statusLabel.textColor = .secondaryLabelColor

        let scriptColumn = NSStackView(views: [sceneLabel, scriptLabel, NSView(), statusLabel])
        scriptColumn.orientation = .vertical
        scriptColumn.alignment = .leading
        scriptColumn.spacing = 14

        let body = NSStackView(views: [imageView, scriptColumn])
        body.orientation = .horizontal
        body.alignment = .top
        body.spacing = 28

        levelMeter.minValue = -55
        levelMeter.maxValue = 0
        levelMeter.warningValue = -12
        levelMeter.criticalValue = -4
        levelMeter.levelIndicatorStyle = .continuousCapacity
        levelMeter.isEditable = false
        levelMeter.setContentHuggingPriority(.defaultLow, for: .horizontal)
        let levelLabel = NSTextField(labelWithString: "INPUT LEVEL")
        levelLabel.font = .monospacedSystemFont(ofSize: 11, weight: .bold)
        levelLabel.textColor = .secondaryLabelColor
        levelLabel.widthAnchor.constraint(equalToConstant: 82).isActive = true
        let meterRow = NSStackView(views: [levelLabel, levelMeter])
        meterRow.orientation = .horizontal
        meterRow.alignment = .centerY
        meterRow.spacing = 10

        inputPopup.target = self
        inputPopup.action = #selector(inputChanged)
        inputPopup.setContentHuggingPriority(.defaultLow, for: .horizontal)
        refreshInputsButton.target = self
        refreshInputsButton.action = #selector(refreshInputs)
        refreshInputsButton.bezelStyle = .rounded
        monitorCheckbox.target = self
        monitorCheckbox.action = #selector(monitorChanged)
        monitorCheckbox.state = .off
        let inputLabel = NSTextField(labelWithString: "MIC")
        inputLabel.font = .monospacedSystemFont(ofSize: 11, weight: .bold)
        inputLabel.textColor = .secondaryLabelColor
        let audioBar = NSStackView(views: [inputLabel, inputPopup, refreshInputsButton, monitorCheckbox])
        audioBar.orientation = .horizontal
        audioBar.alignment = .centerY
        audioBar.spacing = 10
        loadInputDevices()

        for button in [backButton, nextButton, recordButton, stopButton, playButton, redoButton, keepButton] {
            button.target = self
            button.bezelStyle = .rounded
        }
        backButton.action = #selector(previousLine)
        nextButton.action = #selector(nextLine)
        recordButton.action = #selector(recordTake)
        stopButton.action = #selector(stopTake)
        playButton.action = #selector(playTake)
        redoButton.action = #selector(recordTake)
        keepButton.action = #selector(keepAndNext)
        recordButton.keyEquivalent = "r"
        stopButton.keyEquivalent = "s"
        playButton.keyEquivalent = "p"
        keepButton.keyEquivalent = "\r"
        recordButton.contentTintColor = .systemRed
        keepButton.contentTintColor = .systemGreen

        let controls = NSStackView(views: [backButton, nextButton, NSView(), recordButton, stopButton, playButton, keepButton])
        controls.orientation = .horizontal
        controls.alignment = .centerY
        controls.spacing = 10

        let root = NSStackView(views: [header, body, audioBar, meterRow, controls])
        root.orientation = .vertical
        root.alignment = .leading
        root.spacing = 18
        root.edgeInsets = NSEdgeInsets(top: 22, left: 24, bottom: 22, right: 24)
        root.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(root)
        NSLayoutConstraint.activate([
            root.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            root.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            root.topAnchor.constraint(equalTo: content.topAnchor),
            root.bottomAnchor.constraint(equalTo: content.bottomAnchor),
            header.widthAnchor.constraint(equalTo: root.widthAnchor, constant: -48),
            body.widthAnchor.constraint(equalTo: root.widthAnchor, constant: -48),
            body.heightAnchor.constraint(greaterThanOrEqualToConstant: 430),
            audioBar.widthAnchor.constraint(equalTo: root.widthAnchor, constant: -48),
            meterRow.widthAnchor.constraint(equalTo: root.widthAnchor, constant: -48),
            levelMeter.heightAnchor.constraint(equalToConstant: 12),
            controls.widthAnchor.constraint(equalTo: root.widthAnchor, constant: -48),
        ])
        applyTheme()
    }

    @objc private func systemAppearanceChanged() {
        DispatchQueue.main.async { [weak self] in self?.applyTheme() }
    }

    private func applyTheme() {
        // Keep the layer surfaces transparent so AppKit resolves semantic
        // window/text/control colors against the *current* system appearance.
        // Caching a dynamic NSColor as CGColor here would freeze whichever
        // appearance happened to be active at launch.
        window?.contentView?.layer?.backgroundColor = nil
        imageView.layer?.backgroundColor = nil
        window?.contentView?.needsDisplay = true
    }

    private func stateIndex(for id: String) -> Int {
        manifest.lines.firstIndex(where: { $0.id == id })!
    }

    private func state(for id: String) -> NarrationLineState {
        manifest.lines[stateIndex(for: id)]
    }

    private func showCurrent() {
        guard index >= 0, index < spec.lines.count else { return }
        stopPlayback()
        pendingTake = nil
        let line = spec.lines[index]
        let lineState = state(for: line.id)
        let kept = spec.lines.filter { state(for: $0.id).selectedTake != nil }.count
        progressLabel.stringValue = "LINE \(index + 1) OF \(spec.lines.count)  ·  \(kept) KEPT"
        sceneLabel.stringValue = "\(line.id)  ·  \(line.title)"
        scriptLabel.stringValue = line.text
        if let image = line.image {
            let expanded = NSString(string: image).expandingTildeInPath
            let imageURL = expanded.hasPrefix("/")
                ? URL(fileURLWithPath: expanded)
                : specURL.deletingLastPathComponent().appendingPathComponent(expanded)
            imageView.image = NSImage(contentsOf: imageURL)
        } else {
            imageView.image = NarratorIcon.make(size: 400)
        }
        if let selected = lineState.selectedTake,
           let take = lineState.takes.last(where: { $0.path == selected }) {
            statusLabel.stringValue = String(format: "✓ kept take · %.1f sec · record again to replace it", take.duration)
        } else if lineState.takes.isEmpty {
            statusLabel.stringValue = "Read the line naturally. Record (R), Stop (S), Play (P), then Keep (Return)."
        } else {
            statusLabel.stringValue = "\(lineState.takes.count) unkept take\(lineState.takes.count == 1 ? "" : "s") · make or choose a take"
        }
        levelMeter.doubleValue = -55
        recordButton.title = lineState.selectedTake == nil ? "● Record take" : "↺ Record replacement"
        backButton.isEnabled = index > 0
        nextButton.isEnabled = index + 1 < spec.lines.count
        setRecording(false)
        playButton.isEnabled = lineState.selectedTake != nil
        redoButton.isEnabled = true
        keepButton.isEnabled = lineState.selectedTake != nil
        keepButton.title = index + 1 == spec.lines.count ? "Keep & Finish ✓" : "Keep & Next →"
    }

    private func setRecording(_ active: Bool) {
        recordButton.isHidden = active
        stopButton.isHidden = !active
        backButton.isEnabled = !active && index > 0
        nextButton.isEnabled = !active && index + 1 < spec.lines.count
        playButton.isEnabled = !active && (pendingTake != nil || state(for: spec.lines[index].id).selectedTake != nil)
        redoButton.isEnabled = !active
        keepButton.isEnabled = !active && (pendingTake != nil || state(for: spec.lines[index].id).selectedTake != nil)
        inputPopup.isEnabled = !active
        refreshInputsButton.isEnabled = !active
    }

    @objc private func previousLine() { if index > 0 { index -= 1; showCurrent() } }
    @objc private func nextLine() { if index + 1 < spec.lines.count { index += 1; showCurrent() } }

    @objc private func recordTake() {
        stopPlayback()
        let begin = { [weak self] in DispatchQueue.main.async { self?.beginRecording() } }
        switch AVCaptureDevice.authorizationStatus(for: .audio) {
        case .authorized: begin()
        case .notDetermined:
            AVCaptureDevice.requestAccess(for: .audio) { granted in
                if granted { begin() } else { self.showMicDenied() }
            }
        default: showMicDenied()
        }
    }

    private func showMicDenied() {
        DispatchQueue.main.async {
            let alert = NSAlert()
            alert.messageText = "Microphone access is off"
            alert.informativeText = "Enable microphone access for Narrator Wizard (or Terminal) in System Settings → Privacy & Security → Microphone."
            alert.runModal()
        }
    }

    private struct InputDevice {
        let id: AudioDeviceID
        let name: String
    }

    private func inputDevices() -> [InputDevice] {
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDevices,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain
        )
        var byteCount: UInt32 = 0
        guard AudioObjectGetPropertyDataSize(AudioObjectID(kAudioObjectSystemObject), &address, 0, nil, &byteCount) == noErr else { return [] }
        var ids = [AudioDeviceID](repeating: 0, count: Int(byteCount) / MemoryLayout<AudioDeviceID>.size)
        guard AudioObjectGetPropertyData(AudioObjectID(kAudioObjectSystemObject), &address, 0, nil, &byteCount, &ids) == noErr else { return [] }
        return ids.compactMap { id in
            var streams = AudioObjectPropertyAddress(
                mSelector: kAudioDevicePropertyStreams,
                mScope: kAudioDevicePropertyScopeInput,
                mElement: kAudioObjectPropertyElementMain
            )
            var streamBytes: UInt32 = 0
            guard AudioObjectGetPropertyDataSize(id, &streams, 0, nil, &streamBytes) == noErr, streamBytes > 0 else { return nil }
            var unmanagedName: Unmanaged<CFString>?
            var nameSize = UInt32(MemoryLayout<Unmanaged<CFString>?>.size)
            var nameAddress = AudioObjectPropertyAddress(
                mSelector: kAudioObjectPropertyName,
                mScope: kAudioObjectPropertyScopeGlobal,
                mElement: kAudioObjectPropertyElementMain
            )
            guard AudioObjectGetPropertyData(id, &nameAddress, 0, nil, &nameSize, &unmanagedName) == noErr,
                  let name = unmanagedName?.takeUnretainedValue() else { return nil }
            return InputDevice(id: id, name: name as String)
        }.sorted { $0.name.localizedCaseInsensitiveCompare($1.name) == .orderedAscending }
    }

    private func defaultInputID() -> AudioDeviceID? {
        var id = AudioDeviceID(0)
        var size = UInt32(MemoryLayout<AudioDeviceID>.size)
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDefaultInputDevice,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain
        )
        guard AudioObjectGetPropertyData(AudioObjectID(kAudioObjectSystemObject), &address, 0, nil, &size, &id) == noErr else { return nil }
        return id
    }

    private func loadInputDevices(prefer id: AudioDeviceID? = nil) {
        let devices = inputDevices()
        let selected = id ?? defaultInputID()
        inputPopup.removeAllItems()
        for device in devices {
            inputPopup.addItem(withTitle: device.name)
            inputPopup.lastItem?.representedObject = NSNumber(value: device.id)
        }
        if let selected, let item = inputPopup.itemArray.first(where: { ($0.representedObject as? NSNumber)?.uint32Value == selected }) {
            inputPopup.select(item)
        }
        inputPopup.toolTip = devices.isEmpty ? "No audio inputs found" : "The selected device becomes macOS's default input for this recording session."
    }

    @objc private func refreshInputs() {
        loadInputDevices()
        if monitorCheckbox.state == .on { restartMonitoring() }
    }

    @objc private func inputChanged() {
        guard let id = (inputPopup.selectedItem?.representedObject as? NSNumber)?.uint32Value else { return }
        var selected = AudioDeviceID(id)
        let size = UInt32(MemoryLayout<AudioDeviceID>.size)
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDefaultInputDevice,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain
        )
        let status = AudioObjectSetPropertyData(AudioObjectID(kAudioObjectSystemObject), &address, 0, nil, size, &selected)
        if status == noErr {
            statusLabel.stringValue = "Input selected: \(inputPopup.titleOfSelectedItem ?? "microphone")"
            if monitorCheckbox.state == .on { restartMonitoring() }
        } else {
            statusLabel.stringValue = "Could not select that input (CoreAudio \(status))"
        }
    }

    @objc private func monitorChanged() {
        if monitorCheckbox.state == .on { startMonitoring() }
        else { stopMonitoring() }
    }

    private func restartMonitoring() {
        stopMonitoring()
        startMonitoring()
    }

    private func startMonitoring() {
        let engine = AVAudioEngine()
        let input = engine.inputNode
        let format = input.outputFormat(forBus: 0)
        guard format.channelCount > 0 else {
            monitorCheckbox.state = .off
            statusLabel.stringValue = "The selected input has no active channels."
            return
        }
        engine.connect(input, to: engine.mainMixerNode, format: format)
        engine.mainMixerNode.outputVolume = 0.72
        do {
            try engine.start()
            monitorEngine = engine
            statusLabel.stringValue = "Input monitoring is on — use headphones to prevent feedback."
        } catch {
            monitorCheckbox.state = .off
            statusLabel.stringValue = "Could not monitor input: \(error.localizedDescription)"
        }
    }

    private func stopMonitoring() {
        monitorEngine?.stop()
        monitorEngine = nil
    }

    private func restoreOriginalInput() {
        guard var originalInputID else { return }
        let size = UInt32(MemoryLayout<AudioDeviceID>.size)
        var address = AudioObjectPropertyAddress(
            mSelector: kAudioHardwarePropertyDefaultInputDevice,
            mScope: kAudioObjectPropertyScopeGlobal,
            mElement: kAudioObjectPropertyElementMain
        )
        _ = AudioObjectSetPropertyData(AudioObjectID(kAudioObjectSystemObject), &address, 0, nil, size, &originalInputID)
    }

    private func beginRecording() {
        let line = spec.lines[index]
        let si = stateIndex(for: line.id)
        let takeNumber = manifest.lines[si].takes.count + 1
        let lineDir = outURL.appendingPathComponent(line.id, isDirectory: true)
        try? FileManager.default.createDirectory(at: lineDir, withIntermediateDirectories: true)
        let path = lineDir.appendingPathComponent(String(format: "take-%03d.wav", takeNumber))
        let settings: [String: Any] = [
            AVFormatIDKey: kAudioFormatLinearPCM,
            AVSampleRateKey: 48_000,
            AVNumberOfChannelsKey: 1,
            AVLinearPCMBitDepthKey: 24,
            AVLinearPCMIsFloatKey: false,
            AVLinearPCMIsBigEndianKey: false,
        ]
        do {
            let recorder = try AVAudioRecorder(url: path, settings: settings)
            recorder.isMeteringEnabled = true
            recorder.prepareToRecord()
            guard recorder.record() else { throw CocoaError(.fileWriteUnknown) }
            self.recorder = recorder
            pendingTake = nil
            statusLabel.stringValue = "● recording \(line.id) — read the line, then press Stop (S)"
            statusLabel.textColor = .systemRed
            setRecording(true)
            meterTimer?.invalidate()
            meterTimer = Timer.scheduledTimer(withTimeInterval: 0.05, repeats: true) { [weak self] _ in
                guard let self, let recorder = self.recorder else { return }
                recorder.updateMeters()
                self.levelMeter.doubleValue = Double(recorder.averagePower(forChannel: 0))
            }
        } catch {
            statusLabel.stringValue = "Could not record: \(error.localizedDescription)"
            statusLabel.textColor = .systemRed
        }
    }

    @objc private func stopTake() {
        guard let recorder else { return }
        recorder.stop()
        meterTimer?.invalidate()
        meterTimer = nil
        self.recorder = nil
        let duration: Double
        if let file = try? AVAudioFile(forReading: recorder.url) {
            duration = Double(file.length) / file.fileFormat.sampleRate
        } else { duration = recorder.currentTime }
        let take = NarrationTake(path: recorder.url.path, duration: duration, recordedAt: Self.isoNow())
        let si = stateIndex(for: spec.lines[index].id)
        manifest.lines[si].takes.append(take)
        pendingTake = take
        persist()
        statusLabel.textColor = .secondaryLabelColor
        statusLabel.stringValue = String(format: "Take %d ready · %.1f sec · Play it, Keep it, or record another", manifest.lines[si].takes.count, duration)
        setRecording(false)
        recordButton.title = "↺ Record another"
        playButton.isEnabled = true
        keepButton.isEnabled = true
        playTake()
    }

    @objc private func playTake() {
        stopPlayback()
        let selected = pendingTake?.path ?? state(for: spec.lines[index].id).selectedTake
        guard let selected else { return }
        do {
            let player = try AVAudioPlayer(contentsOf: URL(fileURLWithPath: selected))
            player.delegate = self
            self.player = player
            player.play()
            statusLabel.stringValue = "▶ playing take"
        } catch {
            statusLabel.stringValue = "Could not play take: \(error.localizedDescription)"
        }
    }

    private func stopPlayback() {
        player?.stop()
        player = nil
    }

    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        statusLabel.stringValue = pendingTake == nil ? "✓ kept take" : "Take ready · Keep it or record another"
    }

    @objc private func keepAndNext() {
        let si = stateIndex(for: spec.lines[index].id)
        if let pendingTake { manifest.lines[si].selectedTake = pendingTake.path }
        guard manifest.lines[si].selectedTake != nil else { return }
        persist()
        if index + 1 < spec.lines.count {
            index += 1
            showCurrent()
        } else {
            finish()
        }
    }

    private func finish() {
        stopPlayback()
        persist()
        let kept = spec.lines.filter { state(for: $0.id).selectedTake != nil }.count
        sceneLabel.stringValue = kept == spec.lines.count ? "✓ Narration session complete" : "Narration session saved"
        scriptLabel.stringValue = kept == spec.lines.count
            ? "All \(kept) lines have kept takes. The manifest is ready for the video build."
            : "\(kept) of \(spec.lines.count) lines have kept takes. You can close now and resume from this screenplay later."
        imageView.image = NarratorIcon.make(size: 400)
        statusLabel.stringValue = manifestURL.path
        for button in [backButton, nextButton, recordButton, stopButton, playButton, redoButton, keepButton] { button.isHidden = true }
        revealButton.title = "Reveal completed session"
    }

    private func persist() {
        manifest.updatedAt = Self.isoNow()
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.prettyPrinted, .sortedKeys, .withoutEscapingSlashes]
        if let data = try? encoder.encode(manifest) {
            try? data.write(to: manifestURL, options: .atomic)
        }
    }

    @objc private func revealRecordings() {
        NSWorkspace.shared.activateFileViewerSelecting([manifestURL])
    }

    private func resolvedProjectURL(_ path: String) -> URL {
        let expanded = NSString(string: path).expandingTildeInPath
        return expanded.hasPrefix("/")
            ? URL(fileURLWithPath: expanded).standardizedFileURL
            : specURL.deletingLastPathComponent().appendingPathComponent(expanded).standardizedFileURL
    }

    @objc private func showCurrentVideo() {
        if let videoWindow {
            videoWindow.makeKeyAndOrderFront(nil)
            videoPlayer?.play()
            return
        }
        guard let path = spec.video else { return }
        let url = resolvedProjectURL(path)
        guard FileManager.default.fileExists(atPath: url.path) else {
            statusLabel.stringValue = "Current video is missing: \(url.path)"
            return
        }
        let player = AVPlayer(url: url)
        let availableHeight = (window?.screen ?? NSScreen.main)?.visibleFrame.height ?? 900
        let playerHeight = min(960, max(640, availableHeight - 80))
        let playerWidth = playerHeight * 9 / 16
        let playerSize = NSSize(width: playerWidth, height: playerHeight)
        let playerView = AVPlayerView(frame: NSRect(origin: .zero, size: playerSize))
        playerView.autoresizingMask = [.width, .height]
        playerView.player = player
        // Inline controls keep the playback timeline visibly distinct from
        // the main window's microphone input meter.
        playerView.controlsStyle = .inline
        playerView.videoGravity = .resizeAspectFill
        playerView.showsFullScreenToggleButton = true

        let preview = NSWindow(
            contentRect: NSRect(origin: .zero, size: playerSize),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered,
            defer: false
        )
        preview.title = "Current cut — \(spec.title)"
        preview.contentAspectRatio = NSSize(width: 9, height: 16)
        preview.contentMinSize = playerSize
        preview.contentView = playerView
        preview.setContentSize(playerSize)
        preview.isReleasedWhenClosed = false
        preview.center()
        window?.addChildWindow(preview, ordered: .above)
        videoWindow = preview
        videoPlayer = player
        videoCloseObserver = NotificationCenter.default.addObserver(
            forName: NSWindow.willCloseNotification,
            object: preview,
            queue: .main
        ) { [weak self] _ in
            self?.videoPlayer?.pause()
            self?.videoPlayer = nil
            self?.videoWindow = nil
            if let observer = self?.videoCloseObserver {
                NotificationCenter.default.removeObserver(observer)
                self?.videoCloseObserver = nil
            }
        }
        preview.makeKeyAndOrderFront(nil)
        player.play()
    }

    func windowWillClose(_ notification: Notification) {
        recorder?.stop()
        stopMonitoring()
        videoPlayer?.pause()
        videoWindow?.close()
        restoreOriginalInput()
        persist()
    }

    deinit {
        DistributedNotificationCenter.default().removeObserver(self)
        if let videoCloseObserver { NotificationCenter.default.removeObserver(videoCloseObserver) }
    }
}
