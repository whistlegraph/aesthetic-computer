// AudioGroupPreview — open a GROUP of audio files as a wall of chromeless
// glass panels, grid-tiled, so "listen to these renders" is a glance plus a
// jukebox instead of a blind afplay. The ImageGroupPreview contract applied
// to sound: one panel per file showing cover art (ID3/M4A artwork when the
// file has it), title/artist/album metadata, the technical line (duration,
// sample rate, channels, format), and the decoded waveform with a live
// playhead.
//
// How it gets asked: `slab-audio` appends "path\tsession_id" lines to
// $SLAB_HOME/state/open-audio; the menubar's 2 s tick consumes the file on
// the main thread (tiny stat — no shell-outs, per slab-menubar-perf). A new
// group REPLACES the previous wall, matching slab-images.
//
// Playback is a QUEUE, not a chorus: exactly one panel plays at a time,
// starting with the first; when a file ends the next tile takes over. Click
// a tile to jump the queue there, space toggles pause, ←/→ step, Esc/⌘W
// dismisses a tile (dismissing the playing one advances). slab-afplay stays
// the headless hook-and-ding player; this is the *preview* instrument.
import AppKit
import AVFoundation

extension Paths {
    /// "path\tsession_id" per line; the whole file is one group, consumed
    /// each tick and shown as a tiled playing wall.
    static var audioGroupRequestFile: String { "\(slabHome)/state/open-audio" }
}

final class AudioGroupPreview: NSObject, AVAudioPlayerDelegate {
    static let shared = AudioGroupPreview()
    private var controllers: [AudioPanelController] = []
    private var player: AVAudioPlayer?
    private var activeIndex: Int = -1
    private var progressTimer: Timer?

    var openPaths: [String] { controllers.map { $0.path } }
    var isShowing: Bool { !controllers.isEmpty }

    /// Called from the main-thread side of AppDelegate.refresh() every tick.
    /// `emojiFor` maps the asking Claude session to its sticky TitleEmoji so
    /// each tile wears the mark of the prompt that asked.
    func consumeRequests(emojiFor: (String) -> String = { _ in "" }) {
        let file = Paths.audioGroupRequestFile
        guard FileManager.default.fileExists(atPath: file) else { return }
        let text = (try? String(contentsOfFile: file, encoding: .utf8)) ?? ""
        try? FileManager.default.removeItem(atPath: file)
        var entries: [(String, String)] = []
        for line in text.split(separator: "\n") {
            let parts = line.split(separator: "\t", maxSplits: 1, omittingEmptySubsequences: false)
            let path = (parts[0].trimmingCharacters(in: .whitespaces) as NSString).expandingTildeInPath
            let sid = parts.count > 1 ? parts[1].trimmingCharacters(in: .whitespaces) : ""
            if !path.isEmpty && FileManager.default.fileExists(atPath: path) {
                entries.append((path, sid.isEmpty ? "" : emojiFor(sid)))
            }
        }
        guard !entries.isEmpty else { return }
        openGroup(entries)
    }

    /// A new group replaces the previous wall (slab-images semantics).
    /// Entries are (path, emoji) pairs.
    func openGroup(_ entries: [(String, String)]) {
        closeAll()
        for (i, e) in entries.enumerated() {
            guard let c = AudioPanelController(
                path: e.0, emoji: e.1,
                onSelect: { [weak self] ctrl in self?.select(ctrl) },
                onClose: { [weak self] ctrl in self?.dropped(ctrl) })
            else { continue }
            c.indexBadge = "\(i + 1)/\(entries.count)"
            controllers.append(c)
        }
        guard !controllers.isEmpty else { return }
        layoutGrid()
        NSApp.activate(ignoringOtherApps: true)
        for c in controllers { c.orderFront() }
        play(index: 0)
        controllers.first?.focus()
    }

    // MARK: jukebox

    private func play(index: Int) {
        guard controllers.indices.contains(index) else { stop(); return }
        player?.stop()
        for (i, c) in controllers.enumerated() { c.setActive(i == index, playing: false) }
        activeIndex = index
        let path = controllers[index].path
        guard let p = try? AVAudioPlayer(contentsOf: URL(fileURLWithPath: path)) else {
            // Undecodable file: skip forward rather than wedging the queue.
            advance()
            return
        }
        player = p
        p.delegate = self
        p.play()
        controllers[index].setActive(true, playing: true)
        startProgressTimer()
    }

    private func advance() {
        let next = activeIndex + 1
        if controllers.indices.contains(next) { play(index: next) } else { stop() }
    }

    private func stop() {
        player?.stop()
        player = nil
        progressTimer?.invalidate()
        progressTimer = nil
        if controllers.indices.contains(activeIndex) {
            controllers[activeIndex].setActive(false, playing: false)
        }
        activeIndex = -1
    }

    private func select(_ ctrl: AudioPanelController) {
        guard let i = controllers.firstIndex(where: { $0 === ctrl }) else { return }
        if i == activeIndex { togglePause() } else { play(index: i) }
    }

    func togglePause() {
        guard let p = player, controllers.indices.contains(activeIndex) else { return }
        if p.isPlaying { p.pause() } else { p.play() }
        controllers[activeIndex].setActive(true, playing: p.isPlaying)
    }

    func step(_ delta: Int) {
        guard !controllers.isEmpty else { return }
        let base = activeIndex < 0 ? 0 : activeIndex
        let i = min(max(base + delta, 0), controllers.count - 1)
        if i != activeIndex { play(index: i) }
    }

    private func startProgressTimer() {
        progressTimer?.invalidate()
        progressTimer = Timer.scheduledTimer(withTimeInterval: 1.0 / 30.0, repeats: true) { [weak self] _ in
            guard let self = self, let p = self.player,
                  self.controllers.indices.contains(self.activeIndex) else { return }
            let dur = max(p.duration, 0.001)
            self.controllers[self.activeIndex].setProgress(p.currentTime / dur, time: p.currentTime)
        }
    }

    func audioPlayerDidFinishPlaying(_ player: AVAudioPlayer, successfully flag: Bool) {
        if controllers.indices.contains(activeIndex) {
            controllers[activeIndex].setProgress(1, time: player.duration)
        }
        advance()
    }

    // MARK: wall plumbing

    private func dropped(_ ctrl: AudioPanelController) {
        guard let i = controllers.firstIndex(where: { $0 === ctrl }) else { return }
        let wasActive = i == activeIndex
        controllers.remove(at: i)
        if wasActive {
            activeIndex -= 1 // advance() targets the tile that slid into place
            if controllers.isEmpty { stop() } else { advance() }
        } else if i < activeIndex {
            activeIndex -= 1
        }
    }

    /// Same grid as ImageGroupPreview (cols = ceil(sqrt n), top-to-bottom
    /// rows) but with a CAPPED cell: audio tiles are dashboards, not
    /// canvases, so a lone file opens as a card instead of swallowing the
    /// whole screen. Small walls center; big walls fill as before.
    private func layoutGrid() {
        let n = controllers.count
        guard n > 0 else { return }
        let screen = NSScreen.main?.visibleFrame
            ?? NSRect(x: 0, y: 0, width: 1440, height: 900)
        let cols = Int(ceil(Double(n).squareRoot()))
        let rows = Int(ceil(Double(n) / Double(cols)))
        let gap: CGFloat = 16
        let maxW: CGFloat = 560, maxH: CGFloat = 340
        let cellW = min((screen.width - gap * CGFloat(cols + 1)) / CGFloat(cols), maxW)
        let cellH = min((screen.height - gap * CGFloat(rows + 1)) / CGFloat(rows), maxH)
        let gridW = CGFloat(cols) * cellW + gap * CGFloat(cols - 1)
        let gridH = CGFloat(rows) * cellH + gap * CGFloat(rows - 1)
        let ox = screen.minX + (screen.width - gridW) / 2
        let oyTop = screen.maxY - (screen.height - gridH) / 2
        for (i, c) in controllers.enumerated() {
            let col = i % cols
            let row = i / cols
            let x = ox + CGFloat(col) * (cellW + gap)
            let yTop = oyTop - CGFloat(row) * (cellH + gap)
            c.fit(in: NSRect(x: x, y: yTop - cellH, width: cellW, height: cellH))
        }
    }

    func focus(_ path: String) { controllers.first(where: { $0.path == path })?.focus() }

    func closeAll() {
        stop()
        for c in Array(controllers) { c.close() }
        controllers.removeAll()
    }
}

/// One chromeless glass panel for one audio file: artwork (when embedded),
/// metadata stack, decoded waveform with playhead. Clicking anywhere selects
/// this tile in the queue (play / toggle pause).
private final class AudioPanelController: NSObject, NSWindowDelegate {
    let path: String
    private let emoji: String
    private let panel: AudioPanel
    private let onClose: (AudioPanelController) -> Void
    private let waveform = WaveformView()
    private let titleField = NSTextField(labelWithString: "")
    private let artistField = NSTextField(labelWithString: "")
    private let techField = NSTextField(labelWithString: "")
    private let timeField = NSTextField(labelWithString: "")
    private let stateDot = NSTextField(labelWithString: "")
    private let badge = NSTextField(labelWithString: "")
    private var artView: NSImageView?
    var indexBadge: String = "" { didSet { updateBadge() } }
    private var duration: Double = 0

    init?(path: String, emoji: String,
          onSelect: @escaping (AudioPanelController) -> Void,
          onClose: @escaping (AudioPanelController) -> Void) {
        self.path = path
        self.emoji = emoji
        self.onClose = onClose
        panel = AudioPanel(
            contentRect: NSRect(x: 0, y: 0, width: 520, height: 320),
            styleMask: [.titled, .closable, .resizable, .fullSizeContentView],
            backing: .buffered, defer: false)
        super.init()
        panel.onSelect = { [weak self] in if let self = self { onSelect(self) } }
        panel.titleVisibility = .hidden
        panel.titlebarAppearsTransparent = true
        panel.isMovableByWindowBackground = true
        panel.standardWindowButton(.miniaturizeButton)?.isHidden = true
        panel.standardWindowButton(.zoomButton)?.isHidden = true
        panel.hidesOnDeactivate = false
        panel.isReleasedWhenClosed = false
        panel.isRestorable = false
        panel.delegate = self
        panel.title = (path as NSString).lastPathComponent
        panel.isOpaque = false
        panel.backgroundColor = .clear
        panel.minSize = NSSize(width: 260, height: 170)

        let glass = NSVisualEffectView()
        glass.material = .hudWindow
        glass.blendingMode = .behindWindow
        glass.state = .active
        glass.wantsLayer = true
        glass.layer?.cornerRadius = 12
        panel.contentView = glass

        buildContent(in: glass)
        loadMetadata()
        loadWaveformAsync()
        setActive(false, playing: false)
    }

    // MARK: layout — [art?] / title / artist·album / tech / waveform / time

    private func buildContent(in content: NSView) {
        func style(_ f: NSTextField, size: CGFloat, weight: NSFont.Weight, color: NSColor) {
            f.font = .monospacedSystemFont(ofSize: size, weight: weight)
            f.textColor = color
            f.lineBreakMode = .byTruncatingMiddle
            f.translatesAutoresizingMaskIntoConstraints = false
        }
        style(titleField, size: 13, weight: .semibold, color: .labelColor)
        style(artistField, size: 11, weight: .regular, color: .secondaryLabelColor)
        style(techField, size: 10, weight: .regular, color: .tertiaryLabelColor)
        style(timeField, size: 10, weight: .regular, color: .secondaryLabelColor)
        style(stateDot, size: 12, weight: .bold, color: .labelColor)
        timeField.alignment = .right

        waveform.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(waveform)
        content.addSubview(titleField)
        content.addSubview(artistField)
        content.addSubview(techField)
        content.addSubview(timeField)
        content.addSubview(stateDot)

        // artwork slot fills whatever vertical room the metadata + waveform
        // leave; hidden (zero-height) until loadMetadata finds embedded art
        let art = NSImageView()
        art.imageScaling = .scaleProportionallyUpOrDown
        art.imageAlignment = .alignCenter
        art.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(art)
        artView = art

        NSLayoutConstraint.activate([
            art.topAnchor.constraint(equalTo: content.topAnchor, constant: 40),
            art.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
            art.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),

            titleField.topAnchor.constraint(equalTo: art.bottomAnchor, constant: 8),
            titleField.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
            titleField.trailingAnchor.constraint(equalTo: stateDot.leadingAnchor, constant: -8),
            stateDot.centerYAnchor.constraint(equalTo: titleField.centerYAnchor),
            stateDot.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),

            artistField.topAnchor.constraint(equalTo: titleField.bottomAnchor, constant: 2),
            artistField.leadingAnchor.constraint(equalTo: titleField.leadingAnchor),
            artistField.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),

            techField.topAnchor.constraint(equalTo: artistField.bottomAnchor, constant: 2),
            techField.leadingAnchor.constraint(equalTo: titleField.leadingAnchor),
            techField.trailingAnchor.constraint(equalTo: timeField.leadingAnchor, constant: -8),
            timeField.centerYAnchor.constraint(equalTo: techField.centerYAnchor),
            timeField.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),
            timeField.widthAnchor.constraint(greaterThanOrEqualToConstant: 96),

            waveform.topAnchor.constraint(equalTo: techField.bottomAnchor, constant: 8),
            waveform.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 16),
            waveform.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -16),
            waveform.bottomAnchor.constraint(equalTo: content.bottomAnchor, constant: -14),
            waveform.heightAnchor.constraint(greaterThanOrEqualToConstant: 44),
        ])
        installBadge(in: content)
    }

    private func installBadge(in content: NSView) {
        let chip = NSVisualEffectView()
        chip.material = .hudWindow
        chip.blendingMode = .withinWindow
        chip.state = .active
        chip.wantsLayer = true
        chip.layer?.cornerRadius = 8
        chip.translatesAutoresizingMaskIntoConstraints = false
        badge.font = .monospacedSystemFont(ofSize: 10.5, weight: .semibold)
        badge.textColor = .secondaryLabelColor
        badge.lineBreakMode = .byTruncatingMiddle
        badge.translatesAutoresizingMaskIntoConstraints = false
        badge.toolTip = path
        updateBadge()
        chip.addSubview(badge)
        content.addSubview(chip)
        NSLayoutConstraint.activate([
            chip.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 12),
            chip.topAnchor.constraint(equalTo: content.topAnchor, constant: 10),
            badge.leadingAnchor.constraint(equalTo: chip.leadingAnchor, constant: 10),
            badge.trailingAnchor.constraint(equalTo: chip.trailingAnchor, constant: -10),
            badge.centerYAnchor.constraint(equalTo: chip.centerYAnchor),
            badge.widthAnchor.constraint(lessThanOrEqualToConstant: 280),
            chip.heightAnchor.constraint(equalToConstant: 24),
        ])
    }

    private func updateBadge() {
        let name = (path as NSString).lastPathComponent
        let lead = emoji.isEmpty ? "" : emoji + " "
        let tail = indexBadge.isEmpty ? "" : "  ·  \(indexBadge)"
        badge.stringValue = lead + name + tail
    }

    // MARK: metadata — embedded tags + the technical line

    private func loadMetadata() {
        let url = URL(fileURLWithPath: path)
        let asset = AVURLAsset(url: url)
        var title = "", artist = "", album = ""
        var artwork: NSImage?
        for item in asset.commonMetadata {
            guard let key = item.commonKey else { continue }
            switch key {
            case .commonKeyTitle: title = item.stringValue ?? ""
            case .commonKeyArtist: artist = item.stringValue ?? ""
            case .commonKeyAlbumName: album = item.stringValue ?? ""
            case .commonKeyArtwork:
                if let data = item.dataValue { artwork = NSImage(data: data) }
            default: break
            }
        }
        titleField.stringValue = title.isEmpty
            ? (path as NSString).lastPathComponent : title
        let who = [artist, album].filter { !$0.isEmpty }.joined(separator: " — ")
        artistField.stringValue = who
        artistField.isHidden = who.isEmpty

        var tech: [String] = [(url.pathExtension.uppercased())]
        if let af = try? AVAudioFile(forReading: url) {
            duration = Double(af.length) / af.fileFormat.sampleRate
            tech.append("\(Int(af.fileFormat.sampleRate)) Hz")
            tech.append(af.fileFormat.channelCount == 1 ? "mono" : "\(af.fileFormat.channelCount)ch")
            if let bits = af.fileFormat.settings[AVLinearPCMBitDepthKey] as? Int {
                tech.append("\(bits)-bit")
            }
        } else {
            duration = asset.duration.seconds
        }
        techField.stringValue = tech.joined(separator: " · ")
        timeField.stringValue = "0:00 / " + Self.mmss(duration)

        if let art = artwork {
            artView?.image = art
        } else {
            // no embedded cover: give the waveform the vertical room instead
            artView?.isHidden = true
            let collapse = artView!.heightAnchor.constraint(equalToConstant: 0)
            collapse.priority = .required
            collapse.isActive = true
        }
    }

    /// Decode min/max peaks off-main so a wall of files doesn't stall the tick.
    private func loadWaveformAsync() {
        let p = path
        DispatchQueue.global(qos: .utility).async { [weak self] in
            guard let self = self else { return }
            let peaks = WaveformView.decodePeaks(path: p, bins: 480)
            DispatchQueue.main.async { self.waveform.peaks = peaks }
        }
    }

    // MARK: queue-facing state

    func setActive(_ active: Bool, playing: Bool) {
        stateDot.stringValue = active ? (playing ? "▶" : "⏸") : ""
        waveform.isActive = active
        panel.contentView?.layer?.borderWidth = active ? 2 : 0
        panel.contentView?.layer?.borderColor =
            NSColor.controlAccentColor.withAlphaComponent(0.8).cgColor
        if !active { setProgress(0, time: 0) }
    }

    func setProgress(_ fraction: Double, time: Double) {
        waveform.progress = CGFloat(min(max(fraction, 0), 1))
        timeField.stringValue = Self.mmss(time) + " / " + Self.mmss(duration)
    }

    private static func mmss(_ t: Double) -> String {
        guard t.isFinite, t >= 0 else { return "0:00" }
        return String(format: "%d:%02d", Int(t) / 60, Int(t) % 60)
    }

    // MARK: window plumbing

    func fit(in cell: NSRect) { panel.setFrame(cell, display: true) }
    func orderFront() { panel.orderFront(nil) }
    func focus() {
        NSApp.activate(ignoringOtherApps: true)
        panel.makeKeyAndOrderFront(nil)
    }
    func close() { panel.close() }
    func windowWillClose(_ notification: Notification) { onClose(self) }
}

/// The waveform strip: symmetric min/max columns, played portion in the
/// accent color, a playhead line while active.
private final class WaveformView: NSView {
    var peaks: [CGFloat] = [] { didSet { needsDisplay = true } }
    var progress: CGFloat = 0 { didSet { needsDisplay = true } }
    var isActive: Bool = false { didSet { needsDisplay = true } }

    override func draw(_ dirtyRect: NSRect) {
        guard !peaks.isEmpty else {
            NSColor.tertiaryLabelColor.setStroke()
            let mid = NSBezierPath()
            mid.move(to: NSPoint(x: 0, y: bounds.midY))
            mid.line(to: NSPoint(x: bounds.width, y: bounds.midY))
            mid.stroke()
            return
        }
        let n = peaks.count
        let w = bounds.width / CGFloat(n)
        let midY = bounds.midY
        let maxH = bounds.height / 2 - 1
        let playedX = progress * bounds.width
        for i in 0..<n {
            let x = CGFloat(i) * w
            let h = max(peaks[i] * maxH, 0.5)
            let played = x <= playedX && isActive
            (played ? NSColor.controlAccentColor
                    : NSColor.secondaryLabelColor.withAlphaComponent(0.55)).setFill()
            NSRect(x: x, y: midY - h, width: max(w - 1, 0.5), height: h * 2).fill()
        }
        if isActive {
            NSColor.controlAccentColor.setFill()
            NSRect(x: playedX - 0.5, y: 0, width: 1.5, height: bounds.height).fill()
        }
    }

    /// Chunked AVAudioFile read → per-bin absolute peaks, normalized to 1.
    /// The read buffer MUST use the file's own processingFormat — read(into:)
    /// throws on any channel-count mismatch, so a mono buffer against a stereo
    /// file decodes zero frames and the strip renders as a flat line.
    static func decodePeaks(path: String, bins: Int) -> [CGFloat] {
        guard let file = try? AVAudioFile(forReading: URL(fileURLWithPath: path)),
              file.length > 0
        else { return [] }
        let fmt = file.processingFormat // float32 deinterleaved, N channels
        let total = Int(file.length)
        let perBin = max(total / bins, 1)
        var out = [CGFloat](repeating: 0, count: bins)
        let chunk: AVAudioFrameCount = 65536
        guard let buf = AVAudioPCMBuffer(pcmFormat: fmt, frameCapacity: chunk) else { return [] }
        let channels = Int(fmt.channelCount)
        var frame = 0
        while frame < total {
            buf.frameLength = 0
            guard (try? file.read(into: buf, frameCount: chunk)) != nil, buf.frameLength > 0,
                  let data = buf.floatChannelData else { break }
            for i in 0..<Int(buf.frameLength) {
                let bin = min((frame + i) / perBin, bins - 1)
                var v: CGFloat = 0
                for ch in 0..<channels { v = max(v, CGFloat(abs(data[ch][i]))) }
                if v > out[bin] { out[bin] = v }
            }
            frame += Int(buf.frameLength)
        }
        let peak = out.max() ?? 0
        if peak > 0 { for i in 0..<bins { out[i] /= peak } }
        return out
    }
}

/// Key-able chromeless panel. Click selects the tile in the queue; space
/// toggles pause, ←/→ step the queue, Esc/⌘W dismisses the tile.
private final class AudioPanel: NSPanel {
    var onSelect: (() -> Void)?
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }
    override func cancelOperation(_ sender: Any?) { close() }
    override func mouseDown(with event: NSEvent) {
        onSelect?()
        super.mouseDown(with: event)
    }
    override func keyDown(with event: NSEvent) {
        switch event.charactersIgnoringModifiers {
        case " ": AudioGroupPreview.shared.togglePause()
        case String(UnicodeScalar(NSRightArrowFunctionKey)!): AudioGroupPreview.shared.step(1)
        case String(UnicodeScalar(NSLeftArrowFunctionKey)!): AudioGroupPreview.shared.step(-1)
        default: super.keyDown(with: event)
        }
    }
}

// MARK: - menu management (slab manages the viewers)

extension AppDelegate {
    @objc func focusAudio(_ sender: NSMenuItem) {
        if let path = sender.representedObject as? String { AudioGroupPreview.shared.focus(path) }
    }

    @objc func closeAllAudio() { AudioGroupPreview.shared.closeAll() }

    @objc func openAudioFromPanel() {
        NSApp.activate(ignoringOtherApps: true)
        let panel = NSOpenPanel()
        panel.allowedContentTypes = [.audio]
        panel.allowsMultipleSelection = true
        if panel.runModal() == .OK, !panel.urls.isEmpty {
            AudioGroupPreview.shared.openGroup(panel.urls.map { ($0.path, "") })
        }
    }
}
