// ClipWizardController.swift — the wizard window: step through the
// track's sections, audition every take (current + archived), see the
// exact INPUTS each shot was made from (panel, end-frame, prompt),
// pick the keeper, hear the section's slice of the actual track under
// the loop, re-roll with an editable prompt (or package the ask for
// Claude), and assemble the final cut.
//
// Resizable; manual frames recomputed in relayout(). Column plan:
//   header:  §index name · range            [◀] [▶]  n/m
//   left:    ScrubPlayerView (16:9, tap-drag scrub, bottom progress)
//            take chips + ♪ music
//            [↻ re-roll] [⛭ assemble] [▶ final]  status
//   right:   INPUTS sidebar — first panel, end panel (morphs), the
//            full prompt, duration/billing meta.
import AppKit
import AVFoundation

final class ClipWizardController: NSWindowController, NSWindowDelegate {
    let project: Project
    let jobs = JobRunner()
    var current = 0
    var previewingFinal = false
    var previewFile: String?

    var mascot: MascotView!
    var titleLabel: NSTextField!
    var progressLabel: NSTextField!
    var prevButton: NSButton!
    var nextButton: NSButton!
    var playerView: ScrubPlayerView!
    var player: AVPlayer?
    var loopObserver: NSObjectProtocol?
    var takesStrip: NSStackView!
    var musicButton: NSButton!
    var musicPlayer: AVAudioPlayer?
    var musicStopTimer: Timer?
    var rerollButton: NSButton!
    var assembleButton: NSButton!
    var finalButton: NSButton!
    var statusLabel: NSTextField!
    // inputs sidebar
    var inputsHeading: NSTextField!
    var panelThumb: NSImageView!
    var endThumb: NSImageView!
    var endThumbCaption: NSTextField!
    var promptScroll: NSScrollView!
    var promptText: NSTextView!
    var metaLabel: NSTextField!

    let sidebarW: CGFloat = 300

    init(project: Project) {
        self.project = project
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 1180, height: 760),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered, defer: false
        )
        window.title = "ClipWizard — \(project.slug)"
        window.appearance = NSAppearance(named: .darkAqua)
        // shrinks to a corner-of-screen monitor; rows fold away as the
        // window tightens (relayout breakpoints)
        window.minSize = NSSize(width: 360, height: 280)
        window.center()
        super.init(window: window)
        window.delegate = self
        setupUI()
        relayout()
        showSection(0)
    }

    required init?(coder: NSCoder) { fatalError() }

    // ── UI construction (frames assigned in relayout) ─────────────────
    func setupUI() {
        guard let cv = window?.contentView else { return }
        cv.wantsLayer = true

        mascot = MascotView(frame: cv.bounds)
        mascot.autoresizingMask = [.width, .height]
        cv.addSubview(mascot)

        titleLabel = label(size: 19, weight: .semibold)
        progressLabel = label(size: 12, weight: .regular)
        progressLabel.alignment = .right
        progressLabel.textColor = .secondaryLabelColor
        prevButton = pushButton("◀", action: #selector(prevSection))
        nextButton = pushButton("▶", action: #selector(nextSection))

        playerView = ScrubPlayerView(frame: .zero)

        takesStrip = NSStackView(frame: .zero)
        takesStrip.orientation = .horizontal
        takesStrip.alignment = .centerY
        takesStrip.spacing = 8
        musicButton = pushButton("♪ music", action: #selector(toggleMusic))

        rerollButton = pushButton("↻ re-roll ($)", action: #selector(reroll))
        assembleButton = pushButton("⛭ assemble cut", action: #selector(assemble))
        finalButton = pushButton("▶ final", action: #selector(previewFinal))
        statusLabel = label(size: 11, weight: .regular)
        statusLabel.textColor = .secondaryLabelColor
        statusLabel.font = NSFont.monospacedSystemFont(ofSize: 11, weight: .regular)
        statusLabel.lineBreakMode = .byTruncatingHead

        inputsHeading = label(size: 11, weight: .semibold)
        inputsHeading.stringValue = "INPUTS"
        inputsHeading.textColor = .tertiaryLabelColor
        panelThumb = thumb()
        endThumb = thumb()
        endThumbCaption = label(size: 10, weight: .regular)
        endThumbCaption.textColor = .tertiaryLabelColor
        metaLabel = label(size: 11, weight: .regular)
        metaLabel.font = NSFont.monospacedSystemFont(ofSize: 11, weight: .regular)
        metaLabel.textColor = .secondaryLabelColor
        metaLabel.maximumNumberOfLines = 2

        promptText = NSTextView(frame: .zero)
        promptText.isEditable = false
        promptText.drawsBackground = true
        promptText.backgroundColor = NSColor.black.withAlphaComponent(0.35)
        promptText.textColor = .labelColor
        promptText.font = NSFont.systemFont(ofSize: 12)
        promptText.textContainerInset = NSSize(width: 8, height: 8)
        promptText.autoresizingMask = [.width]
        promptScroll = NSScrollView(frame: .zero)
        promptScroll.documentView = promptText
        promptScroll.hasVerticalScroller = true
        promptScroll.drawsBackground = false
        promptScroll.wantsLayer = true
        promptScroll.layer?.cornerRadius = 6

        cv.addSubview(titleLabel)
        cv.addSubview(progressLabel)
        cv.addSubview(prevButton)
        cv.addSubview(nextButton)
        cv.addSubview(playerView)
        cv.addSubview(takesStrip)
        cv.addSubview(musicButton)
        cv.addSubview(rerollButton)
        cv.addSubview(assembleButton)
        cv.addSubview(finalButton)
        cv.addSubview(statusLabel)
        cv.addSubview(inputsHeading)
        cv.addSubview(panelThumb)
        cv.addSubview(endThumb)
        cv.addSubview(endThumbCaption)
        cv.addSubview(promptScroll)
        cv.addSubview(metaLabel)
    }

    private func label(size: CGFloat, weight: NSFont.Weight) -> NSTextField {
        let l = NSTextField(frame: .zero)
        l.isEditable = false; l.isBordered = false; l.isSelectable = false
        l.backgroundColor = .clear
        l.font = NSFont.systemFont(ofSize: size, weight: weight)
        return l
    }

    private func pushButton(_ title: String, action: Selector) -> NSButton {
        let b = NSButton(frame: .zero)
        b.title = title
        b.bezelStyle = .rounded
        b.target = self
        b.action = action
        return b
    }

    private func thumb() -> NSImageView {
        let iv = NSImageView(frame: .zero)
        iv.imageScaling = .scaleProportionallyUpOrDown
        iv.wantsLayer = true
        iv.layer?.cornerRadius = 6
        iv.layer?.masksToBounds = true
        iv.layer?.backgroundColor = NSColor.black.withAlphaComponent(0.3).cgColor
        return iv
    }

    // ── responsive layout ─────────────────────────────────────────────
    func windowDidResize(_ notification: Notification) { relayout() }

    func relayout() {
        guard let cv = window?.contentView else { return }
        let W = cv.bounds.width, H = cv.bounds.height

        // Breakpoints — rows and columns fold away as space tightens so
        // the player always wins:
        //   compact (<900 w): inputs sidebar folds, player full width
        //   narrow  (<660 w): action buttons compress to glyph labels
        //   short   (<480 h): takes strip + music fold (◀ ▶ still steps)
        let compact = W < 900
        let narrow = W < 660
        let short = H < 480
        let pad: CGFloat = narrow ? 10 : 18

        let effSidebarW: CGFloat = compact ? 0 : sidebarW
        let sidebarHidden = compact
        inputsHeading.isHidden = sidebarHidden
        panelThumb.isHidden = sidebarHidden
        promptScroll.isHidden = sidebarHidden
        metaLabel.isHidden = sidebarHidden
        endThumb.isHidden = sidebarHidden || endThumb.image == nil
        endThumbCaption.isHidden = sidebarHidden || endThumb.image == nil
        statusLabel.isHidden = W < 700
        takesStrip.isHidden = short
        musicButton.isHidden = short

        // header
        let headerH: CGFloat = narrow ? 30 : 34
        let headerY = H - headerH - 8
        let navW: CGFloat = narrow ? 36 : 44
        titleLabel.font = NSFont.systemFont(ofSize: narrow ? 14 : 19, weight: .semibold)
        titleLabel.frame = NSRect(x: pad, y: headerY,
                                  width: max(100, W - 2 * navW - 90 - 3 * pad), height: 28)
        progressLabel.frame = NSRect(x: W - pad - 60, y: headerY + 4, width: 60, height: 20)
        prevButton.frame = NSRect(x: W - pad - 60 - 2 * navW - 12, y: headerY, width: navW, height: 26)
        nextButton.frame = NSRect(x: W - pad - 60 - navW - 6, y: headerY, width: navW, height: 26)

        // bottom action row
        let barY: CGFloat = narrow ? 10 : 16
        let barH: CGFloat = narrow ? 26 : 30
        rerollButton.title = narrow ? "↻ $" : "↻ re-roll ($)"
        assembleButton.title = narrow ? "⛭ cut" : "⛭ assemble cut"
        finalButton.title = narrow ? "▶" : "▶ final"
        let bw: [CGFloat] = narrow ? [56, 70, 44] : [120, 140, 90]
        rerollButton.frame = NSRect(x: pad, y: barY, width: bw[0], height: barH)
        assembleButton.frame = NSRect(x: pad + bw[0] + 8, y: barY, width: bw[1], height: barH)
        finalButton.frame = NSRect(x: pad + bw[0] + bw[1] + 16, y: barY, width: bw[2], height: barH)
        let buttonsEnd = pad + bw[0] + bw[1] + bw[2] + 28

        // left column rows, bottom-up: bar → strip → player fills rest
        let leftW = W - effSidebarW - (compact ? 2 : 3) * pad
        let stripRowH: CGFloat = short ? 0 : 40
        let stripY = barY + barH + (short ? 4 : 8)
        if !short {
            takesStrip.frame = NSRect(x: pad, y: stripY, width: leftW - 116, height: 32)
            musicButton.frame = NSRect(x: pad + leftW - 104, y: stripY + 2, width: 104, height: 28)
        }
        statusLabel.frame = NSRect(x: buttonsEnd, y: barY + 2, width: max(0, leftW - buttonsEnd + pad), height: 26)

        let playerTop = headerY - 8
        let playerBottom = stripY + stripRowH
        let availH = max(80, playerTop - playerBottom)
        let playerW = min(leftW, availH * 16 / 9)
        let playerH = playerW * 9 / 16
        // center the player in the left column when height-constrained
        let px = pad + (leftW - playerW) / 2
        playerView.frame = NSRect(x: px, y: playerTop - playerH, width: playerW, height: playerH)

        // sidebar (right), top-down: heading → panel → end panel →
        // prompt fills the middle → meta pinned at the bar
        if !sidebarHidden {
            let sx = W - pad - sidebarW
            var y = headerY - 4
            inputsHeading.frame = NSRect(x: sx, y: y - 14, width: sidebarW, height: 14)
            y -= 22
            let thumbH = sidebarW * 2 / 3   // panels are 3:2
            panelThumb.frame = NSRect(x: sx, y: y - thumbH, width: sidebarW, height: thumbH)
            y -= thumbH + 6
            if !endThumb.isHidden {
                let endH = thumbH * 0.62
                endThumb.frame = NSRect(x: sx, y: y - endH, width: sidebarW * 0.62, height: endH)
                endThumbCaption.frame = NSRect(x: sx + sidebarW * 0.62 + 8, y: y - 20,
                                               width: sidebarW * 0.38 - 8, height: 16)
                y -= endH + 8
            }
            metaLabel.frame = NSRect(x: sx, y: barY, width: sidebarW, height: 32)
            promptScroll.frame = NSRect(x: sx, y: barY + 40, width: sidebarW,
                                        height: max(0, y - (barY + 48)))
        }
    }

    // ── section navigation + takes ────────────────────────────────────
    func showSection(_ idx: Int) {
        guard !project.sections.isEmpty else { return }
        current = max(0, min(project.sections.count - 1, idx))
        previewingFinal = false
        let s = project.sections[current]
        titleLabel.stringValue = String(format: "§%d %@ · %.2f–%.2fs (%.2fs)",
                                        s.index, s.name, s.start, s.end, s.exact)
        progressLabel.stringValue = "\(current + 1)/\(project.sections.count)"
        rebuildTakesStrip()
        showInputs(for: s)
        let target = s.takes.first(where: { $0.file == s.picked }) ?? s.takes.first
        loadTake(target)
        stopMusic()
    }

    func showInputs(for s: Section) {
        let info = project.shotInfo[s.name]
        panelThumb.image = info.flatMap { NSImage(contentsOfFile: $0.image) }
        if let end = info?.endImage {
            endThumb.isHidden = false
            endThumbCaption.isHidden = false
            endThumb.image = NSImage(contentsOfFile: end)
            endThumbCaption.stringValue = "→ morph target\n(last frame)"
        } else {
            endThumb.isHidden = true
            endThumbCaption.isHidden = true
            endThumb.image = nil
        }
        promptText.string = info?.prompt ?? "(no shots.json — run the driver once, e.g. --dry-run, to emit the inputs manifest)"
        if let info {
            let cost = Double(info.dur) * (info.ratePerSec ?? 0.2419)
            metaLabel.stringValue = String(format: "%ds gen → trim %.2fs · %@ · ~$%.2f/roll",
                                           info.dur, info.exact, info.tier ?? "fast", cost)
        } else {
            metaLabel.stringValue = ""
        }
        relayout()
    }

    func rebuildTakesStrip() {
        takesStrip.arrangedSubviews.forEach { $0.removeFromSuperview() }
        guard !previewingFinal else { return }
        let s = project.sections[current]
        if s.takes.isEmpty {
            let l = label(size: 12, weight: .regular)
            l.stringValue = "no takes yet — ↻ re-roll to generate"
            l.textColor = .secondaryLabelColor
            takesStrip.addArrangedSubview(l)
            return
        }
        for t in s.takes {
            let isPicked = t.file == s.picked
            let isPreviewing = t.file == previewFile
            let b = NSButton(title: (isPicked ? "✓ " : "") + t.label, target: self,
                             action: #selector(takeChipClicked(_:)))
            b.bezelStyle = .rounded
            b.setButtonType(.momentaryPushIn)
            b.identifier = NSUserInterfaceItemIdentifier(t.file)
            if isPicked { b.contentTintColor = .systemGreen }
            else if isPreviewing { b.contentTintColor = .systemYellow }
            takesStrip.addArrangedSubview(b)
        }
        let hint = label(size: 10, weight: .regular)
        hint.stringValue = "click = preview · ⌥click = pick"
        hint.textColor = .tertiaryLabelColor
        takesStrip.addArrangedSubview(hint)
    }

    @objc func takeChipClicked(_ sender: NSButton) {
        guard let file = sender.identifier?.rawValue else { return }
        let s = project.sections[current]
        if NSEvent.modifierFlags.contains(.option) {
            project.pick(section: s.name, file: file)
            status("picked \(file) for \(s.name)")
        }
        loadTake(project.sections[current].takes.first(where: { $0.file == file }))
        rebuildTakesStrip()
    }

    func loadTake(_ take: Take?) {
        if let obs = loopObserver { NotificationCenter.default.removeObserver(obs) }
        loopObserver = nil
        guard let take else { previewFile = nil; playerView.player = nil; player = nil; return }
        previewFile = take.file
        let item = AVPlayerItem(url: take.url)
        let p = AVPlayer(playerItem: item)
        p.isMuted = true
        loopObserver = NotificationCenter.default.addObserver(
            forName: .AVPlayerItemDidPlayToEndTime, object: item, queue: .main
        ) { [weak p] _ in
            p?.seek(to: .zero)
            p?.play()
        }
        player = p
        playerView.player = p
        p.play()
        rebuildTakesStrip()
    }

    @objc func prevSection() { showSection(current - 1) }
    @objc func nextSection() { showSection(current + 1) }

    // ── music slice ───────────────────────────────────────────────────
    @objc func toggleMusic() {
        if musicPlayer?.isPlaying == true { stopMusic(); return }
        let s = project.sections[current]
        guard let mp = try? AVAudioPlayer(contentsOf: project.audioURL) else {
            status("audio missing: \(project.audioURL.lastPathComponent)"); return
        }
        musicPlayer = mp
        mp.currentTime = s.start
        player?.seek(to: .zero)
        player?.play()
        mp.play()
        musicButton.title = "■ music"
        musicStopTimer = Timer.scheduledTimer(withTimeInterval: s.exact, repeats: false) { [weak self] _ in
            self?.stopMusic()
        }
    }

    func stopMusic() {
        musicStopTimer?.invalidate()
        musicStopTimer = nil
        musicPlayer?.stop()
        musicPlayer = nil
        musicButton.title = "♪ music"
    }

    // ── re-roll: editable prompt, or package the ask for Claude ───────
    @objc func reroll() {
        guard !jobs.running else { return }
        let s = project.sections[current]
        let info = project.shotInfo[s.name]

        let alert = NSAlert()
        alert.messageText = "Re-roll \(s.name)"
        let cost = info.map { Double($0.dur) * ($0.ratePerSec ?? 0.2419) } ?? 0
        alert.informativeText = String(format:
            "Edit the prompt below, then generate (~$%.2f — the current take is archived, never deleted). Or copy the shot context to ask Claude for a rewrite.", cost)
        alert.addButton(withTitle: "Re-roll ($)")
        alert.addButton(withTitle: "Copy ask-Claude")
        alert.addButton(withTitle: "Cancel")

        let editor = NSTextView(frame: NSRect(x: 0, y: 0, width: 460, height: 220))
        editor.string = info?.prompt ?? ""
        editor.font = NSFont.systemFont(ofSize: 12)
        editor.isEditable = true
        editor.textContainerInset = NSSize(width: 6, height: 6)
        editor.autoresizingMask = [.width]
        let scroll = NSScrollView(frame: NSRect(x: 0, y: 0, width: 460, height: 220))
        scroll.documentView = editor
        scroll.hasVerticalScroller = true
        alert.accessoryView = scroll

        let response = alert.runModal()
        let edited = editor.string.trimmingCharacters(in: .whitespacesAndNewlines)
        if response == .alertFirstButtonReturn {
            var args = ["--only", s.name, "--force"]
            if !edited.isEmpty && edited != info?.prompt {
                args += ["--prompt", edited]
            }
            runJob(args: args, verb: "re-rolling \(s.name)")
        } else if response == .alertSecondButtonReturn {
            let ask = """
            Please re-roll the \(project.slug) motion shot "\(s.name)" (section §\(s.index), \
            \(String(format: "%.2f", s.exact))s). Current prompt:

            \(info?.prompt ?? "(unknown)")

            What I want changed:
            """
            NSPasteboard.general.clearContents()
            NSPasteboard.general.setString(ask, forType: .string)
            status("ask-Claude copied — paste it to Claude and describe the change")
        }
    }

    @objc func assemble() {
        guard !jobs.running else { return }
        runJob(args: ["--assemble"], verb: "assembling")
    }

    func runJob(args: [String], verb: String) {
        setActionsEnabled(false)
        status("\(verb) …")
        _ = jobs.run(driver: project.driverURL, args: args, cwd: project.repoRoot,
                     onLine: { [weak self] chunk in
                         let line = chunk.split(separator: "\n").last.map(String.init) ?? chunk
                         self?.status("\(verb) · \(line.trimmingCharacters(in: .whitespaces))")
                     },
                     onExit: { [weak self] code in
                         guard let self else { return }
                         self.setActionsEnabled(true)
                         self.project.rescan()
                         self.status(code == 0 ? "\(verb) done ✓" : "\(verb) failed (exit \(code)) — see terminal")
                         if code == 0 && args.contains("--assemble") {
                             self.previewFinal()
                         } else {
                             self.showSection(self.current)
                         }
                     })
    }

    func setActionsEnabled(_ on: Bool) {
        rerollButton.isEnabled = on
        assembleButton.isEnabled = on
    }

    // ── final preview (scrubbable, with sound) ────────────────────────
    @objc func previewFinal() {
        guard FileManager.default.fileExists(atPath: project.finalURL.path) else {
            status("no final cut yet — ⛭ assemble first"); return
        }
        stopMusic()
        previewingFinal = true
        if let obs = loopObserver { NotificationCenter.default.removeObserver(obs) }
        loopObserver = nil
        previewFile = nil
        let p = AVPlayer(url: project.finalURL)
        p.isMuted = false
        player = p
        playerView.player = p
        p.play()
        titleLabel.stringValue = "FINAL CUT — \(project.slug)"
        status("playing \(project.finalURL.lastPathComponent) — tap-drag to scrub")
        rebuildTakesStrip()
    }

    func status(_ s: String) { statusLabel.stringValue = s }

    func windowWillClose(_ notification: Notification) {
        stopMusic()
        player?.pause()
    }
}
