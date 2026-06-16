// ShotWizardController.swift — the wizard window: the storyboard as a
// living sequence. A bottom STRIP shows every shot in order with its real
// generated output as the thumbnail; click to select, ◀ ▶ to re-order the
// sequence (written straight back to board.json). The big player previews
// the selected shot (its clip, or its source still); ▶ play-all runs the
// whole board end-to-end so you can watch the storyboard before assembling.
// The right sidebar carries the selected shot's VO line, lane, source,
// motion prompt, and status, with ↻ generate / ⛭ assemble.
import AppKit
import AVFoundation

final class ShotWizardController: NSWindowController, NSWindowDelegate {
    let board: Board
    let jobs = JobRunner()
    var sel = 0

    var backdrop: BackdropView!
    var titleLabel: NSTextField!
    var totalLabel: NSTextField!
    var playAllButton: NSButton!
    var playerView: ScrubPlayerView!
    var stillView: NSImageView!
    var player: AVPlayer?
    var loopObserver: NSObjectProtocol?

    var stripScroll: NSScrollView!
    var stripStack: NSStackView!
    var moveLeftButton: NSButton!
    var moveRightButton: NSButton!

    // sidebar
    var idLabel: NSTextField!
    var laneLabel: NSTextField!
    var timeLabel: NSTextField!
    var voText: NSTextView!
    var voScroll: NSScrollView!
    var sourceThumb: NSImageView!
    var promptText: NSTextView!
    var promptScroll: NSScrollView!
    var statusLabel: NSTextField!
    var genButton: NSButton!
    var assembleButton: NSButton!
    var jobProgress: NSProgressIndicator!
    var jobLog: NSTextField!

    let sidebarW: CGFloat = 320
    let stripH: CGFloat = 110
    var thumbCache: [String: NSImage] = [:]

    init(board: Board) {
        self.board = board
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 1180, height: 760),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered, defer: false
        )
        window.title = "ShotWizard — \(board.title)"
        window.minSize = NSSize(width: 720, height: 480)
        window.center()
        super.init(window: window)
        window.delegate = self
        setupUI()
        relayout()
        buildStrip()
        select(0)
    }

    required init?(coder: NSCoder) { fatalError() }

    // ── UI construction ───────────────────────────────────────────────
    func setupUI() {
        guard let cv = window?.contentView else { return }
        cv.wantsLayer = true

        backdrop = BackdropView(frame: cv.bounds)
        backdrop.autoresizingMask = [.width, .height]
        cv.addSubview(backdrop)

        titleLabel = label(19, .semibold)
        titleLabel.stringValue = board.title
        totalLabel = label(12, .regular)
        totalLabel.textColor = .secondaryLabelColor
        totalLabel.alignment = .right
        let last = board.shots.last?.t1 ?? 0
        totalLabel.stringValue = String(format: "%d shots · %.0fs", board.shots.count, last)
        playAllButton = button("▶ play all", #selector(playAll))

        playerView = ScrubPlayerView(frame: .zero)
        stillView = NSImageView(frame: .zero)
        stillView.imageScaling = .scaleProportionallyDown
        stillView.wantsLayer = true
        stillView.layer?.backgroundColor = NSColor.black.cgColor
        stillView.layer?.cornerRadius = 8
        stillView.isHidden = true

        stripStack = NSStackView(frame: .zero)
        stripStack.orientation = .horizontal
        stripStack.alignment = .centerY
        stripStack.spacing = 8
        stripStack.edgeInsets = NSEdgeInsets(top: 6, left: 6, bottom: 6, right: 6)
        stripScroll = NSScrollView(frame: .zero)
        stripScroll.documentView = stripStack
        stripScroll.hasHorizontalScroller = true
        stripScroll.hasVerticalScroller = false
        stripScroll.drawsBackground = false
        stripScroll.wantsLayer = true
        stripScroll.layer?.cornerRadius = 8
        stripScroll.layer?.backgroundColor = NSColor.labelColor.withAlphaComponent(0.05).cgColor

        moveLeftButton = button("◀ move", #selector(moveShotLeft))
        moveRightButton = button("move ▶", #selector(moveShotRight))

        idLabel = label(15, .semibold)
        laneLabel = label(11, .semibold)
        laneLabel.textColor = .systemTeal
        timeLabel = label(11, .regular)
        timeLabel.textColor = .secondaryLabelColor
        timeLabel.font = NSFont.monospacedSystemFont(ofSize: 11, weight: .regular)

        voText = NSTextView(frame: .zero)
        voText.isEditable = false
        voText.drawsBackground = false
        voText.font = NSFont.systemFont(ofSize: 13)
        voText.textColor = .labelColor
        voText.textContainerInset = NSSize(width: 6, height: 6)
        voScroll = scroll(voText)

        sourceThumb = NSImageView(frame: .zero)
        sourceThumb.imageScaling = .scaleProportionallyUpOrDown
        sourceThumb.wantsLayer = true
        sourceThumb.layer?.cornerRadius = 6
        sourceThumb.layer?.masksToBounds = true
        sourceThumb.layer?.backgroundColor = NSColor.labelColor.withAlphaComponent(0.10).cgColor

        promptText = NSTextView(frame: .zero)
        promptText.isEditable = false
        promptText.drawsBackground = true
        promptText.backgroundColor = NSColor.labelColor.withAlphaComponent(0.07)
        promptText.textColor = .labelColor
        promptText.font = NSFont.systemFont(ofSize: 11)
        promptText.textContainerInset = NSSize(width: 6, height: 6)
        promptScroll = scroll(promptText)

        statusLabel = label(11, .semibold)
        genButton = button("↻ generate", #selector(generate))
        assembleButton = button("⛭ assemble", #selector(assemble))

        jobProgress = NSProgressIndicator(frame: .zero)
        jobProgress.style = .bar
        jobProgress.isIndeterminate = true
        jobProgress.isHidden = true
        jobLog = label(10, .regular)
        jobLog.textColor = .tertiaryLabelColor
        jobLog.font = NSFont.monospacedSystemFont(ofSize: 10, weight: .regular)
        jobLog.lineBreakMode = .byTruncatingHead

        let views: [NSView] = [
            titleLabel, totalLabel, playAllButton, playerView, stillView,
            stripScroll, moveLeftButton, moveRightButton, idLabel, laneLabel,
            timeLabel, voScroll, sourceThumb, promptScroll, statusLabel,
            genButton, assembleButton, jobProgress, jobLog,
        ]
        for v in views { cv.addSubview(v) }
    }

    private func label(_ size: CGFloat, _ weight: NSFont.Weight) -> NSTextField {
        let l = NSTextField(frame: .zero)
        l.isEditable = false; l.isBordered = false; l.isSelectable = false
        l.backgroundColor = .clear
        l.font = NSFont.systemFont(ofSize: size, weight: weight)
        return l
    }

    private func button(_ title: String, _ action: Selector) -> NSButton {
        let b = NSButton(frame: .zero)
        b.title = title; b.bezelStyle = .rounded
        b.target = self; b.action = action
        return b
    }

    private func scroll(_ doc: NSView) -> NSScrollView {
        let s = NSScrollView(frame: .zero)
        s.documentView = doc
        s.hasVerticalScroller = true
        s.drawsBackground = false
        s.wantsLayer = true
        s.layer?.cornerRadius = 6
        return s
    }

    // ── layout ────────────────────────────────────────────────────────
    func windowDidResize(_ notification: Notification) { relayout() }

    func relayout() {
        guard let cv = window?.contentView else { return }
        let W = cv.bounds.width, H = cv.bounds.height
        let pad: CGFloat = 16

        // header
        let headerY = H - 42
        titleLabel.frame = NSRect(x: pad, y: headerY, width: W - 2 * pad - 220, height: 28)
        playAllButton.frame = NSRect(x: W - pad - 110, y: headerY, width: 110, height: 28)
        totalLabel.frame = NSRect(x: W - pad - 110 - 150 - 8, y: headerY + 4, width: 150, height: 20)

        // bottom strip + move buttons
        let stripY = pad
        let moveW: CGFloat = 78
        moveLeftButton.frame = NSRect(x: pad, y: stripY + (stripH - 26) / 2, width: moveW, height: 26)
        moveRightButton.frame = NSRect(x: W - pad - moveW, y: stripY + (stripH - 26) / 2, width: moveW, height: 26)
        stripScroll.frame = NSRect(x: pad + moveW + 8, y: stripY,
                                   width: W - 2 * (pad + moveW + 8), height: stripH)

        // right sidebar
        let sx = W - pad - sidebarW
        let contentTop = headerY - 12
        let contentBottom = stripY + stripH + 12
        idLabel.frame = NSRect(x: sx, y: contentTop - 22, width: sidebarW, height: 22)
        laneLabel.frame = NSRect(x: sx, y: contentTop - 40, width: 90, height: 16)
        timeLabel.frame = NSRect(x: sx + 92, y: contentTop - 40, width: sidebarW - 92, height: 16)
        let voH: CGFloat = 86
        voScroll.frame = NSRect(x: sx, y: contentTop - 48 - voH, width: sidebarW, height: voH)
        let thumbH = sidebarW * 9 / 16
        sourceThumb.frame = NSRect(x: sx, y: contentTop - 56 - voH - thumbH, width: sidebarW, height: thumbH)
        // action row at the sidebar bottom
        genButton.frame = NSRect(x: sx, y: contentBottom, width: 150, height: 28)
        assembleButton.frame = NSRect(x: sx + 158, y: contentBottom, width: sidebarW - 158, height: 28)
        statusLabel.frame = NSRect(x: sx, y: contentBottom + 34, width: sidebarW, height: 16)
        jobProgress.frame = NSRect(x: sx, y: contentBottom + 54, width: sidebarW, height: 12)
        jobLog.frame = NSRect(x: sx, y: contentBottom + 70, width: sidebarW, height: 14)
        let promptTop = contentTop - 64 - voH - thumbH
        let promptBottom = contentBottom + 92
        promptScroll.frame = NSRect(x: sx, y: promptBottom,
                                    width: sidebarW, height: max(0, promptTop - promptBottom))

        // main preview fills the left of the sidebar, between header and strip
        let leftW = sx - pad - 14
        let availTop = headerY - 12
        let availBottom = stripY + stripH + 12
        let availH = max(80, availTop - availBottom)
        let pw = min(leftW, availH * 16 / 9)
        let ph = pw * 9 / 16
        let px = pad + (leftW - pw) / 2
        let frame = NSRect(x: px, y: availTop - ph, width: pw, height: ph)
        playerView.frame = frame
        stillView.frame = frame
    }

    // ── strip ─────────────────────────────────────────────────────────
    func buildStrip() {
        stripStack.arrangedSubviews.forEach { $0.removeFromSuperview() }
        for (i, shot) in board.shots.enumerated() {
            let b = NSButton(frame: NSRect(x: 0, y: 0, width: 150, height: 92))
            b.imagePosition = .imageOnly
            b.bezelStyle = .regularSquare
            b.isBordered = false
            b.image = thumbnail(for: shot)
            b.imageScaling = .scaleProportionallyUpOrDown
            b.tag = i
            b.target = self
            b.action = #selector(stripClicked(_:))
            b.toolTip = "\(i + 1). \(shot.id) · \(shot.lane.rawValue) · \(shot.effectiveStatus)"
            b.wantsLayer = true
            b.layer?.cornerRadius = 6
            b.layer?.masksToBounds = true
            b.layer?.borderWidth = i == sel ? 3 : 1.5
            b.layer?.borderColor = borderColor(for: shot, selected: i == sel).cgColor
            b.widthAnchor.constraint(equalToConstant: 150).isActive = true
            b.heightAnchor.constraint(equalToConstant: 92).isActive = true
            stripStack.addArrangedSubview(b)
        }
    }

    func borderColor(for shot: Shot, selected: Bool) -> NSColor {
        if selected { return .systemYellow }
        switch shot.effectiveStatus {
        case "done": return .systemGreen
        case "wip": return .systemOrange
        default: return NSColor.labelColor.withAlphaComponent(0.25)
        }
    }

    // Thumbnail: first frame of the clip, else the card/still, else a
    // lane-glyph placeholder. Cached by file path.
    func thumbnail(for shot: Shot) -> NSImage {
        let size = NSSize(width: 150, height: 92)
        if let url = board.previewURL(shot) {
            if let cached = thumbCache[url.path] { return cached }
            if url.pathExtension.lowercased() == "mp4", let img = frameImage(url, size: size) {
                thumbCache[url.path] = img; return img
            }
            if let img = NSImage(contentsOf: url) {
                thumbCache[url.path] = img; return img
            }
        }
        if let su = board.sourceURL(shot), let img = NSImage(contentsOf: su) { return img }
        return placeholder(shot, size: size)
    }

    func frameImage(_ url: URL, size: NSSize) -> NSImage? {
        let asset = AVURLAsset(url: url)
        let gen = AVAssetImageGenerator(asset: asset)
        gen.appliesPreferredTrackTransform = true
        gen.maximumSize = CGSize(width: 300, height: 300)
        let t = CMTime(seconds: min(1.0, asset.duration.seconds / 2), preferredTimescale: 600)
        guard let cg = try? gen.copyCGImage(at: t, actualTime: nil) else { return nil }
        return NSImage(cgImage: cg, size: size)
    }

    func placeholder(_ shot: Shot, size: NSSize) -> NSImage {
        let img = NSImage(size: size)
        img.lockFocus()
        NSColor.labelColor.withAlphaComponent(0.08).setFill()
        NSBezierPath(rect: NSRect(origin: .zero, size: size)).fill()
        let para = NSMutableParagraphStyle(); para.alignment = .center
        let s = "\(shot.lane.glyph)\n\(shot.lane.rawValue)"
        s.draw(in: NSRect(x: 0, y: 22, width: size.width, height: 50), withAttributes: [
            .font: NSFont.systemFont(ofSize: 15, weight: .semibold),
            .foregroundColor: NSColor.secondaryLabelColor,
            .paragraphStyle: para,
        ])
        img.unlockFocus()
        return img
    }

    @objc func stripClicked(_ sender: NSButton) { select(sender.tag) }

    // ── selection / preview ───────────────────────────────────────────
    func select(_ i: Int) {
        guard board.shots.indices.contains(i) else { return }
        sel = i
        let shot = board.shots[i]
        idLabel.stringValue = "\(i + 1). \(shot.id)"
        laneLabel.stringValue = "\(shot.lane.glyph) \(shot.lane.rawValue)"
        timeLabel.stringValue = String(format: "%.1f–%.1fs (%.1fs)", shot.t0, shot.t1, shot.dur)
        voText.string = shot.vo ?? "—"
        promptText.string = shot.prompt ?? "(no motion prompt)"
        sourceThumb.image = board.sourceURL(shot).flatMap { NSImage(contentsOf: $0) }
        let st = shot.effectiveStatus
        statusLabel.stringValue = "status: \(st)"
        statusLabel.textColor = st == "done" ? .systemGreen : (st == "wip" ? .systemOrange : .secondaryLabelColor)
        loadPreview(shot)
        buildStrip()
        relayout()
    }

    func loadPreview(_ shot: Shot) {
        if let obs = loopObserver { NotificationCenter.default.removeObserver(obs); loopObserver = nil }
        // a rendered clip → play it (looped); else show the still/card/source
        if let url = board.previewURL(shot), url.pathExtension.lowercased() == "mp4" {
            stillView.isHidden = true
            playerView.isHidden = false
            let item = AVPlayerItem(url: url)
            let p = AVPlayer(playerItem: item)
            p.isMuted = true
            loopObserver = NotificationCenter.default.addObserver(
                forName: .AVPlayerItemDidPlayToEndTime, object: item, queue: .main
            ) { [weak p] _ in p?.seek(to: .zero); p?.play() }
            player = p
            playerView.player = p
            p.play()
        } else {
            player = nil
            playerView.player = nil
            playerView.isHidden = true
            stillView.isHidden = false
            stillView.image = board.previewURL(shot).flatMap { NSImage(contentsOf: $0) }
                ?? board.sourceURL(shot).flatMap { NSImage(contentsOf: $0) }
                ?? placeholder(shot, size: NSSize(width: 640, height: 360))
        }
    }

    // ── play the whole board end-to-end ───────────────────────────────
    @objc func playAll() {
        if let obs = loopObserver { NotificationCenter.default.removeObserver(obs); loopObserver = nil }
        let items: [AVPlayerItem] = board.shots.compactMap { shot in
            guard let u = board.previewURL(shot), u.pathExtension.lowercased() == "mp4" else { return nil }
            return AVPlayerItem(url: u)
        }
        guard !items.isEmpty else { status("no rendered clips yet — generate some shots first"); return }
        stillView.isHidden = true
        playerView.isHidden = false
        let q = AVQueuePlayer(items: items)
        q.isMuted = false
        // try to lay the VO under the run, if present
        if let vo = board.voAudioURL { attachVO(vo, to: q) }
        player = q
        playerView.player = q
        q.play()
        status("playing all \(items.count) rendered shots…")
    }

    var voPlayer: AVAudioPlayer?
    func attachVO(_ url: URL, to q: AVQueuePlayer) {
        voPlayer?.stop()
        guard let vp = try? AVAudioPlayer(contentsOf: url) else { return }
        voPlayer = vp
        vp.play()
    }

    // ── reorder (the core "line up the sequence" gesture) ─────────────
    @objc func moveShotLeft() {
        guard sel > 0 else { return }
        board.move(from: sel, to: sel - 1)
        sel -= 1
        buildStrip(); select(sel)
        status("moved ◀ — sequence saved")
    }

    @objc func moveShotRight() {
        guard sel < board.shots.count - 1 else { return }
        board.move(from: sel, to: sel + 1)
        sel += 1
        buildStrip(); select(sel)
        status("moved ▶ — sequence saved")
    }

    // ── generate / assemble via the board driver ──────────────────────
    @objc func generate() {
        guard !jobs.running else { return }
        guard let driver = board.driverURL else { status("no driver in board.json (add \"driver\")"); return }
        let shot = board.shots[sel]
        runJob(driver: driver, args: ["--shot", shot.id], verb: "generating \(shot.id)")
    }

    @objc func assemble() {
        guard !jobs.running else { return }
        guard let driver = board.driverURL else { status("no driver in board.json (add \"driver\")"); return }
        runJob(driver: driver, args: ["--assemble"], verb: "assembling")
    }

    func runJob(driver: URL, args: [String], verb: String) {
        genButton.isEnabled = false
        assembleButton.isEnabled = false
        jobProgress.isHidden = false
        jobProgress.startAnimation(nil)
        status(verb + " …")
        _ = jobs.run(driver: driver, args: args, cwd: board.baseDir,
                     onLine: { [weak self] chunk in
                         let line = chunk.split(separator: "\n").last.map(String.init) ?? chunk
                         self?.jobLog.stringValue = line.trimmingCharacters(in: .whitespaces)
                     },
                     onExit: { [weak self] code in
                         guard let self else { return }
                         self.jobProgress.stopAnimation(nil)
                         self.jobProgress.isHidden = true
                         self.genButton.isEnabled = true
                         self.assembleButton.isEnabled = true
                         self.thumbCache.removeAll()
                         self.board.reload()
                         self.status(code == 0 ? "\(verb) done ✓" : "\(verb) failed (exit \(code))")
                         self.buildStrip()
                         self.select(min(self.sel, self.board.shots.count - 1))
                     })
    }

    func status(_ s: String) { jobLog.stringValue = s }

    func windowWillClose(_ notification: Notification) {
        voPlayer?.stop()
        player?.pause()
    }
}
