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

final class ShotWizardController: NSWindowController, NSWindowDelegate, NSTextFieldDelegate {
    let board: Board
    let jobs = JobRunner()
    var sel = 0

    var backdrop: BackdropView!
    var titleLabel: NSTextField!
    var totalLabel: NSTextField!
    var playAllButton: NSButton!
    var navPrevButton: NSButton!     // browse ‹ (select previous shot)
    var navNextButton: NSButton!     // browse › (select next shot)
    var playerView: ScrubPlayerView!
    var stillView: NSImageView!
    var player: AVPlayer?
    var loopObserver: NSObjectProtocol?

    var stripScroll: NSScrollView!
    var strip: ThumbStripView!

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

    // review panel
    var scriptLabel: NSTextField!
    var refsLabel: NSTextField!
    var promptLabel: NSTextField!
    var refsRow: NSView!
    var refsScroll: NSScrollView!
    var approveButton: NSButton!
    var rejectButton: NSButton!
    var verdictLabel: NSTextField!
    var noteField: NSTextField!
    var regenButton: NSButton!

    let sidebarW: CGFloat = 360
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
        // ←/→ arrow keys NAVIGATE between shots (non-destructive). Reordering
        // is only the explicit "◀ reorder / reorder ▶" buttons. Skip while a
        // text field is being edited so arrows still move the caret there.
        NSEvent.addLocalMonitorForEvents(matching: .keyDown) { [weak self] e in
            guard let self else { return e }
            if self.window?.firstResponder is NSText { return e }
            if e.keyCode == 123 { self.select(max(0, self.sel - 1)); return nil }            // ←
            if e.keyCode == 124 { self.select(min(self.board.shots.count - 1, self.sel + 1)); return nil } // →
            return e
        }
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
        // Browse buttons — step the SELECTION through the shots (same as the
        // ←/→ arrow keys; non-destructive, never reorder).
        navPrevButton = button("‹ prev", #selector(navPrev))
        navNextButton = button("next ›", #selector(navNext))

        playerView = ScrubPlayerView(frame: .zero)
        stillView = NSImageView(frame: .zero)
        stillView.imageScaling = .scaleProportionallyDown
        stillView.wantsLayer = true
        stillView.layer?.backgroundColor = NSColor.black.cgColor
        stillView.layer?.cornerRadius = 8
        stillView.isHidden = true

        // The filmstrip: a thumbnail per shot, click to select, DRAG to
        // reorder. Lives inside a horizontal scroll view.
        strip = ThumbStripView(frame: .zero)
        strip.controller = self
        stripScroll = NSScrollView(frame: .zero)
        stripScroll.documentView = strip
        stripScroll.hasHorizontalScroller = true
        stripScroll.hasVerticalScroller = false
        stripScroll.drawsBackground = false
        stripScroll.wantsLayer = true
        stripScroll.layer?.cornerRadius = 8
        stripScroll.layer?.backgroundColor = NSColor.labelColor.withAlphaComponent(0.05).cgColor

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

        // ── review panel ──
        scriptLabel = sectionLabel("SCRIPT / CAPTIONS")
        refsLabel = sectionLabel("REFS (prompt inputs)")
        promptLabel = sectionLabel("PROMPT")
        refsRow = NSView(frame: NSRect(x: 0, y: 0, width: sidebarW, height: 60))
        refsRow.wantsLayer = true
        refsScroll = NSScrollView(frame: .zero)
        refsScroll.documentView = refsRow
        refsScroll.hasHorizontalScroller = true
        refsScroll.hasVerticalScroller = false
        refsScroll.drawsBackground = false
        refsScroll.wantsLayer = true
        refsScroll.layer?.cornerRadius = 6
        refsScroll.layer?.backgroundColor = NSColor.labelColor.withAlphaComponent(0.06).cgColor
        verdictLabel = label(12, .semibold)
        approveButton = button("✓ approve", #selector(approveShot))
        rejectButton = button("✗ reject", #selector(rejectShot))
        regenButton = button("↻ regenerate", #selector(regenShot))
        noteField = NSTextField(frame: .zero)
        noteField.placeholderString = "comment / reason…"
        noteField.font = NSFont.systemFont(ofSize: 12)
        noteField.delegate = self

        let views: [NSView] = [
            titleLabel, totalLabel, playAllButton, navPrevButton, navNextButton, playerView, stillView,
            stripScroll, idLabel, laneLabel, timeLabel,
            scriptLabel, voScroll, refsLabel, refsScroll, promptLabel, promptScroll,
            verdictLabel, approveButton, rejectButton, noteField,
            regenButton, assembleButton, statusLabel, jobProgress, jobLog,
        ]
        for v in views { cv.addSubview(v) }
    }

    private func sectionLabel(_ s: String) -> NSTextField {
        let l = label(10, .semibold)
        l.textColor = .secondaryLabelColor
        l.stringValue = s
        return l
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

        // header: title (left) · [‹ prev][next ›] · total · ▶ play all (right)
        let headerY = H - 42
        let navW: CGFloat = 64
        playAllButton.frame = NSRect(x: W - pad - 110, y: headerY, width: 110, height: 28)
        navNextButton.frame = NSRect(x: W - pad - 110 - 8 - navW, y: headerY, width: navW, height: 28)
        navPrevButton.frame = NSRect(x: navNextButton.frame.minX - 4 - navW, y: headerY, width: navW, height: 28)
        totalLabel.frame = NSRect(x: navPrevButton.frame.minX - 8 - 130, y: headerY + 4, width: 130, height: 20)
        titleLabel.frame = NSRect(x: pad, y: headerY, width: max(80, totalLabel.frame.minX - 2 * pad), height: 28)

        // bottom filmstrip — full width (drag a tile to reorder)
        let stripY = pad
        stripScroll.frame = NSRect(x: pad, y: stripY, width: W - 2 * pad, height: stripH)
        strip?.reload()

        // right sidebar — review inspector
        let sx = W - pad - sidebarW
        let contentTop = headerY - 12
        let contentBottom = stripY + stripH + 12

        // top-down: id · lane/time · SCRIPT · REFS · PROMPT(fills)
        idLabel.frame     = NSRect(x: sx, y: contentTop - 22, width: sidebarW, height: 22)
        laneLabel.frame   = NSRect(x: sx, y: contentTop - 40, width: 90, height: 16)
        timeLabel.frame   = NSRect(x: sx + 92, y: contentTop - 40, width: sidebarW - 92, height: 16)
        let voH: CGFloat = 72
        scriptLabel.frame = NSRect(x: sx, y: contentTop - 58, width: sidebarW, height: 14)
        voScroll.frame    = NSRect(x: sx, y: contentTop - 58 - voH, width: sidebarW, height: voH)
        let refsH: CGFloat = 60
        let refsLabY = contentTop - 58 - voH - 20
        refsLabel.frame   = NSRect(x: sx, y: refsLabY, width: sidebarW, height: 14)
        refsScroll.frame  = NSRect(x: sx, y: refsLabY - refsH, width: sidebarW, height: refsH)
        let promptLabY = refsLabY - refsH - 20
        promptLabel.frame = NSRect(x: sx, y: promptLabY, width: sidebarW, height: 14)

        // bottom-up: [regen | assemble] · note · [approve | reject  verdict] · status · progress · log
        let rowH: CGFloat = 28
        regenButton.frame    = NSRect(x: sx, y: contentBottom, width: 168, height: rowH)
        assembleButton.frame = NSRect(x: sx + 176, y: contentBottom, width: sidebarW - 176, height: rowH)
        noteField.frame      = NSRect(x: sx, y: contentBottom + 36, width: sidebarW, height: 24)
        approveButton.frame  = NSRect(x: sx, y: contentBottom + 68, width: 116, height: rowH)
        rejectButton.frame   = NSRect(x: sx + 122, y: contentBottom + 68, width: 104, height: rowH)
        verdictLabel.frame   = NSRect(x: sx + 232, y: contentBottom + 72, width: sidebarW - 232, height: 20)
        statusLabel.frame    = NSRect(x: sx, y: contentBottom + 102, width: sidebarW, height: 16)
        jobProgress.frame    = NSRect(x: sx, y: contentBottom + 122, width: sidebarW, height: 12)
        jobLog.frame         = NSRect(x: sx, y: contentBottom + 138, width: sidebarW, height: 14)

        // prompt fills the gap between PROMPT label and the bottom block
        let promptBottom = contentBottom + 158
        promptScroll.frame = NSRect(x: sx, y: promptBottom,
                                    width: sidebarW, height: max(40, promptLabY - promptBottom))

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
    func buildStrip() { strip?.reload() }

    func borderColor(for shot: Shot, selected: Bool) -> NSColor {
        if selected { return .systemYellow }
        if shot.approved == true { return .systemGreen }
        if shot.approved == false { return .systemRed }
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
        voText.string = (shot.vo?.isEmpty == false) ? shot.vo! : "—"
        // felt prompt + refs from the gen sidecar (felt-<beat>.meta.json)
        if let m = board.meta(shot) {
            promptText.string = m.prompt
            populateRefs(m.refs)
        } else {
            promptText.string = shot.prompt ?? "(no prompt sidecar yet — ↻ regenerate to capture it)"
            populateRefs([])
        }
        noteField.stringValue = shot.note ?? ""
        updateVerdict(shot)
        let st = shot.effectiveStatus
        statusLabel.stringValue = "status: \(st)"
        statusLabel.textColor = st == "done" ? .systemGreen : (st == "wip" ? .systemOrange : .secondaryLabelColor)
        loadPreview(shot)
        buildStrip()
        relayout()
    }

    func updateVerdict(_ shot: Shot) {
        switch shot.approved {
        case .some(true):  verdictLabel.stringValue = "✓ approved"; verdictLabel.textColor = .systemGreen
        case .some(false): verdictLabel.stringValue = "✗ rejected"; verdictLabel.textColor = .systemRed
        default:           verdictLabel.stringValue = "— unreviewed"; verdictLabel.textColor = .secondaryLabelColor
        }
    }

    /// Lay out the shot's ref thumbnails (screens + jeffrey) in the refs strip.
    func populateRefs(_ urls: [URL]) {
        refsRow.subviews.forEach { $0.removeFromSuperview() }
        let tw: CGFloat = 84, th: CGFloat = 52, gap: CGFloat = 6
        for (i, u) in urls.enumerated() {
            let iv = NSImageView(frame: NSRect(x: gap + CGFloat(i) * (tw + gap), y: 4, width: tw, height: th))
            iv.imageScaling = .scaleProportionallyUpOrDown
            iv.wantsLayer = true
            iv.layer?.cornerRadius = 4
            iv.layer?.masksToBounds = true
            iv.layer?.backgroundColor = NSColor.black.withAlphaComponent(0.25).cgColor
            iv.image = NSImage(contentsOf: u)
            iv.toolTip = u.lastPathComponent
            refsRow.addSubview(iv)
        }
        let total = gap + CGFloat(max(0, urls.count)) * (tw + gap)
        refsRow.frame = NSRect(x: 0, y: 0,
                               width: max(total, refsScroll.contentSize.width), height: 60)
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

    // ── browse (non-destructive: just move the selection) ─────────────
    @objc func navPrev() { select(max(0, sel - 1)) }
    @objc func navNext() { select(min(board.shots.count - 1, sel + 1)) }

    // ── reorder (drag-and-drop in the filmstrip) ──────────────────────
    func reorder(from: Int, to: Int) {
        guard from != to, board.shots.indices.contains(from), board.shots.indices.contains(to) else { return }
        board.move(from: from, to: to)
        select(to)                       // select() rebuilds the strip + preview
        status("reordered — sequence saved")
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

    // ── review: approve / reject / note / regenerate ──────────────────
    @objc func approveShot() {
        board.setApproved(sel, board.shots[sel].approved == true ? nil : true)
        updateVerdict(board.shots[sel]); buildStrip()
        status("\(board.shots[sel].id): \(board.shots[sel].approved == true ? "approved ✓" : "cleared")")
    }

    @objc func rejectShot() {
        board.setApproved(sel, board.shots[sel].approved == false ? nil : false)
        updateVerdict(board.shots[sel]); buildStrip()
        let rejected = board.shots[sel].approved == false
        status(rejected ? "\(board.shots[sel].id): rejected ✗ — add a reason, then ↻ regenerate"
                        : "\(board.shots[sel].id): cleared")
    }

    func controlTextDidEndEditing(_ obj: Notification) {
        guard (obj.object as? NSTextField) === noteField else { return }
        board.setNote(sel, noteField.stringValue)
    }

    @objc func regenShot() {
        guard !jobs.running else { return }
        let shot = board.shots[sel]
        guard let beat = shot.beat else { status("no felt beat for this shot (regen only works on felt cards)"); return }
        let gen = board.baseDir.appendingPathComponent("gen-felt.mjs")
        guard FileManager.default.fileExists(atPath: gen.path) else { status("gen-felt.mjs not found beside board.json"); return }
        var args = ["--only", beat, "--force"]
        if let n = shot.note, !n.isEmpty { args += ["--note", n] }   // fold reject reason into the prompt
        runJob(driver: gen, args: args, verb: "regenerating \(beat)")
    }

    func runJob(driver: URL, args: [String], verb: String) {
        genButton.isEnabled = false
        assembleButton.isEnabled = false
        regenButton.isEnabled = false
        regenButton.title = "⏳ regenerating…"
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
                         self.regenButton.isEnabled = true
                         self.regenButton.title = "↻ regenerate"
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
