// JukeController.swift — the JukeWizard window.
//
//   left:  the queue (every track across every lane), each row showing
//          its stars + a 💬 count; click to load.
//   right: the track title, the waveform player (click=seek,
//          ⌥click=comment), a transport row, a star-rating row, a notes
//          box, and the timestamped comments list (click a comment to
//          jump there). Everything persists to <track>.juke.json.
//
// --watch <dir> arms the auto-pop: when a fresh audio file lands in a
// watched folder it's added to the queue and starts playing, window to
// front — so a new render announces itself.
import AppKit

// the JukeWizard palette — pulled from the mascot illy: deep teal robe,
// buttery gold star, coral hat-band, warm cream ground.
enum Palette {
    static let teal   = NSColor(srgbRed: 0.10, green: 0.52, blue: 0.55, alpha: 1)
    static let gold   = NSColor(srgbRed: 0.95, green: 0.74, blue: 0.20, alpha: 1)
    static let coral  = NSColor(srgbRed: 0.95, green: 0.45, blue: 0.38, alpha: 1)
    static let cream  = NSColor(srgbRed: 0.99, green: 0.97, blue: 0.91, alpha: 1)
    static let inkDim = NSColor(srgbRed: 0.42, green: 0.40, blue: 0.36, alpha: 1)
    static func bg(_ dark: Bool) -> NSColor {
        dark ? NSColor(srgbRed: 0.10, green: 0.12, blue: 0.13, alpha: 1) : cream
    }
}

final class JukeController: NSWindowController, NSWindowDelegate,
                            NSTableViewDataSource, NSTableViewDelegate,
                            WaveformViewDelegate, NSTextViewDelegate {
    let library: Library
    let watchDirs: [String]
    let selectPath: String?
    var current: Int = -1
    var menuBar: MenuBarCD?
    var watchTimer: Timer?
    var watchMtimes: [String: Date] = [:]
    var keyMonitor: Any?

    // sort
    enum SortMode: Int, CaseIterable {
        case defaultOrder, newest, oldest, stars, title, lane, bpm, duration
        var label: String {
            switch self {
            case .defaultOrder: return "status · recent"
            case .newest:       return "newest rendered"
            case .oldest:       return "oldest rendered"
            case .stars:        return "★ rating"
            case .title:        return "title A–Z"
            case .lane:         return "lane"
            case .bpm:          return "BPM"
            case .duration:     return "duration"
            }
        }
    }
    var sortMode: SortMode = .defaultOrder
    var sortPopup: NSPopUpButton!

    // release-link services (button per platform, shown when a URL exists)
    enum LinkService: Int, CaseIterable {
        case spotify, apple, youtube, distrokid
        var title: String {
            switch self {
            case .spotify:   return "♫ Spotify"
            case .apple:     return " Apple"
            case .youtube:   return "▶ YouTube"
            case .distrokid: return "◆ DistroKid"
            }
        }
        var color: NSColor {
            switch self {
            case .spotify:   return NSColor(srgbRed: 0.11, green: 0.73, blue: 0.33, alpha: 1)
            case .apple:     return NSColor(srgbRed: 0.98, green: 0.24, blue: 0.36, alpha: 1)
            case .youtube:   return NSColor(srgbRed: 0.90, green: 0.13, blue: 0.13, alpha: 1)
            case .distrokid: return Palette.inkDim
            }
        }
        func url(_ l: TrackLinks?) -> String? {
            switch self {
            case .spotify:   return l?.spotify
            case .apple:     return l?.apple
            case .youtube:   return l?.youtube
            case .distrokid: return l?.distrokid
            }
        }
    }

    // views
    var backdrop: BackdropView!
    var listTable: NSTableView!
    var titleLabel: NSTextField!
    var laneLabel: NSTextField!
    var artView: NSImageView!
    var linkButtons: [NSButton] = []
    var mediaButtons: [NSButton] = []
    var wave: WaveformView!
    var playButton: NSButton!
    var timeLabel: NSTextField!
    var commentNowButton: NSButton!
    var starButtons: [NSButton] = []
    var notesView: NSTextView!
    var notesScroll: NSScrollView!
    var commentsTable: NSTableView!
    var listScroll: NSScrollView!
    var commentsScroll: NSScrollView!

    let sidebarW: CGFloat = 290

    init(library: Library, watch: [String], select selectArg: String? = nil) {
        self.library = library
        self.watchDirs = watch
        self.selectPath = selectArg
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 1120, height: 720),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered, defer: false)
        window.title = "JukeWizard — \(library.tracks.count) tracks"
        window.isRestorable = false          // don't let AppKit re-select a stale row over our pick
        window.isReleasedWhenClosed = false  // keep it around so the menu-bar CD can reopen it
        window.minSize = NSSize(width: 720, height: 460)
        window.center()
        super.init(window: window)
        window.delegate = self
        setupUI()
        relayout()
        // the spinning-CD menu-bar presence (persists when the window is closed)
        menuBar = MenuBarCD()
        menuBar?.onClick = { [weak self] in self?.toggleWindowFromMenuBar() }
        // open on the requested track (and play it) if given; else the top.
        if let sp = selectPath {
            let want = URL(fileURLWithPath: (sp as NSString).expandingTildeInPath).standardizedFileURL.path
            if let idx = library.tracks.firstIndex(where: { $0.url.standardizedFileURL.path == want }) {
                select(idx, autoplay: true)
            } else if !library.tracks.isEmpty { select(0, autoplay: false) }
        } else if !library.tracks.isEmpty { select(0, autoplay: false) }
        armWatch()
        installKeyMonitor()
    }
    required init?(coder: NSCoder) { fatalError() }
    deinit { if let m = keyMonitor { NSEvent.removeMonitor(m) } }

    // ── keyboard control (yields to text editing) ────────────────────────
    private func installKeyMonitor() {
        keyMonitor = NSEvent.addLocalMonitorForEvents(matching: .keyDown) { [weak self] e in
            guard let self, self.window?.isKeyWindow == true else { return e }
            if let r = self.window?.firstResponder, r is NSText { return e }   // editing notes
            return self.handleKey(e) ? nil : e
        }
    }
    private func handleKey(_ e: NSEvent) -> Bool {
        switch e.keyCode {
        case 49: togglePlay(); return true                                  // space
        case 123: wave.seek(to: wave.currentTime - 5); return true          // ←  back 5s
        case 124: wave.seek(to: wave.currentTime + 5); return true          // →  fwd 5s
        case 126: prevTrack(); return true                                  // ↑  prev track
        case 125: nextTrack(); return true                                  // ↓  next track
        case 18, 19, 20, 21, 23:                                            // 1–5 stars
            let map: [UInt16: Int] = [18: 1, 19: 2, 20: 3, 21: 4, 23: 5]
            if let n = map[e.keyCode], let t = track {
                t.data.stars = n; renderStars(n); t.save()
                listTable.reloadData(forRowIndexes: IndexSet(integer: current), columnIndexes: IndexSet(integer: 0))
            }
            return true
        case 8: addCommentNow(); return true                               // c  comment @ now
        case 29: clearStarsClicked(); return true                          // 0  clear stars
        default: return false
        }
    }

    // ── construction ─────────────────────────────────────────────────────
    private func setupUI() {
        guard let content = window?.contentView else { return }
        content.wantsLayer = true
        applyThemeBackground()
        DistributedNotificationCenter.default().addObserver(
            forName: NSNotification.Name("AppleInterfaceThemeChangedNotification"),
            object: nil, queue: .main) { [weak self] _ in self?.applyThemeBackground() }

        // living video backdrop behind everything (never intercepts clicks;
        // falls back to the faded static illy until the loop is generated)
        backdrop = BackdropView(frame: content.bounds)
        backdrop.autoresizingMask = [.width, .height]
        content.addSubview(backdrop)

        sortPopup = NSPopUpButton(frame: .zero, pullsDown: false)
        sortPopup.addItems(withTitles: SortMode.allCases.map { "sort: \($0.label)" })
        sortPopup.selectItem(at: sortMode.rawValue)
        sortPopup.target = self
        sortPopup.action = #selector(sortChanged(_:))
        sortPopup.bezelStyle = .rounded
        sortPopup.controlSize = .small
        sortPopup.font = NSFont.systemFont(ofSize: 11)
        content.addSubview(sortPopup)

        listTable = NSTableView()
        let col = NSTableColumn(identifier: .init("track"))
        col.title = "queue"
        listTable.addTableColumn(col)
        listTable.headerView = nil
        listTable.rowHeight = 34
        listTable.dataSource = self
        listTable.delegate = self
        listTable.target = self
        listTable.action = #selector(listClicked)
        listScroll = NSScrollView()
        listScroll.documentView = listTable
        listScroll.hasVerticalScroller = true
        listScroll.borderType = .bezelBorder
        content.addSubview(listScroll)

        titleLabel = label("", size: 20, bold: true)
        laneLabel = label("", size: 11, color: .secondaryLabelColor)
        laneLabel.usesSingleLineMode = false
        laneLabel.maximumNumberOfLines = 2
        laneLabel.lineBreakMode = .byWordWrapping
        content.addSubview(titleLabel)
        content.addSubview(laneLabel)

        // cover art — square thumbnail top-right; click to open full size.
        artView = NSImageView()
        artView.imageScaling = .scaleProportionallyUpOrDown
        artView.wantsLayer = true
        artView.layer?.cornerRadius = 6
        artView.layer?.masksToBounds = true
        artView.layer?.borderWidth = 1
        artView.layer?.borderColor = Palette.inkDim.withAlphaComponent(0.4).cgColor
        artView.isHidden = true
        let artClick = NSClickGestureRecognizer(target: self, action: #selector(openArt))
        artView.addGestureRecognizer(artClick)
        content.addSubview(artView)

        // per-service release links (only the ones this track has are shown).
        for svc in LinkService.allCases {
            let b = NSButton(title: svc.title, target: self, action: #selector(linkClicked(_:)))
            b.bezelStyle = .inline
            b.tag = svc.rawValue
            b.contentTintColor = svc.color
            b.isHidden = true
            linkButtons.append(b)
            content.addSubview(b)
        }

        wave = WaveformView(frame: .zero)
        wave.delegate = self
        content.addSubview(wave)

        playButton = NSButton(title: "▶", target: self, action: #selector(togglePlay))
        playButton.bezelStyle = .rounded
        playButton.setButtonType(.momentaryPushIn)
        content.addSubview(playButton)
        let prev = NSButton(title: "⏮", target: self, action: #selector(prevTrack))
        prev.bezelStyle = .rounded; prev.tag = 1
        let next = NSButton(title: "⏭", target: self, action: #selector(nextTrack))
        next.bezelStyle = .rounded; next.tag = 2
        content.addSubview(prev); content.addSubview(next)
        transportExtra = [prev, next]
        commentNowButton = NSButton(title: "＋ comment @ now", target: self, action: #selector(addCommentNow))
        commentNowButton.bezelStyle = .rounded
        content.addSubview(commentNowButton)
        timeLabel = label("0:00 / 0:00", size: 12, color: .secondaryLabelColor)
        timeLabel.alignment = .right
        content.addSubview(timeLabel)

        for i in 1...5 {
            let b = NSButton(title: "☆", target: self, action: #selector(starClicked(_:)))
            b.tag = i
            b.isBordered = false
            b.font = NSFont.systemFont(ofSize: 22)
            b.contentTintColor = .systemYellow
            starButtons.append(b)
            content.addSubview(b)
        }
        clearStars = NSButton(title: "clear", target: self, action: #selector(clearStarsClicked))
        clearStars.bezelStyle = .inline
        content.addSubview(clearStars)

        notesView = NSTextView()
        notesView.isRichText = false
        notesView.font = NSFont.systemFont(ofSize: 13)
        notesView.delegate = self
        notesView.isAutomaticQuoteSubstitutionEnabled = false
        notesScroll = NSScrollView()
        notesScroll.documentView = notesView
        notesScroll.hasVerticalScroller = true
        notesScroll.borderType = .bezelBorder
        content.addSubview(notesScroll)
        notesPlaceholder = label("notes…", size: 12, color: .tertiaryLabelColor)
        content.addSubview(notesPlaceholder)

        commentsTable = NSTableView()
        let cc = NSTableColumn(identifier: .init("comment"))
        cc.title = "comments"
        commentsTable.addTableColumn(cc)
        commentsTable.headerView = nil
        commentsTable.rowHeight = 24
        commentsTable.dataSource = self
        commentsTable.delegate = self
        commentsTable.target = self
        commentsTable.doubleAction = #selector(commentDoubleClicked)
        commentsTable.action = #selector(commentClicked)
        commentsScroll = NSScrollView()
        commentsScroll.documentView = commentsTable
        commentsScroll.hasVerticalScroller = true
        commentsScroll.borderType = .bezelBorder
        content.addSubview(commentsScroll)
        commentsHeader = label("comments  ·  space play · ←→ seek · ↑↓ track · 1–5 stars · C comment", size: 11, color: .secondaryLabelColor)
        content.addSubview(commentsHeader)
        delCommentButton = NSButton(title: "– delete", target: self, action: #selector(deleteComment))
        delCommentButton.bezelStyle = .inline
        delCommentButton.contentTintColor = Palette.coral
        content.addSubview(delCommentButton)

        // colored controls — teal transport, gold rating, coral accents.
        playButton.contentTintColor = Palette.teal
        transportExtra.forEach { $0.contentTintColor = Palette.teal }
        commentNowButton.contentTintColor = Palette.coral
        clearStars.contentTintColor = Palette.inkDim
        titleLabel.textColor = Palette.teal
        laneLabel.textColor = Palette.coral
        commentsHeader.textColor = Palette.inkDim
        listTable.backgroundColor = .clear
        commentsTable.backgroundColor = .clear
        listTable.enclosingScrollView?.drawsBackground = false
        commentsTable.enclosingScrollView?.drawsBackground = false
        notesScroll.drawsBackground = false
        notesView.drawsBackground = false
    }

    private func applyThemeBackground() {
        let dark = window?.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        window?.backgroundColor = Palette.bg(dark)
        window?.contentView?.layer?.backgroundColor = Palette.bg(dark).withAlphaComponent(0.6).cgColor
    }

    var transportExtra: [NSButton] = []
    var clearStars: NSButton!
    var notesPlaceholder: NSTextField!
    var commentsHeader: NSTextField!
    var delCommentButton: NSButton!

    private func label(_ s: String, size: CGFloat, bold: Bool = false, color: NSColor = .labelColor) -> NSTextField {
        let l = NSTextField(labelWithString: s)
        l.font = bold ? NSFont.boldSystemFont(ofSize: size) : NSFont.systemFont(ofSize: size)
        l.textColor = color
        return l
    }

    // ── layout ───────────────────────────────────────────────────────────
    func windowDidResize(_ notification: Notification) { relayout() }

    private func relayout() {
        guard let content = window?.contentView else { return }
        let W = content.bounds.width, H = content.bounds.height
        let pad: CGFloat = 12
        // proportional, clamped — looks right tiny or huge.
        let sidebarW = max(200, min(340, W * 0.26))
        let sortH: CGFloat = 22
        sortPopup.frame = NSRect(x: pad, y: H - pad - sortH, width: sidebarW, height: sortH)
        listScroll.frame = NSRect(x: pad, y: pad, width: sidebarW, height: H - pad * 2 - sortH - 6)
        let rx = sidebarW + pad * 2
        let rw = W - rx - pad
        // cover art — square thumbnail pinned top-right of the right panel.
        let artSize: CGFloat = artView.isHidden ? 0 : min(84, max(56, rw * 0.14))
        let textW = rw - (artSize > 0 ? artSize + 10 : 0)
        if artSize > 0 {
            artView.frame = NSRect(x: rx + rw - artSize, y: H - pad - artSize, width: artSize, height: artSize)
        }
        var y = H - pad - 28
        titleLabel.frame = NSRect(x: rx, y: y, width: textW, height: 28)
        y -= 26
        // per-service link buttons flow left→right on their own row.
        var lx = rx
        for b in linkButtons where !b.isHidden {
            let bw = b.attributedTitle.size().width + 16
            b.frame = NSRect(x: lx, y: y, width: bw, height: 20)
            lx += bw + 6
        }
        let hasLinks = linkButtons.contains { !$0.isHidden }
        if hasLinks { y -= 24 }
        laneLabel.frame = NSRect(x: rx, y: y, width: textW, height: 28)
        y -= 8
        // media buttons (reels/stories) flow on their own row when present.
        if !mediaButtons.isEmpty {
            var mx = rx
            y -= 22
            for b in mediaButtons {
                let bw = min(160, b.attributedTitle.size().width + 16)
                if mx + bw > rx + rw { mx = rx; y -= 22 }   // wrap
                b.frame = NSRect(x: mx, y: y, width: bw, height: 20)
                mx += bw + 6
            }
            y -= 6
        }
        let waveH: CGFloat = max(110, min(195, H * 0.22))
        y -= waveH
        wave.frame = NSRect(x: rx, y: y, width: rw, height: waveH)
        y -= 8 + 30
        playButton.frame = NSRect(x: rx, y: y, width: 46, height: 30)
        transportExtra[0].frame = NSRect(x: rx + 52, y: y, width: 46, height: 30)
        transportExtra[1].frame = NSRect(x: rx + 104, y: y, width: 46, height: 30)
        commentNowButton.frame = NSRect(x: rx + 160, y: y, width: 170, height: 30)
        timeLabel.frame = NSRect(x: rx + rw - 150, y: y + 6, width: 150, height: 18)
        y -= 8 + 30
        for (i, b) in starButtons.enumerated() {
            b.frame = NSRect(x: rx + CGFloat(i) * 30, y: y, width: 30, height: 30)
        }
        clearStars.frame = NSRect(x: rx + 5 * 30 + 8, y: y + 4, width: 50, height: 22)
        y -= 8
        // comments block pinned to the bottom; notes fill the middle.
        let commentsH: CGFloat = max(120, (y - pad) * 0.42)
        let commentsTop = pad + commentsH
        delCommentButton.frame = NSRect(x: rx + rw - 70, y: pad + commentsH + 2, width: 70, height: 18)
        commentsHeader.frame = NSRect(x: rx, y: pad + commentsH + 2, width: rw - 75, height: 16)
        commentsScroll.frame = NSRect(x: rx, y: pad, width: rw, height: commentsH)
        let notesTop = y
        let notesBottom = commentsTop + 22
        let notesH = max(60, notesTop - notesBottom)
        notesScroll.frame = NSRect(x: rx, y: notesBottom, width: rw, height: notesH)
        notesPlaceholder.frame = NSRect(x: rx + 6, y: notesBottom + notesH - 20, width: 100, height: 16)
    }

    // ── menu-bar CD ────────────────────────────────────────────────────────
    // Keep the bar disc's tempo + spin in step with playback.
    private func refreshMenuBar() {
        menuBar?.setBPM(track?.meta?.bpm.map(Double.init))
        menuBar?.setPlaying(wave.isPlaying)
    }
    @objc private func toggleWindowFromMenuBar() {
        guard let w = window else { return }
        if w.isVisible && w.isKeyWindow {
            w.orderOut(nil)
        } else {
            w.makeKeyAndOrderFront(nil)
            NSApp.activate(ignoringOtherApps: true)
        }
    }

    // ── selection / playback ──────────────────────────────────────────────
    private var track: Track? { (current >= 0 && current < library.tracks.count) ? library.tracks[current] : nil }

    func select(_ i: Int, autoplay: Bool) {
        guard i >= 0, i < library.tracks.count else { return }
        commitNotes()
        current = i
        let t = library.tracks[i]
        titleLabel.stringValue = t.title
        laneLabel.stringValue = Self.metaLine(t)
        laneLabel.textColor = Self.statusColor(t.meta?.status)
        loadArt(t)
        loadLinks(t)
        buildMediaButtons(t)
        relayout()                    // links/media/art change the header height
        wave.load(url: t.url)
        wave.comments = t.data.comments
        notesView.string = t.data.notes
        notesPlaceholder.isHidden = !t.data.notes.isEmpty
        renderStars(t.data.stars)
        commentsTable.reloadData()
        listTable.selectRowIndexes(IndexSet(integer: i), byExtendingSelection: false)
        listTable.scrollRowToVisible(i)
        updateTime()
        if autoplay { wave.play(); playButton.title = "❚❚" } else { playButton.title = "▶" }
        refreshMenuBar()               // new tempo + play state → spin the bar CD
    }

    // ── sorting ────────────────────────────────────────────────────────────
    @objc private func sortChanged(_ sender: NSPopUpButton) {
        guard let m = SortMode(rawValue: sender.indexOfSelectedItem) else { return }
        sortMode = m
        applySort()
    }
    private static func statusRank(_ s: String?) -> Int {
        switch s {
        case "RELEASED": return 0
        case "MASTERING", "SUBMITTED": return 1
        case "RENDER": return 2
        case "WIP", "IDEA": return 3
        default: return 4
        }
    }
    private func applySort() {
        // keep the current track selected across the reorder (match by URL)
        let currentURL = track?.url.standardizedFileURL.path
        let mode = sortMode
        library.reorder { a, b in
            switch mode {
            case .defaultOrder:
                let ra = Self.statusRank(a.meta?.status), rb = Self.statusRank(b.meta?.status)
                if ra != rb { return ra < rb }
                return (a.meta?.updated ?? "") > (b.meta?.updated ?? "")   // recent first
            case .newest:
                return (a.meta?.updated ?? "") > (b.meta?.updated ?? "")
            case .oldest:
                return (a.meta?.updated ?? "") < (b.meta?.updated ?? "")
            case .stars:
                if a.data.stars != b.data.stars { return a.data.stars > b.data.stars }
                return (a.meta?.updated ?? "") > (b.meta?.updated ?? "")
            case .title:
                return a.title.localizedCaseInsensitiveCompare(b.title) == .orderedAscending
            case .lane:
                if a.lane != b.lane { return a.lane.localizedCaseInsensitiveCompare(b.lane) == .orderedAscending }
                return a.title.localizedCaseInsensitiveCompare(b.title) == .orderedAscending
            case .bpm:
                return (a.meta?.bpm ?? 0) > (b.meta?.bpm ?? 0)
            case .duration:
                return (a.meta?.durationSec ?? 0) > (b.meta?.durationSec ?? 0)
            }
        }
        listTable.reloadData()
        if let cu = currentURL,
           let idx = library.tracks.firstIndex(where: { $0.url.standardizedFileURL.path == cu }) {
            current = idx
            listTable.selectRowIndexes(IndexSet(integer: idx), byExtendingSelection: false)
            listTable.scrollRowToVisible(idx)
        }
    }

    @objc private func listClicked() {
        let r = listTable.clickedRow
        if r >= 0 { select(r, autoplay: true) }
    }
    @objc private func togglePlay() {
        wave.togglePlay()
        playButton.title = wave.isPlaying ? "❚❚" : "▶"
        refreshMenuBar()
    }
    @objc private func prevTrack() { if current > 0 { select(current - 1, autoplay: true) } }
    @objc private func nextTrack() { if current < library.tracks.count - 1 { select(current + 1, autoplay: true) } }

    // ── rating ─────────────────────────────────────────────────────────────
    private func renderStars(_ n: Int) {
        for (i, b) in starButtons.enumerated() { b.title = (i < n) ? "★" : "☆" }
    }
    @objc private func starClicked(_ sender: NSButton) {
        guard let t = track else { return }
        t.data.stars = (t.data.stars == sender.tag) ? sender.tag - 1 : sender.tag  // click same top star to step down
        renderStars(t.data.stars)
        t.save()
        listTable.reloadData(forRowIndexes: IndexSet(integer: current), columnIndexes: IndexSet(integer: 0))
    }
    @objc private func clearStarsClicked() {
        guard let t = track else { return }
        t.data.stars = 0; renderStars(0); t.save()
        listTable.reloadData(forRowIndexes: IndexSet(integer: current), columnIndexes: IndexSet(integer: 0))
    }

    // ── notes ────────────────────────────────────────────────────────────
    func textDidChange(_ notification: Notification) {
        notesPlaceholder.isHidden = !notesView.string.isEmpty
    }
    func textDidEndEditing(_ notification: Notification) { commitNotes() }
    private func commitNotes() {
        guard let t = track else { return }
        if t.data.notes != notesView.string { t.data.notes = notesView.string; t.save() }
    }

    // ── comments ───────────────────────────────────────────────────────────
    @objc private func addCommentNow() { promptComment(at: wave.currentTime) }
    func waveformRequestComment(at t: Double) { promptComment(at: t) }

    private func promptComment(at t: Double) {
        guard let tr = track else { return }
        let wasPlaying = wave.isPlaying
        wave.pause(); playButton.title = "▶"
        let a = NSAlert()
        a.messageText = "Comment @ \(JukeController.mmss(t))"
        a.addButton(withTitle: "Add"); a.addButton(withTitle: "Cancel")
        let field = NSTextField(frame: NSRect(x: 0, y: 0, width: 320, height: 24))
        field.placeholderString = "what about this moment?"
        a.accessoryView = field
        a.window.initialFirstResponder = field
        if a.runModal() == .alertFirstButtonReturn {
            let text = field.stringValue.trimmingCharacters(in: .whitespacesAndNewlines)
            if !text.isEmpty {
                tr.data.comments.append(Comment(t: t, text: text))
                tr.data.comments.sort { $0.t < $1.t }
                tr.save()
                wave.comments = tr.data.comments
                commentsTable.reloadData()
                listTable.reloadData(forRowIndexes: IndexSet(integer: current), columnIndexes: IndexSet(integer: 0))
            }
        }
        if wasPlaying { wave.play(); playButton.title = "❚❚" }
        refreshMenuBar()
    }
    @objc private func commentClicked() {
        guard let t = track else { return }
        let r = commentsTable.clickedRow
        if r >= 0 && r < t.data.comments.count { wave.seek(to: t.data.comments[r].t) }
    }
    @objc private func commentDoubleClicked() { commentClicked() }
    @objc private func deleteComment() {
        guard let t = track else { return }
        let r = commentsTable.selectedRow
        guard r >= 0 && r < t.data.comments.count else { return }
        t.data.comments.remove(at: r)
        t.save()
        wave.comments = t.data.comments
        commentsTable.reloadData()
        listTable.reloadData(forRowIndexes: IndexSet(integer: current), columnIndexes: IndexSet(integer: 0))
    }

    // ── waveform delegate ───────────────────────────────────────────────────
    func waveformDidFinish() { playButton.title = "▶"; nextTrack(); refreshMenuBar() }
    func waveformTick() { updateTime() }
    private func updateTime() {
        timeLabel.stringValue = "\(JukeController.mmss(wave.currentTime)) / \(JukeController.mmss(wave.duration))"
    }

    // ── tables ───────────────────────────────────────────────────────────────
    func numberOfRows(in tableView: NSTableView) -> Int {
        if tableView == listTable { return library.tracks.count }
        return track?.data.comments.count ?? 0
    }
    func tableView(_ tableView: NSTableView, objectValueFor tableColumn: NSTableColumn?, row: Int) -> Any? {
        if tableView == listTable {
            let t = library.tracks[row]
            let stars = String(repeating: "★", count: t.data.stars) + String(repeating: "☆", count: 5 - t.data.stars)
            let cc = t.data.comments.count
            let badge = cc > 0 ? "  💬\(cc)" : ""
            let st = t.meta?.status.map { " · \($0)" } ?? ""
            return "\(stars)  \(t.title)\n      \(t.lane)\(st)\(badge)"
        } else {
            guard let t = track, row < t.data.comments.count else { return "" }
            let c = t.data.comments[row]
            return "\(JukeController.mmss(c.t))  \(c.text)"
        }
    }

    // ── auto-pop watcher ─────────────────────────────────────────────────────
    private func armWatch() {
        guard !watchDirs.isEmpty else { return }
        // seed mtimes so only files that change AFTER launch trigger a pop.
        for d in watchDirs { for (p, m) in scan(d) { watchMtimes[p] = m } }
        watchTimer = Timer.scheduledTimer(withTimeInterval: 2.0, repeats: true) { [weak self] _ in self?.pollWatch() }
    }
    private func scan(_ dir: String) -> [(String, Date)] {
        let url = URL(fileURLWithPath: (dir as NSString).expandingTildeInPath)
        let items = (try? FileManager.default.contentsOfDirectory(
            at: url, includingPropertiesForKeys: [.contentModificationDateKey])) ?? []
        var out: [(String, Date)] = []
        for f in items where Library.audioExts.contains(f.pathExtension.lowercased()) {
            let m = (try? f.resourceValues(forKeys: [.contentModificationDateKey]))?.contentModificationDate ?? .distantPast
            out.append((f.standardizedFileURL.path, m))
        }
        return out
    }
    private func pollWatch() {
        for d in watchDirs {
            let laneName = URL(fileURLWithPath: (d as NSString).expandingTildeInPath).lastPathComponent
            for (p, m) in scan(d) {
                let prev = watchMtimes[p]
                if prev == nil || m > prev! {
                    watchMtimes[p] = m
                    if prev == nil && watchTimer == nil { continue }   // initial seed guard
                    popPlay(path: p, lane: laneName)
                }
            }
        }
    }
    private func popPlay(path: String, lane: String) {
        // Quietly fold new renders into the queue — NEVER switch the
        // current track, start playback, or steal focus (that broke the
        // flow). The new row just appears in the list; you choose when.
        let url = URL(fileURLWithPath: path)
        let here = url.standardizedFileURL.path
        let sel = listTable.selectedRow
        if library.tracks.contains(where: { $0.url.standardizedFileURL.path == here }) {
            listTable.reloadData()                 // refresh its sidecar/stars
        } else if library.addFile(url, lane: lane) != nil {
            window?.title = "JukeWizard — \(library.tracks.count) tracks"
            listTable.reloadData()
        }
        if sel >= 0 { listTable.selectRowIndexes(IndexSet(integer: sel), byExtendingSelection: false) }
    }

    // ── util ───────────────────────────────────────────────────────────────
    static func mmss(_ s: Double) -> String {
        guard s.isFinite && s >= 0 else { return "0:00" }
        let total = Int(s.rounded())
        return String(format: "%d:%02d", total / 60, total % 60)
    }

    // ── /pop library metadata display ────────────────────────────────────────
    static func statusColor(_ s: String?) -> NSColor {
        switch s {
        case "RELEASED": return NSColor.systemGreen
        case "MASTERING", "SUBMITTED": return Palette.teal
        case "RENDER": return Palette.gold
        case "WIP", "IDEA": return Palette.coral
        default: return Palette.inkDim
        }
    }
    static func bestLink(_ t: Track) -> String? {
        t.meta?.links?.spotify ?? t.meta?.links?.youtube ?? t.meta?.links?.apple ?? t.meta?.links?.distrokid
    }
    private static func ago(_ iso: String?) -> String? {
        guard let iso, let d = ISO8601DateFormatter().date(from: iso) else { return nil }
        let s = Date().timeIntervalSince(d)
        if s < 3600 { return "\(Int(s / 60))m ago" }
        if s < 86400 { return "\(Int(s / 3600))h ago" }
        if s < 86400 * 30 { return "\(Int(s / 86400))d ago" }
        return "\(Int(s / (86400 * 30)))mo ago"
    }
    private static func size(_ b: Int?) -> String? {
        guard let b else { return nil }
        return b >= 1_000_000 ? String(format: "%.1f MB", Double(b) / 1e6) : "\(b / 1000) KB"
    }
    // one secondary line: lane · backend · STATUS · updated · revisions · dur · bpm · key · size
    static func metaLine(_ t: Track) -> String {
        guard let m = t.meta else { return t.lane }
        var parts: [String] = [t.lane]
        if let b = m.backend { parts.append(b) }
        if let s = m.status { parts.append(s) }
        if let u = ago(m.updated) { parts.append("updated \(u)") }
        if let r = m.revisions { parts.append("\(r) rev") }
        if let d = m.durationSec { parts.append(mmss(d)) }
        if let bpm = m.bpm { parts.append("\(bpm) BPM") }
        if let k = m.key { parts.append(k) }
        if let sz = size(m.bytes) { parts.append(sz) }
        return parts.joined(separator: " · ")
    }
    // ── art + links + media ──────────────────────────────────────────────────
    private func loadArt(_ t: Track) {
        artView.image = nil
        artView.isHidden = true
        guard let p = t.meta?.art else { return }
        let url = URL(fileURLWithPath: (p as NSString).expandingTildeInPath)
        if let img = NSImage(contentsOf: url) {
            artView.image = img
            artView.isHidden = false
        }
    }
    private func loadLinks(_ t: Track) {
        for b in linkButtons {
            let svc = LinkService(rawValue: b.tag)!
            b.isHidden = (svc.url(t.meta?.links) == nil)
        }
    }
    private func buildMediaButtons(_ t: Track) {
        mediaButtons.forEach { $0.removeFromSuperview() }
        mediaButtons.removeAll()
        guard let media = t.meta?.media, !media.isEmpty else { return }
        for (i, m) in media.enumerated() {
            let name = URL(fileURLWithPath: m.path).deletingPathExtension().lastPathComponent
            // trim the track title prefix so the label reads "reel", "story"…
            var short = name
            if short.lowercased().hasPrefix(t.title.lowercased()) {
                short = String(short.dropFirst(t.title.count)).trimmingCharacters(in: CharacterSet(charactersIn: "-_ "))
            }
            if short.isEmpty { short = m.kind }
            let b = NSButton(title: "▶ \(short)", target: self, action: #selector(mediaClicked(_:)))
            b.bezelStyle = .inline
            b.tag = i
            b.contentTintColor = Palette.teal
            b.font = NSFont.systemFont(ofSize: 11)
            mediaButtons.append(b)
            window?.contentView?.addSubview(b)
        }
    }
    @objc private func linkClicked(_ sender: NSButton) {
        guard let t = track, let svc = LinkService(rawValue: sender.tag),
              let s = svc.url(t.meta?.links), let u = URL(string: s) else { return }
        NSWorkspace.shared.open(u)
    }
    @objc private func mediaClicked(_ sender: NSButton) {
        guard let t = track, let media = t.meta?.media,
              sender.tag < media.count else { return }
        let url = URL(fileURLWithPath: (media[sender.tag].path as NSString).expandingTildeInPath)
        NSWorkspace.shared.open(url)
    }
    @objc private func openArt() {
        guard let t = track, let p = t.meta?.art else { return }
        NSWorkspace.shared.open(URL(fileURLWithPath: (p as NSString).expandingTildeInPath))
    }
}
