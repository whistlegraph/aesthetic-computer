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
    var activityTimer: Timer?
    var activityPollInFlight = false
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

    // views — Winamp-style: now-playing header on top, track list underneath.
    var nowPlaying: NowPlayingMedia!
    var listTable: NSTableView!
    var titleLabel: NSTextField!
    var artistLabel: NSTextField!
    var laneLabel: NSTextField!
    var activityLabel: NSTextField!
    var linkButtons: [NSButton] = []
    var wave: WaveformView!
    var spotifyProgress: SpotifyProgressView!
    var spotifySearchField: NSSearchField!
    var sourceTabs: NSSegmentedControl!
    var appearanceTabs: NSSegmentedControl!
    var playButton: NSButton!
    var ledLabel: NSTextField!
    var notesToggle: NSButton!
    var roomButton: NSButton!
    var roomPopover: NSPopover?
    var roomMixer: RoomMixerView?
    var miniPopover: NSPopover?
    var miniPlayer: JukeMiniPlayerView?
    let roomAudio = JukeRoomAudio()
    let spotify = JukeSpotify()
    var spotifyMode = false
    var spotifyResults: [SpotifyTrackResult] = []
    var selectedSpotifyRow = -1
    var spotifyState: SpotifyPlaybackState?
    var spotifyArtworkURL: URL?
    var spotifyArt: NSImage?
    var drawerPanel: NSView!
    var drawerOpen = false
    var currentArt: NSImage?
    var quickVolume: Float = 0.8
    var commentNowButton: NSButton!
    var starButtons: [NSButton] = []
    var notesView: NSTextView!
    var notesScroll: NSScrollView!
    var commentsTable: NSTableView!
    var listScroll: NSScrollView!
    var commentsScroll: NSScrollView!

    let sidebarW: CGFloat = 290

    enum AppearanceMode: Int {
        case automatic, light, dark
    }
    var appearanceMode: AppearanceMode = .automatic

    init(library: Library, watch: [String], select selectArg: String? = nil,
         spotifySearch: String? = nil) {
        self.library = library
        self.watchDirs = watch
        self.selectPath = selectArg
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 820, height: 540),
            styleMask: [.titled, .closable, .miniaturizable, .resizable],
            backing: .buffered, defer: false)
        window.title = "JukeWizard — \(library.tracks.count) tracks"
        // JukeWizard is a compact listening utility: keep it visible above
        // normal document windows and available across Spaces. Previously it
        // could fall behind a full-screen development stack while its process
        // remained healthy, which looked exactly like a crash.
        window.level = .floating
        window.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary]
        window.hidesOnDeactivate = false
        window.isMovableByWindowBackground = true
        window.isRestorable = false          // don't let AppKit re-select a stale row over our pick
        window.isReleasedWhenClosed = false  // keep it around so the menu-bar CD can reopen it
        window.minSize = NSSize(width: 640, height: 420)
        window.center()
        super.init(window: window)
        window.delegate = self
        setupUI()
        appearanceMode = AppearanceMode(rawValue: UserDefaults.standard.integer(forKey: "appearanceMode")) ?? .automatic
        appearanceTabs.selectedSegment = appearanceMode.rawValue
        applyAppearance()
        DistributedNotificationCenter.default().addObserver(
            forName: NSNotification.Name("AppleInterfaceThemeChangedNotification"),
            object: nil, queue: .main
        ) { [weak self] _ in
            guard self?.appearanceMode == .automatic else { return }
            self?.applyAppearance()
        }
        if UserDefaults.standard.object(forKey: "playerVolume") != nil {
            quickVolume = UserDefaults.standard.float(forKey: "playerVolume")
        }
        wave.volume = quickVolume
        relayout()
        // the spinning-CD menu-bar presence (persists when the window is closed)
        menuBar = MenuBarCD()
        menuBar?.onOpen = { [weak self] in self?.quickOpenFull() }
        menuBar?.onVolumeChanged = { [weak self] value in self?.setQuickVolume(value) }
        menuBar?.setVolume(quickVolume)
        roomAudio.onState = { [weak self] state in
            DispatchQueue.main.async { self?.renderRoomState(state) }
        }
        spotify.onState = { [weak self] state in self?.renderSpotifyState(state) }
        spotify.onStatus = { [weak self] message, failed in
            guard let self, self.spotifyMode else { return }
            self.activityLabel.stringValue = message
            self.activityLabel.textColor = failed ? .systemRed : Palette.teal
        }
        // open on the requested track (and play it) if given; else the top.
        if let sp = selectPath {
            let want = URL(fileURLWithPath: (sp as NSString).expandingTildeInPath).standardizedFileURL.path
            if let idx = library.tracks.firstIndex(where: { $0.url.standardizedFileURL.path == want }) {
                select(idx, autoplay: true)
            } else if !library.tracks.isEmpty { select(0, autoplay: false) }
        } else if !library.tracks.isEmpty { select(0, autoplay: false) }
        activateSpotifyMode()
        spotify.start()
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.8) { [weak self] in
            guard let self, case .idle = self.roomAudio.state else { return }
            self.roomAudio.apply(self.savedRoomLayout(), pan: self.savedRoomPan())
        }
        if let query = spotifySearch?.trimmingCharacters(in: .whitespacesAndNewlines), !query.isEmpty {
            spotifySearchField.stringValue = query
            searchSpotify(query, autoplayFirst: true)
        }
        armWatch()
        armActivityStatus()
        installKeyMonitor()
    }
    required init?(coder: NSCoder) { fatalError() }
    deinit { if let m = keyMonitor { NSEvent.removeMonitor(m) } }

    // Keep the single player window alive: the menu-bar CD and Dock icon can
    // restore it instantly, and playback/queue state cannot be lost on close.
    func windowShouldClose(_ sender: NSWindow) -> Bool {
        sender.orderOut(nil)
        return false
    }

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
        case 123:
            if spotifyMode { spotify.seek(offsetMS: -5000) } else { wave.seek(to: wave.currentTime - 5) }
            return true                                                     // ←  back 5s
        case 124:
            if spotifyMode { spotify.seek(offsetMS: 5000) } else { wave.seek(to: wave.currentTime + 5) }
            return true                                                     // →  fwd 5s
        case 126: prevTrack(); return true                                  // ↑  prev track
        case 125: nextTrack(); return true                                  // ↓  next track
        case 18, 19, 20, 21, 23:                                            // 1–5 stars
            let map: [UInt16: Int] = [18: 1, 19: 2, 20: 3, 21: 4, 23: 5]
            if !spotifyMode, let n = map[e.keyCode], let t = track {
                t.data.stars = n; renderStars(n); t.save()
                listTable.reloadData(forRowIndexes: IndexSet(integer: current), columnIndexes: IndexSet(integer: 0))
            }
            return true
        case 8: if !spotifyMode { addCommentNow() }; return true            // c  comment @ now
        case 29: if !spotifyMode { clearStarsClicked() }; return true       // 0  clear stars
        default: return false
        }
    }

    // ── construction ─────────────────────────────────────────────────────
    private func setupUI() {
        guard let content = window?.contentView else { return }
        content.wantsLayer = true
        applyThemeBackground()

        // ── now-playing header: big art / streaming video on the left ─────────
        nowPlaying = NowPlayingMedia(frame: .zero)
        content.addSubview(nowPlaying)

        titleLabel = label("", size: 19, bold: true)
        titleLabel.lineBreakMode = .byTruncatingTail
        titleLabel.textColor = Palette.gold
        artistLabel = label("", size: 13, color: .secondaryLabelColor)
        laneLabel = label("", size: 11, color: .secondaryLabelColor)
        laneLabel.lineBreakMode = .byTruncatingTail
        content.addSubview(titleLabel); content.addSubview(artistLabel); content.addSubview(laneLabel)
        activityLabel = label("● watching agents + renders", size: 10, color: Palette.teal)
        activityLabel.lineBreakMode = .byTruncatingTail
        content.addSubview(activityLabel)

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

        spotifyProgress = SpotifyProgressView(frame: .zero)
        spotifyProgress.isHidden = true
        spotifyProgress.onSeek = { [weak self] target in
            guard let self, let state = self.spotifyState else { return }
            self.spotify.seek(offsetMS: Int((target - state.position) * 1000))
        }
        content.addSubview(spotifyProgress)

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

        // green LED time readout (Winamp signature) — monospace, glowing green.
        ledLabel = label("0:00 / 0:00", size: 15, color: NSColor(srgbRed: 0.30, green: 1.0, blue: 0.45, alpha: 1))
        ledLabel.font = NSFont.monospacedDigitSystemFont(ofSize: 15, weight: .bold)
        ledLabel.alignment = .right
        content.addSubview(ledLabel)

        notesToggle = NSButton(title: "♪ notes", target: self, action: #selector(toggleDrawer))
        notesToggle.bezelStyle = .rounded
        notesToggle.setButtonType(.pushOnPushOff)
        content.addSubview(notesToggle)

        roomButton = NSButton(title: "▰ Neo L · ▰ Blueberry R", target: self, action: #selector(showRoomMixer))
        roomButton.bezelStyle = .rounded
        roomButton.setButtonType(.momentaryPushIn)
        roomButton.toolTip = "Choose the MacBooks, channels, and pan for room playback"
        if #unavailable(macOS 14.2) {
            roomButton.isEnabled = false
            roomButton.toolTip = "Room audio requires macOS 14.2 or newer"
        }
        content.addSubview(roomButton)

        // ── the track list (underneath) ───────────────────────────────────────
        sortPopup = NSPopUpButton(frame: .zero, pullsDown: false)
        sortPopup.addItems(withTitles: SortMode.allCases.map { "sort: \($0.label)" })
        sortPopup.selectItem(at: sortMode.rawValue)
        sortPopup.target = self
        sortPopup.action = #selector(sortChanged(_:))
        sortPopup.bezelStyle = .rounded
        sortPopup.controlSize = .small
        sortPopup.font = NSFont.systemFont(ofSize: 11)
        content.addSubview(sortPopup)

        sourceTabs = NSSegmentedControl(labels: ["Spotify", "Aesthetic"],
                                        trackingMode: .selectOne, target: self,
                                        action: #selector(sourceTabChanged))
        sourceTabs.selectedSegment = 0
        sourceTabs.controlSize = .small
        content.addSubview(sourceTabs)

        appearanceTabs = NSSegmentedControl(labels: ["Auto", "Light", "Dark"],
                                             trackingMode: .selectOne, target: self,
                                             action: #selector(appearanceChanged))
        appearanceTabs.selectedSegment = 0
        appearanceTabs.controlSize = .small
        appearanceTabs.toolTip = "Follow macOS, or pin JukeWizard to light or dark"
        content.addSubview(appearanceTabs)

        spotifySearchField = NSSearchField(frame: .zero)
        spotifySearchField.placeholderString = "Search Spotify"
        spotifySearchField.sendsSearchStringImmediately = false
        spotifySearchField.sendsWholeSearchString = true
        spotifySearchField.target = self
        spotifySearchField.action = #selector(searchSpotifyFromField)
        content.addSubview(spotifySearchField)

        listTable = NSTableView()
        let col = NSTableColumn(identifier: .init("track"))
        listTable.addTableColumn(col)
        listTable.headerView = nil
        listTable.rowHeight = TrackRowView.height
        listTable.backgroundColor = .clear
        listTable.selectionHighlightStyle = .none
        listTable.dataSource = self
        listTable.delegate = self
        listTable.setDraggingSourceOperationMask([.copy], forLocal: false)
        listTable.target = self
        listTable.action = #selector(listClicked)
        listScroll = NSScrollView()
        listScroll.documentView = listTable
        listScroll.hasVerticalScroller = true
        listScroll.drawsBackground = false
        listScroll.borderType = .noBorder
        content.addSubview(listScroll)

        buildDrawer(in: content)

        playButton.contentTintColor = Palette.teal
        transportExtra.forEach { $0.contentTintColor = Palette.teal }
    }

    // The collapsible notes + comments + rating drawer (hidden by default).
    private func buildDrawer(in content: NSView) {
        drawerPanel = NSView()
        drawerPanel.wantsLayer = true
        drawerPanel.layer?.backgroundColor = NSColor.black.withAlphaComponent(0.82).cgColor
        drawerPanel.layer?.cornerRadius = 10
        drawerPanel.layer?.borderWidth = 1
        drawerPanel.layer?.borderColor = Palette.teal.withAlphaComponent(0.5).cgColor
        drawerPanel.isHidden = true
        content.addSubview(drawerPanel)

        for i in 1...5 {
            let b = NSButton(title: "☆", target: self, action: #selector(starClicked(_:)))
            b.tag = i; b.isBordered = false
            b.font = NSFont.systemFont(ofSize: 22)
            b.contentTintColor = .systemYellow
            starButtons.append(b); drawerPanel.addSubview(b)
        }
        clearStars = NSButton(title: "clear", target: self, action: #selector(clearStarsClicked))
        clearStars.bezelStyle = .inline; clearStars.contentTintColor = Palette.inkDim
        drawerPanel.addSubview(clearStars)

        notesView = NSTextView()
        notesView.isRichText = false
        notesView.font = NSFont.systemFont(ofSize: 13)
        notesView.delegate = self
        notesView.isAutomaticQuoteSubstitutionEnabled = false
        notesScroll = NSScrollView()
        notesScroll.documentView = notesView
        notesScroll.hasVerticalScroller = true
        notesScroll.borderType = .bezelBorder
        drawerPanel.addSubview(notesScroll)
        notesPlaceholder = label("notes…", size: 12, color: .tertiaryLabelColor)
        drawerPanel.addSubview(notesPlaceholder)

        commentNowButton = NSButton(title: "＋ comment @ now", target: self, action: #selector(addCommentNow))
        commentNowButton.bezelStyle = .rounded; commentNowButton.contentTintColor = Palette.coral
        drawerPanel.addSubview(commentNowButton)

        commentsTable = NSTableView()
        let cc = NSTableColumn(identifier: .init("comment"))
        commentsTable.addTableColumn(cc)
        commentsTable.headerView = nil
        commentsTable.rowHeight = 24
        commentsTable.backgroundColor = .clear
        commentsTable.dataSource = self
        commentsTable.delegate = self
        commentsTable.target = self
        commentsTable.doubleAction = #selector(commentDoubleClicked)
        commentsTable.action = #selector(commentClicked)
        commentsScroll = NSScrollView()
        commentsScroll.documentView = commentsTable
        commentsScroll.hasVerticalScroller = true
        commentsScroll.drawsBackground = false
        commentsScroll.borderType = .bezelBorder
        drawerPanel.addSubview(commentsScroll)
        commentsHeader = label("comments · click to jump", size: 11, color: .secondaryLabelColor)
        drawerPanel.addSubview(commentsHeader)
        delCommentButton = NSButton(title: "– delete", target: self, action: #selector(deleteComment))
        delCommentButton.bezelStyle = .inline; delCommentButton.contentTintColor = Palette.coral
        drawerPanel.addSubview(delCommentButton)

        commentsHeader.textColor = Palette.inkDim
        notesScroll.drawsBackground = false
        notesView.drawsBackground = false
    }

    @objc private func toggleDrawer() {
        drawerOpen.toggle()
        drawerPanel.isHidden = !drawerOpen
        notesToggle.state = drawerOpen ? .on : .off
        relayout()
    }

    private func applyThemeBackground() {
        let dark = window?.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        let chassis = Palette.bg(dark)
        window?.backgroundColor = chassis
        window?.contentView?.layer?.backgroundColor = chassis.cgColor
        window?.contentView?.needsDisplay = true
        listTable?.reloadData()
    }

    private func applyAppearance() {
        switch appearanceMode {
        case .automatic: window?.appearance = nil
        case .light: window?.appearance = NSAppearance(named: .aqua)
        case .dark: window?.appearance = NSAppearance(named: .darkAqua)
        }
        applyThemeBackground()
    }

    @objc private func appearanceChanged() {
        appearanceMode = AppearanceMode(rawValue: appearanceTabs.selectedSegment) ?? .automatic
        UserDefaults.standard.set(appearanceMode.rawValue, forKey: "appearanceMode")
        applyAppearance()
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
        let pad: CGFloat = 8
        let topBarH: CGFloat = 34
        let contentTop = H - topBarH
        sourceTabs.frame = NSRect(x: pad, y: H - 27, width: 170, height: 22)
        appearanceTabs.frame = NSRect(x: W - pad - 172, y: H - 27, width: 172, height: 22)
        // ── header (now-playing) across the top ───────────────────────────────
        let headerH = max(178, min(245, (H - topBarH) * 0.39))
        let headerBottom = contentTop - headerH
        let mediaSide = min(headerH - pad * 2, W * 0.34)
        nowPlaying.frame = NSRect(x: pad, y: headerBottom + pad, width: mediaSide, height: headerH - pad * 2)
        let rx = pad + mediaSide + 10
        let rw = max(120, W - rx - pad)

        var y = contentTop - pad - 25
        titleLabel.frame = NSRect(x: rx, y: y, width: rw, height: 25)
        y -= 19
        artistLabel.frame = NSRect(x: rx, y: y, width: rw, height: 17)
        y -= 15
        laneLabel.frame = NSRect(x: rx, y: y, width: rw, height: 16)
        y -= 16
        activityLabel.frame = NSRect(x: rx, y: y, width: rw, height: 14)
        y -= 21
        var lx = rx                                   // per-service link buttons row
        for b in linkButtons where !b.isHidden {
            let bw = b.attributedTitle.size().width + 16
            b.frame = NSRect(x: lx, y: y, width: bw, height: 20)
            lx += bw + 6
        }
        let linksBottom = y

        // transport row pinned to the header's bottom edge
        let transY = headerBottom + pad
        transportExtra[0].frame = NSRect(x: rx, y: transY, width: 34, height: 25)
        playButton.frame        = NSRect(x: rx + 37, y: transY, width: 44, height: 25)
        transportExtra[1].frame = NSRect(x: rx + 84, y: transY, width: 34, height: 25)
        notesToggle.frame       = NSRect(x: rx + 124, y: transY, width: 75, height: 25)
        let roomWidth = max(70, min(210, rw - 203 - 145))
        roomButton.frame        = NSRect(x: rx + 203, y: transY, width: roomWidth, height: 25)
        ledLabel.frame          = NSRect(x: rx + rw - 135, y: transY + 3, width: 135, height: 20)

        // waveform fills the space between the links row and the transport
        let waveTop = linksBottom - 6
        let waveBottom = transY + 31
        wave.frame = NSRect(x: rx, y: waveBottom, width: rw, height: max(32, waveTop - waveBottom))
        spotifyProgress.frame = wave.frame

        // ── track list underneath ─────────────────────────────────────────────
        let sortY = headerBottom - 2 - 20
        sortPopup.frame = NSRect(x: pad, y: sortY, width: 180, height: 20)
        let searchX = spotifyMode ? pad : pad + 184
        spotifySearchField.frame = NSRect(x: searchX, y: sortY,
                                           width: max(120, W - searchX - pad), height: 20)
        listScroll.frame = NSRect(x: pad, y: pad, width: W - pad * 2, height: sortY - pad - 3)

        // ── drawer overlays the list when open ────────────────────────────────
        if drawerOpen {
            drawerPanel.frame = listScroll.frame.insetBy(dx: 0, dy: 0)
            layoutDrawer()
        }
    }

    private func layoutDrawer() {
        let dw = drawerPanel.bounds.width, dh = drawerPanel.bounds.height, dp: CGFloat = 12
        let starsY = dh - dp - 30
        for (i, b) in starButtons.enumerated() {
            b.frame = NSRect(x: dp + CGFloat(i) * 30, y: starsY, width: 30, height: 30)
        }
        clearStars.frame = NSRect(x: dp + 5 * 30 + 8, y: starsY + 4, width: 50, height: 22)
        commentNowButton.frame = NSRect(x: dw - 180 - dp, y: starsY, width: 180, height: 30)
        let commentsH = max(90, (dh - dp * 2) * 0.4)
        commentsHeader.frame = NSRect(x: dp, y: dp + commentsH + 2, width: dw - 90, height: 16)
        delCommentButton.frame = NSRect(x: dw - 70 - dp, y: dp + commentsH + 2, width: 70, height: 18)
        commentsScroll.frame = NSRect(x: dp, y: dp, width: dw - dp * 2, height: commentsH)
        let notesTop = starsY - 8
        let notesBottom = dp + commentsH + 22
        let notesH = max(50, notesTop - notesBottom)
        notesScroll.frame = NSRect(x: dp, y: notesBottom, width: dw - dp * 2, height: notesH)
        notesPlaceholder.frame = NSRect(x: dp + 6, y: notesBottom + notesH - 20, width: 100, height: 16)
    }

    // ── menu-bar CD ────────────────────────────────────────────────────────
    // Keep the bar disc's tempo + spin in step with playback.
    private func refreshMenuBar() {
        let bpm = spotifyMode ? nil : track?.meta?.bpm.map(Double.init)
        let playing = spotifyMode ? (spotifyState?.isPlaying ?? false) : wave.isPlaying
        let title = spotifyMode ? (spotifyState?.title ?? "Spotify") : (track?.title ?? "JukeWizard")
        menuBar?.setBPM(bpm)
        menuBar?.setNowPlaying(title: title, art: currentArt)
        menuBar?.setPlaying(playing)
        menuBar?.setVolume(quickVolume)
        DockIcon.setNowPlaying(art: currentArt, playing: playing, bpm: bpm)
        miniPlayer?.refresh()
    }

    @objc private func showRoomMixer() {
        let mixer = RoomMixerView(frame: NSRect(x: 0, y: 0, width: 420, height: 220))
        mixer.onLayout = { [weak self, weak mixer] layout in
            guard let self else { return }
            UserDefaults.standard.set(layout.rawValue, forKey: "roomLayout")
            self.roomAudio.apply(layout, pan: self.roomAudio.pan)
            mixer?.show(self.roomAudio.state, layout: self.roomAudio.layout, pan: self.roomAudio.pan)
        }
        mixer.onPan = { [weak self, weak mixer] pan in
            guard let self else { return }
            UserDefaults.standard.set(JukeRoomAudio.Layout.panMono.rawValue, forKey: "roomLayout")
            UserDefaults.standard.set(pan, forKey: "roomPan")
            self.roomAudio.apply(.panMono, pan: pan)
            mixer?.show(self.roomAudio.state, layout: self.roomAudio.layout, pan: self.roomAudio.pan)
        }
        mixer.show(roomAudio.state, layout: roomAudio.layout, pan: roomAudio.pan)
        let controller = NSViewController()
        controller.view = mixer
        let popover = NSPopover()
        popover.behavior = .transient
        popover.contentSize = NSSize(width: 420, height: 220)
        popover.contentViewController = controller
        popover.show(relativeTo: roomButton.bounds, of: roomButton, preferredEdge: .minY)
        roomMixer = mixer
        roomPopover = popover
    }

    private func savedRoomLayout() -> JukeRoomAudio.Layout {
        guard UserDefaults.standard.object(forKey: "roomLayout") != nil else { return .splitLR }
        return JukeRoomAudio.Layout(rawValue: UserDefaults.standard.integer(forKey: "roomLayout")) ?? .splitLR
    }

    private func savedRoomPan() -> Float {
        guard UserDefaults.standard.object(forKey: "roomPan") != nil else { return 0 }
        return UserDefaults.standard.float(forKey: "roomPan")
    }

    private func renderRoomState(_ state: JukeRoomAudio.State) {
        switch state {
        case .idle:
            roomButton.state = .off
            roomButton.title = "▰ Neo · ▰ Blueberry"
            roomButton.contentTintColor = Palette.teal
        case .live(let snapshot):
            roomButton.state = .on
            switch snapshot.layout {
            case .neoStereo: roomButton.title = "▰ Neo · stereo"
            case .blueberryStereo: roomButton.title = "▰ Blueberry · stereo"
            case .mirrorStereo: roomButton.title = "▰ Neo + ▰ Blueberry"
            case .splitLR: roomButton.title = "▰ Neo L · ▰ Blueberry R"
            case .splitRL: roomButton.title = "▰ Neo R · ▰ Blueberry L"
            case .panMono: roomButton.title = "▰ Neo ↔ ▰ Blueberry"
            }
            roomButton.toolTip = "\(snapshot.source.rawValue): Neo \(snapshot.neo), Blueberry \(snapshot.blueberry)"
            roomButton.contentTintColor = Palette.coral
        case .failed(let message):
            roomButton.state = .off
            roomButton.title = "⚠ room"
            roomButton.toolTip = message
            roomButton.contentTintColor = .systemRed
        }
        roomMixer?.show(state, layout: roomAudio.layout, pan: roomAudio.pan)
        miniPlayer?.refresh()
    }

    // ── headless Spotify source ───────────────────────────────────────────
    private func activateSpotifyMode() {
        if wave?.isPlaying == true { wave.pause() }
        if drawerOpen {
            drawerOpen = false
            drawerPanel?.isHidden = true
            notesToggle?.state = .off
        }
        spotifyMode = true
        roomAudio.useSource(.spotify)
        window?.title = "JukeWizard · Spotify"
        wave?.isHidden = true
        spotifyProgress?.isHidden = false
        sortPopup?.isHidden = true
        spotifySearchField?.isHidden = false
        notesToggle?.isEnabled = false
        sourceTabs?.selectedSegment = 0
        currentArt = spotifyArt
        activityLabel?.stringValue = "● juked headless · connecting"
        activityLabel?.textColor = Palette.teal
        listTable?.reloadData()
        relayout()
        if let state = spotifyState { renderSpotifyState(state) }
        else { refreshMenuBar() }
    }

    private func activateLibraryMode() {
        if spotifyMode, spotifyState?.isPlaying == true { spotify.pause() }
        spotifyMode = false
        roomAudio.useSource(.aesthetic)
        window?.title = "JukeWizard — \(library.tracks.count) tracks"
        wave.isHidden = false
        spotifyProgress.isHidden = true
        sortPopup.isHidden = false
        spotifySearchField.isHidden = true
        notesToggle.isEnabled = true
        sourceTabs.selectedSegment = 1
        listTable.reloadData()
        if let t = track {
            titleLabel.stringValue = t.title
            artistLabel.stringValue = t.meta?.artist ?? "Aesthetic Dot Computer"
            laneLabel.stringValue = Self.metaLine(t)
            updateNowPlaying(t); loadLinks(t); updateTime()
        }
        playButton.title = wave.isPlaying ? "❚❚" : "▶"
        nowPlaying.setPaused(!wave.isPlaying)
        relayout()
        refreshMenuBar()
        pollActivityStatus()
    }

    @objc private func sourceTabChanged() {
        sourceTabs.selectedSegment == 0 ? activateSpotifyMode() : activateLibraryMode()
    }

    @objc private func searchSpotifyFromField() {
        searchSpotify(spotifySearchField.stringValue)
    }

    private func searchSpotify(_ rawQuery: String, autoplayFirst: Bool = false) {
        let query = rawQuery.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !query.isEmpty else { return }
        activateSpotifyMode()
        activityLabel.stringValue = "● searching Spotify for “\(query)”"
        spotify.search(query) { [weak self] result in
            guard let self else { return }
            switch result {
            case .success(let tracks):
                self.spotifyResults = tracks
                self.selectedSpotifyRow = tracks.isEmpty ? -1 : 0
                self.activityLabel.stringValue = tracks.isEmpty
                    ? "● no Spotify tracks for “\(query)”"
                    : "● \(tracks.count) Spotify tracks · click one to play"
                self.activityLabel.textColor = tracks.isEmpty ? Palette.inkDim : Palette.teal
                self.listTable.reloadData()
                if !tracks.isEmpty { self.listTable.scrollRowToVisible(0) }
                if autoplayFirst, !tracks.isEmpty { self.playSpotifyResult(at: 0) }
            case .failure(let error):
                self.activityLabel.stringValue = "⚠ \(error.localizedDescription)"
                self.activityLabel.textColor = .systemRed
            }
        }
    }

    private func playSpotifyResult(at row: Int) {
        guard row >= 0, row < spotifyResults.count else { return }
        let old = selectedSpotifyRow
        selectedSpotifyRow = row
        let result = spotifyResults[row]
        titleLabel.stringValue = result.title
        titleLabel.textColor = NSColor(srgbRed: 0.11, green: 0.73, blue: 0.33, alpha: 1)
        artistLabel.stringValue = result.artists
        laneLabel.stringValue = [result.album, "Spotify · juked"].filter { !$0.isEmpty }.joined(separator: " · ")
        spotifyProgress.duration = result.duration
        spotifyProgress.position = 0
        playButton.title = "❚❚"
        spotify.play(result)
        var rows = IndexSet(integer: row)
        if old >= 0, old < spotifyResults.count { rows.insert(old) }
        listTable.reloadData(forRowIndexes: rows, columnIndexes: IndexSet(integer: 0))
    }

    private func renderSpotifyState(_ state: SpotifyPlaybackState?) {
        spotifyState = state
        guard spotifyMode, let state else { return }
        titleLabel.stringValue = state.title
        titleLabel.textColor = NSColor(srgbRed: 0.11, green: 0.73, blue: 0.33, alpha: 1)
        artistLabel.stringValue = state.artists
        laneLabel.stringValue = [state.album, "Spotify · headless juked"].filter { !$0.isEmpty }.joined(separator: " · ")
        laneLabel.textColor = NSColor(white: 0.68, alpha: 1)
        spotifyProgress.duration = state.duration
        spotifyProgress.position = state.position
        playButton.title = state.isPlaying ? "❚❚" : "▶"
        nowPlaying.setPaused(!state.isPlaying)
        ledLabel.stringValue = "\(Self.mmss(state.position)) / \(Self.mmss(state.duration))"
        if let spotifyArt {
            currentArt = spotifyArt
            nowPlaying.present(art: spotifyArt, videoURL: nil)
        }
        if state.artworkURL != spotifyArtworkURL {
            spotifyArtworkURL = state.artworkURL
            spotifyArt = nil
            currentArt = nil
            guard let url = state.artworkURL else { nowPlaying.present(art: nil, videoURL: nil); return }
            URLSession.shared.dataTask(with: url) { [weak self] data, _, _ in
                guard let data, let art = NSImage(data: data) else { return }
                DispatchQueue.main.async {
                    guard self?.spotifyArtworkURL == url else { return }
                    self?.spotifyArt = art
                    self?.currentArt = art
                    self?.nowPlaying.present(art: art, videoURL: nil)
                    self?.nowPlaying.setPaused(!state.isPlaying)
                    self?.refreshMenuBar()
                }
            }.resume()
        }
        refreshMenuBar()
    }
    var quickTitle: String {
        spotifyMode ? (spotifyState?.title ?? "Spotify") : (track?.title ?? "Aesthetic")
    }
    var quickSubtitle: String {
        if spotifyMode { return [spotifyState?.artists ?? "", "Spotify"].filter { !$0.isEmpty }.joined(separator: " · ") }
        return [track?.meta?.artist ?? "Aesthetic Dot Computer", "Aesthetic"].joined(separator: " · ")
    }
    var quickIsPlaying: Bool {
        spotifyMode ? (spotifyState?.isPlaying ?? false) : wave.isPlaying
    }
    var quickRoomSummary: String {
        switch roomAudio.state {
        case .idle: return "room · off"
        case .failed(let message): return "room · ⚠ \(message)"
        case .live(let snapshot): return "\(snapshot.neo == "off" ? "" : "Neo \(snapshot.neo)")\(snapshot.neo != "off" && snapshot.blueberry != "off" ? "  ·  " : "")\(snapshot.blueberry == "off" ? "" : "Blueberry \(snapshot.blueberry)")"
        }
    }

    private func showMiniPlayer() {
        if miniPopover?.isShown == true {
            miniPopover?.close()
            return
        }
        let player = JukeMiniPlayerView(controller: self)
        let viewController = NSViewController()
        viewController.view = player
        let popover = NSPopover()
        popover.behavior = .transient
        popover.contentSize = NSSize(width: 370, height: 170)
        popover.contentViewController = viewController
        miniPlayer = player
        miniPopover = popover
        menuBar?.show(popover)
    }

    @objc func quickOpenFull() {
        miniPopover?.close()
        guard let w = window else { return }
        if w.isMiniaturized { w.deminiaturize(nil) }
        w.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)
    }

    @objc func quickTogglePlay() { togglePlay(); miniPlayer?.refresh() }
    @objc func quickPrevious() { prevTrack(); miniPlayer?.refresh() }
    @objc func quickNext() { nextTrack(); miniPlayer?.refresh() }
    @objc func quickVolumeChanged(_ sender: NSSlider) {
        quickVolume = max(0, min(1, sender.floatValue))
        wave.volume = quickVolume
        spotify.volume(percent: Int((quickVolume * 100).rounded()))
        UserDefaults.standard.set(quickVolume, forKey: "playerVolume")
        miniPlayer?.refresh()
    }

    @objc func quickVolumeUp() { setQuickVolume(quickVolume + 0.1) }
    @objc func quickVolumeDown() { setQuickVolume(quickVolume - 0.1) }

    private func setQuickVolume(_ value: Float) {
        quickVolume = max(0, min(1, value))
        wave.volume = quickVolume
        spotify.volume(percent: Int((quickVolume * 100).rounded()))
        UserDefaults.standard.set(quickVolume, forKey: "playerVolume")
        miniPlayer?.refresh()
    }

    func makeDockMenu() -> NSMenu {
        let menu = NSMenu(title: "JukeWizard")
        let heading = NSMenuItem(title: "\(quickTitle) — \(quickSubtitle)", action: nil, keyEquivalent: "")
        heading.isEnabled = false
        menu.addItem(heading)
        menu.addItem(.separator())
        for (title, action) in [("Previous", #selector(quickPrevious)),
                                (quickIsPlaying ? "Pause" : "Play", #selector(quickTogglePlay)),
                                ("Next", #selector(quickNext))] {
            let item = NSMenuItem(title: title, action: action, keyEquivalent: "")
            item.target = self
            menu.addItem(item)
        }
        let volume = NSMenuItem(title: "Volume \(Int((quickVolume * 100).rounded()))%",
                                action: nil, keyEquivalent: "")
        volume.isEnabled = false
        menu.addItem(volume)
        let volumeUp = NSMenuItem(title: "Volume Up", action: #selector(quickVolumeUp), keyEquivalent: "")
        volumeUp.target = self
        menu.addItem(volumeUp)
        let volumeDown = NSMenuItem(title: "Volume Down", action: #selector(quickVolumeDown), keyEquivalent: "")
        volumeDown.target = self
        menu.addItem(volumeDown)
        menu.addItem(.separator())
        let room = NSMenuItem(title: quickRoomSummary, action: nil, keyEquivalent: "")
        room.isEnabled = false
        menu.addItem(room)
        let open = NSMenuItem(title: "Open JukeWizard", action: #selector(quickOpenFull), keyEquivalent: "")
        open.target = self
        menu.addItem(open)
        return menu
    }

    // ── selection / playback ──────────────────────────────────────────────
    private var track: Track? { (current >= 0 && current < library.tracks.count) ? library.tracks[current] : nil }

    func select(_ i: Int, autoplay: Bool) {
        guard i >= 0, i < library.tracks.count else { return }
        if spotifyMode { spotify.pause(); activateLibraryMode() }
        commitNotes()
        let old = current
        current = i
        let t = library.tracks[i]
        titleLabel.stringValue = t.title
        titleLabel.textColor = Self.statusColor(t.meta?.status)
        artistLabel.stringValue = t.meta?.artist ?? "Aesthetic Dot Computer"
        laneLabel.stringValue = Self.metaLine(t)
        laneLabel.textColor = .secondaryLabelColor
        updateNowPlaying(t)
        loadLinks(t)
        relayout()                    // link count changes the header row width
        wave.load(url: t.url)
        wave.comments = t.data.comments
        notesView.string = t.data.notes
        notesPlaceholder.isHidden = !t.data.notes.isEmpty
        renderStars(t.data.stars)
        commentsTable.reloadData()
        // refresh only the two affected rows (custom .selected chip)
        var rows = IndexSet(integer: i)
        if old >= 0, old < library.tracks.count { rows.insert(old) }
        listTable.reloadData(forRowIndexes: rows, columnIndexes: IndexSet(integer: 0))
        listTable.scrollRowToVisible(i)
        updateTime()
        if autoplay { wave.play(); playButton.title = "❚❚"; nowPlaying.setPaused(false) }
        else { playButton.title = "▶"; nowPlaying.setPaused(true) }
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
        if spotifyMode { playSpotifyResult(at: r) }
        else if r >= 0 { select(r, autoplay: true) }
    }
    @objc private func togglePlay() {
        if spotifyMode {
            spotify.toggle()
            let playing = !(spotifyState?.isPlaying ?? false)
            playButton.title = playing ? "❚❚" : "▶"
            nowPlaying.setPaused(!playing)
        } else {
            wave.togglePlay()
            playButton.title = wave.isPlaying ? "❚❚" : "▶"
            nowPlaying.setPaused(!wave.isPlaying)
        }
        refreshMenuBar()
    }
    @objc private func prevTrack() {
        if spotifyMode { spotify.previous() }
        else if current > 0 { select(current - 1, autoplay: true) }
    }
    @objc private func nextTrack() {
        if spotifyMode { spotify.next() }
        else if current < library.tracks.count - 1 { select(current + 1, autoplay: true) }
    }

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
        if wasPlaying { wave.play(); playButton.title = "❚❚"; nowPlaying.setPaused(false) }
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
        if let state = spotifyMode ? spotifyState : nil {
            ledLabel.stringValue = "\(Self.mmss(state.position)) / \(Self.mmss(state.duration))"
        } else {
            ledLabel.stringValue = "\(Self.mmss(wave.currentTime)) / \(Self.mmss(wave.duration))"
        }
    }

    // ── live work awareness ────────────────────────────────────────────────
    // Slab's ledger tells us which agents are active; local process inspection
    // catches the narrower render/bake window. Polling is read-only and cheap.
    private func armActivityStatus() {
        pollActivityStatus()
        activityTimer = Timer.scheduledTimer(withTimeInterval: 2.0, repeats: true) { [weak self] _ in
            self?.pollActivityStatus()
        }
    }
    private func pollActivityStatus() {
        guard !spotifyMode, !activityPollInFlight else { return }
        activityPollInFlight = true
        let tracks = library.tracks
        DispatchQueue.global(qos: .utility).async { [weak self] in
            let activities = WorkStatus.snapshot(tracks: tracks)
            DispatchQueue.main.async {
                guard let self else { return }
                self.activityPollInFlight = false
                guard !self.spotifyMode else { return }
                self.renderActivityStatus(activities)
            }
        }
    }

    private func renderActivityStatus(_ activities: [WorkActivity]) {
        for t in library.tracks {
            let matches = activities.filter { a in
                (a.track != nil && a.track == t.title) || (a.track == nil && a.lane == t.lane)
            }
            t.liveStatus = matches.first.map { $0.track == nil ? "\($0.state) in \(t.lane)" : $0.state }
        }
        if activities.isEmpty {
            activityLabel.stringValue = "● agents + renders idle"
            activityLabel.textColor = Palette.inkDim
        } else {
            activityLabel.stringValue = activities.prefix(3).map { a in
                let target = a.track ?? a.lane ?? "pop"
                return "● \(target): \(a.state)"
            }.joined(separator: "   ")
            activityLabel.textColor = activities.contains(where: { $0.state == "baking" }) ? Palette.gold : Palette.teal
        }
        if let t = track {
            laneLabel.stringValue = Self.metaLine(t)
            laneLabel.textColor = t.liveStatus == nil ? .secondaryLabelColor : Palette.gold
        }
        listTable.reloadData()
    }

    // ── tables ───────────────────────────────────────────────────────────────
    func numberOfRows(in tableView: NSTableView) -> Int {
        if tableView == listTable { return spotifyMode ? spotifyResults.count : library.tracks.count }
        return track?.data.comments.count ?? 0
    }
    // list = dressed-up view rows; comments = plain cell strings.
    func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
        if tableView == listTable, spotifyMode, row < spotifyResults.count {
            let cell = (listTable.makeView(withIdentifier: SpotifyTrackRowView.id, owner: self) as? SpotifyTrackRowView)
                ?? { let view = SpotifyTrackRowView(); view.identifier = SpotifyTrackRowView.id; return view }()
            cell.configure(spotifyResults[row])
            cell.selected = row == selectedSpotifyRow
            return cell
        }
        guard tableView == listTable, row < library.tracks.count else { return nil }
        let cell = (listTable.makeView(withIdentifier: TrackRowView.id, owner: self) as? TrackRowView)
            ?? { let v = TrackRowView(); v.identifier = TrackRowView.id; return v }()
        cell.configure(library.tracks[row])
        cell.selected = (row == current)
        return cell
    }
    func tableView(_ tableView: NSTableView, objectValueFor tableColumn: NSTableColumn?, row: Int) -> Any? {
        guard tableView == commentsTable, let t = track, row < t.data.comments.count else { return "" }
        let c = t.data.comments[row]
        return "\(JukeController.mmss(c.t))  \(c.text)"
    }
    // Export the actual audio file to Finder, Messages, Mail, etc. AppKit's
    // file-URL pasteboard type lets each destination decide whether to copy or
    // attach it; JukeWizard never moves or mutates the source track.
    func tableView(_ tableView: NSTableView, pasteboardWriterForRow row: Int) -> NSPasteboardWriting? {
        guard tableView == listTable, !spotifyMode,
              row >= 0, row < library.tracks.count else { return nil }
        return library.tracks[row].url as NSURL
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
    // ── now-playing art + video + links ──────────────────────────────────────
    // Big cover in the header; if the track has a video (a local reel/story, or
    // the CDN cut for a released single) it streams there instead — muted.
    private func updateNowPlaying(_ t: Track) {
        currentArt = t.meta?.art.flatMap {
            NSImage(contentsOf: URL(fileURLWithPath: ($0 as NSString).expandingTildeInPath))
        }
        nowPlaying.present(art: currentArt, videoURL: Self.videoURL(for: t))
    }
    // Prefer a local clip; else stream the released single's CDN cut.
    private static func videoURL(for t: Track) -> URL? {
        if let v = t.meta?.media?.first(where: { $0.kind == "video" }) {
            return URL(fileURLWithPath: (v.path as NSString).expandingTildeInPath)
        }
        if t.meta?.status == "RELEASED" {
            return URL(string: "https://assets.aesthetic.computer/pop/\(t.title).mp4")
        }
        return nil
    }
    private func loadLinks(_ t: Track) {
        for b in linkButtons {
            let svc = LinkService(rawValue: b.tag)!
            b.isHidden = (svc.url(t.meta?.links) == nil)
        }
    }
    @objc private func linkClicked(_ sender: NSButton) {
        guard let t = track, let svc = LinkService(rawValue: sender.tag),
              let s = svc.url(t.meta?.links) else { return }
        if svc == .spotify, let id = JukeSpotify.trackID(from: s) {
            activateSpotifyMode()
            spotify.play(trackID: id)
            return
        }
        guard let u = URL(string: s) else { return }
        NSWorkspace.shared.open(u)
    }
}
