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

// Juke keeps its teal / gold / coral identity, now as translucent washes over
// the same liquid material used by Menu Band.
enum Palette {
    static let teal   = NSColor(srgbRed: 0.10, green: 0.52, blue: 0.55, alpha: 1)
    static let gold   = NSColor(srgbRed: 0.95, green: 0.74, blue: 0.20, alpha: 1)
    static let coral  = NSColor(srgbRed: 0.95, green: 0.45, blue: 0.38, alpha: 1)
    static let cream  = NSColor(srgbRed: 0.99, green: 0.97, blue: 0.91, alpha: 1)
    static let inkDim = NSColor(srgbRed: 0.42, green: 0.40, blue: 0.36, alpha: 1)
    static func bg(_ dark: Bool) -> NSColor {
        dark
            ? NSColor(srgbRed: 0.04, green: 0.12, blue: 0.14, alpha: 0.10)
            : NSColor(srgbRed: 0.92, green: 0.98, blue: 1.00, alpha: 0.04)
    }
    static func deckSurface(_ accent: NSColor, dark: Bool, alpha: CGFloat = 1) -> NSColor {
        let base = dark ? NSColor(white: 0.025, alpha: 1) : cream
        let wash = dark ? 0.20 : 0.14
        return base.blended(withFraction: wash, of: accent)?.withAlphaComponent(alpha)
            ?? base.withAlphaComponent(alpha)
    }
    static func deckInk(_ accent: NSColor, dark: Bool) -> NSColor {
        let target = dark ? NSColor.white : NSColor.black
        return accent.blended(withFraction: dark ? 0.38 : 0.48, of: target) ?? target
    }
}

final class JukeController: NSWindowController, NSWindowDelegate,
                            NSTableViewDataSource, NSTableViewDelegate,
                            WaveformViewDelegate, NSTextViewDelegate {
    let library: Library
    let watchDirs: [String]
    let selectPath: String?
    var playlistName: String?
    let fullLibraryPath: String
    var current: Int = -1
    var watchTimer: Timer?
    var activityTimer: Timer?
    var activityPollInFlight = false
    var watchMtimes: [String: Date] = [:]
    var keyMonitor: Any?
    var lastDisplayedBPM: Double?

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
    var scopeButton: NSButton!

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
    var outputPopup: AudioOutputPopUpButton!
    var outputDevices: [MacAudioOutput.Device] = []
    var playButton: NSButton!
    var speedSlider: NSSlider!
    var speedLabel: NSTextField!
    var speedResetButton: NSButton!
    var ledLabel: NSTextField!
    var notesToggle: NSButton!
    var sourceActionButton: NSButton!
    var cloudWindow: JukeCloudWindowController?
    var djMixer: DJMixerView!
    var providerDeck: JukeProviderDeckView!
    var djMode = false
    var djConfigured = false
    var roomButton: NSButton!
    var djButton: NSButton!
    var djMixer: DJMixerView!
    var djMode = false
    var cloudButton: NSButton!
    var cloudWindow: JukeCloudWindowController?
    var roomPopover: NSPopover?
    var roomMixer: RoomMixerView?
    var miniPopover: NSPopover?
    var miniPlayer: JukeMiniPlayerView?
    let roomAudio = JukeRoomAudio()
    let spotify = JukeSpotify()
    let cloud = JukeCloudClient()
    private lazy var appleMusic = JukeAppleMusic()
    var activeSource: JukeSource = .local
    var spotifyMode = false
    var appleMusicMode = false
    var spotifyResults: [SpotifyTrackResult] = []
    var selectedSpotifyRow = -1
    var spotifyState: SpotifyPlaybackState?
    var spotifyArtworkURL: URL?
    var spotifyArt: NSImage?
    var cloudTracks: [JukeCloudTrack] = []
    var selectedCloudRow = -1
    var appleMusicResults: [AppleMusicTrackResult] = []
    var selectedAppleMusicRow = -1
    var appleMusicPlaying = false
    var appleMusicArtworkURL: URL?
    var appleMusicArt: NSImage?
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
         spotifySearch: String? = nil, startPrimpats: Bool = false,
         startBeats: Bool = false, startRecords: Bool = false) {
        self.library = library
        self.watchDirs = watch
        self.selectPath = selectArg
        self.playlistName = playlistName
        self.fullLibraryPath = fullLibraryPath
        let window = NSWindow(
            contentRect: NSRect(x: 0, y: 0, width: 820, height: 568),
            styleMask: [.titled, .closable, .miniaturizable, .resizable,
                        .fullSizeContentView],
            backing: .buffered, defer: false)
        window.title = ""
        window.setAccessibilityLabel("Menu Band Juke")
        // Juke is a compact Menu Band listening surface: keep it visible above
        // normal document windows and available across Spaces. Previously it
        // could fall behind a full-screen development stack while its process
        // remained healthy, which looked exactly like a crash.
        window.level = .floating
        window.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary]
        window.hidesOnDeactivate = false
        window.isMovableByWindowBackground = true
        window.isOpaque = false
        window.backgroundColor = .clear
        window.titlebarAppearsTransparent = true
        window.titleVisibility = .hidden
        window.titlebarSeparatorStyle = .none
        window.hasShadow = true
        if let frame = window.contentView?.bounds {
            window.contentView = JukeLiquidSurface(frame: frame)
        }
        window.isRestorable = false          // don't let AppKit re-select a stale row over our pick
        window.isReleasedWhenClosed = false  // keep it around so the menu-bar CD can reopen it
        window.minSize = NSSize(width: 720, height: 420)
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
        if UserDefaults.standard.object(forKey: "listPlaybackRate") != nil {
            speedSlider.doubleValue = max(0.5, min(1.5,
                UserDefaults.standard.double(forKey: "listPlaybackRate")))
        }
        speedLabel.stringValue = String(format: "%.2f×", speedSlider.doubleValue)
        relayout()
        // the spinning-CD menu-bar presence (persists when the window is closed)
        menuBar = MenuBarCD()
        menuBar?.onOpen = { [weak self] in self?.quickToggleFull() }
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
        if startBeats {
            setDJMode(true, singleDeck: true)
            djMixer.loadBeats(solo: true)
        } else if startRecords {
            setDJMode(true)
        } else if startPrimpats {
            setDJMode(true)
            djMixer.loadPrimpats()
        }
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
        case 53:
            if playlistName != nil { escapePlaylist(); return true }
            return false                                                     // esc leaves playlist scope
        case 49: togglePlay(); return true                                  // space
        case 123:
            if djMode { djMixer.dominantDeck.deck.seek(to: djMixer.dominantDeck.deck.currentTime - 5) }
            else if spotifyMode { spotify.seek(offsetMS: -5000) }
            else if !appleMusicMode { wave.seek(to: wave.currentTime - 5) }
            return true                                                     // ←  back 5s
        case 124:
            if djMode { djMixer.dominantDeck.deck.seek(to: djMixer.dominantDeck.deck.currentTime + 5) }
            else if spotifyMode { spotify.seek(offsetMS: 5000) }
            else if !appleMusicMode { wave.seek(to: wave.currentTime + 5) }
            return true                                                     // →  fwd 5s
        case 126: prevTrack(); return true                                  // ↑  prev track
        case 125: nextTrack(); return true                                  // ↓  next track
        case 18, 19, 20, 21, 23:                                            // 1–5 stars
            let map: [UInt16: Int] = [18: 1, 19: 2, 20: 3, 21: 4, 23: 5]
            if activeSource == .local, let n = map[e.keyCode], let t = track {
                t.data.stars = n; renderStars(n); t.save()
                listTable.reloadData(forRowIndexes: IndexSet(integer: current), columnIndexes: IndexSet(integer: 0))
            }
            return true
        case 8: if activeSource == .local { addCommentNow() }; return true  // c  comment @ now
        case 29: if activeSource == .local { clearStarsClicked() }; return true // 0  clear stars
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
        speedSlider = NSSlider(value: 1, minValue: 0.5, maxValue: 1.5,
                               target: self, action: #selector(speedChanged(_:)))
        speedSlider.controlSize = .small
        speedSlider.isContinuous = true
        speedSlider.toolTip = "Playback speed · drag the waveform to scratch"
        content.addSubview(speedSlider)
        speedLabel = label("1.00×", size: 11, color: Palette.teal)
        speedLabel.font = NSFont.monospacedDigitSystemFont(ofSize: 11, weight: .semibold)
        speedLabel.alignment = .right
        content.addSubview(speedLabel)
        speedResetButton = NSButton(title: "↺", target: self, action: #selector(resetSpeed))
        speedResetButton.bezelStyle = .inline
        speedResetButton.toolTip = "Reset playback speed"
        speedResetButton.contentTintColor = Palette.teal
        content.addSubview(speedResetButton)
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

        scopeButton = NSButton(title: "", target: self, action: #selector(escapePlaylist))
        scopeButton.bezelStyle = .inline
        scopeButton.controlSize = .small
        scopeButton.contentTintColor = Palette.coral
        scopeButton.toolTip = "Leave this playlist and show all local tracks (Esc)"
        content.addSubview(scopeButton)
        refreshPlaylistScopeButton()

        sourceTabs = NSSegmentedControl(labels: JukeSource.allCases.map { $0.label() },
                                        trackingMode: .selectOne, target: self,
                                        action: #selector(sourceTabChanged))
        sourceTabs.selectedSegment = JukeSource.local.rawValue
        sourceTabs.controlSize = .small
        content.addSubview(sourceTabs)

        appearanceTabs = NSSegmentedControl(labels: ["Auto", "Light", "Dark"],
                                             trackingMode: .selectOne, target: self,
                                             action: #selector(appearanceChanged))
        appearanceTabs.selectedSegment = 0
        appearanceTabs.controlSize = .small
        appearanceTabs.toolTip = "Follow macOS, or pin Menu Band Juke to light or dark"
        content.addSubview(appearanceTabs)

        djButton = NSButton(title: "DJ", target: self, action: #selector(toggleDJMode))
        djButton.bezelStyle = .rounded
        djButton.setButtonType(.pushOnPushOff)
        djButton.contentTintColor = Palette.teal
        djButton.toolTip = "Open the DJ interface"
        content.addSubview(djButton)

        cloudButton = NSButton(title: "☁︎", target: self, action: #selector(showCloud))
        cloudButton.bezelStyle = .rounded
        cloudButton.contentTintColor = Palette.teal
        cloudButton.toolTip = "Sign in and sync tracks with Juke Cloud"
        content.addSubview(cloudButton)

        outputPopup = AudioOutputPopUpButton(frame: .zero, pullsDown: false)
        outputPopup.controlSize = .small
        outputPopup.bezelStyle = .rounded
        outputPopup.font = NSFont.systemFont(ofSize: 11, weight: .medium)
        outputPopup.target = self
        outputPopup.action = #selector(outputDeviceChanged(_:))
        outputPopup.toolTip = "Mac audio output · speakers, headphones, Bluetooth, USB, and displays"
        outputPopup.prepareMenu = { [weak self] in self?.reloadOutputDevices() }
        content.addSubview(outputPopup)
        reloadOutputDevices()

        spotifySearchField = NSSearchField(frame: .zero)
        spotifySearchField.placeholderString = "Search"
        spotifySearchField.sendsSearchStringImmediately = false
        spotifySearchField.sendsWholeSearchString = true
        spotifySearchField.target = self
        spotifySearchField.action = #selector(searchActiveSourceFromField)
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

        djMixer = DJMixerView(frame: .zero)
        djMixer.isHidden = true
        djMixer.onStateChange = { [weak self] in self?.refreshMenuBar() }
        djMixer.onDetach = { [weak self] in self?.window?.orderOut(nil) }
        content.addSubview(djMixer)

        providerDeck = JukeProviderDeckView(frame: .zero)
        providerDeck.isHidden = true
        providerDeck.onToggle = { [weak self] in self?.togglePlay() }
        providerDeck.onSeek = { [weak self] target in
            guard let self, self.spotifyMode, let state = self.spotifyState else { return }
            self.spotify.seek(offsetMS: Int((target - state.position) * 1000))
        }
        content.addSubview(providerDeck)

        playButton.contentTintColor = Palette.teal
        transportExtra.forEach { $0.contentTintColor = Palette.teal }
    }

    // The collapsible notes + comments + rating drawer (hidden by default).
    private func buildDrawer(in content: NSView) {
        drawerPanel = NSView()
        drawerPanel.wantsLayer = true
        drawerPanel.layer?.backgroundColor = NSColor.black.withAlphaComponent(0.52).cgColor
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

    private func setDJMode(_ enabled: Bool, singleDeck: Bool = false) {
        guard enabled != djMode else { return }
        if enabled {
            wave.pause()
            playButton.title = "▶"
            nowPlaying.setPaused(true)
            if drawerOpen {
                drawerOpen = false
                notesToggle.state = .off
            }
            if !djConfigured {
                if singleDeck {
                    djMixer.configureSolo(tracks: library.tracks, primaryIndex: max(0, current))
                } else {
                    djMixer.configure(tracks: library.tracks, primaryIndex: max(0, current))
                }
                djConfigured = true
            }
            djMixer.setMasterVolume(quickVolume)
            djMode = true
            djMixer.isHidden = false
            providerDeck.isHidden = true
            setPlayerChromeHidden(true)
            listScroll.isHidden = false
            activityLabel.isHidden = false
            djMixer.startDisplay()
            roomAudio.useSource(.aesthetic)
            window?.isMovableByWindowBackground = false
        } else {
            djMixer.stopDisplay()
            djMixer.isHidden = true
            djMode = false
            window?.isMovableByWindowBackground = true
            if spotifyMode || appleMusicMode {
                setPlayerChromeHidden(true)
                listScroll.isHidden = false
                activityLabel.isHidden = false
                spotifySearchField.isHidden = false
            } else {
                setPlayerChromeHidden(false)
            }
        }
        relayout()
        refreshPlaybackPresence()
    }

    private func setPlayerChromeHidden(_ hidden: Bool) {
        [nowPlaying, titleLabel, artistLabel, laneLabel, activityLabel, playButton,
         ledLabel, notesToggle, roomButton, speedSlider, speedLabel, speedResetButton,
         sortPopup, spotifySearchField,
         listScroll, wave, spotifyProgress, drawerPanel].forEach { $0?.isHidden = hidden }
        transportExtra.forEach { $0.isHidden = hidden }
        linkButtons.forEach { $0.isHidden = hidden }
        guard !hidden else { return }

        nowPlaying.isHidden = false
        titleLabel.isHidden = false
        artistLabel.isHidden = false
        laneLabel.isHidden = false
        activityLabel.isHidden = false
        playButton.isHidden = false
        ledLabel.isHidden = false
        roomButton.isHidden = false
        let externalSource = spotifyMode || appleMusicMode
        speedSlider.isHidden = externalSource
        speedLabel.isHidden = externalSource
        speedResetButton.isHidden = externalSource
        listScroll.isHidden = false
        transportExtra.forEach { $0.isHidden = false }
        wave.isHidden = externalSource
        spotifyProgress.isHidden = !externalSource
        sortPopup.isHidden = externalSource
        spotifySearchField.isHidden = !externalSource
        notesToggle.isHidden = false
        drawerPanel.isHidden = !drawerOpen
        if !spotifyMode, let t = track { loadLinks(t) }
    }

    private func applyThemeBackground() {
        let dark = window?.effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
        let chassis = Palette.bg(dark)
        window?.backgroundColor = .clear
        (window?.contentView as? JukeLiquidSurface)?.setTint(chassis)
        window?.contentView?.needsDisplay = true
        listTable?.reloadData()
    }

    private func applyAppearance() {
        switch appearanceMode {
        case .automatic: window?.appearance = nil
        case .light: window?.appearance = NSAppearance(named: .aqua)
        case .dark: window?.appearance = NSAppearance(named: .darkAqua)
        }
        djMixer?.setAppearance(window?.appearance)
        providerDeck?.needsDisplay = true
        applyThemeBackground()
    }

    @objc private func toggleDJMode() { setDJMode(!djMode) }

    private func setDJMode(_ enabled: Bool, singleDeck: Bool = false) {
        guard enabled != djMode else { return }
        if enabled {
            if spotifyMode { activateLibraryMode() }
            wave.pause()
            playButton.title = "▶"
            nowPlaying.setPaused(true)
            if drawerOpen {
                drawerOpen = false
                notesToggle.state = .off
            }
            if singleDeck {
                djMixer.configureSolo(tracks: library.tracks, primaryIndex: max(0, current))
            } else {
                djMixer.configure(tracks: library.tracks, primaryIndex: max(0, current))
            }
            djMixer.setMasterVolume(quickVolume)
            djMode = true
            djButton.state = .on
            djButton.contentTintColor = Palette.coral
            djMixer.isHidden = false
            setPlayerChromeHidden(true)
            djMixer.startDisplay()
            roomAudio.useSource(.aesthetic)
            window?.isMovableByWindowBackground = false
            window?.title = "JukeWizard · DJ"
        } else {
            djMixer.pauseAll()
            djMixer.stopDisplay()
            djMixer.isHidden = true
            djMode = false
            djButton.state = .off
            djButton.contentTintColor = Palette.teal
            window?.isMovableByWindowBackground = true
            setPlayerChromeHidden(false)
            window?.title = spotifyMode ? "JukeWizard · Spotify" : "JukeWizard — \(library.tracks.count) tracks"
        }
        relayout()
        refreshMenuBar()
    }

    private func setPlayerChromeHidden(_ hidden: Bool) {
        [nowPlaying, titleLabel, artistLabel, laneLabel, activityLabel, playButton,
         ledLabel, notesToggle, roomButton, sortPopup, spotifySearchField,
         listScroll, wave, spotifyProgress, drawerPanel].forEach { $0?.isHidden = hidden }
        transportExtra.forEach { $0.isHidden = hidden }
        linkButtons.forEach { $0.isHidden = hidden }
        guard !hidden else { return }

        nowPlaying.isHidden = false
        titleLabel.isHidden = false
        artistLabel.isHidden = false
        laneLabel.isHidden = false
        activityLabel.isHidden = false
        playButton.isHidden = false
        ledLabel.isHidden = false
        roomButton.isHidden = false
        listScroll.isHidden = false
        transportExtra.forEach { $0.isHidden = false }
        wave.isHidden = spotifyMode
        spotifyProgress.isHidden = !spotifyMode
        sortPopup.isHidden = spotifyMode
        spotifySearchField.isHidden = !spotifyMode
        notesToggle.isHidden = false
        drawerPanel.isHidden = !drawerOpen
        if !spotifyMode, let t = track { loadLinks(t) }
    }

    func showDetachedPrimpats() {
        if !djMode { setDJMode(true) }
        djMixer.loadPrimpats(openPopouts: true)
    }

    func showDetachedRecords() {
        if !djMode { setDJMode(true) }
        djMixer.loadLibraryRecords(openPopouts: true)
    }

    func showDetachedBeats() {
        if !djMode { setDJMode(true, singleDeck: true) }
        djMixer.loadBeats(openPopouts: true, autoplay: true, solo: true)
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
        djButton.frame = NSRect(x: 184, y: H - 28, width: 52, height: 24)
        let outputX: CGFloat = 242
        cloudButton.frame = NSRect(x: appearanceTabs.frame.minX - 50, y: H - 28,
                                   width: 44, height: 24)
        let outputRight = cloudButton.frame.minX - 6
        outputPopup.frame = NSRect(x: outputX, y: H - 27,
                                   width: max(110, min(260, outputRight - outputX)), height: 22)
        if djMode {
            djMixer.frame = NSRect(x: pad, y: pad, width: W - pad * 2,
                                   height: H - topBarH - pad)
            return
        }
        // ── header (now-playing) across the top ───────────────────────────────
        let headerH = max(178, min(245, (usableTop - topBarH) * 0.39))
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
        let speedY = transY + 30
        speedLabel.frame = NSRect(x: rx, y: speedY + 1, width: 45, height: 18)
        speedSlider.frame = NSRect(x: rx + 48, y: speedY, width: max(70, rw - 80), height: 20)
        speedResetButton.frame = NSRect(x: rx + rw - 27, y: speedY, width: 27, height: 20)
        let waveBottom = speedY + 23
        wave.frame = NSRect(x: rx, y: waveBottom, width: rw, height: max(32, waveTop - waveBottom))
        spotifyProgress.frame = wave.frame

        // ── track list underneath ─────────────────────────────────────────────
        let sortY = headerBottom - 2 - 20
        sortPopup.frame = NSRect(x: pad, y: sortY, width: 180, height: 20)
        let searchX = externalSource ? pad : pad + 184
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
        let bpm = djMode ? djMixer.dominantBPM
            : (spotifyMode ? nil : track?.meta?.bpm.map(Double.init) ?? wave.detectedBPM)
        let playing = djMode ? djMixer.isPlaying
            : (spotifyMode ? (spotifyState?.isPlaying ?? false) : wave.isPlaying)
        let title = djMode ? djMixer.dominantTitle
            : (spotifyMode
                ? MenuBarCD.credit(artist: spotifyState?.artists,
                                   title: spotifyState?.title ?? "")
                : MenuBarCD.credit(
                    artist: track.map { $0.meta?.artist ?? "Aesthetic Dot Computer" },
                    title: track?.title ?? ""))
        menuBar?.setBPM(bpm)
        menuBar?.setNowPlaying(title: title, art: currentArt)
        menuBar?.setPlaying(playing)
        menuBar?.setVolume(quickVolume)
        DockIcon.setNowPlaying(art: currentArt, playing: playing, bpm: bpm)
        miniPlayer?.refresh()
    }

    private func refreshPlaylistScopeButton() {
        guard let scopeButton else { return }
        scopeButton.isHidden = playlistName == nil
        scopeButton.title = playlistName.map { "\($0)  ·  All tracks" } ?? ""
    }

    @objc private func escapePlaylist() {
        guard playlistName != nil else { return }
        let currentPath = track?.url.standardizedFileURL.path
        library.add(path: fullLibraryPath)
        playlistName = nil
        refreshPlaylistScopeButton()
        if let currentPath,
           let index = library.tracks.firstIndex(where: {
               $0.url.standardizedFileURL.path == currentPath
           }) {
            current = index
        }
        listTable.reloadData()
        activityLabel.stringValue = "● full Aesthetic library · \(library.tracks.count) tracks"
        activityLabel.textColor = Palette.teal
        relayout()
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

    // ── music sources ─────────────────────────────────────────────────────
    private func activateSource(_ source: JukeSource) {
        let departingSource = activeSource
        if departingSource == .spotify, source != .spotify { spotify.pause() }
        if departingSource == .appleMusic, source != .appleMusic {
            appleMusicPlaying = false
            if #available(macOS 14.0, *) { appleMusic.pause() }
        }
        if source == .spotify || source == .appleMusic {
            djMixer.pauseAll()
        }
        activeSource = source
        spotifyMode = source == .spotify
        appleMusicMode = source == .appleMusic
        sourceTabs?.selectedSegment = source.rawValue
        djMixer?.setRecordDetachmentAllowed(source.canDetachRecords)

        if drawerOpen {
            drawerOpen = false
            drawerPanel?.isHidden = true
            notesToggle?.state = .off
        }

        switch source {
        case .local:
            providerDeck.isHidden = true
            sourceActionButton.title = "Add files…"
            sourceActionButton.isEnabled = true
            spotifySearchField.placeholderString = "Search \(source.label())"
            setDJMode(true, singleDeck: true)
            activityLabel.stringValue = "\(library.tracks.count) tracks on \(source.label())"
            activityLabel.textColor = Palette.gold
            listTable.reloadData()
            pollActivityStatus()
        case .aesthetic:
            providerDeck.isHidden = true
            sourceActionButton.title = ACSession.shared.token() == nil ? "Sign in" : "Publish…"
            sourceActionButton.isEnabled = true
            spotifySearchField.placeholderString = "Search Aesthetic releases"
            setDJMode(true, singleDeck: true)
            activityLabel.stringValue = "Loading Aesthetic releases…"
            activityLabel.textColor = Palette.coral
            loadAestheticCloud()
        case .spotify:
            sourceActionButton.title = "Connected"
            sourceActionButton.isEnabled = false
            spotifySearchField.placeholderString = "Search Spotify"
            if djMode { setDJMode(false) }
            configureExternalSource()
            providerDeck.configure(source: .spotify)
            providerDeck.update(title: spotifyState?.title ?? "",
                                artist: spotifyState?.artists ?? "",
                                album: spotifyState?.album ?? "",
                                art: spotifyArt,
                                duration: spotifyState?.duration ?? 0,
                                position: spotifyState?.position ?? 0,
                                playing: spotifyState?.isPlaying ?? false,
                                canSeek: true)
            roomAudio.useSource(.spotify)
            currentArt = spotifyArt
            activityLabel.stringValue = "● juked headless · connecting"
            activityLabel.textColor = Palette.teal
            if let state = spotifyState { renderSpotifyState(state) }
        case .appleMusic:
            sourceActionButton.title = "Connect"
            sourceActionButton.isEnabled = true
            spotifySearchField.placeholderString = "Search Apple Music"
            if djMode { setDJMode(false) }
            configureExternalSource()
            providerDeck.configure(source: .appleMusic)
            let selected = appleMusicResults.indices.contains(selectedAppleMusicRow)
                ? appleMusicResults[selectedAppleMusicRow] : nil
            providerDeck.update(title: selected?.title ?? "",
                                artist: selected?.artist ?? "",
                                album: selected?.album ?? "",
                                art: appleMusicArt,
                                duration: selected?.duration ?? 0,
                                position: 0,
                                playing: appleMusicPlaying,
                                canSeek: false)
            activityLabel.stringValue = if #available(macOS 14.0, *) {
                "● Apple Music · ready to connect"
            } else {
                "⚠ Apple Music requires macOS 14 or newer"
            }
            activityLabel.textColor = if #available(macOS 14.0, *) { .labelColor } else { .systemRed }
        }
        relayout()
        refreshPlaybackPresence()
    }

    private func configureExternalSource() {
        setPlayerChromeHidden(true)
        providerDeck?.isHidden = false
        listScroll?.isHidden = false
        activityLabel?.isHidden = false
        sortPopup?.isHidden = true
        spotifySearchField?.isHidden = false
        notesToggle?.isEnabled = false
        listTable?.reloadData()
    }

    private func refreshProviderDeck() {
        guard !providerDeck.isHidden else { return }
        if spotifyMode {
            providerDeck.update(title: spotifyState?.title ?? "",
                                artist: spotifyState?.artists ?? "",
                                album: spotifyState?.album ?? "",
                                art: spotifyArt,
                                duration: spotifyState?.duration ?? 0,
                                position: spotifyState?.position ?? 0,
                                playing: spotifyState?.isPlaying ?? false,
                                canSeek: true)
        } else if appleMusicMode {
            let selected = appleMusicResults.indices.contains(selectedAppleMusicRow)
                ? appleMusicResults[selectedAppleMusicRow] : nil
            providerDeck.update(title: selected?.title ?? "",
                                artist: selected?.artist ?? "",
                                album: selected?.album ?? "",
                                art: appleMusicArt,
                                duration: selected?.duration ?? 0,
                                position: 0,
                                playing: appleMusicPlaying,
                                canSeek: false)
        }
    }

    private func activateSpotifyMode() { activateSource(.spotify) }
    private func activateLibraryMode() { activateSource(.local) }

    @objc private func sourceTabChanged() {
        if djMode { setDJMode(false) }
        sourceTabs.selectedSegment == 0 ? activateSpotifyMode() : activateLibraryMode()
    }

    @objc private func searchActiveSourceFromField() {
        switch activeSource {
        case .local: break
        case .aesthetic:
            selectedCloudRow = filteredCloudTracks.isEmpty ? -1 : 0
            activityLabel.stringValue = "\(filteredCloudTracks.count) Aesthetic release\(filteredCloudTracks.count == 1 ? "" : "s")"
            listTable.reloadData()
        case .spotify: searchSpotify(spotifySearchField.stringValue)
        case .appleMusic: searchAppleMusic(spotifySearchField.stringValue)
        }
    }

    private var filteredCloudTracks: [JukeCloudTrack] {
        let query = spotifySearchField?.stringValue.trimmingCharacters(in: .whitespacesAndNewlines) ?? ""
        guard !query.isEmpty else { return cloudTracks }
        return cloudTracks.filter { $0.name.localizedCaseInsensitiveContains(query) }
    }

    @objc private func sourceActionClicked() {
        switch activeSource {
        case .local:
            let panel = NSOpenPanel()
            panel.allowsMultipleSelection = true
            panel.canChooseDirectories = true
            panel.beginSheetModal(for: window!) { [weak self] response in
                guard response == .OK, let self else { return }
                panel.urls.forEach { self.library.add(path: $0.path) }
                self.listTable.reloadData()
                self.activityLabel.stringValue = "\(self.library.tracks.count) tracks on \(JukeSource.local.label())"
            }
        case .aesthetic:
            if ACSession.shared.token() == nil {
                ACLogin.shared.signIn { [weak self] result in
                    guard case .success = result else { return }
                    self?.sourceActionButton.title = "Publish…"
                    self?.loadAestheticCloud()
                }
            } else {
                publishToAesthetic()
            }
        case .spotify:
            break
        case .appleMusic:
            connectAppleMusic()
        }
    }

    private func loadAestheticCloud() {
        guard ACSession.shared.token() != nil else {
            cloudTracks = []
            selectedCloudRow = -1
            activityLabel.stringValue = "Sign in to browse Aesthetic releases"
            listTable.reloadData()
            return
        }
        Task { [weak self] in
            guard let self else { return }
            do {
                let tracks = try await cloud.list()
                await MainActor.run {
                    guard self.activeSource == .aesthetic else { return }
                    self.cloudTracks = tracks
                    self.selectedCloudRow = tracks.isEmpty ? -1 : 0
                    self.activityLabel.stringValue = tracks.isEmpty
                        ? "No Aesthetic releases"
                        : "\(tracks.count) Aesthetic release\(tracks.count == 1 ? "" : "s")"
                    self.listTable.reloadData()
                }
            } catch {
                await MainActor.run {
                    self.activityLabel.stringValue = error.localizedDescription
                    self.activityLabel.textColor = .systemRed
                }
            }
        }
    }

    private func publishToAesthetic() {
        let panel = NSOpenPanel()
        panel.allowsMultipleSelection = true
        panel.canChooseDirectories = false
        panel.beginSheetModal(for: window!) { [weak self] response in
            guard response == .OK, let self else { return }
            self.activityLabel.stringValue = "Publishing…"
            Task {
                do {
                    for url in panel.urls { _ = try await self.cloud.upload(file: url) }
                    await MainActor.run { self.loadAestheticCloud() }
                } catch {
                    await MainActor.run {
                        self.activityLabel.stringValue = error.localizedDescription
                        self.activityLabel.textColor = .systemRed
                    }
                }
            }
        }
    }

    private func playAestheticCloudResult(at row: Int) {
        let visibleTracks = filteredCloudTracks
        guard visibleTracks.indices.contains(row) else { return }
        selectedCloudRow = row
        listTable.reloadData()
        let cloudTrack = visibleTracks[row]
        activityLabel.stringValue = "Loading \(cloudTrack.name)…"
        Task { [weak self] in
            guard let self else { return }
            do {
                let url = try await cloud.download(cloudTrack)
                await MainActor.run {
                    self.library.addFile(url, lane: "Aesthetic")
                    guard let track = self.library.tracks.first(where: {
                        $0.url.standardizedFileURL == url.standardizedFileURL
                    }) else { return }
                    self.djMixer.loadPrimary(track)
                    self.activityLabel.stringValue = "Pull the record off the bed to float it"
                    self.refreshPlaybackPresence()
                }
            } catch {
                await MainActor.run {
                    self.activityLabel.stringValue = error.localizedDescription
                    self.activityLabel.textColor = .systemRed
                }
            }
        }
    }

    private func connectAppleMusic() {
        guard #available(macOS 14.0, *) else { return }
        activityLabel.stringValue = "Connecting Apple Music…"
        Task { [weak self] in
            guard let self else { return }
            do {
                try await appleMusic.authorize()
                await MainActor.run {
                    self.sourceActionButton.title = "Connected"
                    self.sourceActionButton.isEnabled = false
                    self.activityLabel.stringValue = "● Apple Music · connected"
                    self.spotifySearchField.becomeFirstResponder()
                }
            } catch {
                await MainActor.run {
                    self.activityLabel.stringValue = error.localizedDescription
                    self.activityLabel.textColor = .systemRed
                }
            }
        }
    }

    private func searchAppleMusic(_ rawQuery: String) {
        guard #available(macOS 14.0, *) else { return }
        let query = rawQuery.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !query.isEmpty else { return }
        activityLabel.stringValue = "Searching Apple Music for “\(query)”…"
        Task { [weak self] in
            guard let self else { return }
            do {
                let results = try await appleMusic.search(query)
                await MainActor.run {
                    guard self.activeSource == .appleMusic else { return }
                    self.appleMusicResults = results
                    self.selectedAppleMusicRow = results.isEmpty ? -1 : 0
                    self.sourceActionButton.title = "Connected"
                    self.sourceActionButton.isEnabled = false
                    self.activityLabel.stringValue = results.isEmpty
                        ? "No Apple Music tracks for “\(query)”"
                        : "\(results.count) Apple Music tracks"
                    self.listTable.reloadData()
                }
            } catch {
                await MainActor.run {
                    self.activityLabel.stringValue = error.localizedDescription
                    self.activityLabel.textColor = .systemRed
                }
            }
        }
    }

    private func playAppleMusicResult(at row: Int) {
        guard #available(macOS 14.0, *), appleMusicResults.indices.contains(row) else { return }
        selectedAppleMusicRow = row
        let result = appleMusicResults[row]
        titleLabel.stringValue = result.title
        titleLabel.textColor = .labelColor
        artistLabel.stringValue = result.artist
        laneLabel.stringValue = [result.album, "Apple Music · stays on this deck"]
            .filter { !$0.isEmpty }.joined(separator: " · ")
        spotifyProgress.duration = result.duration
        spotifyProgress.position = 0
        playButton.title = "❚❚"
        appleMusicPlaying = true
        djMixer.pauseAll()
        spotify.pause()
        listTable.reloadData()
        refreshProviderDeck()
        presentAppleMusicArtwork(result.artworkURL)
        Task { [weak self] in
            do {
                try await self?.appleMusic.play(result)
                await MainActor.run { self?.refreshPlaybackPresence() }
            }
            catch {
                await MainActor.run {
                    self?.appleMusicPlaying = false
                    self?.activityLabel.stringValue = error.localizedDescription
                    self?.activityLabel.textColor = .systemRed
                    self?.refreshProviderDeck()
                    self?.refreshPlaybackPresence()
                }
            }
        }
    }

    private func presentAppleMusicArtwork(_ url: URL?) {
        appleMusicArtworkURL = url
        appleMusicArt = nil
        currentArt = nil
        refreshProviderDeck()
        guard let url else { nowPlaying.present(art: nil, videoURL: nil); return }
        URLSession.shared.dataTask(with: url) { [weak self] data, _, _ in
            guard let data, let art = NSImage(data: data) else { return }
            DispatchQueue.main.async {
                guard self?.appleMusicArtworkURL == url else { return }
                self?.appleMusicArt = art
                self?.currentArt = art
                self?.nowPlaying.present(art: art, videoURL: nil)
                self?.nowPlaying.setPaused(false)
                self?.refreshProviderDeck()
                self?.refreshPlaybackPresence()
            }
        }.resume()
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
        laneLabel.stringValue = [result.album, "Spotify · stays on this deck"].filter { !$0.isEmpty }.joined(separator: " · ")
        spotifyProgress.duration = result.duration
        spotifyProgress.position = 0
        playButton.title = "❚❚"
        djMixer.pauseAll()
        if #available(macOS 14.0, *) { appleMusic.pause() }
        appleMusicPlaying = false
        providerDeck.update(title: result.title, artist: result.artists,
                            album: result.album, art: nil,
                            duration: result.duration, position: 0,
                            playing: true, canSeek: true)
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
        laneLabel.stringValue = [state.album, "Spotify · stays on this deck"].filter { !$0.isEmpty }.joined(separator: " · ")
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
        refreshProviderDeck()
        if state.artworkURL != spotifyArtworkURL {
            spotifyArtworkURL = state.artworkURL
            spotifyArt = nil
            currentArt = nil
            refreshProviderDeck()
            guard let url = state.artworkURL else { nowPlaying.present(art: nil, videoURL: nil); return }
            URLSession.shared.dataTask(with: url) { [weak self] data, _, _ in
                guard let data, let art = NSImage(data: data) else { return }
                DispatchQueue.main.async {
                    guard self?.spotifyArtworkURL == url else { return }
                    self?.spotifyArt = art
                    self?.currentArt = art
                    self?.nowPlaying.present(art: art, videoURL: nil)
                    self?.nowPlaying.setPaused(!state.isPlaying)
                    self?.refreshProviderDeck()
                    self?.refreshPlaybackPresence()
                }
            }.resume()
        }
        refreshPlaybackPresence()
    }
    var quickTitle: String {
        if djMode { return djMixer.dominantTitle }
        if spotifyMode { return spotifyState?.title ?? "Spotify" }
        if appleMusicMode, appleMusicResults.indices.contains(selectedAppleMusicRow) {
            return appleMusicResults[selectedAppleMusicRow].title
        }
        return track?.title ?? activeSource.label()
    }
    var quickSubtitle: String {
        if djMode { return String(format: "DJ · %.1f BPM", djMixer.dominantBPM) }
        if spotifyMode { return [spotifyState?.artists ?? "", "Spotify"].filter { !$0.isEmpty }.joined(separator: " · ") }
        if appleMusicMode, appleMusicResults.indices.contains(selectedAppleMusicRow) {
            return [appleMusicResults[selectedAppleMusicRow].artist, "Apple Music"]
                .filter { !$0.isEmpty }.joined(separator: " · ")
        }
        return [track?.meta?.artist ?? "Aesthetic Dot Computer", "Aesthetic"].joined(separator: " · ")
    }
    var quickIsPlaying: Bool {
        djMode ? djMixer.isPlaying
            : (spotifyMode ? (spotifyState?.isPlaying ?? false)
               : (appleMusicMode ? appleMusicPlaying : wave.isPlaying))
    }
    var quickRoomSummary: String {
        switch roomAudio.state {
        case .idle: return "room · off"
        case .failed(let message): return "room · ⚠ \(message)"
        case .live(let snapshot): return "\(snapshot.neo == "off" ? "" : "Neo \(snapshot.neo)")\(snapshot.neo != "off" && snapshot.blueberry != "off" ? "  ·  " : "")\(snapshot.blueberry == "off" ? "" : "Blueberry \(snapshot.blueberry)")"
        }
    }

    @objc func quickOpenFull() {
        miniPopover?.close()
        guard let w = window else { return }
        if w.isMiniaturized { w.deminiaturize(nil) }
        w.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)
    }

    @objc func quickToggleFull() {
        guard let w = window else { return }
        if w.isVisible, !w.isMiniaturized {
            miniPopover?.close()
            w.orderOut(nil)
        } else {
            quickOpenFull()
        }
    }

    @objc func quickTogglePlay() { togglePlay(); miniPlayer?.refresh() }
    @objc func quickPrevious() { prevTrack(); miniPlayer?.refresh() }
    @objc func quickNext() { nextTrack(); miniPlayer?.refresh() }
    @objc func quickVolumeChanged(_ sender: NSSlider) {
        quickVolume = max(0, min(1, sender.floatValue))
        wave.volume = quickVolume
        djMixer.setMasterVolume(quickVolume)
        spotify.volume(percent: Int((quickVolume * 100).rounded()))
        UserDefaults.standard.set(quickVolume, forKey: "playerVolume")
        miniPlayer?.refresh()
    }

    @objc func quickVolumeUp() { setQuickVolume(quickVolume + 0.1) }
    @objc func quickVolumeDown() { setQuickVolume(quickVolume - 0.1) }

    private func reloadOutputDevices() {
        let current = MacAudioOutput.defaultDeviceID()
        outputDevices = MacAudioOutput.devices()
        outputPopup.removeAllItems()
        guard !outputDevices.isEmpty else {
            outputPopup.addItem(withTitle: "No audio outputs")
            outputPopup.isEnabled = false
            return
        }
        outputPopup.isEnabled = true
        for device in outputDevices {
            outputPopup.addItem(withTitle: device.name)
            outputPopup.lastItem?.image = NSImage(systemSymbolName: device.symbolName,
                                                  accessibilityDescription: device.name)
        }
        if let index = outputDevices.firstIndex(where: { $0.id == current }) {
            outputPopup.selectItem(at: index)
            outputPopup.toolTip = "Mac audio output · \(outputDevices[index].name)"
        }
    }

    @objc private func outputDeviceChanged(_ sender: NSPopUpButton) {
        guard outputDevices.indices.contains(sender.indexOfSelectedItem) else { return }
        chooseOutput(outputDevices[sender.indexOfSelectedItem])
    }

    @objc private func outputDeviceMenuItem(_ sender: NSMenuItem) {
        guard let id = (sender.representedObject as? NSNumber)?.uint32Value,
              let device = MacAudioOutput.devices().first(where: { $0.id == id }) else { return }
        chooseOutput(device)
    }

    private func chooseOutput(_ device: MacAudioOutput.Device) {
        guard device.id != MacAudioOutput.defaultDeviceID() else { reloadOutputDevices(); return }
        do {
            try MacAudioOutput.select(device)
            reloadOutputDevices()
            wave.reopenAudioOutput()
            if roomAudio.isDistributing {
                roomAudio.refreshLocalOutputDevice()
            } else if spotifyMode {
                spotify.refreshOutputDevice(resuming: spotifyState)
            }
            activityLabel.stringValue = "● output · \(device.name)"
            activityLabel.textColor = Palette.teal
        } catch {
            reloadOutputDevices()
            activityLabel.stringValue = "⚠ \(error.localizedDescription)"
            activityLabel.textColor = .systemRed
            NSSound.beep()
        }
    }

    private func setQuickVolume(_ value: Float) {
        quickVolume = max(0, min(1, value))
        wave.volume = quickVolume
        djMixer.setMasterVolume(quickVolume)
        spotify.volume(percent: Int((quickVolume * 100).rounded()))
        UserDefaults.standard.set(quickVolume, forKey: "playerVolume")
        miniPlayer?.refresh()
    }

    func makeDockMenu() -> NSMenu {
        let menu = NSMenu(title: "Menu Band Juke")
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
        let output = NSMenuItem(title: "Audio Output", action: nil, keyEquivalent: "")
        let outputMenu = NSMenu(title: "Audio Output")
        let currentOutput = MacAudioOutput.defaultDeviceID()
        for device in MacAudioOutput.devices() {
            let item = NSMenuItem(title: device.name, action: #selector(outputDeviceMenuItem(_:)),
                                  keyEquivalent: "")
            item.target = self
            item.representedObject = NSNumber(value: device.id)
            item.state = device.id == currentOutput ? .on : .off
            item.image = NSImage(systemSymbolName: device.symbolName, accessibilityDescription: device.name)
            outputMenu.addItem(item)
        }
        output.submenu = outputMenu
        menu.addItem(output)
        let open = NSMenuItem(title: "Open JukeWizard", action: #selector(quickOpenFull), keyEquivalent: "")
        open.target = self
        menu.addItem(open)
        return menu
    }

    @objc private func showCloud() {
        if cloudWindow == nil {
            cloudWindow = JukeCloudWindowController(
                currentFile: { [weak self] in self?.track?.url },
                loadLocalFile: { [weak self] in self?.loadCloudFile($0) })
        }
        cloudWindow?.prepareForDisplay()
        cloudWindow?.showWindow(nil)
        cloudWindow?.window?.makeKeyAndOrderFront(nil)
        NSApp.activate(ignoringOtherApps: true)
    }

    private func loadCloudFile(_ url: URL) {
        if let index = library.tracks.firstIndex(where: {
            $0.url.standardizedFileURL == url.standardizedFileURL
        }) {
            select(index, autoplay: true)
            return
        }
        library.addFile(url, lane: "cloud")
        listTable.reloadData()
        if let index = library.tracks.firstIndex(where: {
            $0.url.standardizedFileURL == url.standardizedFileURL
        }) {
            select(index, autoplay: true)
        }
    }

    // ── selection / playback ──────────────────────────────────────────────
    private var track: Track? { (current >= 0 && current < library.tracks.count) ? library.tracks[current] : nil }

    func select(_ i: Int, autoplay: Bool) {
        guard i >= 0, i < library.tracks.count else { return }
        if spotifyMode { spotify.pause(); activateLibraryMode() }
        commitNotes()
        let old = current
        current = i
        lastDisplayedBPM = nil
        let t = library.tracks[i]
        titleLabel.stringValue = t.title
        titleLabel.textColor = Self.statusColor(t.meta?.status)
        artistLabel.stringValue = t.meta?.artist ?? "Aesthetic Dot Computer"
        laneLabel.stringValue = Self.metaLine(t)
        laneLabel.textColor = .secondaryLabelColor
        updateNowPlaying(t)
        loadLinks(t)
        relayout()                    // link count changes the header row width
        if djMode {
            djMixer.loadPrimary(t, autoplay: autoplay)
            var rows = IndexSet(integer: i)
            if old >= 0, old < library.tracks.count { rows.insert(old) }
            if activeSource == .local {
                listTable.reloadData(forRowIndexes: rows, columnIndexes: IndexSet(integer: 0))
                listTable.scrollRowToVisible(i)
            }
            refreshPlaybackPresence()
            return
        }
        wave.load(track: t)
        wave.playbackRate = speedSlider.doubleValue
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
        refreshPlaybackPresence()
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
        switch activeSource {
        case .local: if r >= 0 { select(r, autoplay: true) }
        case .aesthetic: playAestheticCloudResult(at: r)
        case .spotify: playSpotifyResult(at: r)
        case .appleMusic: playAppleMusicResult(at: r)
        }
    }
    @objc private func togglePlay() {
        if djMode {
            djMixer.toggleDominant()
        } else if spotifyMode {
            spotify.toggle()
            let playing = !(spotifyState?.isPlaying ?? false)
            playButton.title = playing ? "❚❚" : "▶"
            nowPlaying.setPaused(!playing)
        } else if appleMusicMode {
            guard #available(macOS 14.0, *) else { return }
            appleMusicPlaying.toggle()
            playButton.title = appleMusicPlaying ? "❚❚" : "▶"
            nowPlaying.setPaused(!appleMusicPlaying)
            Task { [weak self] in
                do { try await self?.appleMusic.toggle() }
                catch {
                    await MainActor.run {
                        self?.appleMusicPlaying.toggle()
                        self?.refreshPlaybackPresence()
                    }
                }
            }
        } else {
            wave.togglePlay()
            playButton.title = wave.isPlaying ? "❚❚" : "▶"
            nowPlaying.setPaused(!wave.isPlaying)
        }
        refreshProviderDeck()
        refreshPlaybackPresence()
    }
    @objc private func speedChanged(_ sender: NSSlider) {
        guard !spotifyMode, !djMode else { return }
        wave.playbackRate = sender.doubleValue
        speedLabel.stringValue = String(format: "%.2f×", sender.doubleValue)
        UserDefaults.standard.set(sender.doubleValue, forKey: "listPlaybackRate")
    }
    @objc private func resetSpeed() {
        speedSlider.doubleValue = 1
        speedChanged(speedSlider)
    }
    @objc private func prevTrack() {
        if djMode { djMixer.stepDominant(by: -1) }
        else if spotifyMode { spotify.previous() }
        else if appleMusicMode { return }
        else if current > 0 { select(current - 1, autoplay: true) }
    }
    @objc private func nextTrack() {
        if djMode { djMixer.stepDominant(by: 1) }
        else if spotifyMode { spotify.next() }
        else if appleMusicMode { return }
        else if current < library.tracks.count - 1 { select(current + 1, autoplay: true) }
    }

    /// Stable command surface used by the user-only Unix socket. This is
    /// always called on AppKit's main thread by JukeControlServer.
    func control(_ request: [String: Any]) -> [String: Any] {
        guard Thread.isMainThread else { return ["ok": false, "error": "control must run on main thread"] }
        guard let command = request["command"] as? String else {
            return ["ok": false, "error": "missing command"]
        }
        func state() -> [String: Any] {
            var out: [String: Any] = [
                "ok": true,
                "mode": djMode ? "dj" : (spotifyMode ? "spotify" : (appleMusicMode ? "appleMusic" : "library")),
                "source": String(describing: activeSource),
                "playing": quickIsPlaying, "index": current,
                "queueCount": library.tracks.count
            ]
            if let t = track { out["title"] = t.title; out["path"] = t.url.path; out["lane"] = t.lane }
            if !spotifyMode && !appleMusicMode && !djMode {
                out["position"] = wave.currentTime; out["duration"] = wave.duration
                out["speed"] = wave.playbackRate
            }
            return out
        }
        switch command {
        case "status": return state()
        case "source":
            guard let rawSource = request["source"] as? String else {
                return ["ok": false, "error": "source requires local, aesthetic, spotify, or appleMusic"]
            }
            let source: JukeSource?
            switch rawSource.lowercased() {
            case "local": source = .local
            case "aesthetic": source = .aesthetic
            case "spotify": source = .spotify
            case "apple", "applemusic", "apple-music": source = .appleMusic
            default: source = nil
            }
            guard let source else {
                return ["ok": false, "error": "unknown source"]
            }
            activateSource(source)
            return state()
        case "detach":
            guard djMode, activeSource.canDetachRecords else {
                return ["ok": false, "error": "this source must stay on the main deck"]
            }
            guard djMixer.detachPrimary() else {
                return ["ok": false, "error": "the main bed has no record"]
            }
            return state()
        case "list":
            let limit = max(1, min(1000, request["limit"] as? Int ?? 500))
            let rows: [[String: Any]] = library.tracks.prefix(limit).enumerated().map { i, t in
                ["index": i, "title": t.title, "path": t.url.path, "lane": t.lane]
            }
            return ["ok": true, "count": library.tracks.count, "tracks": rows, "truncated": rows.count < library.tracks.count]
        case "select", "play":
            if let path = request["path"] as? String {
                let wanted = URL(fileURLWithPath: (path as NSString).expandingTildeInPath).standardizedFileURL.path
                guard let i = library.tracks.firstIndex(where: { $0.url.standardizedFileURL.path == wanted }) else {
                    return ["ok": false, "error": "exact path is not in the queue"]
                }
                select(i, autoplay: command == "play")
            } else if let title = request["title"] as? String {
                let matches = library.tracks.indices.filter { library.tracks[$0].title == title }
                guard matches.count == 1 else {
                    return ["ok": false, "error": matches.isEmpty ? "exact title not found" : "title is ambiguous; use path or index"]
                }
                select(matches[0], autoplay: command == "play")
            } else if let i = request["index"] as? Int {
                guard library.tracks.indices.contains(i) else { return ["ok": false, "error": "index out of range"] }
                select(i, autoplay: command == "play")
            } else if command == "play" {
                guard !spotifyMode, !djMode else { return ["ok": false, "error": "plain play is unavailable in Spotify or DJ mode"] }
                wave.play(); playButton.title = "❚❚"; nowPlaying.setPaused(false); refreshPlaybackPresence()
            } else { return ["ok": false, "error": "select requires exact path, title, or index"] }
            return state()
        case "pause":
            guard !spotifyMode, !djMode else { return ["ok": false, "error": "pause is unavailable in Spotify or DJ mode"] }
            wave.pause(); playButton.title = "▶"; nowPlaying.setPaused(true); refreshPlaybackPresence(); return state()
        case "toggle":
            guard !spotifyMode, !djMode else { return ["ok": false, "error": "toggle is unavailable in Spotify or DJ mode"] }
            togglePlay(); return state()
        case "seek":
            guard !spotifyMode, !djMode, let seconds = request["seconds"] as? Double, seconds.isFinite else {
                return ["ok": false, "error": "seek requires finite seconds in library mode"]
            }
            wave.seek(to: seconds); return state()
        case "speed":
            guard !spotifyMode, !djMode, let value = request["speed"] as? Double,
                  value.isFinite, (0.5...1.5).contains(value) else {
                return ["ok": false, "error": "speed must be between 0.5 and 1.5 in library mode"]
            }
            speedSlider.doubleValue = value; speedChanged(speedSlider); return state()
        case "next":
            guard !spotifyMode, !djMode else { return ["ok": false, "error": "next is unavailable in Spotify or DJ mode"] }
            nextTrack(); return state()
        case "previous":
            guard !spotifyMode, !djMode else { return ["ok": false, "error": "previous is unavailable in Spotify or DJ mode"] }
            prevTrack(); return state()
        default: return ["ok": false, "error": "unknown command"]
        }
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
        refreshPlaybackPresence()
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
    func waveformTick() {
        updateTime()
        let bpm = track?.meta?.bpm.map(Double.init) ?? wave.detectedBPM
        if abs((bpm ?? 0) - (lastDisplayedBPM ?? 0)) >= 0.5 {
            lastDisplayedBPM = bpm
            refreshMenuBar()
        }
    }
    private func updateTime() {
        if let state = spotifyMode ? spotifyState : nil {
            ledLabel.stringValue = "\(Self.mmss(state.position)) / \(Self.mmss(state.duration))"
        } else {
            let bpm = track?.meta?.bpm.map(Double.init) ?? wave.detectedBPM
            let tempo = bpm.map { String(format: " · ≈%.1f BPM", $0) } ?? ""
            ledLabel.stringValue = "\(Self.mmss(wave.currentTime)) / \(Self.mmss(wave.duration))\(tempo)"
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
        guard activeSource == .local, !activityPollInFlight else { return }
        activityPollInFlight = true
        let tracks = library.tracks
        DispatchQueue.global(qos: .utility).async { [weak self] in
            let activities = WorkStatus.snapshot(tracks: tracks)
            DispatchQueue.main.async {
                guard let self else { return }
                self.activityPollInFlight = false
                guard self.activeSource == .local else { return }
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
        if tableView == listTable {
            switch activeSource {
            case .local: return library.tracks.count
            case .aesthetic: return filteredCloudTracks.count
            case .spotify: return spotifyResults.count
            case .appleMusic: return appleMusicResults.count
            }
        }
        return track?.data.comments.count ?? 0
    }
    // list = dressed-up view rows; comments = plain cell strings.
    func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
        if tableView == listTable, activeSource == .spotify, row < spotifyResults.count {
            let cell = (listTable.makeView(withIdentifier: SpotifyTrackRowView.id, owner: self) as? SpotifyTrackRowView)
                ?? { let view = SpotifyTrackRowView(); view.identifier = SpotifyTrackRowView.id; return view }()
            cell.configure(spotifyResults[row])
            cell.selected = row == selectedSpotifyRow
            return cell
        }
        if tableView == listTable, activeSource == .aesthetic, row < filteredCloudTracks.count {
            let cell = (listTable.makeView(withIdentifier: AestheticCloudRowView.id, owner: self) as? AestheticCloudRowView)
                ?? { let view = AestheticCloudRowView(); view.identifier = AestheticCloudRowView.id; return view }()
            cell.configure(filteredCloudTracks[row])
            cell.selected = row == selectedCloudRow
            return cell
        }
        if tableView == listTable, activeSource == .appleMusic, row < appleMusicResults.count {
            let cell = (listTable.makeView(withIdentifier: AppleMusicTrackRowView.id, owner: self) as? AppleMusicTrackRowView)
                ?? { let view = AppleMusicTrackRowView(); view.identifier = AppleMusicTrackRowView.id; return view }()
            cell.configure(appleMusicResults[row])
            cell.selected = row == selectedAppleMusicRow
            return cell
        }
        guard activeSource == .local else { return nil }
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
        guard tableView == listTable, activeSource == .local,
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
