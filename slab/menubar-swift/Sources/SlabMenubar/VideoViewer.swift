// VideoViewer — slab's minimal video player, so "watch this render" doesn't
// mean QuickTime. The PdfViewer contract, but for movies: a chromeless
// glass panel with an AVPlayerView (floating controls) and NOTHING else on
// screen — no filename, no buttons. Right-click for Show in Finder / open
// elsewhere; Esc or ⌘W to dismiss. Playback starts immediately.
//
// How it gets asked: anything (Claude, a render script, the menu) appends
// an absolute path per line to $SLAB_HOME/state/open-video. Ordered playlists
// arrive as one atomic file in $SLAB_HOME/state/open-video-playlists/. The
// menubar's 2 s tick consumes both on the main thread (tiny stats — no
// shell-outs, per slab-menubar-perf). The `slab-video` wrapper in slab/bin
// writes either form. Re-requesting the same source brings its window forward
// and restarts playback. Single-video windows watch their file and reload on
// rewrite, so a render loop (build.mjs → same mp4 path) feels live.
//
// Managing a wall of them: touching $SLAB_HOME/state/close-video closes every
// panel, $SLAB_HOME/state/tile-video re-grids them; opening a second panel
// grids them automatically (the ImageGroupPreview layout). A playlist panel
// carries its queue as a list beside the picture — click a row to jump, the
// playing row is highlighted, and the queue advances on its own.
import AppKit
import AVKit
import AVFoundation

extension Paths {
    /// One absolute video path per line; consumed (deleted) each tick.
    static var videoRequestFile: String { "\(slabHome)/state/open-video" }
    /// One atomic request file per ordered playlist. Each request contains
    /// `path\tsession_id` lines and is removed after consumption.
    static var videoPlaylistRequestDirectory: String {
        "\(slabHome)/state/open-video-playlists"
    }
    /// Sentinels: their presence is the ask; consumed (deleted) each tick.
    static var videoCloseFile: String { "\(slabHome)/state/close-video" }
    static var videoTileFile: String { "\(slabHome)/state/tile-video" }
}

struct OpenVideoItem {
    let id: String
    let title: String
    let toolTip: String
}

final class VideoViewer {
    static let shared = VideoViewer()
    private var controllers: [String: VideoWindowController] = [:]

    var openItems: [OpenVideoItem] {
        controllers.map { id, controller in
            OpenVideoItem(id: id, title: controller.displayName,
                          toolTip: controller.sourcePaths.joined(separator: "\n"))
        }.sorted { $0.title.localizedStandardCompare($1.title) == .orderedAscending }
    }

    /// Called from the main-thread side of AppDelegate.refresh() every tick.
    /// Request lines are "path\tsession_id" (the session id may be empty);
    /// `emojiFor` maps a live Claude session to its sticky TitleEmoji so the
    /// chip wears the mark of the prompt that asked.
    func consumeRequests(emojiFor: (String) -> String = { _ in "" }) {
        if FileManager.default.fileExists(atPath: Paths.videoCloseFile) {
            try? FileManager.default.removeItem(atPath: Paths.videoCloseFile)
            closeAll()
        }
        if FileManager.default.fileExists(atPath: Paths.videoTileFile) {
            try? FileManager.default.removeItem(atPath: Paths.videoTileFile)
            tile()
        }
        let before = controllers.count
        defer { if controllers.count > before, controllers.count > 1 { tile() } }

        let file = Paths.videoRequestFile
        if FileManager.default.fileExists(atPath: file) {
            let text = (try? String(contentsOfFile: file, encoding: .utf8)) ?? ""
            try? FileManager.default.removeItem(atPath: file)
            for line in text.split(separator: "\n") {
                let parts = Self.parseRequestLine(line)
                if !parts.path.isEmpty {
                    open(parts.path, emoji: parts.sessionID.isEmpty ? "" : emojiFor(parts.sessionID))
                }
            }
        }

        let directory = Paths.videoPlaylistRequestDirectory
        guard let requests = try? FileManager.default.contentsOfDirectory(
            at: URL(fileURLWithPath: directory),
            includingPropertiesForKeys: nil,
            options: [.skipsHiddenFiles]
        ) else { return }
        for request in requests.filter({ $0.pathExtension == "playlist" })
            .sorted(by: { $0.lastPathComponent < $1.lastPathComponent }) {
            let text = (try? String(contentsOf: request, encoding: .utf8)) ?? ""
            try? FileManager.default.removeItem(at: request)
            let parts = text.split(separator: "\n").map(Self.parseRequestLine)
            let paths = parts.map(\.path).filter { !$0.isEmpty }
            let sessionID = parts.first(where: { !$0.sessionID.isEmpty })?.sessionID ?? ""
            if !paths.isEmpty {
                openPlaylist(paths, emoji: sessionID.isEmpty ? "" : emojiFor(sessionID))
            }
        }
    }

    private static func parseRequestLine(_ line: Substring) -> (path: String, sessionID: String) {
        let parts = line.split(separator: "\t", maxSplits: 1, omittingEmptySubsequences: false)
        return (
            parts[0].trimmingCharacters(in: .whitespaces),
            parts.count > 1 ? parts[1].trimmingCharacters(in: .whitespaces) : ""
        )
    }

    func open(_ rawPath: String, emoji: String = "") {
        openPlaylist([rawPath], emoji: emoji)
    }

    func openPlaylist(_ rawPaths: [String], emoji: String = "") {
        let paths = rawPaths.map { ($0 as NSString).expandingTildeInPath }
        guard !paths.isEmpty,
              paths.allSatisfy({ FileManager.default.fileExists(atPath: $0) }) else { return }
        let id = paths.joined(separator: "\u{0}")
        if let existing = controllers[id] { existing.focusAndRestart(); return }
        guard let controller = VideoWindowController(paths: paths, emoji: emoji, onClose: { [weak self] in
            self?.controllers.removeValue(forKey: id)
        }) else { return }
        controllers[id] = controller
        controller.focus()
    }

    func focus(_ id: String) { controllers[id]?.focus() }

    /// Grid every open panel across the main screen's visible frame, each
    /// aspect-fit inside its cell (same layout as ImageGroupPreview).
    func tile() {
        let ordered = controllers.values.sorted {
            $0.displayName.localizedStandardCompare($1.displayName) == .orderedAscending
        }
        let n = ordered.count
        guard n > 0 else { return }
        let screen = NSScreen.main?.visibleFrame
            ?? NSRect(x: 0, y: 0, width: 1440, height: 900)
        let cols = Int(ceil(Double(n).squareRoot()))
        let rows = Int(ceil(Double(n) / Double(cols)))
        let gap: CGFloat = 16
        let cellW = (screen.width - gap * CGFloat(cols + 1)) / CGFloat(cols)
        let cellH = (screen.height - gap * CGFloat(rows + 1)) / CGFloat(rows)
        for (i, c) in ordered.enumerated() {
            let col = i % cols
            let row = i / cols
            let x = screen.minX + gap + CGFloat(col) * (cellW + gap)
            let yTop = screen.maxY - gap - CGFloat(row) * (cellH + gap)
            c.fit(in: NSRect(x: x, y: yTop - cellH, width: cellW, height: cellH))
        }
    }

    func closeAll() {
        // close() triggers onClose which mutates the dictionary — iterate a copy.
        for controller in Array(controllers.values) { controller.close() }
    }
}

/// One panel per movie or ordered playlist. Owns the AVPlayerView and, for a
/// single movie, a file monitor that reloads rewritten renders in place.
private final class VideoWindowController: NSObject, NSWindowDelegate {
    let sourcePaths: [String]
    private let emoji: String
    private let panel: VideoPanel
    private let playerView = ContextPlayerView()
    private let player = AVQueuePlayer()
    private let onClose: () -> Void
    private var monitor: DispatchSourceFileSystemObject?
    private var reloadPending = false
    private var currentItemObservation: NSKeyValueObservation?
    private var currentIndex = 0
    /// Picture aspect (w/h) of the first movie, for grid fitting.
    private let aspect: CGFloat
    /// The queue beside the picture — only for playlists.
    private let playlist: PlaylistList?
    private static let playlistWidth: CGFloat = 220

    var displayName: String {
        if sourcePaths.count == 1 { return (sourcePaths[0] as NSString).lastPathComponent }
        return "\(sourcePaths.count) videos — \((sourcePaths[0] as NSString).lastPathComponent)"
    }

    private var currentPath: String {
        if let asset = player.currentItem?.asset as? AVURLAsset { return asset.url.path }
        return sourcePaths[min(currentIndex, sourcePaths.count - 1)]
    }

    init?(paths: [String], emoji: String = "", onClose: @escaping () -> Void) {
        guard let firstPath = paths.first else { return nil }
        self.sourcePaths = paths
        self.emoji = emoji
        self.onClose = onClose

        let asset = AVURLAsset(url: URL(fileURLWithPath: firstPath))
        let pictureSize = VideoWindowController.naturalSize(of: asset)
        aspect = pictureSize.width / max(pictureSize.height, 1)
        var frame = VideoWindowController.idealFrame(for: pictureSize)
        if paths.count > 1 { frame.size.width += Self.playlistWidth }
        playlist = paths.count > 1 ? PlaylistList(paths: paths) : nil
        panel = VideoPanel(
            contentRect: frame,
            styleMask: [.titled, .closable, .resizable, .fullSizeContentView],
            backing: .buffered, defer: false)
        super.init()

        panel.titleVisibility = .hidden
        panel.titlebarAppearsTransparent = true
        panel.isMovableByWindowBackground = true
        panel.standardWindowButton(.miniaturizeButton)?.isHidden = true
        panel.standardWindowButton(.zoomButton)?.isHidden = true
        panel.isFloatingPanel = false
        panel.hidesOnDeactivate = false
        panel.isReleasedWhenClosed = false
        panel.delegate = self
        // For Mission Control, not chrome — the title bar is hidden. The
        // launching Claude session's sticky emoji leads it, so a wall of
        // preview panels still reads back to its prompts at a glance.
        panel.title = (emoji.isEmpty ? "" : emoji + " ")
            + (firstPath as NSString).lastPathComponent
        panel.isOpaque = false
        panel.backgroundColor = .black

        let content = panel.contentView!
        playerView.player = player
        playerView.controlsStyle = .floating
        playerView.showsFullScreenToggleButton = false
        playerView.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(playerView)

        var constraints = [
            playerView.topAnchor.constraint(equalTo: content.topAnchor),
            playerView.bottomAnchor.constraint(equalTo: content.bottomAnchor),
            playerView.leadingAnchor.constraint(equalTo: content.leadingAnchor),
        ]
        if let playlist {
            playlist.translatesAutoresizingMaskIntoConstraints = false
            playlist.onSelect = { [weak self] index in self?.playFrom(index: index) }
            content.addSubview(playlist)
            constraints += [
                playlist.topAnchor.constraint(equalTo: content.topAnchor),
                playlist.bottomAnchor.constraint(equalTo: content.bottomAnchor),
                playlist.trailingAnchor.constraint(equalTo: content.trailingAnchor),
                playlist.widthAnchor.constraint(equalToConstant: Self.playlistWidth),
                playerView.trailingAnchor.constraint(equalTo: playlist.leadingAnchor),
            ]
        } else {
            constraints.append(playerView.trailingAnchor.constraint(equalTo: content.trailingAnchor))
        }
        NSLayoutConstraint.activate(constraints)
        panel.onNext = { [weak self] in self?.next() }
        panel.onPrevious = { [weak self] in self?.previous() }
        currentItemObservation = player.observe(\.currentItem, options: [.new]) { [weak self] _, _ in
            DispatchQueue.main.async { self?.currentItemChanged() }
        }
        installContextMenu()
        playFrom(index: 0)
        if sourcePaths.count == 1 { watchFile() }
    }

    // MARK: context menu — nothing on screen until you ask for it

    /// The panel shows the movie and nothing else: no filename, no escape
    /// button. Everything that used to live in the chip is a right-click
    /// away, so a wall of preview panels is just a wall of moving pictures.
    private func installContextMenu() {
        let menu = NSMenu()
        if sourcePaths.count > 1 {
            menu.addItem(withTitle: "Next Video",
                         action: #selector(nextVideo), keyEquivalent: "")
            menu.addItem(withTitle: "Previous Video",
                         action: #selector(previousVideo), keyEquivalent: "")
            menu.addItem(withTitle: "Restart Playlist",
                         action: #selector(restartPlaylist), keyEquivalent: "")
            menu.addItem(.separator())
        }
        menu.addItem(withTitle: "Show in Finder",
                     action: #selector(showInFinder), keyEquivalent: "")
        menu.addItem(.separator())
        menu.addItem(withTitle: "Open in QuickTime Player",
                     action: #selector(openInQuickTime), keyEquivalent: "")
        // Name the third item after whatever LaunchServices actually hands
        // this filetype to — and skip it when that's QuickTime, which the
        // item above already covers.
        if let defaultApp = Self.defaultApplication(for: currentPath),
           defaultApp.name != "QuickTime Player" {
            let item = menu.addItem(withTitle: "Open in \(defaultApp.name)",
                                    action: #selector(openInDefaultApp), keyEquivalent: "")
            item.target = self
        }
        for item in menu.items where item.action != nil { item.target = self }
        // Right-click anywhere: over the picture, or the letterboxed margin.
        playerView.menu = menu
        panel.contentView?.menu = menu
    }

    /// The app LaunchServices would open this file with, and its display
    /// name. Nil when nothing is registered for the type.
    private static func defaultApplication(for path: String) -> (url: URL, name: String)? {
        let url = URL(fileURLWithPath: path)
        guard let app = NSWorkspace.shared.urlForApplication(toOpen: url) else { return nil }
        return (app, FileManager.default.displayName(atPath: app.path)
            .replacingOccurrences(of: ".app", with: ""))
    }

    @objc private func showInFinder() {
        NSWorkspace.shared.activateFileViewerSelecting([URL(fileURLWithPath: currentPath)])
    }

    @objc private func openInQuickTime() {
        NSWorkspace.shared.open(
            [URL(fileURLWithPath: currentPath)],
            withApplicationAt: URL(fileURLWithPath: "/System/Applications/QuickTime Player.app"),
            configuration: NSWorkspace.OpenConfiguration())
        close()
    }

    @objc private func openInDefaultApp() {
        guard let app = Self.defaultApplication(for: currentPath) else { return }
        NSWorkspace.shared.open(
            [URL(fileURLWithPath: currentPath)], withApplicationAt: app.url,
            configuration: NSWorkspace.OpenConfiguration())
        close()
    }

    @objc private func nextVideo() { next() }
    @objc private func previousVideo() { previous() }
    @objc private func restartPlaylist() { playFrom(index: 0) }

    private func next() {
        guard currentIndex + 1 < sourcePaths.count else { return }
        player.advanceToNextItem()
        player.play()
    }

    private func previous() {
        playFrom(index: max(0, currentIndex - 1))
    }

    private func playFrom(index: Int) {
        guard sourcePaths.indices.contains(index) else { return }
        currentIndex = index
        player.removeAllItems()
        for path in sourcePaths[index...] {
            let item = AVPlayerItem(asset: AVURLAsset(url: URL(fileURLWithPath: path)))
            if player.canInsert(item, after: nil) { player.insert(item, after: nil) }
        }
        updateTitle()
        player.play()
    }

    private func currentItemChanged() {
        guard let asset = player.currentItem?.asset as? AVURLAsset,
              let index = sourcePaths.firstIndex(of: asset.url.path) else { return }
        currentIndex = index
        updateTitle()
    }

    private func updateTitle() {
        let position = sourcePaths.count > 1 ? " — \(currentIndex + 1)/\(sourcePaths.count)" : ""
        panel.title = (emoji.isEmpty ? "" : emoji + " ")
            + (currentPath as NSString).lastPathComponent + position
        playlist?.highlight(index: currentIndex)
    }

    /// Aspect-fit the panel inside a grid cell (the playlist column, when
    /// present, rides along at its fixed width).
    func fit(in cell: NSRect) {
        let extra = playlist == nil ? 0 : Self.playlistWidth
        var width = cell.width
        var height = (width - extra) / max(aspect, 0.001)
        if height > cell.height {
            height = cell.height
            width = height * aspect + extra
        }
        let frame = NSRect(x: cell.midX - width / 2, y: cell.midY - height / 2,
                           width: width, height: height)
        panel.setFrame(frame, display: true, animate: false)
    }

    // MARK: live reload — render loops rewrite the mp4 in place

    private func watchFile() {
        let fd = Darwin.open(sourcePaths[0], O_EVTONLY)
        guard fd >= 0 else { return }
        let source = DispatchSource.makeFileSystemObjectSource(
            fileDescriptor: fd, eventMask: [.write, .rename, .delete, .extend],
            queue: .main)
        source.setEventHandler { [weak self] in self?.scheduleReload() }
        source.setCancelHandler { Darwin.close(fd) }
        source.resume()
        monitor = source
    }

    /// Encoders replace movies non-atomically (ffmpeg writes then moves),
    /// so debounce well past the last event before reloading from the top.
    private func scheduleReload() {
        if reloadPending { return }
        reloadPending = true
        monitor?.cancel()
        monitor = nil
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.0) { [weak self] in
            guard let self = self else { return }
            self.reloadPending = false
            guard FileManager.default.fileExists(atPath: self.sourcePaths[0]) else { return }
            self.playFrom(index: 0)
            self.watchFile()
        }
    }

    // MARK: window plumbing

    func focus() {
        NSApp.activate(ignoringOtherApps: true)
        panel.makeKeyAndOrderFront(nil)
    }

    func focusAndRestart() {
        focus()
        playFrom(index: 0)
    }

    func close() { panel.close() }

    func windowWillClose(_ notification: Notification) {
        monitor?.cancel()
        monitor = nil
        player.pause()
        onClose()
    }

    /// The movie's displayed pixel size (portrait default when unreadable).
    private static func naturalSize(of asset: AVURLAsset) -> NSSize {
        var size = NSSize(width: 1080, height: 1920)
        if let track = asset.tracks(withMediaType: .video).first {
            let natural = track.naturalSize.applying(track.preferredTransform)
            size = NSSize(width: abs(natural.width), height: abs(natural.height))
        }
        return size
    }

    /// Size the window to the video's aspect, ~3/4 of the screen tall for
    /// portrait, ~2/3 wide for landscape.
    private static func idealFrame(for size: NSSize) -> NSRect {
        let screen = NSScreen.main?.visibleFrame
            ?? NSRect(x: 0, y: 0, width: 1440, height: 900)
        let aspect = size.width / max(size.height, 1)
        var height = min(screen.height * 0.78, 980)
        var width = height * aspect
        if width > screen.width * 0.85 {
            width = screen.width * 0.85
            height = width / max(aspect, 0.001)
        }
        return NSRect(x: screen.midX - width / 2, y: screen.midY - height / 2,
                      width: width, height: height)
    }
}

/// The queue beside a playlist's picture: one row per movie, the playing row
/// highlighted, a click jumps. Glass column so it reads as part of the panel.
private final class PlaylistList: NSView, NSTableViewDataSource, NSTableViewDelegate {
    var onSelect: ((Int) -> Void)?
    private let names: [String]
    private let table = NSTableView()
    private var syncing = false

    init(paths: [String]) {
        names = paths.map { ($0 as NSString).lastPathComponent }
        super.init(frame: .zero)
        let glass = NSVisualEffectView()
        glass.material = .hudWindow
        glass.blendingMode = .behindWindow
        glass.state = .active
        glass.translatesAutoresizingMaskIntoConstraints = false
        addSubview(glass)

        let column = NSTableColumn(identifier: NSUserInterfaceItemIdentifier("name"))
        column.resizingMask = .autoresizingMask
        table.addTableColumn(column)
        table.headerView = nil
        table.rowHeight = 26
        table.backgroundColor = .clear
        table.selectionHighlightStyle = .regular
        table.allowsEmptySelection = false
        table.dataSource = self
        table.delegate = self
        table.target = self
        table.action = #selector(rowClicked)
        let scroll = NSScrollView()
        scroll.documentView = table
        scroll.drawsBackground = false
        scroll.hasVerticalScroller = true
        scroll.autohidesScrollers = true
        scroll.translatesAutoresizingMaskIntoConstraints = false
        addSubview(scroll)

        NSLayoutConstraint.activate([
            glass.topAnchor.constraint(equalTo: topAnchor),
            glass.bottomAnchor.constraint(equalTo: bottomAnchor),
            glass.leadingAnchor.constraint(equalTo: leadingAnchor),
            glass.trailingAnchor.constraint(equalTo: trailingAnchor),
            // Clear the hidden title bar's traffic lights.
            scroll.topAnchor.constraint(equalTo: topAnchor, constant: 30),
            scroll.bottomAnchor.constraint(equalTo: bottomAnchor, constant: -8),
            scroll.leadingAnchor.constraint(equalTo: leadingAnchor, constant: 6),
            scroll.trailingAnchor.constraint(equalTo: trailingAnchor, constant: -6),
        ])
    }

    required init?(coder: NSCoder) { nil }

    func highlight(index: Int) {
        guard names.indices.contains(index) else { return }
        syncing = true
        table.selectRowIndexes(IndexSet(integer: index), byExtendingSelection: false)
        table.scrollRowToVisible(index)
        syncing = false
    }

    @objc private func rowClicked() {
        let row = table.clickedRow
        guard row >= 0 else { return }
        onSelect?(row)
    }

    func numberOfRows(in tableView: NSTableView) -> Int { names.count }

    func tableView(_ tableView: NSTableView, viewFor tableColumn: NSTableColumn?, row: Int) -> NSView? {
        let id = NSUserInterfaceItemIdentifier("row")
        let cell = (tableView.makeView(withIdentifier: id, owner: nil) as? NSTableCellView) ?? {
            let cell = NSTableCellView()
            cell.identifier = id
            let label = NSTextField(labelWithString: "")
            label.font = .monospacedSystemFont(ofSize: 11, weight: .regular)
            label.lineBreakMode = .byTruncatingMiddle
            label.translatesAutoresizingMaskIntoConstraints = false
            cell.addSubview(label)
            cell.textField = label
            NSLayoutConstraint.activate([
                label.leadingAnchor.constraint(equalTo: cell.leadingAnchor, constant: 6),
                label.trailingAnchor.constraint(equalTo: cell.trailingAnchor, constant: -6),
                label.centerYAnchor.constraint(equalTo: cell.centerYAnchor),
            ])
            return cell
        }()
        cell.textField?.stringValue = "\(row + 1). \(names[row])"
        return cell
    }

    func tableViewSelectionDidChange(_ notification: Notification) {
        // Keyboard selection in the list also jumps; programmatic sync doesn't.
        guard !syncing, table.selectedRow >= 0 else { return }
        onSelect?(table.selectedRow)
    }
}

/// AVPlayerView eats right-clicks on its own transport controls, so setting
/// `.menu` alone isn't enough to be sure the contextual menu appears. Pop it
/// explicitly on right-mouse-down.
private final class ContextPlayerView: AVPlayerView {
    override func rightMouseDown(with event: NSEvent) {
        guard let menu = menu else { return super.rightMouseDown(with: event) }
        NSMenu.popUpContextMenu(menu, with: event, for: self)
    }
}

/// Titled-but-chromeless panel: key-able so the player controls and Esc
/// work, and Esc (cancelOperation) closes — the "just glance and dismiss"
/// contract. Space toggles play/pause like QuickTime.
private final class VideoPanel: NSPanel {
    var onNext: (() -> Void)?
    var onPrevious: (() -> Void)?
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }
    override func cancelOperation(_ sender: Any?) { close() }
    override func keyDown(with event: NSEvent) {
        if event.keyCode == 124 { onNext?(); return }
        if event.keyCode == 123 { onPrevious?(); return }
        if event.charactersIgnoringModifiers == " ",
           let playerView = contentView?.subviews.compactMap({ $0 as? AVPlayerView }).first,
           let player = playerView.player {
            player.rate == 0 ? player.play() : player.pause()
            return
        }
        super.keyDown(with: event)
    }
}

// MARK: - menu management (slab manages the viewers)

extension AppDelegate {
    @objc func focusVideo(_ sender: NSMenuItem) {
        if let id = sender.representedObject as? String { VideoViewer.shared.focus(id) }
    }

    @objc func closeAllVideos() { VideoViewer.shared.closeAll() }
    @objc func tileAllVideos() { VideoViewer.shared.tile() }

    @objc func openVideoFromPanel() {
        NSApp.activate(ignoringOtherApps: true)
        let panel = NSOpenPanel()
        panel.allowedContentTypes = [.movie, .mpeg4Movie, .quickTimeMovie]
        panel.allowsMultipleSelection = true
        if panel.runModal() == .OK {
            VideoViewer.shared.openPlaylist(panel.urls.map(\.path))
        }
    }
}
