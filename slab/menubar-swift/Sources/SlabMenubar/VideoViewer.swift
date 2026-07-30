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
        panel = VideoPanel(
            contentRect: VideoWindowController.idealFrame(for: asset),
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

        NSLayoutConstraint.activate([
            playerView.topAnchor.constraint(equalTo: content.topAnchor),
            playerView.bottomAnchor.constraint(equalTo: content.bottomAnchor),
            playerView.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            playerView.trailingAnchor.constraint(equalTo: content.trailingAnchor),
        ])
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

    /// Size the window to the video's aspect, ~3/4 of the screen tall for
    /// portrait, ~2/3 wide for landscape.
    private static func idealFrame(for asset: AVURLAsset) -> NSRect {
        let screen = NSScreen.main?.visibleFrame
            ?? NSRect(x: 0, y: 0, width: 1440, height: 900)
        var size = NSSize(width: 1080, height: 1920)
        if let track = asset.tracks(withMediaType: .video).first {
            let natural = track.naturalSize.applying(track.preferredTransform)
            size = NSSize(width: abs(natural.width), height: abs(natural.height))
        }
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
