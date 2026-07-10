// VideoViewer — slab's minimal video player, so "watch this render" doesn't
// mean QuickTime. The PdfViewer contract, but for movies: a chromeless
// glass panel with an AVPlayerView (floating controls) and NOTHING else on
// screen — no filename, no buttons. Right-click for Show in Finder / open
// elsewhere; Esc or ⌘W to dismiss. Playback starts immediately.
//
// How it gets asked: anything (Claude, a render script, the menu) appends
// an absolute path per line to $SLAB_HOME/state/open-video; the menubar's
// 2 s tick consumes the file on the main thread (tiny stat — no shell-outs,
// per slab-menubar-perf). The `slab-video` wrapper in slab/bin does exactly
// that. Re-requesting an open path brings its window forward and restarts
// playback. The viewer watches the file and reloads on rewrite, so a video
// re-render loop (build.mjs → same mp4 path) feels live.
import AppKit
import AVKit
import AVFoundation

extension Paths {
    /// One absolute video path per line; consumed (deleted) each tick.
    static var videoRequestFile: String { "\(slabHome)/state/open-video" }
}

final class VideoViewer {
    static let shared = VideoViewer()
    private var controllers: [String: VideoWindowController] = [:]

    var openPaths: [String] { controllers.keys.sorted() }

    /// Called from the main-thread side of AppDelegate.refresh() every tick.
    /// Request lines are "path\tsession_id" (the session id may be empty);
    /// `emojiFor` maps a live Claude session to its sticky TitleEmoji so the
    /// chip wears the mark of the prompt that asked.
    func consumeRequests(emojiFor: (String) -> String = { _ in "" }) {
        let file = Paths.videoRequestFile
        guard FileManager.default.fileExists(atPath: file) else { return }
        let text = (try? String(contentsOfFile: file, encoding: .utf8)) ?? ""
        try? FileManager.default.removeItem(atPath: file)
        for line in text.split(separator: "\n") {
            let parts = line.split(separator: "\t", maxSplits: 1, omittingEmptySubsequences: false)
            let path = parts[0].trimmingCharacters(in: .whitespaces)
            let sid = parts.count > 1 ? parts[1].trimmingCharacters(in: .whitespaces) : ""
            if !path.isEmpty { open(path, emoji: sid.isEmpty ? "" : emojiFor(sid)) }
        }
    }

    func open(_ rawPath: String, emoji: String = "") {
        let path = (rawPath as NSString).expandingTildeInPath
        if let existing = controllers[path] { existing.focusAndRestart(); return }
        guard FileManager.default.fileExists(atPath: path),
              let controller = VideoWindowController(path: path, emoji: emoji, onClose: { [weak self] in
                  self?.controllers.removeValue(forKey: path)
              })
        else { return }
        controllers[path] = controller
        controller.focus()
    }

    func focus(_ path: String) { controllers[path]?.focus() }

    func closeAll() {
        // close() triggers onClose which mutates the dictionary — iterate a copy.
        for controller in Array(controllers.values) { controller.close() }
    }
}

/// One panel per movie. Owns the AVPlayerView, the chip, and a file monitor
/// that reloads the player item when the file is rewritten on disk.
private final class VideoWindowController: NSObject, NSWindowDelegate {
    private let path: String
    private let emoji: String
    private let panel: VideoPanel
    private let playerView = ContextPlayerView()
    private let player = AVPlayer()
    private let onClose: () -> Void
    private var monitor: DispatchSourceFileSystemObject?
    private var reloadPending = false

    init?(path: String, emoji: String = "", onClose: @escaping () -> Void) {
        self.path = path
        self.emoji = emoji
        self.onClose = onClose

        let asset = AVURLAsset(url: URL(fileURLWithPath: path))
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
            + (path as NSString).lastPathComponent
        panel.isOpaque = false
        panel.backgroundColor = .black

        let content = panel.contentView!
        player.replaceCurrentItem(with: AVPlayerItem(asset: asset))
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
        installContextMenu()
        watchFile()
        player.play()
    }

    // MARK: context menu — nothing on screen until you ask for it

    /// The panel shows the movie and nothing else: no filename, no escape
    /// button. Everything that used to live in the chip is a right-click
    /// away, so a wall of preview panels is just a wall of moving pictures.
    private func installContextMenu() {
        let menu = NSMenu()
        menu.addItem(withTitle: "Show in Finder",
                     action: #selector(showInFinder), keyEquivalent: "")
        menu.addItem(.separator())
        menu.addItem(withTitle: "Open in QuickTime Player",
                     action: #selector(openInQuickTime), keyEquivalent: "")
        // Name the third item after whatever LaunchServices actually hands
        // this filetype to — and skip it when that's QuickTime, which the
        // item above already covers.
        if let defaultApp = Self.defaultApplication(for: path),
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
        NSWorkspace.shared.activateFileViewerSelecting([URL(fileURLWithPath: path)])
    }

    @objc private func openInQuickTime() {
        NSWorkspace.shared.open(
            [URL(fileURLWithPath: path)],
            withApplicationAt: URL(fileURLWithPath: "/System/Applications/QuickTime Player.app"),
            configuration: NSWorkspace.OpenConfiguration())
        close()
    }

    @objc private func openInDefaultApp() {
        guard let app = Self.defaultApplication(for: path) else { return }
        NSWorkspace.shared.open(
            [URL(fileURLWithPath: path)], withApplicationAt: app.url,
            configuration: NSWorkspace.OpenConfiguration())
        close()
    }

    // MARK: live reload — render loops rewrite the mp4 in place

    private func watchFile() {
        let fd = Darwin.open(path, O_EVTONLY)
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
            guard FileManager.default.fileExists(atPath: self.path) else { return }
            let asset = AVURLAsset(url: URL(fileURLWithPath: self.path))
            self.player.replaceCurrentItem(with: AVPlayerItem(asset: asset))
            self.player.play()
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
        player.seek(to: .zero)
        player.play()
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
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }
    override func cancelOperation(_ sender: Any?) { close() }
    override func keyDown(with event: NSEvent) {
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
        if let path = sender.representedObject as? String { VideoViewer.shared.focus(path) }
    }

    @objc func closeAllVideos() { VideoViewer.shared.closeAll() }

    @objc func openVideoFromPanel() {
        NSApp.activate(ignoringOtherApps: true)
        let panel = NSOpenPanel()
        panel.allowedContentTypes = [.movie, .mpeg4Movie, .quickTimeMovie]
        panel.allowsMultipleSelection = true
        if panel.runModal() == .OK {
            for url in panel.urls { VideoViewer.shared.open(url.path) }
        }
    }
}
