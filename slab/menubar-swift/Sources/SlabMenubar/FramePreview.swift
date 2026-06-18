// FramePreview — slab's window for a pulled `frame` screenshot, so "see what
// panda's screen looks like right now" is a glance, not a Finder dig into
// ~/.local/share/slab/frames. The PdfViewer/VideoViewer contract, but for a
// captured desktop JPEG: a chromeless glass panel showing the image, with a
// corner BADGE naming the source machine ("panda", "chicken") in the same
// HUD-chip style as the other viewers. Esc or ⌘W dismisses.
//
// How it gets asked: `frame <machine> --preview` (or `frame view <machine>`)
// pulls the frame, writes the JPEG to ~/.local/share/slab/frames/<machine>.jpg
// like always, then appends "path\tmachine" to $SLAB_HOME/state/open-frame.
// The menubar's 2 s tick consumes that file on the main thread (tiny stat —
// no shell-outs, per slab-menubar-perf) and raises one panel per machine.
// Re-pulling the same machine brings its window forward and reloads the image
// in place, so a `frame chicken --preview` loop feels live (the controller
// keeps overwriting the same <machine>.jpg path).
//
// Unlike video/pdf, the badge is the MACHINE name, not a Claude session emoji:
// a `frame` is fundamentally "which Mac am I looking at", so the fleet identity
// leads. (A session emoji could ride alongside later, but the machine is the
// load-bearing label here.)
import AppKit

extension Paths {
    /// One "path\tmachine" line per pulled frame; consumed each tick.
    static var frameRequestFile: String { "\(slabHome)/state/open-frame" }
}

final class FramePreview {
    static let shared = FramePreview()
    /// Keyed by MACHINE name (not path): one window per fleet Mac, re-pulled
    /// in place. The path may even stay constant across pulls (the controller
    /// overwrites <machine>.jpg), so the machine is the stable identity.
    private var controllers: [String: FrameWindowController] = [:]

    var openMachines: [String] { controllers.keys.sorted() }

    /// Called from the main-thread side of AppDelegate.refresh() every tick.
    /// Request lines are "path\tmachine"; an empty machine falls back to the
    /// file's basename so a bare path still labels something sensible.
    func consumeRequests() {
        let file = Paths.frameRequestFile
        guard FileManager.default.fileExists(atPath: file) else { return }
        let text = (try? String(contentsOfFile: file, encoding: .utf8)) ?? ""
        try? FileManager.default.removeItem(atPath: file)
        for line in text.split(separator: "\n") {
            let parts = line.split(separator: "\t", maxSplits: 1, omittingEmptySubsequences: false)
            let path = parts[0].trimmingCharacters(in: .whitespaces)
            var machine = parts.count > 1 ? parts[1].trimmingCharacters(in: .whitespaces) : ""
            if machine.isEmpty {
                machine = ((path as NSString).lastPathComponent as NSString).deletingPathExtension
            }
            if !path.isEmpty { open(path, machine: machine) }
        }
    }

    func open(_ rawPath: String, machine: String) {
        let path = (rawPath as NSString).expandingTildeInPath
        if let existing = controllers[machine] { existing.focusAndReload(path: path); return }
        guard FileManager.default.fileExists(atPath: path),
              let controller = FrameWindowController(path: path, machine: machine, onClose: { [weak self] in
                  self?.controllers.removeValue(forKey: machine)
              })
        else { return }
        controllers[machine] = controller
        controller.focus()
    }

    func focus(_ machine: String) { controllers[machine]?.focus() }

    func closeAll() {
        // close() triggers onClose which mutates the dictionary — iterate a copy.
        for controller in Array(controllers.values) { controller.close() }
    }
}

/// One panel per machine. Owns the NSImageView, the machine badge, and a file
/// monitor that reloads the image when the JPEG is rewritten on disk (the
/// controller overwrites <machine>.jpg on each pull).
private final class FrameWindowController: NSObject, NSWindowDelegate {
    private var path: String
    private let machine: String
    private let panel: FramePanel
    private let imageView = DraggableFrameImageView()
    private let onClose: () -> Void
    private var monitor: DispatchSourceFileSystemObject?
    private var reloadPending = false

    init?(path: String, machine: String, onClose: @escaping () -> Void) {
        guard let image = NSImage(contentsOfFile: path) else { return nil }
        self.path = path
        self.machine = machine
        self.onClose = onClose

        panel = FramePanel(
            contentRect: FrameWindowController.idealFrame(for: image),
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
        panel.title = machine // for Mission Control, not chrome
        panel.isOpaque = false
        panel.backgroundColor = .black

        let content = panel.contentView!
        imageView.image = image
        imageView.imageScaling = .scaleProportionallyUpOrDown
        imageView.imageAlignment = .alignCenter
        // A glance pane, not an editor: any drag moves the window (matches
        // DraggablePdfView's "click-drag relocates" feel).
        imageView.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(imageView)

        NSLayoutConstraint.activate([
            imageView.topAnchor.constraint(equalTo: content.topAnchor),
            imageView.bottomAnchor.constraint(equalTo: content.bottomAnchor),
            imageView.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            imageView.trailingAnchor.constraint(equalTo: content.trailingAnchor),
        ])
        installBadge()
        watchFile()
    }

    // MARK: badge — the source machine name, leading the corner

    /// A HUD chip in the top-left (the other viewers chip top-right with a
    /// filename; the machine is the headline here so it leads at the corner
    /// the eye lands on first). Style — hudWindow material, 8pt radius,
    /// monospaced — matches PdfViewer/VideoViewer so a wall of slab panels
    /// reads as one family.
    private func installBadge() {
        let chip = NSVisualEffectView()
        chip.material = .hudWindow
        chip.blendingMode = .withinWindow
        chip.state = .active
        chip.wantsLayer = true
        chip.layer?.cornerRadius = 8
        chip.translatesAutoresizingMaskIntoConstraints = false

        // The machine name in bold so it carries as a label from across the
        // room — this IS the answer to "whose screen is this".
        let name = NSTextField(labelWithString: machine)
        name.font = .monospacedSystemFont(ofSize: 12, weight: .semibold)
        name.textColor = .labelColor
        name.lineBreakMode = .byTruncatingMiddle
        name.translatesAutoresizingMaskIntoConstraints = false
        name.toolTip = path

        chip.addSubview(name)
        // The chip must be IN the hierarchy before any chip↔content
        // constraint activates, or AppKit throws (no common ancestor).
        let content = panel.contentView!
        content.addSubview(chip)
        NSLayoutConstraint.activate([
            chip.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 12),
            chip.topAnchor.constraint(equalTo: content.topAnchor, constant: 10),
            name.leadingAnchor.constraint(equalTo: chip.leadingAnchor, constant: 10),
            name.trailingAnchor.constraint(equalTo: chip.trailingAnchor, constant: -10),
            name.centerYAnchor.constraint(equalTo: chip.centerYAnchor),
            name.widthAnchor.constraint(lessThanOrEqualToConstant: 280),
            chip.heightAnchor.constraint(equalToConstant: 24),
        ])
    }

    // MARK: live reload — re-pulls rewrite the JPEG in place

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

    /// `frame.mjs` writes the JPEG with a single writeFileSync (not atomic
    /// rename), so a write can land mid-flight — debounce briefly, then reload
    /// from disk. Re-arm the watcher after (a rename invalidates the fd).
    private func scheduleReload() {
        if reloadPending { return }
        reloadPending = true
        monitor?.cancel()
        monitor = nil
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.3) { [weak self] in
            guard let self = self else { return }
            self.reloadPending = false
            guard FileManager.default.fileExists(atPath: self.path) else { return }
            if let fresh = NSImage(contentsOfFile: self.path) {
                self.imageView.image = fresh
            }
            self.watchFile()
        }
    }

    // MARK: window plumbing

    func focus() {
        NSApp.activate(ignoringOtherApps: true)
        panel.makeKeyAndOrderFront(nil)
    }

    /// Re-pull of an already-open machine: swap to the new path (usually the
    /// same <machine>.jpg), reload the image, re-arm the watcher, and raise.
    func focusAndReload(path newPath: String) {
        if newPath != path {
            monitor?.cancel()
            monitor = nil
            path = newPath
            watchFile()
        }
        if let fresh = NSImage(contentsOfFile: path) { imageView.image = fresh }
        focus()
    }

    func close() { panel.close() }

    func windowWillClose(_ notification: Notification) {
        monitor?.cancel()
        monitor = nil
        onClose()
    }

    /// Size the window to the image's aspect, ~3/4 of the screen tall (matches
    /// the pdf/video viewers' framing).
    private static func idealFrame(for image: NSImage) -> NSRect {
        let screen = NSScreen.main?.visibleFrame
            ?? NSRect(x: 0, y: 0, width: 1440, height: 900)
        let size = image.size.width > 0 && image.size.height > 0
            ? image.size : NSSize(width: 1568, height: 980)
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

/// A glance pane: any click-drag on the image moves the WINDOW (no selection),
/// matching DraggablePdfView. Esc/⌘W dismiss.
private final class DraggableFrameImageView: NSImageView {
    override func mouseDown(with event: NSEvent) { window?.performDrag(with: event) }
}

/// Titled-but-chromeless panel: key-able so Esc works, and Esc
/// (cancelOperation) closes — the "just glance and dismiss" contract.
private final class FramePanel: NSPanel {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }
    override func cancelOperation(_ sender: Any?) { close() }
}

// MARK: - menu management (slab manages the viewers)

extension AppDelegate {
    @objc func focusFrame(_ sender: NSMenuItem) {
        if let machine = sender.representedObject as? String { FramePreview.shared.focus(machine) }
    }

    @objc func closeAllFrames() { FramePreview.shared.closeAll() }
}
