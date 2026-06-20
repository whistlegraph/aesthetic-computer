// ImageGroupPreview — open a GROUP of images as a wall of chromeless glass
// panels, grid-tiled, so reviewing a set (e.g. App Store screenshots) is a
// glance with no Preview.app chrome and no Finder dig. The FramePreview
// contract, generalized from "one panel per fleet machine" to "N panels per
// requested group", with an AXTiler-style grid laid over the main screen.
//
// How it gets asked: a CLI writes one absolute image path per line to
// $SLAB_HOME/state/open-images, then the menubar's 2 s tick consumes the file
// (tiny read, no shell-outs — per slab-menubar-perf) and opens the whole batch
// as ONE group, replacing any previous group. Each panel is Esc/⌘W dismissable
// and click-drag relocatable; "Close All Images" drops the lot.
//
// Tiling: cols = ceil(sqrt(n)), rows = ceil(n/cols); each panel gets a cell of
// the main screen's visibleFrame and aspect-fits its image inside. This is the
// same direct-AX placement idea as AXTiler.tileNow(), but applied to our OWN
// windows (no AX round-trip needed — we set the panel frames in process).
import AppKit

extension Paths {
    /// One absolute image path per line; the whole file is one group, consumed
    /// each tick and shown as a tiled wall.
    static var imageGroupRequestFile: String { "\(slabHome)/state/open-images" }
}

final class ImageGroupPreview {
    static let shared = ImageGroupPreview()
    private var controllers: [ImagePanelController] = []

    var isShowing: Bool { !controllers.isEmpty }

    /// Called from the main-thread side of AppDelegate.refresh() every tick.
    func consumeRequests() {
        let file = Paths.imageGroupRequestFile
        guard FileManager.default.fileExists(atPath: file) else { return }
        let text = (try? String(contentsOfFile: file, encoding: .utf8)) ?? ""
        try? FileManager.default.removeItem(atPath: file)
        let paths = text.split(separator: "\n")
            .map { ($0.trimmingCharacters(in: .whitespaces) as NSString).expandingTildeInPath }
            .filter { !$0.isEmpty && FileManager.default.fileExists(atPath: $0) }
        guard !paths.isEmpty else { return }
        openGroup(paths)
    }

    /// A new group REPLACES the previous one — re-running the CLI gives a clean
    /// wall of the latest set rather than stacking old panels behind new.
    func openGroup(_ paths: [String]) {
        closeAll()
        for (i, p) in paths.enumerated() {
            guard let c = ImagePanelController(path: p, onClose: { [weak self] ctrl in
                self?.controllers.removeAll { $0 === ctrl }
            }) else { continue }
            c.indexBadge = "\(i + 1)/\(paths.count)"
            controllers.append(c)
        }
        layoutGrid()
        // Raise the whole wall; key the first so Esc has a target.
        NSApp.activate(ignoringOtherApps: true)
        for c in controllers { c.orderFront() }
        controllers.first?.focus()
    }

    /// Grid the open panels across the main screen's visible frame.
    private func layoutGrid() {
        let n = controllers.count
        guard n > 0 else { return }
        let screen = NSScreen.main?.visibleFrame
            ?? NSRect(x: 0, y: 0, width: 1440, height: 900)
        let cols = Int(ceil(Double(n).squareRoot()))
        let rows = Int(ceil(Double(n) / Double(cols)))
        let gap: CGFloat = 16
        let cellW = (screen.width - gap * CGFloat(cols + 1)) / CGFloat(cols)
        let cellH = (screen.height - gap * CGFloat(rows + 1)) / CGFloat(rows)
        for (i, c) in controllers.enumerated() {
            let col = i % cols
            let row = i / cols
            // Top-to-bottom rows: AppKit y grows upward, so row 0 is the top.
            let x = screen.minX + gap + CGFloat(col) * (cellW + gap)
            let yTop = screen.maxY - gap - CGFloat(row) * (cellH + gap)
            c.fit(in: NSRect(x: x, y: yTop - cellH, width: cellW, height: cellH))
        }
    }

    func closeAll() {
        for c in Array(controllers) { c.close() }
        controllers.removeAll()
    }
}

/// One chromeless panel for one image. Aspect-fits its image inside an assigned
/// grid cell; any click-drag moves the window; Esc/⌘W dismiss.
private final class ImagePanelController: NSObject, NSWindowDelegate {
    private let path: String
    private let panel: ImagePanel
    private let imageView = DraggableImageView()
    private let onClose: (ImagePanelController) -> Void
    private let aspect: CGFloat
    var indexBadge: String = "" { didSet { badge.stringValue = indexBadge } }
    private let badge = NSTextField(labelWithString: "")

    init?(path: String, onClose: @escaping (ImagePanelController) -> Void) {
        guard let image = NSImage(contentsOfFile: path), image.size.width > 0 else { return nil }
        self.path = path
        self.onClose = onClose
        self.aspect = image.size.width / max(image.size.height, 1)
        panel = ImagePanel(
            contentRect: NSRect(x: 0, y: 0, width: 480, height: 300),
            styleMask: [.titled, .closable, .resizable, .fullSizeContentView],
            backing: .buffered, defer: false)
        super.init()
        panel.titleVisibility = .hidden
        panel.titlebarAppearsTransparent = true
        panel.isMovableByWindowBackground = true
        panel.standardWindowButton(.miniaturizeButton)?.isHidden = true
        panel.standardWindowButton(.zoomButton)?.isHidden = true
        panel.hidesOnDeactivate = false
        panel.isReleasedWhenClosed = false
        panel.isRestorable = false
        panel.delegate = self
        panel.title = (path as NSString).lastPathComponent
        panel.isOpaque = false
        panel.backgroundColor = .black
        panel.minSize = NSSize(width: 200, height: 140)
        if image.size.width > 0 && image.size.height > 0 { panel.contentAspectRatio = image.size }

        let content = panel.contentView!
        imageView.image = image
        imageView.imageScaling = .scaleProportionallyUpOrDown
        imageView.imageAlignment = .alignCenter
        imageView.translatesAutoresizingMaskIntoConstraints = false
        imageView.setContentHuggingPriority(.defaultLow, for: .horizontal)
        imageView.setContentHuggingPriority(.defaultLow, for: .vertical)
        imageView.setContentCompressionResistancePriority(.defaultLow, for: .horizontal)
        imageView.setContentCompressionResistancePriority(.defaultLow, for: .vertical)
        content.addSubview(imageView)
        NSLayoutConstraint.activate([
            imageView.topAnchor.constraint(equalTo: content.topAnchor),
            imageView.bottomAnchor.constraint(equalTo: content.bottomAnchor),
            imageView.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            imageView.trailingAnchor.constraint(equalTo: content.trailingAnchor),
        ])
        installBadge()
    }

    /// HUD chip naming the file + index, matching FramePreview/PdfViewer style.
    private func installBadge() {
        let chip = NSVisualEffectView()
        chip.material = .hudWindow
        chip.blendingMode = .withinWindow
        chip.state = .active
        chip.wantsLayer = true
        chip.layer?.cornerRadius = 8
        chip.translatesAutoresizingMaskIntoConstraints = false
        badge.stringValue = (path as NSString).lastPathComponent
        badge.font = .monospacedSystemFont(ofSize: 11, weight: .semibold)
        badge.textColor = .labelColor
        badge.lineBreakMode = .byTruncatingMiddle
        badge.translatesAutoresizingMaskIntoConstraints = false
        badge.toolTip = path
        chip.addSubview(badge)
        let content = panel.contentView!
        content.addSubview(chip)
        NSLayoutConstraint.activate([
            chip.leadingAnchor.constraint(equalTo: content.leadingAnchor, constant: 12),
            chip.topAnchor.constraint(equalTo: content.topAnchor, constant: 10),
            badge.leadingAnchor.constraint(equalTo: chip.leadingAnchor, constant: 10),
            badge.trailingAnchor.constraint(equalTo: chip.trailingAnchor, constant: -10),
            badge.centerYAnchor.constraint(equalTo: chip.centerYAnchor),
            badge.widthAnchor.constraint(lessThanOrEqualToConstant: 260),
            chip.heightAnchor.constraint(equalToConstant: 24),
        ])
    }

    /// Place the panel inside `cell`, aspect-fit to the image (so the panel is
    /// no bigger than the image needs and stays centered in its grid cell).
    func fit(in cell: NSRect) {
        var w = cell.width
        var h = w / max(aspect, 0.001)
        if h > cell.height { h = cell.height; w = h * aspect }
        let x = cell.minX + (cell.width - w) / 2
        let y = cell.minY + (cell.height - h) / 2
        panel.setFrame(NSRect(x: x, y: y, width: w, height: h), display: true)
    }

    func orderFront() { panel.orderFrontRegardless() }
    func focus() { panel.makeKeyAndOrderFront(nil) }
    func close() { panel.close() }
    func windowWillClose(_ notification: Notification) { onClose(self) }
}

private final class DraggableImageView: NSImageView {
    override func mouseDown(with event: NSEvent) { window?.performDrag(with: event) }
}

private final class ImagePanel: NSPanel {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }
    override func cancelOperation(_ sender: Any?) { close() }
}

// MARK: - slab manages the viewer
extension AppDelegate {
    @objc func closeAllImages() { ImageGroupPreview.shared.closeAll() }
}
