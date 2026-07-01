// PdfViewer — slab's minimal PDF scroller, so "open this PDF" doesn't mean
// Preview.app. A borderless-feeling panel: just the pages, a thin chip with
// the filename and a single "Preview" escape hatch, Esc or ⌘W to dismiss.
//
// How it gets asked: anything (Claude, a script, the menu) appends an
// absolute path per line to $SLAB_HOME/state/open-pdf; the menubar's 2 s
// tick consumes the file on the main thread (tiny stat — no shell-outs,
// per slab-menubar-perf). The `slab-pdf` wrapper in slab/bin does exactly
// that. Re-requesting an open path brings its window forward. The viewer
// watches the file and reloads on rewrite (xelatex loops feel live).
import AppKit
import PDFKit
import CoreImage

extension Paths {
    /// One absolute PDF path per line; consumed (deleted) each tick.
    static var pdfRequestFile: String { "\(slabHome)/state/open-pdf" }
}

final class PdfViewer {
    static let shared = PdfViewer()
    private var controllers: [String: PdfWindowController] = [:]

    var openPaths: [String] { controllers.keys.sorted() }

    /// Called from the main-thread side of AppDelegate.refresh() every tick.
    /// Request lines are "path\tsession_id" (the session id may be empty);
    /// `emojiFor` maps a live Claude session to its sticky TitleEmoji so the
    /// chip wears the mark of the prompt that asked.
    func consumeRequests(emojiFor: (String) -> String = { _ in "" }) {
        let file = Paths.pdfRequestFile
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
        if let existing = controllers[path] { existing.focus(); return }
        guard FileManager.default.fileExists(atPath: path),
              let controller = PdfWindowController(path: path, emoji: emoji, onClose: { [weak self] in
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

/// One panel per document. Owns the PDFView, the chip, and a file monitor
/// that reloads the document in place when the PDF is rewritten on disk.
private final class PdfWindowController: NSObject, NSWindowDelegate {
    private let path: String
    private let emoji: String
    private let panel: PdfPanel
    private let pdfView = DraggablePdfView()
    private let onClose: () -> Void
    private var monitor: DispatchSourceFileSystemObject?
    private var reloadPending = false

    init?(path: String, emoji: String = "", onClose: @escaping () -> Void) {
        guard let document = PDFDocument(url: URL(fileURLWithPath: path)) else { return nil }
        self.path = path
        self.emoji = emoji
        self.onClose = onClose

        panel = PdfPanel(
            contentRect: PdfWindowController.idealFrame(for: document),
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
        panel.title = (path as NSString).lastPathComponent // for Mission Control, not chrome
        // Liquid glass: the window itself is clear; a glass backdrop fills
        // it and the PDF pages float on top (PDFView's own background is
        // cleared below, so the margins/gutters show glass, not gray).
        panel.isOpaque = false
        panel.backgroundColor = .clear

        let content = panel.contentView!
        let backdrop = PdfWindowController.makeGlassBackdrop()
        backdrop.translatesAutoresizingMaskIntoConstraints = false
        content.addSubview(backdrop)

        pdfView.document = document
        pdfView.autoScales = true
        pdfView.displayMode = .singlePageContinuous
        pdfView.displaysPageBreaks = true
        pdfView.pageBreakMargins = NSEdgeInsets(top: 6, left: 0, bottom: 6, right: 0)
        pdfView.translatesAutoresizingMaskIntoConstraints = false
        pdfView.backgroundColor = .clear
        content.addSubview(pdfView)

        NSLayoutConstraint.activate([
            backdrop.topAnchor.constraint(equalTo: content.topAnchor),
            backdrop.bottomAnchor.constraint(equalTo: content.bottomAnchor),
            backdrop.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            backdrop.trailingAnchor.constraint(equalTo: content.trailingAnchor),
            pdfView.topAnchor.constraint(equalTo: content.topAnchor),
            pdfView.bottomAnchor.constraint(equalTo: content.bottomAnchor),
            pdfView.leadingAnchor.constraint(equalTo: content.leadingAnchor),
            pdfView.trailingAnchor.constraint(equalTo: content.trailingAnchor),
        ])
        installChip()
        watchFile()
    }

    /// Liquid glass on macOS 26+, vibrancy on anything older. Both read the
    /// desktop through the window, which is the whole point of the look.
    private static func makeGlassBackdrop() -> NSView {
        if #available(macOS 26.0, *) {
            return NSGlassEffectView()
        }
        let vibrancy = NSVisualEffectView()
        vibrancy.material = .underWindowBackground
        vibrancy.blendingMode = .behindWindow
        vibrancy.state = .active
        return vibrancy
    }

    // MARK: chip — filename + the one escape hatch

    private func installChip() {
        let chip = NSVisualEffectView()
        chip.material = .hudWindow
        chip.blendingMode = .withinWindow
        chip.state = .active
        chip.wantsLayer = true
        chip.layer?.cornerRadius = 8
        chip.translatesAutoresizingMaskIntoConstraints = false

        // the launching Claude session's sticky emoji leads the filename so
        // a wall of preview panels reads back to its prompts at a glance
        let label = (emoji.isEmpty ? "" : emoji + " ") + (path as NSString).lastPathComponent
        let name = NSTextField(labelWithString: label)
        name.font = .monospacedSystemFont(ofSize: 10.5, weight: .regular)
        name.textColor = .secondaryLabelColor
        name.lineBreakMode = .byTruncatingMiddle
        name.translatesAutoresizingMaskIntoConstraints = false
        name.toolTip = path

        chip.addSubview(name)
        // The chip must be IN the hierarchy before any chip↔content
        // constraint activates, or AppKit throws (no common ancestor).
        let content = panel.contentView!
        content.addSubview(chip)
        NSLayoutConstraint.activate([
            chip.trailingAnchor.constraint(equalTo: content.trailingAnchor, constant: -12),
            chip.topAnchor.constraint(equalTo: content.topAnchor, constant: 10),
            name.leadingAnchor.constraint(equalTo: chip.leadingAnchor, constant: 10),
            name.trailingAnchor.constraint(equalTo: chip.trailingAnchor, constant: -10),
            name.centerYAnchor.constraint(equalTo: chip.centerYAnchor),
            name.widthAnchor.constraint(lessThanOrEqualToConstant: 280),
            chip.heightAnchor.constraint(equalToConstant: 24),
        ])
    }

    // MARK: live reload — xelatex/print loops rewrite the file in place

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

    /// Writers replace PDFs non-atomically (xelatex truncates then appends),
    /// so debounce and re-arm rather than reloading per event.
    private func scheduleReload() {
        if reloadPending { return }
        reloadPending = true
        monitor?.cancel()
        monitor = nil
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.4) { [weak self] in
            guard let self = self else { return }
            self.reloadPending = false
            guard FileManager.default.fileExists(atPath: self.path) else { return }
            let page = self.pdfView.currentPage.flatMap { self.pdfView.document?.index(for: $0) }
            if let fresh = PDFDocument(url: URL(fileURLWithPath: self.path)) {
                self.pdfView.document = fresh
                if let page = page, page < fresh.pageCount, let p = fresh.page(at: page) {
                    self.pdfView.go(to: p)
                }
            }
            self.watchFile()
        }
    }

    // MARK: window plumbing

    func focus() {
        NSApp.activate(ignoringOtherApps: true)
        panel.makeKeyAndOrderFront(nil)
    }

    func close() { panel.close() }

    func windowWillClose(_ notification: Notification) {
        monitor?.cancel()
        monitor = nil
        onClose()
    }

    /// Size the window to the first page's aspect, ~3/4 of the screen tall.
    private static func idealFrame(for document: PDFDocument) -> NSRect {
        let screen = NSScreen.main?.visibleFrame
            ?? NSRect(x: 0, y: 0, width: 1440, height: 900)
        let bounds = document.page(at: 0)?.bounds(for: .mediaBox)
            ?? NSRect(x: 0, y: 0, width: 612, height: 792)
        let height = min(screen.height * 0.78, 980)
        let width = min(height * (bounds.width / max(bounds.height, 1)), screen.width * 0.9)
        return NSRect(x: screen.midX - width / 2, y: screen.midY - height / 2,
                      width: width, height: height)
    }
}

/// A glance-pane, not an editor: any single click-drag moves the WINDOW
/// (no text selection — jeffrey's call), except clicks on link annotations,
/// which pass through so a PDF's hyperlinks keep working. Scrolling is
/// unaffected (wheel/trackpad never enters mouseDown).
private final class DraggablePdfView: PDFView {
    override func mouseDown(with event: NSEvent) {
        let viewPoint = convert(event.locationInWindow, from: nil)
        if let page = page(for: viewPoint, nearest: false) {
            let pagePoint = convert(viewPoint, to: page)
            if let annotation = page.annotation(at: pagePoint),
               annotation.type == "Link" || annotation.url != nil {
                super.mouseDown(with: event)
                return
            }
        }
        window?.performDrag(with: event)
    }

    // ── DARK READER — a dark-mode PDF read. macOS Preview has no real PDF dark
    // mode (only the system-wide Accessibility invert), so slab does its own: a
    // Core-Animation layer filter that INVERTS the pages then HUE-ROTATES 180° —
    // the exact `invert(1) hue-rotate(180deg)` trick the Dark Reader browser
    // extension uses, so black-on-cream text becomes light-on-dark while colour
    // photos (e.g. an essay's cover plate) stay roughly true instead of looking
    // like a negative. Only the PDF pages are filtered; the glass backdrop is a
    // sibling view, untouched.
    //
    // It FOLLOWS macOS system dark mode by default (so it flips live when the OS
    // theme changes), and 'd' is a per-window manual override (nil = follow). ──
    private var userOverride: Bool? = nil   // nil = follow the system appearance

    override var acceptsFirstResponder: Bool { true }

    override func keyDown(with event: NSEvent) {
        if event.charactersIgnoringModifiers?.lowercased() == "d",
           !event.modifierFlags.contains(.command) {
            userOverride = !currentlyDark()   // flip relative to what's showing
            applyDarkReader()
            return
        }
        super.keyDown(with: event)
    }

    // NSView calls this whenever the effective appearance changes — i.e. when
    // the user toggles macOS Light/Dark — and once when first shown.
    override func viewDidChangeEffectiveAppearance() {
        super.viewDidChangeEffectiveAppearance()
        applyDarkReader()
    }
    override func viewDidMoveToWindow() {
        super.viewDidMoveToWindow()
        applyDarkReader()
    }

    private func systemIsDark() -> Bool {
        effectiveAppearance.bestMatch(from: [.aqua, .darkAqua]) == .darkAqua
    }
    private func currentlyDark() -> Bool { userOverride ?? systemIsDark() }

    private func applyDarkReader() {
        wantsLayer = true
        guard let invert = CIFilter(name: "CIColorInvert"),
              let hue = CIFilter(name: "CIHueAdjust") else { return }
        hue.setValue(Float.pi, forKey: kCIInputAngleKey)   // 180° — Dark-Reader fidelity
        layer?.filters = currentlyDark() ? [invert, hue] : nil
    }
}

/// Titled-but-chromeless panel: key-able so scrolling and Esc work, and
/// Esc (cancelOperation) closes — the "just glance and dismiss" contract.
private final class PdfPanel: NSPanel {
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { true }
    override func cancelOperation(_ sender: Any?) { close() }
}

// MARK: - menu management (slab manages the viewers)

extension AppDelegate {
    @objc func focusPdf(_ sender: NSMenuItem) {
        if let path = sender.representedObject as? String { PdfViewer.shared.focus(path) }
    }

    @objc func closeAllPdfs() { PdfViewer.shared.closeAll() }

    @objc func openPdfFromPanel() {
        NSApp.activate(ignoringOtherApps: true)
        let panel = NSOpenPanel()
        panel.allowedContentTypes = [.pdf]
        panel.allowsMultipleSelection = true
        if panel.runModal() == .OK {
            for url in panel.urls { PdfViewer.shared.open(url.path) }
        }
    }
}
