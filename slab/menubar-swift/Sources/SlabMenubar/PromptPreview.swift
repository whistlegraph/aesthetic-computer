// PROMPT PREVIEWS.
//
// The piece an Easel session is holding, running in the top-left of that
// session's own terminal pane — opposite the rock, which carries the QR to the
// same address. Two corners saying the same thing to two different readers: the
// stone is for a phone across the desk, this is for the eyes already on the
// pane.
//
// Before this, seeing your own piece meant `/open` and a browser window that
// immediately fell behind the terminal grid, or scanning your own rock with
// your own phone. Neither is a glance. A preview that is simply always up
// costs one decision fewer every time you want to know what you just made.
//
// Pieces use `scan_url`, the address the rock encodes. Pictures, sounds, and
// papers use the local versioned `artifact_preview`; these have no phone QR.
// A successful artifact load updates the visible version and refresh feedback.
//
// The piece runs at one fixed logical viewport the whole time. The resting card is
// that whole viewport composited down to thumbnail size, and command-clicking the
// card scales it back up to 1:1 over the pane. Nothing is resized on the way: a
// live resize of a web view costs a reframe and a black frame or two, and the
// old card paid both on every hover.
//
// The pane itself resizing is the one time the web view must follow, and it
// follows late: while the terminal is being dragged the stage only rescales,
// and the real reframe happens once, after the drag settles, under a snapshot
// of the last frame that stays up until the piece has painted at its new size.
// A WKWebView mid-resize shows its window's backing colour — here, the card's
// black — and that flash was what a resize used to look like.
//
// While the card is open it is the piece, not a picture of it: the pointer
// reaches the web view and so does the keyboard. Pointing at your own work and
// having it ignore you is the wrong kind of preview.
//
// The chrome over it is not decoration. A web view that is covered, throttled
// or simply one save behind shows a frame that looks exactly like a live one,
// and that lie is worse than no preview at all — so the badge's only job is to
// say which of those you are looking at.

import AppKit
import SwiftUI
import WebKit
import PDFKit

/// Where the file on disk stands against what the previewed address is serving.
/// Mirrors `flow` in easel's `slab-session.mjs`; unknown strings read as `live`
/// so an older session never renders as a warning.
enum PromptFlow: String {
    case live
    case ahead
    case pushing

    init(_ raw: String) { self = PromptFlow(rawValue: raw) ?? .live }
}

/// Everything the badge needs to say, resolved to one line by `PromptPreview`
/// so the view itself holds no policy.
struct PromptPreviewState: Equatable {
    var flow: PromptFlow = .live
    var working = false
    /// The pane is covered, so WebKit has throttled the view and the pixels on
    /// screen are however old the last paint was. Never inferred from a
    /// timer — read from the same window-stack test that hides the rock.
    var paused = false
    var piece: String = ""
    var version = 0
    var previewError = false

    /// Nothing to report: the card is showing the current piece, painting, and
    /// the file agrees with it. The overwhelmingly common case, and the one the
    /// chrome says NOTHING about — a badge reading "live" over every pane all
    /// day is a label you stop seeing, which makes the four states that matter
    /// harder to notice, not easier. Silence is the resting state.
    var quiet: Bool { flow == .live && !working && !paused && !previewError }

    /// Only spoken when `quiet` is false.
    var label: String {
        if previewError { return "preview unavailable" }
        if paused { return "paused" }
        if working { return "working" }
        switch flow {
        case .pushing: return "pushing"
        case .ahead: return "ahead"
        case .live: return ""
        }
    }

    var tint: Color {
        if paused { return Color(white: 0.62) }
        if working { return Color(red: 1.0, green: 0.78, blue: 0.22) }
        switch flow {
        case .pushing: return Color(red: 0.40, green: 0.72, blue: 1.0)
        case .ahead:   return Color(red: 1.0, green: 0.55, blue: 0.30)
        case .live:    return Color(white: 0.5)
        }
    }
}

/// The badge, which is usually not there at all. It appears to say one of four
/// things — ahead, pushing, working, paused — and the piece's name, but only
/// while the card is grown, where a name is something you asked for rather than
/// something parked over the art. A piece is free to paint white, so the chip
/// carries its own dark ground rather than trusting contrast.
private struct PromptPreviewBadge: View {
    let state: PromptPreviewState
    let expanded: Bool

    /// The name earns its place only in the grown card; the state earns its
    /// place only when there is one. Both absent is the ordinary case, and the
    /// view draws nothing at all.
    private var text: String {
        let name = [expanded ? state.piece : "", state.version > 0 ? "v\(state.version)" : ""]
            .filter { !$0.isEmpty }.joined(separator: " · ")
        if state.quiet { return name }
        return name.isEmpty ? state.label : "\(name) · \(state.label)"
    }

    var body: some View {
        if text.isEmpty {
            EmptyView()
        } else {
            HStack(spacing: 5) {
                if !state.quiet {
                    Circle()
                        .fill(state.tint)
                        .frame(width: 6, height: 6)
                }
                Text(text)
                    .font(.system(size: 9, weight: .medium, design: .monospaced))
                    .foregroundColor(.white.opacity(0.92))
            }
            .padding(.horizontal, 6)
            .padding(.vertical, 3)
            .background(Capsule().fill(Color.black.opacity(0.62)))
            .overlay(
                Capsule().strokeBorder(
                    state.quiet ? Color.white.opacity(0.18) : state.tint.opacity(0.45),
                    lineWidth: 0.5)
            )
        }
    }
}

final class PromptPreview {
    /// Desktop's compact reference is eleven 16-point text units (176 points).
    /// Slab has no terminal cell metrics here, so this is a bounded approximation;
    /// narrow panes reduce it and the artifact aspect determines its height.
    static let restSize = CGSize(width: 176, height: 120)
    /// Room left between the open card and its pane's bottom-right, so the
    /// terminal never looks completely papered over.
    private static let hoverMargin: CGFloat = 12

    /// Inset from the pane's left edge, and drop below its title bar. The card
    /// parks *inside* the pane rather than over the title: the top-left of a
    /// macOS window belongs to the traffic lights, and a surface that covers
    /// them is a surface that breaks the window.
    private static let leftInset: CGFloat = 8
    private static let titleBar: CGFloat = 30
    /// Breathing room between the title bar and the card. Parked at the title
    /// bar exactly, the card reads as part of the window chrome — and on a pane
    /// tiled to the top of the screen it comes out flush against the menu bar,
    /// which looks like a mistake rather than a companion surface. A dozen
    /// points is enough to say the card is sitting *on* the terminal.
    private static let topPad: CGFloat = 12
    /// How far the hard shadow falls, right and down. Zero blur: the card is a
    /// small screen resting on a terminal, and a soft aura around it is the
    /// wrong physics — a crisp offset edge says the same thing at a fifth of
    /// the pixels and never smears the text underneath.
    private static let shadowDrop: CGFloat = 3
    private static var dropBelowTitle: CGFloat { titleBar + topPad }

    /// A radius this small reads as a cut corner rather than a rounded one,
    /// which is what a screen on a desk looks like.
    private static let cardRadius: CGFloat = 0

    /// How long the card takes to open or close.
    private static let openDuration: TimeInterval = 0.16

    private let window: PromptPreviewWindow
    private let webView: WKWebView
    private let refreshBridge = PromptPreviewRefreshBridge()
    private var lastRefresh = Date.distantPast
    /// Scale through AppKit's frame/bounds mapping, not a layer transform.
    /// WebKit consults the NSView visible rect when deciding which tiles to
    /// paint; a layer-only scale leaves that rect clipped to the thumbnail.
    private let stage = PromptPreviewStage()
    /// The last frame, held over the stage while the web view is reframed.
    private let cover = NSImageView()
    private var coverTimer: Timer?
    private let badgeHost: NSHostingView<PromptPreviewBadge>
    private let border = CALayer()
    /// The card proper — the part of the viewport the eye is shown. The
    /// window is always the whole viewport plus its shadow; the card is a
    /// clipping frame that opens and closes over it.
    private let card = NSView()
    private let shadow = CALayer()

    /// The address currently loaded, so a `sync` that changes nothing does not
    /// restart the piece — a reload is a visible flinch and a lost frame.
    private var loadedURL = ""
    private var requestedArtifact: LocalArtifactPreview?
    private var readyArtifact: LocalArtifactPreview?
    private var artifactLoading = false
    private var artifactFailed = false
    private var artifactDirectory: URL?
    private var artifactNavigation: WKNavigation?
    private var expanded = false
    private var pointerInside = false
    var onExpansionChanged: (() -> Void)?
    var isExpanded: Bool { expanded }
    /// Hover enlargement matches the desktop without changing the live viewport.
    private var expandsOnHover: Bool {
        UserDefaults.standard.string(forKey: "previewInteractionMode") != "compact"
    }
    private var displayAspect: CGFloat = 1.5
    private var state = PromptPreviewState() { didSet { if state != oldValue { redrawChrome() } } }

    /// Where the card is on screen, in AppKit coordinates. The controller's
    /// global pointer monitor tests this and enables mouse delivery only inside
    /// the visible card, never across the transparent backing window.
    private(set) var hitRect = NSRect.zero

    /// The card in CG screen space (top-left origin) — the coordinate system
    /// the window stack speaks, as opposed to `hitRect`'s AppKit one.
    private(set) var cgRect = CGRect.zero

    /// The pane's top-left in AppKit coordinates, where the card's top-left
    /// always is, open or closed.
    private var paneOrigin = NSPoint.zero
    /// The pane this card belongs to. The viewport is sized to it.
    private var paneSize = CGSize(width: 800, height: 600)
    /// Kept from the last `place` so a hover can recompute `cgRect` without
    /// waiting for the next tick to hand the height back.
    private var screenHeightForCG: CGFloat = 0
    /// Available visual space. The web view keeps its own fixed logical size,
    /// independent of this containing window, so opening
    /// the card reframes nothing: what was cropped is simply shown.
    private var viewport = PromptPreview.restSize

    /// Whoever was frontmost when the card took the keyboard, so closing the
    /// card hands focus back to the terminal it was over rather than leaving
    /// a menubar app as the active one.
    private var yieldTo: NSRunningApplication?

    init() {
        let config = WKWebViewConfiguration()
        // A wall of previews must stay silent. AC's audio needs a gesture
        // anyway and this window takes none, but say it rather than rely on it.
        config.mediaTypesRequiringUserActionForPlayback = .all
        config.suppressesIncrementalRendering = false
        config.userContentController.add(refreshBridge, name: "previewReady")
        config.userContentController.addUserScript(WKUserScript(source: """
            window.addEventListener('message', event => {
              if (event.source === window && event.data?.type === 'ready') {
                window.webkit.messageHandlers.previewReady.postMessage('ready');
              }
            });
            """, injectionTime: .atDocumentStart, forMainFrameOnly: true))
        webView = PromptPreviewWebView(frame: .zero, configuration: config)
        webView.setValue(false, forKey: "drawsBackground")
        webView.autoresizingMask = []

        badgeHost = NSHostingView(rootView: PromptPreviewBadge(state: PromptPreviewState(),
                                                               expanded: false))

        window = PromptPreviewWindow(contentRect: NSRect(origin: .zero, size: Self.restSize),
                                     styleMask: .borderless, backing: .buffered, defer: false)
        window.isOpaque = false
        window.backgroundColor = .clear
        // AppKit's window shadow is a soft, untunable bloom. Ours is drawn.
        window.hasShadow = false
        window.level = NSWindow.Level(Int(CGWindowLevelForKey(.normalWindow)) + 1)
        // The backing window is larger than the card. The controller enables
        // input only while the pointer is over the visible card, at either size.
        // That keeps its transparent area from swallowing terminal clicks.
        window.ignoresMouseEvents = true
        window.acceptsMouseMovedEvents = true
        window.collectionBehavior = [.canJoinAllSpaces, .stationary, .ignoresCycle]

        // The window is the whole viewport plus the room its shadow falls
        // into. Everything outside the card is transparent, so the window's
        // size is invisible; it is this big so that opening the card moves no
        // window and resizes no web view — both of which drop frames.
        let content = NSView(frame: NSRect(x: 0, y: 0,
                                           width: Self.restSize.width + Self.shadowDrop,
                                           height: Self.restSize.height + Self.shadowDrop))
        content.wantsLayer = true

        card.frame = NSRect(origin: NSPoint(x: 0, y: Self.shadowDrop), size: Self.restSize)
        card.wantsLayer = true
        card.layer?.masksToBounds = true
        card.layer?.cornerRadius = Self.cardRadius
        card.layer?.backgroundColor = NSColor.black.cgColor
        stage.wantsLayer = true
        stage.autoresizesSubviews = false
        stage.frame = card.bounds
        webView.frame = stage.bounds
        stage.addSubview(webView)
        card.addSubview(stage)
        cover.imageScaling = .scaleAxesIndependently
        cover.isHidden = true
        cover.wantsLayer = true
        cover.frame = card.bounds
        cover.autoresizingMask = [.width, .height]
        card.addSubview(cover)

        border.borderWidth = 1
        border.cornerRadius = Self.cardRadius
        border.frame = card.bounds
        border.autoresizingMask = [.layerWidthSizable, .layerHeightSizable]
        card.layer?.addSublayer(border)

        shadow.backgroundColor = NSColor.black.withAlphaComponent(0.38).cgColor
        shadow.cornerRadius = Self.cardRadius
        content.layer?.addSublayer(shadow)

        card.addSubview(badgeHost)
        content.addSubview(card)
        window.contentView = content
        window.onPrimaryClick = { [weak self] in self?.takeKeyboard() }
        webView.navigationDelegate = refreshBridge
        refreshBridge.onReady = { [weak self] in
            guard let self else { return }
            if self.requestedArtifact != nil { self.artifactReady() }
            else { self.celebrateRefresh() }
        }
        refreshBridge.onFailure = { [weak self] in self?.artifactFailure() }
        refreshBridge.onFinish = { [weak self] navigation in
            guard let self, let navigation, navigation === self.artifactNavigation,
                  self.requestedArtifact?.mime == "application/pdf" else { return }
            self.artifactReady()
        }
        layoutWindow()
        layoutCard(animated: false)
        redrawChrome()
    }

    /// Point the card at an address. `scanURL` is the bare host+path the rock
    /// encodes, so the scheme and the preview's own chrome-suppressing
    /// parameters are added here rather than asked of the session.
    private var publicationToken = ""
    func load(scanURL: String, publication: String = "") {
        let trimmed = scanURL.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty else { return }
        if trimmed == loadedURL {
            if !publication.isEmpty && publication != publicationToken { publicationToken = publication; webView.reloadFromOrigin() }
            return
        }
        publicationToken = publication
        requestedArtifact = nil
        readyArtifact = nil
        artifactLoading = false
        artifactFailed = false
        refreshBridge.localDirectory = nil
        refreshBridge.nonce = nil
        cover.isHidden = true
        loadedURL = trimmed
        setLogicalSize(CGSize(width: 128 * displayAspect, height: 128))
        layoutWindow()
        layoutCard(animated: false)
        // The rock encodes prompt.ac because a QR's payload is measured in
        // bytes and eleven of them decide whether the symbol needs a bigger
        // grid. Nothing is scanning this card, so it loads the address AC calls
        // itself — the same piece, served from the name the work lives under.
        let named = trimmed.replacingOccurrences(of: "prompt.ac/", with: "aesthetic.computer/")
        let base = named.contains("://") ? named : "https://" + named
        // `nogap` + `nolabel` strip the runtime's own frame and corner label:
        // the card is small, and the piece should own all of it. (Those two
        // together mean "kidlisp preview" to a runtime running *inside an
        // iframe* — this is a top-level load, so that branch is not in play.)
        let separator = base.contains("?") ? "&" : "?"
        // `autoreload` because this card has nobody to tap the update badge: a
        // green arrow in the corner of a 128-point window is a control out of
        // reach, sitting on the piece it came to announce. The card takes the
        // deploy silently instead.
        let url = "\(base)\(separator)nogap=true&nolabel=true&autoreload=true"
        guard let target = URL(string: url) else { return }
        webView.load(URLRequest(url: target))
    }

    /// Stage only the nominated output. HTML is generated here, never loaded
    /// from an authored project, and the WebKit read grant covers this copy only.
    func loadArtifact(_ artifact: LocalArtifactPreview) {
        guard artifact.key != loadedURL else { return }
        loadedURL = artifact.key
        requestedArtifact = artifact
        artifactFailed = false
        artifactLoading = true
        let nonce = UUID().uuidString
        var stagedDirectory: URL?
        do {
            let bytes = try artifact.readValidatedFile()
            let dimensions = artifact.dimensions(bytes)
            if artifact.kind == "picture", NSImage(data: bytes) == nil { throw CocoaError(.fileReadCorruptFile) }
            if artifact.mime == "application/pdf", (PDFDocument(data: bytes)?.pageCount ?? 0) == 0 { throw CocoaError(.fileReadCorruptFile) }
            let directory = FileManager.default.temporaryDirectory.appendingPathComponent("slab-artifact-\(UUID().uuidString)", isDirectory: true)
            try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true,
                                                   attributes: [.posixPermissions: 0o700])
            stagedDirectory = directory
            let target: URL
            if artifact.mime == "application/pdf" {
                target = directory.appendingPathComponent("artifact.pdf")
                try bytes.write(to: target, options: .atomic)
            } else {
                let text: String?
                if artifact.kind == "paper" {
                    guard bytes.count <= 2 * 1024 * 1024, let decoded = String(data: bytes, encoding: .utf8) else { throw CocoaError(.fileReadCorruptFile) }
                    text = decoded
                } else {
                    text = nil
                    try bytes.write(to: directory.appendingPathComponent("artifact"), options: .atomic)
                }
                target = directory.appendingPathComponent("index.html")
                let waveform = artifact.kind == "sound" ? LocalArtifactPreview.waveform(bytes) : nil
                try artifact.html(text: text, waveform: waveform, nonce: nonce).write(to: target, atomically: true, encoding: .utf8)
            }
            let start = { [weak self] in
                guard let self, self.requestedArtifact == artifact else {
                    try? FileManager.default.removeItem(at: directory); return
                }
                if let previous = self.artifactDirectory { try? FileManager.default.removeItem(at: previous) }
                self.setLogicalSize(dimensions)
                self.layoutWindow()
                self.layoutCard(animated: false)
                self.artifactDirectory = directory
                self.refreshBridge.localDirectory = directory
                self.refreshBridge.nonce = nonce
                self.artifactNavigation = self.webView.loadFileURL(target, allowingReadAccessTo: directory)
            }
            coverTimer?.invalidate()
            if readyArtifact != nil && cover.isHidden {
                webView.takeSnapshot(with: nil) { [weak self] image, _ in
                    guard let self, self.requestedArtifact == artifact else {
                        try? FileManager.default.removeItem(at: directory); return
                    }
                    if let image { self.cover.image = image; self.cover.isHidden = false }
                    start()
                }
            } else { start() }
        } catch {
            if let stagedDirectory { try? FileManager.default.removeItem(at: stagedDirectory) }
            artifactFailure()
        }
    }

    private func artifactReady() {
        guard let artifact = requestedArtifact, artifactLoading else { return }
        readyArtifact = artifact
        artifactLoading = false
        artifactFailed = false
        cover.isHidden = true
        cover.image = nil
        state.version = artifact.version
        state.piece = artifact.kind.capitalized
        state.previewError = false
        celebrateRefresh()
    }

    private func artifactFailure() {
        guard requestedArtifact != nil else { return }
        artifactLoading = false
        artifactFailed = true
        state.previewError = true
        // The snapshot of the last successful artifact stays visible.
    }

    func setState(_ next: PromptPreviewState) {
        var resolved = next
        if requestedArtifact != nil {
            resolved.version = readyArtifact?.version ?? 0
            resolved.piece = (readyArtifact ?? requestedArtifact)?.kind.capitalized ?? next.piece
            resolved.working = next.working || artifactLoading
            resolved.previewError = artifactFailed
        }
        state = resolved
    }

    /// Triggered by the runtime's boot completion, not by token arrivals or
    /// upload progress. All motion is composited; no permanent animation loop.
    private func celebrateRefresh() {
        guard isOnScreen, Date().timeIntervalSince(lastRefresh) > 0.5 else { return }
        lastRefresh = Date()
        let blink = CABasicAnimation(keyPath: "borderColor")
        blink.fromValue = NSColor.white.cgColor
        blink.toValue = border.borderColor
        blink.duration = 0.45
        border.add(blink, forKey: "pieceReady")
        guard !NSWorkspace.shared.accessibilityDisplayShouldReduceMotion else { return }
        let shake = CAKeyframeAnimation(keyPath: "transform.translation.x")
        shake.values = [0, -2, 2, -1, 1, 0]
        shake.duration = 0.28
        card.layer?.add(shake, forKey: "pieceReady")
        guard let root = window.contentView?.layer else { return }
        let rect = card.frame
        for i in 0..<8 {
            let particle = CALayer()
            particle.frame = CGRect(x: rect.minX + rect.width * CGFloat(i + 1) / 9,
                                    y: rect.minY + 2, width: 3, height: 3)
            particle.backgroundColor = NSColor(calibratedHue: CGFloat(i) / 8,
                                               saturation: 0.65, brightness: 1, alpha: 1).cgColor
            root.addSublayer(particle)
            let fall = CABasicAnimation(keyPath: "position")
            fall.fromValue = NSValue(point: particle.position)
            fall.toValue = NSValue(point: CGPoint(x: particle.position.x + CGFloat(i % 3 - 1) * 10,
                                                  y: particle.position.y - 24 - CGFloat(i % 3) * 7))
            fall.timingFunction = CAMediaTimingFunction(name: .easeIn)
            let fade = CABasicAnimation(keyPath: "opacity")
            fade.fromValue = 1
            fade.toValue = 0
            let group = CAAnimationGroup()
            group.animations = [fall, fade]
            group.duration = 0.6
            group.fillMode = .forwards
            group.isRemovedOnCompletion = false
            particle.add(group, forKey: "fall")
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.65) {
                particle.removeFromSuperlayer()
            }
        }
    }

    /// Set by the controller from the window-stack test, never from a timer.
    func setPaused(_ paused: Bool) {
        guard state.paused != paused else { return }
        state.paused = paused
    }

    /// Sample points of the card in CG screen coordinates (top-left origin),
    /// for the same whole-surface ownership test the rocks use: the card is
    /// hidden as a unit if any corner belongs to a window covering its pane.
    /// Sampling the corners rather than the centre alone is what catches a card
    /// half-slid under a neighbouring pane in a tiled grid.
    var visibilityPoints: [CGPoint] {
        let r = cgRect
        guard !r.isEmpty else { return [] }
        let inset: CGFloat = 4
        return [
            CGPoint(x: r.midX, y: r.midY),
            CGPoint(x: r.minX + inset, y: r.minY + inset),
            CGPoint(x: r.maxX - inset, y: r.minY + inset),
            CGPoint(x: r.minX + inset, y: r.maxY - inset),
            CGPoint(x: r.maxX - inset, y: r.maxY - inset),
        ]
    }

    /// Pointer ownership is independent of enlargement: left-clicks interact
    /// with the live piece even while compact. Hover enlarges the presentation.
    func setHovered(_ hovering: Bool) {
        pointerInside = hovering
        window.ignoresMouseEvents = !hovering
        if expandsOnHover { setExpanded(hovering) }
    }

    private func setExpanded(_ value: Bool) {
        guard value != expanded else { return }
        expanded = value
        layoutCard(animated: true)
        redrawChrome()
        pointerInside = hitRect.contains(NSEvent.mouseLocation)
        window.ignoresMouseEvents = !pointerInside
        if !value { releaseKeyboard() }
        onExpansionChanged?()
    }

    private func takeKeyboard() {
        let front = NSWorkspace.shared.frontmostApplication
        if front?.processIdentifier != ProcessInfo.processInfo.processIdentifier { yieldTo = front }
        NSApp.activate(ignoringOtherApps: true)
        window.makeKey()
        window.makeFirstResponder(webView)
    }

    private func releaseKeyboard() {
        if window.isKeyWindow { window.resignKey() }
        if NSWorkspace.shared.frontmostApplication?.processIdentifier == ProcessInfo.processInfo.processIdentifier,
           let back = yieldTo, !back.isTerminated { back.activate() }
        yieldTo = nil
    }

    /// Park the card in the pane's top-left, under the title bar. `bounds` is
    /// the terminal window in CG screen space (top-left origin), matching what
    /// the rock controller already hands its overlays. Called every tick, so a
    /// pane that has not moved costs nothing here.
    func place(bounds b: (CGFloat, CGFloat, CGFloat, CGFloat), screenHeight: CGFloat) {
        let origin = NSPoint(x: b.0 + Self.leftInset,
                             y: screenHeight - (b.1 + Self.dropBelowTitle))
        let size = CGSize(width: b.2, height: b.3)
        let screen = NSScreen.screens.first(where: { $0.frame.contains(origin) }) ?? NSScreen.main
        let aspect = screen.map { $0.frame.width / max(1, $0.frame.height) } ?? 1.5
        guard origin != paneOrigin || size != paneSize || screenHeight != screenHeightForCG
                || aspect != displayAspect else { return }
        displayAspect = aspect
        paneOrigin = origin
        paneSize = size
        screenHeightForCG = screenHeight
        layoutWindow()
        layoutCard(animated: false)
    }

    /// The piece renders at the pane's viewport: what the terminal shows, less
    /// the card's own insets. Never smaller than the resting card, so a tiny
    /// pane still gets a whole card rather than a sliver.
    private static func viewport(in pane: CGSize, aspect: CGFloat) -> CGSize {
        let availableWidth = max(restSize.width, pane.width - leftInset - hoverMargin)
        let availableHeight = max(restSize.height, pane.height - dropBelowTitle - hoverMargin)
        let width = min(availableWidth, availableHeight * aspect).rounded(.down)
        return CGSize(width: width, height: (width / aspect).rounded(.down))
    }

    /// Resizing the host only scales the existing surface; it never reflows WK.
    private func layoutWindow() {
        let size = stageSize
        let aspect = size.width > 0 && size.height > 0 ? size.width / size.height : displayAspect
        viewport = Self.viewport(in: paneSize, aspect: aspect)
        let frame = NSRect(x: paneOrigin.x,
                           y: paneOrigin.y - viewport.height - Self.shadowDrop,
                           width: viewport.width + Self.shadowDrop,
                           height: viewport.height + Self.shadowDrop)
        if window.frame != frame { window.setFrame(frame, display: false) }
        if loadedURL.isEmpty { setLogicalSize(CGSize(width: 128 * displayAspect, height: 128)) }
    }

    private var stageSize: CGSize { webView.frame.size }

    private func setLogicalSize(_ size: CGSize) {
        guard size.width > 0, size.height > 0 else { return }
        webView.frame = NSRect(origin: .zero, size: size)
        stage.viewportSize = size
        layoutCard(animated: false)
    }

    /// How far the stage is scaled: to fill the viewport when open (one, once
    /// the web view has caught up with the pane), and otherwise the largest
    /// scale at which the whole of it fits the resting card.
    private var scale: CGFloat {
        let s = stageSize
        guard s.width > 0, s.height > 0 else { return 1 }
        return expanded
            ? min(viewport.width / s.width, viewport.height / s.height)
            : min(1, min(Self.restSize.width, max(96, paneSize.width * 0.26)) / s.width,
                  Self.restSize.height / s.height)
    }

    /// The card's size right now: the stage at `scale` — the whole viewport
    /// when open, a thumbnail with the piece's own proportions at rest, rather
    /// than letterboxing it. Its top-left never moves.
    private var cardSize: CGSize {
        let s = scale, size = stageSize
        return CGSize(width: size.width * s, height: size.height * s)
    }

    /// Fit the card, its shadow and its border to `cardSize`, and scale the
    /// stage so the whole viewport fills the card — at 1:1 when open, composited
    /// down when closed. Animated, the card unfolds and the piece grows with
    /// it; the web view itself never changes size, which is what keeps the
    /// unfolding free of the black frames a live resize costs.
    private func layoutCard(animated: Bool) {
        guard let content = window.contentView else { return }
        let size = cardSize
        let rect = NSRect(x: 0, y: content.bounds.height - size.height,
                          width: size.width, height: size.height)
        // The card is what everything else means by "the preview" — the pointer
        // test, the ownership test. Both read its final rect, not the frame
        // mid-animation, so a pointer that opened the card is inside it at once.
        hitRect = NSRect(x: paneOrigin.x, y: paneOrigin.y - size.height,
                         width: size.width, height: size.height)
        cgRect = CGRect(x: hitRect.minX, y: screenHeightForCG - hitRect.maxY,
                        width: hitRect.width, height: hitRect.height)
        let shadowRect = NSRect(x: rect.minX + Self.shadowDrop, y: rect.minY - Self.shadowDrop,
                                width: size.width, height: size.height)
        let borderRect = NSRect(origin: .zero, size: size)
        let stageFrame = NSRect(origin: .zero, size: size)
        if animated {
            NSAnimationContext.runAnimationGroup { context in
                context.duration = Self.openDuration
                context.timingFunction = CAMediaTimingFunction(name: .easeOut)
                card.animator().frame = rect
                stage.animator().frame = stageFrame
            }
            CATransaction.begin()
            CATransaction.setAnimationDuration(Self.openDuration)
            CATransaction.setAnimationTimingFunction(CAMediaTimingFunction(name: .easeOut))
            shadow.frame = shadowRect
            border.frame = borderRect
            CATransaction.commit()
        } else {
            card.frame = rect
            stage.frame = stageFrame
            CATransaction.begin()
            CATransaction.setDisableActions(true)
            shadow.frame = shadowRect
            border.frame = borderRect
            CATransaction.commit()
        }
        layoutBadge()
    }

    private func layoutBadge() {
        badgeHost.layoutSubtreeIfNeeded()
        let size = badgeHost.fittingSize
        let inset: CGFloat = 5
        badgeHost.frame = NSRect(x: inset, y: inset, width: size.width, height: size.height)
    }

    private func redrawChrome() {
        badgeHost.rootView = PromptPreviewBadge(state: state, expanded: expanded)
        // The border is the at-a-glance half of the badge: readable from across
        // the grid, where nine-point type is not. Quiet cards get a plain dark
        // edge that reads as the card's own frame — a wall of green borders
        // would be the same all-day label the chip just stopped being.
        let edge = state.quiet
            ? NSColor.black.withAlphaComponent(0.45)
            : NSColor(state.tint).withAlphaComponent(state.paused ? 0.35 : 0.75)
        border.borderColor = edge.cgColor
        layoutBadge()
    }

    /// Show or hide as one unit, from the same window-stack ownership test that
    /// gates the rock. A card that is ordered out is also a card whose piece
    /// WebKit will stop painting, so this is exactly where `paused` is known.
    func setVisible(_ visible: Bool) {
        if visible {
            if !window.isVisible { window.orderFrontRegardless() }
        } else if window.isVisible {
            setHovered(false)
            setExpanded(false)
            releaseKeyboard()
            window.orderOut(nil)
        }
    }

    // Mirrors easel/src/frame-capture-script.mjs; no request-supplied JavaScript.
    private static let frameCaptureExpression = #"""
(()=>{
 try {
 const root=document.getElementById('aesthetic-computer');
 if(!root)return {pending:true,url:location.href};
 const visible=c=>c&&getComputedStyle(c).display!=='none';
 if(visible(root.querySelector('canvas[data-type="webgpu"]')))throw new Error('WebGPU capture is not supported by this canvas reader');
 const composite=root.querySelector('canvas[data-type="webgl-composite"]');
 const gpu=visible(composite)?composite:null;
 const canvas=gpu||[...root.children].find(c=>c.tagName==='CANVAS'&&!c.dataset.type);
 if(!canvas||!canvas.width||!canvas.height)return {pending:true,url:location.href};
 const width=canvas.width,height=canvas.height;
 if(!width||!height||width>2048||height>2048||width*height>1048576)throw new Error('Canvas exceeds capture limit (1 megapixel)');
 let surface=canvas;
 if(gpu){const gl=gpu.getContext('webgl2')||gpu.getContext('webgl');if(!gl?.getContextAttributes()?.preserveDrawingBuffer)throw new Error('WebGL buffer is not preserved for capture');surface=document.createElement('canvas');surface.width=width;surface.height=height;surface.getContext('2d').drawImage(gpu,0,0);}
 const ctx=surface.getContext('2d');if(!ctx)throw new Error('Pixel buffer unavailable');
 const data=ctx.getImageData(0,0,width,height).data;
 let raw='';for(let at=0;at<data.length;at+=8192)raw+=String.fromCharCode(...data.subarray(at,at+8192));
 return {width,height,rgba:btoa(raw),png:surface.toDataURL('image/png').split(',')[1],source:gpu?'webgl-composite':'software-canvas',renderedRevisionVerified:false,url:location.href};
 }catch(error){return {error:String(error.message||error),url:location.href};}
})()
"""#

    private var framePollAt = Date.distantPast
    private var frameCaptureID: String?
    private var frameContext = ""

    /// A private request broker, driven by the existing overlay refresh tick.
    /// Requests select an identity only; they never supply code or output paths.
    func pollFrameRequests(cwd: String, channel: String, revision: String) {
        frameContext = "\(cwd)\n\(channel)\n\(revision)"
        guard !channel.isEmpty, !revision.isEmpty, cwd.hasPrefix("/"),
              Date().timeIntervalSince(framePollAt) >= 0.25, frameCaptureID == nil else { return }
        framePollAt = Date()
        let base = URL(fileURLWithPath: cwd).appendingPathComponent(".easel", isDirectory: true)
        let requests = base.appendingPathComponent("frame-requests", isDirectory: true)
        let responses = base.appendingPathComponent("frame-responses", isDirectory: true)
        guard Self.frameDirectoryIsSafe(base), Self.frameDirectoryIsSafe(requests),
              Self.frameDirectoryIsSafe(responses),
              let entries = try? FileManager.default.contentsOfDirectory(at: requests, includingPropertiesForKeys: [.isRegularFileKey, .isSymbolicLinkKey, .fileSizeKey]) else { return }
        for file in entries.prefix(64) {
            let id = file.deletingPathExtension().lastPathComponent
            guard file.pathExtension == "json", UUID(uuidString: id) != nil,
                  !FileManager.default.fileExists(atPath: responses.appendingPathComponent(id + ".json").path),
                  let info = try? file.resourceValues(forKeys: [.isRegularFileKey, .isSymbolicLinkKey, .fileSizeKey]),
                  info.isRegularFile == true, info.isSymbolicLink != true, (info.fileSize ?? Int.max) <= 4096,
                  let data = try? Data(contentsOf: file),
                  let request = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any],
                  request["id"] as? String == id,
                  request["channel"] as? String == channel else { continue }
            let fail: (String) -> Void = { error in
                Self.writeFrameResponse(["id": id, "error": error], id: id, directory: responses)
            }
            let created: Date?
            if let ms = request["createdAt"] as? Double {
                created = Date(timeIntervalSince1970: ms / 1000)
            } else if let text = request["createdAt"] as? String {
                let format = ISO8601DateFormatter()
                format.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
                created = format.date(from: text) ?? ISO8601DateFormatter().date(from: text)
            } else { created = nil }
            guard let created, Date().timeIntervalSince(created) >= -1,
                  Date().timeIntervalSince(created) <= 15 else { fail("Frame request expired."); continue }
            guard request["revision"] as? String == revision else { fail("Preview revision changed."); continue }
            guard requestedArtifact == nil, !webView.isLoading else { fail("Piece preview is not ready."); continue }
            guard Self.frameURLMatches(webView.url, channel: channel) else {
                fail("Preview URL does not match the current piece channel."); continue
            }
            frameCaptureID = id
            let context = frameContext
            DispatchQueue.main.asyncAfter(deadline: .now() + 5) { [weak self] in
                guard let self, self.frameCaptureID == id else { return }
                self.frameCaptureID = nil
                fail("Canvas capture timed out.")
            }
            webView.evaluateJavaScript(Self.frameCaptureExpression) { [weak self] value, error in
                guard let self, self.frameCaptureID == id else { return }
                self.frameCaptureID = nil
                guard self.frameContext == context, self.requestedArtifact == nil else { fail("Preview changed during capture."); return }
                if let error {
                    let detail = (error as NSError).userInfo["WKJavaScriptExceptionMessage"] as? String
                    fail(String((detail ?? "Canvas capture failed.").prefix(500))); return
                }
                guard let result = value as? [String: Any] else { fail("Canvas capture failed."); return }
                if result["pending"] as? Bool == true { return }
                guard Self.frameURLMatches(self.webView.url, channel: channel),
                      let capturedURL = result["url"] as? String,
                      Self.frameURLMatches(URL(string: capturedURL), channel: channel) else {
                    fail("Preview navigated during capture."); return
                }
                if let message = result["error"] as? String { fail(String(message.prefix(500))); return }
                guard let source = result["source"] as? String,
                      ["software-canvas", "webgl-composite"].contains(source),
                      let width = result["width"] as? Int, let height = result["height"] as? Int,
                      width > 0, height > 0, width <= 1_048_576 / height,
                      let png = result["png"] as? String, png.count <= 8_000_000,
                      let rgba = result["rgba"] as? String, rgba.count <= 6_000_000,
                      let pixels = Data(base64Encoded: rgba), pixels.count == width * height * 4,
                      let image = Data(base64Encoded: png), image.starts(with: [137,80,78,71,13,10,26,10]) else {
                    fail("Canvas returned an invalid or oversized frame."); return
                }
                Self.writeFrameResponse(["id": id, "channel": channel, "revision": revision,
                    "capturedAt": ISO8601DateFormatter().string(from: Date()), "width": width, "height": height,
                    "png": png, "rgba": rgba, "source": source, "renderedRevisionVerified": false],
                    id: id, directory: responses)
            }
            break
        }
    }

    private static func frameURLMatches(_ url: URL?, channel: String) -> Bool {
        guard let url, url.scheme == "https",
              ["aesthetic.computer", "prompt.ac"].contains(url.host ?? "") else { return false }
        return url.path == "/@" + channel
    }

    private static func frameDirectoryIsSafe(_ directory: URL) -> Bool {
        guard directory.standardizedFileURL.path == directory.resolvingSymlinksInPath().standardizedFileURL.path,
              let info = try? directory.resourceValues(forKeys: [.isDirectoryKey, .isSymbolicLinkKey]),
              info.isDirectory == true, info.isSymbolicLink != true else { return false }
        return true
    }

    private static func writeFrameResponse(_ response: [String: Any], id: String, directory: URL) {
        guard frameDirectoryIsSafe(directory), UUID(uuidString: id) != nil,
              let data = try? JSONSerialization.data(withJSONObject: response) else { return }
        let temporary = directory.appendingPathComponent(".\(UUID().uuidString).tmp")
        let destination = directory.appendingPathComponent(id + ".json")
        guard FileManager.default.createFile(atPath: temporary.path, contents: data,
                                             attributes: [.posixPermissions: 0o600]) else { return }
        defer { try? FileManager.default.removeItem(at: temporary) }
        // moveItem refuses to overwrite an existing file, including a symlink.
        try? FileManager.default.moveItem(at: temporary, to: destination)
    }

    var isOnScreen: Bool { window.isVisible }

    func close() {
        frameCaptureID = nil
        frameContext = ""
        setHovered(false)
        setExpanded(false)
        releaseKeyboard()
        window.onPrimaryClick = nil
        onExpansionChanged = nil
        coverTimer?.invalidate()
        webView.stopLoading()
        refreshBridge.onReady = nil
        refreshBridge.onFailure = nil
        refreshBridge.onFinish = nil
        if let artifactDirectory { try? FileManager.default.removeItem(at: artifactDirectory) }
        webView.configuration.userContentController.removeScriptMessageHandler(forName: "previewReady")
        // Point the view at nothing before tearing down: a WKWebView left
        // holding a running page keeps its content process alive past the
        // window that owned it.
        webView.loadHTMLString("", baseURL: nil)
        window.orderOut(nil)
    }
}

private final class PromptPreviewRefreshBridge: NSObject, WKScriptMessageHandler, WKNavigationDelegate {
    var onReady: (() -> Void)?
    var onFailure: (() -> Void)?
    var onFinish: ((WKNavigation?) -> Void)?
    var localDirectory: URL?
    var nonce: String?
    func userContentController(_ userContentController: WKUserContentController,
                               didReceive message: WKScriptMessage) {
        guard message.frameInfo.isMainFrame, let body = message.body as? String else { return }
        if let nonce {
            if body == "ready:\(nonce)" { onReady?() }
            if body == "failed:\(nonce)" { onFailure?() }
        } else if body == "ready" { onReady?() }
    }
    func webView(_ webView: WKWebView, didFinish navigation: WKNavigation!) { onFinish?(navigation) }
    func webView(_ webView: WKWebView, didFail navigation: WKNavigation!, withError error: Error) {
        if (error as NSError).code != NSURLErrorCancelled { onFailure?() }
    }
    func webView(_ webView: WKWebView, didFailProvisionalNavigation navigation: WKNavigation!, withError error: Error) {
        if (error as NSError).code != NSURLErrorCancelled { onFailure?() }
    }
    func webView(_ webView: WKWebView, decidePolicyFor navigationAction: WKNavigationAction,
                 decisionHandler: @escaping (WKNavigationActionPolicy) -> Void) {
        if let directory = localDirectory {
            guard let url = navigationAction.request.url, url.isFileURL,
                  url.standardizedFileURL.path.hasPrefix(directory.standardizedFileURL.path + "/") else {
                decisionHandler(.cancel); return
            }
        }
        decisionHandler(.allow)
    }
}

/// Keep WebKit's logical viewport fixed as the visible card changes size.
private final class PromptPreviewStage: NSView {
    var viewportSize = CGSize(width: 128, height: 96) {
        didSet { super.setBoundsSize(viewportSize) }
    }

    override func setFrameSize(_ newSize: NSSize) {
        super.setFrameSize(newSize)
        // Frame animation otherwise scales the bounds too, changing the
        // logical viewport while WebKit's frame remains fixed.
        super.setBoundsSize(viewportSize)
    }
}

/// A borderless window that can take the keyboard when the piece is played.
final class PromptPreviewWindow: NSWindow {
    var onPrimaryClick: (() -> Void)?
    override var canBecomeKey: Bool { true }
    override var canBecomeMain: Bool { false }

    override func sendEvent(_ event: NSEvent) {
        if event.type == .leftMouseDown { onPrimaryClick?() }
        super.sendEvent(event)
    }
}

/// The first left-click both focuses and reaches the piece's actual controls.
private final class PromptPreviewWebView: WKWebView {
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
}
