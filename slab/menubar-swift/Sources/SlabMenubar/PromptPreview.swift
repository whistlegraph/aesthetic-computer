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
// What it shows is `scan_url` — the same address the rock encodes — so the two
// surfaces can never disagree about which piece this session is about. It runs
// capped (`maxfps`) because a wall of nine panes each animating at display rate
// is a warm laptop for no one's benefit.
//
// The piece runs at the pane's own viewport the whole time. The resting card is
// a small window onto it — a slow Ken Burns crop at 1:1, the way chat.mjs shows
// a `#painting` — and pointing at the card opens that window up to the whole
// viewport. Nothing is resized on the way: a live resize of a web view costs a
// reframe and a black frame or two, and the old card paid both on every hover.
//
// The chrome over it is not decoration. A web view that is covered, throttled
// or simply one save behind shows a frame that looks exactly like a live one,
// and that lie is worse than no preview at all — so the badge's only job is to
// say which of those you are looking at.

import AppKit
import SwiftUI
import WebKit

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

    /// Nothing to report: the card is showing the current piece, painting, and
    /// the file agrees with it. The overwhelmingly common case, and the one the
    /// chrome says NOTHING about — a badge reading "live" over every pane all
    /// day is a label you stop seeing, which makes the four states that matter
    /// harder to notice, not easier. Silence is the resting state.
    var quiet: Bool { flow == .live && !working && !paused }

    /// Only spoken when `quiet` is false.
    var label: String {
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
        let name = expanded ? state.piece : ""
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
    /// The resting card. Deliberately bigger than the rock: a stone reads as a
    /// status light at 56 points, but a piece has to be recognisable as itself,
    /// and below about this it is a texture.
    static let restSize = CGSize(width: 128, height: 96)
    /// Room left between the open card and its pane's bottom-right, so the
    /// terminal never looks completely papered over.
    private static let hoverMargin: CGFloat = 12

    /// Frames per second while nobody is looking. Four is enough to see that a
    /// piece is moving — which is the only question a glance asks — and cheap
    /// enough to leave running on every pane at once.
    private static let restFPS = 4

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
    private static let cardRadius: CGFloat = 3

    /// One slow lap of the resting crop around the piece. Matches the
    /// `#painting` embeds in chat.mjs, which this card is a cousin of.
    private static let kenBurnsCycle: TimeInterval = 8
    /// How often the crop moves. A pan of a few points a second at 24 steps
    /// reads as continuous; the web view is not repainted by it, only
    /// re-composited.
    private static let kenBurnsInterval: TimeInterval = 1.0 / 24
    /// How long the card takes to open or close.
    private static let openDuration: TimeInterval = 0.16

    private let window: NSWindow
    private let webView: WKWebView
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
    private var expanded = false
    private var state = PromptPreviewState() { didSet { if state != oldValue { redrawChrome() } } }

    /// Where the card is on screen, in AppKit coordinates. The controller's
    /// global pointer monitor tests this; the window itself never takes a
    /// mouse event, so a preview can never swallow a click meant for the
    /// terminal underneath it.
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
    /// The size the piece is actually rendered at — the pane's own viewport,
    /// less the card's insets. The web view is only ever this size, so opening
    /// the card reframes nothing: what was cropped is simply shown.
    private var viewport = PromptPreview.restSize

    /// Where in the lap this card's crop is; a random phase so a wall of cards
    /// does not drift in lockstep.
    private let kenBurnsSeed = Double.random(in: 0..<1)
    private var kenBurnsTimer: Timer?

    init() {
        let config = WKWebViewConfiguration()
        // A wall of previews must stay silent. AC's audio needs a gesture
        // anyway and this window takes none, but say it rather than rely on it.
        config.mediaTypesRequiringUserActionForPlayback = .all
        config.suppressesIncrementalRendering = false
        webView = WKWebView(frame: .zero, configuration: config)
        webView.setValue(false, forKey: "drawsBackground")
        // Positioned by hand: the crop is an offset, never a resize.
        webView.autoresizingMask = []

        badgeHost = NSHostingView(rootView: PromptPreviewBadge(state: PromptPreviewState(),
                                                               expanded: false))

        window = NSWindow(contentRect: NSRect(origin: .zero, size: Self.restSize),
                          styleMask: .borderless, backing: .buffered, defer: false)
        window.isOpaque = false
        window.backgroundColor = .clear
        // AppKit's window shadow is a soft, untunable bloom. Ours is drawn.
        window.hasShadow = false
        window.level = NSWindow.Level(Int(CGWindowLevelForKey(.normalWindow)) + 1)
        // Click-through, like the rock's render surface: hover is discovered by
        // the controller's pointer monitor, never by taking events away from
        // the terminal.
        window.ignoresMouseEvents = true
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
        webView.frame = card.bounds
        card.addSubview(webView)

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
        layoutWindow()
        layoutCard(animated: false)
        redrawChrome()
    }

    /// Point the card at an address. `scanURL` is the bare host+path the rock
    /// encodes, so the scheme and the preview's own chrome-suppressing
    /// parameters are added here rather than asked of the session.
    func load(scanURL: String) {
        let trimmed = scanURL.trimmingCharacters(in: .whitespacesAndNewlines)
        guard !trimmed.isEmpty, trimmed != loadedURL else { return }
        loadedURL = trimmed
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
        let url = "\(base)\(separator)nogap=true&nolabel=true&autoreload=true&maxfps=\(Self.restFPS)"
        guard let target = URL(string: url) else { return }
        webView.load(URLRequest(url: target))
    }

    func setState(_ next: PromptPreviewState) { state = next }

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

    /// Open under the pointer and close when it leaves. The card keeps its
    /// top-left corner, so it opens *into* the pane rather than walking across
    /// the screen — and it opens onto the piece already running at the pane's
    /// own size, so nothing reframes, reloads or goes black on the way.
    func setHovered(_ hovering: Bool) {
        guard hovering != expanded else { return }
        expanded = hovering
        layoutCard(animated: true)
        redrawChrome()
        syncKenBurns()
    }

    /// Park the card in the pane's top-left, under the title bar. `bounds` is
    /// the terminal window in CG screen space (top-left origin), matching what
    /// the rock controller already hands its overlays. Called every tick, so a
    /// pane that has not moved costs nothing here.
    func place(bounds b: (CGFloat, CGFloat, CGFloat, CGFloat), screenHeight: CGFloat) {
        let origin = NSPoint(x: b.0 + Self.leftInset,
                             y: screenHeight - (b.1 + Self.dropBelowTitle))
        let size = CGSize(width: b.2, height: b.3)
        guard origin != paneOrigin || size != paneSize || screenHeight != screenHeightForCG
        else { return }
        paneOrigin = origin
        paneSize = size
        screenHeightForCG = screenHeight
        layoutWindow()
        layoutCard(animated: false)
    }

    /// The piece renders at the pane's viewport: what the terminal shows, less
    /// the card's own insets. Never smaller than the resting card, so a tiny
    /// pane still gets a whole card rather than a sliver.
    private static func viewport(in pane: CGSize) -> CGSize {
        let width = max(restSize.width, (pane.width - leftInset - hoverMargin).rounded(.down))
        let height = max(restSize.height, (pane.height - dropBelowTitle - hoverMargin).rounded(.down))
        return CGSize(width: width, height: height)
    }

    /// Size the window and the web view to the viewport. This is the only
    /// place the web view changes size, and it happens only when the pane
    /// does — which is when the piece would have reframed anyway.
    private func layoutWindow() {
        viewport = Self.viewport(in: paneSize)
        let frame = NSRect(x: paneOrigin.x,
                           y: paneOrigin.y - viewport.height - Self.shadowDrop,
                           width: viewport.width + Self.shadowDrop,
                           height: viewport.height + Self.shadowDrop)
        if window.frame != frame { window.setFrame(frame, display: false) }
        if webView.frame.size != viewport {
            webView.frame = NSRect(origin: webView.frame.origin, size: viewport)
        }
    }

    /// The card's size right now: the whole viewport when open, the resting
    /// card otherwise. Its top-left never moves.
    private var cardSize: CGSize {
        expanded ? viewport
                 : CGSize(width: min(Self.restSize.width, viewport.width),
                          height: min(Self.restSize.height, viewport.height))
    }

    /// Fit the card, its shadow and its border to `cardSize`, and slide the
    /// web view so the card shows the right part of it — everything when
    /// open, the current crop when closed. Animated, the card unfolds over the
    /// piece; the piece itself never changes size, which is what keeps the
    /// unfolding free of the black frames a live resize costs.
    private func layoutCard(animated: Bool) {
        guard let content = window.contentView else { return }
        let size = cardSize
        let rect = NSRect(x: 0, y: content.bounds.height - size.height,
                          width: size.width, height: size.height)
        let crop = expanded ? CGPoint.zero : kenBurnsCrop(at: Date())
        let webOrigin = webOrigin(cardHeight: size.height, crop: crop)
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
        if animated {
            NSAnimationContext.runAnimationGroup { context in
                context.duration = Self.openDuration
                context.timingFunction = CAMediaTimingFunction(name: .easeOut)
                card.animator().frame = rect
                webView.animator().frame = NSRect(origin: webOrigin, size: viewport)
            }
            CATransaction.begin()
            CATransaction.setAnimationDuration(Self.openDuration)
            CATransaction.setAnimationTimingFunction(CAMediaTimingFunction(name: .easeOut))
            shadow.frame = shadowRect
            border.frame = borderRect
            CATransaction.commit()
        } else {
            card.frame = rect
            webView.frame = NSRect(origin: webOrigin, size: viewport)
            CATransaction.begin()
            CATransaction.setDisableActions(true)
            shadow.frame = shadowRect
            border.frame = borderRect
            CATransaction.commit()
        }
        layoutBadge()
    }

    /// Where the web view sits inside a card `cardHeight` tall so that the
    /// crop's top-left (measured from the piece's top-left, the way a picture
    /// is cropped) lands in the card's top-left. AppKit's origin is bottom-left,
    /// so the top edges are aligned by lifting the view by the height it
    /// overhangs, less the crop.
    private func webOrigin(cardHeight: CGFloat, crop: CGPoint) -> NSPoint {
        NSPoint(x: -crop.x, y: cardHeight - viewport.height + crop.y)
    }

    /// The resting crop's position at `time`: a slow circle around the piece
    /// at 1:1, the same lap the `#painting` embeds in chat.mjs take. Nothing
    /// is scaled — the card is a window onto the piece, not a thumbnail of it.
    private func kenBurnsCrop(at time: Date) -> CGPoint {
        let size = cardSize
        let maxX = max(0, viewport.width - size.width)
        let maxY = max(0, viewport.height - size.height)
        guard maxX > 0 || maxY > 0 else { return .zero }
        let progress = (time.timeIntervalSince1970 / Self.kenBurnsCycle + kenBurnsSeed)
            .truncatingRemainder(dividingBy: 1)
        let panX = (cos((progress + 0.25) * .pi * 2) + 1) / 2
        let panY = (sin((progress + 0.65) * .pi * 2) + 1) / 2
        return CGPoint(x: (maxX * panX).rounded(), y: (maxY * panY).rounded())
    }

    /// The crop moves only while there is something to move over and someone
    /// might see it: a closed card on screen. Open, hidden or too small to
    /// crop, the timer is off.
    private func syncKenBurns() {
        let size = cardSize
        let wants = window.isVisible && !expanded
            && (viewport.width > size.width || viewport.height > size.height)
        if wants {
            guard kenBurnsTimer == nil else { return }
            let timer = Timer(timeInterval: Self.kenBurnsInterval, repeats: true) { [weak self] _ in
                self?.stepKenBurns()
            }
            timer.tolerance = Self.kenBurnsInterval / 4
            RunLoop.main.add(timer, forMode: .common)
            kenBurnsTimer = timer
        } else {
            kenBurnsTimer?.invalidate()
            kenBurnsTimer = nil
        }
    }

    private func stepKenBurns() {
        guard !expanded else { return }
        let origin = webOrigin(cardHeight: cardSize.height, crop: kenBurnsCrop(at: Date()))
        if webView.frame.origin != origin { webView.setFrameOrigin(origin) }
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
            window.orderOut(nil)
            setHovered(false)
        }
        syncKenBurns()
    }

    var isOnScreen: Bool { window.isVisible }

    func close() {
        kenBurnsTimer?.invalidate()
        kenBurnsTimer = nil
        webView.stopLoading()
        // Point the view at nothing before tearing down: a WKWebView left
        // holding a running page keeps its content process alive past the
        // window that owned it.
        webView.loadHTMLString("", baseURL: nil)
        window.orderOut(nil)
    }
}
