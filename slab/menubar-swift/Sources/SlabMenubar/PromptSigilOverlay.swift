// PROMPT ROCKS.
//
// The tumbling stones parked at the top-right of every terminal window running
// a live Claude session — one rock per prompt. Say "prompt rocks" and this file
// is what you mean.
//
// A rock's SHAPE is its identity: a 3D sigil grown from the session's
// id + current prompt, so the stone re-forms whenever the session moves on to a
// new prompt. Its MOTION is the status channel — spin speed and direction say
// working / awaiting / complete, and a poke from a peer makes it blink and
// rattle. It carries a pet name in bubble lettering, and pointing at it
// reveals a slowly evolving account of the session.
//
// Each rock is a borderless, click-through normal-level window ordered directly
// above its own terminal. AppKit then gives it the terminal's real place in the
// window stack: windows above that terminal also cover the rock. `overlayAt`
// still checks the stack at the pointer so a covered stone cannot answer hover
// or clicks during the short interval before AppKit/window snapshots settle.

import AppKit
import CoreGraphics
import ApplicationServices
import SceneKit
import Vision

/// Private (but stable, widely-used by window managers) bridge from an AX
/// window element to its CGWindowID — lets a kAXWindowMoved callback map the
/// moved window back to the badge bound to it.
@_silgen_name("_AXUIElementGetWindow")
private func _AXUIElementGetWindow(_ element: AXUIElement, _ windowID: UnsafeMutablePointer<CGWindowID>) -> AXError

/// The global sun that lights every stone, driven by the local clock: it rises
/// in the east, arcs overhead at midday, sets in the west, and drops long
/// shadows near dawn/dusk. Deliberately simple (a daylight arc from local time,
/// no ephemeris) — enough for the light to feel like it belongs to the room's
/// time of day. `hx` is horizontal position (+1 east/right … −1 west/left),
/// `elevation` height (0 horizon … 1 overhead), `intensity` strength, and
/// `drop` the flat shadow offset (screen points, away from the sun).
enum Sun {
    static func light(at date: Date) -> (hx: CGFloat, elevation: CGFloat, intensity: CGFloat, drop: CGSize) {
        let c = Calendar.current.dateComponents([.hour, .minute], from: date)
        let h = CGFloat(c.hour ?? 12) + CGFloat(c.minute ?? 0) / 60
        let sunrise: CGFloat = 6.5, sunset: CGFloat = 19.5
        let hx: CGFloat, e: CGFloat, intensity: CGFloat
        if h < sunrise || h > sunset {
            hx = 0; e = 0.12; intensity = 0.32          // night: dim, near-overhead
        } else {
            let t = (h - sunrise) / (sunset - sunrise)  // 0 dawn … 1 dusk
            hx = cos(t * .pi)                            // +1 east → −1 west
            e = sin(t * .pi)                             // 0 horizon … 1 noon
            intensity = 0.55 + 0.45 * e
        }
        let len: CGFloat = 2 + (1 - e) * 4               // longer shadow when low
        return (hx, e, intensity, CGSize(width: -hx * len, height: -len))
    }
}

/// A short, click-through typographic handoff synchronized with the real
/// Accessibility keystrokes. It visualizes automation without becoming a
/// second UI: the actual prompt still belongs to Terminal/Codex/Claude.
private enum PromptGlyphFlight {
    private static var panel: NSPanel?
    private static var sequence = 0

    static func show(text: String, from source: CGPoint, to destination: CGPoint,
                     color: NSColor, maxWidth: CGFloat, on screen: NSScreen) {
        precondition(Thread.isMainThread)
        var chars = Array(text.prefix(48))
        guard !chars.isEmpty else { return }
        let window = panel ?? makePanel()
        panel = window
        window.setFrame(screen.frame, display: true)
        guard let root = window.contentView?.layer else { return }
        root.sublayers?.forEach { $0.removeFromSuperlayer() }

        let localSource = CGPoint(x: source.x - screen.frame.minX,
                                  y: source.y - screen.frame.minY)
        let localDestination = CGPoint(x: destination.x - screen.frame.minX,
                                       y: destination.y - screen.frame.minY)
        let tileColor = color.withAlphaComponent(0.94)
        let rgb = tileColor.usingColorSpace(.deviceRGB) ?? tileColor
        let luminance = 0.2126 * rgb.redComponent
            + 0.7152 * rgb.greenComponent
            + 0.0722 * rgb.blueComponent
        let promptColor: NSColor = luminance > 0.58
            ? NSColor(deviceWhite: 0.06, alpha: 1)
            : NSColor(deviceWhite: 0.98, alpha: 1)
        // These are visibly shed by the rock's name, so they share its exact
        // playful face before settling onto the terminal caption banner.
        var fontSize: CGFloat = 18
        var flightFont = playfulRockFont(fontSize)
        var fontAttributes: [NSAttributedString.Key: Any] = [.font: flightFont]
        var glyphOffsets: [CGFloat] = []
        var glyphWidths: [CGFloat] = []
        var penX: CGFloat = 0
        func measureRow() {
            glyphOffsets.removeAll(keepingCapacity: true)
            glyphWidths.removeAll(keepingCapacity: true)
            penX = 0
            for character in chars {
                glyphOffsets.append(penX)
                let measured = ceil((String(character) as NSString)
                    .size(withAttributes: fontAttributes).width)
                let tileWidth = max(7, measured + 1)
                glyphWidths.append(tileWidth)
                penX += tileWidth + 0.5
            }
        }
        measureRow()
        if penX > maxWidth {
            // Keep the whole contextual bump inside the responding terminal.
            // Scale its actual font metrics rather than crushing only the
            // positions, with a floor that remains readable from the wall.
            fontSize = max(11, floor(fontSize * maxWidth / penX))
            flightFont = playfulRockFont(fontSize)
            fontAttributes = [.font: flightFont]
            measureRow()
            // Very narrow panes can still be smaller than 48 glyphs at the
            // readable floor. The visual caption may truncate (the complete
            // prompt is still typed into the agent) but it must never escape
            // the responding terminal's content rectangle.
            while chars.count > 1 && penX > maxWidth {
                chars.removeLast()
                measureRow()
            }
        }
        let begin = CACurrentMediaTime() + 0.03
        // Keep the already-landed letters present until the complete nudge has
        // arrived.  Per-glyph 1.5 s fades made the head of a normal heartbeat
        // disappear while its tail was still flying, so it never read as one
        // persistent line of text at the terminal prompt.
        let settleDuration = 0.62 + Double(max(0, chars.count - 1)) * 0.028
        let visibleDuration = settleDuration + 1.35

        // One continuous caption banner sits beneath the complete row.  Add
        // it before the glyphs so Core Animation's sibling order guarantees
        // the lettering remains in front.
        let bannerInset: CGFloat = 6
        let banner = CALayer()
        banner.frame = CGRect(x: localDestination.x - bannerInset,
                              y: localDestination.y - 11,
                              width: penX + bannerInset * 2, height: 22)
        banner.backgroundColor = tileColor.cgColor
        banner.cornerRadius = 3
        banner.shadowColor = color.cgColor
        banner.shadowOpacity = 0.72
        banner.shadowRadius = 4
        banner.shadowOffset = .zero
        root.addSublayer(banner)

        let bannerReveal = CABasicAnimation(keyPath: "transform.scale.x")
        bannerReveal.fromValue = 0
        bannerReveal.toValue = 1
        bannerReveal.duration = settleDuration
        bannerReveal.beginTime = begin
        bannerReveal.timingFunction = CAMediaTimingFunction(name: .easeOut)
        bannerReveal.fillMode = .both
        bannerReveal.isRemovedOnCompletion = false
        banner.anchorPoint = CGPoint(x: 0, y: 0.5)
        banner.position = CGPoint(x: localDestination.x - bannerInset,
                                  y: localDestination.y)
        banner.add(bannerReveal, forKey: "prompt-banner-reveal")

        let bannerFade = CAKeyframeAnimation(keyPath: "opacity")
        bannerFade.values = [0, 1, 1, 0]
        bannerFade.keyTimes = [0, 0.04, 0.94, 1]
        bannerFade.duration = visibleDuration
        bannerFade.beginTime = begin
        bannerFade.fillMode = .both
        bannerFade.isRemovedOnCompletion = false
        banner.add(bannerFade, forKey: "prompt-banner-fade")
        for (index, character) in chars.enumerated() {
            let glyph = CATextLayer()
            glyph.string = String(character)
            glyph.font = flightFont
            glyph.fontSize = fontSize
            glyph.alignmentMode = .center
            glyph.contentsScale = screen.backingScaleFactor
            // Match the bright CLI writing color while retaining enough of
            // this prox's palette to make the automation visibly its own.
            glyph.foregroundColor = promptColor.cgColor
            glyph.shadowColor = NSColor.black.withAlphaComponent(0.35).cgColor
            glyph.shadowOpacity = 0.55
            glyph.shadowRadius = 1
            glyph.shadowOffset = .zero
            let tileWidth = glyphWidths[index]
            glyph.bounds = CGRect(x: 0, y: 0, width: tileWidth, height: 22)
            glyph.position = localDestination
            root.addSublayer(glyph)

            let end = CGPoint(x: localDestination.x + glyphOffsets[index] + tileWidth * 0.5,
                              y: localDestination.y)
            let path = CGMutablePath()
            path.move(to: CGPoint(x: localSource.x + CGFloat(index % 3) * 3,
                                  y: localSource.y + CGFloat(index % 2) * 4))
            path.addQuadCurve(to: end,
                              control: CGPoint(x: (localSource.x + end.x) * 0.5,
                                               y: max(localSource.y, end.y) + 82
                                                   + CGFloat(index % 4) * 7))
            let travel = CAKeyframeAnimation(keyPath: "position")
            travel.path = path
            travel.calculationMode = .paced
            travel.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
            travel.duration = 0.62
            travel.beginTime = begin + Double(index) * 0.028
            travel.fillMode = .both
            travel.isRemovedOnCompletion = false
            glyph.add(travel, forKey: "prompt-flight")

            let fade = CAKeyframeAnimation(keyPath: "opacity")
            fade.values = [0, 1, 1, 1, 0]
            fade.keyTimes = [0, 0.04, 0.82, 0.94, 1]
            fade.duration = visibleDuration
            fade.beginTime = travel.beginTime
            fade.fillMode = .both
            fade.isRemovedOnCompletion = false
            glyph.add(fade, forKey: "prompt-flight-fade")
        }

        sequence += 1
        let token = sequence
        window.orderFrontRegardless()
        let duration = visibleDuration + 0.08
        DispatchQueue.main.asyncAfter(deadline: .now() + duration) {
            guard sequence == token else { return }
            window.orderOut(nil)
            root.sublayers?.forEach { $0.removeFromSuperlayer() }
        }
    }

    private static func makePanel() -> NSPanel {
        let window = NSPanel(contentRect: .zero,
                             styleMask: [.borderless, .nonactivatingPanel],
                             backing: .buffered, defer: false)
        window.isOpaque = false
        window.backgroundColor = .clear
        window.hasShadow = false
        window.level = .screenSaver
        window.ignoresMouseEvents = true
        window.hidesOnDeactivate = false
        window.collectionBehavior = [.canJoinAllSpaces, .stationary,
                                     .ignoresCycle, .fullScreenAuxiliary]
        let view = NSView()
        view.wantsLayer = true
        window.contentView = view
        return window
    }
}

/// MacPal's playful lettering, borrowed for the rock names: Comic Sans MS
/// Bold, falling back to Chalkboard SE, then a heavy system face.
func playfulRockFont(_ pt: CGFloat) -> NSFont {
    for n in ["Comic Sans MS Bold", "ComicSansMS-Bold", "Chalkboard SE Bold", "ChalkboardSE-Bold"] {
        if let f = NSFont(name: n, size: pt) { return f }
    }
    return NSFont.systemFont(ofSize: pt, weight: .heavy)
}

/// One character of a rock's name — a layer that draws its attributed string
/// (bubble lettering: fill + stroke + hard shadow), so each letter can carry
/// its own static jitter and wiggle animation, MacPal-style.
final class RockCharLayer: CALayer {
    var attr: NSAttributedString?
    var inset: CGFloat = 0
    override func draw(in ctx: CGContext) {
        guard let attr = attr else { return }
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(cgContext: ctx, flipped: false)
        attr.draw(at: NSPoint(x: inset, y: inset))
        NSGraphicsContext.restoreGraphicsState()
    }
}

/// Compact, metadata-driven destination mark parked at the bottom-left of a
/// prompt terminal. The deliberately long type name is the feature's canonical
/// identifier: `platform-target-awareness-identifier-badge`. It is not keyed
/// to a pet name or tty; the owning session's `platform_target` marker decides
/// what this view acknowledges.
final class PlatformTargetAwarenessIdentifierBadgeView: NSView {
    var platformTarget = "" {
        didSet { needsDisplay = true }
    }

    override var isFlipped: Bool { false }

    override func draw(_ dirtyRect: NSRect) {
        super.draw(dirtyRect)
        guard !platformTarget.isEmpty else { return }
        let xbox = platformTarget == "xbox"
        let green = NSColor(deviceRed: 0.063, green: 0.486, blue: 0.063, alpha: 1)
        let body = NSBezierPath(roundedRect: bounds.insetBy(dx: 1, dy: 1),
                                xRadius: 7, yRadius: 7)
        (xbox ? green : NSColor(deviceWhite: 0.10, alpha: 0.92)).setFill()
        body.fill()
        NSColor.white.withAlphaComponent(0.25).setStroke()
        body.lineWidth = 1
        body.stroke()

        if xbox {
            // A small Xbox sphere: white field with the familiar green X.
            // It is drawn locally so the acknowledgement needs no network or
            // bundled raster asset and stays sharp at every display scale.
            let sphereRect = NSRect(x: 4, y: 4, width: 16, height: 16)
            NSColor.white.setFill()
            NSBezierPath(ovalIn: sphereRect).fill()
            let xMark = NSBezierPath()
            xMark.lineWidth = 2.15
            xMark.lineCapStyle = .round
            xMark.move(to: NSPoint(x: 7.5, y: 17.0))
            xMark.curve(to: NSPoint(x: 16.5, y: 7.0),
                        controlPoint1: NSPoint(x: 10.0, y: 14.7),
                        controlPoint2: NSPoint(x: 14.3, y: 10.0))
            xMark.move(to: NSPoint(x: 16.5, y: 17.0))
            xMark.curve(to: NSPoint(x: 7.5, y: 7.0),
                        controlPoint1: NSPoint(x: 14.0, y: 14.7),
                        controlPoint2: NSPoint(x: 9.7, y: 10.0))
            green.setStroke()
            xMark.stroke()
        }

        let label = xbox ? "XBOX" : String(platformTarget.uppercased().prefix(8))
        let attributes: [NSAttributedString.Key: Any] = [
            .font: NSFont.systemFont(ofSize: 9, weight: .bold),
            .foregroundColor: NSColor.white,
            .kern: 0.6,
        ]
        let labelRect = NSRect(x: xbox ? 24 : 6, y: 5,
                               width: bounds.width - (xbox ? 27 : 12), height: 13)
        (label as NSString).draw(in: labelRect, withAttributes: attributes)
    }
}

/// One borderless, click-through badge window holding a session's per-prompt
/// sigil — a PromptRock — parked top-right under the title bar of its terminal window. The
/// sigil image is still (shape + strata = which prompt); the badge spins it
/// continuously via a CoreAnimation layer rotation whose speed and direction
/// the controller sets from the session's STATUS — so motion is the status
/// channel. The layer spin is GPU-driven (render-server side, CPU stays idle),
/// so the only recurring cost is the controller's light reposition tick.
///
/// Compositing: the window lives at normal level and is inserted immediately
/// above its terminal. It therefore follows the terminal's z-order instead of
/// painting over unrelated foreground windows.
final class PromptSigilOverlay {
    private static let fuseParticle: CGImage? = {
        let side = 8
        guard let context = CGContext(
            data: nil, width: side, height: side, bitsPerComponent: 8,
            bytesPerRow: side * 4, space: CGColorSpaceCreateDeviceRGB(),
            bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue)
        else { return nil }
        // Deliberately blocky 2 px cluster—no antialiased radial gradient—so
        // sparks and ash share the prompt rocks' crunchy low-resolution hand.
        context.interpolationQuality = .none
        context.setShouldAntialias(false)
        context.setFillColor(NSColor.white.cgColor)
        context.fill(CGRect(x: 2, y: 2, width: 4, height: 4))
        context.fill(CGRect(x: 0, y: 3, width: 2, height: 2))
        context.fill(CGRect(x: 6, y: 4, width: 2, height: 2))
        context.setFillColor(NSColor.white.withAlphaComponent(0.55).cgColor)
        context.fill(CGRect(x: 3, y: 0, width: 2, height: 2))
        return context.makeImage()
    }()

    let sessionId: String
    /// Bare tty name, e.g. `ttys003` — the join key the controller binds to a
    /// CGWindowID.
    let tty: String
    private var terminalFrameCG = CGRect.zero

    /// Key of the sprite sheet currently installed (seed : dark : sun-minute),
    /// so the controller re-renders the frames only when the rock or the sun
    /// actually changes — never on a status change (status is motion + halo).
    var frameKey: String = ""
    private var motion: (period: Double, clockwise: Bool)?
    /// Spring-follow state: `target` is where the terminal wants the badge,
    /// `current` is where it's drawn. `advance` eases current → target each
    /// frame so the badge trails its window with a little inertia (reads as
    /// playful physics rather than tracking lag).
    private var targetOrigin: NSPoint?
    private var currentOrigin: NSPoint?

    let size: CGFloat = 56
    /// Padding around the rock inside the window so the offset drop shadow has
    /// room and isn't clipped by the window edge.
    private let pad: CGFloat = 9
    /// Strip under the rock reserved for its name label.
    private let labelH: CGFloat = 42
    /// Fixed global sun: where the highlight/shadow come from (screen-space,
    /// shared by every stone). Down-right shadow ⇒ light from the upper-left.
    private let shadowDrop = CGSize(width: 3, height: -3)
    private let window: NSWindow
    /// Full-width Loopboy cadence strip, parked just below Terminal's title
    /// bar like an Instagram story timer. Separate from the gem window so it
    /// can span the pane without changing the prox's hit or animation bounds.
    private let heartbeatWindow: NSWindow
    /// Independent bottom-left prompt-terminal acknowledgement. Keeping this
    /// separate from the top-right rock window lets it follow the terminal's
    /// opposite corner while sharing the same tty binding and z-order rules.
    private let platformTargetBadgeWindow: NSWindow
    private let platformTargetBadgeView: PlatformTargetAwarenessIdentifierBadgeView
    private var platformTargetBadgeOrderedAbove: Int?
    private var platformTarget = ""
    private let rockLayer = CALayer()        // plays the pre-rendered rotation frames
    private let shadowLayer = CALayer()      // solid status colour, masked to the rock silhouette
    private let shadowMask = CALayer()       // plays the same frames → the shadow's tumbling shape
    private let nameLayer = CALayer()        // the rock's pet name, under the rock (pixel-text bitmap)
    private let heartbeatTrackLayer = CALayer()
    private let heartbeatFillLayer = CALayer()
    private let heartbeatFuseEmitter = CAEmitterLayer()
    private var heartbeatOrderedAbove: Int?
    private var heartbeatDeadline: Date?
    private var heartbeatInterval: TimeInterval = 60
    private var heartbeatColor = NSColor.systemYellow
    private var heartbeatUrgencyStage = 0
    private let stateLayer = CATextLayer()   // Loopboy phase, beneath its pet name
    private var loopboyState = ""
    private var boxCenter = CGPoint.zero

    /// The rock's pet name (deterministic from its session/thread id) and the
    /// hover copy. The name stays fixed while the visual seed and copy evolve
    /// with the session's current subject.
    private(set) var name: String = ""
    var tooltipTitle: String = ""
    var tooltipBody: String = ""
    var tooltipEdition: String = "PROMPT ROCK  •  LIVING MEMORY"
    var tooltipStatus: String = ""
    var tooltipStats: String = ""
    var tooltipAccent: NSColor { shadowColor ?? .systemGray }
    var tooltipImage: NSImage? {
        // The terminal badge deliberately plays a chunky 30px sprite. The
        // collectible card gets the original high-resolution render instead:
        // same rock, but crisp enough to read as the card's illustration.
        shadowFrames.first.map { NSImage(cgImage: $0, size: NSSize(width: 128, height: 128)) }
    }
    /// Two pre-rendered sprite sheets of one full turn: `rockFrames` is the
    /// chunky low-res copy the rock plays; `shadowFrames` is the crisp high-res
    /// silhouette the shadow plays.
    private var rockFrames: [CGImage] = []
    private var shadowFrames: [CGImage] = []

    init(sessionId: String, tty: String) {
        self.sessionId = sessionId
        self.tty = tty

        let box = size + 2 * pad
        let initial = NSRect(x: -2000, y: -2000, width: box, height: box + labelH)
        window = NSWindow(contentRect: initial, styleMask: [.borderless],
                          backing: .buffered, defer: false)
        window.isOpaque = false
        window.backgroundColor = .clear
        window.hasShadow = false
        window.ignoresMouseEvents = true
        // Stay in the normal-window stack. `orderAbove` places this rock just
        // above its terminal, leaving every window above that terminal above
        // the rock as well.
        window.level = .normal
        window.collectionBehavior = [.fullScreenAuxiliary]

        let heartbeatInitial = NSRect(x: -2000, y: -2000, width: 160, height: 40)
        heartbeatWindow = NSWindow(contentRect: heartbeatInitial, styleMask: [.borderless],
                                   backing: .buffered, defer: false)
        heartbeatWindow.isOpaque = false
        heartbeatWindow.backgroundColor = .clear
        heartbeatWindow.hasShadow = false
        heartbeatWindow.ignoresMouseEvents = true
        // Keep the fuse in the normal window stack. It is ordered immediately
        // above its bound Terminal below, so unrelated windows naturally
        // occlude it instead of the strip painting over them.
        heartbeatWindow.level = .normal
        heartbeatWindow.collectionBehavior = [.fullScreenAuxiliary]
        let heartbeatContainer = NSView(frame: NSRect(origin: .zero, size: heartbeatInitial.size))
        heartbeatContainer.wantsLayer = true
        heartbeatContainer.layer?.masksToBounds = false
        heartbeatWindow.contentView = heartbeatContainer

        let targetBadgeSize = NSSize(width: 60, height: 24)
        let targetBadgeInitial = NSRect(origin: NSPoint(x: -2000, y: -2000),
                                        size: targetBadgeSize)
        platformTargetBadgeWindow = NSWindow(
            contentRect: targetBadgeInitial, styleMask: [.borderless],
            backing: .buffered, defer: false)
        platformTargetBadgeWindow.isOpaque = false
        platformTargetBadgeWindow.backgroundColor = .clear
        platformTargetBadgeWindow.hasShadow = true
        platformTargetBadgeWindow.ignoresMouseEvents = true
        platformTargetBadgeWindow.level = .normal
        platformTargetBadgeWindow.collectionBehavior = [.fullScreenAuxiliary]
        platformTargetBadgeView = PlatformTargetAwarenessIdentifierBadgeView(
            frame: NSRect(origin: .zero, size: targetBadgeSize))
        platformTargetBadgeView.wantsLayer = true
        platformTargetBadgeWindow.contentView = platformTargetBadgeView

        // Container: a flat status-colour drop-shadow disc as a backing
        // sublayer, with the rock-frame layer on top — the disc peeks out on
        // the sun-opposite side as a hard drop shadow.
        let container = NSView(frame: NSRect(origin: .zero, size: initial.size))
        container.wantsLayer = true
        container.layer?.masksToBounds = false
        // The rock box sits above the label strip (label at the window's
        // bottom, rock centred in the remaining square).
        boxCenter = CGPoint(x: box / 2, y: labelH + box / 2)

        // Shadow: a solid status-colour block masked to the rock's silhouette
        // (the same tumbling frames), offset by the sun. So it's the rock's
        // actual shape turning — not a circle. Position offset applied in
        // setLighting.
        shadowLayer.frame = CGRect(x: pad, y: pad + labelH, width: size, height: size)
        shadowLayer.position = boxCenter
        shadowLayer.backgroundColor = NSColor.systemGray.cgColor
        shadowLayer.opacity = 0.9
        shadowMask.frame = CGRect(x: 0, y: 0, width: size, height: size)
        shadowMask.contentsGravity = .resizeAspect
        // Crisp silhouette: the shadow plays the high-res frames, smoothed.
        shadowMask.magnificationFilter = .linear
        shadowMask.minificationFilter = .linear
        shadowMask.contentsScale = 2
        shadowLayer.mask = shadowMask
        container.layer?.addSublayer(shadowLayer)

        rockLayer.frame = CGRect(x: pad, y: pad + labelH, width: size, height: size)
        rockLayer.contentsGravity = .resizeAspect
        // Low-res frames scaled up with nearest-neighbour → chunky low-poly
        // pixels, and cheap.
        rockLayer.magnificationFilter = .nearest
        rockLayer.minificationFilter = .nearest
        rockLayer.contentsScale = 1
        container.layer?.addSublayer(rockLayer)

        // Transparent space above/below lets smoke rise and ash fall without
        // the heartbeat window clipping either stream.
        heartbeatTrackLayer.frame = CGRect(x: 0, y: 24, width: heartbeatInitial.width, height: 3)
        heartbeatTrackLayer.backgroundColor = NSColor(deviceWhite: 0.025, alpha: 0.38).cgColor
        heartbeatTrackLayer.cornerRadius = 0
        heartbeatTrackLayer.shadowColor = NSColor.systemYellow.cgColor
        heartbeatTrackLayer.shadowOpacity = 0.42
        heartbeatTrackLayer.shadowRadius = 3
        heartbeatTrackLayer.shadowOffset = CGSize(width: 0, height: -1)
        heartbeatTrackLayer.isHidden = true
        heartbeatContainer.layer?.addSublayer(heartbeatTrackLayer)
        heartbeatFillLayer.frame = heartbeatTrackLayer.bounds
        // The remaining fuse is pinned to the right. Scaling it 1 → 0 moves
        // its burning left edge from left → right until nothing remains.
        heartbeatFillLayer.anchorPoint = CGPoint(x: 1, y: 0.5)
        heartbeatFillLayer.position = CGPoint(x: heartbeatTrackLayer.bounds.maxX,
                                              y: heartbeatTrackLayer.bounds.midY)
        heartbeatFillLayer.backgroundColor = NSColor(deviceRed: 1, green: 0.9,
                                                     blue: 0.18, alpha: 1).cgColor
        heartbeatFillLayer.shadowColor = NSColor(deviceRed: 1, green: 0.32,
                                                 blue: 0.04, alpha: 1).cgColor
        heartbeatFillLayer.shadowOpacity = 1
        heartbeatFillLayer.shadowRadius = 6
        heartbeatFillLayer.cornerRadius = 0
        heartbeatTrackLayer.addSublayer(heartbeatFillLayer)

        heartbeatFuseEmitter.emitterShape = .point
        heartbeatFuseEmitter.emitterMode = .points
        heartbeatFuseEmitter.renderMode = .additive
        heartbeatFuseEmitter.emitterPosition = CGPoint(x: 0, y: heartbeatTrackLayer.bounds.midY)
        let spark = CAEmitterCell()
        spark.name = "spark"
        spark.contents = Self.fuseParticle
        spark.color = NSColor(deviceRed: 1, green: 0.25, blue: 0.03, alpha: 1).cgColor
        spark.birthRate = 11
        spark.lifetime = 0.75
        spark.lifetimeRange = 0.28
        spark.velocity = 17
        spark.velocityRange = 9
        spark.emissionLongitude = -.pi / 2
        spark.emissionRange = .pi / 5
        spark.scale = 0.72
        spark.scaleRange = 0.22
        spark.scaleSpeed = -0.38
        spark.alphaSpeed = -1.1
        let ash = CAEmitterCell()
        ash.name = "ash"
        ash.contents = Self.fuseParticle
        ash.color = NSColor(deviceWhite: 0.25, alpha: 0.82).cgColor
        ash.birthRate = 9
        ash.lifetime = 1.25
        ash.lifetimeRange = 0.35
        ash.velocity = 18
        ash.velocityRange = 9
        ash.emissionLongitude = -.pi / 2
        ash.emissionRange = .pi / 7
        ash.scale = 0.52
        ash.scaleRange = 0.20
        ash.scaleSpeed = -0.22
        ash.alphaSpeed = -0.72
        let smoke = CAEmitterCell()
        smoke.name = "smoke"
        smoke.contents = Self.fuseParticle
        smoke.color = NSColor(deviceWhite: 0.72, alpha: 0.34).cgColor
        smoke.birthRate = 2.4
        smoke.lifetime = 1.25
        smoke.lifetimeRange = 0.4
        smoke.velocity = 10
        smoke.velocityRange = 5
        smoke.emissionLongitude = .pi / 2
        smoke.emissionRange = .pi / 6
        smoke.scale = 0.82
        smoke.scaleRange = 0.26
        smoke.scaleSpeed = 0.12
        smoke.alphaSpeed = -0.55
        // A dense, very short-lived flame cell forms the hot tip organically.
        // It flickers and changes silhouette frame-to-frame instead of reading
        // as a perfect circular progress scrubber.
        let flame = CAEmitterCell()
        flame.name = "flame"
        flame.contents = Self.fuseParticle
        flame.color = NSColor(deviceRed: 1, green: 0.18, blue: 0.015, alpha: 1).cgColor
        flame.birthRate = 28
        flame.lifetime = 0.20
        flame.lifetimeRange = 0.09
        flame.velocity = 7
        flame.velocityRange = 5
        flame.emissionLongitude = .pi / 2
        flame.emissionRange = .pi / 2.5
        flame.scale = 0.92
        flame.scaleRange = 0.34
        flame.scaleSpeed = -2.4
        flame.alphaSpeed = -2.8
        heartbeatFuseEmitter.emitterCells = [flame, spark, ash, smoke]
        heartbeatTrackLayer.addSublayer(heartbeatFuseEmitter)

        // Name label: the rock's pet name in MacPal bubble lettering, tucked
        // right under the rock so rocks are tellable apart by word as well as
        // by shape. It's a container for per-character RockCharLayers
        // (rebuilt by `rebuildName`) and overlaps the rock box's bottom
        // margin — the sprite never reaches its own edge, so the letters sit
        // close to the stone without touching it.
        nameLayer.frame = CGRect(x: 0, y: 26, width: box, height: 18)
        nameLayer.masksToBounds = false
        container.layer?.addSublayer(nameLayer)

        stateLayer.frame = CGRect(x: 0, y: 8, width: box, height: 18)
        stateLayer.alignmentMode = .center
        stateLayer.contentsScale = NSScreen.main?.backingScaleFactor ?? 2
        stateLayer.isHidden = true
        container.layer?.addSublayer(stateLayer)

        window.contentView = container
    }

    /// Set the rock's name label. `dark` is unused now — MacPal's white-fill
    /// + dark-outline bubble letters read over any background.
    func setName(_ newName: String, dark: Bool) {
        guard name != newName else { return }
        name = newName
        rebuildName()
    }

    /// Update the platform-target-awareness-identifier-badge from durable
    /// session metadata. Normalization keeps marker writers case-insensitive;
    /// clearing the marker removes the acknowledgement immediately.
    func setPlatformTarget(_ rawTarget: String) {
        let normalized = rawTarget.trimmingCharacters(in: .whitespacesAndNewlines)
            .lowercased()
        guard platformTarget != normalized else { return }
        platformTarget = normalized
        platformTargetBadgeView.platformTarget = normalized
        if normalized.isEmpty {
            platformTargetBadgeWindow.orderOut(nil)
            platformTargetBadgeOrderedAbove = nil
        }
    }

    private var labelForeground = NSColor.white
    private var loopboyStyled = false

    /// Give bound client loops a separate motion axis and role-colored type.
    func setLoopboyStyle(_ enabled: Bool, active: Bool, pending: Bool, dark: Bool) {
        let color: NSColor = pending
            ? NSColor(deviceRed: 1.0, green: 0.58, blue: 0.82, alpha: 1.0)
            : NSColor(deviceRed: 1.0, green: 0.94, blue: 0.28, alpha: 1.0)
        let changed = loopboyStyled != enabled || labelForeground != color
        loopboyStyled = enabled
        stateLayer.isHidden = !enabled
        heartbeatTrackLayer.isHidden = !enabled
        if !enabled { heartbeatWindow.orderOut(nil) }
        labelForeground = enabled ? color : .white
        if enabled {
            rockLayer.removeAnimation(forKey: "loopboyYAxis")
            shadowMask.removeAnimation(forKey: "loopboyYAxis")
        } else {
            rockLayer.removeAnimation(forKey: "loopboyYAxis")
            shadowMask.removeAnimation(forKey: "loopboyYAxis")
            rockLayer.transform = CATransform3DIdentity
            shadowMask.transform = CATransform3DIdentity
        }
        if changed { rebuildName() }
    }

    func resetHeartbeatCountdown(interval: TimeInterval = 60) {
        heartbeatInterval = max(0.1, interval)
        heartbeatDeadline = Date().addingTimeInterval(interval)
        heartbeatUrgencyStage = 0
        retime(rockLayer, speed: observed ? 3.2 : (hovered ? 2.6 : 1.0))
        retime(shadowMask, speed: observed ? 3.2 : (hovered ? 2.6 : 1.0))
        heartbeatFillLayer.removeAnimation(forKey: "heartbeatDrain")
        heartbeatFillLayer.removeAnimation(forKey: "heartbeatUrgency")
        heartbeatFuseEmitter.removeAnimation(forKey: "heartbeatFuseTravel")
        heartbeatFuseEmitter.birthRate = 1
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        heartbeatFillLayer.opacity = 1
        heartbeatFillLayer.transform = CATransform3DIdentity
        heartbeatFillLayer.backgroundColor = heartbeatColor.cgColor
        heartbeatFillLayer.shadowColor = heartbeatColor.cgColor
        CATransaction.commit()
        // Fill edge and ember are laid out from one clock in
        // updateHeartbeatCountdown. Keeping no independent x animations means
        // a live terminal resize cannot make their endpoints diverge.

        // Urgency is carried by the gem's motion. The fuse stays continuously
        // opaque; flashing a progress indicator reads as rendering instability.
    }

    func updateHeartbeatCountdown(now: Date) {
        guard loopboyStyled, let deadline = heartbeatDeadline else { return }
        let remaining = deadline.timeIntervalSince(now)
        let progress = max(0, min(1, 1 - remaining / heartbeatInterval))
        let trackWidth = heartbeatTrackLayer.bounds.width
        let pixelScale = max(1, heartbeatWindow.backingScaleFactor)
        let burnX = (trackWidth * progress * pixelScale).rounded() / pixelScale
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        heartbeatFillLayer.opacity = 1
        heartbeatFillLayer.transform = CATransform3DIdentity
        heartbeatFillLayer.bounds = CGRect(
            x: 0, y: 0,
            width: max(0, trackWidth - burnX),
            height: heartbeatTrackLayer.bounds.height)
        heartbeatFillLayer.position = CGPoint(
            x: trackWidth,
            y: heartbeatTrackLayer.bounds.midY)
        heartbeatFuseEmitter.emitterPosition = CGPoint(
            x: burnX,
            y: heartbeatTrackLayer.bounds.midY)
        CATransaction.commit()
        let urgencyStage = remaining <= 5 ? 2 : (remaining <= 12 ? 1 : 0)
        if urgencyStage != heartbeatUrgencyStage {
            heartbeatUrgencyStage = urgencyStage
            let speed: Float = urgencyStage == 2 ? 3.4 : (urgencyStage == 1 ? 1.9 : 1.0)
            retime(rockLayer, speed: speed)
            retime(shadowMask, speed: speed)
        }
        guard now >= deadline else { return }
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        heartbeatFillLayer.bounds.size.width = 0
        CATransaction.commit()
        heartbeatFuseEmitter.birthRate = 0
    }

    func setLoopboyState(_ state: String, animated: Bool = true) {
        stateLayer.isHidden = !loopboyStyled
        guard loopboyState != state else { return }
        loopboyState = state
        guard animated, stateLayer.string != nil else {
            applyStateLabel(state)
            return
        }
        let shake = CAKeyframeAnimation(keyPath: "transform.translation.x")
        shake.values = [0, -4, 5, -3, 2, 0]
        shake.duration = 0.18
        let breakApart = CABasicAnimation(keyPath: "transform.scale")
        breakApart.fromValue = 1.0; breakApart.toValue = 1.7
        breakApart.duration = 0.18
        let fade = CABasicAnimation(keyPath: "opacity")
        fade.fromValue = 1.0; fade.toValue = 0.0; fade.duration = 0.18
        stateLayer.add(shake, forKey: "stateShake")
        stateLayer.add(breakApart, forKey: "stateBreak")
        stateLayer.add(fade, forKey: "stateFade")
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.18) { [weak self] in
            guard let self = self, self.loopboyState == state else { return }
            self.applyStateLabel(state)
            let assemble = CAKeyframeAnimation(keyPath: "transform.scale")
            assemble.values = [0.35, 1.18, 1.0]
            assemble.keyTimes = [0, 0.72, 1]
            assemble.duration = 0.28
            self.stateLayer.add(assemble, forKey: "stateAssemble")
        }
    }

    private func applyStateLabel(_ state: String) {
        let color: NSColor
        switch state {
        case "READING": color = NSColor(deviceRed: 0.28, green: 0.92, blue: 1.0, alpha: 1)
        case "WORKING": color = NSColor(deviceRed: 1.0, green: 0.92, blue: 0.22, alpha: 1)
        case "RESPONDING": color = NSColor(deviceRed: 1.0, green: 0.42, blue: 0.72, alpha: 1)
        default: color = NSColor(deviceWhite: 0.72, alpha: 1)
        }
        stateLayer.string = NSAttributedString(string: state.lowercased(), attributes: [
            .font: playfulRockFont(12),
            .foregroundColor: color,
            .strokeColor: NSColor(white: 0.08, alpha: 0.9),
            .strokeWidth: -1.4,
        ])
    }

    /// Retime a layer's animation clock without a visual jump: local time is
    /// held continuous across the speed change, so a spinning rock or
    /// wiggling letter just accelerates rather than snapping to a new phase.
    private func retime(_ layer: CALayer, speed: Float) {
        guard layer.speed != speed else { return }
        let now = CACurrentMediaTime()
        let cur = layer.convertTime(now, from: nil)
        layer.speed = speed
        layer.timeOffset = cur
        layer.beginTime = now
    }

    /// Spring a layer's scale to `to`, starting from wherever it visually is.
    private func springScale(_ layer: CALayer, to: CGFloat) {
        let spring = CASpringAnimation(keyPath: "transform.scale")
        spring.damping = 12; spring.stiffness = 250; spring.mass = 0.8
        spring.fromValue = layer.presentation()?.value(forKeyPath: "transform.scale") ?? 1.0
        spring.toValue = to
        spring.duration = spring.settlingDuration
        layer.add(spring, forKey: "hoverScale")
        layer.setValue(to, forKeyPath: "transform.scale")
    }

    /// Pointing at the rock wakes it up: the name swells and wiggles faster,
    /// the stone scales up a touch and tumbles quicker, and its flat shadow
    /// drifts further out and fades a little — as if the rock lifts toward
    /// the pointer. Leaving settles everything back.
    private var hovered = false
    func setHovered(_ h: Bool) {
        guard hovered != h else { return }
        hovered = h
        springScale(nameLayer, to: h ? 1.5 : 1.0)
        springScale(rockLayer, to: h ? 1.12 : 1.0)
        springScale(shadowLayer, to: h ? 1.12 : 1.0)
        retime(nameLayer, speed: h ? 2.2 : 1.0)
        retime(rockLayer, speed: h ? 2.6 : 1.0)
        retime(shadowMask, speed: h ? 2.6 : 1.0)
        let drop = appliedDrop ?? shadowDrop
        let k: CGFloat = h ? 1.9 : 1.0
        shadowLayer.position = CGPoint(x: boxCenter.x + drop.width * k,
                                       y: boxCenter.y + drop.height * k)
        shadowLayer.opacity = h ? 0.72 : 0.9
    }

    /// "Someone is reading this prompt right now." Layered ON TOP of the
    /// status motion (which stays the baseline): the stone blinks (opacity
    /// pulse), rattles (a tight position shake), and tumbles much faster —
    /// a visceral tell that a peer or agent just resolved this handle. The
    /// sigil identity itself never changes; only motion + opacity react. The
    /// controller flips this off when the observe window decays, restoring the
    /// status (or hover) motion.
    private var observed = false
    /// Returns true when the observed state actually flipped (so the controller
    /// can log the reaction exactly once per poke burst).
    @discardableResult
    func setObserved(_ active: Bool) -> Bool {
        guard observed != active else { return false }
        observed = active
        if active {
            let blink = CABasicAnimation(keyPath: "opacity")
            blink.fromValue = 1.0; blink.toValue = 0.3
            blink.duration = 0.22; blink.autoreverses = true
            blink.repeatCount = .infinity; blink.isRemovedOnCompletion = false
            rockLayer.add(blink, forKey: "observedBlink")
            nameLayer.add(blink, forKey: "observedBlink")

            let c = rockLayer.position
            let shake = CAKeyframeAnimation(keyPath: "position")
            shake.values = [c, CGPoint(x: c.x - 2.4, y: c.y + 1.4),
                            CGPoint(x: c.x + 2.2, y: c.y - 1.2),
                            CGPoint(x: c.x - 1.6, y: c.y - 1.8), c].map { NSValue(point: $0) }
            shake.duration = 0.13; shake.repeatCount = .infinity
            shake.isRemovedOnCompletion = false
            rockLayer.add(shake, forKey: "observedShake")

            retime(rockLayer, speed: 3.2)
            retime(shadowMask, speed: 3.2)
        } else {
            rockLayer.removeAnimation(forKey: "observedBlink")
            nameLayer.removeAnimation(forKey: "observedBlink")
            rockLayer.removeAnimation(forKey: "observedShake")
            rockLayer.opacity = 1.0
            nameLayer.opacity = 1.0
            retime(rockLayer, speed: hovered ? 2.6 : 1.0)
            retime(shadowMask, speed: hovered ? 2.6 : 1.0)
        }
        return true
    }

    /// Rebuild the name as MacPal-style bubble letters: one RockCharLayer per
    /// character in Comic Sans — white fill, dark outline, a hard offset
    /// shadow in the session's status colour — each with a deterministic
    /// static jitter (baseline nudge + tilt) and a slow staggered wiggle, so
    /// the name is alive the way the pals' names are. Rebuilt on name or
    /// status-colour change; a handful of tiny layers, and the wiggle plays
    /// render-server-side like the tumble.
    private func rebuildName() {
        nameLayer.sublayers?.forEach { $0.removeFromSuperlayer() }
        guard !name.isEmpty else { return }
        let font = playfulRockFont(16)
        let sh = NSShadow()
        sh.shadowColor = shadowColor ?? NSColor.systemGray
        sh.shadowBlurRadius = 0
        sh.shadowOffset = NSSize(width: 1.5, height: -1.5)
        let scale = NSScreen.main?.backingScaleFactor ?? 2
        let inset: CGFloat = 8
        var layers: [RockCharLayer] = []
        var widths: [CGFloat] = []
        var total: CGFloat = 0
        for ch in name {
            let a = NSAttributedString(string: String(ch), attributes: [
                .font: font,
                .foregroundColor: labelForeground,
                .strokeColor: NSColor(white: 0.08, alpha: 1),
                .strokeWidth: -3.5,
                .shadow: sh,
            ])
            let chSize = a.size()
            let l = RockCharLayer()
            l.attr = a
            l.inset = inset
            l.contentsScale = scale
            l.bounds = CGRect(x: 0, y: 0,
                              width: chSize.width + inset * 2,
                              height: chSize.height + inset * 2)
            l.anchorPoint = CGPoint(x: 0.5, y: 0.5)
            // Deterministic static jitter, hashed from position + name so a
            // rock's lettering is stable across rebuilds.
            var h: UInt32 = 2166136261
            for b in "rock\(layers.count)\(name)".utf8 { h = (h ^ UInt32(b)) &* 16777619 }
            let dy = CGFloat(Int(h % 5)) / 2 - 1
            let rot = (CGFloat(Int((h >> 8) % 9)) - 4) * 0.9 * .pi / 180
            var t = CATransform3DMakeTranslation(0, dy, 0)
            t = CATransform3DRotate(t, rot, 0, 0, 1)
            l.transform = t
            nameLayer.addSublayer(l)
            layers.append(l)
            widths.append(chSize.width)
            total += chSize.width
            l.setNeedsDisplay()
        }
        func eases(_ n: Int) -> [CAMediaTimingFunction] {
            Array(repeating: CAMediaTimingFunction(name: .easeInEaseOut), count: n)
        }
        var x = (nameLayer.bounds.width - total) / 2
        let t0 = CACurrentMediaTime()
        for (i, l) in layers.enumerated() {
            l.position = CGPoint(x: x + widths[i] / 2, y: nameLayer.bounds.height / 2)
            x += widths[i]
            let begin = t0 + Double(i) * 0.12
            let sway = CAKeyframeAnimation(keyPath: loopboyStyled
                ? "transform.translation.x" : "transform.translation.y")
            sway.values = loopboyStyled ? [0, 2.4, -2.0, 0] : [0, 1.2, -0.8, 0]
            sway.keyTimes = [0, 0.25, 0.75, 1]
            sway.timingFunctions = eases(3)
            sway.duration = loopboyStyled ? 1.45 : 1.8
            sway.repeatCount = .infinity
            sway.beginTime = begin; sway.isAdditive = true
            l.add(sway, forKey: loopboyStyled ? "loopboySwayX" : "wiggleY")
            let r = CAKeyframeAnimation(keyPath: "transform.rotation.z")
            r.values = [0, 1.2 * Double.pi / 180, -0.8 * Double.pi / 180, 0]
            r.keyTimes = [0, 0.25, 0.75, 1]
            r.timingFunctions = eases(3)
            r.duration = 1.8; r.repeatCount = .infinity
            r.beginTime = begin; r.isAdditive = true
            l.add(r, forKey: "wiggleR")
        }
    }

    /// Screen-space rect covering the rock + its name label, for the
    /// controller's hover/click hit-tests (the badge window itself stays
    /// mouse-transparent; a global monitor does the pointing).
    var hitRect: NSRect {
        let f = window.frame
        return NSRect(x: f.origin.x + pad, y: f.origin.y,
                      width: size, height: labelH + pad + size)
    }

    /// Install freshly rendered sprite sheets (chunky rock + crisp shadow) and
    /// (re)start playback.
    func setFrames(rock: [CGImage], shadow: [CGImage]) {
        rockFrames = rock
        shadowFrames = shadow
        rockLayer.contents = rock.first
        shadowMask.contents = shadow.first
        positionLabels(for: rock)
        applyPlayback()
    }

    /// Place labels against the union alpha envelope of the animated form.
    /// Sampling the full turn protects asymmetric gems from clipping during
    /// rotation while eliminating the nominal-layer dead gap.
    private func positionLabels(for frames: [CGImage]) {
        guard let first = frames.first else { return }
        var lowestRow = -1
        let step = max(1, frames.count / 18)
        for image in frames.enumerated() where image.offset % step == 0 {
            let cg = image.element
            guard cg.bitsPerPixel == 32, let data = cg.dataProvider?.data,
                  let bytes = CFDataGetBytePtr(data) else { continue }
            let alphaOffset: Int
            switch cg.alphaInfo {
            case .first, .premultipliedFirst, .noneSkipFirst: alphaOffset = 0
            default: alphaOffset = 3
            }
            for row in 0..<cg.height {
                let base = row * cg.bytesPerRow
                var occupied = false
                for col in 0..<cg.width where bytes[base + col * 4 + alphaOffset] > 10 {
                    occupied = true; break
                }
                if occupied { lowestRow = max(lowestRow, row) }
            }
        }
        guard lowestRow >= 0 else { return }
        let bottomInset = CGFloat(first.height - 1 - lowestRow) / CGFloat(first.height) * size
        let visibleBottom = rockLayer.frame.minY + bottomInset
        let nameY = max(20, visibleBottom - 16)
        nameLayer.frame.origin.y = nameY
        stateLayer.frame.origin.y = max(1, nameY - 18)
    }

    /// Tint the flat drop-shadow disc with the session's status colour.
    private var shadowColor: NSColor?
    private var promptColor: NSColor?
    func setShadowColor(_ color: NSColor) {
        guard shadowColor != color else { return }
        shadowColor = color
        shadowLayer.backgroundColor = color.cgColor
        heartbeatTrackLayer.backgroundColor = NSColor(deviceWhite: 0.025, alpha: 0.38).cgColor
        heartbeatTrackLayer.shadowColor = NSColor.black.cgColor
        // The lettering's hard shadow wears the same status colour, so the
        // label reads as part of the same lit object as the rock.
        rebuildName()
    }

    /// The flying keystrokes wear the terminal's actual foreground colour,
    /// which is not necessarily the rock's status/shadow colour.
    func setPromptColor(_ color: NSColor) {
        promptColor = color
    }

    /// The countdown belongs to the terminal it points at. Keep this separate
    /// from the gem/status shadow so a selected Terminal profile's accent is
    /// faithfully carried by the fuse and every falling spark.
    func setHeartbeatColor(_ color: NSColor) {
        heartbeatColor = color
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        heartbeatFillLayer.backgroundColor = color.cgColor
        heartbeatFillLayer.shadowColor = color.cgColor
        CATransaction.commit()
    }

    /// Loopboy rocks are beacons, not merely status shadows. Give their
    /// silhouette a soft outer bloom; ordinary prompt rocks stay crisp.
    func setShining(_ shining: Bool, color: NSColor) {
        shadowLayer.shadowColor = shining ? color.cgColor : nil
        shadowLayer.shadowRadius = shining ? 16 : 0
        shadowLayer.shadowOpacity = shining ? 1.0 : 0
        shadowLayer.shadowOffset = .zero
    }

    /// One fleet-synchronized Loopboy heartbeat beat: flash, blink, and a
    /// short spin burst. The controller gives every gem the same begin time.
    func heartbeatPulse(beginTime: CFTimeInterval) {
        setLoopboyState("READING")
        // The fuse reaches the gem's next beat, then hands its energy into the
        // shared blink/spin/explosion rather than ending as a disconnected bar.
        heartbeatFuseEmitter.birthRate = 2.6
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.18) { [weak self] in
            self?.heartbeatFuseEmitter.birthRate = 1
        }
        let blink = CAKeyframeAnimation(keyPath: "opacity")
        blink.values = [1.0, 0.28, 1.0, 0.42, 1.0]
        blink.keyTimes = [0, 0.18, 0.42, 0.66, 1]
        blink.duration = 1.15
        blink.beginTime = beginTime
        blink.isRemovedOnCompletion = true
        rockLayer.add(blink, forKey: "loopboyHeartbeatBlink")
        nameLayer.add(blink, forKey: "loopboyHeartbeatBlink")

        let color = CAKeyframeAnimation(keyPath: "backgroundColor")
        color.values = [
            (shadowColor ?? .systemYellow).cgColor,
            NSColor(deviceRed: 1.0, green: 0.22, blue: 0.62, alpha: 1).cgColor,
            NSColor(deviceRed: 1.0, green: 0.94, blue: 0.22, alpha: 1).cgColor,
            (shadowColor ?? .systemYellow).cgColor,
        ]
        color.keyTimes = [0, 0.28, 0.62, 1]
        color.duration = 1.15
        color.beginTime = beginTime
        shadowLayer.add(color, forKey: "loopboyHeartbeatColor")

        retime(rockLayer, speed: 4.2)
        retime(shadowMask, speed: 4.2)
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.2) { [weak self] in
            guard let self = self else { return }
            self.retime(self.rockLayer, speed: self.observed ? 3.2 : (self.hovered ? 2.6 : 1.0))
            self.retime(self.shadowMask, speed: self.observed ? 3.2 : (self.hovered ? 2.6 : 1.0))
        }
    }

    var heartbeatTarget: (CGRect, NSScreen)? {
        guard !terminalFrameCG.isEmpty, let screen = window.screen else { return nil }
        return (terminalFrameCG, screen)
    }

    /// Screen-space endpoints for the heartbeat's visible prompt handoff:
    /// gem upper-right → the terminal's bottom input line.
    var promptFlightTarget: (CGPoint, CGPoint, NSColor, NSScreen, CGFloat)? {
        let terminal = terminalFrameCG
        guard !terminal.isEmpty,
              let screen = NSScreen.screens.first(where: {
                  terminal.intersects($0.frame)
              }) ?? NSScreen.main else { return nil }
        let desktopTop = NSScreen.screens.map(\.frame.maxY).max() ?? 0
        // Emit from the visible pet name rather than the rock's corner: the
        // sentence reads as letters jumbling out of the name itself.
        let source = CGPoint(x: window.frame.minX + nameLayer.frame.midX,
                             y: window.frame.minY + nameLayer.frame.midY)
        let terminalBottom = desktopTop - terminal.maxY
        let destination = CGPoint(x: terminal.minX + min(150, terminal.width * 0.18),
                                  y: terminalBottom + 45)
        return (source, destination,
                promptColor ?? shadowColor ?? .systemYellow, screen,
                terminal.maxX)
    }

    /// The global sun's direction is baked into the frames (re-rendered by the
    /// controller when it moves); here we only slide the flat shadow to the
    /// sun-opposite `drop`.
    private var appliedDrop: CGSize?
    func setLighting(drop: CGSize) {
        if appliedDrop != drop {
            appliedDrop = drop
            shadowLayer.position = CGPoint(x: boxCenter.x + drop.width,
                                           y: boxCenter.y + drop.height)
        }
    }

    /// Status-driven tumble. Cheap: a discrete keyframe animation cycling the
    /// cached frames — direction picks the frame order, period sets the turn
    /// time. Playback is server-side, so a slow tumble barely touches the CPU.
    func setMotion(period: Double, clockwise: Bool) {
        if let m = motion, m.period == period, m.clockwise == clockwise { return }
        motion = (period, clockwise)
        applyPlayback()
    }

    private func applyPlayback() {
        guard rockFrames.count > 1, let (period, cw) = motion else {
            rockLayer.removeAnimation(forKey: "tumble")
            shadowMask.removeAnimation(forKey: "tumble")
            return
        }
        func tumble(_ vals: [CGImage]) -> CAKeyframeAnimation {
            let a = CAKeyframeAnimation(keyPath: "contents")
            a.values = cw ? vals : vals.reversed()
            a.calculationMode = .discrete
            a.duration = period
            a.repeatCount = .infinity
            a.isRemovedOnCompletion = false
            return a
        }
        // Rock + shadow play their own sheets at the same timing → in sync.
        rockLayer.add(tumble(rockFrames), forKey: "tumble")
        shadowMask.add(tumble(shadowFrames), forKey: "tumble")
    }

    /// This badge's CGWindowID (valid once it's been ordered on-screen), so the
    /// controller can find it in the global stacking order.
    var windowNumber: Int { window.windowNumber }

    /// Aim the badge at the top-right of its terminal window (`b` = on-screen
    /// bounds {x,y,w,h}, top-left origin), dropped below the title bar. Sets the
    /// spring TARGET only; `advance` does the actual easing. First placement
    /// snaps (no spring from off-screen). No z-order touched here.
    func place(bounds b: (CGFloat, CGFloat, CGFloat, CGFloat), screenHeight: CGFloat) {
        terminalFrameCG = CGRect(x: b.0, y: b.1, width: b.2, height: b.3)
        let titleBar: CGFloat = 30, rightInset: CGFloat = 10
        // Window is padded around the rock; shift origin by -pad so the rock
        // itself (centred in the window) still lands at the top-right spot.
        // The label strip hangs below the rock, so drop by labelH too.
        let originX = b.0 + b.2 - rightInset - size - pad
        let originY = screenHeight - (b.1 + titleBar + size + pad) - labelH
        let t = NSPoint(x: originX, y: originY)
        targetOrigin = t
        if currentOrigin == nil {                 // first appearance: snap there
            currentOrigin = t
            window.setFrameOrigin(t)
        }

        // A slim inset keeps the story-style strip aligned to the pane's
        // content edge rather than colliding with the rounded window corners.
        // Terminal's text canvas begins a few pixels inside the outer window;
        // match that inner edge so the bar and prompt share one baseline.
        let stripInset: CGFloat = 2
        let stripWidth = max(40, b.2 - stripInset * 2)
        let stripTitleBar: CGFloat = 28
        let stripY = screenHeight - (b.1 + stripTitleBar + 5)
        // Preserve the original three-point fuse position while extending a
        // transparent particle field 23 pt below and 12 pt above it.
        heartbeatWindow.setFrame(NSRect(x: b.0 + stripInset, y: stripY - 23,
                                        width: stripWidth, height: 40), display: false)
        // `b` is in CG's top-left coordinates while AppKit windows use a
        // bottom-left origin. Inset the compact target acknowledgement from
        // the prompt terminal's lower-left content corner.
        let targetInset: CGFloat = 10
        platformTargetBadgeWindow.setFrameOrigin(NSPoint(
            x: b.0 + targetInset,
            y: screenHeight - (b.1 + b.3) + targetInset))
        CATransaction.begin()
        CATransaction.setDisableActions(true)
        heartbeatTrackLayer.frame = CGRect(x: 0, y: 24, width: stripWidth, height: 3)
        // Reposition runs before the countdown update. Never reset the fill
        // to full width here: that exposed one rendered max-width frame before
        // the next pass restored current progress, visibly blinking between
        // max and current at the layout cadence.
        let remaining = heartbeatDeadline?.timeIntervalSinceNow ?? heartbeatInterval
        let progress = max(0, min(1, 1 - remaining / heartbeatInterval))
        let pixelScale = max(1, heartbeatWindow.backingScaleFactor)
        let burnX = (stripWidth * progress * pixelScale).rounded() / pixelScale
        heartbeatFillLayer.transform = CATransform3DIdentity
        heartbeatFillLayer.bounds = CGRect(
            x: 0, y: 0,
            width: max(0, stripWidth - burnX),
            height: heartbeatTrackLayer.bounds.height)
        heartbeatFillLayer.position = CGPoint(x: heartbeatTrackLayer.bounds.maxX,
                                              y: heartbeatTrackLayer.bounds.midY)
        heartbeatFuseEmitter.emitterPosition = CGPoint(
            x: burnX, y: heartbeatTrackLayer.bounds.midY)
        CATransaction.commit()
    }

    /// The rock's centre in CG screen coordinates (top-left origin), computed
    /// from its terminal's bounds — the point the occlusion check samples.
    func rockPoint(bounds b: (CGFloat, CGFloat, CGFloat, CGFloat)) -> CGPoint {
        CGPoint(x: b.0 + b.2 - 10 - size / 2, y: b.1 + 30 + size / 2)
    }

    /// Show/hide driven by the controller's occlusion check — the "embedded"
    /// illusion. The badge floats above everything, but it only *shows* while
    /// its corner of the terminal is genuinely visible, so it hides and
    /// reappears with its window exactly as if it were part of it.
    func setVisible(_ v: Bool, above terminalWindowNumber: Int) {
        if v {
            if !window.isVisible { window.orderFrontRegardless() }
            if loopboyStyled && (!heartbeatWindow.isVisible
                                 || heartbeatOrderedAbove != terminalWindowNumber) {
                heartbeatWindow.order(.above, relativeTo: terminalWindowNumber)
                heartbeatOrderedAbove = terminalWindowNumber
            }
            if !platformTarget.isEmpty
                && (!platformTargetBadgeWindow.isVisible
                    || platformTargetBadgeOrderedAbove != terminalWindowNumber) {
                platformTargetBadgeWindow.order(.above, relativeTo: terminalWindowNumber)
                platformTargetBadgeOrderedAbove = terminalWindowNumber
            }
        } else {
            setHovered(false)   // a covered rock stops reacting to the pointer
            if window.isVisible { window.orderOut(nil) }
            if heartbeatWindow.isVisible { heartbeatWindow.orderOut(nil) }
            if platformTargetBadgeWindow.isVisible { platformTargetBadgeWindow.orderOut(nil) }
            heartbeatOrderedAbove = nil
            platformTargetBadgeOrderedAbove = nil
        }
    }

    /// Is the badge actually on screen right now? A hidden rock keeps its
    /// `hitRect`, so the hover hit-test still consults visibility.
    var isOnScreen: Bool { window.isVisible }

    /// Ease the badge toward its target by a frame-rate-independent step.
    /// Returns true while still settling (so the controller keeps the
    /// animation loop warm), false once it's arrived. `tau` is the follow time
    /// constant — smaller is snappier, larger is floatier.
    @discardableResult
    func advance(dt: CGFloat) -> Bool {
        guard let target = targetOrigin else { return false }
        guard var cur = currentOrigin else {
            currentOrigin = target; window.setFrameOrigin(target); return false
        }
        let dx = target.x - cur.x, dy = target.y - cur.y
        if abs(dx) < 0.4 && abs(dy) < 0.4 {
            if cur != target { window.setFrameOrigin(target); currentOrigin = target }
            return false
        }
        // Tight follow: near-immediate tracking with just a whisper of
        // smoothing so the badge still eases rather than teleporting. (Was
        // 0.42 — a floaty drift that read as lag once the wall got busy.)
        let tau: CGFloat = 0.06
        let alpha = 1 - exp(-dt / tau)
        cur.x += dx * alpha
        cur.y += dy * alpha
        currentOrigin = cur
        window.setFrameOrigin(cur)
        return true
    }

    func hide() {
        if window.isVisible { window.orderOut(nil) }
        if heartbeatWindow.isVisible { heartbeatWindow.orderOut(nil) }
        if platformTargetBadgeWindow.isVisible { platformTargetBadgeWindow.orderOut(nil) }
        heartbeatOrderedAbove = nil
        platformTargetBadgeOrderedAbove = nil
    }
    func close() {
        window.orderOut(nil)
        heartbeatWindow.orderOut(nil)
        platformTargetBadgeWindow.orderOut(nil)
        heartbeatOrderedAbove = nil
        platformTargetBadgeOrderedAbove = nil
    }
}

/// A quiet procedural paper stock used behind the card and its illustration
/// matte. Deterministic flecks + short fibres keep every refresh visually
/// stable (no shimmering noise) while breaking the too-perfect glass gradient.
final class CardPaperLayer: CALayer {
    override init() {
        super.init()
        contentsScale = NSScreen.main?.backingScaleFactor ?? 2
        needsDisplayOnBoundsChange = true
    }

    override init(layer: Any) { super.init(layer: layer) }
    required init?(coder: NSCoder) { super.init(coder: coder) }

    override func draw(in ctx: CGContext) {
        var seed: UInt32 = 0x50_52_4f_58
        func next() -> CGFloat {
            seed = seed &* 1_664_525 &+ 1_013_904_223
            return CGFloat(seed & 0x00ff_ffff) / CGFloat(0x0100_0000)
        }
        ctx.saveGState()
        ctx.setBlendMode(.softLight)
        for i in 0..<950 {
            let light = (i & 1) == 0
            ctx.setFillColor(NSColor(white: light ? 1 : 0, alpha: light ? 0.045 : 0.032).cgColor)
            let d = 0.28 + next() * 0.9
            ctx.fill(CGRect(x: next() * bounds.width, y: next() * bounds.height,
                            width: d, height: d * (0.55 + next())))
        }
        ctx.setLineWidth(0.35)
        for i in 0..<75 {
            ctx.setStrokeColor(NSColor(white: (i & 1) == 0 ? 1 : 0,
                                       alpha: (i & 1) == 0 ? 0.035 : 0.022).cgColor)
            let x = next() * bounds.width, y = next() * bounds.height
            ctx.move(to: CGPoint(x: x, y: y))
            ctx.addLine(to: CGPoint(x: min(bounds.width, x + 3 + next() * 17),
                                    y: y + (next() - 0.5) * 1.8))
            ctx.strokePath()
        }
        ctx.restoreGState()
    }
}

/// A card should share on its first click even while Slab itself is not the
/// active app. The non-activating panel stays out of the user's focus chain;
/// this view simply opts the click into its gesture recognizer.
final class ShareableCardView: NSView {
    var onClick: (() -> Void)?
    override func acceptsFirstMouse(for event: NSEvent?) -> Bool { true }
    override func mouseDown(with event: NSEvent) { onClick?() }
}

final class ShareCardPanel: NSPanel {
    override var canBecomeKey: Bool { true }
}

/// The collectible summary card a rock reveals on hover or click. It carries
/// the rock itself as card art, a status-colour frame and badge, the living
/// memoir as flavour text, and an uptime/activity/agent stat strip. One shared
/// instance; it floats above everything and never takes the mouse.
final class SigilBubble: NSObject, NSSharingServicePickerDelegate {
    private let window: ShareCardPanel
    private let container = ShareableCardView()
    private let gradient = CAGradientLayer()
    private let paperTexture = CardPaperLayer()
    private let innerBorder = CALayer()
    private let accentRule = CALayer()
    private let artPanel = NSView()
    private let artBackdrop = CAGradientLayer()
    private let artPaper = CardPaperLayer()
    private let artInnerBorder = CALayer()
    private let art = NSImageView()
    private let kicker = NSTextField(labelWithString: "PROMPT ROCK  •  LIVING MEMORY")
    private let title = NSTextField(labelWithString: "")
    private let statusPill = NSView()
    private let status = NSTextField(labelWithString: "")
    private let storyPanel = NSView()
    private let story = NSTextField(wrappingLabelWithString: "")
    private let statsPanel = NSView()
    private let stats = NSTextField(labelWithString: "")
    private var sharePicker: NSSharingServicePicker?
    private var shareWatchdog: DispatchWorkItem?
    private var exportTitle = "prox"
    private(set) var isSharing = false

    override init() {
        window = ShareCardPanel(contentRect: NSRect(x: 0, y: 0, width: 10, height: 10),
                                styleMask: [.borderless],
                                backing: .buffered, defer: true)
        super.init()
        window.isOpaque = false
        window.backgroundColor = .clear
        window.hasShadow = true
        window.ignoresMouseEvents = false
        window.level = .statusBar
        window.isFloatingPanel = true
        window.becomesKeyOnlyIfNeeded = true
        window.collectionBehavior = [.canJoinAllSpaces, .fullScreenAuxiliary]
        container.wantsLayer = true
        container.layer?.cornerRadius = 15
        container.layer?.masksToBounds = true
        gradient.startPoint = CGPoint(x: 0, y: 1)
        gradient.endPoint = CGPoint(x: 1, y: 0)
        container.layer?.addSublayer(gradient)
        paperTexture.opacity = 0.48
        container.layer?.addSublayer(paperTexture)
        innerBorder.cornerRadius = 11
        innerBorder.borderWidth = 1
        container.layer?.addSublayer(innerBorder)
        container.layer?.addSublayer(accentRule)

        artPanel.wantsLayer = true
        artPanel.layer?.cornerRadius = 9
        artPanel.layer?.masksToBounds = true
        artPanel.layer?.borderWidth = 2
        artBackdrop.startPoint = CGPoint(x: 0, y: 1)
        artBackdrop.endPoint = CGPoint(x: 1, y: 0)
        artPanel.layer?.addSublayer(artBackdrop)
        artPaper.opacity = 0.62
        artPanel.layer?.addSublayer(artPaper)
        artInnerBorder.cornerRadius = 6
        artInnerBorder.borderWidth = 1
        artPanel.layer?.addSublayer(artInnerBorder)
        art.imageScaling = .scaleProportionallyUpOrDown
        art.wantsLayer = true
        art.layer?.magnificationFilter = .linear
        art.layer?.minificationFilter = .trilinear
        artPanel.addSubview(art)

        kicker.font = NSFont.monospacedSystemFont(ofSize: 7.5, weight: .bold)
        kicker.textColor = NSColor(white: 1, alpha: 0.5)
        title.font = playfulRockFont(17)
        title.lineBreakMode = .byTruncatingTail
        statusPill.wantsLayer = true
        statusPill.layer?.cornerRadius = 9
        status.font = NSFont.monospacedSystemFont(ofSize: 8, weight: .bold)
        status.alignment = .center
        status.textColor = .white
        statusPill.addSubview(status)

        storyPanel.wantsLayer = true
        storyPanel.layer?.cornerRadius = 7
        storyPanel.layer?.borderWidth = 1
        story.font = NSFont.systemFont(ofSize: 10.5, weight: .regular)
        story.textColor = NSColor(white: 1, alpha: 0.86)
        story.maximumNumberOfLines = 8
        story.lineBreakMode = .byWordWrapping
        story.cell?.wraps = true
        story.cell?.usesSingleLineMode = false
        storyPanel.addSubview(story)
        statsPanel.wantsLayer = true
        statsPanel.layer?.cornerRadius = 7
        stats.font = NSFont.monospacedSystemFont(ofSize: 8, weight: .semibold)
        stats.alignment = .center

        for view in [artPanel, kicker, title, statusPill, storyPanel, statsPanel] {
            container.addSubview(view)
        }
        statsPanel.addSubview(stats)
        container.onClick = { [weak self] in self?.sharePNG() }
        window.contentView = container
    }

    /// Show the card near `anchor`: tucked under the rock, right-aligned, and
    /// flipped above when there is no room below.
    func show(cardTitle: String, body: String, edition: String,
              cardStatus: String, cardStats: String,
              accent: NSColor, image: NSImage?, near anchor: NSRect) {
        // KidLisp defines the house card at 250 × 350 (5:7). On the desktop we
        // display that design at 76% so it reads as a small collectible beside
        // a terminal rather than covering a large portion of the work itself.
        let s: CGFloat = 0.76
        let w: CGFloat = 250 * s
        let h: CGFloat = 350 * s
        let pad: CGFloat = 11 * s
        let artH: CGFloat = 122 * s
        let storyH: CGFloat = 106 * s
        let statsH: CGFloat = 27 * s

        title.stringValue = cardTitle
        exportTitle = cardTitle
        kicker.stringValue = edition
        status.stringValue = cardStatus.uppercased()
        story.stringValue = body
        story.preferredMaxLayoutWidth = w - pad * 2 - 14 * s
        stats.stringValue = cardStats.uppercased()
        art.image = image
        kicker.font = NSFont.monospacedSystemFont(ofSize: 6.4, weight: .bold)
        title.font = playfulRockFont(14)
        status.font = NSFont.monospacedSystemFont(ofSize: 7, weight: .bold)
        story.font = NSFont.systemFont(ofSize: 9, weight: .regular)
        stats.font = NSFont.monospacedSystemFont(ofSize: 6.5, weight: .semibold)

        // Uniform physical stock follows the system appearance. Status is an
        // accent, not a wash over the whole card: keyline + jewel + fine rails
        // stay colour-coded while every prox belongs to the same card family.
        let dark = AppDelegate.isDarkAppearance()
        let ink = dark ? NSColor(white: 0.94, alpha: 1) : NSColor(white: 0.10, alpha: 1)
        let subdued = dark ? NSColor(white: 0.72, alpha: 1) : NSColor(white: 0.34, alpha: 1)
        let paperA = dark
            ? NSColor(deviceRed: 0.105, green: 0.10, blue: 0.085, alpha: 0.985)
            : NSColor(deviceRed: 0.965, green: 0.935, blue: 0.83, alpha: 0.99)
        let paperB = dark
            ? NSColor(deviceRed: 0.16, green: 0.145, blue: 0.12, alpha: 0.985)
            : NSColor(deviceRed: 1.0, green: 0.975, blue: 0.88, alpha: 0.99)
        let well = dark
            ? NSColor(deviceRed: 0.055, green: 0.052, blue: 0.045, alpha: 0.82)
            : NSColor(deviceRed: 0.91, green: 0.875, blue: 0.77, alpha: 0.88)
        title.textColor = ink
        kicker.textColor = subdued
        story.textColor = ink.withAlphaComponent(0.9)
        stats.textColor = accent.blended(withFraction: dark ? 0.3 : 0.36,
                                         of: dark ? .white : .black) ?? accent

        container.frame = NSRect(x: 0, y: 0, width: w, height: h)
        container.layer?.cornerRadius = 15 * s
        innerBorder.cornerRadius = 11 * s
        artPanel.layer?.cornerRadius = 9 * s
        artInnerBorder.cornerRadius = 6 * s
        statusPill.layer?.cornerRadius = 9 * s
        storyPanel.layer?.cornerRadius = 7 * s
        statsPanel.layer?.cornerRadius = 7 * s
        gradient.frame = container.bounds
        paperTexture.frame = container.bounds
        gradient.cornerRadius = 15 * s
        gradient.colors = [
            paperB.cgColor,
            paperA.cgColor,
            paperB.blended(withFraction: 0.25, of: paperA)!.cgColor,
        ]
        gradient.locations = [0, 0.42, 1]
        container.layer?.borderWidth = 3.5 * s
        container.layer?.borderColor = accent.withAlphaComponent(0.92).cgColor
        innerBorder.frame = container.bounds.insetBy(dx: 5 * s, dy: 5 * s)
        innerBorder.borderColor = ink.withAlphaComponent(0.18).cgColor
        accentRule.frame = CGRect(x: 0, y: h - 5 * s, width: w, height: 5 * s)
        accentRule.backgroundColor = accent.cgColor

        let kickerH: CGFloat = 10 * s
        let headerH: CGFloat = 24 * s
        var top = h - pad
        kicker.frame = NSRect(x: pad, y: top - kickerH, width: w - 2 * pad, height: kickerH)
        top -= kickerH + 4 * s
        let pillW = max(58 * s, status.intrinsicContentSize.width + 16 * s)
        title.frame = NSRect(x: pad, y: top - headerH,
                             width: w - 2 * pad - pillW - 7 * s, height: headerH)
        statusPill.frame = NSRect(x: w - pad - pillW, y: top - 20 * s,
                                  width: pillW, height: 17 * s)
        statusPill.layer?.backgroundColor = accent.withAlphaComponent(0.88).cgColor
        status.frame = statusPill.bounds.insetBy(dx: 6 * s, dy: 2 * s)
        top -= headerH + 7 * s

        // Illustration window: a crisp hi-res rock centred over its own
        // status-tinted scene, with a double little border like a real card's
        // framed art rather than an icon floating in empty UI.
        artPanel.frame = NSRect(x: pad, y: top - artH, width: w - 2 * pad, height: artH)
        artPanel.layer?.borderColor = accent.withAlphaComponent(0.75).cgColor
        artBackdrop.frame = artPanel.bounds
        artPaper.frame = artPanel.bounds
        artBackdrop.colors = [
            well.blended(withFraction: 0.08, of: accent)!.cgColor,
            well.cgColor,
            paperA.cgColor,
        ]
        artBackdrop.locations = [0, 0.58, 1]
        artInnerBorder.frame = artPanel.bounds.insetBy(dx: 4 * s, dy: 4 * s)
        artInnerBorder.borderColor = ink.withAlphaComponent(0.18).cgColor
        let rockSide: CGFloat = 116 * s
        art.frame = NSRect(x: (artPanel.bounds.width - rockSide) / 2,
                           y: (artPanel.bounds.height - rockSide) / 2,
                           width: rockSide, height: rockSide)
        top -= artH + 8 * s

        storyPanel.frame = NSRect(x: pad, y: top - storyH, width: w - 2 * pad, height: storyH)
        storyPanel.layer?.backgroundColor = well.cgColor
        storyPanel.layer?.borderColor = accent.withAlphaComponent(0.3).cgColor
        story.frame = storyPanel.bounds.insetBy(dx: 7 * s, dy: 6 * s)

        statsPanel.frame = NSRect(x: pad, y: pad, width: w - 2 * pad, height: statsH)
        statsPanel.layer?.backgroundColor = accent.withAlphaComponent(0.12).cgColor
        statsPanel.layer?.borderWidth = 1
        statsPanel.layer?.borderColor = accent.withAlphaComponent(0.24).cgColor
        stats.frame = statsPanel.bounds.insetBy(dx: 8 * s, dy: 7 * s)

        var x = anchor.maxX - w
        var y = anchor.minY - h - 4
        // Clamp to the rock's own display. Using `NSScreen.main` made bubbles
        // for rocks on secondary displays materialize at the main display's
        // edge, which looked exactly like hover had stopped working.
        let anchorPoint = NSPoint(x: anchor.midX, y: anchor.midY)
        let anchorScreen = NSScreen.screens.first { $0.frame.contains(anchorPoint) }
        if let vis = (anchorScreen ?? NSScreen.main)?.visibleFrame {
            x = min(max(vis.minX + 4, x), vis.maxX - w - 4)
            if y < vis.minY + 4 { y = anchor.maxY + 4 }
        }
        window.setFrame(NSRect(x: x, y: y, width: w, height: h), display: true)
        window.orderFrontRegardless()
    }

    var frame: NSRect { window.frame }
    func contains(_ point: NSPoint) -> Bool { window.isVisible && window.frame.contains(point) }

    /// Render the finished 5:7 card to a real PNG, then present macOS's
    /// native share sheet beside it. This gives Messages, AirDrop, Mail, etc.
    /// without baking any sharing chrome into the collectible itself.
    @objc func sharePNG() {
        guard !isSharing else { return }
        isSharing = true
        NSLog("🪨 [card] opening native share sheet for \(exportTitle)")
        container.displayIfNeeded()
        guard let rep = container.bitmapImageRepForCachingDisplay(in: container.bounds) else {
            isSharing = false
            return
        }
        container.cacheDisplay(in: container.bounds, to: rep)
        guard let data = rep.representation(using: .png, properties: [:]) else {
            isSharing = false
            return
        }
        // Also stage the same PNG on the pasteboard. The share sheet remains
        // the visible interaction, but ⌘V into Messages is an immediate
        // fallback and requires no second export.
        let pasteItem = NSPasteboardItem()
        pasteItem.setData(data, forType: .png)
        NSPasteboard.general.clearContents()
        NSPasteboard.general.writeObjects([pasteItem])
        let safe = exportTitle.lowercased()
            .replacingOccurrences(of: "[^a-z0-9]+", with: "-", options: .regularExpression)
            .trimmingCharacters(in: CharacterSet(charactersIn: "-"))
        let url = FileManager.default.temporaryDirectory
            .appendingPathComponent("prox-\(safe.isEmpty ? "card" : safe).png")
        guard (try? data.write(to: url, options: .atomic)) != nil else {
            isSharing = false
            return
        }
        let picker = NSSharingServicePicker(items: [url])
        picker.delegate = self
        sharePicker = picker
        shareWatchdog?.cancel()
        let watchdog = DispatchWorkItem { [weak self] in
            guard let self, self.isSharing else { return }
            NSLog("🪨 [card] share sheet watchdog released \(self.exportTitle)")
            self.isSharing = false
            self.sharePicker = nil
            self.hide()
        }
        shareWatchdog = watchdog
        DispatchQueue.main.asyncAfter(deadline: .now() + 12, execute: watchdog)
        // A global monitor may have observed a click that will still activate
        // the app underneath the floating card. Let that mouse-down finish,
        // then reclaim focus and present; otherwise AppKit opens and dismisses
        // the share popover in the same event turn.
        DispatchQueue.main.asyncAfter(deadline: .now() + 0.18) { [weak self, weak picker] in
            guard let self, let picker else { return }
            NSApp.activate(ignoringOtherApps: true)
            self.window.makeKeyAndOrderFront(nil)
            // Activation itself settles asynchronously for an LSUIElement.
            // Anchor the picker only once the panel is the real key window.
            DispatchQueue.main.asyncAfter(deadline: .now() + 0.12) { [weak self, weak picker] in
                guard let self, let picker else { return }
                picker.show(relativeTo: self.container.bounds, of: self.container, preferredEdge: .maxX)
            }
        }
    }

    func sharingServicePicker(_ sharingServicePicker: NSSharingServicePicker,
                              didChoose service: NSSharingService?) {
        NSLog("🪨 [card] share sheet closed (service: \(service?.title ?? "none"))")
        shareWatchdog?.cancel()
        shareWatchdog = nil
        isSharing = false
        sharePicker = nil
        if service == nil { hide() }
    }

    func hide() {
        if !isSharing, window.isVisible { window.orderOut(nil) }
    }
}

/// Owns the live badge set, the status→motion mapping, and a light reposition
/// tick. Energy-conscious: bounds + z-order come from CGWindowList in-process
/// (no fork); osascript runs only to (re)bind each tty to its CGWindowID, on
/// membership change / when a binding goes missing / at a slow safety cadence
/// — never per frame.
final class PromptSigilOverlayController {
    struct PromptParticleTarget {
        let windowID: Int
        let frame: CGRect       // CG/AX coordinates: top-left origin
        let color: NSColor
        let seed: UInt64        // stable prox pet-name seed
    }

    static let shared = PromptSigilOverlayController()
    private init() {}

    private var overlays: [String: PromptSigilOverlay] = [:]
    private var timer: Timer?
    /// tty (bare) → CGWindowID of its terminal window.
    private var binding: [String: Int] = [:]
    /// Bare tty → the cursor accent used by under-window focus particles.
    private var particleColors: [String: NSColor] = [:]
    /// Current live prox windows, shared with focus navigation/highlighting so
    /// those features inherit the controller's tty-accurate Terminal binding.
    var promptWindowIDs: Set<Int> { Set(binding.values) }
    var promptParticleTargets: [PromptParticleTarget] {
        particleColors.compactMap { tty, color in
            guard let id = binding[tty], let b = lastBoundsByNum[id] else { return nil }
            let proxName = overlays.values.first(where: { $0.tty == tty })?.name ?? tty
            return PromptParticleTarget(
                windowID: id,
                frame: CGRect(x: b.0, y: b.1, width: b.2, height: b.3),
                color: color,
                seed: SigilRenderer.seed(for: proxName))
        }
    }
    private var needsRebind = false
    private var bindInFlight = false
    private var lastBind = Date.distantPast
    /// Adaptive cadence: idle slow (low energy), but ramp to display rate while
    /// a tracked window is actually moving so badges snap tight to a drag, then
    /// settle back once it stops.
    private var motionDeadline = Date.distantPast
    private var lastBoundsByNum: [Int: (CGFloat, CGFloat, CGFloat, CGFloat)] = [:]
    private let idleInterval: TimeInterval = 0.2
    private let activeInterval: TimeInterval = 1.0 / 60.0
    /// The cadence currently scheduled, so an AX/move event can promote the
    /// loop from idle to display-rate immediately without restarting a timer
    /// that's already fast (which would starve under a stream of events).
    private var currentInterval: TimeInterval = 0.2
    private var lastTick = Date.distantPast
    private var observerInstalled = false
    /// Live AX observers per terminal app pid (kept alive by this reference).
    private var axObservers: [pid_t: AXObserver] = [:]
    /// Global-sun state, recomputed each wall-clock minute so the whole wall's
    /// light tracks the time of day together.
    private var sunMinute = -1
    private var sun: (hx: CGFloat, elevation: CGFloat, intensity: CGFloat, drop: CGSize) =
        (0.4, 0.7, 0.9, CGSize(width: -2.4, height: -2.4))
    /// Serial queue for the offscreen 3D frame renders — one rock at a time so
    /// a per-minute sun change never spins up 7 Metal renderers at once.
    private let renderQueue = DispatchQueue(label: "computer.slab.sigil-frames", qos: .userInitiated)

    /// Hover/click plumbing for the rocks. The badge windows stay
    /// mouse-transparent (clicks still reach the terminal beneath); GLOBAL
    /// event monitors watch the pointer instead, so pointing at a rock costs
    /// the terminal nothing. The existing 5 Hz overlay tick samples it too as
    /// a fallback when macOS withholds a global monitor after a signed update.
    /// Hover only animates the rock; clicking reveals and pins its card.
    private var mouseMonitors: [Any] = []
    /// The last window stack `reposition` saw (front-to-back, normal level), so
    /// a mouse-move can ask "what's actually on top here?" without a fresh
    /// CGWindowList snapshot per event. Refreshed at the tick rate.
    private var lastStack: [(num: Int, rect: CGRect)] = []
    private var hoverTarget: String?
    private var bubbleFor: String?
    private var bubblePinned = false
    private let bubble = SigilBubble()

    /// Non-capturing AX callback → route on main. Lexically inside the class so
    /// it may reach the private singleton; captures nothing local, so it
    /// converts cleanly to the C function pointer type. A window move/resize
    /// snaps that one badge from the moved window's live AX geometry; a focus
    /// change re-raises behind badges.
    private static let axCallback: AXObserverCallback = { _, element, notification, _ in
        let note = notification as String
        DispatchQueue.main.async {
            PromptSigilOverlayController.shared.handleAX(note: note, window: element)
        }
    }

    func handleAX(note: String, window: AXUIElement) {
        if note == kAXWindowMovedNotification as String
            || note == kAXWindowResizedNotification as String {
            snapFromAX(window)
        } else {
            reposition()   // focus / activation → re-raise behind badges
        }
    }

    /// Reposition the one badge bound to `window` directly from the window's
    /// LIVE AX position/size — fires per drag-frame, so the badge moves on the
    /// same frame as the window (no CGWindowList lag). z-order is left alone
    /// (the window stays frontmost through a drag), so no restack/flicker.
    private func snapFromAX(_ window: AXUIElement) {
        var wid = CGWindowID(0)
        guard _AXUIElementGetWindow(window, &wid) == .success, wid != 0 else { return }
        let num = Int(wid)
        guard let tty = binding.first(where: { $0.value == num })?.key,
              let ov = overlays.values.first(where: { $0.tty == tty }),
              let p = axValue(window, kAXPositionAttribute, .cgPoint) as? CGPoint,
              let s = axValue(window, kAXSizeAttribute, .cgSize) as? CGSize
        else { return }
        let screenH = NSScreen.main?.frame.height ?? 0
        ov.place(bounds: (p.x, p.y, s.width, s.height), screenHeight: screenH)
        lastBoundsByNum[num] = (p.x, p.y, s.width, s.height)
        PromptFocusHighlight.shared.refreshNow()
        promote()   // keep the spring loop warm so the badge eases to the new target
    }

    /// Promote the animation loop to display rate now (e.g. a drag started),
    /// without restarting an already-fast timer — so a stream of AX move events
    /// doesn't continually push the next fire out and starve the loop.
    private func promote() {
        motionDeadline = Date().addingTimeInterval(0.3)
        if currentInterval > activeInterval + 1e-6 {
            scheduleTick(after: activeInterval)
        }
    }

    /// The tiler can move several terminal windows in one AX sweep faster than
    /// per-window notifications arrive. Refresh tty → window ownership now so
    /// rocks transfer with the completed layout instead of waiting for the
    /// five-second safety bind.
    func terminalsDidRetile() {
        needsRebind = true
        promote()
        reposition()
    }

    /// Read an AXValue geometry attribute (point or size) off an element.
    private func axValue(_ el: AXUIElement, _ attr: String, _ type: AXValueType) -> Any? {
        var ref: CFTypeRef?
        guard AXUIElementCopyAttributeValue(el, attr as CFString, &ref) == .success,
              let v = ref, CFGetTypeID(v) == AXValueGetTypeID() else { return nil }
        if type == .cgPoint {
            var p = CGPoint.zero
            return AXValueGetValue(v as! AXValue, .cgPoint, &p) ? p : nil
        } else {
            var sz = CGSize.zero
            return AXValueGetValue(v as! AXValue, .cgSize, &sz) ? sz : nil
        }
    }

    /// Re-raise behind-the-terminal badges the instant another app activates
    /// (clicking from another app onto a terminal), rather than waiting for the
    /// next poll tick — so a cross-app focus has no visible sink.
    private func installActivationObserverIfNeeded() {
        guard !observerInstalled else { return }
        observerInstalled = true
        NSWorkspace.shared.notificationCenter.addObserver(
            forName: NSWorkspace.didActivateApplicationNotification,
            object: nil, queue: .main
        ) { [weak self] _ in self?.reposition() }
    }

    /// Attach an Accessibility focus observer to each running terminal app, so
    /// a SAME-app window switch (terminal → terminal, the common tiled case)
    /// fires kAXFocusedWindowChanged and re-raises the badge instantly instead
    /// of waiting up to a poll tick. Needs AX trust (the same the tiler uses);
    /// without it we silently fall back to the poll. Re-scanned each sync() so
    /// a terminal app launched later gets observed too.
    private func installAXFocusObservers() {
        guard AXIsProcessTrusted() else { return }
        for app in NSWorkspace.shared.runningApplications
            where Self.terminalBundleIds.contains(app.bundleIdentifier ?? "") {
            let pid = app.processIdentifier
            guard axObservers[pid] == nil else { continue }
            var observer: AXObserver?
            guard AXObserverCreate(pid, Self.axCallback, &observer) == .success,
                  let obs = observer else { continue }
            let appEl = AXUIElementCreateApplication(pid)
            AXObserverAddNotification(obs, appEl, kAXFocusedWindowChangedNotification as CFString, nil)
            AXObserverAddNotification(obs, appEl, kAXMainWindowChangedNotification as CFString, nil)
            AXObserverAddNotification(obs, appEl, kAXApplicationActivatedNotification as CFString, nil)
            // Window-level move/resize on the app element fire per drag-frame
            // with the moved window passed back — this is what makes the badge
            // snap to a drag instead of lagging the CGWindowList poll.
            AXObserverAddNotification(obs, appEl, kAXWindowMovedNotification as CFString, nil)
            AXObserverAddNotification(obs, appEl, kAXWindowResizedNotification as CFString, nil)
            CFRunLoopAddSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(obs), .commonModes)
            axObservers[pid] = obs
        }
    }

    private static let terminalBundleIds = ["com.apple.Terminal", "com.googlecode.iterm2"]

    private func installMouseMonitorsIfNeeded() {
        guard mouseMonitors.isEmpty else { return }
        if let move = NSEvent.addGlobalMonitorForEvents(matching: .mouseMoved, handler: { [weak self] _ in
            self?.handleMouseMoved()
        }) { mouseMonitors.append(move) }
        if let click = NSEvent.addGlobalMonitorForEvents(matching: .leftMouseDown, handler: { [weak self] _ in
            self?.handleMouseDown()
        }) { mouseMonitors.append(click) }
        // Clicks received by Slab's own non-activating card panel do not reach
        // a global monitor. Keep a local twin so card → native share is
        // reliable regardless of which side of macOS's event routing wins.
        if let localClick = NSEvent.addLocalMonitorForEvents(matching: .leftMouseDown, handler: {
            [weak self] event in
            guard let self else { return event }
            if self.bubbleFor != nil, self.bubble.contains(NSEvent.mouseLocation) {
                self.bubble.sharePNG()
                return nil
            }
            return event
        }) { mouseMonitors.append(localClick) }
    }

    private func removeMouseMonitors() {
        for m in mouseMonitors { NSEvent.removeMonitor(m) }
        mouseMonitors.removeAll()
        hoverTarget = nil; bubbleFor = nil; bubblePinned = false
        bubble.hide()
    }

    /// The rock under the pointer — or nil when the pointer is over a window
    /// that merely *covers* one. Two gates, because a badge floats above the
    /// whole normal-window stack and is mouse-transparent, so a bare rect test
    /// would let a buried stone wake up through the thing burying it:
    /// the rock has to be on screen, and its terminal has to still be the
    /// topmost normal window under the pointer (the same test `reposition`
    /// runs at the rock's centre, re-run here at the exact point, so partial
    /// coverage of the stone reads correctly too).
    private func overlayAt(_ p: NSPoint) -> PromptSigilOverlay? {
        guard let hit = overlays.values.first(where: { $0.isOnScreen && $0.hitRect.contains(p) }),
              let num = binding[hit.tty]
        else { return nil }
        let screenH = NSScreen.main?.frame.height ?? 0
        let cg = CGPoint(x: p.x, y: screenH - p.y)   // AppKit (bottom-left) → CG (top-left)
        if let top = lastStack.first(where: { $0.rect.contains(cg) })?.num, top != num { return nil }
        return hit
    }

    /// A rock that just went off screen (covered, or its window is gone) must
    /// let go of the pointer: drop its hover and any bubble it owns, so the
    /// card doesn't hang over the window that buried it.
    private func dropInteraction(for ov: PromptSigilOverlay) {
        if hoverTarget == ov.sessionId {
            hoverTarget = nil
        }
        if bubbleFor == ov.sessionId {
            bubbleFor = nil; bubblePinned = false
            bubble.hide()
        }
    }

    /// Hover is visual feedback only. Cards are deliberately click-to-open so
    /// moving across a wall of prompt rocks never fills the desktop with an
    /// unsolicited share card.
    private func handleMouseMoved() {
        if bubble.isSharing { return }
        let point = NSEvent.mouseLocation
        let hit = overlayAt(point)
        if hit?.sessionId == hoverTarget { return }
        if let old = hoverTarget, let oldOv = overlays[old] { oldOv.setHovered(false) }
        hoverTarget = hit?.sessionId
        hit?.setHovered(true)
    }

    /// Click a rock → reveal (pinned, survives mouse-out); click it again or
    /// anywhere else → dismiss. The click still lands in the terminal too
    /// (the badge is mouse-transparent), which is what you want: focus the
    /// window you're asking about.
    private func handleMouseDown() {
        if bubbleFor != nil, bubble.contains(NSEvent.mouseLocation) {
            // Fallback for systems that route the click through the
            // non-activating panel to its underlying app.
            bubble.sharePNG()
            return
        }
        if let ov = overlayAt(NSEvent.mouseLocation) {
            if bubblePinned, bubbleFor == ov.sessionId {
                bubblePinned = false; bubbleFor = nil
                bubble.hide()
            } else {
                bubblePinned = true
                bubbleFor = ov.sessionId
                showBubble(for: ov)
            }
        } else if bubblePinned {
            bubblePinned = false; bubbleFor = nil
            bubble.hide()
        }
    }

    private func showBubble(for ov: PromptSigilOverlay) {
        bubble.show(
            cardTitle: ov.tooltipTitle,
            body: ov.tooltipBody,
            edition: ov.tooltipEdition,
            cardStatus: ov.tooltipStatus,
            cardStats: ov.tooltipStats,
            accent: ov.tooltipAccent,
            image: ov.tooltipImage,
            near: ov.hitRect)
    }

    /// The session's status colour (the same per-status `cursor` accent the
    /// menubar polygon + themed terminals use), for the badge's halo.
    private func statusColor(for state: ClaudeSession.State, agentType: String = "claude") -> NSColor {
        let dark = AppDelegate.isDarkAppearance()
        let cur = AppDelegate.statusDecor(for: state, dark: dark, agentType: agentType).palette.cursor
            ?? (32768, 32768, 32768)
        return NSColor(deviceRed: CGFloat(cur.0) / 65535, green: CGFloat(cur.1) / 65535,
                       blue: CGFloat(cur.2) / 65535, alpha: 1.0)
    }

    private func motion(for state: ClaudeSession.State) -> (Double, Bool) {
        switch state {
        case .working:     return (10, true)
        case .rendering:   return (7,  true)
        case .awaiting:    return (13, false)
        case .interrupted: return (26, false)
        case .complete:    return (38, true)
        case .blank:       return (80, true)
        case .stale:       return (44, false)
        }
    }

    private func loopboySessions() -> Set<String> {
        guard let data = FileManager.default.contents(atPath: Paths.loopboyConfig),
              let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any],
              let loops = obj["loops"] as? [String: Any] else { return [] }
        return Set(loops.values.compactMap {
            ($0 as? [String: Any])?["sessionId"] as? String
        })
    }

    func pulseLoopboy(sessionId: String) {
        let ids = loopboySessions()
        guard ids.contains(sessionId), let overlay = overlays[sessionId] else { return }
        let beat = CACurrentMediaTime() + 0.08
        overlay.heartbeatPulse(beginTime: beat)
        overlay.resetHeartbeatCountdown()
        if let (bounds, screen) = overlay.heartbeatTarget {
            ZoomSpecialMove.fire(around: bounds, on: screen)
        }
    }

    func flyPrompt(sessionId: String, text: String) {
        guard let overlay = overlays[sessionId],
              let target = overlay.promptFlightTarget else { return }
        let destination = cursorDestination(tty: overlay.tty, screen: target.3) ?? target.1
        let availableWidth = max(120, target.4 - destination.x - 12)
        PromptGlyphFlight.show(text: text, from: target.0, to: destination,
                               color: target.2, maxWidth: availableWidth,
                               on: target.3)
    }

    func setPromptColor(sessionId: String, color: NSColor) {
        overlays[sessionId]?.setPromptColor(color)
    }

    func setHeartbeatColor(sessionId: String, color: NSColor) {
        overlays[sessionId]?.setHeartbeatColor(color)
    }

    /// Resolve Terminal/iTerm's real insertion-point rectangle. The AX range
    /// geometry is in global top-left coordinates; PromptGlyphFlight uses
    /// AppKit's bottom-left coordinates, so flip it against the desktop top.
    /// Some terminal versions omit parameterized range bounds; callers retain
    /// the visually safe last-line fallback for that case.
    private func cursorDestination(tty: String, screen: NSScreen) -> CGPoint? {
        let bare = (tty as NSString).lastPathComponent
        guard let wanted = binding[bare] else { return nil }
        let bundleIds = Set(["com.apple.Terminal", "com.googlecode.iterm2"])

        func insertionRect(in element: AXUIElement, depth: Int = 0) -> CGRect? {
            guard depth < 8 else { return nil }
            var roleRef: CFTypeRef?
            _ = AXUIElementCopyAttributeValue(element, kAXRoleAttribute as CFString, &roleRef)
            if (roleRef as? String) == kAXTextAreaRole as String {
                var rangeRef: CFTypeRef?
                if AXUIElementCopyAttributeValue(element, kAXSelectedTextRangeAttribute as CFString,
                                                 &rangeRef) == .success,
                   let rangeRef {
                    var boundsRef: CFTypeRef?
                    if AXUIElementCopyParameterizedAttributeValue(
                        element, kAXBoundsForRangeParameterizedAttribute as CFString,
                        rangeRef, &boundsRef) == .success,
                       let value = boundsRef as! AXValue?, AXValueGetType(value) == .cgRect {
                        var rect = CGRect.zero
                        if AXValueGetValue(value, .cgRect, &rect),
                           rect.width.isFinite, rect.height.isFinite,
                           rect.minX.isFinite, rect.minY.isFinite,
                           !rect.isNull, !rect.isInfinite { return rect }
                    }
                }
            }
            var childrenRef: CFTypeRef?
            guard AXUIElementCopyAttributeValue(element, kAXChildrenAttribute as CFString,
                                                &childrenRef) == .success,
                  let children = childrenRef as? [AXUIElement] else { return nil }
            for child in children {
                if let rect = insertionRect(in: child, depth: depth + 1) { return rect }
            }
            return nil
        }

        func elementRect(_ element: AXUIElement) -> CGRect? {
            var positionRef: CFTypeRef?
            var sizeRef: CFTypeRef?
            guard AXUIElementCopyAttributeValue(element, kAXPositionAttribute as CFString,
                                                &positionRef) == .success,
                  AXUIElementCopyAttributeValue(element, kAXSizeAttribute as CFString,
                                                &sizeRef) == .success,
                  let position = positionRef as! AXValue?,
                  let size = sizeRef as! AXValue? else { return nil }
            var origin = CGPoint.zero
            var dimensions = CGSize.zero
            guard AXValueGetValue(position, .cgPoint, &origin),
                  AXValueGetValue(size, .cgSize, &dimensions) else { return nil }
            return CGRect(origin: origin, size: dimensions)
        }

        /// Terminal text areas occasionally omit selected-range bounds while
        /// repainting. Read only the bound terminal window in that case and
        /// use its lowest OCR row as the visible prompt-line context instead
        /// of guessing from the desktop corner or a fixed bottom inset.
        func ocrPromptRow(windowID: CGWindowID, windowRect: CGRect) -> CGRect? {
            guard let image = CGWindowListCreateImage(.null, .optionIncludingWindow,
                                                      windowID, [.boundsIgnoreFraming])
            else { return nil }
            let request = VNRecognizeTextRequest()
            request.recognitionLevel = .fast
            request.usesLanguageCorrection = false
            request.minimumTextHeight = 0
            try? VNImageRequestHandler(cgImage: image, options: [:]).perform([request])
            let rows = (request.results ?? []).compactMap { observation -> CGRect? in
                guard observation.topCandidates(1).first != nil else { return nil }
                let box = observation.boundingBox
                let rect = CGRect(
                    x: windowRect.minX + box.minX * windowRect.width,
                    y: windowRect.minY + (1 - box.maxY) * windowRect.height,
                    width: box.width * windowRect.width,
                    height: box.height * windowRect.height)
                guard rect.midY > windowRect.minY + 24,
                      rect.midY < windowRect.maxY - 6 else { return nil }
                return rect
            }
            return rows.max(by: { $0.midY < $1.midY })
        }

        for app in NSWorkspace.shared.runningApplications
            where bundleIds.contains(app.bundleIdentifier ?? "") {
            let axApp = AXUIElementCreateApplication(app.processIdentifier)
            var windowsRef: CFTypeRef?
            guard AXUIElementCopyAttributeValue(axApp, kAXWindowsAttribute as CFString,
                                                &windowsRef) == .success,
                  let windows = windowsRef as? [AXUIElement] else { continue }
            for window in windows {
                var wid = CGWindowID(0)
                guard _AXUIElementGetWindow(window, &wid) == .success,
                      Int(wid) == wanted,
                      let windowRect = elementRect(window) else { continue }
                // Terminal occasionally reports a zero/origin range while its
                // screen buffer is repainting.  Reject it unless the insertion
                // point is actually inside this responding terminal; otherwise
                // the flight dives into the bottom corner of the desktop.
                let desktopTop = NSScreen.screens.map(\.frame.maxY).max() ?? screen.frame.maxY
                if let rect = insertionRect(in: window),
                   windowRect.insetBy(dx: -8, dy: -8).intersects(rect),
                   rect.maxX > windowRect.minX + 2,
                   rect.maxY > windowRect.minY + 2 {
                    // AX gives a top-left desktop rectangle. Preserve the
                    // insertion x and flip only y into AppKit coordinates.
                    return CGPoint(x: rect.minX,
                                   y: desktopTop - rect.maxY + rect.height * 0.5)
                }
                if let row = ocrPromptRow(windowID: wid, windowRect: windowRect) {
                    // OCR understands the visible terminal content: continue
                    // immediately after its lowest row, but never beyond the
                    // bound window's usable right edge.
                    return CGPoint(x: min(row.maxX + 5, windowRect.maxX - 120),
                                   y: desktopTop - row.maxY + row.height * 0.5)
                }
            }
        }
        return nil
    }

    /// Raise the terminal window already bound to this tty using Accessibility
    /// only. This avoids Apple Events/TCC while preserving exact-window focus.
    func focusTerminal(tty: String) -> Bool {
        let bare = (tty as NSString).lastPathComponent
        guard let wanted = binding[bare] else { return false }
        let bundleIds = Set(["com.apple.Terminal", "com.googlecode.iterm2"])
        for app in NSWorkspace.shared.runningApplications
            where bundleIds.contains(app.bundleIdentifier ?? "") {
            let axApp = AXUIElementCreateApplication(app.processIdentifier)
            var raw: CFTypeRef?
            guard AXUIElementCopyAttributeValue(axApp, kAXWindowsAttribute as CFString,
                                                &raw) == .success,
                  let windows = raw as? [AXUIElement] else { continue }
            for window in windows {
                var wid = CGWindowID(0)
                guard _AXUIElementGetWindow(window, &wid) == .success,
                      Int(wid) == wanted else { continue }
                _ = app.activate(options: [.activateIgnoringOtherApps])
                _ = AXUIElementSetAttributeValue(window, kAXMainAttribute as CFString,
                                                 kCFBooleanTrue)
                _ = AXUIElementSetAttributeValue(window, kAXFocusedAttribute as CFString,
                                                 kCFBooleanTrue)
                _ = AXUIElementPerformAction(window, kAXRaiseAction as CFString)
                return true
            }
        }
        return false
    }

    /// Reconcile the badge set with the live sessions. Off when `enabled` is
    /// false. Only sessions with a real local tty get a badge.
    func sync(sessions: [ClaudeSession], enabled: Bool) {
        guard enabled else { teardown(); return }
        installActivationObserverIfNeeded()
        installAXFocusObservers()
        installMouseMonitorsIfNeeded()
        installObservedObserverIfNeeded()

        let live = sessions.filter { !$0.tty.isEmpty && $0.remoteHost.isEmpty }
        let liveIds = Set(live.map { $0.sessionId })
        let loopIds = loopboySessions()
        let dark = AppDelegate.isDarkAppearance()
        var liveParticleTtys = Set<String>()

        // Recompute the global sun every 5 minutes — the sun moves slowly, and
        // each change re-renders every rock's sprite sheet, so we don't want to
        // pay that every minute. The whole wall re-lights together.
        let now = Date()
        let bucket = Int(now.timeIntervalSince1970 / 300)
        if bucket != sunMinute {
            sunMinute = bucket
            sun = Sun.light(at: now)
        }

        var membershipChanged = false
        for (sid, ov) in overlays where !liveIds.contains(sid) {
            ov.close(); overlays.removeValue(forKey: sid); membershipChanged = true
            if bubbleFor == sid {
                bubbleFor = nil; bubblePinned = false
                bubble.hide()
            }
        }
        for s in live {
            let bare = (s.tty as NSString).lastPathComponent
            liveParticleTtys.insert(bare)
            let ov: PromptSigilOverlay
            if let existing = overlays[s.sessionId], existing.tty == bare {
                ov = existing
            } else {
                overlays[s.sessionId]?.close()
                ov = PromptSigilOverlay(sessionId: s.sessionId, tty: bare)
                overlays[s.sessionId] = ov
                membershipChanged = true
            }
            // Seed from session id + prompt: the session id guarantees every
            // window a distinct rock even when prompts collide (trivial "..",
            // "yes", blank); the prompt makes the rock re-form as the session
            // moves to a new prompt.
            let seed = SigilRenderer.seed(for: s.sessionId + "\u{1}" + s.subject)
            // Re-render the sprite sheet only when the rock or the sun moved.
            let loopboy = loopIds.contains(s.sessionId)
            let key = "\(seed):\(dark):\(sunMinute):\(loopboy)"
            if ov.frameKey != key {
                ov.frameKey = key
                let (hx, e, inten) = (sun.hx, sun.elevation, sun.intensity)
                renderQueue.async { [weak ov] in
                    let hi = SigilRockFrames.render(
                        seed: seed, dark: dark, sunHx: hx, sunElevation: e,
                        sunIntensity: inten, gem: loopboy)
                    // Gems stay crisp and glass-like; ordinary rocks retain
                    // their deliberately chunky 30px pixel material.
                    let lo = loopboy ? hi : hi.map { SigilRockFrames.downsample($0, to: 30) }
                    DispatchQueue.main.async { ov?.setFrames(rock: lo, shadow: hi) }
                }
            }
            let (basePeriod, cw) = motion(for: s.state)
            let loopboyActive = loopboy && (s.state == .working || s.state == .rendering)
            let loopboyGlow = loopboyActive
                ? NSColor(deviceRed: 1.0, green: 0.72, blue: 0.08, alpha: 1.0)
                : NSColor(deviceRed: 1.0, green: 0.86, blue: 0.20, alpha: 1.0)
            ov.setMotion(period: loopboy ? basePeriod * 0.45 : basePeriod, clockwise: cw)
            let terminalThemeColor = statusColor(for: s.state, agentType: s.agentType)
            particleColors[bare] = terminalThemeColor
            ov.setShadowColor(loopboy
                ? loopboyGlow
                : terminalThemeColor)
            ov.setShining(loopboy, color: loopboyGlow)
            ov.setLoopboyStyle(loopboy, active: loopboyActive, pending: false, dark: dark)
            if loopboy {
                let phase: String
                if s.loopboyState == "responding" {
                    phase = "RESPONDING"
                } else if s.loopboyState == "reading" {
                    phase = "READING"
                } else {
                    switch s.state {
                    case .working, .rendering: phase = "WORKING"
                    case .awaiting: phase = "RESPONDING"
                    case .blank, .complete, .interrupted, .stale: phase = "IDLE"
                    }
                }
                ov.setLoopboyState(phase)
            }
            ov.setLighting(drop: sun.drop)
            // Name + hover copy. The name belongs to the session/thread and
            // stays fixed while the visual rock re-forms on a new prompt.
            // The bubble body prefers a cached
            // haiku-inferred sentence; until that lands it shows the hook
            // summary and prompt excerpt, deduped (the hook line is usually
            // the prompt's own first words — repeating both said nothing).
            ov.setName(SigilRenderer.name(for: s), dark: dark)
            ov.setPlatformTarget(s.platformTarget)
            let title = s.emoji.isEmpty ? ov.name : "\(s.emoji) \(ov.name)"
            ov.tooltipTitle = loopboy ? "↻ Loopboy · \(title)" : title
            let story = (s.loopboyResponse.isEmpty ? nil : s.loopboyResponse)
                ?? ProxMemoirs.shared.text(for: s.sessionId)
                ?? Self.fallbackBody(summary: s.titleString, subject: s.shortSubject)
            let metrics = Self.sessionMetrics(s)
            ov.tooltipBody = story
            ov.tooltipStatus = metrics.status
            ov.tooltipStats = metrics.stats
            // A pinned/open bubble matures in place when a memoir lands.
            if bubbleFor == s.sessionId {
                showBubble(for: ov)
            }
        }
        particleColors = particleColors.filter { liveParticleTtys.contains($0.key) }

        if membershipChanged { needsRebind = true }
        startTimerIfNeeded()
        reposition()
    }

    /// Bubble body when no inferred sentence is cached yet: hook summary and
    /// prompt excerpt, collapsed to one line whenever one contains the other.
    private static func fallbackBody(summary: String, subject: String) -> String {
        func norm(_ t: String) -> String {
            t.lowercased().replacingOccurrences(of: "…", with: "")
                .trimmingCharacters(in: .whitespacesAndNewlines)
        }
        let a = norm(summary), b = norm(subject)
        if b.isEmpty || a == b || a.contains(b) { return summary }
        if b.contains(a) || b.hasPrefix(a) { return subject }
        return summary + "\n" + subject
    }

    private static func sessionMetrics(_ s: ClaudeSession, now: Date = Date())
        -> (status: String, stats: String) {
        func duration(_ seconds: TimeInterval) -> String {
            let value = max(0, Int(seconds))
            if value < 90 { return "\(value)s" }
            if value < 90 * 60 { return "\(value / 60)m" }
            if value < 48 * 3600 { return "\(value / 3600)h \((value % 3600) / 60)m" }
            return "\(value / 86_400)d \((value % 86_400) / 3600)h"
        }
        let state: String
        switch s.state {
        case .working: state = "working"
        case .rendering: state = "rendering"
        case .awaiting: state = "awaiting input"
        case .interrupted: state = "interrupted"
        case .complete: state = "idle"
        case .blank: state = "blank"
        case .stale: state = "stale"
        }
        let up = duration(now.timeIntervalSince(s.started))
        let idle = duration(now.timeIntervalSince(s.updated))
        return (state, "UP \(up)  •  ACTIVE \(idle) AGO  •  \(s.agentLabel)")
    }

    /// Wake the reposition loop the instant a poke lands so the rock reacts
    /// now, not at the next lazy idle tick. Installed once.
    private var observedObserverInstalled = false
    private func installObservedObserverIfNeeded() {
        guard !observedObserverInstalled else { return }
        observedObserverInstalled = true
        NotificationCenter.default.addObserver(
            forName: LedgerStore.observedNote, object: nil, queue: .main
        ) { [weak self] _ in self?.scheduleTick(after: 0) }
    }

    private func teardown() {
        timer?.invalidate(); timer = nil
        for (_, ov) in overlays { ov.close() }
        overlays.removeAll()
        binding.removeAll()
        particleColors.removeAll()
        removeMouseMonitors()
        for (_, obs) in axObservers {
            CFRunLoopRemoveSource(CFRunLoopGetMain(), AXObserverGetRunLoopSource(obs), .commonModes)
        }
        axObservers.removeAll()
    }

    private func startTimerIfNeeded() {
        guard timer == nil, !overlays.isEmpty else { return }
        scheduleTick(after: idleInterval)
    }

    /// Self-rescheduling tick: reposition, then re-arm at the display rate when
    /// a tracked window moved recently (snappy drag-following) or at the idle
    /// rate otherwise (low energy when nothing's moving).
    private func scheduleTick(after interval: TimeInterval) {
        currentInterval = interval
        timer?.invalidate()
        let t = Timer(timeInterval: max(0, interval), repeats: false) { [weak self] _ in
            self?.tick()
        }
        timer = t
        RunLoop.main.add(t, forMode: .common)
    }

    private func tick() {
        let now = Date()
        // Clamp dt so a long idle gap (or first tick) doesn't teleport the
        // spring — it should always ease, never jump.
        let dt = CGFloat(min(0.1, max(0.001, now.timeIntervalSince(lastTick))))
        lastTick = now

        reposition()                       // refresh targets + z-order
        // Global mouse monitors can disappear when macOS revisits Input
        // Monitoring/TCC after a signed app update. Sampling on this already-
        // running low-cost tick makes hover self-healing without a new timer.
        handleMouseMoved()
        guard !overlays.isEmpty else { timer = nil; return }
        var settling = false
        var anyObserved = false
        for (sid, ov) in overlays {
            if ov.advance(dt: dt) { settling = true }
            ov.updateHeartbeatCountdown(now: now)
            // "Being read" reaction — on while the poke window is live, off once
            // it decays. Cheap dict lookup; the blink/shake/spin run server-side.
            if let obs = LedgerStore.shared.observation(for: sid) {
                if ov.setObserved(true) { NSLog("🪨 [ledger] \(ov.name) reacting — read by \(obs.by)") }
                anyObserved = true
            } else {
                ov.setObserved(false)
            }
        }
        // Stay at display rate while a window moved recently, a badge is still
        // catching up, or a rock is reacting to being observed.
        let active = now < motionDeadline || settling || anyObserved
        scheduleTick(after: active ? activeInterval : idleInterval)
    }

    /// In-process snapshot of the on-screen window stack. `terminals` maps
    /// each Terminal/iTerm2 window's CGWindowID to its bounds {x,y,w,h};
    /// `stack` is EVERY normal-level window front-to-back — what the pointer
    /// occlusion check walks to find the topmost window at a rock's spot.
    /// Rock windows belong to this layer too, but are mouse-transparent and
    /// are ignored below by window number when appropriate. No fork.
    private func snapshotWindows()
        -> (terminals: [Int: (CGFloat, CGFloat, CGFloat, CGFloat)], stack: [(num: Int, rect: CGRect)]) {
        let pids = Set(NSWorkspace.shared.runningApplications
            .filter { Self.terminalBundleIds.contains($0.bundleIdentifier ?? "") }
            .map { $0.processIdentifier })
        guard let infos = CGWindowListCopyWindowInfo([.optionOnScreenOnly], kCGNullWindowID) as? [[String: Any]]
        else { return ([:], []) }
        var terminals: [Int: (CGFloat, CGFloat, CGFloat, CGFloat)] = [:]
        var stack: [(num: Int, rect: CGRect)] = []
        for info in infos {
            guard let layer = info[kCGWindowLayer as String] as? Int, layer == 0,
                  let num = info[kCGWindowNumber as String] as? Int,
                  let b = info[kCGWindowBounds as String] as? [String: CGFloat],
                  let x = b["X"], let y = b["Y"], let w = b["Width"], let h = b["Height"]
            else { continue }
            let ownerPid = info[kCGWindowOwnerPID as String] as? pid_t
            // The normal-level heartbeat fuse belongs to this process. If it
            // participates in the occlusion stack, it covers its own bound
            // Terminal on one tick, gets hidden, then reappears on the next —
            // an exact 5 Hz blink loop. Only external normal windows can
            // occlude prompt overlays.
            if ownerPid != getpid() {
                stack.append((num, CGRect(x: x, y: y, width: w, height: h)))
            }
            if let pid = ownerPid, pids.contains(pid) {
                terminals[num] = (x, y, w, h)
            }
        }
        return (terminals, stack)
    }

    /// Reposition every badge from the in-process window snapshot. Detects
    /// window motion (to drive the adaptive cadence) and triggers a throttled
    /// rebind when bindings go stale.
    private func reposition() {
        guard !overlays.isEmpty else { timer?.invalidate(); timer = nil; return }
        let snap = snapshotWindows()
        lastStack = snap.stack        // the hover hit-test reads this between ticks
        let screenH = NSScreen.main?.frame.height ?? 0
        var seen: [Int: (CGFloat, CGFloat, CGFloat, CGFloat)] = [:]
        for (_, ov) in overlays {
            guard let num = binding[ov.tty], let b = snap.terminals[num] else {
                ov.hide(); dropInteraction(for: ov); continue
            }
            ov.place(bounds: b, screenHeight: screenH)
            // The embedded illusion: the badge floats above everything, but
            // only SHOWS while its terminal is the topmost window at the
            // rock's spot — covered corner ⇒ the rock hides with its window,
            // exactly as if it were drawn inside it.
            let p = ov.rockPoint(bounds: b)
            let top = snap.stack.first(where: { $0.rect.contains(p) })?.num
            let visible = (top == nil || top == num)
            ov.setVisible(visible, above: num)
            if !visible { dropInteraction(for: ov) }
            seen[num] = b
            if let prev = lastBoundsByNum[num], prev != b {
                // A tracked window moved/resized — hold display rate for a short
                // tail so the whole drag tracks tightly.
                motionDeadline = Date().addingTimeInterval(0.3)
            }
        }
        lastBoundsByNum = seen
        // Share the prompt-rock controller's adaptive movement cadence with the
        // under-window glow. During a drag this runs at display rate; at rest it
        // becomes only the cheap 5 Hz safety check.
        PromptFocusHighlight.shared.refreshNow()
        // Rebind ONLY on a membership change (one-shot) or the slow safety
        // cadence — never per tick, so osascript stays capped at ~once / 5s.
        if (needsRebind || Date().timeIntervalSince(lastBind) > 5), !bindInFlight {
            rebind()
        }
    }

    /// Establish tty → CGWindowID. osascript gives tty → window bounds (the
    /// only API that knows a tab's tty); we then match those bounds to the
    /// in-process CGWindowList snapshot to recover each window's CGWindowID.
    /// Runs off-main (osascript blocks) and is the sole forking path — gated to
    /// at most one in flight and a slow cadence.
    private func rebind() {
        bindInFlight = true
        lastBind = Date()
        needsRebind = false
        let running = Set(NSWorkspace.shared.runningApplications.compactMap { $0.bundleIdentifier })
        let wantTerminal = running.contains("com.apple.Terminal")
        let wantIterm = running.contains("com.googlecode.iterm2")
        guard wantTerminal || wantIterm else { bindInFlight = false; return }
        let script = boundsScript(terminal: wantTerminal, iterm: wantIterm)

        DispatchQueue.global(qos: .userInitiated).async { [weak self] in
            // Execute in-process. Child `osascript` processes inherit this
            // LSUIElement app as their responsible process and can stall at
            // the Apple Events handoff even when Slab already has permission,
            // leaving every rock off-screen until a four-second timeout. A
            // fresh NSAppleScript on this serial bind path uses Slab's own
            // granted identity and avoids both the fork and that deadlock.
            var appleError: NSDictionary?
            let descriptor = NSAppleScript(source: script)?
                .executeAndReturnError(&appleError)
            let result: (status: Int32, output: String)
            if let appleError {
                result = (-3, appleError.description)
            } else {
                result = (0, descriptor?.stringValue ?? "")
            }
            var ttyBounds: [String: (CGFloat, CGFloat, CGFloat, CGFloat)] = [:]
            for line in result.output.split(separator: "\n") {
                let parts = line.split(separator: "|")
                guard parts.count == 2 else { continue }
                let dev = (parts[0].trimmingCharacters(in: .whitespaces) as NSString).lastPathComponent
                let nums = parts[1].split(separator: ",").compactMap {
                    Double($0.trimmingCharacters(in: .whitespaces)).map { CGFloat($0) }
                }
                guard nums.count == 4 else { continue }
                // osascript bounds {l,t,r,b} → {x,y,w,h}.
                ttyBounds[dev] = (nums[0], nums[1], nums[2] - nums[0], nums[3] - nums[1])
            }
            if result.status != 0 {
                NSLog("🪨 [bind] terminal probe failed status=\(result.status): \(result.output.prefix(240))")
            } else if ttyBounds.isEmpty {
                NSLog("🪨 [bind] terminal probe returned no tty bounds")
            }
            DispatchQueue.main.async {
                guard let self = self else { return }
                // Match against a fresh snapshot (windows may have moved during
                // the osascript round-trip).
                let wins = self.snapshotWindows().terminals
                var newBinding: [String: Int] = [:]
                for (tty, tb) in ttyBounds {
                    if let hit = wins.first(where: { Self.boundsMatch($0.value, tb) }) {
                        newBinding[tty] = hit.key
                    }
                }
                // Never let a transient AppleScript timeout erase a healthy
                // wall. Retain bindings whose tty + CG window are both still
                // live, then merge any fresh matches over them. A failed probe
                // therefore costs one delayed rebind, not five seconds where
                // every rock (and its hover card) disappears.
                let liveTtys = Set(self.overlays.values.map(\.tty))
                var merged = self.binding.filter {
                    liveTtys.contains($0.key) && wins[$0.value] != nil
                }
                if result.status == 0 {
                    merged.merge(newBinding) { _, fresh in fresh }
                }
                self.binding = merged
                self.bindInFlight = false
                self.reposition()
            }
        }
    }

    /// Bounds match within a couple of points (osascript ints vs CGWindow
    /// floats; the occasional 1px rounding).
    private static func boundsMatch(
        _ a: (CGFloat, CGFloat, CGFloat, CGFloat), _ b: (CGFloat, CGFloat, CGFloat, CGFloat)
    ) -> Bool {
        abs(a.0 - b.0) <= 2 && abs(a.1 - b.1) <= 2 && abs(a.2 - b.2) <= 2 && abs(a.3 - b.3) <= 2
    }

    /// AppleScript that emits `<dev-tty>|l,t,r,b` per tab/session for the
    /// running terminals only (never launches a closed one).
    private func boundsScript(terminal: Bool, iterm: Bool) -> String {
        var s = "set out to \"\"\n"
        if terminal {
            s += """
            tell application "Terminal"
                repeat with w in windows
                    try
                        set b to bounds of w
                        set bs to ((item 1 of b) as text) & "," & ((item 2 of b) as text) & "," & ((item 3 of b) as text) & "," & ((item 4 of b) as text)
                        repeat with t in tabs of w
                            try
                                set out to out & (tty of t) & "|" & bs & linefeed
                            end try
                        end repeat
                    end try
                end repeat
            end tell

            """
        }
        if iterm {
            s += """
            tell application "iTerm2"
                repeat with w in windows
                    try
                        set p to position of w
                        set sz to size of w
                        set lx to item 1 of p
                        set ty to item 2 of p
                        set rx to lx + (item 1 of sz)
                        set by to ty + (item 2 of sz)
                        set bs to (lx as text) & "," & (ty as text) & "," & (rx as text) & "," & (by as text)
                        repeat with t in tabs of w
                            repeat with ss in sessions of t
                                try
                                    set out to out & (tty of ss) & "|" & bs & linefeed
                                end try
                            end repeat
                        end repeat
                    end try
                end repeat
            end tell

            """
        }
        s += "return out"
        return s
    }
}
