import AppKit
import ScreenSaver

/// Fullscreen slab status screensaver — geometry only.
///
/// The whole canvas is the menubar motif at scale: a slowly rotating regular
/// polygon with one edge per live Claude session, each edge colored by that
/// session's state (green working / amber-pulse awaiting / slate complete /
/// gray blank / red-blink stale). N=1 is a single bar, N=2 two parallel bars,
/// N≥3 the matching n-gon. The background and the edge palette follow the
/// system light/dark appearance (and slab's force-bright override), so the
/// saver reads the same status colors as the themed terminals. No wordmark,
/// no status line, no metrics, no clock — just the shape. No subprocesses, no
/// disk writes — safe in the sandboxed `legacyScreenSaver` host.
@objc(SlabSaverView)
final class SlabSaverView: ScreenSaverView {

    private var status = SlabStatus()
    private var phase: CGFloat = 0          // seconds, drives pulses
    private var rotation: CGFloat = 0       // radians
    private var frame_: Int = 0

    override init?(frame: NSRect, isPreview: Bool) {
        super.init(frame: frame, isPreview: isPreview)
        animationTimeInterval = 1.0 / 30.0
        reload()
    }

    required init?(coder: NSCoder) {
        super.init(coder: coder)
        animationTimeInterval = 1.0 / 30.0
        reload()
    }

    override var isOpaque: Bool { true }
    override var hasConfigureSheet: Bool { false }
    override var configureSheet: NSWindow? { nil }

    override func startAnimation() { super.startAnimation() }
    override func stopAnimation() { super.stopAnimation() }

    private func reload() { status = SlabStatus.gather() }

    override func animateOneFrame() {
        let dt = CGFloat(animationTimeInterval)
        phase += dt

        // Rotate gently while any thread is alive; quicker the more are
        // waiting on you. Frozen (just breathing) when nothing's live.
        let speed: CGFloat = status.anyLive
            ? 0.06 + 0.06 * CGFloat(status.awaiting)
            : 0
        rotation += speed * dt

        frame_ += 1
        if frame_ % 30 == 0 { reload() }   // ~1s

        setNeedsDisplay(bounds)
    }

    // MARK: - Appearance

    /// Effective dark-mode decision: the macOS appearance unless slab's
    /// force-bright override is set on disk, in which case we pin to the light
    /// palette — mirroring the menubar's `effectiveDark()` so the saver, the
    /// icon, and the themed terminals all agree.
    private func effectiveDark() -> Bool {
        let flag = "\(RealHome.path)/.local/share/slab/state/force-bright"
        if FileManager.default.fileExists(atPath: flag) { return false }
        let match = effectiveAppearance.bestMatch(from: [.darkAqua, .aqua])
        return match == .darkAqua
    }

    // MARK: - Render

    override func draw(_ rect: NSRect) {
        let dark = effectiveDark()
        (dark ? NSColor(deviceWhite: 0.035, alpha: 1)
              : NSColor(deviceWhite: 0.93, alpha: 1)).setFill()
        rect.fill()
        drawPolygon(dark: dark)
    }

    // MARK: polygon (one edge per session)

    private let maxSides = 12

    private func drawPolygon(dark: Bool) {
        let w = bounds.width, h = bounds.height
        let cx = w / 2, cy = h / 2
        let radius = min(w, h) * 0.32
        let lineWidth = max(3, radius * 0.04)

        let sessions = status.sessions
        guard !sessions.isEmpty else {
            // Idle: a single slow-breathing ring so the canvas isn't dead.
            let breathe = 0.5 + 0.5 * cos(phase * 0.9)
            let c = dark
                ? NSColor(deviceHue: 0.58, saturation: 0.22,
                          brightness: 0.32 + 0.20 * breathe, alpha: 1)
                : NSColor(deviceHue: 0.58, saturation: 0.30,
                          brightness: 0.72 - 0.18 * breathe, alpha: 1)
            let ring = NSBezierPath(ovalIn: NSRect(
                x: cx - radius, y: cy - radius,
                width: radius * 2, height: radius * 2))
            ring.lineWidth = lineWidth
            c.setStroke(); ring.stroke()
            return
        }

        let visible = Array(sessions.prefix(maxSides))
        let n = visible.count
        let overflow = sessions.count - n

        if n == 1 {
            segment(center: NSPoint(x: cx, y: cy), length: 2 * radius,
                    angle: rotation, color: edgeColor(visible[0].state, dark: dark),
                    width: lineWidth)
        } else if n == 2 {
            let half = radius * 0.36
            let length = radius * 1.8
            let nx = -sin(rotation), ny = cos(rotation)
            for i in 0..<2 {
                let s: CGFloat = i == 0 ? 1 : -1
                segment(center: NSPoint(x: cx + nx * half * s, y: cy + ny * half * s),
                        length: length, angle: rotation,
                        color: edgeColor(visible[i].state, dark: dark), width: lineWidth)
            }
        } else {
            var verts: [NSPoint] = []
            for k in 0..<n {
                let theta = -CGFloat.pi / 2 - CGFloat.pi / CGFloat(n)
                    + 2 * .pi * CGFloat(k) / CGFloat(n) + rotation
                verts.append(NSPoint(x: cx + radius * cos(theta),
                                     y: cy + radius * sin(theta)))
            }
            for k in 0..<n {
                let p = NSBezierPath()
                p.move(to: verts[k])
                p.line(to: verts[(k + 1) % n])
                p.lineWidth = lineWidth
                p.lineCapStyle = .round
                edgeColor(visible[k].state, dark: dark).setStroke()
                p.stroke()
            }
        }

        if overflow > 0 {
            text("+\(overflow)",
                 at: NSPoint(x: cx, y: cy),
                 size: radius * 0.22, weight: .semibold,
                 color: dark ? NSColor(deviceWhite: 0.7, alpha: 1)
                             : NSColor(deviceWhite: 0.3, alpha: 1), center: true)
        }
    }

    private func segment(center: NSPoint, length: CGFloat, angle: CGFloat,
                         color: NSColor, width: CGFloat) {
        let dx = cos(angle) * length / 2, dy = sin(angle) * length / 2
        let p = NSBezierPath()
        p.move(to: NSPoint(x: center.x - dx, y: center.y - dy))
        p.line(to: NSPoint(x: center.x + dx, y: center.y + dy))
        p.lineWidth = width
        p.lineCapStyle = .round
        color.setStroke(); p.stroke()
    }

    /// Per-session edge color, sourced from the same per-status terminal
    /// palette slab pushes via theme-by-status (the cursor channel — its most
    /// saturated accent), switched by appearance. Kept in sync with the
    /// menubar's `AppDelegate.statusDecor`; attention states animate: awaiting
    /// throbs brighter, stale fades toward the page.
    private func edgeColor(_ st: SlabSessionState, dark: Bool) -> NSColor {
        let c: (Double, Double, Double)
        switch st {
        case .blank:    c = dark ? (50000, 50000, 50000) : (20000, 20000, 20000)
        case .working:  c = dark ? (24000, 56000, 34000) : (4000, 42000, 15000)
        case .complete: c = dark ? (32000, 42000, 57000) : (15000, 25000, 52000)
        case .awaiting: c = dark ? (65535, 46000, 10000) : (65535, 35000, 0)
        case .stale:    c = dark ? (65535, 20000, 20000) : (60000, 4000, 7000)
        }
        let base = NSColor(deviceRed: CGFloat(c.0) / 65535,
                           green: CGFloat(c.1) / 65535,
                           blue: CGFloat(c.2) / 65535, alpha: 1)
        switch st {
        case .awaiting:
            let pulse = 0.5 + 0.5 * cos(phase * .pi * 4)
            return base.blended(withFraction: 0.30 * pulse, of: .white) ?? base
        case .stale:
            let blink = 0.5 + 0.5 * cos(phase * .pi * 2)
            let page = dark ? NSColor(deviceWhite: 0.10, alpha: 1)
                            : NSColor(deviceWhite: 0.92, alpha: 1)
            return base.blended(withFraction: (1 - blink) * 0.5, of: page) ?? base
        default:
            return base
        }
    }

    // MARK: text helper (only the overflow "+N" badge)

    private func text(_ s: String, at: NSPoint, size: CGFloat,
                      weight: NSFont.Weight, color: NSColor, center: Bool = false) {
        let attrs: [NSAttributedString.Key: Any] = [
            .font: NSFont.monospacedSystemFont(ofSize: size, weight: weight),
            .foregroundColor: color,
        ]
        let ns = s as NSString
        let sz = ns.size(withAttributes: attrs)
        var origin = NSPoint(x: at.x, y: at.y - sz.height / 2)
        if center { origin.x = at.x - sz.width / 2 }
        ns.draw(at: origin, withAttributes: attrs)
    }
}
