import AppKit
import ScreenSaver

/// Fullscreen slab status screensaver.
///
/// Centerpiece is the same motif as the menubar icon: a slowly rotating
/// regular polygon with one edge per live Claude session, each edge colored
/// by that session's state (green working / amber-pulse awaiting / slate
/// complete / gray blank / dim-blink stale). N=1 is a single bar, N=2 two
/// parallel bars, N≥3 the matching n-gon. Around it: an abstract numeric
/// breakdown of the prompts and a system-resource readout (CPU, memory,
/// load, uptime). No subprocesses, no disk writes — safe in the sandboxed
/// `legacyScreenSaver` host.
@objc(SlabSaverView)
final class SlabSaverView: ScreenSaverView {

    private let metrics = SystemMetrics()
    private var status = SlabStatus()
    private var sample = SystemMetrics.Sample()
    private var phase: CGFloat = 0          // seconds, drives pulses
    private var rotation: CGFloat = 0       // radians
    private var frame_: Int = 0

    private let clockFmt: DateFormatter = {
        let f = DateFormatter(); f.dateFormat = "HH:mm:ss"; return f
    }()
    private let dateFmt: DateFormatter = {
        let f = DateFormatter(); f.dateFormat = "EEE d MMM"; return f
    }()

    private let bg = NSColor(deviceWhite: 0.035, alpha: 1)

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

    private func reload() {
        status = SlabStatus.gather()
        sample = metrics.sample()
    }

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
        if frame_ % 30 == 0 { reload() }   // ~1s; also the CPU delta window

        setNeedsDisplay(bounds)
    }

    // MARK: - Render

    override func draw(_ rect: NSRect) {
        bg.setFill()
        rect.fill()

        let w = bounds.width, h = bounds.height
        let f = max(0.35, min(w / 1680.0, h / 1050.0))   // global scale

        drawHeader(w: w, h: h, f: f)
        drawPolygon(w: w, h: h, f: f)
        drawStatusBlock(w: w, h: h, f: f)
        drawMetrics(w: w, h: h, f: f)
        drawClock(w: w, h: h, f: f)
    }

    // MARK: header (wordmark + host)

    private func drawHeader(w: CGFloat, h: CGFloat, f: CGFloat) {
        let host = Host.current().localizedName ?? ProcessInfo.processInfo.hostName
        text("slab",
             at: NSPoint(x: w / 2, y: h - 64 * f),
             size: 30 * f, weight: .bold,
             color: NSColor(deviceWhite: 0.82, alpha: 1), center: true)
        text(host,
             at: NSPoint(x: w / 2, y: h - 92 * f),
             size: 13 * f, weight: .regular,
             color: NSColor(deviceWhite: 0.40, alpha: 1), center: true)
    }

    // MARK: polygon (one edge per session)

    private let maxSides = 12

    private func drawPolygon(w: CGFloat, h: CGFloat, f: CGFloat) {
        let cx = w / 2, cy = h * 0.54
        let radius = min(w, h) * 0.21
        let lineWidth = max(2.5, radius * 0.035)

        let sessions = status.sessions
        guard !sessions.isEmpty else {
            // Idle: a single slow-breathing ring so the canvas isn't dead.
            let breathe = 0.5 + 0.5 * cos(phase * 0.9)
            let c = NSColor(deviceHue: 0.58, saturation: 0.22,
                            brightness: 0.32 + 0.20 * breathe, alpha: 1)
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
                    angle: rotation, color: color(visible[0].state),
                    width: lineWidth)
        } else if n == 2 {
            let half = radius * 0.36
            let length = radius * 1.8
            let nx = -sin(rotation), ny = cos(rotation)
            for i in 0..<2 {
                let s: CGFloat = i == 0 ? 1 : -1
                segment(center: NSPoint(x: cx + nx * half * s, y: cy + ny * half * s),
                        length: length, angle: rotation,
                        color: color(visible[i].state), width: lineWidth)
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
                color(visible[k].state).setStroke()
                p.stroke()
            }
        }

        if overflow > 0 {
            text("+\(overflow)",
                 at: NSPoint(x: cx, y: cy),
                 size: radius * 0.22, weight: .semibold,
                 color: NSColor(deviceWhite: 0.7, alpha: 1), center: true)
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

    /// Same palette as the menubar's `IconRenderer.sessionColor`.
    private func color(_ st: SlabSessionState) -> NSColor {
        switch st {
        case .blank:
            return NSColor(deviceWhite: 0.55, alpha: 1)
        case .working:
            return NSColor(deviceHue: 0.33, saturation: 0.70, brightness: 0.78, alpha: 1)
        case .complete:
            return NSColor(deviceHue: 0.58, saturation: 0.30, brightness: 0.70, alpha: 1)
        case .awaiting:
            let pulse = 0.5 + 0.5 * cos(phase * .pi * 4)
            return NSColor(deviceHue: 0.10, saturation: 0.95,
                           brightness: 0.70 + 0.30 * pulse, alpha: 1)
        case .stale:
            let blink = 0.5 + 0.5 * cos(phase * .pi * 2)
            return NSColor(deviceWhite: 0.28 + 0.32 * blink, alpha: 1)
        }
    }

    // MARK: status line + abstract breakdown

    private func drawStatusBlock(w: CGFloat, h: CGFloat, f: CGFloat) {
        let cy = h * 0.54
        let radius = min(w, h) * 0.21
        let baseY = cy - radius - 56 * f

        let line = status.statusLine
        let lineColor: NSColor =
            status.anyAwaiting ? color(.awaiting)
            : status.anyLive   ? color(.working)
            : NSColor(deviceWhite: 0.45, alpha: 1)
        text(line, at: NSPoint(x: w / 2, y: baseY),
             size: 40 * f, weight: .semibold, color: lineColor, center: true)

        // Abstract breakdown: colored count chips, only the non-zero ones.
        var chips: [(String, NSColor)] = []
        if status.working  > 0 { chips.append(("\(status.working) working",  color(.working)))  }
        if status.awaiting > 0 { chips.append(("\(status.awaiting) awaiting", color(.awaiting))) }
        if status.complete > 0 { chips.append(("\(status.complete) complete", color(.complete))) }
        if status.blank    > 0 { chips.append(("\(status.blank) blank",       color(.blank)))    }
        if status.stale    > 0 { chips.append(("\(status.stale) stale",       color(.stale)))    }
        if status.activeSubagents > 0 {
            chips.append(("+\(status.activeSubagents) subagents",
                          NSColor(deviceWhite: 0.55, alpha: 1)))
        }

        if !chips.isEmpty {
            let size = 16 * f, gap = 26 * f
            let attrs = monoAttrs(size: size, weight: .medium, color: .white)
            let widths = chips.map {
                ($0.0 as NSString).size(withAttributes: attrs).width
            }
            let totalW = widths.reduce(0, +) + gap * CGFloat(chips.count - 1)
            var x = w / 2 - totalW / 2
            let y = baseY - 34 * f
            for (i, chip) in chips.enumerated() {
                text(chip.0, at: NSPoint(x: x, y: y), size: size,
                     weight: .medium, color: chip.1, center: false)
                x += widths[i] + gap
            }
        }

        // Mode tags (ambient / muted), faint, just under the breakdown.
        var tags: [String] = []
        if status.ambientActive { tags.append("ambient") }
        if status.muted { tags.append("muted") }
        if !tags.isEmpty {
            text(tags.joined(separator: "  ·  "),
                 at: NSPoint(x: w / 2, y: baseY - 60 * f),
                 size: 13 * f, weight: .regular,
                 color: NSColor(deviceWhite: 0.38, alpha: 1), center: true)
        }
    }

    // MARK: system resources (bottom-left)

    private func drawMetrics(w: CGFloat, h: CGFloat, f: CGFloat) {
        let x = 56 * f
        let barW = max(220 * f, w * 0.20)
        var y = 150 * f
        let dim = NSColor(deviceWhite: 0.42, alpha: 1)
        let val = NSColor(deviceWhite: 0.78, alpha: 1)

        // LOAD
        let (l1, l5, l15) = sample.load
        text("LOAD", at: NSPoint(x: x, y: y), size: 13 * f,
             weight: .semibold, color: dim, center: false)
        text(String(format: "%.2f  %.2f  %.2f", l1, l5, l15),
             at: NSPoint(x: x + 64 * f, y: y), size: 14 * f,
             weight: .regular, color: val, center: false)
        y += 40 * f

        // MEM
        let memTxt = "\(SystemMetrics.formatBytes(sample.memUsed)) / "
            + "\(SystemMetrics.formatBytes(sample.memTotal)) GB"
        labeledBar(label: "MEM", value: memTxt,
                   fraction: sample.memFraction,
                   color: NSColor(deviceHue: 0.58, saturation: 0.45,
                                  brightness: 0.78, alpha: 1),
                   x: x, y: y, barW: barW, f: f)
        y += 56 * f

        // CPU + per-core ticks
        labeledBar(label: "CPU",
                   value: "\(Int(round(sample.cpuBusy * 100)))%",
                   fraction: sample.cpuBusy,
                   color: cpuColor(sample.cpuBusy),
                   x: x, y: y, barW: barW, f: f)
        if !sample.perCore.isEmpty {
            let cores = sample.perCore
            let cw = max(3 * f, (barW - CGFloat(cores.count - 1) * 2 * f)
                                 / CGFloat(cores.count))
            let maxH = 26 * f
            let baseY = y + 26 * f
            var cxp = x
            for v in cores {
                let bh = max(1.5 * f, CGFloat(v) * maxH)
                NSColor(deviceWhite: 0.16, alpha: 1).setFill()
                NSBezierPath(rect: NSRect(x: cxp, y: baseY, width: cw, height: maxH)).fill()
                cpuColor(v).setFill()
                NSBezierPath(rect: NSRect(x: cxp, y: baseY, width: cw, height: bh)).fill()
                cxp += cw + 2 * f
            }
        }
    }

    private func cpuColor(_ v: Double) -> NSColor {
        // green → amber → red as load climbs.
        let hue = 0.33 - 0.33 * CGFloat(min(1, max(0, v)))
        return NSColor(deviceHue: hue, saturation: 0.75, brightness: 0.82, alpha: 1)
    }

    private func labeledBar(label: String, value: String, fraction: Double,
                            color: NSColor, x: CGFloat, y: CGFloat,
                            barW: CGFloat, f: CGFloat) {
        let dim = NSColor(deviceWhite: 0.42, alpha: 1)
        let val = NSColor(deviceWhite: 0.78, alpha: 1)
        text(label, at: NSPoint(x: x, y: y), size: 13 * f,
             weight: .semibold, color: dim, center: false)
        text(value, at: NSPoint(x: x + 64 * f, y: y), size: 14 * f,
             weight: .regular, color: val, center: false)
        let barY = y - 16 * f, barH = 8 * f
        let track = NSBezierPath(roundedRect:
            NSRect(x: x, y: barY, width: barW, height: barH),
            xRadius: barH / 2, yRadius: barH / 2)
        NSColor(deviceWhite: 0.15, alpha: 1).setFill(); track.fill()
        let fw = max(barH, barW * CGFloat(min(1, max(0, fraction))))
        let fill = NSBezierPath(roundedRect:
            NSRect(x: x, y: barY, width: fw, height: barH),
            xRadius: barH / 2, yRadius: barH / 2)
        color.setFill(); fill.fill()
    }

    // MARK: clock + uptime (bottom-right)

    private func drawClock(w: CGFloat, h: CGFloat, f: CGFloat) {
        let now = Date()
        let rx = w - 56 * f
        text(clockFmt.string(from: now),
             at: NSPoint(x: rx, y: 150 * f), size: 34 * f,
             weight: .semibold, color: NSColor(deviceWhite: 0.80, alpha: 1),
             rightAlign: true)
        text(dateFmt.string(from: now),
             at: NSPoint(x: rx, y: 124 * f), size: 14 * f,
             weight: .regular, color: NSColor(deviceWhite: 0.42, alpha: 1),
             rightAlign: true)
        text("up \(SystemMetrics.formatUptime(sample.uptime))",
             at: NSPoint(x: rx, y: 100 * f), size: 13 * f,
             weight: .regular, color: NSColor(deviceWhite: 0.34, alpha: 1),
             rightAlign: true)
    }

    // MARK: text helpers

    private func monoAttrs(size: CGFloat, weight: NSFont.Weight,
                           color: NSColor) -> [NSAttributedString.Key: Any] {
        [.font: NSFont.monospacedSystemFont(ofSize: size, weight: weight),
         .foregroundColor: color]
    }

    /// Draw mono text. `center` horizontally-centers on `at`; `rightAlign`
    /// right-justifies to `at.x`; otherwise `at` is the left baseline-ish
    /// origin. `at.y` is treated as a vertical center.
    private func text(_ s: String, at: NSPoint, size: CGFloat,
                      weight: NSFont.Weight, color: NSColor,
                      center: Bool = false, rightAlign: Bool = false) {
        let attrs = monoAttrs(size: size, weight: weight, color: color)
        let ns = s as NSString
        let sz = ns.size(withAttributes: attrs)
        var origin = NSPoint(x: at.x, y: at.y - sz.height / 2)
        if center { origin.x = at.x - sz.width / 2 }
        else if rightAlign { origin.x = at.x - sz.width }
        ns.draw(at: origin, withAttributes: attrs)
    }
}
