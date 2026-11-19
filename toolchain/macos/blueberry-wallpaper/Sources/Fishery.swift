import AppKit
import QuartzCore
import SceneKit
import AVFoundation
import ImageIO

/// Text that carries its own halo. A Core Animation layer shadow with no
/// shadow path is blurred again from the layer's alpha on every composite;
/// with a hundred labels riding fish that cost the display half its frames.
/// Here the halo is drawn once, into the layer's bitmap, with the glyphs.
final class HaloTextLayer: CATextLayer {
    var halo: CGColor? {
        didSet { if halo != oldValue { setNeedsDisplay() } }
    }
    var haloRadius: CGFloat = 1.5
    /// Table rules drawn under the text: a line between rows and one
    /// between the key and value columns. `gridRows` 0 draws none.
    var gridRows = 0
    var gridRowStep: CGFloat = 0
    var gridColumnX: CGFloat = 0
    var gridColor: CGColor?
    var gridGap: CGFloat = 0
    /// Table cells drawn directly with CoreText, one row per pair, so no
    /// tab or newline ever reaches the font (Unifont draws them as glyphs).
    var cells: [(NSAttributedString, NSAttributedString)] = [] {
        didSet { setNeedsDisplay() }
    }
    var cellLeft: CGFloat = 0
    var cellValueX: CGFloat = 0
    var cellLineHeight: CGFloat = 0
    var cellAscent: CGFloat = 0
    /// Bitmap type (Unifont): no antialiasing, nearest-neighbour scaling.
    var pixelated = false {
        didSet {
            magnificationFilter = pixelated ? .nearest : .linear
            minificationFilter = pixelated ? .nearest : .linear
            setNeedsDisplay()
        }
    }
    /// Pushes the text down from the top edge (CATextLayer has no inset).
    var topInset: CGFloat = 0 {
        didSet { if topInset != oldValue { setNeedsDisplay() } }
    }

    override init() { super.init() }
    override init(layer: Any) {
        super.init(layer: layer)
        if let other = layer as? HaloTextLayer {
            halo = other.halo
            haloRadius = other.haloRadius
            topInset = other.topInset
            gridRows = other.gridRows
            gridRowStep = other.gridRowStep
            gridColumnX = other.gridColumnX
            gridColor = other.gridColor
            gridGap = other.gridGap
            pixelated = other.pixelated
            cells = other.cells
            cellLeft = other.cellLeft
            cellValueX = other.cellValueX
            cellLineHeight = other.cellLineHeight
            cellAscent = other.cellAscent
        }
    }
    required init?(coder: NSCoder) { nil }

    override func draw(in ctx: CGContext) {
        if pixelated {
            ctx.setShouldAntialias(false)
            ctx.setAllowsFontSmoothing(false)
            ctx.interpolationQuality = .none
        }
        if pixelated, let halo, gridRows > 0 || !cells.isEmpty {
            ctx.setShadow(offset: CGSize(width: 1, height: -1), blur: 0, color: halo.copy(alpha: 1) ?? halo)
        }
        if gridRows > 0, let gridColor {
            // Leave a pixel right and bottom for the shadow.
            let h = bounds.height - 1, w = bounds.width - 1
            let fromTop = { (d: CGFloat) in self.isGeometryFlipped ? d : h - d }
            let line = max(1, contentsScale) / contentsScale
            ctx.saveGState()
            ctx.setStrokeColor(gridColor)
            ctx.setLineWidth(line)
            for i in 1..<gridRows {
                // Midway through the gap between row i-1 and row i.
                let y = (fromTop(topInset + CGFloat(i) * gridRowStep - gridGap / 2) + (isGeometryFlipped ? 0 : 1)).rounded() + line / 2
                ctx.move(to: CGPoint(x: 0, y: y))
                ctx.addLine(to: CGPoint(x: w, y: y))
            }
            ctx.move(to: CGPoint(x: gridColumnX.rounded() + line / 2, y: isGeometryFlipped ? 0 : 1))
            ctx.addLine(to: CGPoint(x: gridColumnX.rounded() + line / 2, y: isGeometryFlipped ? h : h + 1))
            ctx.strokePath()
            ctx.restoreGState()
        }
        if !cells.isEmpty {
            let h = bounds.height
            ctx.textMatrix = .identity
            for (i, (k, v)) in cells.enumerated() {
                let fromTop = topInset + CGFloat(i) * gridRowStep + cellAscent
                let y = (isGeometryFlipped ? fromTop : h - fromTop).rounded()
                for (text, x) in [(k, cellLeft), (v, cellValueX)] {
                    ctx.textPosition = CGPoint(x: x.rounded(), y: y)
                    CTLineDraw(CTLineCreateWithAttributedString(text), ctx)
                }
            }
            return
        }
        if topInset != 0 { ctx.translateBy(x: 0, y: isGeometryFlipped ? topInset : -topInset) }
        guard let halo else { return super.draw(in: ctx) }
        if pixelated {
            // A hard one-pixel drop shadow, no blur.
            ctx.setShadow(offset: CGSize(width: 1, height: -1), blur: 0, color: halo.copy(alpha: 1) ?? halo)
            return super.draw(in: ctx)
        }
        // Shadow blur is in device pixels. Twice over, so the halo is
        // dense enough to hold small type against a busy field.
        ctx.setShadow(offset: .zero, blur: haloRadius * 2 * contentsScale, color: halo)
        super.draw(in: ctx)
        super.draw(in: ctx)
    }
}

/// 🔭 The infra constellation, top left: every public host and tailnet machine
/// as a star round its hub, fed by toolchain/analytics/ac-infra-probe.mjs.
/// Vocabulary from the old three.js process tree (vscode-extension/views/
/// process-tree.js): a ringed kernel in the middle, grey connection lines, a
/// hot-pink pulse on a live one, the dead left dim with how long ago.
/// Twinkles and pulses are Core Animations; nothing here runs per frame.
final class ConstellationLayer: CALayer {
    struct Star {
        var id = ""
        var name = ""
        /// The dim half of the label: round-trip ms, or how long away.
        var tail = ""
        /// up | slow | late | down | away
        var state = "up"
        /// A tailnet machine draws as a dot; a host as a four-point star.
        var machine = false
        var tint = NSColor.white
        /// Round trip in ms, 0 when there was none: paces the pulse.
        var ms: Double = 0
    }
    struct Cluster { var hub = Star(); var left: [Star] = []; var right: [Star] = [] }
    struct Style { var font: NSFont; var ink: NSColor; var halo: CGColor; var number: NSColor; var dark: Bool; var u: CGFloat; var scale: CGFloat }

    /// Rebuilt when this changes.
    var key = ""
    private let links = CAShapeLayer()
    private let rings = CAShapeLayer()
    private var stars: [String: CAShapeLayer] = [:]
    private var labels: [String: HaloTextLayer] = [:]
    /// Each star's link, star end first, for the pulse to ride.
    private var spokes: [(id: String, from: CGPoint, to: CGPoint, ms: Double)] = []
    private var hubs: [(at: CGPoint, radius: CGFloat, ms: Double)] = []
    private var unit: CGFloat = 1

    static let amber = NSColor(srgbRed: 1.00, green: 0.72, blue: 0.30, alpha: 1)
    static let red = NSColor(srgbRed: 1.00, green: 0.36, blue: 0.30, alpha: 1)
    static let pulse = NSColor(srgbRed: 1.00, green: 0.41, blue: 0.71, alpha: 1)

    private static let still: [String: CAAction] = [
        "position": NSNull(), "bounds": NSNull(), "opacity": NSNull(), "contents": NSNull(), "hidden": NSNull(),
        "path": NSNull(), "fillColor": NSNull(), "strokeColor": NSNull(), "sublayers": NSNull(), "lineWidth": NSNull(),
    ]

    override init() {
        super.init()
        actions = Self.still
        anchorPoint = CGPoint(x: 0, y: 1)
        for l in [links, rings] {
            l.actions = Self.still
            l.fillColor = nil
            l.lineCap = .round
            addSublayer(l)
        }
    }
    override init(layer: Any) { super.init(layer: layer) }
    required init?(coder: NSCoder) { nil }

    private struct Placed {
        var star: Star
        var at: CGPoint
        var label: CGRect
        var text: NSAttributedString
        var radius: CGFloat
        var hub = false
    }

    private static func hash(_ s: String) -> CGFloat {
        var h: UInt32 = 2166136261
        for b in s.utf8 { h = (h ^ UInt32(b)) &* 16777619 }
        return CGFloat(h % 1000) / 1000
    }

    private func words(_ s: Star, _ style: Style) -> NSAttributedString {
        let (main, dim): (NSColor, NSColor) = {
            switch s.state {
            case "down": return (Self.red, Self.red.withAlphaComponent(0.85))
            case "slow", "late": return (Self.amber, Self.amber.withAlphaComponent(0.8))
            case "away": return (style.ink.withAlphaComponent(0.42), style.ink.withAlphaComponent(0.30))
            default: return (style.ink.withAlphaComponent(0.92), style.number.withAlphaComponent(0.85))
            }
        }()
        let name = s.name.count > 11 ? String(s.name.prefix(10)) + "…" : s.name
        let out = NSMutableAttributedString(string: name, attributes: [.font: style.font, .foregroundColor: main])
        if !s.tail.isEmpty {
            out.append(NSAttributedString(string: " " + s.tail, attributes: [.font: style.font, .foregroundColor: dim]))
        }
        return out
    }

    /// One hub and its fan of stars, in points from the hub's centre (y up).
    /// Stars sit on an ellipse and their names read outward from it, so a
    /// link from the hub never runs under a name.
    private func place(_ c: Cluster, _ style: Style, cell: CGFloat, rowH: CGFloat) -> (items: [Placed], lines: [(Int, CGPoint, CGPoint)]) {
        let u = style.u, r = 4 * u, hubR = 6 * u, gap = 5 * u
        let width = { (t: NSAttributedString) in ceil(t.size().width) + 2 }
        let hubText = words(c.hub, style)
        let hubW = width(hubText)
        var items = [Placed(star: c.hub, at: .zero,
                            label: CGRect(x: (-hubW / 2).rounded(), y: (-hubR - 4 * u - cell).rounded(), width: hubW, height: cell),
                            text: hubText, radius: hubR, hub: true)]
        let a = max(hubW / 2 + 24 * u, 54 * u)
        let b = CGFloat(max(c.left.count, c.right.count) + 1) * rowH
        // The fan opens upward from the hub's own row, so the hub's name
        // underneath is clear of every link.
        for (side, list) in [(CGFloat(-1), c.left), (1, c.right)] {
            for (i, s) in list.enumerated() {
                let y = (CGFloat(list.count - 1 - i) * rowH).rounded()
                let x = (a * (1 - (y / b) * (y / b)).squareRoot() + (i % 2 == 1 ? 9 * u : 0)).rounded()
                let text = words(s, style), w = width(text)
                let left = side < 0 ? -x - r - gap - w : x + r + gap
                items.append(Placed(star: s, at: CGPoint(x: side * x, y: y),
                                    label: CGRect(x: left.rounded(), y: (y - cell / 2).rounded(), width: w, height: cell),
                                    text: text, radius: s.machine ? r * 0.62 : r))
            }
        }
        // Links start where they leave the hub's name and stop short of the star.
        let clear = items[0].label.insetBy(dx: -4 * u, dy: -3 * u)
        var lines: [(Int, CGPoint, CGPoint)] = []
        for (i, p) in items.enumerated() where !p.hub {
            let len = max(1, hypot(p.at.x, p.at.y))
            var t0 = (hubR + 5 * u) / len
            for step in 1...40 {
                let t = CGFloat(step) / 40
                if clear.contains(CGPoint(x: p.at.x * t, y: p.at.y * t)) { t0 = max(t0, t + 3 * u / len) }
            }
            let t1 = 1 - (p.radius + 4 * u) / len
            guard t1 - t0 > 3 * u / len else { continue }
            let from = CGPoint(x: p.at.x * t1, y: p.at.y * t1), to = CGPoint(x: p.at.x * t0, y: p.at.y * t0)
            lines.append((i, from, to))
            for (j, other) in items.enumerated() where j != i {
                for step in 0...20 {
                    let k = CGFloat(step) / 20
                    if other.label.contains(CGPoint(x: from.x + (to.x - from.x) * k, y: from.y + (to.y - from.y) * k)) {
                        print("constellation: link to \(p.star.id) runs under \(other.star.id)")
                        break
                    }
                }
            }
        }
        return (items, lines)
    }

    func show(_ clusters: [Cluster], style: Style) {
        unit = style.u
        let cell = ceil(style.font.ascender - style.font.descender)
        let rowH = cell + 2 * style.u
        var live = Set<String>()
        var cursor: CGFloat = 0
        var placed: [(items: [Placed], lines: [(Int, CGPoint, CGPoint)], box: CGRect)] = []
        for c in clusters {
            let p = place(c, style, cell: cell, rowH: rowH)
            var box = CGRect.null
            for i in p.items {
                box = box.union(i.label).union(CGRect(x: i.at.x - i.radius, y: i.at.y - i.radius, width: i.radius * 2, height: i.radius * 2))
            }
            placed.append((p.items, p.lines, box.insetBy(dx: -2, dy: -2)))
        }
        let height = ceil(placed.map { $0.box.height }.max() ?? 0)
        let path = CGMutablePath(), ringPath = CGMutablePath()
        spokes.removeAll()
        hubs.removeAll()
        for p in placed {
            // Hang each cluster from the top edge.
            let dx = (cursor - p.box.minX).rounded(), dy = (height - p.box.maxY).rounded()
            let move = { (q: CGPoint) in CGPoint(x: q.x + dx, y: q.y + dy) }
            for (i, from, to) in p.lines {
                path.move(to: move(from))
                path.addLine(to: move(to))
                spokes.append((p.items[i].star.id, move(from), move(to), p.items[i].star.ms))
            }
            for item in p.items {
                let s = item.star
                live.insert(s.id)
                let at = move(item.at)
                if item.hub {
                    let ring = item.radius + 2.5 * style.u
                    ringPath.addEllipse(in: CGRect(x: at.x - ring, y: at.y - ring, width: ring * 2, height: ring * 2))
                    hubs.append((at, ring, s.ms))
                }
                let star = stars[s.id] ?? {
                    let l = CAShapeLayer()
                    l.actions = Self.still
                    addSublayer(l)
                    stars[s.id] = l
                    return l
                }()
                let look = "\(s.state)\(s.machine)\(Int(item.radius * 10))\(style.dark)"
                if star.name != look {
                    star.name = look
                    dress(star, s, radius: item.radius, style: style)
                }
                star.position = at
                let label = labels[s.id] ?? {
                    let l = HaloTextLayer()
                    l.actions = Self.still
                    l.pixelated = PixelFont.available
                    l.isWrapped = false
                    l.alignmentMode = .left
                    addSublayer(l)
                    labels[s.id] = l
                    return l
                }()
                label.contentsScale = style.scale
                let said = "\(item.text.string)\(s.state)\(style.dark)\(style.font.pointSize)"
                if label.name != said {
                    label.name = said
                    label.string = item.text
                }
                label.halo = style.halo
                // A pixel right and below for the hard shadow.
                label.frame = CGRect(x: item.label.minX + dx, y: item.label.minY + dy - 2, width: item.label.width + 2, height: item.label.height + 2)
            }
            cursor += p.box.width + 22 * style.u
        }
        for (id, l) in stars where !live.contains(id) { l.removeFromSuperlayer(); stars[id] = nil }
        for (id, l) in labels where !live.contains(id) { l.removeFromSuperlayer(); labels[id] = nil }
        bounds = CGRect(x: 0, y: 0, width: max(0, cursor - 22 * style.u), height: height)
        links.frame = bounds
        links.path = path
        links.lineWidth = max(1, style.u)
        links.strokeColor = style.ink.withAlphaComponent(style.dark ? 0.26 : 0.34).cgColor
        rings.frame = bounds
        rings.path = ringPath
        rings.lineWidth = max(1, style.u)
        rings.strokeColor = style.ink.withAlphaComponent(0.42).cgColor
    }

    private func dress(_ layer: CAShapeLayer, _ s: Star, radius r: CGFloat, style: Style) {
        let path = CGMutablePath()
        if s.machine {
            path.addEllipse(in: CGRect(x: -r, y: -r, width: r * 2, height: r * 2))
        } else {
            for k in 0..<8 {
                let d = k % 2 == 0 ? r : r * 0.36, a = CGFloat(k) * .pi / 4 + .pi / 2
                let p = CGPoint(x: cos(a) * d, y: sin(a) * d)
                if k == 0 { path.move(to: p) } else { path.addLine(to: p) }
            }
            path.closeSubpath()
        }
        layer.path = path
        let base = style.dark ? s.tint : (s.tint.blended(withFraction: 0.5, of: .black) ?? s.tint)
        let color: NSColor = s.state == "down" ? Self.red : s.state == "slow" || s.state == "late" ? Self.amber
            : s.state == "away" ? style.ink.withAlphaComponent(0.45) : base
        // Away is a hollow dot: there, but not here.
        layer.fillColor = s.state == "away" ? nil : color.cgColor
        layer.strokeColor = s.state == "away" ? color.cgColor : NSColor.black.withAlphaComponent(style.dark ? 0.55 : 0.2).cgColor
        layer.lineWidth = s.state == "away" ? max(1, style.u) : 0.5
        layer.removeAnimation(forKey: "twinkle")
        let (low, half): (Float, Double)
        switch s.state {
        case "down": (low, half) = (0.12, 0.9)                         // a slow red blink
        case "slow", "late": (low, half) = (0.45, 2.6)
        case "away": layer.opacity = 1; return
        default: (low, half) = (0.62, 1.7 + Double(Self.hash(s.id)) * 1.9)
        }
        layer.opacity = 1
        let twinkle = CABasicAnimation(keyPath: "opacity")
        twinkle.fromValue = 1
        twinkle.toValue = low
        twinkle.duration = half
        twinkle.autoreverses = true
        twinkle.repeatCount = .infinity
        twinkle.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
        // Begun in the past at a phase of its own, so a rebuild never restarts it.
        twinkle.beginTime = 1 + Double(Self.hash(s.id + "phase")) * half * 2
        layer.add(twinkle, forKey: "twinkle")
    }

    /// A probe came back: a dot runs each answered link from the star to its
    /// hub, taking as long as the answer did, and each hub's ring breathes out.
    func pulse() {
        let now = CACurrentMediaTime()
        let run = { (layer: CALayer, parts: [CAAnimation], duration: Double) in
            let group = CAAnimationGroup()
            group.animations = parts
            group.duration = duration
            group.beginTime = now
            group.fillMode = .both
            group.isRemovedOnCompletion = false
            CATransaction.begin()
            CATransaction.setCompletionBlock { layer.removeFromSuperlayer() }
            layer.add(group, forKey: "life")
            CATransaction.commit()
        }
        for s in spokes where s.ms > 0 {
            let r = 1.8 * unit
            let dot = CAShapeLayer()
            dot.actions = Self.still
            dot.path = CGPath(ellipseIn: CGRect(x: -r, y: -r, width: r * 2, height: r * 2), transform: nil)
            dot.fillColor = Self.pulse.cgColor
            dot.position = s.to
            dot.opacity = 0
            addSublayer(dot)
            let move = CABasicAnimation(keyPath: "position")
            move.fromValue = NSValue(point: s.from)
            move.toValue = NSValue(point: s.to)
            move.timingFunction = CAMediaTimingFunction(name: .easeIn)
            let fade = CAKeyframeAnimation(keyPath: "opacity")
            fade.values = [0, 1, 1, 0]
            fade.keyTimes = [0, 0.15, 0.8, 1]
            run(dot, [move, fade], min(3.2, max(0.5, s.ms / 350)))
        }
        for h in hubs where h.ms > 0 {
            let ring = CAShapeLayer()
            ring.actions = Self.still
            ring.path = CGPath(ellipseIn: CGRect(x: -h.radius, y: -h.radius, width: h.radius * 2, height: h.radius * 2), transform: nil)
            ring.fillColor = nil
            ring.strokeColor = Self.pulse.cgColor
            ring.lineWidth = max(1, unit)
            ring.position = h.at
            ring.opacity = 0
            addSublayer(ring)
            let grow = CABasicAnimation(keyPath: "transform.scale")
            grow.fromValue = 1
            grow.toValue = 1.9
            let fade = CAKeyframeAnimation(keyPath: "opacity")
            fade.values = [0.9, 0.4, 0]
            fade.keyTimes = [0, 0.5, 1]
            run(ring, [grow, fade], 1.6)
        }
    }
}

/// MatrixChunky8, the 8-pixel bitmap face (Resources/chunky8.ttf, baked by
/// bake-chunky8.py), with GNU Unifont behind it for the symbols it lacks.
/// Exact at whole multiples of 8 device pixels; Unifont alone, when the
/// chunky file is missing, at multiples of 16. Looked for beside the binary's
/// app (Resources/) and in ~/.config/blueberry-wallpaper/.
enum PixelFont {
    private static func load(_ name: String, _ ext: String) -> CTFontDescriptor? {
        let paths = [Bundle.main.path(forResource: name, ofType: ext),
                     NSHomeDirectory() + "/.config/blueberry-wallpaper/\(name).\(ext)"].compactMap { $0 }
        for path in paths where FileManager.default.fileExists(atPath: path) {
            let url = URL(fileURLWithPath: path) as CFURL
            CTFontManagerRegisterFontsForURL(url, .process, nil)
            if let list = CTFontManagerCreateFontDescriptorsFromURL(url) as? [CTFontDescriptor], let d = list.first {
                return d
            }
        }
        return nil
    }
    private static let unifont = load("unifont", "otf")
    private static let chunky = load("chunky8", "ttf")
    private static let descriptor: CTFontDescriptor? = {
        guard let chunky else { return unifont }
        guard let unifont else { return chunky }
        return CTFontDescriptorCreateCopyWithAttributes(
            chunky, [kCTFontCascadeListAttribute: [unifont]] as CFDictionary)
    }()
    static var available: Bool { descriptor != nil }
    /// Device pixels in one cell of the face at text scale 1.
    static var cell: CGFloat { chunky != nil ? 8 : 16 }
    /// Type size against a 16-pixel cell: the feeder's `textScale` (1–4)
    /// times `cell / 16`. Boxes that hold type grow by it too.
    static var boost: CGFloat = cell / 16
    /// One cell (times the text scale) at this backing scale, or nil when no file is there.
    static func font(scale: CGFloat) -> NSFont? {
        guard let descriptor else { return nil }
        return CTFontCreateWithFontDescriptor(descriptor, 16 * boost / max(1, scale.rounded()), nil) as NSFont
    }
}

/// A live fishery over the field: everyone on a product right now swims as a
/// labelled fish, and what they do (a generation, a signup, a publish) plays
/// back as ripples and rising bubbles at the moment it happened, plus a
/// corner tally with an hour of generations and a ticker.
///
/// Product-agnostic. Something else (on panda, the vault's Fuser feeder)
/// writes `~/.local/share/blueberry-wallpaper/fishery.json`:
///
///     { name, observedAt, liveMinutes,
///       fish:   [{ id, label, kind: staff|paid|free|anon, lastAt, generations, place }],
///       events: [{ id, fish, kind: generation|failed|signup|workflow|publish|tool|…, at, text, credits }],
///       totals: { generationsHour, creditsHour, swimmersHour, spark: [60 per-minute counts] } }
///
/// Times are Unix seconds. With no file the layer stays empty and costs one
/// stat every two seconds.
final class FisheryLayer: CALayer {
    static let file = NSString(
        string: "~/.local/share/blueberry-wallpaper/fishery.json").expandingTildeInPath
    /// Analytics land late; events replay this long after they happened so
    /// their real spacing survives the batch they arrived in.
    private static let playbackDelay: Double = 75
    private static let staleAfter: Double = 180

    private struct Snapshot {
        var name = ""
        var observedAt: Double = 0
        var liveMinutes: Double = 6
        var fish: [[String: Any]] = []
        var events: [[String: Any]] = []
        var totals: [String: Any] = [:]
        var heatmaps: [[String: Any]] = []
        /// "walkers" draws rax's little people instead of fish.
        var school = ""
    }

    private struct Pending { let playAt: Double; let event: [String: Any] }

    /// The feeder's totals, bridged out of JSON once per file instead of on
    /// every frame.
    private struct Category {
        let key: String, label: String
        let count: Double
        let spark: [CGFloat]
        /// The feeder may bring its own glyph and colour, and say whether the
        /// category stacks into the hour chart.
        let style: (glyph: String, color: NSColor)?
        let chart: Bool
        /// Shown even at zero (e.g. "online now").
        let keep: Bool
    }
    private static let chartKeys: Set<String> = ["image", "video", "model", "touchup", "audio", "other"]
    private static func hex(_ s: String?) -> NSColor? {
        guard let s, s.count == 7, s.hasPrefix("#"), let v = UInt32(s.dropFirst(), radix: 16) else { return nil }
        return NSColor(srgbRed: CGFloat(v >> 16 & 255) / 255, green: CGFloat(v >> 8 & 255) / 255, blue: CGFloat(v & 255) / 255, alpha: 1)
    }
    private var categories: [Category] = []
    private var referrers: [(String, Double)] = []
    private var dataVersion = 0

    /// Where each fish is this frame, for the SceneKit school to draw.
    struct Pose {
        let id: String
        let x: CGFloat, y: CGFloat, length: CGFloat, depth: CGFloat
        let yaw: CGFloat, tilt: CGFloat, wag: CGFloat
        let color: NSColor
        let alpha: CGFloat, glow: CGFloat
        /// Local file of the generation this fish wears right now, or "".
        let skin: String
        /// Fuser employee (fuser.studio email): logo crown and a mint glow.
        let staff: Bool
    }
    private(set) var poses: [Pose] = []

    /// The hour chart for the 3D reef: where it stands on screen (points)
    /// and each minute's stacked category counts.
    struct ReefData {
        let frame: CGRect
        let stacks: [[(NSColor, CGFloat)]]
        let peak: CGFloat
        let version: Int
    }
    private(set) var reef: ReefData?

    /// A feeder is writing: the field is an aquarium, not an empty desktop.
    var stocked: Bool { snapshot.observedAt > 0 }
    /// The feeder asked for walkers (aesthetic.computer) instead of fish.
    var walkers: Bool { snapshot.school == "walkers" }

    private final class Fish {
        let id: String
        let root = CALayer()
        let body = CALayer()
        let shape = CAShapeLayer()
        let tail = CAShapeLayer()
        let eye = CAShapeLayer()
        let label = HaloTextLayer()
        /// Name tag above the fish: avatar + name, and a small line under it.
        let tag = CALayer()
        let sub = HaloTextLayer()
        let face = CALayer()
        var avatar = ""
        /// The avatar as loaded, and what the baked circle was made for.
        var faceSource: CGImage?
        var faceKey = ""
        var skins: [String] = []
        var since: Double = 0
        var pages = 0
        var clicks = 0
        var place = ""
        var from = ""
        var subText = ""
        var subKey = -1
        /// Ghosts: how long they were on, and when they left (Unix seconds).
        var stayed: Double = 0
        var leftAt: Double = 0
        var tagW: CGFloat = 0
        var size: CGFloat = 0
        var kind = "free"
        var name = ""
        var path = ""
        var generations = 0
        /// When this fish's generations played; ✦ counts the last 90 s.
        var hits: [Double] = []
        var shownHits = 0
        var lastAt: Double = 0
        var x: CGFloat = 0
        var y: CGFloat = 0
        var homeY: CGFloat = 0
        var dir: CGFloat = 1
        var facing: CGFloat = 1
        var speed: CGFloat = 30
        var boost: CGFloat = 0
        var glow: CGFloat = 0
        var phase: CGFloat = 0
        var alpha: CGFloat = 0
        var leaving = false
        var length: CGFloat = 0
        /// 0 = far back, 1 = against the glass. `depth` drifts around `reach`.
        var reach: CGFloat = 0.5
        /// Where this fish is drifting to, and when it next changes its mind.
        var targetY: CGFloat = 0
        var nextWander: Double = 0
        var depth: CGFloat = 0.5
        var lunge: CGFloat = 0

        init(id: String) { self.id = id }
    }

    private var snapshot = Snapshot()
    private var fileStamp: Date?
    private var lastPoll: Double = 0
    private var lastTick: Double?
    /// Set when this screen is one pane of a wider tank (see Tank.swift).
    var link: TankLink?
    private var lastSent: Double = 0
    private var linkDataVersion = -1
    /// Where this screen sits in the world, and how wide one screen is.
    var viewport: CGRect = .zero
    /// Room kept clear at the bottom-right for a MacPal badge parked there.
    var badgeInset: CGFloat = 0
    private var fish: [String: Fish] = [:]
    private var seen = Set<String>()
    private var pending: [Pending] = []
    private var primed = false
    private var dark = true
    private var ticker: [(at: Double, who: String, what: String, kind: String)] = []
    private var crawlFresh = 0   // events logged since the crawl last took one

    // Lower third, news style: a LIVE chip and headline over the water, and
    // a crawl band along the bottom edge carrying every event right to left.
    private let hud = CALayer()
    private let liveChip = CALayer()
    private let liveText = HaloTextLayer()
    private let headline = HaloTextLayer()
    private let stats = HaloTextLayer()
    /// Odometer tallies for the last hour: generations, credits, people.
    /// Each counts toward a target that the feeder resets and every played
    /// generation nudges, so the numbers climb live between refreshes.
    private var tallyText: [(number: CATextLayer, caption: CATextLayer)] = []
    private var tallyShown: [Double] = [0, 0, 0]
    private var tallyTarget: [Double] = [0, 0, 0]
    private var tallyFeed: [Double] = [-1, -1, -1]
    /// One tile per category the feeder reports: glyph, count and caption. Counts roll toward the feed and are nudged
    /// live by every event that plays.
    private final class Tile {
        let root = CALayer()
        let number = HaloTextLayer()
        let caption = HaloTextLayer()
        var shown: Double = 0
        var target: Double = 0
        var feed: Double = -1
        var headW: CGFloat = 0
        var capW: CGFloat = 0
    }
    private var tiles: [String: Tile] = [:]
    static let categoryStyle: [String: (glyph: String, color: NSColor)] = [
        "image": ("▣", NSColor(srgbRed: 1.00, green: 0.55, blue: 0.85, alpha: 1)),
        "video": ("▶", NSColor(srgbRed: 1.00, green: 0.62, blue: 0.30, alpha: 1)),
        "model": ("◆", NSColor(srgbRed: 0.40, green: 0.90, blue: 1.00, alpha: 1)),
        "touchup": ("✧", NSColor(srgbRed: 0.76, green: 0.70, blue: 1.00, alpha: 1)),
        "audio": ("♪", NSColor(srgbRed: 1.00, green: 0.88, blue: 0.35, alpha: 1)),
        "other": ("•", NSColor(srgbRed: 0.75, green: 0.78, blue: 0.85, alpha: 1)),
        "signup": ("✚", NSColor(srgbRed: 0.45, green: 1.00, blue: 0.55, alpha: 1)),
        "workflow": ("⌗", NSColor(srgbRed: 0.50, green: 0.72, blue: 1.00, alpha: 1)),
        "publish": ("↗", NSColor(srgbRed: 1.00, green: 0.80, blue: 0.30, alpha: 1)),
    ]
    /// The hour as one chart, bottom right: stacked categories per minute.
    /// Where people came from (24 h), bottom left: site, bar, people.
    private let refHead = HaloTextLayer()
    /// 🗺️ The world panel above "came from", and the feeder's `world`.
    private var world: [String: Any] = [:]
    private let worldFly = WorldFlyLayer()
    private var worldVersion = ""
    private var worldLive: [String: Any] = [:]
    private var worldLiveStamp: Date?
    private var worldLivePolled: CFTimeInterval = 0
    private var worldLiveVersion = 0
    /// 👣 Who is where, alone (people.json, from GET /rax/people): read ten
    /// times a second so the walkers in the model move as people do.
    private static let peopleFile = NSHomeDirectory() + "/.local/share/blueberry-wallpaper/people.json"
    private var peopleLive: [WorldFlyLayer.Person]?
    private var peopleStamp: Date?
    private var peoplePolled: CFTimeInterval = 0
    private var peopleRead: CFTimeInterval = 0
    private static let liveFile = NSHomeDirectory() + "/.local/share/blueberry-wallpaper/world.json"
    private var worldCensus = ""
    private let worldHead = HaloTextLayer()
    /// 🔭 The feeder's `infra` probe and the constellation it draws, top left.
    private var infra: [String: Any] = [:]
    private var infraVersion = 0
    private var infraPulsed: Double = 0
    private let constellation = ConstellationLayer()
    /// The menu bar's height on this screen; the constellation hangs below it.
    var topInset: CGFloat = 0
    private var refRows: [(name: CATextLayer, bar: CALayer, count: CATextLayer)] = []
    /// Measured text widths per row, so the columns shrink to their content.
    private var refWidths = [(name: CGFloat, count: CGFloat)](repeating: (0, 0), count: 7)
    private let chartLabels = (0..<4).map { _ in HaloTextLayer() }
    private var chartKey = ""

    private let band = CALayer()
    private let bandRule = CALayer()
    private let bandTag = CALayer()
    private let bandTagText = HaloTextLayer()
    private let crawl = CALayer()
    /// Each item glides on its own linear Core Animation, so the crawl moves
    /// at the display's refresh rate; x0/t0 let the timer know where it is.
    private var crawlItems: [(layer: CATextLayer, x0: CGFloat, t0: Double, width: CGFloat)] = []
    private var crawlCursor = 0
    /// Hot list: the latest events, top centre, newest first.
    private let hot = CALayer()
    /// Heatmap panel on the right: one page's click heat at a time, panning
    /// slowly down the page, the next map every 45 s.
    private let heat = CALayer()
    private let heatCaption = HaloTextLayer()
    private var heatIndex = -1
    private var heatShownAt: Double = 0
    private var heatFile = ""
    private var heatAspect: CGFloat = 1   // image height / width
    private var hotRows: [CATextLayer] = []
    private var hotTop: Double = 0     // `at` of the newest row last drawn
    private var hotSlide: CGFloat = 0  // rows ease down when one arrives
    private static let clock: DateFormatter = {
        let f = DateFormatter()
        f.dateFormat = "h:mm:ss"
        return f
    }()

    /// Points per "unit", sized to sit with the desktop's own icons and
    /// labels — 1.7× on a 1440-tall screen.
    private var u: CGFloat { max(1, bounds.height / 850) }
    /// Type runs smaller than the fish so the school has room.
    private var tu: CGFloat { u * 0.7 }
    /// Box and row heights that hold type: `tu` grown by the feeder's text scale.
    private var ts: CGFloat { tu * PixelFont.boost }
    /// Top of the bottom HUD (tiles, chart, came-from, hot list): walkers and
    /// their tables stay above it.
    private var hudTop: CGFloat = 0

    var scale: CGFloat = 2 {
        didSet { sublayersNeedScale(self) }
    }

    override init() {
        super.init()
        actions = Self.still
        hud.actions = Self.still
        hud.isHidden = true
        addSublayer(worldFly)
        addSublayer(hud)
        for l in [liveChip, band, bandRule, bandTag, crawl] { l.actions = Self.still }
        hud.addSublayer(band)
        band.addSublayer(crawl)
        crawl.masksToBounds = true
        hud.addSublayer(bandRule)
        hud.addSublayer(bandTag)
        bandTag.addSublayer(Self.text(bandTagText))
        hud.addSublayer(liveChip)
        liveChip.addSublayer(Self.text(liveText))
        for t in [headline, stats] { hud.addSublayer(Self.text(t)) }
        for _ in 0..<3 {
            let pair = (Self.text(HaloTextLayer()), Self.text(HaloTextLayer()))
            hud.addSublayer(pair.0)
            hud.addSublayer(pair.1)
            tallyText.append(pair)
        }
        hot.actions = Self.still
        hud.addSublayer(hot)
        heat.actions = Self.still
        heat.masksToBounds = true
        heat.contentsGravity = .resize
        heat.isHidden = true
        hud.addSublayer(heat)
        hud.addSublayer(Self.text(heatCaption))
        for _ in 0..<7 {
            let row = Self.text(HaloTextLayer())
            row.alignmentMode = .center
            hot.addSublayer(row)
            hotRows.append(row)
        }
        hud.addSublayer(Self.text(refHead))
        hud.addSublayer(Self.text(worldHead))
        for _ in 0..<7 {
            let row = (Self.text(HaloTextLayer()), CALayer(), Self.text(HaloTextLayer()))
            row.1.actions = Self.still
            row.2.alignmentMode = .right
            for l in [row.0, row.1, row.2] as [CALayer] { hud.addSublayer(l) }
            refRows.append(row)
        }
        for l in chartLabels { hud.addSublayer(Self.text(l)) }
        constellation.isHidden = true
        addSublayer(constellation)
    }

    override init(layer: Any) { super.init(layer: layer) }
    required init?(coder: NSCoder) { nil }

    private static let darkHalo = NSColor.black.cgColor
    /// In the light, type is dark ink on a hard coloured shadow.
    private static let lightHalo = NSColor(srgbRed: 1.0, green: 0.42, blue: 0.78, alpha: 1).cgColor

    private static let still: [String: CAAction] = [
        "position": NSNull(), "bounds": NSNull(), "opacity": NSNull(), "transform": NSNull(),
        "contents": NSNull(), "string": NSNull(), "foregroundColor": NSNull(), "backgroundColor": NSNull(),
        "fillColor": NSNull(), "hidden": NSNull(), "frame": NSNull(), "path": NSNull(), "strokeColor": NSNull(),
        "shadowOpacity": NSNull(), "shadowRadius": NSNull(), "shadowColor": NSNull(), "sublayerTransform": NSNull(),
    ]

    private static func text(_ t: CATextLayer) -> CATextLayer {
        t.actions = still
        (t as? HaloTextLayer)?.pixelated = PixelFont.available
        t.contentsScale = 2
        t.alignmentMode = .left
        t.truncationMode = .end
        t.isWrapped = false
        return t
    }

    private func sublayersNeedScale(_ layer: CALayer) {
        for sub in layer.sublayers ?? [] {
            sub.contentsScale = scale
            sublayersNeedScale(sub)
        }
    }

    // ── palette ──────────────────────────────────────────────────────────

    private func ink(_ alpha: CGFloat = 1) -> CGColor {
        (dark ? NSColor(white: 0.96, alpha: alpha) : NSColor(srgbRed: 0.05, green: 0.07, blue: 0.16, alpha: alpha)).cgColor
    }

    // One object per tier, so the school can tell "same colour" by identity.
    private static let staffColor = NSColor(srgbRed: 0.40, green: 0.95, blue: 0.72, alpha: 1)
    private static let paidColor = NSColor(srgbRed: 1.00, green: 0.80, blue: 0.30, alpha: 1)
    private static let anonColor = NSColor(srgbRed: 0.66, green: 0.70, blue: 0.84, alpha: 1)
    private static let ghostColor = NSColor(srgbRed: 0.86, green: 0.90, blue: 1.00, alpha: 1)
    private static let freeColor = NSColor(srgbRed: 0.52, green: 0.82, blue: 1.00, alpha: 1)

    private func color(for kind: String) -> NSColor {
        switch kind {
        case "staff": return Self.staffColor
        case "paid": return Self.paidColor
        case "anon": return Self.anonColor
        case "ghost": return Self.ghostColor
        default: return Self.freeColor
        }
    }

    private func color(forEvent kind: String) -> NSColor {
        switch kind {
        case "generation": return NSColor(srgbRed: 1.00, green: 0.55, blue: 0.85, alpha: 1)
        case "failed": return NSColor(srgbRed: 1.00, green: 0.38, blue: 0.32, alpha: 1)
        case "signup": return NSColor(srgbRed: 0.45, green: 1.00, blue: 0.55, alpha: 1)
        default: return NSColor(srgbRed: 0.80, green: 0.75, blue: 1.00, alpha: 1)
        }
    }

    // ── data ─────────────────────────────────────────────────────────────

    private func poll(now: Double) {
        if let link, link.following {
            guard link.dataVersion != linkDataVersion, let data = link.data else { return }
            linkDataVersion = link.dataVersion
            load(data, now: now)
            return
        }
        guard now - lastPoll > 2 else { return }
        lastPoll = now
        let attrs = try? FileManager.default.attributesOfItem(atPath: Self.file)
        guard let stamp = attrs?[.modificationDate] as? Date else {
            snapshot = Snapshot()
            categories = []
            referrers = []
            return
        }
        guard stamp != fileStamp, let data = FileManager.default.contents(atPath: Self.file) else { return }
        guard load(data, now: now) else { return }
        fileStamp = stamp
        if link?.config.role == .leader { link?.publish(data: data) }
    }

    @discardableResult
    private func load(_ data: Data, now: Double) -> Bool {
        guard let obj = (try? JSONSerialization.jsonObject(with: data)) as? [String: Any] else { return false }
        snapshot = Snapshot(
            name: obj["name"] as? String ?? "",
            observedAt: Self.num(obj["observedAt"]),
            liveMinutes: max(1, Self.num(obj["liveMinutes"], 6)),
            fish: obj["fish"] as? [[String: Any]] ?? [],
            events: obj["events"] as? [[String: Any]] ?? [],
            totals: obj["totals"] as? [String: Any] ?? [:],
            heatmaps: obj["heatmaps"] as? [[String: Any]] ?? [],
            school: obj["school"] as? String ?? "")
        world = obj["world"] as? [String: Any] ?? [:]
        infra = obj["infra"] as? [String: Any] ?? [:]
        infraVersion += 1
        let totals = snapshot.totals
        categories = (totals["categories"] as? [[String: Any]] ?? []).compactMap { row in
            guard let key = row["key"] as? String else { return nil }
            let own = Self.hex(row["color"] as? String).map { (glyph: row["glyph"] as? String ?? "•", color: $0) }
            return Category(key: key, label: row["label"] as? String ?? key, count: Self.num(row["count"]),
                            spark: (row["spark"] as? [NSNumber])?.map { CGFloat($0.doubleValue) } ?? [],
                            style: own ?? Self.categoryStyle[key],
                            chart: row["chart"] as? Bool ?? Self.chartKeys.contains(key),
                            keep: row["keep"] as? Bool ?? false)
        }
        referrers = (totals["referrers"] as? [[String: Any]] ?? []).map {
            ($0["domain"] as? String ?? "", Self.num($0["people"])) }
        let direct = Self.num(totals["directPeople"])
        if direct > 0 { referrers.insert(("direct", direct), at: 0) }
        let boost = CGFloat(max(1, min(4, Self.num(obj["textScale"], 1).rounded()))) * PixelFont.cell / 16
        if boost != PixelFont.boost {
            PixelFont.boost = boost
            for f in fish.values { dress(f) }
            for t in tiles.values { t.number.name = nil; t.caption.name = nil }
            for r in refRows { r.name.name = nil }
            for r in hotRows { r.name = nil }
            refHead.name = nil
            chartKey = ""
        }
        dataVersion += 1
        absorb(now: now)
        return true
    }

    private static func num(_ v: Any?, _ fallback: Double = 0) -> Double {
        (v as? NSNumber)?.doubleValue ?? fallback
    }

    private func absorb(now: Double) {
        let live = Set(snapshot.fish.compactMap { $0["id"] as? String })
        for row in snapshot.fish {
            guard let id = row["id"] as? String else { continue }
            let f = fish[id] ?? spawn(id: id, now: now)
            f.kind = row["kind"] as? String ?? "free"
            f.name = row["label"] as? String ?? ""
            f.path = row["path"] as? String ?? ""
            f.since = Self.num(row["since"], Self.num(row["arrivedAt"]))
            f.pages = Int(Self.num(row["pages"]))
            f.clicks = Int(Self.num(row["clicks"]))
            f.place = row["where"] as? String ?? ""
            f.from = row["from"] as? String ?? ""
            f.stayed = Self.num(row["stayed"])
            f.leftAt = Self.num(row["leftAt"])
            f.skins = row["skins"] as? [String] ?? []
            let avatar = row["avatar"] as? String ?? ""
            if avatar != f.avatar {
                f.avatar = avatar
                let image = avatar.isEmpty ? nil : NSImage(contentsOfFile: avatar)
                f.faceSource = image?.cgImage(forProposedRect: nil, context: nil, hints: nil)
                f.faceKey = ""
                f.face.contents = nil
                f.face.isHidden = f.faceSource == nil
            }
            f.generations = Int(Self.num(row["generations"]))
            f.lastAt = Self.num(row["lastAt"])
            f.leaving = false
            dress(f)
        }
        for f in fish.values where !live.contains(f.id) {
            // Kept alive by a pending event; otherwise it swims off.
            if !pending.contains(where: { $0.event["fish"] as? String == f.id }) { f.leaving = true }
        }

        // The first file seeds the ticker with recent history instead of
        // replaying several minutes of events at once.
        var stagger = 0.0
        for event in snapshot.events {
            guard let id = event["id"] as? String, !seen.contains(id) else { continue }
            seen.insert(id)
            let at = Self.num(event["at"])
            let due = at + Self.playbackDelay
            if !primed && due < now {
                log(event, at: at)
                continue
            }
            var playAt = max(due, now + stagger)
            if due < now { stagger += 0.6; playAt = now + stagger }
            pending.append(Pending(playAt: playAt, event: event))
        }
        pending.sort { $0.playAt < $1.playAt }
        primed = true
        if seen.count > 4000 { seen = Set(snapshot.events.compactMap { $0["id"] as? String }) }
    }

    // ── fish ─────────────────────────────────────────────────────────────

    private func spawn(id: String, now: Double) -> Fish {
        let f = Fish(id: id)
        var h: UInt64 = 1469598103934665603
        for b in id.utf8 { h = (h ^ UInt64(b)) &* 1099511628211 }
        let r = { (shift: UInt64) -> CGFloat in CGFloat((h >> shift) & 0xffff) / 65535 }
        let w = max(bounds.width, 1), ht = max(bounds.height, 1)
        f.dir = r(0) > 0.5 ? 1 : -1
        f.facing = f.dir
        // The first school starts mid-swim across the screen; later
        // arrivals enter from an edge.
        f.x = primed ? (f.dir > 0 ? -60 * u : w + 60 * u) : w * (0.08 + 0.84 * r(8))
        f.homeY = ht * (0.12 + 0.72 * r(16))
        f.y = f.homeY
        f.phase = r(32) * .pi * 2
        f.speed = (14 + 18 * r(48)) * u
        f.reach = 0.12 + 0.8 * r(24)
        f.targetY = f.homeY
        f.depth = f.reach
        f.root.actions = Self.still
        f.body.actions = Self.still
        for s in [f.shape, f.tail, f.eye] { s.actions = Self.still; f.body.addSublayer(s) }
        // The body itself is drawn in 3D by FishSchool; this layer keeps
        // the label riding along.
        f.body.isHidden = true
        f.root.addSublayer(f.body)
        f.tag.actions = Self.still
        f.face.actions = Self.still
        // The circle, its edge and its ring are baked into the bitmap at
        // device pixels (see `faceBitmap`), so the layer only places it.
        f.face.contentsGravity = .resize
        f.face.magnificationFilter = .linear
        f.face.minificationFilter = .trilinear
        f.face.isHidden = true
        f.root.addSublayer(f.tag)
        f.tag.addSublayer(Self.text(f.label))
        f.root.addSublayer(Self.text(f.sub))
        f.tag.addSublayer(f.face)
        for t in [f.label, f.sub] { t.contentsScale = scale }
        f.root.opacity = 0
        insertSublayer(f.root, below: hud)
        fish[id] = f
        return f
    }

    private func dress(_ f: Fish) {
        let anon = f.kind == "anon"
        let len: CGFloat = walkers ? (anon ? 34 : 50 + CGFloat(min(f.generations, 12)) * 2) * u
                                   : (anon ? 22 : 40 + CGFloat(min(f.generations, 12)) * 3) * u
        let c = color(for: f.kind)
        if len != f.length {
            f.length = len
            let h = len * 0.46
            f.shape.path = CGPath(ellipseIn: CGRect(x: -len * 0.5, y: -h * 0.5, width: len, height: h), transform: nil)
            let tail = CGMutablePath()
            tail.move(to: CGPoint(x: 0, y: 0))
            tail.addLine(to: CGPoint(x: -len * 0.42, y: h * 0.55))
            tail.addLine(to: CGPoint(x: -len * 0.32, y: 0))
            tail.addLine(to: CGPoint(x: -len * 0.42, y: -h * 0.55))
            tail.closeSubpath()
            f.tail.path = tail
            f.tail.position = CGPoint(x: -len * 0.42, y: 0)
            let e = max(1.6, len * 0.07)
            f.eye.path = CGPath(ellipseIn: CGRect(x: len * 0.24 - e, y: h * 0.06 - e, width: e * 2, height: e * 2), transform: nil)
        }
        f.shape.fillColor = c.withAlphaComponent(anon ? 0.55 : 0.92).cgColor
        f.tail.fillColor = c.withAlphaComponent(anon ? 0.45 : 0.78).cgColor
        f.eye.fillColor = NSColor(white: 0.04, alpha: anon ? 0 : 0.85).cgColor
        f.body.shadowColor = c.cgColor
        f.body.shadowOffset = .zero
        f.body.shadowOpacity = 0.9
        // Anonymous fish wear the page they're on instead of a name.
        let words = anon || f.name.isEmpty ? f.path : f.name
        f.label.isHidden = words.isEmpty
        let gens = f.hits.isEmpty ? "" : "  ✦\(f.hits.count)"
        f.label.string = NSAttributedString(string: words + (anon ? "" : gens), attributes: [
            .font: anon ? face(NSFont.monospacedSystemFont(ofSize: max(11, 11 * tu), weight: .semibold))
                        : face(NSFont.systemFont(ofSize: max(11, 15 * tu), weight: .heavy)),
            .foregroundColor: NSColor(cgColor: ink(anon ? 0.7 : 0.95)) ?? .white,
        ])
        // In the light each fish's type sits on a shadow of its own colour.
        f.label.halo = dark ? Self.darkHalo
            : (c.blended(withFraction: 0.35, of: .white) ?? c).withAlphaComponent(1).cgColor
        f.sub.halo = f.label.halo
        // Even whole widths, so a centred label starts on a pixel.
        f.label.bounds = CGRect(x: 0, y: 0, width: (((anon ? 600 : 240) * u * PixelFont.boost) / 2).rounded() * 2,
                                height: PixelFont.available ? ceil(((f.label.string as? NSAttributedString)?.size().height ?? 16) + 2) : 21 * tu)
        f.label.anchorPoint = CGPoint(x: 0.5, y: 0)
        f.label.alignmentMode = .center
        f.label.position = .zero
        f.sub.anchorPoint = CGPoint(x: 0.5, y: 1)
        f.sub.alignmentMode = .left
        f.sub.truncationMode = .none
        // A transparent table: rules and type only, each with the hard shadow.
        f.sub.borderWidth = 0
        f.sub.backgroundColor = nil
        f.sub.borderColor = c.cgColor
        // Avatar sits just left of the name, a circle as tall as the type.
        let nameW = (f.label.string as? NSAttributedString)?.size().width ?? 0
        // Whole device pixels, so the baked bitmap maps one to one.
        let side = (28 * tu * scale).rounded() / scale
        f.tagW = nameW + side + 12 * tu
        f.face.bounds = CGRect(x: 0, y: 0, width: side, height: side)
        f.face.contentsScale = scale
        let facePixels = Int((side * scale).rounded())
        let faceKey = "\(f.avatar)|\(facePixels)|\(f.kind)|\(dark)"
        if faceKey != f.faceKey, let source = f.faceSource {
            f.faceKey = faceKey
            f.face.contents = Self.faceBitmap(source, pixels: facePixels, ring: c.cgColor,
                                              ringWidth: max(1, (1.2 * u * scale).rounded()))
        }
        let faceX = ((-nameW / 2 - side / 2 - 6 * tu) * scale).rounded() / scale
        f.face.position = CGPoint(x: faceX, y: (10.5 * tu * scale).rounded() / scale)
        f.subText = "" // redraw the sub-line in the new appearance
        f.subKey = -1
    }

    /// A round avatar at exact device pixels. A profile picture is some ten
    /// times the size it is shown at; left to the compositor that reduction
    /// comes out coarse and jagged. Here it is halved step by step with
    /// smooth interpolation, then clipped to an antialiased circle with its
    /// tier ring drawn on.
    private static func faceBitmap(_ image: CGImage, pixels: Int, ring: CGColor, ringWidth: CGFloat) -> CGImage? {
        guard pixels > 0, let space = CGColorSpace(name: CGColorSpace.sRGB) else { return nil }
        func canvas(_ w: Int, _ h: Int) -> CGContext? {
            let ctx = CGContext(data: nil, width: w, height: h, bitsPerComponent: 8, bytesPerRow: 0, space: space,
                                bitmapInfo: CGImageAlphaInfo.premultipliedLast.rawValue)
            ctx?.interpolationQuality = .high
            ctx?.setShouldAntialias(true)
            return ctx
        }
        var current = image
        while current.width >= pixels * 4, current.height >= pixels * 4 {
            let w = current.width / 2, h = current.height / 2
            guard let ctx = canvas(w, h) else { break }
            ctx.draw(current, in: CGRect(x: 0, y: 0, width: w, height: h))
            guard let half = ctx.makeImage() else { break }
            current = half
        }
        guard let ctx = canvas(pixels, pixels) else { return nil }
        let p = CGFloat(pixels)
        let disc = CGRect(x: 0, y: 0, width: p, height: p).insetBy(dx: 0.5, dy: 0.5)
        ctx.saveGState()
        ctx.addEllipse(in: disc)
        ctx.clip()
        let fill = max(p / CGFloat(current.width), p / CGFloat(current.height))
        let w = CGFloat(current.width) * fill, h = CGFloat(current.height) * fill
        ctx.draw(current, in: CGRect(x: (p - w) / 2, y: (p - h) / 2, width: w, height: h))
        ctx.restoreGState()
        ctx.setStrokeColor(ring)
        ctx.setLineWidth(ringWidth)
        ctx.strokeEllipse(in: disc.insetBy(dx: ringWidth / 2, dy: ringWidth / 2))
        return ctx.makeImage()
    }

    private func swim(_ f: Fish, t: Double, dt: CGFloat, remote: RemoteFish? = nil) {
        f.hits.removeAll { t - $0 > 90 }
        if f.hits.count != f.shownHits {
            f.shownHits = f.hits.count
            dress(f)
        }
        let tt = CGFloat(t)
        if let r = remote {
            // A follower draws where the leader put it; only the bob is
            // local, and that runs on the shared wall clock.
            f.x = r.x; f.homeY = r.homeY; f.dir = r.dir; f.facing = r.facing
            f.alpha = r.alpha; f.glow = r.glow; f.boost = r.boost; f.lunge = r.lunge
            f.leaving = r.leaving
            f.y = walkers ? f.homeY : f.homeY + (sin(tt * 0.55 + f.phase) * 9 + sin(tt * 1.7 + f.phase * 2) * 2) * u
            f.depth = r.depth
        } else {
        let w = bounds.width
        let ghost = f.kind == "ghost"
        let target: CGFloat = f.leaving ? 0 : ghost ? 0.3 : 1
        f.alpha += (target - f.alpha) * min(1, dt * (f.leaving ? 0.6 : 1.5))
        // Leaving fish speed up toward the edge they're facing.
        // Far fish cover less screen: parallax.
        let pace = f.speed * (0.45 + 0.85 * f.depth) * (1 + f.boost * 2.2) * (f.leaving ? 2.4 : 1) * (ghost ? 0.45 : 1)
        f.x += f.dir * pace * dt
        if !f.leaving {
            // Off one side, back in on the other.
            let edge = f.length * 1.3
            if f.x > w + edge { f.x = -edge }
            if f.x < -edge { f.x = w + edge }
            // Every so often: a new depth to drift to, maybe a turn — so the
            // school reaches the corners instead of holding lanes.
            if Double(t) > f.nextWander {
                f.nextWander = Double(t) + Double.random(in: 14...40)
                f.targetY = bounds.height * CGFloat.random(in: 0.06...0.94)
                if Double.random(in: 0...1) < 0.35 { f.dir = -f.dir }
            }
            f.homeY += (f.targetY - f.homeY) * min(1, dt * 0.07)
        }
        f.facing += (f.dir - f.facing) * min(1, dt * 4)
        f.y = walkers ? f.homeY : f.homeY + (sin(tt * 0.55 + f.phase) * 9 + sin(tt * 1.7 + f.phase * 2) * 2) * u
        f.boost *= pow(0.5, dt / 1.2)
        f.glow *= pow(0.5, dt / 0.9)
        f.lunge *= pow(0.5, dt / 3)
        // Slow wander in and out of the tank; a generation lunges forward.
        let wander = sin(tt * 0.05 + f.phase * 3) * 0.18
        f.depth = min(1, max(0, f.reach + wander + f.lunge * 0.5))
        }
        let near = 0.6 + 0.6 * f.depth
        let size = f.length * near
        f.size = size
        f.root.position = CGPoint(x: f.x, y: f.y)
        f.root.zPosition = f.depth
        f.root.opacity = Float(f.alpha)
        // Handle under the belly, its table under the handle: one block
        // beneath each fish, the water above it left clear.
        let labelH = f.label.isHidden ? 0 : f.label.bounds.height
        let tagY = -size * 0.3 - Self.subGap * u - labelH
        if PixelFont.available {
            let px = 1 / max(1, scale.rounded())
            f.tag.transform = CATransform3DIdentity
            f.tag.position = CGPoint(x: (f.x / px).rounded() * px - f.x,
                                     y: ((f.y + tagY) / px).rounded() * px - f.y)
        } else {
            f.tag.transform = CATransform3DMakeScale(0.85 + 0.2 * f.depth, 0.85 + 0.2 * f.depth, 1)
            f.tag.position = CGPoint(x: 0, y: tagY)
        }
        // Session line rides under the belly.
        // Pixel type is never scaled, and its corner lands on a whole pixel.
        if f.sub.pixelated {
            let px = 1 / max(1, scale.rounded())
            let w = f.sub.bounds.width
            f.sub.transform = CATransform3DIdentity
            f.sub.anchorPoint = CGPoint(x: 0, y: 1)
            let left = ((f.x - w / 2) / px).rounded() * px - f.x
            let top = ((f.y + tagY - 2 * u) / px).rounded() * px - f.y
            f.sub.position = CGPoint(x: left, y: top)
        } else {
            f.sub.anchorPoint = CGPoint(x: 0.5, y: 1)
            f.sub.transform = f.tag.transform
            f.sub.position = CGPoint(x: 0, y: tagY - 2 * u)
        }
        updateSub(f, now: t)
        let tilt = walkers ? 0 : sin(tt * 1.1 + f.phase) * 0.06
        f.body.transform = CATransform3DRotate(CATransform3DMakeScale(f.facing, 1, 1), tilt, 0, 0, 1)
        let wag = sin(tt * (4 + f.boost * 10) + f.phase) * (0.22 + f.boost * 0.3)
        f.tail.transform = CATransform3DMakeRotation(wag, 0, 0, 1)
        f.body.shadowRadius = (2 + f.glow * 10) * u
        // Facing +x is yaw 0, -x is π; a slow sway shows the body's volume.
        let yaw = (1 - f.facing) * 0.5 * .pi + sin(tt * 0.8 + f.phase) * (walkers ? 0.12 : 0.32)
        poses.append(Pose(id: f.id, x: f.x, y: f.y, length: size, depth: f.depth, yaw: yaw, tilt: tilt * 2,
                          wag: wag * 1.6, color: color(for: f.kind), alpha: f.alpha,
                          glow: f.glow,
                          skin: f.skins.isEmpty ? "" : f.skins[(Int(t / 9) + Int(f.phase * 10)) % f.skins.count],
                          staff: f.kind == "staff"))
    }

    /// Each fish plus its name tag is a box; overlapping boxes push their
    /// swim depths apart a little every frame, so tags rarely collide and the
    /// school stays between the counters and the hot list.
    /// Water between a fish's belly and its table, and around each fish.
    private static let subGap: CGFloat = 8
    private static let schoolMargin: CGFloat = 14

    private func spaceOut(dt: CGFloat) {
        let school = fish.values.filter { !$0.leaving && $0.alpha > 0.05 }
        let box = { (f: Fish) -> CGRect in
            let tagScale = 0.85 + 0.2 * f.depth
            let m = Self.schoolMargin * self.u
            let half = max(f.size * 0.6, f.tagW * tagScale / 2) + m
            // The table's real height, so a five-row box keeps its own water.
            let table = f.sub.isHidden ? 0 : f.sub.bounds.height
            let labelH = f.label.isHidden ? 0 : f.label.bounds.height
            let low = f.y - f.size * 0.3 - Self.subGap * self.u - labelH - 2 * self.u - table - m
            let high = f.y + f.size * (self.walkers ? 0.72 : 0.24) + m
            return CGRect(x: f.x - half, y: low, width: half * 2, height: high - low)
        }
        let boxes = school.map(box)
        for a in 0..<school.count {
            for b in (a + 1)..<max(a + 1, school.count) {
                let overlap = boxes[a].intersection(boxes[b])
                guard !overlap.isNull, overlap.height > 0, overlap.width > 0 else { continue }
                let (upper, lower) = school[a].homeY >= school[b].homeY ? (school[a], school[b]) : (school[b], school[a])
                let push = min(overlap.height, 60 * u) * min(1, dt * 1.6)
                upper.homeY += push * 0.5
                lower.homeY -= push * 0.5
            }
        }
        let floor = max(112 * u, hudTop + 10 * u), ceiling = bounds.height - 34 * u - 7 * 19 * u
        for f in school {
            // The name and table hang below the body; they clear the HUD too.
            let labelH = f.label.isHidden ? 0 : f.label.bounds.height
            let table = f.sub.isHidden ? 0 : f.sub.bounds.height
            let low = floor + f.size * 0.3 + Self.subGap * u + labelH + 2 * u + table
            let high = ceiling - f.size * (walkers ? 0.72 : 0.24) - 44 * u
            f.homeY = min(max(f.homeY, low), max(low, high))
        }
    }

    /// The pixel face at the whole multiple of its cell that stands in for
    /// the system face's size; the system face when the file is missing.
    func face(_ system: NSFont) -> NSFont {
        guard let base = PixelFont.font(scale: scale) else { return system }
        let px = system.pointSize * scale
        let cells = max(1, min(3, (px / 16 + 0.25).rounded(.down)))
        // `base` already carries the boost.
        return NSFont(descriptor: base.fontDescriptor, size: base.pointSize * cells) ?? base
    }

    /// Syntax colours for the stats box, per appearance.
    private var codeKey: NSColor { NSColor(cgColor: ink(0.48)) ?? .gray }
    private var codeString: NSColor { dark ? NSColor(srgbRed: 0.98, green: 0.80, blue: 0.45, alpha: 1)
                                           : NSColor(srgbRed: 0.62, green: 0.36, blue: 0.02, alpha: 1) }
    private var codeTime: NSColor { dark ? NSColor(srgbRed: 0.78, green: 0.62, blue: 1.0, alpha: 1)
                                         : NSColor(srgbRed: 0.42, green: 0.20, blue: 0.78, alpha: 1) }
    private var codeNumber: NSColor { dark ? NSColor(srgbRed: 0.45, green: 0.90, blue: 0.95, alpha: 1)
                                           : NSColor(srgbRed: 0.0, green: 0.45, blue: 0.55, alpha: 1) }
    private var codeSource: NSColor { dark ? NSColor(srgbRed: 1.0, green: 0.55, blue: 0.70, alpha: 1)
                                           : NSColor(srgbRed: 0.75, green: 0.12, blue: 0.38, alpha: 1) }

    /// A small table under the fish — where, how long, pages, clicks, from —
    /// keys dim, values coloured by type. Redrawn only when it changes.
    private func updateSub(_ f: Fish, now: Double) {
        // Everything in the box moves at most once a minute or per file.
        let clockFrom = f.leftAt > 0 ? f.leftAt : f.since
        let key = (clockFrom > 0 ? max(0, Int(now - clockFrom) / 60) : 0) &* 31 &+ dataVersion
        guard key != f.subKey else { return }
        f.subKey = key
        var rows: [(String, String, NSColor)] = []
        // One Unifont symbol per key keeps the table narrow.
        let span = { (s: Int) -> String in let m = s / 60; return m < 1 ? "<1m" : m < 60 ? "\(m)m" : "\(m / 60)h \(m % 60)m" }
        if f.leftAt > 0 {
            rows.append(("⏱", "stayed " + span(Int(f.stayed)), codeTime))
            rows.append(("↩", "left " + span(Int(now - f.leftAt)) + " ago", codeKey))
        }
        if !f.place.isEmpty { rows.append(("⌖", f.place, codeString)) }
        if f.since > 0, f.leftAt == 0 {
            let m = max(0, Int(now - f.since) / 60)
            rows.append(("⏱", m < 60 ? "\(m)m" : "\(m / 60)h \(m % 60)m", codeTime))
        }
        if f.pages > 0 { rows.append(("☰", "\(f.pages)", codeNumber)) }
        if f.clicks > 0 { rows.append(("↖", "\(f.clicks)", codeNumber)) }
        if !f.from.isEmpty { rows.append(("↪", f.from, codeSource)) }
        let text = rows.map { "\($0.0)\t\($0.1)" }.joined(separator: "\n")
        guard text != f.subText else { return }
        f.subText = text
        f.sub.isHidden = rows.isEmpty
        guard !rows.isEmpty else { f.tagW = ((f.label.string as? NSAttributedString)?.size().width ?? 0) + 34 * tu; return }
        let pixel = PixelFont.font(scale: scale)
        let font = pixel ?? NSFont.monospacedSystemFont(ofSize: max(10, 10 * tu), weight: .medium)
        f.sub.pixelated = pixel != nil
        // On the pixel grid, padding and gaps are whole pixels too.
        let px = 1 / max(1, scale.rounded())
        let snap = { (v: CGFloat) in pixel == nil ? v : (v / px).rounded() * px }
        let pad = snap(pixel == nil ? 5 * tu : 4 * px)
        let keyW = ceil(rows.map { ($0.0 as NSString).size(withAttributes: [.font: font]).width }.max() ?? 8)
        let gap = snap(pixel == nil ? 5 * tu : 3 * px)
        let ascent = ceil(CTFontGetAscent(font))
        let lineH = ceil(ascent + CTFontGetDescent(font))
        var valueW: CGFloat = 0
        f.sub.cells = rows.map { (k, v, color) in
            let value = NSAttributedString(string: v, attributes: [.font: font, .foregroundColor: color])
            valueW = max(valueW, ceil(value.size().width))
            return (NSAttributedString(string: k, attributes: [.font: font, .foregroundColor: color]), value)
        }
        let inset = snap(pixel == nil ? gap * 0.9 : 2 * px)
        let step = lineH + gap
        let width = pad + keyW + gap * 2 + valueW + pad + px
        let height = inset * 2 + CGFloat(rows.count) * lineH + CGFloat(rows.count - 1) * gap + px
        f.sub.string = nil
        f.sub.bounds = CGRect(x: 0, y: 0, width: width, height: height)
        f.sub.topInset = inset
        f.sub.cellLeft = pad
        f.sub.cellValueX = pad + keyW + gap * 2
        f.sub.cellLineHeight = lineH
        f.sub.cellAscent = ascent
        f.sub.gridRows = rows.count
        f.sub.gridRowStep = step
        f.sub.gridGap = gap
        f.sub.gridColumnX = pad + keyW + gap
        f.sub.gridColor = f.sub.borderColor.map { $0.copy(alpha: 0.45) ?? $0 }
        let nameW = (f.label.string as? NSAttributedString)?.size().width ?? 0
        f.tagW = max(nameW + 34 * tu, width)
    }

    // ── events ───────────────────────────────────────────────────────────

    private func play(_ event: [String: Any], now: Double) {
        let kind = event["kind"] as? String ?? ""
        let id = event["fish"] as? String ?? ""
        let f = fish[id] ?? spawn(id: id, now: now)
        f.leaving = false
        f.boost = 1
        f.glow = 1
        f.lunge = 1
        if kind == "generation" {
            f.hits.append(now)
            tallyTarget[0] += 1
            tallyTarget[1] += Self.num(event["credits"])
        }
        if let cat = event["cat"] as? String, let tile = tiles[cat] { tile.target += 1 }
        let c = color(forEvent: kind)
        let at = CGPoint(x: f.x, y: f.y)
        ripple(at: at, color: c, radius: (kind == "signup" ? 60 : 36) * u)
        if kind == "signup" { ripple(at: at, color: c, radius: 90 * u, delay: 0.25) }
        let text = event["text"] as? String ?? ""
        let credits = Int(Self.num(event["credits"]))
        let words: String
        switch kind {
        case "generation": words = credits > 0 ? "\(text)  \(credits)cr" : text
        case "failed": words = "✕ \(text)"
        case "signup": words = "new"
        default: words = text.isEmpty ? kind : text
        }
        bubble(words, at: at, color: c, big: kind == "generation")
        if kind == "generation" || kind == "signup" {
            for i in 0..<5 { fizz(at: at, color: c, delay: Double(i) * 0.12) }
        }
        log(event, at: Self.num(event["at"]))
    }

    private func log(_ event: [String: Any], at: Double) {
        let id = event["fish"] as? String ?? ""
        let name = fish[id].map { $0.kind == "anon" || $0.name.isEmpty ? "someone" : $0.name } ?? "someone"
        let kind = event["kind"] as? String ?? ""
        let text = event["text"] as? String ?? ""
        let credits = Int(Self.num(event["credits"]))
        let cost = credits > 0 ? "  · \(credits) cr" : ""
        let what: String
        switch kind {
        case "generation": what = text + cost
        case "chat": what = text.isEmpty ? "chat" : "chat  \(text)" + cost
        case "failed": what = "failed  \(text)"
        case "signup": what = "signed up"
        case "publish": what = "published"
        case "workflow": what = "new workflow"
        default: what = text.isEmpty ? kind : text
        }
        ticker.insert((at, name, what, kind), at: 0)
        if ticker.count > 24 { ticker.removeLast(ticker.count - 24) }
        crawlFresh = min(crawlFresh + 1, ticker.count)
    }

    private func ripple(at p: CGPoint, color: NSColor, radius: CGFloat, delay: Double = 0) {
        let ring = CAShapeLayer()
        ring.actions = Self.still
        ring.path = CGPath(ellipseIn: CGRect(x: -radius, y: -radius, width: radius * 2, height: radius * 2), transform: nil)
        ring.fillColor = nil
        ring.strokeColor = color.cgColor
        ring.lineWidth = 1.2 * u
        ring.position = p
        ring.opacity = 0
        insertSublayer(ring, below: hud)
        let grow = CABasicAnimation(keyPath: "transform.scale")
        grow.fromValue = 0.1
        grow.toValue = 1
        let fade = CAKeyframeAnimation(keyPath: "opacity")
        fade.values = [0.9, 0.5, 0]
        fade.keyTimes = [0, 0.5, 1]
        animate(ring, [grow, fade], duration: 1.8, delay: delay, ease: .easeOut)
    }

    private func bubble(_ words: String, at p: CGPoint, color: NSColor, big: Bool) {
        let t = Self.text(HaloTextLayer())
        t.contentsScale = scale
        t.string = NSAttributedString(string: words, attributes: [
            .font: face(NSFont.systemFont(ofSize: (big ? 17 : 14) * u, weight: big ? .heavy : .bold)),
            .foregroundColor: color.blended(withFraction: dark ? 0.15 : 0.45, of: dark ? .white : .black) ?? color,
        ])
        t.alignmentMode = .center
        t.bounds = CGRect(x: 0, y: 0, width: 300 * u, height: 24 * u)
        t.position = CGPoint(x: p.x, y: p.y + 14 * u)
        (t as? HaloTextLayer)?.haloRadius = 3
        (t as? HaloTextLayer)?.halo = dark ? Self.darkHalo : Self.lightHalo
        t.opacity = 0
        insertSublayer(t, below: hud)
        let rise = CABasicAnimation(keyPath: "position.y")
        rise.fromValue = p.y + 30 * u
        rise.toValue = p.y + 100 * u
        let fade = CAKeyframeAnimation(keyPath: "opacity")
        fade.values = [0, 1, 1, 0]
        fade.keyTimes = [0, 0.1, 0.65, 1]
        animate(t, [rise, fade], duration: 4.5, delay: 0, ease: .easeOut)
    }

    private func fizz(at p: CGPoint, color: NSColor, delay: Double) {
        let r = CGFloat.random(in: 1.2...3) * u
        let dot = CAShapeLayer()
        dot.actions = Self.still
        dot.path = CGPath(ellipseIn: CGRect(x: -r, y: -r, width: r * 2, height: r * 2), transform: nil)
        dot.fillColor = nil
        dot.strokeColor = color.withAlphaComponent(0.9).cgColor
        dot.lineWidth = 0.7 * u
        dot.position = p
        dot.opacity = 0
        insertSublayer(dot, below: hud)
        let path = CGMutablePath()
        path.move(to: p)
        let dx = CGFloat.random(in: -16...16) * u
        path.addCurve(to: CGPoint(x: p.x + dx, y: p.y + CGFloat.random(in: 40...80) * u),
                      control1: CGPoint(x: p.x - dx, y: p.y + 18 * u),
                      control2: CGPoint(x: p.x + dx * 1.5, y: p.y + 36 * u))
        let move = CAKeyframeAnimation(keyPath: "position")
        move.path = path
        let fade = CAKeyframeAnimation(keyPath: "opacity")
        fade.values = [0, 1, 0]
        fade.keyTimes = [0, 0.15, 1]
        animate(dot, [move, fade], duration: 2.4, delay: delay, ease: .easeOut)
    }

    private func animate(_ layer: CALayer, _ parts: [CAAnimation], duration: Double, delay: Double,
                         ease: CAMediaTimingFunctionName) {
        let group = CAAnimationGroup()
        group.animations = parts
        group.duration = duration
        group.beginTime = CACurrentMediaTime() + delay
        group.fillMode = .both
        group.isRemovedOnCompletion = false
        group.timingFunction = CAMediaTimingFunction(name: ease)
        CATransaction.begin()
        CATransaction.setCompletionBlock { layer.removeFromSuperlayer() }
        layer.add(group, forKey: "life")
        CATransaction.commit()
    }

    // ── tally ────────────────────────────────────────────────────────────

    private func attributed(_ s: String, size: CGFloat, weight: NSFont.Weight, alpha: CGFloat = 1,
                            mono: Bool = false) -> NSAttributedString {
        let font = face(mono ? NSFont.monospacedDigitSystemFont(ofSize: size, weight: weight)
                             : NSFont.systemFont(ofSize: size, weight: weight))
        return NSAttributedString(string: s, attributes: [
            .font: font, .foregroundColor: NSColor(cgColor: ink(alpha)) ?? .white])
    }

    /// Trims to `width` with an ellipsis set in the same face. CATextLayer's
    /// own truncation token comes from another font and knocks a Unifont row
    /// off its baseline.
    private func fitted(_ s: String, size: CGFloat, weight: NSFont.Weight, width: CGFloat) -> String {
        let font = face(NSFont.systemFont(ofSize: size, weight: weight))
        let measure = { (t: String) in ceil((t as NSString).size(withAttributes: [.font: font]).width) }
        guard measure(s) > width else { return s }
        var cut = s
        while !cut.isEmpty, measure(cut + "…") > width { cut.removeLast() }
        return cut + "…"
    }

    private func layoutHUD(now: Double, dt: CGFloat) {
        hud.isHidden = snapshot.observedAt == 0
        guard !hud.isHidden else { return }
        hud.frame = bounds
        let w = bounds.width
        let pad = 16 * u
        let halo = dark ? Self.darkHalo : Self.lightHalo
        let shadow = { (t: CALayer) -> Void in (t as? HaloTextLayer)?.halo = halo }

        let bandH: CGFloat = 0
        for l in [band, bandRule, bandTag, crawl] as [CALayer] { l.isHidden = true }
        let screenW = link == nil ? w : max(1, viewport.width)
        layoutHeat(now: now, width: w, shadow: shadow)

        // Lower third over the water: the hour's numbers and its shape.
        let baseY = bandH + 12 * u
        liveChip.isHidden = true
        headline.isHidden = true

        stats.isHidden = true
        tallyText.forEach { $0.number.isHidden = true; $0.caption.isHidden = true }
        var x = pad
        var shown: [Tile] = []
        let tileH = 26 * ts
        hudTop = baseY + tileH + 16 * ts
        // A site that fell out of the feed has no row to lay it out again:
        // put its tile away, or it stays where it last stood, under the others.
        let live = Set(categories.map(\.key))
        for (key, tile) in tiles where !live.contains(key) {
            tile.root.isHidden = true
            tile.feed = -1; tile.target = 0; tile.shown = 0
        }
        for row in categories {
            guard let style = row.style else { continue }
            let key = row.key
            let count = row.count
            let tile = tiles[key] ?? {
                let t = Tile()
                t.root.actions = Self.still
                for text in [t.number, t.caption] { t.root.addSublayer(Self.text(text)) }
                hud.addSublayer(t.root)
                tiles[key] = t
                return t
            }()
            if count != tile.feed { tile.feed = count; tile.target = count }
            // Roll fast when far behind, settle gently near the target.
            let gap = tile.target - tile.shown
            tile.shown += gap * Double(min(1, dt * 2.2))
            if abs(gap) < 0.5 { tile.shown = tile.target }
            tile.root.isHidden = !row.keep && tile.target < 0.5 && tile.shown < 0.5
            guard !tile.root.isHidden else { continue }
            let digits = Int(tile.shown.rounded()).formatted()
            if tile.number.name != digits + (dark ? "d" : "l") {
            tile.number.name = digits + (dark ? "d" : "l")
            let head = NSMutableAttributedString(string: style.glyph + " ", attributes: [
                .font: face(NSFont.systemFont(ofSize: max(11, 15 * tu), weight: .black)), .foregroundColor: style.color])
            head.append(attributed(digits, size: max(11, 22 * tu), weight: .black, mono: true))
            tile.number.string = head
            tile.headW = ceil(tile.number.preferredFrameSize().width)
            }
            if tile.caption.name != row.label {
                tile.caption.name = row.label
                tile.caption.string = NSAttributedString(string: row.label, attributes: [
                    .font: face(NSFont.systemFont(ofSize: max(11, 10 * tu), weight: .heavy)),
                    .foregroundColor: style.color.withAlphaComponent(0.85)])
                tile.capW = ceil(tile.caption.preferredFrameSize().width)
            }
            let headW = tile.headW, capW = tile.capW
            let tileW = max(headW, capW, 64 * ts)
            tile.root.frame = CGRect(x: x, y: baseY, width: tileW, height: tileH + 16 * ts)
            tile.caption.frame = CGRect(x: 0, y: 0, width: tileW, height: 13 * ts)
            tile.number.frame = CGRect(x: 0, y: 12 * ts, width: headW + 4, height: tileH)
            shadow(tile.number)
            shadow(tile.caption)
            x += tileW + 26 * ts
            shown.append(tile)
        }
        // Right-align the row against the screen's bottom-right corner.
        let rowW = max(0, x - 26 * ts - pad)
        let right = w - pad - (badgeInset > 0 ? badgeInset + 4 * ts : 0)
        let shift = right - rowW - pad
        for tile in shown { tile.root.frame.origin.x += shift }
        // In a tank this lands whole on the last screen, above its tiles.
        let chartW = min(max(rowW, 320 * ts), screenW - 2 * pad)
        layoutChart(right: right, bottom: baseY + tileH + 30 * ts, width: chartW)
        if let chart = reef?.frame, chart.width > 0 { hudTop = max(hudTop, chart.maxY + 16 * ts) }
        let refRight = layoutReferrers(left: pad, bottom: baseY, shadow: shadow)
        if !refHead.isHidden { hudTop = max(hudTop, refHead.frame.maxY) }
        layoutWorld(left: pad, bottom: (refHead.isHidden ? baseY : refHead.frame.maxY) + 14 * ts, shadow: shadow)
        // Three columns along the bottom: came-from, the ticker, then the
        // tiles under their chart. The ticker takes what the other two leave,
        // so it never runs under the numbers or the chart's axis.
        let gutter = 28 * ts
        let hotLeft = refRight + gutter
        let hotRoom = right - chartW - gutter - hotLeft
        layoutHot(now: now, left: hotLeft, bottom: baseY, width: min(900 * ts, screenW * 0.5, hotRoom), dt: dt, shadow: shadow)
    }

    /// Sixty minutes of generations, each a stack of its categories' colours,
    /// with a time axis and the busiest minute marked. Redrawn only when the
    /// feeder's numbers change.
    /// Name, bar, count — each column as wide as its content. Returns the
    /// block's right edge.
    private func layoutReferrers(left: CGFloat, bottom: CGFloat, shadow: (CALayer) -> Void) -> CGFloat {
        let rows = Array(referrers.prefix(refRows.count))
        let rowH = 17 * ts
        let nameMax = 150 * ts, barW = 96 * ts, gap = 8 * ts
        let size = max(11, 12 * tu)
        refHead.isHidden = rows.isEmpty
        guard !rows.isEmpty else {
            for row in refRows { for l in [row.name, row.bar, row.count] as [CALayer] { l.isHidden = true } }
            return left
        }
        let headKey = "came from · 24h\(dark)"
        if refHead.name != headKey {
            refHead.name = headKey
            refHead.string = attributed("came from · last 24h", size: max(11, 10 * tu), weight: .heavy, alpha: 0.55)
        }
        refHead.frame = CGRect(x: left, y: bottom + CGFloat(rows.count) * rowH + 4 * ts, width: 300 * ts, height: 14 * ts)
        shadow(refHead)
        for (i, row) in refRows.enumerated() where i < rows.count {
            let (domain, people) = rows[i]
            let key = "\(domain)\(Int(people))\(dark)\(Int(nameMax))"
            guard row.name.name != key else { continue }
            row.name.name = key
            let direct = domain == "direct"
            row.name.string = attributed(fitted(domain, size: size, weight: .bold, width: nameMax), size: size,
                                         weight: direct ? .semibold : .bold, alpha: direct ? 0.6 : 0.92)
            row.count.string = attributed(Int(people).formatted(), size: size, weight: .heavy, alpha: direct ? 0.6 : 0.9, mono: true)
            refWidths[i] = (min(nameMax, ceil(row.name.preferredFrameSize().width)), ceil(row.count.preferredFrameSize().width))
        }
        let nameW = refWidths.prefix(rows.count).map { $0.name }.max() ?? 0
        let countW = refWidths.prefix(rows.count).map { $0.count }.max() ?? 0
        // One square-root scale for every row: direct can dwarf the sources
        // without flattening them to nothing.
        let top = max(rows.map { $0.1 }.max() ?? 1, 1)
        let barX = left + nameW + gap
        for (i, row) in refRows.enumerated() {
            let visible = i < rows.count
            for l in [row.name, row.bar, row.count] as [CALayer] { l.isHidden = !visible }
            guard visible else { continue }
            let (domain, people) = rows[i]
            let y = bottom + CGFloat(rows.count - 1 - i) * rowH
            let barLen = max(3 * ts, barW * CGFloat((people / top).squareRoot()))
            row.name.frame = CGRect(x: left, y: y, width: nameW + 2, height: rowH)
            row.bar.frame = CGRect(x: barX, y: y + rowH * 0.3, width: barLen, height: rowH * 0.42)
            row.bar.cornerRadius = rowH * 0.21
            row.bar.backgroundColor = (domain == "direct" ? NSColor(cgColor: ink(0.25)) ?? .gray
                                                          : NSColor(srgbRed: 0.45, green: 0.80, blue: 1.0, alpha: 0.85)).cgColor
            // The count rides the end of its own bar.
            row.count.alignmentMode = .left
            row.count.frame = CGRect(x: barX + barLen + gap * 0.75, y: y, width: countW + 2, height: rowH)
            shadow(row.name)
            shadow(row.count)
        }
        return barX + barW + gap * 0.75 + countW
    }

    /// The world from the air (FlyBy.swift), a small wireframe model in the
    /// bottom left corner above "came from", and the census under it.
    private func layoutWorld(left: CGFloat, bottom: CGFloat, shadow: (CALayer) -> Void) {
        // ⚡ The feeder's fast lane (world.json): rax's map again, written
        // whenever it changes and looked for twice a second. It wins over
        // the snapshot's copy.
        let clock = CACurrentMediaTime()
        if clock - worldLivePolled > 0.5 {
            worldLivePolled = clock
            let stamp = (try? FileManager.default.attributesOfItem(atPath: Self.liveFile))?[.modificationDate] as? Date
            if stamp != worldLiveStamp {
                worldLiveStamp = stamp
                worldLive = FileManager.default.contents(atPath: Self.liveFile)
                    .flatMap { try? JSONSerialization.jsonObject(with: $0) as? [String: Any] } ?? [:]
                worldLiveVersion += 1
            }
        }
        let map = worldLive["structure"] != nil ? worldLive : world
        let empty = (map["structure"] as? [Any] ?? []).isEmpty
        worldFly.isHidden = empty
        worldHead.isHidden = empty
        guard !empty else { return }
        let panel = CGRect(x: left, y: bottom + 18 * ts, width: (250 * u).rounded(), height: (156 * u).rounded())
        let version = "\(worldLiveVersion)\(dataVersion)\(dark)\(Int(ts * 100))\(scale)\(panel.size)"
        if worldVersion != version {
            worldVersion = version
            var drawn = WorldFlyLayer.World(map)
            // The people file is the newer word on who is where, unless the
            // feeder stopped writing it and the map has moved on since.
            if let live = peopleLive, let stamp = peopleStamp, stamp.timeIntervalSince(worldLiveStamp ?? .distantPast) > -6 {
                drawn.people = live
            }
            worldCensus = drawn.census
            worldFly.show(drawn, size: panel.size,
                          style: .init(font: face(NSFont.systemFont(ofSize: max(11, 10 * tu), weight: .heavy)),
                                       ink: NSColor(cgColor: ink()) ?? .white,
                                       halo: dark ? Self.darkHalo : Self.lightHalo, scale: scale))
        }
        if clock - peoplePolled > 0.1 {
            peoplePolled = clock
            let stamp = (try? FileManager.default.attributesOfItem(atPath: Self.peopleFile))?[.modificationDate] as? Date
            if stamp != peopleStamp {
                peopleStamp = stamp
                if let rows = FileManager.default.contents(atPath: Self.peopleFile)
                    .flatMap({ try? JSONSerialization.jsonObject(with: $0) as? [String: Any] })?["people"] as? [[String: Any]] {
                    let live = WorldFlyLayer.people(rows)
                    peopleLive = live
                    // Slide over the time since the last reading, within reason.
                    worldFly.move(live, over: min(0.6, max(0.12, clock - peopleRead)))
                    peopleRead = clock
                }
            }
        }
        let title = worldCensus
        if worldHead.name != title + (dark ? "d" : "l") {
            worldHead.name = title + (dark ? "d" : "l")
            worldHead.string = attributed(title, size: max(11, 10 * tu), weight: .heavy, alpha: 0.55)
        }
        worldFly.anchorPoint = .zero
        worldFly.position = panel.origin
        worldHead.frame = CGRect(x: left, y: bottom, width: 420 * ts, height: 14 * ts)
        shadow(worldHead)
    }

    /// Hosts round lith, tailnet machines round this Mac, hung from the top
    /// left of the first screen in the strip the walkers keep out of.
    /// Rebuilt when the probe, the appearance or the type changes.
    private func layoutConstellation(now: Double) {
        let nodes = infra["nodes"] as? [[String: Any]] ?? []
        let at = Self.num(infra["observedAt"])
        let fresh = !nodes.isEmpty && now - at < 300
        constellation.isHidden = !fresh
        guard fresh else { return }
        let top = bounds.height - topInset - 10 * u
        let font = face(NSFont.systemFont(ofSize: max(11, 12 * tu), weight: .bold))
        let cell = ceil(font.ascender - font.descender)
        // Walkers stop 167 units from the top (see spaceOut); the hub's name
        // takes a row and a half under the fan.
        let rows = max(3, Int((top - (bounds.height - 167 * u) - cell * 1.5 - 12 * u) / (cell + 2 * u)) + 1)
        let key = "\(infraVersion)\(dark)\(PixelFont.boost)\(rows)\(scale)\(Int(u * 100))"
        if constellation.key != key {
            constellation.key = key
            constellation.contentsScale = scale
            constellation.show(clusters(nodes, rows: rows, now: now),
                               style: .init(font: font, ink: NSColor(cgColor: ink()) ?? .white,
                                            halo: dark ? Self.darkHalo : Self.lightHalo, number: codeNumber,
                                            dark: dark, u: u, scale: scale))
        }
        constellation.position = CGPoint(x: (16 * u).rounded(), y: top.rounded())
        if at != infraPulsed {
            infraPulsed = at
            constellation.pulse()
        }
    }

    /// Tints from the old process tree's categories.
    private static let infraTint: [String: NSColor] = [
        "kernel": NSColor(srgbRed: 0.53, green: 0.80, blue: 1.00, alpha: 1),
        "proxy": NSColor(srgbRed: 0.42, green: 0.62, blue: 1.00, alpha: 1),
        "db": NSColor(srgbRed: 1.00, green: 0.92, blue: 0.42, alpha: 1),
        "dev": NSColor(srgbRed: 0.42, green: 1.00, blue: 0.62, alpha: 1),
        "bridge": NSColor(srgbRed: 1.00, green: 0.66, blue: 0.42, alpha: 1),
    ]

    private func clusters(_ nodes: [[String: Any]], rows: Int, now: Double) -> [ConstellationLayer.Cluster] {
        typealias Star = ConstellationLayer.Star
        let white = NSColor(white: 0.97, alpha: 1)
        let host = { (n: [String: Any]) -> Star in
            let state = n["state"] as? String ?? "up", ms = Self.num(n["ms"])
            let tail = state == "down" || state == "late" ? state
                : "\(Int(ms))" + (n["note"] as? String == "baking" ? " ⚒" : "")
            return Star(id: "h:" + (n["id"] as? String ?? ""), name: n["id"] as? String ?? "", tail: tail, state: state,
                        tint: Self.infraTint[n["kind"] as? String ?? ""] ?? white, ms: state == "up" || state == "slow" ? ms : 0)
        }
        // The hub is whatever most hosts hang off (lith).
        var counts: [String: Int] = [:]
        for n in nodes { counts[n["parent"] as? String ?? "", default: 0] += 1 }
        let hubID = counts.filter { !$0.key.isEmpty }.max { $0.value < $1.value }?.key ?? ""
        var out: [ConstellationLayer.Cluster] = []
        if let hub = nodes.first(where: { $0["id"] as? String == hubID }) {
            let up = nodes.first { $0["id"] as? String == hub["parent"] as? String }
            let around = Array(nodes.filter { $0["parent"] as? String == hubID }.prefix(rows * 2 - (up == nil ? 0 : 1)))
            let leftCount = (around.count + (up == nil ? 0 : 1) + 1) / 2 - (up == nil ? 0 : 1)
            // The edge sits on lith's own row, straight to its left.
            let left = around.prefix(leftCount).map(host) + (up.map { [host($0)] } ?? [])
            out.append(.init(hub: host(hub), left: left, right: around.dropFirst(leftCount).map(host)))
        }
        guard let tailnet = infra["tailnet"] as? [String: Any], let me = tailnet["self"] as? [String: Any] else { return out }
        let ago = { (seen: Double) -> String in
            guard seen > 0 else { return "" }
            let s = max(0, now - seen)
            return s < 5400 ? "\(Int(s / 60))m" : s < 172800 ? "\(Int(s / 3600))h" : "\(Int(s / 86400))d"
        }
        // Machines that are already a host star above are not drawn twice.
        let peers = (tailnet["peers"] as? [[String: Any]] ?? []).filter { ($0["node"] as? String ?? "").isEmpty }.prefix(rows * 2)
        let machines = peers.map { p -> Star in
            let online = p["online"] as? Bool ?? false
            return Star(id: "m:" + (p["name"] as? String ?? ""), name: p["name"] as? String ?? "",
                        tail: online ? "" : ago(Self.num(p["lastSeen"])), state: online ? "up" : "away", machine: true, tint: white)
        }
        let slowLink = infra["uplink"] as? String == "slow"
        let leftCount = (machines.count + 1) / 2
        out.append(.init(hub: Star(id: "m:self", name: me["name"] as? String ?? "this mac", tail: slowLink ? "slow link" : "",
                                   state: slowLink ? "slow" : "up", tint: Self.infraTint["kernel"] ?? white),
                         left: Array(machines.prefix(leftCount)), right: Array(machines.dropFirst(leftCount))))
        return out
    }

    private func layoutChart(right: CGFloat, bottom: CGFloat, width: CGFloat) {
        let height = 64 * tu
        let frame = CGRect(x: right - width, y: bottom + 14 * tu, width: width, height: height)
        let key = "\(frame)\(dark)\(dataVersion)"
        guard key != chartKey else { return }
        chartKey = key
        var stacks = [[(NSColor, CGFloat)]](repeating: [], count: 60)
        for row in categories where row.chart {
            guard let c = row.style?.color else { continue }
            for (i, v) in row.spark.prefix(60).enumerated() where v > 0 { stacks[i].append((c, v)) }
        }
        let totalsPerMinute = stacks.map { $0.reduce(0) { $0 + $1.1 } }
        let peak = max(totalsPerMinute.max() ?? 1, 1)
        // The columns themselves are drawn in 3D by Reef.
        reef = ReefData(frame: frame, stacks: stacks, peak: peak, version: (reef?.version ?? 0) + 1)
        let peakAt = totalsPerMinute.firstIndex(of: peak) ?? 0
        // The peak rides the axis line with "now"; floating over the columns it
        // collided with the tallest ones.
        let words = ["60 min ago", "30", "now · peak \(Int(peak))/min", ""]
        for (i, label) in chartLabels.enumerated() {
            label.string = attributed(words[i], size: max(11, 9 * tu), weight: i == 3 ? .bold : .semibold, alpha: 0.55)
            let lw = ceil(label.preferredFrameSize().width)
            let x: CGFloat
            switch i {
            case 0: x = frame.minX
            case 1: x = frame.midX - lw / 2
            case 2: x = frame.maxX - lw
            default: x = min(frame.maxX - lw, max(frame.minX, frame.minX + frame.width * CGFloat(peakAt) / 60 - lw / 2))
            }
            label.isHidden = words[i].isEmpty
            label.frame = CGRect(x: x, y: frame.minY - 13 * ts, width: lw + 2, height: 12 * ts)
        }
    }

    private func layoutHeat(now: Double, width w: CGFloat, shadow: (CALayer) -> Void) {
        let maps = snapshot.heatmaps
        heat.isHidden = maps.isEmpty
        heatCaption.isHidden = maps.isEmpty
        guard !maps.isEmpty else { return }
        let pw = min(w * 0.24, 640 * u), top = bounds.height - 40 * u, bottom = 112 * u
        let frame = CGRect(x: w - pw - 16 * u, y: bottom, width: pw, height: top - bottom - 18 * u)
        if heat.frame != frame {
            heat.frame = frame
            heatFile = ""   // re-pan for the new size
        }
        heat.cornerRadius = 8 * u
        heat.borderWidth = 1
        heat.borderColor = ink(0.22)
        if heatIndex < 0 || now - heatShownAt > 45 {
            heatIndex = (heatIndex + 1) % maps.count
            heatShownAt = now
        }
        let map = maps[min(heatIndex, maps.count - 1)]
        let file = map["file"] as? String ?? ""
        if file != heatFile, let image = NSImage(contentsOfFile: file),
           let cg = image.cgImage(forProposedRect: nil, context: nil, hints: nil) {
            heatFile = file
            heatAspect = CGFloat(cg.height) / CGFloat(max(cg.width, 1))
            let fade = CATransition()
            fade.type = .fade
            fade.duration = 1.2
            heat.add(fade, forKey: "swap")
            heat.contents = cg
            // Show a panel-shaped window of the page and glide it top to
            // bottom over the map's 45 s.
            let visible = min(1, (frame.height / frame.width) / heatAspect)
            let pan = CABasicAnimation(keyPath: "contentsRect")
            pan.fromValue = CGRect(x: 0, y: 1 - visible, width: 1, height: visible)
            pan.toValue = CGRect(x: 0, y: 0, width: 1, height: visible)
            pan.duration = 45
            pan.timingFunction = CAMediaTimingFunction(name: .easeInEaseOut)
            pan.fillMode = .forwards
            pan.isRemovedOnCompletion = false
            heat.contentsRect = CGRect(x: 0, y: 0, width: 1, height: visible)
            heat.add(pan, forKey: "pan")
            let hours = Int(Self.num(map["hours"], 24))
            let caption = NSMutableAttributedString(attributedString:
                attributed((map["page"] as? String ?? "") + "   ", size: max(11, 11 * tu), weight: .heavy))
            caption.append(attributed("\(hours >= 48 ? "\(hours / 24) days" : "\(hours)h") · " +
                "\(Int(Self.num(map["clicks"])).formatted()) clicks · \(Int(Self.num(map["people"])).formatted()) people",
                size: max(11, 10 * tu), weight: .semibold, alpha: 0.65, mono: true))
            heatCaption.string = caption
        }
        heatCaption.frame = CGRect(x: frame.minX, y: frame.maxY + 4 * u, width: pw, height: 15 * u)
        shadow(heatCaption)
    }

    /// Newest at the top; a new arrival slides the rest down one row, and
    /// rows thin out with age until five minutes have passed.
    private func layoutHot(now: Double, left: CGFloat, bottom: CGFloat, width: CGFloat, dt: CGFloat,
                           shadow: (CALayer) -> Void) {
        let rowH = 19 * ts
        if let newest = ticker.first, newest.at != hotTop {
            if hotTop > 0 { hotSlide = rowH }
            hotTop = newest.at
        }
        hotSlide *= pow(0.5, dt / 0.18)
        hot.frame = bounds
        let shownRows = min(hotRows.count, max(3, Int(7 / PixelFont.boost)))
        hudTop = max(hudTop, bottom + CGFloat(shownRows) * rowH)
        for (i, row) in hotRows.enumerated() {
            guard i < ticker.count, i < shownRows else { row.isHidden = true; continue }
            let e = ticker[i]
            let age = max(0, now - e.at)
            // Eased, so a line stays readable for most of its five minutes.
            let fade = pow(CGFloat(max(0, 1 - age / 300)), 0.4)
            row.isHidden = fade <= 0 || width < 120 * ts
            guard !row.isHidden else { continue }
            let key = "\(e.at)\(e.who)\(e.what)\(dark)\(Int(width))"
            if row.name != key {
            row.name = key
            // Hours and minutes only: the row's place and fade carry the rest.
            let s = NSMutableAttributedString(attributedString:
                attributed(String(Self.clock.string(from: Date(timeIntervalSince1970: e.at)).dropLast(3)) + "  ",
                           size: max(11, 11 * tu), weight: .semibold, alpha: 0.5, mono: true))
            s.append(attributed(e.who + "  ", size: max(11, 13 * tu), weight: .heavy))
            let size = max(11, 13 * tu)
            let room = max(0, width - ceil(s.size().width))
            s.append(NSAttributedString(string: fitted(e.what, size: size, weight: .bold, width: room), attributes: [
                .font: face(NSFont.systemFont(ofSize: size, weight: .bold)),
                .foregroundColor: color(forEvent: e.kind)]))
            row.string = s
            }
            // Newest on the bottom line; older lines climb as new ones land.
            row.alignmentMode = .left
            row.frame = CGRect(x: left, y: bottom + CGFloat(i) * rowH - hotSlide, width: width, height: rowH)
            // The arriving row fades in; the rest thin with rank and age.
            let entering = i == 0 ? min(1, 1 - hotSlide / rowH + 0.1) : 1
            row.opacity = Float(fade * (1 - CGFloat(i) * 0.11) * entering)
            shadow(row)
        }
    }

    /// Items glide left; when the tail clears the right edge the next event
    /// joins — fresh events first, otherwise the recent ones come round again.
    private func advanceCrawl(width: CGFloat, height: CGFloat) {
        let speed = 55 * u
        let now = CACurrentMediaTime()
        let x = { (item: (layer: CATextLayer, x0: CGFloat, t0: Double, width: CGFloat)) in
            item.x0 - speed * CGFloat(now - item.t0) }
        while let first = crawlItems.first, x(first) + first.width < 0 {
            first.layer.removeFromSuperlayer()
            crawlItems.removeFirst()
        }
        guard !ticker.isEmpty else { return }
        var tail = crawlItems.last.map { x($0) + $0.width } ?? width
        while tail < width + 40 * u {
            let index: Int
            if crawlFresh > 0 {
                crawlFresh -= 1
                index = min(crawlFresh, ticker.count - 1)
            } else {
                crawlCursor = (crawlCursor + 1) % ticker.count
                index = crawlCursor
            }
            let row = ticker[index]
            let item = Self.text(HaloTextLayer())
            item.contentsScale = scale
            let s = NSMutableAttributedString(attributedString:
                attributed(row.who + "  ", size: max(11, 12 * tu), weight: .heavy))
            s.append(NSAttributedString(string: row.what, attributes: [
                .font: face(NSFont.systemFont(ofSize: max(11, 12 * tu), weight: .bold)),
                .foregroundColor: color(forEvent: row.kind)]))
            s.append(NSAttributedString(string: "     ◆     ", attributes: [
                .font: face(NSFont.systemFont(ofSize: max(11, 8 * tu), weight: .bold)),
                .foregroundColor: NSColor(cgColor: ink(0.35)) ?? .gray,
                .baselineOffset: 2 * u]))
            item.string = s
            let itemW = ceil(item.preferredFrameSize().width)
            item.anchorPoint = .zero
            item.bounds = CGRect(x: 0, y: 0, width: itemW, height: 17 * u)
            let y = (height - 16 * u) / 2
            let travel = tail + itemW
            item.position = CGPoint(x: -itemW, y: y)
            let glide = CABasicAnimation(keyPath: "position.x")
            glide.fromValue = tail
            glide.toValue = -itemW
            glide.duration = Double(travel / speed)
            glide.timingFunction = CAMediaTimingFunction(name: .linear)
            glide.beginTime = now
            glide.fillMode = .backwards
            crawl.addSublayer(item)
            item.add(glide, forKey: "crawl")
            crawlItems.append((item, tail, now, itemW))
            tail += itemW
        }
    }

    // ── frame ────────────────────────────────────────────────────────────

    /// 30 Hz from the wallpaper's main-thread timer.
    func tick(now: Double, dark: Bool) {
        let wasDark = self.dark
        self.dark = dark
        poll(now: now)
        let dt = CGFloat(min(0.1, now - (lastTick ?? now)))
        lastTick = now
        let live = snapshot.observedAt > 0 && now - snapshot.observedAt < Self.staleAfter
        if !live { fish.values.forEach { $0.leaving = true } }

        CATransaction.begin()
        CATransaction.setDisableActions(true)
        if wasDark != dark { fish.values.forEach(dress) }
        while let next = pending.first, next.playAt <= now {
            pending.removeFirst()
            play(next.event, now: now)
        }
        poses.removeAll(keepingCapacity: true)
        if let remote = link?.fish(at: now) {
            for (id, f) in fish {
                f.root.isHidden = remote[id] == nil
                if let r = remote[id] { swim(f, t: now, dt: dt, remote: r); continue }
                // Gone from the leader's tank and from the data: gone here too.
                if f.leaving { f.root.removeFromSuperlayer(); fish[id] = nil }
            }
        } else {
            for f in fish.values { swim(f, t: now, dt: dt) }
            spaceOut(dt: dt)
            if let link, link.config.role == .leader, now - lastSent >= 1.0 / 30 {
                lastSent = now
                link.publish(fish: fish.map { ($0.key, RemoteFish(x: $0.value.x, homeY: $0.value.homeY,
                    depth: $0.value.depth, dir: $0.value.dir, facing: $0.value.facing, alpha: $0.value.alpha,
                    glow: $0.value.glow, boost: $0.value.boost, lunge: $0.value.lunge, leaving: $0.value.leaving)) },
                    at: now)
            }
        }
        for (id, f) in fish where link?.following != true && f.leaving && (f.alpha < 0.02 || f.x < -f.length * 1.5 || f.x > bounds.width + f.length * 1.5) {
            f.root.removeFromSuperlayer()
            fish[id] = nil
        }
        layoutHUD(now: now, dt: dt)
        layoutConstellation(now: now)
        CATransaction.commit()
    }
}

/// The fishery's fish as lit SceneKit bodies in the wallpaper's own scene, so
/// they share its light and depth with the drifting marks. FisheryLayer swims
/// them in view points; this maps each pose into the orthographic stage.
final class FishSchool {
    let root = SCNNode()
    /// The wallpaper's emblem (the Fuser mark); employees wear a small one.
    var badge: SCNNode?
    /// Draw rax's little people (aesthetic.computer) instead of fish.
    var walkers = false

    private final class Body {
        let node = SCNNode()
        let turn = SCNNode()
        let tail = SCNNode()
        let skin = SCNMaterial()
        /// The body wears the owner's latest generation when there is one.
        let coat = SCNMaterial()
        /// A solid rim in the tier colour: an inverted hull (a slightly
        /// larger copy, front faces culled, unlit) so it reads at any angle.
        let rim = SCNMaterial()
        /// Staff only: a wider additive shell around the body that pulses.
        let glow = SCNMaterial()
        var glowHull: SCNNode?
        var sphere: SCNGeometry?
        var wearing = ""
        var crown: SCNNode?
        var player: AVQueuePlayer?
        var looper: AVPlayerLooper?
        // What the materials currently show, so a frame only touches them
        // when something actually moved.
        var tier: NSColor?
        var water: NSColor?
        var light = false
        var hazeStep = -1
        var emissive: NSColor?
        var glowStep = -1
        // Walkers only: hinged legs and arms, a white head and hands, and
        // the shirt colour rax gives each seat.
        var walker = false
        let paper = SCNMaterial()
        var cloth: NSColor?
        var legs: [SCNNode] = []
        var arms: [SCNNode] = []
        var stride: CGFloat = 0
        var lastX: CGFloat?
        var blinkAt: Double = 0
        var eyes: [SCNNode] = []
    }
    private var bodies: [String: Body] = [:]

    /// rax's seat colours (rax/world.mjs COLORS) and its purple brain.
    private static let raxColors: [UInt32] = [0xff593b, 0x69bfff, 0xffd23f, 0x5ad17a, 0xc77dff, 0xff8fc7, 0x3fe0d0, 0xff9f40]
    private static let brain: UInt32 = 0xb46bff
    private static func rgb(_ v: UInt32, _ k: CGFloat = 1) -> NSColor {
        NSColor(srgbRed: min(1, CGFloat(v >> 16 & 255) / 255 * k), green: min(1, CGFloat(v >> 8 & 255) / 255 * k),
                blue: min(1, CGFloat(v & 255) / 255 * k), alpha: 1)
    }

    // Generation stills decode on a background queue; a frame only ever
    // picks up finished images. `nil` marks a file that failed.
    private static let loader = DispatchQueue(label: "computer.aesthetic.blueberry-wallpaper.skins", qos: .utility)
    private let skinLock = NSLock()
    private var skins: [String: CGImage?] = [:]
    private var loading = Set<String>()

    /// `.some(image)` when decoded, `.some(nil)` when the file is no good,
    /// `nil` while it is still on its way.
    private func skin(_ path: String) -> CGImage?? {
        skinLock.lock()
        defer { skinLock.unlock() }
        if let known = skins[path] { return known }
        guard loading.insert(path).inserted else { return nil }
        Self.loader.async { [weak self] in
            var image: CGImage?
            if let source = CGImageSourceCreateWithURL(URL(fileURLWithPath: path) as CFURL, nil) {
                image = CGImageSourceCreateImageAtIndex(
                    source, 0, [kCGImageSourceShouldCacheImmediately: true] as CFDictionary)
            }
            guard let self else { return }
            self.skinLock.lock()
            if self.skins.count > 120 { self.skins.removeAll() }
            self.skins[path] = .some(image)
            self.loading.remove(path)
            self.skinLock.unlock()
        }
        return nil
    }

    /// The window is going away: stop any video skins.
    func retire() {
        for b in bodies.values { b.player?.pause() }
    }

    /// The colour far fish fade into; the wallpaper keeps it on its backdrop.
    var water = NSColor(srgbRed: 0.03, green: 0.03, blue: 0.08, alpha: 1)
    /// Light appearance: rims deepen toward this so bodies hold on pale water.
    var light = false
    private static let lightEdge = NSColor(srgbRed: 0.04, green: 0.06, blue: 0.30, alpha: 1)

    init() {
        root.position = SCNVector3(0, 0, 0.8)
        // A cool rim from behind-left rounds every body off the backdrop.
        let rim = SCNNode()
        rim.light = SCNLight()
        rim.light?.type = .directional
        rim.light?.intensity = 650
        rim.light?.color = NSColor(srgbRed: 0.7, green: 0.85, blue: 1, alpha: 1)
        rim.eulerAngles = SCNVector3(0.4, CGFloat.pi * 0.85, 0)
        root.addChildNode(rim)
    }

    private func build(_ id: String) -> Body {
        if walkers { return buildWalker(id) }
        let b = Body()
        // Physically based so the scene's environment light wraps each
        // body; a clear coat gives the wet sheen.
        b.skin.lightingModel = .physicallyBased
        b.skin.metalness.contents = 0.0
        b.skin.roughness.contents = 0.38
        b.skin.clearCoat.contents = 1.0
        b.skin.clearCoatRoughness.contents = 0.08
        b.coat.lightingModel = .physicallyBased
        b.coat.metalness.contents = 0.0
        b.coat.roughness.contents = 0.3
        b.coat.clearCoat.contents = 1.0
        b.coat.clearCoatRoughness.contents = 0.06
        // The picture wraps twice around the body, so each flank shows it
        // whole rather than half of one stretched all the way round.
        b.coat.diffuse.wrapS = .repeat
        b.coat.diffuse.contentsTransform = SCNMatrix4MakeScale(2, 1, 1)
        b.coat.diffuse.mipFilter = .linear
        b.rim.lightingModel = .constant
        b.rim.cullMode = .front
        b.rim.diffuse.contents = NSColor.white
        // Each solid piece carries its own hull, scaled up about its centre.
        let outline = { (piece: SCNNode, grow: SCNVector3) in
            guard let g = piece.geometry?.copy() as? SCNGeometry else { return }
            g.materials = [b.rim]
            let hull = SCNNode(geometry: g)
            hull.scale = grow
            hull.renderingOrder = -1
            piece.addChildNode(hull)
        }
        let ink = SCNMaterial()
        ink.lightingModel = .constant
        ink.diffuse.contents = NSColor(white: 0.03, alpha: 1)
        let shine = SCNMaterial()
        shine.lightingModel = .constant
        shine.diffuse.contents = NSColor.white

        // Unit fish facing +x, one unit nose to tail.
        let sphere = SCNSphere(radius: 0.5)
        sphere.segmentCount = 36
        sphere.materials = [b.skin]
        b.sphere = sphere
        let body = SCNNode(geometry: sphere)
        body.position = SCNVector3(0.06, 0, 0)
        body.scale = SCNVector3(0.86, 0.46, 0.42)
        b.turn.addChildNode(body)
        b.glow.lightingModel = .constant
        b.glow.cullMode = .front
        b.glow.blendMode = .add
        b.glow.writesToDepthBuffer = false
        b.glow.diffuse.contents = NSColor.black
        if let g = sphere.copy() as? SCNGeometry {
            g.materials = [b.glow]
            let hull = SCNNode(geometry: g)
            hull.scale = SCNVector3(1 + 0.11 / 0.43, 1 + 0.11 / 0.23, 1 + 0.11 / 0.21)
            hull.renderingOrder = -2
            hull.isHidden = true
            body.addChildNode(hull)
            b.glowHull = hull
        }
        // Body is squashed (0.86 × 0.46 × 0.42), so grow each axis by the same
        // absolute amount, not the same ratio, for an even line.
        let line: CGFloat = 0.035
        outline(body, SCNVector3(1 + line / 0.43, 1 + line / 0.23, 1 + line / 0.21))

        let fin = { (bottom: CGFloat, height: CGFloat) -> SCNNode in
            let cone = SCNCone(topRadius: 0, bottomRadius: bottom, height: height)
            cone.radialSegmentCount = 24
            cone.materials = [b.skin]
            return SCNNode(geometry: cone)
        }
        // Tail: a flattened cone, apex at the body, hinged so it can wag.
        b.tail.position = SCNVector3(-0.32, 0, 0)
        let blade = fin(0.24, 0.3)
        blade.eulerAngles = SCNVector3(0, 0, -CGFloat.pi / 2)
        blade.scale = SCNVector3(1, 1, 0.18)
        blade.position = SCNVector3(-0.13, 0, 0)
        b.tail.addChildNode(blade)
        outline(blade, SCNVector3(1.22, 1.16, 1.5))
        b.turn.addChildNode(b.tail)

        let dorsal = fin(0.13, 0.2)
        dorsal.scale = SCNVector3(1, 1, 0.16)
        dorsal.position = SCNVector3(0.0, 0.22, 0)
        dorsal.eulerAngles = SCNVector3(0, 0, 0.5)
        b.turn.addChildNode(dorsal)
        outline(dorsal, SCNVector3(1.28, 1.2, 1.5))

        for side: CGFloat in [-1, 1] {
            let eye = SCNSphere(radius: 0.05)
            eye.materials = [ink]
            let e = SCNNode(geometry: eye)
            e.position = SCNVector3(0.3, 0.05, side * 0.125)
            let glint = SCNSphere(radius: 0.016)
            glint.materials = [shine]
            let g = SCNNode(geometry: glint)
            g.position = SCNVector3(0.022, 0.022, side * 0.035)
            e.addChildNode(g)
            b.turn.addChildNode(e)
        }
        b.node.addChildNode(b.turn)
        root.addChildNode(b.node)
        bodies[id] = b
        return b
    }

    /// One of rax's people (rax/view.mjs `fighters`): a shirt-coloured torso
    /// and limbs, a white head under a purple brain cap, white hands, two
    /// dark eyes. Built in rax's metres facing +z, then fitted to the fish's
    /// unit: one unit tall, feet at -0.3 so the name hangs just below them,
    /// turned to face +x like a fish.
    private func buildWalker(_ id: String) -> Body {
        let b = Body()
        b.walker = true
        var h: UInt64 = 1469598103934665603
        for c in id.utf8 { h = (h ^ UInt64(c)) &* 1099511628211 }
        b.cloth = Self.rgb(Self.raxColors[Int(h % UInt64(Self.raxColors.count))])
        for m in [b.skin, b.paper] {
            m.lightingModel = .physicallyBased
            m.metalness.contents = 0.0
            m.roughness.contents = 0.55
        }
        b.paper.diffuse.contents = NSColor.white
        b.rim.lightingModel = .constant
        b.rim.cullMode = .front
        b.rim.diffuse.contents = NSColor.white
        b.glow.lightingModel = .constant
        b.glow.cullMode = .front
        b.glow.blendMode = .add
        b.glow.writesToDepthBuffer = false
        b.glow.diffuse.contents = NSColor.black
        let flat = { (c: NSColor) -> SCNMaterial in
            let m = SCNMaterial()
            m.lightingModel = .physicallyBased
            m.metalness.contents = 0.0
            m.roughness.contents = 0.5
            m.diffuse.contents = c
            return m
        }
        let brain = flat(Self.rgb(Self.brain)), lobe = flat(Self.rgb(Self.brain, 1.1)), fold = flat(Self.rgb(Self.brain, 0.55))
        let ink = SCNMaterial()
        ink.lightingModel = .constant
        ink.diffuse.contents = NSColor(white: 0.07, alpha: 1)

        let line: CGFloat = 0.045 // metres of rim around each block
        @discardableResult
        func box(_ parent: SCNNode, _ x: CGFloat, _ y: CGFloat, _ z: CGFloat,
                 _ w: CGFloat, _ hh: CGFloat, _ d: CGFloat, _ m: SCNMaterial, rim: Bool = true) -> SCNNode {
            let g = SCNBox(width: w, height: hh, length: d, chamferRadius: 0)
            g.materials = [m]
            let n = SCNNode(geometry: g)
            n.position = SCNVector3(x, y, z)
            parent.addChildNode(n)
            if rim, let hull = g.copy() as? SCNGeometry {
                hull.materials = [b.rim]
                let o = SCNNode(geometry: hull)
                o.scale = SCNVector3(1 + 2 * line / w, 1 + 2 * line / hh, 1 + 2 * line / d)
                o.renderingOrder = -1
                n.addChildNode(o)
            }
            return n
        }
        // rax's figure spans 0.22 m (sole) to 1.925 m (brain top).
        let fit = SCNNode()
        let k: CGFloat = 1 / 1.705
        fit.scale = SCNVector3(k, k, k)
        fit.position = SCNVector3(0, -0.3 - 0.22 * k, 0)
        fit.eulerAngles = SCNVector3(0, CGFloat.pi / 2, 0)
        b.turn.addChildNode(fit)

        let torso = box(fit, 0, 1.02, 0, 0.42, 0.62, 0.26, b.skin)
        if let g = torso.geometry?.copy() as? SCNGeometry {
            g.materials = [b.glow]
            let hull = SCNNode(geometry: g)
            hull.scale = SCNVector3(1 + 0.3 / 0.42, 1 + 0.3 / 0.62, 1 + 0.3 / 0.26)
            hull.renderingOrder = -2
            hull.isHidden = true
            torso.addChildNode(hull)
            b.glowHull = hull
        }
        box(fit, 0, 1.59, 0, 0.44, 0.42, 0.36, b.paper)
        box(fit, 0, 1.835, 0, 0.36, 0.09, 0.3, brain)
        box(fit, -0.09, 1.89, 0, 0.15, 0.06, 0.26, lobe, rim: false)
        box(fit, 0.09, 1.89, 0, 0.15, 0.06, 0.26, lobe, rim: false)
        box(fit, 0, 1.88, 0, 0.012, 0.07, 0.27, fold, rim: false)
        for side: CGFloat in [-1, 1] {
            b.eyes.append(box(fit, side * 0.095, 1.6, 0.181, 0.07, 0.07, 0.012, ink, rim: false))
            // Legs hinge at the hip, arms at the shoulder (rax's heights).
            let hip = SCNNode()
            hip.position = SCNVector3(side * 0.15, 0.68, 0)
            box(hip, 0, -0.23, 0, 0.13, 0.46, 0.16, b.skin)
            fit.addChildNode(hip)
            b.legs.append(hip)
            let shoulder = SCNNode()
            shoulder.position = SCNVector3(side * 0.3, 1.27, 0)
            box(shoulder, 0, -0.23, 0, 0.13, 0.46, 0.16, b.skin)
            box(shoulder, 0, -0.5, 0, 0.16, 0.17, 0.2, b.paper)
            fit.addChildNode(shoulder)
            b.arms.append(shoulder)
        }
        b.node.addChildNode(b.turn)
        root.addChildNode(b.node)
        bodies[id] = b
        return b
    }

    /// Put a generation on the body: stills as textures, videos as muted
    /// loops. An empty path goes back to the plain tier colour.
    private func wear(_ b: Body, _ path: String) {
        let video = ["mp4", "mov", "webm"].contains((path as NSString).pathExtension.lowercased())
        var still: CGImage?
        if !path.isEmpty, !video {
            // Keep the current look until the new one has decoded.
            guard let loaded = skin(path) else { return }
            still = loaded
        }
        b.wearing = path
        b.player?.pause()
        b.player = nil
        b.looper = nil
        guard !path.isEmpty else {
            b.sphere?.materials = [b.skin]
            return
        }
        if video {
            let player = AVQueuePlayer()
            player.isMuted = true
            b.looper = AVPlayerLooper(player: player, templateItem: AVPlayerItem(url: URL(fileURLWithPath: path)))
            player.play()
            b.player = player
            b.coat.diffuse.contents = player
        } else {
            guard let still else {
                b.sphere?.materials = [b.skin]
                return
            }
            b.coat.diffuse.contents = still
        }
        b.sphere?.materials = [b.coat]
    }

    private static let mint = NSColor(srgbRed: 0.40, green: 0.95, blue: 0.72, alpha: 1)
    private static let staffRim = NSColor(srgbRed: 1.0, green: 0.31, blue: 0.85, alpha: 1)

    /// `halfHeight` is the stage's orthographic half-height in scene units.
    /// Runs inside the renderer's update, where scene changes apply at once
    /// with no transaction to wait on.
    func sync(_ poses: [FisheryLayer.Pose], size: CGSize, halfHeight: CGFloat) {
        guard size.height > 0 else { return }
        let k = halfHeight / (size.height / 2)
        var live = Set<String>()
        for p in poses {
            live.insert(p.id)
            var found = bodies[p.id]
            if let old = found, old.walker != walkers {
                old.player?.pause()
                old.node.removeFromParentNode()
                found = nil
            }
            let b = found ?? build(p.id)
            // Haze: far fish sink toward the water's colour, in 48 steps so
            // a slow drift in depth is not a new material every frame.
            let hazeStep = Int(((1 - p.depth) * 48).rounded())
            if b.hazeStep != hazeStep || b.tier !== p.color || b.water !== water || b.light != light {
                b.hazeStep = hazeStep
                b.light = light
                b.tier = p.color
                b.water = water
                let haze = CGFloat(hazeStep) / 48 * 0.22
                let base = b.cloth ?? p.color
                b.skin.diffuse.contents = base.blended(withFraction: haze, of: water) ?? base
                if b.walker { b.paper.diffuse.contents = NSColor.white.blended(withFraction: haze, of: water) ?? .white }
                // The rim stays the pure tier colour, only lightly hazed with depth.
                // In the light it is deepened, so a body reads against pale water.
                let edge = light ? (p.color.blended(withFraction: 0.42, of: Self.lightEdge) ?? p.color) : p.color
                b.rim.diffuse.contents = edge.blended(withFraction: haze * 0.35, of: water) ?? edge
            }
            // Fuser staff: a hot-pink rim that blinks, with a glow shell
            // breathing with it. Wall clock, so every pane of a tank agrees.
            if p.staff {
                let t = Date().timeIntervalSince1970.truncatingRemainder(dividingBy: 1e5)
                let wave = 0.5 + 0.5 * sin(t * 2 * .pi * 1.1)
                let blink = CGFloat(pow(wave, 3))
                b.rim.diffuse.contents = Self.staffRim.blended(withFraction: 0.55 * (1 - blink), of: water) ?? Self.staffRim
                b.glow.diffuse.contents = Self.staffRim.withAlphaComponent(1).blended(withFraction: 1 - (0.15 + 0.55 * blink), of: .black)
                b.glowHull?.isHidden = false
                b.tier = nil   // re-tint the rim normally if they stop being staff
            } else if b.glowHull?.isHidden == false {
                b.glowHull?.isHidden = true
            }
            // The glow is the tier colour at an intensity, not a new colour.
            var emissive = p.color
            var glow = min(1, p.glow * 0.85)
            if p.staff {
                // A slow mint breath under any event glow.
                let breath = 0.22 + 0.12 * sin(Date().timeIntervalSince1970.truncatingRemainder(dividingBy: 1e5) * 1.4)
                if breath >= glow { emissive = Self.mint; glow = breath }
            }
            if b.emissive !== emissive {
                b.emissive = emissive
                b.skin.emission.contents = emissive
                b.coat.emission.contents = emissive
            }
            let glowStep = Int((glow * 255).rounded())
            if b.glowStep != glowStep {
                b.glowStep = glowStep
                b.skin.emission.intensity = glow
                b.coat.emission.intensity = glow
            }
            // Staff wear the blinking outline; the old spinning crown is gone.
            if let crown = b.crown {
                crown.removeFromParentNode()
                b.crown = nil
            }
            if !b.walker, p.skin != b.wearing { wear(b, p.skin) }
            let s = p.length * k
            // The drifting marks sit at z 0 (root +1.2): far fish pass behind them.
            b.node.position = SCNVector3((p.x - size.width / 2) * k, (p.y - size.height / 2) * k, p.depth * 2.4 - 2)
            b.node.scale = SCNVector3(s, s, s)
            b.node.opacity = p.alpha
            if b.walker {
                // Steps follow the ground actually covered, so feet never
                // skate: about one stride per body length.
                let moved = b.lastX.map { abs(p.x - $0) } ?? 0
                b.lastX = p.x
                if moved < p.length { b.stride += moved / max(1, p.length * 0.16) }
                let swing = sin(b.stride)
                for (i, leg) in b.legs.enumerated() { leg.eulerAngles = SCNVector3((i == 0 ? swing : -swing) * 0.6, 0, 0) }
                for (i, arm) in b.arms.enumerated() { arm.eulerAngles = SCNVector3((i == 0 ? -swing : swing) * 0.45, 0, 0) }
                // Three-quarter view: the walk leans toward the glass, and a
                // turn swings round through facing it.
                let yaw = -p.yaw - 0.6 * cos(p.yaw)
                b.turn.eulerAngles = SCNVector3(0, yaw, 0)
                b.turn.position = SCNVector3(0, abs(swing) * 0.025, 0)
                let now = Date().timeIntervalSince1970
                if now > b.blinkAt + 0.14 { b.blinkAt = now + Double.random(in: 2...5) }
                let shut = now > b.blinkAt && now < b.blinkAt + 0.14
                for e in b.eyes { e.scale = SCNVector3(1, shut ? 0.18 : 1, 1) }
            } else {
                b.turn.eulerAngles = SCNVector3(0, p.yaw, p.tilt)
                b.tail.eulerAngles = SCNVector3(0, p.wag, 0)
            }
        }
        for (id, b) in bodies where !live.contains(id) {
            b.player?.pause()
            b.node.removeFromParentNode()
            bodies[id] = nil
        }
    }
}

/// The last hour as a small glass reef in the scene: one column per minute,
/// stacked by what was generated, tilted so tops and sides catch the light,
/// swaying slowly. Rebuilt (growing up from the floor) when the numbers move.
final class Reef {
    let root = SCNNode()
    private let tilt = SCNNode()
    private var version = -1

    init() { root.addChildNode(tilt) }

    func sync(_ data: FisheryLayer.ReefData?, size: CGSize, halfHeight: CGFloat) {
        guard let data, size.height > 0 else { root.isHidden = true; return }
        root.isHidden = false
        let k = halfHeight / (size.height / 2)
        let f = data.frame
        root.position = SCNVector3((f.midX - size.width / 2) * k, (f.minY - size.height / 2) * k, 0.4)
        // Wall clock, so every screen of a tank sways the reef together.
        let t = Date().timeIntervalSince1970.truncatingRemainder(dividingBy: 1e5)
        tilt.eulerAngles = SCNVector3(0.34, -0.42 + 0.09 * sin(t * 0.13), 0)
        guard data.version != version else { return }
        version = data.version
        tilt.childNodes.forEach { $0.removeFromParentNode() }

        // In scene units: the reef is as wide as the flat chart was, and
        // taller, since tilting shortens it.
        let width = f.width * k * 0.92
        let colW = width / 60
        let tall = f.height * k * 1.8
        let side = colW * 0.72
        for (i, stack) in data.stacks.enumerated() {
            let column = SCNNode()
            column.position = SCNVector3((CGFloat(i) - 29.5) * colW, 0, 0)
            var y: CGFloat = 0
            let segments = stack.isEmpty ? [(NSColor(white: 0.6, alpha: 1), data.peak * 0.015)] : stack
            for (color, v) in segments {
                let h = max(colW * 0.12, tall * v / data.peak)
                let box = SCNBox(width: side, height: h, length: side, chamferRadius: min(side, h) * 0.18)
                let glass = SCNMaterial()
                glass.lightingModel = .physicallyBased
                glass.diffuse.contents = color
                glass.emission.contents = color.blended(withFraction: 0.75, of: .black)
                glass.roughness.contents = 0.22
                glass.metalness.contents = 0.0
                glass.transparency = stack.isEmpty ? 0.35 : 0.88
                glass.blendMode = .alpha
                box.materials = [glass]
                let node = SCNNode(geometry: box)
                node.position = SCNVector3(0, y + h / 2, 0)
                column.addChildNode(node)
                y += h
            }
            // Grow up from the floor, left to right.
            column.scale = SCNVector3(1, 0.001, 1)
            column.pivot = SCNMatrix4Identity
            let grow = SCNAction.customAction(duration: 0.9) { node, elapsed in
                let p = min(1, elapsed / 0.9)
                let eased = 1 - pow(1 - p, 3)
                node.scale = SCNVector3(1, max(0.001, eased), 1)
            }
            column.runAction(.sequence([.wait(duration: Double(i) * 0.012), grow]))
            tilt.addChildNode(column)
        }
    }
}
