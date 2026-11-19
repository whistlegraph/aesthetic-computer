import AppKit
import QuartzCore

/// ✈️ rax from the air, a small wireframe model in a corner, drawn from
/// rax's own map (GET /rax/map, rax/map.mjs): the plaza with the tower
/// over it, every open block with its lots and houses, the blocks that open
/// next drawn faint, what stands in the rooms, and who is there now. The
/// whole world is seen from an isometric angle, with no perspective, and
/// swings slowly side to side.
///
/// This file knows how to draw four kinds of shape and how to colour the
/// roles it has met. It holds none of rax's measurements: when rax
/// changes, the map changes and this follows.
///
/// Everything is a plain colour layer or a text layer under one transform, so
/// it stays sharp. The swing is one keyframe animation on the rig, sampled
/// ten times a second and looped by Core Animation. Nothing here runs per
/// frame.
///
/// 👣 Who is there is kept apart from what stands: `move` takes only the
/// people (GET /rax/people, several times a second) and slides each walker
/// to where they now are over the time between two readings, leaving a line
/// along the ground where they have been for the last minute and a half.
final class WorldFlyLayer: CALayer {
    typealias Person = (handle: String, x: CGFloat, y: CGFloat, z: CGFloat, floor: Int)
    /// People as rax lists them, in the map or alone.
    static func people(_ rows: [[String: Any]]) -> [Person] {
        let num = { (v: Any?) -> CGFloat in CGFloat((v as? NSNumber)?.doubleValue ?? 0) }
        return rows.map { ($0["handle"] as? String ?? "", num($0["x"]), num($0["y"]), num($0["z"]), Int(num($0["floor"]))) }
    }
    /// One shape of rax's map (rax/map.mjs), in world metres: x east,
    /// z north, y up.
    struct Shape {
        var kind = "", role = "", label = ""
        var x0: CGFloat = 0, z0: CGFloat = 0, x1: CGFloat = 0, z1: CGFloat = 0, y0: CGFloat = 0, y1: CGFloat = 0
        /// A panel's middle, turn, width and height; a stack's step and count.
        var x: CGFloat = 0, z: CGFloat = 0, yaw: CGFloat = 0, w: CGFloat = 0, h: CGFloat = 0
        var step: CGFloat = 0, count = 0
        var color: NSColor?

        init(_ row: [String: Any]) {
            let num = { (k: String) -> CGFloat in CGFloat((row[k] as? NSNumber)?.doubleValue ?? 0) }
            kind = row["k"] as? String ?? ""
            role = row["role"] as? String ?? ""
            label = row["label"] as? String ?? ""
            x0 = num("x0"); z0 = num("z0"); x1 = num("x1"); z1 = num("z1")
            // A flat or a stack lies at `y`; a panel's foot is lifted to it.
            y0 = row["y0"] == nil ? num("y") : num("y0"); y1 = num("y1")
            x = num("x"); z = num("z"); yaw = num("yaw"); w = num("w"); h = num("h")
            step = num("step"); count = Int(num("count"))
            if let c = (row["color"] as? NSNumber)?.intValue {
                color = NSColor(srgbRed: CGFloat((c >> 16) & 0xff) / 255, green: CGFloat((c >> 8) & 0xff) / 255,
                                blue: CGFloat(c & 0xff) / 255, alpha: 1)
            }
        }
    }
    /// rax as GET /rax/map draws it. Nothing of rax's geometry is kept
    /// here: what stands in the world is whatever shapes the map carries.
    struct World {
        var revision = ""
        /// West, south, width and depth of the open blocks, in metres.
        var extent = CGRect.zero
        /// What stands for good, and what people have put in the rooms.
        var structure: [Shape] = []
        var content: [Shape] = []
        /// Everyone in rax now; `floor` is 0 on the ground.
        var people: [Person] = []
        var floors = 0, top = 0, settled = 0, across = 0

        init(_ map: [String: Any]) {
            let num = { (v: Any?) -> CGFloat in CGFloat((v as? NSNumber)?.doubleValue ?? 0) }
            revision = map["revision"] as? String ?? ""
            let e = map["extent"] as? [String: Any] ?? [:]
            extent = CGRect(x: num(e["x0"]), y: num(e["z0"]), width: num(e["x1"]) - num(e["x0"]), height: num(e["z1"]) - num(e["z0"]))
            structure = (map["structure"] as? [[String: Any]] ?? []).map(Shape.init)
            content = (map["content"] as? [[String: Any]] ?? []).map(Shape.init)
            people = WorldFlyLayer.people(map["people"] as? [[String: Any]] ?? [])
            let tower = map["tower"] as? [String: Any] ?? [:], census = map["census"] as? [String: Any] ?? [:]
            floors = Int(num(tower["floors"])); top = Int(num(tower["top"]))
            settled = Int(num(census["settled"])); across = Int(num(census["across"]))
        }

        var isEmpty: Bool { structure.isEmpty }
        /// The line under the model.
        var census: String {
            var parts = ["rax"]
            if floors > 0 { parts.append("\(floors) floor\(floors == 1 ? "" : "s")") }
            parts.append("\(settled) home\(settled == 1 ? "" : "s")")
            if across > 0 { parts.append("\(across) m across") }
            return parts.joined(separator: " · ")
        }
        /// What the swing depends on: what stands for good, and whether the
        /// rooms hold anything. A room filling up does not start it over.
        var shape: String { "\(revision)/\(structure.count)/\(extent)/\(content.isEmpty)" }
    }
    struct Style {
        var font: NSFont
        var ink: NSColor
        var halo: CGColor
        var scale: CGFloat
    }
    /// One place the camera rests: what it looks at, from where, for how long.
    private struct Stop {
        var x: CGFloat = 0, z: CGFloat = 0
        var yaw: CGFloat = 0, pitch: CGFloat = 0
        /// Metres across the screen at the point looked at.
        var span: CGFloat = 16
        /// Radians the camera swings round the point while it rests.
        var sway: CGFloat = 0
        var travel: Double = 0, hold: Double = 0
    }

    private let rig = CATransformLayer()
    private let land = CATransformLayer()
    private let crowd = CATransformLayer()
    private var stops: [Stop] = []
    private var started: CFTimeInterval = 0
    private var total: Double = 0
    private var flightKey = ""
    /// Thinnest line, in metres, that still shows at the model's size.
    private var hair: CGFloat = 0
    /// What the last `show` worked out, kept so `move` can place people alone.
    private var perMetre: CGFloat = 1
    private var mark: CGFloat = 0.42
    private var crown: (y: CGFloat, floor: Int, cut: CGFloat)?
    private var styled: Style?
    /// One person standing in the model, and the line behind them.
    private struct Walker {
        var pole: CALayer, head: CALayer, sign: CALayer, trail: CAShapeLayer
        var label: String
    }
    private var walkers: [String: Walker] = [:]
    /// Where each handle has been: ground points, the height they were at, and when.
    private var trails: [String: [(at: CGPoint, y: CGFloat, t: CFTimeInterval)]] = [:]
    private static let trailSeconds: CFTimeInterval = 90

    /// Panel heights of tower drawn to scale above the ground.
    private static let rise: CGFloat = 1.15
    /// Radians down from overhead: the isometric angle.
    private static let pitch: CGFloat = 0.955
    private static let lobbyPink = NSColor(srgbRed: 1.0, green: 0.2, blue: 0.59, alpha: 1)
    private static let homePurple = NSColor(srgbRed: 0.71, green: 0.42, blue: 1.0, alpha: 1)
    private static let buildGold = NSColor(srgbRed: 1.0, green: 0.80, blue: 0.30, alpha: 1)
    private static let screenCyan = NSColor(srgbRed: 0.35, green: 0.90, blue: 0.95, alpha: 1)
    private static let awakeGreen = NSColor(srgbRed: 0.35, green: 0.86, blue: 0.50, alpha: 1)
    /// A walker's layers: still in everything but where they stand, so a
    /// move inside a transaction slides them there.
    private static let walking: [String: CAAction] = still.filter { $0.key != "position" && $0.key != "zPosition" }
    private static let still: [String: CAAction] = [
        "contents": NSNull(), "bounds": NSNull(), "position": NSNull(), "hidden": NSNull(),
        "sublayerTransform": NSNull(), "transform": NSNull(), "sublayers": NSNull(),
        "backgroundColor": NSNull(), "borderColor": NSNull(), "borderWidth": NSNull(), "zPosition": NSNull(),
    ]

    override init() {
        super.init()
        actions = Self.still
        for l in [rig, land, crowd] { l.actions = Self.still }
        rig.addSublayer(land)
        rig.addSublayer(crowd)
        addSublayer(rig)
    }
    override init(layer: Any) { super.init(layer: layer) }
    required init?(coder: NSCoder) { nil }

    // ── drawing: one metre is one layer unit ─────────────────────────────

    /// An outline lying flat, on the ground or `lift` metres up, with an optional fill.
    private func flat(_ r: CGRect, _ color: NSColor, width: CGFloat, fill: NSColor? = nil, lift: CGFloat = 0, into group: CALayer) {
        let l = CALayer()
        l.actions = Self.still
        l.frame = r
        l.zPosition = lift
        l.borderColor = color.cgColor
        l.borderWidth = max(width, hair)
        l.backgroundColor = fill?.cgColor
        l.allowsEdgeAntialiasing = true
        group.addSublayer(l)
    }
    /// A rectangle stood upright on the ground at (x, z), facing south before
    /// `yaw` turns it; `lift` raises its foot.
    @discardableResult
    private func upright(_ size: CGSize, x: CGFloat, z: CGFloat, yaw: CGFloat = 0, lift: CGFloat = 0,
                         border: NSColor? = nil, width: CGFloat = 0, fill: NSColor? = nil, into group: CALayer) -> CALayer {
        let l = CALayer()
        l.actions = Self.still
        l.bounds = CGRect(origin: .zero, size: size)
        l.anchorPoint = CGPoint(x: 0.5, y: 0)
        l.position = CGPoint(x: x, y: z)
        l.zPosition = lift
        l.borderColor = border?.cgColor
        l.borderWidth = width > 0 ? max(width, hair) : 0
        l.backgroundColor = fill?.cgColor
        l.allowsEdgeAntialiasing = true
        l.transform = CATransform3DConcat(CATransform3DMakeRotation(.pi / 2, 1, 0, 0), CATransform3DMakeRotation(yaw, 0, 0, 1))
        group.addSublayer(l)
        return l
    }
    /// Words stood upright, drawn at the size they show at.
    @discardableResult
    private func sign(_ words: String, _ color: NSColor, x: CGFloat, z: CGFloat, lift: CGFloat, style: Style,
                      perMetre: CGFloat, into group: CALayer) -> CALayer {
        let t = HaloTextLayer()
        t.actions = Self.still
        t.pixelated = PixelFont.available
        t.contentsScale = style.scale
        t.halo = style.halo
        t.isWrapped = false
        t.isDoubleSided = false
        t.string = NSAttributedString(string: words, attributes: [.font: style.font, .foregroundColor: color])
        let need = t.preferredFrameSize()
        t.bounds = CGRect(x: 0, y: 0, width: ceil(need.width) + 4, height: ceil(need.height) + 2)
        t.alignmentMode = .center
        t.anchorPoint = CGPoint(x: 0.5, y: 0)
        t.position = CGPoint(x: x, y: z)
        t.zPosition = lift
        t.transform = CATransform3DConcat(CATransform3DMakeScale(1 / perMetre, 1 / perMetre, 1 / perMetre),
                                          CATransform3DMakeRotation(.pi / 2, 1, 0, 0))
        group.addSublayer(t)
        return t
    }

    /// Sets everyone where they now are. With `seconds`, walkers already
    /// standing slide there over that long (the time between two readings),
    /// so motion read a few times a second looks continuous. Someone new
    /// appears in place; someone gone is taken away with their line.
    func move(_ people: [Person], over seconds: CFTimeInterval = 0) {
        guard let style = styled else { return }
        let now = CACurrentMediaTime()
        CATransaction.begin()
        CATransaction.setDisableActions(seconds <= 0)
        CATransaction.setAnimationDuration(seconds)
        CATransaction.setAnimationTimingFunction(CAMediaTimingFunction(name: .linear))
        var here = Set<String>()
        for p in people where !p.handle.isEmpty {
            here.insert(p.handle)
            // Someone above the break stands on the top slab drawn.
            var y = p.y, name = "@" + p.handle
            if let c = crown, p.y > c.cut { y = c.y; name += " \(p.floor)" }
            let at = CGPoint(x: p.x, y: p.z)
            if var w = walkers[p.handle] {
                if w.label != name {
                    w.sign.removeFromSuperlayer()
                    w.sign = sign(name, Self.awakeGreen, x: p.x, z: p.z, lift: y + 1.3 + mark * 1.4, style: style, perMetre: perMetre, into: crowd)
                    w.sign.actions = Self.walking
                    w.label = name
                    walkers[p.handle] = w
                }
                w.pole.position = at; w.pole.zPosition = y
                w.head.position = at; w.head.zPosition = y + 1.3
                w.sign.position = at; w.sign.zPosition = y + 1.3 + mark * 1.4
            } else {
                let trail = CAShapeLayer()
                trail.actions = Self.still.merging(["path": NSNull()]) { a, _ in a }
                trail.fillColor = nil
                trail.strokeColor = Self.awakeGreen.withAlphaComponent(0.55).cgColor
                trail.lineWidth = max(hair * 1.4, 0.05)
                trail.lineJoin = .round
                trail.lineCap = .round
                crowd.addSublayer(trail)
                let pole = upright(CGSize(width: hair, height: 1.3), x: p.x, z: p.z, lift: y, fill: Self.awakeGreen, into: crowd)
                let head = upright(CGSize(width: mark, height: mark), x: p.x, z: p.z, lift: y + 1.3, border: Self.awakeGreen, width: 0.07, into: crowd)
                let words = sign(name, Self.awakeGreen, x: p.x, z: p.z, lift: y + 1.3 + mark * 1.4, style: style, perMetre: perMetre, into: crowd)
                for l in [pole, head, words] { l.actions = Self.walking }
                walkers[p.handle] = Walker(pole: pole, head: head, sign: words, trail: trail, label: name)
            }
            // The line behind them: a new point for every step, none for standing.
            var line = trails[p.handle] ?? []
            if let last = line.last, abs(last.y - y) > 2 { line.removeAll() } // another floor: a new line
            if line.last.map({ hypot($0.at.x - at.x, $0.at.y - at.y) > 0.15 }) ?? true { line.append((at, y, now)) }
            line.removeAll { now - $0.t > Self.trailSeconds }
            trails[p.handle] = line
            if let trail = walkers[p.handle]?.trail {
                let path = CGMutablePath()
                if line.count > 1 { path.addLines(between: line.map { $0.at }) }
                trail.path = path
                trail.zPosition = y
            }
        }
        for (handle, w) in walkers where !here.contains(handle) {
            for l in [w.pole, w.head, w.sign, w.trail as CALayer] { l.removeFromSuperlayer() }
            walkers[handle] = nil
            trails[handle] = nil
        }
        CATransaction.commit()
    }

    /// The colour a role is drawn in, its line weight and how full it is.
    /// A role this build has not met is drawn in plain ink.
    private func look(_ s: Shape, ink: NSColor) -> (line: NSColor, width: CGFloat, fill: NSColor?) {
        if let c = s.color { return (c, 0.06, c.withAlphaComponent(0.45)) }
        switch s.role {
        case "plaza": return (Self.lobbyPink.withAlphaComponent(0.85), 0.09, Self.lobbyPink.withAlphaComponent(0.05))
        case "block": return (ink.withAlphaComponent(0.42), 0.07, nil)
        case "yard": return (Self.awakeGreen.withAlphaComponent(0.22), 0.05, Self.awakeGreen.withAlphaComponent(0.03))
        case "next": return (ink.withAlphaComponent(0.10), 0.06, nil)
        case "lot": return (ink.withAlphaComponent(0.16), 0.035, nil)
        case "home": return (Self.homePurple.withAlphaComponent(0.95), 0.06, Self.homePurple.withAlphaComponent(0.07))
        case "house": return (Self.homePurple.withAlphaComponent(0.85), 0.06, Self.homePurple.withAlphaComponent(0.05))
        case "floor": return (ink.withAlphaComponent(0.62), 0.07, ink.withAlphaComponent(0.05))
        case "leg": return (ink.withAlphaComponent(0.7), 0.06, ink.withAlphaComponent(0.7))
        case "glass": return (Self.screenCyan.withAlphaComponent(0.55), 0.05, Self.screenCyan.withAlphaComponent(0.05))
        case "car": return (Self.lobbyPink.withAlphaComponent(0.9), 0.06, Self.lobbyPink.withAlphaComponent(0.10))
        case "shaft": return (Self.lobbyPink.withAlphaComponent(0.38), 0.04, nil)
        case "fixture": return (ink.withAlphaComponent(0.4), 0.04, nil)
        case "build": return (Self.buildGold, 0.05, nil)
        case "screen": return (Self.screenCyan, 0.06, Self.screenCyan.withAlphaComponent(0.08))
        default: return (ink.withAlphaComponent(0.5), 0.05, nil)
        }
    }

    /// A standing box between two heights: four walls, or one sheet when it
    /// is thin, or a post when it is thin both ways.
    private func box(_ r: CGRect, from y0: CGFloat, to y1: CGFloat, _ l: (line: NSColor, width: CGFloat, fill: NSColor?),
                     into group: CALayer) {
        let tall = y1 - y0, thin: CGFloat = 0.9
        guard tall > 0 else { return }
        if r.width < thin && r.height < thin {
            upright(CGSize(width: max(r.width, hair * 2), height: tall), x: r.midX, z: r.midY, lift: y0, fill: l.line, into: group)
        } else if r.height < thin {
            upright(CGSize(width: r.width, height: tall), x: r.midX, z: r.midY, lift: y0, border: l.line, width: l.width, fill: l.fill, into: group)
        } else if r.width < thin {
            upright(CGSize(width: r.height, height: tall), x: r.midX, z: r.midY, yaw: .pi / 2, lift: y0, border: l.line, width: l.width,
                    fill: l.fill, into: group)
        } else {
            for z in [r.minY, r.maxY] {
                upright(CGSize(width: r.width, height: tall), x: r.midX, z: z, lift: y0, border: l.line, width: l.width, fill: l.fill, into: group)
            }
            for x in [r.minX, r.maxX] {
                upright(CGSize(width: r.height, height: tall), x: x, z: r.midY, yaw: .pi / 2, lift: y0, border: l.line, width: l.width,
                        fill: l.fill, into: group)
            }
        }
    }

    /// Draws the world and, when its shape or the space changed, starts the
    /// swing over. `size` is the space the model sits in, in points.
    ///
    /// The ground is to scale. The tower is to scale as far up as `rise`
    /// allows; past that it is broken and its top two slabs are set just
    /// above the break, with the number of floors over them.
    func show(_ world: World, size: CGSize, style: Style) {
        bounds = CGRect(origin: .zero, size: size)
        let ink = style.ink
        // The world's diagonal fits the width, so it clears the space at any turn.
        let e = world.extent
        let span = max(1, hypot(e.width, e.height) * 1.1)
        let perMetre = size.width / span
        hair = 0.75 / perMetre
        let mark = max(0.42, 3 / perMetre)
        self.perMetre = perMetre
        self.mark = mark
        self.styled = style
        // Metres of height drawn to scale: what stands `Self.rise` panel
        // heights above the ground at this size.
        let ceiling = size.height * Self.rise / (perMetre * sin(Self.pitch))
        /// Where a broken tower's top is drawn, and the floor it stands for.
        var crown: (y: CGFloat, floor: Int, cut: CGFloat)?

        land.sublayers?.forEach { $0.removeFromSuperlayer() }
        for s in world.structure + world.content {
            let l = look(s, ink: ink), r = CGRect(x: s.x0, y: s.z0, width: s.x1 - s.x0, height: s.z1 - s.z0)
            switch s.kind {
            case "flat":
                flat(r.insetBy(dx: s.role == "lot" || s.role == "home" ? 0.15 : 0, dy: s.role == "lot" || s.role == "home" ? 0.15 : 0),
                     l.line, width: l.width, fill: l.fill, lift: s.y0, into: land)
            case "box":
                // Anything taller than the scale allows stops at the break.
                box(r, from: s.y0, to: min(s.y1, max(ceiling, s.y0 + 1)), l, into: land)
                // Something low and wide also shows its top.
                if s.y1 <= ceiling, r.width >= 0.9, r.height >= 0.9, s.y1 - s.y0 < 2 {
                    flat(r, l.line, width: l.width, lift: s.y1, into: land)
                }
            case "stack":
                guard s.step > 0, s.count > 0 else { continue }
                let whole = Int(max(1, ((ceiling - s.y0) / s.step).rounded(.down)))
                let shown = s.count <= whole + 2 ? s.count : whole
                for k in 0..<shown { flat(r, l.line, width: l.width, fill: l.fill, lift: s.y0 + CGFloat(k) * s.step, into: land) }
                guard shown < s.count else { continue }
                // The break: two storeys of nothing, then the last two slabs.
                let cut = s.y0 + CGFloat(shown - 1) * s.step, y = cut + 3 * s.step
                for k in 0..<2 { flat(r, l.line, width: l.width, fill: l.fill, lift: y + CGFloat(k) * s.step, into: land) }
                for dot in 1...3 {
                    flat(CGRect(x: r.midX - mark / 2, y: r.midY - mark / 2, width: mark, height: mark), l.line, width: l.width,
                         fill: l.line, lift: cut + CGFloat(dot) * s.step * 0.75, into: land)
                }
                crown = (y, s.count - 1, cut)
                sign("\(world.top)", Self.lobbyPink, x: r.midX, z: r.midY, lift: y + s.step * 2.5, style: style, perMetre: perMetre, into: land)
            case "panel":
                upright(CGSize(width: s.w, height: s.h), x: s.x, z: s.z, yaw: s.yaw, lift: s.y0, border: l.line, width: l.width,
                        fill: l.fill, into: land)
            default: continue
            }
        }

        // The model may have changed size: everyone is stood again at the new
        // scale, in place, keeping the lines of where they have been.
        self.crown = crown
        crowd.sublayers?.forEach { $0.removeFromSuperlayer() }
        walkers.removeAll()
        move(world.people)

        let key = "\(world.shape)\(Int(size.width))x\(Int(size.height))"
        guard key != flightKey else { return }
        flightKey = key

        sublayerTransform = CATransform3DIdentity
        rig.position = CGPoint(x: size.width / 2, y: size.height / 2)
        // One pose: the whole world, swung a third of a turn and back.
        stops = [Stop(x: e.midX, z: e.midY, pitch: Self.pitch, span: span, sway: 0.55, travel: 24, hold: 24)]
        fly(size: size)
    }

    /// The camera for one pose: the world slid so the point looked at is in
    /// the middle, turned, tipped away, and sized.
    private func camera(_ s: Stop, size: CGSize) -> CATransform3D {
        let k = size.width / max(1, s.span)
        var m = CATransform3DMakeTranslation(-s.x, -s.z, 0)
        m = CATransform3DConcat(m, CATransform3DMakeRotation(-s.yaw, 0, 0, 1))
        m = CATransform3DConcat(m, CATransform3DMakeRotation(-s.pitch, 1, 0, 0))
        m = CATransform3DConcat(m, CATransform3DMakeScale(k, k, k))
        return m
    }

    private func fly(size: CGSize) {
        let ease = { (t: CGFloat) -> CGFloat in t * t * (3 - 2 * t) }
        let mix = { (a: CGFloat, b: CGFloat, t: CGFloat) -> CGFloat in a + (b - a) * t }
        // A stop's pose `t` of the way through its rest: a slow swing.
        let rest = { (s: Stop, t: CGFloat) -> Stop in
            var p = s
            p.yaw = s.yaw - s.sway * cos(.pi * t)
            return p
        }
        let step = 0.1
        var frames: [NSValue] = []
        for (n, s) in stops.enumerated() {
            let from = rest(stops[(n + stops.count - 1) % stops.count], 1), to = rest(s, 0)
            let far = hypot(to.x - from.x, to.z - from.z)
            for i in 0..<Int((s.travel / step).rounded()) {
                let e = ease(CGFloat(Double(i) * step / s.travel))
                var p = Stop()
                p.x = mix(from.x, to.x, e); p.z = mix(from.z, to.z, e)
                p.yaw = mix(from.yaw, to.yaw, e); p.pitch = mix(from.pitch, to.pitch, e)
                // A long hop climbs on the way.
                p.span = mix(from.span, to.span, e) + far * 0.2 * sin(.pi * e)
                frames.append(NSValue(caTransform3D: camera(p, size: size)))
            }
            for i in 0..<Int((s.hold / step).rounded()) {
                frames.append(NSValue(caTransform3D: camera(rest(s, CGFloat(Double(i) * step / s.hold)), size: size)))
            }
        }
        rig.removeAnimation(forKey: "fly")
        total = 0
        guard frames.count > 1 else { return }
        total = Double(frames.count) * step
        frames.append(frames[0])
        started = CACurrentMediaTime()
        let a = CAKeyframeAnimation(keyPath: "transform")
        a.values = frames
        a.calculationMode = .linear
        a.duration = total
        a.repeatCount = .infinity
        a.isRemovedOnCompletion = false
        a.beginTime = rig.convertTime(started, from: nil)
        rig.transform = frames[0].caTransform3DValue
        rig.add(a, forKey: "fly")
    }
}
