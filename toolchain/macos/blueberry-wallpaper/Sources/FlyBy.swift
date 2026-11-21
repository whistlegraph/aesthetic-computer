import AppKit
import QuartzCore
import simd

/// The Rax atlas. Longitude follows the east-west lap and latitude shows
/// its north-south range. The wallpaper turns the model with Core Animation.
final class WorldFlyLayer: CALayer {
    /// `watching` is someone looking on from the air: x, y, z is their eye,
    /// and `handle` is the name to show ("@jeffrey", or a guest's two words).
    typealias Person = (handle: String, x: CGFloat, y: CGFloat, z: CGFloat, floor: Int, watching: Bool)
    /// People as rax lists them, in the map or alone: `people` stand in the
    /// world, `watchers` look on from above it.
    static func people(_ rows: [[String: Any]], watching: Bool = false) -> [Person] {
        let num = { (v: Any?) -> CGFloat in CGFloat((v as? NSNumber)?.doubleValue ?? 0) }
        return rows.map { ($0[watching ? "name" : "handle"] as? String ?? "", num($0["x"]), num($0["y"]), num($0["z"]),
                           Int(num($0["floor"])), watching) }
    }
    static func everyone(in map: [String: Any]) -> [Person] {
        people(map["people"] as? [[String: Any]] ?? []) + people(map["watchers"] as? [[String: Any]] ?? [], watching: true)
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
        var lap: CGFloat = 0, planetRadius: CGFloat = 0
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
            let planet = map["planet"] as? [String: Any]
            // Older cached maps only contain the extent of the surveyed land.
            lap = max(1, num(planet?["lap"]) > 0 ? num(planet?["lap"]) : max(extent.width, extent.height) * 2)
            planetRadius = num(planet?["radius"])
            structure = (map["structure"] as? [[String: Any]] ?? []).map(Shape.init)
            content = (map["content"] as? [[String: Any]] ?? []).map(Shape.init)
            people = WorldFlyLayer.everyone(in: map)
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
        /// What the orbit depends on: what stands for good, and whether the
        /// rooms hold anything. A room filling up does not start it over.
        var shape: String { "\(revision)/\(lap)/\(planetRadius)/\(structure.count)/\(extent)/\(content.isEmpty)" }
    }
    struct Style {
        var font: NSFont
        var ink: NSColor
        var halo: CGColor
        var scale: CGFloat
    }
    private let outline = CAShapeLayer()
    private let rig = CATransformLayer()
    private let land = CATransformLayer()
    private let crowd = CATransformLayer()
    private var lap: CGFloat = 1
    private var radius: CGFloat { lap / (2 * .pi) }
    private var flightKey = ""
    /// Thinnest line, in metres, that still shows at the model's size.
    private var hair: CGFloat = 0
    /// What the last `show` worked out, kept so `move` can place people alone.
    private var perMetre: CGFloat = 1
    private var mark: CGFloat = 0.42
    private var heightScale: CGFloat = 1
    // Heights near the ground use the planet's physical radius. Compress tall
    // towers and high-flying spectators to keep them inside the atlas panel.
    private func elevation(_ height: CGFloat) -> CGFloat {
        let h = height * heightScale
        return radius * h / (radius + abs(h))
    }
    private var styled: Style?
    /// One person standing in the model, and the line behind them.
    private struct Walker {
        var frame: CATransformLayer, pole: CALayer, head: CALayer, sign: CALayer, trail: CATransformLayer
        var label: String
    }
    private var walkers: [String: Walker] = [:]
    /// Where each handle has been: ground points, the height they were at, and when.
    private var trails: [String: [(at: CGPoint, y: CGFloat, t: CFTimeInterval)]] = [:]
    private static let trailSeconds: CFTimeInterval = 90
    /// Handles (lower case) whose own body has walked in from the field and
    /// stands on the model: their pole and head step aside for it. The name
    /// and the line behind them stay.
    var embodied = Set<String>() {
        didSet { if embodied != oldValue { stepAside() } }
    }
    private func stepAside() {
        for (id, w) in walkers {
            let gone = embodied.contains(id.lowercased())
            w.pole.isHidden = gone
            w.head.isHidden = gone
        }
    }
    /// Where a handle (lower case) stands right now, in this layer's points:
    /// their feet on the model as it is drawn this frame, mid-slide and
    /// mid-swing. Nil when they are not in rax.
    func spot(_ handle: String) -> CGPoint? {
        guard !isHidden, let w = walkers.first(where: { $0.key.lowercased() == handle })?.value else { return nil }
        let pole = w.pole.presentation() ?? w.pole
        let m = CATransform3DConcat((w.frame.presentation() ?? w.frame).transform, (rig.presentation() ?? rig).transform)
        let h = pole.zPosition
        return CGPoint(x: rig.position.x + h * m.m31 + m.m41,
                       y: rig.position.y + h * m.m32 + m.m42)
    }

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
        addSublayer(outline)
        addSublayer(rig)
    }
    override init(layer: Any) { super.init(layer: layer) }
    required init?(coder: NSCoder) { nil }

    // ── drawing: one metre is one layer unit ─────────────────────────────

    // Local layer axes are east, north and up. Rotate that frame onto
    // the globe at the supplied world coordinates.
    private func globeFrame(_ x: CGFloat, _ z: CGFloat) -> CATransform3D {
        let longitude = 2 * .pi * x / lap
        let latitude = .pi * min(0.5, max(-0.5, z / lap))
        var m = CATransform3DMakeTranslation(0, 0, radius)
        m = CATransform3DConcat(m, CATransform3DMakeRotation(-latitude, 1, 0, 0))
        return CATransform3DConcat(m, CATransform3DMakeRotation(longitude, 0, 1, 0))
    }

    private func circle(_ r: CGFloat, transform: CATransform3D, color: NSColor, into group: CALayer) {
        let ring = CAShapeLayer()
        ring.path = CGPath(ellipseIn: CGRect(x: -r, y: -r, width: 2*r, height: 2*r), transform: nil)
        ring.strokeColor = color.cgColor
        ring.fillColor = nil
        ring.lineWidth = hair
        ring.transform = transform
        group.addSublayer(ring)
    }

    // A chord in three dimensions, for surveyed borders and walkers' trails.
    private func segment(_ a: CGPoint, _ b: CGPoint, lift: CGFloat, color: NSColor, into group: CALayer) {
        func point(_ p: CGPoint) -> SIMD3<Double> {
            let m = globeFrame(p.x, p.y)
            return SIMD3(Double(m.m41 + lift*m.m31), Double(m.m42 + lift*m.m32), Double(m.m43 + lift*m.m33))
        }
        let start = point(a), end = point(b), d = end - start, length = simd_length(d)
        guard length > 0.0001 else { return }
        let mid = (start + end) / 2
        let l = CALayer()
        l.actions = Self.still
        l.bounds = CGRect(x: 0, y: 0, width: length, height: hair)
        l.position = CGPoint(x: mid.x, y: mid.y)
        l.zPosition = mid.z
        l.backgroundColor = color.cgColor
        l.transform = CATransform3DConcat(CATransform3DMakeRotation(-asin(max(-1, min(1, d.z/length))), 0, 1, 0),
                                          CATransform3DMakeRotation(atan2(d.y, d.x), 0, 0, 1))
        group.addSublayer(l)
    }

    private func border(_ r: CGRect, lift: CGFloat, color: NSColor) {
        let corners = [CGPoint(x: r.minX, y: r.minY), CGPoint(x: r.maxX, y: r.minY),
                       CGPoint(x: r.maxX, y: r.maxY), CGPoint(x: r.minX, y: r.maxY)]
        for i in corners.indices {
            let a = corners[i], b = corners[(i+1) % 4]
            let n = max(1, Int(ceil(hypot(b.x-a.x, b.y-a.y) / (lap/64))))
            var last = a
            for j in 1...n {
                let t = CGFloat(j)/CGFloat(n), next = CGPoint(x: a.x+(b.x-a.x)*t, y: a.y+(b.y-a.y)*t)
                segment(last, next, lift: lift, color: color, into: land)
                last = next
            }
        }
    }

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
            // 👀 An eye in the air is drawn apart from a body: cyan, on a hair
            // from the ground up to where it looks from, its line on the ground.
            let id = (p.watching ? "eye " : "") + p.handle
            let tint = p.watching ? Self.screenCyan : Self.awakeGreen
            here.insert(id)

            let y = elevation(p.y)
            let name = (p.watching ? p.handle : "@"+p.handle) + (p.floor > 0 ? " \(p.floor)" : "")
            let at = CGPoint(x: p.x, y: p.z)
            let foot: CGFloat = p.watching ? min(0, y) : y
            let headY = p.watching ? y : foot + mark
            let tall: CGFloat = p.watching ? max(0.2, abs(y)) : mark
            if walkers[id] == nil {
                let frame = CATransformLayer(), trail = CATransformLayer()
                frame.transform = globeFrame(p.x, p.z)
                crowd.addSublayer(frame)
                crowd.addSublayer(trail)
                let history = trails[id] ?? []
                for (a, b) in zip(history, history.dropFirst()) {
                    segment(a.at, b.at, lift: p.watching ? 0 : b.y, color: tint.withAlphaComponent(0.55), into: trail)
                }
                let pole = upright(CGSize(width: hair, height: tall), x: 0, z: 0, lift: foot, fill: tint, into: frame)
                let head = upright(CGSize(width: mark, height: mark), x: 0, z: 0, lift: headY, border: tint, width: hair, into: frame)
                let words = sign(name, tint, x: 0, z: 0, lift: headY + mark, style: style, perMetre: perMetre, into: frame)
                for layer in [pole, head, words] { layer.actions = Self.walking }
                walkers[id] = Walker(frame: frame, pole: pole, head: head, sign: words, trail: trail, label: name)
            }
            guard var w = walkers[id] else { continue }
            w.frame.transform = globeFrame(p.x, p.z)
            w.pole.zPosition = foot
            w.pole.bounds.size.height = tall
            w.head.zPosition = headY
            if w.label != name {
                w.sign.removeFromSuperlayer()
                w.sign = sign(name, tint, x: 0, z: 0, lift: headY + mark, style: style, perMetre: perMetre, into: w.frame)
                w.sign.actions = Self.walking
                w.label = name
                walkers[id] = w
            }
            w.sign.zPosition = headY + mark
            var line = trails[id] ?? []
            if let last = line.last, abs(last.y-y) > 2 * heightScale || hypot(last.at.x-at.x, last.at.y-at.y) > lap/4 {
                line.removeAll()
                w.trail.sublayers = nil
            }
            while let first = line.first, now-first.t > Self.trailSeconds {
                line.removeFirst()
                w.trail.sublayers?.first?.removeFromSuperlayer()
            }
            if let last = line.last {
                if hypot(last.at.x-at.x, last.at.y-at.y) > 0.15 {
                    segment(last.at, at, lift: foot, color: tint.withAlphaComponent(0.55), into: w.trail)
                    line.append((at, y, now))
                }
            } else { line.append((at, y, now)) }
            trails[id] = line
        }
        for (handle, w) in walkers where !here.contains(handle) {
            w.frame.removeFromSuperlayer()
            w.trail.removeFromSuperlayer()
            walkers[handle] = nil
            trails[handle] = nil
        }
        stepAside()
        CATransaction.commit()
    }

    /// The colour a role is drawn in, its line weight and how full it is.
    /// A role this build has not met is drawn in plain ink.
    private func look(_ s: Shape, ink: NSColor) -> (line: NSColor, width: CGFloat, fill: NSColor?) {
        if let c = s.color { return (c, 0.06, c.withAlphaComponent(0.45)) }
        switch s.role {
        case "plaza": return (Self.lobbyPink.withAlphaComponent(0.85), 0.09, Self.lobbyPink.withAlphaComponent(0.05))
        case "block": return (ink.withAlphaComponent(0.42), 0.07, nil)
        case "grounds": return (Self.awakeGreen.withAlphaComponent(0.42), 0.05, nil)
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
    /// rotation over. `size` is the space the model sits in, in points.
    ///
    func show(_ world: World, size: CGSize, style: Style) {
        bounds = CGRect(origin: .zero, size: size)
        let ink = style.ink
        guard size.width > 0, size.height > 0 else { return }
        lap = world.lap
        heightScale = world.planetRadius > 0 ? radius / world.planetRadius : 1
        let perMetre = min(size.width / (3.8*radius), size.height / (2.55*radius))
        hair = 0.8 / perMetre
        let mark = max(0.42, 3 / perMetre)
        self.perMetre = perMetre
        self.mark = mark
        self.styled = style
        let globeRadius = radius * perMetre
        outline.path = CGPath(ellipseIn: CGRect(x: size.width/2 - globeRadius, y: size.height/2 - globeRadius,
                                                width: 2*globeRadius, height: 2*globeRadius), transform: nil)
        outline.strokeColor = ink.withAlphaComponent(0.48).cgColor
        outline.fillColor = Self.screenCyan.withAlphaComponent(0.045).cgColor
        outline.lineWidth = 0.8
        land.sublayers?.forEach { $0.removeFromSuperlayer() }
        // Six meridians and five parallels trace both hemispheres of the atlas.
        let grid = ink.withAlphaComponent(0.36)
        for k in 0..<6 {
            circle(radius, transform: CATransform3DMakeRotation(CGFloat(k) * .pi/6, 0, 1, 0), color: grid, into: land)
        }
        for k in -2...2 {
            let lat = CGFloat(k) * .pi/6
            var m = CATransform3DMakeRotation(.pi/2, 1, 0, 0)
            m.m42 = radius*sin(lat)
            circle(radius*cos(lat), transform: m, color: grid, into: land)
        }
        for s in world.structure + world.content {
            let l = look(s, ink: ink)
            var r = CGRect(x: s.x0, y: s.z0, width: s.x1-s.x0, height: s.z1-s.z0)
            if s.kind == "flat", s.role != "paint" {
                border(r, lift: s.y0, color: l.line)
                continue
            }
            let x = s.kind == "panel" ? s.x : r.midX, z = s.kind == "panel" ? s.z : r.midY
            let width = r.width * cos(.pi * min(0.5, abs(z / lap)))
            let patch = CATransformLayer()
            patch.transform = globeFrame(x, z)
            land.addSublayer(patch)
            // North-south spans pole to pole in one lap, east-west goes all the way around.
            r = CGRect(x: -width/2, y: -r.height/4, width: width, height: r.height/2)
            switch s.kind {
            case "flat":
                flat(r.insetBy(dx: s.role == "lot" || s.role == "home" ? 0.15 : 0, dy: s.role == "lot" || s.role == "home" ? 0.15 : 0),
                     l.line, width: l.width, fill: l.fill, lift: s.y0, into: patch)
            case "box":
                let bottom = elevation(s.y0), top = elevation(s.y1)
                box(r, from: bottom, to: top, l, into: patch)
                if r.width >= 0.9, r.height >= 0.45, s.y1-s.y0 < 2 {
                    flat(r, l.line, width: l.width, lift: top, into: patch)
                }
            case "stack":
                guard s.step > 0, s.count > 0 else { continue }
                var last = -CGFloat.infinity
                for k in 0..<s.count {
                    let y = elevation(s.y0 + CGFloat(k)*s.step)
                    if y-last < 2*hair { break }
                    flat(r, l.line, width: l.width, fill: l.fill, lift: y, into: patch)
                    last = y
                }
                let roof = elevation(s.y0 + CGFloat(s.count-1)*s.step)
                if roof > last {
                    flat(r, l.line, width: l.width, fill: l.fill, lift: roof, into: patch)
                    sign("\(world.top)", Self.lobbyPink, x: 0, z: 0, lift: roof+mark, style: style, perMetre: perMetre, into: patch)
                }
            case "panel":
                upright(CGSize(width: s.w, height: elevation(s.y0+s.h)-elevation(s.y0)), x: 0, z: 0, yaw: s.yaw, lift: elevation(s.y0), border: l.line, width: l.width,
                        fill: l.fill, into: patch)
            default: continue
            }
        }

        // The model may have changed size: everyone is stood again at the new
        // scale, in place, keeping the lines of where they have been.
        crowd.sublayers?.forEach { $0.removeFromSuperlayer() }
        walkers.removeAll()
        trails = trails.filter { id, _ in world.people.contains { ($0.watching ? "eye " : "")+$0.handle == id } }
        move(world.people)

        let key = "\(world.shape)\(Int(size.width))x\(Int(size.height))"
        guard key != flightKey else { return }
        flightKey = key

        sublayerTransform = CATransform3DIdentity
        rig.position = CGPoint(x: size.width / 2, y: size.height / 2)
        // A complete revolution exposes the back of the map. Core Animation
        // interpolates these transforms without a wallpaper render loop.
        let frames = (0...120).map { n -> NSValue in
            var m = CATransform3DMakeRotation(CGFloat(n) * .pi/60 + 0.45, 0, 1, 0)
            m = CATransform3DConcat(m, CATransform3DMakeRotation(-0.45, 1, 0, 0))
            return NSValue(caTransform3D: CATransform3DConcat(m, CATransform3DMakeScale(perMetre, perMetre, perMetre)))
        }
        let spin = CAKeyframeAnimation(keyPath: "transform")
        spin.values = frames
        spin.duration = 48
        spin.repeatCount = .infinity
        rig.transform = frames[0].caTransform3DValue
        rig.add(spin, forKey: "fly")
    }
}
