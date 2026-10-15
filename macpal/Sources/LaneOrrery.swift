// LaneOrrery — the fleet in miniature, for a collapsed pal.
//
// Folded into its chip, a pal has no room for cards, so the work becomes a
// swarm of fireflies around the badge, in three sizes:
//   lanes   big sticker fireflies, one per Iris lane in its own color, with a
//           faint spark trailing per queued task
//   prox    tiny fireflies, one per open prompt rock on THIS machine, colored
//           by agent (Claude terracotta, Codex green, Aesel violet)
//   fleet   the smallest specks, one per open rock on every OTHER machine,
//           colored by machine
// Motion carries state. Lanes: running circles the whole badge, waiting
// tours it slowly, idle potters about its patch, blocked holds still and
// flashes red. Prox: working circles, awaiting hovers and beckons, finished
// rests dimly. Paths are seeded by identity and phased by the wall clock, so
// a refresh never makes the swarm jump. Reduce Motion stills them all.

import AppKit

struct OrreryBody: Equatable {
    let color: UInt32
    let state: String
    let moons: Int
}

func orreryBodies(_ mission: Mission?) -> [OrreryBody] {
    guard let items = mission?.items else { return [] }
    var bodies: [OrreryBody] = []
    for (i, header) in items.enumerated() where header.isLaneHeader {
        let lane = header.laneID
        var moons = 0
        for row in items[(i + 1)...] {
            if row.laneID != lane || row.isLaneHeader { break }
            if row.isQueued {
                // "+N more queued" counts its N.
                let n = row.text.hasPrefix("+") ? Int(row.text.dropFirst().prefix { $0.isNumber }) ?? 1 : 1
                moons += n
            }
        }
        bodies.append(OrreryBody(color: header.laneColor ?? 0x888888,
                                 state: (header.laneState ?? "idle").lowercased(), moons: moons))
    }
    return bodies
}

// One open prompt rock, as the Slab ledger reports it.
struct ProxSpark: Equatable {
    let id: String
    let host: String
    let agent: String
    let status: String
    let local: Bool
}

// Every rock this machine's Slab ledger knows: its own (local.json) and each
// cached peer's. Mirrors prox_list's notion of "open": finished rocks idle a
// day are residue, and a peer whose ledger hasn't refreshed in a day is off.
func loadProxSparks(ledgerDir: String = NSHomeDirectory() + "/.config/slab/ledger",
                    now: Date = Date()) -> [ProxSpark] {
    func read(_ path: String) -> [String: Any]? {
        guard let d = FileManager.default.contents(atPath: path) else { return nil }
        return (try? JSONSerialization.jsonObject(with: d)) as? [String: Any]
    }
    func ms(_ v: Any?) -> Double { (v as? NSNumber)?.doubleValue ?? 0 }
    let nowMs = now.timeIntervalSince1970 * 1000
    let day = 24 * 3600 * 1000.0
    var ledgers: [([String: Any], Bool)] = []
    if let local = read(ledgerDir + "/local.json") { ledgers.append((local, true)) }
    let selfHost = (ledgers.first?.0["host"] as? String)?.lowercased()
    let peers = ((try? FileManager.default.contentsOfDirectory(atPath: ledgerDir + "/peers")) ?? [])
        .filter { $0.hasSuffix(".json") }.sorted()
    for f in peers {
        guard let p = read(ledgerDir + "/peers/" + f),
              (p["host"] as? String)?.lowercased() != selfHost,
              nowMs - ms(p["updatedAt"]) < day else { continue }
        ledgers.append((p, false))
    }
    var seen = Set<String>()
    var out: [ProxSpark] = []
    for (ledger, local) in ledgers {
        for e in (ledger["entries"] as? [[String: Any]]) ?? [] {
            guard let id = e["id"] as? String, seen.insert(id).inserted else { continue }
            let status = ((e["status"] as? String) ?? "blank").lowercased()
            let finished = ["complete", "interrupted", "blank"].contains(status)
            if finished && nowMs - ms(e["updated"]) > day { continue }
            out.append(ProxSpark(id: id, host: ((e["host"] as? String) ?? (ledger["host"] as? String) ?? "?").lowercased(),
                                 agent: ((e["agentType"] as? String) ?? "claude").lowercased(),
                                 status: status, local: local))
        }
    }
    return out
}

func proxAgentColor(_ agent: String) -> UInt32 {
    switch agent {
    case "claude": return 0xD97757
    case "codex", "openai": return 0x10A37F
    case "aesel", "easel": return 0x8B5CF6
    default: return 0x9AA0A6
    }
}

// Each machine's identity color, after its avatar where it has one.
func proxMachineColor(_ host: String) -> UInt32 {
    switch host {
    case "frisbee": return 0x38BDF8
    case "neo": return 0xA855F7
    case "blueberry": return 0x3B5BDB
    case "panda": return 0xF1F1F1
    case "chicken": return 0xFACC15
    case "cookie": return 0xC08457
    case "jasellite": return 0x94A3B8
    case "poorslice": return 0xF97316
    default:
        // Anyone else gets a stable hue from their name.
        let h = host.unicodeScalars.reduce(UInt32(5381)) { ($0 &* 33) &+ $1.value }
        let c = NSColor(hue: CGFloat(h % 360) / 360, saturation: 0.6, brightness: 0.9, alpha: 1)
            .usingColorSpace(.sRGB)!
        return UInt32(c.redComponent * 255) << 16 | UInt32(c.greenComponent * 255) << 8 | UInt32(c.blueComponent * 255)
    }
}

final class LaneOrreryView: NSView, ChipRoaming {
    static let size = NSSize(width: 64, height: 46)
    private(set) var bodies: [OrreryBody] = []
    private(set) var sparks: [ProxSpark] = []
    var isEmpty: Bool { bodies.isEmpty && sparks.isEmpty }
    private var dark: Bool?
    // The whole chip in this view's coordinates (set by the pal's layout), so
    // busy fireflies can circle the badge rather than keep to the side strip.
    var roam: CGRect = .zero {
        didSet { if roam != oldValue { rebuild() } }
    }

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        layer?.masksToBounds = false
    }
    required init?(coder: NSCoder) { fatalError("no nib") }
    override func hitTest(_ p: NSPoint) -> NSView? { nil }   // clicks reach the chip

    func update(_ next: [OrreryBody], sparks nextSparks: [ProxSpark], dark nextDark: Bool) {
        // Rebuild only when something changed; wall-clock phasing keeps the
        // unchanged fireflies exactly where they were.
        guard next != bodies || nextSparks != sparks || nextDark != dark else { return }
        bodies = next; sparks = nextSparks; dark = nextDark
        rebuild()
    }

    // Where along a loop of `period` seconds a firefly is right now: a fixed
    // per-identity phase plus the wall clock, so rebuilding resumes in place.
    private static func phase(_ period: Double, _ seed: CGFloat) -> Double {
        (Date().timeIntervalSince1970 + Double(seed) * period).truncatingRemainder(dividingBy: period)
    }

    private static func seed(_ text: String) -> UInt64 {
        text.utf8.reduce(UInt64(1469598103934665603)) { ($0 ^ UInt64($1)) &* 1099511628211 }
    }

    override func viewDidChangeBackingProperties() {
        super.viewDidChangeBackingProperties()
        layer?.sublayers?.forEach { $0.contentsScale = window?.backingScaleFactor ?? 2 }
    }

    // A small deterministic generator, so a rebuild gives each lane the same
    // flight path rather than a fresh jitter every refresh.
    private struct Seeded {
        var x: UInt64
        mutating func next() -> CGFloat {
            x = x &* 6364136223846793005 &+ 1442695040888963407
            return CGFloat(x >> 33) / CGFloat(1 << 31)
        }
    }

    // A closed, smooth loop through `count` random points inside `area`.
    private static func flight(in area: CGRect, count: Int, rng: inout Seeded) -> CGPath {
        let pts = (0..<count).map { _ in
            CGPoint(x: area.minX + rng.next() * area.width, y: area.minY + rng.next() * area.height)
        }
        return smooth(pts)
    }

    // A wobbly closed loop around `area`'s middle: `count` points stepping
    // round an ellipse, each pushed in or out a little, smoothed by flight().
    private static func circuit(around area: CGRect, count: Int, reverse: Bool,
                                rng: inout Seeded) -> CGPath {
        let c = CGPoint(x: area.midX, y: area.midY)
        let start = rng.next() * 2 * .pi
        let pts: [CGPoint] = (0..<count).map { k in
            let a = start + (reverse ? -1 : 1) * CGFloat(k) / CGFloat(count) * 2 * .pi
                + (rng.next() - 0.5) * 0.5
            let rx = area.width / 2 * (0.78 + 0.22 * rng.next())
            let ry = area.height / 2 * (0.7 + 0.3 * rng.next())
            return CGPoint(x: c.x + rx * cos(a), y: c.y + ry * sin(a))
        }
        return smooth(pts)
    }

    private static func smooth(_ pts: [CGPoint]) -> CGPath {
        let path = CGMutablePath()
        func mid(_ a: CGPoint, _ b: CGPoint) -> CGPoint { CGPoint(x: (a.x + b.x) / 2, y: (a.y + b.y) / 2) }
        path.move(to: mid(pts[pts.count - 1], pts[0]))
        for i in 0..<pts.count {
            path.addQuadCurve(to: mid(pts[i], pts[(i + 1) % pts.count]), control: pts[i])
        }
        path.closeSubpath()
        return path
    }

    private func rebuild() {
        layer?.sublayers?.forEach { $0.removeFromSuperlayer() }
        guard !isEmpty, let root = layer else { return }
        let reduce = NSWorkspace.shared.accessibilityDisplayShouldReduceMotion
        let own = NSRect(origin: .zero, size: Self.size).insetBy(dx: 4, dy: 5)
        // The circuit encloses the whole chip when the pal has told us where it is.
        let ring = (roam.isEmpty ? own : roam.union(own)).insetBy(dx: 5, dy: 5)
        let b = own
        let n = max(1, bodies.count)
        let strip = b.width / CGFloat(n)
        let red = hexColor(0xE5484D)

        for (i, body) in bodies.enumerated() {
            var rng = Seeded(x: UInt64(i + 1) &* 0x9E3779B97F4A7C15 ^ UInt64(body.color))
            let state = body.state
            let blocked = ["blocked", "failed", "error"].contains(state)
            let running = state == "running"
            let waiting = state == "waiting" || state.contains("review")
            let color = hexColor(body.color)
            // Each lane keeps to its own strip, overlapping its neighbours a
            // little so the swarm reads as one cloud, not four cages.
            let area = CGRect(x: b.minX + strip * CGFloat(i) - strip * 0.25, y: b.minY,
                              width: strip * 1.5, height: b.height)
                .intersection(b)
            // Busy lanes circumnavigate the badge (alternate lanes the other
            // way round); waiting lanes take the same tour at a stroll; idle
            // lanes potter about their own patch beside the name.
            let patch = CGRect(x: area.midX - 7, y: area.midY - 6, width: 14, height: 12)
            let path = running || waiting
                ? Self.circuit(around: ring, count: 9, reverse: i % 2 == 1, rng: &rng)
                : Self.flight(in: patch, count: 5, rng: &rng)
            let period: Double = running ? Double(ring.width) / 14 + Double(rng.next()) * 3
                : waiting ? Double(ring.width) / 5 : 20
            let start = path.boundingBoxOfPath.origin == .zero ? CGPoint(x: area.midX, y: area.midY)
                : CGPoint(x: path.boundingBoxOfPath.midX, y: path.boundingBoxOfPath.midY)

            func flyer(_ size: CGFloat, glow: CGFloat, tint: NSColor, opacity: Float) -> CALayer {
                let l = CALayer()
                l.bounds = CGRect(x: 0, y: 0, width: size, height: size)
                l.cornerRadius = size / 2
                l.position = start
                l.backgroundColor = tint.cgColor
                l.shadowColor = tint.cgColor
                l.shadowOpacity = 1; l.shadowRadius = glow; l.shadowOffset = .zero
                l.opacity = opacity
                return l
            }

            // Trailing sparks for the queue: same path, a beat behind.
            for m in 0..<min(3, body.moons) {
                let spark = flyer(1.6, glow: 1.5, tint: color, opacity: 0.55)
                if !reduce && !blocked {
                    let follow = CAKeyframeAnimation(keyPath: "position")
                    follow.path = path; follow.calculationMode = .paced
                    follow.duration = period; follow.repeatCount = .infinity
                    follow.timeOffset = Self.phase(period, CGFloat(i) * 0.37) - Double(m + 1) * 0.35
                    spark.add(follow, forKey: "fly")
                }
                root.addSublayer(spark)
            }

            let fly = flyer(running ? 7 : 6, glow: running ? 4 : 2.5,
                            tint: blocked ? red : color, opacity: running ? 1 : waiting ? 0.85 : 0.6)
            // Sticker finish, so a speck reads at a glance on any desktop: an
            // ink outline in the comic style, and a white core lit a touch
            // up-left. The core is a sublayer, so it flies and blinks along.
            fly.borderColor = hexColor(0x1B1530).withAlphaComponent(0.85).cgColor
            fly.borderWidth = 1
            let core = CALayer()
            let cd = fly.bounds.width * 0.42
            core.bounds = CGRect(x: 0, y: 0, width: cd, height: cd)
            core.cornerRadius = cd / 2
            core.position = CGPoint(x: fly.bounds.midX - 0.5, y: fly.bounds.midY + 0.5)
            core.backgroundColor = NSColor.white.cgColor
            fly.addSublayer(core)
            // Drop shadow in the pal's comic style (the name and avatar wear
            // it too): a hard accent-colored copy, no blur, cast down-right. It
            // sits in a transform offset so the shared position animation
            // carries both without any arithmetic.
            let drop = CALayer()
            drop.bounds = fly.bounds
            drop.cornerRadius = fly.cornerRadius
            drop.position = start
            drop.transform = CATransform3DMakeTranslation(1.5, -1.5, 0)
            drop.backgroundColor = accent.cgColor
            root.addSublayer(drop)
            if !reduce {
                if !blocked {
                    let move = CAKeyframeAnimation(keyPath: "position")
                    move.path = path; move.calculationMode = .paced
                    move.duration = period; move.repeatCount = .infinity
                    move.timeOffset = Self.phase(period, CGFloat(i) * 0.37)
                    fly.add(move, forKey: "fly")
                    drop.add(move, forKey: "fly")
                }
                // The blink: brief flares for the busy, slow breaths for the
                // waiting, a rare wink for the idle, an alarm for the blocked.
                let blink = CAKeyframeAnimation(keyPath: "opacity")
                let lo: Float = blocked ? 0.3 : running ? 0.6 : waiting ? 0.45 : 0.4
                let hi: Float = blocked || running ? 1 : waiting ? 0.9 : 0.75
                blink.values = [lo, hi, lo, lo]
                blink.keyTimes = blocked ? [0, 0.25, 0.5, 1] : [0, 0.12, 0.3, 1]
                blink.duration = blocked ? 1 : running ? 1.4 + Double(rng.next()) : waiting ? 3.5 : 6
                blink.timeOffset = Double(rng.next()) * blink.duration
                blink.repeatCount = .infinity
                fly.add(blink, forKey: "blink")
                let flare = CABasicAnimation(keyPath: "shadowRadius")
                flare.fromValue = running ? 2.5 : 1.5; flare.toValue = running ? 5.5 : 3
                flare.duration = blink.duration / 2; flare.autoreverses = true
                flare.timeOffset = blink.timeOffset; flare.repeatCount = .infinity
                fly.add(flare, forKey: "flare")
            }
            root.addSublayer(fly)
        }
        // Prox: tiny for this machine, tinier for the fleet, drawn under the
        // lanes so the big fireflies stay on top.
        let laneLayers = root.sublayers ?? []
        for spark in sparks {
            var rng = Seeded(x: Self.seed(spark.id))
            let working = spark.status == "working" || spark.status == "rendering"
            let awaiting = spark.status == "awaiting"
            let size: CGFloat = spark.local ? 3.6 : 2.3
            let color = hexColor(spark.local ? proxAgentColor(spark.agent) : proxMachineColor(spark.host))
            // Working rocks circle the badge on their own lane of the circuit;
            // awaiting ones hover somewhere near the name; finished ones settle.
            let lane = ring.insetBy(dx: rng.next() * 7, dy: rng.next() * 6)
            let spot = CGPoint(x: ring.minX + rng.next() * ring.width, y: ring.minY + rng.next() * ring.height)
            let hover = CGRect(x: spot.x - 4, y: spot.y - 3, width: 8, height: 6).intersection(ring)
            let path = working
                ? Self.circuit(around: lane, count: 8, reverse: rng.next() > 0.5, rng: &rng)
                : Self.flight(in: awaiting ? hover : hover.insetBy(dx: 2, dy: 1.5), count: 4, rng: &rng)
            let period: Double = working ? Double(lane.width) / (spark.local ? 16 : 11) + Double(rng.next()) * 2
                : awaiting ? 9 : 24
            let l = CALayer()
            l.bounds = CGRect(x: 0, y: 0, width: size, height: size)
            l.cornerRadius = size / 2
            l.position = spot
            l.backgroundColor = color.cgColor
            l.shadowColor = color.cgColor
            l.shadowOpacity = 1; l.shadowRadius = spark.local ? 2 : 1.4; l.shadowOffset = .zero
            l.opacity = working ? 1 : awaiting ? 0.85 : 0.4
            if spark.local {
                l.borderColor = hexColor(0x1B1530).withAlphaComponent(0.8).cgColor
                l.borderWidth = 0.75
            } else if spark.host == "panda" && dark != true {
                // White panda specks need an edge on a light desktop.
                l.borderColor = NSColor.black.withAlphaComponent(0.35).cgColor
                l.borderWidth = 0.5
            }
            var drop: CALayer?
            if spark.local {
                let d = CALayer()
                d.bounds = l.bounds; d.cornerRadius = l.cornerRadius; d.position = spot
                d.transform = CATransform3DMakeTranslation(1, -1, 0)
                d.backgroundColor = accent.cgColor
                d.opacity = l.opacity
                root.insertSublayer(d, below: laneLayers.first)
                drop = d
            }
            if !reduce {
                let move = CAKeyframeAnimation(keyPath: "position")
                move.path = path; move.calculationMode = .paced
                move.duration = period; move.repeatCount = .infinity
                move.timeOffset = Self.phase(period, rng.next())
                l.add(move, forKey: "fly")
                drop?.add(move, forKey: "fly")
                if awaiting {
                    // Beckoning: a slow swell, the rock waiting on you.
                    let beckon = CABasicAnimation(keyPath: "opacity")
                    beckon.fromValue = 0.35; beckon.toValue = 1
                    beckon.duration = 1.1; beckon.autoreverses = true; beckon.repeatCount = .infinity
                    beckon.timeOffset = Self.phase(2.2, rng.next())
                    l.add(beckon, forKey: "beckon")
                }
            }
            root.insertSublayer(l, below: laneLayers.first)
        }
        let scale = window?.backingScaleFactor ?? 2
        root.sublayers?.forEach { $0.contentsScale = scale }
        let here = sparks.filter { $0.local }.count
        var tip = bodies.isEmpty ? [] : ["\(bodies.count) lanes: " + bodies.map { $0.state }.joined(separator: ", ")]
        if here > 0 { tip.append("\(here) prox here") }
        if sparks.count > here {
            let hosts = Set(sparks.filter { !$0.local }.map { $0.host }).sorted()
            tip.append("\(sparks.count - here) across " + hosts.joined(separator: ", "))
        }
        toolTip = tip.joined(separator: " · ")
    }
}
