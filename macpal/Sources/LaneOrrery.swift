// LaneOrrery — the lane board in miniature, for a collapsed pal.
//
// Folded into its chip, a pal has no room for cards, so the lanes become a
// few fireflies beside the name: one glowing dot per lane in its own color,
// each wandering its own looping path in a strip of the box (board order,
// left to right), with a faint spark trailing it per queued task. How a
// firefly moves and blinks carries its lane's state:
//   running   wanders busily, bright pulse     waiting  drifts slowly, soft glow
//   idle      dim, nearly still, rare blink    blocked  holds still, flashing red
// Reduce Motion stills them all but keeps colors and glows.

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

final class LaneOrreryView: NSView {
    static let size = NSSize(width: 70, height: 32)
    private(set) var bodies: [OrreryBody] = []
    private var dark: Bool?

    override init(frame: NSRect) {
        super.init(frame: frame)
        wantsLayer = true
        layer?.masksToBounds = false
    }
    required init?(coder: NSCoder) { fatalError("no nib") }
    override func hitTest(_ p: NSPoint) -> NSView? { nil }   // clicks reach the chip

    func update(_ next: [OrreryBody], dark nextDark: Bool) {
        // Rebuilding restarts every orbit, so only do it when something moved.
        guard next != bodies || nextDark != dark else { return }
        bodies = next; dark = nextDark
        rebuild()
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
        let path = CGMutablePath()
        func mid(_ a: CGPoint, _ b: CGPoint) -> CGPoint { CGPoint(x: (a.x + b.x) / 2, y: (a.y + b.y) / 2) }
        path.move(to: mid(pts[count - 1], pts[0]))
        for i in 0..<count {
            path.addQuadCurve(to: mid(pts[i], pts[(i + 1) % count]), control: pts[i])
        }
        path.closeSubpath()
        return path
    }

    private func rebuild() {
        layer?.sublayers?.forEach { $0.removeFromSuperlayer() }
        guard !bodies.isEmpty, let root = layer else { return }
        let reduce = NSWorkspace.shared.accessibilityDisplayShouldReduceMotion
        let b = NSRect(origin: .zero, size: Self.size).insetBy(dx: 4, dy: 5)
        let n = bodies.count
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
            let wander: CGRect = running ? area
                : waiting ? area.insetBy(dx: area.width * 0.2, dy: area.height * 0.25)
                : CGRect(x: area.midX - 2, y: area.midY - 2, width: 4, height: 4)
            let path = Self.flight(in: wander, count: running ? 6 : 4, rng: &rng)
            let period: Double = running ? 7 + Double(rng.next()) * 4 : waiting ? 18 : 26
            let start = CGPoint(x: wander.midX, y: wander.midY)

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
                    follow.timeOffset = period * (Double(i) * 0.37).truncatingRemainder(dividingBy: 1)
                        - Double(m + 1) * 0.35
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
                    move.timeOffset = period * (Double(i) * 0.37).truncatingRemainder(dividingBy: 1)
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
        let scale = window?.backingScaleFactor ?? 2
        root.sublayers?.forEach { $0.contentsScale = scale }
        toolTip = bodies.map { $0.state }.joined(separator: " · ")
    }
}
