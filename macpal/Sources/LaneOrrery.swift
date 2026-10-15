// LaneOrrery — the lane board in miniature, for a collapsed pal.
//
// Folded into its chip, a pal has no room for cards, so the lanes become a
// small tilted solar system beside the name: Iris is the sun, each lane a
// planet in its own color on its own orbit (board order, inside out), and
// each queued task a moon. Motion carries the state:
//   running   orbits (inner lanes faster)      waiting  drifts slowly
//   idle      rests, dimmed                    blocked  stops under a red pulse
// Reduce Motion parks every planet but keeps the colors and halos.

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
    static let size = NSSize(width: 96, height: 46)
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

    private func rebuild() {
        layer?.sublayers?.forEach { $0.removeFromSuperlayer() }
        guard !bodies.isEmpty, let root = layer else { return }
        let reduce = NSWorkspace.shared.accessibilityDisplayShouldReduceMotion
        let b = NSRect(origin: .zero, size: Self.size)
        let center = CGPoint(x: b.midX, y: b.midY)
        let n = bodies.count
        // Orbits fill the box: innermost clears the sun, outermost the edge.
        let rx0: CGFloat = 13, rxMax = b.width / 2 - 5
        let ry0: CGFloat = 7, ryMax = b.height / 2 - 5
        let anyRunning = bodies.contains { $0.state == "running" }

        // Sun: a warm dot with a glow that breathes while any lane works.
        let sun = CALayer()
        sun.bounds = CGRect(x: 0, y: 0, width: 10, height: 10)
        sun.position = center
        sun.cornerRadius = 5
        sun.backgroundColor = hexColor(0xFFC23D).cgColor
        sun.shadowColor = hexColor(0xFFB000).cgColor
        sun.shadowOpacity = 0.9; sun.shadowRadius = 3; sun.shadowOffset = .zero
        if anyRunning && !reduce {
            let glow = CABasicAnimation(keyPath: "shadowRadius")
            glow.fromValue = 3; glow.toValue = 7
            glow.duration = 1.6; glow.autoreverses = true; glow.repeatCount = .infinity
            sun.add(glow, forKey: "glow")
        }

        for (i, body) in bodies.enumerated() {
            let t = n == 1 ? 1 : CGFloat(i) / CGFloat(n - 1)
            let rx = rx0 + (rxMax - rx0) * t, ry = ry0 + (ryMax - ry0) * t
            let orbitRect = CGRect(x: center.x - rx, y: center.y - ry, width: rx * 2, height: ry * 2)
            let color = hexColor(body.color)
            let idle = !["running", "waiting", "blocked", "review", "awaiting review"].contains(body.state)

            let ring = CAShapeLayer()
            ring.path = CGPath(ellipseIn: orbitRect, transform: nil)
            ring.fillColor = nil
            ring.strokeColor = color.withAlphaComponent(idle ? 0.22 : 0.42).cgColor
            ring.lineWidth = 1
            root.addSublayer(ring)

            // Planet carrier: moves along the orbit and holds its moons.
            let carrier = CALayer()
            carrier.bounds = CGRect(x: 0, y: 0, width: 18, height: 18)
            let phase = CGFloat(i) * 2.399   // golden angle: no two planets line up
            carrier.position = CGPoint(x: center.x + rx * cos(phase), y: center.y + ry * sin(phase))

            let planet = CALayer()
            let d: CGFloat = idle ? 6.5 : 7.5
            planet.bounds = CGRect(x: 0, y: 0, width: d, height: d)
            planet.position = CGPoint(x: 9, y: 9)
            planet.cornerRadius = d / 2
            // Near-white lanes (snow) need an edge to exist on a light desktop.
            planet.backgroundColor = color.cgColor
            planet.borderColor = (dark == true ? NSColor.white : NSColor.black).withAlphaComponent(0.35).cgColor
            planet.borderWidth = 0.5
            planet.opacity = idle ? 0.5 : 1
            carrier.addSublayer(planet)

            if body.state == "blocked" || body.state == "failed" || body.state == "error" {
                let halo = CALayer()
                halo.bounds = CGRect(x: 0, y: 0, width: 14, height: 14)
                halo.position = planet.position
                halo.cornerRadius = 7
                halo.borderColor = hexColor(0xE5484D).cgColor
                halo.borderWidth = 1.2
                if !reduce {
                    let pulse = CABasicAnimation(keyPath: "opacity")
                    pulse.fromValue = 1; pulse.toValue = 0.15
                    pulse.duration = 0.9; pulse.autoreverses = true; pulse.repeatCount = .infinity
                    halo.add(pulse, forKey: "pulse")
                }
                carrier.addSublayer(halo)
            }

            // Moons: one per queued task (up to three), circling the planet.
            let moons = min(3, body.moons)
            if moons > 0 {
                let moonRing = CALayer()
                moonRing.bounds = carrier.bounds
                moonRing.position = planet.position
                for m in 0..<moons {
                    let a = CGFloat(m) / CGFloat(moons) * 2 * .pi
                    let moon = CALayer()
                    moon.bounds = CGRect(x: 0, y: 0, width: 2.5, height: 2.5)
                    moon.cornerRadius = 1.25
                    moon.position = CGPoint(x: 9 + 7 * cos(a), y: 9 + 7 * sin(a))
                    moon.backgroundColor = (dark == true ? NSColor.white : NSColor.darkGray).cgColor
                    moonRing.addSublayer(moon)
                }
                if !reduce {
                    let spin = CABasicAnimation(keyPath: "transform.rotation.z")
                    spin.fromValue = 0; spin.toValue = 2 * Double.pi
                    spin.duration = 3; spin.repeatCount = .infinity
                    moonRing.add(spin, forKey: "spin")
                }
                carrier.addSublayer(moonRing)
            }

            // Running orbits; waiting drifts; idle and blocked hold still.
            let period: Double? = body.state == "running" ? 5 + Double(i) * 2.5
                : (body.state == "waiting" || body.state.contains("review")) ? 40 : nil
            if let period = period, !reduce {
                let orbit = CAKeyframeAnimation(keyPath: "position")
                orbit.path = CGPath(ellipseIn: orbitRect, transform: nil)
                orbit.calculationMode = .paced
                orbit.duration = period
                orbit.repeatCount = .infinity
                orbit.timeOffset = period * Double(phase / (2 * .pi))
                carrier.add(orbit, forKey: "orbit")
            }
            root.addSublayer(carrier)
        }
        root.addSublayer(sun)
        let scale = window?.backingScaleFactor ?? 2
        root.sublayers?.forEach { $0.contentsScale = scale }
        toolTip = bodies.map { $0.state }.joined(separator: " · ")
    }
}
