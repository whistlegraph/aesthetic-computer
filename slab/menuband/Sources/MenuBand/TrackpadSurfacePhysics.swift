// The trackpad surface, as physics: a decaying energy field and a damped
// membrane. Both read pure normalized surface coordinates, so they belong to
// no window system — the phone edition (apple/trackdrum) compiles this file
// unchanged. PitchBendCursor.swift, which draws them on AppKit, is next door.
//
// The bare `* 1.64` scalings are the trackpad's real 1.64:1 aspect: x and y
// are both normalized 0…1, so a physical distance has to put the ratio back.

import CoreGraphics
import Foundation

/// Sparse inertial energy field shared by the physical and electronic
/// surfaces. Charges persist, merge locally, and decay; audio and drawing read
/// the same values so an illuminated region is literally an energized region.
struct TrackpadSurfaceEnergy {
    struct Charge: Equatable {
        var point: CGPoint
        var level: Double
    }

    private(set) var charges: [Charge] = []
    private var lastTime: Double = 0
    private let decaySeconds = 0.90

    mutating func reset(at now: Double) {
        charges.removeAll(keepingCapacity: true)
        lastTime = now
    }

    mutating func decay(to now: Double) {
        if lastTime == 0 { lastTime = now; return }
        let dt = max(0, min(1.0, now - lastTime))
        lastTime = now
        let factor = exp(-dt / decaySeconds)
        for index in charges.indices { charges[index].level *= factor }
        charges.removeAll { $0.level < 0.008 }
    }

    mutating func energize(at point: CGPoint, amount: Double, now: Double) {
        decay(to: now)
        let addition = min(1, max(0, amount))
        guard addition > 0 else { return }
        if let index = charges.indices.min(by: {
            Self.distanceSquared(charges[$0].point, point)
                < Self.distanceSquared(charges[$1].point, point)
        }), Self.distanceSquared(charges[index].point, point) < 0.050 {
            let old = charges[index]
            let blend = min(0.58, 0.18 + addition * 0.42)
            charges[index].point = CGPoint(
                x: old.point.x + (point.x - old.point.x) * blend,
                y: old.point.y + (point.y - old.point.y) * blend
            )
            charges[index].level = min(1, old.level + addition * (1 - old.level))
        } else {
            charges.append(Charge(point: point, level: addition))
            if charges.count > 12 {
                charges.remove(at: charges.indices.min(by: {
                    charges[$0].level < charges[$1].level
                })!)
            }
        }
    }

    mutating func constrain(with anchors: [CGPoint], duration: Double, now: Double) {
        decay(to: now)
        guard !anchors.isEmpty else { return }
        let dt = max(0, min(0.050, duration))
        for index in charges.indices {
            var pull = 0.0
            for anchor in anchors {
                let distance = sqrt(Self.distanceSquared(charges[index].point, anchor))
                pull += max(0, 1 - distance / 0.42)
            }
            charges[index].level *= exp(-dt * pull * 1.8)
        }
    }

    mutating func energy(at point: CGPoint, now: Double) -> Double {
        decay(to: now)
        return Self.energy(at: point, charges: charges)
    }

    mutating func snapshot(at now: Double) -> [Charge] {
        decay(to: now)
        return charges
    }

    static func energy(at point: CGPoint, charges: [Charge]) -> Double {
        var total = 0.0
        for charge in charges {
            let dx = Double(point.x - charge.point.x) * 1.64
            let dy = Double(point.y - charge.point.y)
            let d2 = dx * dx + dy * dy
            total += charge.level * exp(-d2 / (2 * 0.19 * 0.19))
        }
        return min(1, total)
    }

    private static func distanceSquared(_ a: CGPoint, _ b: CGPoint) -> Double {
        let dx = Double(a.x - b.x) * 1.64
        let dy = Double(a.y - b.y)
        return dx * dx + dy * dy
    }
}

/// A damped, rim-fixed membrane advanced from the same touch clock as the
/// instrument. Every cell exchanges velocity with its neighbors, so a strike
/// bends and rings the whole sheet instead of moving an isolated decoration.
struct TrackpadMembraneSimulation {
    struct Snapshot {
        let columns: Int
        let rows: Int
        let heights: [Double]

        var isFlat: Bool {
            !heights.contains { abs($0) > 0.000_5 }
        }

        func height(at point: CGPoint) -> CGFloat {
            let x = max(0, min(1, point.x)) * CGFloat(columns - 1)
            let y = max(0, min(1, point.y)) * CGFloat(rows - 1)
            let x0 = min(columns - 1, Int(floor(x)))
            let y0 = min(rows - 1, Int(floor(y)))
            let x1 = min(columns - 1, x0 + 1)
            let y1 = min(rows - 1, y0 + 1)
            let tx = Double(x - CGFloat(x0))
            let ty = Double(y - CGFloat(y0))
            func sample(_ column: Int, _ row: Int) -> Double {
                heights[row * columns + column]
            }
            let bottom = sample(x0, y0) * (1 - tx) + sample(x1, y0) * tx
            let top = sample(x0, y1) * (1 - tx) + sample(x1, y1) * tx
            return CGFloat(bottom * (1 - ty) + top * ty)
        }
    }

    private let columns = 23
    private let rows = 15
    private var heights: [Double] = Array(repeating: 0, count: 23 * 15)
    private var velocities: [Double] = Array(repeating: 0, count: 23 * 15)
    private var lastTime: Double = 0

    mutating func reset(at now: Double) {
        heights = Array(repeating: 0, count: columns * rows)
        velocities = Array(repeating: 0, count: columns * rows)
        lastTime = now
    }

    mutating func impulse(at point: CGPoint, amount: Double) {
        for row in 1..<(rows - 1) {
            for column in 1..<(columns - 1) {
                let normalized = CGPoint(
                    x: CGFloat(column) / CGFloat(columns - 1),
                    y: CGFloat(row) / CGFloat(rows - 1)
                )
                let dx = Double(normalized.x - point.x) * 1.64
                let dy = Double(normalized.y - point.y)
                let falloff = exp(-(dx * dx + dy * dy) / (2 * 0.105 * 0.105))
                // Taut fabric takes a compact shove instead of a broad fluid
                // splash. The stronger damping in `step` turns this into one
                // readable rebound rather than a long train of ripples.
                velocities[row * columns + column] += max(0, amount) * falloff * 6.2
            }
        }
    }

    mutating func advance(to now: Double, touches: [CGPoint]) {
        if lastTime == 0 { lastTime = now; return }
        let elapsed = max(0, min(0.080, now - lastTime))
        lastTime = now
        guard elapsed > 0 else { return }
        let stepCount = max(1, Int(ceil(elapsed / (1.0 / 240.0))))
        let dt = elapsed / Double(stepCount)
        for _ in 0..<stepCount { step(dt: dt, touches: touches) }
    }

    private mutating func step(dt: Double, touches: [CGPoint]) {
        var nextHeights = heights
        var nextVelocities = velocities
        for row in 1..<(rows - 1) {
            for column in 1..<(columns - 1) {
                let index = row * columns + column
                let horizontal = heights[index - 1] + heights[index + 1]
                    - 2 * heights[index]
                let vertical = heights[index - columns] + heights[index + columns]
                    - 2 * heights[index]
                // The rectangular grid has more columns solely to preserve
                // physical sampling density; both axes carry equal tension.
                let point = CGPoint(
                    x: CGFloat(column) / CGFloat(columns - 1),
                    y: CGFloat(row) / CGFloat(rows - 1)
                )
                // The middle gives under a finger, while tension rises sharply
                // into the clamped rim. Strong velocity + displacement damping
                // permits one short rebound without pond-like travelling waves.
                let edgeDistance = min(min(Double(point.x), 1 - Double(point.x)),
                                       min(Double(point.y), 1 - Double(point.y)))
                let rim = 1 - min(1, edgeDistance / 0.5)
                let rimSquared = rim * rim
                let tension = 235 + 215 * rimSquared
                let damping = 23 + 8 * rimSquared
                let spring = 48 + 52 * rimSquared
                var acceleration = tension * (horizontal + vertical)
                    - damping * velocities[index] - spring * heights[index]
                for touch in touches {
                    let dx = Double(point.x - touch.x) * 1.64
                    let dy = Double(point.y - touch.y)
                    let pressure = exp(-(dx * dx + dy * dy) / (2 * 0.115 * 0.115))
                    acceleration += pressure * (0.72 - heights[index]) * 165
                }
                let velocity = velocities[index] + acceleration * dt
                nextVelocities[index] = velocity
                nextHeights[index] = heights[index] + velocity * dt
            }
        }
        heights = nextHeights
        velocities = nextVelocities
    }

    func snapshot() -> Snapshot {
        Snapshot(columns: columns, rows: rows, heights: heights)
    }
}
