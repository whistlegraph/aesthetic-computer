import Foundation

public enum StrokeSmoothing: Int, CaseIterable {
    case off, light, balanced, strong, string6, string12, string24
    public var title: String {
        ["Off", "Smooth 1", "Smooth 2", "Smooth 3", "String 6", "String 12", "String 24"][rawValue]
    }
    public var stringLength: Double? {
        switch self { case .string6: return 6; case .string12: return 12; case .string24: return 24; default: return nil }
    }
    var parameters: (cutoff: Double, beta: Double) {
        switch self {
        case .light: return (5, 0.06)
        case .strong: return (1, 0.012)
        default: return (2, 0.025)
        }
    }
}

/// Screen-point input filter. The stored painting contains its final emitted
/// geometry, so reopening never filters a stroke for a second time.
/// Adaptive modes implement the speed-dependent cutoff from the 1€ filter
/// (Casiez, Roussel & Vogel, CHI 2012). String modes constrain the brush to a
/// tether, following the interaction described by Lazy Nezumi's public guide.
public struct StrokeStabilizer {
    public let mode: StrokeSmoothing
    public private(set) var position: Point
    public private(set) var target: Point
    private var previousRaw: Point
    private var velocity = Point()
    private var previousTime: Double

    public init(point: Point, time: Double, mode: StrokeSmoothing) {
        position = point; target = point; previousRaw = point; previousTime = time; self.mode = mode
    }
    public mutating func update(_ raw: Point, at time: Double) -> Point {
        guard raw.x.isFinite, raw.y.isFinite, time.isFinite else { return position }
        target = raw
        if mode == .off { position = raw; previousRaw = raw; previousTime = time; return position }
        if let length = mode.stringLength {
            let delta = raw - position, distance = hypot(delta.x, delta.y)
            if distance > length { position = position + delta * ((distance - length) / distance) }
            previousRaw = raw; previousTime = time
            return position
        }
        guard time > previousTime else { return position }
        let dt = time - previousTime
        func alpha(_ cutoff: Double) -> Double { 1 / (1 + 1 / (2 * .pi * cutoff * dt)) }
        let derivative = (raw - previousRaw) * (1 / dt)
        velocity = velocity + (derivative - velocity) * alpha(1)
        let parameters = mode.parameters
        let cutoff = parameters.cutoff + parameters.beta * hypot(velocity.x, velocity.y)
        position = position + (raw - position) * alpha(cutoff)
        previousRaw = raw; previousTime = time
        return position
    }
    /// Adaptive modes finish at the actual lift position, with no extrapolation
    /// beyond it. A pulled string stops at its brush tip, never snapping its tail.
    public mutating func finish() -> [Point] {
        guard mode != .off, mode.stringLength == nil else { return [] }
        var result: [Point] = []
        for _ in 0..<48 {
            let delta = target - position
            if hypot(delta.x, delta.y) <= 0.05 { break }
            result.append(update(target, at: previousTime + 1.0 / 120))
        }
        if position != target { position = target; result.append(target) }
        return result
    }
}
