import Foundation

public struct Point: Codable, Equatable {
    public var x: Double
    public var y: Double
    public init(_ x: Double = 0, _ y: Double = 0) { self.x = x; self.y = y }
    public static func + (a: Point, b: Point) -> Point { Point(a.x + b.x, a.y + b.y) }
    public static func - (a: Point, b: Point) -> Point { Point(a.x - b.x, a.y - b.y) }
    public static func * (a: Point, b: Double) -> Point { Point(a.x * b, a.y * b) }
}

public struct Camera: Codable, Equatable {
    public static let zoomRange = 1e-12...1e12
    public var center = Point()
    public var zoom: Double = 1
    public init() {}
    public func world(_ screen: Point, size: Point) -> Point {
        center + (screen - size * 0.5) * (1 / zoom)
    }
    public func screen(_ world: Point, size: Point) -> Point {
        (world - center) * zoom + size * 0.5
    }
    public mutating func magnify(_ factor: Double, anchor: Point, size: Point) {
        let fixed = world(anchor, size: size)
        zoom = min(Self.zoomRange.upperBound, max(Self.zoomRange.lowerBound, zoom * factor))
        center = fixed - (anchor - size * 0.5) * (1 / zoom)
    }
}

public struct Brush: Codable, Equatable {
    public var marker: Bool? = nil // Absent in version-1 airbrush strokes.
    public var radius: Double = 55
    public var hardness: Double = 0.15
    public var flow: Double = 0.35
    public var scatter: Double = 0.35
    public var red: Double = 0.12
    public var green: Double = 0.22
    public var blue: Double = 0.8
    public init() {}
    public var valid: Bool {
        radius.isFinite && radius > 0 && radius <= 1e18 &&
        [hardness, flow, scatter, red, green, blue].allSatisfy { $0.isFinite && (0...1).contains($0) }
    }
}

/// World-space emission knots, including brush state at that instant. These,
/// plus the seed and algorithm version, are the document (never a pixel cache).
public struct Sample: Codable, Equatable {
    public var point: Point
    public var brush: Brush
    public init(point: Point, brush: Brush) { self.point = point; self.brush = brush }
    public static func interpolate(_ a: Sample, _ b: Sample, _ t: Double) -> Sample {
        func mix(_ x: Double, _ y: Double) -> Double { x + (y - x) * t }
        var brush = a.brush
        brush.radius = mix(a.brush.radius, b.brush.radius)
        brush.hardness = mix(a.brush.hardness, b.brush.hardness)
        brush.flow = mix(a.brush.flow, b.brush.flow)
        brush.scatter = mix(a.brush.scatter, b.brush.scatter)
        brush.red = mix(a.brush.red, b.brush.red)
        brush.green = mix(a.brush.green, b.brush.green)
        brush.blue = mix(a.brush.blue, b.brush.blue)
        return Sample(point: a.point + (b.point - a.point) * t, brush: brush)
    }
}

public struct Stroke: Codable, Equatable {
    public var seed: UInt64
    public var samples: [Sample]
    public init(seed: UInt64, samples: [Sample] = []) { self.seed = seed; self.samples = samples }
}

public struct Painting: Codable, Equatable {
    public var version = 2
    public var strokes: [Stroke] = []
    public var camera = Camera()
    public init() {}
    public static func decode(_ data: Data) throws -> Painting {
        let p = try JSONDecoder().decode(Painting.self, from: data)
        guard p.version == 1 || p.version == 2 else { throw PaintError.unsupportedVersion }
        guard p.camera.zoom.isFinite, Camera.zoomRange.contains(p.camera.zoom),
              finite(p.camera.center), p.strokes.count <= 100_000,
              p.strokes.reduce(0, { $0 + $1.samples.count }) <= 2_000_000,
              p.strokes.allSatisfy({ $0.samples.allSatisfy { finite($0.point) && $0.brush.valid } })
        else { throw PaintError.invalidDocument }
        return p
    }
    private static func finite(_ p: Point) -> Bool {
        p.x.isFinite && p.y.isFinite && abs(p.x) <= 1e24 && abs(p.y) <= 1e24
    }
    public func encoded() throws -> Data {
        let encoder = JSONEncoder()
        encoder.outputFormatting = [.sortedKeys]
        var document = self
        if strokes.contains(where: { $0.samples.contains(where: { $0.brush.marker == true }) }) { document.version = 2 }
        return try encoder.encode(document)
    }
}

public enum PaintError: Error { case unsupportedVersion, invalidDocument }

/// Version 1 uses counter-addressed SplitMix64. No shared PRNG, clock, Swift
/// Hasher, GPU randomness, or dependence on visible tiles / render order.
public enum Spray {
    public static let step = 1.0 / 120.0
    public static func bits(seed: UInt64, index: UInt64) -> UInt64 {
        var z = seed &+ (index &+ 1) &* 0x9e3779b97f4a7c15
        z = (z ^ (z >> 30)) &* 0xbf58476d1ce4e5b9
        z = (z ^ (z >> 27)) &* 0x94d049bb133111eb
        return z ^ (z >> 31)
    }
    public static func unit(seed: UInt64, index: UInt64) -> Double {
        Double(bits(seed: seed, index: index) >> 11) * (1.0 / 9007199254740992.0)
    }
    public static func dabs(_ sample: Sample, seed: UInt64, index: Int) -> [Dab] {
        let b = sample.brush
        var dabs: [Dab] = []
        let exposure = b.marker == true ? -log(max(1e-9, 1 - b.flow)) : b.flow * 7 * step
        if b.scatter < 1 {
            dabs.append(Dab(point: sample.point, radius: b.radius,
                            opacity: 1 - exp(-exposure * (1 - b.scatter)), brush: b))
        }
        if b.scatter > 0 {
            // A per-knot stream permits random access and regeneration of any
            // stroke chunk. Rejection sampling avoids angular/trig randomness.
            let knotSeed = bits(seed: seed, index: UInt64(index))
            var counter: UInt64 = 0
            func random() -> Double {
                defer { counter &+= 1 }
                return unit(seed: knotSeed, index: counter)
            }
            for _ in 0..<12 {
                var p: Point
                repeat { p = Point(random() * 2 - 1, random() * 2 - 1) }
                while p.x * p.x + p.y * p.y > 1
                let r = 0.025 + random() * 0.055
                // Keep deposition approximately stable as droplet size varies.
                let alpha = 1 - exp(-exposure * b.scatter / (12 * r * r))
                dabs.append(Dab(point: sample.point + p * (b.radius * (1 - r)),
                                radius: b.radius * r, opacity: alpha, brush: b))
            }
        }
        return dabs
    }
    public static func replay(_ stroke: Stroke) -> [Dab] {
        stroke.samples.enumerated().flatMap { dabs($0.element, seed: stroke.seed, index: $0.offset) }
    }
}

public struct Dab: Equatable {
    public let point: Point
    public let radius: Double
    public let opacity: Double
    public let brush: Brush
}

/// Converts timestamped input into a fixed 120 Hz deposition sequence. The
/// same piecewise-linear input produces the same knots regardless of event
/// batching. Frame rendering never advances this generator.
public struct StrokeSampler {
    private var previous: Sample
    private var previousTime: Double = 0
    private var nextIndex = 1
    public init(first: Sample) { previous = first }
    public mutating func append(_ sample: Sample, at time: Double) -> [Sample] {
        guard time.isFinite, time > previousTime else { return [] }
        if sample.brush.marker == true {
            let delta = sample.point - previous.point
            let distance = hypot(delta.x, delta.y)
            let spacing = max(1e-30, min(previous.brush.radius, sample.brush.radius) * 0.5)
            let count = min(4096, max(1, Int(min(4096, ceil(distance / spacing)))))
            let result = distance > 0 || sample.brush != previous.brush
                ? (1...count).map { Sample.interpolate(previous, sample, Double($0) / Double(count)) } : []
            previous = sample; previousTime = time
            return result
        }
        var result: [Sample] = []
        while Double(nextIndex) * Spray.step <= time + 1e-10 {
            let t = min(1, max(0, (Double(nextIndex) * Spray.step - previousTime) / (time - previousTime)))
            result.append(.interpolate(previous, sample, t))
            nextIndex += 1
        }
        previous = sample
        previousTime = time
        return result
    }
}
