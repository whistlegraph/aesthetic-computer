import Foundation

public struct Point: Codable, Sendable { public var x: Int; public var y: Int; public init(x: Int, y: Int) { self.x = x; self.y = y } }
public struct Operation: Codable, Sendable {
    public var op: String
    public var points: [Point]?
    public var steps: Int?
    public var amount: Int?
    public var color: [Int]?
    public var radius: Int?
    public var durationTicks: Int?
    public var jitter: Int?
    public var drift: Point?
}
public struct Brush: Codable, Sendable {
    public var schema: String
    public var version: Int
    public var id: String
    public var operations: [Operation]
}
// A recorded input stream, independent of brush implementation or random seed.
public struct GestureSample: Codable, Sendable {
    public var x: Int; public var y: Int; public var tick: Int
    public init(x: Int, y: Int, tick: Int) { self.x = x; self.y = y; self.tick = tick }
}
public struct Invocation: Codable, Sendable {
    public var brush: Brush
    public var seed: Int
    public var tick: Int
    public var gesture: [GestureSample]?
    public init(brush: Brush, seed: Int, tick: Int, gesture: [GestureSample]? = nil) {
        self.brush = brush; self.seed = seed; self.tick = tick; self.gesture = gesture
    }
}
public struct Painting: Codable, Sendable {
    public var schema: String
    public var version: Int
    public var width: Int
    public var height: Int
    public var base: [Int]
    public var steps: [Invocation]
    public var cursor: Int
    public init(width: Int, height: Int, base: [Int]) {
        schema = "ac-painting-prototype"; version = 1
        self.width = width; self.height = height; self.base = base
        steps = []; cursor = 0
    }
}
public struct RenderRequest: Codable, Sendable {
    public var document: Painting
    public var preview: Invocation?
}
public struct RenderResult: Codable, Sendable {
    public var document: Painting
    public var pixels: [UInt8]
    public init(document: Painting, pixels: [UInt8]) {
        self.document = document; self.pixels = pixels
    }
}
public enum BrushError: Error { case invalid(String) }

private func check(_ condition: Bool, _ message: String) throws {
    if !condition { throw BrushError.invalid(message) }
}
private func bounded(_ n: Int, _ low: Int, _ high: Int) throws {
    try check(n >= low && n <= high, "Integer outside bounds")
}
private func wrapped(_ n: Int, _ size: Int) -> Int { ((n % size) + size) % size }
private func roundDivide(_ n: Int, _ d: Int) -> Int { (n + d / 2) / d }

public func validateBrush(_ invocation: Invocation) throws {
    try bounded(invocation.seed, 0, 0xffff_ffff)
    try bounded(invocation.tick, 0, 3600)
    if let gesture = invocation.gesture {
        try check(!gesture.isEmpty && gesture.count <= 128, "Invalid gesture")
        var previous = -1
        for sample in gesture {
            try bounded(sample.x, 0, 255); try bounded(sample.y, 0, 255)
            try bounded(sample.tick, 0, 3600)
            try check(sample.tick > previous, "Gesture times must increase")
            previous = sample.tick
        }
    }
    let brush = invocation.brush
    try check(brush.schema == "ac-brush" && brush.version == 1 &&
        !brush.id.isEmpty && brush.id.utf16.count <= 128 &&
        !brush.operations.isEmpty && brush.operations.count <= 32, "Unsupported brush")
    for op in brush.operations {
        if op.op == "invert" {
            if let amount = op.amount { try bounded(amount, 0, 255) }
            if let duration = op.durationTicks { try bounded(duration, 1, 3600) }
            continue
        }
        if op.op == "stroke" {
            guard invocation.gesture != nil, let color = op.color, let radius = op.radius else {
                throw BrushError.invalid("Stroke requires gesture, color and radius")
            }
            try check(color.count == 4, "Invalid RGBA")
            for c in color { try bounded(c, 0, 255) }
            try bounded(radius, 0, 16)
            continue
        }
        guard ["path", "walk"].contains(op.op), let color = op.color,
            let radius = op.radius, let duration = op.durationTicks,
            let jitter = op.jitter, let drift = op.drift else {
            throw BrushError.invalid("Unsupported or incomplete operation")
        }
        if op.op == "walk" {
            guard let steps = op.steps else { throw BrushError.invalid("Missing walk length") }
            try bounded(steps, 2, 128)
        } else {
            guard let points = op.points else { throw BrushError.invalid("Missing path") }
            try check(!points.isEmpty && points.count <= 128, "Invalid path")
            for point in points { try bounded(point.x, 0, 255); try bounded(point.y, 0, 255) }
        }
        try check(color.count == 4, "Invalid RGBA")
        for c in color { try bounded(c, 0, 255) }
        try bounded(radius, 0, 16); try bounded(duration, 1, 3600)
        try bounded(jitter, 0, 16); try bounded(drift.x, -8, 8); try bounded(drift.y, -8, 8)
    }
}

public func renderBrush(_ base: [UInt8], width: Int, height: Int, invocation: Invocation) throws -> [UInt8] {
    try bounded(width, 1, 256); try bounded(height, 1, 256)
    try check(base.count == width * height * 4, "Invalid canvas")
    try validateBrush(invocation)
    var pixels = base
    var seed = UInt32(invocation.seed)
    var budget = 2_000_000
    func spend() throws {
        budget -= 1
        try check(budget >= 0, "Brush work limit exceeded")
    }
    func random(_ range: Int) -> Int {
        seed = seed &* 1664525 &+ 1013904223
        return Int(seed) % range
    }
    func stamp(_ x: Int, _ y: Int, _ radius: Int, _ color: [Int]) throws {
        for dy in -radius...radius { for dx in -radius...radius {
            try spend()
            let px = x + dx, py = y + dy
            if dx * dx + dy * dy > radius * radius || px < 0 || py < 0 || px >= width || py >= height { continue }
            let i = (py * width + px) * 4
            let sa = color[3], da = Int(pixels[i + 3])
            let alpha = sa * 255 + da * (255 - sa)
            if alpha == 0 { continue }
            for c in 0..<3 {
                pixels[i + c] = UInt8(roundDivide(color[c] * sa * 255 + Int(pixels[i + c]) * da * (255 - sa), alpha))
            }
            pixels[i + 3] = UInt8(roundDivide(alpha, 255))
        }}
    }
    for op in invocation.brush.operations {
        if op.op == "invert" {
            let duration = op.durationTicks ?? 1
            let amount = op.durationTicks == nil ? (op.amount ?? 255) : roundDivide((op.amount ?? 255) * min(invocation.tick, duration), duration)
            for i in stride(from: 0, to: pixels.count, by: 4) {
                try spend()
                for c in 0..<3 { let original = Int(pixels[i + c]); pixels[i + c] = UInt8(roundDivide(original * (255-amount) + (255-original) * amount, 255)) }
            }
            continue
        }
        if op.op == "stroke" {
            let samples = invocation.gesture!, first = samples[0]
            guard invocation.tick >= first.tick else { continue }
            try stamp(first.x, first.y, op.radius!, op.color!)
            for index in 1..<samples.count {
                let previous = samples[index - 1], target = samples[index]
                if invocation.tick <= previous.tick { break }
                var x = previous.x, y = previous.y
                let dx = abs(target.x - x), dy = -abs(target.y - y)
                let sx = x < target.x ? 1 : -1, sy = y < target.y ? 1 : -1
                var error = dx + dy
                let elapsed = min(invocation.tick, target.tick) - previous.tick
                let length = max(dx, -dy) * elapsed / (target.tick - previous.tick)
                for _ in 0..<length {
                    let e2 = 2 * error
                    if e2 >= dy { error += dy; x += sx }
                    if e2 <= dx { error += dx; y += sy }
                    try stamp(x, y, op.radius!, op.color!)
                }
                if invocation.tick < target.tick { break }
            }
            continue
        }
        // All required fields are checked before rendering any operation.
        let radius = op.radius!, color = op.color!, drift = op.drift ?? Point(x: 0, y: 0), jitter = op.jitter ?? 0
        var sourcePoints = op.points ?? []
        if op.op == "walk" {
            var x = random(width), y = random(height), vx = random(7) - 3, vy = random(7) - 3
            sourcePoints = [Point(x: x, y: y)]
            for _ in 1..<op.steps! {
                vx = max(-3, min(3, vx + random(3) - 1))
                vy = max(-3, min(3, vy + random(3) - 1))
                if vx == 0 && vy == 0 { vx = 1 }
                if x + vx < 0 || x + vx >= width { vx = -vx }
                if y + vy < 0 || y + vy >= height { vy = -vy }
                x = max(0, min(width - 1, x + vx)); y = max(0, min(height - 1, y + vy))
                sourcePoints.append(Point(x: x, y: y))
            }
        }
        let points = sourcePoints.map { p in
            Point(x: wrapped(p.x + invocation.tick * drift.x + random(2 * jitter + 1) - jitter, width),
                  y: wrapped(p.y + invocation.tick * drift.y + random(2 * jitter + 1) - jitter, height))
        }
        let duration = op.durationTicks!
        let progress = (points.count - 1) * min(invocation.tick, duration)
        let complete = progress / duration, remainder = progress % duration
        let count = 1 + complete + (remainder > 0 ? 1 : 0)
        try stamp(points[0].x, points[0].y, radius, color)
        for i in 1..<count {
            var x = points[i - 1].x, y = points[i - 1].y
            let target = points[i]
            let end = i > complete ? Point(
                x: roundDivide(x * (duration - remainder) + target.x * remainder, duration),
                y: roundDivide(y * (duration - remainder) + target.y * remainder, duration)) : target
            let dx = abs(end.x - x), dy = -abs(end.y - y)
            let sx = x < end.x ? 1 : -1, sy = y < end.y ? 1 : -1
            var error = dx + dy
            while x != end.x || y != end.y {
                let e2 = 2 * error
                if e2 >= dy { error += dy; x += sx }
                if e2 <= dx { error += dx; y += sy }
                try stamp(x, y, radius, color)
            }
        }
    }
    return pixels
}

public func renderDocument(_ document: Painting, preview: Invocation? = nil) throws -> [UInt8] {
    try check(document.schema == "ac-painting-prototype" && document.version == 1, "Unsupported document")
    try bounded(document.width, 1, 256); try bounded(document.height, 1, 256)
    try check(document.base.count == document.width * document.height * 4, "Invalid canvas")
    for b in document.base { try bounded(b, 0, 255) }
    try check(document.steps.count <= 64, "Invalid history")
    try bounded(document.cursor, 0, document.steps.count)
    for step in document.steps { try validateBrush(step) }
    var pixels = document.base.map { UInt8($0) }
    for step in document.steps.prefix(document.cursor) {
        pixels = try renderBrush(pixels, width: document.width, height: document.height, invocation: step)
    }
    if let preview {
        pixels = try renderBrush(pixels, width: document.width, height: document.height, invocation: preview)
    }
    return pixels
}
