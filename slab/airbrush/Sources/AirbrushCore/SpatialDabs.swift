import Foundation

public struct PaintRect: Equatable {
    public var minX: Double, minY: Double, maxX: Double, maxY: Double
    public init(_ x: Double, _ y: Double, _ width: Double, _ height: Double) {
        minX = x; minY = y; maxX = x + width; maxY = y + height
    }
    public func intersects(_ b: PaintRect) -> Bool {
        maxX >= b.minX && minX <= b.maxX && maxY >= b.minY && minY <= b.maxY
    }
    public func expanded(_ amount: Double) -> PaintRect {
        PaintRect(minX - amount, minY - amount, maxX - minX + 2 * amount, maxY - minY + 2 * amount)
    }
    public func union(_ b: PaintRect) -> PaintRect {
        let x = min(minX, b.minX), y = min(minY, b.minY)
        return PaintRect(x, y, max(maxX, b.maxX) - x, max(maxY, b.maxY) - y)
    }
}

/// A sparse raster pyramid: a level doubles pixels per world unit, while
/// every resident tile stays 256 pixels. Negative coordinates use floor.
public struct PaintTileKey: Hashable {
    public static let pixels = 256
    public let level: Int, x: Int, y: Int
    public init(level: Int, x: Int, y: Int) { self.level = level; self.x = x; self.y = y }
    public var scale: Double { pow(2, Double(level)) }
    public var span: Double { Double(Self.pixels) / scale }
    public var rect: PaintRect { PaintRect(Double(x) * span, Double(y) * span, span, span) }
    public var parent: PaintTileKey {
        PaintTileKey(level: level - 1, x: Int(floor(Double(x) / 2)), y: Int(floor(Double(y) / 2)))
    }
    public static func visible(camera: Camera, size: Point, backingScale: Double) -> [PaintTileKey] {
        var level = max(-60, min(60, Int(floor(log2(max(1e-30, camera.zoom * backingScale))))))
        var span = Double(pixels) / pow(2, Double(level))
        let a = camera.world(Point(), size: size), b = camera.world(size, size: size)
        // Keep both allocation and integer addressing bounded on huge displays
        // or extreme world coordinates; prefer a coarser preview to overflow.
        while (ceil((b.x - a.x) / span) + 2) * (ceil((b.y - a.y) / span) + 2) > 128
            || max(abs(a.x), abs(a.y), abs(b.x), abs(b.y)) / span > Double(Int.max / 4) {
            level -= 1; span *= 2
        }
        let x0 = Int(floor(a.x / span)), x1 = Int(floor(b.x / span))
        let y0 = Int(floor(a.y / span)), y1 = Int(floor(b.y / span))
        return (y0...y1).flatMap { y in (x0...x1).map { PaintTileKey(level: level, x: $0, y: y) } }
    }
}

/// Append-only, chronological bounding-volume tree. Branches summarize ranges
/// of the stroke history, so a spatial query can skip distant history without
/// sorting or losing alpha compositing order. A resumable cursor budgets both
/// tree traversal and individual dab tests, including entirely invisible work.
public final class SpatialDabs {
    fileprivate final class Node {
        let start: Int, capacity: Int
        var bounds: PaintRect?
        var left: Node?, right: Node?
        init(start: Int, capacity: Int) { self.start = start; self.capacity = capacity }
        func insert(index: Int, bounds b: PaintRect) {
            bounds = bounds.map { $0.union(b) } ?? b
            guard capacity > 128 else { return }
            let half = capacity / 2
            if index < start + half {
                if left == nil { left = Node(start: start, capacity: half) }
                left!.insert(index: index, bounds: b)
            } else {
                if right == nil { right = Node(start: start + half, capacity: half) }
                right!.insert(index: index, bounds: b)
            }
        }
    }
    fileprivate var root = Node(start: 0, capacity: 128)
    public private(set) var dabs: [Dab] = []
    public var count: Int { dabs.count }
    public init() {}
    public func truncate(to count: Int) {
        precondition(count >= 0 && count <= dabs.count)
        dabs.removeLast(dabs.count - count)
        // Bounds may conservatively include removed marks; cursor targets
        // exclude their slots. No full-tree rebuild is needed for undo.
    }
    public func append(contentsOf newDabs: [Dab]) {
        for dab in newDabs {
            let index = dabs.count
            if index == root.capacity {
                let parent = Node(start: 0, capacity: root.capacity * 2)
                parent.left = root; parent.bounds = root.bounds; root = parent
            }
            root.insert(index: index, bounds: Self.bounds(dab))
            dabs.append(dab)
        }
    }
    fileprivate static func bounds(_ dab: Dab) -> PaintRect {
        PaintRect(dab.point.x - dab.radius, dab.point.y - dab.radius, dab.radius * 2, dab.radius * 2)
    }
    public func query(_ rect: PaintRect, after: Int = 0, before: Int? = nil) -> Cursor {
        Cursor(store: self, rect: rect, after: after, before: before)
    }
    public final class Cursor {
        private let store: SpatialDabs
        private let rect: PaintRect
        private let after: Int
        public let target: Int
        private var stack: [Node]
        private var next = 0, end = 0
        public var done: Bool { stack.isEmpty && next == end }
        fileprivate init(store: SpatialDabs, rect: PaintRect, after: Int, before: Int?) {
            self.store = store; self.rect = rect; self.after = after
            target = min(store.count, before ?? store.count)
            stack = after < target ? [store.root] : []
        }
        public func take(work: inout Int, maximum: Int = 1024) -> [Dab] {
            var result: [Dab] = []
            while work > 0 && result.count < maximum {
                if next < end {
                    let dab = store.dabs[next]
                    next += 1; work -= 1
                    if dab.opacity > 0 && Self.overlaps(dab, rect) { result.append(dab) }
                } else if let node = stack.popLast() {
                    work -= 1
                    guard node.start < target, node.start + node.capacity > after,
                          let bounds = node.bounds, bounds.intersects(rect) else { continue }
                    if node.capacity <= 128 {
                        next = max(after, node.start); end = min(target, node.start + node.capacity)
                    } else {
                        if let right = node.right { stack.append(right) }
                        if let left = node.left { stack.append(left) }
                    }
                } else { break }
            }
            return result
        }
        private static func overlaps(_ dab: Dab, _ rect: PaintRect) -> Bool {
            SpatialDabs.bounds(dab).intersects(rect)
        }
    }
}
