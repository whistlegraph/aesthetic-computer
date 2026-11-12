import MetalKit
import AirbrushCore

/// Authoritative stroke history plus disposable, bounded viewport textures.
final class PaintTileCache {
    struct Metrics {
        var queryWork = 0, deposits = 0, fragmentArea = 0, passes = 0, quads = 0
        var liveDeposits = 0, partialTiles = 0
        var resident = 0, pending = false
    }
    static let gutter = 2
    static let textureSize = PaintTileKey.pixels + 2 * gutter
    static let maximumTiles = 192
    static let workBudget = 8192
    static let depositBudget = 2048
    static let fragmentBudget = 4 * 1024 * 1024
    static let passBudget = 12

    private final class Layer {
        let texture: MTLTexture
        var initialized = false, applied: Int
        var query: SpatialDabs.Cursor?
        var pending: [Dab] = []
        var offset = 0
        init(_ texture: MTLTexture, after: Int = 0) { self.texture = texture; applied = after }
        func ready(_ target: Int) -> Bool { initialized && applied >= target }
    }
    private final class Tile {
        let key: PaintTileKey
        var base: Layer, ink: Layer?, merge: Layer?
        var cut: Int?
        var required: Int, used: UInt64 = 0
        var current: Bool { cut == nil && base.ready(required) }
        init(key: PaintTileKey, texture: MTLTexture, required: Int) {
            self.key = key; base = Layer(texture); self.required = required
        }
        var bounds: PaintRect { key.rect.expanded(Double(PaintTileCache.gutter + 1) / key.scale) }
    }
    private let painter: PaintRenderer
    private(set) var store = SpatialDabs()
    private var tiles: [PaintTileKey: Tile] = [:]
    private var serial: UInt64 = 0
    private var roundRobin = 0
    private var liveStart: Int?
    private var liveBounds: PaintRect?
    private var fallback: MTLTexture?
    private var fallbackCamera = Camera()
    private var fallbackSize = Point()
    private(set) var needsRefinement = false
    private(set) var metrics = Metrics()
    init(painter: PaintRenderer) { self.painter = painter }

    private func split(_ tile: Tile, at start: Int) {
        guard tile.cut == nil, start > 0,
              let tex = texture(width: Self.textureSize, height: Self.textureSize) else { return }
        tile.cut = start
        tile.ink = Layer(tex, after: start)
    }
    func append(_ dabs: [Dab], live: Bool = false) {
        guard !dabs.isEmpty else { return }
        let start = store.count
        store.append(contentsOf: dabs)
        var minX = Double.infinity, minY = Double.infinity
        var maxX = -Double.infinity, maxY = -Double.infinity
        for d in dabs {
            minX = min(minX, d.point.x - d.radius); minY = min(minY, d.point.y - d.radius)
            maxX = max(maxX, d.point.x + d.radius); maxY = max(maxY, d.point.y + d.radius)
        }
        let dirty = PaintRect(minX, minY, maxX - minX, maxY - minY)
        for tile in tiles.values where tile.bounds.intersects(dirty) {
            // Warm tiles append directly. A cold tile gets a separate live
            // suffix, so an old replay cannot hold fresh ink behind it.
            if live && !tile.current { split(tile, at: start) }
            tile.required = store.count
        }
        if live {
            liveStart = min(liveStart ?? start, start)
            liveBounds = liveBounds.map { $0.union(dirty) } ?? dirty
        }
        needsRefinement = true
    }
    func replace(_ dabs: [Dab]) {
        tiles.removeAll(); fallback = nil; liveStart = nil; liveBounds = nil
        store = SpatialDabs(); append(dabs); needsRefinement = true
    }
    func truncate(to count: Int) {
        store.truncate(to: count)
        tiles.removeAll(); fallback = nil; liveStart = nil; liveBounds = nil; needsRefinement = true
    }
    private func texture(width: Int, height: Int) -> MTLTexture? {
        let descriptor = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .bgra8Unorm,
            width: width, height: height, mipmapped: false)
        descriptor.storageMode = .private
        descriptor.usage = [.renderTarget, .shaderRead]
        return painter.device.makeTexture(descriptor: descriptor)
    }

    /// Limits are cumulative frame ceilings. Live ink uses at most half the
    /// frame, reserving progress for historical refinement and exact merging.
    private func advance(_ layer: Layer, tile: Tile, target: Int, transparent: Bool,
                         live: Bool = false, command: MTLCommandBuffer) {
        guard !layer.ready(target) else { return }
        let divisor = live ? 2 : 1
        let workLimit = Self.workBudget / divisor, depositLimit = Self.depositBudget / divisor
        let pixelLimit = Self.fragmentBudget / divisor, passLimit = Self.passBudget / divisor
        guard metrics.queryWork < workLimit, metrics.deposits < depositLimit,
              metrics.fragmentArea + Self.textureSize * Self.textureSize <= pixelLimit,
              metrics.passes < passLimit else { return }
        if layer.query == nil { layer.query = store.query(tile.bounds, after: layer.applied, before: target) }
        var batch: [Dab] = []
        while metrics.queryWork < workLimit && metrics.deposits < depositLimit {
            if layer.offset == layer.pending.count {
                var available = workLimit - metrics.queryWork
                let initial = available
                layer.pending = layer.query!.take(work: &available, maximum: min(256, depositLimit - metrics.deposits))
                metrics.queryWork += initial - available
                layer.offset = 0
                if layer.pending.isEmpty { break }
            }
            let dab = layer.pending[layer.offset]
            let diameter = min(Double(Self.textureSize), dab.radius * tile.key.scale * 2 + 2)
            let cost = max(1, Int(ceil(diameter * diameter)))
            if metrics.fragmentArea + cost > pixelLimit { break }
            batch.append(dab); layer.offset += 1
            metrics.deposits += 1; metrics.fragmentArea += cost
            if live { metrics.liveDeposits += 1 }
        }
        if !batch.isEmpty || !layer.initialized {
            let pass = MTLRenderPassDescriptor()
            pass.colorAttachments[0].texture = layer.texture
            var camera = Camera()
            camera.center = Point(tile.key.rect.minX + tile.key.span / 2, tile.key.rect.minY + tile.key.span / 2)
            camera.zoom = tile.key.scale
            let dimension = Double(Self.textureSize)
            painter.encode(dabs: batch, camera: camera, size: Point(dimension, dimension),
                pixels: CGSize(width: dimension, height: dimension), pass: pass,
                command: command, clear: !layer.initialized, transparent: transparent)
            layer.initialized = true; metrics.passes += 1
        }
        if layer.query!.done && layer.offset == layer.pending.count {
            layer.applied = layer.query!.target; layer.query = nil
            layer.pending.removeAll(keepingCapacity: true); layer.offset = 0
        }
    }
    private func copy(_ from: MTLTexture, _ to: MTLTexture, command: MTLCommandBuffer) {
        guard let blit = command.makeBlitCommandEncoder() else { return }
        blit.copy(from: from, sourceSlice: 0, sourceLevel: 0, sourceOrigin: MTLOrigin(),
            sourceSize: MTLSize(width: from.width, height: from.height, depth: 1),
            to: to, destinationSlice: 0, destinationLevel: 0, destinationOrigin: MTLOrigin())
        blit.endEncoding()
    }
    func encode(camera: Camera, size: Point, pixels: CGSize, priority: Point? = nil,
                pass: MTLRenderPassDescriptor, command: MTLCommandBuffer) {
        metrics = Metrics(); serial &+= 1
        let scale = Double(pixels.width) / max(1, size.x)
        let keys = PaintTileKey.visible(camera: camera, size: size, backingScale: scale)
        let wanted = Set(keys)
        for key in keys {
            if tiles[key] == nil {
                if tiles.count >= Self.maximumTiles,
                   let victim = tiles.values.filter({ !wanted.contains($0.key) }).min(by: { $0.used < $1.used }) {
                    tiles.removeValue(forKey: victim.key)
                }
                if tiles.count < Self.maximumTiles, let tex = texture(width: Self.textureSize, height: Self.textureSize) {
                    let tile = Tile(key: key, texture: tex, required: store.count)
                    if let start = liveStart, let dirty = liveBounds, tile.bounds.intersects(dirty) { split(tile, at: start) }
                    tiles[key] = tile
                }
            }
            tiles[key]?.used = serial
        }
        liveStart = nil; liveBounds = nil
        let focus = priority ?? size * 0.5
        let work = keys.compactMap { tiles[$0] }.filter { !$0.current }.sorted {
            func distance(_ tile: Tile) -> Double {
                let p = camera.screen(Point(tile.key.rect.minX + tile.key.span / 2,
                    tile.key.rect.minY + tile.key.span / 2), size: size) - focus
                return p.x * p.x + p.y * p.y
            }
            return distance($0) < distance($1)
        }
        // Fresh suffix first, independently of the old history cursor.
        for tile in work {
            if let ink = tile.ink { advance(ink, tile: tile, target: tile.required, transparent: true, live: true, command: command) }
        }
        let warm = work.filter { $0.cut == nil && $0.base.initialized && $0.base.applied > 0 }
        let cold = work.filter { !($0.cut == nil && $0.base.initialized && $0.base.applied > 0) }
        var ordered = warm
        if !cold.isEmpty {
            let start = roundRobin % cold.count
            ordered += Array(cold[start...]) + Array(cold[..<start]); roundRobin += 1
        }
        for tile in ordered {
            advance(tile.base, tile: tile, target: tile.cut ?? tile.required, transparent: false, command: command)
            if let cut = tile.cut, tile.base.ready(cut) {
                // Replay the suffix onto a copy of the exact prefix. Flattening
                // the transparent preview would change BGRA8 rounding/order.
                if tile.merge == nil, metrics.passes < Self.passBudget,
                   metrics.fragmentArea + Self.textureSize * Self.textureSize <= Self.fragmentBudget,
                   let tex = texture(width: Self.textureSize, height: Self.textureSize) {
                    copy(tile.base.texture, tex, command: command)
                    tile.merge = Layer(tex, after: cut); tile.merge!.initialized = true
                    metrics.passes += 1; metrics.fragmentArea += Self.textureSize * Self.textureSize
                }
                if let merge = tile.merge {
                    advance(merge, tile: tile, target: tile.required, transparent: false, command: command)
                    if merge.ready(tile.required) { tile.base = merge; tile.merge = nil; tile.ink = nil; tile.cut = nil }
                }
            }
        }
        var quads: [PaintRenderer.TileQuad] = []
        if let fallback {
            let origin = camera.screen(fallbackCamera.world(Point(), size: fallbackSize), size: size) * scale
            let dimensions = fallbackSize * (camera.zoom / fallbackCamera.zoom * scale)
            quads.append(.init(texture: fallback, rect: SIMD4(Float(origin.x), Float(origin.y), Float(dimensions.x), Float(dimensions.y))))
        }
        var complete = true
        for key in keys {
            let tile = tiles[key]
            var source = tile
            if tile?.current != true { complete = false }
            // Partial tiles are visible immediately; only uninitialized tiles
            // need a ready ancestor or the last completed viewport.
            if source?.base.initialized != true {
                var ancestor = key
                for _ in 0..<4 {
                    ancestor = ancestor.parent
                    if let cached = tiles[ancestor], cached.current { source = cached; cached.used = serial; break }
                }
            }
            let origin = camera.screen(Point(key.rect.minX, key.rect.minY), size: size) * scale
            let extent = key.span * camera.zoom * scale
            let rect = SIMD4(Float(origin.x), Float(origin.y), Float(extent), Float(extent))
            let dimension = Double(Self.textureSize)
            func quad(_ layer: Layer, sourceKey: PaintTileKey) -> PaintRenderer.TileQuad {
                let uvX = (Double(Self.gutter) + (key.rect.minX - sourceKey.rect.minX) * sourceKey.scale) / dimension
                let uvY = (Double(Self.gutter) + (key.rect.minY - sourceKey.rect.minY) * sourceKey.scale) / dimension
                let uvSize = key.span * sourceKey.scale / dimension
                return .init(texture: layer.texture, rect: rect, uv: SIMD4(Float(uvX), Float(uvY), Float(uvSize), Float(uvSize)))
            }
            if let source, source.base.initialized {
                quads.append(quad(source.base, sourceKey: source.key))
                if !source.current { metrics.partialTiles += 1 }
            }
            if let ink = tile?.ink, ink.initialized { quads.append(quad(ink, sourceKey: key)) }
        }
        painter.composite(quads, pixels: pixels, pass: pass, command: command)
        if complete, let output = pass.colorAttachments[0].texture {
            if fallback?.width != output.width || fallback?.height != output.height { fallback = texture(width: output.width, height: output.height) }
            if let fallback {
                copy(output, fallback, command: command)
                fallbackCamera = camera; fallbackSize = size
            }
        }
        needsRefinement = !complete
        metrics.quads = quads.count; metrics.resident = tiles.count; metrics.pending = !complete
    }
}
