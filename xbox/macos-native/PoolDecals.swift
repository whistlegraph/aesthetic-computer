import JavaScriptCore
import MetalKit

/// One logical 2048² pool texture. Three GPU snapshots follow the renderer's
/// in-flight slots; each receives only the dirty bounds accumulated since use.
final class PoolDecals {
    private let surface = ac_pool_create()!
    private weak var scene: MetalSceneView?
    private var textures: [MTLTexture] = []
    private var pending = [CGRect?](repeating: nil, count: 3)

    init(scene: MetalSceneView) {
        self.scene = scene
        let descriptor = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .rgba8Unorm,
            width: 2048, height: 2048, mipmapped: false)
        descriptor.storageMode = scene.device?.hasUnifiedMemory == false ? .managed : .shared
        descriptor.usage = .shaderRead
        if let device = scene.device {
            for _ in 0..<3 { if let texture = device.makeTexture(descriptor: descriptor) { textures.append(texture) } }
        }
        scene.poolDecalTexture = { [weak self] slot in self?.texture(slot) }
    }
    deinit { ac_pool_destroy(surface) }

    func clear() -> Bool {
        guard textures.count == 3 else { return false }
        ac_pool_clear(surface)
        return true
    }
    func stamp(_ values: [Float]) -> Bool {
        guard values.count == 12 || values.count == 15 else { return false }
        return values.withUnsafeBufferPointer { values.count == 15 ? ac_pool_tint(surface, $0.baseAddress!) : ac_pool_stamp(surface, $0.baseAddress!) }
    }
    func upload(_ vertices: [Float], _ faces: [Float]) -> Int {
        guard !vertices.isEmpty, !faces.isEmpty else { return -1 }
        return vertices.withUnsafeBufferPointer { v in faces.withUnsafeBufferPointer { f in
            ac_pool_mesh(surface, v.baseAddress!, Int32(v.count), f.baseAddress!, Int32(f.count)) ? 0 : -1
        } }
    }
    func draw(handle: Int, camera: [Float], bounds: [Float]) -> Int {
        guard handle == 0, camera.count == 27, bounds.count == 4, let scene else { return 0 }
        var count: Int32 = 0
        return camera.withUnsafeBufferPointer { m in bounds.withUnsafeBufferPointer { b in
            guard let values = ac_pool_draw(surface, m.baseAddress!, b.baseAddress!, &count), count > 0 else { return 0 }
            scene.setPoolDecals(values, vertexCount: Int(count))
            return Int(count) / 3
        } }
    }
    private func texture(_ slot: Int) -> MTLTexture? {
        guard textures.count == 3, textures.indices.contains(slot) else { return nil }
        var bounds = [Int32](repeating: 0, count: 4)
        if ac_pool_pixels(surface, &bounds) != nil {
            let dirty = CGRect(x: Int(bounds[0]), y: Int(bounds[1]),
                width: Int(bounds[2] - bounds[0]), height: Int(bounds[3] - bounds[1]))
            for i in pending.indices { pending[i] = pending[i]?.union(dirty) ?? dirty }
            ac_pool_clean(surface)
        }
        if let dirty = pending[slot], !dirty.isEmpty, let pixels = ac_pool_all_pixels(surface) {
            let x = Int(dirty.minX), y = Int(dirty.minY)
            textures[slot].replace(region: MTLRegionMake2D(x, y, Int(dirty.width), Int(dirty.height)),
                mipmapLevel: 0, withBytes: pixels + (y * 2048 + x) * 4, bytesPerRow: 2048 * 4)
            pending[slot] = nil
        }
        return textures[slot]
    }

    static func floats(_ value: JSValue, limit: Int) -> [Float] {
        let count = Int(value.forProperty("length")?.toInt32() ?? 0)
        guard count > 0, count <= limit else { return [] }
        return (0..<count).map { Float(value.atIndex($0).toDouble()) }
    }
}
