#if SCENE4
import JavaScriptCore
import MetalKit

/// sceneApi 4 on the Mac (xbox/native-bios/SCENE-API-4.md): the Xbox's GPU
/// meshes in Metal, so a big-map piece can be previewed on a desk.
/// Validation, culling and light masks are the shared scene4.hpp, reached
/// through Scene4Bridge; this file owns the Metal objects and the JavaScript
/// bindings. It draws inside MetalSceneView's world pass, through a
/// `worldPass` hook the shared renderer must offer. Without the hook nothing
/// installs, and a piece finds no gpu* functions and takes its older path.
final class Scene4 {
    private struct Mesh {
        let vertices: MTLBuffer, skin: MTLBuffer?, indices: MTLBuffer
        let indexType: MTLIndexType, indexCount: Int
        let bounds: [Float], bounded: Bool, maxJoint: Int, bytes: Int
    }
    private struct Texture { let texture: MTLTexture; let flags: Int; let bytes: Int }
    private struct Material {
        var color = SIMD4<Float>(1, 1, 1, 1), emissive = SIMD3<Float>(0, 0, 0)
        var lightmapScale: Float = 1, alphaCutoff: Float = 0
        var texture = 0, lightmap = 0, cull = 0
        var unlit = false, fog = true, depthWrite = true
    }
    private struct Queued {
        let mesh: Mesh, texture: Texture?, lightmap: Texture?, material: Material
        let camera: Int, firstInstance: Int, instanceCount: Int, firstBone: Int
        let box: [Float]
    }
    private struct DrawUniforms { var tint: SIMD4<Float>; var emissive: SIMD4<Float>; var info: SIMD4<UInt32>; var extra: SIMD4<Float> }

    // The same caps as scene4.hpp; the bridge enforces the per-mesh ones.
    static let maxMeshBytes = 192 << 20, maxTextureBytes = 128 << 20, maxTextureSide = 2048
    static let maxMeshes = 4096, maxTextures = 1024, maxMaterials = 4096
    static let maxBones = 64, maxLights = 32, maxDraws = 4096, maxInstances = 65536, maxFrameBones = 16384
    private static let minDepth: Float = -1.455, maxDepth: Float = 1.4

    private let device: MTLDevice
    private let uploads: MTLCommandQueue
    private let pipeline: MTLRenderPipelineState, skinPipeline: MTLRenderPipelineState
    private let depthWrite: MTLDepthStencilState, depthRead: MTLDepthStencilState
    private let samplers: [MTLSamplerState]
    private let white: MTLTexture
    private let stage: () -> CGSize

    private var meshes: [Int: Mesh] = [:], textures: [Int: Texture] = [:]
    private var materials: [Int: Material] = [:]
    private var meshBytes = 0, textureBytes = 0
    private var environment: [Float] = [0.72, 0.72, 0.72, 0.28, 0.28, 0.28, 0, 0, 0, 0, 0, 1, 0]
    private var lights: [Float] = []
    private var queue: [Queued] = [], instances: [Float] = [], bones: [Float] = [], cameras: [[Float]] = []
    private var culled = 0, triangles = 0, dropped = 0
    private var last = (draws: 0, culled: 0, triangles: 0, dropped: 0)
    // Per-frame instance and bone data rides a ring the GPU may still be reading.
    private var ring: [(instances: MTLBuffer?, bones: MTLBuffer?)] = Array(repeating: (nil, nil), count: 4)
    private var ringIndex = 0

    /// nil when the renderer has no world-pass hook or Metal refuses the pipeline.
    static func attach(to scene: MetalSceneView, stage: @escaping () -> CGSize) -> Scene4? {
        guard scene.responds(to: NSSelectorFromString("setWorldPass:")) else {
            NSLog("oskiewar scene4 unavailable: MetalSceneView has no worldPass hook")
            return nil
        }
        guard let device = scene.device, let renderer = try? Scene4(device: device, stage: stage) else {
            NSLog("oskiewar scene4 unavailable: pipeline")
            return nil
        }
        let hook: @convention(block) (MTLRenderCommandEncoder, CGSize) -> Void = { [weak renderer] encoder, size in
            renderer?.encode(encoder, drawable: size)
        }
        scene.setValue(unsafeBitCast(hook, to: AnyObject.self), forKey: "worldPass")
        return renderer
    }

    private init(device: MTLDevice, stage: @escaping () -> CGSize) throws {
        self.device = device
        self.stage = stage
        guard let queue = device.makeCommandQueue() else { throw Scene4Error.metal }
        uploads = queue
        let library = try device.makeLibrary(source: Self.shaders, options: nil)
        let descriptor = MTLRenderPipelineDescriptor()
        descriptor.colorAttachments[0].pixelFormat = .bgra8Unorm
        descriptor.depthAttachmentPixelFormat = .depth32Float
        descriptor.fragmentFunction = library.makeFunction(name: "mesh4_fragment")
        descriptor.vertexFunction = library.makeFunction(name: "mesh4_vertex")
        pipeline = try device.makeRenderPipelineState(descriptor: descriptor)
        descriptor.vertexFunction = library.makeFunction(name: "mesh4_skin_vertex")
        skinPipeline = try device.makeRenderPipelineState(descriptor: descriptor)
        let depth = MTLDepthStencilDescriptor()
        depth.depthCompareFunction = .lessEqual
        depth.isDepthWriteEnabled = true
        guard let write = device.makeDepthStencilState(descriptor: depth) else { throw Scene4Error.metal }
        depth.isDepthWriteEnabled = false
        guard let read = device.makeDepthStencilState(descriptor: depth) else { throw Scene4Error.metal }
        depthWrite = write; depthRead = read
        // [clamp][nearest], as the texture flags pick them; slot 2 also serves lightmaps.
        samplers = try (0..<4).map { i in
            let s = MTLSamplerDescriptor()
            let nearest = i & 1 != 0, clamp = i & 2 != 0
            s.minFilter = nearest ? .nearest : .linear
            s.magFilter = nearest ? .nearest : .linear
            s.mipFilter = nearest ? .nearest : .linear
            s.maxAnisotropy = nearest ? 1 : 4
            s.sAddressMode = clamp ? .clampToEdge : .repeat
            s.tAddressMode = clamp ? .clampToEdge : .repeat
            guard let state = device.makeSamplerState(descriptor: s) else { throw Scene4Error.metal }
            return state
        }
        let one = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .rgba8Unorm, width: 1, height: 1, mipmapped: false)
        one.storageMode = device.hasUnifiedMemory ? .shared : .managed
        guard let texture = device.makeTexture(descriptor: one) else { throw Scene4Error.metal }
        var pixel: UInt32 = 0xffffffff
        texture.replace(region: MTLRegionMake2D(0, 0, 1, 1), mipmapLevel: 0, withBytes: &pixel, bytesPerRow: 4)
        white = texture
    }
    private enum Scene4Error: Error { case metal }

    /// The shell begins a frame again on every wipe(), so only a frame that
    /// queued something becomes the one gpuStats() reports.
    func beginFrame() {
        if !queue.isEmpty || culled > 0 || dropped > 0 { last = (queue.count, culled, triangles, dropped) }
        queue.removeAll(keepingCapacity: true)
        instances.removeAll(keepingCapacity: true)
        bones.removeAll(keepingCapacity: true)
        cameras.removeAll(keepingCapacity: true)
        culled = 0; triangles = 0; dropped = 0
    }

    // MARK: drawing

    private func encode(_ encoder: MTLRenderCommandEncoder, drawable: CGSize) {
        guard !queue.isEmpty else { return }
        let stage = self.stage()
        guard stage.width > 0, stage.height > 0 else { return }
        ringIndex = (ringIndex + 1) % ring.count
        guard let instanceBuffer = reserve(&ring[ringIndex].instances, instances.count * 4),
              let boneBuffer = reserve(&ring[ringIndex].bones, max(1, bones.count) * 4) else { return }
        _ = instances.withUnsafeBytes { memcpy(instanceBuffer.contents(), $0.baseAddress!, $0.count) }
        if !bones.isEmpty { _ = bones.withUnsafeBytes { memcpy(boneBuffer.contents(), $0.baseAddress!, $0.count) } }
        var scene = sceneUniforms()
        encoder.setFragmentBytes(&scene, length: scene.count * 16, index: 0)
        encoder.setVertexBuffer(instanceBuffer, offset: 0, index: 2)
        encoder.setVertexBuffer(boneBuffer, offset: 0, index: 3)
        encoder.setFrontFacing(.counterClockwise)
        let scaleX = Double(drawable.width) / Double(stage.width), scaleY = Double(drawable.height) / Double(stage.height)
        var camera = -1, onScreen = true
        for q in queue {
            if q.camera != camera {
                camera = q.camera
                let m = cameras[camera]
                var view = viewUniforms(m, stage: stage)
                encoder.setVertexBytes(&view, length: view.count * 16, index: 4)
                encoder.setFragmentBytes(&view, length: view.count * 16, index: 2)
                let edge = { (v: Float, scale: Double, limit: CGFloat) in
                    Int(max(0, min(Double(limit), (Double(v) * scale).rounded())))
                }
                let left = edge(m[18], scaleX, drawable.width), top = edge(m[19], scaleY, drawable.height)
                let right = edge(m[20], scaleX, drawable.width), bottom = edge(m[21], scaleY, drawable.height)
                onScreen = right > left && bottom > top
                if onScreen { encoder.setScissorRect(MTLScissorRect(x: left, y: top, width: right - left, height: bottom - top)) }
            }
            guard onScreen else { continue }
            let skinned = q.mesh.skin != nil
            encoder.setRenderPipelineState(skinned ? skinPipeline : pipeline)
            encoder.setDepthStencilState(q.material.depthWrite ? depthWrite : depthRead)
            encoder.setCullMode(q.material.cull == 1 ? .back : q.material.cull == 2 ? .front : .none)
            encoder.setVertexBuffer(q.mesh.vertices, offset: 0, index: 0)
            if let skin = q.mesh.skin { encoder.setVertexBuffer(skin, offset: 0, index: 1) }
            let flags = q.texture?.flags ?? 0
            encoder.setFragmentTexture(q.texture?.texture ?? white, index: 0)
            encoder.setFragmentTexture(q.lightmap?.texture ?? white, index: 1)
            encoder.setFragmentSamplerState(samplers[(flags & 2 != 0 ? 2 : 0) + (flags & 4 != 0 ? 1 : 0)], index: 0)
            encoder.setFragmentSamplerState(samplers[2], index: 1)
            let mat = q.material
            let bits = UInt32((mat.unlit ? 1 : 0) | (mat.fog ? 2 : 0) | (q.texture != nil ? 4 : 0) |
                (q.lightmap != nil ? 8 : 0) | (skinned ? 16 : 0))
            // Lights are matched here, against the frame's final list.
            let mask: UInt32 = mat.unlit ? 0 : lights.withUnsafeBufferPointer { l in
                q.box.withUnsafeBufferPointer { ac_scene4_light_mask(l.baseAddress, lights.count / 8, $0.baseAddress) }
            }
            var draw = DrawUniforms(tint: mat.color,
                emissive: SIMD4(mat.emissive.x, mat.emissive.y, mat.emissive.z, mat.lightmapScale),
                info: SIMD4(UInt32(q.firstInstance), UInt32(q.firstBone), bits, mask),
                extra: SIMD4(mat.alphaCutoff, 0, 0, 0))
            encoder.setVertexBytes(&draw, length: MemoryLayout<DrawUniforms>.stride, index: 5)
            encoder.setFragmentBytes(&draw, length: MemoryLayout<DrawUniforms>.stride, index: 1)
            encoder.drawIndexedPrimitives(type: .triangle, indexCount: q.mesh.indexCount, indexType: q.mesh.indexType,
                indexBuffer: q.mesh.indices, indexBufferOffset: 0, instanceCount: q.instanceCount)
        }
        // Hand the encoder back as the scene left it for its later draws.
        encoder.setCullMode(.none)
        encoder.setFrontFacing(.clockwise)
        encoder.setScissorRect(MTLScissorRect(x: 0, y: 0, width: Int(drawable.width), height: Int(drawable.height)))
    }

    private func reserve(_ buffer: inout MTLBuffer?, _ bytes: Int) -> MTLBuffer? {
        if let buffer, buffer.length >= bytes { return buffer }
        buffer = device.makeBuffer(length: max(bytes, max(4096, (buffer?.length ?? 0) * 2)), options: .storageModeShared)
        return buffer
    }

    private func viewUniforms(_ m: [Float], stage: CGSize) -> [SIMD4<Float>] {
        [SIMD4(m[0], m[1], m[2], 0), SIMD4(m[3], m[4], m[5], m[12]), SIMD4(m[6], m[7], m[8], m[13]),
         SIMD4(m[9], m[10], m[11], m[17]), SIMD4(m[14], m[15], m[16], m[16] > 0.999 ? 1 : 0),
         SIMD4(m[22], m[23], Self.minDepth, Self.maxDepth),
         SIMD4(Float(stage.width), Float(stage.height), 0, 0), SIMD4(m[24], m[25], m[26], 0)]
    }

    private func sceneUniforms() -> [SIMD4<Float>] {
        let e = environment
        var out = [SIMD4<Float>(e[0], e[1], e[2], 0), SIMD4(e[3], e[4], e[5], 0), SIMD4(e[6], e[7], e[8], e[11]),
                   SIMD4(e[9], e[10], 0, 0)]
        let count = lights.count / 8
        for i in 0..<Self.maxLights {
            out.append(i < count ? SIMD4(lights[i * 8], lights[i * 8 + 1], lights[i * 8 + 2], lights[i * 8 + 3]) : .zero)
        }
        for i in 0..<Self.maxLights {
            out.append(i < count ? SIMD4(lights[i * 8 + 4], lights[i * 8 + 5], lights[i * 8 + 6], 0) : .zero)
        }
        return out
    }

    private func queueDraw(mesh: Mesh, material: Material, camera: [Float], rows: [Float], count: Int,
                           bones palette: UnsafePointer<Float>?, boneCount: Int) -> Int {
        guard queue.count < Self.maxDraws, instances.count / 16 + count <= Self.maxInstances,
              bones.count / 12 + boneCount <= Self.maxFrameBones else { dropped += 1; return 0 }
        var box: [Float] = [1e30, 1e30, 1e30, -1e30, -1e30, -1e30]
        var world = [Float](repeating: 0, count: 6)
        let first = instances.count / 16
        let distance = environment[12]
        rows.withUnsafeBufferPointer { all in camera.withUnsafeBufferPointer { cam in mesh.bounds.withUnsafeBufferPointer { bounds in
            for i in 0..<count {
                let instance = all.baseAddress! + i * 16
                if mesh.bounded {
                    guard ac_scene4_visible(cam.baseAddress, instance, bounds.baseAddress, distance, &world) != 0 else { continue }
                    for j in 0..<3 { box[j] = min(box[j], world[j]); box[j + 3] = max(box[j + 3], world[j + 3]) }
                }
                instances.append(contentsOf: UnsafeBufferPointer(start: instance, count: 16))
            }
        } } }
        let kept = instances.count / 16 - first
        guard kept > 0 else { culled += 1; return 0 }
        if !mesh.bounded { box = [-1e30, -1e30, -1e30, 1e30, 1e30, 1e30] }
        var firstBone = 0
        if mesh.skin != nil, let palette {
            firstBone = bones.count / 12
            var rowsOut = [Float](repeating: 0, count: 12)
            for b in 0..<boneCount {
                ac_scene4_rows(palette + b * 12, &rowsOut)
                bones.append(contentsOf: rowsOut)
            }
        }
        if cameras.last != camera { cameras.append(camera) }
        queue.append(Queued(mesh: mesh, texture: textures[material.texture], lightmap: textures[material.lightmap],
            material: material, camera: cameras.count - 1, firstInstance: first, instanceCount: kept,
            firstBone: firstBone, box: box))
        let drawn = kept * (mesh.indexCount / 3)
        triangles += drawn
        return drawn
    }

    // MARK: JavaScript

    private struct Typed { let type: JSTypedArrayType; let base: UnsafeMutableRawPointer; let bytes: Int }
    private static func typed(_ value: JSValue?) -> Typed? {
        guard let value, !value.isUndefined, !value.isNull,
              let context = value.context?.jsGlobalContextRef else { return nil }
        let type = JSValueGetTypedArrayType(context, value.jsValueRef, nil)
        guard type != kJSTypedArrayTypeNone, type != kJSTypedArrayTypeArrayBuffer,
              let object = JSValueToObject(context, value.jsValueRef, nil),
              let base = JSObjectGetTypedArrayBytesPtr(context, object, nil) else { return nil }
        return Typed(type: type, base: base, bytes: JSObjectGetTypedArrayByteLength(context, object, nil))
    }
    private static func present(_ value: JSValue?) -> Bool { value.map { !$0.isUndefined && !$0.isNull } ?? false }
    private static func raise(_ kind: String, _ message: String) -> JSValue {
        let context = JSContext.current()!
        context.exception = context.objectForKeyedSubscript(kind).construct(withArguments: [message])
        return JSValue(undefinedIn: context)
    }
    private static func number(_ value: Int) -> JSValue { JSValue(int32: Int32(value), in: JSContext.current()) }
    private static var arguments: [JSValue] { JSContext.currentArguments() as? [JSValue] ?? [] }

    func install(into js: JSContext) {
        let gpuMesh: @convention(block) (JSValue) -> JSValue = { [weak self] desc in
            guard let self else { return Self.number(-1) }
            return self.upload(desc)
        }
        let gpuMeshFree: @convention(block) (Int) -> Bool = { [weak self] handle in
            guard let self, let mesh = self.meshes.removeValue(forKey: handle) else { return false }
            self.meshBytes -= mesh.bytes
            return true
        }
        let gpuTexture: @convention(block) () -> JSValue = { [weak self] in
            self?.uploadTexture(Self.arguments) ?? Self.number(-1)
        }
        let gpuTextureFree: @convention(block) (Int) -> Bool = { [weak self] handle in
            guard let self, let texture = self.textures.removeValue(forKey: handle) else { return false }
            self.textureBytes -= texture.bytes
            return true
        }
        let gpuMaterial: @convention(block) () -> JSValue = { [weak self] in
            self?.material(Self.arguments.first) ?? Self.number(-1)
        }
        let gpuMaterialFree: @convention(block) (Int) -> Bool = { [weak self] handle in
            self?.materials.removeValue(forKey: handle) != nil
        }
        let gpuEnvironment: @convention(block) (JSValue) -> JSValue = { [weak self] values in
            guard let self, let t = Self.typed(values), t.type == kJSTypedArrayTypeFloat32Array else {
                return Self.raise("RangeError", "gpuEnvironment needs a finite Float32Array of 13 floats")
            }
            var out = [Float](repeating: 0, count: 13)
            guard ac_scene4_environment(t.base.assumingMemoryBound(to: Float.self), t.bytes / 4, &out) != 0 else {
                return Self.raise("RangeError", "gpuEnvironment needs a finite Float32Array of 13 floats")
            }
            self.environment = out
            return JSValue(undefinedIn: JSContext.current())
        }
        let gpuLights: @convention(block) (JSValue, Int) -> JSValue = { [weak self] values, count in
            guard let self else { return JSValue(undefinedIn: JSContext.current()) }
            guard count >= 0, count <= Self.maxLights else {
                return Self.raise("RangeError", "gpuLights needs a Float32Array and a count up to \(Self.maxLights)")
            }
            if count == 0 { self.lights = []; return JSValue(undefinedIn: JSContext.current()) }
            var out = [Float](repeating: 0, count: count * 8)
            guard let t = Self.typed(values), t.type == kJSTypedArrayTypeFloat32Array, t.bytes / 4 >= count * 8,
                  ac_scene4_lights(t.base.assumingMemoryBound(to: Float.self), count, &out) != 0 else {
                return Self.raise("RangeError", "gpuLights: count × 8 finite floats")
            }
            self.lights = out
            return JSValue(undefinedIn: JSContext.current())
        }
        let gpuDraw: @convention(block) () -> JSValue = { [weak self] in
            self?.draw(Self.arguments, instanced: false) ?? Self.number(0)
        }
        let gpuDrawInstances: @convention(block) () -> JSValue = { [weak self] in
            self?.draw(Self.arguments, instanced: true) ?? Self.number(0)
        }
        let gpuStats: @convention(block) () -> NSDictionary = { [weak self] in
            guard let self else { return [:] }
            return ["meshes": self.meshes.count, "meshBytes": self.meshBytes, "textures": self.textures.count,
                    "textureBytes": self.textureBytes, "materials": self.materials.count,
                    "draws": self.last.draws, "culled": self.last.culled, "triangles": self.last.triangles,
                    "dropped": self.last.dropped] as NSDictionary
        }
        js.setObject(gpuMesh, forKeyedSubscript: "gpuMesh" as NSString)
        js.setObject(gpuMeshFree, forKeyedSubscript: "gpuMeshFree" as NSString)
        js.setObject(gpuTexture, forKeyedSubscript: "gpuTexture" as NSString)
        js.setObject(gpuTextureFree, forKeyedSubscript: "gpuTextureFree" as NSString)
        js.setObject(gpuMaterial, forKeyedSubscript: "gpuMaterial" as NSString)
        js.setObject(gpuMaterialFree, forKeyedSubscript: "gpuMaterialFree" as NSString)
        js.setObject(gpuEnvironment, forKeyedSubscript: "gpuEnvironment" as NSString)
        js.setObject(gpuLights, forKeyedSubscript: "gpuLights" as NSString)
        js.setObject(gpuDraw, forKeyedSubscript: "gpuDraw" as NSString)
        js.setObject(gpuDrawInstances, forKeyedSubscript: "gpuDrawInstances" as NSString)
        js.setObject(gpuStats, forKeyedSubscript: "gpuStats" as NSString)
        js.setObject([
            "maxVertices": 1 << 20, "maxIndices": 3 << 20, "meshBytes": Self.maxMeshBytes,
            "textureBytes": Self.maxTextureBytes, "maxTextureSide": Self.maxTextureSide,
            "maxMeshes": Self.maxMeshes, "maxTextures": Self.maxTextures, "maxMaterials": Self.maxMaterials,
            "maxBones": Self.maxBones, "maxLights": Self.maxLights, "maxDraws": Self.maxDraws,
            "maxInstances": Self.maxInstances, "maxFrameBones": Self.maxFrameBones,
        ] as NSDictionary, forKeyedSubscript: "gpuLimits" as NSString)
    }

    private func upload(_ desc: JSValue) -> JSValue {
        guard desc.isObject else { return Self.raise("TypeError", "gpuMesh: needs a descriptor object") }
        guard let positions = Self.typed(desc.forProperty("positions")), positions.type == kJSTypedArrayTypeFloat32Array,
              positions.bytes % 12 == 0 else {
            return Self.raise("TypeError", "gpuMesh: positions must be a Float32Array of xyz")
        }
        let n = positions.bytes / 12
        guard let indices = Self.typed(desc.forProperty("indices")),
              indices.type == kJSTypedArrayTypeUint16Array || indices.type == kJSTypedArrayTypeUint32Array else {
            return Self.raise("TypeError", "gpuMesh: indices must be a Uint16Array or Uint32Array")
        }
        // Optional fields: absent is fine, the wrong kind or length is the piece's bug.
        var bad: String?
        func field(_ name: String, _ types: [JSTypedArrayType], _ count: Int) -> Typed? {
            let value = desc.forProperty(name)
            guard Self.present(value) else { return nil }
            guard let t = Self.typed(value), types.contains(t.type) else { bad = bad ?? name; return nil }
            let size = t.type == kJSTypedArrayTypeFloat32Array ? 4 : 1
            if t.bytes / size != count { bad = bad ?? name; return nil }
            return t
        }
        let bytes8 = [kJSTypedArrayTypeUint8Array, kJSTypedArrayTypeUint8ClampedArray]
        let normals = field("normals", [kJSTypedArrayTypeFloat32Array], n * 3)
        let colors = field("colors", bytes8, n * 4)
        let uvs = field("uvs", [kJSTypedArrayTypeFloat32Array], n * 2)
        let uvs2 = field("uvs2", [kJSTypedArrayTypeFloat32Array], n * 2)
        let joints = field("joints", bytes8, n * 4)
        let weights = field("weights", [kJSTypedArrayTypeFloat32Array] + bytes8, n * 4)
        let bounds = field("bounds", [kJSTypedArrayTypeFloat32Array], 6)
        if let bad { return Self.raise("TypeError", "gpuMesh: \(bad) has the wrong type or length") }
        let floats = { (t: Typed?) in t.map { UnsafePointer($0.base.assumingMemoryBound(to: Float.self)) } }
        let bytes = { (t: Typed?) in t.map { UnsafePointer($0.base.assumingMemoryBound(to: UInt8.self)) } }
        var input = ac_scene4_input()
        input.positions = floats(positions); input.vertex_count = n
        input.normals = floats(normals); input.colors = bytes(colors)
        input.uvs = floats(uvs); input.uvs2 = floats(uvs2); input.joints = bytes(joints)
        if weights?.type == kJSTypedArrayTypeFloat32Array { input.weights = floats(weights) } else { input.weight_bytes = bytes(weights) }
        if indices.type == kJSTypedArrayTypeUint16Array {
            input.indices16 = UnsafePointer(indices.base.assumingMemoryBound(to: UInt16.self)); input.index_count = indices.bytes / 2
        } else {
            input.indices32 = UnsafePointer(indices.base.assumingMemoryBound(to: UInt32.self)); input.index_count = indices.bytes / 4
        }
        input.bounds = floats(bounds)
        var error: UnsafePointer<CChar>?
        guard let packed = ac_scene4_pack(&input, &error) else {
            return Self.raise("TypeError", "gpuMesh: " + (error.map { String(cString: $0) } ?? "invalid"))
        }
        defer { ac_scene4_mesh_free(packed) }
        let size = ac_scene4_bytes(packed)
        guard meshBytes + size <= Self.maxMeshBytes else { return Self.number(-1) }
        guard let handle = (1...Self.maxMeshes).first(where: { meshes[$0] == nil }) else { return Self.number(-1) }
        var length = 0, wide: Int32 = 0
        let vertexBytes = ac_scene4_vertices(packed, &length)!
        guard let vertexBuffer = device.makeBuffer(bytes: vertexBytes, length: length, options: .storageModeShared) else { return Self.number(-1) }
        var skinBuffer: MTLBuffer?
        if let skin = ac_scene4_skin(packed, &length) {
            skinBuffer = device.makeBuffer(bytes: skin, length: length, options: .storageModeShared)
            if skinBuffer == nil { return Self.number(-1) }
        }
        let indexBytes = ac_scene4_indices(packed, &length, &wide)!
        guard let indexBuffer = device.makeBuffer(bytes: indexBytes, length: length, options: .storageModeShared) else { return Self.number(-1) }
        var box = [Float](repeating: 0, count: 6)
        let bounded = ac_scene4_bounds(packed, &box) != 0
        meshes[handle] = Mesh(vertices: vertexBuffer, skin: skinBuffer, indices: indexBuffer,
            indexType: wide != 0 ? .uint32 : .uint16, indexCount: ac_scene4_index_count(packed),
            bounds: box, bounded: bounded, maxJoint: Int(ac_scene4_max_joint(packed)), bytes: size)
        meshBytes += size
        return Self.number(handle)
    }

    private func uploadTexture(_ args: [JSValue]) -> JSValue {
        guard args.count >= 3, let pixels = Self.typed(args[2]),
              pixels.type == kJSTypedArrayTypeUint8Array || pixels.type == kJSTypedArrayTypeUint8ClampedArray else {
            return Self.raise("TypeError", "gpuTexture: needs width, height and a Uint8Array of rgba")
        }
        let width = Int(args[0].toInt32()), height = Int(args[1].toInt32())
        let flags = args.count > 3 ? Int(args[3].toInt32()) : 0
        let pow2 = { (v: Int) in v > 0 && v & (v - 1) == 0 }
        guard pow2(width), pow2(height), width <= Self.maxTextureSide, height <= Self.maxTextureSide else {
            return Self.raise("RangeError", "gpuTexture: sides must be powers of two up to \(Self.maxTextureSide)")
        }
        guard pixels.bytes == width * height * 4 else { return Self.raise("RangeError", "gpuTexture: rgba must be width*height*4 bytes") }
        guard (0...7).contains(flags) else { return Self.raise("RangeError", "gpuTexture: unknown flags") }
        let mips = flags & 1 != 0
        let size = width * height * 4 * (mips ? 4 : 3) / 3
        guard textureBytes + size <= Self.maxTextureBytes,
              let handle = (1...Self.maxTextures).first(where: { textures[$0] == nil }) else { return Self.number(-1) }
        let descriptor = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .rgba8Unorm, width: width, height: height, mipmapped: mips)
        descriptor.storageMode = device.hasUnifiedMemory ? .shared : .managed
        guard let texture = device.makeTexture(descriptor: descriptor) else { return Self.number(-1) }
        texture.replace(region: MTLRegionMake2D(0, 0, width, height), mipmapLevel: 0, withBytes: pixels.base, bytesPerRow: width * 4)
        if mips, let command = uploads.makeCommandBuffer(), let blit = command.makeBlitCommandEncoder() {
            blit.generateMipmaps(for: texture)
            blit.endEncoding()
            command.commit()
            command.waitUntilCompleted()  // upload time, not frame time
        }
        textures[handle] = Texture(texture: texture, flags: flags, bytes: size)
        textureBytes += size
        return Self.number(handle)
    }

    private func material(_ desc: JSValue?) -> JSValue {
        var m = Material()
        if let desc, Self.present(desc) {
            guard desc.isObject else { return Self.raise("TypeError", "gpuMaterial: needs a descriptor object") }
            var bad = false
            func numbers(_ name: String, _ count: Int) -> [Float]? {
                let value = desc.forProperty(name)
                guard Self.present(value), let value else { return nil }
                return (0..<count).map { i in
                    let v = value.atIndex(i).toDouble()
                    if !v.isFinite { bad = true }
                    return Float(v)
                }
            }
            func scalar(_ name: String, _ fallback: Double) -> Float {
                let value = desc.forProperty(name)
                guard Self.present(value), let value else { return Float(fallback) }
                let v = value.toDouble()
                if !v.isFinite { bad = true }
                return Float(v)
            }
            func flag(_ name: String, _ fallback: Bool) -> Bool {
                let value = desc.forProperty(name)
                guard let value, !value.isUndefined else { return fallback }
                return value.toBool()
            }
            if let c = numbers("color", 4) { m.color = SIMD4(c[0], c[1], c[2], c[3]).clamped(lowerBound: .zero, upperBound: SIMD4(repeating: 16)) }
            if let e = numbers("emissive", 3) { m.emissive = SIMD3(e[0], e[1], e[2]).clamped(lowerBound: .zero, upperBound: SIMD3(repeating: 16)) }
            m.texture = Int(scalar("texture", 0))
            m.lightmap = Int(scalar("lightmap", 0))
            m.lightmapScale = min(16, max(0, scalar("lightmapScale", 1)))
            m.alphaCutoff = min(1, max(0, scalar("alphaCutoff", 0)))
            let cull = desc.forProperty("cull")
            if let cull, !cull.isUndefined {
                switch cull.isString ? cull.toString() ?? "" : "" {
                case "none": m.cull = 0
                case "back": m.cull = 1
                case "front": m.cull = 2
                default: bad = true
                }
            }
            if bad { return Self.raise("TypeError", "gpuMaterial: non-finite number or unknown cull") }
            m.unlit = flag("unlit", false); m.fog = flag("fog", true); m.depthWrite = flag("depthWrite", true)
        }
        guard let handle = (1...Self.maxMaterials).first(where: { materials[$0] == nil }) else { return Self.number(-1) }
        materials[handle] = m
        return Self.number(handle)
    }

    private func draw(_ args: [JSValue], instanced: Bool) -> JSValue {
        let call = instanced ? "gpuDrawInstances" : "gpuDraw"
        guard args.count >= (instanced ? 5 : 3) else { return Self.raise("TypeError", "\(call): needs mesh, material and camera") }
        guard let mesh = meshes[Int(args[0].toInt32())], let material = materials[Int(args[1].toInt32())] else { return Self.number(0) }
        guard let cam = Self.typed(args[2]), cam.type == kJSTypedArrayTypeFloat32Array, cam.bytes == 27 * 4,
              ac_scene4_camera_valid(cam.base.assumingMemoryBound(to: Float.self)) != 0 else {
            return Self.raise("RangeError", "\(call) needs a 27-float camera")
        }
        let camera = Array(UnsafeBufferPointer(start: cam.base.assumingMemoryBound(to: Float.self), count: 27))
        var rows: [Float]
        var count = 1
        if instanced {
            count = Int(args[4].toInt32())
            guard let values = Self.typed(args[3]), values.type == kJSTypedArrayTypeFloat32Array,
                  count >= 0, count <= Self.maxInstances, values.bytes / 4 >= count * 16,
                  ac_scene4_finite(values.base.assumingMemoryBound(to: Float.self), count * 16) != 0 else {
                return Self.raise("RangeError", "\(call): count × 16 finite floats, up to \(Self.maxInstances)")
            }
            if count == 0 { return Self.number(0) }
            let source = values.base.assumingMemoryBound(to: Float.self)
            rows = [Float](repeating: 0, count: count * 16)
            for i in 0..<count {
                rows.withUnsafeMutableBufferPointer { ac_scene4_rows(source + i * 16, $0.baseAddress! + i * 16) }
                for k in 0..<4 { rows[i * 16 + 12 + k] = min(16, max(0, source[i * 16 + 12 + k])) }
            }
        } else {
            var columns: [Float] = [1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0]
            if args.count > 3, Self.present(args[3]) {
                guard let model = Self.typed(args[3]), model.type == kJSTypedArrayTypeFloat32Array, model.bytes == 48,
                      ac_scene4_finite(model.base.assumingMemoryBound(to: Float.self), 12) != 0 else {
                    return Self.raise("RangeError", "gpuDraw: model is 12 finite floats (x axis, y axis, z axis, origin)")
                }
                columns = Array(UnsafeBufferPointer(start: model.base.assumingMemoryBound(to: Float.self), count: 12))
            }
            rows = [Float](repeating: 1, count: 16)
            rows.withUnsafeMutableBufferPointer { ac_scene4_rows(columns, $0.baseAddress!) }
        }
        var palette: UnsafePointer<Float>?
        var boneCount = 0
        if mesh.skin != nil {
            let index = instanced ? 5 : 4
            guard args.count > index, let b = Self.typed(args[index]), b.type == kJSTypedArrayTypeFloat32Array, b.bytes % 48 == 0 else {
                return Self.raise("TypeError", "\(call): a skinned mesh needs bones (12 floats each)")
            }
            boneCount = b.bytes / 48
            guard boneCount <= Self.maxBones, boneCount > mesh.maxJoint else {
                return Self.raise("RangeError", "\(call): \(boneCount) bones for joints up to \(mesh.maxJoint) (cap \(Self.maxBones))")
            }
            palette = UnsafePointer(b.base.assumingMemoryBound(to: Float.self))
            guard ac_scene4_finite(palette, boneCount * 12) != 0 else { return Self.raise("RangeError", "\(call): non-finite bone") }
        }
        return Self.number(queueDraw(mesh: mesh, material: material, camera: camera, rows: rows, count: count,
                                     bones: palette, boneCount: boneCount))
    }

    // MARK: shaders — xbox/native-bios/Mesh4*.hlsl, line for line.

    private static let shaders = """
    #include <metal_stdlib>
    using namespace metal;
    struct Vertex4 { packed_float3 position; packed_char4 normal; packed_uchar4 color; packed_float2 uv; packed_float2 uv2; };
    struct Skin4 { uchar4 joints; uchar4 weights; };
    struct Instance4 { float4 r0, r1, r2, tint; };
    struct View4 { float4 camPos, right, up, forward, proj, depthMap, stage, sunDir; };
    struct Scene4 { float4 ambient, sun, fogColor, fogRange; float4 lightPos[32]; float4 lightColor[32]; };
    struct Draw4 { float4 tint, emissive; uint4 info; float4 extra; };
    struct Raster4 {
      float4 position [[position]];
      float clip [[clip_distance]] [1];
      float3 world [[user(world)]]; float3 normal [[user(normal)]]; float4 color [[user(color)]];
      float2 uv [[user(uv)]]; float2 uv2 [[user(uv2)]];
    };
    struct Fragment4 {
      float4 position [[position]];
      float3 world [[user(world)]]; float3 normal [[user(normal)]]; float4 color [[user(color)]];
      float2 uv [[user(uv)]]; float2 uv2 [[user(uv2)]];
    };
    static float3 row3(Instance4 m, float4 p) { return float3(dot(m.r0, p), dot(m.r1, p), dot(m.r2, p)); }
    static float3 bone3(const device float4* bones, uint b, float4 p) { return float3(dot(bones[b], p), dot(bones[b + 1], p), dot(bones[b + 2], p)); }
    static Raster4 place(float3 p, float3 n, Vertex4 v, Instance4 inst, constant View4& view) {
      const float3 world = row3(inst, float4(p, 1));
      const float3 d = world - view.camPos.xyz;
      const float3 c = float3(dot(d, view.right.xyz), dot(d, view.up.xyz), dot(d, view.forward.xyz));
      const float depth = clamp(view.depthMap.x + c.z * view.depthMap.y, view.depthMap.z, view.depthMap.w);
      const float zn = (depth + 1.5) / 3.0;
      const float2 halfStage = view.stage.xy * 0.5;
      Raster4 o;
      if (view.proj.w > 0.5) {
        const float w = c.z;
        const float sxw = view.right.w * w + c.x * view.proj.y, syw = view.up.w * w - c.y * view.proj.y;
        o.position = float4(sxw / halfStage.x - w, w - syw / halfStage.y, zn * w, w);
      } else {
        const float k = view.proj.x + (view.proj.y / max(c.z, 1e-3) - view.proj.x) * view.proj.z;
        const float sx = view.right.w + c.x * k, sy = view.up.w - c.y * k;
        o.position = float4(sx / halfStage.x - 1.0, 1.0 - sy / halfStage.y, zn, 1.0);
      }
      o.clip[0] = c.z - view.forward.w;
      o.world = world;
      o.normal = row3(inst, float4(n, 0));
      o.color = float4(uchar4(v.color)) / 255.0 * inst.tint;
      o.uv = float2(v.uv); o.uv2 = float2(v.uv2);
      return o;
    }
    vertex Raster4 mesh4_vertex(uint vid [[vertex_id]], uint iid [[instance_id]],
        const device Vertex4* verts [[buffer(0)]], const device Instance4* instances [[buffer(2)]],
        constant View4& view [[buffer(4)]], constant Draw4& draw [[buffer(5)]]) {
      const Vertex4 v = verts[vid];
      return place(float3(v.position), float3(char4(v.normal).xyz) / 127.0, v, instances[draw.info.x + iid], view);
    }
    vertex Raster4 mesh4_skin_vertex(uint vid [[vertex_id]], uint iid [[instance_id]],
        const device Vertex4* verts [[buffer(0)]], const device Skin4* skin [[buffer(1)]],
        const device Instance4* instances [[buffer(2)]], const device float4* bones [[buffer(3)]],
        constant View4& view [[buffer(4)]], constant Draw4& draw [[buffer(5)]]) {
      const Vertex4 v = verts[vid];
      const Skin4 s = skin[vid];
      const float3 p = float3(v.position), n = float3(char4(v.normal).xyz) / 127.0;
      float3 sp = 0, sn = 0;
      for (int k = 0; k < 4; ++k) {
        const float w = float(s.weights[k]) / 255.0;
        if (w > 0) {
          const uint b = (draw.info.y + s.joints[k]) * 3;
          sp += w * bone3(bones, b, float4(p, 1));
          sn += w * bone3(bones, b, float4(n, 0));
        }
      }
      return place(sp, sn, v, instances[draw.info.x + iid], view);
    }
    fragment float4 mesh4_fragment(Fragment4 i [[stage_in]], bool front [[front_facing]],
        constant Scene4& scene [[buffer(0)]], constant Draw4& draw [[buffer(1)]], constant View4& view [[buffer(2)]],
        texture2d<float> albedo [[texture(0)]], texture2d<float> lightmap [[texture(1)]],
        sampler albedoSampler [[sampler(0)]], sampler lightmapSampler [[sampler(1)]]) {
      const uint flags = draw.info.z;
      float4 base = i.color * draw.tint;
      if (flags & 4) base *= albedo.sample(albedoSampler, i.uv);
      if (base.a < draw.extra.x) discard_fragment();
      float3 lit = 1;
      if (!(flags & 1)) {
        float3 n = normalize(i.normal);
        if (!front) n = -n;
        lit = (flags & 8) ? lightmap.sample(lightmapSampler, i.uv2).rgb * draw.emissive.w
          : scene.ambient.rgb + scene.sun.rgb * saturate(-dot(n, view.sunDir.xyz));
        uint mask = draw.info.w;
        while (mask) {
          const uint k = ctz(mask);
          mask &= mask - 1;
          const float3 L = scene.lightPos[k].xyz - i.world;
          const float d2 = dot(L, L), r2 = scene.lightPos[k].w * scene.lightPos[k].w;
          if (d2 < r2) {
            float fall = 1.0 - d2 / r2;
            fall *= fall;
            lit += scene.lightColor[k].rgb * fall * saturate(dot(n, L * rsqrt(max(d2, 1e-8))));
          }
        }
      }
      float3 rgb = base.rgb * lit + draw.emissive.rgb;
      if ((flags & 2) && scene.fogRange.y > scene.fogRange.x) {
        const float f = saturate((length(i.world - view.camPos.xyz) - scene.fogRange.x) / (scene.fogRange.y - scene.fogRange.x));
        rgb = mix(rgb, scene.fogColor.rgb, f * scene.fogColor.w);
      }
      if (base.a < 0.999) {
        const uint2 pixel = uint2(i.position.xy) & 3;
        const uint pattern[16] = {0, 8, 2, 10, 12, 4, 14, 6, 3, 11, 1, 9, 15, 7, 13, 5};
        if (base.a < (pattern[pixel.y * 4 + pixel.x] + 0.5) / 16.0) discard_fragment();
      }
      return float4(rgb, 1.0);
    }
    """
}
#endif
