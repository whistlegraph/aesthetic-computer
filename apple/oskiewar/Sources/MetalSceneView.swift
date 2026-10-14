import MetalKit

struct SceneVertex {
    var position: SIMD4<Float>
    var color: SIMD4<Float>
}

struct PoolDecalVertex {
    var position: SIMD4<Float>
    var uvq: SIMD4<Float>
}

/// Depth-bearing Oskiewar scene surface. The JavaScript camera already emits
/// projected logical x/y and canonical z; this view performs only the shared
/// logical-to-clip transform also used by the web and Xbox adapters.
///
/// One renderer serves both Apple apps. MTKView exists on iOS and macOS alike,
/// and the engine speaks the same triangle stream to every host it has ever
/// had — canvas on the web, D3D on Xbox, Metal here — so the platforms differ
/// only where their frameworks genuinely differ, and that difference is fenced
/// to the few lines below rather than paid for with a second renderer.
final class MetalSceneView: MTKView, MTKViewDelegate {
    /// The HUD writes nameplates, a clock, and a command stream — hundreds of
    /// glyphs, not thousands. Six vertices each.
    static let maxGlyphs = 4_096
    /// The stage the engine is drawing into, in its own logical units. Batches
    /// read it from here rather than carrying it per triangle: eight thousand
    /// triangles a frame should not restate the size of the screen eight
    /// thousand times.
    var logicalSize = CGSize(width: 1920, height: 1080)
    private var sceneVertices: [SceneVertex] = []
    private let commandQueue: MTLCommandQueue
    private let pipeline: MTLRenderPipelineState
    private let depthState: MTLDepthStencilState
    private let poolDecalPipeline: MTLRenderPipelineState
    private let poolDecalDepth: MTLDepthStencilState
    private var poolDecalVertices: [PoolDecalVertex] = []
    var poolDecalTexture: ((Int) -> MTLTexture?)?
    // Each slot stays owned by its command buffer until the GPU completes it.
    // Pool marks can outgrow the initial allocation; never truncate a scene
    // before its later-submitted fighters have reached the GPU.
    private final class Uploads {
        var scene: MTLBuffer?
        var glyphs: MTLBuffer?
        var decals: MTLBuffer?
        var sdfFigures: MTLBuffer?
        var sdfPrims: MTLBuffer?
        var bubbles: MTLBuffer?
    }
    /// Raymarched figures; nil when the device refused the pipeline, and the
    /// piece then draws triangles (sdfFigure returns false).
    private let sdf: SdfFigures?
    /// The shared ink line; nil falls back to drawing straight to the screen.
    private let ink: InkOutlines?
    var inkOutlines = true
    private var skyCamera: [Float]?
    private var skyTime: Float = 0
    /// The piece's camera for this frame, so the sky turns with the view.
    func sceneCamera(_ camera: UnsafePointer<Float>, time: Float) {
        sceneLock.lock()
        defer { sceneLock.unlock() }
        for i in 0..<27 where !camera[i].isFinite { return }
        skyCamera = Array(UnsafeBufferPointer(start: camera, count: 27))
        skyTime = time
    }
    private let uploads = (0..<3).map { _ in Uploads() }
    private let inFlight = DispatchSemaphore(value: 3)
    private var uploadIndex = 0

    private func reserve(_ buffer: inout MTLBuffer?, bytes: Int) -> Bool {
        if let buffer, buffer.length >= bytes { return true }
        buffer = device?.makeBuffer(length: max(bytes, max(256, (buffer?.length ?? 0) * 2)),
                                    options: .storageModeShared)
        return buffer != nil
    }
    private let glyphAtlas: GlyphAtlas
    private let glyphPipeline: MTLRenderPipelineState
    private var glyphVertices: [GlyphVertex] = []
    private let sceneLock = NSLock()
    private var background = MTLClearColor(red: 7 / 255, green: 8 / 255,
                                            blue: 28 / 255, alpha: 1)

    init(frame: CGRect = .zero) {
        guard let device = MTLCreateSystemDefaultDevice(),
              let queue = device.makeCommandQueue() else {
            fatalError("Metal is unavailable")
        }
        commandQueue = queue
        let source = """
        #include <metal_stdlib>
        using namespace metal;
        struct Vertex { float4 position; float4 color; };
        struct Raster { float4 position [[position]]; float4 color; };
        vertex Raster scene_vertex(const device Vertex *vertices [[buffer(0)]],
                                   uint id [[vertex_id]]) {
          Raster out;
          out.position = vertices[id].position;
          out.color = vertices[id].color;
          return out;
        }
        fragment float4 scene_fragment(Raster in [[stage_in]]) {
          if(in.color.a<0.999){
            uint2 pixel=uint2(in.position.xy)&3;
            const uint pattern[16]={0,8,2,10,12,4,14,6,3,11,1,9,15,7,13,5};
            if(in.color.a<(pattern[pixel.y*4+pixel.x]+0.5)/16.0)discard_fragment();
          }
          return float4(in.color.rgb, 1.0);
        }
        struct DecalVertex { float4 position; float4 uvq; };
        struct DecalRaster { float4 position [[position]]; float3 uvq; };
        vertex DecalRaster decal_vertex(const device DecalVertex *v [[buffer(0)]], uint id [[vertex_id]]) {
          DecalRaster out; out.position = v[id].position; out.uvq = v[id].uvq.xyz; return out;
        }
        fragment float4 decal_fragment(DecalRaster in [[stage_in]], texture2d<float> atlas [[texture(0)]]) {
          constexpr sampler linear(filter::linear, address::clamp_to_edge);  // smooth chalk and marks, not texel blocks
          float4 color = atlas.sample(linear, in.uvq.xy / in.uvq.z);
          if (color.a < 0.02) discard_fragment();
          return color;
        }
        """
        do {
            let library = try device.makeLibrary(source: source, options: nil)
            let descriptor = MTLRenderPipelineDescriptor()
            descriptor.vertexFunction = library.makeFunction(name: "scene_vertex")
            descriptor.fragmentFunction = library.makeFunction(name: "scene_fragment")
            descriptor.colorAttachments[0].pixelFormat = .bgra8Unorm
            descriptor.depthAttachmentPixelFormat = .depth32Float
            pipeline = try device.makeRenderPipelineState(descriptor: descriptor)
            descriptor.vertexFunction = library.makeFunction(name: "decal_vertex")
            descriptor.fragmentFunction = library.makeFunction(name: "decal_fragment")
            let blend = descriptor.colorAttachments[0]!
            blend.isBlendingEnabled = true
            blend.sourceRGBBlendFactor = .sourceAlpha
            blend.destinationRGBBlendFactor = .oneMinusSourceAlpha
            blend.sourceAlphaBlendFactor = .one
            blend.destinationAlphaBlendFactor = .oneMinusSourceAlpha
            poolDecalPipeline = try device.makeRenderPipelineState(descriptor: descriptor)
        } catch {
            fatalError("Metal scene pipeline failed: \(error)")
        }
        sdf = try? SdfFigures(device: device)
        ink = try? InkOutlines(device: device)
        glyphAtlas = GlyphAtlas(device: device)
        do {
            glyphPipeline = try GlyphAtlas.pipeline(device: device)
        } catch {
            fatalError("Metal glyph pipeline failed: \(error)")
        }
        let depth = MTLDepthStencilDescriptor()
        depth.isDepthWriteEnabled = true
        depth.depthCompareFunction = .lessEqual
        guard let state = device.makeDepthStencilState(descriptor: depth) else {
            fatalError("Metal depth state failed")
        }
        depthState = state
        depth.isDepthWriteEnabled = false
        guard let decalState = device.makeDepthStencilState(descriptor: depth) else {
            fatalError("Metal decal depth state failed")
        }
        poolDecalDepth = decalState
        super.init(frame: frame, device: device)
        colorPixelFormat = .bgra8Unorm
        depthStencilPixelFormat = .depth32Float
        clearDepth = 1
        framebufferOnly = true
        isPaused = true
        enableSetNeedsDisplay = true
        preferredFramesPerSecond = 60
        delegate = self
        #if canImport(UIKit)
        isMultipleTouchEnabled = true
        #endif
    }

    required init(coder: NSCoder) { fatalError("init(coder:) is unsupported") }

    func beginFrame(red: Double, green: Double, blue: Double) {
        sceneLock.lock()
        sceneVertices.removeAll(keepingCapacity: true)
        poolDecalVertices.removeAll(keepingCapacity: true)
        glyphVertices.removeAll(keepingCapacity: true)
        sdf?.clear()
        glyphAtlas.beginFrame()
        background = MTLClearColor(red: clamped(red), green: clamped(green),
            blue: clamped(blue), alpha: 1)
        sceneLock.unlock()
    }

    /// The four writing hands the engine expects — `write`, `systemWrite`,
    /// `comicWrite`, `ywftWrite` — differ only in which face they ask for, so
    /// the host binds the family at registration and they all arrive here.
    func write(_ text: String, family: String, x: Double, y: Double,
               size: Double, red: Double, green: Double, blue: Double) {
        guard !text.isEmpty else { return }
        sceneLock.lock()
        defer { sceneLock.unlock() }
        guard glyphVertices.count < Self.maxGlyphs * 6 else { return }
        glyphAtlas.write(text, family: family, x: x, y: y, size: size,
                         red: red, green: green, blue: blue,
                         into: &glyphVertices)
    }

    @discardableResult
    func triangle(_ values: [Double], logicalSize: CGSize) -> Bool {
        guard values.count >= 12, logicalSize.width > 0, logicalSize.height > 0,
              values.prefix(12).allSatisfy(\.isFinite) else { return false }
        sceneLock.lock()
        defer { sceneLock.unlock() }
        let ink = SIMD4<Float>(Float(clamped255(values[9])),
                               Float(clamped255(values[10])),
                               Float(clamped255(values[11])), values.count>12 ? Float(max(0,min(1,values[12]))) : 1)
        for vertex in 0..<3 {
            let at = vertex * 3
            let x = Float(values[at] / Double(logicalSize.width / 2) - 1)
            let y = Float(1 - values[at + 1] / Double(logicalSize.height / 2))
            let z = Float(max(0, min(1, (values[at + 2] + 1.5) / 3)))
            sceneVertices.append(SceneVertex(position: SIMD4(x, y, z, 1), color: ink))
        }
        return true
    }

    /// The batched path, and the one that carries a real fight. The engine can
    /// hand a whole frame over as one Float32Array — twelve floats per
    /// triangle, nine of position and three of ink, the same dozen the
    /// per-triangle call takes — so the crossing from JavaScript into Swift
    /// happens once instead of eight thousand times. The upload buffer grows
    /// to fit a complete frame; later-submitted fighters are never truncated.
    @discardableResult
    func triangleBatch(_ values: UnsafePointer<Float>, count: Int) -> Int {
        let halfWidth = Float(logicalSize.width / 2)
        let halfHeight = Float(logicalSize.height / 2)
        guard count > 0, halfWidth > 0, halfHeight > 0 else { return 0 }
        sceneLock.lock()
        defer { sceneLock.unlock() }
        let drawn = count
        sceneVertices.reserveCapacity(sceneVertices.count + drawn * 3)
        var appended = 0
        for triangle in 0..<drawn {
            let base = triangle * 12
            // A triangle is all three of its corners or none of them. Checking
            // per vertex and skipping the bad one would leave two vertices
            // behind, and the vertex stream has no seams — every later triangle
            // would be assembled from its neighbours' corners.
            var finite = true
            for offset in 0..<12 where !values[base + offset].isFinite {
                finite = false
            }
            guard finite else { continue }
            let ink = SIMD4<Float>(clampedChannel(values[base + 9]),
                                   clampedChannel(values[base + 10]),
                                   clampedChannel(values[base + 11]), 1)
            for vertex in 0..<3 {
                let at = base + vertex * 3
                sceneVertices.append(SceneVertex(
                    position: SIMD4(values[at] / halfWidth - 1,
                                    1 - values[at + 1] / halfHeight,
                                    max(0, min(1, (values[at + 2] + 1.5) / 3)), 1),
                    color: ink))
            }
            appended += 1
        }
        return appended
    }

    /// sceneApi 3: queue one raymarched figure for this frame.
    func sdfFigure(camera: UnsafePointer<Float>, prims: UnsafePointer<Float>, count: Int) -> Bool {
        sceneLock.lock()
        defer { sceneLock.unlock() }
        return sdf?.queue(camera: camera, values: prims, count: count) ?? false
    }

    func sdfBubble(camera: UnsafePointer<Float>, x: Float, y: Float, z: Float,
                   radius: Float, tint: SIMD3<Float>) -> Bool {
        sceneLock.lock()
        defer { sceneLock.unlock() }
        return sdf?.queueBubble(camera: camera, x: x, y: y, z: z, radius: radius, tint: tint) ?? false
    }

    func setPoolDecals(_ values: UnsafePointer<Float>, vertexCount: Int) {
        sceneLock.lock()
        defer { sceneLock.unlock() }
        let hw = Float(logicalSize.width / 2), hh = Float(logicalSize.height / 2)
        guard hw > 0, hh > 0, vertexCount > 0, vertexCount % 3 == 0 else { return }
        poolDecalVertices.removeAll(keepingCapacity: true)
        poolDecalVertices.reserveCapacity(vertexCount)
        for i in 0..<vertexCount {
            let at = i * 6
            poolDecalVertices.append(PoolDecalVertex(
                position: SIMD4(values[at] / hw - 1, 1 - values[at + 1] / hh,
                    max(0, min(1, (values[at + 2] + 1.5) / 3)), 1),
                uvq: SIMD4(values[at + 3], values[at + 4], values[at + 5], 0)))
        }
    }

    /// Hand the assembled frame to the display. The two frameworks spell this
    /// differently — AppKit's `setNeedsDisplay` wants a rectangle and offers a
    /// whole-view flag instead — and this is the entire difference.
    func present() {
        #if canImport(UIKit)
        setNeedsDisplay()
        #else
        needsDisplay = true
        #endif
    }

    func draw(in view: MTKView) {
        sceneLock.lock()
        let vertices = sceneVertices
        let glyphs = glyphVertices
        let poolDecals = poolDecalVertices
        let clear = background
        let stage = logicalSize
        let sdfFigures = sdf?.figures ?? []
        let sdfPrims = sdf?.prims ?? []
        let sdfViews = sdf?.views ?? []
        let bubbles = sdf?.bubbles ?? []
        let bubbleViews = sdf?.bubbleViews ?? []
        sceneLock.unlock()
        guard let pass = currentRenderPassDescriptor,
              let drawable = currentDrawable,
              let command = commandQueue.makeCommandBuffer() else { return }
        guard inFlight.wait(timeout: .now()) == .success else { return }
        var submitted = false
        defer { if !submitted { inFlight.signal() } }
        let slot = uploads[uploadIndex]
        guard reserve(&slot.scene, bytes: vertices.count * MemoryLayout<SceneVertex>.stride),
              reserve(&slot.glyphs, bytes: glyphs.count * MemoryLayout<GlyphVertex>.stride),
              reserve(&slot.decals, bytes: poolDecals.count * MemoryLayout<PoolDecalVertex>.stride),
              reserve(&slot.sdfFigures, bytes: sdfFigures.count * MemoryLayout<SdfFigureGpu>.stride),
              reserve(&slot.sdfPrims, bytes: sdfPrims.count * MemoryLayout<Float>.stride),
              reserve(&slot.bubbles, bytes: bubbles.count * MemoryLayout<SdfFigureGpu>.stride),
              let vertexBuffer = slot.scene, let glyphBuffer = slot.glyphs,
              let decalBuffer = slot.decals else { return }
        let decalTexture = poolDecals.isEmpty ? nil : poolDecalTexture?(uploadIndex)
        pass.colorAttachments[0].clearColor = clear
        pass.colorAttachments[0].loadAction = .clear
        pass.depthAttachment.clearDepth = 1
        pass.depthAttachment.loadAction = .clear
        // With ink outlines the world renders offscreen first; the drawable
        // then receives it through the outline pass, and text goes on last.
        let offscreen = inkOutlines ? ink?.targets(device: device!, size: drawableSize) : nil
        var scenePass = pass
        if let (color, depth) = offscreen {
            scenePass = MTLRenderPassDescriptor()
            scenePass.colorAttachments[0].texture = color
            scenePass.colorAttachments[0].loadAction = .clear
            scenePass.colorAttachments[0].storeAction = .store
            scenePass.colorAttachments[0].clearColor = clear
            scenePass.depthAttachment.texture = depth
            scenePass.depthAttachment.loadAction = .clear
            scenePass.depthAttachment.storeAction = .store
            scenePass.depthAttachment.clearDepth = 1
        }
        guard var encoder = command.makeRenderCommandEncoder(descriptor: scenePass) else { return }
        if !vertices.isEmpty {
            _ = vertices.withUnsafeBytes { bytes in
                memcpy(vertexBuffer.contents(), bytes.baseAddress!, bytes.count)
            }
            encoder.setRenderPipelineState(pipeline)
            encoder.setDepthStencilState(depthState)
            encoder.setVertexBuffer(vertexBuffer, offset: 0, index: 0)
            encoder.drawPrimitives(type: .triangle, vertexStart: 0,
                                   vertexCount: vertices.count)
        }
        if let sdf, !sdfFigures.isEmpty, let figureBuffer = slot.sdfFigures,
           let primBuffer = slot.sdfPrims {
            _ = sdfFigures.withUnsafeBytes { memcpy(figureBuffer.contents(), $0.baseAddress!, $0.count) }
            _ = sdfPrims.withUnsafeBytes { memcpy(primBuffer.contents(), $0.baseAddress!, $0.count) }
            encoder.setRenderPipelineState(sdf.pipeline)
            encoder.setDepthStencilState(depthState)
            encoder.setVertexBuffer(figureBuffer, offset: 0, index: 0)
            encoder.setFragmentBuffer(figureBuffer, offset: 0, index: 0)
            encoder.setFragmentBuffer(primBuffer, offset: 0, index: 1)
            for run in sdfViews where run.count > 0 {
                var constants = SdfFigures.constants(camera: run.camera, first: run.first,
                    drawable: drawableSize, stage: stage)
                encoder.setVertexBytes(&constants, length: MemoryLayout<SdfViewConstants>.stride, index: 2)
                encoder.setFragmentBytes(&constants, length: MemoryLayout<SdfViewConstants>.stride, index: 2)
                encoder.drawPrimitives(type: .triangle, vertexStart: 0, vertexCount: 6,
                                       instanceCount: run.count)
            }
        }
        if let decalTexture, !poolDecals.isEmpty {
            _ = poolDecals.withUnsafeBytes { memcpy(decalBuffer.contents(), $0.baseAddress!, $0.count) }
            encoder.setRenderPipelineState(poolDecalPipeline)
            encoder.setDepthStencilState(poolDecalDepth)
            encoder.setVertexBuffer(decalBuffer, offset: 0, index: 0)
            encoder.setFragmentTexture(decalTexture, index: 0)
            encoder.drawPrimitives(type: .triangle, vertexStart: 0, vertexCount: poolDecals.count)
        }
        // Bubbles blend over everything solid in the world, before the HUD.
        if let sdf, !bubbles.isEmpty, let bubbleBuffer = slot.bubbles {
            _ = bubbles.withUnsafeBytes { memcpy(bubbleBuffer.contents(), $0.baseAddress!, $0.count) }
            encoder.setRenderPipelineState(sdf.bubblePipeline)
            encoder.setDepthStencilState(poolDecalDepth)
            encoder.setVertexBuffer(bubbleBuffer, offset: 0, index: 0)
            encoder.setFragmentBuffer(bubbleBuffer, offset: 0, index: 0)
            for run in bubbleViews where run.count > 0 {
                var constants = SdfFigures.constants(camera: run.camera, first: run.first,
                    drawable: drawableSize, stage: stage)
                encoder.setVertexBytes(&constants, length: MemoryLayout<SdfViewConstants>.stride, index: 2)
                encoder.setFragmentBytes(&constants, length: MemoryLayout<SdfViewConstants>.stride, index: 2)
                encoder.drawPrimitives(type: .triangle, vertexStart: 0, vertexCount: 6,
                                       instanceCount: run.count)
            }
        }
        if let ink, let (color, depth) = offscreen {
            encoder.endEncoding()
            guard let screen = command.makeRenderCommandEncoder(descriptor: pass) else { return }
            encoder = screen
            var scale = Float(drawableSize.width / max(1, stage.width))
            encoder.setRenderPipelineState(ink.pipeline)
            encoder.setDepthStencilState(nil)
            encoder.setFragmentTexture(color, index: 0)
            encoder.setFragmentTexture(depth, index: 1)
            encoder.setFragmentBytes(&scale, length: MemoryLayout<Float>.stride, index: 0)
            var sky = SdfViewConstants()
            if let camera = skyCamera {
                sky = SdfFigures.constants(camera: camera, first: 0, drawable: drawableSize, stage: stage)
                sky.stage.z = 1
            } else {
                sky.stage = SIMD4(Float(stage.width), Float(stage.height), 0, 0)
            }
            sky.target.w = skyTime
            encoder.setFragmentBytes(&sky, length: MemoryLayout<SdfViewConstants>.stride, index: 1)
            encoder.drawPrimitives(type: .triangle, vertexStart: 0, vertexCount: 3)
        }
        // Text last and depth-free: the HUD is not in the world, it is over it.
        if !glyphs.isEmpty {
            let capacity = glyphBuffer.length / MemoryLayout<GlyphVertex>.stride
            // Whole quads only. A long line can push past the buffer, and
            // truncating mid-quad would hand the rasterizer a torn triangle
            // rather than one fewer letter.
            let count = min(glyphs.count, capacity) / 6 * 6
            glyphs.withUnsafeBytes { bytes in
                guard let base = bytes.baseAddress else { return }
                memcpy(glyphBuffer.contents(), base,
                       count * MemoryLayout<GlyphVertex>.stride)
            }
            var frame = SIMD2<Float>(Float(stage.width), Float(stage.height))
            encoder.setRenderPipelineState(glyphPipeline)
            encoder.setDepthStencilState(nil)
            encoder.setVertexBuffer(glyphBuffer, offset: 0, index: 0)
            encoder.setVertexBytes(&frame, length: MemoryLayout<SIMD2<Float>>.stride,
                                   index: 1)
            encoder.setFragmentTexture(glyphAtlas.texture, index: 0)
            encoder.drawPrimitives(type: .triangle, vertexStart: 0,
                                   vertexCount: count)
        }
        encoder.endEncoding()
        command.present(drawable)
        let available = inFlight
        command.addCompletedHandler { _ in available.signal() }
        uploadIndex = (uploadIndex + 1) % uploads.count
        submitted = true
        command.commit()
    }

    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {}

    private func clamped(_ channel: Double) -> Double {
        max(0, min(255, channel)) / 255
    }

    private func clamped255(_ channel: Double) -> Double {
        max(0, min(255, channel)) / 255
    }

    private func clampedChannel(_ channel: Float) -> Float {
        max(0, min(255, channel)) / 255
    }
}
