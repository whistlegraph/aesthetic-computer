import AppKit
import MetalKit
import AirbrushCore

private struct GPUStamp {
    var geometry: SIMD4<Float>
    var color: SIMD4<Float>
}

final class PaintRenderer {
    let device: MTLDevice
    let queue: MTLCommandQueue
    private let pipeline: MTLRenderPipelineState
    private let tilePipeline: MTLRenderPipelineState
    static let paper = MTLClearColorMake(0.965, 0.953, 0.925, 1)

    init() throws {
        guard let device = MTLCreateSystemDefaultDevice(), let queue = device.makeCommandQueue() else {
            throw NSError(domain: "Airbrush", code: 1, userInfo: [NSLocalizedDescriptionKey: "Metal is unavailable."])
        }
        self.device = device
        self.queue = queue
        let options = MTLCompileOptions()
        options.fastMathEnabled = false
        let library = try device.makeLibrary(source: Self.shader, options: options)
        let descriptor = MTLRenderPipelineDescriptor()
        descriptor.vertexFunction = library.makeFunction(name: "paintVertex")
        descriptor.fragmentFunction = library.makeFunction(name: "paintFragment")
        let color = descriptor.colorAttachments[0]!
        color.pixelFormat = .bgra8Unorm
        color.isBlendingEnabled = true
        color.sourceRGBBlendFactor = .one
        color.destinationRGBBlendFactor = .oneMinusSourceAlpha
        color.sourceAlphaBlendFactor = .one
        color.destinationAlphaBlendFactor = .oneMinusSourceAlpha
        pipeline = try device.makeRenderPipelineState(descriptor: descriptor)
        descriptor.vertexFunction = library.makeFunction(name: "tileVertex")
        descriptor.fragmentFunction = library.makeFunction(name: "tileFragment")
        color.isBlendingEnabled = true // Opaque tiles and premultiplied live ink.
        tilePipeline = try device.makeRenderPipelineState(descriptor: descriptor)
    }

    // Subtract camera origin in Double before passing screen-local floats to
    // Metal. Stored world coordinates never go through the viewport transform.
    func encode(dabs: [Dab], camera: Camera, size: Point, pixels: CGSize,
                pass: MTLRenderPassDescriptor, command: MTLCommandBuffer, clear: Bool = true,
                transparent: Bool = false) {
        pass.colorAttachments[0].loadAction = clear ? .clear : .load
        pass.colorAttachments[0].storeAction = .store
        pass.colorAttachments[0].clearColor = transparent ? MTLClearColorMake(0, 0, 0, 0) : Self.paper
        guard let encoder = command.makeRenderCommandEncoder(descriptor: pass) else { return }
        defer { encoder.endEncoding() }
        encoder.setRenderPipelineState(pipeline)
        var viewport = SIMD2<Float>(Float(pixels.width), Float(pixels.height))
        encoder.setVertexBytes(&viewport, length: MemoryLayout<SIMD2<Float>>.stride, index: 1)
        let scale = pixels.width / max(1, size.x)
        var stamps: [GPUStamp] = []
        stamps.reserveCapacity(min(dabs.count, 65_536))
        func flush() {
            guard !stamps.isEmpty else { return }
            stamps.withUnsafeBytes { bytes in
                guard let base = bytes.baseAddress,
                      let buffer = device.makeBuffer(bytes: base, length: bytes.count, options: .storageModeShared) else { return }
                encoder.setVertexBuffer(buffer, offset: 0, index: 0)
                encoder.drawPrimitives(type: .triangle, vertexStart: 0, vertexCount: 6, instanceCount: stamps.count)
            }
            stamps.removeAll(keepingCapacity: true)
        }
        for dab in dabs {
            let p = camera.screen(dab.point, size: size)
            let r = dab.radius * camera.zoom
            guard p.x + r >= -1, p.y + r >= -1, p.x - r <= size.x + 1,
                  p.y - r <= size.y + 1, dab.opacity > 0 else { continue }
            stamps.append(GPUStamp(
                geometry: SIMD4(Float(p.x * scale), Float(p.y * scale), Float(r * scale), Float(dab.brush.hardness)),
                color: SIMD4(Float(dab.brush.red), Float(dab.brush.green), Float(dab.brush.blue), Float(dab.opacity))))
            if stamps.count == 65_536 { flush() }
        }
        flush()
    }

    struct TileQuad {
        let texture: MTLTexture
        let rect: SIMD4<Float>
        var uv = SIMD4<Float>(0, 0, 1, 1)
    }
    func composite(_ tiles: [TileQuad], pixels: CGSize,
                   pass: MTLRenderPassDescriptor, command: MTLCommandBuffer) {
        pass.colorAttachments[0].loadAction = .clear
        pass.colorAttachments[0].storeAction = .store
        pass.colorAttachments[0].clearColor = Self.paper
        guard let encoder = command.makeRenderCommandEncoder(descriptor: pass) else { return }
        encoder.setRenderPipelineState(tilePipeline)
        var viewport = SIMD2<Float>(Float(pixels.width), Float(pixels.height))
        encoder.setVertexBytes(&viewport, length: MemoryLayout<SIMD2<Float>>.stride, index: 1)
        for tile in tiles {
            var parameters = [tile.rect, tile.uv]
            parameters.withUnsafeMutableBytes { bytes in
                encoder.setVertexBytes(bytes.baseAddress!, length: bytes.count, index: 0)
            }
            encoder.setFragmentTexture(tile.texture, index: 0)
            encoder.drawPrimitives(type: .triangle, vertexStart: 0, vertexCount: 6)
        }
        encoder.endEncoding()
    }

    func snapshot(dabs: [Dab], camera: Camera, size: Point, width: Int, height: Int) throws -> Data {
        let descriptor = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .bgra8Unorm,
            width: width, height: height, mipmapped: false)
        descriptor.storageMode = .shared
        descriptor.usage = [.renderTarget]
        guard let texture = device.makeTexture(descriptor: descriptor), let command = queue.makeCommandBuffer() else {
            throw NSError(domain: "Airbrush", code: 2, userInfo: [NSLocalizedDescriptionKey: "Could not allocate export."])
        }
        let pass = MTLRenderPassDescriptor()
        pass.colorAttachments[0].texture = texture
        encode(dabs: dabs, camera: camera, size: size,
               pixels: CGSize(width: width, height: height), pass: pass, command: command)
        command.commit()
        command.waitUntilCompleted()
        if let error = command.error { throw error }
        var bytes = Data(count: width * height * 4)
        bytes.withUnsafeMutableBytes { raw in
            texture.getBytes(raw.baseAddress!, bytesPerRow: width * 4,
                from: MTLRegionMake2D(0, 0, width, height), mipmapLevel: 0)
        }
        return bytes
    }

    static func png(_ pixels: Data, width: Int, height: Int) throws -> Data {
        let provider = CGDataProvider(data: pixels as CFData)!
        let info = CGBitmapInfo.byteOrder32Little.union(CGBitmapInfo(rawValue: CGImageAlphaInfo.premultipliedFirst.rawValue))
        guard let cg = CGImage(width: width, height: height, bitsPerComponent: 8, bitsPerPixel: 32,
            bytesPerRow: width * 4, space: CGColorSpaceCreateDeviceRGB(), bitmapInfo: info,
            provider: provider, decode: nil, shouldInterpolate: false, intent: .defaultIntent),
              let png = NSBitmapImageRep(cgImage: cg).representation(using: .png, properties: [:]) else {
            throw NSError(domain: "Airbrush", code: 3, userInfo: [NSLocalizedDescriptionKey: "Could not encode PNG."])
        }
        return png
    }

    static let shader = """
    #include <metal_stdlib>
    using namespace metal;
    struct Stamp { float4 geometry; float4 color; };
    struct Out {
        float4 position [[position]];
        float2 local;
        float radius;
        float hardness;
        float4 color;
    };
    vertex Out paintVertex(uint vertexIndex [[vertex_id]], uint instance [[instance_id]],
                           const device Stamp *stamps [[buffer(0)]],
                           constant float2 &viewport [[buffer(1)]]) {
        const float2 corners[6] = {float2(-1,-1),float2(1,-1),float2(-1,1),
                                   float2(-1,1),float2(1,-1),float2(1,1)};
        Stamp s = stamps[instance];
        float r = max(s.geometry.z, 0.001f);
        float2 local = corners[vertexIndex] * (r + 1.0f);
        float2 pixel = s.geometry.xy + local;
        Out out;
        out.position = float4(pixel.x / viewport.x * 2 - 1, 1 - pixel.y / viewport.y * 2, 0, 1);
        out.local = local;
        out.radius = r;
        out.hardness = s.geometry.w;
        out.color = s.color;
        return out;
    }
    fragment float4 paintFragment(Out in [[stage_in]]) {
        float distance = length(in.local);
        float coverage = 1 - smoothstep(in.radius - 0.5f, in.radius + 0.5f, distance);
        float soft = 1 - smoothstep(in.hardness * in.radius, in.radius, distance);
        float profile = in.hardness >= 0.999f ? coverage : soft * coverage;
        float alpha = in.color.a * profile;
        return float4(in.color.rgb * alpha, alpha);
    }
    struct TileOut { float4 position [[position]]; float2 uv; };
    vertex TileOut tileVertex(uint i [[vertex_id]], constant float4 *p [[buffer(0)]],
                              constant float2 &viewport [[buffer(1)]]) {
        const float2 corners[6] = {float2(0,0),float2(1,0),float2(0,1),
                                   float2(0,1),float2(1,0),float2(1,1)};
        float2 pixel = p[0].xy + corners[i] * p[0].zw;
        TileOut out;
        out.position = float4(pixel.x / viewport.x * 2 - 1, 1 - pixel.y / viewport.y * 2, 0, 1);
        out.uv = p[1].xy + corners[i] * p[1].zw;
        return out;
    }
    fragment float4 tileFragment(TileOut in [[stage_in]], texture2d<float> tile [[texture(0)]]) {
        constexpr sampler s(coord::normalized, address::clamp_to_edge, filter::linear);
        return tile.sample(s, in.uv);
    }
    """
}
