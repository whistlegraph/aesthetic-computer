import MetalKit

/// One ink line for the whole picture. The scene renders offscreen; this pass
/// copies it to the drawable and draws ink wherever depth jumps — a body in
/// front of a wall, a ramp's lip against the floor, the park against the sky —
/// so SDF figures, triangle meshes and props share a single outline instead of
/// each carrying its own. A second difference (left + right − 2·centre) marks
/// discontinuities and ignores smooth slopes, which a plain gradient would
/// shade in as false lines at grazing angles.
final class InkOutlines {
    let pipeline: MTLRenderPipelineState
    private(set) var color: MTLTexture?
    private(set) var depth: MTLTexture?

    init(device: MTLDevice) throws {
        let library = try device.makeLibrary(source: Self.source, options: nil)
        let descriptor = MTLRenderPipelineDescriptor()
        descriptor.vertexFunction = library.makeFunction(name: "ink_vertex")
        descriptor.fragmentFunction = library.makeFunction(name: "ink_fragment")
        descriptor.colorAttachments[0].pixelFormat = .bgra8Unorm
        descriptor.depthAttachmentPixelFormat = .depth32Float
        pipeline = try device.makeRenderPipelineState(descriptor: descriptor)
    }

    /// Offscreen targets matching the drawable; rebuilt only on resize.
    func targets(device: MTLDevice, size: CGSize) -> (MTLTexture, MTLTexture)? {
        let width = max(1, Int(size.width)), height = max(1, Int(size.height))
        if let color, let depth, color.width == width, color.height == height { return (color, depth) }
        let colorDescriptor = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .bgra8Unorm,
            width: width, height: height, mipmapped: false)
        colorDescriptor.usage = [.renderTarget, .shaderRead]
        colorDescriptor.storageMode = .private
        let depthDescriptor = MTLTextureDescriptor.texture2DDescriptor(pixelFormat: .depth32Float,
            width: width, height: height, mipmapped: false)
        depthDescriptor.usage = [.renderTarget, .shaderRead]
        depthDescriptor.storageMode = .private
        guard let newColor = device.makeTexture(descriptor: colorDescriptor),
              let newDepth = device.makeTexture(descriptor: depthDescriptor) else { return nil }
        color = newColor; depth = newDepth
        return (newColor, newDepth)
    }

    static let source = """
    #include <metal_stdlib>
    using namespace metal;
    struct InkRaster { float4 position [[position]]; };
    vertex InkRaster ink_vertex(uint id [[vertex_id]]) {
      float2 p = float2(float((id << 1) & 2), float(id & 2));
      InkRaster out; out.position = float4(p * 2.0 - 1.0, 0.0, 1.0); return out;
    }
    fragment float4 ink_fragment(InkRaster in [[stage_in]],
                                 texture2d<float> scene [[texture(0)]],
                                 depth2d<float> depth [[texture(1)]],
                                 constant float& scale [[buffer(0)]]) {
      uint2 size = uint2(scene.get_width(), scene.get_height());
      int2 p = int2(in.position.xy);
      int w = max(1, int(round(scale)));
      auto at = [&](int2 q) { return depth.read(uint2(clamp(q, int2(0), int2(size) - 1))); };
      float c = at(p);
      float3 color = scene.read(uint2(p)).rgb;
      // HUD furniture sits in front of everything (depth near 0): leave it be.
      if (c < 0.02) return float4(color, 1.0);
      float horizontal = abs(at(p + int2(w, 0)) + at(p - int2(w, 0)) - 2.0 * c);
      float vertical = abs(at(p + int2(0, w)) + at(p - int2(0, w)) - 2.0 * c);
      float edge = max(horizontal, vertical);
      // ~40 world units of depth break at the game's depth scale.
      float ink = smoothstep(0.0012, 0.0030, edge);
      return float4(mix(color, float3(20.0, 17.0, 28.0) / 255.0, ink), 1.0);
    }
    """
}
