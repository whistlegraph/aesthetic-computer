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
    struct SkyView { float4 camPos, right, up, forward, proj, depthMap, viewport, target, stage; };
    static float hash(float2 p) { return fract(sin(dot(p, float2(127.1, 311.7))) * 43758.5453); }
    static float noise(float2 p) {
      float2 i = floor(p), f = fract(p), u = f * f * (3.0 - 2.0 * f);
      return mix(mix(hash(i), hash(i + float2(1, 0)), u.x), mix(hash(i + float2(0, 1)), hash(i + float2(1, 1)), u.x), u.y);
    }
    static float fbm(float2 p) {
      float v = 0.0, a = 0.5;
      for (int i = 0; i < 5; ++i) { v += a * noise(p); p = p * 2.03 + 17.1; a *= 0.5; }
      return v;
    }
    // The sky, in the game's y-down world: elevation is -rd.y. Cel-banded
    // clouds with an ink rim keep it in the same drawn language as the park.
    static float3 skyColor(float3 rd, float time) {
      float e = -rd.y;
      float3 zenith = float3(0.30, 0.55, 0.90), horizon = float3(0.80, 0.88, 0.95);
      float3 haze = float3(0.93, 0.86, 0.78), ground = float3(0.62, 0.66, 0.70);
      float3 color = e >= 0.0 ? mix(horizon, zenith, pow(saturate(e), 0.5)) : mix(horizon, ground, saturate(-e * 5.0));
      color = mix(color, haze, exp(-abs(e) * 18.0) * 0.55);
      float3 sun = normalize(float3(0.35, -0.8, -0.45));
      float s = max(dot(rd, sun), 0.0);
      color += float3(1.0, 0.86, 0.6) * (pow(s, 12.0) * 0.18 + pow(s, 180.0) * 0.5);
      color = mix(color, float3(1.0, 0.97, 0.88), smoothstep(0.9993, 0.9996, s));
      if (e > 0.015) {
        float2 uv = rd.xz / e * 0.35 + float2(time * 0.012, time * 0.004);
        float n = fbm(uv);
        float body = smoothstep(0.52, 0.56, n), shade = smoothstep(0.64, 0.68, n);
        float fade = smoothstep(0.015, 0.2, e);
        float3 cloud = mix(float3(0.80, 0.84, 0.92), float3(1.0, 0.99, 0.96), shade);
        float rim = (smoothstep(0.50, 0.52, n) - smoothstep(0.52, 0.54, n)) * fade;
        color = mix(color, cloud, body * fade);
        color = mix(color, float3(20.0, 17.0, 28.0) / 255.0, rim * 0.55);
      }
      return color;
    }
    fragment float4 ink_fragment(InkRaster in [[stage_in]],
                                 texture2d<float> scene [[texture(0)]],
                                 depth2d<float> depth [[texture(1)]],
                                 constant float& scale [[buffer(0)]],
                                 constant SkyView& v [[buffer(1)]]) {
      uint2 size = uint2(scene.get_width(), scene.get_height());
      int2 p = int2(in.position.xy);
      int w = max(1, int(round(scale)));
      auto at = [&](int2 q) { return depth.read(uint2(clamp(q, int2(0), int2(size) - 1))); };
      float c = at(p);
      float3 color = scene.read(uint2(p)).rgb;
      // Behind everything solid: the cleared background always, and in a
      // perspective scene the piece's flat backdrop too.
      bool perspective = v.stage.z > 0.5 && v.proj.z > 0.5;
      if (c >= 0.999 || (perspective && c >= 0.975)) {
        float2 logical = in.position.xy * (v.stage.xy / float2(size));
        float3 rd;
        if (perspective) {
          float sx = logical.x - v.right.w, sy = v.up.w - logical.y;
          rd = normalize(v.right.xyz * (sx / v.proj.y) + v.up.xyz * (sy / v.proj.y) + v.forward.xyz);
        } else {
          rd = normalize(float3(0.0, -(0.55 - logical.y / max(1.0, v.stage.y)), 1.0));
        }
        return float4(skyColor(rd, v.target.w), 1.0);
      }
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
