import MetalKit

/// Raymarched figures (sceneApi 3), the Metal twin of
/// xbox/native-bios/SdfPixelShader.hlsl. The piece sends each figure as a list
/// of world-space round cones plus the game's own 27-float native camera; each
/// figure draws as one screen rectangle whose pixels march only that figure
/// and write true depth beside the triangle world.
struct SdfViewConstants {
    var camPos = SIMD4<Float>(), right = SIMD4<Float>(), up = SIMD4<Float>()
    var forward = SIMD4<Float>(), proj = SIMD4<Float>(), depthMap = SIMD4<Float>()
    var viewport = SIMD4<Float>(), target = SIMD4<Float>(), stage = SIMD4<Float>()
}

struct SdfFigureGpu {
    var sphere: SIMD4<Float>
    var range: SIMD4<Float>
}

final class SdfFigures {
    static let maxFigures = 256, maxPrims = 8_192
    static let outlinePx: Float = 1.6, blend: Float = 6

    let pipeline: MTLRenderPipelineState
    /// Bubbles blend over the scene: analytic spheres with a fresnel rim and
    /// thin-film colour, depth-tested but never written.
    let bubblePipeline: MTLRenderPipelineState
    private(set) var bubbles: [SdfFigureGpu] = []
    private(set) var bubbleViews: [(camera: [Float], first: Int, count: Int)] = []
    private(set) var figures: [SdfFigureGpu] = []
    private(set) var prims: [Float] = []
    /// One entry per run of figures that share a camera: the P1 inset
    /// interleaves a second view, and each run is one instanced draw.
    private(set) var views: [(camera: [Float], first: Int, count: Int)] = []

    init(device: MTLDevice) throws {
        let library = try device.makeLibrary(source: Self.source, options: nil)
        let descriptor = MTLRenderPipelineDescriptor()
        descriptor.vertexFunction = library.makeFunction(name: "sdf_vertex")
        descriptor.fragmentFunction = library.makeFunction(name: "sdf_fragment")
        descriptor.colorAttachments[0].pixelFormat = .bgra8Unorm
        descriptor.depthAttachmentPixelFormat = .depth32Float
        pipeline = try device.makeRenderPipelineState(descriptor: descriptor)
        descriptor.fragmentFunction = library.makeFunction(name: "bubble_fragment")
        let blend = descriptor.colorAttachments[0]!
        blend.isBlendingEnabled = true
        blend.sourceRGBBlendFactor = .one
        blend.destinationRGBBlendFactor = .oneMinusSourceAlpha
        blend.sourceAlphaBlendFactor = .one
        blend.destinationAlphaBlendFactor = .oneMinusSourceAlpha
        bubblePipeline = try device.makeRenderPipelineState(descriptor: descriptor)
    }

    /// One bubble: world centre, radius and tint (0–255), in the given camera.
    func queueBubble(camera: UnsafePointer<Float>, x: Float, y: Float, z: Float,
                     radius: Float, tint: SIMD3<Float>) -> Bool {
        guard bubbles.count < 32, [x, y, z, radius].allSatisfy(\.isFinite),
              radius > 0, radius < 4096 else { return false }
        for i in 0..<27 where !camera[i].isFinite { return false }
        let view = Array(UnsafeBufferPointer(start: camera, count: 27))
        if bubbleViews.last?.camera != view { bubbleViews.append((view, bubbles.count, 0)) }
        bubbles.append(SdfFigureGpu(sphere: SIMD4(x, y, z, radius),
            range: SIMD4(tint / 255, 0)))
        bubbleViews[bubbleViews.count - 1].count += 1
        return true
    }

    func clear() {
        figures.removeAll(keepingCapacity: true)
        prims.removeAll(keepingCapacity: true)
        views.removeAll(keepingCapacity: true)
        bubbles.removeAll(keepingCapacity: true)
        bubbleViews.removeAll(keepingCapacity: true)
    }

    /// camera: 27 floats. values: count × 12 floats (a.xyz r1, b.xyz r2, rgb, hard).
    func queue(camera: UnsafePointer<Float>, values: UnsafePointer<Float>, count: Int) -> Bool {
        guard count > 0, count <= 48, figures.count < Self.maxFigures,
              prims.count / 12 + count <= Self.maxPrims else { return false }
        for i in 0..<27 where !camera[i].isFinite { return false }
        for i in 0..<count * 12 where !values[i].isFinite || abs(values[i]) > 1e6 { return false }
        let view = Array(UnsafeBufferPointer(start: camera, count: 27))
        if views.last?.camera != view { views.append((view, figures.count, 0)) }
        var lo = SIMD3<Float>(repeating: .greatestFiniteMagnitude), hi = -lo
        for i in 0..<count {
            let p = values + i * 12
            for end in 0..<2 {
                let c = p + end * 4
                let r = max(0, min(512, c[3]))
                let point = SIMD3(c[0], c[1], c[2])
                lo = simd_min(lo, point - r); hi = simd_max(hi, point + r)
            }
        }
        let center = (lo + hi) * 0.5
        let radius = simd_length(hi - lo) * 0.5 + Self.blend + 2
        figures.append(SdfFigureGpu(sphere: SIMD4(center, radius),
            range: SIMD4(Float(prims.count / 12), Float(count), 0, 0)))
        prims.append(contentsOf: UnsafeBufferPointer(start: values, count: count * 12))
        views[views.count - 1].count += 1
        return true
    }

    static func constants(camera m: [Float], first: Int, drawable: CGSize, stage: CGSize) -> SdfViewConstants {
        SdfViewConstants(
            camPos: SIMD4(m[0], m[1], m[2], 0), right: SIMD4(m[3], m[4], m[5], m[12]),
            up: SIMD4(m[6], m[7], m[8], m[13]), forward: SIMD4(m[9], m[10], m[11], m[17]),
            proj: SIMD4(m[14], m[15], m[16], Float(stage.width / max(1, drawable.width))),
            depthMap: SIMD4(m[22], m[23], outlinePx, blend), viewport: SIMD4(m[18], m[19], m[20], m[21]),
            target: SIMD4(Float(drawable.width), Float(drawable.height), Float(first), 0),
            stage: SIMD4(Float(stage.width), Float(stage.height), 0, 0))
    }

    static let source = """
    #include <metal_stdlib>
    using namespace metal;
    struct SdfView { float4 camPos, right, up, forward, proj, depthMap, viewport, target, stage; };
    struct SdfFigureData { float4 sphere; float4 range; };
    struct SdfPrim { float4 a; float4 b; float4 c; };
    struct SdfRaster { float4 position [[position]]; uint figure [[flat]]; };
    struct SdfOut { float4 color [[color(0)]]; float depth [[depth(any)]]; };

    static float3 toView(constant SdfView& v, float3 p) {
      float3 d = p - v.camPos.xyz;
      return float3(dot(d, v.right.xyz), dot(d, v.up.xyz), dot(d, v.forward.xyz));
    }
    static float scaleAt(constant SdfView& v, float z) {
      return v.proj.x + (v.proj.y / max(z, 1e-3) - v.proj.x) * v.proj.z;
    }
    vertex SdfRaster sdf_vertex(uint vid [[vertex_id]], uint iid [[instance_id]],
                                const device SdfFigureData* figures [[buffer(0)]],
                                constant SdfView& v [[buffer(2)]]) {
      SdfRaster out;
      uint f = uint(v.target.z) + iid;
      out.figure = f;
      float4 s = figures[f].sphere;
      float3 c = toView(v, s.xyz);
      float2 lo = v.viewport.xy, hi = v.viewport.zw;
      if (c.z - s.w > v.forward.w) {
        float kc = scaleAt(v, c.z), kn = scaleAt(v, c.z - s.w);
        float2 center = float2(v.right.w + c.x * kc, v.up.w - c.y * kc);
        float2 extent = abs(c.xy) * abs(kn - kc) + s.w * kn + v.depthMap.z + 4.0;
        lo = max(lo, center - extent); hi = min(hi, center + extent);
      }
      const uint corner[6] = {0, 1, 2, 2, 1, 3};
      uint k = corner[vid % 6];
      float2 p = float2((k & 1) ? hi.x : lo.x, (k & 2) ? hi.y : lo.y);
      out.position = float4(p.x / (v.stage.x * 0.5) - 1.0, 1.0 - p.y / (v.stage.y * 0.5), 0.0, 1.0);
      return out;
    }
    static float roundCone(float3 p, float3 a, float3 b, float r1, float r2) {
      float3 ba = b - a; float l2 = dot(ba, ba);
      if (l2 < 1e-4) return length(p - a) - max(r1, r2);
      float rr = r1 - r2, a2 = l2 - rr * rr, il2 = 1.0 / l2;
      float3 pa = p - a; float y = dot(pa, ba), z = y - l2;
      float3 q = pa * l2 - ba * y;
      float x2 = dot(q, q), y2 = y * y * l2, z2 = z * z * l2;
      float k = sign(rr) * rr * rr * x2;
      if (sign(z) * a2 * z2 > k) return sqrt(x2 + z2) * il2 - r2;
      if (sign(y) * a2 * y2 < k) return sqrt(x2 + y2) * il2 - r1;
      return (sqrt(x2 * a2 * il2) + y * rr) * il2 - r1;
    }
    static float smoothMin(float a, float b, float k) {
      if (k <= 0.0) return min(a, b);
      float h = max(k - abs(a - b), 0.0) / k;
      return min(a, b) - h * h * k * 0.25;
    }
    static float figureDistance(float3 p, uint f, const device SdfFigureData* figures,
                                const device SdfPrim* prims, float k, thread float3& ink) {
      uint first = uint(figures[f].range.x), count = uint(figures[f].range.y);
      float d = 1e9, best = 1e9; ink = float3(1.0);
      for (uint i = 0; i < count; ++i) {
        SdfPrim prim = prims[first + i];
        float di = roundCone(p, prim.a.xyz, prim.b.xyz, prim.a.w, prim.b.w);
        if (di < best) { best = di; ink = prim.c.rgb; }
        d = prim.c.w > 0.5 ? min(d, di) : smoothMin(d, di, k);
      }
      return d;
    }
    // The bubble: one analytic sphere per instance, range.xyz its tint.
    fragment SdfOut bubble_fragment(SdfRaster in [[stage_in]],
                                    const device SdfFigureData* bubbles [[buffer(0)]],
                                    constant SdfView& v [[buffer(2)]]) {
      float4 sphere = bubbles[in.figure].sphere;
      float3 tint = bubbles[in.figure].range.xyz;
      float2 logical = in.position.xy * (v.stage.xy / v.target.xy);
      float sx = logical.x - v.right.w, sy = v.up.w - logical.y;
      float zc = max(toView(v, sphere.xyz).z, v.forward.w);
      float A = v.proj.x * (1.0 - v.proj.z), B = v.proj.y * v.proj.z;
      float denom = A * zc + B, kz = denom / zc;
      float3 p0 = float3(sx / kz, sy / kz, zc);
      float3 dv = float3(sx * B / (denom * denom), sy * B / (denom * denom), 1.0);
      float3 ro = v.camPos.xyz + v.right.xyz * p0.x + v.up.xyz * p0.y + v.forward.xyz * p0.z;
      float3 rd = normalize(v.right.xyz * dv.x + v.up.xyz * dv.y + v.forward.xyz * dv.z);
      float3 oc = ro - sphere.xyz;
      float b = dot(oc, rd), h = b * b - (dot(oc, oc) - sphere.w * sphere.w);
      if (h < 0.0) discard_fragment();
      float t = -b - sqrt(h);
      float3 p = ro + rd * t, n = normalize(p - sphere.xyz);
      float facing = abs(dot(n, rd));
      float fresnel = pow(1.0 - facing, 2.2);
      // Thin film: the rim runs through the spectrum as its thickness changes.
      float film = fresnel * 1.6 + dot(n, float3(0.3, 0.6, 0.2));
      float3 iridescent = 0.5 + 0.5 * cos(6.2831 * (film + float3(0.0, 0.33, 0.67)));
      float3 light = normalize(float3(0.35, -0.8, -0.45));
      float glint = pow(max(dot(reflect(rd, n), light), 0.0), 48.0);
      float3 color = mix(tint, iridescent, 0.55) * (0.35 + fresnel) + glint;
      float alpha = clamp(0.1 + fresnel * 0.85 + glint, 0.0, 1.0);
      float viewZ = dot(p - v.camPos.xyz, v.forward.xyz);
      if (viewZ < v.forward.w) discard_fragment();
      SdfOut out;
      out.color = float4(color * alpha, alpha);
      float depth = clamp(v.depthMap.x + viewZ * v.depthMap.y, -1.499, 1.4);
      out.depth = saturate((depth + 1.5) / 3.0);
      return out;
    }
    fragment SdfOut sdf_fragment(SdfRaster in [[stage_in]],
                                 const device SdfFigureData* figures [[buffer(0)]],
                                 const device SdfPrim* prims [[buffer(1)]],
                                 constant SdfView& v [[buffer(2)]]) {
      uint f = in.figure;
      float4 sphere = figures[f].sphere;
      float k = v.depthMap.w;
      float2 logical = in.position.xy * (v.stage.xy / v.target.xy);
      float sx = logical.x - v.right.w, sy = v.up.w - logical.y;
      float zc = max(toView(v, sphere.xyz).z, v.forward.w);
      float A = v.proj.x * (1.0 - v.proj.z), B = v.proj.y * v.proj.z;
      float denom = A * zc + B, kz = denom / zc;
      float3 p0 = float3(sx / kz, sy / kz, zc);
      float3 dv = float3(sx * B / (denom * denom), sy * B / (denom * denom), 1.0);
      float3 ro = v.camPos.xyz + v.right.xyz * p0.x + v.up.xyz * p0.y + v.forward.xyz * p0.z;
      float3 rd = normalize(v.right.xyz * dv.x + v.up.xyz * dv.y + v.forward.xyz * dv.z);
      float pixel = v.proj.w / kz;
      float3 oc = ro - sphere.xyz;
      float b = dot(oc, rd), h = b * b - (dot(oc, oc) - sphere.w * sphere.w);
      if (h < 0.0) discard_fragment();
      float q = sqrt(h), t = -b - q, tEnd = -b + q;
      float minD = 1e9, minT = t, hitT = 0; bool hit = false;
      float3 ink;
      for (int i = 0; i < 64 && t < tEnd; ++i) {
        float d = figureDistance(ro + rd * t, f, figures, prims, k, ink);
        if (d < minD) { minD = d; minT = t; }
        if (d < pixel * 0.25) { hit = true; hitT = t; break; }
        t += d;
      }
      bool outline = false;
      if (!hit) {
        if (minD >= v.depthMap.z * pixel) discard_fragment();
        outline = true; hitT = minT;
      }
      float3 p = ro + rd * hitT;
      float viewZ = dot(p - v.camPos.xyz, v.forward.xyz);
      if (viewZ < v.forward.w) discard_fragment();
      float3 color = float3(20.0, 17.0, 28.0) / 255.0;
      if (!outline) {
        float3 base; figureDistance(p, f, figures, prims, k, base);
        float e = max(pixel * 0.5, 0.25);
        float3 unused;
        float3 n = normalize(
          float3(1, -1, -1) * figureDistance(p + float3(e, -e, -e), f, figures, prims, k, unused) +
          float3(-1, -1, 1) * figureDistance(p + float3(-e, -e, e), f, figures, prims, k, unused) +
          float3(-1, 1, -1) * figureDistance(p + float3(-e, e, -e), f, figures, prims, k, unused) +
          float3(1, 1, 1) * figureDistance(p + float3(e, e, e), f, figures, prims, k, unused));
        float light = n.x * 0.35 - n.y * 0.8 - n.z * 0.45;
        float band = light > 0.35 ? 1.0 : light > -0.25 ? 0.8 : 0.55;
        float occlusion = 0.0;
        for (int j = 1; j <= 3; ++j) {
          float hh = float(j) * 5.0;
          occlusion += (hh - figureDistance(p + n * hh, f, figures, prims, k, unused)) / hh;
        }
        occlusion = clamp(1.0 - occlusion * 0.18, 0.6, 1.0);
        float rim = pow(1.0 - saturate(dot(n, -rd)), 3.0);
        color = base * band * occlusion + rim * 0.22;
      }
      SdfOut out;
      out.color = float4(color, 1.0);
      float depth = clamp(v.depthMap.x + viewZ * v.depthMap.y, -1.499, 1.4);
      out.depth = saturate((depth + 1.5) / 3.0);
      return out;
    }
    """
}
