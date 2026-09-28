#include "SdfFigure.hlsli"

struct PixelInput {
  float4 position : SV_POSITION;
  nointerpolation uint figure : FIGURE;
};
struct PixelOutput {
  float4 color : SV_TARGET;
  float depth : SV_DEPTH;
};

// Inigo Quilez's round cone: a capsule whose ends take different radii.
float RoundCone(float3 p, float3 a, float3 b, float r1, float r2) {
  const float3 ba = b - a;
  const float l2 = dot(ba, ba);
  if (l2 < 1e-4) return length(p - a) - max(r1, r2);
  const float rr = r1 - r2, a2 = l2 - rr * rr, il2 = 1.0 / l2;
  const float3 pa = p - a;
  const float y = dot(pa, ba), z = y - l2;
  const float3 q = pa * l2 - ba * y;
  const float x2 = dot(q, q), y2 = y * y * l2, z2 = z * z * l2;
  const float k = sign(rr) * rr * rr * x2;
  if (sign(z) * a2 * z2 > k) return sqrt(x2 + z2) * il2 - r2;
  if (sign(y) * a2 * y2 < k) return sqrt(x2 + y2) * il2 - r1;
  return (sqrt(x2 * a2 * il2) + y * rr) * il2 - r1;
}
float SmoothMin(float a, float b, float k) {
  if (k <= 0.0) return min(a, b);
  const float h = max(k - abs(a - b), 0.0) / k;
  return min(a, b) - h * h * k * 0.25;
}
// Distance to one figure. Body prims melt together; hard prims (eyes, hair
// edges) cut in with a plain union. ink is the nearest prim's colour.
float Figure(float3 p, uint f, out float3 ink) {
  const uint first = uint(figures[f].range.x), count = uint(figures[f].range.y);
  float d = 1e9, best = 1e9;
  ink = float3(1, 1, 1);
  [loop] for (uint i = 0; i < count; ++i) {
    const SdfPrim prim = prims[first + i];
    const float di = RoundCone(p, prim.a.xyz, prim.b.xyz, prim.a.w, prim.b.w);
    if (di < best) { best = di; ink = prim.c.rgb; }
    d = prim.c.w > 0.5 ? min(d, di) : SmoothMin(d, di, depthMap.w);
  }
  return d;
}
float FigureDistance(float3 p, uint f) { float3 ink; return Figure(p, f, ink); }
float2 RaySphere(float3 ro, float3 rd, float4 s) {
  const float3 oc = ro - s.xyz;
  const float b = dot(oc, rd), c = dot(oc, oc) - s.w * s.w, h = b * b - c;
  if (h < 0.0) return float2(1, -1);
  const float q = sqrt(h);
  return float2(-b - q, -b + q);
}

PixelOutput main(PixelInput input) {
  const uint f = input.figure;
  const float4 sphere = figures[f].sphere;
  // The ray through this pixel, linearised at the figure's depth. It is exact
  // for pure perspective and pure orthographic; the game's blend is curved.
  const float2 logical = input.position.xy * (float2(1920, 1080) / target.xy);
  const float sx = logical.x - right.w, sy = up.w - logical.y;
  const float zc = max(ToView(sphere.xyz).z, forward.w);
  const float A = proj.x * (1.0 - proj.z), B = proj.y * proj.z;
  const float denom = A * zc + B, kz = denom / zc;
  const float3 p0 = float3(sx / kz, sy / kz, zc);
  const float3 dv = float3(sx * B / (denom * denom), sy * B / (denom * denom), 1.0);
  const float3 ro = camPos.xyz + right.xyz * p0.x + up.xyz * p0.y + forward.xyz * p0.z;
  const float3 rd = normalize(right.xyz * dv.x + up.xyz * dv.y + forward.xyz * dv.z);
  const float pixel = proj.w / kz;  // world size of one target pixel here

  const float2 span = RaySphere(ro, rd, sphere);
  if (span.y < span.x) discard;
  float t = span.x, minD = 1e9, minT = t, hitT = 0;
  bool hit = false;
  [loop] for (int i = 0; i < 64 && t < span.y; ++i) {
    const float d = FigureDistance(ro + rd * t, f);
    if (d < minD) { minD = d; minT = t; }
    if (d < pixel * 0.25) { hit = true; hitT = t; break; }
    t += d;
  }
  bool ink = false;
  if (!hit) {
    // A near miss inside the outline width becomes a silhouette line.
    if (minD >= depthMap.z * pixel) discard;
    ink = true; hitT = minT;
  }
  const float3 p = ro + rd * hitT;
  const float viewZ = dot(p - camPos.xyz, forward.xyz);
  if (viewZ < forward.w) discard;

  float3 color = float3(20, 17, 28) / 255.0;
  if (!ink) {
    float3 base;
    Figure(p, f, base);
    const float e = max(pixel * 0.5, 0.25);
    const float3 n = normalize(
      float3(1, -1, -1) * FigureDistance(p + float3(e, -e, -e), f) +
      float3(-1, -1, 1) * FigureDistance(p + float3(-e, -e, e), f) +
      float3(-1, 1, -1) * FigureDistance(p + float3(-e, e, -e), f) +
      float3(1, 1, 1) * FigureDistance(p + float3(e, e, e), f));
    // The same three cel bands the triangle figures bake into their meshes.
    const float light = n.x * .35 - n.y * .8 - n.z * .45;
    const float band = light > .35 ? 1.0 : light > -.25 ? .8 : .55;
    float occlusion = 0;
    [unroll] for (int k = 1; k <= 3; ++k) {
      const float h = k * 5.0;
      occlusion += (h - FigureDistance(p + n * h, f)) / h;
    }
    occlusion = clamp(1.0 - occlusion * .18, .6, 1.0);
    const float rim = pow(1.0 - saturate(dot(n, -rd)), 3.0);
    color = base * band * occlusion + rim * .22;
  }
  PixelOutput output;
  output.color = float4(color, 1.0);
  const float depth = clamp(depthMap.x + viewZ * depthMap.y, -1.499, 1.4);
  output.depth = saturate((depth + 1.5) / 3.0);
  return output;
}
