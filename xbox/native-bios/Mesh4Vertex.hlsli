// The vertex stage for both mesh kinds; Mesh4SkinVertexShader.hlsl defines
// MESH4_SKINNED first.
#include "Mesh4.hlsli"

StructuredBuffer<Mesh4Instance> instances : register(t0);
StructuredBuffer<float4> bones : register(t3);  // three rows a bone

struct Mesh4Vertex {
  float3 position : POSITION;
  float4 normal : NORMAL;
  float4 color : COLOR0;
  float2 uv : TEXCOORD0;
  float2 uv2 : TEXCOORD1;
#ifdef MESH4_SKINNED
  uint4 joints : BLENDINDICES;
  float4 weights : BLENDWEIGHT;
#endif
};

float3 Row3(float4 r0, float4 r1, float4 r2, float4 p) { return float3(dot(r0, p), dot(r1, p), dot(r2, p)); }

Mesh4Pixel main(Mesh4Vertex v, uint instanceId : SV_InstanceID) {
  const Mesh4Instance inst = instances[info.x + instanceId];
  float3 p = v.position, n = v.normal.xyz;
#ifdef MESH4_SKINNED
  float3 sp = 0, sn = 0;
  [unroll] for (int k = 0; k < 4; ++k) {
    const float w = v.weights[k];
    if (w > 0) {
      const uint b = (info.y + v.joints[k]) * 3;
      sp += w * Row3(bones[b], bones[b + 1], bones[b + 2], float4(p, 1));
      sn += w * Row3(bones[b], bones[b + 1], bones[b + 2], float4(n, 0));
    }
  }
  p = sp; n = sn;
#endif
  const float3 world = Row3(inst.r0, inst.r1, inst.r2, float4(p, 1));
  const float3 d = world - camPos.xyz;
  const float3 view = float3(dot(d, right.xyz), dot(d, up.xyz), dot(d, forward.xyz));
  const float depth = clamp(depthMap.x + view.z * depthMap.y, depthMap.z, depthMap.w);
  const float zn = (depth + 1.5) / 3.0;
  const float2 halfStage = stage.xy * 0.5;
  Mesh4Pixel o;
  if (proj.w > 0.5) {
    // Pure perspective, kept affine in view space (w = view depth) so the
    // rasterizer interpolates and clips exactly as the projection says.
    const float w = view.z;
    const float sxw = right.w * w + view.x * proj.y, syw = up.w * w - view.y * proj.y;
    o.position = float4(sxw / halfStage.x - w, w - syw / halfStage.y, zn * w, w);
  } else {
    const float k = proj.x + (proj.y / max(view.z, 1e-3) - proj.x) * proj.z;
    const float sx = right.w + view.x * k, sy = up.w - view.y * k;
    o.position = float4(sx / halfStage.x - 1.0, 1.0 - sy / halfStage.y, zn, 1.0);
  }
  o.clip = view.z - forward.w;
  o.world = world;
  o.normal = Row3(inst.r0, inst.r1, inst.r2, float4(n, 0));
  o.color = v.color * inst.tint;
  o.uv = v.uv;
  o.uv2 = v.uv2;
  return o;
}
