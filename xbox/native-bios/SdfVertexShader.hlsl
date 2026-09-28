#include "SdfFigure.hlsli"

struct PixelInput {
  float4 position : SV_POSITION;
  nointerpolation uint figure : FIGURE;
};

static const uint corner[6] = {0, 1, 2, 2, 1, 3};

// One screen rectangle per figure: its bounding sphere projected with the
// nearest scale the sphere can reach, so the rectangle is conservative for
// any blend of orthographic and perspective.
PixelInput main(uint vertexId : SV_VertexID, uint instanceId : SV_InstanceID) {
  PixelInput output;
  const uint f = uint(target.z) + instanceId;
  output.figure = f;
  const float4 s = figures[f].sphere;
  const float3 v = ToView(s.xyz);
  const float nearZ = forward.w;
  float2 lo = viewport.xy, hi = viewport.zw;
  if (v.z - s.w > nearZ) {
    const float kc = ScaleAt(v.z), kn = ScaleAt(v.z - s.w);
    const float2 center = float2(right.w + v.x * kc, up.w - v.y * kc);
    const float pad = depthMap.z + 4.0;
    const float2 extent = abs(v.xy) * abs(kn - kc) + s.w * kn + pad;
    lo = max(lo, center - extent);
    hi = min(hi, center + extent);
  }
  const uint c = corner[vertexId % 6];
  const float2 p = float2((c & 1) ? hi.x : lo.x, (c & 2) ? hi.y : lo.y);
  output.position = float4(p.x / 960.0 - 1.0, 1.0 - p.y / 540.0, 0.0, 1.0);
  return output;
}
