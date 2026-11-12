#include "Mesh4.hlsli"

Texture2D albedo : register(t1);
Texture2D lightmap : register(t2);
SamplerState albedoSampler : register(s0);
SamplerState lightmapSampler : register(s1);

// Smooth shading: the interpolated vertex normal lit by ambient, the sun and
// the frame's point lights this draw's bounds can reach, then distance fog.
float4 main(Mesh4Pixel i, bool front : SV_IsFrontFace) : SV_TARGET {
  const uint flags = info.z;
  float4 base = i.color * tint;
  if (flags & kTextured) base *= albedo.Sample(albedoSampler, i.uv);
  if (base.a < extra.x) discard;
  float3 lit = 1;
  if (!(flags & kUnlit)) {
    float3 n = normalize(i.normal);
    if (!front) n = -n;  // a two-sided face lights its back as a front
    lit = (flags & kLightmapped) ? lightmap.Sample(lightmapSampler, i.uv2).rgb * emissive.w
      : ambient.rgb + sun.rgb * saturate(-dot(n, sunDir.xyz));
    uint mask = info.w;
    [loop] while (mask) {
      const uint k = firstbitlow(mask);
      mask &= mask - 1;
      const float3 L = lightPos[k].xyz - i.world;
      const float d2 = dot(L, L), r2 = lightPos[k].w * lightPos[k].w;
      if (d2 < r2) {
        float fall = 1.0 - d2 / r2;
        fall *= fall;
        lit += lightColor[k].rgb * fall * saturate(dot(n, L * rsqrt(max(d2, 1e-8))));
      }
    }
  }
  float3 rgb = base.rgb * lit + emissive.rgb;
  if ((flags & kFog) && fogRange.y > fogRange.x) {
    const float f = saturate((length(i.world - camPos.xyz) - fogRange.x) / (fogRange.y - fogRange.x));
    rgb = lerp(rgb, fogColor.rgb, f * fogColor.w);
  }
  // The triangle list's screen-door: depth stays right without sorting.
  if (base.a < 0.999) {
    const uint2 pixel = uint2(i.position.xy) & 3;
    const uint pattern[16] = {0, 8, 2, 10, 12, 4, 14, 6, 3, 11, 1, 9, 15, 7, 13, 5};
    clip(base.a - (pattern[pixel.y * 4 + pixel.x] + 0.5) / 16.0);
  }
  return float4(rgb, 1.0);
}
