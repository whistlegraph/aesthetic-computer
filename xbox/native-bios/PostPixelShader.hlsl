Texture2D sceneTexture : register(t0);
SamplerState sceneSampler : register(s0);

cbuffer PostConstants : register(b0) {
  float2 texel;
  float timeSeconds;
  float stencilPass;
};

struct PixelInput {
  float4 position : SV_POSITION;
  float2 uv : TEXCOORD0;
};

float4 main(PixelInput input) : SV_TARGET {
  const float2 centered = input.uv * 2.0 - 1.0;
  const float radial = dot(centered, centered);
  const float3 center = sceneTexture.Sample(sceneSampler, input.uv).rgb;
  float3 color = center;

  // Stable one-pixel dither and a very light scan modulation keep the output
  // alive without hiding pixel art or softening nearest-neighbour sprites.
  const float dither = frac(sin(dot(floor(input.position.xy),
    float2(12.9898, 78.233))) * 43758.5453) - 0.5;
  const float scan = 0.975 + 0.025 * sin(input.position.y * 3.14159265);
  const float vignette = saturate(1.08 - radial * 0.18);
  if (stencilPass > 0.5) {
    // Compact FXAA runs only where triangle geometry wrote stencil.
    const float3 nw = sceneTexture.Sample(sceneSampler,
      input.uv + float2(-texel.x, -texel.y)).rgb;
    const float3 ne = sceneTexture.Sample(sceneSampler,
      input.uv + float2(texel.x, -texel.y)).rgb;
    const float3 sw = sceneTexture.Sample(sceneSampler,
      input.uv + float2(-texel.x, texel.y)).rgb;
    const float3 se = sceneTexture.Sample(sceneSampler,
      input.uv + float2(texel.x, texel.y)).rgb;
    const float3 weights = float3(0.299, 0.587, 0.114);
    const float lc = dot(center, weights), lnw = dot(nw, weights);
    const float lne = dot(ne, weights), lsw = dot(sw, weights);
    const float lse = dot(se, weights);
    float2 direction = float2(-((lnw + lne) - (lsw + lse)),
      (lnw + lsw) - (lne + lse));
    const float reduce = max((lnw + lne + lsw + lse) / 32.0, 1.0 / 128.0);
    direction = clamp(direction /
      (min(abs(direction.x), abs(direction.y)) + reduce), -8.0, 8.0) * texel;
    const float3 sampleA = 0.5 * (
      sceneTexture.Sample(sceneSampler, input.uv + direction * (-1.0 / 6.0)).rgb +
      sceneTexture.Sample(sceneSampler, input.uv + direction * (1.0 / 6.0)).rgb);
    const float3 sampleB = sampleA * 0.5 + 0.25 * (
      sceneTexture.Sample(sceneSampler, input.uv + direction * -0.5).rgb +
      sceneTexture.Sample(sceneSampler, input.uv + direction * 0.5).rgb);
    const float lmin = min(lc, min(min(lnw, lne), min(lsw, lse)));
    const float lmax = max(lc, max(max(lnw, lne), max(lsw, lse)));
    const float lb = dot(sampleB, weights);
    color = lb < lmin || lb > lmax ? sampleA : sampleB;
  }
  color = saturate(color * scan * vignette + dither / 255.0);
  return float4(color, 1.0);
}
