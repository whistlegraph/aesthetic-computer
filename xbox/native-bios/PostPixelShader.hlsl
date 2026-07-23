Texture2D sceneTexture : register(t0);
SamplerState pointSampler : register(s0);

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
  const float offset = 0.55 + radial * 1.35;
  const float3 center = sceneTexture.Sample(pointSampler, input.uv).rgb;
  float3 color;
  color.r = sceneTexture.Sample(pointSampler,
    input.uv + float2(texel.x * offset, 0.0)).r;
  color.g = center.g;
  color.b = sceneTexture.Sample(pointSampler,
    input.uv - float2(texel.x * offset, 0.0)).b;

  // Stable one-pixel dither and a very light scan modulation keep the output
  // alive without hiding pixel art or softening nearest-neighbour sprites.
  const float dither = frac(sin(dot(floor(input.position.xy),
    float2(12.9898, 78.233))) * 43758.5453) - 0.5;
  const float scan = 0.975 + 0.025 * sin(input.position.y * 3.14159265);
  const float vignette = saturate(1.08 - radial * 0.18);
  color = saturate(color * scan * vignette + dither / 255.0);

  if (stencilPass > 0.5) {
    const float3 right = sceneTexture.Sample(pointSampler,
      input.uv + float2(texel.x, 0.0)).rgb;
    const float3 down = sceneTexture.Sample(pointSampler,
      input.uv + float2(0.0, texel.y)).rgb;
    const float edge = saturate(length(center - right) + length(center - down));
    const float pulse = 0.5 + 0.5 * sin(timeSeconds * 1.7);
    color = saturate(color + float3(0.05, 0.015, 0.08) *
      (0.45 + pulse * 0.25 + edge * 0.8));
  }
  return float4(color, 1.0);
}
