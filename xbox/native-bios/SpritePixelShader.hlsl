Texture2D spriteAtlas : register(t0);
SamplerState pointSampler : register(s0);

struct PixelInput {
  float4 position : SV_POSITION;
  float2 uv : TEXCOORD0;
  float4 color : COLOR0;
};

float4 main(PixelInput input) : SV_TARGET {
  const float4 sprite = spriteAtlas.Sample(pointSampler, input.uv);
  clip(sprite.a - 0.5);
  return float4(input.color.rgb * sprite.rgb, input.color.a * sprite.a);
}
