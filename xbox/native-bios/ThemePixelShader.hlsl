Texture2D spriteAtlas : register(t0);
SamplerState linearSampler : register(s0);
struct PixelInput {
  float4 position : SV_POSITION;
  float2 uv : TEXCOORD0;
  float4 color : COLOR0;
};
float4 main(PixelInput input) : SV_TARGET {
  const float4 sprite = spriteAtlas.Sample(linearSampler, input.uv);
  clip(sprite.a - (1.0 / 255.0));
  return float4(input.color.rgb * sprite.rgb, input.color.a * sprite.a);
}
