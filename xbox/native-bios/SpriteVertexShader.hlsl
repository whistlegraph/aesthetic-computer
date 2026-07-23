struct VertexInput {
  float3 position : POSITION;
  float2 uv : TEXCOORD0;
  float4 color : COLOR0;
};

struct PixelInput {
  float4 position : SV_POSITION;
  float2 uv : TEXCOORD0;
  float4 color : COLOR0;
};

PixelInput main(VertexInput input) {
  PixelInput output;
  output.position = float4(input.position, 1.0);
  output.uv = input.uv;
  output.color = input.color;
  return output;
}
