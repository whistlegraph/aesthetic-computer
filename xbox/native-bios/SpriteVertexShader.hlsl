struct VertexInput {
  float3 position : POSITION;
  float2 uv : TEXCOORD0;
  float4 color : COLOR0;
  float reciprocalDepth : TEXCOORD1;
};

struct PixelInput {
  float4 position : SV_POSITION;
  float2 uv : TEXCOORD0;
  float4 color : COLOR0;
  float reciprocalDepth : TEXCOORD1;
};

PixelInput main(VertexInput input) {
  PixelInput output;
  output.position = float4(input.position, 1.0);
  output.uv = input.uv;
  output.color = input.color;
  output.reciprocalDepth = input.reciprocalDepth;
  return output;
}
