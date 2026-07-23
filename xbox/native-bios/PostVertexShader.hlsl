struct PixelInput {
  float4 position : SV_POSITION;
  float2 uv : TEXCOORD0;
};

PixelInput main(uint vertexId : SV_VertexID) {
  PixelInput output;
  output.uv = float2((vertexId << 1) & 2, vertexId & 2);
  output.position = float4(output.uv.x * 2.0 - 1.0,
                           1.0 - output.uv.y * 2.0, 0.0, 1.0);
  return output;
}
