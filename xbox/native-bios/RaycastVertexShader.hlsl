// One raycast sprite: a screen square around the object, four corners as a
// strip. `local` runs -1..1 across it, y up, the same as the web's scene.
cbuffer Raycast : register(b0) {
  float2 centre;
  float2 extent;
  float depth;
  float depthRange;
  float roll;
  float tumble;
};

struct PixelInput {
  float4 position : SV_POSITION;
  float2 local : LOCAL;
};

PixelInput main(uint vertexId : SV_VertexID) {
  const float2 corner = float2((vertexId & 1) ? 1.0 : -1.0, (vertexId & 2) ? 1.0 : -1.0);
  PixelInput output;
  output.local = corner;
  output.position = float4(centre + corner * extent, 0.0, 1.0);
  return output;
}
