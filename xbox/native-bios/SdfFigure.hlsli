// Shared by SdfVertexShader.hlsl and SdfPixelShader.hlsl (sceneApi 3).
// Figures arrive as world-space round cones (xbox/live/sdf/workbench.html is
// the WGSL reference). The camera is the game's own 27-float native camera,
// so a figure lands on the same pixels and depths as the triangles around it.

cbuffer SdfView : register(b0) {
  float4 camPos;    // xyz camera position
  float4 right;     // xyz view right,   w centerX (logical px)
  float4 up;        // xyz view up,      w centerY
  float4 forward;   // xyz view forward, w near
  float4 proj;      // orthoScale, focal, perspective blend, logical px per target px
  float4 depthMap;  // depthBase, depthSlope, outline (logical px), blend radius
  float4 viewport;  // minX, minY, maxX, maxY (logical px)
  float4 target;    // target width, target height, first figure, unused
};

struct SdfFigureData { float4 sphere; float4 range; };  // range.x first prim, .y count
struct SdfPrim { float4 a; float4 b; float4 c; };         // a.w r1, b.w r2, c.rgb ink, c.w 1 = hard union

StructuredBuffer<SdfFigureData> figures : register(t0);
StructuredBuffer<SdfPrim> prims : register(t1);

float3 ToView(float3 p) {
  const float3 d = p - camPos.xyz;
  return float3(dot(d, right.xyz), dot(d, up.xyz), dot(d, forward.xyz));
}
// Screen scale at view depth z: the game blends orthographic and perspective.
float ScaleAt(float z) {
  return proj.x + (proj.y / max(z, 1e-3) - proj.x) * proj.z;
}
