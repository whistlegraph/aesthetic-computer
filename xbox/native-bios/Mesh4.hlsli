// sceneApi 4 (SCENE-API-4.md): GPU-resident meshes. The camera is the game's
// 27-float native camera, so a vertex lands on the pixel and depth the
// triangle list would give it; the Mac shell's Metal copy is in
// xbox/macos-native/Scene4.swift.

cbuffer Mesh4View : register(b0) {
  float4 camPos;    // xyz camera position
  float4 right;     // xyz view right,   w centre x (logical px)
  float4 up;        // xyz view up,      w centre y
  float4 forward;   // xyz view forward, w near
  float4 proj;      // ortho scale, focal, perspective blend, 1 = pure perspective
  float4 depthMap;  // depth base, depth per view unit, min depth, max depth
  float4 stage;     // logical width, logical height
  float4 sunDir;    // xyz: the way the sunlight travels (camera m24..26)
};
cbuffer Mesh4Scene : register(b1) {
  float4 ambient;   // rgb
  float4 sun;       // rgb
  float4 fogColor;  // rgb, w: the most fog can cover (0..1)
  float4 fogRange;  // start, end (no fog when end <= start)
  float4 lightPos[32];    // xyz, w radius
  float4 lightColor[32];  // rgb
};
cbuffer Mesh4Draw : register(b2) {
  float4 tint;      // material colour
  float4 emissive;  // rgb, w lightmap scale
  uint4 info;       // first instance, first bone, flags, light mask
  float4 extra;     // alpha cutoff
};

static const uint kUnlit = 1, kFog = 2, kTextured = 4, kLightmapped = 8;

struct Mesh4Instance { float4 r0; float4 r1; float4 r2; float4 tint; };

struct Mesh4Pixel {
  float4 position : SV_POSITION;
  float clip : SV_ClipDistance0;   // view depth past the near plane
  float3 world : WORLD;
  float3 normal : NORMAL;
  float4 color : COLOR0;
  float2 uv : TEXCOORD0;
  float2 uv2 : TEXCOORD1;
};
