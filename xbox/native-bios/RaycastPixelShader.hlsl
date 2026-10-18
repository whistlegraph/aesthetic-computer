// The beach ball, shaded per pixel: a ray through every pixel of the square
// hits an exact sphere, whose normal lights it and whose depth places it
// among the triangles. A line-for-line port of the web scene's fragment
// shader (xbox/live/scene3d-webgl.mjs) so the ball reads the same on every
// host: eight gores turning with the roll, welded seams, white pole caps
// with a valve, a lit side, a highlight, a cool rim and a darker underside.
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

struct PixelOutput {
  float4 color : SV_TARGET;
  float depth : SV_DEPTH;
};

static const float3 beach[8] = {
  float3(.24, .47, .95), float3(.66, .33, .88), float3(.93, .26, .42), float3(1.0, .53, .22),
  float3(1.0, .84, .24), float3(.36, .82, .44), float3(.2, .78, .82), float3(.24, .47, .95),
};

float3 RotateX(float3 v, float a) {
  const float c = cos(a), s = sin(a);
  return float3(v.x, c * v.y - s * v.z, s * v.y + c * v.z);
}

float3 RotateZ(float3 v, float a) {
  const float c = cos(a), s = sin(a);
  return float3(c * v.x - s * v.y, s * v.x + c * v.y, v.z);
}

PixelOutput main(PixelInput input) {
  const float r = length(input.local);
  const float aa = fwidth(r);
  const float cover = 1.0 - smoothstep(1.0 - aa, 1.0 + aa * .25, r);
  if (cover <= 0.0) discard;
  const float rr = min(r * r, 1.0);
  const float3 n = float3(input.local, sqrt(1.0 - rr));
  // Object space: the ball rolls in the picture plane and is tipped toward
  // the lens so its pole and gores read as a sphere, not a pinwheel.
  const float3 o = RotateX(RotateZ(n, -roll), -.62 + tumble);
  const float lon = atan2(o.x, o.z) / 6.2831853 + .5;
  const float gore = lon * 8.0;
  float3 base = beach[uint(floor(gore)) & 7];
  // A fine welded seam where two gores meet.
  const float edge = min(frac(gore), 1.0 - frac(gore));
  const float seamWidth = fwidth(gore) * 1.2 + .015;
  base *= lerp(.8, 1.0, smoothstep(seamWidth * .5, seamWidth, edge));
  // White pole caps with a valve on one.
  const float cap = smoothstep(.83, .86, abs(o.y));
  base = lerp(base, float3(.97, .96, .92), cap);
  const float valve = smoothstep(.985, .99, o.y);
  base = lerp(base, float3(.82, .8, .76), valve);
  const float3 light = normalize(float3(-.45, .62, .64));
  const float diffuse = max(dot(n, light), 0.0);
  const float wrap = max(dot(n, light) * .5 + .5, 0.0);
  const float3 halfway = normalize(light + float3(0.0, 0.0, 1.0));
  const float spec = pow(max(dot(n, halfway), 0.0), 90.0) * .85;
  const float sheen = pow(max(dot(n, halfway), 0.0), 9.0) * .18;
  const float rim = pow(1.0 - n.z, 3.0);
  float3 colour = base * (.28 + .52 * diffuse + .3 * wrap);
  colour += float3(1.0, 1.0, 1.0) * (spec + sheen);
  colour += float3(.75, .86, 1.0) * rim * .45;
  // Ambient occlusion toward the floor side.
  colour *= lerp(.78, 1.0, smoothstep(-1.0, .1, n.y));
  PixelOutput output;
  output.color = float4(min(colour, float3(1.0, 1.0, 1.0)), cover);
  output.depth = saturate(depth - depthRange * n.z);
  return output;
}
