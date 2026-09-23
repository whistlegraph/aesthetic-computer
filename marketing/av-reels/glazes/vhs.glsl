// 📼 vhs — reel-finish glaze, modelled on the tape's signal chain rather than
// stacked screen effects (libplacebo / mpv user-shader format, 3 passes).
//
//   1 RECORD    RGB → YIQ; per-line time-base error (head wobble, top-edge
//               flagging, head-switch tear) displaces the whole line; luma is
//               band-limited (~3 MHz ≈ 3 px here) and the fields bob (60i).
//   2 COLOR     color-under: chroma band-limited ~10× harder than luma,
//               delayed to the right, averaged with the line above (comb),
//               with per-line phase jitter (hue wander).
//   3 PLAYBACK  the VCR's sharpening overshoots (edge halos), tape hiss runs
//               ALONG lines (streaks, not square grain), oxide dropouts,
//               chroma noise blotches, a mistracking band, milky blacks.
//
// "Lines" are 4 output px (480 over 1920) — the same grid everything keys
// to, so the look survives Instagram's re-encode. All randomness hashes
// `frame`, so a render is deterministic and audio sync is untouched.
// Tuned CRISP: AC's pixels should still read through the tape.

//!HOOK MAIN
//!BIND HOOKED
//!SAVE TAPE
//!COMPONENTS 4
//!DESC ac glaze: vhs 1 record

#define LINE_PX 4.0

float hash(vec2 p) { return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453); }
float vnoise(float x, float seed) {                  // smooth 1D value noise
  float i = floor(x), f = fract(x);
  return mix(hash(vec2(i, seed)), hash(vec2(i + 1.0, seed)), f * f * (3.0 - 2.0 * f));
}
vec3 toYIQ(vec3 c) {
  return vec3(dot(c, vec3(0.299, 0.587, 0.114)),
              dot(c, vec3(0.596, -0.274, -0.322)),
              dot(c, vec3(0.211, -0.523, 0.312)));
}

vec4 hook() {
  vec2 uv = HOOKED_pos, px = HOOKED_pt;
  float f = float(frame), lines = HOOKED_size.y / LINE_PX;
  float line = floor(uv.y * lines);

  // Time-base error: slow head wobble + per-line jitter, in output px.
  float tbe = (vnoise(line * 0.045 + f * 0.11, 1.0) - 0.5) * 3.0
            + (hash(vec2(line, f)) - 0.5) * 0.8;
  float top = 1.0 - smoothstep(0.0, 0.035, uv.y);     // flagging: top skews
  tbe += top * top * 22.0 * (0.7 + 0.3 * vnoise(f * 0.07, 2.0));
  float foot = smoothstep(0.978, 0.99, uv.y);         // head-switch tear
  tbe += foot * (26.0 + 30.0 * hash(vec2(floor(f / 2.0), line)));

  // 60i: each frame is one field; the other field's lines are bobbed in.
  float parity = mod(f, 2.0);
  float fieldY = (floor((line - parity) / 2.0) * 2.0 + parity + 0.5) / lines;
  float y = mix(uv.y, fieldY, 0.35);

  vec2 p = vec2(uv.x - tbe * px.x, y);

  // Luma band-limit: 5-tap gaussian, σ ≈ 1.5 px.
  float Y = toYIQ(HOOKED_tex(p).rgb).x * 0.4;
  Y += (toYIQ(HOOKED_tex(p - vec2(1.5 * px.x, 0.0)).rgb).x +
        toYIQ(HOOKED_tex(p + vec2(1.5 * px.x, 0.0)).rgb).x) * 0.22;
  Y += (toYIQ(HOOKED_tex(p - vec2(3.0 * px.x, 0.0)).rgb).x +
        toYIQ(HOOKED_tex(p + vec2(3.0 * px.x, 0.0)).rgb).x) * 0.08;

  vec2 iq = toYIQ(HOOKED_tex(p).rgb).yz;
  return vec4(Y, iq * 0.5 + 0.5, foot);
}

//!HOOK MAIN
//!BIND HOOKED
//!BIND TAPE
//!SAVE CHROMA
//!COMPONENTS 4
//!DESC ac glaze: vhs 2 color-under

#define LINE_PX 4.0

float hash(vec2 p) { return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453); }

vec4 hook() {
  vec2 uv = TAPE_pos, px = TAPE_pt;
  float f = float(frame), lines = TAPE_size.y / LINE_PX;
  float line = floor(uv.y * lines);

  // Chroma band-limit: 13 taps over ±24 px, delayed 7 px right.
  vec2 c = vec2(0.0); float wsum = 0.0;
  for (int i = -6; i <= 6; i++) {
    float w = exp(-float(i * i) / 18.0);
    vec2 q = vec2(uv.x - 7.0 * px.x + float(i) * 4.0 * px.x, uv.y);
    c += (TAPE_tex(q).yz * 2.0 - 1.0) * w;
    c += (TAPE_tex(q - vec2(0.0, LINE_PX * px.y)).yz * 2.0 - 1.0) * w;   // comb: line above
    wsum += 2.0 * w;
  }
  c /= wsum;

  // Phase jitter per line → hue wander; a touch of saturation loss.
  float a = (hash(vec2(line * 0.5, floor(f / 3.0))) - 0.5) * 0.12;
  c = mat2(cos(a), -sin(a), sin(a), cos(a)) * c * 0.84;
  return vec4(0.0, c * 0.5 + 0.5, 1.0);
}

//!HOOK MAIN
//!BIND HOOKED
//!BIND TAPE
//!BIND CHROMA
//!DESC ac glaze: vhs 3 playback

#define LINE_PX 4.0

float hash(vec2 p) { return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453); }
float vnoise(float x, float seed) {
  float i = floor(x), f = fract(x);
  return mix(hash(vec2(i, seed)), hash(vec2(i + 1.0, seed)), f * f * (3.0 - 2.0 * f));
}
vec3 toRGB(vec3 y) {
  return vec3(y.x + 0.956 * y.y + 0.621 * y.z,
              y.x - 0.272 * y.y - 0.647 * y.z,
              y.x - 1.106 * y.y + 1.703 * y.z);
}

vec4 hook() {
  vec2 uv = TAPE_pos, px = TAPE_pt;
  float f = float(frame), lines = TAPE_size.y / LINE_PX;
  float line = floor(uv.y * lines);
  vec4 tape = TAPE_tex(uv);
  float Y = tape.x, foot = tape.w;

  // Playback sharpening overshoot: bright/dark halo trailing each edge.
  float Yl = TAPE_tex(uv - vec2(3.0 * px.x, 0.0)).x;
  float Yr = TAPE_tex(uv + vec2(3.0 * px.x, 0.0)).x;
  Y += 0.55 * (Y - 0.5 * (Yl + Yr)) + 0.18 * (Y - Yl);

  // Mistracking band drifting down: hiss swamps it.
  float bandY = fract(f / 60.0 * 0.04 + 0.3);
  float band = 1.0 - smoothstep(0.0, 0.03, abs(uv.y - bandY));

  // Tape hiss runs along the line: smooth noise in x, new per line & frame.
  float seed = hash(vec2(line, f));
  float hiss = vnoise(uv.x * TAPE_size.x / 5.0 + seed * 91.0, seed) - 0.5;
  Y += hiss * (0.05 + band * 0.4);

  // Oxide dropouts: rare bright dashes, a few lines tall.
  float dline = floor(line / 2.0);
  float d = hash(vec2(dline, floor(f / 2.0) * 1.7));
  if (d > 0.9975) {
    float x0 = hash(vec2(dline, f + 5.0)), len = 0.04 + 0.2 * hash(vec2(f, dline));
    float inDash = step(x0, uv.x) * step(uv.x, x0 + len);
    Y = mix(Y, 0.95, inDash * 0.85);
  }

  // Chroma + blotchy chroma noise (coarse, along the line).
  vec2 iq = CHROMA_tex(uv).yz * 2.0 - 1.0;
  float cseed = hash(vec2(floor(line / 3.0), floor(f / 2.0)));
  iq += (vec2(vnoise(uv.x * TAPE_size.x / 40.0, cseed),
              vnoise(uv.x * TAPE_size.x / 40.0, cseed + 7.0)) - 0.5) * (0.035 + band * 0.12);

  vec3 col = toRGB(vec3(Y, iq));
  col = mix(col, vec3(hash(vec2(uv.x * 300.0, f + line))), foot * 0.6);   // tear hiss

  // Milky blacks, slightly soft whites.
  col = col * 0.9 + 0.055;
  return vec4(clamp(col, 0.0, 1.0), 1.0);
}
