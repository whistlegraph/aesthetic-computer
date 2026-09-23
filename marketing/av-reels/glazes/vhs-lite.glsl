// 📼 vhs-lite — the first, single-pass VHS reel glaze (libplacebo / mpv user-shader format).
// Tape look over the whole composed 1080×1920 frame: chroma smeared right of
// a softened luma, per-line jitter, a slow tracking band, head-switching
// noise at the foot, grain, chunky scanlines, vignette. Everything keys off
// `frame`, so a render is deterministic and audio sync is untouched.
// Keep details chunky (≥3 px): Instagram's re-encode eats finer grain.

//!HOOK MAIN
//!BIND HOOKED
//!DESC ac glaze: vhs-lite

float hash(vec2 p) { return fract(sin(dot(p, vec2(127.1, 311.7))) * 43758.5453); }

vec3 toYIQ(vec3 c) {
  return vec3(dot(c, vec3(0.299, 0.587, 0.114)),
              dot(c, vec3(0.596, -0.274, -0.322)),
              dot(c, vec3(0.211, -0.523, 0.312)));
}

vec3 toRGB(vec3 y) {
  return vec3(y.x + 0.956 * y.y + 0.621 * y.z,
              y.x - 0.272 * y.y - 0.647 * y.z,
              y.x - 1.106 * y.y + 1.703 * y.z);
}

vec4 hook() {
  vec2 uv = HOOKED_pos, px = HOOKED_pt;
  float t = float(frame) / 60.0;
  float tick = floor(float(frame) / 2.0);            // tape noise at 30 Hz

  // Per-line horizontal wobble (tape lines ~3 px tall).
  float line = floor(uv.y * HOOKED_size.y / 3.0);
  float jitter = (hash(vec2(line, tick)) - 0.5) * 1.2 * px.x;

  // Tracking band drifting down the frame.
  float bandY = fract(t * 0.045);
  float band = 1.0 - smoothstep(0.0, 0.035, abs(uv.y - bandY));
  jitter += band * (hash(vec2(line * 0.37, tick)) - 0.5) * 18.0 * px.x;

  // Head-switching tear across the bottom few lines.
  float foot = smoothstep(0.975, 1.0, uv.y);
  jitter += foot * (0.012 + 0.01 * hash(vec2(tick, 3.0)));

  vec2 p = vec2(uv.x + jitter, uv.y);

  // Luma: slightly softened. Chroma: wide box smear, pushed right.
  vec3 c = toYIQ(HOOKED_tex(p).rgb);
  float yl = (toYIQ(HOOKED_tex(p - vec2(px.x, 0.0)).rgb).x +
              toYIQ(HOOKED_tex(p + vec2(px.x, 0.0)).rgb).x) * 0.5;
  c.x = mix(c.x, yl, 0.45);
  vec2 iq = vec2(0.0);
  for (int i = 0; i < 8; i++)
    iq += toYIQ(HOOKED_tex(p + vec2((float(i) - 1.0) * 2.5 * px.x, 0.0)).rgb).yz;
  c.yz = iq / 8.0 * 0.82;                              // a little washed out

  vec3 col = toRGB(c);

  // Grain, extra hiss inside the tracking band.
  vec2 cell = floor(uv * HOOKED_size / 3.0);
  col += (hash(cell + tick) - 0.5) * (0.07 + band * 0.25);
  col = mix(col, vec3(hash(vec2(line, tick * 1.3))), foot * 0.5);

  // Chunky scanlines (4 px period) and a soft vignette.
  col *= 0.9 + 0.1 * step(0.5, fract(uv.y * HOOKED_size.y / 4.0));
  col *= mix(0.72, 1.0, smoothstep(0.85, 0.35, length((uv - 0.5) * vec2(1.0, 0.8))));

  // Tape's warm lift in the blacks.
  col = col * 0.94 + vec3(0.035, 0.02, 0.03);
  return vec4(clamp(col, 0.0, 1.0), 1.0);
}
