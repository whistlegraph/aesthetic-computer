// 📺 crt — reel-finish glaze (libplacebo / mpv user-shader format).
// A curved tube: barrel warp, RGB aperture triads, 4 px scanlines, a cheap
// bloom from a wide tap ring, rounded dark corners, slight flicker.
// Deterministic per `frame`; chunky enough to survive Instagram's re-encode.

//!HOOK MAIN
//!BIND HOOKED
//!DESC ac glaze: crt

vec4 hook() {
  vec2 uv = HOOKED_pos, px = HOOKED_pt;

  // Barrel distortion.
  vec2 cc = uv - 0.5;
  vec2 p = 0.5 + cc * (1.0 + dot(cc, cc) * vec2(0.10, 0.06));
  if (p.x < 0.0 || p.x > 1.0 || p.y < 0.0 || p.y > 1.0) return vec4(0.0, 0.0, 0.0, 1.0);

  vec3 col = HOOKED_tex(p).rgb;

  // Bloom: a ring of wide taps, added back soft.
  vec3 glow = vec3(0.0);
  for (int i = 0; i < 8; i++) {
    float a = float(i) * 0.785398;
    glow += HOOKED_tex(p + vec2(cos(a), sin(a)) * 6.0 * px).rgb;
  }
  col += glow / 8.0 * 0.35;

  // Aperture grille: R, G, B columns, 3 px each.
  float triad = mod(floor(p.x * HOOKED_size.x / 3.0), 3.0);
  vec3 mask = vec3(triad == 0.0 ? 1.0 : 0.7, triad == 1.0 ? 1.0 : 0.7, triad == 2.0 ? 1.0 : 0.7);
  col *= mask;

  // Scanlines, 4 px period, gentler on bright pixels.
  float scan = step(0.5, fract(p.y * HOOKED_size.y / 4.0));
  float lum = dot(col, vec3(0.299, 0.587, 0.114));
  col *= mix(0.78, 1.0, max(scan, lum * 0.6));

  // Rounded corners + vignette, and a whisper of 30 Hz flicker.
  vec2 edge = smoothstep(vec2(0.0), vec2(0.03, 0.02), p) * smoothstep(vec2(0.0), vec2(0.03, 0.02), 1.0 - p);
  col *= edge.x * edge.y;
  col *= mix(0.7, 1.0, smoothstep(0.8, 0.3, length(cc)));
  col *= 0.985 + 0.015 * mod(floor(float(frame) / 2.0), 2.0);

  return vec4(clamp(col * 1.08, 0.0, 1.0), 1.0);
}
