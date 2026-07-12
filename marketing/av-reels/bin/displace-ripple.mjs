// displace-ripple.mjs — a LOCAL GLITCH displacement burst at each tap. Not a
// smooth pond ripple (too soft, spread too far) — a tight digital disturbance:
// blocky horizontal tear-slices (quantized to the chunky pixel grid), a
// chromatic RGB split, and per-frame flicker, confined to a small radius around
// the touch and decaying fast. In the spirit of /pop's per-row warp but glitched.
//
// Operates on the raw RGBA frame buffer inside stamp-reel's frame pump, bounded
// to each burst's local box so it stays cheap.

export function buildRipples(presses = []) {
  return presses.map((p) => ({ t: p.t0, x: p.x, y: p.y }));
}

// cheap deterministic hash → [0,1)
function hash(n) { const x = Math.sin(n * 12.9898) * 43758.5453; return x - Math.floor(x); }

// dst ← src with a local glitch burst per active tap. src/dst are RGBA Uint8 W*H*4.
export function applyRipples(src, dst, W, H, ripples, t, opts = {}) {
  const LIFE = opts.life ?? 0.3;      // s — short, punchy
  const R0 = opts.radius ?? 175;      // px — LOCAL (small blast radius)
  const CHUNK = opts.chunk ?? 18;     // px — tear-slice height (chunky)
  const AMP = opts.amp ?? 46;         // px — max horizontal tear
  const CHROMA = opts.chroma ?? 12;   // px — R/B channel split
  dst.set(src);
  const clampX = (x) => (x < 0 ? 0 : x >= W ? W - 1 : x);
  for (const rp of ripples) {
    const age = t - rp.t;
    if (age < 0 || age > LIFE) continue;
    const cx = rp.x * W, cy = rp.y * H;
    const env = (1 - age / LIFE) ** 1.5;     // fast decay from the hit
    const R = R0 * (0.7 + 0.3 * env);
    const fseed = Math.floor(t * 30);        // reseed each frame → flicker
    const seed = Math.floor(rp.x * 971 + rp.y * 613);
    const x0 = Math.max(0, Math.floor(cx - R)), x1 = Math.min(W - 1, Math.ceil(cx + R));
    const y0 = Math.max(0, Math.floor(cy - R)), y1 = Math.min(H - 1, Math.ceil(cy + R));
    for (let py = y0; py <= y1; py++) {
      const dy = py - cy;
      // per-slice horizontal tear, quantized to CHUNK, flickering per frame
      const block = Math.floor(py / CHUNK);
      const h = hash(block * 131 + fseed * 977 + seed);
      const tear = Math.round(((h - 0.5) * 2 * AMP) / CHUNK) * CHUNK;
      const row = py * W;
      for (let px = x0; px <= x1; px++) {
        const dx = px - cx;
        const d = Math.sqrt(dx * dx + dy * dy);
        if (d > R) continue;
        const fall = 1 - d / R;
        const ff = fall * fall * env;
        if (ff < 0.05) continue;
        const ox = Math.round(tear * ff);
        const cr = Math.round(CHROMA * ff);
        const di = (row + px) * 4;
        const gI = (row + clampX(px + ox)) * 4;
        dst[di] = src[(row + clampX(px + ox + cr)) * 4];       // R shifted +
        dst[di + 1] = src[gI + 1];                             // G at tear
        dst[di + 2] = src[(row + clampX(px + ox - cr)) * 4 + 2]; // B shifted −
        dst[di + 3] = 255;
      }
    }
  }
}
