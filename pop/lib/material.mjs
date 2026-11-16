// pop/lib/material.mjs — a material for the master.
//
// The bell engine gives a strike a body (glass, bronze, steel, stone). This
// gives the whole record one: the last thing the mix passes through before
// the limiter, so every voice ends up made of the same stuff.
//
//   import { applyMaterial, MATERIALS } from "../../lib/material.mjs";
//   applyMaterial(mix, 48000, "tape");          // interleaved stereo Float32Array, in place
//   applyMaterial(mix, 48000, "tape", 0.6);     // amount 0..1 scales every trait
//
// A material is plain numbers — add one by adding an entry.

export const MATERIALS = {
  // Plastic tape: a type-I cassette on a consumer deck. The capstan is never
  // quite round (wow) and the tape chatters over the head (flutter); oxide
  // saturates softly and sooner in the highs; the head adds a bump near
  // 90 Hz and loses the top octave; the left and right tracks leak into each
  // other; and the tape itself hisses.
  tape: {
    wow: { hz: 0.55, cents: 5 }, flutter: { hz: 7.3, cents: 1.6 }, scrape: { hz: 0.11, cents: 2 },
    drive: 1.5, bias: 0.06, emphasisHz: 2600,
    bumpHz: 90, bumpDb: 2.2, topHz: 11500,
    crosstalk: 0.05, hissDb: -62, dropouts: 0.012,
  },
};

export function applyMaterial(buf, sr, name = "tape", amount = 1) {
  const M = MATERIALS[name];
  if (!M) throw new Error(`material: unknown '${name}' (have ${Object.keys(MATERIALS).join(", ")})`);
  const n = buf.length >> 1, TAU = Math.PI * 2;
  // pitch deviation in cents → delay swing in samples
  const swing = (m) => (2 ** (m.cents * amount / 1200) - 1) / (TAU * m.hz) * sr;
  const wA = swing(M.wow), fA = swing(M.flutter), sA = swing(M.scrape);
  const base = Math.ceil(wA + fA + sA) + 4, RB = 1 << Math.ceil(Math.log2(base * 2 + 8)), mask = RB - 1;
  const ring = [new Float32Array(RB), new Float32Array(RB)];
  // head bump: one peaking biquad
  const A = 10 ** (M.bumpDb * amount / 40), w0 = TAU * M.bumpHz / sr, al = Math.sin(w0) / (2 * 0.9);
  const a0 = 1 + al / A, b0 = (1 + al * A) / a0, b1 = -2 * Math.cos(w0) / a0, b2 = (1 - al * A) / a0, a1 = b1, a2 = (1 - al / A) / a0;
  const st = [0, 1].map(() => ({ x1: 0, x2: 0, y1: 0, y2: 0, pre: 0, de: 0, lp1: 0, lp2: 0 }));
  const kE = 1 - Math.exp(-TAU * M.emphasisHz / sr), kT = 1 - Math.exp(-TAU * (M.topHz + (20000 - M.topHz) * (1 - amount)) / sr);
  const drive = 1 + (M.drive - 1) * amount, norm = 1 / Math.tanh(drive), xt = M.crosstalk * amount;
  const hiss = 10 ** (M.hissDb / 20) * amount;
  let seed = 0x7a9e5eed, hl = 0, hr = 0, drop = 1, dropTo = 1, dropLeft = 0;
  const rnd = () => { seed ^= seed << 13; seed ^= seed >>> 17; seed ^= seed << 5; return (seed >>> 0) / 4294967296; };
  for (let i = 0; i < n; i++) {
    const t = i / sr;
    // the tape's speed is shared by both tracks, so one delay for both ears
    const d = base + wA * Math.sin(TAU * M.wow.hz * t) + fA * Math.sin(TAU * M.flutter.hz * t + 1.3) + sA * Math.sin(TAU * M.scrape.hz * t + 0.4);
    const di = Math.floor(d), fr = d - di;
    // brief oxide dropouts: the level dips for a few tens of milliseconds
    if (dropLeft-- <= 0) { dropTo = rnd() < M.dropouts * amount ? 0.55 + 0.3 * rnd() : 1; dropLeft = Math.floor(sr * (0.03 + 0.25 * rnd())); }
    drop += (dropTo - drop) * 0.002;
    const out = [0, 0];
    for (let ch = 0; ch < 2; ch++) {
      const r = ring[ch], s = st[ch];
      r[i & mask] = buf[i * 2 + ch];
      let x = r[(i - di) & mask] * (1 - fr) + r[(i - di - 1) & mask] * fr;
      const y = b0 * x + b1 * s.x1 + b2 * s.x2 - a1 * s.y1 - a2 * s.y2;
      s.x2 = s.x1; s.x1 = x; s.y2 = s.y1; s.y1 = y; x = y;
      // highs hit the oxide harder: lift them into the saturator, drop them back after
      s.pre += kE * (x - s.pre);
      const hot = x + (x - s.pre) * 0.9 * amount;
      const sat = (Math.tanh(hot * drive + M.bias * amount) - Math.tanh(M.bias * amount)) * norm;
      s.de += kE * (sat - s.de);
      x = sat - (sat - s.de) * (0.9 * amount / (1 + 0.9 * amount));
      s.lp1 += kT * (x - s.lp1); s.lp2 += kT * (s.lp1 - s.lp2);
      out[ch] = s.lp2 * drop;
    }
    hl += 0.35 * (rnd() * 2 - 1 - hl); hr += 0.35 * (rnd() * 2 - 1 - hr);
    buf[i * 2] = out[0] + out[1] * xt + (rnd() * 2 - 1 - hl) * hiss;
    buf[i * 2 + 1] = out[1] + out[0] * xt + (rnd() * 2 - 1 - hr) * hiss;
  }
  return buf;
}
