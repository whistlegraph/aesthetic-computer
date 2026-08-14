#!/usr/bin/env node
// skid-audition.mjs — six friction voices, side by side, isolated.
//
// The track keeps sounding wrong in different directions (buzzy, then too
// bright), so this stops guessing inside a four-minute render: each variant
// plays three gestures — drag / skid / slide — with two seconds of silence
// between variants, and a spoken-position marker in the filename order below.
//
//   node pop/cult/bin/skid-audition.mjs   → out/skid-audition.wav + .mp3
//
// A → the current render5 voice (noise-dominant, drumhead range)
// B → + STICK-SLIP: friction on a membrane judders, it does not hiss
// C → stick-slip, drier and shorter (a flick, not a drag)
// D → stick-slip + head resonance body (tension-tuned, not a tone)
// E → the Swift "physical" weights verbatim, at drumhead frequencies
// F → the Swift "synthetic" ring-mod mode

import { writeFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(HERE, "..", "out");
const SR = 48000, TAU = Math.PI * 2;
let seed = 0x2f6e2b1;
const rnd = () => { seed ^= seed << 13; seed ^= seed >>> 17; seed ^= seed << 5; seed >>>= 0; return seed / 0xffffffff * 2 - 1; };
const clamp = (v, a, b) => (v < a ? a : v > b ? b : v);
const smooth = (u) => { u = clamp(u, 0, 1); return u * u * (3 - 2 * u); };

// One gesture. `mode` selects the texture recipe.
function friction(buf, t, dur, {
  shape = "drag", gain = 1, cut0 = 1300, cut1 = null, res0 = 190, res1 = null,
  rough = 0.55, rel = 0.10, mode = "A",
} = {}) {
  const c1 = cut1 ?? cut0, r1 = res1 ?? res0;
  const n = Math.round((dur + rel + 0.06) * SR), i0 = Math.round(t * SR);
  const atkA = 1 - Math.exp(-1 / (SR * 0.0025));
  const relA = 1 - Math.exp(-1 / (SR * rel));
  let lvl = 0, nf = 0, ns = 0, ph = 0, slip = 0, slipLp = 0, bodyA = 0, bodyB = 0;
  for (let i = 0; i < n; i++) {
    const u = i / SR, x = u / dur;
    let target = 0;
    if (x < 1) target = shape === "skid" ? Math.exp(-x * 4.2)
      : shape === "slide" ? Math.sin(Math.PI * x)
        : Math.pow(smooth(clamp(x / 0.92, 0, 1)), 1.6);
    lvl += (target > lvl ? atkA : relA) * (target - lvl);
    const k = clamp(x, 0, 1);
    const cut = cut0 + (c1 - cut0) * k, res = res0 + (r1 - res0) * k;
    const fa = 1 - Math.exp((-TAU * cut) / SR);
    const sa = 1 - Math.exp((-TAU * Math.max(35, cut * 0.18)) / SR);
    const white = rnd();
    nf += fa * (white - nf); ns += sa * (white - ns);
    const band = nf - ns;
    const motion = 1 + Math.tanh(band * 8) * 0.055;
    ph += (res * motion) / SR; if (ph >= 1) ph -= Math.floor(ph);
    const carrier = Math.sin(TAU * ph);
    const gnarl = Math.tanh(band * (5 + rough * 5));

    // STICK-SLIP: the finger grabs and releases many times a second. That
    // judder — not the noise colour — is what says "skidding on a head".
    // Rate rises with gesture speed; depth with roughness.
    const slipHz = 34 + 190 * k + 70 * rough;
    slip += (TAU * slipHz) / SR; if (slip >= TAU) slip -= TAU;
    const slipRaw = Math.sin(slip) * 0.5 + 0.5;
    slipLp += 0.35 * (slipRaw - slipLp);
    const judder = 1 - (0.55 + 0.35 * rough) * Math.pow(slipLp, 1.7);

    // head body: two shallow resonators, not a sine — a membrane, not a tone
    const bfa = 1 - Math.exp((-TAU * res * 1.0) / SR);
    const bfb = 1 - Math.exp((-TAU * res * 2.7) / SR);
    bodyA += bfa * (gnarl - bodyA); bodyB += bfb * (gnarl - bodyB);
    const body = (bodyA * 0.7 + bodyB * 0.3);

    let tex;
    if (mode === "A") tex = gnarl * 0.92 + carrier * (0.015 + Math.abs(gnarl) * 0.085);
    else if (mode === "B") tex = (gnarl * 0.92 + carrier * 0.03) * judder;
    else if (mode === "C") tex = (gnarl * 0.86 + carrier * 0.02) * judder;
    else if (mode === "D") tex = (gnarl * 0.55 + body * 0.85 + carrier * 0.03) * judder;
    else if (mode === "E") tex = gnarl * 0.44 + carrier * (0.08 + Math.abs(gnarl) * (0.42 + rough * 0.30));
    else tex = nf * carrier * 1.35;

    const tail = clamp((n - 1 - i) / (0.010 * SR), 0, 1);
    const w = tail * tail * (3 - 2 * tail);
    buf[i0 + i] = (buf[i0 + i] || 0) + tex * lvl * 0.50 * gain * w;
  }
}

const VARIANTS = [
  ["A  current render5 (noise-dominant)",      { mode: "A", cut0: 1530, res0: 200 }],
  ["B  + stick-slip judder",                    { mode: "B", cut0: 1400, res0: 190 }],
  ["C  stick-slip, drier + shorter",            { mode: "C", cut0: 1150, res0: 170, rough: 0.72 }],
  ["D  stick-slip + head body",                 { mode: "D", cut0: 1250, res0: 205, rough: 0.66 }],
  ["E  Swift physical weights, drumhead range", { mode: "E", cut0: 1300, res0: 190 }],
  ["F  Swift synthetic ring-mod",               { mode: "F", cut0: 1300, res0: 190 }],
];

const SLOT = 4.2;
const total = Math.ceil(VARIANTS.length * SLOT + 1) * SR;
const buf = new Float64Array(total);
VARIANTS.forEach(([name, o], vi) => {
  const t0 = vi * SLOT + 0.25;
  console.log(`${vi + 1}. ${name}`);
  friction(buf, t0 + 0.0, 0.42, { shape: "drag",  gain: 1.00, cut1: (o.cut0 ?? 1300) * 1.9, res1: (o.res0 ?? 190) * 1.5, ...o });
  friction(buf, t0 + 1.1, 0.26, { shape: "skid",  gain: 1.10, cut1: (o.cut0 ?? 1300) * 0.7, ...o });
  friction(buf, t0 + 2.0, 0.90, { shape: "slide", gain: 0.85, cut1: (o.cut0 ?? 1300) * 1.4, res1: (o.res0 ?? 190) * 0.8, ...o });
});

let peak = 0; for (const v of buf) peak = Math.max(peak, Math.abs(v));
const g = peak ? 0.72 / peak : 1;
const pcm = Buffer.alloc(buf.length * 2);
for (let i = 0; i < buf.length; i++) pcm.writeInt16LE(Math.max(-32767, Math.min(32767, (buf[i] * g * 32767) | 0)), i * 2);
const raw = resolve(OUT, ".skid-audition.raw");
writeFileSync(raw, pcm);
const wav = resolve(OUT, "skid-audition.wav"), mp3 = resolve(OUT, "skid-audition.mp3");
spawnSync("ffmpeg", ["-y", "-v", "error", "-f", "s16le", "-ar", String(SR), "-ac", "1", "-i", raw, wav], { stdio: "inherit" });
spawnSync("ffmpeg", ["-y", "-v", "error", "-i", wav, "-c:a", "libmp3lame", "-b:a", "256k", mp3], { stdio: "inherit" });
spawnSync("rm", ["-f", raw]);
console.log(`\n✓ ${mp3}\n  each variant: drag @0.0  ·  skid @1.1  ·  slide @2.0  (${SLOT}s apart)`);
