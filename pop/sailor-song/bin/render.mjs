#!/usr/bin/env node
// render.mjs — Sailor Song, outside remix v2. FOLLOW: nothing is warped.
//
//   her voice   src/vox/vocals-aesthetivox.wav — WORLD-regulated, G# minor
//               in the guitar's +13¢ frame (bin/aesthetivox.py)
//   her guitar  Demucs other+bass+drums, untouched
//   the clock   measures.json — bars anchored on her strum motif X..XX..X
//               (bin/measures.py), so bar 1 is always her bar 1 even where
//               the beat tracker slipped (27.85 s, 61.17 s)
//   the kit     hits land on HER strum points: kick 1 + "and of 2", snare 3,
//               rim on "and of 4"; hats on 8ths from chorus 1
//   sine beds   voice-led sine choir, orchestrated by section — sub, tenor
//               pair under her, a high pair over her, her own register
//               (G#3–G#4, median D#4) left clear. In the choruses a sine
//               shadows her tuned melody a diatonic 3rd above
//               (vox-notes.json); chorus 2 and the bridge add its octave.
//
//   node pop/sailor-song/bin/render.mjs            → out/sailor-song-v2.{wav,mp3}
//   node pop/sailor-song/bin/render.mjs --from 55 --to 95   (an excerpt)

import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const STEMS = resolve(LANE, "../samples/sailor-song-take/stems/htdemucs");
const OUT = resolve(LANE, "out");
const SR = 48_000;
mkdirSync(OUT, { recursive: true });

const flags = {};
for (let i = 2; i < process.argv.length; i++) {
  const a = process.argv[i];
  if (!a.startsWith("--")) continue;
  const n = process.argv[i + 1];
  if (n !== undefined && !n.startsWith("--")) { flags[a.slice(2)] = n; i++; } else flags[a.slice(2)] = true;
}

const M = JSON.parse(readFileSync(resolve(LANE, "measures.json"), "utf8"));
const VN = JSON.parse(readFileSync(resolve(LANE, "vox-notes.json"), "utf8")).notes;
const TUNE = 0.13; // guitar's frame, semitones over A440

// ── io ───────────────────────────────────────────────────────────────────
function to48(src, tag) {
  const dst = resolve(LANE, `src/vox/${tag}-48k.wav`);
  if (!existsSync(dst)) spawnSync("ffmpeg", ["-v", "error", "-y", "-i", src, "-ar", String(SR), "-ac", "2", "-c:a", "pcm_f32le", dst]);
  return dst;
}
function readWav(path) {
  const b = readFileSync(path);
  let p = 12, fmt, data;
  while (p < b.length) {
    const id = b.toString("ascii", p, p + 4), sz = b.readUInt32LE(p + 4);
    if (id === "fmt ") fmt = { tag: b.readUInt16LE(p + 8), ch: b.readUInt16LE(p + 10), bits: b.readUInt16LE(p + 22) };
    if (id === "data") data = b.subarray(p + 8, p + 8 + sz);
    p += 8 + sz + (sz & 1);
  }
  const bps = fmt.bits / 8, n = Math.floor(data.length / bps / fmt.ch);
  const rd = (k) => fmt.bits === 16 ? data.readInt16LE(k * 2) / 32768 : fmt.tag === 3 ? data.readFloatLE(k * 4) : data.readInt32LE(k * 4) / 2147483648;
  const L = new Float32Array(n), R = new Float32Array(n);
  for (let i = 0; i < n; i++) { L[i] = rd(i * fmt.ch); R[i] = rd(i * fmt.ch + (fmt.ch > 1 ? 1 : 0)); }
  return { L, R };
}
function writeWav(path, L, R) {
  const n = L.length, b = Buffer.alloc(44 + n * 4);
  b.write("RIFF", 0); b.writeUInt32LE(36 + n * 4, 4); b.write("WAVEfmt ", 8);
  b.writeUInt32LE(16, 16); b.writeUInt16LE(1, 20); b.writeUInt16LE(2, 22);
  b.writeUInt32LE(SR, 24); b.writeUInt32LE(SR * 4, 28); b.writeUInt16LE(4, 32);
  b.writeUInt16LE(16, 34); b.write("data", 36); b.writeUInt32LE(n * 4, 40);
  for (let i = 0; i < n; i++) {
    b.writeInt16LE(Math.round(Math.max(-1, Math.min(1, L[i])) * 32767), 44 + i * 4);
    b.writeInt16LE(Math.round(Math.max(-1, Math.min(1, R[i])) * 32767), 46 + i * 4);
  }
  writeFileSync(path, b);
}

const vox = readWav(resolve(LANE, "src/vox/vocals-aesthetivox.wav"));
const gtr = readWav(to48(resolve(STEMS, "guitar.wav"), "guitar"));
const halo = readWav(resolve(LANE, "src/vox/vocals-halo.wav"));
const FROM = Number(flags.from ?? 0), TO = Number(flags.to ?? vox.L.length / SR);
const n = Math.ceil((TO - FROM) * SR);
const at = (t) => Math.floor((t - FROM) * SR);

// ── sections (bar numbers from measures.json, words mapped in src/) ─────
const SECTIONS = [
  { name: "intro",   from: 1,  to: 10 },
  { name: "verse1",  from: 11, to: 27 },
  { name: "chorus1", from: 28, to: 43 },
  { name: "verse2",  from: 44, to: 51 },
  { name: "chorus2", from: 52, to: 67 },
  { name: "break",   from: 68, to: 71 },
  { name: "bridge",  from: 72, to: 80 },
  { name: "outro",   from: 81, to: 999 },
];
const sectionOf = (bar) => SECTIONS.find((s) => bar >= s.from && bar <= s.to)?.name;
// ── the arrangement ─────────────────────────────────────────────────────
// Per section: drum + sine layer levels (0 = out), her own harmonies
// (stem → [gain, pan]), and vocal presence (reverb send, air lift above
// 7 kHz — closer + brighter in the verses and bridge, roomier in choruses).
const ARR = {
  intro:   { kick: 0,  snare: 0,  rim: 0,  hat: 0,  sub: .5, tenor: .6, high: 0,  third: 0,  octave: 0,  arp: .5, bell: 0,  descant: 0,  hook: 0,
             harm: {}, send: .12, air: .2 },
  verse1:  { kick: .7, snare: 0,  rim: .5, hat: 0,  sub: .8, tenor: .8, high: 0,  third: 0,  octave: 0,  arp: .35, bell: .8, descant: 0, hook: 0,
             harm: {}, send: .07, air: .45 },
  chorus1: { kick: 1,  snare: .8, rim: .6, hat: .6, sub: 1,  tenor: 1,  high: .6, third: .35, octave: 0, arp: .6, bell: .5, descant: 0,  hook: 0,
             harm: { up3: [.34, -.55], down6: [.28, .55] }, send: .15, air: .25 },
  verse2:  { kick: .8, snare: .5, rim: .5, hat: .3, sub: .9, tenor: .9, high: .3, third: 0,  octave: 0,  arp: .5, bell: .8, descant: 0,  hook: 0,
             harm: { down3: [.24, .5] }, send: .08, air: .45 },
  chorus2: { kick: 1,  snare: 1,  rim: .6, hat: .8, sub: 1,  tenor: 1,  high: .8, third: .3, octave: .3, arp: .7, bell: .4, descant: 1,  hook: 0,
             harm: { up3: [.34, -.6], down3: [.3, .6], down8: [.24, 0] }, send: .16, air: .3 },
  break:   { kick: 0,  snare: 0,  rim: 0,  hat: 0,  sub: .8, tenor: 1,  high: 1,  third: 0,  octave: 0,  arp: .6, bell: 0,  descant: .6, hook: 1,
             harm: {}, send: .2, air: .2 },
  bridge:  { kick: .7, snare: .6, rim: .5, hat: .4, sub: 1,  tenor: 1,  high: .9, third: 0,  octave: .3, arp: .8, bell: .5, descant: .5, hook: 0,
             harm: "rove", send: .09, air: .5 },
  outro:   { kick: .5, snare: 0,  rim: .4, hat: 0,  sub: .8, tenor: .8, high: .6, third: 0,  octave: 0,  arp: .4, bell: .8, descant: 0,  hook: 0,
             harm: { down8: [.3, 0], up3: [.2, -.4] }, send: .2, air: .15 },
};
// "pitching around": in the bridge her harmony changes interval and side
// every bar.
const ROVE = [{ up3: [.36, -.6] }, { up5: [.3, .6] }, { down3: [.34, -.4] }, { up3: [.3, .5], down6: [.24, -.5] }];
const harmFor = (bar) => { const a = ARR[sectionOf(bar.n)]; return a.harm === "rove" ? ROVE[(bar.n - 72) % 4] : a.harm; };
const ORCH = Object.fromEntries(Object.entries(ARR).map(([k, v]) => [k, v]));

// ── buses ────────────────────────────────────────────────────────────────
const dL = new Float32Array(n), dR = new Float32Array(n);   // drums
const sL = new Float32Array(n), sR = new Float32Array(n);   // sines
const add = (buf, i, v) => { if (i >= 0 && i < n) buf[i] += v; };

function kick(t, g) {
  const s0 = at(t); let ph = 0;
  for (let i = 0; i < 0.36 * SR; i++) { const u = i / SR;
    ph += (2 * Math.PI * (46 + 58 * Math.exp(-u * 32))) / SR;
    const v = Math.sin(ph) * Math.exp(-u * 7.5) * g; add(dL, s0 + i, v); add(dR, s0 + i, v); }
}
function snare(t, g) {
  const s0 = at(t); let prev = 0, lp = 0, ph = 0;
  for (let i = 0; i < 0.24 * SR; i++) { const u = i / SR, nz = Math.random() * 2 - 1;
    const hp = nz - prev; prev = nz; lp += (hp - lp) * 0.12; ph += (2 * Math.PI * 180) / SR;
    // brushy: slower swell-in, darker noise, softer body
    const v = (lp * Math.min(1, u / 0.004) * Math.exp(-u * 11) * 1.6 + Math.sin(ph) * 0.22 * Math.exp(-u * 30)) * g;
    add(dL, s0 + i, v * 0.92); add(dR, s0 + i, v); }
}
function rim(t, g) {           // woody cross-stick: two damped partials + click
  const s0 = at(t); let p1 = 0, p2 = 0;
  for (let i = 0; i < 0.07 * SR; i++) { const u = i / SR;
    p1 += (2 * Math.PI * 1650) / SR; p2 += (2 * Math.PI * 520) / SR;
    const v = (Math.sin(p1) * 0.5 + Math.sin(p2)) * Math.exp(-u * 70) * g;
    add(dL, s0 + i, v * 1.1); add(dR, s0 + i, v * 0.8); }
}
function hat(t, g, pan) {
  const s0 = at(t); let prev = 0;
  for (let i = 0; i < 0.03 * SR; i++) { const nz = Math.random() * 2 - 1, hp = nz - prev; prev = nz;
    const v = hp * Math.exp(-(i / SR) * 300) * g * 0.6; add(dL, s0 + i, v * (1 - pan)); add(dR, s0 + i, v * (1 + pan)); }
}

// Sine voice: slow swell, gentle 4.5 Hz tremolo, a ±3¢ detuned twin for width.
function sine(t0, dur, midi, g, pan, { atk = 0.35, rel = 0.9, det = 3 } = {}) {
  const f = 440 * Math.pow(2, (midi + TUNE - 69) / 12);
  const s0 = at(t0), len = Math.floor((dur + rel) * SR);
  const fa = f * Math.pow(2, det / 1200), fb = f * Math.pow(2, -det / 1200);
  let pa = Math.random() * 6, pb = Math.random() * 6;
  const gl = g * Math.sqrt((1 - pan) / 2), gr = g * Math.sqrt((1 + pan) / 2);
  for (let i = 0; i < len; i++) {
    const u = i / SR;
    const env = Math.min(1, u / atk) * (u > dur ? Math.exp(-(u - dur) / (rel / 3)) : 1);
    pa += (2 * Math.PI * fa) / SR; pb += (2 * Math.PI * fb) / SR;
    const trem = 1 - 0.08 * (0.5 + 0.5 * Math.sin(2 * Math.PI * 4.5 * u));
    add(sL, s0 + i, Math.sin(pa) * env * trem * gl);
    add(sR, s0 + i, Math.sin(pb) * env * trem * gr);
  }
}

// ── harmony: voice-led chord tones ──────────────────────────────────────
const CHORD = { "G#m": [8, 11, 3], "Emaj7": [8, 11, 3], B: [11, 3, 6] }; // Emaj7 bars sound G#m (bass G#)
const ROOT = { "G#m": 44, "Emaj7": 44, B: 47 };                          // G#2, B2
const SCALE = [8, 10, 11, 1, 3, 4, 6];                                   // G# natural minor
function nearestTone(pcs, target, lo, hi) {
  let best = null;
  for (let m = lo; m <= hi; m++) if (pcs.includes(((m % 12) + 12) % 12) && (best === null || Math.abs(m - target) < Math.abs(best - target))) best = m;
  return best;
}
function lead(prev, pcs, lo, hi) {   // move each voice to the nearest chord tone, no doubling
  const used = new Set();
  return prev.map((p) => { let m = nearestTone(pcs.filter((c) => !used.has(c)).length ? pcs.filter((c) => !used.has(c)) : pcs, p, lo, hi); used.add(((m % 12) + 12) % 12); return m; });
}
function thirdAbove(m) {           // diatonic third in G# minor
  const pc = ((m % 12) + 12) % 12, oct = Math.floor(m / 12);
  const i = SCALE.indexOf(pc);
  if (i < 0) return m + 3;
  const t = SCALE[(i + 2) % 7];
  return oct * 12 + t + (t < pc ? 12 : 0);
}

// A struck sine: bell (inharmonic 2.76 partial, long) or celesta (octave
// partial, short). Both sit above her register.
function bell(t0, midi, g, pan, { dec = 1.4, part = 2.76, pg = 0.25 } = {}) {
  const f = 440 * Math.pow(2, (midi + TUNE - 69) / 12), s0 = at(t0), len = Math.floor(dec * 4 * SR);
  const gl = g * Math.sqrt((1 - pan) / 2), gr = g * Math.sqrt((1 + pan) / 2);
  for (let i = 0; i < len; i++) { const u = i / SR;
    const v = (Math.sin(2 * Math.PI * f * u) * Math.exp(-u / dec) + pg * Math.sin(2 * Math.PI * f * part * u) * Math.exp(-u / (dec * 0.3))) * Math.min(1, u / 0.003);
    add(sL, s0 + i, v * gl); add(sR, s0 + i, v * gr); }
}
const celesta = (t0, midi, g, pan) => bell(t0, midi, g, pan, { dec: 0.35, part: 2, pg: 0.15 });

let tenor = lead([52, 56, 59], CHORD["G#m"], 47, 58);   // under her
let high = [75, 78];                                     // over her
let desc = 83;                                           // descant, drifts down
const barOf = (t) => M.bars.find((b) => t >= b.t && t < b.t + b.dur);
for (const b of M.bars) {
  const sec = sectionOf(b.n), o = ARR[sec];
  if (!o || b.t + b.dur < FROM || b.t > TO) continue;
  const bt = b.beats, pcs = CHORD[b.chord] || CHORD["G#m"];
  const mid = (k) => bt[k] + (bt[k + 1] - bt[k]) / 2;
  // bridge builds: everything swells bar by bar
  const swell = sec === "bridge" ? 0.7 + 0.3 * ((b.n - 72) / 8) : 1;
  // drums on her strum points (X..XX..X)
  if (o.kick) { kick(bt[0], 0.95 * o.kick); if (bt.length > 2) kick(mid(1), 0.6 * o.kick); }
  if (o.snare && bt.length > 3) snare(bt[2], 0.6 * o.snare * swell);
  if (o.rim && bt.length > 4) rim(mid(3), 0.22 * o.rim);
  if (o.hat) for (let k = 0; k < bt.length - 1; k++) { hat(bt[k], 0.15 * o.hat, -0.35); hat(mid(k), 0.09 * o.hat, 0.35); }
  // sine choir
  const dur = b.dur * 0.97;
  if (o.sub) { sine(b.t, dur, ROOT[b.chord] - 12, 0.30 * o.sub, 0, { det: 0 }); sine(b.t, dur, ROOT[b.chord], 0.10 * o.sub, 0, { det: 1 }); }
  tenor = lead(tenor, pcs, 47, 58);
  if (o.tenor) tenor.forEach((m, k) => sine(b.t, dur, m, 0.07 * o.tenor * swell, [-0.5, 0, 0.5][k]));
  high = lead(high, pcs, 70, 83);
  if (o.high) high.forEach((m, k) => sine(b.t, dur, m, 0.035 * o.high * swell, k ? 0.7 : -0.7, { atk: 0.8, rel: 1.4, det: 5 }));
  // descant: one long high chord tone per bar, leaning downward by step
  if (o.descant) {
    const c = [desc - 1, desc - 2, desc, desc + 1].map((x) => nearestTone(pcs, x, 76, 88)).find((x) => x <= desc) ?? nearestTone(pcs, desc, 76, 88);
    desc = c <= 77 ? 87 : c;
    sine(b.t, dur, c, 0.03 * o.descant, 0.2, { atk: 1.0, rel: 1.8, det: 4 });
  }
  // celesta arpeggio on her 8th grid: chord tones rising, 16ths in the last bridge bars
  if (o.arp) {
    const tones = [0, 1, 2, 3, 4, 5].map((k) => nearestTone(pcs, 68 + k * 3, 68, 86));
    const dens = sec === "bridge" && b.n >= 77 ? 4 : 2;
    let k = 0;
    for (let j = 0; j < bt.length - 1; j++) for (let q = 0; q < dens; q++) {
      const t = bt[j] + ((bt[j + 1] - bt[j]) * q) / dens;
      celesta(t, tones[k++ % tones.length], 0.035 * o.arp * swell, (k % 2 ? -0.5 : 0.5));
    }
  }
}
// her melody, shadowed: a sine 3rd above + an octave line
for (const v of VN) {
  if (v.t < FROM || v.t > TO || v.dur < 0.18) continue;
  const bar = barOf(v.t), o = bar && ARR[sectionOf(bar.n)];
  if (!o) continue;
  if (o.third) sine(v.t, v.dur, thirdAbove(v.midi), 0.05 * o.third, 0.3, { atk: 0.06, rel: 0.35, det: 2 });
  if (o.octave) sine(v.t, v.dur, v.midi + 12, 0.025 * o.octave, -0.3, { atk: 0.08, rel: 0.5, det: 4 });
}
// bell answers: in each breath gap > 1 s, her last three notes echo back
// an octave up on bells, one per 8th.
for (let i = 1; i < VN.length; i++) {
  const prev = VN[i - 1], end = prev.t + prev.dur, gap = VN[i].t - end;
  if (gap < 1.0 || end < FROM || end > TO) continue;
  const bar = barOf(end), o = bar && ARR[sectionOf(bar.n)];
  if (!o || !o.bell) continue;
  const e8 = (bar.beats[1] - bar.beats[0]) / 2;
  const tail = VN.slice(Math.max(0, i - 3), i);
  tail.forEach((v, k) => { const t = end + 0.12 + k * e8; if (t < VN[i].t - 0.1) bell(t, v.midi + 12, 0.05 * o.bell, k % 2 ? 0.4 : -0.4); });
}
// the hook, on sines, over the guitar-only break: chorus 1's first four
// bars of her tuned melody re-laid bar-for-bar onto bars 68–71.
{
  const src = M.bars.filter((b) => b.n >= 28 && b.n < 32), dst = M.bars.filter((b) => b.n >= 68 && b.n < 72);
  for (const v of VN) {
    const bi = src.findIndex((b) => v.t >= b.t && v.t < b.t + b.dur);
    if (bi < 0 || !dst[bi]) continue;
    const f = (v.t - src[bi].t) / src[bi].dur, sc = dst[bi].dur / src[bi].dur;
    const t = dst[bi].t + f * dst[bi].dur;
    sine(t, v.dur * sc, v.midi, 0.07, 0, { atk: 0.05, rel: 0.6, det: 3 });
    sine(t, v.dur * sc, v.midi + 12, 0.025, 0.3, { atk: 0.08, rel: 0.8, det: 5 });
  }
}

// ── reverb: 8-line FDN (Hadamard feedback, damped), the shared heaven ───
// Voice, halo and sines all send here so they sit in ONE space. Long
// (~3.2 s), dark (damping ~5 kHz), 40 ms predelay so her words stay dry-clear.
function fdn(inL, inR, { t60 = 3.2, damp = 0.35, pre = 0.04 } = {}) {
  const lens = [1433, 1601, 1867, 2053, 2251, 2399, 2617, 2797].map((x) => Math.round(x * SR / 44100));
  const bufs = lens.map((l) => new Float32Array(l)), idx = lens.map(() => 0), lp = lens.map(() => 0);
  const gains = lens.map((l) => Math.pow(10, (-3 * l) / (SR * t60)));
  const P = Math.floor(pre * SR);
  const oL = new Float32Array(inL.length), oR = new Float32Array(inL.length);
  const o = new Float32Array(8);
  for (let i = 0; i < inL.length; i++) {
    const xl = i >= P ? inL[i - P] : 0, xr = i >= P ? inR[i - P] : 0;
    for (let k = 0; k < 8; k++) { const y = bufs[k][idx[k]]; lp[k] += (y - lp[k]) * (1 - damp); o[k] = lp[k]; }
    // 8-point Hadamard, normalized
    const a0 = o[0] + o[1], a1 = o[0] - o[1], a2 = o[2] + o[3], a3 = o[2] - o[3], a4 = o[4] + o[5], a5 = o[4] - o[5], a6 = o[6] + o[7], a7 = o[6] - o[7];
    const b0 = a0 + a2, b1 = a1 + a3, b2 = a0 - a2, b3 = a1 - a3, b4 = a4 + a6, b5 = a5 + a7, b6 = a4 - a6, b7 = a5 - a7;
    const h = [b0 + b4, b1 + b5, b2 + b6, b3 + b7, b0 - b4, b1 - b5, b2 - b6, b3 - b7];
    for (let k = 0; k < 8; k++) {
      bufs[k][idx[k]] = h[k] * 0.35355 * gains[k] + (k % 2 ? xr : xl) * 0.5;
      idx[k] = (idx[k] + 1) % lens[k];
    }
    oL[i] = o[0] + o[2] + o[4] + o[6]; oR[i] = o[1] + o[3] + o[5] + o[7];
  }
  return [oL, oR];
}

// ── mix ──────────────────────────────────────────────────────────────────
// Her take is the record. Presence rides per section (send + air), her own
// harmonies ride per bar with 150 ms gain glides, the kit sits dark and low.
const HARM = {};
for (const k of ["up3", "down3", "up5", "down6", "down8"]) {
  const p = resolve(LANE, `src/vox/harm-${k}.wav`);
  if (existsSync(p)) HARM[k] = readWav(p).L;
}
// per-sample automation from per-bar targets
const auto = (fn) => {
  const a = new Float32Array(n), glide = 1 - Math.exp(-1 / (0.15 * SR));
  let cur = null;
  for (let i = 0; i < n; i++) {
    const t = FROM + i / SR, b = barOf(t) || (t < M.bars[0].t ? M.bars[0] : M.bars.at(-1));
    const tgt = fn(b);
    cur = cur === null ? tgt : cur + (tgt - cur) * glide;
    a[i] = cur;
  }
  return a;
};
const sendA = auto((b) => ARR[sectionOf(b.n)].send);
const airA = auto((b) => ARR[sectionOf(b.n)].air);
const hg = {}, hp = {};
for (const k of Object.keys(HARM)) {
  hg[k] = auto((b) => (harmFor(b)[k] || [0, 0])[0]);
  hp[k] = auto((b) => (harmFor(b)[k] || [0, 0])[1]);
}
const HDELAY = { up3: 0.018, up5: 0.022, down3: 0.026, down6: 0.03, down8: 0.012 };  // a breath apart, like real doubles

const L = new Float32Array(n), R = new Float32Array(n);
const s0 = Math.floor(FROM * SR);
const sendL = new Float32Array(n), sendR = new Float32Array(n);
let dlp = 0, drp = 0, vlp = 0;
const dk = 1 - Math.exp(-2 * Math.PI * 4500 / SR);
const ak = 1 - Math.exp(-2 * Math.PI * 7000 / SR);
for (let i = 0; i < n; i++) {
  const vi = s0 + i, v0 = vox.L[vi] || 0, hv = (halo.L[vi] || 0) * 0.14;
  vlp += (v0 - vlp) * ak;
  const v = v0 + (v0 - vlp) * airA[i] * 1.6;               // air lift above 7 kHz
  let hl = 0, hr = 0;
  for (const k in HARM) {
    const g = hg[k][i];
    if (g < 0.002) continue;
    const x = (HARM[k][vi - Math.floor(HDELAY[k] * SR)] || 0) * g * 0.55, pn = hp[k][i];
    hl += x * Math.sqrt((1 - pn) / 2); hr += x * Math.sqrt((1 + pn) / 2);
  }
  dlp += (dL[i] - dlp) * dk; drp += (dR[i] - drp) * dk;
  L[i] = v + hl + (gtr.L[vi] || 0) * 0.72 + dlp * 0.45 + sL[i] * 0.75 + hv * 0.6;
  R[i] = v + hr + (gtr.R[vi] || 0) * 0.72 + drp * 0.45 + sR[i] * 0.75 + hv * 0.6;
  const sd = sendA[i];
  sendL[i] = v * sd + hl * 0.35 + hv + sL[i] * 0.4 + dlp * 0.05 + (gtr.L[vi] || 0) * 0.06;
  sendR[i] = v * sd + hr * 0.35 + hv + sR[i] * 0.4 + drp * 0.05 + (gtr.R[vi] || 0) * 0.06;
}
// the room: present but near — early reflections always on, a 2.2 s tail behind
const room = [[0.011, 0.16, 0], [0.017, 0.14, 1], [0.023, 0.11, 0], [0.031, 0.1, 1], [0.043, 0.07, 0], [0.053, 0.06, 1]];
const [wL, wR] = fdn(sendL, sendR, { t60: 2.2, damp: 0.4, pre: 0.025 });
for (let i = n - 1; i >= 0; i--) {
  let el = 0, er = 0;
  for (const [d, g, ch] of room) { const j = i - Math.floor(d * SR); if (j >= 0) { const x = (sendL[j] + sendR[j]) * g; if (ch) er += x; else el += x; } }
  L[i] += wL[i] * 0.4 + el; R[i] += wR[i] * 0.4 + er;
}
const fade = Math.floor(1.0 * SR);
for (let i = 0; i < fade; i++) { const g = i / fade; L[i] *= g; R[i] *= g; L[n - 1 - i] *= g; R[n - 1 - i] *= g; }
let peak = 0;
for (let i = 0; i < n; i++) peak = Math.max(peak, Math.abs(L[i]), Math.abs(R[i]));
for (let i = 0; i < n; i++) { L[i] *= 0.89 / peak; R[i] *= 0.89 / peak; }

const tag = flags.from || flags.to ? `v4-${Math.round(FROM)}-${Math.round(TO)}` : "v4";
const wav = resolve(OUT, `sailor-song-${tag}.wav`);
writeWav(wav, L, R);
spawnSync("ffmpeg", ["-v", "error", "-y", "-i", wav, "-b:a", "256k", wav.replace(/\.wav$/, ".mp3")]);
console.log(`✓ ${(n / SR).toFixed(1)}s → ${wav.replace(/\.wav$/, ".mp3")}`);
