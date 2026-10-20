// deep.mjs — a deep house bed under the regridded YC vocal. All synthesis.
//   node deep.mjs   (reads grid2.json; writes deep/*.wav)
// Smooth sine kick, sine pads in the back, a sub that walks the off-beats,
// filtered FM-Rhodes stabs with a dotted echo, swung shaker, rim, soft clap.
// The pump is drawn into the pads/bass/stabs from the kick grid itself.
import { readFileSync, writeFileSync, mkdirSync } from "node:fs";

const SR = 48000;
const { bpm, seconds } = JSON.parse(readFileSync("grid2.json", "utf8"));
const BEAT = 60 / bpm, BAR = BEAT * 4;
const NB = Math.ceil(seconds / BAR); // vocal bars
const END = NB; // the last chord lands on this bar
const TOTAL = (END + 2.5) * BAR;
const N = Math.ceil(TOTAL * SR);
mkdirSync("deep", { recursive: true });
const st = (n) => [new Float32Array(N), new Float32Array(N)];
const S = { kick: st(), sub: st(), pad: st(), keys: st(), shaker: st(), rim: st(), clap: st(), fx: st() };
let seed = 0x1234567; const rnd = () => ((seed = (seed * 1664525 + 1013904223) >>> 0) / 4294967296) * 2 - 1;
const add = (s, i, l, r) => { if (i >= 0 && i < N) { s[0][i] += l; s[1][i] += r; } };
const hz = (m) => 440 * 2 ** ((m - 69) / 12);
const T = (bar, beat = 0) => bar * BAR + beat * BEAT;

// Sections over the vocal bars (49): in · A · drop out · B · break · C · end.
const sec = (b) => b < 8 ? "in" : b < 16 ? "A" : b < 20 ? "air" : b < 34 ? "B" : b < 38 ? "air2" : b < END ? "C" : "end";
console.log(`${bpm} BPM · ${NB} vocal bars · ends on bar ${END} · ${TOTAL.toFixed(1)}s`);
console.log(Array.from({ length: END + 1 }, (_, b) => sec(b)[0]).join(""));

// Harmony, one chord a bar: Bm9 · Gmaj9 · Dmaj9 · A9sus — all inside D major,
// so the pentatonic melody sits on chord tones or soft extensions.
const CH = [
  { root: 35, tones: [59, 62, 66, 69, 73] }, // Bm9   B D F# A C#
  { root: 31, tones: [55, 59, 62, 66, 69] }, // Gmaj9 G B D F# A
  { root: 38, tones: [62, 66, 69, 73, 76] }, // Dmaj9 D F# A C# E
  { root: 33, tones: [57, 62, 64, 67, 71] }, // A9sus A D E G B
];
const chord = (b) => CH[b % 4];

// ── the pump: a smooth duck after every kick, drawn as a gain curve ───────
const kicks = [];
for (let b = 0; b < END; b++) { const s = sec(b);
  if (s === "air" || s === "air2") continue;
  if (s === "in" && b < 2) continue; // the build: kick fades in from 4 s, full by 16 s
  for (let q = 0; q < 4; q++) kicks.push({ t: T(b, q), g: s === "in" ? 0.3 + 0.11 * (b - 2) : 1 }); }
kicks.push({ t: T(END), g: 1 });
const pump = new Float32Array(N).fill(1);
for (const k of kicks) { const s0 = Math.round(k.t * SR), len = Math.round(BEAT * SR);
  for (let i = 0; i < len && s0 + i < N; i++) { const x = i / len; const d = 1 - 0.62 * k.g * Math.exp(-x * 4.2) * (1 - Math.exp(-i / (SR * 0.004)));
    pump[s0 + i] = Math.min(pump[s0 + i], d); } }

// ── kick: three layers — beater click, chest punch, sub tail — then drive ──
function kick(t, g) {
  const s0 = Math.round(t * SR); let ph = 0, lp = 0, hp = 0;
  for (let i = 0; i < SR * 0.75; i++) { const x = i / SR;
    const f = 50 + 190 * Math.exp(-x * 38) + 30 * Math.exp(-x * 9); // fast snap from 270 Hz down to 50
    ph += 2 * Math.PI * f / SR;
    const body = Math.sin(ph) * Math.exp(-x * 3.4) * Math.min(1, x / 0.0008);  // long sub tail
    const punch = Math.sin(ph * 2.02) * Math.exp(-x * 34) * 0.55;               // the chest hit, 100–500 Hz
    const nz = rnd(); lp += 0.5 * (nz - lp); hp = nz - lp;
    const click = hp * Math.exp(-x * 900) * 0.9 + Math.sin(2 * Math.PI * 3200 * x) * Math.exp(-x * 600) * 0.25; // beater
    const v = (Math.tanh((body + punch) * 2.6) * 0.82 + click) * g;
    add(S.kick, s0 + i, v, v); }
}
for (const k of kicks) kick(k.t, k.g);

// ── sub bass: sine + a little 2nd harmonic so phones hear it ─────────────
function subNote(t, dur, m, g) {
  const s0 = Math.round(t * SR), n = Math.round(dur * SR), f = hz(m); let ph = 0;
  for (let i = 0; i < n; i++) { const x = i / SR; ph += 2 * Math.PI * f / SR;
    const env = Math.min(1, x / 0.01) * Math.min(1, (n - i) / (SR * 0.04));
    const v = Math.tanh((Math.sin(ph) + 0.28 * Math.sin(2 * ph) + 0.08 * Math.sin(3 * ph)) * 1.3) * env * g;
    add(S.sub, s0 + i, v, v); }
}
for (let b = 0; b < END; b++) { const s = sec(b), c = chord(b);
  if (s === "in" && b < 4) { subNote(T(b), BAR * 0.95, c.root + 12, 0.5); continue; }
  if (s === "air" || s === "air2") { subNote(T(b), BAR * 0.98, c.root + 12, 0.45); continue; }
  // the deep-house off-beat walk: the "&" of every beat, an octave pop on the last
  for (let q = 0; q < 4; q++) subNote(T(b, q + 0.5), BEAT * 0.42, c.root + 12 + (q === 3 && b % 2 ? 12 : 0), 0.72);
  if (s === "C" || b % 4 === 3) subNote(T(b, 3.75), BEAT * 0.2, c.root + 19, 0.45);
}
subNote(T(END), BAR * 2, CH[2].root + 12, 0.8);

// ── sine pads, behind everything: slow, detuned, breathing ───────────────
function padChord(t, dur, tones, g) {
  const s0 = Math.round(t * SR), n = Math.round(dur * SR);
  tones.forEach((m, v) => { const f = hz(m - 12), pan = (v / (tones.length - 1)) * 1.2 - 0.6; let a = 0, b2 = 0;
    for (let i = 0; i < n + SR * 1.2 && s0 + i < N; i++) { const x = i / SR;
      a += 2 * Math.PI * f / SR; b2 += 2 * Math.PI * f * 2.003 / SR;
      const env = Math.min(1, x / 0.6) * (i < n ? 1 : Math.max(0, 1 - (i - n) / (SR * 1.2)));
      const trem = 1 + 0.12 * Math.sin(2 * Math.PI * 0.23 * x + v * 1.7);
      const s = (Math.sin(a) + 0.18 * Math.sin(b2)) * env * trem * g * 0.16;
      add(S.pad, s0 + i, s * (1 - pan), s * (1 + pan)); } });
}
for (let b = 0; b <= END; b++) padChord(T(b), b === END ? BAR * 2 : BAR, chord(b === END ? 2 : b).tones, sec(b).startsWith("air") ? 1.25 : 1);

// ── FM Rhodes stabs, low-passed, with a dotted-8th echo ──────────────────
function rhodes(t, dur, m, g, pan = 0) {
  const s0 = Math.round(t * SR), n = Math.round((dur + 1.0) * SR), f = hz(m); let lp = 0;
  for (let i = 0; i < n && s0 + i < N; i++) { const x = i / SR;
    const idx = 1.6 * Math.exp(-x * 9) + 0.25; // bright bark, then mellow
    const mod = Math.sin(2 * Math.PI * f * x) * idx;
    const car = Math.sin(2 * Math.PI * f * x + mod) + 0.12 * Math.sin(2 * Math.PI * f * 4.01 * x) * Math.exp(-x * 30);
    const env = Math.min(1, x / 0.003) * Math.exp(-x * (x < dur ? 2.2 : 9));
    const v = car * env * g; lp += 0.22 * (v - lp);
    add(S.keys, s0 + i, lp * (1 - pan), lp * (1 + pan)); }
}
const stabRhythm = { A: [0.5, 1.75, 2.5, 3.5], B: [0.5, 1.75, 2.5, 3.5], C: [0.5, 1.25, 1.75, 2.5, 3.5] };
for (let b = 0; b < END; b++) { const s = sec(b), pat = stabRhythm[s] || (s === "in" && b >= 6 ? [0.5, 1.75, 2.5] : null), c = chord(b);
  if (!pat) { if (s.startsWith("air")) for (const m of c.tones.slice(0, 4)) rhodes(T(b), BAR * 0.9, m, 0.11); continue; }
  for (const q of pat) for (const [j, m] of c.tones.slice(0, 4).entries()) {
    rhodes(T(b, q), BEAT * 0.3, m, 0.1, (j - 1.5) * 0.15);
    rhodes(T(b, q + 0.75), BEAT * 0.3, m, 0.035, 0.5); // the echo, panned off
  } }
for (const m of CH[2].tones) rhodes(T(END), BAR * 1.5, m, 0.14);

// ── percussion ────────────────────────────────────────────────────────────
function noiseHit(s, t, len, dec, hpC, g, pan = 0) { const s0 = Math.round(t * SR); let lp = 0;
  for (let i = 0; i < SR * len; i++) { const x = i / SR, nz = rnd(); lp += hpC * (nz - lp);
    const v = (nz - lp) * Math.exp(-x * dec) * Math.min(1, x / 0.002) * g; add(s, s0 + i, v * (1 - pan), v * (1 + pan)); } }
const swing = (q) => (q * 4) % 2 === 1 ? 0.07 * BEAT : 0; // late 16ths
for (let b = 0; b < END; b++) { const s = sec(b);
  const groove = s === "A" || s === "B" || s === "C";
  if (groove || (s === "in" && b >= 2)) for (let q = 0; q < 4; q += 0.25) {
    const acc = (q * 4) % 2 === 0 ? (q % 1 === 0.5 ? 0.16 : 0.09) : 0.06;
    noiseHit(S.shaker, T(b, q) + swing(q), 0.07, 55, 0.75, s === "in" ? acc * 0.6 : acc, 0.25); }
  if (groove || (s === "in" && b >= 6)) { // rim on the syncopations, softly
    for (const q of [0.75, 2.25, 3.5]) { const s0 = Math.round(T(b, q) * SR);
      for (let i = 0; i < SR * 0.05; i++) { const x = i / SR, v = (Math.sin(2 * Math.PI * 1650 * x) * 0.6 + rnd() * 0.3) * Math.exp(-x * 90) * 0.22;
        add(S.rim, s0 + i, v * 0.7, v * 1.2); } } }
  if (groove) { for (const q of [1, 3]) { const s0 = Math.round(T(b, q) * SR); let lp = 0;
      for (let i = 0; i < SR * 0.45; i++) { const x = i / SR, nz = rnd(); lp += 0.3 * (nz - lp);
        const env = [0, 0.009, 0.019].reduce((a, o) => a + (x >= o ? Math.exp(-(x - o) * (o > 0.015 ? 11 : 140)) : 0), 0);
        const v = (nz - lp) * env * 0.4; add(S.clap, s0 + i, v, v * 0.92); } }
    for (let q = 0.5; q < 4; q += 1) noiseHit(S.shaker, T(b, q), 0.3, 12, 0.55, 0.08, -0.3); } // open hat
}
// sweeps into B and C, a soft downlifter out of them
function sweep(t0, t1, g, up = true) { const a = Math.round(t0 * SR), b = Math.round(t1 * SR); let lp = 0;
  for (let i = a; i < b && i < N; i++) { const p = (i - a) / (b - a), c = up ? 0.01 + 0.35 * p * p : 0.36 - 0.35 * p, nz = rnd(); lp += c * (nz - lp);
    const env = up ? p * p : (1 - p) ** 2; add(S.fx, i, lp * env * g * (1 - p * 0.4), lp * env * g * (0.6 + p * 0.4)); } }
sweep(T(18), T(20), 0.5); sweep(T(36), T(38), 0.55); sweep(T(16), T(17), 0.35, false); sweep(T(34), T(35), 0.35, false);

// apply the pump to everything melodic, then write
for (const k of ["sub", "pad", "keys"]) for (const ch of S[k]) for (let i = 0; i < N; i++) ch[i] *= pump[i];
const wav = (L, R) => { const n = L.length, b = Buffer.alloc(44 + n * 4);
  b.write("RIFF", 0); b.writeUInt32LE(36 + n * 4, 4); b.write("WAVEfmt ", 8); b.writeUInt32LE(16, 16); b.writeUInt16LE(1, 20); b.writeUInt16LE(2, 22);
  b.writeUInt32LE(SR, 24); b.writeUInt32LE(SR * 4, 28); b.writeUInt16LE(4, 32); b.writeUInt16LE(16, 34); b.write("data", 36); b.writeUInt32LE(n * 4, 40);
  let pk = 1e-9; for (let i = 0; i < n; i++) pk = Math.max(pk, Math.abs(L[i]), Math.abs(R[i]));
  const g = 0.89 / pk;
  for (let i = 0; i < n; i++) { b.writeInt16LE(Math.round(L[i] * g * 32767), 44 + i * 4); b.writeInt16LE(Math.round(R[i] * g * 32767), 46 + i * 4); }
  return b; };
for (const [k, [L, R]] of Object.entries(S)) writeFileSync(`deep/${k}.wav`, wav(L, R));
writeFileSync("deep/song.json", JSON.stringify({ bpm, bars: END, end: T(END), total: TOTAL }));
console.log(`stems → deep/ (${Object.keys(S).join(", ")})`);
