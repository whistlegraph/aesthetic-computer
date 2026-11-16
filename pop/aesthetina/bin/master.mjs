#!/usr/bin/env node
// master.mjs — aesthetina's master stage, from the house tools:
//   0. weight: the drums get a parallel acdsp drumSlam and a soft clip, the low stem
//      a saturator (harmonics above 180 Hz), so the level comes from the stems and
//      the limiter has little left to do.
//   1. spatial: each stem from render.mjs (out/stems-<arr>/) is played on a pair of
//      virtual speakers through the measured KEMAR head (fedac/native/tools/hrir,
//      as notespatial does it), blended with the dry stem so the low end and
//      the punch stay put. Kick and bass stay centred and dry. Stems with an
//      `orbit` travel: the pair circles the head once every `bars` bars.
//      The scene is elastic (after wannadash's elasticizeBus): every stem with a `body`
//      hangs on a spring, and the arrangement's IMPACTS throw them. Displacement is
//      position (azimuth through the head, and pan on the dry part), distance (level)
//      and propagation delay (the slide: a Doppler bend out and back); at the impact a
//      few milliseconds of the stem repeat as a shard. Words and bass keep their place.
//   2. bus: acdsp's danceMasterBus (pop/lib/master.mjs — 1176 glue + mud/presence/air).
//   3. law: notespatial/bin/master.sh — 20 Hz HP, bass mono under 120 Hz, one static
//      gain, 4× oversampled true-peak limiter. Target from pop/MASTERING.md.
//
//   node pop/aesthetina/bin/master.mjs [--arr=radio] [--lufs=-10] [--ceiling=-2]
//   → out/aesthetina-<arr>-master.{flac,mp3}
import { readFileSync, writeFileSync, mkdirSync, existsSync } from "node:fs";
import { execFileSync, spawnSync } from "node:child_process";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import * as master from "../../lib/master.mjs";

const HERE = dirname(fileURLToPath(import.meta.url)), ROOT = join(HERE, "..", "..", "..");
const arg = (k, d) => process.argv.find((a) => a.startsWith(`--${k}=`))?.split("=")[1] ?? d;
const ARR = arg("arr", "radio"), LUFS = Number(arg("lufs", -9)), CEIL = Number(arg("ceiling", -2.6));
const META = JSON.parse(readFileSync(join(HERE, "..", "arrangements", `${ARR}.json`), "utf8"));
const BAR = 240 / (META.bpm ?? 128);
const ARRMOD = await import(join(HERE, "..", "arrangements", `${ARR}.mjs`));
const IMPACTS = (ARRMOD.IMPACTS ?? []).map((e) => ({ ...e, t: e.bar * BAR }));
const LEVELS = ARRMOD.LEVELS ?? null;
const OUT = join(HERE, "..", "out"), STEMS = join(OUT, `stems-${ARR}`), WORK = join(OUT, `master-${ARR}`);
mkdirSync(WORK, { recursive: true });
const SR = 44100;

// where each stem sits: virtual speaker pair at ±az (deg), elevation, and how much of
// the binaural image is blended in (0 = dry stereo, 1 = all head)
const PLACE = {
  drums: { az: 0, el: 0, mix: 0, chain: "drumSlam", parallel: 0.5, clip: 4, gain: -2, subcut: 0.75, body: { mass: 1.42, dir: 0.7, shard: 0.017 } }, // slammed copy under the dry hits, peaks shaved
  low: { az: 0, el: 0, mix: 0, drive: 2.6, gain: 1 },
  lyric: { az: 0, el: 0, mix: 0, gain: 6, chain: "smoothVocal" }, // the statements: one voice, dead centre, dry, evenly held, smooth
  vox: { az: 25, el: 0, mix: 0.35, body: { mass: 0.74, dir: -0.42, shard: 0.026 } }, // toots, screams, chord words: beside the words
  choir: { az: 90, el: 10, mix: 0.8, orbit: { bars: 8 }, body: { mass: 0.6, dir: -0.9, shard: 0.023 } }, // circles the head, left and right opposite each other
  lead: { az: 20, el: 10, mix: 0.45, body: { mass: 0.9, dir: 0.5, shard: 0.021 } },
  guitar: { az: 55, el: 0, mix: 0.5, gain: 4, body: { mass: 1.0, dir: 0.86, shard: 0.031 } },
  piano: { az: 35, el: 0, mix: 0.4, gain: -1, body: { mass: 1.2, dir: -0.6, shard: 0.022 } }, // the grand: in front, a little wide
  synth: { az: 75, el: 20, mix: 0.7, gain: -2, body: { mass: 0.66, dir: 0.6, shard: 0.027 } }, // pad and arpeggio: wide and a little above
  keys: { az: 45, el: 0, mix: 0.55, body: { mass: 0.58, dir: -0.98, shard: 0.013 } },
  bells: { az: 60, el: 40, mix: 0.8, orbit: { bars: 16, dir: -1 }, body: { mass: 0.5, dir: 0.96, shard: 0.019 } }, // overhead, turning the other way
  space: { az: 120, el: 10, mix: 0.75, body: { mass: 0.8, dir: -0.7, shard: 0.029 } }, // the room and the pumping plate open up behind
};
for (const k of Object.keys(PLACE)) if (!existsSync(join(STEMS, `${k}.wav`))) delete PLACE[k]; // older renders have no choir stem

function readWav(file) {
  const b = readFileSync(file), n = (b.length - 44) / 8;
  const L = new Float32Array(n), R = new Float32Array(n);
  for (let i = 0; i < n; i++) { L[i] = b.readFloatLE(44 + i * 8); R[i] = b.readFloatLE(48 + i * 8); }
  return { L, R, n };
}
function readStem(name) {
  const p = PLACE[name], file = join(STEMS, `${name}.wav`);
  let s = readWav(file);
  if (p.chain) { // a stem's own acdsp chain before it's placed (parallel: that share of the chained copy beside the dry)
    const pre = join(WORK, `0-${name}.wav`);
    const spec = CHAINS[p.chain] ? CHAINS[p.chain]() : master.presets[p.chain]({ in_db: 0, out_db: 0 });
    const r = master.processWav(file, pre, spec, { float: true, quiet: true });
    if (!r.ok) throw new Error(`acdsp ${name}: ${r.stderr}`);
    const c = readWav(pre), w = p.parallel ?? 1;
    for (let i = 0; i < s.n; i++) { s.L[i] = s.L[i] * (1 - w) + c.L[i] * w; s.R[i] = s.R[i] * (1 - w) + c.R[i] * w; }
  }
  if (p.subcut) { // take that share of everything under ~70 Hz out: the kick keeps its knock and chest, loses the rumble
    const a = 1 - Math.exp(-2 * Math.PI * 70 / SR); let l = 0, r = 0;
    for (let i = 0; i < s.n; i++) { l += a * (s.L[i] - l); r += a * (s.R[i] - r); s.L[i] -= p.subcut * l; s.R[i] -= p.subcut * r; }
  }
  if (p.drive || p.clip) {
    let pk = 0; for (let i = 0; i < s.n; i++) pk = Math.max(pk, Math.abs(s.L[i]), Math.abs(s.R[i]));
    if (p.drive) { // saturate: odd harmonics on the bass, level kept
      const k = p.drive / pk, g = pk / Math.tanh(p.drive);
      for (let i = 0; i < s.n; i++) { s.L[i] = Math.tanh(s.L[i] * k) * g; s.R[i] = Math.tanh(s.R[i] * k) * g; }
    }
    if (p.clip) { // soft clip: everything above (peak − clip dB) folds into a tanh knee, then the headroom is given back
      const thr = pk * 10 ** (-p.clip / 20), room = pk - thr;
      const f = (x) => { const a = Math.abs(x); return a <= thr * 0.7 ? x : Math.sign(x) * (thr * 0.7 + thr * 0.3 * Math.tanh((a - thr * 0.7) / (thr * 0.3))); };
      const up = pk / thr;
      for (let i = 0; i < s.n; i++) { s.L[i] = f(s.L[i]) * up; s.R[i] = f(s.R[i]) * up; }
    }
  }
  return s;
}

// the head: measured KEMAR, energy-equalised per direction (as climbalift.mjs)
const idx = JSON.parse(readFileSync(join(ROOT, "fedac/native/tools/hrir/kemar-compact.json"), "utf8"));
const raw = readFileSync(join(ROOT, "fedac/native/tools/hrir/kemar-compact.bin")), taps = idx.taps;
// a gentler lead chain than acdsp's vocalLead (8:1, presence +2): 4:1 with a slower release,
// the harsh and sibilant bands down, a little warmth, less presence push
const CHAINS = {
  smoothVocal: () => master.chain(master.eq("rumble"), master.compressor("1176", { ratio: 4, in: 0, out: 0, attack: 4, release: 5, iron: 0.4 }),
    master.eq("nasal", -1.5), master.eq("harshness", -2), master.eq("presence", +1), master.eq("sibilance", -3), master.eq("warmth", +1), master.eq("air", +1)),
};
const HR = new Map();
function hrir(azDeg, elDeg) {
  const key = `${Math.round(azDeg)}|${elDeg}`;
  if (!HR.has(key)) HR.set(key, hrirRaw(azDeg, elDeg));
  return HR.get(key);
}
function hrirRaw(azDeg, elDeg) {
  const ring = idx.elevations.reduce((b, e) => (Math.abs(e.el - elDeg) < Math.abs(b.el - elDeg) ? e : b));
  let az = ((azDeg % 360) + 360) % 360, swap = false;
  if (az > 180) { az = 360 - az; swap = true; }
  const a = ring.azimuths.reduce((b, c) => (Math.abs(c.az - az) < Math.abs(b.az - az) ? c : b));
  const Lr = new Float32Array(taps), Rr = new Float32Array(taps);
  for (let i = 0; i < taps; i++) { Lr[i] = raw.readInt16LE(a.off + i * 2) / 32768; Rr[i] = raw.readInt16LE(a.off + taps * 2 + i * 2) / 32768; }
  let e = 0; for (let i = 0; i < taps; i++) e += Lr[i] * Lr[i] + Rr[i] * Rr[i];
  const g = Math.sqrt(1.84 / e) / Math.SQRT2; // two speakers per stem: keep the level
  for (let i = 0; i < taps; i++) { Lr[i] *= g; Rr[i] *= g; }
  return swap ? [Rr, Lr] : [Lr, Rr];
}
function convAdd(x, ir, out, w) {
  const n = x.length, t = ir.length;
  for (let i = 0; i < n; i++) {
    const xi = x[i]; if (xi === 0) continue;
    const lim = Math.min(t, n - i), v = xi * w;
    for (let k = 0; k < lim; k++) out[i + k] += v * ir[k];
  }
}

// the elastic field. Returns the stem's signed displacement over time (null when it never moves)
// and rewrites the stem in place: delayed by its travel, attenuated by distance, fractured at the impact.
const clamp = (x, a, b) => Math.min(b, Math.max(a, x)), smooth = (x) => x * x * (3 - 2 * x);
function elastic(s, body, bi, name) {
  if (!IMPACTS.length) return null;
  const N = s.n, spring = new Float32Array(N);
  const lin = (a, q) => { if (q <= 0) return a[0]; const k = q | 0; return k + 1 >= a.length ? a[a.length - 1] : a[k] + (a[k + 1] - a[k]) * (q - k); };
  for (const ex of IMPACTS) {
    const start = Math.max(0, Math.round((ex.t - 0.08) * SR)), end = Math.min(N, Math.round((ex.t + ex.duration + 0.1) * SR));
    const srcL = s.L.slice(start, end), srcR = s.R.slice(start, end);
    const anchor = Math.max(0, Math.round((ex.t + 0.035 + bi * 0.009) * SR) - start), grain = Math.max(16, Math.round(body.shard * SR));
    const grounded = name === "drums" ? ex.keepTime ?? 1 : 1, hzB = ex.hz * (1 + (ex.dispersion ?? 0) * (bi - 4) / 4);
    for (let i = Math.max(start, Math.round(ex.t * SR)); i < end; i++) {
      const age = i / SR - ex.t; if (age > ex.duration) break;
      const edge = smooth(clamp(age / ex.attack, 0, 1)) * smooth(clamp((ex.duration - age) / ex.release, 0, 1));
      const sp = (ex.strength / body.mass) * Math.exp(-ex.damping * age) * Math.sin(2 * Math.PI * hzB * age) * edge * grounded;
      spring[i] += sp;
      const travel = Math.abs(sp), local = i - start, pos = Math.max(0, local - travel * (0.012 + 0.021 / body.mass) * SR);
      const dist = (1 + 0.11 * travel) / (1 + 0.24 * travel);
      let ml = lin(srcL, pos) * dist, mr = lin(srcR, pos) * dist;
      const fracture = ex.glitch * Math.exp(-2.7 * age) * smooth(clamp(age / 0.035, 0, 1)) * edge;
      if (fracture > 0.012) {
        const gp = Math.min(srcL.length - 2, anchor + ((local - anchor + grain * 64) % grain)), gl = lin(srcL, gp), gr = lin(srcR, gp), swap = bi & 1;
        ml = ml * (1 - 0.34 * fracture) + (swap ? gr : gl) * 0.34 * fracture; mr = mr * (1 - 0.34 * fracture) + (swap ? gl : gr) * 0.34 * fracture;
      }
      const wet = grounded * edge * clamp(0.14 + 0.58 * travel + 0.08 * fracture, 0, 0.9);
      s.L[i] = srcL[local] * (1 - wet) + ml * wet; s.R[i] = srcR[local] * (1 - wet) + mr * wet;
    }
  }
  return spring;
}

// the head: Hann blocks (50 % overlap), each convolved with the measured direction the
// pair has at that moment (its orbit plus its displacement), so the image glides
const SWING = 75; // degrees of azimuth per unit of displacement
function placeAdd(s, p, spring, L, R) {
  const H = 1024, W = 2 * H, win = new Float32Array(W);
  for (let i = 0; i < W; i++) win[i] = 0.5 - 0.5 * Math.cos((2 * Math.PI * i) / W);
  const per = p.orbit ? p.orbit.bars * BAR * SR : 0, dir = p.orbit?.dir ?? 1, N = s.n, bl = new Float32Array(W), br = new Float32Array(W);
  for (let b0 = -H; b0 < N; b0 += H) {
    const c = clamp(b0 + H, 0, N - 1);
    const turn = (per ? dir * 360 * ((b0 + H) / per) : 0) + (spring ? SWING * p.body.dir * clamp(spring[c], -1.6, 1.6) : 0);
    const [lL, lR] = hrir(turn - p.az, p.el), [rL, rR] = hrir(turn + p.az, p.el);
    let any = false;
    for (let i = 0; i < W; i++) { const j = b0 + i, ok = j >= 0 && j < N; bl[i] = ok ? s.L[j] * win[i] : 0; br[i] = ok ? s.R[j] * win[i] : 0; any ||= bl[i] !== 0 || br[i] !== 0; }
    if (!any) continue;
    for (let i = 0; i < W; i++) {
      const j = b0 + i, xl = bl[i] * p.mix, xr = br[i] * p.mix;
      if (j < 0 || (xl === 0 && xr === 0)) continue;
      const lim = Math.min(taps, N - j);
      for (let k = 0; k < lim; k++) { L[j + k] += xl * lL[k] + xr * rL[k]; R[j + k] += xl * lR[k] + xr * rR[k]; }
    }
  }
}

// lyric-keyed ducking: the rest dips (up to DUCK dB) wherever a statement is sung
const DUCK = 4.5, DUCKED = new Set(["lead", "guitar", "synth", "piano", "keys", "bells", "vox", "choir", "space"]); // everything that shares the words' range
let voxDuck = null;
{
  const v = readStem("lyric"), env = new Float32Array(v.n);
  const att = Math.exp(-1 / (0.005 * SR)), rel = Math.exp(-1 / (0.18 * SR));
  let e = 0;
  for (let i = 0; i < v.n; i++) { const x = Math.max(Math.abs(v.L[i]), Math.abs(v.R[i])); e = x > e ? att * e + (1 - att) * x : rel * e + (1 - rel) * x; env[i] = e; }
  const ref = Float32Array.from(env).sort()[Math.floor(v.n * 0.9)] || 1;
  voxDuck = new Float32Array(v.n);
  for (let i = 0; i < v.n; i++) voxDuck[i] = 10 ** (-DUCK * Math.min(1, env[i] / ref) / 20);
}

// 1 · spatial
let N = 0;
const mixL = [], mixR = [];
const outL = () => mixL[0], outR = () => mixR[0];
for (const [name, p] of Object.entries(PLACE)) {
  const s = readStem(name);
  if (!N) { N = s.n; mixL.push(new Float32Array(N)); mixR.push(new Float32Array(N)); }
  const L = outL(), R = outR(), dry = 1 - p.mix, gs = 10 ** ((p.gain ?? 0) / 20);
  if (gs !== 1 || DUCKED.has(name)) for (let i = 0; i < s.n; i++) { const g = gs * (DUCKED.has(name) ? voxDuck[i] : 1); s.L[i] *= g; s.R[i] *= g; }
  const spring = p.body ? elastic(s, p.body, Object.keys(PLACE).indexOf(name), name) : null;
  for (let i = 0; i < N; i++) { // the dry part swings too: equal-power pan by displacement
    const pan = spring ? clamp(p.body.dir * spring[i] * 0.86, -0.92, 0.92) : 0;
    L[i] += s.L[i] * dry * Math.sqrt(1 - pan); R[i] += s.R[i] * dry * Math.sqrt(1 + pan);
  }
  if (p.mix > 0) placeAdd(s, p, spring, L, R);
  console.log(`  ${name.padEnd(6)} ±${p.az}° el ${p.el}° · head ${Math.round(p.mix * 100)}%${p.orbit ? ` · orbits every ${p.orbit.bars} bars` : ""}${p.body ? ` · mass ${p.body.mass}` : ""}${p.gain ? ` · ${p.gain > 0 ? "+" : ""}${p.gain} dB` : ""}${p.chain ? ` · ${p.chain}` : ""}${DUCKED.has(name) ? ` · ducks −${DUCK} dB under vocals` : ""}`);
}
function writeF32(path, L, R) {
  let pk = 0; for (let i = 0; i < L.length; i++) pk = Math.max(pk, Math.abs(L[i]), Math.abs(R[i]));
  const g = 0.5 / pk, n = L.length, b = Buffer.alloc(44 + n * 8); // −6 dBFS peak into the bus
  b.write("RIFF", 0); b.writeUInt32LE(36 + n * 8, 4); b.write("WAVEfmt ", 8); b.writeUInt32LE(16, 16); b.writeUInt16LE(3, 20);
  b.writeUInt16LE(2, 22); b.writeUInt32LE(SR, 24); b.writeUInt32LE(SR * 8, 28); b.writeUInt16LE(8, 32); b.writeUInt16LE(32, 34);
  b.write("data", 36); b.writeUInt32LE(n * 8, 40);
  for (let i = 0; i < n; i++) { const f = i > n - SR ? (n - i) / SR : 1; b.writeFloatLE(L[i] * g * f, 44 + i * 8); b.writeFloatLE(R[i] * g * f, 48 + i * 8); }
  writeFileSync(path, b);
}
const spatial = join(WORK, "1-spatial.wav");
writeF32(spatial, outL(), outR());

// 2 · bus: acdsp dance master bus
const bus = join(WORK, "2-bus.wav");
const chainSpec = master.chain(master.presets.danceMasterBus({ in_db: -2, out_db: +2, iron: 0.45 }), master.eq({ type: "peak", f: 280, q: 0.8, g: -2.5 }), master.eq({ type: "highshelf", f: 2500, q: 0.6, g: 2.5 })); // less low-mid, a shelf up from the upper mids: the mix was 75 % below 400 Hz
const r = master.processWav(spatial, bus, chainSpec, { float: true, quiet: true });
if (!r.ok) throw new Error(`acdsp: ${r.stderr}`);
console.log(`  bus    ${chainSpec}`);

// 2b · the level arc: the arrangement's LEVELS, applied after the bus so the compressor does not undo it
let lawIn = bus;
if (LEVELS) {
  const b = readWav(bus), arc = join(WORK, "3-arc.wav"), half = 0.5 * BAR * SR;
  const dbAt = (i) => {
    let k = 0; while (k + 1 < LEVELS.length && i >= LEVELS[k + 1][0] * BAR * SR - half) k++;
    const into = k > 0 ? Math.min(1, (i - (LEVELS[k][0] * BAR * SR - half)) / half) : 1;
    return k > 0 ? LEVELS[k - 1][1] + (LEVELS[k][1] - LEVELS[k - 1][1]) * into : LEVELS[0][1];
  };
  const n = b.n, buf = Buffer.alloc(44 + n * 8);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + n * 8, 4); buf.write("WAVEfmt ", 8); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20);
  buf.writeUInt16LE(2, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 8, 28); buf.writeUInt16LE(8, 32); buf.writeUInt16LE(32, 34);
  buf.write("data", 36); buf.writeUInt32LE(n * 8, 40);
  let g = 1;
  for (let i = 0; i < n; i++) { if (i % 64 === 0) g = 10 ** (dbAt(i) / 20); buf.writeFloatLE(b.L[i] * g, 44 + i * 8); buf.writeFloatLE(b.R[i] * g, 48 + i * 8); }
  writeFileSync(arc, buf); lawIn = arc;
  console.log(`  arc    ${LEVELS.map(([bar, db]) => `${bar}:${db > 0 ? "+" : ""}${db}`).join(" ")} dB`);
}

// 3 · law: measure, one static gain, true-peak limiter (notespatial/bin/master.sh)
const loud = (f) => {
  const t = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-i", f, "-af", "ebur128=peak=true", "-f", "null", "-"], { encoding: "utf8" }).stderr;
  const sum = t.slice(t.lastIndexOf("Summary"));
  return { I: Number(sum.match(/I:\s+(-?[\d.]+)/)[1]), LRA: Number(sum.match(/LRA:\s+(-?[\d.]+)/)[1]), TP: Number(sum.match(/Peak:\s+(-?[\d.]+)/)[1]) };
};
const pre = loud(lawIn);
const gain = +(LUFS - pre.I).toFixed(2);
const flac = join(OUT, `aesthetina-${ARR}-master.flac`), mp3 = join(OUT, `aesthetina-${ARR}-master.mp3`);
execFileSync("bash", [join(ROOT, "pop/notespatial/bin/master.sh"), lawIn, String(gain), flac, String(CEIL)]);
const post = loud(flac);
execFileSync("ffmpeg", ["-y", "-loglevel", "error", "-i", flac, "-b:a", "320k", "-metadata", `title=Aesthetina${ARR === "country" ? "" : ` (${ARR})`}`,
  "-metadata", "artist=Aesthetic Dot Computer", mp3]);
console.log(`  law    in ${pre.I} LUFS → +${gain} dB → limiter ${CEIL} dBTP`);
console.log(`master  ${post.I} LUFS · ${post.TP} dBTP · LRA ${post.LRA} · PLR ${(post.TP - post.I).toFixed(1)}\n${flac}\n${mp3}`);
