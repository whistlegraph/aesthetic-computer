#!/usr/bin/env node
// master.mjs — aesthetina's master stage, from the house tools:
//   1. spatial: each stem from render.mjs (out/stems-<arr>/) is played on a pair of
//      virtual speakers through the measured KEMAR head (fedac/native/tools/hrir,
//      as notespatial does it), blended with the dry stem so the low end and
//      the punch stay put. Kick and bass stay centred and dry.
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
const ARR = arg("arr", "radio"), LUFS = Number(arg("lufs", -10)), CEIL = Number(arg("ceiling", -2));
const OUT = join(HERE, "..", "out"), STEMS = join(OUT, `stems-${ARR}`), WORK = join(OUT, `master-${ARR}`);
mkdirSync(WORK, { recursive: true });
const SR = 44100;

// where each stem sits: virtual speaker pair at ±az (deg), elevation, and how much of
// the binaural image is blended in (0 = dry stereo, 1 = all head)
const PLACE = {
  drums: { az: 0, el: 0, mix: 0 },
  low: { az: 0, el: 0, mix: 0 },
  lyric: { az: 0, el: 0, mix: 0, gain: 4, chain: "vocalLead" }, // the statements: dead centre, dry, compressed, present
  vox: { az: 15, el: 0, mix: 0.3, gain: 0 }, // toots, choir, arpeggios, screams: around the words
  lead: { az: 20, el: 10, mix: 0.45 },
  keys: { az: 45, el: 0, mix: 0.55 },
  bells: { az: 70, el: 20, mix: 0.7 },
  space: { az: 120, el: 10, mix: 0.75 }, // the room and the pumping plate open up behind
};

function readStem(name) {
  let file = join(STEMS, `${name}.wav`);
  if (PLACE[name].chain) { // a stem's own acdsp chain before it's placed
    const pre = join(WORK, `0-${name}.wav`);
    const r = master.processWav(file, pre, master.presets[PLACE[name].chain]({ in_db: 0, out_db: 0 }), { float: true, quiet: true });
    if (!r.ok) throw new Error(`acdsp ${name}: ${r.stderr}`);
    file = pre;
  }
  const b = readFileSync(file), n = (b.length - 44) / 8;
  const L = new Float32Array(n), R = new Float32Array(n);
  for (let i = 0; i < n; i++) { L[i] = b.readFloatLE(44 + i * 8); R[i] = b.readFloatLE(48 + i * 8); }
  return { L, R, n };
}

// the head: measured KEMAR, energy-equalised per direction (as climbalift.mjs)
const idx = JSON.parse(readFileSync(join(ROOT, "fedac/native/tools/hrir/kemar-compact.json"), "utf8"));
const raw = readFileSync(join(ROOT, "fedac/native/tools/hrir/kemar-compact.bin")), taps = idx.taps;
function hrir(azDeg, elDeg) {
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

// lyric-keyed ducking: the rest dips (up to DUCK dB) wherever a statement is sung
const DUCK = 4, DUCKED = new Set(["lead", "keys", "bells", "vox", "space"]); // everything that shares the words' range
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
  for (let i = 0; i < N; i++) { L[i] += s.L[i] * dry; R[i] += s.R[i] * dry; }
  if (p.mix > 0) {
    const [lL, lR] = hrir(-p.az, p.el), [rL, rR] = hrir(p.az, p.el); // left speaker, right speaker
    convAdd(s.L, lL, L, p.mix); convAdd(s.L, lR, R, p.mix);
    convAdd(s.R, rL, L, p.mix); convAdd(s.R, rR, R, p.mix);
  }
  console.log(`  ${name.padEnd(6)} ±${p.az}° el ${p.el}° · head ${Math.round(p.mix * 100)}%${p.gain ? ` · +${p.gain} dB` : ""}${p.chain ? ` · ${p.chain}` : ""}${DUCKED.has(name) ? ` · ducks −${DUCK} dB under vocals` : ""}`);
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
const chainSpec = master.presets.danceMasterBus({ in_db: -2, out_db: +2, iron: 0.45 });
const r = master.processWav(spatial, bus, chainSpec, { float: true, quiet: true });
if (!r.ok) throw new Error(`acdsp: ${r.stderr}`);
console.log(`  bus    ${chainSpec}`);

// 3 · law: measure, one static gain, true-peak limiter (notespatial/bin/master.sh)
const loud = (f) => {
  const t = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-i", f, "-af", "ebur128=peak=true", "-f", "null", "-"], { encoding: "utf8" }).stderr;
  const sum = t.slice(t.lastIndexOf("Summary"));
  return { I: Number(sum.match(/I:\s+(-?[\d.]+)/)[1]), LRA: Number(sum.match(/LRA:\s+(-?[\d.]+)/)[1]), TP: Number(sum.match(/Peak:\s+(-?[\d.]+)/)[1]) };
};
const pre = loud(bus);
const gain = +(LUFS - pre.I).toFixed(2);
const flac = join(OUT, `aesthetina-${ARR}-master.flac`), mp3 = join(OUT, `aesthetina-${ARR}-master.mp3`);
execFileSync("bash", [join(ROOT, "pop/notespatial/bin/master.sh"), bus, String(gain), flac, String(CEIL)]);
const post = loud(flac);
execFileSync("ffmpeg", ["-y", "-loglevel", "error", "-i", flac, "-b:a", "320k", "-metadata", `title=Aesthetina${ARR === "country" ? "" : ` (${ARR})`}`,
  "-metadata", "artist=Aesthetic Dot Computer", mp3]);
console.log(`  law    in ${pre.I} LUFS → +${gain} dB → limiter ${CEIL} dBTP`);
console.log(`master  ${post.I} LUFS · ${post.TP} dBTP · LRA ${post.LRA} · PLR ${(post.TP - post.I).toFixed(1)}\n${flac}\n${mp3}`);
