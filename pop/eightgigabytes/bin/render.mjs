#!/usr/bin/env node
// render.mjs — eight gigabytes, start to finish:
//   1. check the score (syllables = notes, no member sings over itself)
//   2. write one .mbscore per member and sing it with Menu Band's own
//      offline singer (slab/menuband singrender — the same MenuBandSinger
//      the trio performs with; Noelle / Allison / Junior on this Mac)
//   3. build the bed in node from synthesized parts (bottom-up, pop/SCORE.md):
//      clock tick, kick, trackpad tap, sub bass, sine pad, menu-bar FM
//      piano, the GM-78-style whistle answering the voices
//   4. place every sung line at its beat, treat the vocal bus, mix, master
//      (ffmpeg loudnorm −14 LUFS / −1.5 dBTP) → out/eightgigabytes.{wav,mp3}
//   5. hear it back: whisper-cli on neo's lines, WER per line → out/hear.json
//
//   node pop/eightgigabytes/bin/render.mjs            # everything
//   node pop/eightgigabytes/bin/render.mjs --no-sing  # reuse rendered voices
//   node pop/eightgigabytes/bin/render.mjs --no-hear  # skip the whisper pass
//   node pop/eightgigabytes/bin/render.mjs --check    # validate the score only

import { existsSync, mkdirSync, readFileSync, writeFileSync, rmSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { instrumentEvents, kind } from "../arrangement.mjs";
import { BPM, TITLE, KEY, SECTIONS, AT, TOTAL_BEATS, CHORDS, ROOT, LINES, VOCAL_GAIN } from "../score.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..");
const OUT = resolve(LANE, "out");
const STEMS = resolve(OUT, "stems");
const SING = process.env.SINGRENDER || resolve(REPO, "slab/menuband/.build/release/singrender");
const WHISPER_MODEL = process.env.WHISPER_MODEL
  || [resolve(REPO, "recap/models/ggml-small.en.bin"), resolve(REPO, "recap/models/ggml-base.en.bin")].find(existsSync);
const flags = Object.fromEntries(process.argv.slice(2).map((a) => a.replace(/^--/, "").split("=")).map(([k, v]) => [k, v ?? true]));
mkdirSync(STEMS, { recursive: true });

const SR = 48000;
const beat = 60 / BPM;
const secs = (b) => b * beat;
const hz = (m) => 440 * 2 ** ((m - 69) / 12);
const dB = (d) => 10 ** (d / 20);
const MEMBERS = ["neo", "blueberry", "frisbee"];
const VOICE = { neo: "Noelle", blueberry: "Allison", frisbee: "Junior" };
const PAN = { neo: 0, blueberry: 0.38, frisbee: -0.38 };     // blueberry is the right channel (voice.json)
const PRE = 0.6;                                             // seconds before beat 0
const TAIL = 2.5;

// ── 1. check ───────────────────────────────────────────────────────────
const toks = (n) => n.trim().split(/\s+/).map((t) => { const [k, d] = t.split(":"); return { k, d: Number(d) }; });
const syls = (w) => w.trim().split(/\s+/).flatMap((x) => x.split("-")).length;
let bad = 0;
for (const l of LINES) {
  const n = toks(l.n), pitched = n.filter((t) => t.k !== "r").length, s = syls(l.w);
  l.len = n.reduce((a, t) => a + t.d, 0);
  l.end = l.at + l.len;
  if (pitched !== s) { console.error(`✗ ${l.m} @${l.at} "${l.w}": ${s} syllables, ${pitched} notes`); bad++; }
  for (const t of n) if (t.k !== "r" && !Number.isInteger(Number(t.k))) { console.error(`✗ bad note ${t.k}`); bad++; }
}
for (const m of MEMBERS) {
  const ls = LINES.filter((l) => l.m === m).sort((a, b) => a.at - b.at);
  for (let i = 1; i < ls.length; i++) if (ls[i].at < ls[i - 1].end - 1e-9) {
    console.error(`✗ ${m}: "${ls[i - 1].w}" (ends ${ls[i - 1].end}) overlaps "${ls[i].w}" (at ${ls[i].at})`); bad++;
  }
}
if (bad) process.exit(1);
console.log(`✓ score: ${LINES.length} lines, ${TOTAL_BEATS} beats, ${(secs(TOTAL_BEATS)).toFixed(1)} s at ${BPM} bpm`);
if (flags.check) process.exit(0);

// ── 2. sing ────────────────────────────────────────────────────────────
// One mbscore per member: every line in order, rests between. singrender
// splits on " / " and gives each line back as its own wav + spanOffset
// (seconds from beat 0 to the wav's first sample).
function memberScore(m) {
  const ls = LINES.filter((l) => l.m === m).sort((a, b) => a.at - b.at);
  const notes = [], lyrics = [];
  let pos = 0;
  for (const l of ls) {
    if (l.at > pos + 1e-9) notes.push(`r:${+(l.at - pos).toFixed(4)}`);
    for (const t of toks(l.n)) notes.push(`${t.k}:${t.d}`);
    lyrics.push(l.w);
    pos = l.end;
  }
  return { ls, notes: notes.join(","), lyrics: lyrics.join(" / ") };
}
const voiceJson = (m) => {
  const p = resolve(REPO, "grants/culturehub-la-2026/macneopolitan/members", m, "voice.json");
  return existsSync(p) ? JSON.parse(readFileSync(p, "utf8")).aesthetivox : {};
};
const manifests = {};
for (const m of MEMBERS) {
  const dir = resolve(STEMS, m);
  const manPath = resolve(dir, "manifest.json");
  const sc = memberScore(m);
  writeFileSync(resolve(STEMS, `${m}.mbscore`), JSON.stringify({
    title: `${TITLE} — ${m}`, bpm: BPM, machines: 3, voices: [{ name: `${m} (${VOICE[m]})`, program: 78, velocity: 84,
      singVoice: VOICE[m], sayVoice: VOICE[m], lyrics: sc.lyrics, notes: sc.notes }] }, null, 1));
  if (flags["no-sing"] && existsSync(manPath)) { manifests[m] = JSON.parse(readFileSync(manPath, "utf8")); continue; }
  if (!existsSync(SING)) { console.error(`✗ ${SING} missing — cd slab/menuband && swift build -c release --product singrender`); process.exit(1); }
  rmSync(dir, { recursive: true, force: true }); mkdirSync(dir, { recursive: true });
  const prof = voiceJson(m);
  const kv = [`notes=${sc.notes}`, `lyrics=${sc.lyrics}`, `singVoice=${VOICE[m]}`,
    `singVibratoHz=${prof.sing?.vibrato_hz ?? 5}`, `singVibCents=${prof.sing?.vibrato_depth_cents ?? 18}`,
    `singLock=${prof.sing?.harmony_lock ?? 0.875}`, `singF0Floor=${prof.f0_floor ?? 70}`].join(";");
  process.stdout.write(`▸ ${m} sings ${sc.ls.length} lines in ${VOICE[m]} … `);
  const t0 = Date.now();
  let man = null;
  for (let attempt = 0; attempt < 3 && !man; attempt++) {
    const r = spawnSync(SING, ["--kv", kv, "--bpm", String(BPM), "--fs", String(SR), "--out", dir],
      { encoding: "utf8", maxBuffer: 1 << 27, env: { ...process.env, SINGER_SPEECH_CACHE: resolve(OUT, ".speech-cache") } });
    if (r.status !== 0) { console.error(`\n  singrender failed (attempt ${attempt + 1})\n${(r.stderr || "").slice(-600)}`); continue; }
    const j = JSON.parse(r.stdout);
    const failed = j.lines.filter((l) => l.error || !l.wav);
    if (failed.length) { console.error(`\n  ${failed.length} line(s) failed: ${failed.map((l) => l.lyrics).join(" | ")}`); if (attempt < 2) continue; }
    man = j;
  }
  if (!man) process.exit(1);
  writeFileSync(manPath, JSON.stringify(man, null, 1));
  manifests[m] = man;
  console.log(`${man.lines.length} wavs, ${((Date.now() - t0) / 1000).toFixed(1)} s`);
}

// ── wav i/o ────────────────────────────────────────────────────────────
function readWav(path) {
  const b = readFileSync(path);
  const fmt = { ch: b.readUInt16LE(22), sr: b.readUInt32LE(24), bits: b.readUInt16LE(34), tag: b.readUInt16LE(20) };
  let o = 12, data = null;
  while (o + 8 <= b.length) {
    const id = b.toString("ascii", o, o + 4), len = b.readUInt32LE(o + 4);
    if (id === "data") { data = b.subarray(o + 8, o + 8 + len); break; }
    o += 8 + len + (len & 1);
  }
  const n = data.length / (fmt.bits / 8) / fmt.ch;
  const out = new Float32Array(n);
  for (let i = 0; i < n; i++) {
    let v = 0;
    for (let c = 0; c < fmt.ch; c++) {
      const idx = (i * fmt.ch + c) * (fmt.bits / 8);
      v += fmt.tag === 3 ? data.readFloatLE(idx) : fmt.bits === 16 ? data.readInt16LE(idx) / 32768
        : fmt.bits === 24 ? ((data[idx] | (data[idx + 1] << 8) | (data[idx + 2] << 16)) << 8 >> 8) / 8388608 : data.readInt32LE(idx) / 2147483648;
    }
    out[i] = v / fmt.ch;
  }
  if (fmt.sr !== SR) {                    // linear resample
    const m = Math.round(n * SR / fmt.sr), r = new Float32Array(m);
    for (let i = 0; i < m; i++) { const x = i * fmt.sr / SR, j = Math.floor(x), f = x - j; r[i] = (out[j] ?? 0) * (1 - f) + (out[j + 1] ?? 0) * f; }
    return r;
  }
  return out;
}
function writeWavF32(path, L, R) {
  const n = L.length, b = Buffer.alloc(44 + n * 8);
  b.write("RIFF", 0); b.writeUInt32LE(36 + n * 8, 4); b.write("WAVE", 8); b.write("fmt ", 12);
  b.writeUInt32LE(16, 16); b.writeUInt16LE(3, 20); b.writeUInt16LE(2, 22); b.writeUInt32LE(SR, 24);
  b.writeUInt32LE(SR * 8, 28); b.writeUInt16LE(8, 32); b.writeUInt16LE(32, 34); b.write("data", 36); b.writeUInt32LE(n * 8, 40);
  for (let i = 0; i < n; i++) { b.writeFloatLE(L[i], 44 + i * 8); b.writeFloatLE(R[i], 48 + i * 8); }
  writeFileSync(path, b);
}

// ── 3. the bed ─────────────────────────────────────────────────────────
const N = Math.ceil((PRE + secs(TOTAL_BEATS) + TAIL) * SR);
const bus = () => ({ L: new Float32Array(N), R: new Float32Array(N) });
const drums = bus(), bass = bus(), pad = bus(), keys = bus(), whistle = bus(), vox = bus();
const voxBy = { neo: bus(), blueberry: bus(), frisbee: bus() };
const T = (b) => PRE + secs(b);                                // beat → seconds
function place(b, sig, t, gain = 1, pan = 0) {
  const i0 = Math.round(t * SR), gl = gain * Math.cos((pan + 1) * Math.PI / 4), gr = gain * Math.sin((pan + 1) * Math.PI / 4);
  for (let i = 0; i < sig.length && i0 + i < N; i++) { b.L[i0 + i] += sig[i] * gl; b.R[i0 + i] += sig[i] * gr; }
}
let seed = 8;
const rnd = () => { seed = (seed * 1664525 + 1013904223) >>> 0; return seed / 4294967296 * 2 - 1; };
const env = (n, a, d, curve = 5) => { const e = new Float32Array(n); for (let i = 0; i < n; i++) { const t = i / SR; e[i] = t < a ? t / a : Math.exp(-(t - a) / d * curve); } return e; };
function biquadBP(x, f, q) {                                    // simple RBJ bandpass
  const w = 2 * Math.PI * f / SR, al = Math.sin(w) / (2 * q), b0 = al, b2 = -al, a0 = 1 + al, a1 = -2 * Math.cos(w), a2 = 1 - al;
  const y = new Float32Array(x.length); let x1 = 0, x2 = 0, y1 = 0, y2 = 0;
  for (let i = 0; i < x.length; i++) { const v = (b0 * x[i] + b2 * x2 - a1 * y1 - a2 * y2) / a0; x2 = x1; x1 = x[i]; y2 = y1; y1 = v; y[i] = v; }
  return y;
}
function kick(vel = 1) {
  const n = Math.round(0.42 * SR), y = new Float32Array(n); let ph = 0;
  for (let i = 0; i < n; i++) { const t = i / SR, f = 46 + 90 * Math.exp(-t * 28); ph += 2 * Math.PI * f / SR; y[i] = Math.tanh(1.6 * Math.sin(ph)) * Math.exp(-t * 7.5) * vel + (t < 0.004 ? rnd() * 0.25 * (1 - t / 0.004) : 0); }
  return y;
}
function tap(vel = 1) {                                          // fingers on a trackpad
  const n = Math.round(0.16 * SR), noise = new Float32Array(n);
  for (let i = 0; i < n; i++) noise[i] = rnd();
  const bp = biquadBP(noise, 2300, 1.3), e = env(n, 0.001, 0.028), y = new Float32Array(n);
  for (let i = 0; i < n; i++) { const t = i / SR; y[i] = (bp[i] * 1.8 * e[i] + Math.sin(2 * Math.PI * 185 * t) * Math.exp(-t * 45) * 0.55) * vel; }
  return y;
}
function tick(vel = 1, f = 3150) {                               // the clock
  const n = Math.round(0.03 * SR), y = new Float32Array(n);
  for (let i = 0; i < n; i++) { const t = i / SR; y[i] = (Math.sin(2 * Math.PI * f * t) * Math.exp(-t * 700) + rnd() * 0.12 * Math.exp(-t * 1400)) * vel; }
  return y;
}
function tone(midi, dur, { partials = [[1, 1]], a = 0.005, r = 0.06, vib = 0, vibHz = 5.5, breath = 0, sat = 0 } = {}) {
  const n = Math.round((dur + r) * SR), y = new Float32Array(n), f0 = hz(midi);
  let ph = 0;
  for (let i = 0; i < n; i++) {
    const t = i / SR, g = t < a ? t / a : t < dur ? 1 : Math.max(0, 1 - (t - dur) / r);
    const v = vib ? 2 ** (vib / 1200 * Math.sin(2 * Math.PI * vibHz * t) * Math.min(1, t / 0.25)) : 1;
    ph += 2 * Math.PI * f0 * v / SR;
    let s = 0; for (const [ratio, amp] of partials) s += amp * Math.sin(ph * ratio);
    if (breath) s += rnd() * breath;
    y[i] = (sat ? Math.tanh(s * sat) / Math.tanh(sat) : s) * g;
  }
  return y;
}
function fmKey(midi, dur, vel = 1) {                             // the piano in the menu bar
  const n = Math.round((dur + 0.5) * SR), y = new Float32Array(n), f = hz(midi);
  for (let i = 0; i < n; i++) {
    const t = i / SR, idx = 2.6 * Math.exp(-t * 6) + 0.25, g = Math.exp(-t * 2.2) * (t < dur ? 1 : Math.exp(-(t - dur) * 14));
    y[i] = Math.sin(2 * Math.PI * f * t + idx * Math.sin(2 * Math.PI * 2 * f * t)) * g * vel * Math.min(1, t / 0.003);
  }
  return y;
}
const padTone = (m, dur) => tone(m, dur, { partials: [[1, 1], [2, 0.3], [3, 0.12], [0.5, 0.25], [4, 0.05]], a: 0.5, r: 0.7 });
const bassTone = (m, dur) => tone(m, dur, { partials: [[1, 1], [2, 0.35], [3, 0.08]], a: 0.006, r: 0.05, sat: 1.4 });
const whistleTone = (m, dur) => tone(m, dur, { partials: [[1, 1], [2, 0.05]], a: 0.045, r: 0.12, vib: 22, vibHz: 5.2, breath: 0.035 });

for (const e of instrumentEvents()) {
  const synth = { kick: () => kick(e.velocity), tap: () => tap(e.velocity),
    tick: () => tick(e.velocity, e.frequency), bass: () => bassTone(e.midi, e.duration),
    pad: () => padTone(e.midi, e.duration), keys: () => fmKey(e.midi, e.duration, e.velocity),
    whistle: () => whistleTone(e.midi, e.duration) };
  place({ drums, bass, pad, keys, whistle }[e.bus], synth[e.type](), PRE + e.t, e.gain, e.pan);
}

// ── 4. voices on the timeline ──────────────────────────────────────────
// vocal treatment through the house chain when acdsp is built (pop/dsp/c)
const ACDSP = resolve(REPO, "pop/dsp/c/acdsp");
const master = existsSync(ACDSP) ? await import(resolve(REPO, "pop/lib/master.mjs")) : null;
function treated(wav) {
  if (!master) return readWav(wav);
  const out = wav.replace(/\.wav$/, ".vx.wav");
  const r = master.processWav(wav, out, master.presets.vocalLead({ in_db: 0, out_db: -1 }), { quiet: true });
  return readWav(r.ok ? out : wav);
}
const placedLines = [];
for (const m of MEMBERS) {
  const sc = memberScore(m), man = manifests[m];
  man.lines.forEach((ml, i) => {
    const l = sc.ls[i];
    if (!ml.wav || !l) return;
    const role = l.w === "hmm" ? "hum" : (m === "frisbee" && l.len <= 2.5) ? "echo" : m;
    const g = dB(VOCAL_GAIN[role]);
    const sig = treated(ml.wav);
    place(vox, sig, PRE + ml.spanOffset, g * 0.9, PAN[m] * (role === "hum" ? 1.6 : 1));
    place(voxBy[m], sig, PRE + ml.spanOffset, g * 0.9, 0);
    placedLines.push({ m, role, at: l.at, w: l.w, n: l.n, wav: ml.wav, spanOffset: ml.spanOffset, duration: ml.duration });
  });
}
// a small room on the voices (Schroeder: 4 combs + 2 allpass), wet under
function reverb(x, combs, wet) {
  const y = new Float32Array(x.length), bufs = combs.map((d) => new Float32Array(Math.round(d * SR))), idx = combs.map(() => 0), lp = combs.map(() => 0);
  for (let i = 0; i < x.length; i++) {
    let s = 0;
    for (let c = 0; c < combs.length; c++) { const b = bufs[c]; const v = b[idx[c]]; lp[c] = lp[c] * 0.35 + v * 0.65; b[idx[c]] = x[i] + lp[c] * 0.79; idx[c] = (idx[c] + 1) % b.length; s += v; }
    y[i] = s / combs.length;
  }
  for (const d of [0.0051, 0.0017]) { const b = new Float32Array(Math.round(d * SR)); let k = 0; for (let i = 0; i < y.length; i++) { const v = b[k]; const u = y[i] + v * 0.5; b[k] = u; k = (k + 1) % b.length; y[i] = v - 0.5 * u; } }
  for (let i = 0; i < y.length; i++) y[i] *= wet;
  return y;
}
const vmono = new Float32Array(N); for (let i = 0; i < N; i++) vmono[i] = (vox.L[i] + vox.R[i]) * 0.5;
const rvL = reverb(vmono, [0.0297, 0.0371, 0.0411, 0.0437], dB(-15)), rvR = reverb(vmono, [0.0313, 0.0353, 0.0427, 0.0449], dB(-15));

// ── mix ────────────────────────────────────────────────────────────────
// 2026-09-26 jeffrey: "the instruments are a bit too loud vs the voices" → bed −3.5 dB, harmonies up
const MIX = { drums: dB(-10.5), bass: dB(-17.5), pad: dB(-9), keys: dB(-9.5), whistle: dB(-11.5), vox: dB(0.5) };
// the bed breathes: quieter under the intro, the bridge and the outro
const bedScale = (i) => { const b = (i / SR - PRE) / beat, id = b < 0 ? "intro" : kind(Math.min(b, TOTAL_BEATS - 1e-6)); return id === "bridge" ? 0.6 : id === "intro" ? 0.72 : id === "outro" ? 0.75 : 1; };
const BED = new Set(["drums", "bass", "pad", "keys"]);
const L = new Float32Array(N), Rr = new Float32Array(N);
const scale = new Float32Array(N); { let cur = bedScale(0); for (let i = 0; i < N; i++) { const want = bedScale(i); cur += (want - cur) * 0.00004; scale[i] = cur; } }
for (const [name, b] of Object.entries({ drums, bass, pad, keys, whistle, vox })) { const bed = BED.has(name); for (let i = 0; i < N; i++) { const g = MIX[name] * (bed ? scale[i] : 1); L[i] += b.L[i] * g; Rr[i] += b.R[i] * g; } }
for (let i = 0; i < N; i++) { L[i] += rvL[i] * MIX.vox; Rr[i] += rvR[i] * MIX.vox; }
// gentle bus saturation + peak guard
let peak = 0; for (let i = 0; i < N; i++) { L[i] = Math.tanh(L[i] * 1.1); Rr[i] = Math.tanh(Rr[i] * 1.1); peak = Math.max(peak, Math.abs(L[i]), Math.abs(Rr[i])); }
const norm = dB(-1) / Math.max(peak, 1e-6); for (let i = 0; i < N; i++) { L[i] *= norm; Rr[i] *= norm; }
const RAW = resolve(OUT, "eightgigabytes-mix.wav");
writeWavF32(RAW, L, Rr);
for (const [name, b] of Object.entries({ drums, bass, pad, keys, whistle, vox })) writeWavF32(resolve(STEMS, `${name}.wav`), b.L, b.R);
for (const [m, b] of Object.entries(voxBy)) writeWavF32(resolve(STEMS, `vox-${m}.wav`), b.L, b.R);

// the record starts on the voice (jeffrey 2026-09-26): trim to a third of
// a second before neo's pickup, the wake chord caught mid-swell
const TRIM = Math.max(0, PRE + secs(AT.r1 - 1) - 0.35);
// master: two-pass loudnorm → 24-bit wav + 320k mp3
const WAV = resolve(OUT, "eightgigabytes.wav"), MP3 = resolve(OUT, "eightgigabytes.mp3");
const ln = "loudnorm=I=-14:TP=-1.5:LRA=9";
const m1 = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-i", RAW, "-af", `${ln}:print_format=json`, "-f", "null", "-"], { encoding: "utf8" });
const js = JSON.parse((m1.stderr.match(/\{[\s\S]*\}/) || ["{}"])[0]);
const ln2 = js.input_i ? `${ln}:measured_I=${js.input_i}:measured_TP=${js.input_tp}:measured_LRA=${js.input_lra}:measured_thresh=${js.input_thresh}:offset=${js.target_offset}:linear=true` : ln;
spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-ss", TRIM.toFixed(3), "-i", RAW, "-af", `afade=t=in:d=0.3,${ln2},alimiter=limit=0.94:attack=3:release=60`, "-ar", "48000", "-c:a", "pcm_s24le", WAV]);
spawnSync("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-i", WAV, "-c:a", "libmp3lame", "-b:a", "320k", "-metadata", `title=${TITLE}`, "-metadata", "artist=Aesthetic Dot Computer", MP3]);
const meas = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-i", WAV, "-af", "ebur128=peak=true", "-f", "null", "-"], { encoding: "utf8" }).stderr;
const last = (re) => { const all = [...meas.matchAll(re)]; return all.length ? all[all.length - 1][1] : "?"; };
const lufs = last(/I:\s+(-?[\d.]+) LUFS/g), lra = last(/LRA:\s+([\d.]+) LU/g), tp = last(/Peak:\s+(-?[\d.]+) dBFS/g);
console.log(`✓ ${MP3}\n  ${(N / SR).toFixed(1)} s · ${lufs} LUFS · LRA ${lra} · peak ${tp} dBTP`);
// excerpts for judging: the opening chorus alone, and the front of the
// record (intro → chorus → the crash → chorus)
for (const [name, endBeat] of [["chorus", AT.v2], ["front", AT.bridge]]) {
  const end = PRE - TRIM + secs(endBeat) + 0.3, CH = resolve(OUT, `eightgigabytes-${name}`);
  spawnSync("ffmpeg", ["-y", "-loglevel", "error", "-t", end.toFixed(3), "-i", WAV, "-af", `afade=t=out:st=${(end - 0.6).toFixed(3)}:d=0.6`, "-c:a", "pcm_s24le", `${CH}.wav`]);
  spawnSync("ffmpeg", ["-y", "-loglevel", "error", "-i", `${CH}.wav`, "-c:a", "libmp3lame", "-b:a", "320k", "-metadata", `title=${TITLE} (${name})`, `${CH}.mp3`]);
  console.log(`✓ ${CH}.mp3 (${end.toFixed(1)} s)`);
}

// ── 5. hear it back ────────────────────────────────────────────────────
const num = (s) => s.replace(/[^a-z' ]/g, " ").replace(/\s+/g, " ").trim();
function wer(ref, hyp) {
  const a = num(ref.toLowerCase()).split(" "), b = num(hyp.toLowerCase()).split(" ").filter(Boolean);
  const d = Array.from({ length: a.length + 1 }, (_, i) => [i, ...Array(b.length).fill(0)]); for (let j = 1; j <= b.length; j++) d[0][j] = j;
  for (let i = 1; i <= a.length; i++) for (let j = 1; j <= b.length; j++) d[i][j] = Math.min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + (a[i - 1] === b[j - 1] ? 0 : 1));
  return d[a.length][b.length] / a.length;
}
if (!flags["no-hear"] && WHISPER_MODEL && spawnSync("which", ["whisper-cli"]).status === 0) {
  const rows = [];
  for (const p of placedLines.filter((l) => l.role !== "hum" && l.role !== "echo")) {
    const w16 = p.wav.replace(/\.wav$/, ".16k.wav");
    spawnSync("ffmpeg", ["-y", "-loglevel", "error", "-i", p.wav, "-ar", "16000", "-ac", "1", w16]);
    const r = spawnSync("whisper-cli", ["-m", WHISPER_MODEL, "-f", w16, "-l", "en", "-nt", "-np", "-t", "4"], { encoding: "utf8" });
    rmSync(w16, { force: true });
    const heard = (r.stdout || "").replace(/\[[^\]]*\]/g, " ").replace(/\s+/g, " ").trim();
    const text = p.w.replace(/-/g, "");
    rows.push({ m: p.m, at: p.at, text, heard, wer: +wer(text, heard).toFixed(2) });
  }
  const mean = rows.reduce((a, r) => a + r.wer, 0) / rows.length;
  writeFileSync(resolve(OUT, "hear.json"), JSON.stringify({ model: WHISPER_MODEL.split("/").pop(), meanWer: +mean.toFixed(3), lines: rows }, null, 1));
  console.log(`\n♪ heard back (${WHISPER_MODEL.split("/").pop()}) — mean WER ${(mean * 100).toFixed(0)}%`);
  for (const r of rows) console.log(`  ${String(r.wer).padEnd(4)} ${r.m.padEnd(9)} ${r.text}\n       → ${r.heard}`);
}
writeFileSync(resolve(OUT, "timeline.json"), JSON.stringify({ title: TITLE, bpm: BPM, key: KEY, seconds: N / SR - TRIM, pre: PRE - TRIM, trim: TRIM, totalBeats: TOTAL_BEATS, sections: SECTIONS.map((s) => ({ id: s.id, at: +(PRE - TRIM + secs(s.start)).toFixed(2), bars: s.bars })), lines: placedLines }, null, 1));
