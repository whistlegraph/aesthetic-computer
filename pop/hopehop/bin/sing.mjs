#!/usr/bin/env node
// sing.mjs — turn the spoken jeffrey-pvc stem into a SUNG, harmonized,
// emo-rapper lead: slow drawn-out melody, vowels held to the notes,
// stacked harmonies.
//
// Pipeline:
//   1. WORLD f0-replace (bin/pitchsnap_world.py): swap each word's pitch
//      onto a smooth pentatonic melody (higher register), anchored at the
//      word's real start, jeffrey's voice character preserved.
//   2. SLOW + VOWEL HOLD: place words on a time-scaled grid (--slow / or
//      derived from --target-dur) so the singing drags; split each word
//      into onset/vowel/coda, keep consonants crisp at natural speed, and
//      stretch ONLY the vowel to fill its (now larger) note slot.
//   3. HARMONY STACK: formant-preserving pitch-shift copies (octave up
//      for shimmer, octave down for body, a fifth) summed under the lead.
//
// Output: hopehop/out/hopehop-vocal-sung.mp3  (feed to render --vocal-stem)
//
//   node pop/hopehop/bin/sing.mjs --target-dur 100 --transpose 7
//   node pop/hopehop/bin/sing.mjs --slow 1.3 --harmonies "12:0.32,-12:0.5,7:0.3"

import { existsSync, readFileSync, writeFileSync, mkdirSync, unlinkSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const POP = resolve(LANE, "..");
const PY = resolve(POP, ".venv", "bin", "python");
const PSNAP = resolve(POP, "bin", "pitchsnap_world.py");
const SR = 48000;

const argv = process.argv.slice(2);
const takeFlag = (n, d) => { const i = argv.indexOf(n); return i >= 0 && argv[i + 1] ? argv[i + 1] : d; };
const VOCAL = resolve(process.cwd(), takeFlag("--vocal", `${LANE}/out/hopehop-vocal.mp3`));
const ALIGN = resolve(process.cwd(), takeFlag("--align", `${VOCAL}.alignment.json`));
const OUT   = resolve(process.cwd(), takeFlag("--out", `${LANE}/out/hopehop-vocal-sung.mp3`));
const RETAIN = takeFlag("--retain", "1.0");
const XFADE = takeFlag("--xfade-ms", "120");
const VIB_HZ = takeFlag("--vibrato-hz", "5.0");
const VIB_CENTS = takeFlag("--vibrato-cents", "12");
const TRANSPOSE = parseInt(takeFlag("--transpose", "7"), 10);   // semitones to raise the melody
const LEGATO = parseFloat(takeFlag("--legato", "0.97"));        // fill this fraction of each note slot
const MAX_HOLD = parseFloat(takeFlag("--max-hold", "2.6"));     // cap a single sustained vowel (s)
const CRISP = takeFlag("--crisp", "4");                         // rubberband crispness for the vowel body
const TARGET_DUR = parseFloat(takeFlag("--target-dur", "100")); // desired sung length (sets --slow)
const SLOW_FLAG = takeFlag("--slow", "");                       // explicit slow factor (overrides target-dur)
// harmony stack: "semis:gain,semis:gain,…" relative to the lead
const HARMONIES = takeFlag("--harmonies", "12:0.32,-12:0.5,7:0.3")
  .split(",").map((s) => s.trim()).filter(Boolean)
  .map((s) => { const [semi, g] = s.split(":"); return { semi: parseFloat(semi), gain: parseFloat(g) }; });

if (!existsSync(VOCAL)) { console.error(`✗ vocal not found: ${VOCAL}`); process.exit(1); }
if (!existsSync(ALIGN)) { console.error(`✗ alignment not found: ${ALIGN}`); process.exit(1); }
mkdirSync(`${LANE}/out`, { recursive: true });

const sh = (cmd, args, opt = {}) => spawnSync(cmd, args, { stdio: ["ignore", "inherit", "inherit"], ...opt });
const TMP = tmpdir();

// ── melody contour — C-major pentatonic wave, raised by --transpose ──
const NM = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"];
const midiToNote = (m) => NM[((m % 12) + 12) % 12] + (Math.floor(m / 12) - 1);
const BASE = [55, 57, 60, 57, 55, 52, 50, 52, 55, 57, 60, 62, 60, 57, 55, 52];
const CONTOUR = BASE.map((m) => midiToNote(m + TRANSPOSE));
console.log(`• melody: ${CONTOUR.length}-note pentatonic wave, ${midiToNote(50 + TRANSPOSE)}–${midiToNote(62 + TRANSPOSE)} (transpose +${TRANSPOSE})`);

// ── real words → (start, end, note); skip pure-punctuation tokens ────
const align = JSON.parse(readFileSync(ALIGN, "utf8"));
const raw = (align.words || []).filter((w) => /[a-z0-9]/i.test(w.text || ""));
const W = [];
let ci = 0;
for (const w of raw) {
  const s = (w.fromMs ?? 0) / 1000, e = (w.toMs ?? 0) / 1000;
  if (W.length && s <= W[W.length - 1].s) continue;     // starts must increase
  W.push({ s, e: Math.max(e, s + 0.05), note: CONTOUR[ci++ % CONTOUR.length] });
}
const NAT_SPAN = W[W.length - 1].e - W[0].s;
const SLOW = SLOW_FLAG ? parseFloat(SLOW_FLAG) : Math.max(1, TARGET_DUR / NAT_SPAN);
console.log(`• ${W.length} words · natural ${NAT_SPAN.toFixed(1)}s → slow ×${SLOW.toFixed(2)} (≈${(NAT_SPAN * SLOW).toFixed(0)}s)`);

// ── 1 · WORLD pitch-replace, anchored per-word ───────────────────────
const inWav = resolve(TMP, "hh-sing-in.wav"), psWav = resolve(TMP, "hh-sing-ps.wav");
if (sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", VOCAL, "-ar", String(SR), "-ac", "1", inWav]).status !== 0) process.exit(1);
console.log("• WORLD f0-replace (per-word, jeffrey's voice preserved) …");
if (sh(PY, [PSNAP, inWav, psWav,
  "--notes", W.map((w) => w.note).join(","),
  "--note-starts", W.map((w) => w.s.toFixed(3)).join(","),
  "--retain", RETAIN, "--xfade-ms", XFADE,
  "--vibrato-hz", VIB_HZ, "--vibrato-cents", VIB_CENTS, "--vibrato-onset-ms", "260"]).status !== 0) process.exit(1);

// ── tiny f32 WAV read/write + rubberband helpers (mono) ──────────────
function readF32(path) {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-loglevel", "error", "-i", path, "-f", "f32le", "-ar", String(SR), "-ac", "1", "-"], { maxBuffer: 1 << 30 });
  return new Float32Array(r.stdout.buffer, r.stdout.byteOffset, Math.floor(r.stdout.length / 4));
}
function writeF32Wav(buf, path) {
  const n = buf.length, b = Buffer.alloc(44 + n * 4);
  b.write("RIFF", 0); b.writeUInt32LE(36 + n * 4, 4); b.write("WAVE", 8);
  b.write("fmt ", 12); b.writeUInt32LE(16, 16); b.writeUInt16LE(3, 20); b.writeUInt16LE(1, 22);
  b.writeUInt32LE(SR, 24); b.writeUInt32LE(SR * 4, 28); b.writeUInt16LE(4, 32); b.writeUInt16LE(32, 34);
  b.write("data", 36); b.writeUInt32LE(n * 4, 40);
  for (let i = 0; i < n; i++) b.writeFloatLE(buf[i], 44 + i * 4);
  writeFileSync(path, b);
}
const rbIn = resolve(TMP, "hh-rb-in.wav"), rbOut = resolve(TMP, "hh-rb-out.wav");
function stretchSeg(seg, ratio) {
  if (seg.length < 64 || Math.abs(ratio - 1) < 0.02) return seg;
  writeF32Wav(seg, rbIn);
  const r = spawnSync("rubberband", ["-t", ratio.toFixed(4), "-c", CRISP, rbIn, rbOut], { stdio: "ignore" });
  if (r.status !== 0 || !existsSync(rbOut)) return seg;
  return readF32(rbOut);
}
function pitchShift(buf, semis) {
  if (semis === 0) return buf.slice();
  const pin = resolve(TMP, "hh-harm-in.wav"), pout = resolve(TMP, `hh-harm-${semis}.wav`);
  writeF32Wav(buf, pin);
  const r = spawnSync("rubberband", ["-p", String(semis), "-F", "--pitch-hq", pin, pout], { stdio: "ignore" });
  try { unlinkSync(pin); } catch {}
  if (r.status !== 0 || !existsSync(pout)) return buf.slice();
  return readF32(pout);
}

// ── 2 · slow + vowel-hold — place words on a ×SLOW grid, hold vowels ─
console.log(`• slowing + holding vowels (legato ${LEGATO}, max-hold ${MAX_HOLD}s, crisp ${CRISP}) …`);
const src = readF32(psWav);
const t0 = W[0].s;
const outStart = (i) => (W[i].s - t0) * SLOW;
const slot = (i) => (i + 1 < W.length ? (W[i + 1].s - W[i].s) : (W[i].e - W[i].s + 0.6)) * SLOW;
const out = new Float32Array(Math.round((outStart(W.length - 1) + MAX_HOLD + 1.5) * SR));
let held = 0;
for (let i = 0; i < W.length; i++) {
  const w = W[i];
  const a = Math.round(w.s * SR), z = Math.min(src.length, Math.round(w.e * SR));
  if (z <= a) continue;
  const word = src.subarray(a, z), wn = word.length;
  const onN = Math.min(Math.round(0.05 * SR), Math.floor(wn * 0.3));   // onset consonant
  const coN = Math.min(Math.round(0.04 * SR), Math.floor(wn * 0.25));  // coda consonant
  const midN = Math.max(0, wn - onN - coN);                            // vowel body
  const target = Math.min(slot(i) * LEGATO, (w.e - w.s) + MAX_HOLD);   // total held length (s)
  let body = word.subarray(onN, onN + midN);
  if (midN > 64 && target * SR > wn) {
    const ratio = Math.max(1, (target * SR - onN - coN) / midN);
    body = stretchSeg(body, ratio);
    if (ratio > 1.05) held++;
  }
  const piece = new Float32Array(onN + body.length + coN);
  piece.set(word.subarray(0, onN), 0);
  piece.set(body, onN);
  piece.set(word.subarray(onN + midN, onN + midN + coN), onN + body.length);
  const j0 = Math.round(outStart(i) * SR), xf = Math.min(96, onN);
  for (let k = 0; k < piece.length; k++) {
    const o = j0 + k; if (o < 0 || o >= out.length) break;
    let g = 1; if (k < xf) g = k / xf; if (k > piece.length - xf) g = Math.min(g, (piece.length - k) / xf);
    out[o] += piece[k] * g;
  }
}
console.log(`• held ${held}/${W.length} vowels out to their notes`);

// ── 3 · harmony stack — pitch-shifted copies under the lead ──────────
const mix = new Float32Array(out.length);
for (let i = 0; i < out.length; i++) mix[i] = out[i];           // lead = 1.0
for (const h of HARMONIES) {
  console.log(`• harmony ${h.semi > 0 ? "+" : ""}${h.semi} semis @ ${h.gain}`);
  const v = pitchShift(out, h.semi);
  for (let i = 0; i < out.length && i < v.length; i++) mix[i] += v[i] * h.gain;
}
// peak-safety
let pk = 0; for (let i = 0; i < mix.length; i++) pk = Math.max(pk, Math.abs(mix[i]));
if (pk > 0.99) { const g = 0.99 / pk; for (let i = 0; i < mix.length; i++) mix[i] *= g; }
const sungWav = resolve(TMP, "hh-sing-sung.wav");
writeF32Wav(mix, sungWav);

// ── encode → mp3 ─────────────────────────────────────────────────────
if (sh("ffmpeg", ["-hide_banner", "-loglevel", "error", "-y", "-i", sungWav, "-c:a", "libmp3lame", "-b:a", "256k", OUT]).status !== 0) process.exit(1);
for (const f of [inWav, rbIn, rbOut]) { try { unlinkSync(f); } catch {} }
console.log(`✓ sung+harmonized vocal (${(mix.length / SR).toFixed(1)}s) → ${OUT.replace(POP + "/", "pop/")}`);
