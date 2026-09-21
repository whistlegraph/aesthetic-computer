#!/usr/bin/env node
// hear.mjs — can the words be heard? Render every sung line of a score
// OFFLINE with Menu Band's own singer (slab/menuband: `singrender`, the same
// MenuBandSinger.swift the app runs), run the WAVs back through Whisper
// (whisper-cli, ggml-base.en — the house model in recap/models), and score
// each line's transcript against its lyric as word error rate. No audio
// device is touched; nothing opens. The spoken TTS source is scored too —
// the ceiling the singing pass can only fall from.
//
//   node bin/hear.mjs scores/dialog-07-before.mbscore          one score
//   node bin/hear.mjs 'scores/dialog-*.mbscore' --tag baseline  many, tagged
//   node bin/hear.mjs … --voice 0 --rate 0.42 --lock 0.6       overrides
//   node bin/hear.mjs … --keep DIR                             keep the WAVs there
//
// Results: hear/<tag>.json (per line: lyric, heard, wer, spoken-wer) and a
// table on stdout. The WER is whisper-base's — a coarse, honest judge: it
// hears what a stranger in the room would.
import { existsSync, mkdirSync, readFileSync, readdirSync, writeFileSync, rmSync } from "node:fs";
import { basename, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";
import { tmpdir } from "node:os";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const REPO = resolve(LANE, "..", "..", "..");
const RENDER = process.env.SINGRENDER || resolve(REPO, "slab/menuband/.build/release/singrender");   // SINGRENDER=… tries another build
// the judge: small.en when it is on disk (it hears sung words far more
// steadily than base.en — 2026-09-21 A/B), else the house base.en
const MODEL = process.env.WHISPER_MODEL
  || [resolve(REPO, "recap/models/ggml-small.en.bin"), resolve(REPO, "recap/models/ggml-base.en.bin")].find(existsSync);

// ---- words --------------------------------------------------------------
const SMALL = ["zero","one","two","three","four","five","six","seven","eight","nine","ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"];
const TENS = ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"];
function num2words(n) {
  if (n < 20) return SMALL[n];
  if (n < 100) return TENS[Math.floor(n / 10)] + (n % 10 ? " " + SMALL[n % 10] : "");
  if (n < 1000) return SMALL[Math.floor(n / 100)] + " hundred" + (n % 100 ? " " + num2words(n % 100) : "");
  if (n < 1_000_000) return num2words(Math.floor(n / 1000)) + " thousand" + (n % 1000 ? " " + num2words(n % 1000) : "");
  return String(n);
}
const ORD = { first: "one", second: "two", third: "three", fourth: "four", fifth: "five", sixth: "six", seventh: "seven", eighth: "eight", ninth: "nine", tenth: "ten", nineteenth: "nineteen", twentieth: "twenty" };
export function normalize(s) {
  let t = String(s).toLowerCase();
  for (const [re, to] of CONTR) t = t.replace(re, to);
  return t
    .replace(/(\d),(\d)/g, "$1$2")
    .replace(/\d+(st|nd|rd|th)\b/g, (m) => m.replace(/\D/g, ""))
    .replace(/\d+/g, (m) => num2words(parseInt(m, 10)))
    .replace(/[^a-z\s']/g, " ")
    .replace(/'/g, "")
    .split(/\s+/).filter(Boolean)
    .map((w) => ORD[w] || w)
    .filter((w) => w !== "and");                    // "three hundred and five" = "three hundred five"
}
export function wer(ref, hyp) {
  const R = normalize(ref), H = normalize(hyp);
  const d = Array.from({ length: R.length + 1 }, (_, i) => [i, ...Array(H.length).fill(0)]);
  for (let j = 1; j <= H.length; j++) d[0][j] = j;
  for (let i = 1; i <= R.length; i++) for (let j = 1; j <= H.length; j++)
    d[i][j] = Math.min(d[i - 1][j] + 1, d[i][j - 1] + 1, d[i - 1][j - 1] + (R[i - 1] === H[j - 1] ? 0 : 1));
  return { errors: d[R.length][H.length], words: R.length, wer: R.length ? d[R.length][H.length] / R.length : 0 };
}

// Contractions are the same words heard: "there's" is "there is", not a
// lost word. Expanded before scoring, on both sides.
const CONTR = [[/\b(there|it|he|she|that|what|who|here|where)'s\b/g, "$1 is"], [/\b(we|you|they)'re\b/g, "$1 are"],
  [/\bi'm\b/g, "i am"], [/\b(i|you|we|they)'ve\b/g, "$1 have"], [/\b(i|you|he|she|we|they)'ll\b/g, "$1 will"],
  [/\b(i|you|he|she|we|they)'d\b/g, "$1 would"], [/\bcan't\b/g, "can not"], [/\bwon't\b/g, "will not"], [/\bain't\b/g, "is not"],
  [/\b(is|are|was|were|do|does|did|has|have|had|could|would|should)n't\b/g, "$1 not"]];
const argv = process.argv.slice(2);
const flags = {};
const words = [];
for (let i = 0; i < argv.length; i++) {
  if (argv[i].startsWith("--")) { const k = argv[i].slice(2); const v = argv[i + 1] && !argv[i + 1].startsWith("--") ? argv[++i] : "1"; flags[k] = v; }
  else words.push(argv[i]);
}
// --rescore: recompute every hear/*.json's scores from its stored transcripts
// with the current normalize() — after a fairness change to the scorer.
if (flags.rescore) {
  for (const f of readdirSync(resolve(LANE, "hear")).filter((f) => f.endsWith(".json"))) {
    const p = resolve(LANE, "hear", f), r = JSON.parse(readFileSync(p, "utf8"));
    let e = 0, w = 0, se = 0, sw = 0;
    for (const s of r.scores) for (const v of s.voices) for (const l of v.lines) {
      const x = wer(l.text, l.heard); l.wer = x.wer; l.errors = x.errors; l.words = x.words; e += x.errors; w += x.words;
      if (l.spokenHeard != null) { const y = wer(l.text, l.spokenHeard); l.spokenWer = y.wer; se += y.errors; sw += y.words; }
    }
    r.total = { errors: e, words: w, wer: w ? e / w : 0, spokenErrors: se, spokenWords: sw, spokenWer: sw ? se / sw : 0 };
    writeFileSync(p, JSON.stringify(r, null, 2) + "\n");
    console.log(`${f.padEnd(22)} ${(100 * r.total.wer).toFixed(1).padStart(5)}% (${e}/${w})`);
  }
  process.exit(0);
}
// --compare A B [C…]: table the runs side by side (total, per voice, and the
// lines that moved), from hear/<tag>.json — no rendering.
if (flags.compare) {
  const tags = [flags.compare, ...words];
  const runs = tags.filter((t) => existsSync(resolve(LANE, "hear", `${t}.json`)) || (console.log(`  (no hear/${t}.json — skipped)`), false))
    .map((t) => ({ t, r: JSON.parse(readFileSync(resolve(LANE, "hear", `${t}.json`), "utf8")) }));
  const cell = (r) => `${(100 * r.total.wer).toFixed(1).padStart(5)}% (${r.total.errors}/${r.total.words})`;
  console.log("\nrun".padEnd(15) + tags.map((t) => t.padStart(18)).join(""));
  console.log("total".padEnd(14) + runs.map(({ r }) => cell(r).padStart(18)).join(""));
  const members = [...new Set(runs.flatMap(({ r }) => r.scores.flatMap((s) => s.voices.map((v) => v.member))))];
  for (const m of members) {
    const row = runs.map(({ r }) => { let e = 0, w = 0; for (const s of r.scores) for (const v of s.voices) if (v.member === m) for (const l of v.lines) { e += l.errors; w += l.words; } return `${(100 * e / w).toFixed(1).padStart(5)}% (${e}/${w})`; });
    console.log(m.padEnd(14) + row.map((c) => c.padStart(18)).join(""));
  }
  if (runs.length === 2) {
    const key = (s, v, l) => `${s.score}|${v.member}|${l.line}`;
    const A = new Map(), B = new Map();
    for (const s of runs[0].r.scores) for (const v of s.voices) for (const l of v.lines) A.set(key(s, v, l), l);
    for (const s of runs[1].r.scores) for (const v of s.voices) for (const l of v.lines) B.set(key(s, v, l), l);
    const moved = [...A.keys()].filter((k) => B.has(k) && A.get(k).errors !== B.get(k).errors)
      .map((k) => ({ k, d: B.get(k).errors - A.get(k).errors, a: A.get(k), b: B.get(k) })).sort((x, y) => x.d - y.d);
    console.log(`\n${moved.filter((m) => m.d < 0).length} lines better, ${moved.filter((m) => m.d > 0).length} worse:`);
    for (const m of moved) console.log(`  ${m.d < 0 ? "▲" : "▼"} ${String(m.d).padStart(3)}  ${m.a.text}\n        ${tags[0]}: ${m.a.heard}\n        ${tags[1]}: ${m.b.heard}`);
  }
  process.exit(0);
}
if (!words.length) { console.log("usage: node bin/hear.mjs <score.mbscore|glob> [--tag name] [--voice i] [--rate r] [--lock l] [--vib-hz h] [--vib-cents c] [--f0-floor f] [--keep DIR] [--no-spoken] [--env K=V,…]"); process.exit(1); }
if (!existsSync(RENDER)) { console.log(`✗ ${RENDER} missing — cd slab/menuband && swift build -c release --product singrender`); process.exit(1); }
if (!existsSync(MODEL)) { console.log(`✗ whisper model missing: ${MODEL}`); process.exit(1); }

// scores: literal paths or a glob on the basename
const scores = [];
for (const w of words) {
  if (w.includes("*")) {
    const dir = resolve(LANE, dirname(w));
    const re = new RegExp("^" + basename(w).replace(/[.+^${}()|[\]\\]/g, "\\$&").replace(/\*/g, ".*") + "$");
    for (const f of readdirSync(dir).sort()) if (re.test(f)) scores.push(resolve(dir, f));
  } else scores.push(resolve(LANE, w));
}
const tag = flags.tag || new Date().toISOString().slice(0, 16).replace(/[-:T]/g, "");
const keep = flags.keep ? resolve(flags.keep) : resolve(tmpdir(), "mnp-hear", tag);
mkdirSync(keep, { recursive: true });
const env = { ...process.env, SINGER_SPEECH_CACHE: process.env.SINGER_SPEECH_CACHE || resolve(tmpdir(), "mnp-hear", "speech-cache") };
for (const kvp of String(flags.env || "").split(",").filter(Boolean)) { const [k, v] = kvp.split("="); env[k] = v; }

// ---- the payload, exactly as trio.mjs posts it ----------------------------
function kvFor(score, voice) {
  const member = String(voice.name).split(/\s|·/)[0];
  const vjPath = resolve(LANE, "members", member, "voice.json");
  const vj = existsSync(vjPath) ? JSON.parse(readFileSync(vjPath, "utf8")) : {};
  const prof = vj.aesthetivox || {};
  // --transpose N: shift every note N semitones — a stand-in voice of another
  // register (a male voice on a line written around C4) sings where it lives
  const tr = Number(flags.transpose || 0);
  const notes = tr ? String(voice.notes).split(",").map((t) => { const [k, d] = t.split(":"); return /^\d+$/.test(k) ? `${Number(k) + tr}:${d}` : t; }).join(",") : voice.notes;
  const kv = [`notes=${notes}`, `lyrics=${String(voice.lyrics).replace(/[;=]/g, " ")}`,
    `singVoice=${flags["voice-name"] || voice.singVoice || prof.base_voice || "Fred"}`,
    `singVibratoHz=${flags["vib-hz"] ?? voice.singVibratoHz ?? prof.sing?.vibrato_hz ?? 5}`,
    `singVibCents=${flags["vib-cents"] ?? prof.sing?.vibrato_depth_cents ?? 18}`,
    `singLock=${flags.lock ?? prof.sing?.harmony_lock ?? 0.875}`,
    `singF0Floor=${flags["f0-floor"] ?? (flags["floor-was"] != null ? prof.f0_floor_was : null) ?? prof.f0_floor ?? 55}`];
  return { kv: kv.join(";"), member };
}

// --prime: render only, and skip Whisper entirely. The point is the spoken
// TTS cache: a line can only be spoken where its voice is installed, so the
// machine that HAS the voice primes the cache and the evaluation host (which
// may have neither the voice nor the spare capacity) reads it.
const primeOnly = flags.prime != null;
function transcribe(wav) {
  if (primeOnly) return "";
  const w16 = wav.replace(/\.wav$/, ".16k.wav");
  spawnSync("ffmpeg", ["-y", "-loglevel", "error", "-i", wav, "-ar", "16000", "-ac", "1", w16], { stdio: "ignore" });
  const r = spawnSync("whisper-cli", ["-m", MODEL, "-f", w16, "-l", "en", "-nt", "-np", "-t", "4"], { encoding: "utf8" });
  rmSync(w16, { force: true });
  return (r.stdout || "").replace(/\[[^\]]*\]/g, " ").replace(/\s+/g, " ").trim();
}

// ---- run ------------------------------------------------------------------
const results = { tag, at: new Date().toISOString(), model: basename(MODEL), flags, scores: [] };
let sumE = 0, sumW = 0, spE = 0, spW = 0;
const spoken = flags["no-spoken"] == null;
for (const sp of scores) {
  const score = JSON.parse(readFileSync(sp, "utf8"));
  const bpm = score.bpm || 120;
  const voices = (score.voices || []).map((v, i) => ({ v, i })).filter(({ v, i }) => v.lyrics && (flags.voice == null || Number(flags.voice) === i));
  const entry = { score: basename(sp), bpm, voices: [] };
  console.log(`\n♪ ${score.title || basename(sp)} — ${bpm} bpm`);
  for (const { v, i } of voices) {
    const { kv, member } = kvFor(score, v);
    const out = resolve(keep, basename(sp, ".mbscore"), member);
    mkdirSync(out, { recursive: true });
    const args = ["--kv", kv, "--bpm", String(bpm), "--out", out];
    if (spoken && !primeOnly) args.push("--spoken");
    if (flags.rate) args.push("--rate", flags.rate);
    // A render can lose a line to a stalled synthesizer under load (20 s
    // cap) — run again and take the better manifest, so runs stay comparable.
    let man = null;
    for (let attempt = 0; attempt < 3; attempt++) {
      const r = spawnSync(RENDER, args, { encoding: "utf8", env, maxBuffer: 1 << 26 });
      if (r.status !== 0) { console.log(`  ✗ ${member}: singrender failed (attempt ${attempt + 1})\n${(r.stderr || "").slice(-400)}`); continue; }
      const m = JSON.parse(r.stdout);
      const bad = m.lines.filter((l) => l.error || !l.wav).length;
      if (!man || bad < man.lines.filter((l) => l.error || !l.wav).length) man = m;
      if (bad === 0) break;
      console.log(`  ↻ ${member}: ${bad} line(s) failed to render — retrying`);
    }
    if (!man) continue;
    const ve = { member, voice: man.voice, lines: [] };
    console.log(`  ${member} (${man.voice})`);
    for (const ln of man.lines) {
      if (ln.error || !ln.wav) { console.log(`    ${String(ln.line).padStart(2)}. ✗ ${ln.error}`); continue; }
      const heard = transcribe(ln.wav);
      const s = wer(ln.text, heard);
      let sh = null, ss = null;
      if (ln.spoken) { sh = transcribe(ln.spoken); ss = wer(ln.text, sh); spE += ss.errors; spW += ss.words; }
      sumE += s.errors; sumW += s.words;
      const mark = s.wer === 0 ? "✓" : s.wer <= 0.25 ? "~" : "✗";
      console.log(`    ${String(ln.line).padStart(2)}. ${mark} ${(s.wer * 100).toFixed(0).padStart(3)}%${ss ? ` (spoken ${(ss.wer * 100).toFixed(0)}%)` : ""}  ${ln.duration.toFixed(1)}s  ${ln.text}`);
      if (s.wer > 0) console.log(`         heard: ${heard || "(nothing)"}`);
      ve.lines.push({ line: ln.line, text: ln.text, lyrics: ln.lyrics, notes: ln.notes, duration: ln.duration, peak: ln.peak,
        notesUsed: ln.notesUsed, noteCount: ln.noteCount, renderMs: ln.renderMs, heard, wer: s.wer, errors: s.errors, words: s.words,
        spokenHeard: sh, spokenWer: ss?.wer ?? null, wav: ln.wav, spokenWav: ln.spoken ?? null });
    }
    entry.voices.push(ve);
  }
  results.scores.push(entry);
}
results.total = { errors: sumE, words: sumW, wer: sumW ? sumE / sumW : 0, spokenErrors: spE, spokenWords: spW, spokenWer: spW ? spE / spW : 0 };
mkdirSync(resolve(LANE, "hear"), { recursive: true });
writeFileSync(resolve(LANE, "hear", `${tag}.json`), JSON.stringify(results, null, 2) + "\n");
console.log(`\n═ ${tag}: sung WER ${(results.total.wer * 100).toFixed(1)}% (${sumE}/${sumW} words)${spW ? ` · spoken source WER ${(results.total.spokenWer * 100).toFixed(1)}%` : ""}`);
console.log(`  wavs: ${keep}\n  json: hear/${tag}.json`);
