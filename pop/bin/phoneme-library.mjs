#!/usr/bin/env node
// phoneme-library.mjs — grow the jeffrey phoneme library to the full
// permutative set: every onset (a–z consonants, digraphs, clusters) × every
// vowel (CV), every vowel × every coda (VC), and the bare vowels.
//
// Renders missing clips with jeffrey-pvc on eleven_v4 (the library's own
// settings), keeps the untrimmed return in raw/, trims both ends into
// syllables/, and rewrites library.json. Existing clips are never re-rendered
// unless --force.
//
//   node pop/bin/phoneme-library.mjs [--dry-run] [--force] [--only bah,truh] [--style whisper] [--words kum=come,pew]
//
// --style renders an inflection bank (CV + bare vowels) through a v4 audio tag
// into syllables-<style>/: whisper, breathy, shout, angry, scream.
//
// Key: ELEVENLABS_API_KEY from env, else the vault's lith/.env.

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const VAULT = process.env.AC_VAULT || join(ROOT, "aesthetic-computer-vault");
// --voice prutti: Prutti's consented IVC (Pruttivox settings) into voice/prutti-phonemes.
// Klokkentales release gate: his clone + performance need his approval before anything ships.
const VOICE = process.argv.includes("--voice") ? process.argv[process.argv.indexOf("--voice") + 1] : "jeffrey";
const LIB = join(VAULT, "voice", `${VOICE}-phonemes`);
const args = process.argv.slice(2);
const DRY = args.includes("--dry-run"), FORCE = args.includes("--force");
const STYLE = args.includes("--style") ? args[args.indexOf("--style") + 1] : null;
// --words name[=text],… renders whole sung syllables ("kum=come") into words/, for lyrics
const WORDS = args.includes("--words") ? args[args.indexOf("--words") + 1].split(",").map((w) => { const [name, text] = w.split("="); return { name, kind: "word", text: text ?? name }; }) : null;
const ONLY = args.includes("--only") ? new Set(args[args.indexOf("--only") + 1].split(",")) : null;

// vowels spelt as they sound; the five originals keep their names
// (ai = the "eye" diphthong, typed as "igh" so the TTS reads it right)
const VOWELS = ["ah", "eh", "ee", "oh", "oo", "ay", "uh", "aw", "ow", "ai", "oy", "ih", "er"];
// onsets: one per consonant letter (c → ch, q → kw, x lives in the codas as ks),
// digraphs, and the common English clusters
const ONSETS = [
  "b", "ch", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "kw", "r", "s", "t", "v", "w", "y", "z",
  "sh", "th", "zh",
  "tr", "dr", "kr", "gr", "br", "pr", "fr", "thr", "st", "sk", "sp", "sm", "sn", "sl", "pl", "bl", "kl", "gl", "fl", "sw", "tw",
  "str", "spr", "skr", "spl", "shr", "skw",
];
const CODAS = ["b", "d", "f", "g", "k", "l", "m", "n", "p", "r", "s", "t", "v", "z", "sh", "ch", "th", "ks", "ng",
  "nd", "nt", "st", "rk", "ld", "lt", "mp", "nk", "ts"];
// which letter key each onset/vowel belongs to (the typed-melody law)
const LETTER = {
  a: "ah", e: "eh", i: "ee", o: "oh", u: "oo",
  b: "b", c: "ch", d: "d", f: "f", g: "g", h: "h", j: "j", k: "k", l: "l", m: "m", n: "n", p: "p",
  q: "kw", r: "r", s: "s", t: "t", v: "v", w: "w", x: "ks", y: "y", z: "z",
};

// spellings the TTS reads as letters ("ehs" → "E-H-S") get a plain-word text
const SPELL = { ehp: "ep", ehs: "ess", ehv: "ev", ohg: "ogue", uhp: "up" };

const vt = (v, bare = false) => (v === "ai" ? (bare ? "eye" : "igh") : v);
const textOf = (it) => it.text ?? SPELL[it.name] ?? (it.kind === "v" ? vt(it.vowel, true) : (it.onset ?? "") + vt(it.vowel) + (it.coda ?? ""));
const STYLES = {
  whisper: (t) => `[whispers] ${t}`,
  breathy: (t) => `[breathy] ${t}`,
  shout: (t) => `[shouting] ${t.toUpperCase()}!`,
  angry: (t) => `[angry] ${t.toUpperCase()}!`,
  scream: (t) => `[screaming] ${t.toUpperCase().replace(/([AEIOUY])([^AEIOUY]*)$/, "$1$1$1$2")}!`,
};
if (STYLE && !STYLES[STYLE]) throw new Error(`unknown style ${STYLE}: ${Object.keys(STYLES).join(", ")}`);
const SYL = join(LIB, WORDS ? "words" : STYLE ? `syllables-${STYLE}` : "syllables"), RAW = join(LIB, WORDS ? "raw-words" : STYLE ? `raw-${STYLE}` : "raw");

const items = [];
for (const v of VOWELS) items.push({ name: v, kind: "v", vowel: v });
for (const o of ONSETS) for (const v of VOWELS) items.push({ name: o + v, kind: "cv", onset: o, vowel: v });
for (const v of VOWELS) for (const c of CODAS) items.push({ name: v + c, kind: "vc", vowel: v, coda: c });

if (VOICE === "prutti" && !existsSync(join(LIB, "library.json"))) {
  mkdirSync(LIB, { recursive: true });
  const id = readFileSync(join(VAULT, "lith", ".env"), "utf8").match(/^PRUTTI_ELEVENLABS_VOICE_ID=(.*)$/m)?.[1].replace(/^"|"$/g, "");
  writeFileSync(join(LIB, "library.json"), JSON.stringify({ voice: "prutti-ivc", voice_id: id, model: "eleven_v4",
    voice_settings: { stability: 0.5, similarity_boost: 0.9, style: 0.48, use_speaker_boost: true, speed: 0.98 },
    consent: "Klokkentales SCORE: Prutti approves clone + performance before release; drafts only until then", words: {} }, null, 2) + "\n");
}
const lib = JSON.parse(readFileSync(join(LIB, "library.json"), "utf8"));
const settings = lib.voice_settings;
mkdirSync(RAW, { recursive: true });
mkdirSync(SYL, { recursive: true });

function apiKey() {
  if (process.env.ELEVENLABS_API_KEY) return process.env.ELEVENLABS_API_KEY;
  const env = readFileSync(join(VAULT, "lith", ".env"), "utf8");
  return env.match(/^ELEVENLABS_API_KEY=(.*)$/m)?.[1].replace(/^"|"$/g, "");
}

async function render(text, out) {
  const key = apiKey();
  for (let attempt = 0; attempt < 5; attempt++) {
    const res = await fetch(`https://api.elevenlabs.io/v1/text-to-speech/${lib.voice_id}?output_format=mp3_22050_32`, {
      method: "POST",
      headers: { "xi-api-key": key, "content-type": "application/json" },
      body: JSON.stringify({ text, model_id: lib.model, voice_settings: settings }),
    });
    if (res.ok) { writeFileSync(out, Buffer.from(await res.arrayBuffer())); return; }
    if (res.status === 429 || res.status >= 500) { await new Promise((r) => setTimeout(r, 1500 * (attempt + 1))); continue; }
    throw new Error(`${text}: ${res.status} ${await res.text()}`);
  }
  throw new Error(`${text}: retries exhausted`);
}

// trim both ends only (a single-pass stop cut clips inside the syllable)
function trim(raw, out) {
  const floor = STYLE === "whisper" || STYLE === "breathy" ? -58 : -45;
  const sr = `silenceremove=start_periods=1:start_threshold=${floor}dB:start_silence=0.01`;
  execFileSync("ffmpeg", ["-y", "-loglevel", "error", "-i", raw, "-af", `${sr},areverse,${sr},areverse`,
    "-ar", "22050", "-ac", "1", "-b:a", "32k", out]);
}
const duration = (f) => Number(execFileSync("ffprobe", ["-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", f]).toString());

const set = WORDS ? WORDS : STYLE ? items.filter((it) => it.kind !== "vc" || ONLY?.has(it.name)) : items; // --only can pull a VC into a bank
const todo = set.filter((it) => (!ONLY || ONLY.has(it.name)) && (FORCE || !existsSync(join(SYL, it.name + ".mp3"))));
const say = (it) => (STYLE ? STYLES[STYLE](textOf(it)) : textOf(it));
console.log(`${STYLE ?? "plain"}: ${set.length} syllables in the set · ${todo.length} to render · ~${todo.reduce((n, it) => n + say(it).length, 0)} characters`);
if (DRY) { console.log(todo.map((t) => t.name).join(" ")); process.exit(0); }

const short = [];
let done = 0;
async function worker() {
  while (todo.length) {
    const it = todo.shift();
    const raw = join(RAW, it.name + ".mp3"), out = join(SYL, it.name + ".mp3");
    if (FORCE || !existsSync(raw)) await render(say(it), raw);
    trim(raw, out);
    const d = duration(out);
    if (d < 0.18 || d > (STYLE === "scream" ? 2.2 : 1.2)) short.push(`${it.name} ${d.toFixed(2)}s`);
    if (++done % 25 === 0) console.log(`  ${done} rendered`);
  }
}
await Promise.all([worker(), worker(), worker(), worker()]);

// manifest: keep everything already there, add the permutative index
const reread = JSON.parse(readFileSync(join(LIB, "library.json"), "utf8")); // parallel style runs each own one key
if (WORDS) {
  reread.words = reread.words || {};
  for (const it of WORDS) if (existsSync(join(SYL, it.name + ".mp3"))) reread.words[it.name] = it.text;
  writeFileSync(join(LIB, "library.json"), JSON.stringify(reread, null, 2) + "\n");
  console.log(`${done} rendered · words bank indexes ${Object.keys(reread.words).length}`);
  if (short.length) console.log(`check by ear: ${short.join(", ")}`);
  process.exit(0);
}
if (STYLE) {
  reread.styles = reread.styles || {};
  reread.styles[STYLE] = { tag: STYLES[STYLE]("{text}"), dir: `syllables-${STYLE}`, syllables: set.filter((it) => existsSync(join(SYL, it.name + ".mp3"))).map((it) => it.name) };
  writeFileSync(join(LIB, "library.json"), JSON.stringify(reread, null, 2) + "\n");
  console.log(`${done} rendered · ${STYLE} bank indexes ${reread.styles[STYLE].syllables.length}`);
  if (short.length) console.log(`check by ear: ${short.join(", ")}`);
  process.exit(0);
}
Object.assign(lib, { styles: reread.styles });
lib.vowels_v2 = VOWELS;
lib.onsets = ONSETS;
lib.codas = CODAS;
lib.letters = LETTER;
lib.syllables = lib.syllables || {};
for (const it of items) if (existsSync(join(LIB, "syllables", it.name + ".mp3"))) lib.syllables[it.name] = it.name;
lib.index = items.filter((it) => existsSync(join(LIB, "syllables", it.name + ".mp3")));
writeFileSync(join(LIB, "library.json"), JSON.stringify(lib, null, 2) + "\n");
console.log(`${done} rendered · library.json now indexes ${lib.index.length}`);
if (short.length) console.log(`check by ear (duration outside 0.18–1.2 s): ${short.join(", ")}`);
